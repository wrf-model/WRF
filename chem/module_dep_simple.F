    MODULE module_dep_simple
!
! many of these parameters will depend on the RADM mechanism!
! if you change it, lets talk about it and get it done!!!
!

      INTEGER, PARAMETER :: dep_seasons = 5 ,   nlu = 25,                   &
        nseason = 1, nseasons = 2
!
! following currently hardwired to USGS
!
      INTEGER, PARAMETER :: isice_temp=24,iswater_temp=16
      character, parameter :: mminlu='USGS'
!
      INTEGER :: ixxxlu(nlu)
      REAL :: kpart(nlu),                                                   &
        rac(nlu,dep_seasons), rclo(nlu,dep_seasons), rcls(nlu,dep_seasons), &
        rgso(nlu,dep_seasons), rgss(nlu,dep_seasons),                       &
        ri(nlu,dep_seasons), rlu(nlu,dep_seasons)
!
! NO MORE THAN 1000 SPECIES FOR DEPOSITION
!
      REAL, DIMENSION (1:1000) :: dratio,hstar,hstar4,f0,dhr,scpr23
! .. Default Accessibility ..
      PUBLIC
! ..
    CONTAINS
       subroutine wesely_driver(id,ktau,dtstep,                           &
               config_flags,                                              &
               gmt,julday,t_phy,moist,p8w,t8w,                            &
               p_phy,chem,rho_phy,dz8w,vgs,aer_res,                &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               numchem,                                &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!----------------------------------------------------------------------
  USE module_model_constants 
  USE module_configure
  USE module_state_description                       
  USE module_data_sorgam
! USE module_data_radm2                               
! USE module_data_racm                               
          
   INTEGER,      INTENT(IN   ) :: id,julday,          &
                                  numchem,                                &
                                  ids,ide, jds,jde, kds,kde,              &
                                  ims,ime, jms,jme, kms,kme,              &
                                  its,ite, jts,jte, kts,kte     
   INTEGER,      INTENT(IN   ) ::                                         &
                                  ktau            
      REAL,      INTENT(IN   ) ::                                         &
                             dtstep,gmt
!
! advected moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),               &
         INTENT(IN ) ::                                   moist  
!
! advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                &
         INTENT(INOUT ) ::                                   chem
!
! deposition velocities
!
   REAL, DIMENSION( its:ite, jts:jte, numchem ),                          &
         INTENT(INOUT ) ::   vgs                     
!
! input from met model
!

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,              &
          INTENT(IN   ) ::                                                &
                                                      t_phy,              &
                                                      p_phy,              &
                                                      dz8w,               &
                                                      z    ,              &
                                              t8w,p8w,z_at_w ,            &
                                                    rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,              &
          INTENT(IN   ) ::                                                &
                                                     ivgtyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,              &
          INTENT(IN   ) ::                                                &
                                                     tsk,                 &
                                                     gsw,                 &
                                                  vegfra,                 &
                                                     pbl,                 &
                                                     rmol,                &
                                                     ust,                 &
                                                     xlat,                &
                                                     xlong,               &
                                                     znt

!--- deposition and emissions stuff
! ..
! .. Local Scalars ..
      REAL ::  clwchem,dvfog,dvpart,rad,rhchem,ta,ustar,vegfrac,z1,zntt
      INTEGER :: iland, iprt, iseason, jce, jcs, n, nr, ipr,jpr,nvr
      LOGICAL :: highnh3, rainflag, vegflag, wetflag
! ..
! .. Local Arrays ..  
      REAL :: p(kts:kte-1), srfres(numchem),vgs0d(numchem) !                                             
! necessary for aerosols (module dependent)         
!  
      real :: aer_res(its:ite,jts:jte),rcx(numchem)
                                                     
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
          
                                                     
! ..                                                 
! .. Intrinsic Functions ..                       
      INTRINSIC max, min                             
! ..                                                 
      CALL wrf_debug(15,'in dry_dep_wesely')
      iseason = 1
      if(julday.lt.90.or.julday.gt.270)then
        iseason=2
        CALL wrf_debug(15,'setting iseason to 2')
      endif
      do 100 j=jts,jte 
      do 100 i=its,ite 
      iprt=0
      iland = ivgtyp(i,j)
      ta = tsk(i,j)
      rad = gsw(i,j)
      vegfrac = vegfra(i,j)
      pa = .01*p_phy(i,kts,j)
      clwchem = moist(i,kts,j,p_qc)
      ustar = ust(i,j)
      zntt = znt(i,j)
      z1 = z_at_w(i,kts+1,j)-z_at_w(i,kts,j)

!     Set logical default values
      rainflag = .FALSE.
      wetflag = .FALSE.
      highnh3 = .FALSE.
      if(p_qr.gt.1)then
         if(moist(i,kts,j,p_qr).gt.0.)rainflag = .true.
      endif
      rhchem = MIN( 100.,100. * moist(i,kts,j,p_qv) / &
               (3.80*exp(17.27*(t_phy(i,kts,j)-273.)/(t_phy(i,kts,j)-36.))/pa))
      rhchem = MAX(5.,RHCHEM)
      if (rhchem >= 95.) wetflag = .true.

      if(chem(i,kts,j,p_nh3).gt.2.*chem(i,kts,j,p_so2))highnh3 = .true.

!--- deposition
      
!     if(snowc(i,j).gt.0.)iseason=4
      CALL rc(rcx,ta,rad,rhchem,iland,iseason,numchem,      &
        wetflag,rainflag,highnh3,iprt,p_o3,p_so2,p_nh3)
      DO n = 1, numchem-2
        srfres(n) = rcx(n)
      END DO
      CALL deppart(rmol(i,j),ustar,rhchem,clwchem,iland,dvpart,dvfog)
      vgs0d=0.
      aer_res(i,j)=0.
      CALL landusevg(vgs0d,ustar,rmol(i,j),zntt,z1,dvpart,iland,        &
           numchem,srfres,aer_res(i,j),p_sulf)
      vgs(i,j,1:numchem)=vgs0d(1:numchem)

100   continue


END SUBROUTINE wesely_driver

! **********************************************************************
! **************************  SUBROUTINE RC  ***************************
! **********************************************************************
      SUBROUTINE rc(rcx,t,rad,rh,iland,iseason,numchem,             &
          wetflag,rainflag,highnh3,iprt,lo3,lso2,lnh3)
!     THIS SUBROUTINE CALCULATES SURFACE RESISTENCES ACCORDING
!     TO THE MODEL OF
!     M. L. WESELY,
!     ATMOSPHERIC ENVIRONMENT 23 (1989), 1293-1304
!     WITH SOME ADDITIONS ACCORDING TO
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY  WINFRIED SEIDL, APRIL 1997
!     MODYFIED BY WINFRIED SEIDL, MARCH 2000
!                    FOR MM5 VERSION 3
!----------------------------------------------------------------------
! .. Scalar Arguments ..
        REAL :: rad, rh, t
        INTEGER :: iland, iseason, numchem
        LOGICAL :: highnh3, rainflag, wetflag
        real :: rcx(numchem)
! ..
        INTEGER :: iprt,lo3,lso2,lnh3
! ..
! .. Local Scalars ..
        REAL :: rclx, rdc, resice, rgsx, rluo1, rluo2, rlux, rmx, rs, rsmx, &
          tc, rdtheta, z
        INTEGER :: n
! ..
! .. Local Arrays ..
        REAL :: hstary(numchem)
! ..
! .. Intrinsic Functions ..
        INTRINSIC exp
! ..
        DO n = 1, numchem
          rcx(n) = 1.
        END DO

        tc = t - 273.15
        rdtheta = 0.

        z = 200./(rad+0.1)

!!!  HARDWIRE VALUES FOR TESTING
!       z=0.4727409
!       tc=22.76083
!       t=tc+273.15
!       rad = 412.8426
!       rainflag=.false.
!       wetflag=.false.

        IF ((tc<=0.) .OR. (tc>=40.)) THEN
          rs = 9999.
        ELSE
          rs = ri(iland,iseason)*(1+z*z)*(400./(tc*(40.-tc)))
        END IF
        rdc = 100*(1.+1000./(rad+10))/(1+1000.*rdtheta)
        rluo1 = 1./(1./3000.+1./3./rlu(iland,iseason))
        rluo2 = 1./(1./1000.+1./3./rlu(iland,iseason))
        resice = 1000.*exp(-tc-4.)

        DO n = 1, numchem
          IF (hstar(n)==0.) GO TO 10
          hstary(n) = hstar(n)*exp(dhr(n)*(1./t-1./298.))
          rmx = 1./(hstary(n)/3000.+100.*f0(n))
          rsmx = rs*dratio(n) + rmx
          rclx = 1./(hstary(n)/1.E+5/rcls(iland,iseason)+f0(n)/rclo(iland, &
            iseason)) + resice
          rgsx = 1./(hstary(n)/1.E+5/rgss(iland,iseason)+f0(n)/rgso(iland, &
            iseason)) + resice
          rlux = rlu(iland,iseason)/(1.E-5*hstary(n)+f0(n)) + resice
          IF (wetflag) THEN
            rlux = 1./(1./3./rlu(iland,iseason)+1.E-7*hstary(n)+f0(n)/rluo1)
          END IF
          IF (rainflag) THEN
            rlux = 1./(1./3./rlu(iland,iseason)+1.E-7*hstary(n)+f0(n)/rluo2)
          END IF
          rcx(n) = 1./(1./rsmx+1./rlux+1./(rdc+rclx)+1./(rac(iland, &
            iseason)+rgsx))
          IF (rcx(n)<1.) rcx(n) = 1.
10      END DO

!     SPECIAL TREATMENT FOR OZONE
        hstary(lo3) = hstar(lo3)*exp(dhr(lo3)*(1./t-1./298.))
        rmx = 1./(hstary(lo3)/3000.+100.*f0(lo3))
        rsmx = rs*dratio(lo3) + rmx
        rlux = rlu(iland,iseason)/(1.E-5*hstary(lo3)+f0(lo3)) + resice
        rclx = rclo(iland,iseason) + resice
        rgsx = rgso(iland,iseason) + resice
        IF (wetflag) rlux = rluo1
        IF (rainflag) rlux = rluo2
        rcx(lo3) = 1./(1./rsmx+1./rlux+1./(rdc+rclx)+1./(rac(iland, &
          iseason)+rgsx))
        IF (rcx(lo3)<1.) rcx(lo3) = 1.

!     SPECIAL TREATMENT FOR SO2 (Wesely)
!       HSTARY(LSO2)=HSTAR(LSO2)*EXP(DHR(LSO2)*(1./T-1./298.))
!       RMX=1./(HSTARY(LSO2)/3000.+100.*F0(LSO2))
!       RSMX=RS*DRATIO(LSO2)+RMX
!       RLUX=RLU(ILAND,ISEASON)/(1.E-5*HSTARY(LSO2)+F0(LSO2))
!    &       +RESICE
!       RCLX=RCLS(ILAND,ISEASON)+RESICE
!       RGSX=RGSS(ILAND,ISEASON)+RESICE
!       IF ((wetflag).OR.(RAINFLAG)) THEN
!         IF (ILAND.EQ.1) THEN
!           RLUX=50.
!         ELSE
!           RLUX=100.
!         END IF
!       END IF
!       RCX(LSO2)=1./(1./RSMX+1./RLUX+1./(RDC+RCLX)
!    &                +1./(RAC(ILAND,ISEASON)+RGSX))
!       IF (RCX(LSO2).LT.1.) RCX(LSO2)=1.

!     SO2 according to Erisman et al. 1994
!       R_STOM
        rsmx = rs*dratio(lso2)
!       R_EXT
        IF (tc>(-1.)) THEN
          IF (rh<81.3) THEN
            rlux = 25000.*exp(-0.0693*rh)
          ELSE
            rlux = 0.58E12*exp(-0.278*rh)
          END IF
        END IF
        IF (((wetflag) .OR. (rainflag)) .AND. (tc>(-1.))) THEN
          rlux = 1.
        END IF
        IF ((tc>=(-5.)) .AND. (tc<=(-1.))) THEN
          rlux = 200.
        END IF
        IF (tc<(-5.)) THEN
          rlux = 500.
        END IF
!       INSTEAD OF R_INC R_CL and R_DC of Wesely are used
        rclx = rcls(iland,iseason)
!       DRY SURFACE
        rgsx = 1000.
!       WET SURFACE
        IF ((wetflag) .OR. (rainflag)) THEN
          IF (highnh3) THEN
            rgsx = 0.
          ELSE
            rgsx = 500.
          END IF
        END IF
!       WATER
        IF (iland==iswater_temp) THEN
          rgsx = 0.
        END IF
!       SNOW
        IF ((iseason==4) .OR. (iland==isice_temp)) THEN
          IF (tc>2.) THEN
            rgsx = 0.
          END IF
          IF ((tc>=(-1.)) .AND. (tc<=2.)) THEN
            rgsx = 70.*(2.-tc)
          END IF
          IF (tc<(-1.)) THEN
            rgsx = 500.
          END IF
        END IF
!       TOTAL SURFACE RESISTENCE
        IF ((iseason/=4) .AND. (ixxxlu(iland)/=1) .AND. (iland/=iswater_temp) .AND. &
            (iland/=isice_temp)) THEN
          rcx(lso2) = 1./(1./rsmx+1./rlux+1./(rclx+rdc+rgsx))
        ELSE
          rcx(lso2) = rgsx
        END IF
        IF (rcx(lso2)<1.) rcx(lso2) = 1.
!     NH3 according to Erisman et al. 1994
!       R_STOM
        rsmx = rs*dratio(lnh3)
!       GRASSLAND (PASTURE DURING GRAZING)
        IF (ixxxlu(iland)==3) THEN
          IF (iseason==1) THEN
!           SUMMER
            rcx(lnh3) = 1000.
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!           WINTER, NO SNOW
            IF (tc>-1.) THEN
              IF (rad/=0.) THEN
                rcx(lnh3) = 50.
              ELSE
                rcx(lnh3) = 100.
              END IF
              IF ((wetflag) .OR. (rainflag)) THEN
                rcx(lnh3) = 20.
              END IF
            END IF
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(lnh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(lnh3) = 500.
            END IF
          END IF
        END IF
!       AGRICULTURAL LAND (CROPS AND UNGRAZED PASTURE)
        IF (ixxxlu(iland)==2) THEN
          IF (iseason==1) THEN
!           SUMMER
            IF (rad/=0.) THEN
              rcx(lnh3) = rsmx
            ELSE
              rcx(lnh3) = 200.
            END IF
            IF ((wetflag) .OR. (rainflag)) THEN
              rcx(lnh3) = 50.
            END IF
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!           WINTER, NO SNOW
            IF (tc>-1.) THEN
              IF (rad/=0.) THEN
                rcx(lnh3) = rsmx
              ELSE
                rcx(lnh3) = 300.
              END IF
              IF ((wetflag) .OR. (rainflag)) THEN
                rcx(lnh3) = 100.
              END IF
            END IF
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(lnh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(lnh3) = 500.
            END IF
          END IF
        END IF
!       SEMI-NATURAL ECOSYSTEMS AND FORESTS
        IF ((ixxxlu(iland)==4) .OR. (ixxxlu(iland)==5) .OR. (ixxxlu( &
            iland)==6)) THEN
          IF (rad/=0.) THEN
            rcx(lnh3) = 500.
          ELSE
            rcx(lnh3) = 1000.
          END IF
          IF ((wetflag) .OR. (rainflag)) THEN
            IF (highnh3) THEN
              rcx(lnh3) = 100.
            ELSE
              rcx(lnh3) = 0.
            END IF
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!           WINTER, NO SNOW
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(lnh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(lnh3) = 500.
            END IF
          END IF
        END IF
!       WATER
        IF (iland==iswater_temp) THEN
          rcx(lnh3) = 0.
        END IF
!       URBAN AND DESERT (SOIL SURFACES)
        IF (ixxxlu(iland)==1) THEN
          IF ( .NOT. wetflag) THEN
            rcx(lnh3) = 50.
          ELSE
            rcx(lnh3) = 0.
          END IF
        END IF
!       SNOW COVERED SURFACES OR PERMANENT ICE
        IF ((iseason==4) .OR. (iland==isice_temp)) THEN
          IF (tc>2.) THEN
            rcx(lnh3) = 0.
          END IF
          IF ((tc>=(-1.)) .AND. (tc<=2.)) THEN
            rcx(lnh3) = 70.*(2.-tc)
          END IF
          IF (tc<(-1.)) THEN
            rcx(lnh3) = 500.
          END IF
        END IF
        IF (rcx(lnh3)<1.) rcx(lnh3) = 1.

      END SUBROUTINE rc
! **********************************************************************
! ************************ SUBROUTINE DEPPART **************************
! **********************************************************************
      SUBROUTINE deppart(rmol,ustar,rh,clw,iland,dvpart,dvfog)
!     THIS SUBROUTINE CALCULATES SURFACE DEPOSITION VELOCITIES
!     FOR FINE AEROSOL PARTICLES ACCORDING TO THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY WINFRIED SEIDL, APRIL 1997
!     MODIFIED BY WINFRIED SEIDL, MARCH 2000
!            FOR MM5 VERSION 3
! ----------------------------------------------------------------------
! .. Scalar Arguments ..
        REAL :: clw, dvfog, dvpart, rh, rmol, ustar
        INTEGER :: iland
! ..
! .. Intrinsic Functions ..
        INTRINSIC exp
! ..
        dvpart = ustar/kpart(iland)
        IF (rmol<0.) THEN
!         INSTABLE LAYERING CORRECTION
          dvpart = dvpart*(1.+(-300.*rmol)**0.66667)
        END IF
        IF (rh>80.) THEN
!         HIGH RELATIVE HUMIDITY CORRECTION
!         ACCORDING TO J. W. ERISMAN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 321-332
          dvpart = dvpart*(1.+0.37*exp((rh-80.)/20.))
        END IF

!       SEDIMENTATION VELOCITY OF FOG WATER ACCORDING TO
!       R. FORKEL, W. SEIDL, R. DLUGI AND E. DEIGELE
!       J. GEOPHYS. RES. 95D (1990), 18501-18515
        dvfog = 0.06*clw
        IF (ixxxlu(iland)==5) THEN
!         TURBULENT DEPOSITION OF FOG WATER IN CONIFEROUS FOREST ACCORDI
!         A. T. VERMEULEN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 375-386
          dvfog = dvfog + 0.195*ustar*ustar
        END IF

      END SUBROUTINE deppart
      SUBROUTINE landusevg(vgs,ustar,rmol,z0,zz,dvparx,iland,numchem, &
          srfres,aer_res,lsulf)
!     This subroutine calculates the species specific deposition velocit
!     as a function of the local meteorology and land use.  The depositi
!     Velocity is also landuse specific.
!     Reference: Hsieh, C.M., Wesely, M.L. and Walcek, C.J. (1986)
!                A Dry Deposition Module for Regional Acid Deposition
!                EPA report under agreement DW89930060-01
!     Revised version by Darrell Winner (January 1991)
!        Environmental Engineering Science 138-78
!           California Institute of Technology
!              Pasadena, CA  91125
!     Modified by Winfried Seidl (August 1997)
!       Fraunhofer-Institut fuer Atmosphaerische Umweltforschung
!                    Garmisch-Partenkirchen, D-82467
!          for use of Wesely and Erisman surface resistances
!     Inputs:
!        Ustar  : The grid average friction velocity (m/s)
!        Rmol   : Reciprocal of the Monin-Obukhov length (1/m)
!        Z0     : Surface roughness height for the grid square (m)
!        SrfRes : Array of landuse/atmospheric/species resistances (s/m)
!        Slist  : Array of chemical species codes
!        Dvparx : Array of surface deposition velocity of fine aerosol p
!     Outputs:
!        Vgs    : Array of species and landuse specific deposition
!                 velocities (m/s)
!        Vg     : Cell-average deposition velocity by species (m/s)
!     Variables used:
!        SCPR23  : (Schmidt #/Prandtl #)**(2/3) Diffusion correction fac
!        Zr      : Reference Height (m)
!        Iatmo   : Parameter specifying the stabilty class (Function of
!        Z0      : Surface roughness height (m)
!        Vk      : Von Karman''s constant
!     Local Variables
! .. Scalar Arguments ..
        REAL :: dvparx, rmol, ustar, z0, zz
        real :: aer_res
        INTEGER :: iland, numchem, lsulf
! ..
! .. Array Arguments ..
        REAL :: srfres(numchem), vgs(numchem)
! ..
! .. Local Scalars ..
        REAL :: vgp, vgpart, zr
        INTEGER :: jspec
! ..
! .. Local Arrays ..
        REAL :: vgspec(numchem)
! ..
!   Set the reference height (10.0 m)
        zr = 10.0

!   CALCULATE THE DEPOSITION VELOCITY without any surface
!   resistance term, i.e. 1 / (ra + rb)

        CALL depvel(numchem,rmol,zr,z0,ustar,vgspec,vgpart,aer_res)

!   Calculate the deposition velocity for each species
!   and grid cell by looping through all the possibile combinations
!   of the two

        vgp = 1.0/((1.0/vgpart)+(1.0/dvparx))

!   Loop through the various species

        DO jspec = 1, numchem

!   Add in the surface resistance term, rc (SrfRes)

          vgs(jspec) = 1.0/(1.0/vgspec(jspec)+srfres(jspec))
        END DO
        vgs(lsulf) = vgp

        CALL cellvg(vgs,ustar,zz,zr,rmol,numchem)

        RETURN
      END SUBROUTINE landusevg

      SUBROUTINE cellvg(vgtemp,ustar,dz,zr,rmol,nspec)
!     THIS PROGRAM HAS BEEN DESIGNED TO CALCULATE THE CELL AVERAGE
!     DEPOSITION VELOCITY GIVEN THE VALUE OF VG AT SOME REFERENCE
!     HEIGHT ZR WHICH IS MUCH SMALLER THAN THE CELL HEIGHT DZ.
!       PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner    (February 1991)
!.....PROGRAM VARIABLES...
!     VgTemp   - DEPOSITION VELOCITY AT THE REFERENCE HEIGHT
!     USTAR    - FRICTION VELOCITY
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     DZ       - CELL HEIGHT
!     CELLVG   - CELL AVERAGE DEPOSITION VELOCITY
!     VK       - VON KARMAN CONSTANT
!     Local Variables
! .. Scalar Arguments ..
        REAL :: dz, rmol, ustar, zr
        INTEGER :: nspec
! ..
! .. Array Arguments ..
        REAL :: vgtemp(nspec)
! ..
! .. Local Scalars ..
        REAL :: a, fac, pdz, pzr, vk
        INTEGER :: nss
! ..
! .. Intrinsic Functions ..
        INTRINSIC alog, sqrt
! ..
!     Set the von Karman constant
        vk = 0.4

!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE


        DO nss = 1, nspec
          IF (rmol<0) THEN
            pdz = sqrt(1.0-9.0*dz*rmol)
            pzr = sqrt(1.0-9.0*zr*rmol)
            fac = ((pdz-1.0)/(pzr-1.0))*((pzr+1.0)/(pdz+1.0))
            a = 0.74*dz*alog(fac) + (0.164/rmol)*(pdz-pzr)
          ELSE IF (rmol==0) THEN
            a = 0.74*(dz*alog(dz/zr)-dz+zr)
          ELSE
            a = 0.74*(dz*alog(dz/zr)-dz+zr) + (2.35*rmol)*(dz-zr)**2
          END IF

!     CALCULATE THE DEPOSITION VELOCITIY

          vgtemp(nss) = vgtemp(nss)/(1.0+vgtemp(nss)*a/(vk*ustar*(dz-zr)))
        END DO

        RETURN
      END SUBROUTINE cellvg
      SUBROUTINE depvel(numchem,rmol,zr,z0,ustar,depv,vgpart,aer_res)
!     THIS FUNCTION HAS BEEN DESIGNED TO EVALUATE AN UPPER LIMIT
!     FOR THE POLLUTANT DEPOSITION VELOCITY AS A FUNCTION OF THE
!     SURFACE ROUGHNESS AND METEOROLOGICAL CONDITIONS.
!     PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner  (Feb. 1991)
!                  by Winfried Seidl     (Aug. 1997)
!.....PROGRAM VARIABLES...
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     Z0       - SURFACE ROUGHNESS HEIGHT
!     SCPR23   - (Schmidt #/Prandtl #)**(2/3) Diffusion correction fact
!     UBAR     - ABSOLUTE VALUE OF SURFACE WIND SPEED
!     DEPVEL   - POLLUTANT DEPOSITION VELOCITY
!     Vk       - VON KARMAN CONSTANT
!     USTAR    - FRICTION VELOCITY U*
!     POLINT   - POLLUTANT INTEGRAL
!.....REFERENCES...
!     MCRAE, G.J. ET AL. (1983) MATHEMATICAL MODELING OF PHOTOCHEMICAL
!       AIR POLLUTION, ENVIRONMENTAL QUALITY LABORATORY REPORT 18,
!       CALIFORNIA INSTITUTE OF TECHNOLOGY, PASADENA, CALIFORNIA.
!.....RESTRICTIONS...
!     1. THE MODEL EDDY DIFFUSIVITIES ARE BASED ON MONIN-OBUKHOV
!        SIMILARITY THEORY AND SO ARE ONLY APPLICABLE IN THE
!        SURFACE LAYER, A HEIGHT OF O(30M).
!     2. ALL INPUT UNITS MUST BE CONSISTENT
!     3. THE PHI FUNCTIONS USED TO CALCULATE THE FRICTION
!        VELOCITY U* AND THE POLLUTANT INTEGRALS ARE BASED
!        ON THE WORK OF BUSINGER ET AL.(1971).
!     4. THE MOMENTUM AND POLLUTANT DIFFUSIVITIES ARE NOT
!        THE SAME FOR THE CASES L<0 AND L>0.
!     Local Variables
! .. Scalar Arguments ..
        REAL :: rmol, ustar, vgpart, z0, zr
        INTEGER :: numchem
! ..
! .. Array Arguments ..
        REAL :: depv(numchem)
! ..
! .. Local Scalars ..
        REAL :: ao, ar, polint, vk, aer_res
        INTEGER :: l
! ..
! .. Intrinsic Functions ..
        INTRINSIC alog
! ..
!     Set the von Karman constant
        vk = 0.4

!     Calculate the diffusion correction factor
!     SCPR23 is calculated as (Sc/Pr)**(2/3) using Sc= 1.15 and Pr= 1.0
!     SCPR23 = 1.10

!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE

        IF (rmol<0) THEN
          ar = ((1.0-9.0*zr*rmol)**(0.25)+0.001)**2
          ao = ((1.0-9.0*z0*rmol)**(0.25)+0.001)**2
          polint = 0.74*(alog((ar-1.0)/(ar+1.0))-alog((ao-1.0)/(ao+1.0)))
        ELSE IF (rmol==0) THEN
          polint = 0.74*alog(zr/z0)
        ELSE
          polint = 0.74*alog(zr/z0) + 4.7*rmol*(zr-z0)
        END IF

!     CALCULATE THE Maximum DEPOSITION VELOCITY

        DO l = 1, numchem
          depv(l) = ustar*vk/(2.0*scpr23(l)+polint)
        END DO
        vgpart = ustar*vk/polint
        aer_res=polint/(ustar*vk)

        RETURN
      END SUBROUTINE depvel

      SUBROUTINE dep_init(id,config_flags)
  USE module_model_constants
  USE module_configure
  USE module_state_description                       
   integer, intent(in) :: id
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
! ..
! .. Scalar Arguments ..
        INTEGER :: numchem
!       parameter (numchem=p_ho2-1)
! ..
! .. Local Scalars ..
        REAL :: sc
        INTEGER :: iland, iseason, l
        integer :: iprt
! ..
! .. Local Arrays ..
        REAL :: dat1(nlu,dep_seasons), dat2(nlu,dep_seasons),         &
                dat3(nlu,dep_seasons), dat4(nlu,dep_seasons),         &
                dat5(nlu,dep_seasons), dat6(nlu,dep_seasons),         &
                dat7(nlu,dep_seasons), dvj(p_ho2)
        numchem=p_ho2
! ..
! .. Data Statements ..
!     RI for stomatal resistance
!      data ((ri(ILAND,ISEASON),ILAND=1,nlu),ISEASON=1,dep_seasons)/0.10E+11, &
        DATA ((dat1(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.60E+02, 0.60E+02, 0.60E+02, 0.60E+02, 0.70E+02, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.12E+03, 0.70E+02, 0.13E+03, 0.70E+02, &
          0.13E+03, 0.10E+03, 0.10E+11, 0.80E+02, 0.10E+03, 0.10E+11, &
          0.80E+02, 0.10E+03, 0.10E+03, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, 0.10E+11, &
          0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, &
          0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, &
          0.10E+11, 0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.70E+02, 0.40E+03, 0.80E+03, 0.10E+11, &
          0.10E+11, 0.80E+03, 0.10E+11, 0.10E+11, 0.80E+03, 0.80E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.14E+03, 0.24E+03, 0.24E+03, 0.24E+03, &
          0.12E+03, 0.14E+03, 0.25E+03, 0.70E+02, 0.25E+03, 0.19E+03, &
          0.10E+11, 0.16E+03, 0.19E+03, 0.10E+11, 0.16E+03, 0.19E+03, &
          0.19E+03, 0.10E+11, 0.10E+11, 0.10E+11/
! ..
        IF (nlu/=25) THEN
          PRINT *, 'number of land use classifications not correct '
          CALL wrf_error_fatal ( "LAND USE CLASSIFICATIONS NOT 25")
        END IF
        IF (dep_seasons/=5) THEN
          PRINT *, 'number of dep_seasons not correct '
          CALL wrf_error_fatal ( "DEP_SEASONS NOT 5")
        END IF

!     SURFACE RESISTANCE DATA FOR DEPOSITION MODEL OF
!     M. L. WESELY, ATMOSPHERIC ENVIRONMENT 23 (1989) 1293-1304

!     Seasonal categories:
!     1: midsummer with lush vegetation
!     2: autumn with unharvested cropland
!     3: late autumn with frost, no snow
!     4: winter, snow on ground and subfreezing
!     5: transitional spring with partially green short annuals

!     Land use types:
!     USGS type                                Wesely type
!      1: Urban and built-up land              1
!      2: Dryland cropland and pasture         2
!      3: Irrigated cropland and pasture       2
!      4: Mix. dry/irrg. cropland and pasture  2
!      5: Cropland/grassland mosaic            2
!      6: Cropland/woodland mosaic             4
!      7: Grassland                            3
!      8: Shrubland                            3
!      9: Mixed shrubland/grassland            3
!     10: Savanna                              3, always summer
!     11: Deciduous broadleaf forest           4
!     12: Deciduous needleleaf forest          5, autumn and winter modi
!     13: Evergreen broadleaf forest           4, always summer
!     14: Evergreen needleleaf forest          5
!     15: Mixed Forest                         6
!     16: Water Bodies                         7
!     17: Herbaceous wetland                   9
!     18: Wooded wetland                       6
!     19: Barren or sparsely vegetated         8
!     20: Herbaceous Tundra                    9
!     21: Wooded Tundra                        6
!     22: Mixed Tundra                         6
!     23: Bare Ground Tundra                   8
!     24: Snow or Ice                          -, always winter
!     25: No data                              8


!     Order of data:
!      |
!      |   seasonal category
!     \|/
!     ---> landuse type
!     1       2       3       4       5       6       7       8       9
!     RLU for outer surfaces in the upper canopy
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            ri(iland,iseason) = dat1(iland,iseason)
          END DO
        END DO
!      data ((rlu(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat2(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, &
          0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, &
          0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.20E+04, 0.60E+04, 0.90E+04, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rlu(iland,iseason) = dat2(iland,iseason)
          END DO
        END DO
!     RAC for transfer that depends on canopy height and density
!      data ((rac(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+03, &
        DATA ((dat3(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+04, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.00E+00, 0.30E+03, 0.20E+04, 0.00E+00, &
          0.30E+03, 0.20E+04, 0.20E+04, 0.00E+00, 0.00E+00, 0.00E+00, &
          0.10E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+04, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.15E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.17E+04, 0.00E+00, 0.20E+03, 0.17E+04, &
          0.00E+00, 0.20E+03, 0.17E+04, 0.17E+04, 0.00E+00, 0.00E+00, &
          0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, 0.10E+03, &
          0.15E+04, 0.00E+00, 0.10E+03, 0.15E+04, 0.15E+04, 0.00E+00, &
          0.00E+00, 0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+02, 0.10E+04, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, &
          0.50E+02, 0.15E+04, 0.00E+00, 0.50E+02, 0.15E+04, 0.15E+04, &
          0.00E+00, 0.00E+00, 0.00E+00, 0.10E+03, 0.50E+02, 0.50E+02, &
          0.50E+02, 0.50E+02, 0.12E+04, 0.80E+02, 0.80E+02, 0.80E+02, &
          0.10E+03, 0.12E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, &
          0.00E+00, 0.20E+03, 0.15E+04, 0.00E+00, 0.20E+03, 0.15E+04, &
          0.15E+04, 0.00E+00, 0.00E+00, 0.00E+00/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rac(iland,iseason) = dat3(iland,iseason)
          END DO
        END DO
!     RGSS for ground surface  SO2
!      data ((rgss(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.40E+03, &
        DATA ((dat4(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.40E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, 0.10E+04, &
          0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+04, &
          0.40E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.50E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, &
          0.10E+04, 0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, &
          0.10E+04, 0.40E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, 0.10E+01, 0.10E+01, &
          0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, 0.20E+03, 0.10E+04, &
          0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.50E+03, 0.10E+03, 0.10E+03, 0.10E+01, &
          0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+04, 0.10E+03, 0.10E+04, 0.50E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, &
          0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, &
          0.10E+01, 0.10E+01, 0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, &
          0.20E+03, 0.10E+04, 0.10E+03, 0.10E+04/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rgss(iland,iseason) = dat4(iland,iseason)
          END DO
        END DO
!     RGSO for ground surface  O3
!      data ((rgso(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.30E+03, &
        DATA ((dat5(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.30E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, &
          0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03, &
          0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.80E+03, 0.30E+03, &
          0.40E+03, 0.80E+03, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, &
          0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, &
          0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, &
          0.35E+04, 0.40E+03, 0.60E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.20E+03, 0.35E+04, 0.35E+04, 0.20E+04, &
          0.35E+04, 0.35E+04, 0.40E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.40E+03, 0.35E+04, 0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, &
          0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, &
          0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rgso(iland,iseason) = dat5(iland,iseason)
          END DO
        END DO
!     RCLS for exposed surfaces in the lower canopy  SO2
!      data ((rcls(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat6(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.20E+04, 0.40E+04, 0.10E+11, 0.90E+04, 0.40E+04, &
          0.10E+11, 0.90E+04, 0.40E+04, 0.40E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.30E+04, 0.60E+04, 0.10E+11, 0.90E+04, &
          0.60E+04, 0.10E+11, 0.90E+04, 0.60E+04, 0.60E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.20E+04, 0.20E+03, 0.40E+03, 0.10E+11, &
          0.90E+04, 0.40E+03, 0.10E+11, 0.90E+04, 0.40E+03, 0.40E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rcls(iland,iseason) = dat6(iland,iseason)
          END DO
        END DO
!     RCLO for exposed surfaces in the lower canopy  O3
!      data ((rclo(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat7(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, 0.40E+03, &
          0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.40E+03, 0.60E+03, &
          0.10E+11, 0.40E+03, 0.60E+03, 0.60E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, &
          0.40E+03, 0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.80E+03, &
          0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.40E+03, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.10E+04, 0.15E+04, 0.60E+03, 0.10E+11, &
          0.80E+03, 0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.10E+04, 0.50E+03, 0.15E+04, 0.10E+04, 0.15E+04, 0.70E+03, &
          0.10E+11, 0.60E+03, 0.70E+03, 0.10E+11, 0.60E+03, 0.70E+03, &
          0.70E+03, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
          DO iland = 1, nlu
            rclo(iland,iseason) = dat7(iland,iseason)
          END DO
        END DO
        DO l = 1, numchem
          hstar(l) = 0.
          hstar4(l) = 0.
          dhr(l) = 0.
          f0(l) = 0.
          dvj(l) = 99.
        END DO

!     HENRY''S LAW COEFFICIENTS
!     Effective Henry''s law coefficient at pH 7
!     [KH298]=mole/(l atm)

        hstar(p_no2) = 6.40E-3
        hstar(p_no) = 1.90E-3
        hstar(p_pan) = 2.97E+0
        hstar(p_o3) = 1.13E-2
        hstar(p_hcho) = 2.97E+3
        hstar(p_aco3) = 1.14E+1
        hstar(p_tpan) = 2.97E+0
        hstar(p_hono) = 3.47E+5
        hstar(p_no3) = 1.50E+1
        hstar(p_hno4) = 2.00E+13
        hstar(p_h2o2) = 7.45E+4
        hstar(p_co) = 8.20E-3
        hstar(p_ald) = 1.14E+1
        hstar(p_op1) = 2.21E+2
        hstar(p_op2) = 1.68E+6
        hstar(p_paa) = 4.73E+2
        hstar(p_ket) = 3.30E+1
        hstar(p_gly) = 1.40E+6
        hstar(p_mgly) = 3.71E+3
        hstar(p_dcb) = 1.40E+6
        hstar(p_onit) = 1.13E+0
        hstar(p_so2) = 2.53E+5
        hstar(p_eth) = 2.00E-3
        hstar(p_hc3) = 1.42E-3
        hstar(p_hc5) = 1.13E-3
        hstar(p_hc8) = 1.42E-3
        hstar(p_olt) = 4.76E-3
        hstar(p_oli) = 1.35E-3
        hstar(p_tol) = 1.51E-1
        hstar(p_csl) = 4.00E+5
        hstar(p_xyl) = 1.45E-1
        hstar(p_iso) = 4.76E-3
        hstar(p_hno3) = 2.69E+13
        hstar(p_ora1) = 9.85E+6
        hstar(p_ora2) = 9.63E+5
        hstar(p_nh3) = 1.04E+4
        hstar(p_n2o5) = 1.00E+10
        if(p_ol2.gt.1)hstar(p_ol2) = 4.67E-3
!
!  FOLLOWING FOR RACM
!
        if(p_ete.gt.1)then
           HSTAR(p_ETE )=4.67E-3
           HSTAR(p_API )=4.76E-3
           HSTAR(p_LIM )=4.76E-3
           HSTAR(p_DIEN)=4.76E-3
           HSTAR(p_CH4 )=1.50E-3
           HSTAR(p_CO2 )=1.86E-1
           HSTAR(p_MACR)=1.14E+1
           HSTAR(p_UDD )=1.40E+6
           HSTAR(p_HKET)=7.80E+3
           DHR(p_ETE )=    0.
           DHR(p_API )=    0.
           DHR(p_LIM )=    0.
           DHR(p_DIEN)=    0.
           DHR(p_CH4 )=    0.
           DHR(p_CO2 )= 1636.
           DHR(p_MACR)= 6266.
           DHR(p_UDD )=    0.
           DHR(p_HKET)=    0.
           F0(p_ETE )=0.
           F0(p_API )=0.
           F0(p_LIM )=0.
           F0(p_DIEN)=0.
           F0(p_CH4 )=0.
           F0(p_CO2 )=0.
           F0(p_MACR)=0.
           F0(p_UDD )=0.
           F0(p_HKET)=0.
           DVJ(p_ETE )=0.189
           DVJ(p_API )=0.086
           DVJ(p_LIM )=0.086
           DVJ(p_DIEN)=0.136
           DVJ(p_CH4 )=0.250
           DVJ(p_CO2 )=0.151
           DVJ(p_MACR)=0.120
           DVJ(p_UDD )=0.092
           DVJ(p_HKET)=0.116
        endif
!     -DH/R (for temperature correction)
!     [-DH/R]=K

        dhr(p_no2) = 2500.
        dhr(p_no) = 1480.
        dhr(p_pan) = 5760.
        dhr(p_o3) = 2300.
        dhr(p_hcho) = 7190.
        dhr(p_aco3) = 6266.
        dhr(p_tpan) = 5760.
        dhr(p_hono) = 3775.
        dhr(p_no3) = 0.
        dhr(p_hno4) = 0.
        dhr(p_h2o2) = 6615.
        dhr(p_co) = 0.
        dhr(p_ald) = 6266.
        dhr(p_op1) = 5607.
        dhr(p_op2) = 10240.
        dhr(p_paa) = 6170.
        dhr(p_ket) = 5773.
        dhr(p_gly) = 0.
        dhr(p_mgly) = 7541.
        dhr(p_dcb) = 0.
        dhr(p_onit) = 5487.
        dhr(p_so2) = 5816.
        dhr(p_eth) = 0.
        dhr(p_hc3) = 0.
        dhr(p_hc5) = 0.
        dhr(p_hc8) = 0.
        dhr(p_olt) = 0.
        dhr(p_oli) = 0.
        dhr(p_tol) = 0.
        dhr(p_csl) = 0.
        dhr(p_xyl) = 0.
        dhr(p_iso) = 0.
        dhr(p_hno3) = 8684.
        dhr(p_ora1) = 5716.
        dhr(p_ora2) = 8374.
        dhr(p_nh3) = 3660.
        dhr(p_n2o5) = 0.
        if(p_ol2.gt.1)dhr(p_ol2) = 0.
!     REACTIVITY FACTORS
!     [f0]=1

        f0(p_no2) = 0.1
        f0(p_no) = 0.
        f0(p_pan) = 0.1
        f0(p_o3) = 1.
        f0(p_hcho) = 0.
        f0(p_aco3) = 1.
        f0(p_tpan) = 0.1
        f0(p_hono) = 0.1
        f0(p_no3) = 1.
        f0(p_hno4) = 0.1
        f0(p_h2o2) = 1.
        f0(p_co) = 0.
        f0(p_ald) = 0.
        f0(p_op1) = 0.1
        f0(p_op2) = 0.1
        f0(p_paa) = 0.1
        f0(p_ket) = 0.
        f0(p_gly) = 0.
        f0(p_mgly) = 0.
        f0(p_dcb) = 0.
        f0(p_onit) = 0.
        f0(p_so2) = 0.
        f0(p_eth) = 0.
        f0(p_hc3) = 0.
        f0(p_hc5) = 0.
        f0(p_hc8) = 0.
        f0(p_olt) = 0.
        f0(p_oli) = 0.
        f0(p_tol) = 0.
        f0(p_csl) = 0.
        f0(p_xyl) = 0.
        f0(p_iso) = 0.
        f0(p_hno3) = 0.
        f0(p_ora1) = 0.
        f0(p_ora2) = 0.
        f0(p_nh3) = 0.
        f0(p_n2o5) = 1.
        if(p_ol2.gt.1)f0(p_ol2) = 0.
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)

        dvj(p_no2) = 0.147
        dvj(p_no) = 0.183
        dvj(p_pan) = 0.091
        dvj(p_o3) = 0.175
        dvj(p_hcho) = 0.183
        dvj(p_aco3) = 0.115
        dvj(p_tpan) = 0.082
        dvj(p_hono) = 0.153
        dvj(p_no3) = 0.127
        dvj(p_hno4) = 0.113
        dvj(p_h2o2) = 0.171
        dvj(p_co) = 0.189
        dvj(p_ald) = 0.151
        dvj(p_op1) = 0.144
        dvj(p_op2) = 0.127
        dvj(p_paa) = 0.115
        dvj(p_ket) = 0.118
        dvj(p_gly) = 0.131
        dvj(p_mgly) = 0.118
        dvj(p_dcb) = 0.107
        dvj(p_onit) = 0.092
        dvj(p_so2) = 0.126
        dvj(p_eth) = 0.183
        dvj(p_hc3) = 0.151
        dvj(p_hc5) = 0.118
        dvj(p_hc8) = 0.094
        dvj(p_olt) = 0.154
        dvj(p_oli) = 0.121
        dvj(p_tol) = 0.104
        dvj(p_csl) = 0.096
        dvj(p_xyl) = 0.097
        dvj(p_iso) = 0.121
        dvj(p_hno3) = 0.126
        dvj(p_ora1) = 0.153
        dvj(p_ora2) = 0.124
        dvj(p_nh3) = 0.227
        dvj(p_n2o5) = 0.110
        dvj(p_ho) = 0.243
        dvj(p_ho2) = 0.174
        if(p_ol2.gt.1)dvj(p_ol2) = 0.189
        DO l = 1, numchem
          hstar4(l) = hstar(l) ! preliminary              
! Correction of diff. coeff
          dvj(l) = dvj(l)*(293.15/298.15)**1.75
          sc = 0.15/dvj(l) ! Schmidt Number at 20°C   
          dratio(l) = 0.242/dvj(l) !                                            ! of water vapor and gas at
! Ratio of diffusion coeffi
          scpr23(l) = (sc/0.72)**(2./3.) ! (Schmidt # / Prandtl #)**
        END DO



!     DATA FOR AEROSOL PARTICLE DEPOSITION FOR THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL AND P. WYERS
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607

!     vd = (u* / k) * CORRECTION FACTORS

!     CONSTANT K FOR LANDUSE TYPES:
! urban and built-up land                  
        kpart(1) = 500.
! dryland cropland and pasture             
        kpart(2) = 500.
! irrigated cropland and pasture           
        kpart(3) = 500.
! mixed dryland/irrigated cropland and past
        kpart(4) = 500.
! cropland/grassland mosaic                
        kpart(5) = 500.
! cropland/woodland mosaic                 
        kpart(6) = 100.
! grassland                                
        kpart(7) = 500.
! shrubland                                
        kpart(8) = 500.
! mixed shrubland/grassland                
        kpart(9) = 500.
! savanna                                  
        kpart(10) = 500.
! deciduous broadleaf forest               
        kpart(11) = 100.
! deciduous needleleaf forest              
        kpart(12) = 100.
! evergreen broadleaf forest               
        kpart(13) = 100.
! evergreen needleleaf forest              
        kpart(14) = 100.
! mixed forest                             
        kpart(15) = 100.
! water bodies                             
        kpart(16) = 500.
! herbaceous wetland                       
        kpart(17) = 500.
! wooded wetland                           
        kpart(18) = 500.
! barren or sparsely vegetated             
        kpart(19) = 500.
! herbaceous tundra                        
        kpart(20) = 500.
! wooded tundra                            
        kpart(21) = 100.
! mixed tundra                             
        kpart(22) = 500.
! bare ground tundra                       
        kpart(23) = 500.
! snow or ice                              
        kpart(24) = 500.
!     Comments:
        kpart(25) = 500.
!     Erisman et al. (1994) give
!     k = 500 for low vegetation and k = 100 for forests.

!     For desert k = 500 is taken according to measurements
!     on bare soil by
!     J. Fontan, A. Lopez, E. Lamaud and A. Druilhet (1997)
!     "Vertical Flux Measurements of the Submicronic Aerosol Particles
!     and Parametrisation of the Dry Deposition Velocity"
!     in: "Biosphere-Atmosphere Exchange of Pollutants
!     and Trace Substances"
!     Editor: S. Slanina. Springer-Verlag Berlin, Heidelberg, 1997
!     pp. 381-390

!     For coniferous forest the Erisman value of  k = 100 is taken.
!     Measurements of Erisman et al. (1997) in a coniferous forest
!     in the Netherlands, lead to values of k between 20 and 38
!     (Atmospheric Environment 31 (1997), 321-332).
!     However, these high values of vd may be reached during
!     instable cases. The eddy correlation measurements
!     of Gallagher et al. (1997) made during the same experiment
!     show for stable cases (L>0) values of k between 200 and 250
!     at minimum (Atmospheric Environment 31 (1997), 359-373).
!     Fontan et al. (1997) found k = 250 in a forest
!     of maritime pine in southwestern France.

!     For gras, model calculations of Davidson et al. support
!     the value of 500.
!     C. I. Davidson, J. M. Miller and M. A. Pleskov
!     "The Influence of Surface Structure on Predicted Particles
!     Dry Deposition to Natural Gras Canopies"
!     Water, Air, and Soil Pollution 18 (1982) 25-43

!     Snow covered surface: The experiment of Ibrahim et al. (1983)
!     gives k = 436 for 0.7 um diameter particles.
!     The deposition velocity of Milford and Davidson (1987)
!     gives k = 154 for continental sulfate aerosol.
!     M. Ibrahim, L. A. Barrie and F. Fanaki
!     Atmospheric Environment 17 (1983), 781-788

!     J. B. Milford and C. I. Davidson
!     "The Sizes of Particulate Sulfate and Nitrate in the Atmosphere
!     - A Review"
!     JAPCA 37 (1987), 125-134
! no data                                  
!       WRITE (0,*) ' return from rcread '
!     *********************************************************

!     Simplified landuse scheme for deposition and biogenic emission
!     subroutines
!     (ISWATER and ISICE are already defined elsewhere,
!     therefore water and ice are not considered here)

!     1 urban or bare soil
!     2 agricultural
!     3 grassland
!     4 deciduous forest
!     5 coniferous and mixed forest
!     6 other natural landuse categories


        IF (mminlu=='OLD ') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 3
          ixxxlu(4) = 4
          ixxxlu(5) = 5
          ixxxlu(6) = 5
          ixxxlu(7) = 0
          ixxxlu(8) = 6
          ixxxlu(9) = 1
          ixxxlu(10) = 6
          ixxxlu(11) = 0
          ixxxlu(12) = 4
          ixxxlu(13) = 6
        END IF
        IF (mminlu=='USGS') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 2
          ixxxlu(4) = 2
          ixxxlu(5) = 2
          ixxxlu(6) = 4
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 3
          ixxxlu(10) = 6
          ixxxlu(11) = 4
          ixxxlu(12) = 5
          ixxxlu(13) = 4
          ixxxlu(14) = 5
          ixxxlu(15) = 5
          ixxxlu(16) = 0
          ixxxlu(17) = 6
          ixxxlu(18) = 4
          ixxxlu(19) = 1
          ixxxlu(20) = 6
          ixxxlu(21) = 4
          ixxxlu(22) = 6
          ixxxlu(23) = 1
          ixxxlu(24) = 0
          ixxxlu(25) = 1
        END IF
        IF (mminlu=='SiB ') THEN
          ixxxlu(1) = 4
          ixxxlu(2) = 4
          ixxxlu(3) = 4
          ixxxlu(4) = 5
          ixxxlu(5) = 5
          ixxxlu(6) = 6
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 6
          ixxxlu(10) = 6
          ixxxlu(11) = 1
          ixxxlu(12) = 2
          ixxxlu(13) = 6
          ixxxlu(14) = 1
          ixxxlu(15) = 0
          ixxxlu(16) = 0
          ixxxlu(17) = 1
        END IF
        print *,'in dep_init, p_ho2 = ',p_ho2
        RETURN
      END SUBROUTINE dep_init
    END MODULE module_dep_simple
