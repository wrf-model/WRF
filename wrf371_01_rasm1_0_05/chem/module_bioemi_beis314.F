MODULE module_bioemi_beis314

!
!  BEIS3.14 Modified to include sesquiterpenes and MBO
!  BEIS3.13 Emissions Module for WRF-Chem
!  BEIS3.11 Written by Greg Frost 6/2004
!
!  Modified to BEIS3.14 by Stu McKeen 11/9/09 - addition of sesquiterpene total emissions
!  Modified to BEIS3.13 by Stu McKeen 9/27/07
! Note: species to vegetation assignment table (beis3_efac_v0.99_053105.txt) must be
! applied to front end for BEIS3.13 reference emission files - done outside of WRF-Chem
! BEIS3.13 also recommends 10meter or 2m air temp. for isoprene emissions, (no guidance)
! See: Changes to the Biogenic Emissions Inventory System Version 3 (BEIS3)
! Donna Schwede, George Pouliot, and Tom Pierce, presented at 2005 CMAS conference:
! available on the web at http://www/cmascenter.org/conference/2005/archive.cfm
!
!  Using off-line gridded standard biogenic emissions
!  for each model compound with such emissions,
!  model shortwave solar flux (isoprene only),
!  & air temperature, pressure, and density in lowest model level, 
!  calculates actual biogenic emissions of each compound.
!  Based on hrbeis311.f from BEIS3.11 for SMOKE, with major
!  surgery performed on original routines for use with WRF-Chem.
!  This version assumes chemical mechanism is RACM.
!  The following 16 RACM compounds have biogenic emissions:
!  iso, no, oli, api, lim, xyl, hc3, ete, olt, ket, ald, hcho, eth, ora2, co, nr
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12

    CONTAINS
      SUBROUTINE bio_emissions_beis314(id,config_flags,ktau,curr_secs, &
               dtstep,julday,gmt,xlat,xlong,t_phy,p_phy,gsw,           &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               sebio_sesq,sebio_mbo,                                   & 
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,   &
               ebio_sesq,ebio_mbo,                                     & ! sesq and mbo are added
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )

  USE module_configure
  USE module_state_description

      IMPLICIT NONE

! .. Parameters ..
      TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

! .. Indices ..
      INTEGER,   INTENT(IN   ) :: id,                                  &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
! .. Passed variables ..
      INTEGER, INTENT (IN)  ::    ktau,                                & ! time step number
                                  julday                                 ! current simulation julian day

      REAL(KIND=8), INTENT(IN) :: curr_secs                              ! seconds into the simulation

      REAL, INTENT (IN)   ::      gmt,dtstep

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
          INTENT(IN   ) ::                                             &
                                  t_phy,                               & !air T (K)
                                  p_phy                                  !P (Pa)

      REAL,  DIMENSION( ims:ime , jms:jme ),                           &
          INTENT(IN   ) ::                                             &
                                  xlat,                                & !latitude (deg)
                                  xlong,                               & !longitude (deg)
                                  gsw                                    !downward shortwave surface flux (W/m^2)

! Normalized biogenic emissions for standard conditions (moles compound/km^2/hr)
      REAL,  DIMENSION( ims:ime , jms:jme ),                           &
          INTENT(IN   ) ::                                             &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               sebio_sesq,sebio_mbo,                                   &
               noag_grow,noag_nongrow,nononag

! Leaf area index for isoprene
      REAL,  DIMENSION( ims:ime , jms:jme ),                           &
          INTENT(IN   ) ::        slai 

! Actual biogenic emissions (moles compound/km^2/hr)
      REAL,  DIMENSION( ims:ime , jms:jme ),                           &
          INTENT(INOUT  ) ::                                           &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,   &
               ebio_sesq,ebio_mbo   

! .. Local Scalars .. 

      INTEGER :: i,j

! Variables for 1 element of I/O arrays 
!   met and phys input variables
      REAL  ::  tair      ! surface air temperature (K)
      REAL  ::  tsolar    ! downward shortwave surface flux (W/m^2)
      REAL  ::  pres      ! surface pressure (mb)
      REAL  ::  ylat      ! latitude (deg) 
      REAL  ::  ylong     ! longitude (deg) 
!   normalized emissions (moles compound/km^2/hr)
      REAL  :: se_iso,se_oli,se_api,se_lim,se_xyl,      &
               se_hc3,se_ete,se_olt,se_ket,se_ald,      &
               se_hcho,se_eth,se_ora2,se_co,se_nr,      &
               se_mbo,se_sesq,                          & 
               growagno,ngrowagno,nonagno
!   leaf area index for isoprene
      REAL  ::  tlai  
!   actual emissions for NO
      REAL  :: e_no

! Other parameters needed in calculations
!  Guenther's parameterizations: Guenther et al. JGR 98, 12609-12617, 1993
      REAL  ::  ct, dt       !  Guenther's temperature correction for isoprene
      REAL  ::  cfno         !  NO correction factor
      REAL  ::  cfovoc       !  non-isoprene correction factor
      REAL  ::  par          !  PAR = photosynthetically active radiation (micromole/m^2/s)
      REAL  ::  csubl        !  C sub l from Guenther
      REAL  ::  zen          !  zenith angle (radians)
      REAL  ::  coszen       !  cosine(zenith angle)
      REAL  ::  pardb        !  PAR direct beam
      REAL  ::  pardif       !  PAR diffuse
      REAL :: gmtp           !  current simulation time

! Error message variables
      INTEGER , PARAMETER ::  ldev = 6    !  unit number for log file
      CHARACTER*256   ::   mesg

! Functions called directly or indirectly
!   clnew          calculates csubl based on zenith angle, par, and lai
!   cguen          Guenther's equation for computing light correction
!   fertilizer_adj computes fertlizer adjustment factor
!   veg_adj        computes vegatation adjustment factor
!   growseason     computes day of growing season

! Subroutines called directly or indirectly
!   calc_zenithb    calculates zenith angle from latitude, longitude, julian day, and GMT
!                   NOTE: longitude input for this routine is nonstandard: >0 for W, <0 for E!!
!   getpar         computes PAR (direct beam and diffuse) in umol/m2-sec from downward shortwave flux
!   hrno           algorithm to estimate NO emissions; does not include precipitation adjustment

!***************************************
!   begin body of subroutine bio_emissions_beis314
                         
! hour into integration
!The old gmtp method will break for runs longer than about 12 days with r4
!      gmtp=float(ktau)*dtstep/3600.
      gmtp = curr_secs/3600._8
!     
      gmtp=mod(gmt+gmtp,24.)
      write(mesg,*) 'calculate beis314 emissions at gmtp = ',gmtp
      call wrf_debug(15,mesg)
      DO 100 j=jts,jte  
      DO 100 i=its,ite  

           tair = t_phy(i,kts,j)
           pres = .01*p_phy(i,kts,j)
           ylat = xlat(i,j)
           ylong = xlong(i,j)
           tsolar = gsw(i,j)
           tlai = slai(i,j)
           se_iso  = sebio_iso(i,j)
           se_oli  = sebio_oli(i,j)
           se_api  = sebio_api(i,j)
           se_lim  = sebio_lim(i,j)
           se_xyl  = sebio_xyl(i,j)
           se_hc3  = sebio_hc3(i,j)
           se_ete  = sebio_ete(i,j)
           se_olt  = sebio_olt(i,j)
           se_ket  = sebio_ket(i,j)
           se_ald  = sebio_ald(i,j)
           se_hcho  = sebio_hcho(i,j)
           se_eth  = sebio_eth(i,j)
           se_ora2  = sebio_ora2(i,j)
           se_co  = sebio_co(i,j)
           se_nr  = sebio_nr(i,j)

           ! SESQ and MBO are added
           se_sesq  = sebio_sesq(i,j)
           se_mbo  = sebio_mbo(i,j)

           growagno = noag_grow(i,j)
           ngrowagno = noag_nongrow(i,j) 
           nonagno = nononag(i,j)

!....Perform checks on max and min bounds for temperature

           IF (tair .LT. 200.0) THEN
!              WRITE( mesg, 94010 )
!    &         'tair=', tair,
!    &         'too low at i,j= ',i,',',j
               WRITE( ldev, * ) mesg
           END IF

           IF (tair .GT. 315.0 ) THEN
!              WRITE( mesg, 94020 )
!    &         'tair=', tair,
!    &         'too high at i,j= ',i,',',j,
!    &         '...resetting to 315K' 
               tair = 315.0
!              WRITE( ldev, * ) mesg
           ENDIF

!... Isoprene emissions
!...... Calculate temperature correction term
           dt   = 28668.514 / tair
           ct   = EXP( 37.711 - 0.398570815 * dt ) /    &
                         (1.0 + EXP( 91.301 - dt ) )

!...... Calculate zenith angle in radians
!        NOTE: nonstandard longitude input here: >0 for W, <0 for E!!
           CALL calc_zenithb(ylat,-ylong,julday,gmtp,zen)
           coszen = COS(zen)

!...... Convert tsolar to PAR and find direct and diffuse fractions
           CALL getpar( tsolar, pres, zen, pardb, pardif )
           par = pardb + pardif

!...... Check max/min bounds of PAR and calculate
!...... biogenic isoprene
           IF ( par .LT. 0.00 .OR. par .GT. 2600.0 ) THEN
!                     WRITE( mesg, 94010 )
!    &                 'PAR=', par,
!    &                 'out of range at i,j= ',i,',',j
!                     WRITE( ldev, * ) mesg
           ENDIF

!...... Check max bound of LAI
           IF ( tlai .GT. 10.0 ) THEN
!                    WRITE( mesg, 94010 )
!    &                'LAI=', tlai,
!    &                'out of range at i,j= ',i,',',j
!                    WRITE( ldev, * ) mesg
           ENDIF

!...... Initialize csubl
           csubl = 0.0

!...... If PAR < 0.01 or zenith angle > 89 deg, set isoprene emissions to 0.
           IF ( par .LE. 0.01 .OR. coszen .LE. 0.02079483 ) THEN
               ebio_iso(i,j) = 0.0

           ELSE

!...... Calculate csubl including shading if LAI > 0.1
               IF ( tlai .GT. 0.1 ) THEN
                     csubl = clnew( zen, pardb, pardif, tlai )

!...... Otherwise calculate csubl without considering  LAI
               ELSE  ! keep this or not?
                     csubl  = cguen( par )

               ENDIF

               ebio_iso(i,j) = se_iso * ct * csubl

           ENDIF


!... Other biogenic emissions except NO:
!...... RACM: oli, api, lim, hc3, ete, olt, ket, ald, hcho, eth, ora2, co

           cfovoc = EXP( 0.09 * ( tair - 303.0 ) )

           ebio_oli(i,j)   =  se_oli * cfovoc
           ebio_api(i,j)   =  se_api * cfovoc
           ebio_lim(i,j)   =  se_lim * cfovoc
           ebio_xyl(i,j)   =  se_xyl * cfovoc
           ebio_hc3(i,j)   =  se_hc3 * cfovoc
           ebio_ete(i,j)   =  se_ete * cfovoc
           ebio_olt(i,j)   =  se_olt * cfovoc
           ebio_ket(i,j)   =  se_ket * cfovoc
           ebio_ald(i,j)   =  se_ald * cfovoc
           ebio_hcho(i,j)  =  se_hcho * cfovoc
           ebio_eth(i,j)   =  se_eth * cfovoc
           ebio_ora2(i,j)  =  se_ora2 * cfovoc
           ebio_co(i,j)    =  se_co * cfovoc
           ebio_nr(i,j)    =  se_nr * cfovoc

           ! SESQ and MBO are added
           ebio_sesq(i,j)  =  se_sesq * cfovoc
           ebio_mbo(i,j)   =  se_mbo * cfovoc

!... NO emissions

           CALL hrno( julday, growagno, ngrowagno, nonagno, tair, e_no)

           ebio_no(i,j) = e_no

 100  CONTINUE

      RETURN


!******************  FORMAT  STATEMENTS   ******************************

!...........   Informational (LOG) message formats... 92xxx


!...........   Internal buffering formats............ 94xxx


94010   FORMAT( A, F10.2, 1X, A, I4, A1, I4)
94020   FORMAT( A, F10.2, 1X, A, I4, A1, I4, 1X, A)


!***************** CONTAINS ********************************************
            CONTAINS

            REAL FUNCTION clnew( zen, pardb, pardif, tlai )

!........  Function to calculate csubl based on zenith angle, PAR, and LAI 
!******** Reference:CN98
!      Campbell, G.S. and J.M. Norman. 1998. An Introduction to Environmental Biophysics,
!      Springer-Verlag, New York.

            IMPLICIT NONE

            REAL, INTENT (IN) :: pardb    ! direct beam PAR( umol/m2-s)
            REAL, INTENT (IN) :: pardif   ! diffuse PAR ( umol/m2-s)
            REAL, INTENT (IN) :: zen      ! solar zenith angle (radians)
            REAL, INTENT (IN) :: tlai     ! leaf area index for grid cell
!.............  Local variables
            REAL kbe                ! extinction coefficient for direct beam
            REAL ALPHA              ! leave absorptivity
            REAL KD                 ! extinction coefficient for diffuse radiation
            REAL SQALPHA            ! square root of alpha
            REAL canparscat         ! exponentially wtd scattered PAR (umol/m2-s)
            REAL canpardif          ! exponentially wtd diffuse PAR (umol/m2-s)
            REAL parshade           ! PAR on shaded leaves (umol/m2-s)
            REAL parsun             ! PAR on sunlit leaves (umol/m2-s)
            REAL laisun             ! LAI that is sunlit
            REAL fracsun            ! fraction of leaves that are sunlit
            REAL fracshade          ! fraction of leaves that are shaded

            ALPHA = 0.8
            SQALPHA = SQRT(0.8)
            KD = 0.68

!...........  CN98 - eqn 15.4, assume x=1

            kbe = 0.5 * SQRT(1. + TAN( zen ) * TAN( zen ))
 
!..........   CN98 - p. 261 (this is usually small)

            canparscat = 0.5 * pardb * (EXP(-1.*sqalpha*kbe*tlai) -    &
                     EXP(-1.* kbe * tlai))

!..........   CN98 - p. 261 (assume exponentially wtd avg)

            canpardif  = pardif * (1.-EXP(-1.*sqalpha*kd*tlai)) /      &
                     (sqalpha*kd*tlai)

!.........    CN98 - p. 261 (for next 3 eqns)

            parshade   = canpardif + canparscat
	    parsun     = kbe * pardb  + parshade
	    laisun     = (1. - EXP( -1. * kbe * tlai))/kbe
	    fracsun    = laisun/tlai
	    fracshade  = 1. - fracsun

!..........  cguen is guenther's eqn for computing light correction as a 
!..........  function of PAR...fracSun should probably be higher since 
!..........  sunlit leaves tend to be thicker than shaded leaves.  But 
!..........  since we need to make crude asmptns regarding leave 
!..........  orientation (x=1), will not attempt to fix at the moment.

            clnew =fracsun * cguen(parsun) + fracshade * cguen(parshade)

            RETURN 
            END FUNCTION clnew

            REAL FUNCTION cguen( partmp ) 

!..........  Guenther's equation for computing light correction
!    Reference:   Guenther, A., B. Baugh, G. Brasseur, J. Greenberg, P. Harley, L. Klinger,
!   D. Serca, and L. Vierling, 1999: Isoprene emission estimates and uncertainties
!   for the Central African EXPRESSO Study domain. J. Geophys. Res., 104, 30625-30639.

            IMPLICIT NONE
            REAL, INTENT (IN) :: partmp
            REAL, PARAMETER :: alpha = 0.001
            REAL, PARAMETER :: cl = 1.42

            IF ( partmp .LE. 0.01) THEN
               cguen = 0.0
            ELSE
               cguen = (alpha *cl * partmp) /                         &
                   SQRT(1. + alpha * alpha * partmp * partmp)
            ENDIF
 
            RETURN
            END FUNCTION cguen

      END SUBROUTINE bio_emissions_beis314

!=================================================================

      SUBROUTINE calc_zenithb(lat,long,ijd,gmt,zenith)
        ! Based on calc_zenith from WRF-Chem module_phot_mad.F
        ! this subroutine calculates solar zenith angle for a
        ! time and location.  must specify:
        ! input:
        ! lat - latitude in decimal degrees
        ! long - longitude in decimal degrees 
        ! NOTE: Nonstandard convention for long: >0 for W, <0 for E!!
        ! gmt  - greenwich mean time - decimal military eg.
        ! 22.75 = 45 min after ten pm gmt
        ! output
        ! zenith - in radians (GJF, 6/2004)
        ! remove azimuth angle calculation since not needed (GJF, 6/2004)
        ! .. Scalar Arguments ..
        CHARACTER*256   ::   mesg
        REAL :: gmt, lat, long, zenith
        INTEGER :: ijd
        ! .. Local Scalars ..
        REAL :: csz, cw, d, decl, dr, ec, epsi, eqt, eyt, feqt, feqt1, &
          feqt2, feqt3, feqt4, feqt5, feqt6, feqt7, lbgmt, lzgmt, ml, pepsi, &
          pi, ra, rdecl, reqt, rlt, rml, rphi, rra, ssw, sw, tab, w, wr, &
          yt, zpt, zr
        INTEGER :: jd
        ! .. Intrinsic Functions ..
        INTRINSIC acos, atan, cos, min, sin, tan
        ! convert to radians
        pi = 3.1415926535590
        dr = pi/180.
        rlt = lat*dr
        rphi = long*dr

        ! ???? + (yr - yref)

        jd = ijd

        d = jd + gmt/24.0
        ! calc geom mean longitude
        ml = 279.2801988 + .9856473354*d + 2.267E-13*d*d
        rml = ml*dr

        ! calc equation of time in sec
        ! w = mean long of perigee
        ! e = eccentricity
        ! epsi = mean obliquity of ecliptic
        w = 282.4932328 + 4.70684E-5*d + 3.39E-13*d*d
        wr = w*dr
        ec = 1.6720041E-2 - 1.1444E-9*d - 9.4E-17*d*d
        epsi = 23.44266511 - 3.5626E-7*d - 1.23E-15*d*d
        pepsi = epsi*dr
        yt = (tan(pepsi/2.0))**2
        cw = cos(wr)
        sw = sin(wr)
        ssw = sin(2.0*wr)
        eyt = 2.*ec*yt
        feqt1 = sin(rml)*(-eyt*cw-2.*ec*cw)
        feqt2 = cos(rml)*(2.*ec*sw-eyt*sw)
        feqt3 = sin(2.*rml)*(yt-(5.*ec**2/4.)*(cw**2-sw**2))
        feqt4 = cos(2.*rml)*(5.*ec**2*ssw/4.)
        feqt5 = sin(3.*rml)*(eyt*cw)
        feqt6 = cos(3.*rml)*(-eyt*sw)
        feqt7 = -sin(4.*rml)*(.5*yt**2)
        feqt = feqt1 + feqt2 + feqt3 + feqt4 + feqt5 + feqt6 + feqt7
        eqt = feqt*13751.0

        ! convert eq of time from sec to deg
        reqt = eqt/240.
        ! calc right ascension in rads
        ra = ml - reqt
        rra = ra*dr
        ! calc declination in rads, deg
        tab = 0.43360*sin(rra)
        rdecl = atan(tab)
        decl = rdecl/dr
        ! calc local hour angle
        lbgmt = 12.0 - eqt/3600. + long*24./360.
        lzgmt = 15.0*(gmt-lbgmt)
        zpt = lzgmt*dr
        csz = sin(rlt)*sin(rdecl) + cos(rlt)*cos(rdecl)*cos(zpt)
        if(csz.gt.1) then
           write(mesg,*) 'calczen,csz ',csz
           call wrf_debug(15,mesg)
        endif
        csz = min(1.,csz)
        zr = acos(csz)
!       zenith = zr/dr
! keep zenith angle in radians for later use (GJF 6/2004)
        zenith = zr 

        RETURN

      END SUBROUTINE calc_zenithb

!=================================================================


        SUBROUTINE getpar( tsolar, pres, zen, pardb, pardif )

!***********************************************************************
!  subroutine body starts at line  
!
!  DESCRIPTION:
!  
!        Based on code from Bart Brashers (10/2000), which was based on
!        code from Weiss and Norman (1985).  
!
!
!  PRECONDITIONS REQUIRED:
!     Solar radiation (W/m2) and pressure (mb)
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION  HISTORY:
!    3/01 : Prototype by JMV
! 
!***********************************************************************
!
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)Id: getpar.f,v 1.1.1.1 2001/03/27 19:08:49 smith_w Exp 
!
! COPYRIGHT (C) 2001, MCNC--North Carolina Supercomputing Center
! All Rights Reserved
!
! See file COPYRIGHT for conditions of use.
!
! MCNC-Environmental Programs Group
! P.O. Box 12889
! Research Triangle Park, NC  27709-2889
!
! env_progs@mcnc.org
!
! Pathname: Source: /env/proj/archive/cvs/jmv/beis3v0.9/getpar.f,v 
! Last updated: Date: 2001/03/27 19:08:49  
!
!***********************************************************************

      IMPLICIT NONE

!........ Inputs

        REAL , INTENT  (IN) :: tsolar   ! modeled or observed total radiation (W/m2)
        REAL , INTENT  (IN) :: pres     ! atmospheric pressure (mb)
        REAL , INTENT  (IN) :: zen      ! solar zenith angle (radians)
 
!........ Outputs

        REAL, INTENT (OUT) :: pardb     ! direct beam PAR( umol/m2-s)
        REAL, INTENT (OUT) :: pardif    ! diffuse PAR ( umol/m2-s)

!...........   PARAMETERS and their descriptions:

        REAL, PARAMETER :: watt2umol = 4.6  ! convert W/m^2 to umol/m^2-s (4.6)

!      
        REAL ratio		! transmission fraction for total radiation
        REAL ot                 ! optical thickness
        REAL rdvis              ! possible direct visible beam (W/m^2)
        REAL rfvis              ! possible visible diffuse (W/m^2)
        REAL wa                 ! water absorption in near-IR (W/m^2)
        REAL rdir               ! direct beam in near-IR (W/m^2)
        REAL rfir               ! diffuse near-IR (W/m^2)
        REAL rvt                ! total possible visible radiation (W/m^2)
        REAL rirt               ! total possible near-IR radiation (W/m^2)
        REAL fvis               ! fraction of visible to total 
        REAL fvb                ! fraction of visible that is direct beam
        REAL fvd                ! fraction of visible that is diffuse

!***************************************
!   begin body of subroutine  

!............ Assume that PAR = 0 if zenith angle is greater than 87 degrees
!............ or if solar radiation is zero

        IF (zen .GE. 1.51844 .OR. tsolar .LE. 0.) THEN
             pardb  = 0.
             pardif = 0.
             RETURN
        ENDIF
	   
!............ Compute clear sky (aka potential) radiation terms

        ot    = pres / 1013.25 / COS(zen)              !Atmospheric Optical thickness
        rdvis = 600. * EXP(-.185 * ot) * COS(zen)      !Direct visible beam, eqn (1)
        rfvis = 0.42 * (600 - rdvis) * COS(zen)        !Visible Diffuse, eqn (3)
        wa    = 1320 * .077 * (2. * ot)**0.3           !water absorption in near-IR, eqn (6)
        rdir  = (720. * EXP(-0.06 * ot)-wa) * COS(zen) !Direct beam near-IR, eqn (4)
        rfir  = 0.65 * (720. - wa - rdir) * COS(zen)   !Diffuse near-IR, eqn (5)

        rvt   = rdvis + rfvis                    !Total visible radiation, eqn (9)
        rirt  = rdir + rfir                      !Total near-IR radiation, eqn (10) 
        fvis  = rvt/(rirt + rvt)                 !Fraction of visible to total radiation, eqn 7
        ratio = tsolar /(rirt + rvt)             !Ratio of "actual" to clear sky solar radiation

!............ Compute fraction of visible that is direct beam

        IF (ratio .GE. 0.89) THEN
           fvb = rdvis/rvt * 0.941124
        ELSE IF (ratio .LE. 0.21) THEN
           fvb = rdvis/rvt * 9.55E-3
        ELSE
           fvb = rdvis/rvt * (1.-((0.9 - ratio)/0.7)**0.666667)
        ENDIF
        fvd = 1. - fvb

!............ Compute PAR (direct beam and diffuse) in umol/m2-sec

        pardb  = tsolar * fvis * fvb * watt2umol	
        pardif = tsolar * fvis * fvd * watt2umol      


        RETURN 

!******************  FORMAT  STATEMENTS   ******************************

!...........   Informational (LOG) message formats... 92xxx


!...........   Internal buffering formats............ 94xxx

        END SUBROUTINE getpar

      SUBROUTINE hrno( julday, growagno, ngrowagno, nonagno, tairin, e_no)

!***********************************************************************
!  subroutine body starts at line  150
!
!  DESCRIPTION:
!  
!     Uses new NO algorithm NO = Normalized*Tadj*Fadj*Cadj
!     to estimate NO emissions 
!     Information needed to estimate NO emissions
!     Julian Day          (integer)   julday 
!     Surface Temperature (MCIP field) tair    (K)
!   Note:  Precipitation adjustment not used in the WRF-Chem implementation of BEIS3.11
!          because of differences in soil categories between BEIS and WRF-Chem
!  
!     The calculation are based on the following paper by J.J. Yienger and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender       
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and Nitric Oxide Emissions from Agricultural Processes
!       Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!        Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature
!
!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     fertilizer_adj computes fertlizer adjustment factor
!     veg_adj        computes vegatation adjustment factor
!     growseason     computes day of growing season
!      
!
!  REVISION  HISTORY:
!    10/01 : Prototype by GAP
! 
!***********************************************************************
!
! Project Title: BEIS3 Enhancements for NO emission calculation
! File: hrno.f
!
!
!***********************************************************************

      IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT (IN)  :: julday   !  current julian day


        REAL, INTENT (IN)  ::  tairin     !  air temperature (K)
        REAL, INTENT (IN)  ::  growagno     !  norm NO emissions, agricultural, growing
        REAL, INTENT (IN)  ::  ngrowagno    !  norm NO emissions, agricultural, not growing
        REAL, INTENT (IN)  ::  nonagno      !  norm NO emissions, non-agricultural

        REAL, INTENT (OUT)  ::  e_no      !  output NO emissions

!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL            cfno         !  NO correction factor
        REAL            cfnograss    !  NO correction factor for grasslands
        REAL            tsoi         ! soil temperature
        REAL            tair         ! air temperature

        REAL          :: cfnowet, cfnodry

        INTEGER gday
!***********************************************************************

        tair = tairin

!............. calculate NO emissions by going thru temperature cases

        gday = growseason(julday)
!       Control of growing season should be done in the input files for BEIS3.13
        ! gday = 91
        IF (gday .eq. 0) THEN         !not growing season
           IF ( tair .GT. 303.00 ) tair = 303.00

           IF ( tair .GT. 268.8690 ) THEN  
              cfno = EXP( 0.04686 * tair  -  14.30579 ) ! grass (from BEIS2)
           ELSE
              cfno = 0.0
           ENDIF

           e_no =                   &
                 ngrowagno * cfno   &     !agriculture
                 +  nonagno * cfno   !  non-agriculture

        ELSE 

           tsoi = 0.72*tair+82.28
           IF (tsoi .LE. 273.16) tsoi = 273.16
           IF (tsoi .GE. 303.16) tsoi = 303.16

           cfnodry = (1./3.)*(1./30.)*(tsoi-273.16)  ! see YL 1995 Equa 9a p. 11452
           IF (tsoi .LE. 283.16) THEN       ! linear cold case
              cfnowet = (tsoi-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
           ELSE                             ! exponential case
              cfnowet =  EXP(0.103*(tsoi-273.16))   &
                         *EXP(-0.103*30.0)
           ENDIF
           cfno = 0.5*cfnowet + 0.5*cfnodry

           IF ( tair .GT. 303.00 ) tair = 303.00

           IF ( tair .GT. 268.8690 ) THEN  
              cfnograss = EXP( 0.04686 * tair  -  14.30579 ) ! grass (from BEIS2)
           ELSE
              cfnograss = 0.0
           ENDIF

           e_no =  growagno * cfno *fertilizer_adj(julday)*veg_adj(julday)   &
                  +  nonagno * cfnograss                   

        ENDIF

        RETURN

!***************** CONTAINS ********************************************
        CONTAINS

        REAL FUNCTION fertilizer_adj(julday)
!*****************************************************************
!
!  SUMMARY:
!  computes fertilizer adjustment factor from Julian day
!
!  FUNCTION CALLS:
!     growseason     computes day of growing season
!
!  NOTE: julday = Julian day format
!       
!*****************************************************************
        implicit none
        integer julday
!
!******** local scratch variables
!
       integer gday
!
!******** function calls
!
     gday = growseason(julday)
!       Control of growing season should be done in the input files for BEIS3.13
     ! gday = 91
      
      IF (gday .EQ. 0) THEN
          fertilizer_adj = 0.0
      ELSEIF ((gday .GE. 1) .AND. (gday .LT. 30)) THEN ! first month of growing season
          fertilizer_adj = 1.0
      ELSEIF (gday .GE. 30)   THEN
          fertilizer_adj = 1.0+30.0/184.0-float(gday)/184.0
      ELSE
          write (*,*) 'ERROR: invalid Julian day'
	  write (*,*) 'julday = ', julday
	  write (*,*) 'growing season day = ',gday
	  CALL wrf_error_fatal ( 'INVALID GROWING SEASON DAY')
      ENDIF
	
      RETURN

      END FUNCTION fertilizer_adj


      REAL FUNCTION veg_adj(julday)
!*****************************************************************
!
!  SUMMARY:
!  computes vegetation adjustment factor from Julian day
!
!  FUNCTION CALLS:
!     growseason     computes day of growing season
!
!  NOTE: julday = Julian day format
!       
!*****************************************************************
      implicit none
  
       integer julday


!
!******** locals
!
      integer gday

!
!******* function calls
!
      gday = growseason(julday)
!       Control of growing season should be done in the input files for BEIS3.13
      !gday = 91
      
      IF (gday .LE. 30) THEN
          veg_adj = 1.0
      ELSEIF ((gday .GT. 30) .AND. (gday .LT. 60)) THEN 
          veg_adj = 1.5-(float(gday)/60.0)
      ELSEIF (gday .GE. 60) THEN 
          veg_adj = 0.5
      ELSE
          write (*,*) 'ERROR: invalid Julian day'
	  write (*,*) 'julday = ', julday
	  write (*,*) 'growing season day = ',gday
	  CALL wrf_error_fatal ( 'veg_adj: INVALID GROWING SEASON DAY' )
      ENDIF


      RETURN


      END FUNCTION veg_adj      

     END SUBROUTINE hrno 

      INTEGER FUNCTION growseason(julday)
!*****************************************************************
!
!  SUMMARY:
!  computes day of growing season from Julian day
!
!  NOTE: julday = Julian day format
!       
!*****************************************************************
      implicit none       
      integer julday

!******* 
!       
!     
!  given Julian day, compute day of growing season
!     
!         
!
!******** locals

      integer gsjulian_start
      integer gsjulian_end

      data gsjulian_start /91/ !=April 1 in non-leap-year
      data gsjulian_end /304/ !=Oct 31 in non-leap-year
	 
      IF      ((julday .GE. gsjulian_start)       &
         .AND. (julday .LE. gsjulian_end)) THEN   !  growing season
       
         growseason = julday-gsjulian_start+1

	  
      ELSEIF  ((julday .GE. 1)     &       ! before or after growing season
         .AND. (julday .LE. 366)) THEN      
     
         growseason = 0
	 
      ELSE
          write (*,*) 'ERROR: Invalid julday '
	  write (*,*) 'julday = ',julday
	  CALL wrf_error_fatal ( 'growseason: INVALID JULIAN DAY')
      ENDIF


      RETURN
      END FUNCTION growseason


END MODULE module_bioemi_beis314
