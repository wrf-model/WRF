!
!-- Module for Gravity Wave Drag (GWD) and Mountain Blocking (MB)
!
!-- Initially incorporated into the WRF NMM from the GFS by B. Ferrier
!   in April/May 2007.  
!
!   Search for "ORIGINAL DOCUMENTATION BLOCK" for further description.
!
!#######################################################################
!
      MODULE module_gwd
!
!      USE MODULE_DM               ! to get processor element
      USE MODULE_EXT_INTERNAL     ! to assign fortan unit number
!
!-- Contains subroutines GWD_init, GWD_driver, and GWD_col
!
!#######################################################################
!
      INTEGER, PARAMETER :: KIND_PHYS=SELECTED_REAL_KIND(13,60) ! the '60' maps to 64-bit real
      INTEGER,PRIVATE,SAVE :: IMX, NMTVR, IDBG, JDBG
      REAL,PRIVATE,SAVE :: RAD_TO_DEG   !-- Convert radians to degrees
      REAL,PRIVATE,SAVE :: DEG_TO_RAD   !-- Convert degrees to radians
      REAL (KIND=KIND_PHYS),PRIVATE,SAVE :: DELTIM,RDELTIM
      REAL(kind=kind_phys),PRIVATE,PARAMETER :: SIGFAC=0.0   !-- Key tunable parameter
!dbg      real,private,save :: dumin,dumax,dvmin,dvmax   !dbg
!
      CONTAINS
!
!-- Initialize variables used in GWD + MB
!
      SUBROUTINE GWD_init (DTPHS,DELX,DELY,CEN_LAT,CEN_LON,RESTRT       &
     &                     ,GLAT,GLON,CROT,SROT,HANGL                   &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE )

!
      IMPLICIT NONE
!
!== INPUT:
!-- DELX, DELY - DX, DY grid resolutions in zonal, meridional directions (m)
!-- CEN_LAT, CEN_LON - central latitude, longitude (degrees)
!-- RESTRT - logical flag for restart file (true) or WRF input file (false)
!-- GLAT, GLON - central latitude, longitude at mass points (radians)
!-- CROT, SROT - cosine and sine of the angle between Earth and model coordinates
!-- HANGL  - angle of the mountain range w/r/t east (convert to degrees)
!
!-- Saved variables within module:
!-- IMX - in the GFS it is an equivalent number of points along a latitude 
!         circle (e.g., IMX=3600 for a model resolution of 0.1 deg) 
!       => Calculated at start of model integration in GWD_init
!-- NMTVR - number of input 2D orographic fields
!-- GRAV = gravitational acceleration
!-- DELTIM - physics time step (s)
!-- RDELTIM - reciprocal of physics time step (s)
!
!== INPUT indices:
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
      REAL, INTENT(IN) :: DTPHS,DELX,DELY,CEN_LAT,CEN_LON
      LOGICAL, INTENT(IN) :: RESTRT
      REAL, INTENT(IN), DIMENSION (ims:ime,jms:jme) :: GLON,GLAT
      REAL, INTENT(OUT), DIMENSION (ims:ime,jms:jme) :: CROT,SROT
      REAL, INTENT(INOUT), DIMENSION (ims:ime,jms:jme) :: HANGL
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE
!
!-- Local variables:
!
      REAL, PARAMETER :: POS1=1.,NEG1=-1.
      REAL :: DX,DTR,LAT0,LoN0,CLAT0,SLAT0,CLAT,DLON,X,Y,TLON,ROT
      INTEGER :: I,J

!dbg
!dbg real :: xdbg,ydbg,d_x,d_y,dist,dist_min
!dbg data xdbg,ydbg / -118.3,36 / ! 118.3W 36 N

!
!-----------------------------------------------------------------------
!
      DX=SQRT((DELX)**2+(DELY)**2)   !-- Model resolution in degrees
!-- IMX is the number of grid points along a latitude circle in the GFS
      IMX=INT(360./DX+.5)

!dbg IMX=1152     !dbg -- Match the grid point printed from GFS run

      NMTVR=14            !-- 14 input fields for orography
      DELTIM=DTPHS
      RDELTIM=1./DTPHS
!
!-- Calculate angle of rotation (ROT) between Earth and model coordinates,
!   but pass back out cosine (CROT) and sine (SROT) of this angle
!
      DTR=ACOS(-1.)/180. !-- convert from degrees to radians
      DEG_TO_RAD=DTR     !-- save conversion from degrees to radians
!
      LAT0=DTR*CEN_LON   !-- central latitude of grid in radians
      LoN0=DTR*CEN_LAT   !-- central longitude of grid in radians
!
      DTR=1./DTR         !-- convert from radians to degrees
      RAD_TO_DEG=DTR     !-- save conversion from radians to degrees
!
      CLAT0=COS(LAT0)
      SLAT0=SIN(LAT0)
      DO J=JTS,JTE
        DO I=ITS,ITE
          CLAT=COS(GLAT(I,J))
          DLON=GLON(I,J)-LoN0
          X=CLAT0*CLAT*COS(DLON)+SLAT0*SIN(GLAT(I,J))
          Y=-CLAT*SIN(DLON)
          TLON=ATAN(Y/X)              !-- model longitude
          X=SLAT0*SIN(TLON)/CLAT
          Y=MIN(POS1, MAX(NEG1, X) )
          ROT=ASIN(Y)                 !-- angle between geodetic & model coordinates
          CROT(I,J)=COS(ROT)
          SROT(I,J)=SIN(ROT)
        ENDDO    !-- I
      ENDDO      !-- J
      IF (.NOT.RESTRT) THEN
!-- Convert from radians to degrees for WRF input files only.
!   There should have ano further conversion from rad to degree at this
!   point since HANGL read from wrfinput_d01 already in degree even in
!   the non restart mode... Chanh
!
!        DO J=JTS,JTE
!          DO I=ITS,ITE
!            HANGL(I,J)=DTR*HANGL(I,J)  !-- convert to degrees (+/-90 deg)
!          ENDDO    !-- I
!        ENDDO      !-- J
      ENDIF
!dbg
!dbg dumin=-1.
!dbg dumax=1.
!dbg dvmin=-1.
!dbg dvmax=1.
!dbg print *,'delx=',delx,'  dely=',dely,'  dx=',dx,'  imx=',imx
!dbg dtr=1./dtr             !-- convert from degrees back to radians
!dbg dist_min=dtr*DX        !-- grid length in radians
!dbg xdbg=dtr*xdbg          !-- convert xdbg to radians
!dbg ydbg=dtr*ydbg          !-- convert ydbg to radians
!dbg idbg=-100
!dbg jdbg=-100
!dbg print *,'dtr,dx,dist_min,xdbg,ydbg=',dtr,dx,dist_min,xdbg,ydbg
!dbg do j=jts,jte
!dbg   do i=its,ite
!dbg !-- Find i,j for xdbg, ydbg
!dbg     d_x=cos(glat(i,j))*(glon(i,j)-xdbg)
!dbg     d_y=(glat(i,j)-ydbg)
!dbg     dist=sqrt(d_x*d_x+d_y*d_y)
!dbg !! print *,'i,j,glon,glat,d_x,d_y,dist=',i,j,glon(i,j),glat(i,j),d_x,d_y,dist
!dbg     if (dist < dist_min) then
!dbg       dist_min=dist
!dbg       idbg=i
!dbg       jdbg=j
!dbg print *,'dist_min,idbg,jdbg=',dist_min,idbg,jdbg
!dbg     endif
!dbg   enddo    !-- I
!dbg enddo      !-- J
!dbg if (idbg>0 .and. jdbg>0) print *,'idbg=',idbg,'  jdbg=',jdbg

!
      END SUBROUTINE GWD_init
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GWD_driver(U,V,T,Q,Z,DP,PINT,PMID,EXNR, KPBL, ITIME    &
     &                     ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW       &
     &                     ,HLENW,HLENS,HLENSW,HLENNW                   &
     &                     ,HANGL,HANIS,HSLOP,HZMAX,CROT,SROT           &
     &                     ,DUDT,DVDT,UGWDsfc,VGWDsfc,XLAND             &      !ADDED XLAND FOR SKIPPING OCEAN POINTS(KWON)
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE )
!
!== INPUT:
!-- U, V - zonal (U), meridional (V) winds at mass points (m/s)
!-- T, Q - temperature (C), specific humidity (kg/kg)
!-- DP - pressure thickness (Pa)
!-- Z - geopotential height (m)
!-- PINT, PMID - interface and midlayer pressures, respectively (Pa)
!-- EXNR - (p/p0)**(Rd/Cp)
!-- KPBL - vertical index at PBL top
!-- ITIME - model time step (=NTSD)
!-- HSTDV - orographic standard deviation
!-- HCNVX - normalized 4th moment of the orographic convexity
!-- Template for the next two sets of 4 arrays:
!             NWD  1   2   3   4   5   6   7   8
!              WD  W   S  SW  NW   E   N  NE  SE
!-- Orographic asymmetry (HASYx, x=1-4) for upstream & downstream flow (4 planes)
!-- * HASYW - orographic asymmetry for upstream & downstream flow in W-E plane
!-- * HASYS - orographic asymmetry for upstream & downstream flow in S-N plane
!-- * HASYSW - orographic asymmetry for upstream & downstream flow in SW-NE plane
!-- * HASYNW - orographic asymmetry for upstream & downstream flow in NW-SE plane
!-- Orographic length scale or mountain width (4 planes)
!-- * HLENW - orographic length scale for upstream & downstream flow in W-E plane
!-- * HLENS - orographic length scale for upstream & downstream flow in S-N plane
!-- * HLENSW - orographic length scale for upstream & downstream flow in SW-NE plane
!-- * HLENNW - orographic length scale for upstream & downstream flow in NW-SE plane
!-- HANGL  - angle (degrees) of the mountain range w/r/t east
!-- HANIS - anisotropy/aspect ratio of orography
!-- HSLOP - slope of orography
!-- HZMAX - max height above mean orography
!-- CROT, SROT - cosine & sine of the angle between Earth & model coordinates
!
!== OUTPUT:
!-- DUDT, DVDT - zonal, meridional wind tendencies
!-- UGWDsfc, VGWDsfc - zonal, meridional surface wind stresses (N/m**2)
!
!== INPUT indices:
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start i index for tile
!-- ite           end i index for tile
!-- jts           start j index for tile
!-- jte           end j index for tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!-- INPUT variables:
!
      REAL, INTENT(IN), DIMENSION (ims:ime, kms:kme, jms:jme) ::        &
     &                                   U,V,T,Q,Z,DP,PINT,PMID,EXNR
      REAL, INTENT(IN), DIMENSION (ims:ime, jms:jme) :: HSTDV,HCNVX     &
     &      ,HASYW,HASYS,HASYSW,HASYNW,HLENW,HLENS,HLENSW,HLENNW,HANGL  &
     &      ,HANIS,HSLOP,HZMAX,CROT,SROT, XLAND                           !ADDED XLAND BY KWON
      INTEGER, INTENT(IN), DIMENSION (ims:ime, jms:jme) :: KPBL
      INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde                    &
     &,                      ims,ime,jms,jme,kms,kme                    &
     &,                      its,ite,jts,jte,kts,kte,ITIME

!
!-- OUTPUT variables:
!
      REAL, INTENT(OUT), DIMENSION (ims:ime, kms:kme, jms:jme) ::       &
     &                                                        DUDT,DVDT
      REAL, INTENT(OUT), DIMENSION (ims:ime, jms:jme) :: UGWDsfc,VGWDsfc
!
!-- Local variables
!-- DUsfc, DVsfc - zonal, meridional wind stresses (diagnostics)
!
      INTEGER, PARAMETER :: IM=1    !-- Reduces changes in subroutine GWPDS
      REAL(KIND=KIND_PHYS), PARAMETER :: G=9.806, GHALF=.5*G            &
     &,                                  THRESH=1.E-6, dtlarge=1.   !dbg
      INTEGER, DIMENSION (IM) :: LPBL
      REAL(KIND=KIND_PHYS), DIMENSION (IM,4) :: OA4,CLX4
      REAL(KIND=KIND_PHYS), DIMENSION (IM) :: DUsfc,DVsfc               &
     &,                              HPRIME,OC,THETA,SIGMA,GAMMA,ELVMAX
      REAL(KIND=KIND_PHYS), DIMENSION (IM,KTS:KTE) :: DUDTcol,DVDTcol   &
     &,                    Ucol,Vcol,Tcol,Qcol,DPcol,Pcol,EXNcol,PHIcol
      REAL(KIND=KIND_PHYS), DIMENSION (IM,KTS:KTE+1) :: PINTcol,PHILIcol
      INTEGER :: I,J,IJ,K,Imid,Jmid                             
      REAL :: Ugeo,Vgeo,Umod,Vmod, TERRtest,TERRmin
      REAL(KIND=KIND_PHYS) :: TEST
      CHARACTER(LEN=255) :: message

!dbg
logical :: lprnt  !dbg
character (len=26) :: label
integer :: kpblmin,kpblmax, mype, iunit
real :: hzmaxmin,hzmaxmax,hanglmin,hanglmax,hslopmin,hslopmax,hanismin,hanismax  &
,hstdvmin,hstdvmax,hcnvxmin,hcnvxmax,hasywmin,hasywmax,hasysmin,hasysmax  &
,hasyswmin,hasyswmax,hasynwmin,hasynwmax,hlenwmin,hlenwmax,hlensmin,hlensmax  &
,hlenswmin,hlenswmax,hlennwmin,hlennwmax,zsmin,zsmax,delu,delv
! Added this declaration
real :: helvmin,helvmax
!dbg

!
!--------------------------  Executable below  -------------------------
!


lprnt=.false.
!dbg
if (itime <= 1) then
  CALL WRF_GET_MYPROC(MYPE)   !-- Get processor number
  kpblmin=100
  kpblmax=-100
  hzmaxmin=1.e6
  hzmaxmax=-1.e6
  hanglmin=1.e6
  hanglmax=-1.e6
  hslopmin=1.e6
  hslopmax=-1.e6
  hanismin=1.e6
  hanismax=-1.e6
  hstdvmin=1.e6
  hstdvmax=-1.e6
  hcnvxmin=1.e6
  hcnvxmax=-1.e6
  hasywmin=1.e6
  hasywmax=-1.e6
  hasysmin=1.e6
  hasysmax=-1.e6
  hasyswmin=1.e6
  hasyswmax=-1.e6
  hasynwmin=1.e6
  hasynwmax=-1.e6
  hlenwmin=1.e6
  hlenwmax=-1.e6
  hlensmin=1.e6
  hlensmax=-1.e6
  hlenswmin=1.e6
  hlenswmax=-1.e6
  hlennwmin=1.e6
  hlennwmax=-1.e6
  zsmin=1.e6
  zsmax=-1.e6
! Added initialization of helvmin and helvmax
  helvmin=1.e6
  helvmax=-1.e6
  do j=jts,jte
    do i=its,ite
      kpblmin=min(kpblmin,kpbl(i,j))
      kpblmax=max(kpblmax,kpbl(i,j))
      helvmin=min(helvmin,hzmax(i,j))
      helvmax=max(helvmax,hzmax(i,j))
      hanglmin=min(hanglmin,hangl(i,j))
      hanglmax=max(hanglmax,hangl(i,j))
      hslopmin=min(hslopmin,hslop(i,j))
      hslopmax=max(hslopmax,hslop(i,j))
      hanismin=min(hanismin,hanis(i,j))
      hanismax=max(hanismax,hanis(i,j))
      hstdvmin=min(hstdvmin,hstdv(i,j))
      hstdvmax=max(hstdvmax,hstdv(i,j))
      hcnvxmin=min(hcnvxmin,hcnvx(i,j))
      hcnvxmax=max(hcnvxmax,hcnvx(i,j))
      hasywmin=min(hasywmin,hasyw(i,j))
      hasywmax=max(hasywmax,hasyw(i,j))
      hasysmin=min(hasysmin,hasys(i,j))
      hasysmax=max(hasysmax,hasys(i,j))
      hasyswmin=min(hasyswmin,hasysw(i,j))
      hasyswmax=max(hasyswmax,hasysw(i,j))
      hasynwmin=min(hasynwmin,hasynw(i,j))
      hasynwmax=max(hasynwmax,hasynw(i,j))
      hlenwmin=min(hlenwmin,hlenw(i,j))
      hlenwmax=max(hlenwmax,hlenw(i,j))
      hlensmin=min(hlensmin,hlens(i,j))
      hlensmax=max(hlensmax,hlens(i,j))
      hlenswmin=min(hlenswmin,hlensw(i,j))
      hlenswmax=max(hlenswmax,hlensw(i,j))
      hlennwmin=min(hlennwmin,hlennw(i,j))
      hlennwmax=max(hlennwmax,hlennw(i,j))
      zsmin=min(zsmin,z(i,1,j))
      zsmax=max(zsmax,z(i,1,j))
    enddo
  enddo
  write(message,*) 'Maximum and minimum values within GWD-driver for MYPE=',MYPE
  write(message,"(i3,2(a,e12.5))") mype,':  deltim=',deltim,'  rdeltim=',rdeltim
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,i3))")    mype,':  kpblmin=',kpblmin,'  kpblmax=',kpblmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  helvmin=',helvmin,'  helvmax=',helvmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hanglmin=',hanglmin,'  hanglmax=',hanglmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hslopmin=',hslopmin,'  hslopmax=',hslopmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hanismin=',hanismin,'  hanismax=',hanismax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hstdvmin=',hstdvmin,'  hstdvmax=',hstdvmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hcnvxmin=',hcnvxmin,'  hcnvxmax=',hcnvxmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasywmin=',hasywmin,'  hasywmax=',hasywmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasysmin=',hasysmin,'  hasysmax=',hasysmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasyswmin=',hasyswmin,'  hasyswmax=',hasyswmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasynwmin=',hasynwmin,'  hasynwmax=',hasynwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlenwmin=',hlenwmin,'  hlenwmax=',hlenwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlensmin=',hlensmin,'  hlensmax=',hlensmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlenswmin=',hlenswmin,'  hlenswmax=',hlenswmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlennwmin=',hlennwmin,'  hlennwmax=',hlennwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  zsmin=',zsmin,'  zsmax=',zsmax
  CALL wrf_message(trim(message))

endif   ! if (itime <= 1) then
!dbg

!
!-- Initialize variables
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUDT(I,K,J)=0.
        DVDT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JMS,JME
      DO I=IMS,IME
        UGWDsfc(I,J)=0.
        VGWDsfc(I,J)=0.
!        IF(ITIMIE.LE.2.OR.ITIME.GE.900) THEN
!        PRINT *,'KWON: I J HSTDV  IN GWD_DRIVER ',ITIME,I,J,HSTDV(I,J)
!        ENDIF
      ENDDO
      ENDDO
!
!-- For debugging, find approximate center point within each tile
!
!dbg       Imid=.5*(ITS+ITE)
!dbg       Jmid=.5*(JTS+JTE)
!
!INITIALIZE THE OUPUT OF GWD_COL  KWON
          DO K=KTS,KTE
            DUDTcol(IM,K)=0.
            DVDTcol(IM,K)=0.
          ENDDO
          DUsfc(IM)=0.             !-- U wind stress
          DVsfc(IM)=0.             !-- V wind stress


      DO J=JTS,JTE
        DO I=ITS,ITE
          if (kpbl(i,j)<kts .or. kpbl(i,j)>kte) go to 100
!
!-- Initial test to see if GWD calculations should be made, otherwise skip
!
          TERRtest=HZMAX(I,J)+SIGFAC*HSTDV(I,J)
          TERRmin=Z(I,2,J)-Z(I,1,J)
          IF (TERRtest < TERRmin) GO TO 100
!  ADDED BY KWON TO SKIP OCEAN POINTS
          IF (XLAND(I,J).GE.1.5)  GO TO 100
!
!-- For debugging:
!
!dbg lprnt=.false.
!dbg if (i==idbg .and. j==jdbg .and. itime<=1) lprnt=.true.
!dbg ! 200   CONTINUE
!
          DO K=KTS,KTE
            DUDTcol(IM,K)=0.
            DVDTcol(IM,K)=0.
!
!-- Transform/rotate winds from model to geodetic (Earth) coordinates
!
            Ucol(IM,K)=U(I,K,J)*CROT(I,J)+V(I,K,J)*SROT(I,J)
            Vcol(IM,K)=V(I,K,J)*CROT(I,J)-U(I,K,J)*SROT(I,J)
!
            Tcol(IM,K)=T(I,K,J)
            Qcol(IM,K)=Q(I,K,J)
!
!-- Convert from Pa to centibars, which is what's used in subroutine GWD_col
!
            DPcol(IM,K)=.001*DP(I,K,J)
            PINTcol(IM,K)=.001*PINT(I,K,J)
            Pcol(IM,K)=.001*PMID(I,K,J)
            EXNcol(IM,K)=EXNR(I,K,J)
!
!-- Next 2 fields are geopotential above the surface at the lower interface 
!   and at midlayer
!
            PHILIcol(IM,K)=G*(Z(I,K,J)-Z(I,1,J))
            PHIcol(IM,K)=GHALF*(Z(I,K,J)+Z(I,K+1,J))-G*Z(I,1,J)
          ENDDO   !- K
!
          PINTcol(IM,KTE+1)=.001*PINT(I,KTE+1,J)
          PHILIcol(IM,KTE+1)=G*(Z(I,KTE+1,J)-Z(I,1,J))
!
!-- Terrain-specific inputs:
!
          HPRIME(IM)=HSTDV(I,J)   !-- standard deviation of orography
          OC(IM)=HCNVX(I,J)       !-- Normalized convexity
          OA4(IM,1)=HASYW(I,J)    !-- orographic asymmetry in W-E plane
          OA4(IM,2)=HASYS(I,J)    !-- orographic asymmetry in S-N plane
          OA4(IM,3)=HASYSW(I,J)   !-- orographic asymmetry in SW-NE plane
          OA4(IM,4)=HASYNW(I,J)   !-- orographic asymmetry in NW-SE plane
          CLX4(IM,1)=HLENW(I,J)   !-- orographic length scale in W-E plane
          CLX4(IM,2)=HLENS(I,J)   !-- orographic length scale in S-N plane
          CLX4(IM,3)=HLENSW(I,J)  !-- orographic length scale in SW-NE plane
          CLX4(IM,4)=HLENNW(I,J)  !-- orographic length scale in NW-SE plane
          THETA(IM)=HANGL(I,J)       !
          SIGMA(IM)=HSLOP(I,J)       !
          GAMMA(IM)=HANIS(I,J)       !
          ELVMAX(IM)=HZMAX(I,J)      !
          LPBL(IM)=KPBL(I,J)      !
!dbg           IF (LPBL(IM)<KTS .OR. LPBL(IM)>KTE)     &
!dbg      & print *,'Wacky values for KPBL: I,J,N,LPBL=',I,J,ITIME,LPBL(IM)
!
!-- Output (diagnostics)
!
          DUsfc(IM)=0.             !-- U wind stress
          DVsfc(IM)=0.             !-- V wind stress
!
!dbg
!dbg if (LPRNT) then
!dbg !
!dbg !-- Following code is for ingesting GFS-derived inputs for final testing
!dbg !
!dbg   CALL INT_GET_FRESH_HANDLE(iunit)
!dbg   close(iunit)
!dbg   open(unit=iunit,file='gfs_gwd.input',form='formatted',iostat=ier)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (Ucol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (Vcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (Tcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (Qcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (PINTcol(im,k), k=kts,kte+1)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (DPcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (Pcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (EXNcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (PHILIcol(im,k), k=kts,kte+1)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (PHIcol(im,k), k=kts,kte)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) hprime(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) oc(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (oa4(im,k), k=1,4)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) (clx4(im,k), k=1,4)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) theta(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) sigma(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) gamma(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) elvmax(im)
!dbg   read(iunit,*)    ! skip line
!dbg   read(iunit,*) lpbl(im)
!dbg   close(iunit)
write(label,"('GWD:i,j,n=',2i5,i6)") I,J,ITIME
!dbg   write(6,"(2a)") LABEL,' in GWD_driver: K U V T Q Pi DP P EX PHIi PHI'
!dbg   do k=kts,kte
!dbg     write(6,"(i3,10e12.4)") k,Ucol(im,k),Vcol(im,k),Tcol(im,k)          &
!dbg     ,Qcol(im,k),PINTcol(im,k),DPcol(im,k),Pcol(im,k),EXNcol(im,k)  &
!dbg     ,PHILIcol(im,k),PHIcol(im,k)
!dbg   enddo
!dbg   write(6,"(2(a,e12.4),2(a,4e12.4/),4(a,e12.4),a,i3) )")         &
!dbg    'GWD_driver:  hprime(im)=',hprime(im),'  oc(im)=',oc(im)    &
!dbg   ,'  oa4(im,1-4)=',(oa4(im,k),k=1,4)                          &
!dbg   ,'  clx4(im,1-4)=',(clx4(im,k),k=1,4)                        &
!dbg   ,'  theta=',theta(im),'  sigma(im)=',sigma(im)               &
!dbg   ,'  gamma(im)=',gamma(im),'  elvmax(im)=',elvmax(im)         &
!dbg   ,'  lpbl(im)=',lpbl(im)
!dbg endif
!dbg
!=======================================================================
!
          CALL GWD_col(DVDTcol,DUDTcol, DUsfc,DVsfc                     & ! Output
     &,              Ucol,Vcol,Tcol,Qcol,PINTcol,DPcol,Pcol,EXNcol      & ! Met input
     &,              PHILIcol,PHIcol                                    & ! Met input
     &,              HPRIME,OC,OA4,CLX4,THETA,SIGMA,GAMMA,ELVMAX        & ! Topo input
     &,              LPBL,IM,KTS,KTE,LABEL,LPRNT )                        ! Indices + debugging
!
!=======================================================================
!
!dbg
!dbg !
!dbg ! IF (.NOT.LPRNT) THEN
!dbg !   TEST=0.
!dbg !   DO K=KTS,KTE
!dbg !     TEST=MAX( TEST, ABS(DUDTcol(IM,K)), ABS(DVDTcol(IM,K)) )
!dbg !     if ( ABS(DUDTcol(IM,K)) > RDELTIM) print *,'k,DUDTcol=',k,DUDTcol(IM,K)
!dbg !     if ( ABS(DVDTcol(IM,K)) > RDELTIM) print *,'k,DVDTcol=',k,DVDTcol(IM,K)
!dbg !   ENDDO
!dbg !   IF (TEST>RDELTIM) THEN
!dbg !     LPRNT=.TRUE.
!dbg !     Imid=I
!dbg !     Jmid=J
!dbg !     GO TO 200
!dbg !   ENDIF
!dbg ! ENDIF
!dbg ! TEST=ABS(DUsfc(IM))+ABS(DVsfc(IM))
!dbg ! if (.not.lprnt) then
!dbg !   do k=kts,kte
!dbg !     du=DUDTcol(IM,K)*DELTIM
!dbg !     dv=DVDTcol(IM,K)*DELTIM
!dbg !     if (du < dumin) then
!dbg !       lprnt=.true.
!dbg !       dumin=1.5*du
!dbg !     endif
!dbg !     if (du > dumax) then
!dbg !       lprnt=.true.
!dbg !       dumax=1.5*du
!dbg !     endif
!dbg !     if (dv < dvmin) then
!dbg !       lprnt=.true.
!dbg !       dvmin=1.5*dv
!dbg !     endif
!dbg !     if (dv > dvmax) then
!dbg !       lprnt=.true.
!dbg !       dvmax=1.5*dv
!dbg !     endif
!dbg !   enddo
!dbg !   if (lprnt) go to 200
!dbg ! else
!dbg if (lprnt) then
!dbg   print *,'DUsfc,DVsfc,CROT,SROT,DELTIM=',DUsfc(IM),DVsfc(IM) &
!dbg   &,CROT(I,J),SROT(I,J),DELTIM
!dbg   print *,' K  P     |     Ucol      Ugeo  DUDTcol*DT   U        Umod    DU=DUDT*DT ' &
!dbg                    ,'|     Vcol      Vgeo  DVDTcol*DT   V        Vmod    DV=DVDT*DT'
!dbg ENDIF

          DO K=KTS,KTE
            TEST=ABS(DUDTcol(IM,K))+ABS(DVDTcol(IM,K))
            IF (TEST > THRESH) THEN

!dbg
!dbg if (lprnt) then
!dbg !dbg  DUDTcol(IM,K)=0.   !-- Test rotation
!dbg !dbg  DVDTcol(IM,K)=0.   !-- Test rotation
!dbg !-- Now replace with the original winds before they were written over by
!dbg !   the values from the GFS
!dbg   Ucol(IM,K)=U(I,K,J)*CROT(I,J)+V(I,K,J)*SROT(I,J)
!dbg   Vcol(IM,K)=V(I,K,J)*CROT(I,J)-U(I,K,J)*SROT(I,J)
!dbg endif

!
!-- First update winds in geodetic coordinates
!
              Ugeo=Ucol(IM,K)+DUDTcol(IM,K)*DELTIM
              Vgeo=Vcol(IM,K)+DVDTcol(IM,K)*DELTIM
!
!-- Transform/rotate winds from geodetic back to model coordinates
!
              Umod=Ugeo*CROT(I,J)-Vgeo*SROT(I,J)
              Vmod=Ugeo*SROT(I,J)+Vgeo*CROT(I,J)
!
!-- Calculate wind tendencies from the updated model winds
!
              DUDT(I,K,J)=RDELTIM*(Umod-U(I,K,J))
              DVDT(I,K,J)=RDELTIM*(Vmod-V(I,K,J))
!
!dbg

test=abs(dudt(i,k,j))+abs(dvdt(i,k,j))
if (test > dtlarge) write(6,"(2a,i2,2(a,e12.4))")  &
label,' => k=',k,'  dudt=',dudt(i,k,j),'  dvdt=',dvdt(i,k,j)

!dbg if (lprnt) write(6,"(i2,f8.2,2(' | ',6f10.4)")  K,10.*Pcol(IM,K)  &
!dbg ,Ucol(IM,K),Ugeo,DUDTcol(IM,K)*DELTIM,U(I,K,J),Umod,DUDT(I,K,J)*DELTIM    &
!dbg ,Vcol(IM,K),Vgeo,DVDTcol(IM,K)*DELTIM,V(I,K,J),Vmod,DVDT(I,K,J)*DELTIM
!dbg
            ENDIF     !- IF (TEST > THRESH) THEN
!
          ENDDO   !- K
!
!-- Transform/rotate surface wind stresses from geodetic to model coordinates
!
          UGWDsfc(I,J)=DUsfc(IM)*CROT(I,J)-DVsfc(IM)*SROT(I,J)
          VGWDsfc(I,J)=DUsfc(IM)*SROT(I,J)+DVsfc(IM)*CROT(I,J)
!
100       CONTINUE
        ENDDO     !- I
      ENDDO       !- J
!
      END SUBROUTINE GWD_driver
!
!-----------------------------------------------------------------------
!
!-- "A", "B" (from GFS) in GWD_col are DVDTcol, DUDTcol, respectively in GWD_driver
!
      SUBROUTINE GWD_col (A,B, DUsfc,DVsfc                              &  !-- Output
     &, U1,V1,T1,Q1, PRSI,DEL,PRSL,PRSLK, PHII,PHIL                     &  !-- Met inputs
     &, HPRIME,OC,OA4,CLX4,THETA,SIGMA,GAMMA,ELVMAX                     &  !-- Topo inputs
     &, KPBL,IM,KTS,KTE, LABEL,LPRNT )                                     !-- Input indices, debugging
!
!=== Output fields
!
!-- A (DUDT), B (DVDT) - output zonal & meridional wind tendencies in Earth coordinates (m s^-2)
!-- DUsfc, DVsfc - surface zonal meridional wind stresses in Earth coordinates (m s^-1?)
!
!=== Input fields
!
!-- U1, V1 - zonal, meridional wind (m/s)
!-- T1 - temperature (deg K)
!-- Q1 - specific humidity (kg/kg)
!-- PRSI - lower interface pressure in centibars (1000 Pa)
!-- DEL - pressure thickness of layer in centibars (1000 Pa)
!-- PRSL - midlayer pressure in centibars (1000 Pa)
!-- PRSLK - Exner function, (P/P0)**(Rd/CP)
!-- PHII - lower interface geopotential in mks units
!-- PHIL - midlayer geopotential in mks units
!-- KDT - number of time steps into integration for diagnostics
!-- HPRIME - orographic standard deviation
!-- OC - normalized 4th moment of the orographic convexity
!-- OA4 - orographic asymmetry for upstream & downstream flow measured 
!         along 4 vertical planes (W-E, S-N, SW-NE, NW-SE)
!-- CLX4 - orographic length scale or mountain width measured along
!          4 vertical planes (W-E, S-N, SW-NE, NW-SE)
!-- THETA - angle of the mountain range w/r/t east
!-- SIGMA - slope of orography
!-- GAMMA - anisotropy/aspect ratio
!-- ELVMAX - max height above mean orography
!-- KPBL(IM) - vertical index at the top of the PBL
!-- KM - number of vertical levels
!
!== For diagnostics
!-- LABEL - character string for diagnostic prints
!-- LPRNT - logical flag for prints
!
!#######################################################################
!##################  ORIGINAL DOCUMENTATION BLOCK  #####################
!######  The following comments are from the original GFS code  ########
!#######################################################################
!   ********************************************************************
! ----->  I M P L E M E N T A T I O N    V E R S I O N   <----------
!
!          --- Not in this code --  History of GWDP at NCEP----
!              ----------------     -----------------------
!  VERSION 3  MODIFIED FOR GRAVITY WAVES, LOCATION: .FR30(V3GWD)  *J*
!---       3.1 INCLUDES VARIABLE SATURATION FLUX PROFILE CF ISIGST
!---       3.G INCLUDES PS COMBINED W/ PH (GLAS AND GFDL)
!-----         ALSO INCLUDED IS RI  SMOOTH OVER A THICK LOWER LAYER
!-----         ALSO INCLUDED IS DECREASE IN DE-ACC AT TOP BY 1/2
!-----     THE NMC GWD INCORPORATING BOTH GLAS(P&S) AND GFDL(MIGWD)
!-----        MOUNTAIN INDUCED GRAVITY WAVE DRAG 
!-----    CODE FROM .FR30(V3MONNX) FOR MONIN3
!-----        THIS VERSION (06 MAR 1987)
!-----        THIS VERSION (26 APR 1987)    3.G
!-----        THIS VERSION (01 MAY 1987)    3.9
!-----    CHANGE TO FORTRAN 77 (FEB 1989)     --- HANN-MING HENRY JUANG
!----- 
!
!   VERSION 4
!                ----- This code -----
!
!-----   MODIFIED TO IMPLEMENT THE ENHANCED LOW TROPOSPHERIC GRAVITY
!-----   WAVE DRAG DEVELOPED BY KIM AND ARAKAWA(JAS, 1995).
!        Orographic Std Dev (hprime), Convexity (OC), Asymmetry (OA4)
!        and Lx (CLX4) are input topographic statistics needed.
!
!-----   PROGRAMMED AND DEBUGGED BY HONG, ALPERT AND KIM --- JAN 1996.
!-----   debugged again - moorthi and iredell --- may 1998.
!-----
!       Further Cleanup, optimization and modification
!                                       - S. Moorthi May 98, March 99.
!-----   modified for usgs orography data (ncep office note 424)
!        and with several bugs fixed  - moorthi and hong --- july 1999.
!
!-----   Modified & implemented into NRL NOGAPS
!                                       - Young-Joon Kim, July 2000
!-----
!   VERSION lm MB  (6): oz fix 8/2003
!                ----- This code -----
!
!------   Changed to include the Lott and Miller Mtn Blocking
!         with some modifications by (*j*)  4/02
!        From a Principal Coordinate calculation using the
!        Hi Res 8 minute orography, the Angle of the
!        mtn with that to the East (x) axis is THETA, the slope
!        parameter SIGMA. The anisotropy is in GAMMA - all  are input
!        topographic statistics needed.  These are calculated off-line
!        as a function of model resolution in the fortran code ml01rg2.f,
!        with script mlb2.sh.   (*j*)
!-----   gwdps_mb.f version (following lmi) elvmax < hncrit (*j*)
!        MB3a expt to enhance elvmax mtn hgt see sigfac & hncrit
!-----
!----------------------------------------------------------------------C
!==== Below in "!GFS " are the original subroutine call and comments from 
!     /nwprod/sorc/global_fcst.fd/gwdps_v.f as of April 2007
!GFS       SUBROUTINE GWDPS(IM,IX,IY,KM,A,B,U1,V1,T1,Q1,KPBL,
!GFS      &               PRSI,DEL,PRSL,PRSLK,PHII, PHIL,RCL,DELTIM,KDT,
!GFS      &               HPRIME,OC,OA4,CLX4,THETA,SIGMA,GAMMA,ELVMAX,
!GFS      &               DUsfc,DVsfc,G, CP, RD, RV, IMX,
!GFS      &               nmtvr, me, lprnt, ipr)
!GFS !------------------------------------------------------------------
!GFS !    USE
!GFS !        ROUTINE IS CALLED FROM GBPHYS  (AFTER CALL TO MONNIN)
!GFS !
!GFS !    PURPOSE
!GFS !        USING THE GWD PARAMETERIZATIONS OF PS-GLAS AND PH-
!GFS !        GFDL TECHNIQUE.  THE TIME TENDENCIES OF U V
!GFS !        ARE ALTERED TO INCLUDE THE EFFECT OF MOUNTAIN INDUCED
!GFS !        GRAVITY WAVE DRAG FROM SUB-GRID SCALE OROGRAPHY INCLUDING
!GFS !        CONVECTIVE BREAKING, SHEAR BREAKING AND THE PRESENCE OF
!GFS !        CRITICAL LEVELS
!GFS !
!GFS !  INPUT
!GFS !        A(IY,KM)  NON-LIN TENDENCY FOR V WIND COMPONENT
!GFS !        B(IY,KM)  NON-LIN TENDENCY FOR U WIND COMPONENT
!GFS !        U1(IX,KM) ZONAL WIND / SQRT(RCL)  M/SEC  AT T0-DT
!GFS !        V1(IX,KM) MERIDIONAL WIND / SQRT(RCL) M/SEC AT T0-DT
!GFS !        T1(IX,KM) TEMPERATURE DEG K AT T0-DT
!GFS !        Q1(IX,KM) SPECIFIC HUMIDITY AT T0-DT
!GFS !
!GFS !        RCL     A scaling factor = RECIPROCAL OF SQUARE OF COS(LAT)
!GFS !                FOR MRF GFS.  
!GFS !        DELTIM  TIME STEP    SECS
!GFS !        SI(N)   P/PSFC AT BASE OF LAYER N
!GFS !        SL(N)   P/PSFC AT MIDDLE OF LAYER N
!GFS !        DEL(N)  POSITIVE INCREMENT OF P/PSFC ACROSS LAYER N
!GFS !        KPBL(IM) is the index of the top layer of the PBL
!GFS !        ipr & lprnt for diagnostics
!GFS !
!GFS !  OUTPUT
!GFS !        A, B    AS AUGMENTED BY TENDENCY DUE TO GWDPS
!GFS !                OTHER INPUT VARIABLES UNMODIFIED.
!GFS !   ********************************************************************
!
      IMPLICIT NONE
!
!-- INPUT:
!
      INTEGER, INTENT(IN) :: IM,KTS,KTE
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,KTS:KTE) ::        &
     &                                 U1,V1,T1,Q1,DEL,PRSL,PRSLK,PHIL
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,KTS:KTE+1) ::      &
     &                                                       PRSI,PHII
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,4) :: OA4,CLX4
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM) ::                &
     &                              HPRIME,OC,THETA,SIGMA,GAMMA,ELVMAX
      INTEGER, INTENT(IN), DIMENSION(IM) :: KPBL
      CHARACTER (LEN=26), INTENT(IN) :: LABEL
      LOGICAL, INTENT(IN) :: LPRNT
!
!-- OUTPUT:
!
      REAL(kind=kind_phys), INTENT(INOUT), DIMENSION(IM,KTS:KTE) :: A,B
      REAL(kind=kind_phys), INTENT(INOUT), DIMENSION(IM) :: DUsfc,DVsfc
!
!-----------------------------------------------------------------------
!-- LOCAL variables:
!-----------------------------------------------------------------------
!
!     Some constants
!
!
      REAL(kind=kind_phys), PARAMETER :: PI=3.1415926535897931        &
     &,        G=9.806, CP=1004.6, RD=287.04, RV=461.6                &
     &,        FV=RV/RD-1., RDI=1./RD, GOR=G/RD, GR2=G*GOR, GOCP=G/CP &
     &,        ROG=1./G, ROG2=ROG*ROG                                 &
     &,        DW2MIN=1., RIMIN=-100., RIC=0.25, BNV2MIN=1.0E-5       &
     &,        EFMIN=0.0, EFMAX=10.0, hpmax=200.0                     & ! or hpmax=2500.0
     &,        FRC=1.0, CE=0.8, CEOFRC=CE/FRC, frmax=100.             &
     &,        CG=0.5, GMAX=1.0, CRITAC=5.0E-4, VELEPS=1.0            &
     &,        FACTOP=0.5, RLOLEV=500.0, HZERO=0., HONE=1.            & ! or RLOLEV=0.5
     &,        HE_4=.0001, HE_2=.01                                   & 
!
!-- Lott & Miller mountain blocking => aka "lm mtn blocking"
!
     &,  cdmb = 1.0        &    ! non-dim sub grid mtn drag Amp (*j*)
!  hncrit set to 8000m and sigfac added to enhance elvmax mtn hgt
     &,  hncrit=8000.      &    ! Max value in meters for ELVMAX (*j*)
!module top!     &,  sigfac=3.0        &    ! MB3a expt test for ELVMAX factor (*j*)  => control value is 0.1
!module top    &,  sigfac=0.         &    ! MB3a expt test for ELVMAX factor (*j*)  => control value is 0.1
     &,  hminmt=50.        &    ! min mtn height (*j*)
     &,  hstdmin=25.       &    ! min orographic std dev in height
     &,  minwnd=0.1        &    ! min wind component (*j*)
     &,  dpmin=5.0              ! Minimum thickness of the reference layer (centibars)
                                ! values of dpmin=0, 20 have also been used
!
      integer, parameter :: mdir=8
      real(kind=kind_phys), parameter :: FDIR=mdir/(PI+PI)
!
!-- Template:
!             NWD  1   2   3   4   5   6   7   8
!              WD  W   S  SW  NW   E   N  NE  SE
!
      integer,save :: nwdir(mdir)
      data nwdir /6,7,5,8,2,3,1,4/
!
      LOGICAL ICRILV(IM)
!
!----   MOUNTAIN INDUCED GRAVITY WAVE DRAG
!
!
! for lm mtn blocking
      real(kind=kind_phys), DIMENSION(IM) :: WK,PE,EK,ZBK,UP,TAUB,XN    &
     & ,YN,UBAR,VBAR,ULOW,OA,CLX,ROLL,ULOI,DTFAC,XLINV,DELKS,DELKS1     &
     & ,SCOR,BNV2bar, ELEVMX   ! ,PSTAR
!
      real(kind=kind_phys), DIMENSION(IM,KTS:KTE) ::                    &
     &                      BNV2LM,DB,ANG,UDS,BNV2,RI_N,TAUD,RO,VTK,VTJ
      real(kind=kind_phys), DIMENSION(IM,KTS:KTE-1) :: VELCO
      real(kind=kind_phys), DIMENSION(IM,KTS:KTE+1) :: TAUP
      real(kind=kind_phys), DIMENSION(KTE-1) :: VELKO
!
      integer, DIMENSION(IM) ::                                         &
     &                 kref,kint,iwk,iwk2,ipt,kreflm,iwklm,iptlm,idxzb
!
! for lm mtn blocking
!
      real(kind=kind_phys) :: ZLEN, DBTMP, R, PHIANG, DBIM              &
     &,                   xl,     rcsks, bnv,   fr                      &
     &,                   brvf,   cleff, tem,   tem1,  tem2, temc, temv &
     &,                   wdir,   ti,    rdz,   dw2,   shr2, bvf2       &
     &,                   rdelks, wtkbj, efact, coefm, gfobnv           &
     &,                   scork,  rscor, hd,    fro,   rim,  sira       &
     &,                   dtaux,  dtauy, pkp1log, pklog
!
      integer :: ncnt, kmm1, kmm2, lcap, lcapp1, kbps, kbpsp1,kbpsm1    &
     &, kmps, kmpsp1, idir, nwd, i, j, k, klcap, kp1, kmpbl, npt, npr   &
     &, idxm1, ktrial, klevm1, kmll,kmds, KM                            &
!     &, ihit,jhit                                                       &
     &, ME              !-- processor element for debugging

real :: rcl,rcs  !dbg

!
!-----------------------------------------------------------------------
!
      KM = KTE
      npr = 0
      DO I = 1, IM
         DUsfc(I) = 0.
         DVsfc(I) = 0.
!
!-- ELEVMX is a local array that could be changed below
!
         ELEVMX(I) = ELVMAX(I)
      ENDDO
!
!-- Note that A, B already set to zero as DUDTcol, DVDTcol in subroutine GWD_driver
!
      ipt = 0
      npt = 0
      IF (NMTVR >= 14) then 
        DO I = 1,IM
          IF (elvmax(i) > HMINMT .AND. hprime(i) > HE_4) then
             npt = npt + 1
             ipt(npt) = i
          ENDIF
        ENDDO
      ELSE
        DO I = 1,IM
          IF (hprime(i) > HE_4) then
            npt = npt + 1
            ipt(npt) = i
          ENDIF
        ENDDO
      ENDIF    !-- IF (NMTVR >= 14) then 
!

!dbg
rcl=1.
rcs=1.
!dbg if (lprnt) then
!dbg !-- Match what's in the GFS:
!dbg !dbg  rcl=1.53028780126139008   ! match GFS point at 36N
!dbg !dbg  rcs=sqrt(rcl)
!dbg   i=im
!dbg   write(6,"(a,a)") LABEL,' in GWD_col: K U V T Q Pi DP P EX PHIi PHI'
!dbg   do k=kts,kte
!dbg     write(6,"(i3,10e12.4)") k,U1(i,k),V1(i,k),T1(i,k),Q1(i,k),PRSI(i,k)   &
!dbg     ,DEL(i,k),PRSL(i,k),PRSLK(i,k),PHII(i,k),PHIL(i,k)
!dbg   enddo
!dbg   write(6,"(2(a,e12.4),2(a,4e12.4/),4(a,e12.4),a,i3) )")   &
!dbg    'GWD_col:  hprime(i)=',hprime(i),'  oc(i)=',oc(i)       &
!dbg   ,'  oa4(i,1-4)=',(oa4(i,k),k=1,4)                        &
!dbg   ,'  clx4(i,1-4)=',(clx4(i,k),k=1,4)                      &
!dbg   ,'  theta(i)=',theta(i),'  sigma(i)=',sigma(i)           &
!dbg   ,'  gamma(i)=',gamma(i),'  elvmax(i)=',elvmax(i)         &
!dbg   ,'  lpbl(i)=',kpbl(i)
!dbg endif
!dbg if (lprnt) CALL WRF_GET_MYPROC(ME)

!
!-- Note important criterion for immediately exiting routine!
!
      IF (npt <= 0) RETURN     ! No gwd/mb calculation done!
!
      do i=1,npt
        IDXZB(i) = 0
      enddo
!
      DO K = 1, KM
      DO I = 1, IM
      DB(I,K) = 0.
      ANG(I,K) = 0.
      UDS(I,K) = 0.
      ENDDO
      ENDDO
!
!
!     NCNT   = 0
      KMM1   = KM - 1
      KMM2   = KM - 2
      LCAP   = KM
      LCAPP1 = LCAP + 1
!
!
      IF (NMTVR .eq. 14) then 
! ----  for lm and gwd calculation points
!
! --- iwklm is the level above the height of the mountain.
! --- idxzb is the level of the dividing streamline.
! INITIALIZE DIVIDING STREAMLINE (DS) CONTROL VECTOR
!
        do i=1,npt
          iwklm(i) = 2
          kreflm(i) = 0
        enddo
!
!
! start lm mtn blocking (mb) section
!
!..............................
!..............................
!
!  (*j*)  11/03:  test upper limit on KMLL=km - 1
!      then do not need hncrit -- test with large hncrit first.
!       KMLL  = km / 2 ! maximum mtnlm height : # of vertical levels / 2
        KMLL = kmm1
! --- No mtn should be as high as KMLL (so we do not have to start at 
! --- the top of the model but could do calc for all levels).
!

!dbg
!dbg if (lprnt) print *,'k   pkp1log   pklog    vtj(i,k)     vtk(i,k)    ro(i,k)'

        DO I = 1, npt
          j = ipt(i)
          ELEVMX(J) = min (ELEVMX(J) + sigfac * hprime(j), hncrit)

!dbg
!dbg if (lprnt) print *,'k=',k,'  elevmx(j)=',elevmx(j),'  elvmax(j)=',elvmax(j)  &
!dbg ,'  sigfac*hprime(j)=',sigfac*hprime(j)

        ENDDO

        DO K = 1,KMLL
          DO I = 1, npt
            j = ipt(i)
! --- interpolate to max mtn height for index, iwklm(I) wk[gz]
! --- ELEVMX is limited to hncrit because to hi res topo30 orog.
            pkp1log =  phil(j,k+1) * ROG
            pklog =  phil(j,k) * ROG
            if ( ( ELEVMX(j) .le.  pkp1log ) .and.                      &
     &           ( ELEVMX(j) .ge.   pklog  ) ) THEN
! ---        wk for diags but can be saved and reused.  
               wk(i)  = G * ELEVMX(j) / ( phil(j,k+1) - phil(j,k) )
               iwklm(I)  =  MAX(iwklm(I), k+1 ) 

!dbg if (lprnt) print *,'k,wk(i),iwklm(i)=',k,wk(i),iwklm(i)    !dbg

            endif
!
! ---        find at prsl levels large scale environment variables
! ---        these cover all possible mtn max heights
            VTJ(I,K)  = T1(J,K)  * (1.+FV*Q1(J,K))  ! virtual temperature
            VTK(I,K)  = VTJ(I,K) / PRSLK(J,K)       ! potential temperature
            RO(I,K)   = RDI * PRSL(J,K) / VTJ(I,K)  ! DENSITY (1.e-3 kg m^-3)

!dbg if (lprnt) write(6,"(i2,5e12.4)") k,pkp1log,pklog,vtj(i,k),vtk(i,k),ro(i,k)   !dbg

          ENDDO    !-- DO I = 1, npt
!
        ENDDO      !-- DO K = 1,KMLL
!
! testing for highest model level of mountain top
!
!         ihit = 2
!         jhit = 0
!        do i = 1, npt
!        j=ipt(i)
!          if ( iwklm(i) .gt. ihit ) then 
!            ihit = iwklm(i)
!            jhit = j
!          endif
!        enddo
!     if (lprnt) print *, ' mb: kdt,max(iwklm),jhit,phil,me=',   &
!    &          kdt,ihit,jhit,phil(jhit,ihit),me
!        
!dbg if (lprnt) print *,'k    rdz    bnv2lm(i,k)'   !dbg
        klevm1 = KMLL - 1
        DO K = 1, klevm1  
          DO I = 1, npt
           j   = ipt(i)
            RDZ  = g   / ( phil(j,k+1) - phil(j,k) )
! ---                               Brunt-Vaisala Frequency
            BNV2LM(I,K) = (G+G) * RDZ * ( VTK(I,K+1)-VTK(I,K) )         &
     &                     / ( VTK(I,K+1)+VTK(I,K) )
            bnv2lm(i,k) = max( bnv2lm(i,k), bnv2min )

!dbg if (lprnt) write(6,"(i2,2e12.4)") k,rdz,bnv2lm(i,k)   !dbg

          ENDDO
        ENDDO
!
        DO I = 1, npt
          J   = ipt(i)
          DELKS(I)  = 1.0 / (PRSI(J,1) - PRSI(J,iwklm(i)))
          DELKS1(I) = 1.0 / (PRSL(J,1) - PRSL(J,iwklm(i)))
          UBAR (I)  = 0.0
          VBAR (I)  = 0.0
          ROLL (I)  = 0.0
          PE   (I)  = 0.0
          EK   (I)  = 0.0
          BNV2bar(I) = (PRSL(J,1)-PRSL(J,2)) * DELKS1(I) * BNV2LM(I,1)
        ENDDO
!
! --- find the dividing stream line height 
! --- starting from the level above the max mtn downward
! --- iwklm(i) is the k-index of mtn elevmx elevation
!
        DO Ktrial = KMLL, 1, -1
          DO I = 1, npt
             IF ( Ktrial .LT. iwklm(I) .and. kreflm(I) .eq. 0 ) then
                kreflm(I) = Ktrial

if (lprnt) print *,'Ktrial,iwklm(i),kreflm(i)=',Ktrial,iwklm(i),kreflm(I)

             ENDIF
          ENDDO
        ENDDO
!
! --- in the layer kreflm(I) to 1 find PE (which needs N, ELEVMX)
! ---  make averages, guess dividing stream (DS) line layer.
! ---  This is not used in the first cut except for testing and
! --- is the vert ave of quantities from the surface to mtn top.
!   

!dbg
!dbg if (lprnt) print *,' k      rdelks      ubar        vbar     roll       ' &
!dbg ,'bnv2bar      rcsks       rcs'

        DO I = 1, npt
          DO K = 1, Kreflm(I)
            J        = ipt(i)
            RDELKS     = DEL(J,K) * DELKS(I)

!dbg
            RCSKS      = RCS      * RDELKS
            UBAR(I)    = UBAR(I)  + RCSKS  * U1(J,K) ! trial Mean U below 
            VBAR(I)    = VBAR(I)  + RCSKS  * V1(J,K) ! trial Mean V below 

            ROLL(I)    = ROLL(I)  + RDELKS * RO(I,K) ! trial Mean RO below 
            RDELKS     = (PRSL(J,K)-PRSL(J,K+1)) * DELKS1(I)
            BNV2bar(I) = BNV2bar(I) + BNV2lm(I,K) * RDELKS
! --- these vert ave are for diags, testing and GWD to follow (*j*).

!dbg
!dbg if (lprnt) write(6,"(i2,7e12.4)") k,rdelks,ubar(i),vbar(i),roll(i)  &
!dbg ,bnv2bar(i),rcsks,rcs

          ENDDO
        ENDDO

!dbg
!dbg if (lprnt) print *, 'kreflm(npt)=',kreflm(npt)  &
!dbg ,'  bnv2bar(npt)=',bnv2bar(npt)  &
!dbg ,'  ubar(npt)=',ubar(npt)  &
!dbg ,'  vbar(npt)=',vbar(npt)  &
!dbg ,'  roll(npt)=',roll(npt)  &
!dbg ,'  delks(npt)=',delks(npt)  &
!dbg ,'  delks1(npt)=',delks1(npt)

!
! --- integrate to get PE in the trial layer.
! --- Need the first layer where PE>EK - as soon as 
! --- IDXZB is not 0 we have a hit and Zb is found.
!
        DO I = 1, npt
          J = ipt(i)

!dbg
!dbg if (lprnt) print *,'k   phiang    u1(j,k)     v1(j,k)     theta(j)'   &
!dbg ,'     ang(i,k)     uds(i,k)      pe(i)     up(i)     ek(i)'

          DO K = iwklm(I), 1, -1
            PHIANG   =  RAD_TO_DEG*atan2(V1(J,K),U1(J,K))
            ANG(I,K) = ( THETA(J) - PHIANG )
            if ( ANG(I,K) .gt.  90. ) ANG(I,K) = ANG(I,K) - 180.
            if ( ANG(I,K) .lt. -90. ) ANG(I,K) = ANG(I,K) + 180.
!
!dbg            UDS(I,K) =                                                  &
            UDS(I,K) = rcs*                                             &
      &          MAX(SQRT(U1(J,K)*U1(J,K) + V1(J,K)*V1(J,K)), minwnd)
! --- Test to see if we found Zb previously
            IF (IDXZB(I) .eq. 0 ) then
              PE(I) = PE(I) + BNV2lm(I,K) *                             &
     &           ( G * ELEVMX(J) - phil(J,K) ) *                        &
     &           ( PHII(J,K+1) - PHII(J,K) ) * ROG2

!dbg
!dbg     &           ( PHII(J,K+1) - PHII(J,K) ) / (G*G)
!dbg if (lprnt) print *,   &
!dbg 'k=',k,'  pe(i)=',pe(i),'  bnv2lm(i,k)=',bnv2lm(i,k)  &
!dbg ,'  g=',g,'  elevmx(j)=',elevmx(j)  &
!dbg ,'  g*elevmx(j)-phil(j,k)=',g*elevmx(j)-phil(j,k)  &
!dbg ,'  (phii(j,k+1)-phi(j,k))/(g*g)=',(PHII(J,K+1)-PHII(J,K) )*ROG2

! --- KE
! --- Wind projected on the line perpendicular to mtn range, U(Zb(K)).
! --- kinetic energy is at the layer Zb
! --- THETA ranges from -+90deg |_ to the mtn "largest topo variations"
              UP(I)  =  UDS(I,K) * cos(DEG_TO_RAD*ANG(I,K))
              EK(I)  = 0.5 *  UP(I) * UP(I) 

! --- Dividing Stream lime  is found when PE =exceeds EK.
              IF ( PE(I) .ge.  EK(I) ) IDXZB(I) = K
! --- Then mtn blocked flow is between Zb=k(IDXZB(I)) and surface
!
            ENDIF     !-- IF (IDXZB(I) .eq. 0 ) then

!dbg
!dbg if (lprnt) write(6,"(i2,9e12.4)")   &
!dbg k,phiang,u1(j,k),v1(j,k),theta(j),ang(i,k),uds(i,k),pe(i),up(i),ek(i)

          ENDDO       !-- DO K = iwklm(I), 1, -1
        ENDDO         !-- DO I = 1, npt
!
        DO I = 1, npt
          J    = ipt(i)
! --- Calc if N constant in layers (Zb guess) - a diagnostic only.
          ZBK(I) =  ELEVMX(J) - SQRT(UBAR(I)**2 + VBAR(I)**2)/BNV2bar(I)
        ENDDO
!

!dbg
!dbg if (lprnt) print *,'iwklm=',iwklm(npt),'  ZBK=',ZBK(npt)

!
! --- The drag for mtn blocked flow
! 

!dbg
!dbg if (lprnt) print *,'k   phil(j,k)  g*hprime(j)   '   &
!dbg ,'phil(j,idxzb(i))-phil(j,k)    zlen   r    dbtmp    db(i,k)'

!
        DO I = 1, npt
          J = ipt(i)
          ZLEN = 0.
          IF ( IDXZB(I) .gt. 0 ) then 
            DO K = IDXZB(I), 1, -1
              IF (PHIL(J,IDXZB(I)) > PHIL(J,K)) THEN
                ZLEN = SQRT( ( PHIL(J,IDXZB(I))-PHIL(J,K) ) /           &
     &                       ( PHIL(J,K ) + G * hprime(J) ) )
! --- lm eq 14:
                R = (cos(DEG_TO_RAD*ANG(I,K))**2 + GAMMA(J) * sin(DEG_TO_RAD*ANG(I,K))**2) / &
     &              (gamma(J) * cos(DEG_TO_RAD*ANG(I,K))**2 + sin(DEG_TO_RAD*ANG(I,K))**2)
! --- (negative of DB -- see sign at tendency)
                DBTMP = 0.25 *  CDmb *                                  &
     &                  MAX( 2. - 1. / R, HZERO ) * sigma(J) *          &
     &                  MAX(cos(DEG_TO_RAD*ANG(I,K)), gamma(J)*sin(DEG_TO_RAD*ANG(I,K))) *  &
     &                  ZLEN / hprime(J) 
                DB(I,K) =  DBTMP * UDS(I,K)    
!

!dbg
!dbg if (lprnt) write(6,"(i3,7e12.4)")  &
!dbg k,phil(j,k),g*hprime(j),phil(j,idxzb(i))-phil(j,k),zlen,r,dbtmp,db(i,k)

!
              ENDIF        !-- IF (PHIL(J,IDXZB(I)) > PHIL(J,K) .AND. DEN > 0.) THEN
            ENDDO          !-- DO K = IDXZB(I), 1, -1
          endif
        ENDDO              !-- DO I = 1, npt
!
!.............................
!.............................
! end  mtn blocking section
!
      ENDIF      !-- IF ( NMTVR .eq. 14) then 
!
!.............................
!.............................
!
      KMPBL  = km / 2 ! maximum pbl height : # of vertical levels / 2
!
!  Scale cleff between IM=384*2 and 192*2 for T126/T170 and T62
!
      if (imx .gt. 0) then
!       cleff = 1.0E-5 * SQRT(FLOAT(IMX)/384.0) !  this is inverse of CLEFF!
!       cleff = 1.0E-5 * SQRT(FLOAT(IMX)/192.0) !  this is inverse of CLEFF!
        cleff = 0.5E-5 * SQRT(FLOAT(IMX)/192.0) !  this is inverse of CLEFF!
!       cleff = 2.0E-5 * SQRT(FLOAT(IMX)/192.0) !  this is inverse of CLEFF!
!       cleff = 2.5E-5 * SQRT(FLOAT(IMX)/192.0) !  this is inverse of CLEFF!
      endif

!dbg
!dbg if (lprnt) print *,'imx, cleff, rcl, rcs=',imx,cleff,rcl,rcs
!dbg if (lprnt) print *,' k    vtj       vtk       ro'

      DO K = 1,KM
        DO I =1,npt
          J         = ipt(i)
          VTJ(I,K)  = T1(J,K)  * (1.+FV*Q1(J,K))
          VTK(I,K)  = VTJ(I,K) / PRSLK(J,K)
          RO(I,K)   = RDI * PRSL(J,K) / VTJ(I,K)  ! DENSITY 
          TAUP(I,K) = 0.0

!dbg
!dbg if (lprnt) write(6,"(i2,3e12.4)")  k,vtj(i,k),vtk(i,k),ro(i,k)   !dbg

        ENDDO
      ENDDO

!dbg
!dbg if (lprnt) print *,' k     ti          tem         rdz         tem1'  &
!dbg ,'        tem2        dw2         shr2        bvf2     ri_n(i,k)   bnv2(i,k)'

      DO K = 1,KMM1
        DO I =1,npt
          J         = ipt(i)
          TI        = 2.0 / (T1(J,K)+T1(J,K+1))
          TEM       = TI  / (PRSL(J,K)-PRSL(J,K+1))
!         RDZ       = GOR * PRSI(J,K+1) * TEM
          RDZ       = g   / (phil(j,k+1) - phil(j,k))
          TEM1      = U1(J,K) - U1(J,K+1)
          TEM2      = V1(J,K) - V1(J,K+1)

!dbg
!          DW2       = TEM1*TEM1 + TEM2*TEM2
          DW2       = rcl*(TEM1*TEM1 + TEM2*TEM2)

          SHR2      = MAX(DW2,DW2MIN) * RDZ * RDZ
          BVF2      = G*(GOCP+RDZ*(VTJ(I,K+1)-VTJ(I,K))) * TI
          ri_n(I,K) = MAX(BVF2/SHR2,RIMIN)   ! Richardson number
!                                              Brunt-Vaisala Frequency
!         TEM       = GR2 * (PRSL(J,K)+PRSL(J,K+1)) * TEM
!         BNV2(I,K) = TEM * (VTK(I,K+1)-VTK(I,K))/(VTK(I,K+1)+VTK(I,K))
          BNV2(I,K) = (G+G) * RDZ * (VTK(I,K+1)-VTK(I,K))               &
     &                            / (VTK(I,K+1)+VTK(I,K))
          bnv2(i,k) = max( bnv2(i,k), bnv2min )

!dbg
!dbg if (lprnt) write(6,"(i2,10e12.4)")  &
!dbg k,ti,tem,rdz,tem1,tem2,dw2,shr2,bvf2,ri_n(i,k),bnv2(i,k)

!
        ENDDO     !-- DO K = 1,KMM1
      ENDDO       !-- DO I =1,npt
!

!dbg
!dbg if (lprnt) print *,'kmm1,bnv2=',kmm1,bnv2(npt,kmm1)

!
!     Apply 3 point smoothing on BNV2
!
!     do k=1,km
!       do i=1,im
!         vtk(i,k) = bnv2(i,k)
!       enddo
!     enddo
!     do k=2,kmm1
!       do i=1,im
!         bnv2(i,k) = 0.25*(vtk(i,k-1)+vtk(i,k+1)) + 0.5*vtk(i,k)
!       enddo
!     enddo
!
!     Finding the first interface index above 50 hPa level
!
      do i=1,npt
        iwk(i) = 2
      enddo

!dbg if (lprnt) print *,'kmpbl=',kmpbl    !dbg

      DO K=3,KMPBL
        DO I=1,npt
          j   = ipt(i)
          tem = (prsi(j,1) - prsi(j,k))
          if (tem .lt. dpmin) iwk(i) = k
        enddo
      enddo
!
      KBPS = 1
      KMPS = KM
      DO I=1,npt
        J         = ipt(i)
        kref(I)   = MAX(IWK(I), KPBL(J)+1 ) ! reference level 
        DELKS(I)  = 1.0 / (PRSI(J,1) - PRSI(J,kref(I)))
        DELKS1(I) = 1.0 / (PRSL(J,1) - PRSL(J,kref(I)))
        UBAR (I)  = 0.0
        VBAR (I)  = 0.0
        ROLL (I)  = 0.0
        KBPS      = MAX(KBPS,  kref(I))
        KMPS      = MIN(KMPS,  kref(I))
!
        BNV2bar(I) = (PRSL(J,1)-PRSL(J,2)) * DELKS1(I) * BNV2(I,1)
      ENDDO
!
!
      KBPSP1 = KBPS + 1
      KBPSM1 = KBPS - 1
      DO K = 1,KBPS
        DO I = 1,npt
          IF (K .LT. kref(I)) THEN
            J          = ipt(i)
            RDELKS     = DEL(J,K) * DELKS(I)

!dbg
!            UBAR(I)    = UBAR(I)  + RDELKS * U1(J,K)   ! Mean U below kref
!            VBAR(I)    = VBAR(I)  + RDELKS * V1(J,K)   ! Mean V below kref
            RCSKS      = RCS      * RDELKS
            UBAR(I)    = UBAR(I)  + RCSKS  * U1(J,K)   ! Mean U below kref
            VBAR(I)    = VBAR(I)  + RCSKS  * V1(J,K)   ! Mean V below kref

!
            ROLL(I)    = ROLL(I)  + RDELKS * RO(I,K)   ! Mean RO below kref
            RDELKS     = (PRSL(J,K)-PRSL(J,K+1)) * DELKS1(I)
            BNV2bar(I) = BNV2bar(I) + BNV2(I,K) * RDELKS
          ENDIF
        ENDDO
      ENDDO
!

!dbg
!dbg if(lprnt) print *,'ubar(npt)=',ubar(npt)  &
!dbg ,'  vbar(npt)=',vbar(npt),'  roll(npt)=',roll(npt)  &
!dbg ,'  bnv2bar(npt)=',bnv2bar(npt)

!
!     FIGURE OUT LOW-LEVEL HORIZONTAL WIND DIRECTION AND FIND 'OA'
!
!             NWD  1   2   3   4   5   6   7   8
!              WD  W   S  SW  NW   E   N  NE  SE
!
      DO I = 1,npt
        J      = ipt(i)
        wdir   = atan2(UBAR(I),VBAR(I)) + pi
        idir   = mod(nint(fdir*wdir),mdir) + 1
        nwd    = nwdir(idir)
        OA(I)  = (1-2*INT( (NWD-1)/4 )) * OA4(J,MOD(NWD-1,4)+1)
        CLX(I) = CLX4(J,MOD(NWD-1,4)+1)
      ENDDO
!
!-----XN,YN            "LOW-LEVEL" WIND PROJECTIONS IN ZONAL
!                                    & MERIDIONAL DIRECTIONS
!-----ULOW             "LOW-LEVEL" WIND MAGNITUDE -        (= U)
!-----BNV2             BNV2 = N**2
!-----TAUB             BASE MOMENTUM FLUX
!-----= -(RO * U**3/(N*XL)*GF(FR) FOR N**2 > 0
!-----= 0.                        FOR N**2 < 0
!-----FR               FROUDE    =   N*HPRIME / U
!-----G                GMAX*FR**2/(FR**2+CG/OC)
!
!-----INITIALIZE SOME ARRAYS
!
      DO I = 1,npt
        XN(I)     = 0.0
        YN(I)     = 0.0
        TAUB (I)  = 0.0
        ULOW (I)  = 0.0
        DTFAC(I)  = 1.0
        ICRILV(I) = .FALSE. ! INITIALIZE CRITICAL LEVEL CONTROL VECTOR
!
!----COMPUTE THE "LOW LEVEL" WIND MAGNITUDE (M/S)
!
        ULOW(I) = MAX(SQRT(UBAR(I)*UBAR(I) + VBAR(I)*VBAR(I)), HONE)
        ULOI(I) = 1.0 / ULOW(I)
      ENDDO

!dbg
!dbg if (lprnt) print *,'idir,nwd,wdir,oa,clx,ulow,uloi='   &
!dbg ,idir,nwd,wdir,oa(npt),clx(npt),ulow(npt),uloi(npt)

!
      DO  K = 1,KMM1
        DO  I = 1,npt
          J            = ipt(i)

!dbg
!          VELCO(I,K)   = 0.5*  ((U1(J,K)+U1(J,K+1))*UBAR(I)             &
          VELCO(I,K)   = 0.5*rcs*((U1(J,K)+U1(J,K+1))*UBAR(I)            &
     &                       +  (V1(J,K)+V1(J,K+1))*VBAR(I))

          VELCO(I,K)   = VELCO(I,K) * ULOI(I)

!dbg if (lprnt) write(6,"(a,i2,a,e12.4)") 'k=',k,' velco(i,k)=',velco(i,k)  !dbg

!         IF ((VELCO(I,K).LT.VELEPS) .AND. (VELCO(I,K).GT.0.)) THEN
!           VELCO(I,K) = VELEPS
!         ENDIF
        ENDDO
      ENDDO
!
!   find the interface level of the projected wind where
!   low levels & upper levels meet above pbl
!
      do i=1,npt
        kint(i) = km
      enddo
      do k = 1,kmm1
        do i = 1,npt
          IF (K .GT. kref(I)) THEN
            if(velco(i,k) .lt. veleps .and. kint(i) .eq. km) then
              kint(i) = k+1
            endif
          endif
        enddo
      enddo
!  WARNING  KINT = KREF !!!!!!!!!
      do i=1,npt
        kint(i) = kref(i)
      enddo
!
!
      DO I = 1,npt
        J      = ipt(i)
        BNV    = SQRT( BNV2bar(I) )
        FR     = BNV     * ULOI(I) * min(HPRIME(J),hpmax)
        FR     = MIN(FR, FRMAX)
        XN(I)  = UBAR(I) * ULOI(I)
        YN(I)  = VBAR(I) * ULOI(I)
!
!     Compute the base level stress and store it in TAUB
!     CALCULATE ENHANCEMENT FACTOR, NUMBER OF MOUNTAINS & ASPECT
!     RATIO CONST. USE SIMPLIFIED RELATIONSHIP BETWEEN STANDARD
!     DEVIATION & CRITICAL HGT
!
        EFACT    = (OA(I) + 2.) ** (CEOFRC*FR)
        EFACT    = MIN( MAX(EFACT,EFMIN), EFMAX )
!
        COEFM    = (1. + CLX(I)) ** (OA(I)+1.)
!
        XLINV(I) = COEFM * CLEFF
!
        TEM      = FR    * FR * OC(J)
        GFOBNV   = GMAX  * TEM / ((TEM + CG)*BNV)  ! G/N0
!
        TAUB(I)  = XLINV(I) * ROLL(I) * ULOW(I) * ULOW(I)               &
     &           * ULOW(I)  * GFOBNV  * EFACT         ! BASE FLUX Tau0
!
!         tem      = min(HPRIME(I),hpmax)
!         TAUB(I)  = XLINV(I) * ROLL(I) * ULOW(I) * BNV * tem * tem
!
        K        = MAX(1, kref(I)-1)
        TEM      = MAX(VELCO(I,K)*VELCO(I,K), HE_4)
        SCOR(I)  = BNV2(I,K) / TEM  ! Scorer parameter below ref level
      ENDDO    !-- DO I = 1,npt
!

!dbg
!dbg if (lprnt) write(6,"(a,i2,10(a,e12.4))")  &
!dbg  'kint=',kint(npt),' bnv=',bnv,'  fr=',fr,'  xn=',xn(npt)  &
!dbg ,'  yn=',yn(npt),'  efact=',efact,'  coefm=',coefm,'  xlinv(npt)=',xlinv(npt)   &
!dbg ,'  gfobnv=',gfobnv,'  taub(npt)=',taub(npt),'  scor(npt)=',scor(npt)

!                                                                       
!----SET UP BOTTOM VALUES OF STRESS
!
      DO K = 1, KBPS
        DO I = 1,npt
          IF (K .LE. kref(I)) TAUP(I,K) = TAUB(I)
        ENDDO
      ENDDO

!
!   Now compute vertical structure of the stress.
!
      DO K = KMPS, KMM1                   ! Vertical Level K Loop!
        KP1 = K + 1
        DO I = 1, npt
!
!-----UNSTABLE LAYER IF RI < RIC
!-----UNSTABLE LAYER IF UPPER AIR VEL COMP ALONG SURF VEL <=0 (CRIT LAY)
!---- AT (U-C)=0. CRIT LAYER EXISTS AND BIT VECTOR SHOULD BE SET (.LE.)
!
          IF (K .GE. kref(I)) THEN
            ICRILV(I) = ICRILV(I) .OR. ( ri_n(I,K) .LT. RIC)            &
     &                            .OR. (VELCO(I,K) .LE. 0.0)
          ENDIF
        ENDDO
!
        DO I = 1,npt
          IF (K .GE. kref(I))   THEN

!dbg
!dbg if (lprnt) write(6,"(2(a,i2),a,L1,3(a,e12.4))") 'k=',k,'  kref(i)=',kref(i)  &
!dbg ,'  icrilv(i)=',icrilv(i),'  taup(i,k)=',taup(i,k)  &
!dbg ,'  ri_n(i,k)=',ri_n(i,k),'  velco(i,k)=',velco(i,k)

!
            IF (.NOT.ICRILV(I) .AND. TAUP(I,K) .GT. 0.0 ) THEN
              TEMV = 1.0 / max(VELCO(I,K), HE_2)
!             IF (OA(I) .GT. 0. .AND.  PRSI(ipt(i),KP1).GT.RLOLEV) THEN
              IF (OA(I).GT.0. .AND. kp1 .lt. kint(i)) THEN
                SCORK   = BNV2(I,K) * TEMV * TEMV
                RSCOR   = MIN(HONE, SCORK / SCOR(I))
                SCOR(I) = SCORK
              ELSE 
                RSCOR   = 1.
              ENDIF
!
              BRVF = SQRT(BNV2(I,K))        ! Brunt-Vaisala Frequency
!             TEM1 = XLINV(I)*(RO(I,KP1)+RO(I,K))*BRVF*VELCO(I,K)*0.5
              TEM1 = XLINV(I)*(RO(I,KP1)+RO(I,K))*BRVF*0.5              &
     &                       * max(VELCO(I,K),HE_2)
              HD   = SQRT(TAUP(I,K) / TEM1)
              FRO  = BRVF * HD * TEMV
!
!    RIM is the  MINIMUM-RICHARDSON NUMBER BY SHUTTS (1985)
!
              TEM2   = SQRT(ri_n(I,K))
              TEM    = 1. + TEM2 * FRO
              RIM    = ri_n(I,K) * (1.-FRO) / (TEM * TEM)
!
!    CHECK STABILITY TO EMPLOY THE SATURATION HYPOTHESIS
!    OF LINDZEN (1981) EXCEPT AT TROPOSPHERIC DOWNSTREAM REGIONS
!
!                                       ----------------------

!dbg
!dbg if (lprnt) write(6,"(a,i2,10(a,e12.4))")  &
!dbg  'k=',k,'  brvf=',brvf,'  tem1=',tem1,'  xlinv(i)=',xlinv(i)   &
!dbg ,'ROavg=',.5*(RO(I,KP1)+RO(I,K)),'  hd=',hd,'  temv=',temv,'  fro=',fro  &
!dbg ,'  tem2=',tem2,'  tem=',tem,'  rim=',rim

!
              IF (RIM .LE. RIC .AND.                                    &
!    &           (OA(I) .LE. 0. .OR.  PRSI(ipt(I),KP1).LE.RLOLEV )) THEN
     &           (OA(I) .LE. 0. .OR.  kp1 .ge. kint(i) )) THEN
                 TEMC = 2.0 + 1.0 / TEM2
                 HD   = VELCO(I,K) * (2.*SQRT(TEMC)-TEMC) / BRVF
                 TAUP(I,KP1) = TEM1 * HD * HD
              ELSE 
                 TAUP(I,KP1) = TAUP(I,K) * RSCOR
              ENDIF
              taup(i,kp1) = min(taup(i,kp1), taup(i,k))

!dbg
!dbg if (lprnt) write(6,"(a,i2,2(a,e12.4))") 'kp1=',kp1  &
!dbg ,'  taup(i,kp1)=',taup(i,kp1),'  taup(i,k)=',taup(i,k)

            ENDIF    !-- IF (.NOT.ICRILV(I) .AND. TAUP(I,K) .GT. 0.0 ) THEN
          ENDIF      !-- IF (K .GE. kref(I))   THEN
        ENDDO        !-- DO I = 1,npt
      ENDDO          !-- DO K = KMPS, KMM1
!
!     DO I=1,IM
!       taup(i,km+1) = taup(i,km)
!     ENDDO
!
      IF(LCAP .LE. KM) THEN

!dbg if (lprnt) print *,'lcap,lcapp1,km=',lcap,lcapp1,km   !dbg

         DO KLCAP = LCAPP1, KM+1
            DO I = 1,npt
              SIRA          = PRSI(ipt(I),KLCAP) / PRSI(ipt(I),LCAP)
              TAUP(I,KLCAP) = SIRA * TAUP(I,LCAP)

!dbg
!dbg if (lprnt) write(6,"(a,i2,5(a,e12.4))")  &
!dbg  'klcap=',klcap,'  sira=',sira,'  prsi(ipt(i),klcap)=', prsi(ipt(i),klcap)   &
!dbg ,'  prsi(ipt(i),lcap)=',prsi(ipt(i),lcap),'  taup(i,lcap)=',taup(i,lcap)  &
!dbg ,'  taup(i,klcap)=',taup(i,klcap)

!
            ENDDO
         ENDDO
      ENDIF
!
!     Calculate - (g/p*)*d(tau)/d(sigma) and Decel terms DTAUX, DTAUY
!
      DO I=1,npt
!       SCOR(I) = 1.0 / PSTAR(I)

!dbg
!        SCOR(I) = 1.0
        SCOR(I) = 1.0/RCS

      ENDDO
      DO K = 1,KM
        DO I = 1,npt
          TAUD(I,K) = G * (TAUP(I,K+1) - TAUP(I,K)) * SCOR(I)           &
     &                                              / DEL(ipt(I),K)

!dbg
!dbg if (lprnt) write(6,"(a,i2,4(a,e12.4))") 'k=',k,'  taud(i,k)=',taud(i,k)  &
!dbg ,'  D(taup)=',TAUP(I,K+1)-TAUP(I,K),'  del(ipt(i),k)=',del(ipt(i),k)  &
!dbg ,'  scor(i)=',scor(i)

        ENDDO
      ENDDO

!
!------LIMIT DE-ACCELERATION (MOMENTUM DEPOSITION ) AT TOP TO 1/2 VALUE
!------THE IDEA IS SOME STUFF MUST GO OUT THE TOP
!
      DO KLCAP = LCAP, KM
         DO I = 1,npt
            TAUD(I,KLCAP) = TAUD(I,KLCAP) * FACTOP

!dbg
!dbg if (lprnt) write(6,"(a,i2,a,e12.4)") 'klcap=',klcap,'  taud(i,klcap)=',taud(i,klcap)

         ENDDO
      ENDDO
!
!------IF THE GRAVITY WAVE DRAG WOULD FORCE A CRITICAL LINE IN THE
!------LAYERS BELOW SIGMA=RLOLEV DURING THE NEXT DELTIM TIMESTEP,
!------THEN ONLY APPLY DRAG UNTIL THAT CRITICAL LINE IS REACHED.
!
      DO K = 1,KMM1
        DO I = 1,npt
           IF (K .GT. kref(I) .and. PRSI(ipt(i),K) .GE. RLOLEV) THEN
             IF(TAUD(I,K).NE.0.) THEN

!dbg
!               TEM = DELTIM * TAUD(I,K)
               TEM = rcs*DELTIM * TAUD(I,K)

               DTFAC(I) = MIN(DTFAC(I),ABS(VELCO(I,K)/TEM))

!dbg
!dbg if (lprnt) write(6,"(a,i2,2(a,e12.4))") 'k=',k  &
!dbg ,'  tem=',tem,'  dtfac(i)=',dtfac(i)

             ENDIF
           ENDIF
        ENDDO
      ENDDO
!     if(lprnt .and. npr .gt. 0) then
!       print *, before  A=,A(npr,:)
!       print *, before  B=,B(npr,:)
!     endif
!

!dbg
!dbg if (lprnt) write(6,"(a,i2,3(a,e12.4))") 'idxzb(npt)=',idxzb(npt)  &
!dbg ,'  dtfac=',dtfac(npt),'  xn=',xn(npt),'  yn=',yn(npt)

!
      DO K = 1,KM
        DO I = 1,npt
          J          = ipt(i)
          TAUD(I,K)  = TAUD(I,K) * DTFAC(I)
          DTAUX      = TAUD(I,K) * XN(I)
          DTAUY      = TAUD(I,K) * YN(I)
! ---  lm mb (*j*)  changes overwrite GWD
          if ( K .lt. IDXZB(I) .AND. IDXZB(I) .ne. 0 ) then
            DBIM = DB(I,K) / (1.+ abs(DB(I,K))*DELTIM)
            A(J,K)  = - DBIM * V1(J,K) + A(J,K)
            B(J,K)  = - DBIM * U1(J,K) + B(J,K)
            DUsfc(J)   = DUsfc(J) - DBIM * V1(J,K) * DEL(J,K)
            DVsfc(J)   = DVsfc(J) - DBIM * U1(J,K) * DEL(J,K)

!dbg
!dbg if (lprnt .and. dbim > 0.) write(6,"(a,i2,4(a,e12.4))")  &
!dbg  'k=',k,'  db(i,k)=',db(i,k),'  dbim=',dbim  &
!dbg ,'  dudt=',-dbim*u1(j,k),'  dvdt=',-dbim*v1(j,k)

!
          else
!
            A(J,K)     = DTAUY     + A(J,K)
            B(J,K)     = DTAUX     + B(J,K)
            DUsfc(J)   = DUsfc(J)  + DTAUX * DEL(J,K)
            DVsfc(J)   = DVsfc(J)  + DTAUY * DEL(J,K)

!dbg
!dbg if (lprnt .and. dtaux+dtauy/=0.) write(6,"(a,i2,2(a,e12.4))")  &
!dbg ',k=',k,'  dudt=dtaux=',dtaux,'  dvdt=dtauy=',dtauy

          endif

!dbg
!dbg if (lprnt) write(6,"(a,i2,2(a,e12.4))") 'k=',k  &
!dbg ,'  dusfc(j)=',dusfc(j),'  dvsfc(j)=',dvsfc(j)

        ENDDO      !-- DO I = 1,npt
      ENDDO        !-- DO K = 1,KM
!     if (lprnt) then
!       print *, in gwdps_lm.f after  A=,A(ipr,:)
!       print *, in gwdps_lm.f after  B=,B(ipr,:)
!       print *, DB=,DB(ipr,:)
!     endif
      DO I = 1,npt
        J          = ipt(i)
!       TEM    = (-1.E3/G) * PSTAR(I)

!dbg
!        TEM    =  -1.E3/G
        TEM    =  -1.E3*ROG*rcs
        DUsfc(J) = TEM * DUsfc(J)
        DVsfc(J) = TEM * DVsfc(J)

!dbg
!dbg if (lprnt .and. i.eq.npr) write(6,"(3(a,e12.4))") 'tem=',tem  &
!dbg ,'  dusfc(j)=',dusfc(j),'  dvsfc(j)=',dvsfc(j)

      ENDDO
!
      END SUBROUTINE GWD_col
!
!#######################################################################
!
      END MODULE module_gwd
