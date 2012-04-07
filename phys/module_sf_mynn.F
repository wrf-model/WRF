MODULE module_sf_mynn

!-------------------------------------------------------------------
!Modifications implemented by Joseph Olson NOAA/GSD/AMB - CU/CIRES
!for WRFv3.4:
!
!   BOTH LAND AND WATER:
!1) Calculation of stability parameter (z/L) taken from Li et al. (2010 BLM)
!2) Fixed isfflx=0 option to turn off scalar fluxes, but keep momentum
!   fluxes for idealized studies (credit: Anna Fitch).
!3) Kinematic viscosity now varies with temperature
!4) Uses Monin-Obukhov flux-profile relationships more consistent with
!   those used in the MYNN PBL code.
!5) Allows negative QFX, similar to MYJ scheme
!
!   LAND only:
!1) Scalar roughness length parameterization of Yang et al. (2002 QJRMS,
!   2008 JAMC) and Chen et al. (2010 J. of Hydromet), with modifications.
!2) Relaxed u* minimum from 0.1 to 0.01
!
!   WATER only:
!1) Charnock relationship with varying Charnock parameter is taken from
!   the COARE3.0 bulk algorithm (Fairall et al 2003). A formulation for
!   the aerodynamic roughness length was also taken from Davis et al (2008)
!   and Donelan et al. (2004), but this formulation seems to result in a 
!   hign surface wind speed bias for low/moderate wind speeds, so the
!   varying Charnock relationship is default.
!2) Scalar roughness length is taken from the COARE3.0 bulk algorithm 
!   (Fairall et al 2003). A formulation for the scalar roughness length 
!   was also taken from Garrat (1992), but since this formula makes zt 
!   proportional to zo, heat and moisture fluxes are dependent
!   upon which formulation for aerodynamic roughness length is used. 
!   Therefore, the COARE3.0 formulation (with slight modifications) is 
!   used as default instead.
!-------------------------------------------------------------------
  USE module_model_constants, only: &
       &p1000mb, cp, xlv, ep_2

  USE module_sf_sfclay, ONLY: sfclayinit
  USE module_bl_mynn,   only: tv0, mym_condensation
  
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------

  REAL, PARAMETER :: xlvcp=xlv/cp, ep_3=1.-ep_2
 
  REAL, PARAMETER :: wmin=0.1    ! Minimum wind speed
  REAL, PARAMETER :: zm2h=7.4    ! = z_0m/z_0h

  REAL, PARAMETER :: charnock=0.016, bvisc=1.5e-5, z0hsea=5.e-5

  REAL, PARAMETER :: VCONVC=1.0
  REAL, PARAMETER :: CZ0=charnock
  
  REAL, DIMENSION(0:1000 ),SAVE          :: PSIMTB,PSIHTB


CONTAINS

!-------------------------------------------------------------------
  SUBROUTINE mynn_sf_init_driver(allowed_to_read)

    LOGICAL, INTENT(in) :: allowed_to_read

    !Fill the PSIM and PSIH tables. The subroutine "sfclayinit" 
    !can be found in module_sf_sfclay.F. This subroutine returns
    !the forms from Dyer and Hicks (1974) or Paulson (1970)???.
       
    CALL sfclayinit(allowed_to_read)

  END SUBROUTINE mynn_sf_init_driver


!-------------------------------------------------------------------
   SUBROUTINE SFCLAY_mynn(U3D,V3D,T3D,QV3D,P3D,dz8w,               &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     itimestep,ch,th3d,pi3d,qc3d,                  &
                     tsq,qsq,cov,qcg,                              &
!JOE-add zo/zt
!                     z0zt_ratio,BulkRi,wstar,&
!JOE-end z0/zt
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- dz8w        dz between full levels (m)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G           acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- R           gas constant for dry air (J/kg/K)
!-- XLV         latent heat of vaporization for water (J/kg)
!-- PSFC        surface pressure (Pa)
!-- ZNT         roughness length (m)
!-- UST         u* in similarity theory (m/s)
!-- PBLH        PBL height from previous time (m)
!-- MAVAIL      surface moisture availability (between 0 and 1)
!-- ZOL         z/L height over Monin-Obukhov length
!-- MOL         T* (similarity theory) (K)
!-- REGIME      flag indicating PBL regime (stable, unstable, etc.)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- XLAND       land mask (1 for land, 2 for water)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          net upward latent heat flux at surface (W/m^2)
!-- TSK         surface temperature (K)
!-- FLHC        exchange coefficient for heat (W/m^2/K)
!-- FLQC        exchange coefficient for moisture (kg/m^2/s)
!-- CHS         heat/moisture exchange coefficient for LSM (m/s)
!-- QGH         lowest-level saturated mixing ratio
!-- U10         diagnostic 10m u wind
!-- V10         diagnostic 10m v wind
!-- TH2         diagnostic 2m theta (K)
!-- T2          diagnostic 2m temperature (K)
!-- Q2          diagnostic 2m mixing ratio (kg/kg)
!-- GZ1OZ0      log(z/z0) where z0 is roughness length
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- ISFFLX      isfflx=1 for surface heat and moisture fluxes
!-- DX          horizontal grid size (m)
!-- SVP1        constant for saturation vapor pressure (kPa)
!-- SVP2        constant for saturation vapor pressure (dimensionless)
!-- SVP3        constant for saturation vapor pressure (K)
!-- SVPT0       constant for saturation vapor pressure (K)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- EP2         constant for specific humidity calculation 
!               (R_d/R_v) (dimensionless)
!-- KARMAN      Von Karman constant
!-- EOMEG       angular velocity of earth's rotation (rad/s)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------
      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
!                                                               
      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
!
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w
                                        
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                             QC3D,&
                                            th3d,pi3d,tsq,qsq,cov

      INTEGER, INTENT(in) :: itimestep

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN) ::&
           &    qcg
      
      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::&
           & ch

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  )               ::                U10, &
                                                              V10, &
                                                              TH2, &
                                                               T2, &
!JOE-use value from LSM                                        Q2, &
                                                               Q2
!JOE-moved down below                                          QSFC

!
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                    MOL,RMOL,QSFC
!m the following 5 are change to memory size
!
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D
                                        
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            QGH

!JOE-begin
!      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
!                INTENT(OUT)               ::         z0zt_ratio, &
!                                                      BulkRi,wstar
!JOE-end 
                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX


!----------- LOCAL VARS -----------------------------------

      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,qc1d

      REAL,     DIMENSION( its:ite ) ::                    dz8w1d

      REAL,     DIMENSION( its:ite ) ::  vt1,vq1
      REAL,     DIMENSION(kts:kts+1) ::  thl, qw, vt, vq
      REAL                           ::  ql

      INTEGER ::  I,J,K
!-----------------------------------------------------------

      DO J=jts,jte
        DO i=its,ite
          dz8w1d(I) = dz8w(i,kts,j)
        ENDDO
   
        DO i=its,ite
           U1D(i) =U3D(i,kts,j)
           V1D(i) =V3D(i,kts,j)
           QV1D(i)=QV3D(i,kts,j)
           QC1D(i)=QC3D(i,kts,j)
           P1D(i) =P3D(i,kts,j)
           T1D(i) =T3D(i,kts,j)
        ENDDO

        IF (itimestep==1) THEN
           DO i=its,ite
              vt1(i)=0.
              vq1(i)=0.
              UST(i,j)=0.02*SQRT(U1D(i)*U1D(i) + V1D(i)*V1D(i))
              MOL(i,j)=0.     ! Tstar
           ENDDO
        ELSE
           DO i=its,ite
              do k = kts,kts+1
                ql = qc3d(i,k,j)/(1.+qc3d(i,k,j))
                qw(k) = qv3d(i,k,j)/(1.+qv3d(i,k,j)) + ql
                thl(k) = th3d(i,k,j)-xlvcp*ql/pi3d(i,k,j)
              end do

! NOTE: The last grid number is kts+1 instead of kte.
              CALL mym_condensation (kts,kts+1, &
                   &            dz8w(i,kts:kts+1,j), &
                   &            thl(kts:kts+1), qw(kts:kts+1), &
                   &            p3d(i,kts:kts+1,j),&
                   &            pi3d(i,kts:kts+1,j), &
                   &            tsq(i,kts:kts+1,j), &
                   &            qsq(i,kts:kts+1,j), &
                   &            cov(i,kts:kts+1,j), &
                   &            vt(kts:kts+1), vq(kts:kts+1))

              vt1(i) = vt(kts)
              vq1(i) = vq(kts)
           ENDDO
        ENDIF

        CALL SFCLAY1D_mynn(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,          &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j),      &
                QSFC(ims,j),LH(ims,j),                             &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,  &
                ch(ims,j),vt1,vq1,qc1d,qcg(ims,j),&
!JOE-begin
!                z0zt_ratio(ims,j),BulkRi(ims,j),wstar(ims,j),      &
                itimestep,&
!JOE-end
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          )

      ENDDO


    END SUBROUTINE SFCLAY_MYNN

!-------------------------------------------------------------------
   SUBROUTINE SFCLAY1D_mynn(J,UX,VX,T1D,QV1D,P1D,dz8w1d,                &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,      &
                     XLAND,HFX,QFX,TSK,                            &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH,              &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     ch,vt1,vq1,qc1d,qcg,&
!JOE-add
!                     zratio,BRi,wstar,itimestep,                   &
                     itimestep,                                    &
!JOE-end
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      REAL,     PARAMETER  :: XKA=2.4E-5   !molecular diffusivity
      REAL,     PARAMETER  :: PRT=1.       !prandlt number

      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J
!                                                               
      INTEGER,  INTENT(in)    :: itimestep
      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT

!
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
!
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL
!m the following 5 are changed to memory size---
!
      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS
!JOE-add
!      REAL,     DIMENSION( ims:ime )                             , &
!                INTENT(OUT)     ::                zratio,BRi,wstar
      REAL,     DIMENSION( ims:ime )  ::           zratio,BRi,wstar
!JOE-end

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                       QGH,QSFC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(OUT)     ::                        U10,V10, &
!JOE-make qsfc inout (moved up)                   TH2,T2,Q2,QSFC,LH
                                                TH2,T2,Q2,LH
                                    
      REAL,     INTENT(IN)               ::   CP,G,ROVCP,R,XLV,DX

! MODULE-LOCAL VARIABLES, DEFINED IN SUBROUTINE SFCLAY
      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::  dz8w1d

      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::      UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,qc1d
 
      REAL,     DIMENSION( ims:ime ), INTENT(IN)    :: qcg
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) :: ch

      REAL,     DIMENSION( its:ite ), INTENT(IN)    :: vt1,vq1

! LOCAL VARS

      REAL,     DIMENSION( its:ite ) :: z_t,z_q

      REAL :: thl1,sqv1,sqc1,exner1,sqvg,sqcg,vv,ww

      REAL,     DIMENSION( its:ite )        ::                 ZA, &
                                                        THVX,ZQKL, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                           PSIH10, &
                                                           PSIM10, &
                                                           GZ2OZ0, &
                                                          GZ10OZ0
!
      REAL,     DIMENSION( its:ite )        ::        RHOX,GOVRTH
!
      REAL,     DIMENSION( its:ite)         ::          SCR4
      REAL,     DIMENSION( its:ite )        ::         THGB, PSFC

      REAL,     DIMENSION( its:ite )        :: GZ2OZt,GZ10OZt,GZ1OZt

!
      INTEGER ::  N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL    ::  DTG,PSIX,USTM,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2
      REAL    ::  FLUXC,VSGD
      real    ::  restar,VISC,psilim
!-------------------------------------------------------------------

!----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:  
      DO I=its,ite
         ! PSFC cmb
         PSFC(I)=PSFCPA(I)/1000.
         THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP                
      ENDDO                                              
!                                                                                   
!     SCR4(I,K) STORES EITHER TEMPERATURE OR VIRTUAL TEMPERATURE,
!     DEPENDING ON IFDRY (CURRENTLY NOT USED, SO SCR4 == TVX).                                       
                                                                                 
      DO 30 I=its,ite
         ! PL cmb
         PL=P1D(I)/1000.                                                   
         THCON=(100./PL)**ROVCP                                                 
         THX(I)=T1D(I)*THCON                                               
         SCR4(I)=T1D(I)                                                    
         THVX(I)=THX(I)                                                     
         QX(I)=0.                                                             
   30 CONTINUE                                                                 

      ! INITIALIZE SOME VARIABLES HERE:
      DO I=its,ite
         QGH(I)=0.                                                                
         FLHC(I)=0.                                                               
         FLQC(I)=0.  
         CPM(I)=CP                                                             
      ENDDO
                                                                                
!     IF(IDRY.EQ.1)GOTO 80                                                   
      DO 50 I=its,ite
         QX(I)=QV1D(I)/(1.+QV1D(I))        !CONVERT TO SPEC HUM
         TVCON=(1.+EP1*QX(I))                                      
         THVX(I)=THX(I)*TVCON                                               
         SCR4(I)=T1D(I)*TVCON                                              
   50 CONTINUE                                                                 
!                                                                                
      DO 60 I=its,ite
        IF (TSK(I) .LT. 273.15) THEN
           !SATURATION VAPOR PRESSURE WRT ICE
           E1=SVP1*EXP(4648*(1./273.15 - 1./TSK(I)) - &
                  11.64*LOG(273.15/TSK(I)) + 0.02265*(273.15 - TSK(I)))
        ELSE
           !SATURATION VAPOR PRESSURE WRT WATER
           E1=SVP1*EXP(SVP2*(TSK(I)-SVPT0)/(TSK(I)-SVP3))
        ENDIF
        QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)   !specific humidity
        !QSFC(I)=EP2*E1/(PSFC(I)-E1)       !mixing ratio                                  
        !FOR LAND POINTS, QSFC can come from previous time step (in LSM)
        !if(xland(i).gt.1.5 .or. QSFC(i).le.0.0) QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)     
  
        ! QGH CHANGED TO USE LOWEST-LEVEL AIR TEMP CONSISTENT WITH MYJSFC CHANGE
        ! Q2SAT = QGH IN LSM
        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
        PL=P1D(I)/1000.
        QGH(I)=EP2*E1/(PL-ep_3*E1)     !specific humidity
        !QGH(I)=EP2*E1/(PL-E1)         !mixing ratio    
        CPM(I)=CP*(1.+0.84*QX(I)/(1.-qx(i)))

   60 CONTINUE                                                                   
   80 CONTINUE
                                                                                 
!-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             
!     LEVEL, AND THE LAYER THICKNESSES.                                          
                                                                                 
      DO I=its,ite
         RHOX(I)=PSFC(I)*1000./(R*SCR4(I))                                       
         ZQKL(I)=dz8w1d(I)            !first full-sigma level
         ZA(I)=0.5*ZQKL(I)            !first half-sigma level                            
         GOVRTH(I)=G/THX(I)                                                    
      ENDDO
                                                                                 
!-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               
!     AKB(1976), EQ(12).                                                         
                   
      DO 260 I=its,ite

         WSPD(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))                        

         IF((XLAND(I)-1.5).GE.0)THEN

            !COMPUTE KINEMATIC VISCOSITY
            VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5

            !--------------------------------------
            ! WATER
            !--------------------------------------
            !GET ZNT
            !--------------------------------------
            !CALL davis_etal_2008(ZNT(i),UST(i))
            !CALL Taylor_Yelland_2001(ZNT(i),UST(i),WSPD(i))
            CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc)

            !COMPUTE ROUGHNESS REYNOLDS NUMBER (restar) USING NEW ZNT
            ! AHW: Garratt formula: Calculate roughness Reynolds number
            !      Kinematic viscosity of air (linear approx to
            !      temp dependence at sea level)
            restar=MAX(ust(i)*ZNT(i)/visc, 0.1)

            !--------------------------------------
            !GET z_t and z_q
            !--------------------------------------
            !CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,UST(I),KARMAN,XLAND(I))
            !CALL garrat_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
            CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)

            zratio(i)=znt(i)/z_t(i)

         ELSE

            !--------------------------------------
            ! LAND
            !--------------------------------------

            !COMPUTE ROUGHNESS REYNOLDS NUMBER (restar) USING DEFAULT ZNT
            !restar=MAX(ust(i)*ZNT(i)/bvisc, 1.0)
            VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5
            restar=MAX(ust(i)*ZNT(i)/visc, 0.1)

            !--------------------------------------
            !GET z_t and z_q
            !--------------------------------------
            !CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,UST(I),KARMAN,XLAND(I))
            !CALL garrat_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
            CALL Yang_2008(ZNT(i),z_t(i),z_q(i),UST(i),MOL(I),restar,visc,XLAND(I))

            zratio(i)=znt(i)/z_t(i)

         ENDIF


         GZ1OZ0(I)= LOG(ZA(I)/ZNT(I))
         GZ1OZt(I)= LOG(ZA(I)/z_t(i))           
         GZ2OZ0(I)= LOG(2./ZNT(I))                                        
         GZ2OZt(I)= LOG(2./z_t(i))                                        
         GZ10OZ0(I)=LOG(10./ZNT(I)) 
         GZ10OZt(I)=LOG(10./z_t(i)) 

         !account for partial condensation
         exner1=(p1d(i)/p1000mb)**ROVCP
         sqc1=qc1d(i)/(1.+qc1d(i))         !convert to spec hum.
         sqv1=qx(i)
         thl1=THX(I)-xlvcp/exner1*sqc1
         sqvg=qsfc(i)
         sqcg=qcg(i)/(1.+qcg(i))  !convert to specific humidity

         vv = thl1-THGB(I)
         ww = mavail(i)*(sqv1-sqvg) + (sqc1-sqcg)

         TSKV=THGB(I)*(1.+EP1*QSFC(I)*MAVAIL(I)) 
         !TSKV=THGB(I)*(1.+EP1*QSFC(I))                     

         !DTHVDZ=(THVX(I)-TSKV)                     
         DTHVDZ= (vt1(i) + 1.0)*vv + (vq1(i) + tv0)*ww

         !--------------------------------------------------------
         !  Calculate the convective velocity scale (WSTAR) and 
         !  subgrid-scale velocity (VSGD) following Beljaars (1995, QJRMS) 
         !  and Mahrt and Sun (1995, MWR), respectively
         !-------------------------------------------------------
         !       VCONV = 0.25*sqrt(g/tskv*pblh(i)*dthvm)
         !  Use Beljaars over land, old MM5 (Wyngaard) formula over water
         if (xland(i).lt.1.5) then     !LAND (xland == 1)

            fluxc = max(hfx(i)/rhox(i)/cp                    &
                 + ep1*tskv*qfx(i)/rhox(i),0.)
            !JOE:VCONV = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33
            WSTAR(I) = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33

         else                          !WATER (xland == 2)

            IF(-DTHVDZ.GE.0)THEN
               DTHVM=-DTHVDZ
            ELSE
               DTHVM=0.
            ENDIF
            !JOE-the Wyngaard formula is 2-3 times larger than the Beljaars 
            !formula, so switch to Beljaars for water, but use VCONVC = 1.25,
            !as in the COARE3.0 bulk parameterizations.
            !WSTAR(I) = 2.*SQRT(DTHVM)
            fluxc = max(hfx(i)/rhox(i)/cp                    &
                + ep1*tskv*qfx(i)/rhox(i),0.)
            WSTAR(I) = 1.25*(g/TSK(i)*pblh(i)*fluxc)**.33

         endif

         !--------------------------------------------------------
         ! Mahrt and Sun low-res correction
         ! (for 13 km ~ 0.37 m/s; for 3 km == 0 m/s)
         !--------------------------------------------------------
         VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33

         WSPD(I)=SQRT(WSPD(I)*WSPD(I)+WSTAR(I)*WSTAR(I)+vsgd*vsgd)
         WSPD(I)=AMAX1(WSPD(I),wmin)

         !--------------------------------------------------------
         ! CALCULATE THE BULK RICHARDSON NUMBER OF SURFACE LAYER, 
         ! ACCORDING TO AKB(1976), EQ(12). 
         !--------------------------------------------------------
         BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))
         !SET LIMITS ACCORDING TO Li et al. (2010) Boundary-Layer Meteorol (p.158)
         BR(I)=MAX(BR(I),-2.0)
         BR(I)=MIN(BR(I),1.0)
         BRi(I)=BR(I)  !new variable for output - BR is not a "state" variable.
               
         !  IF PREVIOUSLY UNSTABLE, DO NOT LET INTO REGIMES 1 AND 2 (STABLE)
         if (itimestep .GT. 1) THEN
            IF(MOL(I).LT.0.)BR(I)=AMIN1(BR(I),0.0)
         ENDIF
!jdf
         RMOL(I)=-GOVRTH(I)*DTHVDZ*ZA(I)*KARMAN
!jdf
!JOE-begin
!        IF(I .eq. 2)THEN
!          write(*,1006)"BR:",BR(I)," fluxc:",fluxc," vt1:",vt1(i)," vq1:",vq1(i)
!          write(*,1007)"XLAND:",XLAND(I)," WSPD:",WSPD(I)," DTHVDZ:",DTHVDZ," WSTAR:",WSTAR(I)
!        ENDIF
!JOE-end
                         
  260 CONTINUE                                                                   

 1006   format(A,F7.3,A,f9.4,A,f9.5,A,f9.4)
 1007   format(A,F2.0,A,f6.2,A,f7.3,A,f7.2)

!--------------------------------------------------------------------      
!--- DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            
! 
!                                                                                
!     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           
!     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              
!                                                                                
!     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
!                                                                                
!        1. BR .GE. 0.2;                                                         
!               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
!                                                                                
!        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       
!               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                
!               (REGIME=2),                                                      
!                                                                                
!        3. BR .EQ. 0.0                                                          
!               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
!                                                                                
!        4. BR .LT. 0.0                                                          
!               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
!                                                                                
!--------------------------------------------------------------------

  DO 320 I=its,ite

     IF (BR(I) .GT. 0.2) THEN                                           
        !===================================================
        !---CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    
        !===================================================
        REGIME(I)=1.

        !COMPUTE z/L
        CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))

        !COMPUTE PSIM and PSIH
        IF((XLAND(I)-1.5).GE.0)THEN                                            
           ! WATER
           !CALL PSI_Suselj_Sood_2010(PSIM(I),PSIH(I),ZOL(I)) !produces neg TKE
           !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))

           !LOWER LIMIT ON PSI IN STABLE CONDITIONS
            psilim = -20.
        ELSE           
           ! LAND  
           !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Zilitinkevich_Esau_2007(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))

           !LOWER LIMIT ON PSI IN STABLE CONDITIONS
            psilim = -20.   !JOE: relaxed from -10 to -20.
        ENDIF      

        PSIM(I)=AMAX1(PSIM(I),psilim)
        PSIH(I)=AMAX1(PSIH(I),psilim)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),psilim)
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),psilim)
        PSIH2(I)=PSIM2(I)    
        RMOL(I) = ZOL(I)/ZA(I) !1.0/L                                     

     ELSEIF(BR(I) .GT. 0. .AND. BR(I) .LE. 0.2) THEN                                                 
        !========================================================
        !---CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     
        !========================================================
        REGIME(I)=2.    
                                                       
        !COMPUTE z/L
        CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))

        !COMPUTE PSIM and PSIH
        IF((XLAND(I)-1.5).GE.0)THEN                                            
           ! WATER
           !CALL PSI_Suselj_Sood_2010(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))

           !LOWER LIMIT ON PSI IN STABLE CONDITIONS
            psilim = -10.   !JOE: This limit is never hit.
        ELSE           
           ! LAND  
           !CALL PSI_Beljaars_Holtslag_1991(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Zilitinkevich_Esau_2007(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))

           !LOWER LIMIT ON PSI IN STABLE CONDITIONS
            psilim = -10.   !JOE: This limit is never hit.
        ENDIF              

        ! LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     
        PSIM(I)=AMAX1(PSIM(I),psilim)
        PSIH(I)=AMAX1(PSIH(I),psilim)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),psilim)                               
        PSIH10(I)=PSIM10(I)                                          
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),psilim)                              
        PSIH2(I)=PSIM2(I)
        ! 1.0 over Monin-Obukhov length
        RMOL(I)= ZOL(I)/ZA(I)

     ELSEIF(BR(I) .EQ. 0.) THEN                  
        !=========================================================  
        !-----CLASS 3; FORCED CONVECTION/NEUTRAL:                                                
        !=========================================================
        REGIME(I)=3.

        PSIM(I)=0.0                                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=0.                                                   
        PSIH10(I)=PSIM10(I)                                           
        PSIM2(I)=0.                                                  
        PSIH2(I)=PSIM2(I)                                           
                                           
        !ZOL(I)=0.                                             
        IF(UST(I) .LT. 0.01)THEN                                                 
          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
        ELSE                                                                     
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I)) 
        ENDIF                                                                    
        RMOL(I) = ZOL(I)/ZA(I)  

     ELSEIF(BR(I) .LT. 0.)THEN            
        !==========================================================
        !-----CLASS 4; FREE CONVECTION:                                                  
        !==========================================================
        REGIME(I)=4.

        !COMPUTE z/L
        CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))

        ZOL10=10./ZA(I)*ZOL(I)
        ZOL2=2./ZA(I)*ZOL(I)
        ZOL(I)=AMIN1(ZOL(I),0.)
        ZOL(I)=AMAX1(ZOL(I),-9.9999)
        ZOL10=AMIN1(ZOL10,0.)
        ZOL10=AMAX1(ZOL10,-9.9999)
        ZOL2=AMIN1(ZOL2,0.)
        ZOL2=AMAX1(ZOL2,-9.9999)
        NZOL=INT(-ZOL(I)*100.)
        RZOL=-ZOL(I)*100.-NZOL
        NZOL10=INT(-ZOL10*100.)
        RZOL10=-ZOL10*100.-NZOL10
        NZOL2=INT(-ZOL2*100.)
        RZOL2=-ZOL2*100.-NZOL2

        !COMPUTE PSIM and PSIH
        IF((XLAND(I)-1.5).GE.0)THEN                                            
           ! WATER
           !CALL PSI_Suselj_Sood_2010(PSIM(I),PSIH(I),ZOL(I))
           !CALL PSI_Hogstrom_1996(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ELSE           
           ! LAND  
           !CALL PSI_Hogstrom_1996(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
           !CALL PSI_Businger_1971(PSIM(I),PSIH(I),ZOL(I))
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ENDIF              

        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))

        !---LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND
        !---HIGH ROUGHNESS.  THIS PREVENTS DENOMINATOR IN FLUXES
        !---FROM GETTING TOO SMALL
        PSIH(I)=AMIN1(PSIH(I),0.85*GZ1OZ0(I))
        PSIM(I)=AMIN1(PSIM(I),0.85*GZ1OZ0(I))
        PSIH2(I)=AMIN1(PSIH2(I),0.85*GZ2OZ0(I))
        PSIM10(I)=AMIN1(PSIM10(I),0.85*GZ10OZ0(I))

        RMOL(I) = ZOL(I)/ZA(I)  

     ENDIF

  320 CONTINUE   

!-----------------------------------------------------------
!-----COMPUTE U*, T*, AND SURFACE DIAGNOSTICS:                       
!-----------------------------------------------------------
  DO 330 I=its,ite

     !------------------------------------------------------------
     !-----COMPUTE THE FRICTIONAL VELOCITY:                                           
     !     ZA(1982) EQS(2.60),(2.61).                                                 
     !------------------------------------------------------------
      DTG=THX(I)-THGB(I)                                                   
      PSIX=GZ1OZ0(I)-PSIM(I)                                                   
      PSIX10=GZ10OZ0(I)-PSIM10(I)
      ! TO PREVENT OSCILLATIONS AVERAGE WITH OLD VALUE 
      UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX 
      IF((XLAND(I)-1.5).LT.0.)THEN        !LAND
          !JOE: UST(I)=AMAX1(UST(I),0.1)
          UST(I)=AMAX1(UST(I),0.01)  !Relaxing this limit
      ENDIF

     !------------------------------------------------------------
     !-----COMPUTE THE RESISTANCE (PSIQ AND PSIT):                                           
     !------------------------------------------------------------
      ! LOWER LIMIT ADDED TO PREVENT LARGE FLHC IN SOIL MODEL
      ! ACTIVATES IN UNSTABLE CONDITIONS WITH THIN LAYERS OR HIGH Z0
      !PSIT=AMAX1(GZ1OZ0(I)-PSIH(I),2.)
      PSIT=AMAX1(GZ1OZt(I)-PSIH(I),2.)
      PSIT2=MAX(GZ2OZt(I)-PSIH2(I),2.)
                                  
      IF((XLAND(I)-1.5).GE.0)THEN                                            
         PSIQ=MAX(LOG(za(i)/z_q(I))-PSIH(I),2.) 
         PSIQ2=MAX(LOG(2./z_q(I))-PSIH2(I),2.)  
      ELSE                                                                     
         !CARLSON AND BOLAND (1978)(independent of zq):
         ZL=0.01                                                                
         PSIQ=MAX(LOG(KARMAN*UST(I)*ZA(I)/XKA + ZA(I)/ZL)-PSIH(I),2.0)   
         PSIQ2=MAX(LOG(KARMAN*UST(I)*2./XKA + 2./ZL)-PSIH2(I),2.0)                                  
      ENDIF                                                                    
                                 
      !------------------------------------------------------------
      !COMPUTE THE TEMPERATURE SCALE (or FRICTION TEMPERATURE, T*)
      !------------------------------------------------------------
      MOL(I)=KARMAN*DTG/PSIT/PRT
      !t_star(I) = -HFX(I)/(UST(I)*CPM(I)*RHOX(I))
      !t_star(I) = MOL(I)

      !-----------------------------------------------------
      !COMPUTE 10 M WNDS
      ! If the lowest model level is close to 10-m, use it 
      ! instead of the flux-based formula.
      if (ZA(i) .gt. 7.0 .and. ZA(i) .lt. 13.0) then
         U10(I)=UX(I)                                   
         V10(I)=VX(I)
      else                                 
         U10(I)=UX(I)*PSIX10/PSIX                                    
         V10(I)=VX(I)*PSIX10/PSIX     
      endif                              

      !-----------------------------------------------------
      !GET 2 M T, TH, AND Q
      !THESE WILL BE OVERWRITTEN FOR LAND POINTS IN THE LSM 
      TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                
      Q2(I)=QSFC(I)+(QX(I)-QSFC(I))*PSIQ2/PSIQ                   
      T2(I) = TH2(I)*(PSFC(I)/100.)**ROVCP                     

  330 CONTINUE                                                                   
                                                                                  
!-----------------------------------------------------------
!-----COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       
!-----------------------------------------------------------
                                                                                 
      DO i=its,ite
        QFX(i) =0.                                                              
        HFX(i) =0.                                                              
      ENDDO

      !-----------------------------------------------------
      !-- BUT FIRST, RECALCULATE ROUGHNESS LENGTHS (ZNT, Z_T, Z_Q).
      !-- USING AN UPDATED U*.          
      !-----------------------------------------------------
                                                                                 
      DO 360 I=its,ite

         IF((XLAND(I)-1.5).GE.0)THEN                                            

            !COMPUTE KINEMATIC VISCOSITY
            ! AHW: Garratt formula: Calculate kinematic viscosity 
            !      of air (linear approc to temp dependence at sea lev)
            VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5

            !--------------------------------------
            ! WATER
            !--------------------------------------
            ! GET ZNT
            !--------------------------------------
            !CALL davis_etal_2008(ZNT(i),UST(i))
            !CALL Taylor_Yelland_2001(ZNT(i),UST(i),WSPD(i))
            CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc)

            !COMPUTE ROUGHNESS REYNOLDS NUMBER WITH NEW ZNT
             restar=MAX(ust(i)*ZNT(i)/visc, 0.1)

            !--------------------------------------
            ! GET z_t and z_q
            !--------------------------------------
            !CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,UST(I),KARMAN,XLAND(I))
            !CALL garrat_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
            CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)

            zratio(i)=znt(i)/z_t(i)

         ELSE

            !--------------------------------------
            ! LAND
            !--------------------------------------

            !COMPUTE ROUGHNESS REYNOLDS NUMBER (restar)
            VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5
            restar=MAX(ust(i)*ZNT(i)/visc, 0.1)

            !--------------------------------------
            ! GET z_t and z_q
            !--------------------------------------
            !CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,UST(I),KARMAN,XLAND(I))
            !CALL garrat_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
            CALL Yang_2008(ZNT(i),z_t(i),z_q(i),UST(i),MOL(I),restar,visc,XLAND(I))

            zratio(i)=znt(i)/z_t(i)

         ENDIF    

        !------------------------------------------
        ! CALCULATE THE EXCHANGE COEFFICIENTS FOR HEAT (FLHC)
        ! AND MOISTURE (FLQC)
        !------------------------------------------
        IF (ISFFLX.GT.0) THEN
           IF((XLAND(I)-1.5).GE.0)THEN                                            
              PSIQ=MAX(LOG(za(i)/z_q(I))-PSIH(I),2.) 
           ELSE                                                                     
              !CARLSON AND BOLAND (1978)(independent of zq):
              ZL=0.01                                                                
              PSIQ=MAX(LOG(KARMAN*UST(I)*ZA(I)/XKA + ZA(I)/ZL)-PSIH(I),2.0)   
           ENDIF                                                                    

           !FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/(   &
           !        ALOG(KARMAN*UST(I)*ZA(I)/XKA+ZA(I)/ZL)-PSIH(I))

           !FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/(   &
           !     &             ALOG(za(i)/z_q(i))-PSIH(I))
           FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/PSIQ

           DTTHX=ABS(THX(I)-THGB(I))                                            
           IF(DTTHX.GT.1.E-5)THEN                                                   
             FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))          
           ELSE                                                                     
             FLHC(I)=0.                                                             
           ENDIF   
        ENDIF   

!        IF (I .eq. 2) THEN
!          write(*,1001)"REGIME:",REGIME(I)," z/L:",ZOL(I)," UST:",UST(I)," Tstar:",MOL(I)
!          write(*,1002)"PSIM:",PSIM(I)," PSIH:",PSIH(I)," FLHC:",FLHC(I)," FLQC:",FLQC(I)
!          write(*,1003)"CPM:",CPM(I)," RHOX:",RHOX(I)," L:",ZOL(I)/ZA(I)," DTHV:",THX(I)-THGB(I)
!          write(*,1004)"Z0/Zt:",zratio(I)," Z0:",ZNT(I)," Zt:",z_t(I)," za:",za(I)
!          write(*,1005)"Re:",restar," MAVAIL:",MAVAIL(I)," QSFC(I):",QSFC(I)," QX(I):",QX(I)
!          print*,"VISC=",VISC," Z0:",ZNT(I)," T1D(i):",T1D(i)
!          write(*,*)"============================================="
!        ENDIF
                         
  360 CONTINUE 
                                                   
 1001   format(A,F2.0, A,f10.4,A,f5.3, A,f11.5)
 1002   format(A,f7.2, A,f7.2, A,f9.2, A,f10.6)
 1003   format(A,f7.2, A,f7.2, A,f10.3,A,f10.3)
 1004   format(A,f11.3,A,f9.7, A,f9.7, A,f6.2, A,f10.3)
 1005   format(A,f9.2,A,f6.4,A,f7.4,A,f7.4)

  IF (ISFFLX .GT. 0) THEN                                                
     !----------------------------------
     !-----COMPUTE SURFACE MOISTURE FLUX:
     !                                                                                
     !IF(IDRY .EQ. 1)GOTO 390                                                

     DO I=its,ite
        QFX(I)=FLQC(I)*(QSFC(I)-QX(I))                                     
        !JOE: QFX(I)=AMAX1(QFX(I),0.)   !does not allows neg QFX           
        QFX(I)=AMAX1(QFX(I),-0.02)      !allows small neg QFX, like MYJ  
        LH(I)=XLV*QFX(I)
     ENDDO                                                             

  390 CONTINUE                                                                 
     !----------------------------------
     !-----COMPUTE SURFACE HEAT FLUX:
     !                                                                                
      DO 400 I=its,ite
        IF(XLAND(I)-1.5.GT.0.)THEN                                           
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
        ELSEIF(XLAND(I)-1.5.LT.0.)THEN                                       
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
          HFX(I)=AMAX1(HFX(I),-250.)                                       
        ENDIF                                                                  
  400 CONTINUE                                                                 
         
      DO I=its,ite

!         CHS(I)=UST(I)*KARMAN/(ALOG(KARMAN*UST(I)*ZA(I) &
!                /XKA+ZA(I)/ZL)-PSIH(I))

         CHS(I)=UST(I)*KARMAN/(LOG(ZA(I)/z_t(I))- &
              &PSIH(I))

! The exchange coefficient for cloud water is assumed to be the same as
! that for heat. CH is multiplied by WSPD.

!!         ch(i)=chs(i)
         ch(i)=flhc(i)/( cpm(i)*rhox(i) )

!        GZ2OZ0(I)=ALOG(2./ZNT(I))
!        PSIM2(I)=-10.*GZ2OZ0(I)
!        PSIM2(I)=AMAX1(PSIM2(I),-10.)
!        PSIH2(I)=PSIM2(I)
!         CQS2(I)=UST(I)*KARMAN/(ALOG(KARMAN*UST(I)*2.0  &
!               /XKA+2.0/ZL)-PSIH2(I))
!         CHS2(I)=UST(I)*KARMAN/(GZ2OZ0(I)-PSIH2(I))

         !THESE ARE USED FOR 2-M DIAGNOSTICS ONLY
         CQS2(I)=UST(I)*KARMAN/(LOG(2.0/z_q(I))-PSIH2(I))
         CHS2(I)=UST(I)*KARMAN/(GZ2OZt(I)-PSIH2(I))

      ENDDO

   ENDIF

                                                                                
   END SUBROUTINE SFCLAY1D_mynn

!-------------------------------------------------------------------          
   SUBROUTINE zilitinkevich_1995(Z_0,Zt,Zq,restar,ustar,KARMAN,landsea)

       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0,restar,ustar,KARMAN,landsea
       REAL, INTENT(OUT) :: Zt,Zq
       REAL, PARAMETER :: C1=0.075

       IF (landsea-1.5 .GT. 0) THEN    !WATER
          !THIS IS BASED ON Zilitinkevich, Grachev, and Fairall (2001;
          !Their equations 15 and 16).
          IF (restar .LT. 0.1) THEN
             Zt = Z_0*EXP(KARMAN*2.0)
             Zt = MIN( Zt, 5.0e-5)
             Zt = MAX( Zt, 2.0e-9) !same lower limit as ECMWF
             Zq = Z_0*EXP(KARMAN*3.0)
             Zq = MIN( Zq, 5.0e-5)
             Zq = MAX( Zq, 2.0e-9)
          ELSE
             Zt = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-3.2))
             Zt = MIN( Zt, 5.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-4.2))
             Zq = MIN( Zt, 5.0e-5)
             Zq = MAX( Zt, 2.0e-9)
          ENDIF
       ELSE                             !LAND
          Zt = Z_0*EXP(-KARMAN*C1*SQRT(restar))
          Zt = MAX( Zt, Z_0/200.)
          Zt = MIN( Zt, Z_0)

          Zq = Z_0*EXP(-KARMAN*C1*SQRT(restar))
          Zq = MAX( Zq, Z_0/200.)
          Zq = MIN( Zq, Z_0)

          !Zq = Zt
       ENDIF
                   
       return

   END SUBROUTINE zilitinkevich_1995
!--------------------------------------------------------------------
   SUBROUTINE davis_etal_2008(Z_0,ustar)

    !This formulation for roughness length was designed to match 
    !the labratory experiments of Donelan et al. (2004).
    !This formulation produces smaller Z_0 between 5-20 m/s than
    !the Taylor_Yelland_2001 or Charnock subroutines (below).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar
       REAL, INTENT(OUT)  :: Z_0

       Z_0 = 10.*EXP(-10./(ustar**(1./3.)))
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)
                   
       return

   END SUBROUTINE davis_etal_2008
!--------------------------------------------------------------------
   SUBROUTINE Taylor_Yelland_2001(Z_0,ustar,wsp10)

    !This formulation for roughness length was designed account for 
    !wave steepness.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar,wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, parameter  :: g=9.81, pi=3.14159265
       REAL :: hs, Tp, Lp

       !hs is the significant wave height
        hs = 0.0248*(wsp10**2.)
       !Tp dominant wave period
        Tp = 0.729*MAX(wsp10,0.1)
       !Lp is the wavelength of the dominant wave
        Lp = g*Tp**2/(2*pi)

       Z_0 = 1200.*hs*(hs/Lp)**4.5
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)
                   
       return

   END SUBROUTINE Taylor_Yelland_2001
!--------------------------------------------------------------------
   SUBROUTINE charnock_1955(Z_0,ustar,wsp10,visc)
 
    !This version of Charnock's relation employs a varying
    !Charnock parameter, similar to COARE3.0 [Fairall et al. (2003)].
    !The Charnoch parameter CZC is varied from .011 to .018 
    !between 10-m wsp = 10 and 18. 

       IMPLICIT NONE
       REAL, INTENT(IN)  :: ustar, visc, wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER   :: G=9.81, CZO2=0.011
       REAL              :: CZC    !variable charnock "constant"   

       CZC = CZO2 + 0.007*MIN(MAX((wsp10-10.)/8., 0.), 1.0)
       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.1))
       Z_0 = MAX( Z_0, 1.27e-7)  !These max/mins were suggested by
       Z_0 = MIN( Z_0, 2.85e-3)  !Davis et al. (2008)

       return

   END SUBROUTINE charnock_1955
!--------------------------------------------------------------------
   SUBROUTINE garrat_1992(Zt,Zq,Z_0,Ren,landsea)

    !This formulation for the thermal and moisture roughness lengths 
    !(Zt and Zq) relates them to Z0 via the roughness Reynolds number (Ren).
    !This formula comes from Fairall et al. (2003). It is modified from
    !the original Garrat-Brutsaert model to better fit the COARE/HEXMAX
    !data. The formula for land uses a constant ratio (Z_0/7.4) taken
    !from Garrat (1992).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren, Z_0,landsea
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: Rq
       REAL, PARAMETER  :: e=2.71828183

       IF (landsea-1.5 .GT. 0) THEN    !WATER

          Zt = Z_0*EXP(2.0 - (2.48*(Ren**0.25)))
          Zq = Z_0*EXP(2.0 - (2.28*(Ren**0.25)))

          Zq = MIN( Zq, 5.0e-5)
          Zq = MAX( Zq, 2.0e-9)
          Zt = MIN( Zt, 5.0e-5)
          Zt = MAX( Zt, 2.0e-9) !same lower limit as ECMWF
       ELSE                            !LAND
          Zq = Z_0/(e**2.)      !taken from Garrat (1980,1992)
          Zt = Zq
       ENDIF
                   
       return

    END SUBROUTINE garrat_1992
!--------------------------------------------------------------------
    SUBROUTINE fairall_2001(Zt,Zq,Ren,ustar,visc)

    !This formulation for thermal and moisture roughness length (Zt and Zq) 
    !as a function of the roughness Reynolds number (Ren) comes from the 
    !COARE3.0 formulation, empirically derived from COARE and HEXMAX data
    ![Fairall et al. (2003)]. Edson et al. (2004; JGR) suspected that this
    !relationship overestimated roughness lengths for low Reynolds number 
    !flows, so a smooth flow relationship, taken from Garrat (1992, p. 102),
    !is used for flows with Ren < 2. 
    !
    !This is for use over water only.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Ren,ustar,visc
       REAL, INTENT(OUT) :: Zt,Zq

       IF (Ren .le. 2.) then

          Zt = (5.5e-5)*(Ren**(-0.63))
          !FOR SMOOTH SEAS, USE GARRAT FOR Zq
          Zq = 0.2*visc/MAX(ustar,0.1)

       ELSE
          
          !FOR ROUGH SEAS, USE FAIRALL
          Zt = (5.5e-5)*(Ren**(-0.63))
          Zq = Zt
 
       ENDIF

       Zt = MIN(Zt,5.5e-5)
       Zt = MAX(Zt,2.0e-9)

       Zq = MIN(Zt,5.5e-5)
       Zq = MAX(Zt,2.0e-9) 
                   
       return

    END SUBROUTINE fairall_2001
!--------------------------------------------------------------------
    SUBROUTINE Yang_2008(Z_0,Zt,Zq,ustar,tstar,Ren,visc,landsea)

    !This is a modified version of Yang et al (2002 QJRMS, 2008 JAMC) 
    !and Chen et al (2010, J of Hydromet). Although it was originally 
    !designed for arid regions with bare soil, it is modified 
    !here to perform over a broader spectrum of vegetation.
    !
    !The original formulation relates the thermal roughness length (Zt) 
    !to u* and T*:
    !  
    ! Zt = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar)**0.25))
    !
    !where ht = Renc*visc/ustar and the critical Reynolds number 
    !(Renc) = 70. Beta was originally = 10 (2002 paper) but was revised 
    !to 7.2 (in 2008 paper). Their form typically varies the
    !ratio Z0/Zt by a few orders of magnitude (1-1E5).
    !
    !This modified form uses beta = 2.0, so zo/zt generally only varies by
    !10-300 over grass and cropland to about  20-2000 over forests.
    !
    !This should only be used over land!

       IMPLICIT NONE
       REAL, INTENT(IN)  :: Z_0, Ren, ustar, tstar, visc, landsea
       REAL              :: ht, tstar2
       REAL, INTENT(OUT) :: Zt,Zq
       REAL, PARAMETER  :: Renc=70., beta=2.0, e=2.71828183

       ht     = Renc*visc/MAX(ustar,0.01)
       tstar2 = MIN(tstar+0.2,0.0)

       Zt     = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar2)**0.25))
       Zq     = Zt
                   
       Zt = MIN(Zt, Z_0/(e**2.))  !Garrat (1980,1992)
       Zq = MIN(Zq, Z_0/(e**2.)) 

       return

    END SUBROUTINE Yang_2008
!--------------------------------------------------------------------
    SUBROUTINE PSI_Hogstrom_1996(psi_m, psi_h, zL, Zt, Z_0, Za)

    ! This subroutine returns the stability functions based off
    ! of Hogstrom (1996).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  
       zhL = Zt*zL/Za

       IF (zL .gt. 0.) THEN  !STABLE (not well tested - seem large)

          psi_m = -5.3*(zL - zmL)
          psi_h = -8.0*(zL - zhL)
 
       ELSE                 !UNSTABLE

          x = (1.-19.0*zL)**0.25
          x0= (1.-19.0*zmL)**0.25
          y = (1.-11.6*zL)**0.5
          y0= (1.-11.6*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + LOG((1.+x**2.)/(1.+x0**2.)) - &
                  2.*ATAN(x) + 2*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Hogstrom_1996
!--------------------------------------------------------------------
    SUBROUTINE PSI_DyerHicks(psi_m, psi_h, zL, Zt, Z_0, Za)

    ! This subroutine returns the stability functions based off
    ! of Hogstrom (1996), but with different constants compatible
    ! with Dyer and Hicks (1970/74?). This formulation is used for
    ! testing/development by Nakanishi (personal communication).

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, x0, y, y0, zmL, zhL

       zmL = Z_0*zL/Za  
       zhL = Zt*zL/Za

       IF (zL .gt. 0.) THEN  !STABLE

          psi_m = -5.0*(zL - zmL)
          psi_h = -5.0*(zL - zhL)
 
       ELSE                 !UNSTABLE

          x = (1.-16.*zL)**0.25   
          x0= (1.-16.*zmL)**0.25
          !x = (1.-19.*zL)**0.25   !Hogstrom's form
          !x0= (1.-19.*zmL)**0.25

          y = (1.-16.*zL)**0.5
          y0= (1.-16.*zhL)**0.5

          psi_m = 2.*LOG((1.+x)/(1.+x0)) + LOG((1.+x**2.)/(1.+x0**2.)) - &
                  2.*ATAN(x) + 2*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))

       ENDIF
                   
       return

    END SUBROUTINE PSI_DyerHicks
!--------------------------------------------------------------------
    SUBROUTINE PSI_Beljaars_Holtslag_1991(psi_m, psi_h, zL)

    ! This subroutine returns the stability functions based off
    ! of Beljaar and Holtslag 1991, which is an extension of Holtslag
    ! and Debruin 1989.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: a=1., b=0.666, c=5., d=0.35

       IF (zL .lt. 0.) THEN  !UNSTABLE

          WRITE(*,*)"WARNING: Universal stability function from"
          WRITE(*,*)"        Beljaars and Holtslag (1991) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 !STABLE

          psi_m = -(a*zL + b*(zL -(c/d))*exp(-d*zL) + (b*c/d))
          psi_h = -((1.+.666*a*zL)**1.5 + &
                  b*(zL - (c/d))*exp(-d*zL) + (b*c/d) -1.)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Beljaars_Holtslag_1991
!--------------------------------------------------------------------
    SUBROUTINE PSI_Zilitinkevich_Esau_2007(psi_m, psi_h, zL)

    ! This subroutine returns the stability functions come from
    ! Zilitinkevich and Esau (2007, BM), which are formulatioed from the
    ! "generalized similarity theory" and tuned to the LES DATABASE64
    ! to determine their dependence on z/L.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Cm=3.0, Ct=2.5

       IF (zL .lt. 0.) THEN  !UNSTABLE

          WRITE(*,*)"WARNING: Universal stability function from"
          WRITE(*,*)"        Zilitinkevich and Esau (2007) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
 
       ELSE                 !STABLE

          psi_m = -Cm*(zL**(5./6.))
          psi_h = -Ct*(zL**(4./5.))

       ENDIF
                   
       return

    END SUBROUTINE PSI_Zilitinkevich_Esau_2007
!--------------------------------------------------------------------
    SUBROUTINE PSI_Businger_1971(psi_m, psi_h, zL)

    ! This subroutine returns the flux-profile relationships
    ! of Businger el al. 1971.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL  :: x, y
       REAL, PARAMETER  ::  Pi180 = 3.14159265/180.

       IF (zL .lt. 0.) THEN  !UNSTABLE

          x = (1. - 15.0*zL)**0.25
          y = (1. - 9.0*zL)**0.5

          psi_m = LOG(((1.+x)/2.)**2.) + LOG((1.+x**2.)/2.) - &
                           2.*ATAN(x) + Pi180*90.
          psi_h = 2.*LOG((1.+y)/2.)

       ELSE                 !STABLE

          psi_m = -4.7*zL
          psi_h = -(4.7/0.74)*zL

       ENDIF
                   
       return

    END SUBROUTINE PSI_Businger_1971
!--------------------------------------------------------------------
    SUBROUTINE PSI_Suselj_Sood_2010(psi_m, psi_h, zL)

    !This subroutine returns flux-profile relatioships based off
    !of Lobocki (1993), which is derived from the MY-level 2 model.
    !Suselj and Sood (2010) applied the surface layer length scales
    !from Nakanishi (2001) to get this new relationship. These functions
    !are more agressive (larger magnitude) than most formulations. They
    !showed improvement over water, but untested over land.

       IMPLICIT NONE
       REAL, INTENT(IN)  :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER  :: Rfc=0.19, Ric=0.183, PHIT=0.8

       IF (zL .gt. 0.) THEN  !STABLE

          psi_m = -(zL/Rfc + 1.1223*EXP(1.-1.6666/zL))
          !psi_h = -zL*Ric/((Rfc**2.)*PHIT) + 8.209*(zL**1.1091)
          !THEIR EQ FOR PSI_H CRASHES THE MODEL AND DOES NOT MATCH
          !THEIR FIG 1. THIS EQ (BELOW) MATCHES THEIR FIG 1 BETTER:
          psi_h = -(zL*Ric/((Rfc**2.)*5.) + 7.09*(zL**1.1091))
 
       ELSE                 !UNSTABLE

          psi_m = 0.9904*LOG(1. - 14.264*zL)
          psi_h = 1.0103*LOG(1. - 16.3066*zL)

       ENDIF
                   
       return

    END SUBROUTINE PSI_Suselj_Sood_2010
!--------------------------------------------------------------------
    SUBROUTINE Li_etal_2010(zL, Rib, zaz0, z0zt)

    !This subroutine returns a more robust z/L that best matches
    !the z/L from Hogstrom (1996) for unstable conditions and Beljaars
    !and Holtslag (1991) for stable conditions.

       IMPLICIT NONE
       REAL, INTENT(OUT)  :: zL
       REAL, INTENT(IN) :: Rib, zaz0, z0zt
       REAL :: alfa, beta, zaz02, z0zt2
       REAL, PARAMETER  :: au11=0.045, bu11=0.003, bu12=0.0059, &
                           bu21=-0.0828, bu22=0.8845, bu31=0.1739, &
                           bu32=-0.9213, bu33=-0.1057
       REAL, PARAMETER  :: aw11=0.5738, aw12=-0.4399, aw21=-4.901,&
                           aw22=52.50, bw11=-0.0539, bw12=1.540, &
                           bw21=-0.669, bw22=-3.282
       REAL, PARAMETER  :: as11=0.7529, as21=14.94, bs11=0.1569,&
                           bs21=-0.3091, bs22=-1.303
          
       !set limits according to Li et al (2010), p 157.
       zaz02=zaz0
       IF (zaz0 .lt. 100.0) zaz02=100.
       IF (zaz0 .gt. 100000.0) zaz02=100000.

       !set more limits according to Li et al (2010)
       z0zt2=z0zt
       IF (z0zt .lt. 0.5) z0zt2=0.5
       IF (z0zt .gt. 100.0) z0zt2=100.

       alfa = LOG(zaz02)
       beta = LOG(z0zt2)

       IF (Rib .le. 0.0) THEN
          zL = au11*alfa*Rib**2 + (                   &
                  (bu11*beta + bu12)*alfa**2 +        &
                  (bu21*beta + bu22)*alfa    +        &
                  (bu31*beta**2 + bu32*beta + bu33))*Rib
          !if(zL .LT. -15 .OR. zl .GT. 0.)print*,"VIOLATION Rib<0:",zL
          zL = MAX(zL,-15.) !LIMITS SET ACCORDING TO Li et al (2010)
          zL = MIN(zL,0.)   !Figure 1.
       ELSEIF (Rib .gt. 0.0 .AND. Rib .le. 0.2) THEN
          zL = ((aw11*beta + aw12)*alfa +             &
                (aw21*beta + aw22))*Rib**2 +          &
               ((bw11*beta + bw12)*alfa +             &
                (bw21*beta + bw22))*Rib
          !if(zL .LT. 0 .OR. zl .GT. 4)print*,"VIOLATION 0<Rib<0.2:",zL
          zL = MIN(zL,4.) !LIMITS APPROX SET ACCORDING TO Li et al (2010)
          zL = MAX(zL,0.) !THEIR FIGURE 1B.
       ELSE
          zL = (as11*alfa + as21)*Rib + bs11*alfa +   &
                bs21*beta + bs22
          !if(zL .LE. 1 .OR. zl .GT. 23)print*,"VIOLATION Rib>0.2:",zL
          zL = MIN(zL,20.) !LIMITS ACCORDING TO Li et al (2010), THIER
                           !FIGUE 1C.
          zL = MAX(zL,1.)
       ENDIF

       return

    END SUBROUTINE Li_etal_2010
!--------------------------------------------------------------------


END MODULE module_sf_mynn
