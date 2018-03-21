!!
!! LOG
!!  2015-12-10  Weiguo Wang  added gfs scale-aware SAS scheme to HWRF
!!    A description of this updated Scale-aware SAS can be found in the HWRF Scientific Documentation:  
!!    https://dtcenter.org/HurrWRF/users/docs/scientific_documents/HWRFv3.9a_ScientificDoc.pdf


MODULE module_cu_scalesas 

CONTAINS

!-----------------------------------------------------------------
      SUBROUTINE CU_SCALESAS(DT,ITIMESTEP,STEPCU,                        &
                 RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,               &
                 RUCUTEN,RVCUTEN,                                   & 
                 RAINCV,PRATEC,HTOP,HBOT,                           &
                 U3D,V3D,W,T3D,QV3D,QC3D,QI3D,PI3D,RHO3D,           &
                 DZ8W,PCPS,P8W,XLAND,CU_ACT_FLAG,                   &
                 P_QC,                                              & 
                 MOMMIX, & ! gopal's doing
                 PGCON,sas_mass_flux,                               &
                 shalconv,shal_pgcon,                               &
                 HPBL2D,EVAP2D,HEAT2D,                              & !Kwon for shallow convection
                 P_QI,P_FIRST_SCALAR,                               & 
                 DX2D, DY,                                          & ! Wang W for scale-aware cnv
                 SCALEFUN, SCALEFUN1,                               & !cnv scale functions
                 SIGMU,SIGMU1,                                      &
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte                          )

!-------------------------------------------------------------------
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS , ONLY : gfuncphys
      USE MODULE_GFS_PHYSCONS, grav => con_g, CP => con_CP, HVAP => con_HVAP  &
     &,             RV => con_RV, FV => con_fvirt, T0C => con_T0C       &
     &,             CVAP => con_CVAP, CLIQ => con_CLIQ                  & 
     &,             EPS => con_eps, EPSM1 => con_epsm1                  &
     &,             ROVCP => con_rocp, RD => con_rd
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- TH3D	3D potential temperature (K)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- QC3D        3D cloud mixing ratio (Kg/Kg)
!-- QI3D        3D ice mixing ratio (Kg/Kg)
!-- P8w         3D pressure at full levels (Pa)
!-- Pcps        3D pressure (Pa)
!-- PI3D	3D exner function (dimensionless)
!-- rr3D	3D dry air density (kg/m^3)
!-- RUBLTEN     U tendency due to
!               PBL parameterization (m/s^2)
!-- RVBLTEN     V tendency due to
!               PBL parameterization (m/s^2)
!-- RTHBLTEN    Theta tendency due to
!               PBL parameterization (K/s)
!-- RQVBLTEN    Qv tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQCBLTEN    Qc tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQIBLTEN    Qi tendency due to
!               PBL parameterization (kg/kg/s)
!
!-- MOMMIX      MOMENTUM MIXING COEFFICIENT (can be set in the namelist)
!-- RUCUTEN     U tendency due to Cumulus Momentum Mixing (gopal's doing for SAS)
!-- RVCUTEN     V tendency due to Cumulus Momentum Mixing (gopal's doing for SAS)
!
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- GRAV        acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- RD          gas constant for dry air (J/kg/K)
!-- ROVG 	R/G
!-- P_QI	species index for cloud ice
!-- dz8w	dz between full levels (m)
!-- z		height above sea level (m)
!-- PSFC        pressure at the surface (Pa)
!-- UST		u* in similarity theory (m/s)
!-- PBL		PBL height (m)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- HFX		upward heat flux at the surface (W/m^2)
!-- QFX		upward moisture flux at the surface (kg/m^2/s)
!-- TSK		surface temperature (K)
!-- GZ1OZ0      log(z/z0) where z0 is roughness length
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- DT		time step (s)
!-- rvovrd      R_v divided by R_d (dimensionless)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- KARMAN      Von Karman constant
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

      INTEGER ::                        ICLDCK

      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ITIMESTEP,                      &     !NSTD
                                        P_FIRST_SCALAR,                 &
                                        P_QC,                           &
                                        P_QI,                           &
                                        STEPCU

      REAL,    INTENT(IN) ::                                            &
                                        DT

      REAL,    INTENT(IN) ::            DY 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX2D
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SCALEFUN,SCALEFUN1, & !updaft area fraction for deep and shallow cnv
                                                     SIGMU, SIGMU1      !updaft area fraction for      deep and shallow cnv


      REAL, OPTIONAL, INTENT(IN) :: PGCON,sas_mass_flux,shal_pgcon
      INTEGER, OPTIONAL, INTENT(IN) :: shalconv
      REAL(kind=kind_phys)       :: PGCON_USE,SHAL_PGCON_USE,massf
      INTEGER :: shalconv_use
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::      &
                                        RQCCUTEN,                       &
                                        RQICUTEN,                       &
                                        RQVCUTEN,                       &
                                        RTHCUTEN
      REAL, DIMENSION(ims:ime, jms:jme, kms:kme), INTENT(INOUT) ::      &
                                        RUCUTEN,                        &  
                                        RVCUTEN                             
      REAL, OPTIONAL,   INTENT(IN) ::    MOMMIX

      REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                   &
                         INTENT(IN) :: HPBL2D,EVAP2D,HEAT2D                !Kwon for sha

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        RAINCV, PRATEC

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::              &
                                        HBOT,                           &
                                        HTOP

      LOGICAL, DIMENSION(IMS:IME,JMS:JME), INTENT(INOUT) ::             &
                                        CU_ACT_FLAG


      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        DZ8W,                           &
                                        P8w,                            &
                                        Pcps,                           &
                                        PI3D,                           &
                                        QC3D,                           &
                                        QI3D,                           &
                                        QV3D,                           &
                                        RHO3D,                          &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D,                            &
                                        W

!--------------------------- LOCAL VARS ------------------------------

      REAL,    DIMENSION(ims:ime, jms:jme) ::                           &
                                        PSFC

      REAL,    DIMENSION(its:ite, jts:jte) ::                           &
                                        RAINCV1, PRATEC1
      REAL,    DIMENSION(its:ite, jts:jte) ::                           &
                                        RAINCV2, PRATEC2

      REAL     (kind=kind_phys) ::                                      &
                                        DELT,                           &
                                        DPSHC,                          &
                                        RDELT,                          &
                                        RSEED

      REAL     (kind=kind_phys), DIMENSION(its:ite) ::                  &
                                        CLDWRK,                         &
                                        PS,                             &
                                        RCS,                            &
                                        RN,                             &
                                        SLIMSK,                         &
                                        HPBL,EVAP,HEAT                   ! for shallow convection

      REAL     (kind=kind_phys), DIMENSION(its:ite) :: garea             ! grid box area in m^2, 
                                                                         ! scale-aware cnv

      REAL     (kind=kind_phys), DIMENSION(its:ite) :: SCALEFUN_out,SCALEFUN1_out,&  !updraft area fraction for deep and shallow cnv 
                                                      SIGMU_out,SIGMU1_out  !updraft area fraction for deep and shallow cnv


      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte+1) ::       &
                                        PRSI                            

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte) ::         &
                                        DEL,                            &
                                        DOT,                            &
                                        PHIL,                           &
                                        PRSL,                           &
                                        PRSLK,                          &
                                        Q1,                             & 
                                        T1,                             & 
                                        U1,                             & 
                                        V1,                             & 
                                        ZI,                             & 
                                        ZL 

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte) ::         &
                                        cnvw,cnvc,ud_mf,dd_mf,dt_mf      ! *_mf not useful for HWRF. it is for transport

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte, 2) ::      &
                                        QL 

      INTEGER, DIMENSION(its:ite) ::                                    &
                                        KBOT,                           &
                                        KTOP,                           &
                                        KCNV

      INTEGER ::                                                        &
                                        I,                              &
                                        IGPVS,                          &
                                        IM,                             &
                                        J,                              &
                                        JCAP,                           &
                                        K,                              &
                                        KM,                             &
                                        KP,                             &
                                        KX,                             &
                                        NCLOUD 

      DATA IGPVS/0/

!-----------------------------------------------------------------------
!
  !     write(0,*)' in scale-aware sas'

      if(present(shalconv)) then
         shalconv_use=shalconv
      else
#if (NMM_CORE==1)
         shalconv_use=0
#else
#if (EM_CORE==1)
         shalconv_use=1
#else
         shalconv_use=0
#endif
#endif
      endif

      if(present(pgcon)) then
         pgcon_use  = pgcon
      else
!        pgcon_use  = 0.7     ! Gregory et al. (1997, QJRMS)
         pgcon_use  = 0.55    ! Zhang & Wu (2003,JAS), used in GFS (25km res spectral)
!        pgcon_use  = 0.2     ! HWRF, for model tuning purposes
!        pgcon_use  = 0.3     ! GFDL, or so I am told

         ! For those attempting to tune pgcon:

         ! The value of 0.55 comes from an observational study of
         ! synoptic-scale deep convection and 0.7 came from an
         ! incorrect fit to the same data.  That value is likely
         ! correct for deep convection at gridscales near that of GFS,
         ! but is questionable in shallow convection, or for scales
         ! much finer than synoptic scales.

         ! Then again, the assumptions of SAS break down when the
         ! gridscale is near the convection scale anyway.  In a large
         ! storm such as a hurricane, there is often no environment to
         ! detrain into since adjancent gridsquares are also undergoing
         ! active convection.  Each gridsquare will no longer have many
         ! updrafts and downdrafts.  At sub-convective timescales, you
         ! will find unstable columns for many (say, 5 second length)
         ! timesteps in a real atmosphere during a convection cell's
         ! lifetime, so forcing it to be neutrally stable is unphysical.

         ! Hence, in scales near the convection scale (cells have
         ! ~0.5-4km diameter in hurricanes), this parameter is more of a
         ! tuning parameter to get a scheme that is inappropriate for
         ! that resolution to do a reasonable job.

         ! Your mileage might vary.

         ! - Sam Trahan
      endif

      if(present(sas_mass_flux)) then
         massf=sas_mass_flux
         ! Use this to reduce the fluxes added by SAS to prevent
         ! computational instability as a result of large fluxes.
      else
         massf=9e9 ! large number to disable check
      endif

      if(present(shal_pgcon)) then
         if(shal_pgcon>=0) then
            shal_pgcon_use  = shal_pgcon
         else
            ! shal_pgcon<0 means use deep pgcon
            shal_pgcon_use  = pgcon_use
         endif
      else
         ! Default: Same as deep convection pgcon
         shal_pgcon_use  = pgcon_use
         ! Read the warning above though.  It may be advisable for
         ! these to be different.  
      endif

      DO J=JTS,JTE
         DO I=ITS,ITE
            CU_ACT_FLAG(I,J)=.TRUE.
         ENDDO
      ENDDO
 
      IM=ITE-ITS+1
      KX=KTE-KTS+1
      JCAP=126
      DPSHC=30_kind_phys
      DELT=DT*STEPCU
      RDELT=1./DELT
      NCLOUD=1


   DO J=jms,jme
     DO I=ims,ime
       PSFC(i,j)=P8w(i,kms,j)
     ENDDO
   ENDDO

   if(igpvs.eq.0) CALL GFUNCPHYS
   igpvs=1

!-------------  J LOOP (OUTER) --------------------------------------------------

   big_outer_j_loop: DO J=jts,jte

! --------------- compute zi and zl -----------------------------------------
      DO i=its,ite
        ZI(I,KTS)=0.0
      ENDDO

      DO k=kts+1,kte
        KM=K-1
        DO i=its,ite
          ZI(I,K)=ZI(I,KM)+dz8w(i,km,j)
        ENDDO
      ENDDO

      DO k=kts+1,kte
        KM=K-1
        DO i=its,ite
          ZL(I,KM)=(ZI(I,K)+ZI(I,KM))*0.5
        ENDDO
      ENDDO

      DO i=its,ite
        ZL(I,KTE)=2.*ZI(I,KTE)-ZL(I,KTE-1)
      ENDDO

! --------------- end compute zi and zl -------------------------------------

      DO i=its,ite
        !!PS(i)=PSFC(i,j)*.001
        PS(i)=PSFC(i,j)      ! wang, in Pa
        RCS(i)=1.
        SLIMSK(i)=ABS(XLAND(i,j)-2.)

        garea(I)=DX2D(i,j)*DY*2.0         ! grid box area in m^2
        KCNV(I)=0                         ! initialize KCNV
      ENDDO

#if (NMM_CORE == 1)
      if(shalconv_use==1) then
      DO i=its,ite
         HPBL(I) = HPBL2D(I,J)          ! for shallow convection
         EVAP(I) = EVAP2D(I,J)          ! for shallow convection
         HEAT(I) = HEAT2D(I,J)          ! for shallow convection
      ENDDO
      endif
#endif

      DO i=its,ite
        PRSI(i,kts)=PS(i)
      ENDDO

      DO k=kts,kte
        kp=k+1
        DO i=its,ite
          !PRSL(I,K)=Pcps(i,k,j)*.001
          PRSL(I,K)=Pcps(i,k,j)          ! in Pa
          PHIL(I,K)=ZL(I,K)*GRAV
          !DOT(i,k)=-5.0E-4*GRAV*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j))
          DOT(i,k)=-0.5*GRAV*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j)) ! in Pa
        ENDDO
      ENDDO

      DO k=kts,kte
        DO i=its,ite
          DEL(i,k)=PRSL(i,k)*GRAV/RD*dz8w(i,k,j)/T3D(i,k,j)
          U1(i,k)=U3D(i,k,j)
          V1(i,k)=V3D(i,k,j)
          Q1(i,k)=QV3D(i,k,j)/(1.+QV3D(i,k,j))
          T1(i,k)=T3D(i,k,j)
          QL(i,k,1)=QI3D(i,k,j)/(1.+QI3D(i,k,j))
          QL(i,k,2)=QC3D(i,k,j)/(1.+QC3D(i,k,j))
          !PRSLK(I,K)=(PRSL(i,k)*.01)**ROVCP
          PRSLK(I,K)=(PRSL(i,k)*1.0e-5)**ROVCP   ! prsl in Pa
        ENDDO
      ENDDO

      DO k=kts+1,kte+1
        km=k-1
        DO i=its,ite
          PRSI(i,k)=PRSI(i,km)-del(i,km) 
        ENDDO
      ENDDO

       !    write(0,*)'dx2d=',dx2d(its,jts:jts+5)
       !    write(0,*)'dy=',dy
       !    write(0,*)'ps=',ps(its)
       !    write(0,*)'del=',del(its,kts:kts+5)
       !    write(0,*)'prsl=',prsl(its,kts:kts+5)
       !    write(0,*)'dot=',dot(its,kts:kts+5)

!   2016-03-22  near final version of scale-aware convection
       call mfdeepcnv(im,im,kx,delt,del,prsl,ps,phil,ql,       & 
     &     q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kcnv,nint(slimsk),garea,         &
     &     dot,ncloud,ud_mf,dd_mf,dt_mf,cnvw,cnvc, SIGMU_out,SCALEFUN_out)

      do i=its,ite
        RAINCV1(I,J)=RN(I)*1000./STEPCU
        PRATEC1(I,J)=RN(I)*1000./(STEPCU * DT)
      enddo
!
      do i=its,ite
        RAINCV2(I,J)=0.
        PRATEC2(I,J)=0.
      enddo
!
!
      if_shallow_conv: if(shalconv_use==1) then
#if (NMM_CORE == 1)
         ! NMM calls the new shallow convection developed by J Han
         ! (Added to WRF by Y.Kwon)

!   2016-03-22  near final version of scale-aware convection
       call mfshalcnv(im,im,kx,delt,del,prsl,ps,phil,ql,        &
     &     q1,t1,u1,v1,rn,kbot,ktop,kcnv,nint(slimsk),garea,                  &
     &     dot,ncloud,hpbl,ud_mf,dt_mf,cnvw,cnvc,SIGMU1_out,SCALEFUN1_out)

      DO I=ITS,ITE
        RAINCV2(I,J)=RN(I)*1000./STEPCU
        PRATEC2(I,J)=RN(I)*1000./(STEPCU * DT)
      ENDDO
!
#else
#if (EM_CORE == 1)
        ! NOTE: ARW should be able to call the new shalcnv here, but
        ! they need to add the three new variables, so I'm leaving the
        ! old shallow convection call here - Sam Trahan
       
      !! removed  CALL OLD_ARW_SHALCV(IM,IM,KX,DELT,DEL,PRSI,PRSL,PRSLK,KCNV,Q1,T1,DPSHC)
#else
        ! Shallow convection is untested for other cores.
#endif
#endif
     endif if_shallow_conv

!! output updraft area fractions for deep and shallow cnv
      do i=its,ite
        SCALEFUN(I,J)=SCALEFUN_OUT(i)
        SCALEFUN1(I,J)=SCALEFUN1_OUT(i)
        SIGMU(I,J)=SIGMU_OUT(i)
        SIGMU1(I,J)=SIGMU1_OUT(i)
      enddo


        DO I=ITS,ITE
        RAINCV(I,J)= RAINCV1(I,J) + RAINCV2(I,J)
        PRATEC(I,J)= PRATEC1(I,J) + PRATEC2(I,J)
        HBOT(I,J)=KBOT(I)
        HTOP(I,J)=KTOP(I)
      ENDDO

      DO K=KTS,KTE
        DO I=ITS,ITE
          RTHCUTEN(I,K,J)=(T1(I,K)-T3D(I,K,J))/PI3D(I,K,J)*RDELT
          RQVCUTEN(I,K,J)=(Q1(I,K)/(1.-q1(i,k))-QV3D(I,K,J))*RDELT
        ENDDO
      ENDDO

!===============================================================================
!     ADD MOMENTUM MIXING TERM AS TENDENCIES. This is gopal's doing for SAS
!     MOMMIX is the reduction factor set to 0.7 by default. Because NMM has 
!     divergence damping term, a reducion factor for cumulum mixing may be
!     required otherwise storms were too weak.
!===============================================================================
!
#if (NMM_CORE == 1)
      DO K=KTS,KTE
        DO I=ITS,ITE
!         RUCUTEN(I,J,K)=MOMMIX*(U1(I,K)-U3D(I,K,J))*RDELT
!         RVCUTEN(I,J,K)=MOMMIX*(V1(I,K)-V3D(I,K,J))*RDELT
         RUCUTEN(I,J,K)=(U1(I,K)-U3D(I,K,J))*RDELT
         RVCUTEN(I,J,K)=(V1(I,K)-V3D(I,K,J))*RDELT
        ENDDO
      ENDDO
#endif


      IF(P_QC .ge. P_FIRST_SCALAR)THEN
        DO K=KTS,KTE
          DO I=ITS,ITE
            RQCCUTEN(I,K,J)=(QL(I,K,2)/(1.-ql(i,k,2))-QC3D(I,K,J))*RDELT
          ENDDO
        ENDDO
      ENDIF

      IF(P_QI .ge. P_FIRST_SCALAR)THEN
        DO K=KTS,KTE
          DO I=ITS,ITE
            RQICUTEN(I,K,J)=(QL(I,K,1)/(1.-ql(i,k,1))-QI3D(I,K,J))*RDELT
          ENDDO
        ENDDO
      ENDIF

   ENDDO big_outer_j_loop    ! Outer most J loop

   END SUBROUTINE CU_SCALESAS

!====================================================================
   SUBROUTINE scalesasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,          &
                      RUCUTEN,RVCUTEN,                              &   
                      RESTART,P_QC,P_QI,P_FIRST_SCALAR,             &
                      allowed_to_read,                              &
                      ids, ide, jds, jde, kds, kde,                 &
                      ims, ime, jms, jme, kms, kme,                 &
                      its, ite, jts, jte, kts, kte                  )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  allowed_to_read,restart
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::  &
                                                              RTHCUTEN, &
                                                              RQVCUTEN, &
                                                              RQCCUTEN, &
                                                              RQICUTEN
   REAL,     DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(OUT) ::  &
                                                              RUCUTEN,  & ! gopal's doing for SAS
                                                              RVCUTEN   

   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

#ifdef HWRF
!zhang's doing
   IF(.not.restart .or. .not.allowed_to_read)THEN
!end of zhang's doing
#else
   IF(.not.restart)THEN
#endif
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
       RTHCUTEN(i,k,j)=0.
       RQVCUTEN(i,k,j)=0.
       RUCUTEN(i,j,k)=0.   
       RVCUTEN(i,j,k)=0.    
     ENDDO
     ENDDO
     ENDDO

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQCCUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF
   ENDIF

      END SUBROUTINE scalesasinit

!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! scale aware SAS 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine mfdeepcnv(im,ix,km,delt,delp,prslp,psp,phil,ql,       & 
     &     q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kcnv,islimsk,garea,         &
     &     dot,ncloud,ud_mf,dd_mf,dt_mf,cnvw,cnvc, sigmuout,scaldfunc)
!
!      use machine , only : kind_phys
!      use funcphys , only : fpvs
!      use physcons, grav => con_g, cp => con_cp, hvap => con_hvap
       USE MODULE_GFS_MACHINE, ONLY : kind_phys
       USE MODULE_GFS_FUNCPHYS, ONLY : fpvs
       USE MODULE_GFS_PHYSCONS, grav => con_g, cp => con_cp         &
     &  ,            hvap => con_hvap                               &
     &,             rv => con_rv, fv => con_fvirt, t0c => con_t0c    &
     &,             rd => con_rd, cvap => con_cvap, cliq => con_cliq &
     &,             eps => con_eps, epsm1 => con_epsm1



      implicit none
!
      integer            im, ix,  km, ncloud,                      &
     &                   kbot(im), ktop(im), kcnv(im)         
!    &,                  me
      real(kind=kind_phys) delt
      real(kind=kind_phys) psp(im),    delp(ix,km), prslp(ix,km)
      real(kind=kind_phys) ps(im),     del(ix,km),  prsl(ix,km),   &
     &                     ql(ix,km,2),q1(ix,km),   t1(ix,km),     &
     &                     u1(ix,km),  v1(ix,km),                  &
!    &                     u1(ix,km),  v1(ix,km),   rcs(im),
     &                     cldwrk(im), rn(im),      garea(im),     &
     &                     dot(ix,km), phil(ix,km),                &
     &                     cnvw(ix,km),cnvc(ix,km),                &
! hchuang code change mass flux output
     &                     ud_mf(im,km),dd_mf(im,km),dt_mf(im,km)
!
      integer              i, indx, jmn, k, kk, km1, n
      integer, dimension(im), intent(in) :: islimsk
!     integer              latd,lond
!
      real(kind=kind_phys) clam,    cxlamu,  cxlamd,               &
     &                     xlamde,  xlamdd,                        &
     &                     crtlamu, crtlamd
! 
!     real(kind=kind_phys) detad
      real(kind=kind_phys) adw,     aup,     aafac,               &
     &                     beta,    betal,   betas,               &
     &                     c0l,     c0s,     d0,                  &
     &                     c1,      asolfac,                      &
     &                     dellat,  delta,   desdt,   dg,         &
     &                     dh,      dhh,     dp,                  &
     &                     dq,      dqsdp,   dqsdt,   dt,         &
     &                     dt2,     dtmax,   dtmin,               &
     &                     dxcrtas, dxcrtuf,                      &
     &                     dv1h,    dv2h,    dv3h,                &
     &                     dv1q,    dv2q,    dv3q,                &
     &                     dz,      dz1,     e1,      edtmax,     &
     &                     edtmaxl, edtmaxs, el2orc,  elocp,      &
     &                     es,      etah,                           &
     &                     cthk,    dthk,                           &
     &                     evef,    evfact,  evfactl, fact1,         &
     &                     fact2,   factor,                          &
     &                     g,       gamma,   pprime,  cm,            &
     &                     qlk,     qrch,    qs,                   &
     &                     rain,    rfact,   shear,   tfac,        &
     &                     val,     val1,    val2,                 &
     &                     w1,      w1l,     w1s,     w2,          &
     &                     w2l,     w2s,     w3,      w3l,         &
     &                     w3s,     w4,      w4l,     w4s,         &
     &                     rho,     betaw,                        &
     &                     xdby,    xpw,     xpwd,                 &
!    &                     xqrch,   mbdt,    tem,                  &
     &                     xqrch,   tem,     tem1,    tem2,        &  
     &                     ptem,    ptem1,   ptem2,                &
     &                     pgcon
!
      integer              kb(im), kbcon(im), kbcon1(im),           &
     &                     ktcon(im), ktcon1(im), ktconn(im),       &
     &                     jmin(im), lmin(im), kbmax(im),           &
     &                     kbm(im), kmax(im)                        
!
!     real(kind=kind_phys) aa1(im),     acrt(im),   acrtfct(im),    & 
      real(kind=kind_phys) aa1(im),                                 & 
     &                     umean(im),   tauadv(im), gdx(im),        &
     &                     delhbar(im), delq(im),   delq2(im),      &
     &                     delqbar(im), delqev(im), deltbar(im),    &
     &                     deltv(im),   dtconv(im), edt(im),        &
     &                     edto(im),    edtx(im),   fld(im),        &
     &                     hcdo(im,km), hmax(im),   hmin(im),       &
     &                     ucdo(im,km), vcdo(im,km),aa2(im),        &
     &                     pdot(im),    po(im,km),                  &
     &                     pwavo(im),   pwevo(im),  mbdt(im),       &
     &                     qcdo(im,km), qcond(im),  qevap(im),      & 
     &                     rntot(im),   vshear(im), xaa0(im),       &
     &                     xk(im),      xlamd(im),  cina(im),       &
     &                     xmb(im),     xmbmax(im), xpwav(im),      &
     &                     xpwev(im),   xlamx(im),                  &
     &                     delubar(im),delvbar(im)
!
      real(kind=kind_phys) c0(im)
!cj
      real(kind=kind_phys) cinpcr,  cinpcrmx,  cinpcrmn,            &
     &                     cinacr,  cinacrmx,  cinacrmn
!cj
!
!  parameters for updraft velocity calculation
      real(kind=kind_phys) bet1,    cd1,     f1,      gam1,         &
     &                     bb1,     bb2,     wucb
!
!c  physical parameters
      parameter(g=grav,asolfac=0.89)
      parameter(elocp=hvap/cp,el2orc=hvap*hvap/(rv*cp))
      parameter(c0s=.002,c1=.002,d0=.01)
      parameter(c0l=c0s*asolfac)
!
! asolfac: aerosol-aware parameter based on Lim & Hong (2012)
!      asolfac= cx / c0s(=.002)
!      cx = min([-0.7 ln(Nccn) + 24]*1.e-4, c0s)
!      Nccn: CCN number concentration in cm^(-3)
!      Until a realistic Nccn is provided, typical Nccns are assumed
!      as Nccn=100 for sea and Nccn=7000 for land 
!
      parameter(cm=1.0,delta=fv)
      parameter(fact1=(cvap-cliq)/rv,fact2=hvap/rv-fact1*t0c)
      parameter(cthk=200.,dthk=25.)
      parameter(cinpcrmx=180.,cinpcrmn=120.)
      parameter(cinacrmx=-120.,cinacrmn=-120.)
      parameter(bet1=1.875,cd1=.506,f1=2.0,gam1=.5)
      parameter(betaw=.03,dxcrtas=8.e3,dxcrtuf=15.e3)
!
!  local variables and arrays
      real(kind=kind_phys) pfld(im,km),    to(im,km),     qo(im,km),   &
     &                     uo(im,km),      vo(im,km),     qeso(im,km)
!  for updraft velocity calculation
      real(kind=kind_phys) wu2(im,km),     buo(im,km),    drag(im,km)
      real(kind=kind_phys) wc(im),         scaldfunc(im), sigmagfm(im)
    
      real(kind=kind_phys) sigmuout(im)
!
!c  cloud water
!     real(kind=kind_phys) tvo(im,km)
      real(kind=kind_phys) qlko_ktcon(im), dellal(im,km), tvo(im,km),   &
     &                     dbyo(im,km),    zo(im,km),                   &
     &                     xlamue(im,km),  xlamud(im,km),               &
     &                     fent1(im,km),   fent2(im,km),  frh(im,km),   &
     &                     heo(im,km),     heso(im,km),                 &
     &                     qrcd(im,km),    dellah(im,km), dellaq(im,km), &
     &                     dellau(im,km),  dellav(im,km), hcko(im,km),   &
     &                     ucko(im,km),    vcko(im,km),   qcko(im,km),   &
     &                     eta(im,km),     etad(im,km),   zi(im,km),     &
     &                     qrcko(im,km),   qrcdo(im,km),                 &
     &                     pwo(im,km),     pwdo(im,km),   c0t(im,km),    &
     &                     tx1(im),        sumx(im),      cnvwt(im,km)
!    &,                    rhbar(im)
!
      logical totflg, cnvflg(im), asqecflg(im), flg(im)
!
!    asqecflg: flag for the quasi-equilibrium assumption of Arakawa-Schubert
!
!     real(kind=kind_phys) pcrit(15), acritt(15), acrit(15)
!!    save pcrit, acritt
!     data pcrit/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,
!    &           350.,300.,250.,200.,150./
!     data acritt/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,
!    &           .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
!c  gdas derived acrit
!c     data acritt/.203,.515,.521,.566,.625,.665,.659,.688,
!c    &            .743,.813,.886,.947,1.138,1.377,1.896/
      real(kind=kind_phys) tf, tcr, tcrf
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf))
!
!c-----------------------------------------------------------------------
!
!************************************************************************
!     convert input Pa terms to Cb terms  -- Moorthi
      ps   = psp   * 0.001
      prsl = prslp * 0.001
      del  = delp  * 0.001
!************************************************************************
!
!
      km1 = km - 1
!c
!c  initialize arrays
!c
      do i=1,im
        cnvflg(i) = .true.
        rn(i)=0.
        mbdt(i)=10.
        kbot(i)=km+1
        ktop(i)=0
        kbcon(i)=km
        ktcon(i)=1
        ktconn(i)=1
        dtconv(i) = 3600.
        cldwrk(i) = 0.
        pdot(i) = 0.
        lmin(i) = 1
        jmin(i) = 1
        qlko_ktcon(i) = 0.
        edt(i)  = 0.
        edto(i) = 0.
        edtx(i) = 0.
!       acrt(i) = 0.
!       acrtfct(i) = 1.
        aa1(i)  = 0.
        aa2(i)  = 0.
        xaa0(i) = 0.
        cina(i) = 0.
        pwavo(i)= 0.
        pwevo(i)= 0.
        xpwav(i)= 0.
        xpwev(i)= 0.
        vshear(i) = 0.
        gdx(i) = sqrt(garea(i))

         scaldfunc(i)=-1.0   ! initialized wang
         sigmagfm(i)=-1.0
         sigmuout(i)=-1.0

      enddo
!
      do i=1,im
        if(islimsk(i) == 1) then
           c0(i) = c0l
        else
           c0(i) = c0s
        endif
      enddo
      do k = 1, km
        do i = 1, im
          if(t1(i,k) > 273.16) then
            c0t(i,k) = c0(i)
          else
            tem = d0 * (t1(i,k) - 273.16)
            tem1 = exp(tem)
            c0t(i,k) = c0(i) * tem1
          endif
        enddo
      enddo
!
      do k = 1, km
        do i = 1, im
          cnvw(i,k) = 0.
          cnvc(i,k) = 0.
        enddo
      enddo
! hchuang code change
      do k = 1, km
        do i = 1, im
          ud_mf(i,k) = 0.
          dd_mf(i,k) = 0.
          dt_mf(i,k) = 0.
        enddo
      enddo
!c
!     do k = 1, 15
!       acrit(k) = acritt(k) * (975. - pcrit(k))
!     enddo
!
      dt2 = delt
!     val   =         1200.
      val   =         600.
      dtmin = max(dt2, val )
!     val   =         5400.
      val   =         10800.
      dtmax = max(dt2, val )
!c  model tunable parameters are all here
      edtmaxl = .3
      edtmaxs = .3
      clam    = .1
      aafac   = .1
!     betal   = .15
!     betas   = .15
      betal   = .05
      betas   = .05
!c     evef    = 0.07
      evfact  = 0.3
      evfactl = 0.3
!
      crtlamu = 1.0e-4
      crtlamd = 1.0e-4
!
      cxlamu  = 1.0e-3
      cxlamd  = 1.0e-4
      xlamde  = 1.0e-4
      xlamdd  = 1.0e-4
!
!     pgcon   = 0.7     ! Gregory et al. (1997, QJRMS)
      pgcon   = 0.55    ! Zhang & Wu (2003,JAS)
!
      w1l     = -8.e-3 
      w2l     = -4.e-2
      w3l     = -5.e-3 
      w4l     = -5.e-4
      w1s     = -2.e-4
      w2s     = -2.e-3
      w3s     = -1.e-3
      w4s     = -2.e-5
!c
!c  define top layer for search of the downdraft originating layer
!c  and the maximum thetae for updraft
!c
      do i=1,im
        kbmax(i) = km
        kbm(i)   = km
        kmax(i)  = km
        tx1(i)   = 1.0 / ps(i)
      enddo
!     
      do k = 1, km
        do i=1,im
          if (prsl(i,k)*tx1(i) > 0.04) kmax(i)  = k + 1
          if (prsl(i,k)*tx1(i) > 0.45) kbmax(i) = k + 1
          if (prsl(i,k)*tx1(i) > 0.70) kbm(i)   = k + 1
        enddo
      enddo
      do i=1,im
        kmax(i)  = min(km,kmax(i))
        kbmax(i) = min(kbmax(i),kmax(i))
        kbm(i)   = min(kbm(i),kmax(i))
      enddo
!c
!c  hydrostatic height assume zero terr and initially assume
!c    updraft entrainment rate as an inverse function of height 
!c
      do k = 1, km
        do i=1,im
          zo(i,k) = phil(i,k) / g
        enddo
      enddo
      do k = 1, km1
        do i=1,im
          zi(i,k) = 0.5*(zo(i,k)+zo(i,k+1))
          xlamue(i,k) = clam / zi(i,k)
!         xlamue(i,k) = max(xlamue(i,k), crtlamu)
        enddo
      enddo
!c
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c   convert surface pressure to mb from cb
!c
      do k = 1, km
        do i = 1, im
          if (k <= kmax(i)) then
            pfld(i,k) = prsl(i,k) * 10.0
            eta(i,k)  = 1.
            fent1(i,k)= 1.
            fent2(i,k)= 1.
            frh(i,k)  = 0.
            hcko(i,k) = 0.
            qcko(i,k) = 0.
            qrcko(i,k)= 0.
            ucko(i,k) = 0.
            vcko(i,k) = 0.
            etad(i,k) = 1.
            hcdo(i,k) = 0.
            qcdo(i,k) = 0.
            ucdo(i,k) = 0.
            vcdo(i,k) = 0.
            qrcd(i,k) = 0.
            qrcdo(i,k)= 0.
            dbyo(i,k) = 0.
            pwo(i,k)  = 0.
            pwdo(i,k) = 0.
            dellal(i,k) = 0.
            to(i,k)   = t1(i,k)
            qo(i,k)   = q1(i,k)
            uo(i,k)   = u1(i,k)
            vo(i,k)   = v1(i,k)
!           uo(i,k)   = u1(i,k) * rcs(i)
!           vo(i,k)   = v1(i,k) * rcs(i)
            wu2(i,k)  = 0.
            buo(i,k)  = 0.
            drag(i,k) = 0.
            cnvwt(i,k)= 0.
          endif
        enddo
      enddo
!c
!c  column variables
!c  p is pressure of the layer (mb)
!c  t is temperature at t-dt (k)..tn
!c  q is mixing ratio at t-dt (kg/kg)..qn
!c  to is temperature at t+dt (k)... this is after advection and turbulan
!c  qo is mixing ratio at t+dt (kg/kg)..q1
!c
      do k = 1, km
        do i=1,im
          if (k <= kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
!c
!c  compute moist static energy
!c
      do k = 1, km
        do i=1,im
          if (k <= kmax(i)) then
!           tem       = g * zo(i,k) + cp * to(i,k)
            tem       = phil(i,k) + cp * to(i,k)
            heo(i,k)  = tem  + hvap * qo(i,k)
            heso(i,k) = tem  + hvap * qeso(i,k)
!c           heo(i,k)  = min(heo(i,k),heso(i,k))
          endif
        enddo
      enddo
!c
!c  determine level with largest moist static energy
!c  this is the level where updraft starts
!c
      do i=1,im
        hmax(i) = heo(i,1)
        kb(i)   = 1
      enddo
      do k = 2, km
        do i=1,im
          if (k <= kbm(i)) then
            if(heo(i,k) > hmax(i)) then
              kb(i)   = k
              hmax(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
!c
      do k = 1, km1
        do i=1,im
          if (k <= kmax(i)-1) then
            dz      = .5 * (zo(i,k+1) - zo(i,k))
            dp      = .5 * (pfld(i,k+1) - pfld(i,k))
            es      = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime  = pfld(i,k+1) + epsm1 * es
            qs      = eps * es / pprime
            dqsdp   = - qs / pprime
            desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt   = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt      = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq      = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
!
      do k = 1, km1
        do i=1,im
          if (k <= kmax(i)-1) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            frh(i,k)  = 1. - min(qo(i,k)/qeso(i,k), 1._kind_phys)
            heo(i,k)  = .5 * g * (zo(i,k) + zo(i,k+1)) +          &
     &                  cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +          &
     &                  cp * to(i,k) + hvap * qeso(i,k)
            uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
            vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
          endif
        enddo
      enddo
!c
!c  look for the level of free convection as cloud base
!c
      do i=1,im
        flg(i)   = .true.
        kbcon(i) = kmax(i)
      enddo
      do k = 1, km1
        do i=1,im
          if (flg(i) .and. k <= kbmax(i)) then
            if(k > kb(i) .and. heo(i,kb(i)) > heso(i,k)) then
              kbcon(i) = k
              flg(i)   = .false.
            endif
          endif
        enddo
      enddo
!c
      do i=1,im
        if(kbcon(i) == kmax(i)) cnvflg(i) = .false.
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
      do i=1,im
        if(cnvflg(i)) then
!         pdot(i)  = 10.* dot(i,kbcon(i))
          pdot(i)  = 0.01 * dot(i,kbcon(i)) ! Now dot is in Pa/s
        endif
      enddo
!c
!c   turn off convection if pressure depth between parcel source level
!c      and cloud base is larger than a critical value, cinpcr
!c
      do i=1,im
        if(cnvflg(i)) then
          if(islimsk(i) == 1) then
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          endif
          if(pdot(i) <= w4) then
            tem = (pdot(i) - w4) / (w3 - w4)
          elseif(pdot(i) >= -w4) then
            tem = - (pdot(i) + w4) / (w4 - w3)
          else
            tem = 0.
          endif
          val1    =            -1.
          tem = max(tem,val1)
          val2    =             1.
          tem = min(tem,val2)
          ptem = 1. - tem
          ptem1= .5*(cinpcrmx-cinpcrmn)
          cinpcr = cinpcrmx - ptem * ptem1
          tem1 = pfld(i,kb(i)) - pfld(i,kbcon(i))
          if(tem1 > cinpcr) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  assume that updraft entrainment rate above cloud base is
!c    same as that at cloud base
!c
      do i=1,im
        if(cnvflg(i)) then
          xlamx(i) = xlamue(i,kbcon(i))
        endif
      enddo
      do k = 2, km1
        do i=1,im
          if(cnvflg(i).and.                                        &
     &      (k > kbcon(i) .and. k < kmax(i))) then
              xlamue(i,k) = xlamx(i)
          endif
        enddo
      enddo
!c
!c  specify a background (turbulent) detrainment rate for the updrafts
!c
      do k = 1, km1
        do i=1,im
          if(cnvflg(i) .and. k < kmax(i)) then
            xlamud(i,k) = xlamx(i)
!           xlamud(i,k) = crtlamd
          endif
        enddo
      enddo
!c
!c  functions rapidly decreasing with height, mimicking a cloud ensemble
!c    (Bechtold et al., 2008)
!c
      do k = 2, km1
        do i=1,im
          if(cnvflg(i).and.                                        &
     &      (k > kbcon(i) .and. k < kmax(i))) then
              tem = qeso(i,k)/qeso(i,kbcon(i))
              fent1(i,k) = tem**2
              fent2(i,k) = tem**3
          endif
        enddo
      enddo
!c
!c  final entrainment and detrainment rates as the sum of turbulent part and
!c    organized entrainment depending on the environmental relative humidity
!c    (Bechtold et al., 2008)
!c
      do k = 2, km1
        do i=1,im
          if(cnvflg(i) .and.                                            &
     &      (k > kbcon(i) .and. k < kmax(i))) then
              tem = cxlamu * frh(i,k) * fent2(i,k)
              xlamue(i,k) = xlamue(i,k)*fent1(i,k) + tem
!             tem1 = cxlamd * frh(i,k)
!             xlamud(i,k) = xlamud(i,k) + tem1
          endif
        enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
!c  determine updraft mass flux for the subcloud layers
!c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i)) then
            if(k < kbcon(i) .and. k >= kb(i)) then
              dz       = zi(i,k+1) - zi(i,k)
              tem      = 0.5*(xlamud(i,k)+xlamud(i,k+1))
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k+1))-tem
              eta(i,k) = eta(i,k+1) / (1. + ptem * dz)
            endif
          endif
        enddo
      enddo
!c
!c  compute mass flux above cloud base
!c
      do i = 1, im
        flg(i) = cnvflg(i)
      enddo
      do k = 2, km1
        do i = 1, im
         if(flg(i))then
           if(k > kbcon(i) .and. k < kmax(i)) then
              dz       = zi(i,k) - zi(i,k-1)
              tem      = 0.5*(xlamud(i,k)+xlamud(i,k-1))
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k-1))-tem
              eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
              if(eta(i,k) <= 0.) then
                kmax(i) = k
                ktconn(i) = k
                flg(i)   = .false.
              endif
           endif
         endif
        enddo
      enddo
!c
!c  compute updraft cloud properties
!c
      do i = 1, im
        if(cnvflg(i)) then
          indx         = kb(i)
          hcko(i,indx) = heo(i,indx)
          ucko(i,indx) = uo(i,indx)
          vcko(i,indx) = vo(i,indx)
          pwavo(i)     = 0.
        endif
      enddo
!c
!c  cloud property is modified by the entrainment process
!c
!  cm is an enhancement factor in entrainment rates for momentum
!
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < kmax(i)) then
              dz   = zi(i,k) - zi(i,k-1)
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.25 * (xlamud(i,k)+xlamud(i,k-1)) * dz
              factor = 1. + tem - tem1
              hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*          &
     &                     (heo(i,k)+heo(i,k-1)))/factor
              dbyo(i,k) = hcko(i,k) - heso(i,k)
!
              tem  = 0.5 * cm * tem
              factor = 1. + tem
              ptem = tem + pgcon
              ptem1= tem - pgcon
              ucko(i,k) = ((1.-tem)*ucko(i,k-1)+ptem*uo(i,k)      &
     &                     +ptem1*uo(i,k-1))/factor
              vcko(i,k) = ((1.-tem)*vcko(i,k-1)+ptem*vo(i,k)      &
     &                     +ptem1*vo(i,k-1))/factor
            endif
          endif
        enddo
      enddo
!c
!c   taking account into convection inhibition due to existence of
!c    dry layers below cloud base
!c
      do i=1,im
        flg(i) = cnvflg(i)
        kbcon1(i) = kmax(i)
      enddo
      do k = 2, km1
      do i=1,im
        if (flg(i) .and. k < kmax(i)) then
          if(k >= kbcon(i) .and. dbyo(i,k) > 0.) then
            kbcon1(i) = k
            flg(i)    = .false.
          endif
        endif
      enddo
      enddo
      do i=1,im
        if(cnvflg(i)) then
          if(kbcon1(i) == kmax(i)) cnvflg(i) = .false.
        endif
      enddo
      do i=1,im
        if(cnvflg(i)) then
          tem = pfld(i,kbcon(i)) - pfld(i,kbcon1(i))
          if(tem > dthk) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  calculate convective inhibition
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < kbcon1(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma                      &
     &                 * to(i,k) / hvap
              cina(i) = cina(i) +                                   &
!    &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
     &                 dz1 * (g / (cp * to(i,k)))                   &
     &                 * dbyo(i,k) / (1. + gamma)                   &
     &                 * rfact
              val = 0.
              cina(i) = cina(i) +                                   &
!    &                 dz1 * eta(i,k) * g * delta *
     &                 dz1 * g * delta *                            &
     &                 max(val,(qeso(i,k) - qo(i,k)))
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
!
!         if(islimsk(i) == 1) then
!           w1 = w1l
!           w2 = w2l
!           w3 = w3l
!           w4 = w4l
!         else
!           w1 = w1s
!           w2 = w2s
!           w3 = w3s
!           w4 = w4s
!         endif
!         if(pdot(i) <= w4) then
!           tem = (pdot(i) - w4) / (w3 - w4)
!         elseif(pdot(i) >= -w4) then
!           tem = - (pdot(i) + w4) / (w4 - w3)
!         else
!           tem = 0.
!         endif
!
!         val1    =            -1.
!         tem = max(tem,val1)
!         val2    =             1.
!         tem = min(tem,val2)
!         tem = 1. - tem
!         tem1= .5*(cinacrmx-cinacrmn)
!         cinacr = cinacrmx - tem * tem1
!
          cinacr = cinacrmx
          if(cina(i) < cinacr) cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  determine first guess cloud top as the level of zero buoyancy
!c
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon(i) = 1
      enddo
      do k = 2, km1
      do i = 1, im
        if (flg(i) .and. k < kmax(i)) then
          if(k > kbcon1(i) .and. dbyo(i,k) < 0.) then
             ktcon(i) = k
             flg(i)   = .false.
          endif
        endif
      enddo
      enddo
!c
      do i = 1, im
        if(cnvflg(i)) then
          if(ktcon(i) == 1 .and. ktconn(i) > 1) then
             ktcon(i) = ktconn(i)
          endif
          tem = pfld(i,kbcon(i))-pfld(i,ktcon(i))
          if(tem < cthk) cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  search for downdraft originating level above theta-e minimum
!c
      do i = 1, im
        if(cnvflg(i)) then
           hmin(i) = heo(i,kbcon1(i))
           lmin(i) = kbmax(i)
           jmin(i) = kbmax(i)
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i) .and. k <= kbmax(i)) then
            if(k > kbcon1(i) .and. heo(i,k) < hmin(i)) then
               lmin(i) = k + 1
               hmin(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
!c
!c  make sure that jmin(i) is within the cloud
!c
      do i = 1, im
        if(cnvflg(i)) then
          jmin(i) = min(lmin(i),ktcon(i)-1)
          jmin(i) = max(jmin(i),kbcon1(i)+1)
          if(jmin(i) >= ktcon(i)) cnvflg(i) = .false.
        endif
      enddo
!c
!c  specify upper limit of mass flux at cloud base
!c
      do i = 1, im
        if(cnvflg(i)) then
!         xmbmax(i) = .1
!
          k = kbcon(i)
          dp = 1000. * del(i,k)
          xmbmax(i) = dp / (g * dt2)
!         mbdt(i) = 0.1 * dp / g
!
!         tem = dp / (g * dt2)
!         xmbmax(i) = min(tem, xmbmax(i))
        endif
      enddo
!c
!c  compute cloud moisture property and precipitation
!c
      do i = 1, im
        if (cnvflg(i)) then
!         aa1(i) = 0.
          qcko(i,kb(i)) = qo(i,kb(i))
          qrcko(i,kb(i)) = qo(i,kb(i))
!         rhbar(i) = 0.
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < ktcon(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                    &
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
!cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.25 * (xlamud(i,k)+xlamud(i,k-1)) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*         &
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
!cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
!c
!             rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
!c
!c  check if there is excess moisture to release latent heat
!c
              if(k >= kbcon(i) .and. dq > 0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud > 0 .and. k > jmin(i)) then
                  dp = 1000. * del(i,k)
                  ptem = c0t(i,k) + c1
                  qlk = dq / (eta(i,k) + etah * ptem * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0t(i,k) * dz)
                endif
!               aa1(i) = aa1(i) - dz * g * qlk * etah
!               aa1(i) = aa1(i) - dz * g * qlk
                buo(i,k) = buo(i,k) - g * qlk
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0t(i,k) * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
!               cnvwt(i,k) = (etah*qlk + pwo(i,k)) * g / dp
                cnvwt(i,k) = etah * qlk * g / dp
              endif
!
!  compute buoyancy and drag for updraft velocity
!
              if(k >= kbcon(i)) then
                rfact =  1. + delta * cp * gamma                  &
     &                   * to(i,k) / hvap
                buo(i,k) = buo(i,k) + (g / (cp * to(i,k)))          &
     &                   * dbyo(i,k) / (1. + gamma)                &
     &                   * rfact
                val = 0.
                buo(i,k) = buo(i,k) + g * delta *                   &
     &                     max(val,(qeso(i,k) - qo(i,k)))
                drag(i,k) = max(xlamue(i,k),xlamud(i,k))
              endif
!
            endif
          endif
        enddo
      enddo
!c
!     do i = 1, im
!       if(cnvflg(i)) then
!         indx = ktcon(i) - kb(i) - 1
!         rhbar(i) = rhbar(i) / float(indx)
!       endif
!     enddo
!c
!c  calculate cloud work function
!c
!     do k = 2, km1
!       do i = 1, im
!         if (cnvflg(i)) then
!           if(k >= kbcon(i) .and. k < ktcon(i)) then
!             dz1 = zo(i,k+1) - zo(i,k)
!             gamma = el2orc * qeso(i,k) / (to(i,k)**2)
!             rfact =  1. + delta * cp * gamma
!    &                 * to(i,k) / hvap
!             aa1(i) = aa1(i) +
!!   &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
!    &                 dz1 * (g / (cp * to(i,k)))
!    &                 * dbyo(i,k) / (1. + gamma)
!    &                 * rfact
!             val = 0.
!             aa1(i) = aa1(i) +
!!   &                 dz1 * eta(i,k) * g * delta *
!    &                 dz1 * g * delta *
!    &                 max(val,(qeso(i,k) - qo(i,k)))
!           endif
!         endif
!       enddo
!     enddo
!
!  calculate cloud work function
!
      do i = 1, im
        if (cnvflg(i)) then
          aa1(i) = 0.
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k >= kbcon(i) .and. k < ktcon(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
!             aa1(i) = aa1(i) + buo(i,k) * dz1 * eta(i,k)
              aa1(i) = aa1(i) + buo(i,k) * dz1
            endif
          endif
        enddo
      enddo
!
      do i = 1, im
        if(cnvflg(i) .and. aa1(i) <= 0.) cnvflg(i) = .false.
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  estimate the onvective overshooting as the level 
!c    where the [aafac * cloud work function] becomes zero,
!c    which is the final cloud top
!c
      do i = 1, im
        if (cnvflg(i)) then
          aa2(i) = aafac * aa1(i)
        endif
      enddo
!c
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon1(i) = kmax(i)
      enddo
      do k = 2, km1
        do i = 1, im
          if (flg(i)) then
            if(k >= ktcon(i) .and. k < kmax(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma                     &
     &                 * to(i,k) / hvap
              aa2(i) = aa2(i) +                                    &
!    &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
     &                 dz1 * (g / (cp * to(i,k)))                  &
     &                 * dbyo(i,k) / (1. + gamma)                  &
     &                 * rfact
!             val = 0.
!             aa2(i) = aa2(i) +
!!   &                 dz1 * eta(i,k) * g * delta *
!    &                 dz1 * g * delta *
!    &                 max(val,(qeso(i,k) - qo(i,k)))
              if(aa2(i) < 0.) then
                ktcon1(i) = k
                flg(i) = .false.
              endif
            endif
          endif
        enddo
      enddo
!c
!c  compute cloud moisture property, detraining cloud water 
!c    and precipitation in overshooting layers 
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k >= ktcon(i) .and. k < ktcon1(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                           &
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
!cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.25 * (xlamud(i,k)+xlamud(i,k-1)) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                &
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
!cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
!c
!c  check if there is excess moisture to release latent heat
!c
              if(dq > 0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud > 0) then
                  dp = 1000. * del(i,k)
                  ptem = c0t(i,k) + c1
                  qlk = dq / (eta(i,k) + etah * ptem * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0t(i,k) * dz)
                endif
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0t(i,k) * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
!               cnvwt(i,k) = (etah*qlk + pwo(i,k)) * g / dp
                cnvwt(i,k) = etah * qlk * g / dp
              endif
            endif
          endif
        enddo
      enddo
!
!  compute updraft velocity square(wu2)
!
!     bb1 = 2. * (1.+bet1*cd1)
!     bb2 = 2. / (f1*(1.+gam1))
!
!     bb1 = 3.9
!     bb2 = 0.67
!
!     bb1 = 2.0
!     bb2 = 4.0
!
      bb1 = 4.0
      bb2 = 0.8
!
      do i = 1, im
        if (cnvflg(i)) then
          k = kbcon1(i)
          tem = po(i,k) / (rd * to(i,k))
          wucb = -0.01 * dot(i,k) / (tem * g)
          if(wucb > 0.) then
            wu2(i,k) = wucb * wucb
          else
            wu2(i,k) = 0.
          endif
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kbcon1(i) .and. k < ktcon(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              tem  = 0.25 * bb1 * (drag(i,k)+drag(i,k-1)) * dz
              tem1 = 0.5 * bb2 * (buo(i,k)+buo(i,k-1)) * dz
              ptem = (1. - tem) * wu2(i,k-1)
              ptem1 = 1. + tem
              wu2(i,k) = (ptem + tem1) / ptem1
              wu2(i,k) = max(wu2(i,k), 0._kind_phys)
            endif
          endif
        enddo
      enddo
!
!  compute updraft velocity average over the whole cumulus
!
      do i = 1, im
        wc(i) = 0.
        sumx(i) = 0.
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kbcon1(i) .and. k < ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem = 0.5 * (sqrt(wu2(i,k)) + sqrt(wu2(i,k-1)))
              wc(i) = wc(i) + tem * dz
              sumx(i) = sumx(i) + dz
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          if(sumx(i) == 0.) then
             cnvflg(i)=.false.
          else
             wc(i) = wc(i) / sumx(i)
          endif
          val = 1.e-4
          if (wc(i) < val) cnvflg(i)=.false.
        endif
      enddo
!c
!c exchange ktcon with ktcon1
!c
      do i = 1, im
        if(cnvflg(i)) then
          kk = ktcon(i)
          ktcon(i) = ktcon1(i)
          ktcon1(i) = kk
        endif
      enddo
!c
!c  this section is ready for cloud water
!c
      if(ncloud > 0) then
!c
!c  compute liquid and vapor separation at cloud top
!c
      do i = 1, im
        if(cnvflg(i)) then
          k = ktcon(i) - 1
          gamma = el2orc * qeso(i,k) / (to(i,k)**2)
          qrch = qeso(i,k)                                        &
     &         + gamma * dbyo(i,k) / (hvap * (1. + gamma))
          dq = qcko(i,k) - qrch
!c
!c  check if there is excess moisture to release latent heat
!c
          if(dq > 0.) then
            qlko_ktcon(i) = dq
            qcko(i,k) = qrch
          endif
        endif
      enddo
      endif
!c
!ccccc if(lat.==.latd.and.lon.==.lond.and.cnvflg(i)) then
!ccccc   print *, ' aa1(i) before dwndrft =', aa1(i)
!ccccc endif
!c
!c------- downdraft calculations
!c
!c--- compute precipitation efficiency in terms of windshear
!c
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 0.
        endif
      enddo
      do k = 2, km
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k <= ktcon(i)) then
              shear= sqrt((uo(i,k)-uo(i,k-1)) ** 2               &
     &                  + (vo(i,k)-vo(i,k-1)) ** 2)
              vshear(i) = vshear(i) + shear
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i))-zi(i,kb(i)))
          e1=1.591-.639*vshear(i)                                   &
     &       +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
          edt(i)=1.-e1
          val =         .9
          edt(i) = min(edt(i),val)
          val =         .0
          edt(i) = max(edt(i),val)
          edto(i)=edt(i)
          edtx(i)=edt(i)
        endif
      enddo
!c
!c  determine detrainment rate between 1 and kbcon
!c
      do i = 1, im
        if(cnvflg(i)) then
          sumx(i) = 0.
        endif
      enddo
      do k = 1, km1
      do i = 1, im
        if(cnvflg(i)) then
          if(k >= 1 .and. k < kbcon(i)) then
            dz = zi(i,k+1) - zi(i,k)
            sumx(i) = sumx(i) + dz
          endif
        endif
      enddo
      enddo
      do i = 1, im
        beta = betas
        if(islimsk(i) == 1) beta = betal
        if(cnvflg(i)) then
          dz  = (sumx(i)+zi(i,1))/float(kbcon(i))
          tem = 1./float(kbcon(i))
          xlamd(i) = (1.-beta**tem)/dz
        endif
      enddo
!c
!c  determine downdraft mass flux
!c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)-1) then
           if(k < jmin(i) .and. k >= kbcon(i)) then
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1. - ptem * dz)
           else if(k < kbcon(i)) then
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamd(i) + xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1. - ptem * dz)
           endif
          endif
        enddo
      enddo
!c
!c--- downdraft moisture properties
!c
      do i = 1, im
        if(cnvflg(i)) then
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcdo(i,jmn)= qo(i,jmn)
          ucdo(i,jmn) = uo(i,jmn)
          vcdo(i,jmn) = vo(i,jmn)
          pwevo(i) = 0.
        endif
      enddo
!cj
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k < jmin(i)) then
              dz = zi(i,k+1) - zi(i,k)
              if(k >= kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              hcdo(i,k) = ((1.-tem1)*hcdo(i,k+1)+tem*0.5*            &
     &                     (heo(i,k)+heo(i,k+1)))/factor
              dbyo(i,k) = hcdo(i,k) - heso(i,k)
!
              tem  = 0.5 * cm * tem
              factor = 1. + tem
              ptem = tem - pgcon
              ptem1= tem + pgcon
              ucdo(i,k) = ((1.-tem)*ucdo(i,k+1)+ptem*uo(i,k+1)       &
     &                     +ptem1*uo(i,k))/factor
              vcdo(i,k) = ((1.-tem)*vcdo(i,k+1)+ptem*vo(i,k+1)       &
     &                     +ptem1*vo(i,k))/factor
          endif
        enddo
      enddo
!c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k < jmin(i)) then
              gamma      = el2orc * qeso(i,k) / (to(i,k)**2)
              qrcdo(i,k) = qeso(i,k)+                               &
     &                (1./hvap)*(gamma/(1.+gamma))*dbyo(i,k)
!             detad      = etad(i,k+1) - etad(i,k)
!cj
              dz = zi(i,k+1) - zi(i,k)
              if(k >= kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              qcdo(i,k) = ((1.-tem1)*qrcdo(i,k+1)+tem*0.5*          &
     &                     (qo(i,k)+qo(i,k+1)))/factor
!cj
!             pwdo(i,k)  = etad(i,k+1) * qcdo(i,k+1) -
!    &                     etad(i,k) * qrcdo(i,k)
!             pwdo(i,k)  = pwdo(i,k) - detad *
!    &                    .5 * (qrcdo(i,k) + qrcdo(i,k+1))
!cj
              pwdo(i,k)  = etad(i,k) * (qcdo(i,k) - qrcdo(i,k))
              pwevo(i)   = pwevo(i) + pwdo(i,k)
          endif
        enddo
      enddo
!c
!c--- final downdraft strength dependent on precip
!c--- efficiency (edt), normalized condensate (pwav), and
!c--- evaporate (pwev)
!c
      do i = 1, im
        edtmax = edtmaxl
        if(islimsk(i) == 0) edtmax = edtmaxs
        if(cnvflg(i)) then
          if(pwevo(i) < 0.) then
            edto(i) = -edto(i) * pwavo(i) / pwevo(i)
            edto(i) = min(edto(i),edtmax)
          else
            edto(i) = 0.
          endif
        endif
      enddo
!c
!c--- downdraft cloudwork functions
!c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k < jmin(i)) then
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt=to(i,k)
              dg=gamma
              dh=heso(i,k)
              dz=-1.*(zo(i,k+1)-zo(i,k))
!             aa1(i)=aa1(i)+edto(i)*dz*etad(i,k)
              aa1(i)=aa1(i)+edto(i)*dz                             &
     &               *(g/(cp*dt))*((dhh-dh)/(1.+dg))               &
     &               *(1.+delta*cp*dg*dt/hvap)
              val=0.
!             aa1(i)=aa1(i)+edto(i)*dz*etad(i,k)
              aa1(i)=aa1(i)+edto(i)*dz                              &
     &               *g*delta*max(val,(qeso(i,k)-qo(i,k)))
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i) .and. aa1(i) <= 0.) then
           cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c--- what would the change be, that a cloud with unit mass
!c--- will do to the environment?
!c
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. k <= kmax(i)) then
            dellah(i,k) = 0.
            dellaq(i,k) = 0.
            dellau(i,k) = 0.
            dellav(i,k) = 0.
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          dp = 1000. * del(i,1)
          dellah(i,1) = edto(i) * etad(i,1) * (hcdo(i,1)           &
     &                   - heo(i,1)) * g / dp
          dellaq(i,1) = edto(i) * etad(i,1) * (qrcdo(i,1)          &
     &                   - qo(i,1)) * g / dp
          dellau(i,1) = edto(i) * etad(i,1) * (ucdo(i,1)           &
     &                   - uo(i,1)) * g / dp
          dellav(i,1) = edto(i) * etad(i,1) * (vcdo(i,1)           &
     &                   - vo(i,1)) * g / dp
        endif
      enddo
!c
!c--- changed due to subsidence and entrainment
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i) .and. k < ktcon(i)) then
              aup = 1.
              if(k <= kb(i)) aup = 0.
              adw = 1.
              if(k > jmin(i)) adw = 0.
              dp = 1000. * del(i,k)
              dz = zi(i,k) - zi(i,k-1)
!c
              dv1h = heo(i,k)
              dv2h = .5 * (heo(i,k) + heo(i,k-1))
              dv3h = heo(i,k-1)
              dv1q = qo(i,k)
              dv2q = .5 * (qo(i,k) + qo(i,k-1))
              dv3q = qo(i,k-1)
!c
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1))
              tem1 = 0.5 * (xlamud(i,k)+xlamud(i,k-1))
!c
              if(k <= kbcon(i)) then
                ptem  = xlamde
                ptem1 = xlamd(i)+xlamdd
              else
                ptem  = xlamde
                ptem1 = xlamdd
              endif
!cj
              dellah(i,k) = dellah(i,k) +                                  &
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1h                      &
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3h                  &
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2h*dz        &
     &    +  aup*tem1*eta(i,k-1)*.5*(hcko(i,k)+hcko(i,k-1))*dz             &
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(hcdo(i,k)+hcdo(i,k-1))*dz    &
     &         ) *g/dp
!cj
              dellaq(i,k) = dellaq(i,k) +                                   &
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1q                       &
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3q                    &
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2q*dz         &
     &    +  aup*tem1*eta(i,k-1)*.5*(qrcko(i,k)+qcko(i,k-1))*dz             &
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(qrcdo(i,k)+qcdo(i,k-1))*dz     &
     &         ) *g/dp
!cj
              tem1=eta(i,k)*(uo(i,k)-ucko(i,k))
              tem2=eta(i,k-1)*(uo(i,k-1)-ucko(i,k-1))
              ptem1=etad(i,k)*(uo(i,k)-ucdo(i,k))
              ptem2=etad(i,k-1)*(uo(i,k-1)-ucdo(i,k-1))
              dellau(i,k) = dellau(i,k) +                                   &
     &           (aup*(tem1-tem2)-adw*edto(i)*(ptem1-ptem2))*g/dp
!cj
              tem1=eta(i,k)*(vo(i,k)-vcko(i,k))
              tem2=eta(i,k-1)*(vo(i,k-1)-vcko(i,k-1))
              ptem1=etad(i,k)*(vo(i,k)-vcdo(i,k))
              ptem2=etad(i,k-1)*(vo(i,k-1)-vcdo(i,k-1))
              dellav(i,k) = dellav(i,k) +                                   &
     &           (aup*(tem1-tem2)-adw*edto(i)*(ptem1-ptem2))*g/dp
!cj
          endif
        enddo
      enddo
!c
!c------- cloud top
!c
      do i = 1, im
        if(cnvflg(i)) then
          indx = ktcon(i)
          dp = 1000. * del(i,indx)
          dv1h = heo(i,indx-1)
          dellah(i,indx) = eta(i,indx-1) *                           &
     &                     (hcko(i,indx-1) - dv1h) * g / dp
          dv1q = qo(i,indx-1)
          dellaq(i,indx) = eta(i,indx-1) *                           &
     &                     (qcko(i,indx-1) - dv1q) * g / dp
          dellau(i,indx) = eta(i,indx-1) *                             &
     &             (ucko(i,indx-1) - uo(i,indx-1)) * g / dp
          dellav(i,indx) = eta(i,indx-1) *                            &
     &             (vcko(i,indx-1) - vo(i,indx-1)) * g / dp
!c
!c  cloud water
!c
          dellal(i,indx) = eta(i,indx-1) *                             &
     &                     qlko_ktcon(i) * g / dp
        endif
      enddo
!c
!c------- final changed variable per unit mass flux
!c
!  if grid size is less than a threshold value (dxcrtas), 
!    the quasi-equilibrium assumption of Arakawa-Schubert is not
!      used any longer. 
!
      do i = 1, im
         asqecflg(i) = cnvflg(i)
         if(asqecflg(i) .and. gdx(i) < dxcrtas) then
            asqecflg(i) = .false.
         endif
      enddo
!
      do k = 1, km
        do i = 1, im
          if (asqecflg(i) .and. k <= kmax(i)) then
            if(k > ktcon(i)) then
              qo(i,k) = q1(i,k)
              to(i,k) = t1(i,k)
            endif
            if(k <= ktcon(i)) then
              qo(i,k) = dellaq(i,k) * mbdt(i) + q1(i,k)
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              to(i,k) = dellat * mbdt(i) + t1(i,k)
              val   =           1.e-10
              qo(i,k) = max(qo(i,k), val  )
            endif
          endif
        enddo
      enddo
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
!c--- the above changed environment is now used to calulate the
!c--- effect the arbitrary cloud (with unit mass flux)
!c--- would have on the stability,
!c--- which then is used to calculate the real mass flux,
!c--- necessary to keep this change in balance with the large-scale
!c--- destabilization.
!c
!c--- environmental conditions again, first heights
!c
      do k = 1, km
        do i = 1, im
          if(asqecflg(i) .and. k <= kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k)+epsm1*qeso(i,k))
            val       =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
!c
!c--- moist static energy
!c
      do k = 1, km1
        do i = 1, im
          if(asqecflg(i) .and. k <= kmax(i)-1) then
            dz = .5 * (zo(i,k+1) - zo(i,k))
            dp = .5 * (pfld(i,k+1) - pfld(i,k))
            es = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime = pfld(i,k+1) + epsm1 * es
            qs = eps * es / pprime
            dqsdp = - qs / pprime
            desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
      do k = 1, km1
        do i = 1, im
          if(asqecflg(i) .and. k <= kmax(i)-1) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1 * qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)   = .5 * g * (zo(i,k) + zo(i,k+1)) +             &
     &                    cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +               &
     &                  cp * to(i,k) + hvap * qeso(i,k)
          endif
        enddo
      enddo
      do i = 1, im
        if(asqecflg(i)) then
          k = kmax(i)
          heo(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qo(i,k)
          heso(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qeso(i,k)
!c         heo(i,k) = min(heo(i,k),heso(i,k))
        endif
      enddo
!c
!c**************************** static control
!c
!c------- moisture and cloud work functions
!c
      do i = 1, im
        if(asqecflg(i)) then
          xaa0(i) = 0.
          xpwav(i) = 0.
        endif
      enddo
!c
      do i = 1, im
        if(asqecflg(i)) then
          indx = kb(i)
          hcko(i,indx) = heo(i,indx)
          qcko(i,indx) = qo(i,indx)
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (asqecflg(i)) then
            if(k > kb(i) .and. k <= ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.25 * (xlamud(i,k)+xlamud(i,k-1)) * dz
              factor = 1. + tem - tem1
              hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*            &
     &                     (heo(i,k)+heo(i,k-1)))/factor
            endif
          endif
        enddo
      enddo
      do k = 2, km1
        do i = 1, im
          if (asqecflg(i)) then
            if(k > kb(i) .and. k < ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              xdby = hcko(i,k) - heso(i,k)
              xqrch = qeso(i,k)                                      &
     &              + gamma * xdby / (hvap * (1. + gamma))
!cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.25 * (xlamud(i,k)+xlamud(i,k-1)) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*             &
     &                     (qo(i,k)+qo(i,k-1)))/factor
!cj
              dq = eta(i,k) * (qcko(i,k) - xqrch)
!c
              if(k >= kbcon(i) .and. dq > 0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud > 0 .and. k > jmin(i)) then
                  ptem = c0t(i,k) + c1
                  qlk = dq / (eta(i,k) + etah * ptem * dz)
                else
                  qlk = dq / (eta(i,k) + etah * c0t(i,k) * dz)
                endif
                if(k < ktcon1(i)) then
!                 xaa0(i) = xaa0(i) - dz * g * qlk * etah
                  xaa0(i) = xaa0(i) - dz * g * qlk
                endif
                qcko(i,k) = qlk + xqrch
                xpw = etah * c0t(i,k) * dz * qlk
                xpwav(i) = xpwav(i) + xpw
              endif
            endif
            if(k >= kbcon(i) .and. k < ktcon1(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma                       &
     &                 * to(i,k) / hvap
              xaa0(i) = xaa0(i)                                         &
!    &                + dz1 * eta(i,k) * (g / (cp * to(i,k)))
     &                + dz1 * (g / (cp * to(i,k)))                      &
     &                * xdby / (1. + gamma)                           &
     &                * rfact
              val=0.
              xaa0(i) = xaa0(i) +                                     &
!    &                 dz1 * eta(i,k) * g * delta *
     &                 dz1 * g * delta *                              &
     &                 max(val,(qeso(i,k) - qo(i,k)))
            endif
          endif
        enddo
      enddo
!c
!c------- downdraft calculations
!c
!c--- downdraft moisture properties
!c
      do i = 1, im
        if(asqecflg(i)) then
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcd(i,jmn) = qo(i,jmn)
          xpwev(i) = 0.
        endif
      enddo
!cj
      do k = km1, 1, -1
        do i = 1, im
          if (asqecflg(i) .and. k < jmin(i)) then
              dz = zi(i,k+1) - zi(i,k)
              if(k >= kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              hcdo(i,k) = ((1.-tem1)*hcdo(i,k+1)+tem*0.5*           &
     &                     (heo(i,k)+heo(i,k+1)))/factor
          endif
        enddo
      enddo
!cj
      do k = km1, 1, -1
        do i = 1, im
          if (asqecflg(i) .and. k < jmin(i)) then
              dq = qeso(i,k)
              dt = to(i,k)
              gamma    = el2orc * dq / dt**2
              dh       = hcdo(i,k) - heso(i,k)
              qrcd(i,k)=dq+(1./hvap)*(gamma/(1.+gamma))*dh
!             detad    = etad(i,k+1) - etad(i,k)
!cj
              dz = zi(i,k+1) - zi(i,k)
              if(k >= kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              qcdo(i,k) = ((1.-tem1)*qrcd(i,k+1)+tem*0.5*       &
     &                     (qo(i,k)+qo(i,k+1)))/factor
!cj
!             xpwd     = etad(i,k+1) * qcdo(i,k+1) -
!    &                   etad(i,k) * qrcd(i,k)
!             xpwd     = xpwd - detad *
!    &                 .5 * (qrcd(i,k) + qrcd(i,k+1))
!cj
              xpwd     = etad(i,k) * (qcdo(i,k) - qrcd(i,k))
              xpwev(i) = xpwev(i) + xpwd
          endif
        enddo
      enddo
!c
      do i = 1, im
        edtmax = edtmaxl
        if(islimsk(i) == 0) edtmax = edtmaxs
        if(asqecflg(i)) then
          if(xpwev(i) >= 0.) then
            edtx(i) = 0.
          else
            edtx(i) = -edtx(i) * xpwav(i) / xpwev(i)
            edtx(i) = min(edtx(i),edtmax)
          endif
        endif
      enddo
!c
!c
!c--- downdraft cloudwork functions
!c
!c
      do k = km1, 1, -1
        do i = 1, im
          if (asqecflg(i) .and. k < jmin(i)) then
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt= to(i,k)
              dg= gamma
              dh= heso(i,k)
              dz=-1.*(zo(i,k+1)-zo(i,k))
!             xaa0(i)=xaa0(i)+edtx(i)*dz*etad(i,k)
              xaa0(i)=xaa0(i)+edtx(i)*dz                           &
     &                *(g/(cp*dt))*((dhh-dh)/(1.+dg))               &
     &                *(1.+delta*cp*dg*dt/hvap)
              val=0.
!             xaa0(i)=xaa0(i)+edtx(i)*dz*etad(i,k)
              xaa0(i)=xaa0(i)+edtx(i)*dz                            &
     &                *g*delta*max(val,(qeso(i,k)-qo(i,k)))
          endif
        enddo
      enddo
!c
!c  calculate critical cloud work function
!c
!     do i = 1, im
!       if(cnvflg(i)) then
!         if(pfld(i,ktcon(i)) < pcrit(15))then
!           acrt(i)=acrit(15)*(975.-pfld(i,ktcon(i)))
!    &              /(975.-pcrit(15))
!         else if(pfld(i,ktcon(i)) > pcrit(1))then
!           acrt(i)=acrit(1)
!         else
!           k =  int((850. - pfld(i,ktcon(i)))/50.) + 2
!           k = min(k,15)
!           k = max(k,2)
!           acrt(i)=acrit(k)+(acrit(k-1)-acrit(k))*
!    &           (pfld(i,ktcon(i))-pcrit(k))/(pcrit(k-1)-pcrit(k))
!         endif
!       endif
!     enddo
!     do i = 1, im
!       if(cnvflg(i)) then
!         if(islimsk(i) == 1) then
!           w1 = w1l
!           w2 = w2l
!           w3 = w3l
!           w4 = w4l
!         else
!           w1 = w1s
!           w2 = w2s
!           w3 = w3s
!           w4 = w4s
!         endif
!c
!c  modify critical cloud workfunction by cloud base vertical velocity
!c
!         if(pdot(i) <= w4) then
!           acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
!         elseif(pdot(i) >= -w4) then
!           acrtfct(i) = - (pdot(i) + w4) / (w4 - w3)
!         else
!           acrtfct(i) = 0.
!         endif
!         val1    =            -1.
!         acrtfct(i) = max(acrtfct(i),val1)
!         val2    =             1.
!         acrtfct(i) = min(acrtfct(i),val2)
!         acrtfct(i) = 1. - acrtfct(i)
!c
!c  modify acrtfct(i) by colume mean rh if rhbar(i) is greater than 80 percent
!c
!c         if(rhbar(i) >= .8) then
!c           acrtfct(i) = acrtfct(i) * (.9 - min(rhbar(i),.9)) * 10.
!c         endif
!c
!c  modify adjustment time scale by cloud base vertical velocity
!c
!         dtconv(i) = dt2 + max((1800. - dt2),0.) *
!    &                (pdot(i) - w2) / (w1 - w2)
!c         dtconv(i) = max(dtconv(i), dt2)
!c         dtconv(i) = 1800. * (pdot(i) - w2) / (w1 - w2)
!
!         dtconv(i) = max(dtconv(i),dtmin)
!         dtconv(i) = min(dtconv(i),dtmax)
!c
!       endif
!     enddo
!
!  compute convective turn-over time
!
      do i= 1, im
        if(cnvflg(i)) then
          tem = zi(i,ktcon1(i)) - zi(i,kbcon1(i))
          dtconv(i) = tem / wc(i)
          dtconv(i) = max(dtconv(i),dtmin)
          dtconv(i) = min(dtconv(i),dtmax)
        endif
      enddo
!
!  compute advective time scale using a mean cloud layer wind speed
!
      do i= 1, im
        if(cnvflg(i)) then
          sumx(i) = 0.
          umean(i) = 0.
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if(cnvflg(i)) then
            if(k >= kbcon1(i) .and. k < ktcon1(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem = sqrt(u1(i,k)*u1(i,k)+v1(i,k)*v1(i,k))
              umean(i) = umean(i) + tem * dz
              sumx(i) = sumx(i) + dz
            endif
          endif
        enddo
      enddo
      do i= 1, im
        if(cnvflg(i)) then
           umean(i) = umean(i) / sumx(i)
           umean(i) = max(umean(i), 1._kind_phys)
           tauadv(i) = gdx(i) / umean(i)
        endif
      enddo
!c
!c  compute cloud base mass flux as a function of the mean
!c     updraft velcoity for the grid sizes where
!c    the quasi-equilibrium assumption of Arakawa-Schubert is not
!c      valid any longer. 
!c
      do i= 1, im
        if(cnvflg(i) .and. .not.asqecflg(i)) then
          k = kbcon(i)
          rho = po(i,k)*100. / (rd*to(i,k))
          tfac = tauadv(i) / dtconv(i)
          tfac = min(tfac, 1._kind_phys)
          xmb(i) = tfac*betaw*rho*wc(i)
        endif
      enddo
!c
!c  compute cloud base mass flux using
!c    the quasi-equilibrium assumption of Arakawa-Schubert 
!c
      do i= 1, im
        if(asqecflg(i)) then
!         fld(i)=(aa1(i)-acrt(i)*acrtfct(i))/dtconv(i)
          fld(i)=aa1(i)/dtconv(i)
          if(fld(i) <= 0.) then
            asqecflg(i) = .false.
            cnvflg(i) = .false.
          endif
        endif
        if(asqecflg(i)) then
!c         xaa0(i) = max(xaa0(i),0.)
          xk(i) = (xaa0(i) - aa1(i)) / mbdt(i)
          if(xk(i) >= 0.) then
            asqecflg(i) = .false.
            cnvflg(i) = .false.
          endif
        endif
!c
!c--- kernel, cloud base mass flux
!c
        if(asqecflg(i)) then
          tfac = tauadv(i) / dtconv(i)
          tfac = min(tfac, 1._kind_phys)
          xmb(i) = -tfac * fld(i) / xk(i)
!         xmb(i) = min(xmb(i),xmbmax(i))
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!
!--- modified Grell & Freitas' (2014) updraft fraction which uses
!     actual entrainment rate at cloud base
!
      do i = 1, im
        if(cnvflg(i)) then
          tem = min(max(xlamx(i), 7.e-5_kind_phys), 3.e-4_kind_phys)
          tem = 0.2 / tem
          tem1 = 3.14 * tem * tem
          sigmagfm(i) = tem1 / garea(i)
          sigmagfm(i) = max(sigmagfm(i), 0.001_kind_phys)
          sigmagfm(i) = min(sigmagfm(i), 0.999_kind_phys)
        endif
      enddo
!
!--- compute scale-aware function based on Arakawa & Wu (2013)
!
      do i = 1, im
        if(cnvflg(i)) then
          if (gdx(i) < dxcrtuf) then
            scaldfunc(i) = (1.-sigmagfm(i)) * (1.-sigmagfm(i))
            scaldfunc(i) = max(min(scaldfunc(i), 1.0_kind_phys), 0._kind_phys)
            sigmuout(i)=sigmagfm(i) 
          else
            scaldfunc(i) = 1.0
          endif
          xmb(i) = xmb(i) * scaldfunc(i)
          xmb(i) = min(xmb(i),xmbmax(i))
        endif
      enddo
!c
!c  restore to,qo,uo,vo to t1,q1,u1,v1 in case convection stops
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            to(i,k) = t1(i,k)
            qo(i,k) = q1(i,k)
            uo(i,k) = u1(i,k)
            vo(i,k) = v1(i,k)
            qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val     =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
          endif
        enddo
      enddo
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
!c--- feedback: simply the changes from the cloud with unit mass flux
!c---           multiplied by  the mass flux necessary to keep the
!c---           equilibrium with the larger-scale.
!c
      do i = 1, im
        delhbar(i) = 0.
        delqbar(i) = 0.
        deltbar(i) = 0.
        delubar(i) = 0.
        delvbar(i) = 0.
        qcond(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            if(k <= ktcon(i)) then
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
              q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
!             tem = 1./rcs(i)
!             u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
!             v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem
              u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2
              v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2
              dp = 1000. * del(i,k)
              delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g
              delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g
              deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g
              delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/g
              delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/g
            endif
          endif
        enddo
      enddo
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            if(k <= ktcon(i)) then
              qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
              qeso(i,k) = eps * qeso(i,k)/(pfld(i,k) + epsm1*qeso(i,k))
              val     =             1.e-8
              qeso(i,k) = max(qeso(i,k), val )
            endif
          endif
        enddo
      enddo
!c
      do i = 1, im
        rntot(i) = 0.
        delqev(i) = 0.
        delq2(i) = 0.
        flg(i) = cnvflg(i)
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            if(k < ktcon(i)) then
              aup = 1.
              if(k <= kb(i)) aup = 0.
              adw = 1.
              if(k >= jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i) * .001 * dt2
            endif
          endif
        enddo
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (k <= kmax(i)) then
            deltv(i) = 0.
            delq(i) = 0.
            qevap(i) = 0.
            if(cnvflg(i) .and. k < ktcon(i)) then
              aup = 1.
              if(k <= kb(i)) aup = 0.
              adw = 1.
              if(k >= jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * .001 * dt2
            endif
            if(flg(i) .and. k < ktcon(i)) then
              evef = edt(i) * evfact
              if(islimsk(i) == 1) evef=edt(i) * evfactl
!             if(islimsk(i) == 1) evef=.07
!c             if(islimsk(i) == 1) evef = 0.
              qcond(i) = evef * (q1(i,k) - qeso(i,k))                   &
     &                 / (1. + el2orc * qeso(i,k) / t1(i,k)**2)
              dp = 1000. * del(i,k)
              if(rn(i) > 0. .and. qcond(i) < 0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dt2*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i) > 0. .and. qcond(i) < 0. .and.                   &
     &           delq2(i) > rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i) > 0. .and. qevap(i) > 0.) then
                q1(i,k) = q1(i,k) + qevap(i)
                t1(i,k) = t1(i,k) - elocp * qevap(i)
                rn(i) = rn(i) - .001 * qevap(i) * dp / g
                deltv(i) = - elocp*qevap(i)/dt2
                delq(i) =  + qevap(i)/dt2
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
              delqbar(i) = delqbar(i) + delq(i)*dp/g
              deltbar(i) = deltbar(i) + deltv(i)*dp/g
            endif
          endif
        enddo
      enddo
!cj
!     do i = 1, im
!     if(me == 31 .and. cnvflg(i)) then
!     if(cnvflg(i)) then
!       print *, ' deep delhbar, delqbar, deltbar = ',
!    &             delhbar(i),hvap*delqbar(i),cp*deltbar(i)
!       print *, ' deep delubar, delvbar = ',delubar(i),delvbar(i)
!       print *, ' precip =', hvap*rn(i)*1000./dt2
!       print*,'pdif= ',pfld(i,kbcon(i))-pfld(i,ktcon(i))
!     endif
!     enddo
!c
!c  precipitation rate converted to actual precip
!c  in unit of m instead of kg
!c
      do i = 1, im
        if(cnvflg(i)) then
!c
!c  in the event of upper level rain evaporation and lower level downdraft
!c    moistening, rn can become negative, in this case, we back out of the
!c    heating and the moistening
!c
          if(rn(i) < 0. .and. .not.flg(i)) rn(i) = 0.
          if(rn(i) <= 0.) then
            rn(i) = 0.
          else
            ktop(i) = ktcon(i)
            kbot(i) = kbcon(i)
            kcnv(i) = 1
            cldwrk(i) = aa1(i)
          endif
        endif
      enddo
!c
!c  convective cloud water
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i) > 0.) then
            if (k >= kbcon(i) .and. k < ktcon(i)) then
              cnvw(i,k) = cnvwt(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
!c
!c  convective cloud cover
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i) > 0.) then
            if (k >= kbcon(i) .and. k < ktcon(i)) then
              cnvc(i,k) = 0.04 * log(1. + 675. * eta(i,k) * xmb(i)) 
              cnvc(i,k) = min(cnvc(i,k), 0.6_kind_phys)
              cnvc(i,k) = max(cnvc(i,k), 0.0_kind_phys)
            endif
          endif
        enddo
      enddo

!c
!c  cloud water
!c
      if (ncloud > 0) then
!
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i) > 0.) then
!           if (k > kb(i) .and. k <= ktcon(i)) then
            if (k >= kbcon(i) .and. k <= ktcon(i)) then
              tem  = dellal(i,k) * xmb(i) * dt2
              tem1 = max(0.0_kind_phys, min(1.0_kind_phys, (tcr-t1(i,k))*tcrf))
              if (ql(i,k,2) > -999.0) then
                ql(i,k,1) = ql(i,k,1) + tem * tem1            ! ice
                ql(i,k,2) = ql(i,k,2) + tem *(1.0-tem1)       ! water
              else
                ql(i,k,1) = ql(i,k,1) + tem
              endif
            endif
          endif
        enddo
      enddo
!
      endif
!c
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. rn(i) <= 0.) then
            if (k <= kmax(i)) then
              t1(i,k) = to(i,k)
              q1(i,k) = qo(i,k)
              u1(i,k) = uo(i,k)
              v1(i,k) = vo(i,k)
            endif
          endif
        enddo
      enddo
!
! hchuang code change
!
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. rn(i) > 0.) then
            if(k >= kb(i) .and. k < ktop(i)) then
              ud_mf(i,k) = eta(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i) .and. rn(i) > 0.) then
           k = ktop(i)-1
           dt_mf(i,k) = ud_mf(i,k)
        endif
      enddo
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. rn(i) > 0.) then
            if(k >= 1 .and. k <= jmin(i)) then
              dd_mf(i,k) = edto(i) * etad(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
!!
      return
      end subroutine mfdeepcnv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine mfshalcnv(im,ix,km,delt,delp,prslp,psp,phil,ql,        &
     &     q1,t1,u1,v1,rn,kbot,ktop,kcnv,islimsk,garea,                  &
!     &     dot,ncloud,hpbl,ud_mf,dt_mf,cnvw,cnvc)
     &     dot,ncloud,hpbl,ud_mf,dt_mf,cnvw,cnvc,sigmagfm,scaldfunc)
!    &     dot,ncloud,hpbl,ud_mf,dt_mf,cnvw,cnvc,me)
!
!      use machine , only : kind_phys
!      use funcphys , only : fpvs
!      use physcons, grav => con_g, cp => con_cp, hvap => con_hvap
       USE MODULE_GFS_MACHINE, ONLY : kind_phys
       USE MODULE_GFS_FUNCPHYS, ONLY : fpvs
       USE MODULE_GFS_PHYSCONS, grav => con_g, cp => con_cp         &
     &  ,            hvap => con_hvap                               &
     &,             rv => con_rv, fv => con_fvirt, t0c => con_t0c    &
     &,             rd => con_rd, cvap => con_cvap, cliq => con_cliq &
     &,             eps => con_eps, epsm1 => con_epsm1



      implicit none
!
      integer            im, ix,  km, ncloud,                           &
     &                   kbot(im), ktop(im), kcnv(im) 
!    &,                  me
      real(kind=kind_phys) delt
      real(kind=kind_phys) psp(im),    delp(ix,km), prslp(ix,km)
      real(kind=kind_phys) ps(im),     del(ix,km),  prsl(ix,km),         &
     &                     ql(ix,km,2),q1(ix,km),   t1(ix,km),            &
     &                     u1(ix,km),  v1(ix,km),                          &
!    &                     u1(ix,km),  v1(ix,km),   rcs(im),
     &                     rn(im),     garea(im),                          &
     &                     dot(ix,km), phil(ix,km), hpbl(im),              &
     &                     cnvw(ix,km),cnvc(ix,km)                        &
! hchuang code change mass flux output
     &,                    ud_mf(im,km),dt_mf(im,km)
!
      integer              i,j,indx, k, kk, km1, n
      integer              kpbl(im)
      integer, dimension(im), intent(in) :: islimsk
!
      real(kind=kind_phys) dellat,  delta,                                &
     &                     c0l,     c0s,     d0,                          &
     &                     c1,      asolfac,                              &
     &                     desdt,   dp,                                   &
     &                     dq,      dqsdp,   dqsdt,   dt,                &
     &                     dt2,     dtmax,   dtmin,   dxcrt,              &
     &                     dv1h,    dv2h,    dv3h,                        &
     &                     dv1q,    dv2q,    dv3q,                        &
     &                     dz,      dz1,     e1,      clam,               &  
     &                     el2orc,  elocp,   aafac,   cm,                 &
     &                     es,      etah,    h1,                          &
     &                     evef,    evfact,  evfactl, fact1,               &
     &                     fact2,   factor,  dthk,                         & 
     &                     g,       gamma,   pprime,  betaw,              &
     &                     qlk,     qrch,    qs,                           &
     &                     rfact,   shear,   tfac,                         &
     &                     val,     val1,    val2,                         &
     &                     w1,      w1l,     w1s,     w2,                  &
     &                     w2l,     w2s,     w3,      w3l,                 &
     &                     w3s,     w4,      w4l,     w4s,                 &
     &                     rho,     tem,     tem1,    tem2,                &
     &                     ptem,    ptem1,                                 &
     &                     pgcon
!
      integer              kb(im), kbcon(im), kbcon1(im),                  &
     &                     ktcon(im), ktcon1(im), ktconn(im),              &
     &                     kbm(im), kmax(im)
!
      real(kind=kind_phys) aa1(im),     cina(im),                          &
     &                     umean(im),  tauadv(im),  gdx(im),                &
     &                     delhbar(im), delq(im),   delq2(im),              &
     &                     delqbar(im), delqev(im), deltbar(im),            &
     &                     deltv(im),   dtconv(im), edt(im),                 &
     &                     pdot(im),    po(im,km),                          &
     &                     qcond(im),   qevap(im),  hmax(im),               &
     &                     rntot(im),   vshear(im),                          &
     &                     xlamud(im),  xmb(im),    xmbmax(im),              &
     &                     delubar(im), delvbar(im)   
!
      real(kind=kind_phys) c0(im)
!c
      real(kind=kind_phys) crtlamd
!
      real(kind=kind_phys) cinpcr,  cinpcrmx,  cinpcrmn,                &
     &                     cinacr,  cinacrmx,  cinacrmn
!
!  parameters for updraft velocity calculation
      real(kind=kind_phys) bet1,    cd1,     f1,      gam1,              &
     &                     bb1,     bb2,     wucb
!cc
!c  physical parameters
      parameter(g=grav,asolfac=0.89)
      parameter(elocp=hvap/cp,                                         &
     &          el2orc=hvap*hvap/(rv*cp))
      parameter(c0s=0.002,c1=5.e-4,d0=.01)
      parameter(c0l=c0s*asolfac)
!
! asolfac: aerosol-aware parameter based on Lim & Hong (2012)
!      asolfac= cx / c0s(=.002)
!      cx = min([-0.7 ln(Nccn) + 24]*1.e-4, c0s)
!      Nccn: CCN number concentration in cm^(-3)
!      Until a realistic Nccn is provided, typical Nccns are assumed
!      as Nccn=100 for sea and Nccn=7000 for land
!
      parameter(cm=1.0,delta=fv)
      parameter(fact1=(cvap-cliq)/rv,fact2=hvap/rv-fact1*t0c)
      parameter(dthk=25.)
      parameter(cinpcrmx=180.,cinpcrmn=120.)
      parameter(cinacrmx=-120.,cinacrmn=-120.)
      parameter(crtlamd=3.e-4)
      parameter(dtmax=10800.,dtmin=600.)
      parameter(bet1=1.875,cd1=.506,f1=2.0,gam1=.5)
      parameter(betaw=.03,dxcrt=15.e3)
      parameter(h1=0.33333333)
!c  local variables and arrays
      real(kind=kind_phys) pfld(im,km),    to(im,km),     qo(im,km),    &
     &                     uo(im,km),      vo(im,km),     qeso(im,km)
!  for updraft velocity calculation
      real(kind=kind_phys) wu2(im,km),     buo(im,km),    drag(im,km)
      real(kind=kind_phys) wc(im),         scaldfunc(im), sigmagfm(im)
!
!c  cloud water
!     real(kind=kind_phys) qlko_ktcon(im), dellal(im,km), tvo(im,km),
      real(kind=kind_phys) qlko_ktcon(im), dellal(im,km),                    &
     &                     dbyo(im,km),    zo(im,km),     xlamue(im,km),      &
     &                     heo(im,km),     heso(im,km),                       &
     &                     dellah(im,km),  dellaq(im,km),                     & 
     &                     dellau(im,km),  dellav(im,km), hcko(im,km),        &
     &                     ucko(im,km),    vcko(im,km),   qcko(im,km),        &
     &                     qrcko(im,km),   eta(im,km),                        &
     &                     zi(im,km),      pwo(im,km),    c0t(im,km),         &
     &                     sumx(im),       tx1(im),       cnvwt(im,km) 
!
      logical totflg, cnvflg(im), flg(im)
!
      real(kind=kind_phys) tf, tcr, tcrf
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf))
!
!c-----------------------------------------------------------------------
!
!************************************************************************
!     convert input Pa terms to Cb terms  -- Moorthi
      ps   = psp   * 0.001
      prsl = prslp * 0.001
      del  = delp  * 0.001
!************************************************************************
!
      km1 = km - 1
!c
!c  initialize arrays
!c
      do i=1,im
        cnvflg(i) = .true.
        if(kcnv(i) == 1) cnvflg(i) = .false.
        if(cnvflg(i)) then
          kbot(i)=km+1
          ktop(i)=0
        endif
        rn(i)=0.
        kbcon(i)=km
        ktcon(i)=1
        ktconn(i)=1
        kb(i)=km
        pdot(i) = 0.
        qlko_ktcon(i) = 0.
        edt(i)  = 0.
        aa1(i)  = 0.
        cina(i) = 0.
        vshear(i) = 0.
        gdx(i) = sqrt(garea(i))
          scaldfunc(i)=-1.0  ! wang initialized
          sigmagfm(i)=-1.0
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
      do i=1,im
        if(islimsk(i) == 1) then
           c0(i) = c0l
        else
           c0(i) = c0s
        endif
      enddo
!
      do k = 1, km
        do i = 1, im
          if(t1(i,k) > 273.16) then
            c0t(i,k) = c0(i)
          else
            tem = d0 * (t1(i,k) - 273.16)
            tem1 = exp(tem)
            c0t(i,k) = c0(i) * tem1
          endif
        enddo
      enddo
!
      do k = 1, km
        do i = 1, im
          cnvw(i,k) = 0.
          cnvc(i,k) = 0.
        enddo
      enddo
! hchuang code change
      do k = 1, km
        do i = 1, im
          ud_mf(i,k) = 0.
          dt_mf(i,k) = 0.
        enddo
      enddo
!c
      dt2   = delt
!
!c  model tunable parameters are all here
      clam    = .3
      aafac   = .1
!c     evef    = 0.07
      evfact  = 0.3
      evfactl = 0.3
!
!     pgcon   = 0.7     ! Gregory et al. (1997, QJRMS)
      pgcon   = 0.55    ! Zhang & Wu (2003,JAS)
      w1l     = -8.e-3 
      w2l     = -4.e-2
      w3l     = -5.e-3 
      w4l     = -5.e-4
      w1s     = -2.e-4
      w2s     = -2.e-3
      w3s     = -1.e-3
      w4s     = -2.e-5
!c
!c  define top layer for search of the downdraft originating layer
!c  and the maximum thetae for updraft
!c
      do i=1,im
        kbm(i)   = km
        kmax(i)  = km
        tx1(i)   = 1.0 / ps(i)
      enddo
!     
      do k = 1, km
        do i=1,im
          if (prsl(i,k)*tx1(i) > 0.70) kbm(i)   = k + 1
          if (prsl(i,k)*tx1(i) > 0.60) kmax(i)  = k + 1
        enddo
      enddo
      do i=1,im
        kbm(i)   = min(kbm(i),kmax(i))
      enddo
!c
!c  hydrostatic height assume zero terr and compute
!c  updraft entrainment rate as an inverse function of height
!c
      do k = 1, km
        do i=1,im
          zo(i,k) = phil(i,k) / g
        enddo
      enddo
      do k = 1, km1
        do i=1,im
          zi(i,k) = 0.5*(zo(i,k)+zo(i,k+1))
          xlamue(i,k) = clam / zi(i,k)
        enddo
      enddo
      do i=1,im
        xlamue(i,km) = xlamue(i,km1)
      enddo
!c
!c  pbl height
!c
      do i=1,im
        flg(i) = cnvflg(i)
        kpbl(i)= 1
      enddo
      do k = 2, km1
        do i=1,im
          if (flg(i) .and. zo(i,k) <= hpbl(i)) then
            kpbl(i) = k
          else
            flg(i) = .false.
          endif
        enddo
      enddo
      do i=1,im
        kpbl(i)= min(kpbl(i),kbm(i))
      enddo
!c
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c   convert surface pressure to mb from cb
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            pfld(i,k) = prsl(i,k) * 10.0
            eta(i,k)  = 1.
            hcko(i,k) = 0.
            qcko(i,k) = 0.
            qrcko(i,k)= 0.
            ucko(i,k) = 0.
            vcko(i,k) = 0.
            dbyo(i,k) = 0.
            pwo(i,k)  = 0.
            dellal(i,k) = 0.
            to(i,k)   = t1(i,k)
            qo(i,k)   = q1(i,k)
            uo(i,k)   = u1(i,k)
            vo(i,k)   = v1(i,k)
!           uo(i,k)   = u1(i,k) * rcs(i)
!           vo(i,k)   = v1(i,k) * rcs(i)
            wu2(i,k)  = 0.
            buo(i,k)  = 0.
            drag(i,k) = 0.
            cnvwt(i,k) = 0.
          endif
        enddo
      enddo
!c
!c  column variables
!c  p is pressure of the layer (mb)
!c  t is temperature at t-dt (k)..tn
!c  q is mixing ratio at t-dt (kg/kg)..qn
!c  to is temperature at t+dt (k)... this is after advection and turbulan
!c  qo is mixing ratio at t+dt (kg/kg)..q1
!c
      do k = 1, km
        do i=1,im
          if (cnvflg(i) .and. k <= kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
!c
!c  compute moist static energy
!c
      do k = 1, km
        do i=1,im
          if (cnvflg(i) .and. k <= kmax(i)) then
!           tem       = g * zo(i,k) + cp * to(i,k)
            tem       = phil(i,k) + cp * to(i,k)
            heo(i,k)  = tem  + hvap * qo(i,k)
            heso(i,k) = tem  + hvap * qeso(i,k)
!c           heo(i,k)  = min(heo(i,k),heso(i,k))
          endif
        enddo
      enddo
!c
!c  determine level with largest moist static energy within pbl
!c  this is the level where updraft starts
!c
      do i=1,im
         if (cnvflg(i)) then
            hmax(i) = heo(i,1)
            kb(i) = 1
         endif
      enddo
      do k = 2, km
        do i=1,im
          if (cnvflg(i) .and. k <= kpbl(i)) then
            if(heo(i,k) > hmax(i)) then
              kb(i)   = k
              hmax(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
!c
      do k = 1, km1
        do i=1,im
          if (cnvflg(i) .and. k <= kmax(i)-1) then
            dz      = .5 * (zo(i,k+1) - zo(i,k))
            dp      = .5 * (pfld(i,k+1) - pfld(i,k))
            es      = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime  = pfld(i,k+1) + epsm1 * es
            qs      = eps * es / pprime
            dqsdp   = - qs / pprime
            desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt   = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt      = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq      = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
!
      do k = 1, km1
        do i=1,im
          if (cnvflg(i) .and. k <= kmax(i)-1) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)  = .5 * g * (zo(i,k) + zo(i,k+1)) +             &
     &                  cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +              &
     &                  cp * to(i,k) + hvap * qeso(i,k)
            uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
            vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
          endif
        enddo
      enddo
!c
!c  look for the level of free convection as cloud base
!c
      do i=1,im
        flg(i)   = cnvflg(i)
        if(flg(i)) kbcon(i) = kmax(i)
      enddo
      do k = 2, km1
        do i=1,im
          if (flg(i) .and. k < kbm(i)) then
            if(k > kb(i) .and. heo(i,kb(i)) > heso(i,k)) then
              kbcon(i) = k
              flg(i)   = .false.
            endif
          endif
        enddo
      enddo
!c
      do i=1,im
        if(cnvflg(i)) then
          if(kbcon(i) == kmax(i)) cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
      do i=1,im
        if(cnvflg(i)) then
!         pdot(i)  = 10.* dot(i,kbcon(i))
          pdot(i)  = 0.01 * dot(i,kbcon(i)) ! Now dot is in Pa/s
        endif
      enddo
!c
!c   turn off convection if pressure depth between parcel source level
!c      and cloud base is larger than a critical value, cinpcr
!c
      do i=1,im
        if(cnvflg(i)) then
          if(islimsk(i) == 1) then
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          endif
          if(pdot(i) <= w4) then
            tem = (pdot(i) - w4) / (w3 - w4)
          elseif(pdot(i) >= -w4) then
            tem = - (pdot(i) + w4) / (w4 - w3)
          else
            tem = 0.
          endif
          val1    =            -1.
          tem = max(tem,val1)
          val2    =             1.
          tem = min(tem,val2)
          ptem = 1. - tem
          ptem1= .5*(cinpcrmx-cinpcrmn)
          cinpcr = cinpcrmx - ptem * ptem1
          tem1 = pfld(i,kb(i)) - pfld(i,kbcon(i))
          if(tem1 > cinpcr) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  specify the detrainment rate for the updrafts
!c
      do i = 1, im
        if(cnvflg(i)) then
          xlamud(i) = xlamue(i,kbcon(i))
!         xlamud(i) = crtlamd
        endif
      enddo
!c
!c  determine updraft mass flux for the subcloud layers
!c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i)) then
            if(k < kbcon(i) .and. k >= kb(i)) then
              dz       = zi(i,k+1) - zi(i,k)
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k+1))-xlamud(i)
              eta(i,k) = eta(i,k+1) / (1. + ptem * dz)
            endif
          endif
        enddo
      enddo
!c
!c  compute mass flux above cloud base
!c
      do i = 1, im
        flg(i) = cnvflg(i)
      enddo
      do k = 2, km1
        do i = 1, im
         if(flg(i))then
           if(k > kbcon(i) .and. k < kmax(i)) then
              dz       = zi(i,k) - zi(i,k-1)
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k-1))-xlamud(i)
              eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
              if(eta(i,k) <= 0.) then
                kmax(i) = k
                ktconn(i) = k
                kbm(i) = min(kbm(i),kmax(i))
                flg(i) = .false.
              endif
           endif
         endif
        enddo
      enddo
!c
!c  compute updraft cloud property
!c
      do i = 1, im
        if(cnvflg(i)) then
          indx         = kb(i)
          hcko(i,indx) = heo(i,indx)
          ucko(i,indx) = uo(i,indx)
          vcko(i,indx) = vo(i,indx)
        endif
      enddo
!c
!  cm is an enhancement factor in entrainment rates for momentum
!
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < kmax(i)) then
              dz   = zi(i,k) - zi(i,k-1)
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*              &
     &                     (heo(i,k)+heo(i,k-1)))/factor
              dbyo(i,k) = hcko(i,k) - heso(i,k)
!
              tem  = 0.5 * cm * tem
              factor = 1. + tem
              ptem = tem + pgcon
              ptem1= tem - pgcon
              ucko(i,k) = ((1.-tem)*ucko(i,k-1)+ptem*uo(i,k)             &
     &                     +ptem1*uo(i,k-1))/factor
              vcko(i,k) = ((1.-tem)*vcko(i,k-1)+ptem*vo(i,k)              &
     &                     +ptem1*vo(i,k-1))/factor
            endif
          endif
        enddo
      enddo
!c
!c   taking account into convection inhibition due to existence of
!c    dry layers below cloud base
!c
      do i=1,im
        flg(i) = cnvflg(i)
        kbcon1(i) = kmax(i)
      enddo
      do k = 2, km1
      do i=1,im
        if (flg(i) .and. k < kbm(i)) then
          if(k >= kbcon(i) .and. dbyo(i,k) > 0.) then
            kbcon1(i) = k
            flg(i)    = .false.
          endif
        endif
      enddo
      enddo
      do i=1,im
        if(cnvflg(i)) then
          if(kbcon1(i) == kmax(i)) cnvflg(i) = .false.
        endif
      enddo
      do i=1,im
        if(cnvflg(i)) then
          tem = pfld(i,kbcon(i)) - pfld(i,kbcon1(i))
          if(tem > dthk) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  calculate convective inhibition
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < kbcon1(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma                       &
     &                 * to(i,k) / hvap
              cina(i) = cina(i) +                                     &
!    &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
     &                 dz1 * (g / (cp * to(i,k)))                     &
     &                 * dbyo(i,k) / (1. + gamma)                     &
     &                 * rfact
              val = 0.
              cina(i) = cina(i) +                                      &
!    &                 dz1 * eta(i,k) * g * delta *
     &                 dz1 * g * delta *                                &
     &                 max(val,(qeso(i,k) - qo(i,k)))
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
!
!         if(islimsk(i) == 1) then
!           w1 = w1l
!           w2 = w2l
!           w3 = w3l
!           w4 = w4l
!         else
!           w1 = w1s
!           w2 = w2s
!           w3 = w3s
!           w4 = w4s
!         endif
!         if(pdot(i) <= w4) then
!           tem = (pdot(i) - w4) / (w3 - w4)
!         elseif(pdot(i) >= -w4) then
!           tem = - (pdot(i) + w4) / (w4 - w3)
!         else
!           tem = 0.
!         endif
!
!         val1    =            -1.
!         tem = max(tem,val1)
!         val2    =             1.
!         tem = min(tem,val2)
!         tem = 1. - tem
!         tem1= .5*(cinacrmx-cinacrmn)
!         cinacr = cinacrmx - tem * tem1
!
          cinacr = cinacrmx
          if(cina(i) < cinacr) cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  determine first guess cloud top as the level of zero buoyancy
!c    limited to the level of P/Ps=0.7
!c
      do i = 1, im
        flg(i) = cnvflg(i)
        if(flg(i)) ktcon(i) = kbm(i)
      enddo
      do k = 2, km1
      do i=1,im
        if (flg(i) .and. k < kbm(i)) then
          if(k > kbcon1(i) .and. dbyo(i,k) < 0.) then
             ktcon(i) = k
             flg(i)   = .false.
          endif
        endif
      enddo
      enddo
!c
!c  specify upper limit of mass flux at cloud base
!c
      do i = 1, im
        if(cnvflg(i)) then
!         xmbmax(i) = .1
!
          k = kbcon(i)
          dp = 1000. * del(i,k)
          xmbmax(i) = dp / (g * dt2)
!
!         tem = dp / (g * dt2)
!         xmbmax(i) = min(tem, xmbmax(i))
        endif
      enddo
!c
!c  compute cloud moisture property and precipitation
!c
      do i = 1, im
        if (cnvflg(i)) then
          aa1(i) = 0.
          qcko(i,kb(i)) = qo(i,kb(i))
          qrcko(i,kb(i)) = qo(i,kb(i))
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < ktcon(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                           &
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
!cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                 &
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
!cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
!c
!             rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
!c
!c  below lfc check if there is excess moisture to release latent heat
!c
              if(k >= kbcon(i) .and. dq > 0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud > 0) then
                  dp = 1000. * del(i,k)
                  ptem = c0t(i,k) + c1
                  qlk = dq / (eta(i,k) + etah * ptem * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0t(i,k) * dz)
                endif
                buo(i,k) = buo(i,k) - g * qlk
                qcko(i,k)= qlk + qrch
                pwo(i,k) = etah * c0t(i,k) * dz * qlk
                cnvwt(i,k) = etah * qlk * g / dp
              endif
!
!  compute buoyancy and drag for updraft velocity
!
              if(k >= kbcon(i)) then
                rfact =  1. + delta * cp * gamma                     &
     &                   * to(i,k) / hvap
                buo(i,k) = buo(i,k) + (g / (cp * to(i,k)))            &
     &                   * dbyo(i,k) / (1. + gamma)                    &
     &                   * rfact
                val = 0.
                buo(i,k) = buo(i,k) + g * delta *                      &
     &                     max(val,(qeso(i,k) - qo(i,k)))
                drag(i,k) = max(xlamue(i,k),xlamud(i))
              endif
!
            endif
          endif
        enddo
      enddo
!c
!c  calculate cloud work function
!c
!     do k = 2, km1
!       do i = 1, im
!         if (cnvflg(i)) then
!           if(k >= kbcon(i) .and. k < ktcon(i)) then
!             dz1 = zo(i,k+1) - zo(i,k)
!             gamma = el2orc * qeso(i,k) / (to(i,k)**2)
!             rfact =  1. + delta * cp * gamma
!    &                 * to(i,k) / hvap
!             aa1(i) = aa1(i) +
!!   &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
!    &                 dz1 * (g / (cp * to(i,k)))
!    &                 * dbyo(i,k) / (1. + gamma)
!    &                 * rfact
!             val = 0.
!             aa1(i) = aa1(i) +
!!   &                 dz1 * eta(i,k) * g * delta *
!    &                 dz1 * g * delta *
!    &                 max(val,(qeso(i,k) - qo(i,k)))
!           endif
!         endif
!       enddo
!     enddo
!     do i = 1, im
!       if(cnvflg(i) .and. aa1(i) <= 0.) cnvflg(i) = .false.
!     enddo
!
!  calculate cloud work function
!
      do i = 1, im
        if (cnvflg(i)) then
          aa1(i) = 0.
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k >= kbcon(i) .and. k < ktcon(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              aa1(i) = aa1(i) + buo(i,k) * dz1
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i) .and. aa1(i) <= 0.) cnvflg(i) = .false.
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
!c
!c  estimate the onvective overshooting as the level
!c    where the [aafac * cloud work function] becomes zero,
!c    which is the final cloud top
!c    limited to the level of P/Ps=0.7
!c
      do i = 1, im
        if (cnvflg(i)) then
          aa1(i) = aafac * aa1(i)
        endif
      enddo
!c
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon1(i) = kbm(i)
      enddo
      do k = 2, km1
        do i = 1, im
          if (flg(i)) then
            if(k >= ktcon(i) .and. k < kbm(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma                        &
     &                 * to(i,k) / hvap
              aa1(i) = aa1(i) +                                          &
!    &                 dz1 * eta(i,k) * (g / (cp * to(i,k)))
     &                 dz1 * (g / (cp * to(i,k)))                           &
     &                 * dbyo(i,k) / (1. + gamma)                           &
     &                 * rfact
!             val = 0.
!             aa1(i) = aa1(i) +
!!   &                 dz1 * eta(i,k) * g * delta *
!    &                 dz1 * g * delta *
!    &                 max(val,(qeso(i,k) - qo(i,k)))
              if(aa1(i) < 0.) then
                ktcon1(i) = k
                flg(i) = .false.
              endif
            endif
          endif
        enddo
      enddo
!c
!c  compute cloud moisture property, detraining cloud water
!c    and precipitation in overshooting layers
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k >= ktcon(i) .and. k < ktcon1(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)                                             &
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
!cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                   &
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
!cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
!c
!c  check if there is excess moisture to release latent heat
!c
              if(dq > 0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud > 0) then
                  dp = 1000. * del(i,k)
                  ptem = c0t(i,k) + c1
                  qlk = dq / (eta(i,k) + etah * ptem * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0t(i,k) * dz)
                endif
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0t(i,k) * dz * qlk
                cnvwt(i,k) = etah * qlk * g / dp
              endif
            endif
          endif
        enddo
      enddo
!
!  compute updraft velocity square(wu2)
!
!     bb1 = 2. * (1.+bet1*cd1)
!     bb2 = 2. / (f1*(1.+gam1))
!
!     bb1 = 3.9
!     bb2 = 0.67
!
!     bb1 = 2.0
!     bb2 = 4.0
!
      bb1 = 4.0
      bb2 = 0.8
!
      do i = 1, im
        if (cnvflg(i)) then
          k = kbcon1(i)
          tem = po(i,k) / (rd * to(i,k))
          wucb = -0.01 * dot(i,k) / (tem * g)
          if(wucb > 0.) then
            wu2(i,k) = wucb * wucb
          else
            wu2(i,k) = 0.
          endif
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kbcon1(i) .and. k < ktcon(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              tem  = 0.25 * bb1 * (drag(i,k)+drag(i,k-1)) * dz
              tem1 = 0.5 * bb2 * (buo(i,k)+buo(i,k-1)) * dz
              ptem = (1. - tem) * wu2(i,k-1)
              ptem1 = 1. + tem
              wu2(i,k) = (ptem + tem1) / ptem1
              wu2(i,k) = max(wu2(i,k), 0._kind_phys)
            endif
          endif
        enddo
      enddo
!
!  compute updraft velocity averaged over the whole cumulus
!
      do i = 1, im
        wc(i) = 0.
        sumx(i) = 0.
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kbcon1(i) .and. k < ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem = 0.5 * (sqrt(wu2(i,k)) + sqrt(wu2(i,k-1)))
              wc(i) = wc(i) + tem * dz
              sumx(i) = sumx(i) + dz
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          if(sumx(i) == 0.) then
             cnvflg(i)=.false.
          else
             wc(i) = wc(i) / sumx(i)
          endif
          val = 1.e-4
          if (wc(i) < val) cnvflg(i)=.false.
        endif
      enddo
!c
!c exchange ktcon with ktcon1
!c
      do i = 1, im
        if(cnvflg(i)) then
          kk = ktcon(i)
          ktcon(i) = ktcon1(i)
          ktcon1(i) = kk
        endif
      enddo
!c
!c  this section is ready for cloud water
!c
      if(ncloud > 0) then
!c
!c  compute liquid and vapor separation at cloud top
!c
      do i = 1, im
        if(cnvflg(i)) then
          k = ktcon(i) - 1
          gamma = el2orc * qeso(i,k) / (to(i,k)**2)
          qrch = qeso(i,k)                                             &
     &         + gamma * dbyo(i,k) / (hvap * (1. + gamma))
          dq = qcko(i,k) - qrch
!c
!c  check if there is excess moisture to release latent heat
!c
          if(dq > 0.) then
            qlko_ktcon(i) = dq
            qcko(i,k) = qrch
          endif
        endif
      enddo
      endif
!c
!c--- compute precipitation efficiency in terms of windshear
!c
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 0.
        endif
      enddo
      do k = 2, km
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k <= ktcon(i)) then
              shear= sqrt((uo(i,k)-uo(i,k-1)) ** 2                     &
     &                  + (vo(i,k)-vo(i,k-1)) ** 2)
              vshear(i) = vshear(i) + shear
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i))-zi(i,kb(i)))
          e1=1.591-.639*vshear(i)                                         &
     &       +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
          edt(i)=1.-e1
          val =         .9
          edt(i) = min(edt(i),val)
          val =         .0
          edt(i) = max(edt(i),val)
        endif
      enddo
!c
!c--- what would the change be, that a cloud with unit mass
!c--- will do to the environment?
!c
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. k <= kmax(i)) then
            dellah(i,k) = 0.
            dellaq(i,k) = 0.
            dellau(i,k) = 0.
            dellav(i,k) = 0.
          endif
        enddo
      enddo
!c
!c--- changed due to subsidence and entrainment
!c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k < ktcon(i)) then
              dp = 1000. * del(i,k)
              dz = zi(i,k) - zi(i,k-1)
!c
              dv1h = heo(i,k)
              dv2h = .5 * (heo(i,k) + heo(i,k-1))
              dv3h = heo(i,k-1)
              dv1q = qo(i,k)
              dv2q = .5 * (qo(i,k) + qo(i,k-1))
              dv3q = qo(i,k-1)
!c
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1))
              tem1 = xlamud(i)
!cj
              dellah(i,k) = dellah(i,k) +                                  &
     &     ( eta(i,k)*dv1h - eta(i,k-1)*dv3h                                &
     &    -  tem*eta(i,k-1)*dv2h*dz                                          &
     &    +  tem1*eta(i,k-1)*.5*(hcko(i,k)+hcko(i,k-1))*dz                  &      
     &         ) *g/dp
!cj
              dellaq(i,k) = dellaq(i,k) +                                 &
     &     ( eta(i,k)*dv1q - eta(i,k-1)*dv3q                              &
     &    -  tem*eta(i,k-1)*dv2q*dz                                       &
     &    +  tem1*eta(i,k-1)*.5*(qrcko(i,k)+qcko(i,k-1))*dz               &
     &         ) *g/dp
!cj
              tem1=eta(i,k)*(uo(i,k)-ucko(i,k))
              tem2=eta(i,k-1)*(uo(i,k-1)-ucko(i,k-1))
              dellau(i,k) = dellau(i,k) + (tem1-tem2) * g/dp
!cj
              tem1=eta(i,k)*(vo(i,k)-vcko(i,k))
              tem2=eta(i,k-1)*(vo(i,k-1)-vcko(i,k-1))
              dellav(i,k) = dellav(i,k) + (tem1-tem2) * g/dp
!cj
            endif
          endif
        enddo
      enddo
!c
!c------- cloud top
!c
      do i = 1, im
        if(cnvflg(i)) then
          indx = ktcon(i)
          dp = 1000. * del(i,indx)
          dv1h = heo(i,indx-1)
          dellah(i,indx) = eta(i,indx-1) *                            &
     &                     (hcko(i,indx-1) - dv1h) * g / dp
          dv1q = qo(i,indx-1)
          dellaq(i,indx) = eta(i,indx-1) *                             &
     &                     (qcko(i,indx-1) - dv1q) * g / dp
          dellau(i,indx) = eta(i,indx-1) *                               &
     &             (ucko(i,indx-1) - uo(i,indx-1)) * g / dp
          dellav(i,indx) = eta(i,indx-1) *                             &
     &             (vcko(i,indx-1) - vo(i,indx-1)) * g / dp
!c
!c  cloud water
!c
          dellal(i,indx) = eta(i,indx-1) *                             &
     &                     qlko_ktcon(i) * g / dp
        endif
      enddo
!
!
!  compute convective turn-over time
!
      do i= 1, im
        if(cnvflg(i)) then
          tem = zi(i,ktcon1(i)) - zi(i,kbcon1(i))
          dtconv(i) = tem / wc(i)
          dtconv(i) = max(dtconv(i),dtmin)
          dtconv(i) = max(dtconv(i),dt2)
          dtconv(i) = min(dtconv(i),dtmax)
        endif
      enddo
!
!  compute advective time scale using a mean cloud layer wind speed
!
      do i= 1, im
        if(cnvflg(i)) then
          sumx(i) = 0.
          umean(i) = 0.
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if(cnvflg(i)) then
            if(k >= kbcon1(i) .and. k < ktcon1(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem = sqrt(u1(i,k)*u1(i,k)+v1(i,k)*v1(i,k))
              umean(i) = umean(i) + tem * dz
              sumx(i) = sumx(i) + dz
            endif
          endif
        enddo
      enddo
      do i= 1, im
        if(cnvflg(i)) then
           umean(i) = umean(i) / sumx(i)
           umean(i) = max(umean(i), 1._kind_phys)
           tauadv(i) = gdx(i) / umean(i)
        endif
      enddo
!c
!c  compute cloud base mass flux as a function of the mean
!c     updraft velcoity
!c
      do i= 1, im
        if(cnvflg(i)) then
          k = kbcon(i)
          rho = po(i,k)*100. / (rd*to(i,k))
          tfac = tauadv(i) / dtconv(i)
          tfac = min(tfac, 1._kind_phys)
          xmb(i) = tfac*betaw*rho*wc(i)
        endif
      enddo
!
!--- modified Grell & Freitas' (2014) updraft fraction which uses
!     actual entrainment rate at cloud base
!
      do i = 1, im
        if(cnvflg(i)) then
          tem = min(max(xlamue(i,kbcon(i)), 2.e-4_kind_phys), 6.e-4_kind_phys)
          tem = 0.2 / tem
          tem1 = 3.14 * tem * tem
          sigmagfm(i) = tem1 / garea(i)
          sigmagfm(i) = max(sigmagfm(i), 0.001_kind_phys)
          sigmagfm(i) = min(sigmagfm(i), 0.999_kind_phys)
        endif
      enddo
!
!--- compute scale-aware function based on Arakawa & Wu (2013)
!
!
      do i = 1, im
        if(cnvflg(i)) then
          if (gdx(i) < dxcrt) then
            scaldfunc(i) = (1.-sigmagfm(i)) * (1.-sigmagfm(i))
            scaldfunc(i) = max(min(scaldfunc(i), 1.0), 0._kind_phys)
          else
            scaldfunc(i) = 1.0
          endif
          xmb(i) = xmb(i) * scaldfunc(i)
          xmb(i) = min(xmb(i),xmbmax(i))
        endif
      enddo
!c
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k <= kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val     =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
          endif
        enddo
      enddo
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c
      do i = 1, im
        delhbar(i) = 0.
        delqbar(i) = 0.
        deltbar(i) = 0.
        delubar(i) = 0.
        delvbar(i) = 0.
        qcond(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k <= ktcon(i)) then
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
              q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
!             tem = 1./rcs(i)
!             u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
!             v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem
              u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2
              v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2
              dp = 1000. * del(i,k)
              delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g
              delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g
              deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g
              delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/g
              delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/g
            endif
          endif
        enddo
      enddo
!
      do k = 1, km
        do i = 1, im
          if (cnvflg(i)) then
            if(k > kb(i) .and. k <= ktcon(i)) then
              qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
              qeso(i,k) = eps * qeso(i,k)/(pfld(i,k) + epsm1*qeso(i,k))
              val     =             1.e-8
              qeso(i,k) = max(qeso(i,k), val )
            endif
          endif
        enddo
      enddo
!c
      do i = 1, im
        rntot(i) = 0.
        delqev(i) = 0.
        delq2(i) = 0.
        flg(i) = cnvflg(i)
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (cnvflg(i)) then
            if(k < ktcon(i) .and. k > kb(i)) then
              rntot(i) = rntot(i) + pwo(i,k) * xmb(i) * .001 * dt2
            endif
          endif
        enddo
      enddo
!c
!c evaporating rain
!c
      do k = km, 1, -1
        do i = 1, im
          if (k <= kmax(i)) then
            deltv(i) = 0.
            delq(i) = 0.
            qevap(i) = 0.
            if(cnvflg(i)) then
              if(k < ktcon(i) .and. k > kb(i)) then
                rn(i) = rn(i) + pwo(i,k) * xmb(i) * .001 * dt2
              endif
            endif
            if(flg(i) .and. k < ktcon(i)) then
              evef = edt(i) * evfact
              if(islimsk(i) == 1) evef=edt(i) * evfactl
!             if(islimsk(i) == 1) evef=.07
!c             if(islimsk(i) == 1) evef = 0.
              qcond(i) = evef * (q1(i,k) - qeso(i,k))                       &
     &                 / (1. + el2orc * qeso(i,k) / t1(i,k)**2)
              dp = 1000. * del(i,k)
              if(rn(i) > 0. .and. qcond(i) < 0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dt2*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i) > 0. .and. qcond(i) < 0. .and.                       &
     &           delq2(i) > rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i) > 0. .and. qevap(i) > 0.) then
                tem  = .001 * dp / g
                tem1 = qevap(i) * tem
                if(tem1 > rn(i)) then
                  qevap(i) = rn(i) / tem
                  rn(i) = 0.
                else
                  rn(i) = rn(i) - tem1
                endif
                q1(i,k) = q1(i,k) + qevap(i)
                t1(i,k) = t1(i,k) - elocp * qevap(i)
                deltv(i) = - elocp*qevap(i)/dt2
                delq(i) =  + qevap(i)/dt2
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
              delqbar(i) = delqbar(i) + delq(i)*dp/g
              deltbar(i) = deltbar(i) + deltv(i)*dp/g
            endif
          endif
        enddo
      enddo
!cj
!     do i = 1, im
!     if(me == 31 .and. cnvflg(i)) then
!     if(cnvflg(i)) then
!       print *, ' shallow delhbar, delqbar, deltbar = ',
!    &             delhbar(i),hvap*delqbar(i),cp*deltbar(i)
!       print *, ' shallow delubar, delvbar = ',delubar(i),delvbar(i)
!       print *, ' precip =', hvap*rn(i)*1000./dt2
!       print*,'pdif= ',pfld(i,kbcon(i))-pfld(i,ktcon(i))
!     endif
!     enddo
!cj
      do i = 1, im
        if(cnvflg(i)) then
          if(rn(i) < 0. .or. .not.flg(i)) rn(i) = 0.
          ktop(i) = ktcon(i)
          kbot(i) = kbcon(i)
          kcnv(i) = 2
        endif
      enddo
!c
!c  convective cloud water
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i)) then
            if (k >= kbcon(i) .and. k < ktcon(i)) then
              cnvw(i,k) = cnvwt(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo

!c
!c  convective cloud cover
!c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i)) then
            if (k >= kbcon(i) .and. k < ktcon(i)) then
              cnvc(i,k) = 0.04 * log(1. + 675. * eta(i,k) * xmb(i))
              cnvc(i,k) = min(cnvc(i,k), 0.2_kind_phys)
              cnvc(i,k) = max(cnvc(i,k), 0.0_kind_phys)
            endif
          endif
        enddo
      enddo

!c
!c  cloud water
!c
      if (ncloud > 0) then
!
      do k = 1, km1
        do i = 1, im
          if (cnvflg(i)) then
!           if (k > kb(i) .and. k <= ktcon(i)) then
            if (k >= kbcon(i) .and. k <= ktcon(i)) then
              tem  = dellal(i,k) * xmb(i) * dt2
              tem1 = max(0.0_kind_phys, min(1.0_kind_phys, (tcr-t1(i,k))*tcrf))
              if (ql(i,k,2) > -999.0) then
                ql(i,k,1) = ql(i,k,1) + tem * tem1            ! ice
                ql(i,k,2) = ql(i,k,2) + tem *(1.0-tem1)       ! water
              else
                ql(i,k,1) = ql(i,k,1) + tem
              endif
            endif
          endif
        enddo
      enddo
!
      endif
!
! hchuang code change
!
      do k = 1, km
        do i = 1, im
          if(cnvflg(i)) then
            if(k >= kb(i) .and. k < ktop(i)) then
              ud_mf(i,k) = eta(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
           k = ktop(i)-1
           dt_mf(i,k) = ud_mf(i,k)
        endif
      enddo
!!
      return
      end subroutine mfshalcnv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END MODULE module_cu_scalesas

