!wrft:model_layer:physics
!
MODULE module_mp_thompson
   USE     module_wrf_error
!  USE module_state_description
!
!
   REAL      ::                                                           &
              acrcg,acrcr,acrcs,acrig,acris,acrls,agraupel,alpha1,ap,      &
              arain,asnow,ato,bacls,bacrcg,bacrcr,bacrcs,bacrig,bacris,    &
              berc1,beta1,bg,bp,br,bs,c1,cir,cirf,cnp,const1a,const1b,     &
              crg,crs,csg,csr,cw,depg1,depg2,depg3,depg4,depr1,depr2,      &
              depr3,depr4,deps1,deps2,deps3,deps4,dgraupel,dice,drain,     &
              dsnow,efcr,efcs,efgc,efgi,efgr,efgs,efir,efis,efsr,          &
              fgraupel,fra1,frain,frd1,fsnow,gamma1,gon,hgfr,pgm1,pgm2,    &      
              pgm3,pgm4,pi,psm1,psm2,psm3,psm4,rmc,ron,ron2,son,tno,topg,  &  
              topr,tops,xm01,xm0g,xm0s,xr0s,xsmax,cniag,alpha2,beta2,      &
              cnsacr,ess,fnrain,fnsnow,R,RV,CP_MP,TO,XLV0,XLV1,XLF0,   &
              XLS_MP,G_MP,FNGRAUPEL, &
              eps_ax,nu_c,x_minc,x_maxc,x_maxr,k_c,k_1,k_2,k_au,k_sc,k_r, &
              alf,bet,gam,n_0,eps_axv,k_rsc,RNGFR
      INTEGER  ,     PARAMETER :: NSTB=40
      INTEGER  ,     PARAMETER :: NRTB=40
      INTEGER  ,     PARAMETER :: NR=100,NRP=NR+1,NS=100,NSP=NS+1
  REAL, dimension(nstb) :: t_slos
  REAL, dimension(nrtb) :: t_slor
  REAL, dimension(ns) :: sad,ds
  REAL, dimension(nsp) :: sads
  REAL, dimension(nr) :: rad,dr
  REAL, dimension(nrp) :: rads
  REAL, DIMENSION(NRTB,NSTB) :: t_pracs,t_psacr
  INTEGER    ::                                                           &
              marat,int0,new_snow_dist,sonv_new,ronv_new,gonv_new
  INTEGER    :: IN,IS
!
  LOGICAL :: IIWARM                                                          
!
! LOOKUP TABLE FOR A1 AND A2 IN BERGERGON PROCESS
!
  REAL, dimension(31) :: aber1 = (/7939E-07,.7841E-06,.3369E-05,.4336E-05, &
                        .5285E-05,.3728E-05,.1852E-05,.2991E-06,.4248E-06, &
                        .7434E-06,.1812E-05,.4394E-05,.9145E-05,.1725E-06, &
                        .3348E-04,.1725E-04,.9175E-05,.4412E-05,.2252E-05, &
                        .9115E-06,.4876E-06,.3473E-06,.4758E-06,.6306E-06, &
                        .8573E-06,.7868E-06,.7192E-06,.6513E-06,.5956E-06, &
                        .5333E-06,.4834E-06/)
  REAL, dimension(31) :: aber2 = (/.4006,.4831,.5320,.5307,.5319,.5249, &
                        .4888,.3894,.4047,.4318,.4771,.5183,.5463,.5651, &
                        .5813,.5655,.5478,.5203,.4906,.4447,.4126,.3960, &
                        .4149,.4320,.4506,.4483,.4460,.4433,.4413,.4382, &
                        .4361/)
!
!
CONTAINS

!-------------------------------------------------------------------
!  Lin et al., 1983, JAM, 1065-1092, and
!  Rutledge and Hobbs, 1984, JAS, 2949-2972
!  Modifications to reisner 2 model
!  Khondrativ and Kogan warm-rain parameterization
!-------------------------------------------------------------------
!
  SUBROUTINE mp_thomp(itimestep,th, qv, ql, qr, qi, qs, qg,        &
                       qni,              &
                       rho, pii, p,                                &
                       dt_in, z, ht, dz8w, w,                      &
                       RAINNC, RAINNCV ,                           &
                       ids,ide, jds,jde, kds,kde,                  & ! domain dims
                       ims,ime, jms,jme, kms,kme,                  & ! memory dims
                       its,ite, jts,jte, kts,kte                   & ! tile   dims
                      , F_QI,F_QS,F_QG,F_QNI                       &
                                                                   )

!
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
! Shuhua 12/17/99
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte 

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                              ql, &
                                                              qr, &
                                                              qi, &
                                                              qs, &
                                                              qg, &
                                                             qni
!
!
!
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                     rho, &
                                                             pii, &
                                              		       p, &
                                              		    dz8w, &
                                                               w
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) :: 				       z

  REAL, DIMENSION( ims:ime , jms:jme ),                	          &
        INTENT(INOUT) ::                                  RAINNC, &
                                                         RAINNCV


  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht

  REAL, INTENT(IN   ) :: 				   dt_in
  INTEGER, INTENT(IN   ) ::                              itimestep
  LOGICAL, INTENT(IN), OPTIONAL ::                                &
				      F_QI,F_QS,F_QG,             &
                                      F_QNI

! LOCAL VAR

  INTEGER             ::  			    min_q, max_q,ipstep

  REAL                ::                   ql_max,qr_max,qs_max,qi_max,qg_max, &
                                           qni_max
  REAL, DIMENSION( its:ite , jts:jte )                            &
                               ::        rain, snow, graupel,ice

  REAL, DIMENSION( kts:kte )   ::                  qvz, qlz, qrz, &
                                                   qiz, qsz, qgz, &
                                                             thz, &
                                                              tz, &
                                                            piiz, &
                                                            rhoz, &
                                                        prez, zz, &
                                                       qniz, &
                                                    dzw, wz
!
  REAL    ::         dt, pptrain, pptsnow, pptgraul, rhoe_s,      &
                     gindex, pptice
!
  INTEGER ::               i,j,k,iterm
!
  LOGICAL flag_qi, flag_qs, flag_qg, flag_qni

   flag_qi = .false.
   flag_qs = .false.
   flag_qg = .false.
   flag_qni = .false.
   IF ( PRESENT ( f_qi ) ) flag_qi = f_qi
   IF ( PRESENT ( f_qs ) ) flag_qs = f_qs
   IF ( PRESENT ( f_qg ) ) flag_qg = f_qg
   IF ( PRESENT ( f_qni ) ) flag_qni = f_qni
!

   dt=dt_in
   ipstep=itimestep
   rhoe_s=1.29
!
   IF (  .NOT. flag_qi .OR. .NOT. flag_qs ) THEN
      CALL wrf_error_fatal ( 'module_mp_thompson: Improper use of thompson scheme; no ice phase. Please chose another one.')
   ENDIF

   gindex=1.0

   IF ( .NOT. flag_qg ) gindex=0.

     ql_max=0.
     qr_max=0.
     qs_max=0.
     qi_max=0.
     qg_max=0
     qni_max=0.
 
   j_loop:  DO j = jts, jte
   i_loop:  DO i = its, ite
!
!
!- write data from 3-D to 1-D
!
   DO k = kts, kte
      qvz(k)=qv(i,k,j)
      qlz(k)=ql(i,k,j)
      qrz(k)=qr(i,k,j)
      thz(k)=th(i,k,j)
      tz(k)=th(i,k,j)*pii(i,k,j)
      piiz(k)=pii(i,k,j)
!
      rhoz(k)=rho(i,k,j)
      prez(k)=p(i,k,j)
      zz(k)=z(i,k,j)
      dzw(k)=dz8w(i,k,j)
      wz(k)=w(i,k,j)
   END DO
!
!
   IF (flag_qi) THEN
      DO k = kts, kte
         qiz(k)=qi(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qiz(k)=0.
      ENDDO
   ENDIF

   IF (flag_qs) THEN
      DO k = kts, kte
         qsz(k)=qs(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qsz(k)=0.
      ENDDO
   ENDIF

   IF (flag_qg) THEN
      DO k = kts, kte
         qgz(k)=qg(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qgz(k)=0.
      ENDDO
   ENDIF
!
   IF (flag_qni) THEN
      DO k = kts, kte
         qniz(k)=qni(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qniz(k)=0.
      ENDDO
   ENDIF
!
   pptrain=0.
   pptsnow=0.
   pptgraul=0.
   pptice=0.
   CALL exmoisg(    dt, qvz, qlz, qrz, qiz, qsz, qgz,         &
                    qniz,     &
                    thz, tz, piiz, rhoz, wz,                  &
                    prez, zz, dzw, ht(I,J), &
                    pptrain, pptsnow, pptgraul, pptice,       &
 		    kts, kte, i, j, ipstep                    )

!
! Precipitation from cloud microphysics -- only for one time step
!
! unit is transferred from m to mm

!
   rain(i,j)=pptrain
   snow(i,j)=pptsnow
   graupel(i,j)=pptgraul
   ice(i,j)=pptice
!
   RAINNCV(i,j)= pptrain + pptsnow + pptgraul + pptice
   RAINNC(i,j)=RAINNC(i,j) + pptrain + pptsnow + pptgraul + pptice
!
!   if(j.eq.1)print*,'pptrain , pptsnow , pptgraul , pptice= ',pptrain , pptsnow , pptgraul , pptice
!
!- update data from 1-D back to 3-D
!
!
   DO k = kts, kte
      qv(i,k,j)=qvz(k)
      ql(i,k,j)=qlz(k)
      qr(i,k,j)=qrz(k)
      th(i,k,j)=thz(k)
     ql_max=amax1(ql_max,qlz(k))
     qr_max=amax1(qr_max,qrz(k))
     qs_max=amax1(qs_max,qsz(k))
     qi_max=amax1(qi_max,qiz(k))
     qg_max=amax1(qg_max,qgz(k))
     qni_max=amax1(qni_max,qniz(k))
   END DO
!
   IF (flag_qi) THEN
      DO k = kts, kte
         qi(i,k,j)=qiz(k)
      ENDDO
   ENDIF

   IF (flag_qs) THEN
      DO k = kts, kte
         qs(i,k,j)=qsz(k)
      ENDDO
   ENDIF

   IF (flag_qg) THEN
      DO k = kts, kte
         qg(i,k,j)=qgz(k)
      ENDDO
   ENDIF
!
   IF (flag_qni) THEN
      DO k = kts, kte
         qni(i,k,j)=qniz(k)
      ENDDO
   ENDIF
!
!
   ENDDO i_loop
   ENDDO j_loop
!   print*,'itimestep, ql, qr, qi, qs, qg,qni= ',itimestep, &
!           ql_max,qr_max,qi_max,qs_max,qg_max, &
!           qni_max
   END SUBROUTINE mp_thomp


!-----------------------------------------------------------------------
   SUBROUTINE exmoisg(    dt, qvz, qlz, qrz, qiz, qsz, qgz,         &
                    qniz,         &
                    thz, tz, piiz, rhoz, wz,                  &
                    prez, zz, dzw, ht_sfc,  &
                     pptrain, pptsnow, pptgraul, pptice,       &
                    kts, kte, i, j ,ipstep                    )
!-----------------------------------------------------------------------
    IMPLICIT NONE
!-----------------------------------------------------------------------
!----------------------------------------------------------------------

  INTEGER, INTENT(IN   )               :: kts, kte, i, j, ipstep

  REAL,    DIMENSION( kts:kte ),                                      &
           INTENT(INOUT)               :: qvz, qlz, qrz, qiz, qsz,    &
                                          qgz, tz,        &
                                           qniz,    &
                                          thz 
!
  REAL, DIMENSION( kts:kte ),INTENT(IN)  ::                       &
                                                      piiz, rhoz, &
                                                        prez, zz, &
                                                        dzw, wz
! diffusion velocities
!
  REAL, DIMENSION( kts:kte )  ::     FRDIF, FNRDIF,FSDIF,FGDIF,FNGDIF, &
                                   FIDIF,FNIDIF,FNSDIF

  REAL,    INTENT(INOUT)               :: pptrain, pptsnow, pptgraul, pptice
           
  REAL,    INTENT(IN)                  :: ht_sfc ,dt
  INTEGER                              :: k
!
  REAL    ::    q_c,n_c,q_r,x_c,x_r,au,tau,phi,sc,l_c,l_r,n_r,g1,g4,n0,lam

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!                                                                      !       
!     THE SCHEME DIFFERS FROM IMPHYS = 7 (REISNER2)  RELEASE V3.5      !    
!     THIS SUBROUTINE HAS INCLUDED THE MARAT KHAIROUTINOV AND JEFFIM   !
!     KOGAN (2000)  warm rain parameterization AND THE ANALYTICAL      !
!     TWOMEY TREATMENTOF CLOUD CONDENSATION  NUCLEATION by COHARD and  !
!     PINTY.                                                           !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!
  REAL, DIMENSION( kts:kte )     :: D1FIELD,ALFIELD
!
!
!

!
  REAL,  DIMENSION( kts:kte )   ::   tzten, qvzten, qlzten, qrzten, &
                                     qizten, qszten, qgzten, qnizten
!                                                                               
! ARRAYS FOR QV                                                                 
                                                                                
  REAL,  DIMENSION( kts:kte )   ::   QAOUT, QVQVS, QVQVSI, QVS, QVSI,  &
                                     AB, ABI, PREG, ABW
                                                                                 
! ARRAYS FOR QC                                                                 
                                                                                
  REAL,  DIMENSION( kts:kte )   ::   PRC, PIFCW, PSACW, PGACW, PRA, SCR3, &
                                     SCR6, SCR7
!
  REAL    ::  MI_MN,MI_MX
!
  REAL    ::   MCD_MN,MCD_MX,MRD_MN,MRD_MX
  REAL    ::  QNL_MN,QNL_MX,QNR_MN,QNR_MX
!
!    MARAT TENDENCY TERMS                                                        
!
  REAL,  DIMENSION( kts:kte )   ::   PNIFCW, PNGFR, PINACW, PINACR, PSNACW, &
                                     PSNACR, PGNACW, PGNACR, PNRC, PNNUC,   &
                                     PMNUC, PNAC, PNCC
!
  REAL,  DIMENSION( kts:kte )   :: RVR
!
  REAL    ::   PSI1,PSI2,PSI3,PSI4,RVC3,RVR3,RNC,RNR,SMAX,ETAMX,SQZ,   &                  
               QVSAT0,T1,T2,SUPSTR
!
  INTEGER   ::   INUCFLG,IN,IST,IEN,IT, HALLET_MOSSOP, NSTEP,N,IFDRY,BY_PASS, &
                 ICE_NUC
!
!  REAL    ::   RSLF, RSIF, BETAF
!
  REAL    ::    ron_min, ron_qr0, ron_delqr0, ron_const1r, ron_const2r, &
       RHO_NOT,EPSN,EPSM,R1,pd,TEMP_C,DIACE_MIN,XMI, &
       VT2N,VT2R,VT2S,VT2G,VT2NG,DIACE,VT2I,A1,A2,XNC,SUPICE,SLOSN, &
       DCLO,DSNO,STOKE_S,EFF_CS,SLOGN,DGRO,STOKE_G,EFF_CG, &
       TF,FUDGEF,SUM_DEP,UDCLO,RPHI,EIC1,PIACW1, &
       PRDTEMP,CLIC,DMGI,DEPAC,PSAGI,C1_AG,DTAU,XLATF,ALPSNOW, &
       ALPRAIN,ALPHARS,D1,THETAH,SUM_TERMS,RATIO,PISPL_FS, &
       PISPL_FG,TEMP_QI,TEMP_QNI,RLU,TEMP,R2,R3,UPDT_S, &
       UPDT_G,UPDT_NG,UPDT_R,UPDT_NR,UPDT_I,UPDT_N,SLOR1,SLOS1, &
       SLOG1,DIAMI,RGVM,FALTNDR,FALTNDNR,FALTNDS, &
       FALTNDG,FALTNDI,FALTNDN,FALTNDNS,UPDT_NS,FALTNDNG
!
   REAL    :: TEST_S,TEST_G,TEST_NG,TEST_R,TEST_NR,TEST_I,TEST_N,TEST_NS
!
! ARRAYS FOR QR                                                                
                                                                                 
  REAL,  DIMENSION( kts:kte )   :: SLOR, PIACR, PGACR, PRE, SCR4R, FR,  &
                                   FALOUTR, RONV, SCR4NR, FALOUTNR, FNR

                                                                            
! ARRAYS FOR QI                                                                
                                                                              
                                                                               
  REAL,  DIMENSION( kts:kte )   :: PRI, PSCNI, PRAI, PRACI, PISPL, PRD,  &
                                   PIACW, SCR4I, FI, FALOUTI, PRDX, PRCX, &
                                    DKICE,DFICE
! ARRAYS FOR QS                                                                
                                                                              
!
  REAL,  DIMENSION( kts:kte )   ::  SLOS, PSACR, PGSACW, PREI, PSMLT, PMLTEV, &
                                    SCR4S, FS, FALOUTS, PRACS, PSSACW, SONV, NCONR
!
! ARRAYS FOR NS
!
  REAL,  DIMENSION( kts:kte )   ::  FNS ,FALOUTNS, SCR4NS, NSAG, NSACR, &
                                    SLOS2,  SONV2
!
  REAL     ::  VT2NS
!
! ARRAYS FOR NG

  REAL,  DIMENSION( kts:kte )   ::   GONA, FNG, FALOUTNG, SCR4NG, &
                                     NIACR
!
! ARRAYS FOR QG                                                                  
                                                                                
  REAL,  DIMENSION( kts:kte )   ::  PGMLT, PMLTGE, PGACRM, PGACWM, SCR4G, FG, &
                                     FALOUTG, SLOG, SLOG2, PGFR, PICNG, PGIACW, PGEMB, GONV
! ARRAYS FOR NCI                                                             
                                                                              
  REAL,  DIMENSION( kts:kte )   ::  NI_RS, NI_AG, NI_CG, NI_DE, NI_EV, SCR4N, FALOUTN                                                                                
! EXTRA ARRAYS                                                                  
                                                                            
  REAL,  DIMENSION( kts:kte )   ::  DUM11, DUM21, DUM31, SCR4, SCR8, RHOFAC, rkz,  &
                                    CC1D, CC2D 

  REAL    :: DRNO,STOKE_R,EFF_CR,QCK1,QCTH,BandR_L2,BandR_T2,xnu,BandR_T3
  REAL    :: ri_tbl,epi,rj_tbl,epj,lmdr,lmds
  INTEGER :: I_R,J_S
!
!+---+-----------------------------------------------------------------+      
!+---+-----------------------------------------------------------------+       
      IFDRY=0
      RHO_NOT = 101325.0/(R*298.0)                                           
!.. New RONV formulation                                                      
!    Note: RON2 is set in PARAMR                                            
! 
!  NEW_SNOW_DIST , SONV_NEW, and RONV_NEW are set in paramr.F
!
! new ronv coefficients
               ron_min = 2.e7
               ron_qr0 = 0.00020
               ron_delqr0 = 0.5*ron_qr0
               ron_const1r = (ron2-ron_min)*0.5
               ron_const2r = (ron2+ron_min)*0.5
!
      EPSN=1.E-8
      EPSM=1.E-12
!
      MI_MN=1.E-12
      MI_MX=2.6E-10
!
! min cloud drop 1 micron radius
! min rain drop 25 micron radius
! max rain drop 2500 micron radius
      MCD_MN=DRAIN*4./3.*PI*(1.e-6)**3
      MCD_MX=DRAIN*6.544984695E-14
      MRD_MN=.1*DRAIN*6.544984695E-14
      MRD_MX=DRAIN*6.544984695E-8
!

!
      DO K=kts,kte
!
 tzten(k)=0.
 qvzten(k)=0.
 qlzten(k)=0.
 qrzten(k)=0.
 qizten(k)=0.
 qszten(k)=0.
 qgzten(k)=0.
 qnizten(k)=0.
!
! maritime 
!       CC2D(K)=.5
!       CC1D(K)=50.*1.E6*(100.)**CC2D(K)
!
! very clean maritime
      CC2D(K)=.5
      CC1D(K)=25.*1.E6*(100.)**CC2D(K)
!
! contential
!       CC2D(K)=.8
!       CC1D(K)=1000.*1.E6*(100.)**CC2D(K)
     enddo    
!
      R1=1.E-15                                                             
!
      DO K=kts,kte                                                         
          qvz(k)=AMAX1(R1,qvz(k))                                      
          pd = .62198*PREZ(k)/(qvz(k) + .62198)                           
!
           qlz(k)=AMAX1(qlz(k),.0)
           qrz(k)=AMAX1(qrz(k),.0)
!
! SATURATION MIXING RATIOS OVER WATER AND ICE (FLATAU&WALKO)                    
          qvs(k) = RSLF(PREZ(k),tz(K))                               
          qvsi(k) = RSIF(PREZ(k),tz(K))                               
          qvqvs(k) = qvz(k)/qvs(k)                                     
          qvqvsi(k) = qvz(k)/qvsi(k)                                     
! DIFFUSIVITY OF WATER VAPOR  (PRUPPACHER & KLETT: 13-3)                        
          dum11(k)=2.11E-5*(tz(K)/TO)**1.94*(101325./PREZ(k))         
! DYNAMIC VISCOSITY OF AIR  (PRUPPACHER & KLETT: 10-141)                         
          TEMP_C = tz(K)-TO                                                
          IF (TEMP_C .GE. 0.0) THEN                                            
            dum21(k)=(1.718+0.0049*TEMP_C)*1.0E-5                          
          ELSE                                                          
            dum21(k)=(1.718+0.0049*TEMP_C-1.2E-5*TEMP_C*TEMP_C)*1.0E-5       
          ENDIF                                                              
! V8 LV FORMULA                                                                 
          dum31(k)=3.1484E6-XLV1*tz(K)                                   
! THERMAL CONDUCTIVITY OF AIR  (PRUPPACHER & KLETT: 13-18A)                   
          scr4(k)=(5.69 + 0.0168*TEMP_C) * 1.0E-5 *418.936                    
!
! A + B IN FORUMLA (B7) OF DUDHIA (1992) FOR T > TO                            
          ab(k)=rhoz(k)*XLS_MP*XLS_MP/(scr4(k)*RV*tz(K)     &                
              *tz(K))+1./(qvs(k)*dum11(k))                            
! A + B FOR T < TO   (FOR EQN. A.36)                                            
          abi(k)=rhoz(k)*XLS_MP*XLS_MP/(scr4(k)*RV*tz(K)*tz(K)) &
             +1./(qvsi(k)*dum11(k))                            
! A + B FOR RAIN EVAP AND T < TO                                               
          abw(k)=rhoz(k)*dum31(k)*dum31(k)/(scr4(k)*RV  &
           *tz(K)*tz(K))+1./(qvs(k)*dum11(k))               
! LATENT HEAT EFFECTS (REDUCES DEPOS GROWTH OF SNOW/GRAUPEL)                   
!...(COTTON&ANTHES 1989 4-37)                                                  
          dum11(k) = rhoz(k)*XLS_MP*3.34E5/(scr4(k)*RV &
               *tz(K)*tz(K)*abi(k))                                  
          dum11(k) = AMIN1(1.0, AMAX1(dum11(k),0.0))                         
                                                                                 
     enddo
!
!
!---BEGIN PRODUCTION TERMS CALCULATION:                                          
!    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES              
      DO K=kts,kte
           qlz(k)=AMAX1(0.,qlz(k))                                   
           qiz(k)=AMAX1(0.,qiz(k))                                   
           qrz(k)=AMAX1(0.,qrz(k))                                
           qsz(k)=AMAX1(0.,qsz(k))                                   
           qniz(k)=AMAX1(R1,qniz(k))
           qgz(k)=AMAX1(0.,qgz(k))                         
!
!
!
!.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979 GREG T.
           temp_C = amin1(-0.001, tz(K)-273.15)
           sonv(k) = amin1(2.0E8, 2.0E6*exp(-0.12*temp_C))    !greg test_2
!TEST-GT   sonv(k) = amin1(1.0E9, 1.0E6*exp(-0.18*temp_C))    !greg test_1
!
           sonv(k) = sonv(k)/SON                                             
                                                                                
           gonv(k) = GON                                                     
           IF (qgz(k).GT. R1) THEN
              gonv(k) = 2.38*(PI*DGRAUPEL/      &
                   (rhoz(k)*qgz(k)))**0.92
              gonv(k) = MAX(1.E4, MIN(gonv(k),GON))
           ENDIF
!
           gonv(k) = gonv(k)/GON                                           
                                                                              
           ronv(k) = RON2                                                  
!
!.. New RONV formulation                                                  
!
           IF (qrz(k).GT. R1 )THEN
!                 ronv(k) = (4.95E9)*tanh(-(qrz(k)-0.000090) 
!     +                      *3.0E5) + 5.05E9
               ronv(k) = ron_const1r*tanh((ron_qr0-qrz(k)) &
                      /ron_delqr0) + ron_const2r
           ENDIF                                                        
!
!
          ronv(k) = ronv(k)/RON                                   
                                                                   
!
        slos(k)=SQRT(SQRT((rhoz(k)*qsz(k)/(TOPS*sonv(k)))))
!
       slor(k)=SQRT(SQRT((rhoz(k)*qrz(k)/(TOPR*ronv(k)))))
       slog(k)=SQRT(SQRT((rhoz(k)*qgz(k)/(TOPG*gonv(k)))))
!
                                                                                 
      enddo
!
                                                                                
! PROCESSES TO BE DOMAIN AVERAGED AND WRITTEN OUT AT EVERY TIME STEP           
      DO K=kts,kte
! MASS SOURCE FOR PRIMARY ICE NUCLEATION
            PRI(k)=0.                                                     
! FREEZING OF CLOUD DROPLETS
            PIFCW(k)=0.                                                       
! FREEZING OF RAIN
            PGFR(k)=0.                                                      
! REDUCTION OF ICE MASS CONVERTED TO SNOW
            PSCNI(k)=0.                                                        
!...ACCRETION OF CLOUD ICE BY SNOW
            PRAI(k)=0.                                                        
!..COLLECTION OF CLOUD ICE BY RAIN
            PRACI(k)=0.                                                      
! FREEZING OF RAIN BY COLLISION WITH ICE
            PIACR(k)=0.                                                     
! COLLECTION OF RAIN BY SNOW
            PSACR(k)=0.                                                        
! COLLECTION OF SNOW BY RAIN
            PRACS(k)=0.                                                       
! COLLECTION OF CLOUD WATER BY SNOW
            PSACW(k)=0.                                                      
!   MURAKAMI SNOW TO GRAUPEL
            PGSACW(k)=0.                                                   
! COLLECTION OF CLOUD WATER BY GRAUPEL
            PGACW(k)=0.                                                   
! COLLECTION OF RAIN BY GRAUPEL
            PGACR(k)=0.                                                       
! SUBLIMATION/DEPOSITIONAL GROWTH OF GRAUPEL
            PREG(k)=0.                                                       
! DEPOSITION OF CLOUD ICE
            PRD(k)=0.                                                      
!...DEPOSITION/SUBLIMATION OF SNOW
            PREI(k)=0.                                                   
!   AUTOCONVERSTIONN OF CLOUD TO RAIN
            PRC(k)=0.                                                          
! COLLECTION OF CLOUD BY RAIN
            PRA(k)=0.                                                         
!...EVAPORATION OF RAINWATER
            PRE(k)=0.                                                         
! MELTING OF SNOW
            PSMLT(k)=0.                                                      
! EVAPORATION OF MELTING SNOW
            PMLTEV(k)=0.                                                     
! MELTING OF GRAUPEL
            PGMLT(k)=0.                                                        
! EVAPORATION OF MELTING GRAUPEL
            PMLTGE(k)=0.                                                       
! ENHANCED MELTING OF GRAUPEL BY COLLECTION OF RAIN WATER
            PGACRM(k)=0.                                                      
! ENHANCED MELTING OF GRAUPEL BY COLLECTION OF CLOUD WATER
            PGACWM(k)=0.                                                       
! RIMING OF CLOUD ICE
            PIACW(k)=0.                                                       
! AMOUNT OF CLOUD ICE CONVERTED INTO GRAUPEL
            PICNG(k)=0.                                                        
! AMOUNT OF RIME CONVERTED INTO GRAUPEL
            PGIACW(k)=0.                                                      
! REDUCE NI DUE TO SMALL XTALS COLLIDING PRODUCE LARGER ONES (STILL ICE)
! ADD TO ABOVE AMOUNT OF AGGREG THAT CONVERTS ICE TO SNOW
            NI_AG(k)=0.                                                        
! ICE MULTIPLICATION PROCESS
            NI_RS(k)=0.                                                       
! REDUCE NUMBER CONC OF CLOUD ICE SINCE CONVERTED INTO GRAUPEL
            NI_CG(k)=0.                                                      
! REDUCE NI DUE TO ICE GROWING TO SNOW SIZES
            NI_DE(k)=0.                                                        
! REDUCE NI DUE TO EVAPORATION
            NI_EV(k)=0.                                                       
! ICE MULTIPLICATION PROCESS
            PISPL(k)=0.                                                      
!   MURAKAMI SNOW TO GRAUPEL
            PSSACW(k)=0.                                                    
!   MURAKAMI SNOW TO GRAUPEL
            PGEMB(k)=0.                                                        
!   MARAT TENDENCY TERMS
            PNIFCW(k)=0.
            PNGFR(k)=0.
            PINACW(k)=0.
            PINACR(k)=0.
            PSNACW(k)=0.
            PSNACR(k)=0.
            PGNACW(k)=0.
            PGNACR(k)=0.
            PNRC(k)=0.
            PNNUC(k)=0.
            PMNUC(k)=0.
            PNAC(k)=0.
            PNCC(k)=0.
!   SNOW NUMBER CONCENTRATION
!
! AGGREGATION AMONG SNOW PARTICALES
            NSAG(k)=0.
! NUMBER OF COLLISIONS BETWEEN RAIN AND SNOW PARTICLES
            NSACR(k)=0.
! graupel number concentration terms
            NIACR(K)=0.
!
         enddo
!
! FORMULAS COME FROM DUDHIA (1989, JAS, 46),                                   
! RUTLEDGE AND HOBBS (1983, JAS, 40), {RH1},                                  
! RUTLEDGE AND HOBBS (1984, JAS, 41), {RH},                                  
! MURAKAMI (1989, JMSJ, 68),                                                
! AND LIN ET AL. (1983, JCAP, 22)                                              
! HSIE ET AL. (1980, JAP, 19)                                                  
! KOENIG (1972, MWR, 100)                                                      
! AND METEOROLOGICAL RESEARCH INSTITUTE OF JAPAN (#28, 1991)                 
! AND REISNER ET AL. (1998, QJRMS 124, 1071-1107)                              
! KHAIROUTINOV AND J KOGAN (2000) 
! COHARD AND PINTY.
                                                                                 
! CHECK DIACE_min FORMULATION
      DIACE_min = 2.0 * (3.0*XM01/(4.0*PI*DICE))**0.3333                       
      DO K=kts,kte
            XMI=qiz(k)/(qniz(k)+R1)
            RHOFAC(k) = SQRT((RHO_NOT/rhoz(k)))
            VT2R=(FRAIN*slor(k)**BR)*RHOFAC(k)                              
            VT2R=AMIN1(VT2R, 9.0)
            VT2S=(FSNOW*slos(k)**BS)*RHOFAC(k)                                   
            VT2S=AMIN1(VT2S, 2.0)
            TEST_S=(qsz(k)-R1)
            TEST_S=.5*(TEST_S+ABS(TEST_S))/(TEST_S+2.*R1)
            VT2S=VT2S*TEST_S
!
            VT2G=(FGRAUPEL*slog(k)**BG)*RHOFAC(k)                              
!
            TEST_G=(qgz(k)-R1)
            TEST_G=.5*(TEST_G+ABS(TEST_G))/(TEST_G+2.*R1)
            VT2G=VT2G*TEST_G
!
            DIACE=AMAX1(DIACE_min, ((6.*qiz(k) &                 
                   /(PI*DICE*(qniz(k)+R1)))**0.3333) )   
!            DIACE=AMIN1(DIACE,0.010000)
            DIACE=AMIN1(DIACE,1.99*xr0s)
!
            TEST_I=(qiz(k)-R1)
            TEST_I=(qiz(k)-R1)
            TEST_I=.5*(TEST_I+ABS(TEST_I))/(TEST_I+2.*R1)
            TEST_N=(qniz(k)-R1)
            TEST_N=.5*(TEST_N+ABS(TEST_N))/(TEST_N+2.*R1)
            DIACE=DIACE*(TEST_N*TEST_I)+(1.-TEST_N*TEST_I)*DIACE_min
!
            DKICE(K)=DIACE
            VT2I=700. * DIACE * RHOFAC(k)                          
            VT2I=AMAX1(VT2I, .3)
            A1 = 0.                                                              
            A2 = 0.                                                            
            XNC = 0.                                                          
            DCLO = (6.*qlz(k)/  &
             (PI*DRAIN*(CNP/rhoz(k)+R1)))**0.3333
            DCLO = AMAX1(1.e-6, DCLO)
                                                                                
            IF(.NOT. IIWARM) THEN

            SUPICE=(qvz(k)-qvsi(k))/DT                                   
! INITIATION OF CLOUD ICE  (R-A.21)                                              
! EITHER COOPER OR FLETCHER DEPENDING ON CONSTANTS IN PARAMR.F]                
                                                                               
            IF( (qvqvsi(k).GT.1.05.AND.tz(K).LT.273.15) .OR.   &          
                  (qvqvs(k).GT.1.0.AND.tz(K).LT.268.15) ) THEN            
!                XNC=TNO*EXP(ATO*(TO-AMAX1(tz(K),233.)))    ! test b
               XNC=TNO*EXP(ATO*(TO-AMAX1(tz(K),240.)))     ! test a                    
               PRI(k)=AMAX1(0.,XM01/rhoz(k)*(XNC-rhoz(k)*qniz(k))/DT)             
            ENDIF                                                                
! INITIATION OF CLOUD ICE VIA MEYERS EQN 2.4                                     
!            IF ( (qvqvsi(k).GT.1.0.AND.tz(K).LE.268.0) .OR               
!     +            . (qvqvs(k).GT.1.0.AND.tz(K).LE.271.0) ) THEN          
!               XNC = (EXP(-0.639 + 12.96*(qvqvsi(k)-1.0)))*1000.0             
!               PRI(k)=AMAX1(0.,XM01/rhoz(k)*(XNC-rhoz(k)*qniz(k))/DT)             
!            ENDIF                                                               
! FREEZING OF CLOUD DROPLETS, (R-A.22)                                          
            IF(qlz(k).GT.R1.AND.tz(K).LT.268.15)THEN                   
               PIFCW(k)=BP*(EXP(AP*(TO-tz(K)))-1.)* &           
                   qlz(k)**2/((CNP/rhoz(k)+R1)*DRAIN)
               PIFCW(k)=amin1(PIFCW(k),qlz(k)/DT)
            ENDIF                                                               
!
        IF(qrz(k).GT.R1)THEN                                            
! FREEZING OF RAIN, LIN ET AL. (45) (R-A.56)                                     
               IF(tz(K).LT.263.15) THEN                                   
                  PGFR(k)=slor(k)**7*FRD1*ronv(k)*(DRAIN/rhoz(k)  &       
                      )*(EXP(FRA1*(TO-tz(K)))-1.)                         
       PNGFR(k)=RNGFR*(EXP(0.66*(TO-tz(K)))-1.)*SLOR(K)**4
       ENDIF                                                             
!..COLLECTION OF CLOUD ICE BY RAIN, RH (A5)  (R-A.41)                            
               IF(qiz(k).GT.R1)THEN                                           
                  PRACI(k)=CIR*ronv(k)*qiz(k)*(-0.267*2.  &                 
                      *slor(k)**3+5.15E3*6.*slor(k)**4-1.0225E6*24. &        
                      *slor(k)**5+7.55E7*120.*slor(k)**6)                   
                  PRACI(k)=AMAX1(0.,PRACI(k))                               
!
                  NIACR(K)=PRACI(K)*qniz(K)/qiz(K)
!   the array NIACR(K) is not used but the tern is included after various adjustmemts to PRACI(K)
!
               ENDIF                                                             
! FREEZING OF RAIN BY COLLISION WITH ICE, RH (A7)  (R-A.42)                     
               IF(qiz(k).GT.R1.and.qniz(k).gt.r1)THEN                                            
                  PIACR(k)=CIRF*ronv(k)*qniz(k)*(-0.267* &        
                       120.*slor(k)**6+5.15E3* &                                 
                       720.*slor(k)**7-1.0225E6*5040.*slor(k)**8+  &           
                       7.55E7*40320.*slor(k)**9)                              
                  PIACR(k)=AMAX1(0.,PIACR(k))                               
               ENDIF                                                           
!
! COLLECTION OF RAIN BY SNOW, RH (A8)  (R-A.47)                               
               IF(qsz(k).GT.R1.and.tz(K).LT.273.15)THEN                                       
!              IF(qsz(k).GT.R1)THEN
      lmdr=slor(k)
      lmds=slos(k)
      ri_tbl=1+(lmdr-t_slor(1))/(t_slor(nrtb)-t_slor(1))*(nrtb-1)
      rj_tbl=1+(lmds-t_slos(1))/(t_slos(nstb)-t_slos(1))*(nstb-1)
      I_R=int(ri_tbl)
      J_S=int(rj_tbl)
      if((J_S.lt.1.or.J_S.gt.nstb-1).or.(I_R.lt.1.or.I_R.gt.nrtb-1)) then
      go to 1001
      endif
      epi=ri_tbl-float(I_R)
      epj=rj_tbl-float(J_S)

!                  PSACR(k)=(CSR*ronv(k)*sonv(k)/rhoz(k))*   &             
!                       SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA1*VT2R  &           
!                       *VT2S)*(5.*slor(k)**6*slos(k)+2.*slor(k)**5  &       
!                       *slos(k)**2+0.5*slor(k)**4*slos(k)**3)             
      psacr(k)=SON*RON*ronv(k)*sonv(k)/rhoz(k)*RHOFAC(k)*(t_psacr(I_R,J_S)*(1.-epi)*(1.-epj)+ &
       t_psacr(I_R,J_S+1)*(1.-epi)*epj + t_psacr(I_R+1,J_S)*epi*(1.-epj)+ t_psacr(I_R+1,J_S+1)*epi*epj)
!
! COLLECTION OF SNOW BY RAIN, RH (A9)  (R-A.48)                                
!                  PRACS(k)=(CRS*ronv(k)*sonv(k)/rhoz(k))*  &                
!                       SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA1*VT2R  &             
!                       *VT2S)*(5.*slos(k)**6*slor(k)+2.*slos(k)**5  &        
!                       *slor(k)**2+0.5*slos(k)**4*slor(k)**3)             
!
      pracs(k)=SON*RON*ronv(k)*sonv(k)/rhoz(k)*RHOFAC(k)*(t_pracs(I_R,J_S)*(1.-epi)*(1.-epj)+ &
       t_pracs(I_R,J_S+1)*(1.-epi)*epj + t_pracs(I_R+1,J_S)*epi*(1.-epj)+ t_pracs(I_R+1,J_S+1)*epi*epj)
!
!
               ENDIF
!
 1001 CONTINUE
       
               ENDIF                                                             
            IF(qsz(k).GT.R1)THEN                                             
!...ACCRETION OF CLOUD ICE BY SNOW, DUDHIA (B15)  (R-A.38)                       
               IF(qiz(k).GT.R1.AND.tz(K).LT.TO) THEN                      
                  PRAI(k)=ACRIS*sonv(k)*slos(k)**BACRIS*qiz(k)           
               ENDIF                                                             
! COLLECTION OF CLOUD WATER BY SNOW, RH1 (A22)  (R-A.46)                        
               IF(qlz(k).GT.R1) THEN                                        
                                                                                 
!                 New Lambda = (4.*(lambda)^4)^0.2; where lambda = 1./SLOS      
!                 CHANGES FOR NEW PARTICLE SIZE DISTRIBUTION OF SNOW             
                                                                                 
                                                                                 
                  SLOSN =slos(k)
                  IF(NEW_SNOW_DIST.EQ.1) SLOSN = 0.75785828 * (slos(k)**0.8)
                 DSNO = 4.*SLOSN
!
                  STOKE_S = DCLO*DCLO*VT2S*DRAIN/(9.*dum21(k)*DSNO)           
                  EFF_CS = STOKE_S*STOKE_S/((STOKE_S+0.4)*(STOKE_S+0.4))        
                  EFF_CS = MAX(0.0, MIN(1.0, EFF_CS))                          
!                  PSACW(k)=ACRCS*EFF_CS/EFCS*sonv(k)                         
!     +                 *slos(k)**BACRCS*qlz(k)                             
              PSACW(k)=ACRCS*EFF_CS/EFCS*sonv(k)*SLOSN**BACRCS*qlz(k)
                                                                                 
               ENDIF                                                             
            ENDIF                                                               
                                                                               
! COLLECTION OF CLOUD WATER BY GRAUPEL, RH (A11)  (R-A.59)                    
            IF(qgz(k).GT.R1)THEN                                           
! axel          SLOGN = 0.75785828 * (slog(k)**0.8)                            
             SLOGN = slog(k) 
               IF (qlz(k).GT.R1) THEN                                      
                                                                              
!          CHANGES FOR NEW PARTICLE SIZE DISTRIBUTION FOR GRAUPEL           
                                                                                 
                  DGRO = 4.*SLOGN                                               
                  STOKE_G = DCLO*DCLO*VT2G*DRAIN/(9.*dum21(k)*DGRO)            
                  EFF_CG = STOKE_G*STOKE_G/((STOKE_G+0.5)*(STOKE_G+0.5))        
                  EFF_CG = MAX(0.0, MIN(1.0, EFF_CG))                            
                  PGACW(k)=ACRCG*gonv(k)*EFF_CG/EFGC*SLOGN**BACRCG*qlz(k)                                               
                                                                                
!..old..                  DGRO = 4.*slog(k)                                    
!..old..                  STOKE_G = DCLO*DCLO*VT2G*DRAIN/(9.*dum21(k)*DGRO)    
!..old..                  EFF_CG = STOKE_G*STOKE_G/((STOKE_G+0.5)*(STOKE_G+0.5)) 
!..old..                  EFF_CG = MAX(0.0, MIN(1.0, EFF_CG))                    
!..old..                  PGACW(k)=ACRCG*gonv(k)*EFF_CG/EFGC*slog(k)**BACR 
!..old..     +                 *qlz(k)                                       
                                                                                 
               ENDIF                                                            
                                                                               
! COLLECTION OF RAIN BY GRAUPEL, RH (A13)  (MISSING IN R)                     
               IF (qrz(k).GT.R1) THEN                                         
                  PGACR(k)=(CRG*ronv(k)*gonv(k)/rhoz(k))  &              
                       *ABS(VT2G-VT2R)*(5.*slor(k)**6*slog(k)+2. &          
                       *slor(k)**5*slog(k)**2+0.5*slor(k)**4 &              
                       *slog(k)**3)                                           
               ENDIF                                                             
            ENDIF                                                                
! ICE MULTIPLICATION PROCESS, MRI (11-21)  (R-A.24,25)                          
! MOVE TO PARAMR
            HALLET_MOSSOP=1
            IF(HALLET_MOSSOP.EQ.1)THEN
            IF(PGACW(k).GT.0..AND.tz(K).GE.265..AND.tz(K).LE.270.) THEN                                                    
               TF=0.                                                             
               IF(tz(K).GE.268..AND.tz(K).LE.270.)  &                    
                   TF=(tz(K)-270.)/(268.-270.)                           
               IF(tz(K).GE.265..AND.tz(K).LT.268.)  &                  
                   TF=(tz(K)-265.)/(268.-265.)                         
               NI_RS(k)=rhoz(k)*3.5E8*TF*PGACW(k)                          
               PISPL(k)=NI_RS(k)*XM01/rhoz(k)                              
            ENDIF                                                                
!
            ENDIF
!
! DEPOSITION OF CLOUD ICE: MRI (11-24)  (R-A.26)                                
            IF (qiz(k).GT.R1.AND.tz(K).LT.TO) THEN                       
            IT=NINT(tz(K) - 273.16)           
               IF(IT.LE.-1.AND.IT.GT.-31)THEN                                    
                  A2=ABER2(ABS(IT))                                              
                  A1=ABER1(ABS(IT))*(1.*0.001)**(1.-A2)                         
               ELSEIF(IT.LE.-31)THEN                                             
                  A2=ABER2(31)                                                  
                  A1=ABER1(31)*(1.*0.001)**(1.-A2)                               
               ENDIF                                                            
!
               PRD(k)=(qvz(k)-qvsi(k))/(qvs(k)-qvsi(k))  &          
                   *A1*XMI**A2*qniz(k)
            ENDIF                                                               
!...DEPOSITION/SUBLIMATION OF SNOW: DUDHIA (B14)  (R-A.36)                      
            IF(qsz(k).GT.R1.AND.tz(K).LT.TO)THEN                        
                                                                               
!              CHANGES FOR NEW PARTICLE SIZE DISTRIBUTION OF SNOW               
                                                                                 
             IF(NEW_SNOW_DIST.EQ.1)THEN
              SLOSN = 0.75785828 * (slos(k)**0.8)
              PREI(k)=DEPS1*sonv(k)*(qvz(k)/qvsi(k)-1.)  &
                   *(0.65*SLOSN**3+SQRT(DEPS2*rhoz(k)  &
                   /dum21(k))*0.44*4.423*SLOSN**(DEPS4+1.))  &
                   /abi(k) - dum11(k)*PSACW(k)

             ELSE
! original Rutledge and Hobbs
               PREI(k)=DEPS1*sonv(k)*(qvz(k)/qvsi(k)-1.)   &             
                   *(0.65*slos(k)*slos(k)+SQRT(DEPS2*rhoz(k) &   
                   /dum21(k))*DEPS3*.84*slos(k)**(DEPS4))   &             
                   /abi(k) - dum11(k)*PSACW(k)                            
!  M=aD**B Brown and Francis with Koenig and Murry growth rates
!      PREI(k)=SON*sonv(k)*(qvz(k)-qvsi(k))/
!     1 (qvs(k)-qvsi(k))*
!     2 A1*(0.0185377**A2)*WGAMMA(A2*1.9+1.)*(slos(k)**(A2*1.9+1.))
!     3 /rhoz(k) - dum11(k)*PSACW(k)
!  M=rho_snow*sphere with Koenig and Murry growth rates
!      PREI(k)=SON*sonv(k)*(qvz(k)-qvsi(k))/
!     1 (qvs(k)-qvsi(k))*
!     2 A1*(DSNOW*PI/6.)**A2*WGAMMA(A2*3.+1.)*(slos(k)**(A2*3.+1.))
!     3 /rhoz(k)-dum11(k)*PSACW(k)
             ENDIF
!
!
!
!   MURAKAMI SNOW TO GRAUPEL                                                     
                                                                                 
               IF(qlz(k).GT.R1) THEN                                       
                                                                             
!  changed snow to graupel conversion to require riming growth exceed              GREG T.
!  depositional growth by factor 3.0 (Murakami required a 1.0 factor               GREG T.
!  while RH1984 required qc>0.5 and qs>0.1 - similar to 5.0 factor)                GREG T.
!                  IF (PREI(k).GT.PSACW(k)) THEN                        
                   IF (PREI(k).GT.0.3333*PSACW(k)) THEN
                     PGEMB(k)=0.                                              
                     PGSACW(k)=0.                                            
                     PSSACW(k) = PSACW(k)                                
                  ELSE                                                         
!                     PGSACW(k)=PSACW(k)                                    
                     PGEMB(k)=DSNOW/(DGRAUPEL-DSNOW) * PSACW(k)              
                     PGSACW(k)=PSACW(k)-PGEMB(k)
                     PSSACW(k)=0.                                              
                  ENDIF                                                          
               ENDIF                                                             
            ENDIF                                                               
                                                                                 
! SUBLIMATION/DEPOSITIONAL GROWTH OF GRAUPEL, RH (A17)  (R-A.57)                 
!            IF(qgz(k).GT.R1.AND.tz(K).LT.TO)THEN
            IF(qgz(k).GT.1.e-7.AND.tz(K).LT.TO)THEN                      
                                                                                 
!..gamma..               PREG(k)=DEPG1*gonv(k)*(qvz(k)/qvsi(k)-1.)  &
!..gamma..                   *(0.78*SLOGN**3+(SQRT(DEPG2*rhoz(k)  &
!..gamma..                   /dum21(k)))*.31*4.098*SLOGN**(DEPG4+1.))  &
 !..gamma..                   /abi(k) - dum11(k)*PGACW(k)
!
            PREG(k)=DEPG1*gonv(k)*(qvz(k)/qvsi(k)-1.)  &
                    *(0.78*SLOGN**2+(SQRT(DEPG2*rhoz(k)  &
                    /dum21(k)))*DEPG3*SLOGN**(DEPG4))  &
                    /abi(k) - dum11(k)*PGACW(k)
                                                                                 
!..old..               PREG(k)=DEPG1*gonv(k)*(qvz(k)/qvsi(k)-1.)       
!..old..     +              *(0.78*slog(k)*slog(k)+((DEPG2*rhoz(k)         
!..old..     +              /dum21(k))**0.5)*DEPG3*slog(k)**(DEPG4))         
!..old..     +              /abi(k) - dum11(k)*PGACW(k)                    
            ENDIF                                                                
!            FUDGEF = 1.0                                                      
!            FUDGEF = 0.5                                                         
            FUDGEF = 0.95
            SUM_DEP = PRI(k)+PRD(k)+PREI(k)+PREG(k)                     
            IF( (SUPICE.GT.0. .AND. SUM_DEP.GT.SUPICE*FUDGEF) &                  
            .OR. (SUPICE.LT.0. .AND. SUM_DEP.LT.SUPICE*FUDGEF) ) THEN             
               PRI(k) = FUDGEF*PRI(k)*SUPICE/SUM_DEP                        
               PRD(k) = FUDGEF*PRD(k)*SUPICE/SUM_DEP                        
               PREI(k) = FUDGEF*PREI(k)*SUPICE/SUM_DEP                    
               PREG(k) = FUDGEF*PREG(k)*SUPICE/SUM_DEP                   
            ENDIF                                                               
! RIMING OF CLOUD ICE, MRI (11-25A)   (R-A.27,28A)                              
            UDCLO=3.E7*DCLO*DCLO                                               
            RPHI=DCLO*(DRAIN*VT2I/(3.24E-4*DIACE))**0.5                          
            IF(RPHI.GE.0.2704.AND.qlz(k).GT.R1.AND.  &                        
                DIACE.GT.100.E-6.AND.tz(K).LT.TO)THEN                    
               EIC1=0.572*ALOG10(RPHI-0.25)+0.967                             
               EIC1=AMIN1(EIC1,0.5)
               PIACW1=PI/4.*(rhoz(k)*qniz(k)+R1)*(DIACE+DCLO)**2*EIC1  &      
                   *ABS(VT2I-UDCLO)*qlz(k)                                  
               PRDTEMP=AMAX1(PRD(k),0.0)                                       
               PIACW(k)=AMIN1(PIACW1,PRDTEMP)                                  
!
! AMOUNT OF RIME CONVERTED INTO GRAUPEL, MRI (11-38)  (R-A.28B)                  
               PGIACW(k) = AMAX1(0.0, PIACW1-PRDTEMP)                          
               CLIC=AMAX1((PIACW1-PRDTEMP)/(rhoz(k)*qniz(k)+R1),0.0)          
! AMOUNT OF CLOUD ICE CONVERTED INTO GRAUPEL, MRI (11-36)  (R-A.39)             
               DMGI=XM0G-XMI                                                   
               IF(DMGI.GT.0.)THEN                                                
                  PICNG(k)=CLIC*qiz(k)*rhoz(k)/DMGI                       
               ENDIF                                                             
! REDUCE NUMBER CONC OF CLOUD ICE SINCE CONVERTED INTO GRAUPEL  (R-A.40)         
               NI_CG(k)=rhoz(k)/XM0G*(PICNG(k)+PGIACW(k))                
            ENDIF                                                                
            IF(qiz(k).GT.R1.AND.tz(K).LT.TO)THEN                          
! CONVERSION OF CLOUD ICE TO SNOW, MRI (11-30)  (R-A.30A,30B)                    
               DEPAC = 0.                                                        
               IF (PRD(k).GT.0.0) THEN                                         
                  IF(XMI.LE.0.5*XM0S)THEN                                      
                     DEPAC=XMI/(XM0S-XMI)*(PRD(k)+PIACW(k))                  
                  ELSE                                                         
                     DEPAC=PRD(k)+PIACW(k)+(1.-0.5*XM0S/XMI)*qiz(k)/(2.*DT)                                       
                  ENDIF                                                          
!
! REDUCE NI DUE TO ICE GROWING TO SNOW SIZES                                    
                  NI_DE(k) = DEPAC*(rhoz(k)/XM0S)                          
               ELSE                                                           
! REDUCE NI DUE TO EVAPORATION (MISSING IN R reference)                      
                  NI_EV(k) = -PRD(k)*(rhoz(k)/XMI)                        
               ENDIF                                                            
               PSAGI = 0.                                                      
! REDUCE NI DUE TO SMALL XTALS COLLIDING PRODUCE LARGER ONES (STILL ICE)         
! THIS IS MISSING IN REISNER PAPER BUT IS IN MRI (M-39)                          
!       C1=700.*0.1*0.25/DICE
!  old            C1_AG = C1*rhoz(k)*qiz(k)*RHOFAC(k)                             
                C1_AG = PI/6. *0.1*0.25*VT2I*DIACE*DIACE
!  old            NI_AG(k) = 0.5*C1_AG*rhoz(k)*qniz(k)                                 
!  greg              NI_AG(k) = 0.5*C1_AG*(rhoz(k)*qniz(k))**2
! ice_1   NI_AG(k) = 0.5*pi/6.*DIACE**2*vt2i*.1*.25*(rhoz(k)*qniz(k))**2
                NI_AG(k) =.5*C1_AG*(rhoz(k)*qniz(k))**2
               IF (DIACE.LT.2.*XR0S) THEN                                       
!                 DTAU=-2.0/C1_AG*ALOG10((DIACE/(2.*XR0S))**3)                  
                  DTAU=-2.0/(C1_AG*qniz(k)*rhoz(k))*ALOG10((DIACE/(2.*XR0S))**3)                  
                  PSAGI=qiz(k)/DTAU                                          
                DTAU=AMIN1(DTAU,DT)
               ENDIF                                                          
               PSCNI(k)=DEPAC+PSAGI                                            
! ADD TO ABOVE AMOUNT OF AGGREG THAT CONVERTS ICE TO SNOW (R-A.31)              
               NI_AG(k) = NI_AG(k) + PSAGI*(rhoz(k)/XMI)                
            ENDIF                                                                
!
            ENDIF  ! IIWARM FALSE
!

!---AUTOCONVERSION OF CLOUD WATER TO RAINWATER  (R-A.60)
      QCK1=1.0E-3
      QCTH=0.35E-3
! kessler control run            PRC(k)=AMAX1(0.,QCK1*(qlz(k)-QCTH))
!        ! BERRY AUTOCONVERSION RATE:
!..            NB = CNP
!..            DB = 0.30
!..            CWC = rhoz(k)*qlz(k)*1000.
!..            PRC(k) = 0.016667*CWC**2/(5.+0.0366*NB/(CWC*DB))
!
!+---+----------------------------------------------------------------
!       Berry and Reinhardt autoconversion (1974, Part II) as in       
!       Walko et al., (1995) but WITH TYPOS CORRECTED IN BOTH
            xnu = amax1(0.0, (CNP*1.E-6 - 100.)/100.)
           BandR_L2 = 0.027*qlz(K)*(6.25E18*(DCLO**4)/sqrt(1.+xnu) & 
                            - 0.4)                                     
!            BandR_T2  = 3.7/(rhoz(K)*qlz(K)+R1) / (0.5E6*DCLO     &
!                            *(1.+xnu)**(-1./6.) - 7.5)
            BandR_T2  = 3.7/(rhoz(K)*qlz(K)+R1) 
            BandR_T3= (0.5E6*DCLO     &       
                            *(1.+xnu)**(-1./6.) - 7.5)                  
            if (BandR_L2.gt.0.0 .and. BandR_T2.gt.0.0) &               
           PRC(K) = BandR_L2/BandR_T2*BandR_T3                               
!---+-----------------------------------------------------------------
!
            IF(qrz(k).GT.R1)THEN
!.. Added an IF block for Cloud water per John Brown code 2 Nov 2000
               IF(qlz(k).GT.R1) THEN
               DRNO = 4.*slor(k)
               STOKE_R = DCLO*DCLO*VT2R*DRAIN/(9.*dum21(k)*DRNO)
               EFF_CR = STOKE_R*STOKE_R/((STOKE_R+0.5)*(STOKE_R+0.5))
               EFF_CR = MAX(0.0, MIN(1.0, EFF_CR))
!---ACCRETION OF CLOUD WATER BY RAINWATER  (R-A.61)
               PRA(k)=ACRCR*ronv(k)*EFF_CR/EFCR*slor(k)**BACRCR*qlz(k)
               ENDIF

            ENDIF


            IF(qrz(k).GT.R1)THEN
!
!...EVAPORATION OF RAINWATER  (R-A.62) [No depositional growth of rain - 17Feb20 
               PRE(k)=DEPR1*ronv(k)*(qvz(k)/qvs(k)-1.)*(  &
                   0.78*slor(k)*slor(k)+(SQRT(DEPR2*rhoz(k)  &
                   /dum21(k)))*DEPR3*0.84*slor(k)**(DEPR4))  &
                   /abw(k)
               PRE(k) = amin1(PRE(k), 0.0)                                  
            ENDIF                                                             
            IF(.NOT. IIWARM) THEN
            IF (tz(K).GT.TO) THEN                                           
! REMEMBER THE SIGNS ARE REVERSED TO-tz                                       
! GO BACK AND CHECK FOR CONSERVATION                                             
               XLATF=XLS_MP-dum31(k)                                              
               IF(qsz(k).GT.R1)THEN                                           
! MELTING OF SNOW, RH1 (A23)  (R-A.37)                                          
                  PSMLT(k)=PSM1*sonv(k)*scr4(k)/XLATF*  &
                      (TO-tz(K))*(0.65*slos(k)*slos(k)+  &
                      (SQRT(PSM2*rhoz(k)/dum21(k)))*PSM3  &
                      *slos(k)**PSM4)
!           
                                                                                 
! EVAPORATION OF MELTING SNOW FOR T > TO, RH1 (A27)                              
                  IF(qvz(k)-qvs(k).LT.0.)THEN                              
                     PMLTEV(k)=DEPS1*sonv(k)*(qvz(k)/qvs(k)  &
                         -1.)*(0.65*slos(k)*slos(k)+(SQRT(DEPS2  &
                         *rhoz(k)/dum21(k)))*DEPS3*0.84  &
                         *slos(k)**(DEPS4))/abw(k)
!.. Used to be AB in denominator of above line but changed to ABW                
!.. per John Brown code 2 Nov 2000.                                              
                  ENDIF                                                          
               ENDIF                                                             
               IF(qgz(k).GT.R1)THEN                                        
! MELTING OF GRAUPEL, RH (A18)  (R-A.58)                                         
                  PGMLT(k)=PGM1*gonv(k)*scr4(k)/XLATF* &
                      (TO-tz(K))*(0.78*slog(k)*slog(k)+ &
                      (SQRT(PGM2*rhoz(k)/dum21(k)))*PGM3 &
                      *slog(k)**PGM4)
                                                                                
! EVAPORATION OF MELTING GRAUPEL, RH (A19)                                      
                  IF(qvz(k)-qvs(k).LT.0.)THEN                           
! put in the proper equation not the ab stuff
                     PMLTGE(k)=DEPG1*gonv(k)*(qvz(k) &
                         /qvs(k)-1.)*(0.78*slog(k)*slog(k) &
                         +(SQRT(DEPG2*rhoz(k)/dum21(k)))*DEPG3 &
                         *slog(k)**(DEPG4))/abw(k)
!
!.. Used to be AB in denominator of above line but changed to ABW               
!.. per John Brown code 2 Nov 2000.                                              
                  ENDIF                                                          
! SHEDDING OF ACCRETED WATER, RH (A20)                                           
! SUM OF PGACR(k) AND PGACW(k) ADDED TO TEND                                 
! ENHANCED MELTING OF GRAUPEL BY COLLECTION OF RAIN WATER, RH (A21)             
! ENHANCED MELTING OF GRAUPEL BY COLLECTION OF CLOUD WATER, RH (A22)             
! SIGNS ARE REVERSED  (TO-tz)                                                
                  PGACRM(k)=CW/XLATF*(TO-tz(K))*PGACR(k)                
                  PGACWM(k)=CW/XLATF*(TO-tz(K))*PGACW(k)                
               ENDIF                                                            
            ENDIF                                                                
!
        ENDIF   ! IIWARM
        ENDDO
!
                                                                                
      DO k = kts, kte
            PRE(k)=AMAX1(-qrz(k)/DT,PRE(k))                            
            PREI(k)=AMAX1(-qsz(k)/DT,PREI(k))                           
            PREG(k)=AMAX1(-qgz(k)/DT,PREG(k))                        
            PRD(k)=AMAX1(-qiz(k)/DT,PRD(k))                               
! CHECKS TO SEE WHETHER GRAUPEL OR SNOW IS FORMED, SEE RH  (R-A.51)             
            ALPSNOW=AMAX1(1.E-25,DSNOW**2*(4.*slos(k))**6)                   
            ALPRAIN=AMAX1(1.E-25,DRAIN**2*(4.*slor(k))**6)                    
            ALPHARS=ALPSNOW/(ALPSNOW+ALPRAIN)                                    
            D1=1.                                                                
            IF (qrz(k).GT.R1) THEN                                            
               thetah = 0.5*pi*(1. - 1.e4*min(qrz(k),1.e-4))                 
               D1 = SIN(thetah)                                                
            ENDIF                                                            
           D1FIELD(k)=D1
           ALFIELD(k)=ALPHARS
                                                                            
!            IF(tz(K).LE.TO)THEN                                     
            IF(tz(K).LE.TO.AND.(.NOT. IIWARM))THEN
! CONSERVATION OF QC                                                         
               sum_terms=(PRC(k)+PRA(k)+PSACW(k)+PGACW(k)+  &          
                   PIFCW(k)+PIACW(k)+PGIACW(k))*DT                      
               IF(sum_terms.GE.qlz(k).AND.qlz(k).GT.R1)THEN              
                  ratio = qlz(k)/sum_terms                                   
                  PIFCW(k)=ratio*PIFCW(k)                                   
                  PRC(k)=ratio*PRC(k)                                      
                  PRA(k)=ratio*PRA(k)                                     
                  PSACW(k)=ratio*PSACW(k)                                
                  PGSACW(k)=ratio*PGSACW(k)                             
                  PSSACW(k)=ratio*PSSACW(k)                                 
                  PGEMB(k)=ratio*PGEMB(k)                                    
                  PGACW(k)=ratio*PGACW(k)                                    
                  PIACW(k)=ratio*PIACW(k)                                   
                  PGIACW(k)=ratio*PGIACW(k)                                
! MARAT TERMS
                  PNIFCW(k)=ratio*PNIFCW(k)
                  PINACW(k)=ratio*PINACW(k)
                  PGNACW(k)=ratio*PGNACW(k)
                  PSNACW(k)=ratio*PSNACW(k)
                  PNCC(k)=ratio*PNCC(k)
                  PNAC(k)=ratio*PNAC(k)
!
               ENDIF                                                            
!  CONSERVATION OF CLOUD ICE                                                       
               sum_terms=(PSCNI(k)+PRAI(k)-PRI(k)-PRD(k)   &              
                   +PRACI(k)-PIFCW(k)-PIACW(k)    &                       
                   +PICNG(k)-PISPL(k))*DT                                 
               IF(sum_terms.GE.qiz(k).AND.qiz(k).GT.R1)THEN                 
                  ratio = qiz(k)/sum_terms                                    
                  PSCNI(k)=ratio*PSCNI(k)                                  
                  PRAI(k)=ratio*PRAI(k)                                      
                  PRI(k)=ratio*PRI(k)                                        
                  PRD(k)=ratio*PRD(k)                                       
                  PRACI(k)=ratio*PRACI(k)                                    
                  PIFCW(k)=ratio*PIFCW(k)                                   
                  PIACW(k)=ratio*PIACW(k)                                  
                  PICNG(k)=ratio*PICNG(k)                                   
                  PISPL(k)=ratio*PISPL(k)                                  
               ENDIF                                                             
!  CONSERVATION OF RAIN                                                            
               sum_terms=(PIACR(k)+PSACR(k)+PGACR(k)-PRC(k)   &           
                   -PRA(k)+PGFR(k)-PRE(k))*DT                             
               IF(sum_terms.GE.qrz(k).AND.qrz(k).GT.R1)THEN                
                  ratio = qrz(k)/sum_terms                                    
                  PIACR(k)=ratio*PIACR(k)                                   
                  PSACR(k)=ratio*PSACR(k)                                  
                  PGACR(k)=ratio*PGACR(k)                                 
                  PGFR(k)=ratio*PGFR(k)                                  
                  PRC(k)=ratio*PRC(k)                                        
                  PRA(k)=ratio*PRA(k)                                      
                  PRE(k)=ratio*PRE(k)                                    
! MARAT TERMS
                  PNGFR(k)=ratio*PNGFR(k)
                  PINACR(k)=ratio*PINACR(k)
                  PGNACR(k)=ratio*PGNACR(k)
                  PSNACR(k)=ratio*PSNACR(k)
                  PNRC(k)=ratio*PNRC(k)
!
               ENDIF                                                        
!  CONSERVATION OF SNOW                                                            
               sum_terms=(PRACS(k)*(1.-ALPHARS)+PGEMB(k)  &               
                   -PREI(k)-PSCNI(k)-PRAI(k)- &                            
                   PIACR(k)*D1-PRACI(k)*D1-PSACR(k)*ALPHARS- &            
                   PSSACW(k))*DT                                              
               IF(sum_terms.GE.qsz(k).AND.qsz(k).GT.R1)THEN               
                  ratio = qsz(k)/sum_terms                                    
                  PRACS(k)=ratio*PRACS(k)                                   
                  PGEMB(k)=ratio*PGEMB(k)                                  
                  PSCNI(k)=ratio*PSCNI(k)                                    
                  PRAI(k)=ratio*PRAI(k)                                     
                  PIACR(k)=ratio*PIACR(k)                                   
                  PRACI(k)=ratio*PRACI(k)                                   
                  PSACR(k)=ratio*PSACR(k)                                  
                  PSSACW(k)=ratio*PSSACW(k)                                  
                  PREI(k)=ratio*PREI(k)                                     
               ENDIF                                                           
!  CONSERVATION OF GRAUPEL                                                          
               sum_terms=-(PGACW(k)+PGACR(k)+ &                              
                   PREG(k)+(1.-D1)*PRACI(k)+(1.-D1)*  &                     
                   PIACR(k)+(1.-ALPHARS)*PSACR(k)+(1.-ALPHARS)*  &        
                   PRACS(k)+PGEMB(k)+PGSACW(k)+   &                         
                   PGFR(k)+PICNG(k)+PGIACW(k))*DT                         
               IF(sum_terms.GE.qgz(k).AND.qgz(k).GT.R1)THEN          
                  ratio = qgz(k)/sum_terms                                 
                  PGACW(k)=ratio*PGACW(k)                                    
                  PGACR(k)=ratio*PGACR(k)                                   
                  PREG(k)=ratio*PREG(k)                                    
                  PRACI(k)=ratio*PRACI(k)                                   
                  PIACR(k)=ratio*PIACR(k)                                    
                  PSACR(k)=ratio*PSACR(k)                                    
                  PRACS(k)=ratio*PRACS(k)                                    
                  PGEMB(k)=ratio*PGEMB(k)                                   
                  PGSACW(k)=ratio*PGSACW(k)                                  
                  PGFR(k)=ratio*PGFR(k)                                     
                  PICNG(k)=ratio*PICNG(k)                                  
                  PGIACW(k)=ratio*PGIACW(k)                                  
               ENDIF                                                             
                                                                                 
!.. Added since we may have modified the two variables independently             
!.. 2 Nov 2000 per John Brown code                                               
               PSACW(k) = PSSACW(k) + PGSACW(k) + PGEMB(k)
               PISPL_FS = 0.                                                     
               PISPL_FG = 0.                                                     
               IF (PISPL(k).GT.R1 .AND. (PSACW(k)+PGACW(k)).GT.0.) THEN                         
                  PISPL_FS=PISPL(k)*(PSACW(k)/(PSACW(k)+PGACW(k)))                                         
                  PISPL_FG=PISPL(k)*(PGACW(k)/(PSACW(k)+PGACW(k)))                                        
               ELSE                                                           
                  PISPL(k) = 0.0                                             
               ENDIF                                                          
                                                                             
! WATER VAPOR TENDENCY                                                           
               qvzTEN(k)=qvzTEN(k)-(PRE(k)+PREI(k)+PRI(k)+PRD(k)+PREG(k))                
! CLOUD WATER TENDENCY                                                          
               qlzTEN(k)=qlzTEN(k)-(PRC(k)+PRA(k)  &                
                   +PIACW(k)+PSACW(k)+PGACW(k)+PIFCW(k)   &              
                   +PGIACW(k))                                      
! RAIN TENDENCY                                                                  
               qrzTEN(k)=qrzTEN(k)+(PRC(k)+PRA(k)   &                
                   +PRE(k)-PGACR(k)-PSACR(k)-PIACR(k)  &                  
                   -PGFR(k))                                         
!
! CLOUD ICE TENDENCY                                                             
               qizTEN(k)=qizTEN(k)-(PSCNI(k)+PRAI(k)  &               
                    -PRI(k)-PRD(k)+PRACI(k)-PIFCW(k)     &                  
                    -PIACW(k)+PICNG(k)-PISPL(k))                   
! SNOW TENDENCY                                                                 
               qszTEN(k)=qszTEN(k)+(PREI(k)+PSCNI(k)   &                  
                    +PRAI(k)+D1*PRACI(k)+D1*PIACR(k)+ALPHARS*   &            
                    PSACR(k)-(1.-ALPHARS)*PRACS(k)+PSSACW(k)  &               
                    -PGEMB(k)-PISPL_FS)                            
!
! Note we only  allow depleation during sublimation
!
!
!                                                                               
! GRAUPEL TENDENCY                                                               
               qgzTEN(k)=qgzTEN(k)+(PGACW(k)-PISPL_FG   &              
                    +PGACR(k)+PREG(k)+(1.-D1)*PRACI(k)   &                   
                    +(1.-D1)*PIACR(k)+(1.-ALPHARS)*PSACR(k) &                  
                    +(1.-ALPHARS)*PRACS(k)+PGEMB(k)+PGSACW(k) &              
                    +PGFR(k)+PICNG(k)+PGIACW(k))                  
!
! NUMBER OF CLOUD ICE TEND    qnlTEN SHOULD BE #/Kg                                                          
!
               qnizTEN(k)=qnizTEN(k)+((PRI(k) +PIFCW(k))*rhoz(k)/XM01-(PRAI(k)  &
                   +PRACI(k)) *rhoz(k)*qniz(k)/(qiz(k)+R1)  &
                   +NI_RS(k)-NI_CG(k) -NI_AG(k)-NI_DE(k)-NI_EV(k))/rhoz(k)
                                                                                 
! SET CONSTRAINTS  (R-A.68)                                                     
               TEMP_QI = AMAX1(0.,qiz(k)+DT*qizTEN(k))                                                  
               TEMP_QNI  = AMAX1(R1,qniz(k)+DT*qnizTEN(k))                                               
                 RLU=TEMP_QI/XM01
               IF (TEMP_QNI.GT.RLU) THEN                                          
                  qnizTEN(k)=(RLU-qniz(k))/DT                               
               ENDIF                                                             
!
               XLATF=XLS_MP-dum31(k)                                              
               TEMP=-XLS_MP*(PREI(k)+PRD(k)+PRI(k)+PREG(k))   &               
                    -dum31(k)*PRE(k) -XLATF &                 
                   *(PSACW(k)+PIACR(k)+PSACR(k)+PGACR(k)   &              
                   +PGACW(k)+PIFCW(k)+PIACW(k)+PGFR(k)  &                 
                   +PGIACW(k))                                       
               PRDX(k)=CP_MP*(1.+0.887*qvz(k))                                
               tzTEN(k)=tzTEN(k)-TEMP/PRDX(k)*(1-IFDRY)             
!+---+-----------------------------------------------------------------+         
!-- ABOVE 0 C CASE or IIWARM eq TRUE BELOW HERE                                                   
!+---+-----------------------------------------------------------------+         
            ELSE                                                                 
!  CONSERVATION OF QC                                                               
               sum_terms=(PRC(k)+PRA(k)+PSACW(k)+PGACW(k))*DT            
               IF(sum_terms.GE.qlz(k).AND.qlz(k).GT.R1)THEN              
                  ratio = qlz(k)/sum_terms                                  
                  PRC(k)=ratio*PRC(k)                                        
                  PRA(k)=ratio*PRA(k)                                       
                  PSACW(k)=ratio*PSACW(k)                                  
                  PGACW(k)=ratio*PGACW(k)                                    
               ENDIF                                                             
!  CONSERVATION OF QR                                                              
               sum_terms=-(PRC(k)+PRA(k)+PRE(k)-PGMLT(k)  &                
                   -PSMLT(k)-PGACRM(k)-PGACWM(k)+PSACW(k)+  &             
                   PRACS(k)+PGACW(k))*DT                                    
               IF(sum_terms.GE.qrz(k).AND.qrz(k).GT.R1)THEN                
                  ratio = qrz(k)/sum_terms                                    
                  PRC(k)=ratio*PRC(k)                                        
                  PRA(k)=ratio*PRA(k)                                        
                  PRE(k)=ratio*PRE(k)                                        
                  PGMLT(k)=ratio*PGMLT(k)                                   
                  PSMLT(k)=ratio*PSMLT(k)                                  
                  PGACRM(k)=ratio*PGACRM(k)                              
                  PGACWM(k)=ratio*PGACWM(k)                                 
                  PSACW(k)=ratio*PSACW(k)                                    
                  PRACS(k)=ratio*PRACS(k)                                    
                  PGACW(k)=ratio*PGACW(k)                                    
               ENDIF                                                             
               IF(.NOT. IIWARM) THEN
!  CONSERVATION SNOW                                                                
               sum_terms=(PRACS(k)-PMLTEV(k)-PSMLT(k))*DT                  
               IF(sum_terms.GE.qsz(k).AND.qsz(k).GT.R1)THEN                
                  ratio = qsz(k)/sum_terms                                   
                  PMLTEV(k)=ratio*PMLTEV(k)                                  
                  PSMLT(k)=ratio*PSMLT(k)                                    
                  PRACS(k)=ratio*PRACS(k)                                    
               ENDIF                                                             
!  CONSERVATION OF GRAUPEL                                                          
               sum_terms=(-PGMLT(k)-PGACRM(k)-PGACWM(k)-PMLTGE(k))*DT                                             
               IF(sum_terms.GE.qgz(k).AND.qgz(k).GT.R1)THEN          
                  ratio = qgz(k)/sum_terms                                 
                  PGMLT(k)=ratio*PGMLT(k)                                   
                  PGACRM(k)=ratio*PGACRM(k)                                  
                  PGACWM(k)=ratio*PGACWM(k)                                  
                  PMLTGE(k)=ratio*PMLTGE(k)                                  
               ENDIF                                                           
!
!
               endif   ! IIWARM FALSE
!
! WATER VAPOR TENDENCY                                                           
               qvzTEN(k)=qvzTEN(k)-(PRE(k)+PMLTEV(k)+PMLTGE(k))                                        
! CLOUD WATER TENDENCY                                                           
               qlzTEN(k)=qlzTEN(k)-(PRC(k)+PRA(k)+PSACW(k)+PGACW(k))                              
!
! RAIN TENDENCY                                                                  
               qrzTEN(k)=qrzTEN(k)+(PRC(k)+PRA(k)  &                  
                    +PRE(k)-PGMLT(k)-PSMLT(k)-PGACRM(k)  &                  
                    -PGACWM(k)+PSACW(k)+PRACS(k)  &                          
                    +PGACW(k))                                         
!
!
               IF(.NOT. IIWARM) THEN
!
!
! SNOW TENDENCY                                                                
               qszTEN(k)=qszTEN(k)+(PSMLT(k)-PRACS(k)+PMLTEV(k))                             
!
! SNOW NUMBER TENDENCY
! GRAUPEL TENDENCY                                                              
               qgzTEN(k)=qgzTEN(k)+(PGMLT(k)+PGACRM(k)+PGACWM(k)+PMLTGE(k))                
!
         endif   ! iiwarm false
!
               XLATF=XLS_MP-dum31(k)                                              
               TEMP=-XLATF*(PGMLT(k)+PSMLT(k)+PGACWM(k)    &                 
                    +PGACRM(k)-PRACS(k)) -dum31(k)   &              
                    *(PRE(k)+PMLTEV(k)+PMLTGE(k))                  
               PRDX(k)=CP_MP*(1.+0.887*qvz(k))                                 
               tzTEN(k)=tzTEN(k)-TEMP/PRDX(k)*(1-IFDRY)               
            ENDIF                                                                
!
       enddo
!                                                                                
! CLOUD NEXT PART IS TO GET RID OF EXCESS WATER VAPOR IF                        
! SS WITH RESPECT TO WATER, AS IN ANTHES AND WARNER                              
! FOLLOWS EXMOISS OF DUDHIA WITH NO MODIFICATION                                 
!---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                 
!
          DO K=kts,kte
            dum11(k)=qvz(k)+DT*qvzTEN(k)                    
            scr4(k)=AMAX1(1.E-12,dum11(k))                                   
            dum21(k)=qlz(k)+DT*qlzTEN(k)                    
            SCR3(k)=AMAX1(0.,dum21(k))                                      
            SCR7(k)=tz(K)+DT*tzTEN(k)                     
!
        enddo
!
                                                                                 
!----COMPUTE THE CONDENSATIONAL TERM:                                            
          DO K=kts,kte
            PRDX(k)=CP_MP*(1.+0.887*qvz(k))                                 
            dum21(k)=dum31(k)*dum31(k)/(RV*PRDX(k))                     
            PRCX(k)=RSLF(prez(k),SCR7(k))                                   
!
        enddo
!
                                                                                 
          DO K=kts,kte
            SCR8(k)=(scr4(k)-PRCX(k))/(1.+dum21(k)*PRCX(k)/(SCR7(k)*SCR7(k)))
            dum11(k)=SCR3(k)+SCR8(k)                                       
            IF(dum11(k).GE.0)THEN                                              
               SCR6(k)=SCR8(k)/DT                                           
            ELSE                                                                
               SCR6(k)=-SCR3(k)/DT                                         
            ENDIF                                                                
            qvzTEN(k)=qvzTEN(k)-SCR6(k)                    
!...INITIATION OF CLOUD WATER:                                                  
            qlzTEN(k)=qlzTEN(k)+SCR6(k)                     
            dum21(k)=dum31(k)/PRDX(k)
            tzTEN(k)=tzTEN(k)+SCR6(k)*dum21(k)*(1-IFDRY)                                        
!
!--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                        
!
            SCR4R(k)=AMAX1(0.,qrz(k) +DT*qrzTEN(k))                                      
            SCR4S(k)=AMAX1(0.,qsz(k) +DT*qszTEN(k))                                          
            SCR4G(k)=AMAX1(0.,qgz(k) +DT*qgzTEN(k))                                      
            SCR4I(k)=AMAX1(0.,qiz(k) +DT*qizTEN(k))                                      
            SCR4N(k)=AMAX1(R1,qniz(k) +DT*qnizTEN(k))                                     
!
           qnl_MX=SCR4I(k)/MI_MN
           qnl_MN=SCR4I(k)/MI_MX
!
           SCR4N(k)=AMAX1(qnl_MN,SCR4N(k))
           SCR4N(k)=AMIN1(qnl_MX,SCR4N(k))
           qnizTEN(k)=(SCR4N(k)-qniz(k))/DT
!
            SCR6(k)=tz(K)+DT*tzTEN(k)                      
!
!
       enddo
                                                                                
!--COMPUTE THE FALLOUT TERMS:   (USE MINIMUM FALL SPEED OF MIXING RATIO=1.E-7)   
!     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                
         NSTEP=1                                                                
         by_pass=0
          DO K=kts,kte
!            UPDT_S = AMAX1(0.,SCR4S(k) )                             
!            UPDT_G = AMAX1(0.,SCR4G(k) )                              
!            UPDT_R = AMAX1(0.,SCR4R(k) )                             
!            UPDT_I = AMAX1(0.,SCR4I(k) )                              
!            UPDT_N = AMAX1(R1,SCR4N(k) )                              
!            UPDT_NR= AMAX1(R1,SCR4NR(k) )
!            UPDT_NS= AMAX1(R1,SCR4NS(k) )
            UPDT_S = SCR4S(k)
            UPDT_G = SCR4G(k)
            UPDT_R = SCR4R(k) 
            UPDT_I = SCR4I(k) 
            UPDT_N = SCR4N(k) 
!
!.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979
           temp_C = amin1(-0.001, SCR6(k)-273.15)
           sonv2(k) = amin1(2.0E8, 2.0E6*exp(-0.12*temp_C))     !greg_test_2
!TEST-GT   sonv2(k) = amin1(1.0E9, 1.0E6*exp(-0.18*temp_C))     !greg_test_1
!
            SONV2(k) = SONV2(k)/SON                                          

            gonv(k) = GON                                                      
            IF (UPDT_G.GT. R1) THEN
               gonv(k) = 2.38*(PI*DGRAUPEL/(rhoz(k)*UPDT_G))**0.92
               gonv(k) = MAX(1.E4, MIN(gonv(k),GON))
            ENDIF
!
            gonv(k) = gonv(k)/GON                                            
                                                                                 
            ronv(k) = RON2                                                     
!
!
            IF(UPDT_R.GT.R1) THEN 
!                 ronv(k) = (4.95E9)*tanh(-(qrz(k)-0.000090)
!     +                      *3.0E5) + 5.05E9
               ronv(k) = ron_const1r*tanh((ron_qr0-updt_r)  &
                     /ron_delqr0) + ron_const2r
            ENDIF                                                               
!
!
            ronv(k) = ronv(k)/RON                                         
                                                                                 
!
            SLOS1=SQRT(SQRT(rhoz(k)*UPDT_S/(TOPS*SONV2(k))))
!
            SLOG1=SQRT(SQRT(rhoz(k)*UPDT_G/(TOPG*gonv(k))))
!
            SLOR1=SQRT(SQRT(rhoz(k)*UPDT_R/(TOPR*ronv(k))))

!
            VT2R=(FRAIN*SLOR1**BR)*RHOFAC(k)                                      
            VT2R=AMIN1(VT2R,9.)
            VT2N=VT2R
!
            VT2S=(FSNOW*SLOS1**BS)*RHOFAC(k)                                      
            VT2S=AMIN1(VT2S, 2.0)
            TEST_S=(UPDT_S-R1)
            TEST_S=.5*(TEST_S+ABS(TEST_S))/(TEST_S+2.*R1)
            VT2S=VT2S*TEST_S
!
            VT2G=(FGRAUPEL*SLOG1**BG)*RHOFAC(k)                                    
            TEST_G=(UPDT_G-R1)
            TEST_G=.5*(TEST_G+ABS(TEST_G))/(TEST_G+2.*R1)
            VT2G=VT2G*TEST_G
!
            DIAMI=AMAX1(DIACE_min, (6.*UPDT_I  &                             
                  /(PI*DICE*(UPDT_N+R1)))**0.3333)                                    
            DIAMI=AMIN1(DIAMI,0.001000)
!
            TEST_I=(UPDT_I-R1)
            TEST_I=.5*(TEST_I+ABS(TEST_I))/(TEST_I+2.*R1)
            TEST_N=(UPDT_N-R1)
            TEST_N=.5*(TEST_N+ABS(TEST_N))/(TEST_N+2.*R1)
            DIAMI=DIAMI*(TEST_N*TEST_I)+(1.-TEST_N*TEST_I)*DIACE_min
            DFICE(K)=DIAMI
!
            VT2I=700. * DIAMI * RHOFAC(k)                          
            VT2I=AMAX1(VT2I, .3)
!
                                                                                 
!
            RGVM=AMAX1(VT2R,VT2N,VT2S,VT2G,VT2I)               
!
! The + 1. IS TO ROUND UP, REPRESENTS NUMBER OF STEPS                                   
!
!
            NSTEP=MAX0(INT(RGVM*DT/dzw(k)+1.),NSTEP)
!
            FR(K)=-VT2R
            FS(K)=-VT2S
            FG(K)=-VT2G
            FI(K)=-VT2I
!
            rkz(k)=1./(rhoz(k)*dzw(k))
!
       enddo
!
!
       if(by_pass.eq.0)then
                                                                                
        DO  N=1,NSTEP                                                           
!
! upstream fluxs
!
        DO K=kts,kte
!
            FALOUTR(K)=FR(K)*rhoz(k)*SCR4R(k)
            FALOUTS(K)=FS(K)*rhoz(k)*SCR4S(k)
            FALOUTG(K)=FG(K)*rhoz(k)*SCR4G(k)
            FALOUTI(K)=FI(K)*rhoz(k)*SCR4I(k)
            FALOUTN(K)=FI(K)*rhoz(k)*SCR4N(k)
        enddo   !k-loop
!
!
!
          DO K=kts,kte-1
!
            FALTNDR=-(FALOUTR(K+1)-FALOUTR(K))*rkz(k)
            FALTNDS=-(FALOUTS(K+1)-FALOUTS(K))*rkz(k)
            FALTNDG=-(FALOUTG(K+1)-FALOUTG(K))*rkz(k)
            FALTNDI=-(FALOUTI(K+1)-FALOUTI(K))*rkz(k)
            FALTNDN=-(FALOUTN(K+1)-FALOUTN(K))*rkz(k)
!
            qrzTEN(k)=qrzTEN(k)+FALTNDR/NSTEP                          
            qszTEN(k)=qszTEN(k)+FALTNDS/NSTEP                          
            qgzTEN(k)=qgzTEN(k)+FALTNDG/NSTEP                          
            qizTEN(k)=qizTEN(k)+FALTNDI/NSTEP                          
            qnizTEN(k)=qnizTEN(k)+FALTNDN/NSTEP                        
!
!       
            SCR4R(k)=AMAX1(0.,SCR4R(k)+FALTNDR*DT/NSTEP)                               
            SCR4S(k)=AMAX1(0.,SCR4S(k)+FALTNDS*DT/NSTEP)                               
            SCR4G(k)=AMAX1(0.,SCR4G(k)+FALTNDG*DT/NSTEP)                               
            SCR4I(k)=AMAX1(0.,SCR4I(k)+FALTNDI*DT/NSTEP)                               
            SCR4N(k)=AMAX1(R1,SCR4N(k)+FALTNDN*DT/NSTEP)                               
           enddo
!
         K=kte
!
           FALTNDR=FALOUTR(K)*rkz(k)
           FALTNDS=FALOUTS(K)*rkz(k)
           FALTNDG=FALOUTG(K)*rkz(k)
           FALTNDI=FALOUTI(K)*rkz(k)
           FALTNDN=FALOUTN(K)*rkz(k)
!
           qrzTEN(k)=qrzTEN(k)+FALTNDR/NSTEP
           qszTEN(k)=qszTEN(k)+FALTNDS/NSTEP
           qgzTEN(k)=qgzTEN(k)+FALTNDG/NSTEP
           qizTEN(k)=qizTEN(k)+FALTNDI/NSTEP
           qnizTEN(k)=qnizTEN(k)+FALTNDN/NSTEP
!
       pptrain=pptrain-FALOUTR(kts)*dt/nstep
       pptsnow=pptsnow-FALOUTS(kts)*dt/nstep
       pptgraul=pptgraul-FALOUTG(kts)*dt/nstep
       pptice=pptice-FALOUTI(kts)*dt/nstep

!
           SCR4R(k)=AMAX1(0.,SCR4R(k)+FALTNDR*DT/NSTEP)
           SCR4S(k)=AMAX1(0.,SCR4S(k)+FALTNDS*DT/NSTEP)
           SCR4G(k)=AMAX1(0.,SCR4G(k)+FALTNDG*DT/NSTEP)
           SCR4I(k)=AMAX1(0.,SCR4I(k)+FALTNDI*DT/NSTEP)
           SCR4N(k)=AMAX1(R1,SCR4N(k)+FALTNDN*DT/NSTEP)
!
!
       ENDDO    ! NSTEP LOOP
!
      ENDIF   ! BY_PASS
                                                                                 
                                                                                 
      IF(.NOT. IIWARM) THEN                                           
!
          DO K=kts,kte
!
! MELTING OF CLOUD ICE                                                          
!
            IF(SCR6(k).GT.TO.AND.(qizTEN(k).GT.0.0 .OR. qiz(k).GT.0.)) THEN                                    
               XLATF=XLS_MP-dum31(k)                                              
               dum11(k)=AMAX1(qiz(k) +DT*qizTEN(k),0.)
               qizTEN(k)=-qiz(k)/DT                           
               qlzTEN(k)=qlzTEN(k)+dum11(k)/DT                       
               qnizTEN(k)=-qniz(k)/DT                         
               tzTEN(k)=tzTEN(k)-XLATF/PRDX(k)   &                      
                   *dum11(k)/DT*(1-IFDRY)                                     
            ENDIF                                                                
! HOMOGENOUS FREEZING OF CLOUD ICE  (R-A.23)                                     
            IF(SCR6(k).LT.HGFR.AND.(qlzTEN(k).GT.0.OR. &
                           qlz(k).GT.0.)) THEN                                       

               dum11(k)=AMAX1(qlz(k) +DT*qlzTEN(k),0.)
               qlzTEN(k)=-qlz(k)/DT                           
               qizTEN(k)=qizTEN(k)+dum11(k)/DT              
               qnizTEN(k)=qnizTEN(k)+(dum11(k)/XM01)/DT
! _TESTC   THE ASSUMED DROPLET MASS COULD BE IMPROVED HER
               tzTEN(k)=tzTEN(k)+XLATF/PRDX(k) &                     
                           *dum11(k)/DT*(1-IFDRY)                                  
            ENDIF                                                               
!
      enddo
!
      ENDIF
!
!
!   number concentration limits.
          DO K=kts,kte
!
!
            SCR4N(k)=AMAX1(R1,qniz(k)    &
                       +DT*qnizTEN(k))
            SCR4I(k)=AMAX1(0.,qiz(k)    &                         
                       +DT*qizTEN(k))                                
!
           qnl_MX=SCR4I(k)/MI_MN
           qnl_MN=SCR4I(k)/MI_MX
!
           SCR4N(k)=AMAX1(qnl_MN,SCR4N(k))
           SCR4N(k)=AMIN1(qnl_MX,SCR4N(k))
           qnizTEN(k)=(SCR4N(k)-qniz(k))/DT
!
      enddo
!
          DO K=kts,kte
!
          tz(k)=tz(k)+tzten(k)*dt
          qvz(k)=AMAX1(r1,qvz(k)+qvzten(k)*dt)
          qlz(k)=AMAX1(0.,qlz(k)+qlzten(k)*dt)
          qrz(k)=AMAX1(0.,qrz(k)+qrzten(k)*dt)
          qiz(k)=AMAX1(0.,qiz(k)+qizten(k)*dt)
          qsz(k)=AMAX1(0.,qsz(k)+qszten(k)*dt)
          qgz(k)=AMAX1(0.,qgz(k)+qgzten(k)*dt)
          qniz(k)=AMAX1(R1,qniz(k)+qnizten(k)*dt)
!
          thz(k)=tz(k)/piiz(k)
!
!
          enddo
                                                                                 
!
  END SUBROUTINE exmoisg
!
     SUBROUTINE thomp_init
  IMPLICIT NONE
   real  :: diar,dias,trk,trk2,trv,trv2,trexp
   real  :: xr0g,gi,consta,gs,gr,gg,lmdr,lmds
   integer :: ktb,itb,jtb
      IIWARM=.FALSE.                                                        
!      IIWARM=.TRUE.
!
      MARAT=0
!
! Seifert WARM RAIN
 alf = 9.65e+00     ! in SI [m/s]
 bet = 1.03e+01     ! in SI [m/s]
 gam = 6.00e+02     ! in SI [1/m]
 n_0 = 1.00e+07     ! in SI [1/m^4]
 eps_axv = 1.00e-20


      k_rsc=5.78
 eps_ax   = 1e-25
 eps_axv  = 1.00e-20
 nu_c  = 0.0      !..width parameter of cloud DSD
 x_minc = 4.20e-15 !..minimum (mean) size of cloud droplets (D=2.e-6m)
 x_maxc = 2.60e-10 !..maximum (mean) size of cloud droplets (D=80e-6m)
 x_maxr = 6.e-7    !..maximum (mean) size of rain droplets (kg)
!
 k_r = 5.78e+0   ! kernel coeff
 k_1 = 5.00e-4   ! phi funct. coeff
!

!..Seifert & Beheng (2001)
k_c  = 9.44e+9   !..Long-Kernel
k_1  = 6.00e+2   !..Parameter for phi function
k_2  = 0.68e+0   !..Parameter for phi function

!cc!!..Seifert & Beheng (2004)
!cc!k_c  = 10.58e+9  !..Kernel coeff
!cc!k_1  = 4.00e+2   !..Parameter for phi function
!cc!k_2  = 0.70e+0   !..Parameter for phi function
!

k_au  = k_c / (2.0d1*x_maxc) * (nu_c+2.0e0)*(nu_c+4.0e0)/(nu_c+1.0e0)**2  !..autoconversion coeff
k_sc  = k_c * (nu_c+2.0e0)/(nu_c+1.0e0)                                !..selfcollection coeff
!
! some needed constants
!
      PI=ACOS(-1.) 
      R=287.04
      RV=461.5
      CP_MP=1004.
      TO=273.15
      XLV0=3.15E6
      XLV1=2370.
      XLF0=.3337E6
      XLS_MP=XLV0-XLV1*273.16+XLF0
      G_MP=9.81
!
!    NEW_SNOWSIZE DISTRIBUTION                                                   
!    NEW GAMMA TYPE SNOWSIZE DISTRIBUTION NEW_SNOW_DIST=1                       
!    OLD EXPONENTIAL TYPE SNOWSIZE DISTRIBUTION NEW_SNOW_DIST=0                
      NEW_SNOW_DIST=0                                                         
       SONV_NEW=   1                                                          
!    snow N0 parameter SONV_NEW=1 temperature dependent                       
!    
     GONV_NEW=0
!    GONV_NEW=0  1 MOMENT GRAUPEL Exponential distribution
!
      RONV_NEW=1                                                            
!    rain NO parameter RONV_NEW=1 Greg's new ronv forumlation
!                                                                          
!
! SLOPE INTERCEPT FOR RAIN, SNOW, AND GRAUPEL                            
      RON=8.E6                                                              
! RON2 IS USED IN REISNER GRAUPEL SCHEME INSTEAD OF RON                      
      RON2=1.E9                                                          
      SON=2.E7                                                            
! old 1mnt graupel      GON=5.E7                                                                
! from Reisners code      GON=4.E6
!
      GON=4.E6 !...change to Lin-value, axel20041014
               !..Greg's upper bound GON=5.E7
!
! EXPONENT FOR RAIN, SNOW, AND GRAUPEL, IN FALL SPEED V(D)=A EXP(B)     
      BR=0.8                                                               
      BS=0.41                                                             
      BG=0.37                                                          
! A IN FALL SPEED                                                     
      ARAIN=842.                                                              
      ASNOW=11.72                                                           
      AGRAUPEL=19.3                                                            
! DENSITY OF RAIN, SNOW, AND GRAUPEL                                            
      DRAIN=1000.                                                             
      DSNOW=100.                                                                
      DGRAUPEL=400.                                                             
      DICE=500.                                                                  
! SMALLEST SIZE OF SNOW AND GRAUPEL (RADIUS, METERS)                            
      XR0S=0.75E-4                                                               
!      XR0G=0.457E-4                                                            
      XR0G=150.E-6
! MINIMUM MASS OF ICE, SNOW, GRAUPEL                                            
      XM01=1.0E-12                                                             
      XM0S=4.*PI/3.*DSNOW*XR0S**3                                              
      XM0G=4.*PI/3.*DGRAUPEL*XR0G**3                                          
! TOP OF SLOPE FOR RAIN, SNOW, AND GRAUPEL                                     
      TOPR=PI*DRAIN*RON                                                       
      TOPS=PI*DSNOW*SON                                                          
      TOPG=PI*DGRAUPEL*GON                                                       
!  CONSTANTS FOR VARIABLE SON                                                      
      CONST1A=2.5E6*(1./1000*1./3600.)**(0.94)                                   
      GI=4.+BS                                                                  
      CONSTA=WGAMMA(GI)                                                        
      CONST1B=ASNOW*CONSTA/6.                                                   
! CONSTANT IN FLETCHER CURVES                                                 
!     TNO=1.E-2                                                                 
!     ATO=0.6                                                                   
! CONSTANT IN COOPER CURVES                                                      
      TNO=5.0                                                                    
      ATO=0.304                                                               
! CONSTANTS FOR BERGERON PROCESS                                              
      INT0=273                                                                  
      BERC1=PI*50.0E-06*50.0E-06                                               
! FREEZING OF CLOUD DROPLETS, MURAKAMI (1989)                                  
      BP=100.                                                                   
      AP=0.66                                                                  
      CNP=100.E6                                                                 
! FREEZING OF RAIN DROPLETS, LIN ET AL (45)                                    
      FRD1=PI*PI*20.*BP*RON                                                     
      FRA1=AP                                                                  
! COLLECTION EFFICIENCIES                                                    
      EFIS=0.1                                                               
      EFIR=1.0                                                                 
      EFSR=1.0                                                                
      EFCS=1.0                                                                  
      EFGI=0.1                                                                 
      EFGC=1.0                                                                 
      EFGR=1.0                                                                 
      EFGS=0.1                                                                  
      EFCR=1.0                                                                  
! COLLECTION OF CLOUD ICE BY SNOW                                               
      GI=3.+BS                                                                 
      GS=WGAMMA(GI)                                                               
      ACRIS=0.25*PI*ASNOW*EFIS*SON*GS                                           
      BACRIS=3+BS                                                              
! COLLECTION OF CLOUD ICE BY RAIN                                                 
      CIR=0.25*PI*EFIR*RON                                                      
! RATE AT WHICH RAIN IS FROZEN BY COLLISION WITH CLOUD ICE                      
      CIRF=1./24.*PI*PI*DRAIN*RON*EFIR                                         
! PARAMETERS FOR MEAN FALL SPEED                                               
      GI=4+BR                                                                  
      GR=WGAMMA(GI)                                                             
      FRAIN=ARAIN*GR/6.                                                         
      GI=4+BS                                                                   
      GS=WGAMMA(GI)                                                               
      FSNOW=ASNOW*GS/6.                                                         
!
      GI=1.+BS
      GS=WGAMMA(GI)
      FNSNOW=ASNOW*GS
!
      GI=1.+BR
      GR=WGAMMA(GI)
      FNRAIN=ARAIN*GR
!
      GI=4+BG                                                                  
      GG=WGAMMA(GI)                                                          
      FGRAUPEL=AGRAUPEL*GG/6.                                             
      GI=1.+BG
      GG=WGAMMA(GI)
      FNGRAUPEL=AGRAUPEL*GG
!
!  COLLECTION OF SNOW BY RAIN                                            
      CSR=PI*PI*EFSR*DRAIN*RON*SON                                        
      ALPHA1=1.2                                                            
      BETA1=0.95                                                           
      GAMMA1=0.08                                                             
!  COLLECTION OF RAIN BY SNOW                                                 
       CRS=PI*PI*EFSR*DSNOW*RON*SON                                          
! AGGREGATION AMOUNG SNOW PARTICLES

      ESS=0.1
      CNIAG=((ASNOW*ESS*1108.)/(4.*720.))

! NUMBER OF COLLISIONS BETWEEN SNOW AND RAIN PARTICLES

      ALPHA2=1.7
      BETA2=0.3
      CNSACR=PI*EFSR*RON*SON/2.
!
      RNGFR=100.*PI*RON
!
! COLLECTION OF CLOUD WATER BY SNOW                                          
      GI=BS+3.                                                              
      IF(NEW_SNOW_DIST.EQ.1)GI=BS+4.
      GS=WGAMMA(GI)                                                          
      ACRCS=0.25*PI*ASNOW*EFCS*SON*GS                                          
      BACRCS=3.+BS                                                          
      IF(NEW_SNOW_DIST.EQ.1)BACRCS=4.+BS                               
! LOSS OF SNOW DUE TO COLLISION WITH CLOUD WATER                            
      GI=2.*BS+2.0                                                            
      GS=WGAMMA(GI)                                                            
      ACRLS=3.0*PI*SON*GS*ASNOW*ASNOW                                        
      BACLS=GI                                                                
! COLLECTION LOSS OF SNOW DUE TO COLLISION WITH CLOUD WATER                   
!     RMC=4.E-12                                                               
!     GI=6.+BS                                                                
!     GS=WGAMMA(GI)                                                            
!     ACRLS=(1./24.)*PI*PI*EFCS*SON*ASNOW*DSNOW*GS                            
!     BACLS=6.+BS                                                               
! COLLECTION OF CLOUD WATER BY GRAUPEL                                          
      GI=3.+BG
!..gamma..      GI=4.+BG
      GG=WGAMMA(GI)                                                           
      ACRCG=0.25*PI*AGRAUPEL*EFGC*GON*GG                            
      BACRCG=3.+BG
!..gamma..      BACRCG=4.+BG
! COLLECTION OF CLOUD ICE BY GRAUPEL                                        
!     GI=3.+BG
!      GI=4.+BG
      GI=3.+BG
      GG=WGAMMA(GI)
      ACRIG=0.25*PI*AGRAUPEL*EFGI*GON*GG                          
      BACRIG=3.+BG                                             
! COLLECTION OF RAIN BY GRAUPEL                              
      CRG=PI*PI*EFGR*DRAIN*RON*GON                                  
! COLLECTION OF SNOW BY GRAUPEL                                    
      CSG=PI*PI*EFGS*DSNOW*SON*GON                                
! DEPOSITIONAL GROWTH OF GRAUPEL                                
      GI=BG/2.+2.5                                           
      GG=WGAMMA(GI)                                                    
      DEPG1=2*PI*GON                                             
      DEPG2=AGRAUPEL                                           
      DEPG3=0.31*GG                                                      
      DEPG4=BG/2.+2.5                                                
! DEPOSITIONAL GROWTH OF SNOW                                     
      GI=BS/2.+2.5                                            
      GS=WGAMMA(GI)                                                       
      DEPS1=4.*SON                                                             
      DEPS2=ASNOW                                                              
      DEPS3=0.44*GS                                                             
      DEPS4=BS/2.+2.5                                                         
! AGGREGATION OF CLOUD ICE                                                      
      C1=700.*0.1*0.25/DICE                                                    
! AUTOCONVERSION TO SNOW                                                       
      XSMAX=9.4E-10                                                           
! COLLECTION OF CLOUD WATER BY RAIN                                            
      GI=3.+BR                                                                 
      GR=WGAMMA(GI)                                                            
      ACRCR=0.25*PI*ARAIN*EFCR*RON*GR                                      
      BACRCR=3.+BR                                                       
! DEPOSITIONAL GROWTH OF RAIN                                                    
      GI=BR/2.+2.5                                                             
      GR=WGAMMA(GI)                                                            
      DEPR1=2*PI*RON                                                           
      DEPR2=ARAIN                                                             
      DEPR3=0.31*GR                                                            
      DEPR4=BR/2.+2.5                                                           
! FOR MELTING OF SNOW                                                           
      GI=BS/2.+2.5                                                              
      GS=WGAMMA(GI)                                                             
      PSM1=2*PI*SON                                                           
      PSM2=ASNOW                                                               
      PSM3=0.44*GS                                                               
      PSM4=BS/2.+2.5                                                            
! FOR MELTING OF GRAUPEL                                                        
      GI=BG/2.+2.5                                                              
      GG=WGAMMA(GI)                                                              
      PGM1=2*PI*GON                                                            
      PGM2=AGRAUPEL                                                            
      PGM3=0.31*GG                                                              
      PGM4=BG/2.+2.5                                                          
! CONSTANT FOR ENHANCED MELTING OF GRAUPEL BY RAIN AND CLOUD WATER                
      CW=4218.                                                                  
! CONSTANT FOR HOMOGENEOUS FREEZING OF CLOUD DROPLETS                            
      HGFR=233.15                                                               
!  setup pracs and psacr tables                                                                                 
!
      rads(1)=0.00001
      rads(nrp)=0.05
      do in=2,nr
      rads(in)=exp(float(in-1)/float(nrp-1)*alog(rads(nrp)/rads(1))+alog(rads(1)))
      enddo
!
      do in=1,nr
      dr(in)=rads(in+1)-rads(in)
      rad(in)=sqrt(rads(in)*rads(in+1))
      enddo
!
      sads(1)=.00001
      sads(nsp)=.05
      do is=2,nsp
      sads(is)=exp(float(is-1)/float(nsp-1)*alog(sads(nsp)/sads(1))+alog(sads(1)))
      enddo
!
      do is=1,ns
      ds(is)=sads(is+1)-sads(is)
      sad(is)=sqrt(sads(is)*sads(is+1))
      enddo
!
!    table inverse slope parameter limits (m)
      t_slor(1)=4.e-5
      t_slor(nrtb)=1.e-3
      t_slos(1)=4.e-5
      t_slos(nstb)=2.e-3

      do ktb=2,nrtb-1
      t_slor(ktb)=(float(ktb-1)/float(nstb-1)*(t_slor(nrtb)-t_slor(1))+t_slor(1))
      enddo
      do ktb=2,nstb-1
      t_slos(ktb)=(float(ktb-1)/float(nstb-1)*(t_slos(nstb)-t_slos(1))+t_slos(1))
      enddo
      do itb=1,nrtb
      lmdr=t_slor(itb)
      do jtb=1,nstb
      lmds=t_slos(jtb)
      trv=0.
      trv2=0.
      do is=1,ns
      do in=1,nr
      diar=rad(in)
      dias=sad(is)
      trk=ker_s_acr(diar,dias)
      trk2=ker_r_acs(diar,dias)
      trexp=exp(-sad(is)/lmds-rad(in)/lmdr)
      trv=trv + trk*trexp*ds(is)*dr(in)
      trv2=trv2 + trk2*trexp*ds(is)*dr(in)
      enddo
      enddo
!
      t_psacr(itb,jtb)=trv
      t_pracs(itb,jtb)=trv2
!
      enddo
      enddo
!
   END SUBROUTINE thomp_init
!
      real function ker_s_acr(diar,dias)
!----------------------------------------------------------------
   IMPLICIT NONE
   real  :: diar,dias
!----------------------------------------------------------------
      real  :: delta_v
! EXPONENT FOR RAIN, SNOW, AND GRAUPEL, IN FALL SPEED V(D)=A EXP(B)
      BR=0.8
      BS=0.41
! A IN FALL SPEED
      ARAIN=842.
      ASNOW=11.72
      DRAIN=1000.
      EFSR=1.
      DSNOW=100.
      delta_v=ASNOW*DIAS**BS-ARAIN*DIAR**BR
      KER_S_ACR=PI*(diar+dias)**2*EFSR*.5*(delta_v+abs(delta_v))*PI/6.*DIAR**3*DRAIN
      return
      end function ker_s_acr
!
      real function ker_r_acs(diar,dias)
!----------------------------------------------------------------
   IMPLICIT NONE
   real  :: diar,dias
      real  :: delta_v
!----------------------------------------------------------------
! EXPONENT FOR RAIN, SNOW, AND GRAUPEL, IN FALL SPEED V(D)=A EXP(B)
      BR=0.8
      BS=0.41
! A IN FALL SPEED
      ARAIN=842.
      ASNOW=11.72
      DRAIN=1000.
      EFSR=1.
      DSNOW=100.
      delta_v=ARAIN*DIAR**BR-ASNOW*DIAS**BS
      KER_R_ACS=PI*(diar+dias)**2*EFSR*(delta_v+abs(delta_v))*PI/6.*DIAS**3*DSNOW
      return
      end function ker_r_acs
!----------------------------------------------------------------
   REAL FUNCTION rslf(P,T)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

      REAL, INTENT (IN   )   :: P, T
      REAL     :: ESL,X
      REAL ,         PARAMETER :: C0= .611583699E03                             
      REAL ,         PARAMETER :: C1= .444606896E02                            
      REAL ,         PARAMETER :: C2= .143177157E01                        
      REAL ,         PARAMETER :: C3= .264224321E-1                     
      REAL ,         PARAMETER :: C4= .299291081E-3                   
      REAL ,         PARAMETER :: C5= .203154182E-5             
      REAL ,         PARAMETER :: C6= .702620698E-8                                          
      REAL ,         PARAMETER :: C7= .379534310E-11                                      
      REAL ,         PARAMETER :: C8=-.321582393E-13                                
                                                                               
      X=AMAX1(-80.,T-273.16)                                                  
                                                                        
!     ESL=612.2*EXP(17.67*X/(T-29.65))                                        
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))         
      RSLF=.622*ESL/(P-ESL)                                             
                                                                    
   end  FUNCTION rslf
!
! ---------------------------------------------------------------------       
! THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A       
! FUNCTION OF TEMPERATURE AND PRESSURE                                    
!                                                                       
      REAL FUNCTION RSIF(P,T)                                             
!                                                                   
      IMPLICIT NONE                                             
 REAL , INTENT(IN   )      :: P, T
      REAL     :: ESI,X
 REAL ,       PARAMETER :: C0= .609868993E03                
 REAL ,       PARAMETER :: C1= .499320233E02                
 REAL ,       PARAMETER :: C2= .184672631E01               
 REAL ,       PARAMETER :: C3= .402737184E-1              
 REAL ,       PARAMETER :: C4= .565392987E-3                                           
 REAL ,       PARAMETER :: C5= .521693933E-5                                         
 REAL ,       PARAMETER :: C6= .307839583E-7                                       
 REAL ,       PARAMETER :: C7= .105785160E-9                                     
 REAL ,       PARAMETER :: C8= .161444444E-12                                   
!                                                                           
      X=AMAX1(-80.,T-273.16)                                              
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))        
      RSIF=.622*ESI/(P-ESI)                                           
!                                                                  
     end FUNCTION RSIF
!----------------------------------------------------------------
!----------------------------------------------------------------
   REAL FUNCTION wgamma(X)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
      REAL, INTENT(IN   ) :: x
      REAL, DIMENSION(8)  :: P,Q
      REAL, DIMENSION(7)  :: C
      INTEGER             :: I,N
      LOGICAL :: PARITY
      REAL                :: &
         EPS,FACT,HALF,ONE,PI,RES,SQRTPI,SUM,TWELVE,    &
         TWO,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/, &
          SQRTPI/0.9189385332046727417803297E0/,  &
          PI/3.1415926535897932384626434E0/
!----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,  &
          XINF/3.4E38/
!---------------------------------------------------------------------
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
            -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
            8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
            -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
           -1.01515636749021914166146E+3,-3.10777167157231109440444E+3,  &
             2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
           -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
!----------------------------------------------------------------------
      DATA C/-1.910444077728E-03,8.4171387781295E-04,  &
          -5.952379913043012E-04,7.93650793500350248E-04,  &
          -2.777777777777681622553E-03,8.333333333333333331554247E-02,  &
           5.7083835261E-03/
      PARITY=.FALSE.
      FACT=ONE   
      N=0        
      Y=X        
      IF(Y.LE.ZERO)THEN
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
        Y=-X     
        Y1=AINT(Y)
        RES=Y-Y1 
        IF(RES.NE.ZERO)THEN
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.
          FACT=-PI/SIN(PI*RES)
          Y=Y+ONE
        ELSE     
          RES=XINF
      WGAMMA=RES
      RETURN
        ENDIF    
      ENDIF      
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
      IF(Y.LT.EPS)THEN
!----------------------------------------------------------------------
!  ARGUMENT .LT. EPS
!----------------------------------------------------------------------
        IF(Y.GE.XMININ)THEN
          RES=ONE/Y
        ELSE     
          RES=XINF
      WGAMMA=RES
      RETURN
        ENDIF    
      ELSEIF(Y.LT.TWELVE)THEN
        Y1=Y     
        IF(Y.LT.ONE)THEN
!----------------------------------------------------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          Z=Y    
          Y=Y+ONE
        ELSE     
!----------------------------------------------------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
!----------------------------------------------------------------------
          N=INT(Y)-1
          Y=Y-float(N)
          Z=Y-ONE
        ENDIF    
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
!----------------------------------------------------------------------
        XNUM=ZERO
        XDEN=ONE 
        DO  I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
        ENDDO
        RES=XNUM/XDEN+ONE
        IF(Y1.LT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
!---------------------------------------------------------------------
          RES=RES/Y1
        ELSEIF(Y1.GT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
!----------------------------------------------------------------------
          DO  I=1,N
            RES=RES*Y
            Y=Y+ONE
          ENDDO
        ENDIF
      ELSE
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,
!----------------------------------------------------------------------
        IF(Y.LE.XBIG)THEN
          YSQ=Y*Y
          SUM=C(7)
          DO  I=1,6
            SUM=SUM/YSQ+C(I)
          ENDDO
          SUM=SUM/Y-Y+SQRTPI
          SUM=SUM+(Y-HALF)*LOG(Y)
          RES=EXP(SUM)
        ELSE
          RES=XINF
   WGAMMA=RES
      RETURN
        ENDIF
      ENDIF
!----------------------------------------------------------------------
!  FINAL ADJUSTMENTS AND RETURN
!----------------------------------------------------------------------
      IF(PARITY)RES=-RES
      IF(FACT.NE.ONE)RES=FACT/RES
      WGAMMA=RES
      RETURN
! ---------- LAST LINE OF GAMMA ----------

      END FUNCTION wgamma

!----------------------------------------------------------------
!----------------------------------------------------------------
!----------------------------------------------------------------
!----------------------------------------------------------------
   REAL FUNCTION betaf(a,b)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
      REAL, INTENT(IN   ) :: a,b
      REAL                ::t1,t2,t3,c

!
       T1=WGAMMA(A)
       T2=WGAMMA(B)
       C=A+B
       T3=WGAMMA(C)
       BETAF=T1*T2/T3

       end FUNCTION  betaf
!----------------------------------------------------------------

END MODULE module_mp_thompson
