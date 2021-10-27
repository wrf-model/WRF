!
module module_fr_fire_phys

use module_model_constants, only: cp,xlv
use module_fr_fire_util

PRIVATE

! subroutines and functions 
PUBLIC:: init_fuel_cats,fire_ros,heat_fluxes,set_nfuel_cat,set_fire_params,write_fuels_m
PUBLIC:: fuel_moisture,advance_moisture,read_namelist_fire

! variables
PUBLIC:: moisture_classes
PUBLIC::fuelmc_g

PUBLIC::fire_params

! arrays passed to fire_ros
type fire_params
real,pointer,dimension(:,:):: vx,vy                ! wind velocity (m/s)
real,pointer,dimension(:,:):: zsf                  ! terrain height (m) 
real,pointer,dimension(:,:):: dzdxf,dzdyf          ! terrain grad (1) 
real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0 ! spread formula coefficients
real,pointer,dimension(:,:):: fgip                 ! init mass of surface fuel (kg/m^2)
real,pointer,dimension(:,:):: ischap               ! if fuel is chaparral and want rate of spread treated differently
real,pointer,dimension(:,:):: iboros               ! fire instensity over rate of spread -> avialable fuel x heat of combustion
real,pointer,dimension(:,:):: fuel_time            ! time to burn to 1/e (s) 
real,pointer,dimension(:,:):: fmc_g                ! fuel moisture contents, ground (1) 
end type fire_params

! use as
! type(fire_params)::fp

!***************************************************************************************!
!***                                  Fuel moisture model                            ***!
!***************************************************************************************!
!*** subroutine advance_moisure: a step of time-delay differential equation for each ***!
!***    fuel class (10h, 100h, 1000h) on Earth surface atmospheric mesh, towards     ***!
!***    fuel moisture content equilibrium from the atmosphere state on the surface   ***!
!*** subroutine fuel_moisture: interpolate to fire mesh and average the fuel         ***!
!***    moisture contents from the fuel classes to the fuel present in the fire mesh ***!
!***    cell with weights given in fuel description                                  ***!
!*** See https://www.openwfm.org/wiki/Fuel_moisture_model for further details        ***!
!***    including additional namelist.input and namelist.fire variables.             ***!
!*** Developed 09/2011-09/2012 and merged from https://github.com/openwfm/wrf-fire   ***!
!***    with common ancestor as submitted to V3.3 in 01/2017 commit 3f78267b38e0f0a3 ***!
!*** Reference: J. Mandel, S. Amram, J.D. Beezley, G. Kelman, A.K. Kochanski, V.Y.   ***!
!***    Kondratenko, B.H. Lynn, B. Regev, M. Vejmelka, Recent advances and           ***!
!***    applications of WRF-SFIRE. Natural Hazards and Earth System Science, 14,     ***!
!***    2829-2845, 2014, doi:10.5194/nhess-14-2829-2014                              ***!
!***************************************************************************************!

!! To add moisture classes:
!  1. change parameter max_moisture_classes below
!  2. change the default of nfmc to the same value in Registry/registry.fire
!  3. add the appropriate lines real::fmc_gw<number>= <default values>
!  4. add default 

!*** dimensions
  INTEGER, PARAMETER :: mfuelcats = 30     ! allowable number of fuel categories 
  INTEGER, PARAMETER ::max_moisture_classes=5
!***

  integer, save:: moisture_classes=5
  real, dimension(max_moisture_classes), save:: drying_lag,wetting_lag,saturation_moisture,saturation_rain, &
         rain_threshold,rec_drying_lag_sec,rec_wetting_lag_sec,fmc_gc_initial_value
  integer, dimension(max_moisture_classes), save:: drying_model,wetting_model,fmc_gc_initialization
   ! relative weights of moisture class for each fuel category
   integer::itmp
   CHARACTER (len=80), DIMENSION(max_moisture_classes), save :: moisture_class_name
  REAL, save:: fmc_1h, fmc_10h, fmc_100h, fmc_1000h, fmc_live
    
  data moisture_class_name /'1-h','10-h','100-h','1000-h','Live'/
  data drying_lag          /1., 10., 100., 1000.,1e9/  ! time lag (h) approaching equilibrium moisture 
  data wetting_lag         /1.4,14., 140., 1400.,1e9/  ! time lag (h) for approaching saturation in rain
  data saturation_moisture /2.5, 2.5, 2.5 ,2.5, 2.5/  ! saturation moisture contents (1) in rain
  data saturation_rain     /8.0, 8.0, 8.0, 8.0, 8.0/  ! stronger rain matters only in duration (mm/h) 
  data rain_threshold      /0.05,0.05,0.05,0.05,0.05/ ! rain intensity this small is same as nothing
  data drying_model        /1,   1,   1,   1,   1 / ! future proofing 
  data wetting_model       /1,   1,   1,   1,   1 / ! future proofing
  data fmc_gc_initialization/2,  2,   2,   2,   3 / ! initialization 0=input, 1=from fuelmc_g, 
                                                    ! 2=from equilibrium, 3=from fmc_1h,...,fmc_live
  data fmc_gc_initial_value/0.,  0.,  0.,  0.,  0./ ! initial value used in the model, to be the actual value fmc_1h,...,fmc_live or read in
  data fmc_1h /0.08/, fmc_10h/0.08/, fmc_100h/0.08/, fmc_1000h/0.08/, fmc_live/0.3/
  ! live fuel moisture following https://www.nwcg.gov/publications/pms437/fuel-moisture/live-fuel-moisture-content
  ! from Rothermel, 1983 Table II-2 p.13 https://www.fs.usda.gov/treesearch/pubs/24635
  ! 300% Fresh foliage, annuals developing early in the growing cycle
  ! 200% Maturing foliage, still developing, with full turgor
  ! 100% Mature foliage, new growth complete and comparable to older perennial foliage
  ! 50%  Entering dormancy, coloration starting, some leaves may have dropped from stem
  ! 30%  Completely cured, treat as dead fuel

! =========================================================================

!D in col 2 means quantity derived from the others
!
!  Scalar constants (data same for all fuel categories):
!       HFGL           SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
!       CMBCNST        JOULES PER KG OF DRY FUEL
!       FUELHEAT       FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
!       FUELMC_G       FUEL PARTICLE (SURFACE) MOISTURE CONTENT
!D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
!                        % of total fuel mass that is water (not quite
!                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
!                        so BMST = FUELMC_G / (1 + FUELMC_G)  where
!                        FUELMC_G = moisture content of surface fuel
!
!  Data arrays indexed by fuel category:
!       FGI            INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
!       FUELDEPTHM     FUEL DEPTH, IN M  (CONVERTED TO FT)              
!       SAVR           FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
!                         GRASS: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
!       FUELMCE        MOISTURE CONTENT OF EXTINCTION; 0.30 FOR MANY DEAD FUELS; 0.15 FOR GRASS
!       FUELDENS       OVENDRY PARTICLE DENSITY, LB/FT^3
!       ST             FUEL PARTICLE TOTAL MINERAL CONTENT
!       SE             FUEL PARTICLE EFFECTIVE MINERAL CONTENT
!       WEIGHT         WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
!                        RANGES FROM ~5 (FAST BURNUP) TO 1000 ( ~40% DECR OVER 10 MIN).
!       FCI_D          INITIAL DRY MASS OF CANOPY FUEL
!       FCT            BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
!       ichap          Set=1 if fuel is chaparral and want the rate of spread treated differently, 0 if not
!D      FCI            INITIAL TOTAL MASS OF CANOPY FUEL
!D      FCBR           FUEL CANOPY BURN RATE (KG/M**2/S) 

! =============================================================================
! Anderson 13 surface fire fuel models, along with some
!          estimated canopy properties (for crown fire).
! =============================================================================
!  --- Grass-dominated fuel models
!  FUEL MODEL 1: Short grass (1 ft)
!  FUEL MODEL 2: Timber (grass and understory)
!  FUEL MODEL 3: Tall grass (2.5 ft)
!  --- Shrub-dominated fuel models
!  FUEL MODEL 4: Chaparral (6 ft)
!  FUEL MODEL 5: Brush (2 ft) 
!  FUEL MODEL 6: Dormant brush, hardwood slash
!  FUEL MODEL 7: Southern rough
!  --- Forest litter-dominated fuel models
!  FUEL MODEL 8: Closed timber litter
!  FUEL MODEL 9: Hardwood litter
!  FUEL MODEL 10: Timber (litter + understory)
!  --- Logging debris-dominated fuel models
!  FUEL MODEL 11: Light logging slash
!  FUEL MODEL 12: Medium logging slash
!  FUEL MODEL 13: Heavy logging slash
!  --- Fuel-free 
!  FUEL MODEL 14: no fuel

! scalar fuel coefficients
   REAL, SAVE:: cmbcnst,hfgl,fuelmc_g,fuelmc_c
! computed values
   REAL, SAVE:: fuelheat

! defaults, may be changed in init_fuel_cats
   DATA cmbcnst  / 17.433e+06/             ! J/kg dry fuel
   DATA hfgl     / 17.e4 /                ! W/m^2
   DATA fuelmc_g / 0.08  /                ! set = 0 for dry surface fuel
   DATA fuelmc_c / 1.00  /                ! set = 0 for dry canopy
!  REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
!  REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
!  real, parameter :: xlv      = 2.5e6                  ! to make it selfcontained
!  real, parameter :: cp      =  7.*287./2              ! to make it selfcontained


! fuel categorytables
   INTEGER, PARAMETER :: nf=14              ! number of fuel categories in data stmts
   INTEGER, SAVE      :: nfuelcats = 13     ! number of fuel categories that are specified
   INTEGER, PARAMETER :: zf = mfuelcats-nf  ! number of zero fillers in data stmt 
   INTEGER, SAVE      :: no_fuel_cat = 14   ! special category outside of 1:nfuelcats
   CHARACTER (len=80), DIMENSION(mfuelcats ), save :: fuel_name
   INTEGER, DIMENSION( mfuelcats ), save :: ichap
   REAL   , DIMENSION( mfuelcats ), save :: windrf,weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se, &
                                            fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live, &
                                            fgi_t,fmc_gwt
   REAL,   DIMENSION(mfuelcats,max_moisture_classes), save :: fgi_c, fmc_gw ! fuel moisture class weights
   DATA fuel_name /'1: Short grass (1 ft)', &
     '2: Timber (grass and understory)', &
     '3: Tall grass (2.5 ft)', &
     '4: Chaparral (6 ft)', &
     '5: Brush (2 ft) ', &
     '6: Dormant brush, hardwood slash', &
     '7: Southern rough', &
     '8: Closed timber litter', &
     '9: Hardwood litter', &
     '10: Timber (litter + understory)', &
     '11: Light logging slash', &
     '12: Medium logging slash', &
     '13: Heavy logging slash', &
     '14: no fuel', zf* ' '/

   DATA windrf /0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44, &     
                0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1e-7, zf*0 /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305,zf*0. /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500., zf*0. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 , zf*0. / 
   DATA fueldens / nf * 32., zf*0. /   ! 32 if solid, 19 if rotten.
   DATA st / nf* 0.0555 , zf*0./
   DATA se / nf* 0.010 , zf*0./
! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
!              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
!              (11)-(13) could be 300-1600
   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. , zf*0./ 
! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0., zf*0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60. , zf*0.   /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 , zf*0/
!  DATA fmc_gw05 / 0.000, 0.023, 0.000, 0.230, 0.092, 0.000, 0.017, 0.000, 0.000, 0.092, 0.000, 0.000, 0.000, zf*0/

! fuel loading 1-h, 10-h, 100-h, 1000-h, live following Albini 1976 as reprinted in Anderson 1982 Table 1 (for proportions only)
!                     1      2      3      4      5      6      7      8      9     10     11     12     13     14
  DATA fgi_1h    / 0.74,   2.00,  3.01,  5.01,  1.00,  1.50,  1.13,  1.50,  2.92,  3.01,  1.50,  4.01,  7.01,   0.0,   zf*0./
  DATA fgi_10h   / 0.00,   1.00,  0.00,  4.01,  0.50,  2.50,  1.87,  1.00,  0.41,  2.00,  4.51, 14.03, 23.04,   0.0,   zf*0./
  DATA fgi_100h  / 0.00,   0.50,  0.00,  2.00,  0.00,  2.00,  1.50,  2.50,  0.15,  5.01,  5.51, 16.53, 28.05,   0.0,   zf*0./
  DATA fgi_1000h / 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,    0.0,   zf*0./
  DATA fgi_live  / 0.00,   0.50,  0.000, 5.01,  2.00,  0.00,  0.37,  0.00,  0.00,  2.00,  0.00,  2.3,   0.00,   0.0,   zf*0./

! total fuel loading kg/m^2
  DATA fgi       / 0.166,  0.896, 0.674, 3.591, 0.784, 1.344, 1.091, 1.120, 0.780, 2.692, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /

! =========================================================================
contains

subroutine fuel_moisture(                &
    id,                                  & ! for prints and maybe file names
    nfmc,                                &
    ids,ide, jds,jde,                    & ! atm grid dimensions
    ims,ime, jms,jme,                    &
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & ! fire grid dimensions
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    ir,jr,                               & ! atm/fire grid ratio
    nfuel_cat,                           & ! fuel data
    fmc_gc,                              & ! moisture contents by class on atmospheric grid
    fmc_g                                & ! weighted fuel moisture contents on fire grid
    )

implicit none

!**** arguments
integer, intent(in)::                    &
    id,nfmc,                             &
    ids,ide, jds,jde,                    & ! atm grid dimensions
    ims,ime, jms,jme,                    &
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & ! fire grid dimensions
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    ir,jr                                  ! atm/fire grid ratio


real,intent(in),dimension(ifms:ifme,jfms:jfme):: nfuel_cat ! fuel data
real,intent(inout),dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real,intent(out),dimension(ifms:ifme,jfms:jfme):: fmc_g ! fuel data

!**** local
real, dimension(its-1:ite+1,jts-1:jte+1):: fmc_k  ! copy of fmc_gc(:,k,:) 
real, dimension(ifts-1:ifte+1,jfts-1:jfte+1):: fmc_f      ! interpolation of fmc_gc(:,k,:) to the fire grid 
integer::i,j,k,n
integer::ibs,ibe,jbs,jbe
character(len=128)::msg

call check_mesh_2dim(ifts,ifte,jfts,jfte,ifds,ifde,jfds,jfde) ! check if fire tile fits into domain
call check_mesh_2dim(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme) ! check if fire tile fits into memory

do j=jfts,jfte
    do i=ifts,ifte
        fmc_g(i,j)=0.               ! initialize sum over classes
    enddo
enddo

! one beyond the tile but not beyond the domain boundary
ibs=max(ids,its-1)
ibe=min(ide,ite+1)
jbs=max(jds,jts-1)
jbe=min(jde,jte+1)

call check_mesh_2dim(ibs,ibe,jbs,jbe,ims,ime,jms,jme) ! check if tile with halo fits into memory

do k=1,moisture_classes

    ! copy halo beyond the tile but not beyond the domain boundary
    do j=jbs,jbe
        do i=ibs,ibe
            fmc_k(i,j)=fmc_gc(i,k,j)      ! copy slice to 2d array
        enddo
    enddo

    call print_2d_stats(ibs,ibe,jbs,jbe,its-1,ite+1,jts-1,jte+1,fmc_k,'fuel_moisture: fmc_k')

    ! interpolate moisture contents in the class k to the fire mesh
    call interpolate_z2fire(id,        & ! for debug output, <= 0 no output
    ids,ide,jds,jde,                     & ! atm grid dimensions
    its-1,ite+1,jts-1,jte+1,             & ! memory dimensions of fmc_k
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & ! fire grid dimensions
    ifts-1,ifte+1,jfts-1,jfte+1,         & ! memory dimensions of fmc_f
    ifts,ifte,  jfts,jfte,               &
    ir,jr,                               & ! atm/fire grid ratio
    fmc_k,                               & ! atm grid arrays in
    fmc_f,0)                                 ! fire grid arrays out

    call print_2d_stats(ifts,ifte,jfts,jfte,ifts-1,ifte+1,jfts-1,jfte+1,fmc_f,'fuel_moisture: fmc_f')

    ! add moisture contents for class k to the fuel moisture
    do j=jfts,jfte
        do i=ifts,ifte
            n = nfuel_cat(i,j)
            if(n > 0)then
                fmc_g(i,j)=fmc_g(i,j)+fmc_gw(n,k)*fmc_f(i,j)      ! add to sum over classes
            endif
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fmc_g,'fuel_moisture: fmc_g')

enddo


end subroutine fuel_moisture

subroutine advance_moisture(    &
    initialize,                 & ! initialize timestepping. true on the first call at time 0, then false
    ims,ime,  jms,jme,          & ! memory dimensions
    its,ite,  jts,jte,          & ! tile dimensions
    nfmc,                       & ! dimension of moisture fields
    moisture_dt,                & ! timestep = time step time elapsed from the last call
    fmep_decay_tlag,            & ! moisture extended model assimilated diffs. decay time lag
    rainc, rainnc,              & ! accumulated rain 
    t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    rain_old,                   & ! previous value of accumulated rain
    t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
    rh_fire,                    & ! relative humidity at surface, for diagnostic only
    fmc_gc,                     & ! fuel moisture by class, updated
    fmep,                       & ! fuel moisture extended model parameters
    fmc_equi,                   & ! fuel moisture equilibrium by class, for diagnostics only
    fmc_lag                     & ! fuel moisture tendency by classe, for diagnostics only
    )

implicit none

!*** arguments
logical, intent(in):: initialize
integer, intent(in)::           &
    ims,ime,  jms,jme,          & ! memory dimensions
    its,ite,  jts,jte,          & ! tile dimensions
    nfmc                          ! number of moisture fields
real, intent(in):: moisture_dt, fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi, fmc_lag
real, intent(out), dimension(ims:ime,jms:jme)::rh_fire 

!*** global
! fuel properties moisture set by init_fuel_cats

!*** local
integer:: i,j,k
real::rain_int, T, P, Q, QRS, ES, RH, tend, EMC_d, EMC_w, EMC, R, rain_diff, fmc, rlag, equi, &
    d, w, rhmax, rhmin, change, rainmax,rainmin, fmc_old, H, deltaS, deltaE
real, parameter::tol=1e-2 ! relative change larger than that will switch to exponential ode solver 
character(len=256)::msg
integer::msglevel=2
logical, parameter::check_data=.true.,check_rh=.false.
real::epsilon,Pws,Pw

!*** executable

! check arguments

if(msglevel>1)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,f10.2,a,i4,a,i4)')'advance moisture dt=',moisture_dt,'s using ',moisture_classes,' classes from possible ',nfmc
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg)
endif

if(moisture_classes > nfmc .or. moisture_classes > max_moisture_classes)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,*)'advance_moisture: moisture_classes=',moisture_classes, &
       ' > nfmc=',nfmc,' or >  max_moisture_classes=',max_moisture_classes
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call crash(msg)
endif

call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,t2,'T2')
call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,q2,'Q2')
call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,psfc,'PSFC')

if(initialize) then 
    call message('advance_moisture: initializing, copying surface variables to old')
    call copy2old
else
    call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_gc,'before advance fmc_gc')
endif

if(check_data)then
    do j=jts,jte
        do i=its,ite
            if( .not.(t2(i,j)>0.0 .and. psfc(i,j)>0.0 .and. .not. q2(i,j) < 0.0 ))then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                 write(msg,'(a,2i4,a,3e12.2)')'At i j',i,j,' t2 psfc q2 are ',t2(i,j),psfc(i,j),q2(i,j)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                 call message(msg) 
                 call crash('invalid data passed from WRF, must have t2 psfc>0, q2 >= 0')
            endif
        enddo
    enddo
endif

! one time step

rhmax=-huge(rhmax)
rhmin=huge(rhmin)
rainmax=-huge(rainmax)
rainmin= huge(rainmin)
do j=jts,jte
    do k=1,moisture_classes
        do i=its,ite
            ! old fuel moisture contents
            ! compute the rain intensity from the difference of accumulated rain
            rain_diff = ((rainc(i,j) + rainnc(i,j)) - rain_old(i,j))
            if(moisture_dt > 0.)then
                rain_int  = 3600. * rain_diff / moisture_dt 
            else
                rain_int  = 0.
            endif
            rainmax = max(rainmax,rain_int)
            rainmin = min(rainmin,rain_int)
            R = rain_int - rain_threshold(k)

            ! average the inputs for second order accuracy
            T = 0.5 * (t2_old(i,j) + t2(i,j))
            P = 0.5 * (psfc_old(i,j) + psfc(i,j))
            Q = 0.5 * (q2_old(i,j) + q2(i,j))

            ! compute the relative humidity
            ! ES=610.78*exp(17.269*(T-273.161)/(T-35.861))
            ! QRS=0.622*ES/(P-0.378*ES)
            ! RH = Q/QRS
            ! function rh_from_q from Adam Kochanski following Murphy and Koop, Q.J.R. Meteorol. Soc (2005) 131 1539-1565 eq. (10)
            epsilon = 0.622 ! Molecular weight of water (18.02 g/mol) to molecular weight of dry air (28.97 g/mol)
            ! vapor pressure [Pa]
            Pw=q*P/(epsilon+(1-epsilon)*q); 
            ! saturation vapor pressure [Pa]
            Pws= exp( 54.842763 - 6763.22/T - 4.210 * log(T) + 0.000367*T + &
                tanh(0.0415*(T - 218.8)) * (53.878 - 1331.22/T - 9.44523 * log(T) + 0.014025*T))
            !realtive humidity [1]
            RH = Pw/Pws
            rh_fire(i,j)=RH
            rhmax=max(RH,rhmax)         
            rhmin=min(RH,rhmin)         

            deltaE = fmep(i,1,j)
            deltaS = fmep(i,2,j)

            if(.not.check_rh)then
                RH = min(RH,1.0)
            else
                if(RH < 0.0 .or. RH > 1.0 .or. RH .ne. RH )then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                    write(msg,'(a,2i6,5(a,f10.2))')'At i,j ',i,j,' RH=',RH, &
                        ' from T=',T,' P=',P,' Q=',Q
                    call message(msg) 
                    call crash('Relative humidity must be between 0 and 1, saturated water contents must be >0')
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif
            endif 
            !print *,'ADV_MOIST i=',i,' j=',j,' T=',T,' P=',P,' Q=',Q,' ES=',ES,' QRS=',QRS,' RH=',RH

            if (R > 0.) then
                select case(wetting_model(k))
                case(1) ! saturation_moisture=2.5 wetting_lag=14h saturation_rain=8 mm/h calibrated to VanWagner&Pickett 1985 per 24 hours
                    EMC_w=saturation_moisture(k) + deltaS
                    EMC_d=saturation_moisture(k) + deltaS
                    rlag=rec_wetting_lag_sec(k) * (1. - exp(-R/saturation_rain(k)))
                end select
            else ! not raining
                select case(drying_model(k))
                case(1) ! Van Wagner and Pickett (1972) per Viney (1991) eq (7) (8)
                    H = RH * 100.
                    d=0.942*H**0.679 + 0.000499*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) ! equilibrium moisture for drying
                    w=0.618*H**0.753 + 0.000454*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) ! equilibrium moisture for adsorbtion
                    if(d.ne.d.or.w.ne.w)call crash('equilibrium moisture calculation failed, result is NaN')
                    d = d*0.01
                    w = w*0.01
                    EMC_d = max(max(d,w)+deltaE,0.0)
                    EMC_w = max(min(d,w)+deltaE,0.0)
                    rlag=rec_drying_lag_sec(k)
                end select
            endif
            !*** MODELS THAT ARE NOT OF THE EXPONENTIAL TIME LAG KIND 
            ! ARE RESPONSIBLE FOR THEIR OWN LOGIC, THESE MODELS
            ! SHOULD COMPUTE fmc_gc(i,k,j) DIRECTLY AND SET TLAG < 0
            !
            if(rlag > 0.0)then

                if(.not.initialize .or. fmc_gc_initialization(k).eq.0)then ! take old from before, no initialization
                    fmc_old = fmc_gc(i,k,j)
                elseif(fmc_gc_initialization(k).eq.1)then ! from scalar fuelmc_g
                    fmc_old = fuelmc_g
                elseif(fmc_gc_initialization(k).eq.2)then ! from computed equilibrium
                    fmc_old=0.5*(EMC_d+EMC_w)
                elseif(fmc_gc_initialization(k).eq.3)then ! from scalar parameter
                    fmc_old = fmc_gc_initial_value(k)
                else
                    call crash('bad value of fmc_gc_initialization(k), must be between 0 and 2')
                endif
                equi = max(min(fmc_old, EMC_d),EMC_w) ! take lower or upper equilibrium value 

                change = moisture_dt * rlag 

                if(change  < tol)then
                     if(fire_print_msg.ge.3)call message('midpoint method')
                     fmc = fmc_old + (equi - fmc_old)*change*(1.0 - 0.5*change)  ! 2nd order Taylor
                else
                     if(fire_print_msg.ge.3)call message('exponential method')
                     fmc = fmc_old + (equi - fmc_old)*(1 - exp(-change))
                endif
                fmc_gc(i,k,j) = fmc

                ! diagnostics out
                fmc_equi(i,k,j)=equi
                fmc_lag(i,k,j)=1.0/(3600.0*rlag)
                 
                ! diagnostic prints
                if(fire_print_msg.ge.3)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
		    write(msg,*)'i=',i,' j=',j,'EMC_w=',EMC_w,' EMC_d=',EMC_d
                    call message(msg)
                    write(msg,*)'fmc_old=',fmc,' equi=',equi,' change=',change,' fmc=',fmc
                    call message(msg)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif

            endif
      enddo
   enddo
enddo


! assimilated differences decay
do j=jts,jte
  do k=1,2
    do i=its,ite
      change = moisture_dt / (fmep_decay_tlag * 3600.)
      if(change < tol) then
        fmep(i,k,j) = fmep(i,k,j)*(1.0 - change * (1.0 - 0.5 * change))
      else
        fmep(i,k,j) = fmep(i,k,j)*exp(-change)
      endif
    enddo
  enddo
enddo


if(fire_print_msg.ge.2)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,2)'Rain intensity    min',rainmin,  ' max',rainmax,' mm/h'
    call message(msg) 
    if(rainmin <0.)then
        call message('WARNING rain accumulation must increase')
    endif
    write(msg,2)'Relative humidity min',100*rhmin,' max',100*rhmax,'%'
    call message(msg) 
    if(.not.(rhmax<=1.0 .and. rhmin>=0))then
        call message('WARNING Relative humidity must be between 0 and 100%')
    endif
2   format(2(a,f10.2),a)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
endif

call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_equi,'equilibrium fmc_equi')
call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_lag,'time lag')
call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_gc,'after advance fmc_gc')

call copy2old

return

contains

subroutine copy2old

do j=jts,jte
    do i=its,ite
        rain_old(i,j) = rainc(i,j) + rainnc(i,j)
        t2_old(i,j) = t2(i,j)
        q2_old(i,j) = q2(i,j)
        psfc_old(i,j) = psfc(i,j)
    enddo
enddo

end subroutine copy2old

subroutine get_equi_moist
end subroutine get_equi_moist

end subroutine advance_moisture


subroutine read_namelist_fire(init_fuel_moisture)
implicit none
!*** purpose: read namelist.fire 
!*** arguments: 
logical, intent(in)::init_fuel_moisture
logical, external:: wrf_dm_on_monitor
#ifdef _OPENMP
!integer, external:: OMP_GET_THREAD_NUM
#endif
!*** local
integer:: iounit, i, k, io
character(len=128):: msg
real:: rat
!*** executable

! read 
namelist /fuel_scalars/ cmbcnst,hfgl,fuelmc_g,fuelmc_c,nfuelcats,no_fuel_cat
namelist /fuel_categories/ fuel_name,windrf,fgi,fueldepthm,savr, &
    fuelmce,fueldens,st,se,weight,fci_d,fct,ichap,fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live
namelist /fuel_moisture/ moisture_classes,drying_lag,wetting_lag,saturation_moisture,saturation_rain,rain_threshold, &
    drying_model,wetting_model, moisture_class_name,fmc_gc_initialization, fmc_1h,fmc_10h,fmc_100h,fmc_1000h,fmc_live

#ifdef _OPENMP
!   if (OMP_GET_THREAD_NUM() .ne. 0)then
!      call crash('init_fuel_cats: must be called from master thread')
!   endif
#endif

IF ( wrf_dm_on_monitor() ) THEN
    ! we are the MPI rank 0

    ! read the file
    call message('Reading file namelist.fire',level=0)
    iounit=open_text_file('namelist.fire','read',allow_fail=.true.)
    if(iounit < 0)then
       call message('File namelist.fire not found, using defaults',level=0)
    else  
       read(iounit,fuel_scalars,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_scalars not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_scalars')
       endif
       rewind(iounit)
       read(iounit,fuel_categories,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_categories not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_categories')
       endif
       rewind(iounit)
       read(iounit,fuel_moisture,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_moisture not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_moisture')
       endif
       close(iounit)
    endif

    ! write to output file
    iounit=open_text_file('namelist.fire.output','write')
    write(iounit,fuel_scalars)
    write(iounit,fuel_categories)
    write(iounit,fuel_moisture)
    close(iounit)

    if (nfuelcats>mfuelcats) then
        write(msg,*)'nfuelcats=',nfuelcats,' is too large, increase mfuelcats'
        call crash(msg)
    endif
    if (no_fuel_cat >= 1 .and. no_fuel_cat <= nfuelcats)then
        write(msg,*)'no_fuel_cat=',no_fuel_cat,' may not be between 1 and nfuelcats=',nfuelcats
        call crash(msg)
    endif

    ! convert fuel loads in the fuel classes to internal as weights adding up to one
    ! 
    ! copy the fuel weights and scale to 1

    ! just checking
    if (max_moisture_classes.ne.5)then
        call crash('Must have 5 fuel classes, modify source code if not')
    endif
    !**************************************************************************************************
    ! WARNING: initialization from scalars tied to a particular model with 5 fuel moisture classes
    ! the rest of the code can be used for various models with different number of fuel moisture classes
    !
    fgi_c(1:mfuelcats,1)=fgi_1h
    fgi_c(1:mfuelcats,2)=fgi_10h
    fgi_c(1:mfuelcats,3)=fgi_100h
    fgi_c(1:mfuelcats,4)=fgi_1000h
    fgi_c(1:mfuelcats,5)=fgi_live
    fmc_gc_initial_value(1)=fmc_1h
    fmc_gc_initial_value(2)=fmc_10h
    fmc_gc_initial_value(3)=fmc_100h
    fmc_gc_initial_value(4)=fmc_1000h
    fmc_gc_initial_value(5)=fmc_live
    !
    ! end initialization from scalars tied to a particular model with 5 fuel moisture classes
    ! WARNING: delete this and set fgi_c and fmc_gc_initial_value directly for more general models!
    !**************************************************************************************************

    call message('Scaling fuel loads within each fuel category to averaging weights of fuel moisture classes')
    do i=1,mfuelcats
            fgi_t(i) = 0.
            do k=1,max_moisture_classes
                if(fgi_c(i,k).ge.0.)then
                    fgi_t(i) = fgi_t(i) + fgi_c(i,k)
                else ! no need for OMP CRITICAL this needs to run in master thread anyway
                    write(msg,*)'fuel load in category',i,' fuel class ',k,' is ',fgi_c(i,k),',must be nonegative.'
                    call crash(msg)
                endif
            enddo
            if (fgi_t(i)>0. .or. fgi(i)>0.)then
                if (fgi_t(i)>0.) then
                     rat = fgi(i)/fgi_t(i)
                else
                     rat = 0.
                endif
                write(msg,'(a,i4,1x,a,g13.6,1x,a,g13.6,1x,a,g13.6)') &
                    'fuel category',i,'fuel load',fgi(i),'total by class',fgi_t(i), 'ratio',rat
                call message(msg)
            endif
            ! fuel moisture averaging weights for fuel classes in category i
            fmc_gwt(i)=0.
            do k=1,max_moisture_classes
               if (fgi_t(i) > 0.) then
                   fmc_gw(i,k) = fgi_c(i,k) / fgi_t(i)
                   fmc_gwt(i) = fmc_gwt(i) + fmc_gw(i,k)
               else
                   fmc_gw(i,k) = 0.
               endif
            enddo
    enddo

    
ENDIF

end subroutine read_namelist_fire


subroutine init_fuel_cats(init_fuel_moisture)
implicit none
!*** purpose: initialize fuel tables and variables by constants
!*** arguments: 
logical, intent(in)::init_fuel_moisture
logical, external:: wrf_dm_on_monitor
#ifdef _OPENMP
!integer, external:: OMP_GET_THREAD_NUM
#endif
!*** local
integer:: i,j,k,ii
integer:: kk
character(len=128):: msg
!*** executable

#ifdef _OPENMP
!   if (OMP_GET_THREAD_NUM() .ne. 0)then
!      call crash('init_fuel_cats: must be called from master thread')
!   endif
#endif

call read_namelist_fire(init_fuel_moisture)

! broadcast the contents of the file
call wrf_dm_bcast_real(cmbcnst,1)
call wrf_dm_bcast_real(hfgl,1)
call wrf_dm_bcast_real(fuelmc_g,1)
call wrf_dm_bcast_real(fuelmc_c,1)
call wrf_dm_bcast_integer(nfuelcats,1)
call wrf_dm_bcast_integer(no_fuel_cat,1)
call wrf_dm_bcast_real(windrf,    nfuelcats)
call wrf_dm_bcast_real(fgi,       nfuelcats)
call wrf_dm_bcast_real(fueldepthm,nfuelcats)
call wrf_dm_bcast_real(savr,      nfuelcats)
call wrf_dm_bcast_real(fuelmce,   nfuelcats)
call wrf_dm_bcast_real(fueldens,  nfuelcats)
call wrf_dm_bcast_real(st,        nfuelcats)
call wrf_dm_bcast_real(se,        nfuelcats)
call wrf_dm_bcast_real(weight,    nfuelcats)
call wrf_dm_bcast_real(fci_d,     nfuelcats)
call wrf_dm_bcast_real(fct,       nfuelcats)
call wrf_dm_bcast_integer(ichap,  nfuelcats)
! broadcast moisture tables
call wrf_dm_bcast_integer(moisture_classes,1)
call wrf_dm_bcast_real(drying_lag,     max_moisture_classes)
call wrf_dm_bcast_real(wetting_lag,     max_moisture_classes)
call wrf_dm_bcast_real(saturation_moisture,     max_moisture_classes)
call wrf_dm_bcast_real(saturation_rain,     max_moisture_classes)
call wrf_dm_bcast_real(rain_threshold,     max_moisture_classes)
call wrf_dm_bcast_integer(drying_model,     max_moisture_classes)
call wrf_dm_bcast_integer(wetting_model,     max_moisture_classes)
call wrf_dm_bcast_integer(fmc_gc_initialization,     max_moisture_classes)
call wrf_dm_bcast_real(fmc_gc_initial_value,     max_moisture_classes)
call wrf_dm_bcast_real(fmc_gw,     mfuelcats*max_moisture_classes)

! moisture model derived scalars
do i=1,moisture_classes
    rec_drying_lag_sec(i)  = 1.0/(3600.0*drying_lag(i))
    rec_wetting_lag_sec(i) = 1.0/(3600.0*wetting_lag(i))
enddo

! compute derived scalars

fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb

! compute derived fuel category coefficients 

DO i = 1,nfuelcats
    fci(i) = (1.+fuelmc_c)*fci_d(i)
    if(fct(i) .ne.  0.)then
        fcbr(i) = fci_d(i)/fct(i) !  avoid division by zero
    else
        fcbr(i) = 0
    endif
END DO

! prints

call message('**********************************************************')
call message('FUEL COEFFICIENTS')
write(msg,8)'cmbcnst    ',cmbcnst
call message(msg)
write(msg,8)'hfgl       ',hfgl
call message(msg)
write(msg,8)'fuelmc_g   ',fuelmc_g
call message(msg)
write(msg,8)'fuelmc_c   ',fuelmc_c
call message(msg)
write(msg,8)'fuelheat   ',fuelheat
call message(msg)
write(msg,7)'nfuelcats  ',nfuelcats
call message(msg)
write(msg,7)'no_fuel_cat',no_fuel_cat
call message(msg)
if(init_fuel_moisture)then
    write(msg,7)'moisture_classes',moisture_classes
    call message(msg)
endif

j=1    ! number of columns; max 5, but we do only one anyway, or would need to line up
7 format(a,(1x,i8,4x))
8 format(a,5(1x,g12.5e2))
9 format(a,5(1x,a))
10 format(a,2x,99(1x,f12.5))
11 format(a,2x,99(1x,a12))
do i=1,nfuelcats,j
    k=min(i+j-1,nfuelcats)
    call message(' ')
    write(msg,7)'CATEGORY  ',(ii,ii=i,k)
    call message(msg)
    write(msg,9)'fuel name ',(fuel_name(ii),ii=i,k)
    call message(msg)
!    write(msg,8)'windrf    ',(windrf(ii),ii=i,k)
!    call message(msg)
    write(msg,8)'fgi       ',(fgi(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldepthm',(fueldepthm(ii),ii=i,k)
    call message(msg)
    write(msg,8)'savr      ',(savr(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fuelmce   ',(fuelmce(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldens  ',(fueldens(ii),ii=i,k)
    call message(msg)
    write(msg,8)'st        ',(st(ii),ii=i,k)
    call message(msg)
    write(msg,8)'se        ',(se(ii),ii=i,k)
    call message(msg)
    write(msg,8)'weight    ',(weight(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci_d     ',(fci_d(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fct       ',(fct(ii),ii=i,k)
    call message(msg)
    write(msg,7)'ichap     ',(ichap(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci       ',(fci(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fcbr      ',(fcbr(ii),ii=i,k)
    call message(msg)
    if(init_fuel_moisture)then
        write(msg,11)'fuel class name',(trim(moisture_class_name(kk)),kk=1,moisture_classes),'Total','Total/fgi'
        call message(msg)
        write(msg,10)'fuel load fgi_c',(fgi_c(i,kk),kk=1,moisture_classes),fgi_t(i),fgi_t(i)/fgi(i)
        call message(msg)
        write(msg,10)'fraction fmc_gw',(fmc_gw(i,kk),kk=1,moisture_classes),fmc_gwt(i)
        call message(msg)
    endif
enddo
call message(' ')
call message('**********************************************************')

if(init_fuel_moisture)then
    j=1
    do i=1,moisture_classes,j
        k=min(i+j-1,nfuelcats)
        call message(' ')
        write(msg,7)'FUEL MOISTURE CLASS',(ii,ii=i,k)
        call message(msg)
        write(msg,9)'moisture class name    ',(trim(moisture_class_name(ii)),ii=i,k)
        call message(msg)
        write(msg,7)'drying_model           ',(drying_model(ii),ii=i,k)
        call message(msg)
        write(msg,8)'drying_lag (h)         ',(drying_lag(ii),ii=i,k)
        call message(msg)
        write(msg,7)'wetting_model          ',(wetting_model(ii),ii=i,k)
        call message(msg)
        write(msg,7)'fmc_gc_initialization  ',(fmc_gc_initialization(ii),ii=i,k)
        call message(msg)
        write(msg,8)'wetting_lag (h)        ',(wetting_lag(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'saturation_moisture (1)',(saturation_moisture(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'saturation_rain (mm/h) ',(saturation_rain(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'rain_threshold (mm/h)  ',(rain_threshold(ii),ii=i,k)
        call message(msg)    
    enddo
    call message(' ')
    call message('**********************************************************')
    call message(' ')
endif

! and print to file
IF ( wrf_dm_on_monitor() ) THEN
!jm  call write_fuels_m(61,30.,1.)
ENDIF
end subroutine init_fuel_cats


subroutine write_fuels_m(nsteps,maxwind,maxslope)
implicit none
integer, intent(in):: nsteps   ! number of steps for speed computation
real, intent(in):: maxwind,maxslope ! computer from zero to these

integer:: iounit,k,j,i
type(fire_params)::fp
!type fire_params
!real,pointer,dimension(:,:):: vx,vy                ! wind velocity (m/s)
!real,pointer,dimension(:,:):: zsf                  ! terrain height (m) 
!real,pointer,dimension(:,:):: dzdxf,dzdyf          ! terrain grad (1) 
!real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0 ! spread formula coefficients
!real,pointer,dimension(:,:):: fgip                 ! init mass of surface fuel (kg/m^2)
!real,pointer,dimension(:,:):: ischap               ! 1 if chapparal
!end type fire_params
real, dimension(1:2,1:nsteps), target::vx,vy,zsf,dzdxf,dzdyf,bbb,betafl,phiwc,r_0,fgip,ischap,fmc_g
real, dimension(1:2,1:nsteps), target::iboros
real, dimension(1:2,1:nsteps)::nfuel_cat,fuel_time,ros
real::ros_base,ros_wind,ros_slope,propx,propy,r

fp%vx=>vx
fp%vy=>vy
fp%dzdxf=>dzdxf
fp%dzdyf=>dzdyf
fp%bbb=>bbb
fp%betafl=>betafl
fp%phiwc=>phiwc
fp%r_0=>r_0
fp%fgip=>fgip
fp%ischap=>ischap
fp%iboros=>iboros 
fp%fmc_g=>fmc_g

iounit = open_text_file('fuels.m','write')

10 format('fuel(',i3,').',a,'=',"'",a,"'",';% ',a)
do k=1,nfuelcats
    write(iounit,10)k,'fuel_name',trim(fuel_name(k)),'FUEL MODEL NAME'
    call write_var(k,'windrf',windrf(k),'WIND REDUCTION FACTOR FROM 20ft TO MIDFLAME HEIGHT' )
    call write_var(k,'fgi',fgi(k),'INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)' )
    call write_var(k,'fueldepthm',fueldepthm(k),'FUEL DEPTH (M)')
    call write_var(k,'savr',savr(k),'FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT')
    call write_var(k,'fuelmce',fuelmce(k),'MOISTURE CONTENT OF EXTINCTION')
    call write_var(k,'fueldens',fueldens(k),'OVENDRY PARTICLE DENSITY, LB/FT^3')
    call write_var(k,'st',st(k),'FUEL PARTICLE TOTAL MINERAL CONTENT')
    call write_var(k,'se',se(k),'FUEL PARTICLE EFFECTIVE MINERAL CONTENT')
    call write_var(k,'weight',weight(k),'WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE')
    call write_var(k,'fci_d',fci_d(k),'INITIAL DRY MASS OF CANOPY FUEL')
    call write_var(k,'fct',fct(k),'BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)')
    call write_var(k,'ichap',float(ichap(k)),'1 if chaparral, 0 if not')
    call write_var(k,'fci',fci(k),'INITIAL TOTAL MASS OF CANOPY FUEL')
    call write_var(k,'fcbr',fcbr(k),'FUEL CANOPY BURN RATE (KG/M**2/S)')
    call write_var(k,'hfgl',hfgl,'SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)')
    call write_var(k,'cmbcnst',cmbcnst,'JOULES PER KG OF DRY FUEL')
    call write_var(k,'fuelheat',fuelheat,'FUEL PARTICLE LOW HEAT CONTENT, BTU/LB')
    call write_var(k,'fuelmc_g',fuelmc_g,'FUEL PARTICLE (SURFACE) MOISTURE CONTENT')
    call write_var(k,'fuelmc_c',fuelmc_c,'FUEL PARTICLE (CANOPY) MOISTURE CONTENT')
    ! set up fuel arrays 
    !subroutine set_fire_params( &
    !                       ifds,ifde,jfds,jfde, &
    !                       ifms,ifme,jfms,jfme, &
    !                       ifts,ifte,jfts,jfte, &
    !                       fdx,fdy,nfuel_cat0,  &
    !                       nfuel_cat,fuel_time, &
    !                       fp ) 
    nfuel_cat = k
    call set_fire_params( &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           0.,0.,k,  &
                           nfuel_cat,fuel_time, &
                           fp ) 
    ! set up windspeed and slope table
    propx=1.
    propy=0.
    do j=1,nsteps
       r=float(j-1)/float(nsteps-1)
       ! line 1 varies windspeed (in x direction), zero slope
       vx(1,j)=maxwind*r
       vy(1,j)=0.
       dzdxf(1,j)=0.
       dzdyf(1,j)=0.
       ! line 2 varies slope (in x direction), zero slope
       vx(2,j)=0.
       vy(2,j)=0.
       dzdxf(2,j)=maxslope*r
       dzdyf(2,j)=0.
    enddo
    do j=1,nsteps
       do i=1,2
          call fire_ros(ros_base,ros_wind,ros_slope, &
             propx,propy,i,j,fp)
          ros(i,j)=ros_base+ros_wind+ros_slope
       enddo
       write(iounit,13)k,'wind',j,vx(1,j),'wind speed'
       write(iounit,13)k,'ros_wind',j,ros(1,j),'rate of spread for the wind speed'
       write(iounit,13)k,'slope',j,dzdxf(2,j),'slope'
       write(iounit,13)k,'ros_slope',j,ros(2,j),'rate of spread for the slope'
    enddo
enddo
13 format('fuel(',i3,').',a,'(',i3,')=',g12.5e2,';% ',a)
 
close(iounit)
! stop

contains

subroutine write_var(k,name,value,descr)
! write entry for one variable
integer, intent(in)::k
character(len=*), intent(in)::name,descr
real, intent(in)::value
write(iounit,11)k,name,value
write(iounit,12)k,name,descr
11 format('fuel(',i3,').',a,'=',g12.5e2,  ';')
12 format('fuel(',i3,').',a,"_descr='",a,"';")
end subroutine write_var

end subroutine write_fuels_m

!
!*******************
!

subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,nfuel_cat0,  &
                           nfuel_cat,fuel_time, &
                           fp ) 

implicit none

!*** purpose: Set all fire model params arrays, constant values.

!*** arguments
integer, intent(in)::ifds,ifde,jfds,jfde                        ! fire domain bounds
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire tile bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
integer,intent(in)::nfuel_cat0                                  ! default fuel category, if nfuel_cat=0
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays
type(fire_params),intent(inout)::fp

!*** local

real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
real:: bmst
integer:: i,j,k
integer::nerr
character(len=128)::msg

integer :: kk
integer,parameter :: nf_sb = 204 ! maximum category on 
integer,dimension(1:nf_sb) :: ksb ! Anderson82 + S&B2005 fuel categories array

!*** executable

!*** Scott & Burgan ***! 
! assign no fuel by default to all the categories
do kk=1,nf_sb
   ksb(kk)=14
enddo
! Anderson 1982
ksb(1)=1
ksb(2)=2
ksb(3)=3
ksb(4)=4
ksb(5)=5
ksb(6)=6
ksb(7)=7
ksb(8)=8
ksb(9)=9
ksb(10)=10
ksb(11)=11
ksb(12)=12
ksb(13)=13
! Scott & Burgan crosswalks
! Short grass -- 1
ksb(101)=1
ksb(104)=1
ksb(107)=1
! Timber grass and understory -- 2
ksb(102)=2
ksb(121)=2
ksb(122)=2
ksb(123)=2
ksb(124)=2
! Tall grass -- 3
ksb(103)=3
ksb(105)=3
ksb(106)=3
ksb(108)=3
ksb(109)=3
! Chaparral -- 4
ksb(145)=4
ksb(147)=4
! Brush -- 5
ksb(142)=5
! Dormant Brushi -- 6
ksb(141)=6
ksb(146)=6
! Southern Rough -- 7
ksb(143)=7
ksb(144)=7
ksb(148)=7
ksb(149)=7
! Compact Timber Litter -- 8
ksb(181)=8
ksb(183)=8
ksb(184)=8
ksb(187)=8
! Hardwood Litter -- 9
ksb(182)=9
ksb(186)=9
ksb(188)=9
ksb(189)=9
! Timber (understory) -- 10
ksb(161)=10
ksb(162)=10
ksb(163)=10
ksb(164)=10
ksb(165)=10
! Light Logging Slash -- 11
ksb(185)=11
ksb(201)=11
! Medium Logging Slash -- 12
ksb(202)=12
! Heavy Logging Slash -- 13
ksb(203)=13
ksb(204)=13

! ****** !

nerr=0
do j=jfts,jfte
   do i=ifts,ifte
     ! fuel category 
     k=ksb(int(nfuel_cat(i,j))) ! DME S&B05
     if(k.eq.no_fuel_cat)then   ! no fuel 
        fp%fgip(i,j)=0.            ! no mass 
        fp%ischap(i,j)=0.
        fp%betafl(i,j)=1.          ! DME: set to 1.0 to prevent fp%betafl(i,j)**(-0.3) to be Inf in fire_ros
        fp%bbb(i,j)=1.             !
        fuel_time(i,j)=7./0.85  ! does not matter, just what was there before
        fp%phiwc(i,j)=0.
        fp%r_0(i,j)=0.             ! no fuel, no spread.
        fp%iboros(i,j)=0.   ! DME Ib/ROS zero for no fuel
     else
        if(k.eq.0.and.nfuel_cat0.ge.1.and.nfuel_cat0.le.nfuelcats)then
            ! replace k=0 by default
            k=nfuel_cat0
            nerr=nerr+1
        endif
   
        if(k.lt.1.or.k.gt.nfuelcats)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
            call message(msg)
            call crash('set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=weight(k)/0.85 ! cell based
        
        ! set fuel time constant: weight=1000 => 40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        fp%ischap(i,j)=ichap(k)
        fp%fgip(i,j)=fgi(k)
        if(fire_fmc_read.eq.1)then
           fp%fmc_g(i,j)=fuelmc_g
        endif

        !     ...Settings of fire spread parameters from Rothermel follows. These
        !        don't need to be recalculated later.
        
        bmst     = fp%fmc_g(i,j) / (1.+fp%fmc_g(i,j))
        fuelloadm= (1.-bmst) * fgi(k)  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(k)/0.3048               ! to ft
        fp%betafl(i,j) = fuelload/(fueldepth * fueldens(k))! packing ratio
        betaop = 3.348 * savr(k)**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*fp%fmc_g(i,j)            ! heat of preignition, btu/lb
        epsilon = exp(-138./savr(k) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * savr(k)**0.55)    ! const in wind coef
        fp%bbb(i,j) = 0.02526 * savr(k)**0.54      ! const in wind coef
        e = 0.715 * exp( -3.59e-4 * savr(k))       ! const in wind coef
        fp%phiwc(i,j) = c * (fp%betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * savr(k)**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = fp%betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + st(k))       ! net fuel loading, lb/ft^2
        rtemp1 = fp%fmc_g(i,j)/fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* se(k)**(-0.19)                ! mineral damping coef
        ir = gamma * wn * fuelheat * etam * etas  !rxn intensity,btu/ft^2 min
        ! irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        fp%iboros(i,j) = ir * 1055./( 0.3048**2 * 60.) * 1.e-3 * (60.*12.6/savr(k))     ! I_R x t_r (kJ m^-2)

        xifr = exp( (0.792 + 0.681*savr(k)**0.5) &
            * (fp%betafl(i,j)+0.1)) /(192. + 0.2595*savr(k)) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.
        fp%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min

     endif
  enddo
enddo

if(nerr.gt.1)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i6,a)')'set_fire_params: WARNING: fuel category 0 replaced in',nerr,' cells'
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
endif

end subroutine set_fire_params

!
!*******************
!

subroutine heat_fluxes(dt,fp,                     &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        iffs,iffe,jffs,jffe,                      &  ! fuel_frac_burnt dims
        fgip,fuel_frac_burnt,                     & !in
        grnhft,grnqft)                              !out
implicit none

!*** purpose        
! compute the heat fluxes on the fire grid cells

!*** arguments
type(fire_params), intent(in)::fp
real, intent(in)::dt          ! dt  the fire time step (the fire model advances time by this)
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   ! dimensions                   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft

!*** local
integer::i,j
real:: dmass,bmst
logical::latent

!*** executable        
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! surface fuel dry mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2) = fgi(nfuel_cat(i,j)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         bmst     = fp%fmc_g(i,j)/(1.+fp%fmc_g(i,j))        ! bmst varies by (i,j)
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! surface fire sensible heat flux W/m^2
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! surface fire latent heat flux W/m
         ! xlv is defined in module_model_constants.. Assume 56% of cellulose molecule mass is water.
    enddo
enddo

end subroutine heat_fluxes

!
!**********************
!            


subroutine set_nfuel_cat(   &
    ifms,ifme,jfms,jfme,               &
    ifts,ifte,jfts,jfte,               &
    ifuelread,nfuel_cat0,zsf,nfuel_cat)

implicit none

! set fuel distributions for testing
integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread,nfuel_cat0
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
real, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat

!*** local

! parameters to control execution
integer:: i,j,iu1
real:: t1
character(len=128)msg

!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: ifuelread=',ifuelread 
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)

if (ifuelread .eq. -1 .or. ifuelread .eq. 2) then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    call message('set_nfuel_cat: assuming nfuel_cat initialized already') 
    call message(msg)
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
else if (ifuelread .eq. 0) then
!
    do j=jfts,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=real(nfuel_cat0)
        enddo
    enddo
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: fuel initialized with category',nfuel_cat0
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
         
else if (ifuelread .eq. 1) then
!
!         make dependent on altitude (co mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... user defines fuel category spatial variability ! param!
    do j=jfts,jfte
        do  i=ifts,ifte
            ! nfuel_cat(i,j)= 2     ! grass with understory ! jm does nothing
            !jm t1=zsf(i,j)*slngth/100.
            t1 = zsf(i,j)  ! this is in m
            if(t1.le.1524.)then   !  up to 5000 ft
                nfuel_cat(i,j)= 3  ! tall grass
            else if(t1.ge.1524. .and. t1.le.2073.)then  ! 5.0-6.8 kft.
                nfuel_cat(i,j)= 2  ! grass with understory
            else if(t1.ge.2073..and.t1.le.2438.)then  ! 6.8-8.0 kft.
                nfuel_cat(i,j)= 8  ! timber litter - 10 (ponderosa)
            else if(t1.gt.2438. .and. t1.le. 3354.) then ! 8.0-11.0 kft.
!                 ... could also be mixed conifer.
                nfuel_cat(i,j)= 10 ! timber litter - 8 (lodgepole)
            else if(t1.gt.3354. .and. t1.le. 3658.) then ! 11.0-12.0 kft
                nfuel_cat(i,j)= 1  ! alpine meadow - 1
            else if(t1.gt.3658. ) then  ! > 12.0 kft
                nfuel_cat(i,j)= 14 ! no fuel.
            endif
        enddo
    enddo

    call message('set_nfuel_cat: fuel initialized by altitude')
else

    call crash('set_nfuel_cat: bad ifuelread')
endif
!     .............end  load fuel categories (or constant) here.

end subroutine set_nfuel_cat            

!
!**********************
!            

subroutine fire_ros(ros_base,ros_wind,ros_slope, &
propx,propy,i,j,fp)

implicit none

! copied with the following changes ONLY: 
!   0.5*(speed + abs(speed)) -> max(speed,0.)
!   index l -> j 
!   not using nfuel_cat here - cell info was put into arrays passed as arguments
!       in include file to avoid transcription errors when used elsewhere
!   betaop is absorbed in phiwc, see module_fr_fire_model/fire_startup
!   return the base, wind, and slope contributions to the rate of spread separately
!       because they may be needed to take advantage of known wind and slope vectors.
!       They should add up to get the total rate of spread.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     ... calculates fire spread rate with McArthur formula or Rothermel
!           using fuel type of fuel cell
!      
!         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
!         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
!      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*** arguments
real, intent(out)::ros_base,ros_wind,ros_slope ! rate of spread contribution due to fuel, wind, and slope
real, intent(in)::propx,propy
integer, intent(in)::i,j         ! node mesh coordinates
type(fire_params),intent(in)::fp

!*** local
real:: speed, tanphi ! windspeed and slope in the direction normal to the fireline
real:: umid, phis, phiw, spdms, umidm, excess
real:: ros_back
integer, parameter::ibeh=1
real, parameter::ros_max=6.
character(len=128)msg
real::cor_wind,cor_slope,nvx,nvy,scale


!*** executable

! make sure normal direction is size 1
!scale=sqrt(propx*propx+propy*propy)+tiny(scale)
scale=1.
nvx=propx/scale
nvy=propy/scale
if (fire_advection.ne.0) then ! from flags in module_fr_fire_util
    ! wind speed is total speed 
    speed =  sqrt(fp%vx(i,j)*fp%vx(i,j)+ fp%vy(i,j)*fp%vy(i,j))+tiny(speed)
    ! slope is total slope
    tanphi = sqrt(fp%dzdxf(i,j)*fp%dzdxf(i,j) + fp%dzdyf(i,j)*fp%dzdyf(i,j))+tiny(tanphi)
    ! cos of wind and spread, if >0
    cor_wind =  max(0.,(fp%vx(i,j)*nvx + fp%vy(i,j)*nvy)/speed)
    ! cos of slope and spread, if >0
    cor_slope = max(0., (fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy)/tanphi)
else
    ! wind speed in spread direction
    speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
    ! slope in spread direction
    tanphi = fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
    cor_wind=1.
    cor_slope=1.
endif

if (.not. fp%ischap(i,j) > 0.) then      ! if fuel is not chaparral, calculate rate of spread one of these ways
    if (ibeh .eq. 1) then                ! use Rothermel formula
!       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)            ! 
        umidm = min(spdms,30.)           ! max input wind spd is 30 m/s   !param!
        umid = umidm * 196.850           ! m/s to ft/min
        !  eqn.: phiw = c * umid**bbb(i,j) * (fp%betafl(i,j)/betaop)**(-e) ! wind coef
        phiw = umid**fp%bbb(i,j) * fp%phiwc(i,j) ! wind coef
        phis=0.
        if (tanphi .gt. 0.) then
            phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
        endif
        ! rosm = fp%r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
        ros_base = fp%r_0(i,j) * .00508
        ros_wind = ros_base*phiw
        ros_slope= ros_base*phis
        

    else                                   ! McArthur formula (Australian)
        ! rosm = 0.18*exp(0.8424*max(speed,0.))
        ros_base = 0.18*exp(0.8424)
        ros_wind = 0.18*exp(0.8424*max(speed,0.))
        ros_slope =0.
    endif
!
else   ! chaparral
!        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.)      
    ! rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
    ! note: backing ros is 0 for chaparral without setting nozero value below
    !sp_n=.03333  
    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    !rosm= max(rosm, sp_n)   ! no less than backing r.o.s.

    ros_back=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    ros_wind = 1.2974 * spdms**1.41       ! spread rate, m/s
    ros_wind = max(ros_wind, ros_back)
    ros_slope =0.

endif
! if advection, multiply by the cosines
ros_wind=ros_wind*cor_wind
ros_slope=ros_slope*cor_slope

!     ----------note!  put an 6 m/s cap on max spread rate -----------
! rosm= min(rosm, 6.)         ! no faster than this cap   ! param !

excess = ros_base + ros_wind + ros_slope - ros_max

if (excess > 0.)then
    ! take it out of wind and slope in proportion
    ros_wind = ros_wind - excess*ros_wind/(ros_wind+ros_slope)
    ros_slope = ros_slope - excess*ros_slope/(ros_wind+ros_slope)
endif


!write(msg,*)i,j,' speed=',speed,' tanphi',tanphi,' ros=',ros_base,ros_wind,ros_slope
!call message(msg)
      return

contains
real function nrm2(u,v)
real, intent(in)::u,v
nrm2=sqrt(u*u+v*v)
end function nrm2

end subroutine fire_ros 

end module module_fr_fire_phys
