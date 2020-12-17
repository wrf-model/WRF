!***********************************************************************
!*                   GECROS for early and late covering crops          *
!*     Genotype-by-Environment interaction on CROp growth Simulator    *
!*                                                                     *
!*                         Author: Xinyou YIN                          *
!*                      Crop and Weed Ecology Group                    *
!*                Wageningen University & Research Centre              *
!*               PO Box 430, 6700 AK Wageningen, Netherlands           *
!*                                                                     *
!*                Modified and extended for winter crops               * 
!*                         by Joachim INGWERSEN                        *
!*                         Biogeophysics Group                         *
!*                       University of Hohenheim                       *
!*                                                                     *   
!***********************************************************************

MODULE module_sf_gecros

implicit none

!Runtime constant variables defined within this subroutine
!Crop-specific parameters (former *.tbl file)
REAL, PARAMETER :: EG = 1.        !Efficiency of germination
REAL, PARAMETER :: CFV = 0.48     !Carbon fraction in vegetative organs
REAL, PARAMETER :: YGV = 0.81     !Grwoth efficiency for vegetative growth
REAL, PARAMETER :: FFAT = 0.02    !Fraction of fat in storage organs 
REAL, PARAMETER :: FLIG = 0.06    !Fraction of lignin in storage organs
REAL, PARAMETER :: FOAC = 0.02    !Fraction of organic acids in storage organs 
REAL, PARAMETER :: FMIN = 0.02    !Fraction of minerals in storage organs
REAL, PARAMETER :: LNCI = 0.03586 !Initial value of LNC (g N g-1)
REAL, PARAMETER :: TBD = 0.0      !Base temperature of phenology (C)
REAL, PARAMETER :: TOD = 22.5     !Optimum temperature of phenology (C)
REAL, PARAMETER :: TCD = 37.0     !Ceiling temperature of phenology (C)
REAL, PARAMETER :: TSEN = 1.0     !Curvature of temperature response (1)
REAL, PARAMETER :: SPSP = 0.2     !DS for start of photoperiod-sensitive phase
REAL, PARAMETER :: EPSP = 0.7     !DS for end of photoperiod-sensitive phase
REAL, PARAMETER :: CDMHT = 492.6  !Stem dry weight per unit plant height (g m-2 m2)
REAL, PARAMETER :: PMEH = 0.6468  !Fraction of sigmoid curve inflexion in plant height growth period (1)
REAL, PARAMETER :: ESDI = 1.35    !ESD for indeterminate crops 
REAL, PARAMETER :: PMES = 0.50    !Fraction of sigmoid curve inflexion in seed growth growth period (1) 
REAL, PARAMETER :: TBDV = -1.3    !Base temperature of vernalization (C)
REAL, PARAMETER :: TODV = 4.9     !Optimum temperature of vernalization (C)
REAL, PARAMETER :: TCDV = 15.7    !Ceiling temperature of vernalization (C)
REAL, PARAMETER :: NUPTX = 0.5/24./3600.    !Maximum crop nitrogen uptake (g N m-2 s-1) 
REAL, PARAMETER :: SLA0 = 0.0237  !Specific leaf area constant (m2 leaf g-1)
REAL, PARAMETER :: SLNMIN = 0.35  !Base SLN for photosynthesis (g N m-2 leaf)
REAL, PARAMETER :: RNCMIN = 0.005 !Minimum N concentration in roots (g N g-1)
REAL, PARAMETER :: STEMNC = 0.01  !Minimum N concentration in stem (g N g-1)
REAL, PARAMETER :: RLVDS = 0.0904 !Rate of C turnover from dead leaves to litter (d-1) 
REAL, PARAMETER :: DSCRIT = 0.225 !DS at which winter dormancy ends (1) 
REAL, PARAMETER :: EAJMAX = 48270.!Energy of activation for JMAX (J mol-1) 
REAL, PARAMETER :: XVN = 24.96    !Slope of linear relationship between JMAX and leaf N (mu-mol CO2 s-1 g-1 N)
REAL, PARAMETER :: XJN = 49.92    !Slope of linear relationship between VCMX and leaf N (mu-mol e- s-1 g-1 N)
REAL, PARAMETER :: NPL = 390.0    !Number of plants per m2
REAL, PARAMETER :: SEEDW = 0.0475 !Seed weight (g seed-1)
REAL, PARAMETER :: SEEDNC = 0.020 !Seed N concentration (g N g-1)
REAL, PARAMETER :: BLD = 25.58    !Leaf angle (from horizontal) (degree)
REAL, PARAMETER :: HTMX = 1.1     !Maximum plant height (m)
REAL, PARAMETER :: MTDV = 46.12   !Minimum thermal days for vegetative growth phase (d)
REAL, PARAMETER :: MTDR = 40.22   !Minimum thermal days for reproductive phase (d)
REAL, PARAMETER :: PSEN = -.104   !Photoperiod sensitivity of phenological development (h-1)
REAL, PARAMETER :: C3C4 = 1.      !C3C4 = 1. for C3 crops;  C3C4 = -1. for C4 crops.

!Runtime-constant general parameters
REAL, PARAMETER :: LEGUME = -1. !LEGUME = 1. for leguminous crops;   = -1. for non-leguminous crops.
REAL, PARAMETER :: DETER = 1.   !DETER  = 1. for determinate crops;  = -1. for indeterminate crops.
REAL, PARAMETER :: SLP = -1.    !SLP    = 1. for short-day crops;    = -1. for long-day crops.
REAL, PARAMETER :: NSUP1 = 0.   !NH4-N supply in g/m2/s
REAL, PARAMETER :: NSUP2 = 1./172800. !NO3-N supply in g/m2/s
REAL, PARAMETER :: CO2A=350.    !Atmospherric CO2 concentration (ppm)
REAL, PARAMETER :: FCRSH=0.5    !Initial fraction of C in shoot (-)
REAL, PARAMETER :: FNRSH=0.63   !Initial fraction of N in shoot (-) 
REAL, PARAMETER :: PNPRE=0.7    !Proportion of seed N that comes from non-structural stems during seed fill
REAL, PARAMETER :: CB=0.75      !Factor for initial N concentration of seed fill
REAL, PARAMETER :: CX=1.00      !Factor for final N concentration of seed fill
REAL, PARAMETER :: TM=1.5       !DS when transition from CB to CX is fastest
REAL, PARAMETER :: RSS=100.     !Soil resistance to evaporation (s/m)
REAL, PARAMETER :: LS=0.        !Lodging sverity (value between zero and unity)
REAL, PARAMETER :: WRB=0.25     !Critical root weight density (g/m2/cm depth)
REAL, PARAMETER :: THETA=0.7    !Convexity for light response curve of electron transport (J2) in photosynthesis
REAL, PARAMETER :: PNLS=1.      !Fraction of dead leaf N incorporated into soil litter N
REAL, PARAMETER :: INSP=-2.     !Inclination of sun angle (degree)
REAL, PARAMETER :: CCFIX=6.     !Carbon cost of symbiotic N fixation (g C/g N)
REAL, PARAMETER :: RDMX=130.    !Maximum rooting depth (m)
REAL, PARAMETER :: TCP=86400.   !Conversion factor from day into sec
REAL, PARAMETER :: Z1244=0.272727272727  !12./44.
REAL, PARAMETER :: LOG005=-2.995732274   !LOG(0.05)

!Runtime constant variables computed in driver.f
REAL :: YGO    !Growth efficiency of storage organs (g C/g C)
REAL :: CFO    !Carbon fraction in storage organs (g C/g)
REAL :: LNCMIN !Minimum N concentration in leaves (g N/g)
REAL :: CLVI   !Initial value of CLV
REAL :: CRTI   !Initial value of CRT
REAL :: NLVI   !Initial value of NLV
REAL :: NRTI   !Initial value of NRT
REAL :: HTI    !Initial value of HT
REAL :: RDI    !Initial value of RD

!*** Debugging on/off
LOGICAL :: debugging=.false.

CONTAINS

SUBROUTINE gecros (DOY, DT, CROP, RB, RT, RTS, FB, SNOWH,        & !I
           WN, SFCTMP, EAIR, RSD, RLD, PRCP, WUL, WLL, WCMIN, LWIDTH,     & !I                                                     &  !I  
           STATE_GECROS,                                                  & !H
           ATRJC, ATRJS, FSR, FRSU, ARSWSU, ARSWSH)  !O

IMPLICIT NONE

!    character(len=19), INTENT(IN)  :: nowdate ! string of current date e.g. 2012-30-10_12:00:00
    INTEGER, INTENT(IN) :: CROP     !CROP=1 -> early-covering crop, CROP=2 -> later covering crop         
    REAL, INTENT(IN)    :: DOY      !Julian day of the current time step
    REAL, INTENT(IN)    :: DT       !Integration time step (s)
    REAL, INTENT(IN)    :: RB       !Leaf boundary layer resistance (s/m)
    REAL, INTENT(IN)    :: RT       !Aerodynamic canopy resistance (s/m)
    REAL, INTENT(IN)    :: RTS      !Aerodynamic ground resistance (s/m)
    REAL, INTENT(IN)    :: FB       !Fraction of vegetation cover by snow
    REAL, INTENT(IN)    :: SNOWH    !Snow height (cm)
    REAL, INTENT(IN)    :: WN       !Wind speed (m/s)
    REAL, INTENT(IN)    :: SFCTMP   !Air temperature (K)
    REAL, INTENT(IN)    :: EAIR     !Vapour pressure (Pa)
    REAL, INTENT(IN)    :: RSD      !Downwelling shortwave radiation (W/m2)
    REAL, INTENT(IN)    :: RLD      !Downwelling longwave raidation (W/m2)
    REAL, INTENT(IN)    :: PRCP     !Precipiration (kg/m2/s)
    REAL, INTENT(IN)    :: WUL      !Soil water in the upper layer (L/m2 or mm)
    REAL, INTENT(IN)    :: WLL      !Soil water in the lower layer (L/m2 or mm)
    REAL, INTENT(IN)    :: WCMIN    !Minimum soil watter content (m3/m3)
    REAL, INTENT(IN)    :: LWIDTH   !Leaf width (m) read in from MPTABLE.TBL
    REAL, INTENT(OUT)   :: ATRJC    !Absorbed global radiation by canopy (W/m2)
    REAL, INTENT(OUT)   :: ATRJS    !Absorbed global radiation by soil (W/m2)
    REAL, INTENT(OUT)   :: FSR      !Reflected shortwave radiation (W/m2)
    REAL, INTENT(OUT)   :: FRSU     !Fraction of sunlit leaves in the canopy
    REAL, INTENT(OUT)   :: ARSWSU   !Actual stomatal resistance of sunlit leaves (s/m) 
    REAL, INTENT(OUT)   :: ARSWSH   !Actual stomatal resistance of shaded leaves (s/m)
    REAL, DIMENSION(1:60), INTENT(INOUT) ::  STATE_GECROS !Array with Gecros state variables

    !*** Gecros state variables
    REAL :: DS     !Development stage
    REAL :: LAI    !Green leaf area index
    REAL :: TLAI   !Total leaf area index
    REAL :: CTDU   !Cumulative thermal day unit (d) 
    REAL :: CVDU   !Cumulative thermal day unit of vernalization (d)
    REAL :: CLV    !Carbon in living leaves (g C/m2)
    REAL :: CLVD   !Carbon in dead leaves (g C/m2)
    REAL :: CSST   !Carbon in structural stems (g C/m2)
    REAL :: CSO    !Carbon in storage organs (g C/m2)
    REAL :: CSRT   !Carbon in structural roots (g C/m2)
    REAL :: CRTD   !Carbon in dead roots (g C/m2)
    REAL :: CLVDS  !Carbon in dead leaves that have become litter in soil (g C/m2)
    REAL :: NRT    !Nitrogen in living roots (g N/m2)
    REAL :: NST    !Nitrogen in stems (g N/m2)
    REAL :: NLV    !Nitrogen in living leaves (g N/m2)
    REAL :: NSO    !Nitrogen in storage organs (g N/m2)
    REAL :: TNLV   !Total leaf nitrogen (including N in senesced leaves) (g N/m2)
    REAL :: NLVD   !Nitrogen in dead leaves (g N/m2)
    REAL :: NRTD   !Nitrogen in dead roots (g N/m2)
    REAL :: CRVS   !Carbon in stem reserves (g C/m2)
    REAL :: CRVR   !Carbon in root reserves (g C/m2)
    REAL :: NREOE  !NRES accumulated till the end of seed number determining period (g N/m2)
    REAL :: NREOF  !NRES accumulated till the moment at which seed fill starts (g N/m2)
    REAL :: DCDSR  !Short fall of carbon demand for seed fill in previous time steps (g C/m2)
    REAL :: DCDTR  !Short fall of carbon demand for structural stems in previous time steps (g C/m2)
    REAL :: SLNB   !SLN in bottom leaves of canopy (g N/m2)
    REAL :: LAIC   !Carbon determined LAI (m2/m2)
    REAL :: RMUL   !Total of RMUN+RMUA+RMUS+RMLD
    REAL :: NDEMP  !Crop nitrogen demand of the previous time step (g N/m2/s)
    REAL :: NSUPP  !Nitrogen supply of the previous time step (g N/m2/s)
    REAL :: NFIXT  !Total symbiotically fixed nitrogen during growth (g N/m2)
    REAL :: NFIXR  !Reserve pool of symbiotically fixed nitrogen (g N/m2)
    REAL :: DCDTP  !Carbon demand for structural stem growth of the previous time step (g C/m2/s)
    REAL :: HT     !Canopy height (m)
    REAL :: TPCAN  !Cumulative canopy photosynthesis over growth period (g CO2/m2)
    REAL :: TRESP  !Total crop respiratory cost during growth (g CO2/m2)
    REAL :: TNUPT  !Total crop nitrogen uptake during growth (g N/m2)
    REAL :: LITNT  !Total litter nitrogen entering soil during growth (g N/m2)
    REAL :: WSO     !Yield (g/m2)
    REAL :: WSTRAW  !Straw (g/m2)
    REAL :: GrainNC !Nitrogen in grain (kg N/ha)
    REAL :: StrawNC !Nitrogen in straw (kg N/ha)
    REAL :: APCAN   !Actual gross canopy photosynthesis (g CO2/m2/d)
    
    character(Len=12)  :: inputstring
    INTEGER :: nowday, minutes
    REAL :: SC     !Solar constant (W/m2) 
    REAL :: SINLD  !Seasonal offset of sine of solar height (-)
    REAL :: COSLD  !Amplitude of sine of solar height (-)
    REAL :: DAYL   !Day length (h)
    REAL :: DDLP   !Day length for photoperiodism (h)
    REAL :: DSINBE !DSINB to correct for lower atmospheric transmission at lower solar elevation (s/d)
    REAL :: DVP    !Vapour pressure (kPa)
    REAL :: WNM    !Wind speed (m/s)
    REAL :: TAIR   !Air temperature (C)
    REAL :: ROOTD  !Rooting depth (m)
    REAL :: WLV    !Dry weight of living leaves (g/m2)
    REAL :: WST    !Dry weight of stems (g/m2)
    REAL :: WRT    !Dry weight of roots (g/m2)
    REAL :: WSH    !Dry weight of shoot (g/m2)
    REAL :: WLVD   !Dry weight of dead leaves (g/m2)
    REAL :: WRTD   !Dry weight of dead roots (g/m2)
    REAL :: CRT    !Carbon in living roots (g C/m2)
    REAL :: CSH    !Carbon in living shoot (g C/m2)
    REAL :: NSH    !Nitrogen in living shoot (g N/m2)
    REAL :: DLAI   !LAI of dead leaves still hanging on stem (m2/m2)
    REAL :: ESD    !DS for end of seed-number determing phase (-)
    REAL :: KCRN   !EXtinction coefficient of root nitrogen (m2/g C)
    REAL :: NFIXD  !Crop demand-determined NFIX (g N/2/s)
    REAL :: KR     !Extinction coefficient of root weight density over soil septh (1/cm)
    REAL :: HNCCR  !Critical shoot N concentration ( g N/g)
    REAL :: FVPD   !Slope of linear effect of VPD of intercelluar to ambient CO2 ratio (1/kPa)
    REAL :: NRETS  !Total crop-residue N returned to soil (g N/m2)
    REAL :: WCUL   !Soil water content in uppler layer (m3/m3)
    REAL :: DWSUP  !Water supply for evapotranspiration (mm/s)
    REAL :: CSRTN  !N determined CSRT (g C/m2)
    REAL :: NRES   !Estimated vegetative-organ N remobilizable for seed fill (g N/m2)
    REAL :: ONC    !N concentration in storage organ (g N/g)
    REAL :: RNC    !N concentration in roots (g N/g)
    REAL :: LNC    !N concentration in living leaves (g N/g)
    REAL :: KL     !Extinction coefficient diffuse component for PAE (m2/m2 leaf)
    REAL :: CTOT   !Total C in living shoots and roots (g C/m2)
    REAL :: NSHH   !N in shoots (excluding dead leaves incorporated into soil litter (g N/m2)
    REAL :: NTOT   !Total N in living shoots and roots (g N/m2)
    REAL :: WSHH   !Dry weight of shoots organs (excluding shadded leaves) (g/m2)
    REAL :: TSN    !Total seed number (seeds/m2)
    REAL :: HNC    !Actural N concentration in living shoot (g N/g)
    REAL :: PSO    !Protein content in storage organs (g protein/m2)
    REAL :: KLN    !Intermediate variable to compute KN (g N/m2 leaf) 
    REAL :: NBK    !Intermediate variable to compute KN (g N/m2 leaf)
    REAL :: KW     !Wind speed extinction coefficient in canopy (m2/m2 leaf)
    REAL :: WTOT   !Dry weight of total living plant parts (g/m2)
    REAL :: CCHKIN !C in crop accumulated since start of simulation
    REAL :: NCHKIN !N in crop accumulated since start of simulation
    REAL :: LCRT   !Rate of C loss in rooots because of senescence (g C/m2/s)
    REAL :: TSW    !Thousand seed weight (g)
    REAL :: PNC    !N concentration in living plant material (g N/m2)
    REAL :: NDEMD  !Defiency-driven N demand (g N/m2/s)
    REAL :: FCRVR  !Fraction of new root C partitioned to root reserves (g C/g C)
    REAL :: KN     !Leaf N extinction coefficient in the canopy (m2/m2 leaf)
    REAL :: CCHK   !Diff. between C added to the crop since start and net total C fluxes rel. to CCHKIN (%)
    REAL :: HI     !Harvest index (g/g)
    REAL :: LWRT   !Rate of loss of root biomass because of senescence (g/m2/s)
    REAL :: NCHK   !As CCHK but for N
    REAL :: LAIN   !N-determined LAI (m2/m2)
    REAL :: LNRT   !Rate of loss of N because of senescence (g N/m2/s)
    REAL :: LWLVM  !Intermediate variable to compute LWLV (g/m2/s)
    REAL :: SLNNT  !Value of SLNT with small plant-N increment (g N/m2 leaf)
    REAL :: SLNBC  !SLNB calculated from exponential N profile in canopy (g N/m2 leaf)
    REAL :: SLN    !Specific leaf N content (g N/m2 leaf)
    REAL :: SLA    !Specific leaf area (m2 leaf/g)
    REAL :: LWLV   !Rate of loss of leaf weight because of leaf senescence (g/m2/s)
    REAL :: AESOIL !Actual soil evaporation (mm/s)
    REAL :: APCANN !APCANS with small plant-N increment (g CO2/m2/s) 
    REAL :: APCANS !Actual standing canopy CO2 assimilation (g CO2/m2/s)
    REAL :: ATCAN  !Actual canopy transpiration (mm/s)
    REAL :: DAPAR  !PAR absorbed by canopy (J/m2/s)
    REAL :: DIFS   !Daytime average soil-air temperature difference (C)
    REAL :: DIFSH  !Daytime average shaded leaf-air temperature difference (C)
    REAL :: DIFSU  !Daytime average sunlit leaf-air temperature difference (C)
    REAL :: FRSH   !Fraction of shaded leaves in the canopy
    REAL :: HOD    !Hour of the day
    REAL :: PESOIL !Potential soil evaporation (mm/s) 
    REAL :: PPCAN  !Potential canopy assimilation (g CO2/m2/s)
    REAL :: PTCAN  !Potential canopy transpiration (mm/s)
    REAL :: RCAN   !Canopy resistance (s/m) 
    REAL :: RSLNB  !Rate of change in SLNB (g N/m2 leaf/s)
    REAL :: LNLV   !Rate of loss of leaf N because of senescence (g N/m2/s)
    REAL :: LCLV   !Rate of loss of leaf C because of senescence (g C/m2/s)
    REAL :: RMRE   !Residual maintenance respiration (g CO2/m2/s)
    REAL :: TAVSS  !Soil surface temperature (C)
    REAL :: RMN    !RM calculated with a small increment in plant N (g CO2/m2/s) 
    REAL :: VDU    !Rate of Vernalization day unit increase (d/s)
    REAL :: TDU    !Rate of thermal day unit increase (d/s)
    REAL :: RM     !Non-growth components of respiration, excluding the cost of N fixation (g CO2/m2/s)
    REAL :: LVDS   !Rate of transfer of C from dead leaves to litter (g C/m2/s)
    REAL :: NFIXE  !Available energy-determined NFIX (g N/m2/s)
    REAL :: DVR    !Phenological development rate (1/s)
    REAL :: RX     !Respiratory cost of N fixation (g CO2/m2/s)
    REAL :: RNSUPP !Rate of change in NSUPP (g N/m2/s)
    REAL :: NSUP   !N supply to crop (g N/m2/s)
    REAL :: NFIX   !Symbioticall fixed N (g N/m2/s)
    REAL :: LITN   !Litter N entering soil (g N/m2/s)
    REAL :: LITC   !Litter C entering soil (g C/m2/s)
    REAL :: FDH    !Expected relative growth rate of plant height (1/s)
    REAL :: FDS    !Expected relative growth rate of storage organs (1/s)
    REAL :: SHSAN  !SHSA calculated with a small increment in plant-N (g C/g C/s)
    REAL :: SHSA   !Relative shoot activity (g C/g C/s)
    REAL :: RMUS   !Respiratory cost of mineral uptake (g CO2/m2/s)
    REAL :: NDEMAD !Intermediate variable related to NDEM (g N/m2/s)
    REAL :: NDEMA  !Activity-driven NDEM (g N/m2/s)
    REAL :: NCR    !Intermediate variable (g N/g C)
    REAL :: DERI   !First order-derivative of SHSA with respect to crop N/C ratio (g C/g N/s)
    REAL :: ASSA   !Assimilates available from current photosynthesis for growth (g CO2/m2/s)
    REAL :: RNFIXR !Rate of change in NFIXR (g N/m2/s)
    REAL :: RNDEMP !Rate of change in NDEMP (g N/m2/s)
    REAL :: RMLD   !Respiration due to phloem loading of C assimilates to root (g CO2/m2/s)
    REAL :: RCSRT  !Rate of change in CSRT (g C/m2/s)
    REAL :: NUPTN  !Nitrate-N uptake by the crop (g N/m2/s)
    REAL :: NUPTA  !Ammonium-N uptake by the crop (g N/m2/s)
    REAL :: NDEM   !Crop N demand (g N/m2/s)SUBROUTINE ENERGY
    REAL :: FNSH   !Fraction of newly absorbed N partitioned to shoot (g N/g N)
    REAL :: FCSH   !Fraction of new V partitioned to shoot (g C/g C)
    REAL :: DCSR   !C supply from current photosynthesis for root growth (g C/m2/s)
    REAL :: SLNT   !SLN for top leaves in canopy (g N/m2 leaf)
    REAL :: RMUN   !Respiratory cost of nitrate-N uptake (g CO2/m2/s)
    REAL :: RMUA   !Respiratory cost of ammonium-N uptake (g CO2/m2/s)
    REAL :: NUPT   !Crop N uptake (g N/m2/s)
    REAL :: DCDSC  !C demand for seed filling at current time step (g C/m2/s)
    REAL :: DCDS   !C demand for filling of storage organgs at current time step (g C/m2/s)
    REAL :: FLWCS  !Flow of current assimilated C to storage organs (g C/m2/s) 
    REAL :: DCSS   !C supply from current photosynthesis for shoot growth (g C/m2/s)
    REAL :: DCST   !C suppyl from current photosynthesis for structural stem growth (g C/m2/s)
    REAL :: FCSO   !Fraction of new C partitioned to storage organs (g C/m2/s)
    REAL :: RRMUL  !Rate of change in RMUL (g CO2/m2/s)
    REAL :: RHT    !Rate of change in HT (m/s)
    REAL :: IFSH   !Integral factor of stresses on plant height growth (-)
    REAL :: GAP    !Gap betweeb C supply and C demand for seed growth (g C/m2/s)
    REAL :: CREMSI !Intermediate variable to compute CREMS (g C/m2/s)
    REAL :: CREMS  !C remobilized from stem reserves to storage organs (g C/m2/s)
    REAL :: CREMRI !Intermediate variable to compute CREMR (g C/m2/s)
    REAL :: CREMR  !C remobilized from root reserves to storage organs (g C/m2/s)
    REAL :: RWSO   !Rate of change in storage organs(g/m2/s)
    REAL :: RWRT   !Rate of change in roots (g/m2/s)
    REAL :: RRD    !Rate of change in RD (cm/s)
    REAL :: RDCDTP !Rate of change in DCDTP (g C/m2/s)
    REAL :: RDCDSR !Rate of change in DCDSR (g C/m2/s)
    REAL :: RCSST  !Rate of change in CSST (g C/m2/s)
    REAL :: RCSO   !Rate of change in CSO (g C/m2/s)
    REAL :: RCRVR  !Rate of change in CRVR (g C/m2/s)
    REAL :: FLWCT  !Flow of assimilated C to structural stems (g C/m2/s)
    REAL :: FCSST  !Fraction of new shoot C partitioned to structural stems (g C/m2/s)
    REAL :: FCLV   !Fraction of new shoot C partitioned to leaves (g C/m2/s)
    REAL :: DCDTC  !C demand of structural stem growth at current time step (g C/m2/s)
    REAL :: DCDT   !C demand for the growth of structural stems (g C/m2/s)
    REAL :: FCRVS  !Fraction of new C paritioned to stem reserves (g C/m2/s)
    REAL :: RCLV   !Rate of change in CLV (g C/m2/s)
    REAL :: RCRVS  !Rate of change in CRVS (g C/m2/s)
    REAL :: RDCDTR !Rate of change in DCDTR (g C/m2/s)
    REAL :: RESTOT !Total respiratory cost (g CO2/m2/s)
    REAL :: RG     !Growth respiration (g CO2/m2/s)
    REAL :: RLAI   !Rate of change in LAI (m2 leaf/m2/s)
    REAL :: RNLV   !Rate of change in NLV (g N/m2/s)
    REAL :: RNREOE !Rate of change in NREOE (g N/m2/s)
    REAL :: RNREOF !Rate of change in NREOF (g N/m2/s)
    REAL :: RNRES  !Rate of change in NRES (g N/m2/s)
    REAL :: RNRT   !Rate of change in NRT (g N/m2/s)
    REAL :: RNSO   !Rate of change in NSO (g N/m2/s)
    REAL :: RNST   !Rate of change in NSZ (g N/m2/s)
    REAL :: RRP    !Respiration/photosynthesis ratio (-) 
    REAL :: RTNLV  !Rate of change in TNLV (g N/m2/s)
    REAL :: RWLV   !Rate of change in WLV (g/m2/s)
    REAL :: RWST   !Rate of change in WST (g/m2/s) 
    REAL :: GLAT   !Latitude (degree)   
    REAL :: SD1    !Thickness of upper evaporative soil layer (cm) (equals RDI)
    
    !*** write STATE_GECROS array into Gecros variables
    DS     = STATE_GECROS(1)
    CTDU   = STATE_GECROS(2)
    CVDU   = STATE_GECROS(3)
    CLV    = STATE_GECROS(4)   
    CLVD   = STATE_GECROS(5)
    CSST   = STATE_GECROS(6)
    CSO    = STATE_GECROS(7)
    CSRT   = STATE_GECROS(8)
    CRTD   = STATE_GECROS(9)
    CLVDS  = STATE_GECROS(10)
    NRT    = STATE_GECROS(11)
    NST    = STATE_GECROS(12)
    NLV    = STATE_GECROS(13)
    NSO    = STATE_GECROS(14)
    TNLV   = STATE_GECROS(15)
    NLVD   = STATE_GECROS(16)
    NRTD   = STATE_GECROS(17)
    CRVS   = STATE_GECROS(18)
    CRVR   = STATE_GECROS(19)
    NREOE  = STATE_GECROS(20)
    NREOF  = STATE_GECROS(21)
    DCDSR  = STATE_GECROS(22)
    DCDTR  = STATE_GECROS(23)
    SLNB   = STATE_GECROS(24)
    LAIC   = STATE_GECROS(25)
    RMUL   = STATE_GECROS(26)
    NDEMP  = STATE_GECROS(27)
    NSUPP  = STATE_GECROS(28)
    NFIXT  = STATE_GECROS(29)
    NFIXR  = STATE_GECROS(30)
    DCDTP  = STATE_GECROS(31)
    HT     = STATE_GECROS(32)
    ROOTD  = STATE_GECROS(33)
    TPCAN  = STATE_GECROS(34)
    TRESP  = STATE_GECROS(35)
    TNUPT  = STATE_GECROS(36)
    LITNT  = STATE_GECROS(37)
    GLAT   = STATE_GECROS(44)   
    WSO      = STATE_GECROS(45)
    WSTRAW   = STATE_GECROS(46)
    GrainNC  = STATE_GECROS(47)
    StrawNC  = STATE_GECROS(48)
    LAI      = STATE_GECROS(49)
    TLAI     = STATE_GECROS(50)
    SD1      = STATE_GECROS(52)

    ! Used for debugging
!    if (nowdate(9:12).eq.'2500') then 
!    write(*,*) nowdate, ' 1 ', DS
!    write(*,*) nowdate, ' 2 ', CTDU
!    write(*,*) nowdate, ' 3 ', CVDU
!    write(*,*) nowdate, ' 4 ', CLV
!    write(*,*) nowdate, ' 5 ', CLVD
!    write(*,*) nowdate, ' 6 ', CSST
!    write(*,*) nowdate, ' 7 ', CSO
!    write(*,*) nowdate, ' 8 ', CSRT
!    write(*,*) nowdate, ' 9 ', CRTD
!    write(*,*) nowdate, ' 10 ', CLVDS
!    write(*,*) nowdate, ' 11 ', NRT
!    write(*,*) nowdate, ' 12 ', NST
!    write(*,*) nowdate, ' 13 ', NLV
!    write(*,*) nowdate, ' 14 ', NSO
!    write(*,*) nowdate, ' 15 ', TNLV
!    write(*,*) nowdate, ' 16 ', NLVD
!    write(*,*) nowdate, ' 17 ', NRTD
!    write(*,*) nowdate, ' 18 ', CRVS
!    write(*,*) nowdate, ' 19 ', CRVR
!    write(*,*) nowdate, ' 20 ', NREOE
!    write(*,*) nowdate, ' 21 ', NREOF
!    write(*,*) nowdate, ' 22 ', DCDSR
!    write(*,*) nowdate, ' 23 ', DCDTR
!    write(*,*) nowdate, ' 24 ', SLNB
!    write(*,*) nowdate, ' 25 ', LAIC
!    write(*,*) nowdate, ' 26 ', RMUL
!    write(*,*) nowdate, ' 27 ', NDEMP
!    write(*,*) nowdate, ' 28 ', NSUPP
!    write(*,*) nowdate, ' 29 ', NFIXT
!    write(*,*) nowdate, ' 30 ', NFIXR
!    write(*,*) nowdate, ' 31 ', DCDTP
!    write(*,*) nowdate, ' 32 ', HT
!    write(*,*) nowdate, ' 33 ', ROOTD
!    write(*,*) nowdate, ' 34 ', TPCAN
!    write(*,*) nowdate, ' 35 ', TRESP
!    write(*,*) nowdate, ' 36 ', TNUPT
!    write(*,*) nowdate, ' 37 ', LITNT
!    read(*,*)
!    endif
    
    PPCAN=0.
    APCANS=0.
    APCANN=0.
    APCAN=0.
    PTCAN=0.
    ATCAN=0.
    PESOIL=0.
    AESOIL=0.
    DIFS=0.
    DIFSU=0.
    DIFSH=0.
    DAPAR=0.
    RCAN=0.
    DVR=0.

    nowday = INT(DOY)
    HOD = float(nint((DOY-int(DOY))*86400.))/3600.
    
    ! Conversion from K to C
    TAIR   = SFCTMP - 273.15

    ! Conversion of rel. humidity into VP (kPa)
    DVP    = EAIR*0.001    !Converts EAIR from Pa to kPa
    WNM    = MAX (0.1, WN)
       
    ! Photoperiod, solar constant and daily extraterrestrial radiation
    CALL ASTRO(aint(DOY),GLAT,INSP,SC,SINLD,COSLD,DAYL,DDLP,DSINBE)  
       
    ! Plant weights (g/m2) 
    WLV    = CLV  / CFV 
    WST    = CSST / CFV + CRVS/0.444
    WSO    = CSO  / CFO
    WRT    = CSRT / CFV + CRVR/0.444
    WLVD   = CLVD / CFV
    WRTD   = CRTD / CFV
    
    ! Carbon in shoot and root (g C/m2) 
    CSH    = CLV + CSST + CRVS + CSO
    CRT    = CSRT + CRVR

    ! Nitrogen in shoot (g C/m2) 
    NSH    = NST + NLV + NSO
 
    ! Extinction coefficient of root nitrogen (m2/g C)
    KCRN   = -LOG005/6.3424/CFV/WRB/RDMX
      
    ! DS for end of seed number determining period
    ESD    = INSW(DETER, ESDI, 1.)
     
    ! Dead leaves still hanging on the plant (m2/m2
    DLAI   = (CLVD-CLVDS)/CFV*SLA0

    ! Total leaf area index (dead plus living leaves)
    TLAI   = MAX(0.01,LAI + DLAI)            
    
    ! Crop demand-determined NFIX
    NFIXD  = MAX(0., NDEMP - NSUPP)

    ! Critical shoot nitrogen concentration g N g-1
    HNCCR  = LNCI*EXP(-.4*DS)
    
    !Extinction coefficient of root weight density over the soil depth cm-1
    KR     = -LOG005/RDMX  !cm-1
    
    ! Slope of linear effect of VPD on intercelluar to ambient CO2 ratio (1/kPa)
    FVPD   = INSW (C3C4, 0.195127, 0.116214)
     
    ! Total crop-residue nitrogen returned to soil (g N/m2)
    NRETS  = LITNT+INSW(DS-2.,0.,NLV+NST+NRT+NFIXR+(CLVD-CLVDS)/ &
    CFV*LNCMIN*(1.+PNLS)/2.)

    !if (nowdate(1:12).eq.'201110061930') then  
    !write(*,*) 'Bis hier her bin ich gekommen', ROOTD
    !endif
 
    ! Soil water content of the upper and lower layer (m3/m3)
    WCUL   = (WUL+WCMIN*10.*ROOTD)/10./ROOTD
        
    ! Daily water supply for evapotranspiration (mm/s)
    DWSUP  = MAX(.1/TCP,WUL/TCP+.1/TCP)
    
    ! Nitrogen-determined CSRT (g C/m2)
    CSRTN  = 1./KCRN*LOG(1.+KCRN*MAX(0.,(NRT*CFV-CRVR*RNCMIN))/RNCMIN)
    
    ! Straw weight (g/m2)
    WSTRAW = WLV + WST + (WLVD - CLVDS/CFV)                            

    ! Shoot weight (g/m2)
    WSH    = WLV  + WST + WSO       

    ! Estimated vegetative-organ N remobilizable for seed growth (g N/m2)
    NRES   = NREOF + (NREOE-NREOF)*(ESD-1.)/NOTNUL(MIN(DS,ESD)-1.)
   
    ! Nitrogen concentration in living leaves LAI(g N/m2)
    LNC    = NLV / NOTNUL(WLV)
    
    ! Nitrogen concentration in roots (g N/m2)
    RNC    = NRT / NOTNUL(WRT)
    
    ! Nitrogen concentration seeds (g N/m2)
    ONC    = INSW(-WSO, NSO/NOTNUL(WSO), 0.)

    ! Nitrogen in grain and straw in kg N per ha
    GrainNC = NSO*10.                       
    StrawNC = (NLV+NST)*10.                 

    ! Extinction coefficient of nitrogen and wind
    CALL KDIFF (TLAI,BLD*3.141592654/180.,0.2, KL)

    CTOT   = CSH + CRT
    NSHH   = NSH +(WLVD-CLVDS/CFV)*LNCMIN
    NTOT   = NSH + NRT
    WSHH   = WSH  + (WLVD-CLVDS/CFV)
    TSN    = NRES/PNPRE/SEEDNC/SEEDW
    HNC    = NSH / NOTNUL(WSH)
    
    ! Amount of seed protein
    PSO    = 6.25*WSO*ONC

    KLN    = KL*(TNLV-SLNMIN*TLAI)  

    NBK    = SLNMIN*(1.-EXP(-KL*TLAI))  
    KW     = KL
    WTOT   = WSH  + WRT
 
    ! Crop carbon balance check
    CCHKIN = CTOT + CLVD + CRTD -CLVI-CRTI
      
    ! Crop nitrogen balance check
    NCHKIN = NTOT + NLVD + NRTD -NLVI-NRTI
    
    LCRT   = MAX(MIN(CSRT-1.E-4,CSRT-MIN(CSRTN,CSRT)),0.)/TCP !Eq. 43, p. 38, DELT ok, gC m-2 s-1 

    FCRVR  = INSW(CSRTN-CSRT, 1., 0.)
    PNC    = NTOT / NOTNUL(WTOT)
        
    KN     = 1./TLAI*LOG(MAX(1.001,(KLN+NBK)/(KLN*EXP(-KL*TLAI)+NBK)))
    
    TSW    = WSO/NOTNUL(TSN)*1000.
     
    NDEMD  = INSW(DS-1., WSH*(HNCCR-HNC)*(1.+NRT/MAX(1E-2,NSH))/TCP, 0.)  !Eq. 20, p.25, DELT ok, g N m-2 s-1
    
    ! Biomass formation
    HI     = WSO  / NOTNUL(WSHH)
    CCHK   = (CCHKIN-(TPCAN-TRESP)*Z1244)/NOTNUL(CCHKIN)*100.
    NCHK   = (NCHKIN-TNUPT)/NOTNUL(TNUPT)*100.
    LWRT   = LCRT/CFV   !g m-2 s-1
  
    ! Leaf area development
    LAIN   = LOG(1.+ KN*MAX(0.,NLV)/SLNMIN)/KN
    LNRT   = LWRT*RNCMIN  !g N m-2 s-1

    ! Green leaves still hanging on the plant (m2/m2)
    LAI    = MAX(0.01,MIN(LAIN, LAIC))
   
    ! Leaf senescence
    ! The equation differs somewhat from Yin. The right hand side is not divided by deltaT
    ! This is done by computing LWLV
    ! LWLVM  = (LAIC-MIN(LAIC,LAIN))/SLA0/DELT !Eq. 42, p.36, DELT ok, g m-2 s-1

    LWLVM  = (LAIC-MIN(LAIC,LAIN))/SLA0 !Eq. 42, p.36, DELT ok, g m-2 s-1
    
    ! Specific leaf nitrogen and its profile in the canopy
    SLN    = NLV/LAI
    SLNT   = NLV*KN/(1.-EXP(-KN*LAI))
    SLNBC  = NLV*KN*EXP(-KN*LAI)/(1.-EXP(-KN*LAI))
    SLNNT  = (NLV+0.001*NLV)*KN /(1.-EXP(-KN*LAI))
    SLA    = LAI/NOTNUL(WLV)
    
    ! ji, 1402, Stay-green maize -> no senescense
    LWLV = MIN((WLV-1.E-5)/TCP, (LWLVM+REANOR(ESD-DS,LWLVM)*0.03*WLV)/TCP)   !g m-2 s-1, gecheckt ok
    
    !write(*,*) nowdate, DWSUP,CO2A,LS,EAJMAX,XVN,XJN,THETA,WCUL,FVPD,RB,RT,RTS 
    !read(*,*)
    ! Call TOTPT: Computes daily total photosynthesis and stomatal resistance
    CALL TOTPT(HOD,DS,SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR,DVP,WNM,C3C4,LAI, &
         TLAI,HT,LWIDTH,ROOTD,SD1,RSS,BLD,KN,KW,SLN,SLNT,SLNNT,SLNMIN,FB,&
         DWSUP,CO2A,LS,EAJMAX,XVN,XJN,THETA,WCUL,FVPD,RB,RT,RTS, &
         PPCAN,APCANS,APCANN,APCAN,PTCAN,ATCAN,PESOIL,AESOIL,DIFS,DIFSU, &
         DIFSH,DAPAR,RCAN,ATRJC,ATRJS,FSR,FRSU,FRSH,ARSWSU,ARSWSH)
      
    RSLNB  = (SLNBC-SLNB)/TCP !!Eq. 38, p. 35, DELT ok, g N m-2 leaf s-1

    LNLV   = MIN(LWLV,LWLVM)*LNCMIN + (LWLV-MIN(LWLV,LWLVM))*LNC  !g m-2 s-1
    LCLV   = LWLV*CFV  !g m-2 s-1
    
    ! Residual maintenance respiration g m-2 s-1
    RMRE   = MAX(MIN(44./12.*0.218*(NTOT-WSH*LNCMIN-WRT*RNCMIN)/TCP &
    ,APCAN-(1.E-5+RMUL)/TCP), 0.)  !0.218 has the unit g C g-1 N d-1!
      
    TAVSS  = TAIR + DIFS

    ! Call TUNIT: Computes cumulative thermal units (CTDU)
    CALL TUNIT (1.*CROP, DS,TAIR,MAX(0.,DIFS),DAYL,TBD,TOD,TCD,TBDV,TODV,TCDV,TSEN,TDU,VDU)

    RMN    = MAX(0., MIN(APCAN-1.E-5/TCP,RMUL/TCP) + MAX(MIN(44./12.*0.218* &
    (1.001*NTOT-WSH*LNCMIN-WRT*RNCMIN)/TCP,APCAN-(1.E-5+RMUL)/TCP), 0.))

    RM = MAX(0., MIN(APCAN-1.E-5/TCP,RMUL/TCP) + RMRE)  !gC m-2 s-1

    ! Daily and total C and N returns from crop to soil
    ! ji: RLVDS eingefuehrt. Standardmaessig war der Wert von RLVDS auf 0.1 hard-gecoded
    ! CLVD: Amount of carbon in dead leaves
    ! CLVDS: Amount of carbon in dead leaves that have become litter in soil

    IF(DS.lt.0.25) THEN
       LVDS   = (CLVD-CLVDS)/TCP  !gC m-2 s-1 
    ELSE
       LVDS   = RLVDS*(CLVD-CLVDS)*(TAVSS-TBD)/(TOD-TBD)/TCP  !gC m-2 s-1
    ENDIF
    
    CALL PHENO (1.*CROP,DS,SLP,DDLP,SPSP,EPSP,PSEN,MTDV,MTDR,TDU,CVDU,DVR)

    NFIXE  = MAX(0., APCAN-(1.E-5+RM)/TCP)/CCFIX*Z1244

    ! Daily carbon flow for seed filling
    CALL BETAF(DVR,1.,PMES,LIMIT(1.,2.,DS)-1., FDS)
            
    LITC   =  LCRT + LVDS                  !gC m-2 s-1
    LITN   =  LNRT + LVDS/CFV *LNCMIN*PNLS !gN m-2 s-1
 
    CALL BETAF(DVR,(1.+ESD)/2.,PMEH*(1.+ESD)/2.,MIN((1.+ESD)/2.,DS), FDH)
     
    NSUP   = NSUP1 + NSUP2 !gN m-2 s-1
      
    NFIX   = INSW (LEGUME, 0., MIN(NFIXE, NFIXD)) !N fixation

    RNSUPP = (NSUP - NSUPP)/DT         !RNSUPP g N m-2 s-2
    RX     = 44./12.*(CCFIX*NFIX)
  
!*** Current photo-assimilates (g CO2 m-2 s-1) for growth, and R/P ratio
    ASSA   = APCAN - RM - RX
      
!*** Crop nitrogen demand and uptake (g N m-2 s-1)

    SHSA   = Z1244 * YGV*MAX(1E-16,APCAN -RM -RX)/ MAX(0.1,CSH)
    SHSAN  = Z1244 * YGV*(APCANN-RMN-RX)/ MAX(0.1,CSH)
    DERI   = MAX(0.,(SHSAN - SHSA)/(0.001*MAX(0.01,NTOT)/MAX(0.1,CTOT)))
     
    RMUS   = 0.06*0.05/0.454*YGV*ASSA
    NDEMA  = CRT * SHSA**2/NOTNUL(DERI)
    NCR    = INSW(SLNT-SLNMIN,0.,MIN(NUPTX,NDEMA))/(YGV*MAX(1E-16,APCANS-RM-RX)*Z1244)
    NDEMAD = INSW(LNC-1.5*LNCI, MAX(NDEMA, NDEMD), 0.)
    
!*** Nitrogen partitioning between shoots and roots
    FNSH   = 1./(1.+NCR*DERI/SHSA*CSH/MAX(1E-2,CRT)*NRT/MAX(1E-2,NSH))
       
!*** Carbon partitioning among organs and reserve pools
    FCSH   = 1./(1.+NCR*DERI/SHSA)

!*** ji 10.02.14, winter dormancy
!*** ji 16.06.15, ecc/lcc switch     
    IF (CROP==1) THEN
        NDEM   = INSW(DS-DSCRIT,INSW(SLNMIN-SLN+1.E-5, MIN(NUPTX,.01*NDEMAD), 0.), &
                                    INSW(SLNMIN-SLN+1.E-5, MIN(NUPTX,NDEMAD), 0.)) 
    ELSE
        NDEM   = INSW(SLNMIN-SLN+1.E-5, MIN(NUPTX,NDEMAD), 0.)
    ENDIF

    DCSR   = Z1244*(1.-FCSH)*ASSA
    NUPTN  = MIN(NSUP2, NSUP2/NOTNUL(NSUP)*MAX(0.,NDEM-NFIXR/TCP))
    RCSRT  = Z1244*ASSA*(1.-FCSH)*(1.-FCRVR)*YGV - LCRT
    RNFIXR = NFIX - MIN(NDEM,NFIXR/TCP)
    RMLD   = 0.06*(1.-FCSH)*ASSA
    RNDEMP = (NDEM - NDEMP)/DT         !as for RNSUPP, DELT ok
    NUPTA  = MIN(NSUP1, NSUP1/NOTNUL(NSUP)*MAX(0.,NDEM-NFIXR/TCP))
    
    ! Carbon supply from current photo-assimilates for shoot & root growth
    DCSS   = Z1244*    FCSH *ASSA
    NUPT   = MAX(0., NUPTA + NUPTN + MIN(NDEM, NFIXR/TCP))
    RMUA   = 44./12.*0.17*NUPTA

    CALL SINKG(DS,1.,TSN*SEEDW*CFO,YGO,FDS,DCDSR,DCSS,DT,&
    DCDSC,DCDS,FLWCS)
 
    ! Maintenance and total respiration (g CO2 m-2 d-1)
    RMUN   = 44./12.*2.05*NUPTN
 
    ! Daily carbon flow for structural stem growth
    DCST   = DCSS - FLWCS
    FCSO   = FLWCS/DCSS

    RRMUL  = (RMUN+RMUA+RMUS+RMLD-RMUL)/DT
         
    GAP    = MAX(0., DCDS-DCSS)
  
    !*** ji, 16.06.15, ecc/lcc switch     
    IF (CROP==1) THEN
       IFSH   = INSW(DCST-1E-11, 1., LIMIT(0.,1.,DCST/NOTNUL(DCDTP)))
    ELSE
       IFSH   = LIMIT(0.,1.,DCST/NOTNUL(DCDTP))
    ENDIF  
     
    CREMSI = MIN(0.94*CRVS, CRVS/NOTNUL(CRVS+CRVR)*GAP)/0.94
    CREMRI = MIN(0.94*CRVR, CRVR/NOTNUL(CRVS+CRVR)*GAP)/0.94
    CREMS  = INSW(DCDS-DCSS, 0., CREMSI)
    CREMR  = INSW(DCDS-DCSS, 0., CREMRI)

    IF (CROP==1) THEN
        RHT    = MIN(HTMX-HT, FDH*HTMX*IFSH)
    ELSE
        RHT    = MIN(HTMX-HT, FDH*HTMX*INSW(DCST-1E-4, 1., LIMIT(0.,1.,DCST/NOTNUL(DCDTP))))
    ENDIF
    
    IF (CROP==1) THEN
        CALL SINKG(DS,.1,CDMHT*HTMX*CFV,YGV,FDH*IFSH,DCDTR,DCST,DT, &
        DCDTC,DCDT,FLWCT)
    ELSE
        CALL SINKG(DS,.0,CDMHT*HTMX*CFV,YGV,FDH*IFSH,DCDTR,DCST,DT, &
        DCDTC,DCDT,FLWCT)
    ENDIF
     
    RCRVR  = FCRVR*DCSR - CREMR
     
    IF (CROP==1) THEN
       RDCDTP = (DCDTC-DCDTP)/DT
    ELSE
       DCDTP = DCDTC
       RDCDTP = 0.
    ENDIF

    !ji, the MIN function avoids that a situation occurs during which
    !no assimilates are partioned to the leaves. FCSST max. 85%
    IF (CROP==1) THEN
        FCSST  = MIN(.85,INSW(DS-(ESD+0.2), FLWCT/MAX(1E-16,DCSS), 0.))
        !FCSST  = INSW(DS-(ESD+0.2), FLWCT/DCSS, 0.) original version
     ELSE     
        FCSST  = MIN(.85,INSW(DS-(ESD+0.2), FLWCT/MAX(1E-16,DCSS), 0.))
    ENDIF
    
    RCSO   = Z1244*ASSA*FCSH*FCSO*YGO + 0.94*(CREMS+CREMR)*YGO
    RWSO   = RCSO / CFO
    RDCDSR = MAX(0., (DCDSC-RCSO/YGO))-(FLWCS-MIN(DCDSC,DCSS))
    RWRT   = RCSRT/CFV + RCRVR/0.444
    RCSST  = Z1244*ASSA*    FCSH *    FCSST *YGV
   
    FCLV   = REAAND(LAIN-LAIC,ESD-DS)*(1.-FCSO-FCSST)
      
    RRD    = INSW(ROOTD-RDMX, MIN((RDMX-ROOTD)/TCP,(RWRT+LWRT)/(WRB+KR* &
    (WRT+WRTD))), 0.)
    
    RCLV   = Z1244*ASSA*    FCSH *    FCLV  *  YGV - LCLV
   
    FCRVS  = 1. - FCLV - FCSO - FCSST
 
    RDCDTR = MAX(0., (DCDTC-RCSST/YGV))-(FLWCT-MIN(DCDTC,DCST))
    RCRVS  = FCRVS*DCSS - CREMS
    RWLV   = RCLV / CFV
     
    RNRES  = NUPT-(LNCMIN*(RCLV+LCLV)+RNCMIN*(RCSRT+LCRT)+STEMNC* &
    RCSST)/CFV
 
    RG     = 44./12.*((1.-YGV)/YGV*(RCLV+RCSST+RCSRT+LCLV+LCRT)+ &
    (1.-YGO)/YGO* RCSO)
 
    RWST   = RCSST/ CFV + RCRVS/0.444
    RNREOF = INSW (DS-1.0, RNRES, 0.)
    RNREOE = INSW (DS-ESD, RNRES, 0.)
 
    RESTOT = RM+RX+RG + 44./12.*0.06*(CREMS+CREMR)

    CALL RNACC (FNSH,NUPT,RWST,STEMNC,LNCMIN,RNCMIN,LNC,RNC,NLV,NRT,WLV,WRT, &
    DT,CB,CX,TM,DS,SEEDNC,RWSO,LNLV,LNRT, RNRT,RNST,RNLV,RTNLV,RNSO)

    RRP = RESTOT / APCAN

    CALL RLAIC(1.*CROP,DS,SLA0,RWLV,LAIC,KN,NLV,RNLV,SLNB,RSLNB, RLAI)
    
    ! Used for debugging
!    if (nowdate(9:12).eq.'2500') then 
!    write(*,*) nowdate, ' 1 ', DVR
!    write(*,*) nowdate, ' 2 ', TDU
!    write(*,*) nowdate, ' 3 ', VDU
!    write(*,*) nowdate, ' 4 ' 
!    write(*,*) nowdate, ' 5 ', RCLV
!    write(*,*) nowdate, ' 6 ', LCLV
!    write(*,*) nowdate, ' 7 ', RCSST
!    write(*,*) nowdate, ' 8 ', RCSO
!    write(*,*) nowdate, ' 9 ', RCSRT
!    write(*,*) nowdate, ' 10 ',LCRT
!    write(*,*) nowdate, ' 11 ',LVDS
!    write(*,*) nowdate, ' 12 ',RNRT
!    write(*,*) nowdate, ' 13 ',RNST
!    write(*,*) nowdate, ' 14 ',RNLV
!    write(*,*) nowdate, ' 15 ',RNSO
!    write(*,*) nowdate, ' 16 ',RTNLV
!    write(*,*) nowdate, ' 17 ',LNLV
!    write(*,*) nowdate, ' 18 ',LNRT
!    write(*,*) nowdate, ' 19 ',RCRVS
!    write(*,*) nowdate, ' 20 ',RCRVR
!    write(*,*) nowdate, ' 21 ',RNREOE
!    write(*,*) nowdate, ' 22 ',RNREOF
!    write(*,*) nowdate, ' 23 ',RDCDSR
!    write(*,*) nowdate, ' 24 ',RDCDTR
!    write(*,*) nowdate, ' 25 ',RSLNB
!    write(*,*) nowdate, ' 26 ',RLAI
!    write(*,*) nowdate, ' 27 ',RRMUL
!    write(*,*) nowdate, ' 27 ',RRMUL
!    write(*,*) nowdate, ' 28 ',RNDEMP
!    write(*,*) nowdate, ' 29 ',RNSUPP
!    write(*,*) nowdate, ' 30 ',NFIX
!    write(*,*) nowdate, ' 31 ',RNFIXR
!    write(*,*) nowdate, ' 32 ',RDCDTP
!    write(*,*) nowdate, ' 33 ',RHT
!    write(*,*) nowdate, ' 34 ',RRD
!    write(*,*) nowdate, ' 35 ',APCAN
!    write(*,*) nowdate, ' 36 ',RESTOT
!    write(*,*) nowdate, ' 37 ',NUPT
!    write(*,*) nowdate, ' 38 ',LITN
!    read(*,*)
!    endif
    
    ! Integration of ODEs
    DS     = MAX(0., INTGRL(DS, DVR, DT))
    CTDU   = MAX(0., INTGRL(CTDU, TDU, DT))
    CVDU   = MAX(0., INTGRL(CVDU, VDU, DT))
    CLV    = MAX(0., INTGRL (CLV, RCLV, DT))
    CLVD   = MAX(0., INTGRL (CLVD, LCLV, DT))
    CSST   = MAX(0., INTGRL (CSST, RCSST, DT))
    CSO    = MAX(0., INTGRL (CSO, RCSO, DT))
    CSRT   = MAX(0., INTGRL (CSRT, RCSRT, DT))
    CRTD   = MAX(0., INTGRL (CRTD, LCRT, DT))
    CLVDS  = MAX(0.,INTGRL (CLVDS, LVDS, DT))
    NRT    = MAX(0.,INTGRL (NRT, RNRT, DT))
    NST    = MAX(0.,INTGRL (NST, RNST, DT))
    NLV    = MAX(0.,INTGRL (NLV, RNLV, DT))
    NSO    = MAX(0.,INTGRL (NSO, RNSO, DT))
    TNLV   = MAX(0.,INTGRL (TNLV, RTNLV, DT))
    NLVD   = MAX(0.,INTGRL (NLVD, LNLV, DT))
    NRTD   = MAX(0.,INTGRL (NRTD, LNRT, DT))
    CRVS   = MAX(0.,INTGRL (CRVS, RCRVS, DT))
    CRVR   = MAX(0.,INTGRL (CRVR, RCRVR, DT))
    NREOE  = MAX(0.,INTGRL(NREOE, RNREOE, DT))
    NREOF  = MAX(0.,INTGRL(NREOF, RNREOF, DT))
    DCDSR  = MAX(0.,INTGRL(DCDSR, RDCDSR, DT))
    DCDTR  = MAX(0.,INTGRL(DCDTR, RDCDTR, DT))
    SLNB   = MAX(0.,INTGRL(SLNB, RSLNB, DT))
    LAIC   = MAX(0.,INTGRL(LAIC, RLAI, DT))
    RMUL   = MAX(0.,INTGRL(RMUL, RRMUL, DT))
    NDEMP  = MAX(0.,INTGRL(NDEMP, RNDEMP, DT))
    NSUPP  = MAX(0.,INTGRL(NSUPP, RNSUPP, DT))
    NFIXT  = MAX(0.,INTGRL(NFIXT, NFIX, DT))
    NFIXR  = MAX(0.,INTGRL(NFIXR, RNFIXR, DT))
    DCDTP  = MAX(0.,INTGRL(DCDTP, RDCDTP, DT))
    HT     = MAX(0.,INTGRL(HT, RHT, DT))
    ROOTD  = MAX(1.,INTGRL(ROOTD, RRD, DT))
    TPCAN  = MAX(0.,INTGRL(TPCAN, APCAN, DT))
    TRESP  = MAX(0.,INTGRL(TRESP, RESTOT, DT))
    TNUPT  = MAX(0.,INTGRL(TNUPT, NUPT, DT))
    LITNT  = MAX(1e-3,INTGRL(LITNT, LITN, DT))

    ! Write updated Gecros variables into STATE_GECROS array
    STATE_GECROS(1) = DS
    STATE_GECROS(2) = CTDU
    STATE_GECROS(3) = CVDU 
    STATE_GECROS(4) = CLV     
    STATE_GECROS(5) = CLVD 
    STATE_GECROS(6) = CSST 
    STATE_GECROS(7) = CSO 
    STATE_GECROS(8) = CSRT 
    STATE_GECROS(9) = CRTD 
    STATE_GECROS(10) = CLVDS 
    STATE_GECROS(11) = NRT 
    STATE_GECROS(12) = NST 
    STATE_GECROS(13) = NLV 
    STATE_GECROS(14) = NSO 
    STATE_GECROS(15) = TNLV 
    STATE_GECROS(16) = NLVD 
    STATE_GECROS(17) = NRTD 
    STATE_GECROS(18) = CRVS 
    STATE_GECROS(19) = CRVR 
    STATE_GECROS(20) = NREOE 
    STATE_GECROS(21) = NREOF 
    STATE_GECROS(22) = DCDSR 
    STATE_GECROS(23) = DCDTR 
    STATE_GECROS(24) = SLNB 
    STATE_GECROS(25) = LAIC
    STATE_GECROS(26) = RMUL
    STATE_GECROS(27) = NDEMP 
    STATE_GECROS(28) = NSUPP
    STATE_GECROS(29) = NFIXT
    STATE_GECROS(30) = NFIXR
    STATE_GECROS(31) = DCDTP
    STATE_GECROS(32) = HT 
    STATE_GECROS(33) = ROOTD 
    STATE_GECROS(34) = TPCAN
    STATE_GECROS(35) = TRESP
    STATE_GECROS(36) = TNUPT
    STATE_GECROS(37) = LITNT
    STATE_GECROS(45) = WSO
    STATE_GECROS(46) = WSTRAW
    STATE_GECROS(47) = GrainNC
    STATE_GECROS(48) = StrawNC
    STATE_GECROS(49) = LAI
    STATE_GECROS(50) = TLAI
        
    ! Used for debugging
!    if(debugging) then 
!    if (nowdate(9:12).eq.'1200') then 
!    write(*,*) nowdate, RSD
!    write(*,*) "DS:  ", DS
!    write(*,*) "CTDU:  ",CTDU
!    write(*,*) "CVDU:  ",CVDU
!    write(*,*) "Carbon in living leaves     CLV: ",CLV
!    write(*,*) "Carbon in dead leaves      CLVD: ",CLVD
!    write(*,*) "Carbon in structural stems CSST: ",CSST
!    write(*,*) "Carbon in storage organs    CSO: ",CSO
!    write(*,*) "Carbon in structural roots CSRT: ",CSRT
!    write(*,*) "Carbon in living roots      CRT: ",CRT
!    write(*,*) "Carbon in dead roots       CRTD: ",CRTD
!    write(*,*) "Rooting depth             ROOTD: ",ROOTD
!    write(*,*) "Carbon litter on soil     CLVDS: ",CLVDS
!    write(*,*) "Carbon in stem reserves    CRVS: ",CRVS
!    write(*,*) "Carbon in root reserves    CRVR: ",CRVR
!    write(*,*) "Total leaf area index      TLAI: ",TLAI
!    write(*,*) "Dead  leaf area index      DLAI: ",DLAI
!    write(*,*) "Living leaf area index     GLAI: ",LAI, LAIC, LAIN
!    write(*,*) "Specific leaf area index    SLA: ",SLA
!    write(*,*) "Carbon balance check       CCHK: ",CCHK
!    write(*,*) "Nitrogen balance check     NCHK: ",NCHK
!    read(*,*)
!    endif
!    endif
    
! ----------------------------------------------------------------------
END SUBROUTINE gecros
! ----------------------------------------------------------------------

!******************** SUBROUTINES FOR CROP SIMULATION *******************
!*----------------------------------------------------------------------*
!*  SUBROUTINE TUNIT                                                    *
!*  Purpose: This subroutine calculates the daily amount of thermal day *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                            -       I  *
!*  TMAX    R4  Daily maximum temperature                    oC      I  *
!*  TMIN    R4  Daily minimum temperature                    oC      I  *
!*  DIF     R4  Daytime plant-air temperature differential   oC      I  *
!*  DAYL    R4  Astronomic daylength (base = 0 degrees)      h       I  *
!*  TBD     R4  Base temperature for phenology               oC      I  *
!*  TOD     R4  Optimum temperature for phenology            oC      I  *
!*  TCD     R4  Ceiling temperature for phenology            oC      I  *
!*  TSEN    R4  Curvature for temperature response           -       I  *
!*  TDU     R4  Rate of thermal days increase                d s-1   O  *
!*  VDU     R4  Rate of vernalization days increase          d s-1   O  *
!*----------------------------------------------------------------------*
      SUBROUTINE TUNIT(CROP,DS,TAIR,DIF,DAYL,TBD,TOD,TCD,TBDV,TODV,TCDV,TSEN,TDU,VDU)
      IMPLICIT REAL (A-Z)
      INTEGER I
      
!*---assuming development rate at supra-optimum temperatures during
!*   the reproductive phase equals that at the optimum temperature
      IF (DS.GT.1.) THEN
           TAIR = MIN (TAIR,TOD)
      ELSE
           TAIR = TAIR
      ENDIF

!*---vernalization response (Lenz 2007, p. 26)
!*---Ingwersen 29.6.2010
      IF (TAIR.LT.TBDV .OR. TAIR.GT.TCDV) THEN
           TUV = 0.
      ELSE
           TUV = (((TCDV-TAIR)/(TCDV-TODV))*((TAIR-TBDV)/(TODV-TBDV))**((TODV-TBDV)/(TCDV-TODV)))**TSEN
      ENDIF

!*---instantaneous thermal unit based on bell-shaped temperature response
      IF (TAIR.LT.TBD .OR. TAIR.GT.TCD) THEN
           TU = 0.
      ELSE
           TU = (((TCD-TAIR)/(TCD-TOD))*((TAIR-TBD)/(TOD-TBD))**((TOD-TBD)/(TCD-TOD)))**TSEN
      ENDIF

!*---daily thermal unit
!*** ji, 16.6.15, ecc/lcc switch
      IF (CROP==1) THEN
         TDU = TU/TCP
      ELSE
         TDU = INSW(DS-2.,TU/TCP,0.)
      ENDIF
      VDU = TUV/TCP
      	
      RETURN
      END SUBROUTINE TUNIT


!*----------------------------------------------------------------------*
!*  SUBROUTINE PHENO                                                    *
!*  Purpose: This subroutine calculates phenological development rate.  *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                            -       I  *
!*  SLP     R4  Crop type(1. for short-day,-1. for long-day) -       I  *
!*  DDLP    R4  Daylength for photoperiodism                 h       I  *
!*  SPSP    R4  DS for start of photoperiod-sensitive phase  -       I  *
!*  EPSP    R4  DS for end of photoperiod-sensitive phase    -       I  *
!*  PSEN    R4  Photoperiod sensitivity (+ for SD, - for LD) h-1     I  *
!*  MTDV    R4  Minimum thermal days for vegetative phase    d       I  *
!*  MTDR    R4  Minimum thermal days for reproductive phase  d       I  *
!*  TDU     R4  Thermal unit                                 -       I  *
!*  CVDU    R4  Cumulative vernalization unit (ji, 29.6)     -       I  *
!*  DVR     R4  Development rate                             s-1     O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PHENO (CROP,DS,SLP,DDLP,SPSP,EPSP,PSEN,MTDV,MTDR,TDU,CVDU,DVR)
      IMPLICIT REAL (A-Z)
      
!*---determining if it is for short-day or long-day crop
      IF (SLP.LT.0.) THEN
          MOP = 18.     !minimum optimum photoperiod for long-day crop
          DLP = MIN(MOP,DDLP)
      ELSE
          MOP = 11.     !maximum optimum photoperiod for short-day crop
          DLP = MAX(MOP,DDLP)
      ENDIF

!*---effect of photoperiod on development rate
      IF (DS.LT.SPSP .OR. DS.GT.EPSP) THEN
          EFP = 1.
      ELSE
          EFP = MAX(0., 1.-PSEN*(DLP-MOP))
      ENDIF	

!*---effect of vernalization (ji, 21.6.10)
!*** ji, 16.6.15, ecc/lcc switch
     IF (CROP==1) THEN
        EFV = CVDU**5./(22.5**5. + CVDU**5.)
     ELSE
        EFV = 1.0
     ENDIF
          
!*---development rate of vegetative and reproductive phases
!*---extended for vernalization according to Lenz (2007); ji: 21.6.2010
      IF (DS.LE.0.4) THEN
          DVR   = 1./MTDV*TDU*EFP*EFV
      ENDIF

      IF (DS.GT.0.4 .AND. DS.LE.1.0) THEN
          DVR   = 1./MTDV*TDU*EFP
      ENDIF

      IF (DS.GT.1.0) THEN
          DVR   = 1./MTDR*TDU
      ENDIF

      RETURN
      END SUBROUTINE PHENO


!*----------------------------------------------------------------------*
!*  SUBROUTINE RNACC                                                    *
!*  Purpose: This subroutine calculates rate of N accumulation in organs*
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  FNSH    R4  Fraction of new N partitioned to shoot       -       I  *
!*  NUPT    R4  Nitrogen uptake at a time step               gN/m2/s I  *
!*  RWST    R4  Rate of stem weight                          g/m2/s  I  *
!*  STEMNC  R4  Nitrogen concentration in stem               gN/g    I  *
!*  LNCMIN  R4  Minimum N concentration in leaf              gN/g    I  *
!*  RNCMIN  R4  Minimum N concentration in root              gN/g    I  *
!*  LNC     R4  Nitrogen concentration in leaf               gN/g    I  *
!*  RNC     R4  Nitrogen concentration in root               gN/g    I  *
!*  NLV     R4  Canopy (green)leaf N content                 gN/m2   I  *
!*  NRT     R4  (living)root N content                       gN/m2   I  *
!*  WLV     R4  Canopy (green)leaf weight                    g/m2    I  *
!*  WRT     R4  (living)Root weight                          g/m2    I  *
!*  DELT    R4  Time step of simulation                      s       I  *
!*  CB      R4  Factor for initial N concent. of seed-fill   -       I  *
!*  CX      R4  Factor for final N concent. of seed-fill     -       I  *
!*  TM      R4  DS when transition from CB to CX is fastest  -       I  *
!*  DS      R4  Development stage                            -       I  *
!*  SEEDNC  R4  Standard seed N concentration                gN/g    I  *
!*  RWSO    R4  growth rate of seed                          g/m2/s  I  *
!*  LNLV    R4  Loss rate of NLV due to senescence           gN/m2/s I  *
!*  LNRT    R4  Loss rate of NRT due to senescence           gN/m2/s I  *
!*  RNRT    R4  rate of N accumulation in root               gN/m2/s O  *
!*  RNST    R4  rate of N accumulation in stem               gN/m2/s O  *
!*  RNLV    R4  rate of N accumulation in leaf               gN/m2/s O  *
!*  RTNLV   R4  Positive value of RNLV                       gN/m2/s O  *
!*  RNSO    R4  rate of N accumulation in seed(storage organ)gN/m2/s O  *
!*----------------------------------------------------------------------*
      SUBROUTINE RNACC (FNSH,NUPT,RWST,STEMNC,LNCMIN,RNCMIN,LNC,RNC, &
                        NLV,NRT,WLV,WRT,DELT,CB,CX,TM,DS,SEEDNC, &
                        RWSO,LNLV,LNRT, RNRT,RNST,RNLV,RTNLV,RNSO)
      IMPLICIT REAL (A-Z)
      
!*---amount of N partitioned to shoot
      NSHN   = FNSH * NUPT

!*---leaf N (NLVA) or root N (NRTA) available for remobilization within a day
      NLVA   = INSW(LNCMIN-LNC, NLV-WLV*LNCMIN, 0.)/TCP
      NRTA   = INSW(RNCMIN-RNC, NRT-WRT*RNCMIN, 0.)/TCP
      
      NTA    = NLVA + NRTA

!*---rate of N accumulation in stem
      RNST   = RWST * INSW(-NTA,STEMNC,0.)

!*---expected N dynamics during seed(storage organ) filling
      CDS    = CB+(CX-CB)*(4.-TM-DS)/(2.-TM)*(DS-1.)**(1./(2.-TM))
      ENSNC  = LIMIT(CB,CX,CDS) * SEEDNC

!*---rate of N accumulation in seed
      NGS    = NSHN - RNST - ENSNC*RWSO
      NONC   = MAX(0.,INSW(NTA+NGS,(NTA+NSHN-RNST)/NOTNUL(RWSO),ENSNC))
      RNSO   = RWSO*NONC

!*---rate of N accumulation in leaf!

      NLVN   = INSW(NTA+NGS,-NLVA-LNLV,-NLVA/NOTNUL(NTA)*(-NGS)-LNLV)
      GNLV   = INSW(NGS, NLVN, NSHN-RNST-RNSO-LNLV)
      RNLV   = MAX (-NLV+1.E-7, GNLV)
      RTNLV  = MAX(0., RNLV)

!*---rate of N accumulation in root
      NRTN   = INSW(NTA+NGS, NUPT-NSHN-NRTA-LNRT, &
              NUPT-NSHN-NRTA/NOTNUL(NTA)*(-NGS)-LNRT)
      GNRT   = INSW(NGS, NRTN, NUPT-NSHN-LNRT)
      RNRT   = MAX (-NRT+5.E-8, GNRT)
   	
      RETURN
      END SUBROUTINE RNACC


!*----------------------------------------------------------------------*
!*  SUBROUTINE RLAIC                                                    *
!*  Purpose: This subroutine calculates the daily increase of leaf      *
!           area index (m2 leaf/m2 ground/day).                        *
!                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                          -         I  *
!*  SLA0    R4  Specific leaf area constant                m2 g-1    I  *
!*  RWLV    R4  Rate of increment in leaf weight           g m-2 s-1 I  *
!*  LAI     R4  Leaf area index                            m2 m-2    I  *
!*  KN      R4  Leaf nitrogen extinction coefficient       m2 m-2    I  *
!*  NLV     R4  Total leaf nitrogen content in a canopy    g m-2     I  *
!*  RNLV    R4  Rate of increment in NLV                   g m-2 s-1 I  *
!*  SLNB    R4  Nitrogen content of bottom leaves          g m-2     I  *
!*  RSLNB   R4  Rate of increment in SLNB                  g m-2 s-1 I  *
!*  RLAI    R4  Rate of increment in leaf area index       m2 m-2s-1 O  *
!*----------------------------------------------------------------------*
      SUBROUTINE RLAIC(CROP,DS,SLA0,RWLV,LAI,KN,NLV,RNLV,SLNB,RSLNB, RLAI)
      IMPLICIT REAL (A-Z)
      
      SLNB = MAX(1E-2, SLNB)
      !*---rate of LAI driven by carbon supply
      RLAI   =  INSW(RWLV, MAX(-LAI+1.E-5,SLA0*RWLV), SLA0*RWLV)
      
	  !*---rate of LAI driven by nitrogen during juvenile phase
      !*** ji, 16.6.15, ecc/lcc switch
      IF ((CROP==2) .AND. (LAI.LT.1.5) .AND. (DS.LT.0.75)) THEN
           RLAI  = MAX(0.,(SLNB*RNLV-NLV*RSLNB)/SLNB/(SLNB+KN*NLV))
      ENDIF
  
      
      RETURN
      END SUBROUTINE RLAIC


!*----------------------------------------------------------------------*
!*  SUBROUTINE BETAF                                                    *
!*  Purpose: This subroutine calculates the dynamics of expected growth *
!*           of sinks, based on the beta sigmoid growth equation        *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DVR     R4  Development rate                            s-1      I  *
!*  TE      R4  Stage at which sink growth stops            -        I  *
!*  TX      R4  Stage at which sink growth rate is maximal  -        I  *
!*  TI      R4  Stage of a day considered                   -        I  *
!*  FD      R4  Relative expected growth of a sink at a day s-1      O  *
!*----------------------------------------------------------------------*
      SUBROUTINE BETAF(DVRX,TE,TX,TI, FD)
      
            REAL, INTENT(IN)  :: DVRX, TE, TX, TI
            REAL, INTENT(OUT) :: FD
      
      FD    = DVRX*(2.*TE-TX)*(TE-TI)/TE/(TE-TX)**2*(TI/TE)**(TX/(TE-TX))
      !FD    = DVRX*(TE-TI)/(TE-TX)*(TI/TX)**(TX/(TE-TX)) !Eq. a 1a Yin et al 2003
      
      END SUBROUTINE BETAF


!*----------------------------------------------------------------------*
!*  SUBROUTINE SINKG                                                    *
!*  Purpose: This subroutine calculates carbon demand for sink growth.  *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                           -        I  *
!*  SSG     R4  Stage at which sink growth starts           -        I  *
!*  TOTC    R4  Total carbon in a sink at end of its growth g C/m2   I  *
!*  YG      R4  Growth efficiency                           g C/g C  I  *
!*  FD      R4  Relative expected growth of a sink at a day s-1      I  *
!*  DCDR    R4  Shortfall of C demand in previous days      g C/m2   I  *
!*  DCS     R4  Daily C supply for sink growth              g C/m2/s I  *
!*  DELT    R4  Time step of integration                    s        I  *
!*  DCDC    R4  C demand of the current day                 g C/m2/s O  *
!*  DCD     R4  Daily C demand for sink growth              g C/m2/s O  *
!*  FLWC    R4  Flow of current assimilated C to sink       g C/m2/s O  *
!*----------------------------------------------------------------------*
      SUBROUTINE SINKG(DS,SSG,TOTC,YG,FD,DCDR,DCS,DELT,DCDC,DCD,FLWC)
      IMPLICIT REAL (A-Z)
      
!*---expected demand for C of the current time step
      DCDC   = INSW (DS-SSG, 0., TOTC/YG*FD)

!*---total demand for C at the time step considered
      DCD    = DCDC + MAX(0.,DCDR)/DELT
      
!*---flow of current assimilated carbon to sink
      FLWC   = MIN(DCD, DCS)
  
      RETURN
      END SUBROUTINE SINKG


!*----------------------------------------------------------------------*
!*  SUBROUTINE ASTRO  (from the SUCROS model)                           *
!*  Purpose: This subroutine calculates astronomic daylength,           *
!*           diurnal radiation characteristics such as the daily        *
!*           integral of sine of solar elevation and solar constant.    *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     * 
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DOY     R4  Daynumber (Jan 1st = 1)                       -      I  *
!*  LAT     R4  Latitude of the site                        degree   I  *
!*  INSP    R4  Inclination of sun angle for computing DDLP degree   I  *
!*  SC      R4  Solar constant                             J m-2 s-1 O  *
!*  SINLD   R4  Seasonal offset of sine of solar height       -      O  *
!*  COSLD   R4  Amplitude of sine of solar height             -      O  *
!*  DAYL    R4  Astronomic daylength (base = 0 degrees)       h      O  *
!*  DDLP    R4  Photoperiodic daylength                       h      O  *
!*  DSINBE  R4  Daily total of effective solar height       s d-1    O  *
!*                                                                      *
!*  FATAL ERROR CHECKS (execution terminated, message)                  *
!*  condition: LAT > 67, LAT < -67                                      *
!*                                                                      *
!*  FILE usage : none                                                   *
!*----------------------------------------------------------------------*
      SUBROUTINE ASTRO (DOY,LAT,INSP,SC,SINLD,COSLD,DAYL,DDLP,DSINBE)
      IMPLICIT REAL (A-Z)
      
!*---PI and conversion factor from degrees to radians
      PI    = 3.141592654
      RAD   = PI/180.
!*---check on input range of parameters
      IF (LAT.GT.67.)  STOP 'ERROR IN ASTRO: LAT> 67'
      IF (LAT.LT.-67.) STOP 'ERROR IN ASTRO: LAT>-67'

!*---declination of the sun as function of daynumber (DOY)
      DEC   = -ASIN (SIN (23.45*RAD)*COS (2.*PI*(DOY+10.)/365.))

!*---SINLD, COSLD and AOB are intermediate variables
      SINLD = SIN (RAD*LAT)*SIN (DEC)
      COSLD = COS (RAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD

!*---daylength (DAYL)
      DAYL   = 12.0*(1.+2.*ASIN (AOB)/PI)
      DDLP   = 12.0*(1.+2.*ASIN((-SIN(INSP*RAD)+SINLD)/COSLD)/PI)

      DSINB  = 3600.*(DAYL*SINLD+24.*COSLD*SQRT (1.-AOB*AOB)/PI)
      DSINBE = 3600.*(DAYL*(SINLD+0.4*(SINLD*SINLD+COSLD*COSLD*0.5))+ &
               12.0*COSLD*(2.0+3.0*0.4*SINLD)*SQRT (1.-AOB*AOB)/PI)

!*---solar constant (SC)
      SC     = 1367.*(1.+0.033*COS(2.*PI*(DOY-10.)/365.))

      RETURN
      END SUBROUTINE ASTRO


!*----------------------------------------------------------------------*
!*  SUBROUTINE TOTPT                                                    *
!*  Purpose: This subroutine calculates daily total gross photosynthesis*
!*           and transpiration by performing a Gaussian integration     *
!*           over time. At five different times of the day, temperature *
!*           and radiation are computed to determine assimilation       *
!*           and transpiration whereafter integration takes place.      *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  HOD     R4  Hour of the day                            h         I  *
!*  SC      R4  Solar constant                             J m-2 s-1 I  *
!*  SINLD   R4  Seasonal offset of sine of solar height    -         I  *
!*  COSLD   R4  Amplitude of sine of solar height          -         I  *
!*  DAYL    R4  Astronomic daylength (base = 0 degrees)    h         I  *
!*  DSINBE  R4  Daily total of effective solar height      s d-1     I  *
!*  RSD     R4  Global radiation                           J m-2 s-1 I  *
!*  TAIR    R4  Air temperature                            oC        I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  WNM     R4  daily average wind speed (>=0.1 m/s)       m s-1     I  *
!*  C3C4    R4  Crop type (=1 for C3, -1 for C4 crops)     -         I  *
!*  LAI     R4  (green)Leaf area index                     m2 m-2    I  *
!*  TLAI    R4  Total Leaf area index                      m2 m-2    I  *
!*  HT      R4  Plant height                               m         I  *
!*  LWIDTH  R4  Leaf width                                 m         I  *
!*  RD      R4  Rooting depth                              cm        I  *
!*  SD1     R4  Depth of evaporative upper soil layer      cm        I  *
!*  RSS     R4  Soil resistance,equivalent to leaf stomata s m-1     I  *
!*  BLD     R4  Leaf angle from horizontal                 degree    I  *
!*  KN      R4  Leaf nitrogen extinction coefficient       m2 m-2    I  *
!*  KW      R4  Windspeed extinction coefficient in canopy m2 m-2    I  *
!*  SLN     R4  Average leaf nitrogen content in canopy    g m-2     I  *
!*  SLNT    R4  Top-leaf nitrogen content                  g m-2     I  *
!*  SLNN    R4  Value of SLNT with small plant-N increment g m-2     I  *
!*  SLNMIN  R4  Minimum or base SLNT for photosynthesis    g m-2     I  *
!*  DWSUP   R4  Daily water supply for evapotranspiration  mm s-1    I  *
!*  CO2A    R4  Ambient CO2 concentration                  ml m-3    I  *
!*  LS      R4  Lodging severity                           -         I  *
!*  EAJMAX  R4  Energy of activation for Jmax              J mol-1   I  *
!*  XVN     R4  Slope of linearity between Vcmax & leaf N  umol/g/s  I  *
!*  XJN     R4  Slope of linearity between Jmax & leaf N   umol/g/s  I  *
!*  THETA   R4  Convexity for light response of e-transport   -      I  *
!*  WCUL    R4  Water content of the upper soil layer      m3 m-3    I  *
!*  FVPD    R4  Slope for linear effect of VPD on Ci/Ca    (kPa)-1   I  *
!*  PPCAN   R4  Potential canopy CO2 assimilation          g m-2 s-1 O  *
!*  APCANS  R4  Actual standing-canopy CO2 assimilation    g m-2 s-1 O  *
!*  APCANN  R4  APCANS with small plant-N increment        g m-2 s-1 O  *
!*  APCAN   R4  Actual canopy CO2 assimilation             g m-2 s-1 O  *
!*  PTCAN   R4  Potential canopy transpiration             mm s-1    O  *
!*  ATCAN   R4  Actual canopy transpiration                mm s-1    O  *
!*  PESOIL  R4  Potential soil evaporation                 mm s-1    O  *
!*  AESOIL  R4  Actual soil evaporation                    mm s-1    O  *
!*  DIFS    R4  Soil-air temp. difference                  oC        O  *
!*  DIFSU   R4  Sunlit leaf-air temp. diff.                oC        O  *
!*  DIFSH   R4  Shaded leaf-air temp. diff.                oC        O  *
!*  DAPAR   R4  PAR absorbed by crop canopy                J m-2 s-1 O  *
!*  RCAN    R4  Canopy resistance                          s/m       0  *
!*----------------------------------------------------------------------*
SUBROUTINE TOTPT(HOD,DS, SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR, DVP, &
                 WNM,C3C4,LAI,TLAI,HT,LWIDTH,RD,SD1,RSS,BLD,KN,KW, &
                 SLN,SLNT,SLNN,SLNMIN,FB,DWSUP,CO2A,LS,EAJMAX, &
                 XVN,XJN,THETA,WCUL,FVPD,RB,RT,RTS,PPCAN,APCANS, &
                 APCANN,APCAN,PTCAN,ATCAN,PESOIL,AESOIL,DIFS,DIFSU,DIFSH,DAPAR, &
                 RCAN, ATRJC, ATRJS, FSR, FRSU, FRSH, ARSWSU, ARSWSH)
      
            REAL, INTENT(IN) :: HOD,DS,SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR, DVP, &
                                WNM,C3C4,LAI,TLAI,HT,LWIDTH,RD,SD1,RSS,BLD,KN,KW, &
                                SLN,SLNT,SLNN,SLNMIN,FB,DWSUP,CO2A,LS,EAJMAX, &
                                XVN,XJN,THETA,WCUL,FVPD,RT,RTS !RB

            REAL, INTENT(INOUT) :: PPCAN,APCANS,APCANN,APCAN,PTCAN,ATCAN,PESOIL, &
                                   AESOIL,DIFS,DIFSU,DIFSH,DAPAR,RCAN,ATRJC,ATRJS,FSR,FRSU,FRSH,ARSWSU,ARSWSH
            REAL  :: ACO2I,ADIFS,ADIFSH,ADIFSU,ANIRSH,ANIRSU,ANRAD,APAR,APARSH
            REAL  :: APARSU,ASVP,ATMTR,ATRJSH,ATRJSU,ATSH,ATSU,AV_Albedo
            REAL  :: AV_RSWSH,AV_RSWSU,BL,CUMRSD,DATRJC,DATRJS,DNRAD,FRDF
            REAL  :: GBHC,GBHLF,GBHSH,GBHSU,IAE,IAP,IAPL,IAPN,IAPNN,IAPS,IAT,IPE
            REAL  :: IPH,IPHSOIL,IPP,IPPL,IPT,IRDL,KB,KBPNIR,KBPPAR,KDPNIR,KDPPAR
            REAL  :: NIR,NIRDF,NIRDR,NPSH,NPSHN,NPSU,NPSUN,NRADS,NRADSH,PANSH,PANSU
            REAL  :: PAR,NRADSU,PARDF,PARDR,PASSH,PASSU,PCBNIR,PCBPAR,PCDNIR,PCDPAR
            REAL  :: PHCAN,PHSH,PHSOIL,PHSU,PI,PLFSH,PLFSU,PSNIR,PSPAR,PT1,PTSH,PTSU
            REAL  :: RBHS,RBHSH,RBHSU,RBWS,RBWSH,RBWSU,RSWSH,RSWSU,SCPNIR
            REAL  :: SCPPAR,SINB,SLOPSH,SLOPSU,WND,WSUP,WSUP1,Albedo,RB, DSsw
            REAL  :: TLEAFSH, TLEAFSU

      PI   = 3.141592654

!*---output-variables set to 0. and five different times of a day(HOUR)
      PPCAN  = 0.
      APCANS = 0.
      APCANN = 0.
      APCAN  = 0.
      PTCAN  = 0.
      PHCAN  = 0.
      ATCAN  = 0.
      PESOIL = 0.
      AESOIL = 0.
      PHSOIL = 0.
      DIFS   = 0.
      DIFSU  = 0.
      DIFSH  = 0.
      DAPAR  = 0.
      DNRAD  = 0.
      DATRJC = 0.
      DATRJS = 0.
      AV_RSWSU = 0.
      AV_RSWSH = 0.
      AV_Albedo = 0.
      CUMRSD = 0.
            
!*---sine of solar elevation
      SINB  = MAX (.01, SINLD+COSLD*COS(2.*PI*(HOD-12.)/24.))
      
!*---daytime course of water supply
    WSUP = DWSUP      
    WSUP1 = WSUP*SD1/RD 
 
!*---daytime course of wind speed
      WND   = WNM         !m s-1
            
!*---total incoming PAR and NIR
      PAR   = (1.-FB)*0.5*RSD  !J m-2 s-1
      NIR   = (1.-FB)*0.5*RSD  !J m-2 s-1

!*---diffuse light fraction (FRDF) from atmospheric transmission (ATMTR)
      ATMTR = PAR/(0.5*SC*SINB) ! unitless, fraction

      IF (ATMTR.LE.0.22) THEN
         FRDF = 1.
      ELSE IF (ATMTR.GT.0.22 .AND. ATMTR.LE.0.35) THEN
         FRDF = 1.-6.4*(ATMTR-0.22)**2
      ELSE
         FRDF = 1.47-1.66*ATMTR
      ENDIF

      FRDF = MAX (FRDF, 0.15+0.85*(1.-EXP (-0.1/SINB)))

!*---incoming diffuse PAR (PARDF) and direct PAR (PARDR)
      PARDF = PAR * FRDF !J m-2 s-1
      PARDR = PAR - PARDF !J m-2 s-1

!*---incoming diffuse NIR (NIRDF) and direct NIR (NIRDR)
      NIRDF = NIR * FRDF !J m-2 s-1
      NIRDR = NIR - NIRDF !J m-2 s-1

!*---extinction and reflection coefficients
      BL    = BLD*PI/180.     !leaf angle, conversion to radians
      CALL KBEAM (SINB,BL,KB)

      SCPPAR = 0.2            !leaf scattering coefficient for PAR
      SCPNIR = 0.8            !leaf scattering coefficient for NIR
      CALL KDIFF (TLAI,BL,SCPPAR, KDPPAR)
      CALL KDIFF (TLAI,BL,SCPNIR, KDPNIR)

      CALL REFL (SCPPAR,KB, KBPPAR,PCBPAR)
      CALL REFL (SCPNIR,KB, KBPNIR,PCBNIR)

      PCDPAR = 0.057          !canopy diffuse PAR reflection coefficient
      PCDNIR = 0.389          !canopy diffuse NIR reflection coefficient
      
!*---fraction of sunlit and shaded components in canopy
!* ji 4.7.11 LAI -> TLAI
      FRSU   = 1./KB/TLAI*(1.-EXP(-KB*TLAI))
      FRSH   = 1.-FRSU

!*---leaf boundary layer conductance for canopy, sunlit and shaded leaves  !m s-1
      GBHLF  = 0.01*SQRT(WND/LWIDTH)
      RBHSU  = RB/(FRSU*LAI)     !boundary layer resistance to heat,sunlit part  !s m-1
      RBWSU  = RB/(FRSU*LAI)     !boundary layer resistance to H2O, sunlit part
      RBHSH  = RB/(FRSH*LAI)     !boundary layer resistance to heat,shaded part
      RBWSH  = RB/(FRSH*LAI)     !boundary layer resistance to H2O, shaded part
      RCAN   = WNM*10.

!*---boundary layer resistance for soil !s m-1
      RBHS   = 172.*SQRT(0.05/MAX(0.1,WND*EXP(-KW*TLAI)))
      RBWS   = 0.93*RBHS

!*---photosynthetically active nitrogen for sunlit and shaded leaves
     CALL PAN (SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH)
     CALL PAN (SLNN,SLNMIN,LAI,KN,KB, NPSUN,NPSHN)  

!*---absorbed PAR and NIR by sunlit leaves and shaded leaves
!* ji 4.7.11 LAI->TLAI
!* Ansonsten fuehrt es dazu, dass nach dem Abreifen die Albedo auf >0.7 ansteigt und vom Bestand viel
!* zu wenig Energie mehr absorbiert wird
      CALL LIGAB (SCPPAR,KB,KBPPAR,KDPPAR,PCBPAR,PCDPAR,PARDR,PARDF,TLAI,APARSU,APARSH)
      CALL LIGAB (SCPNIR,KB,KBPNIR,KDPNIR,PCBNIR,PCDNIR,NIRDR,NIRDF,TLAI,ANIRSU,ANIRSH)	
      APAR   = APARSU+APARSH !J m-2 s-1

!*---absorbed total radiation (PAR+NIR) by sunlit and shaded leaves
      ATRJSU = APARSU+ANIRSU    !J m-2 s-1
      ATRJSH = APARSH+ANIRSH    !J m-2 s-1
      ATRJC  = ATRJSH + ATRJSU  !J m-2 s-1 

!*---absorbed total radiation (PAR+NIR) by soil
      PSPAR  = 0.1                                  !soil PAR reflection
      PSNIR  = INSW(WCUL-0.5, 0.52-0.68*WCUL, 0.18) !soil NIR reflection
      ATRJS=(1.-PSPAR)*(PARDR*EXP(-KBPPAR*TLAI)+PARDF*EXP(-KDPPAR*TLAI)) &
           +(1.-PSNIR)*(NIRDR*EXP(-KBPNIR*TLAI)+NIRDF*EXP(-KDPNIR*TLAI))
      
!* ji 4.7.11 Berechnung der Albedo eingefuegt. Nachts wird die Albedo auf einen Durchschnittswert gesetzt (.2)	
     FSR = RSD-ATRJC-ATRJS
    
     if (RSD.gt.0) then
         Albedo = (RSD-ATRJC-ATRJS)/RSD
     else
         Albedo = .2
     endif

!*---instantaneous potential photosynthesis and transpiration
     CALL PPHTR(FRSU,TAIR,DVP,CO2A,C3C4,FVPD,APARSU,NPSU,RBWSU,RBHSU, &
                 RT*FRSU,ATRJSU,ATMTR,EAJMAX,XVN,XJN,THETA,PLFSU, &
                 PTSU,PHSU,RSWSU,NRADSU,SLOPSU)

     CALL PPHTR(FRSH,TAIR,DVP,CO2A,C3C4,FVPD,APARSH,NPSH,RBWSH,RBHSH, &
                 RT*FRSH,ATRJSH,ATMTR,EAJMAX,XVN,XJN,THETA,PLFSH, &
                 PTSH,PHSH,RSWSH,NRADSH,SLOPSH)

     IPP    = PLFSU + PLFSH   !gCO2/m2/s
     IPT    = PTSU + PTSH    !mm s-1
     IPH    = PHSU + PHSH    !J m-2 s-1
     ANRAD  = NRADSU + NRADSH !J m-2 s-1

!*--- PT1: Potential transpiration using water from the upper evaporative layer (mm s-1)
!*--- SD1: thickness of upper evaporative layer (cm); default value: 5 cm
!*--- RD: Rooting depth (cm)
      PT1    = IPT  * SD1/RD

!*---instantaneous potential soil evaporation
      CALL PEVAP (TAIR,DVP,RSS,RTS,RBWS,RBHS,ATRJS,ATMTR, &
                  PT1,WSUP1,IPE,IPHSOIL,NRADS)

!*---instantaneous actual soil evaporation, actual canopy
!*   transpiration and photosynthesis
      IAE    = MIN (IPE,IPE/(PT1+IPE)*WSUP1)            !actual soil evaporation mm s-1
      IAT    = MIN (IPT,PT1/(PT1+IPE)*WSUP1+WSUP-WSUP1) !actual transpiration mm s-1
      ATSU   = PTSU/IPT*IAT                             !actual transpiration of sunlit leaves mm s-1
      ATSH   = PTSH/IPT*IAT                             !actual transpiration of shaded leaves mm s-1
   
      CALL DIFLA (NRADS,IAE,RBHS,RTS, ADIFS)

      CALL APHTR (TAIR,APARSU,DVP,CO2A,C3C4,FVPD,NRADSU,ATSU,PTSU, &
                  RT*FRSU,RBHSU,RBWSU,RSWSU,SLOPSU,NPSU,NPSUN, &
                  EAJMAX,XVN,XJN,THETA,PASSU,PANSU,ADIFSU,ARSWSU)
      
      CALL APHTR (TAIR,APARSH,DVP,CO2A,C3C4,FVPD,NRADSH,ATSH,PTSH, &
                  RT*FRSH,RBHSH,RBWSH,RSWSH,SLOPSH,NPSH,NPSHN, &
                  EAJMAX,XVN,XJN,THETA,PASSH,PANSH,ADIFSH,ARSWSH)
      
      IAPS   = PASSU + PASSH !actual canopy photosynthesis gCO2 m-2 s-1
      IAPN   = PANSU + PANSH !actual canopy photosynthesis with a small N increment gCO2 m-2 s-1
      IAP    = IAPS
      IAPNN  = IAPN

!*---integration of assimilation and transpiration to a daily total
      PPCAN  = IPP
      APCANS = IAPS  
      APCANN = IAPNN 
      APCAN  = IAP
      PTCAN  = IPT   
      PHCAN  = IPH   
      ATCAN  = IAT   
      PESOIL = IPE   
      AESOIL = IAE   
      PHSOIL = IPHSOIL
      DIFS   = ADIFS 
      DIFSU  = ADIFSU
      DIFSH  = ADIFSH
      DAPAR  = APAR  
      DNRAD  = ANRAD    !net absorbed radiation by canopy
      DATRJC = ATRJC    !absorbed radiation by canopy
      DATRJS = ATRJS    !absorbed radiation by soil
      CUMRSD = RSD      !incoming solar radiation
      AV_RSWSU = RSWSU
      AV_RSWSH = RSWSH 
      AV_Albedo = Albedo
      TLEAFSU = TAIR + DIFSU
      TLEAFSH = TAIR + DIFSH

      RETURN
      END SUBROUTINE TOTPT


!*----------------------------------------------------------------------*
!* SUBROUTINE PPHTR                                                     *
!* Purpose: This subroutine calculates potential leaf photosynthesis    *
!*          and transpiration.                                          *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  FRAC    R4  Fraction of leaf classes (sunlit vs shaded) -        I  *
!*  TAIR    R4  Air temperature                            oC        I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  CO2A    R4  Ambient CO2 concentration                  ml m-3    I  *
!*  C3C4    R4  Crop type (=1. for C3, -1 for C4 crops)    -         I  *
!*  FVPD    R4  Slope for linear effect of VPD on Ci/Ca    (kPa)-1   I  *
!*  PAR     R4  Absorbed photosynth. active radiation      J m-2 s-1 I  *
!*  NP      R4  Photosynthetically active N content        g m-2     I  *
!*  RBW     R4  Leaf boundary layer resistance to water    s m-1     I  *
!*  RBH     R4  Leaf boundary layer resistance to heat     s m-1     I  *
!*  RT      R4  Turbulence resistance                      s m-1     I  *
!*  ATRJ    R4  Absorbed global radiation                  J m-2 s-1 I  *
!*  ATMTR   R4  Atmospheric transmissivity                 -         I  *
!*  EAJMAX  R4  Energy of activation for Jmax              J mol-1   I  *
!*  XVN     R4  Slope of linearity between Vcmax & leaf N  umol/g/s  I  *
!*  XJN     R4  Slope of linearity between Jmax  & leaf N  umol/g/s  I  *
!*  THETA   R4  Convexity for light response of e-transport   -      I  *
!*  PLF     R4  Potential leaf photosynthesis              gCO2/m2/s O  *
!*  PT      R4  Potential leaf transpiration               mm s-1    O  *
!*  PH      R4  Potential leaf sensible heat flux          J m-2 s-1 O  *
!*  RSW     R4  Potential stomatal resistance to water     s m-1     O  *
!*  NRADC   R4  Net leaf absorbed radiation                J m-2 s-1 O  *
!*  SLOPEL  R4  Slope of saturated vapour pressure curve   kPa oC-1  O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PPHTR(FRAC,TAIR,DVP,CO2A,C3C4,FVPD,PAR,NP,RBW,RBH,RT, &
            ATRJ,ATMTR,EAJMAX,XVN,XJN,THETA,PLF,PT,PH,RSW,NRADC,SLOPEL)
      IMPLICIT REAL (A-Z)

!*---first-round calculation to determine leaf temperature
      CALL ICO2 (TAIR,DVP,FVPD,CO2A,C3C4, SVP,FCO2I)

      CALL PHOTO(C3C4,PAR,TAIR,FCO2I,NP,EAJMAX,XVN,XJN,THETA,FPLF, &
                 FLRD)
    
      VPD    = MAX (0., SVP- DVP)
      SLOPE  = 4158.6 * SVP/(TAIR + 239.)**2
      CALL GCRSW(FPLF,FLRD,TAIR,CO2A,FCO2I,RBW,RT, FRSW)
 
      CALL PTRAN(FRSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TAIR,DVP, &
                 SLOPE, VPD, FPT, FPH, FNRADC)

      CALL DIFLA (FNRADC,FPT,RBH,RT, FDIF)

      TLEAF  = TAIR + FDIF

!*---second-round calculation to determine potential photosynthesis
!*   and transpiration
      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
      
      CALL DIFLA (FNRADC,FPT,RBH,RT, FDIF)

      TLEAF  = TAIR + FDIF

!*---third-round calculation to determine potential photosynthesis
!*   and transpiration
      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
  
      CALL DIFLA (FNRADC,FPT,RBH,RT, FDIF)

      TLEAF  = TAIR + FDIF

!*---fourth-round calculation to determine potential photosynthesis
!*   and transpiration
      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
      
      RETURN
      END SUBROUTINE PPHTR


!*----------------------------------------------------------------------*
!* SUBROUTINE PEVAP                                                     *
!* Purpose: This subroutine calculates potential soil evaporation.      * 
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  TAIR    R4  Air temperature                            oC        I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  RSS     R4  Soil resistance,equivalent to leaf stomata s m-1     I  *
!*  RTS     R4  Turbulence resistance for soil             s m-1     I  *
!*  RBWS    R4  Soil boundary layer resistance to water    s m-1     I  *
!*  RBHS    R4  Soil boundary layer resistance to heat     s m-1     I  *
!*  ATRJS   R4  Absorbed global radiation by soil          J m-2 s-1 I  *
!*  ATMTR   R4  Atmospheric transmissivity                 -         I  *
!*  PT1     R4  Potential leaf transpiration using water   mm s-1    I  *
!*              from upper evaporative soil layer                       *
!*  WSUP1   R4  Water supply from upper evaporative soil   mm s-1    I  *
!*              layer for evapotranspiration                            *
!*  PESOIL  R4  Potential soil evaporation                 mm s-1    O  *
!*  PHSOIL  R4  Potential soil sensible heat flux          J m-2 s-1 O  *
!*  NRADS   R4  Net soil absorbed radiation                J m-2 s-1 O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PEVAP (TAIR,DVP,RSS,RTS,RBWS,RBHS,ATRJS,ATMTR, &
                        PT1,WSUP1,PESOIL,PHSOIL,NRADS)
      IMPLICIT REAL (A-Z)

!*--- first-round calculation to estimate soil surface temperature (TAVS)
	  SVP    = 0.611*EXP(17.4*TAIR/(TAIR+239.))
      VPD    = MAX (0., SVP-DVP)
      SLOPE  = 4158.6 * SVP/(TAIR + 239.)**2
      CALL PTRAN(RSS,RTS,RBWS,RBHS,ATRJS,ATMTR,1.,TAIR,DVP, &
                 SLOPE,VPD, FPE, FPH, FNRADS)
      FPESOL = MAX(0., FPE)
      FAESOL = MIN(FPESOL,FPESOL/(PT1+FPESOL)*WSUP1)
      CALL DIFLA (FNRADS,FAESOL,RBHS,RTS, FDIFS)
      TAVS   = TAIR + FDIFS

!*---second-round calculation to estimate potential soil evaporation
      SVPS   = 0.611*EXP(17.4*TAVS/(TAVS+239.))
      SLOPES = (SVPS-SVP)/NOTNUL(FDIFS)

      CALL PTRAN(RSS,RTS,RBWS,RBHS,ATRJS,ATMTR,1.,TAVS,DVP, &
                 SLOPES,VPD, PE, PH, NRADS)
      PESOIL = MAX(0., PE)
      PHSOIL = PH
      
      RETURN
      END SUBROUTINE PEVAP

!*----------------------------------------------------------------------*
!* SUBROUTINE APHTR                                                     *
!* Purpose: This subroutine calculates actual leaf photosynthesis when  *
!*          water stress occurs.                                        *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  TAIR    R4  Air temperature                            oC        I  *
!*  PAR     R4  Absorbed photosynth. active radiation      J m-2 s-1 I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  CO2A    R4  Ambient CO2 concentration                  ml m-3    I  *
!*  C3C4    R4  Crop type (=1. for C3, -1 for C4 crops)    -         I  *
!*  FVPD    R4  Slope for linear effect of VPD on Ci/Ca    (kPa)-1   I  *
!*  NRADC   R4  Net leaf absorbed radiation                J m-2 s-1 I  *
!*  AT      R4  Actual leaf transpiration                  mm s-1    I  *
!*  PT      R4  Potential leaf transpiration               mm s-1    I  *
!*  RT      R4  Turbulence resistance                      s m-1     I  *
!*  RBH     R4  Leaf boundary layer resistance to heat     s m-1     I  *
!*  RBW     R4  Leaf boundary layer resistance to water    s m-1     I  *
!*  RSW     R4  Potential stomatal resistance to water     s m-1     I  *
!*  SLOPEL  R4  Slope of saturated vapour pressure curve   kPa oC-1  I  *
!*  NP      R4  Photosynthet. active leaf N content        g m-2     I  *
!*  NPN     R4  NP with small plant-N increment            g m-2     I  *
!*  EAJMAX  R4  Energy of activation for Jmax              J mol-1   I  *
!*  XVN     R4  Slope of linearity between Vcmax & leaf N  umol/g/s  I  *
!*  XJN     R4  Slope of linearity between Jmax  & leaf N  umol/g/s  I  *
!*  THETA   R4  Convexity for light response of e-transport   -      I  *
!*  PLFAS   R4  Actual leaf photosynthesis                 gCO2/m2/s O  *
!*  PLFAN   R4  PLFAS with small plant-N increment         gCO2/m2/s O  *
!*  ADIF    R4  Actual leaf-air temperature difference     oC        O  *
!*  ARSW    R4  Actual stomatal resistance to water        s m-1     O  *
!*----------------------------------------------------------------------*
      SUBROUTINE APHTR(TAIR,PAR,DVP,CO2A,C3C4,FVPD,NRADC,AT,PT,RT,RBH, &
                 RBW,RSW,SLOPEL,NP,NPN,EAJMAX,XVN,XJN,THETA, &
                 PLFAS,PLFAN,ADIF,ARSW)
      IMPLICIT REAL (A-Z)

      PSYCH  = 0.067            !psychrometric constant (kPa/oC)

!*---leaf temperature if water stress occurs
      CALL DIFLA (NRADC,AT,RBH,RT, ADIF)
      ATLEAF = TAIR + ADIF

!*---stomatal resistance to water if water stress occurs
      ARSW = (PT-AT)*(SLOPEL*(RBH+RT)+PSYCH*(RBW+RT))/AT/PSYCH+PT/AT*RSW

!*---potential photosynthesis at the new leaf temperature
      CALL ICO2 (ATLEAF,DVP,FVPD,CO2A,C3C4, SVPA,ACO2I)
      CALL PHOTO(C3C4,PAR,ATLEAF,ACO2I,NPN,EAJMAX,XVN,XJN,THETA,APLFN,ARDN)
      CALL PHOTO(C3C4,PAR,ATLEAF,ACO2I,NP,EAJMAX,XVN,XJN,THETA,APLF,ARD)

!*---actual photosynthesis under water stress condition
      PLFAS  = (1.6*RSW+1.3*RBW+RT)/(1.6*ARSW+1.3*RBW+RT)*(APLF-ARD)+ARD
      PLFAN  = (1.6*RSW+1.3*RBW+RT)/(1.6*ARSW+1.3*RBW+RT)*(APLFN-ARDN)+ARDN
      
      RETURN
      END SUBROUTINE APHTR


!*----------------------------------------------------------------------*
!* SUBROUTINE PTRAN                                                     *
!* Purpose: This subroutine calculates leaf transpiration, using the    *
!*          Penman-Monteith equation                                    *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  RSW     R4  Potential stomatal resistance to water     s m-1     I  *
!*  RT      R4  Turbulence resistance                      s m-1     I  *
!*  RBW     R4  Leaf boundary layer resistance to water    s m-1     I  *
!*  RBH     R4  Leaf boundary layer resistance to heat     s m-1     I  *
!*  ATRJ    R4  Absorbed global radiation                  J m-2 s-1 I  *
!*  ATMTR   R4  Atmospheric transmissivity                 -         I  *
!*  FRAC    R4  Fraction of leaf classes (sunlit vs shaded)-         I  *
!*  TLEAF   R4  Leaf temperature                           oC        I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  SLOPE   R4  Slope of saturated vapour pressure curve   kPa oC-1  I  *
!*  VPD     R4  Saturation vapour pressure deficit of air  kPa       I  *
!*  PT      R4  Potential leaf transpiration               mm s-1    O  *
!*  PH      R4  Potential leaf sensible heat flux          J m-2 s-1 O  * 
!*  NRADC   R4  Net leaf absorbed radiation                J m-2 s-1 O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PTRAN (RSW,RT,RBW,RBH,ATRJ, &
                      ATMTR,FRAC,TLEAF,DVP,SLOPE,VPD,PT,PH,NRADC)
      IMPLICIT REAL (A-Z)

!*---some physical constants
      BOLTZM = 5.668E-8         !Stefan-Boltzmann constant(J/m2/s/K4)
      LHVAP  = 2.4E6            !latent heat of water vaporization(J/kg)
      VHCA   = 1200.            !volumetric heat capacity (J/m3/oC)
      PSYCH  = 0.067            !psychrometric constant (kPa/oC)

!*---net absorbed radiation
      CLEAR  = MAX(0., MIN(1., (ATMTR-0.25)/0.45))    !sky clearness
      BBRAD  = BOLTZM*(TLEAF +273.)**4
      RLWN   = BBRAD*(0.56-0.079*SQRT(DVP*10.))*(0.1+0.9*CLEAR)*FRAC
      NRADC  = ATRJ - RLWN

!*---intermediate variable related to resistances
      PSR    = PSYCH*(RBW+RT+RSW)/(RBH+RT)

!*---Compute PT
!*---radiation-determined term
      PTR    = NRADC*SLOPE/(SLOPE+PSR)/LHVAP

!*---vapour pressure-determined term
      PTD    = (VHCA*VPD/(RBH+RT))/(SLOPE+PSR)/LHVAP
      
!*---potential evaporation or transpiration
      PT     = MAX(1.E-10,PTR+PTD)

!*---ji
!*---Compute PH in W/m2 (see Bolan p. 202)
!*---radiation-determined term
      PHR    = NRADC*PSR/(SLOPE+PSR)

!*---vapour pressure-determined term
      PHD    = (VHCA*VPD/(RBH+RT))/(SLOPE+PSR)

!*---potential evaporation or transpiration
      PH     = PHR-PHD

      RETURN
      END SUBROUTINE PTRAN


!*----------------------------------------------------------------------*
!*  SUBROUTINE DIFLA                                                    *
!*  Purpose: This subroutine calculates leaf(canopy)-air temperature    *
!*           differential.                                              *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  NRADC   R4  Net leaf absorbed radiation                J m-2 s-1 I  *
!*  PT      R4  Potential leaf transpiration               mm s-1    I  *
!*  RBH     R4  Leaf boundary layer resistance to heat     s m-1     I  *
!*  RT      R4  Turbulence resistance                      s m-1     I  *
!*  DIF     R4  Leaf-air temperature difference            oC        O  *
!*----------------------------------------------------------------------*
      SUBROUTINE DIFLA (NRADC,PT,RBH,RT, DIF)
      IMPLICIT REAL  (A-Z)
      
      LHVAP  = 2.4E6            !latent heat of water vaporization(J/kg)
      VHCA   = 1200.            !volumetric heat capacity (J/m3/oC)

      DIF    = LIMIT (-25., 25., (NRADC-LHVAP*PT)*(RBH+RT)/VHCA)

      RETURN
      END SUBROUTINE DIFLA
!*----------------------------------------------------------------------*
!*  SUBROUTINE ICO2                                                     *
!*  Purpose: This subroutine calculates the internal CO2 concentration  *
!*           as affected by vapour pressure deficit.                    *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  TLEAF   R4  Leaf temperature                           oC        I  *
!*  DVP     R4  Vapour pressure                            kPa       I  *
!*  FVPD    R4  Slope for linear effect of VPDL on Ci/Ca   (kPa)-1   I  *
!*  CO2A    R4  Ambient CO2 concentration                  ml m-3    I  *
!*  C3C4    R4  Crop type (=1. for C3, -1 for C4 crops)    -         I  *
!*  SVPL    R4  Saturated vapour pressure of leaf          kPa       O  *
!*  CO2I    R4  intercellular CO2 concentration            ml m-3    O  *
!*----------------------------------------------------------------------*
      SUBROUTINE ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      IMPLICIT REAL (A-Z)
      
!*---air-to-leaf vapour pressure deficit
      SVPL   = 0.611 * EXP(17.4 * TLEAF / (TLEAF + 239.))
      VPDL   = MAX  (0., SVPL - DVP)

!*---Michaelis-Menten const. for CO2 at 25oC (umol/mol)
      KMC25  = INSW(C3C4, 650., 404.9) !greater KMC25 for C4 than C3

!*---Michaelis-Menten const. for O2 at 25oC (mmol/mol)
      KMO25  = INSW(C3C4, 450., 278.4) !greater KMO25 for C4 than C3

!*---CO2 compensation point in absence of dark respiration (GAMMAX)
      O2     = 210.    !oxygen concentration(mmol/mol)
      EAVCMX = 65330.  !energy of activation for Vcmx(J/mol)
      EAKMC  = 79430.  !energy of activation for KMC (J/mol)
      EAKMO  = 36380.  !energy of activation for KMO (J/mol)
      EARD   = 46390.  !energy of activation for dark respiration(J/mol)
      RDVX25 = 0.0089  !ratio of dark respiration to Vcmax at 25oC
      TO     = 298.15

      KMC    = KMC25*EXP((1./TO-1./(TLEAF+273.))*EAKMC/8.314)
      KMO    = KMO25*EXP((1./TO-1./(TLEAF+273.))*EAKMO/8.314)
      GAMMAX = 0.5*EXP(-3.3801+5220./(TLEAF+273.)/8.314)*O2*KMC/KMO

!*---CO2 compensation point (GAMMA)
      RDVCX  = RDVX25*EXP((1./TO-1./(TLEAF+273.))*(EARD-EAVCMX)/8.314)
      GAMMA0 = (GAMMAX+RDVCX*KMC*(1.+O2/KMO))/(1.-RDVCX)
      GAMMA_  = INSW (C3C4, GAMMA0/10., GAMMA0)

!*---internal/ambient CO2 ratio, based on data of Morison & Gifford (1983)
      RCICA  = 1.-(1.-GAMMA_/CO2A)*(0.14+FVPD*VPDL)

!*---intercellular CO2 concentration
      CO2I   = RCICA * CO2A
      
      RETURN
      END SUBROUTINE ICO2


!*----------------------------------------------------------------------*
!*  SUBROUTINE GCRSW                                                    *
!*  Purpose: This subroutine calculates overall leaf conductance        *
!*           for CO2 (GC) and the stomatal resistance to water (RSW).   *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  PLEAF   R4  Gross leaf photosynthesis                  gCO2/m2/s I  *
!*  RDLEAF  R4  Leaf dark respiration                      gCO2/m2/s I  *
!*  TLEAF   R4  Leaf temperature                           oC        I  *
!*  CO2A    R4  Ambient CO2 concentration                  ml m-3    I  *
!*  CO2I    R4  Internal CO2 concentration                 ml m-3    I  *
!*  RT      R4  Turbulence resistance                      s m-1     I  *
!*  RBW     R4  Leaf boundary layer resistance to water    s m-1     I  *
!*  RSW     R4  Potential stomatal resistance to water     s m-1     O  *
!*----------------------------------------------------------------------*
      SUBROUTINE GCRSW (PLEAF,RDLEAF,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      IMPLICIT REAL (A-Z)
      
!*---potential conductance for CO2
!*ji 13.7.11 MAX routine faengt negative Leitfaehigkeiten ab
      GC  = MAX(1E-6,(PLEAF-RDLEAF)*(273.+TLEAF)/0.53717/(CO2A-CO2I))

!*---potential stomatal resistance to water
      RSW = MAX(1E-30, 1./GC - RBW*1.3 - RT)/1.6

      RETURN
      END SUBROUTINE GCRSW


!*----------------------------------------------------------------------*
!*  SUBROUTINE PAN                                                      *
!*  Purpose: This subroutine calculates photosynthetically active       *
!*           nitrogen content for sunlit and shaded parts of canopy.    *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  SLNT    R4  Top-leaf nitrogen content                  g m-2     I  *
!*  SLNMIN  R4  Minimum or base SLNT for photosynthesis    g m-2     I  *
!*  LAI     R4  (green)Leaf area index                     m2 m-2    I  *
!*  KN      R4  Leaf nitrogen extinction coefficient       m2 m-2    I  *
!*  KB      R4  Direct beam radiation extinction coeff.    m2 m-2    I  *
!*  NPSU    R4  Photosynthet. active N for sunlit leaves   g m-2     O  *
!*  NPSH    R4  Photosynthet. active N for shaded leaves   g m-2     O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PAN(SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH)
      IMPLICIT REAL (A-Z)
      
!*---total photosynthetic nitrogen in canopy
      NPC   = SLNT*(1.-EXP(-KN*LAI))/KN-SLNMIN*LAI

!*---photosynthetic nitrogen for sunlit and shaded parts of canopy
      NPSU  = SLNT*(1.-EXP(-(KN+KB)*LAI))/(KN+KB)-SLNMIN*(1.-EXP(-KB*LAI))/KB
      NPSH  = NPC-NPSU
      
      RETURN
      END SUBROUTINE PAN


!*----------------------------------------------------------------------*
!*  SUBROUTINE PHOTO                                                    *
!*  Purpose: This subroutine calculates leaf photosynthesis and dark    *
!*           respiration, based on a renewed Farquhar biochemistry      *
!*           (cf Yin et al.2004. Plant, Cell & Environment 27:1211-1222)*
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  C3C4    R4  Crop type (=1. for C3, -1. for C4 crops)    -        I  *
!*  PAR     R4  Leaf absorbed photosynth. active radiance  J m-2 s-1 I  *
!*  TLEAF   R4  Leaf temperature                           oC        I  *
!*  CO2I    R4  Intercellular CO2 concentration            ml m-3    I  *
!*  NP      R4  Photosynthetically active leaf N content   g m-2     I  *
!*  EAJMAX  R4  Energy of activation for Jmax              J mol-1   I  *
!*  XVN     R4  Slope of linearity between Vcmax & leaf N  umol/g/s  I  *
!*  XJN     R4  Slope of linearity between Jmax  & leaf N  umol/g/s  I  *
!*  THETA   R4  Convexity for light response of e-transport -        I  *
!*  PLEAF   R4  Gross leaf photosynthesis                  gCO2/m2/s O  *
!*  RDLEAF  R4  Leaf dark respiration                      gCO2/m2/s O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PHOTO(C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN, &
                       THETA,PLEAF,RDLEAF)
      IMPLICIT REAL (A-Z)
      
!*---Michaelis-Menten constants for CO2 and O2 at 25oC
      IF (C3C4.LT.0.) THEN
        KMC25  = 650.   !greater KMC25 for C4 than C3; unit:(umol/mol)
        KMO25  = 450.   !greater KMO25 for C4 than C3; unit:(mmol/mol)
      ELSE
        KMC25  = 404.9  !unit:(umol/mol)
        KMO25  = 278.4  !unit:(mmol/mol)
      ENDIF

!*---other constants related to the Farquhar-type photosynthesis model
      O2     = 210.    !oxygen concentration(mmol/mol)
      EAVCMX = 65330.  !energy of activation for Vcmx(J/mol)
      EAKMC  = 79430.  !energy of activation for KMC (J/mol)
      EAKMO  = 36380.  !energy of activation for KMO (J/mol)
      EARD   = 46390.  !energy of activation for dark respiration(J/mol)
      DEJMAX = 200000. !energy of deactivation for JMAX (J/mol)
      SJ     = 650.    !entropy term in JT equation (J/mol/K)
      PHI2M  = 0.85    !maximum electron transport efficiency of PS II
      HH     = 3.      !number of protons required to synthesise 1 ATP
      KTMP   = 1.0     !Factor for reducing photosynthesis in case of T<5C
      JTMAX  = 3.12
      TO     = 298.15
      
!*---PAR photon flux in umol/m2/s absorbed by leaf photo-systems
      UPAR   = 4.56*PAR !4.56 conversion factor in umol/J

!*---Michaelis-Menten constants for CO2 and O2 respectively
      KMC    = KMC25*EXP((1./TO-1./(TLEAF+273.))*EAKMC/8.314)
      KMO    = KMO25*EXP((1./TO-1./(TLEAF+273.))*EAKMO/8.314)

!*---CO2 compensation point in the absence of dark respiration
      GAMMAX = 0.5*EXP(-3.3801+5220./(TLEAF+273.)/8.314)*O2*KMC/KMO

!*---Arrhenius function for the effect of temperature on carboxylation
     VCT    =    EXP((1./TO-1./(TLEAF+273.))*EAVCMX/8.314)

!*---function for the effect of temperature on electron transport
     JT     =    EXP((1./TO-1./(TLEAF+273.))*EAJMAX/8.314)* &
                 (1.+EXP(SJ/8.314-DEJMAX/TO/8.314))/ &
                 (1.+EXP(SJ/8.314-1./(TLEAF+273.) *DEJMAX/8.314))

     VCMX   = XVN*VCT*NP
     JMAX   = XJN*JT *NP
  
!*---CO2 concentration at carboxylation site & electron pathways and
!*   their stoichiometries
      FPSEUD = 0.           !assuming no pseudocyclic e- transport
      IF (C3C4.LT.0.) THEN
        ZZ   = 0.2          !CO2 leakage from bundle-sheath to mesophyll
        CC   = 10.*CO2I     !to mimic C4 CO2 concentrating mechanism
        SF   = 2.*(CC-GAMMAX)/(1.-ZZ)
        FQ   = 1.- FPSEUD- 2.*(4.*CC+8.*GAMMAX)/HH/(SF+3.*CC+7.*GAMMAX)
        FCYC = FQ
      ELSE
        CC   = CO2I
        SF   = 0.
        FQ   = 0.
        FCYC = 1.-(FPSEUD*HH*(SF+3.*CC+7.*GAMMAX)/(4.*CC+8.*GAMMAX)+1.)/ &
                         (HH*(SF+3.*CC+7.*GAMMAX)/(4.*CC+8.*GAMMAX)-1.)
      ENDIF

!*--- electron transport rate in dependence on PAR photon flux
      ALPHA2 = (1.-FCYC)/(1.+(1.-FCYC)/PHI2M)
      X      = ALPHA2*UPAR/MAX(1.E-10,JMAX)
      J2     = JMAX*(1.+X-((1.+X)**2.-4.*X*THETA)**0.5)/2./THETA

!*---rates of carboxylation limited by Rubisco and electron transport
      VC     = VCMX * CC/(CC + KMC*(O2/KMO+1.))
      VJ     =  J2 * CC*(2.+FQ-FCYC)/HH/(SF+3.*CC+7.*GAMMAX)/(1.-FCYC)

!*---gross rate of leaf photosynthesis
      ALF    = (1.-GAMMAX/CC)*MIN(VC,VJ)
      PLEAF  = MAX(1.E-10, (1.E-6)*44.*ALF)

!*---rate of leaf dark respiration
      RDVX25 = 0.0089      !ratio of dark respiration to Vcmax at 25oC
      RDT    = EXP((1./TO-1./(TLEAF+273.))*EARD/8.314)
      RDLEAF = (1.E-6)*44. *RDVX25*(XVN*NP) * RDT

      RETURN
      END SUBROUTINE PHOTO


!*----------------------------------------------------------------------*
!*  SUBROUTINE REFL                                                     *
!*  Purpose: This subroutine calculates reflection coefficients.        *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  SCP     R4  Leaf scattering coefficient                -         I  *
!*  KB      R4  Direct beam radiation extinction coeff.    m2 m-2    I  *
!*  KBP     R4  Scattered beam radiation extinction coeff. m2 m-2    O  *
!*  PCB     R4  Canopy beam radiation reflection coeff.    -         O  *
!*----------------------------------------------------------------------*
      SUBROUTINE REFL (SCP,KB, KBP,PCB)
      IMPLICIT REAL (A-Z)
      
!*--- scattered beam radiation extinction coefficient
      KBP    = KB*SQRT(1.-SCP)

!*---canopy reflection coefficient for horizontal leaves
      PH     = (1.-SQRT(1.-SCP))/(1.+SQRT(1.-SCP))

!*---Canopy beam radiation reflection coefficient
      PCB    = 1.-EXP(-2.*PH*KB/(1.+KB))

      RETURN
      END SUBROUTINE REFL


!*----------------------------------------------------------------------*
!*  SUBROUTINE LIGAB                                                    *
!*  Purpose: This subroutine calculates absorbed light for sunlit and   *
!*           shaded leaves.                                             *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  SCP     R4  Leaf scattering coefficient                -         I  *
!*  KB      R4  Direct beam radiation extinction coeff.    m2 m-2    I  *
!*  KBP     R4  Scattered beam radiation extinction coeff. m2 m-2    I  *
!*  KDP     R4  Diffuse radiation extinction coefficient   m2 m-2    I  *
!*  PCB     R4  Canopy beam radiation reflection coeff.    -         I  *
!*  PCD     R4  Canopy diffuse radiation reflection coeff. -         I  *
!*  IB0     R4  Incident direct-beam radiation             J m-2 s-1 I  *
!*  ID0     R4  Incident diffuse radiation                 J m-2 s-1 I  *
!*  LAI     R4  (green)Leaf area index                     m2 m-2    I  *
!*  ISU     R4  Absorbed radiation by sunlit leaves        J m-2 s-1 O  *
!*  ISH     R4  Absorbed radiation by shaded leaves        J m-2 s-1 O  *
!*----------------------------------------------------------------------*
      SUBROUTINE LIGAB (SCP,KB,KBP,KDP,PCB,PCD,IB0,ID0,LAI, ISU,ISH)
      IMPLICIT REAL (A-Z)
      
!*---total absorbed light by canopy
      IC     = (1.-PCB)*MAX(1e-30,IB0)*(1.-EXP(-KBP*LAI))+ &
               (1.-PCD)*MAX(1e-30,ID0)*(1.-EXP(-KDP*LAI))
      
!*---absorbed light by sunlit and shaded fractions of canopy
      ISU    = (1.-SCP)*MAX(1e-30,IB0)*(1.-EXP(-KB *LAI))+(1.-PCD)*MAX(1e-30,ID0)/(KDP+KB)* &
               KDP*(1.-EXP(-(KDP+KB)*LAI))+MAX(1e-30,IB0)*((1.-PCB)/(KBP+KB)*KBP* &
               (1.-EXP(-(KBP+KB)*LAI))-(1.-SCP)*(1.-EXP(-2.*KB*LAI))/2.)

      ISH    = IC-ISU

      RETURN
      END SUBROUTINE LIGAB


!*----------------------------------------------------------------------*
!*  SUBROUTINE KBEAM                                                    *
!*  Purpose: This subroutine calculates extinction coefficient for      *
!*           direct beam radiation.                                     *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  SINB    R4  Sine of solar elevation                    -         I  *
!*  BL      R4  Leaf angle (from horizontal)               radians   I  *
!*  KB      R4  Direct beam radiation extinction coeff.    m2 m-2    O  *
!*----------------------------------------------------------------------*
      SUBROUTINE KBEAM (SINB,BL, KB)
      IMPLICIT REAL (A-Z)
      
!*---solar elevation in radians
      B      = ASIN(SINB)
      
!*---average projection of leaves in the direction of a solar beam
      IF (SINB.GE.SIN(BL)) THEN
          OAV = SINB*COS(BL)
      ELSE
          OAV = 2./3.141592654*(SINB*COS(BL)*ASIN(TAN(B)/TAN(BL)) &
          +((SIN(BL))**2-SINB**2)**0.5)
      ENDIF

!*---beam radiation extinction coefficient
      KB     = OAV/SINB

      RETURN
      END SUBROUTINE KBEAM


!*----------------------------------------------------------------------*
!*  SUBROUTINE KDIFF                                                    *
!*  Purpose: This subroutine calculates extinction coefficient for      *
!*           diffuse radiation.                                         *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  LAI     R4  Total leaf area index                      m2 m-2    I  *
!*  BL      R4  Leaf angle (from horizontal)               radians   I  *
!*  SCP     R4  Leaf scattering coefficient                -         I  *
!*  KDP     R4  Diffuse radiation extinction coefficient   m2 m-2    O  *
!*----------------------------------------------------------------------*
      SUBROUTINE KDIFF (LAI,BL,SCP, KDP)
      IMPLICIT REAL (A-Z)
      
      PI    = 3.141592654

!*---extinction coefficient of beam lights from 15, 45 and 75 elevations
      CALL KBEAM (SIN(15.*PI/180.),BL, KB15)
      CALL KBEAM (SIN(45.*PI/180.),BL, KB45)
      CALL KBEAM (SIN(75.*PI/180.),BL, KB75)

!*---diffuse light extinction coefficient
      KDP   = -1./LAI*LOG(0.178*EXP(-KB15*(1.-SCP)**0.5*LAI) &
                        +0.514*EXP(-KB45*(1.-SCP)**0.5*LAI) &
                        +0.308*EXP(-KB75*(1.-SCP)**0.5*LAI))

      RETURN
      END SUBROUTINE KDIFF

!*----------------------------------------------------------------------*
!*  FUNCTION INTGRL                                                    *
!*  Purpose: This function integrates a differential equation           *
!*  using the Euler method. Substitutes the intrinsic FST function      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  POOL    R4  Currenmt stock size                         g X m-2  I  *
!*  RATE    R4  Rate of change                               1/d     I  *
!*  DT      R4  Time step                                    d       I  *
!*  INTGRL  R4  Returns the stock size in next time step    g X m-2  0  *
FUNCTION INTGRL(POOL, RATE, DT)
IMPLICIT NONE
real:: INTGRL, POOL, RATE, DT

INTGRL = POOL + RATE*DT

RETURN
END FUNCTION INTGRL

!*----------------------------------------------------------------------*
!*  FUNCTION NOTNUL                                                     *
!*  Y = NOTNUL(X)                                                       *
!*  Y is equal to X but 1.0 in case of x=0.0. Note that X is            *
!*  evaluated without any tolerance interval                            *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  X       R4  Variable to be checked                               I  *
!*----------------------------------------------------------------------*
FUNCTION NOTNUL(X)
IMPLICIT NONE
real:: NOTNUL, X

if (X.ne.0) then
   NOTNUL = X
else
   NOTNUL = 1
endif

RETURN
END FUNCTION NOTNUL

!*----------------------------------------------------------------------*
!*  FUNCTION LIMIT                                                      *
!*  Y = LIMIT(XL, XH, X)                                                *
!*  Y is equal to X but limited between XL and XH                       *
!*  Y - Returned as X bounded on [Xl,XH]                                *
!*  XL- Lower bound of X                                                *
!*  XH- Upper bound of X                                                *
!*  ----   ---- -------                                                 *
!*  X       R4  Variable to be checken                               I  *
!*----------------------------------------------------------------------*
FUNCTION LIMIT(XL, XH, X)
IMPLICIT NONE
real:: LIMIT, XL, XH, X

if (X.ge.XL.and.X.le.XH) then
   LIMIT = X
else
   if (X.gt.XH) then
      LIMIT = XH
   else
      LIMIT = XL
   endif 
endif

RETURN
END FUNCTION LIMIT

!*----------------------------------------------------------------------*
!*  FUNCTION INSW                                                       *
!*  Y = INSW(X, Y1, Y2)                                                 *
!*  Input switch. Y is set equal to Y1 orY2 depending on the value of X *
!*  Y - Returned as either Y1 or Y2                                     *
!*  X - Control variable                                                *
!*  Y1- Returned value of Y if X<0                                      * 
!*  Y2- Returned value of Y if X>=0                                     *
!*  ----   ---- -------                                                 *
!*  X       R4  Variable to be checken                               I  *
!*----------------------------------------------------------------------*
FUNCTION INSW(X, Y1, Y2)
implicit none
real :: INSW, X, Y1, Y2

if (X.lt.0) then
    INSW = Y1
else
    INSW = Y2
endif

RETURN
END FUNCTION INSW


!*----------------------------------------------------------------------*
!*  FUNCTION REAAND                                                     *
!*  Y = REAAND(X1, X2)                                                  *
!*  Returns 1.0 if both input variables are positive, otherwise Y=0.0   *
!*  ----   ---- -------                                                 *
!*  X1      R4  1. variable to be checked                            I  *
!*  X2      R4  2. variable to be checked                            I  *
!*----------------------------------------------------------------------*
FUNCTION REAAND(X1, X2)
implicit none
real :: REAAND, X1, X2

if (X1.gt.0.and.X2.gt.0) then
    REAAND = 1.
else
    REAAND = 0.
endif

RETURN
END FUNCTION REAAND


!*----------------------------------------------------------------------*
!*  FUNCTION REAnOR                                                     *
!*  Y = REANOR(X1, X2)                                                  *
!*  Returns 1.0 if both input variables are less than or equal to 0.,  *
!*  otherwise Y=0.
!*  ----   ---- -------                                                 *
!*  X1      R4  1. variable to be checked                            I  *
!*  X2      R4  2. variable to be checked                            I  *
!*----------------------------------------------------------------------*
FUNCTION REANOR(X1, X2)
implicit none
real :: REANOR, X1, X2

if (X1.le.0.and.X2.le.0) then
    REANOR = 1.
else
    REANOR = 0.
endif

RETURN
END FUNCTION REANOR

END MODULE module_sf_gecros
