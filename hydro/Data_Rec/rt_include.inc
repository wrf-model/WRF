   TYPE RT_FIELD  
   INTEGER :: IX, JX
   logical initialized
  REAL    :: DX,GRDAREART,SUBFLORT,WATAVAILRT,QSUBDRYRT,QSUBBDRYTRT
  REAL    :: SFHEAD1RT,INFXS1RT,QSTRMVOLTRT,QBDRYTRT,SFHEADRT,ETPND1,INFXSRTOT
  REAL    :: LAKE_INFLOTRT,accsuminfxs,diffsuminfxs,RETDEPFRAC
  REAL    :: VERTKSAT,l3temp,l4temp,l3moist,l4moist,RNOF1TOT,RNOF2TOT,RNOF3TOT
  INTEGER :: IXRT,JXRT,vegct
  INTEGER :: AGGFACYRT, AGGFACXRT, KRTel_option, FORC_TYP
  INTEGER :: SATLYRCHKRT,DT_FRACRT
  INTEGER ::  LAKE_CT, STRM_CT
  REAL                                 :: RETDEP_CHAN  ! Channel retention depth
  INTEGER :: NLINKS  !maximum number of unique links in channel
  INTEGER :: NLAKES  !number of lakes
  INTEGER :: MAXORDER !maximum stream order
  integer :: timestep_flag    ! 1 cold start run else continue run


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR ROUTING
  INTEGER, allocatable, DIMENSION(:,:)      :: CH_NETRT !-- keeps track of the 0-1 channel network
  INTEGER, allocatable, DIMENSION(:,:)      :: CH_NETLNK !-- assigns a unique value to each channel gridpoint, called links
  REAL,    allocatable, DIMENSION(:,:)      :: LATVAL,LONVAL !-- lat lon
  REAL,    allocatable, DIMENSION(:,:)      :: TERRAIN
  REAL, allocatable,    DIMENSION(:)        :: CHLAT,CHLON   !  channel lat and lon
  ! INTEGER, allocatable, DIMENSION(:,:)      :: LAKE_MSKRT, BASIN_MSK,LAK_1K
  INTEGER, allocatable, DIMENSION(:,:)      :: LAKE_MSKRT, LAK_1K
  INTEGER, allocatable, DIMENSION(:,:)      :: g_LAK_1K
  REAL,    allocatable, DIMENSION(:,:)      :: ELRT,SOXRT,SOYRT,OVROUGHRT,RETDEPRT
  REAL,    allocatable, DIMENSION(:,:,:)    :: SO8RT
  INTEGER,    allocatable, DIMENSION(:,:,:) :: SO8RT_D, SO8LD_D
  REAL,    allocatable, DIMENSION(:,:)      :: SO8LD_Vmax
  REAL   Vmax
  REAL,    allocatable, DIMENSION(:,:)      :: SFCHEADRT,INFXSRT,LKSAT,LKSATRT
  REAL,    allocatable, DIMENSION(:,:)      :: SFCHEADSUBRT,INFXSUBRT,LKSATFAC
  REAL,    allocatable, DIMENSION(:,:)      :: QSUBRT,ZWATTABLRT,QSUBBDRYRT,SOLDEPRT
  REAL,    allocatable, DIMENSION(:,:)      :: SUB_RESID
  REAL,    allocatable, DIMENSION(:,:)      :: q_sfcflx_x,q_sfcflx_y
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR GW/Baseflow
  INTEGER :: numbasns
  INTEGER, allocatable, DIMENSION(:,:)   :: GWSUBBASMSK  !GW basin mask grid
  REAL,    allocatable, DIMENSION(:,:)   :: qinflowbase  !strm inflow/baseflow from GW
  REAL,    allocatable, DIMENSION(:,:)   :: SOLDRAIN     !time-step drainage
  INTEGER, allocatable, DIMENSION(:,:)   :: gw_strm_msk  !GW basin mask grid
  REAL,    allocatable, DIMENSION(:)     :: z_gwsubbas   !depth in GW bucket
  REAL,    allocatable, DIMENSION(:)     :: qin_gwsubbas !flow to GW bucket
  REAL,    allocatable, DIMENSION(:)     :: qout_gwsubbas!flow from GW bucket
  REAL,    allocatable, DIMENSION(:)     :: gwbas_pix_ct !ct of strm pixels in
  REAL,    allocatable, DIMENSION(:)     :: basns_area   !basin area
  REAL,    allocatable, DIMENSION(:)     :: node_area   !nodes area

  REAL,    allocatable, DIMENSION(:)     :: z_q_bas_parm !GW bucket disch params
  INTEGER, allocatable, DIMENSION(:)     :: ct2_bas       !ct of lnd pixels in basn
  REAL,    allocatable, DIMENSION(:)     :: bas_pcp      !sub-basin avg'd pcp
  INTEGER                                :: bas,bas_id
  CHARACTER(len=19)                      :: header
  CHARACTER(len=1)                       :: jnk
  REAL, allocatable, DIMENSION(:)        :: gw_buck_coeff,gw_buck_exp,z_max  !GW bucket parameters
!DJG Switch for Deep Sat GW Init:
  INTEGER                                :: DEEPGWSPIN  !Switch to setup deep GW spinp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG,DNY   VARIABLES FOR CHANNEL ROUTING
!-- channel params
  INTEGER, allocatable, DIMENSION(:)   :: LINK       !channel link
  INTEGER, allocatable, DIMENSION(:)   :: TO_NODE    !link's to node
  INTEGER, allocatable, DIMENSION(:)   :: FROM_NODE  !link's from node
  INTEGER, allocatable, DIMENSION(:)   :: ORDER      !link's order
  INTEGER, allocatable, DIMENSION(:)   :: STRMFRXSTPTS      !frxst point flag
  INTEGER, allocatable, DIMENSION(:)   :: TYPEL       !type of link Muskingum: 0 strm 1 lake
                                                      !-- Diffusion: 0 edge or pour; 1 interior; 2 lake
  INTEGER, allocatable, DIMENSION(:)   :: TYPEN      !type of link 0 strm 1 lake
  REAL, allocatable, DIMENSION(:)      :: QLAKEI      !lake inflow in difussion scheme
  REAL, allocatable, DIMENSION(:)      :: QLAKEO      !lake outflow in difussion scheme
  INTEGER, allocatable, DIMENSION(:)   :: LAKENODE   !which nodes flow into which lakes
  REAL, allocatable, DIMENSION(:)      :: CVOL       ! channel volume


  REAL, allocatable, DIMENSION(:)      :: MUSK, MUSX !muskingum params
  REAL, allocatable, DIMENSION(:)      :: CHANLEN    !link length
  REAL, allocatable, DIMENSION(:)      :: MannN      !mannings N
  REAL, allocatable, DIMENSION(:)      :: So         !link slope
  REAL, allocatable, DIMENSION(:)      :: ChSSlp, Bw !trapezoid link params
  REAL, allocatable, DIMENSION(:,:)    :: QLINK      !flow in link
  REAL, allocatable, DIMENSION(:)      :: HLINK      !head in link
  REAL, allocatable, DIMENSION(:)      :: ZELEV      !elevation of nodes for channel
  INTEGER, allocatable, DIMENSION(:)   :: CHANXI,CHANYJ !map chan to fine grid
  REAL,  DIMENSION(50)     :: BOTWID,HLINK_INIT,CHAN_SS,CHMann !Channel parms from table

  REAL, allocatable, DIMENSION(:)      :: RESHT  !reservoir height
!-- lake params
  REAL, allocatable, DIMENSION(:)    :: HRZAREA    !horizontal extent of lake, km^2
  REAL, allocatable, DIMENSION(:)    :: WEIRL      !overtop weir length (m)
  REAL, allocatable, DIMENSION(:)    :: ORIFICEC   !coefficient of orifice
  REAL, allocatable, DIMENSION(:)    :: ORIFICEA   !orifice opening area (m^2)
  REAL, allocatable, DIMENSION(:)    :: ORIFICEE   !orifice elevation (m)
  REAL, allocatable, DIMENSION(:)    :: LATLAKE, LONLAKE,ELEVLAKE ! lake info
#ifdef MPP_LAND
  INTEGER, allocatable, DIMENSION(:)    :: lake_index,nlinks_index
  INTEGER, allocatable, DIMENSION(:,:)  :: Link_location
  integer mpp_nlinks, yw_mpp_nlinks
#endif

  REAL,    allocatable, DIMENSION(:,:)      :: OVROUGHRTFAC,RETDEPRTFAC


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR AGGREGATION/DISAGGREGATION
  REAL,    allocatable, DIMENSION(:,:,:)   :: SMCRT,SMCMAXRT,SMCWLTRT,SH2OWGT
  REAL,    allocatable, DIMENSION(:,:)     :: INFXSAGGRT
  REAL,    allocatable, DIMENSION(:,:)     :: DHRT,QSTRMVOLRT,QBDRYRT,LAKE_INFLORT
  !yw REAL,    allocatable, DIMENSION(:,:)     :: QSTRMVOLRT_TS,LAKE_INFLORT_TS
!  REAL,    allocatable, DIMENSION(:,:)     :: QSTRMVOLRT_DUM,LAKE_INFLORT_DUM
  REAL,    allocatable, DIMENSION(:,:)       :: INFXSWGT, ywtmp
  REAL,    allocatable, DIMENSION(:)       :: SMCAGGRT,STCAGGRT,SH2OAGGRT
  REAL    :: INFXSAGG1RT,SFCHEADAGG1RT,SFCHEADAGGRT
  REAL,    allocatable, DIMENSION(:,:,:)       :: dist  ! 8 direction of distance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DJG   VARIABLES FOR ONLINE MASS BALANCE CALCULATION
  REAL(KIND=8)    :: DCMC,DSWE,DACRAIN,DSFCEVP,DCANEVP,DEDIR,DETT,DEPND,DESNO,DSFCRNFF
  REAL(KIND=8)    :: DSMCTOT,RESID,SUMEVP,DUG1RNFF,DUG2RNFF,SMCTOT1,SMCTOT2,DETP
  REAL(KIND=8)    :: suminfxsrt,suminfxs1,suminfxs2,dprcp_ts
  REAL(KIND=8)    :: CHAN_IN1,CHAN_IN2,LAKE_IN1,LAKE_IN2,zzz, CHAN_STOR,CHAN_OUT
  REAL(KIND=8)    :: CHAN_INV,LAKE_INV  !-channel and lake inflow in volume
  REAL(KIND=8)    :: DQBDRY
  REAL    :: QSTRMVOLTRT1,LAKE_INFLOTRT1,QBDRYTOT1,LSMVOL
  REAL(KIND=8),    allocatable, DIMENSION(:)   :: DSMC,SMCRTCHK
  REAL(KIND=8),    allocatable, DIMENSION(:,:)  :: CMC_INIT,SWE_INIT
!  REAL(KIND=8),    allocatable, DIMENSION(:,:,:) :: SMC_INIT
  REAL(KIND=8)            :: SMC_INIT,SMC_FINAL,resid2,resid1
  REAL(KIND=8)            :: chcksm1,chcksm2,CMC1,CMC2,prcp_in,ETATOT,dsmctot_av

  integer :: g_ixrt,g_jxrt,flag
  integer :: allo_status = -99
  integer iywtmp


!-- lake params
  REAL, allocatable, DIMENSION(:)    :: LAKEMAXH   !maximum depth (m)
  REAL, allocatable, DIMENSION(:)    :: WEIRC      !coeff of overtop weir




!DJG Modified namelist for routing and agg. variables
  real Z_tmp

  !!! define land surface grid variables
      REAL,    allocatable, DIMENSION(:,:,:) :: SMC,STC,SH2OX
      REAL,    allocatable, DIMENSION(:,:)   :: SMCMAX1,SMCWLT1,SMCREF1
      INTEGER, allocatable, DIMENSION(:,:)   :: VEGTYP 
      REAL,    allocatable, DIMENSION(:)   :: SLDPTH

!!! define constant/parameter
    real ::   ov_rough(50), ZSOIL(100)
!  out_counts: couput counts for current run.
!  his_out_counts: used for channel routing output and  special for restart. 
!  his_out_counts = previous run + out_counts
    integer :: out_counts, rst_counts, his_out_counts
    
    REAL,    allocatable, DIMENSION(:,:)   :: lat_lsm, lon_lsm
    REAL,    allocatable, DIMENSION(:,:,:) :: dist_lsm

   END TYPE RT_FIELD
