















MODULE module_sf_gecros

implicit none



REAL, PARAMETER :: EG = 1.        
REAL, PARAMETER :: CFV = 0.48     
REAL, PARAMETER :: YGV = 0.81     
REAL, PARAMETER :: FFAT = 0.02    
REAL, PARAMETER :: FLIG = 0.06    
REAL, PARAMETER :: FOAC = 0.02    
REAL, PARAMETER :: FMIN = 0.02    
REAL, PARAMETER :: LNCI = 0.03586 
REAL, PARAMETER :: TBD = 0.0      
REAL, PARAMETER :: TOD = 22.5     
REAL, PARAMETER :: TCD = 37.0     
REAL, PARAMETER :: TSEN = 1.0     
REAL, PARAMETER :: SPSP = 0.2     
REAL, PARAMETER :: EPSP = 0.7     
REAL, PARAMETER :: CDMHT = 492.6  
REAL, PARAMETER :: PMEH = 0.6468  
REAL, PARAMETER :: ESDI = 1.35    
REAL, PARAMETER :: PMES = 0.50    
REAL, PARAMETER :: TBDV = -1.3    
REAL, PARAMETER :: TODV = 4.9     
REAL, PARAMETER :: TCDV = 15.7    
REAL, PARAMETER :: NUPTX = 0.5/24./3600.    
REAL, PARAMETER :: SLA0 = 0.0237  
REAL, PARAMETER :: SLNMIN = 0.35  
REAL, PARAMETER :: RNCMIN = 0.005 
REAL, PARAMETER :: STEMNC = 0.01  
REAL, PARAMETER :: RLVDS = 0.0904 
REAL, PARAMETER :: DSCRIT = 0.225 
REAL, PARAMETER :: EAJMAX = 48270.
REAL, PARAMETER :: XVN = 24.96    
REAL, PARAMETER :: XJN = 49.92    
REAL, PARAMETER :: NPL = 390.0    
REAL, PARAMETER :: SEEDW = 0.0475 
REAL, PARAMETER :: SEEDNC = 0.020 
REAL, PARAMETER :: BLD = 25.58    
REAL, PARAMETER :: HTMX = 1.1     
REAL, PARAMETER :: MTDV = 46.12   
REAL, PARAMETER :: MTDR = 40.22   
REAL, PARAMETER :: PSEN = -.104   
REAL, PARAMETER :: C3C4 = 1.      


REAL, PARAMETER :: LEGUME = -1. 
REAL, PARAMETER :: DETER = 1.   
REAL, PARAMETER :: SLP = -1.    
REAL, PARAMETER :: NSUP1 = 0.   
REAL, PARAMETER :: NSUP2 = 1./172800. 
REAL, PARAMETER :: CO2A=350.    
REAL, PARAMETER :: FCRSH=0.5    
REAL, PARAMETER :: FNRSH=0.63   
REAL, PARAMETER :: PNPRE=0.7    
REAL, PARAMETER :: CB=0.75      
REAL, PARAMETER :: CX=1.00      
REAL, PARAMETER :: TM=1.5       
REAL, PARAMETER :: RSS=100.     
REAL, PARAMETER :: LS=0.        
REAL, PARAMETER :: WRB=0.25     
REAL, PARAMETER :: THETA=0.7    
REAL, PARAMETER :: PNLS=1.      
REAL, PARAMETER :: INSP=-2.     
REAL, PARAMETER :: CCFIX=6.     
REAL, PARAMETER :: RDMX=130.    
REAL, PARAMETER :: TCP=86400.   
REAL, PARAMETER :: Z1244=0.272727272727  
REAL, PARAMETER :: LOG005=-2.995732274   


REAL :: YGO    
REAL :: CFO    
REAL :: LNCMIN 
REAL :: CLVI   
REAL :: CRTI   
REAL :: NLVI   
REAL :: NRTI   
REAL :: HTI    
REAL :: RDI    


LOGICAL :: debugging=.false.

CONTAINS

SUBROUTINE gecros (DOY, DT, CROP, RB, RT, RTS, FB, SNOWH,        & 
           WN, SFCTMP, EAIR, RSD, RLD, PRCP, WUL, WLL, WCMIN, LWIDTH,     & 
           STATE_GECROS,                                                  & 
           ATRJC, ATRJS, FSR, FRSU, ARSWSU, ARSWSH)  

IMPLICIT NONE


    INTEGER, INTENT(IN) :: CROP     
    REAL, INTENT(IN)    :: DOY      
    REAL, INTENT(IN)    :: DT       
    REAL, INTENT(IN)    :: RB       
    REAL, INTENT(IN)    :: RT       
    REAL, INTENT(IN)    :: RTS      
    REAL, INTENT(IN)    :: FB       
    REAL, INTENT(IN)    :: SNOWH    
    REAL, INTENT(IN)    :: WN       
    REAL, INTENT(IN)    :: SFCTMP   
    REAL, INTENT(IN)    :: EAIR     
    REAL, INTENT(IN)    :: RSD      
    REAL, INTENT(IN)    :: RLD      
    REAL, INTENT(IN)    :: PRCP     
    REAL, INTENT(IN)    :: WUL      
    REAL, INTENT(IN)    :: WLL      
    REAL, INTENT(IN)    :: WCMIN    
    REAL, INTENT(IN)    :: LWIDTH   
    REAL, INTENT(OUT)   :: ATRJC    
    REAL, INTENT(OUT)   :: ATRJS    
    REAL, INTENT(OUT)   :: FSR      
    REAL, INTENT(OUT)   :: FRSU     
    REAL, INTENT(OUT)   :: ARSWSU   
    REAL, INTENT(OUT)   :: ARSWSH   
    REAL, DIMENSION(1:60), INTENT(INOUT) ::  STATE_GECROS 

    
    REAL :: DS     
    REAL :: LAI    
    REAL :: TLAI   
    REAL :: CTDU   
    REAL :: CVDU   
    REAL :: CLV    
    REAL :: CLVD   
    REAL :: CSST   
    REAL :: CSO    
    REAL :: CSRT   
    REAL :: CRTD   
    REAL :: CLVDS  
    REAL :: NRT    
    REAL :: NST    
    REAL :: NLV    
    REAL :: NSO    
    REAL :: TNLV   
    REAL :: NLVD   
    REAL :: NRTD   
    REAL :: CRVS   
    REAL :: CRVR   
    REAL :: NREOE  
    REAL :: NREOF  
    REAL :: DCDSR  
    REAL :: DCDTR  
    REAL :: SLNB   
    REAL :: LAIC   
    REAL :: RMUL   
    REAL :: NDEMP  
    REAL :: NSUPP  
    REAL :: NFIXT  
    REAL :: NFIXR  
    REAL :: DCDTP  
    REAL :: HT     
    REAL :: TPCAN  
    REAL :: TRESP  
    REAL :: TNUPT  
    REAL :: LITNT  
    REAL :: WSO     
    REAL :: WSTRAW  
    REAL :: GrainNC 
    REAL :: StrawNC 
    REAL :: APCAN   
    
    character(Len=12)  :: inputstring
    INTEGER :: nowday, minutes
    REAL :: SC     
    REAL :: SINLD  
    REAL :: COSLD  
    REAL :: DAYL   
    REAL :: DDLP   
    REAL :: DSINBE 
    REAL :: DVP    
    REAL :: WNM    
    REAL :: TAIR   
    REAL :: ROOTD  
    REAL :: WLV    
    REAL :: WST    
    REAL :: WRT    
    REAL :: WSH    
    REAL :: WLVD   
    REAL :: WRTD   
    REAL :: CRT    
    REAL :: CSH    
    REAL :: NSH    
    REAL :: DLAI   
    REAL :: ESD    
    REAL :: KCRN   
    REAL :: NFIXD  
    REAL :: KR     
    REAL :: HNCCR  
    REAL :: FVPD   
    REAL :: NRETS  
    REAL :: WCUL   
    REAL :: DWSUP  
    REAL :: CSRTN  
    REAL :: NRES   
    REAL :: ONC    
    REAL :: RNC    
    REAL :: LNC    
    REAL :: KL     
    REAL :: CTOT   
    REAL :: NSHH   
    REAL :: NTOT   
    REAL :: WSHH   
    REAL :: TSN    
    REAL :: HNC    
    REAL :: PSO    
    REAL :: KLN    
    REAL :: NBK    
    REAL :: KW     
    REAL :: WTOT   
    REAL :: CCHKIN 
    REAL :: NCHKIN 
    REAL :: LCRT   
    REAL :: TSW    
    REAL :: PNC    
    REAL :: NDEMD  
    REAL :: FCRVR  
    REAL :: KN     
    REAL :: CCHK   
    REAL :: HI     
    REAL :: LWRT   
    REAL :: NCHK   
    REAL :: LAIN   
    REAL :: LNRT   
    REAL :: LWLVM  
    REAL :: SLNNT  
    REAL :: SLNBC  
    REAL :: SLN    
    REAL :: SLA    
    REAL :: LWLV   
    REAL :: AESOIL 
    REAL :: APCANN 
    REAL :: APCANS 
    REAL :: ATCAN  
    REAL :: DAPAR  
    REAL :: DIFS   
    REAL :: DIFSH  
    REAL :: DIFSU  
    REAL :: FRSH   
    REAL :: HOD    
    REAL :: PESOIL 
    REAL :: PPCAN  
    REAL :: PTCAN  
    REAL :: RCAN   
    REAL :: RSLNB  
    REAL :: LNLV   
    REAL :: LCLV   
    REAL :: RMRE   
    REAL :: TAVSS  
    REAL :: RMN    
    REAL :: VDU    
    REAL :: TDU    
    REAL :: RM     
    REAL :: LVDS   
    REAL :: NFIXE  
    REAL :: DVR    
    REAL :: RX     
    REAL :: RNSUPP 
    REAL :: NSUP   
    REAL :: NFIX   
    REAL :: LITN   
    REAL :: LITC   
    REAL :: FDH    
    REAL :: FDS    
    REAL :: SHSAN  
    REAL :: SHSA   
    REAL :: RMUS   
    REAL :: NDEMAD 
    REAL :: NDEMA  
    REAL :: NCR    
    REAL :: DERI   
    REAL :: ASSA   
    REAL :: RNFIXR 
    REAL :: RNDEMP 
    REAL :: RMLD   
    REAL :: RCSRT  
    REAL :: NUPTN  
    REAL :: NUPTA  
    REAL :: NDEM   
    REAL :: FNSH   
    REAL :: FCSH   
    REAL :: DCSR   
    REAL :: SLNT   
    REAL :: RMUN   
    REAL :: RMUA   
    REAL :: NUPT   
    REAL :: DCDSC  
    REAL :: DCDS   
    REAL :: FLWCS  
    REAL :: DCSS   
    REAL :: DCST   
    REAL :: FCSO   
    REAL :: RRMUL  
    REAL :: RHT    
    REAL :: IFSH   
    REAL :: GAP    
    REAL :: CREMSI 
    REAL :: CREMS  
    REAL :: CREMRI 
    REAL :: CREMR  
    REAL :: RWSO   
    REAL :: RWRT   
    REAL :: RRD    
    REAL :: RDCDTP 
    REAL :: RDCDSR 
    REAL :: RCSST  
    REAL :: RCSO   
    REAL :: RCRVR  
    REAL :: FLWCT  
    REAL :: FCSST  
    REAL :: FCLV   
    REAL :: DCDTC  
    REAL :: DCDT   
    REAL :: FCRVS  
    REAL :: RCLV   
    REAL :: RCRVS  
    REAL :: RDCDTR 
    REAL :: RESTOT 
    REAL :: RG     
    REAL :: RLAI   
    REAL :: RNLV   
    REAL :: RNREOE 
    REAL :: RNREOF 
    REAL :: RNRES  
    REAL :: RNRT   
    REAL :: RNSO   
    REAL :: RNST   
    REAL :: RRP    
    REAL :: RTNLV  
    REAL :: RWLV   
    REAL :: RWST   
    REAL :: GLAT   
    REAL :: SD1    
    
    
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
    
    
    TAIR   = SFCTMP - 273.15

    
    DVP    = EAIR*0.001    
    WNM    = MAX (0.1, WN)
       
    
    CALL ASTRO(aint(DOY),GLAT,INSP,SC,SINLD,COSLD,DAYL,DDLP,DSINBE)  
       
    
    WLV    = CLV  / CFV 
    WST    = CSST / CFV + CRVS/0.444
    WSO    = CSO  / CFO
    WRT    = CSRT / CFV + CRVR/0.444
    WLVD   = CLVD / CFV
    WRTD   = CRTD / CFV
    
    
    CSH    = CLV + CSST + CRVS + CSO
    CRT    = CSRT + CRVR

    
    NSH    = NST + NLV + NSO
 
    
    KCRN   = -LOG005/6.3424/CFV/WRB/RDMX
      
    
    ESD    = INSW(DETER, ESDI, 1.)
     
    
    DLAI   = (CLVD-CLVDS)/CFV*SLA0

    
    TLAI   = MAX(0.01,LAI + DLAI)            
    
    
    NFIXD  = MAX(0., NDEMP - NSUPP)

    
    HNCCR  = LNCI*EXP(-.4*DS)
    
    
    KR     = -LOG005/RDMX  
    
    
    FVPD   = INSW (C3C4, 0.195127, 0.116214)
     
    
    NRETS  = LITNT+INSW(DS-2.,0.,NLV+NST+NRT+NFIXR+(CLVD-CLVDS)/ &
    CFV*LNCMIN*(1.+PNLS)/2.)



    
 
    
    WCUL   = (WUL+WCMIN*10.*ROOTD)/10./ROOTD
        
    
    DWSUP  = MAX(.1/TCP,WUL/TCP+.1/TCP)
    
    
    CSRTN  = 1./KCRN*LOG(1.+KCRN*MAX(0.,(NRT*CFV-CRVR*RNCMIN))/RNCMIN)
    
    
    WSTRAW = WLV + WST + (WLVD - CLVDS/CFV)                            

    
    WSH    = WLV  + WST + WSO       

    
    NRES   = NREOF + (NREOE-NREOF)*(ESD-1.)/NOTNUL(MIN(DS,ESD)-1.)
   
    
    LNC    = NLV / NOTNUL(WLV)
    
    
    RNC    = NRT / NOTNUL(WRT)
    
    
    ONC    = INSW(-WSO, NSO/NOTNUL(WSO), 0.)

    
    GrainNC = NSO*10.                       
    StrawNC = (NLV+NST)*10.                 

    
    CALL KDIFF (TLAI,BLD*3.141592654/180.,0.2, KL)

    CTOT   = CSH + CRT
    NSHH   = NSH +(WLVD-CLVDS/CFV)*LNCMIN
    NTOT   = NSH + NRT
    WSHH   = WSH  + (WLVD-CLVDS/CFV)
    TSN    = NRES/PNPRE/SEEDNC/SEEDW
    HNC    = NSH / NOTNUL(WSH)
    
    
    PSO    = 6.25*WSO*ONC

    KLN    = KL*(TNLV-SLNMIN*TLAI)  

    NBK    = SLNMIN*(1.-EXP(-KL*TLAI))  
    KW     = KL
    WTOT   = WSH  + WRT
 
    
    CCHKIN = CTOT + CLVD + CRTD -CLVI-CRTI
      
    
    NCHKIN = NTOT + NLVD + NRTD -NLVI-NRTI
    
    LCRT   = MAX(MIN(CSRT-1.E-4,CSRT-MIN(CSRTN,CSRT)),0.)/TCP 

    FCRVR  = INSW(CSRTN-CSRT, 1., 0.)
    PNC    = NTOT / NOTNUL(WTOT)
        
    KN     = 1./TLAI*LOG(MAX(1.001,(KLN+NBK)/(KLN*EXP(-KL*TLAI)+NBK)))
    
    TSW    = WSO/NOTNUL(TSN)*1000.
     
    NDEMD  = INSW(DS-1., WSH*(HNCCR-HNC)*(1.+NRT/MAX(1E-2,NSH))/TCP, 0.)  
    
    
    HI     = WSO  / NOTNUL(WSHH)
    CCHK   = (CCHKIN-(TPCAN-TRESP)*Z1244)/NOTNUL(CCHKIN)*100.
    NCHK   = (NCHKIN-TNUPT)/NOTNUL(TNUPT)*100.
    LWRT   = LCRT/CFV   
  
    
    LAIN   = LOG(1.+ KN*MAX(0.,NLV)/SLNMIN)/KN
    LNRT   = LWRT*RNCMIN  

    
    LAI    = MAX(0.01,MIN(LAIN, LAIC))
   
    
    
    
    

    LWLVM  = (LAIC-MIN(LAIC,LAIN))/SLA0 
    
    
    SLN    = NLV/LAI
    SLNT   = NLV*KN/(1.-EXP(-KN*LAI))
    SLNBC  = NLV*KN*EXP(-KN*LAI)/(1.-EXP(-KN*LAI))
    SLNNT  = (NLV+0.001*NLV)*KN /(1.-EXP(-KN*LAI))
    SLA    = LAI/NOTNUL(WLV)
    
    
    LWLV = MIN((WLV-1.E-5)/TCP, (LWLVM+REANOR(ESD-DS,LWLVM)*0.03*WLV)/TCP)   
    
    
    
    
    CALL TOTPT(HOD,DS,SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR,DVP,WNM,C3C4,LAI, &
         TLAI,HT,LWIDTH,ROOTD,SD1,RSS,BLD,KN,KW,SLN,SLNT,SLNNT,SLNMIN,FB,&
         DWSUP,CO2A,LS,EAJMAX,XVN,XJN,THETA,WCUL,FVPD,RB,RT,RTS, &
         PPCAN,APCANS,APCANN,APCAN,PTCAN,ATCAN,PESOIL,AESOIL,DIFS,DIFSU, &
         DIFSH,DAPAR,RCAN,ATRJC,ATRJS,FSR,FRSU,FRSH,ARSWSU,ARSWSH)
      
    RSLNB  = (SLNBC-SLNB)/TCP 

    LNLV   = MIN(LWLV,LWLVM)*LNCMIN + (LWLV-MIN(LWLV,LWLVM))*LNC  
    LCLV   = LWLV*CFV  
    
    
    RMRE   = MAX(MIN(44./12.*0.218*(NTOT-WSH*LNCMIN-WRT*RNCMIN)/TCP &
    ,APCAN-(1.E-5+RMUL)/TCP), 0.)  
      
    TAVSS  = TAIR + DIFS

    
    CALL TUNIT (1.*CROP, DS,TAIR,MAX(0.,DIFS),DAYL,TBD,TOD,TCD,TBDV,TODV,TCDV,TSEN,TDU,VDU)

    RMN    = MAX(0., MIN(APCAN-1.E-5/TCP,RMUL/TCP) + MAX(MIN(44./12.*0.218* &
    (1.001*NTOT-WSH*LNCMIN-WRT*RNCMIN)/TCP,APCAN-(1.E-5+RMUL)/TCP), 0.))

    RM = MAX(0., MIN(APCAN-1.E-5/TCP,RMUL/TCP) + RMRE)  

    
    
    
    

    IF(DS.lt.0.25) THEN
       LVDS   = (CLVD-CLVDS)/TCP  
    ELSE
       LVDS   = RLVDS*(CLVD-CLVDS)*(TAVSS-TBD)/(TOD-TBD)/TCP  
    ENDIF
    
    CALL PHENO (1.*CROP,DS,SLP,DDLP,SPSP,EPSP,PSEN,MTDV,MTDR,TDU,CVDU,DVR)

    NFIXE  = MAX(0., APCAN-(1.E-5+RM)/TCP)/CCFIX*Z1244

    
    CALL BETAF(DVR,1.,PMES,LIMIT(1.,2.,DS)-1., FDS)
            
    LITC   =  LCRT + LVDS                  
    LITN   =  LNRT + LVDS/CFV *LNCMIN*PNLS 
 
    CALL BETAF(DVR,(1.+ESD)/2.,PMEH*(1.+ESD)/2.,MIN((1.+ESD)/2.,DS), FDH)
     
    NSUP   = NSUP1 + NSUP2 
      
    NFIX   = INSW (LEGUME, 0., MIN(NFIXE, NFIXD)) 

    RNSUPP = (NSUP - NSUPP)/DT         
    RX     = 44./12.*(CCFIX*NFIX)
  

    ASSA   = APCAN - RM - RX
      


    SHSA   = Z1244 * YGV*MAX(1E-16,APCAN -RM -RX)/ MAX(0.1,CSH)
    SHSAN  = Z1244 * YGV*(APCANN-RMN-RX)/ MAX(0.1,CSH)
    DERI   = MAX(0.,(SHSAN - SHSA)/(0.001*MAX(0.01,NTOT)/MAX(0.1,CTOT)))
     
    RMUS   = 0.06*0.05/0.454*YGV*ASSA
    NDEMA  = CRT * SHSA**2/NOTNUL(DERI)
    NCR    = INSW(SLNT-SLNMIN,0.,MIN(NUPTX,NDEMA))/(YGV*MAX(1E-16,APCANS-RM-RX)*Z1244)
    NDEMAD = INSW(LNC-1.5*LNCI, MAX(NDEMA, NDEMD), 0.)
    

    FNSH   = 1./(1.+NCR*DERI/SHSA*CSH/MAX(1E-2,CRT)*NRT/MAX(1E-2,NSH))
       

    FCSH   = 1./(1.+NCR*DERI/SHSA)



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
    RNDEMP = (NDEM - NDEMP)/DT         
    NUPTA  = MIN(NSUP1, NSUP1/NOTNUL(NSUP)*MAX(0.,NDEM-NFIXR/TCP))
    
    
    DCSS   = Z1244*    FCSH *ASSA
    NUPT   = MAX(0., NUPTA + NUPTN + MIN(NDEM, NFIXR/TCP))
    RMUA   = 44./12.*0.17*NUPTA

    CALL SINKG(DS,1.,TSN*SEEDW*CFO,YGO,FDS,DCDSR,DCSS,DT,&
    DCDSC,DCDS,FLWCS)
 
    
    RMUN   = 44./12.*2.05*NUPTN
 
    
    DCST   = DCSS - FLWCS
    FCSO   = FLWCS/DCSS

    RRMUL  = (RMUN+RMUA+RMUS+RMLD-RMUL)/DT
         
    GAP    = MAX(0., DCDS-DCSS)
  
    
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

    
    
    IF (CROP==1) THEN
        FCSST  = MIN(.85,INSW(DS-(ESD+0.2), FLWCT/MAX(1E-16,DCSS), 0.))
        
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
        
    


























    

END SUBROUTINE gecros























      SUBROUTINE TUNIT(CROP,DS,TAIR,DIF,DAYL,TBD,TOD,TCD,TBDV,TODV,TCDV,TSEN,TDU,VDU)
      IMPLICIT REAL (A-Z)
      INTEGER I
      


      IF (DS.GT.1.) THEN
           TAIR = MIN (TAIR,TOD)
      ELSE
           TAIR = TAIR
      ENDIF



      IF (TAIR.LT.TBDV .OR. TAIR.GT.TCDV) THEN
           TUV = 0.
      ELSE
           TUV = (((TCDV-TAIR)/(TCDV-TODV))*((TAIR-TBDV)/(TODV-TBDV))**((TODV-TBDV)/(TCDV-TODV)))**TSEN
      ENDIF


      IF (TAIR.LT.TBD .OR. TAIR.GT.TCD) THEN
           TU = 0.
      ELSE
           TU = (((TCD-TAIR)/(TCD-TOD))*((TAIR-TBD)/(TOD-TBD))**((TOD-TBD)/(TCD-TOD)))**TSEN
      ENDIF



      IF (CROP==1) THEN
         TDU = TU/TCP
      ELSE
         TDU = INSW(DS-2.,TU/TCP,0.)
      ENDIF
      VDU = TUV/TCP
      	
      RETURN
      END SUBROUTINE TUNIT






















      SUBROUTINE PHENO (CROP,DS,SLP,DDLP,SPSP,EPSP,PSEN,MTDV,MTDR,TDU,CVDU,DVR)
      IMPLICIT REAL (A-Z)
      

      IF (SLP.LT.0.) THEN
          MOP = 18.     
          DLP = MIN(MOP,DDLP)
      ELSE
          MOP = 11.     
          DLP = MAX(MOP,DDLP)
      ENDIF


      IF (DS.LT.SPSP .OR. DS.GT.EPSP) THEN
          EFP = 1.
      ELSE
          EFP = MAX(0., 1.-PSEN*(DLP-MOP))
      ENDIF	



     IF (CROP==1) THEN
        EFV = CVDU**5./(22.5**5. + CVDU**5.)
     ELSE
        EFV = 1.0
     ENDIF
          


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





































      SUBROUTINE RNACC (FNSH,NUPT,RWST,STEMNC,LNCMIN,RNCMIN,LNC,RNC, &
                        NLV,NRT,WLV,WRT,DELT,CB,CX,TM,DS,SEEDNC, &
                        RWSO,LNLV,LNRT, RNRT,RNST,RNLV,RTNLV,RNSO)
      IMPLICIT REAL (A-Z)
      

      NSHN   = FNSH * NUPT


      NLVA   = INSW(LNCMIN-LNC, NLV-WLV*LNCMIN, 0.)/TCP
      NRTA   = INSW(RNCMIN-RNC, NRT-WRT*RNCMIN, 0.)/TCP
      
      NTA    = NLVA + NRTA


      RNST   = RWST * INSW(-NTA,STEMNC,0.)


      CDS    = CB+(CX-CB)*(4.-TM-DS)/(2.-TM)*(DS-1.)**(1./(2.-TM))
      ENSNC  = LIMIT(CB,CX,CDS) * SEEDNC


      NGS    = NSHN - RNST - ENSNC*RWSO
      NONC   = MAX(0.,INSW(NTA+NGS,(NTA+NSHN-RNST)/NOTNUL(RWSO),ENSNC))
      RNSO   = RWSO*NONC



      NLVN   = INSW(NTA+NGS,-NLVA-LNLV,-NLVA/NOTNUL(NTA)*(-NGS)-LNLV)
      GNLV   = INSW(NGS, NLVN, NSHN-RNST-RNSO-LNLV)
      RNLV   = MAX (-NLV+1.E-7, GNLV)
      RTNLV  = MAX(0., RNLV)


      NRTN   = INSW(NTA+NGS, NUPT-NSHN-NRTA-LNRT, &
              NUPT-NSHN-NRTA/NOTNUL(NTA)*(-NGS)-LNRT)
      GNRT   = INSW(NGS, NRTN, NUPT-NSHN-LNRT)
      RNRT   = MAX (-NRT+5.E-8, GNRT)
   	
      RETURN
      END SUBROUTINE RNACC






















      SUBROUTINE RLAIC(CROP,DS,SLA0,RWLV,LAI,KN,NLV,RNLV,SLNB,RSLNB, RLAI)
      IMPLICIT REAL (A-Z)
      
      SLNB = MAX(1E-2, SLNB)
      
      RLAI   =  INSW(RWLV, MAX(-LAI+1.E-5,SLA0*RWLV), SLA0*RWLV)
      
	  
      
      IF ((CROP==2) .AND. (LAI.LT.1.5) .AND. (DS.LT.0.75)) THEN
           RLAI  = MAX(0.,(SLNB*RNLV-NLV*RSLNB)/SLNB/(SLNB+KN*NLV))
      ENDIF
  
      
      RETURN
      END SUBROUTINE RLAIC

















      SUBROUTINE BETAF(DVRX,TE,TX,TI, FD)
      
            REAL, INTENT(IN)  :: DVRX, TE, TX, TI
            REAL, INTENT(OUT) :: FD
      
      FD    = DVRX*(2.*TE-TX)*(TE-TI)/TE/(TE-TX)**2*(TI/TE)**(TX/(TE-TX))
      
      
      END SUBROUTINE BETAF






















      SUBROUTINE SINKG(DS,SSG,TOTC,YG,FD,DCDR,DCS,DELT,DCDC,DCD,FLWC)
      IMPLICIT REAL (A-Z)
      

      DCDC   = INSW (DS-SSG, 0., TOTC/YG*FD)


      DCD    = DCDC + MAX(0.,DCDR)/DELT
      

      FLWC   = MIN(DCD, DCS)
  
      RETURN
      END SUBROUTINE SINKG


























      SUBROUTINE ASTRO (DOY,LAT,INSP,SC,SINLD,COSLD,DAYL,DDLP,DSINBE)
      IMPLICIT REAL (A-Z)
      

      PI    = 3.141592654
      RAD   = PI/180.

      IF (LAT.GT.67.)  STOP 'ERROR IN ASTRO: LAT> 67'
      IF (LAT.LT.-67.) STOP 'ERROR IN ASTRO: LAT>-67'


      DEC   = -ASIN (SIN (23.45*RAD)*COS (2.*PI*(DOY+10.)/365.))


      SINLD = SIN (RAD*LAT)*SIN (DEC)
      COSLD = COS (RAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD


      DAYL   = 12.0*(1.+2.*ASIN (AOB)/PI)
      DDLP   = 12.0*(1.+2.*ASIN((-SIN(INSP*RAD)+SINLD)/COSLD)/PI)

      DSINB  = 3600.*(DAYL*SINLD+24.*COSLD*SQRT (1.-AOB*AOB)/PI)
      DSINBE = 3600.*(DAYL*(SINLD+0.4*(SINLD*SINLD+COSLD*COSLD*0.5))+ &
               12.0*COSLD*(2.0+3.0*0.4*SINLD)*SQRT (1.-AOB*AOB)/PI)


      SC     = 1367.*(1.+0.033*COS(2.*PI*(DOY-10.)/365.))

      RETURN
      END SUBROUTINE ASTRO






























































SUBROUTINE TOTPT(HOD,DS, SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR, DVP, &
                 WNM,C3C4,LAI,TLAI,HT,LWIDTH,RD,SD1,RSS,BLD,KN,KW, &
                 SLN,SLNT,SLNN,SLNMIN,FB,DWSUP,CO2A,LS,EAJMAX, &
                 XVN,XJN,THETA,WCUL,FVPD,RB,RT,RTS,PPCAN,APCANS, &
                 APCANN,APCAN,PTCAN,ATCAN,PESOIL,AESOIL,DIFS,DIFSU,DIFSH,DAPAR, &
                 RCAN, ATRJC, ATRJS, FSR, FRSU, FRSH, ARSWSU, ARSWSH)
      
            REAL, INTENT(IN) :: HOD,DS,SC,SINLD,COSLD,DAYL,DSINBE,RSD,TAIR, DVP, &
                                WNM,C3C4,LAI,TLAI,HT,LWIDTH,RD,SD1,RSS,BLD,KN,KW, &
                                SLN,SLNT,SLNN,SLNMIN,FB,DWSUP,CO2A,LS,EAJMAX, &
                                XVN,XJN,THETA,WCUL,FVPD,RT,RTS 

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
            

      SINB  = MAX (.01, SINLD+COSLD*COS(2.*PI*(HOD-12.)/24.))
      

    WSUP = DWSUP      
    WSUP1 = WSUP*SD1/RD 
 

      WND   = WNM         
            

      PAR   = (1.-FB)*0.5*RSD  
      NIR   = (1.-FB)*0.5*RSD  


      ATMTR = PAR/(0.5*SC*SINB) 

      IF (ATMTR.LE.0.22) THEN
         FRDF = 1.
      ELSE IF (ATMTR.GT.0.22 .AND. ATMTR.LE.0.35) THEN
         FRDF = 1.-6.4*(ATMTR-0.22)**2
      ELSE
         FRDF = 1.47-1.66*ATMTR
      ENDIF

      FRDF = MAX (FRDF, 0.15+0.85*(1.-EXP (-0.1/SINB)))


      PARDF = PAR * FRDF 
      PARDR = PAR - PARDF 


      NIRDF = NIR * FRDF 
      NIRDR = NIR - NIRDF 


      BL    = BLD*PI/180.     
      CALL KBEAM (SINB,BL,KB)

      SCPPAR = 0.2            
      SCPNIR = 0.8            
      CALL KDIFF (TLAI,BL,SCPPAR, KDPPAR)
      CALL KDIFF (TLAI,BL,SCPNIR, KDPNIR)

      CALL REFL (SCPPAR,KB, KBPPAR,PCBPAR)
      CALL REFL (SCPNIR,KB, KBPNIR,PCBNIR)

      PCDPAR = 0.057          
      PCDNIR = 0.389          
      


      FRSU   = 1./KB/TLAI*(1.-EXP(-KB*TLAI))
      FRSH   = 1.-FRSU


      GBHLF  = 0.01*SQRT(WND/LWIDTH)
      RBHSU  = RB/(FRSU*LAI)     
      RBWSU  = RB/(FRSU*LAI)     
      RBHSH  = RB/(FRSH*LAI)     
      RBWSH  = RB/(FRSH*LAI)     
      RCAN   = WNM*10.


      RBHS   = 172.*SQRT(0.05/MAX(0.1,WND*EXP(-KW*TLAI)))
      RBWS   = 0.93*RBHS


     CALL PAN (SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH)
     CALL PAN (SLNN,SLNMIN,LAI,KN,KB, NPSUN,NPSHN)  





      CALL LIGAB (SCPPAR,KB,KBPPAR,KDPPAR,PCBPAR,PCDPAR,PARDR,PARDF,TLAI,APARSU,APARSH)
      CALL LIGAB (SCPNIR,KB,KBPNIR,KDPNIR,PCBNIR,PCDNIR,NIRDR,NIRDF,TLAI,ANIRSU,ANIRSH)	
      APAR   = APARSU+APARSH 


      ATRJSU = APARSU+ANIRSU    
      ATRJSH = APARSH+ANIRSH    
      ATRJC  = ATRJSH + ATRJSU  


      PSPAR  = 0.1                                  
      PSNIR  = INSW(WCUL-0.5, 0.52-0.68*WCUL, 0.18) 
      ATRJS=(1.-PSPAR)*(PARDR*EXP(-KBPPAR*TLAI)+PARDF*EXP(-KDPPAR*TLAI)) &
           +(1.-PSNIR)*(NIRDR*EXP(-KBPNIR*TLAI)+NIRDF*EXP(-KDPNIR*TLAI))
      

     FSR = RSD-ATRJC-ATRJS
    
     if (RSD.gt.0) then
         Albedo = (RSD-ATRJC-ATRJS)/RSD
     else
         Albedo = .2
     endif


     CALL PPHTR(FRSU,TAIR,DVP,CO2A,C3C4,FVPD,APARSU,NPSU,RBWSU,RBHSU, &
                 RT*FRSU,ATRJSU,ATMTR,EAJMAX,XVN,XJN,THETA,PLFSU, &
                 PTSU,PHSU,RSWSU,NRADSU,SLOPSU)

     CALL PPHTR(FRSH,TAIR,DVP,CO2A,C3C4,FVPD,APARSH,NPSH,RBWSH,RBHSH, &
                 RT*FRSH,ATRJSH,ATMTR,EAJMAX,XVN,XJN,THETA,PLFSH, &
                 PTSH,PHSH,RSWSH,NRADSH,SLOPSH)

     IPP    = PLFSU + PLFSH   
     IPT    = PTSU + PTSH    
     IPH    = PHSU + PHSH    
     ANRAD  = NRADSU + NRADSH 




      PT1    = IPT  * SD1/RD


      CALL PEVAP (TAIR,DVP,RSS,RTS,RBWS,RBHS,ATRJS,ATMTR, &
                  PT1,WSUP1,IPE,IPHSOIL,NRADS)



      IAE    = MIN (IPE,IPE/(PT1+IPE)*WSUP1)            
      IAT    = MIN (IPT,PT1/(PT1+IPE)*WSUP1+WSUP-WSUP1) 
      ATSU   = PTSU/IPT*IAT                             
      ATSH   = PTSH/IPT*IAT                             
   
      CALL DIFLA (NRADS,IAE,RBHS,RTS, ADIFS)

      CALL APHTR (TAIR,APARSU,DVP,CO2A,C3C4,FVPD,NRADSU,ATSU,PTSU, &
                  RT*FRSU,RBHSU,RBWSU,RSWSU,SLOPSU,NPSU,NPSUN, &
                  EAJMAX,XVN,XJN,THETA,PASSU,PANSU,ADIFSU,ARSWSU)
      
      CALL APHTR (TAIR,APARSH,DVP,CO2A,C3C4,FVPD,NRADSH,ATSH,PTSH, &
                  RT*FRSH,RBHSH,RBWSH,RSWSH,SLOPSH,NPSH,NPSHN, &
                  EAJMAX,XVN,XJN,THETA,PASSH,PANSH,ADIFSH,ARSWSH)
      
      IAPS   = PASSU + PASSH 
      IAPN   = PANSU + PANSH 
      IAP    = IAPS
      IAPNN  = IAPN


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
      DNRAD  = ANRAD    
      DATRJC = ATRJC    
      DATRJS = ATRJS    
      CUMRSD = RSD      
      AV_RSWSU = RSWSU
      AV_RSWSH = RSWSH 
      AV_Albedo = Albedo
      TLEAFSU = TAIR + DIFSU
      TLEAFSH = TAIR + DIFSH

      RETURN
      END SUBROUTINE TOTPT


































      SUBROUTINE PPHTR(FRAC,TAIR,DVP,CO2A,C3C4,FVPD,PAR,NP,RBW,RBH,RT, &
            ATRJ,ATMTR,EAJMAX,XVN,XJN,THETA,PLF,PT,PH,RSW,NRADC,SLOPEL)
      IMPLICIT REAL (A-Z)


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



      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
      
      CALL DIFLA (FNRADC,FPT,RBH,RT, FDIF)

      TLEAF  = TAIR + FDIF



      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
  
      CALL DIFLA (FNRADC,FPT,RBH,RT, FDIF)

      TLEAF  = TAIR + FDIF



      CALL ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      CALL PHOTO (C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN,THETA,PLF,LRD)

      SLOPEL = (SVPL-SVP)/NOTNUL(TLEAF-TAIR)

      CALL GCRSW (PLF,LRD,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      CALL PTRAN (RSW,RT,RBW,RBH,ATRJ,ATMTR,FRAC,TLEAF,DVP, &
                  SLOPEL,VPD, PT, PH, NRADC)
      
      RETURN
      END SUBROUTINE PPHTR

























      SUBROUTINE PEVAP (TAIR,DVP,RSS,RTS,RBWS,RBHS,ATRJS,ATMTR, &
                        PT1,WSUP1,PESOIL,PHSOIL,NRADS)
      IMPLICIT REAL (A-Z)


	  SVP    = 0.611*EXP(17.4*TAIR/(TAIR+239.))
      VPD    = MAX (0., SVP-DVP)
      SLOPE  = 4158.6 * SVP/(TAIR + 239.)**2
      CALL PTRAN(RSS,RTS,RBWS,RBHS,ATRJS,ATMTR,1.,TAIR,DVP, &
                 SLOPE,VPD, FPE, FPH, FNRADS)
      FPESOL = MAX(0., FPE)
      FAESOL = MIN(FPESOL,FPESOL/(PT1+FPESOL)*WSUP1)
      CALL DIFLA (FNRADS,FAESOL,RBHS,RTS, FDIFS)
      TAVS   = TAIR + FDIFS


      SVPS   = 0.611*EXP(17.4*TAVS/(TAVS+239.))
      SLOPES = (SVPS-SVP)/NOTNUL(FDIFS)

      CALL PTRAN(RSS,RTS,RBWS,RBHS,ATRJS,ATMTR,1.,TAVS,DVP, &
                 SLOPES,VPD, PE, PH, NRADS)
      PESOIL = MAX(0., PE)
      PHSOIL = PH
      
      RETURN
      END SUBROUTINE PEVAP


































      SUBROUTINE APHTR(TAIR,PAR,DVP,CO2A,C3C4,FVPD,NRADC,AT,PT,RT,RBH, &
                 RBW,RSW,SLOPEL,NP,NPN,EAJMAX,XVN,XJN,THETA, &
                 PLFAS,PLFAN,ADIF,ARSW)
      IMPLICIT REAL (A-Z)

      PSYCH  = 0.067            


      CALL DIFLA (NRADC,AT,RBH,RT, ADIF)
      ATLEAF = TAIR + ADIF


      ARSW = (PT-AT)*(SLOPEL*(RBH+RT)+PSYCH*(RBW+RT))/AT/PSYCH+PT/AT*RSW


      CALL ICO2 (ATLEAF,DVP,FVPD,CO2A,C3C4, SVPA,ACO2I)
      CALL PHOTO(C3C4,PAR,ATLEAF,ACO2I,NPN,EAJMAX,XVN,XJN,THETA,APLFN,ARDN)
      CALL PHOTO(C3C4,PAR,ATLEAF,ACO2I,NP,EAJMAX,XVN,XJN,THETA,APLF,ARD)


      PLFAS  = (1.6*RSW+1.3*RBW+RT)/(1.6*ARSW+1.3*RBW+RT)*(APLF-ARD)+ARD
      PLFAN  = (1.6*RSW+1.3*RBW+RT)/(1.6*ARSW+1.3*RBW+RT)*(APLFN-ARDN)+ARDN
      
      RETURN
      END SUBROUTINE APHTR

























      SUBROUTINE PTRAN (RSW,RT,RBW,RBH,ATRJ, &
                      ATMTR,FRAC,TLEAF,DVP,SLOPE,VPD,PT,PH,NRADC)
      IMPLICIT REAL (A-Z)


      BOLTZM = 5.668E-8         
      LHVAP  = 2.4E6            
      VHCA   = 1200.            
      PSYCH  = 0.067            


      CLEAR  = MAX(0., MIN(1., (ATMTR-0.25)/0.45))    
      BBRAD  = BOLTZM*(TLEAF +273.)**4
      RLWN   = BBRAD*(0.56-0.079*SQRT(DVP*10.))*(0.1+0.9*CLEAR)*FRAC
      NRADC  = ATRJ - RLWN


      PSR    = PSYCH*(RBW+RT+RSW)/(RBH+RT)



      PTR    = NRADC*SLOPE/(SLOPE+PSR)/LHVAP


      PTD    = (VHCA*VPD/(RBH+RT))/(SLOPE+PSR)/LHVAP
      

      PT     = MAX(1.E-10,PTR+PTD)




      PHR    = NRADC*PSR/(SLOPE+PSR)


      PHD    = (VHCA*VPD/(RBH+RT))/(SLOPE+PSR)


      PH     = PHR-PHD

      RETURN
      END SUBROUTINE PTRAN
















      SUBROUTINE DIFLA (NRADC,PT,RBH,RT, DIF)
      IMPLICIT REAL  (A-Z)
      
      LHVAP  = 2.4E6            
      VHCA   = 1200.            

      DIF    = LIMIT (-25., 25., (NRADC-LHVAP*PT)*(RBH+RT)/VHCA)

      RETURN
      END SUBROUTINE DIFLA
















      SUBROUTINE ICO2  (TLEAF,DVP,FVPD,CO2A,C3C4, SVPL,CO2I)
      IMPLICIT REAL (A-Z)
      

      SVPL   = 0.611 * EXP(17.4 * TLEAF / (TLEAF + 239.))
      VPDL   = MAX  (0., SVPL - DVP)


      KMC25  = INSW(C3C4, 650., 404.9) 


      KMO25  = INSW(C3C4, 450., 278.4) 


      O2     = 210.    
      EAVCMX = 65330.  
      EAKMC  = 79430.  
      EAKMO  = 36380.  
      EARD   = 46390.  
      RDVX25 = 0.0089  
      TO     = 298.15

      KMC    = KMC25*EXP((1./TO-1./(TLEAF+273.))*EAKMC/8.314)
      KMO    = KMO25*EXP((1./TO-1./(TLEAF+273.))*EAKMO/8.314)
      GAMMAX = 0.5*EXP(-3.3801+5220./(TLEAF+273.)/8.314)*O2*KMC/KMO


      RDVCX  = RDVX25*EXP((1./TO-1./(TLEAF+273.))*(EARD-EAVCMX)/8.314)
      GAMMA0 = (GAMMAX+RDVCX*KMC*(1.+O2/KMO))/(1.-RDVCX)
      GAMMA_  = INSW (C3C4, GAMMA0/10., GAMMA0)


      RCICA  = 1.-(1.-GAMMA_/CO2A)*(0.14+FVPD*VPDL)


      CO2I   = RCICA * CO2A
      
      RETURN
      END SUBROUTINE ICO2



















      SUBROUTINE GCRSW (PLEAF,RDLEAF,TLEAF,CO2A,CO2I,RBW,RT, RSW)
      IMPLICIT REAL (A-Z)
      


      GC  = MAX(1E-6,(PLEAF-RDLEAF)*(273.+TLEAF)/0.53717/(CO2A-CO2I))


      RSW = MAX(1E-30, 1./GC - RBW*1.3 - RT)/1.6

      RETURN
      END SUBROUTINE GCRSW


















      SUBROUTINE PAN(SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH)
      IMPLICIT REAL (A-Z)
      

      NPC   = SLNT*(1.-EXP(-KN*LAI))/KN-SLNMIN*LAI


      NPSU  = SLNT*(1.-EXP(-(KN+KB)*LAI))/(KN+KB)-SLNMIN*(1.-EXP(-KB*LAI))/KB
      NPSH  = NPC-NPSU
      
      RETURN
      END SUBROUTINE PAN























      SUBROUTINE PHOTO(C3C4,PAR,TLEAF,CO2I,NP,EAJMAX,XVN,XJN, &
                       THETA,PLEAF,RDLEAF)
      IMPLICIT REAL (A-Z)
      

      IF (C3C4.LT.0.) THEN
        KMC25  = 650.   
        KMO25  = 450.   
      ELSE
        KMC25  = 404.9  
        KMO25  = 278.4  
      ENDIF


      O2     = 210.    
      EAVCMX = 65330.  
      EAKMC  = 79430.  
      EAKMO  = 36380.  
      EARD   = 46390.  
      DEJMAX = 200000. 
      SJ     = 650.    
      PHI2M  = 0.85    
      HH     = 3.      
      KTMP   = 1.0     
      JTMAX  = 3.12
      TO     = 298.15
      

      UPAR   = 4.56*PAR 


      KMC    = KMC25*EXP((1./TO-1./(TLEAF+273.))*EAKMC/8.314)
      KMO    = KMO25*EXP((1./TO-1./(TLEAF+273.))*EAKMO/8.314)


      GAMMAX = 0.5*EXP(-3.3801+5220./(TLEAF+273.)/8.314)*O2*KMC/KMO


     VCT    =    EXP((1./TO-1./(TLEAF+273.))*EAVCMX/8.314)


     JT     =    EXP((1./TO-1./(TLEAF+273.))*EAJMAX/8.314)* &
                 (1.+EXP(SJ/8.314-DEJMAX/TO/8.314))/ &
                 (1.+EXP(SJ/8.314-1./(TLEAF+273.) *DEJMAX/8.314))

     VCMX   = XVN*VCT*NP
     JMAX   = XJN*JT *NP
  


      FPSEUD = 0.           
      IF (C3C4.LT.0.) THEN
        ZZ   = 0.2          
        CC   = 10.*CO2I     
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


      ALPHA2 = (1.-FCYC)/(1.+(1.-FCYC)/PHI2M)
      X      = ALPHA2*UPAR/MAX(1.E-10,JMAX)
      J2     = JMAX*(1.+X-((1.+X)**2.-4.*X*THETA)**0.5)/2./THETA


      VC     = VCMX * CC/(CC + KMC*(O2/KMO+1.))
      VJ     =  J2 * CC*(2.+FQ-FCYC)/HH/(SF+3.*CC+7.*GAMMAX)/(1.-FCYC)


      ALF    = (1.-GAMMAX/CC)*MIN(VC,VJ)
      PLEAF  = MAX(1.E-10, (1.E-6)*44.*ALF)


      RDVX25 = 0.0089      
      RDT    = EXP((1./TO-1./(TLEAF+273.))*EARD/8.314)
      RDLEAF = (1.E-6)*44. *RDVX25*(XVN*NP) * RDT

      RETURN
      END SUBROUTINE PHOTO














      SUBROUTINE REFL (SCP,KB, KBP,PCB)
      IMPLICIT REAL (A-Z)
      

      KBP    = KB*SQRT(1.-SCP)


      PH     = (1.-SQRT(1.-SCP))/(1.+SQRT(1.-SCP))


      PCB    = 1.-EXP(-2.*PH*KB/(1.+KB))

      RETURN
      END SUBROUTINE REFL






















      SUBROUTINE LIGAB (SCP,KB,KBP,KDP,PCB,PCD,IB0,ID0,LAI, ISU,ISH)
      IMPLICIT REAL (A-Z)
      

      IC     = (1.-PCB)*MAX(1e-30,IB0)*(1.-EXP(-KBP*LAI))+ &
               (1.-PCD)*MAX(1e-30,ID0)*(1.-EXP(-KDP*LAI))
      

      ISU    = (1.-SCP)*MAX(1e-30,IB0)*(1.-EXP(-KB *LAI))+(1.-PCD)*MAX(1e-30,ID0)/(KDP+KB)* &
               KDP*(1.-EXP(-(KDP+KB)*LAI))+MAX(1e-30,IB0)*((1.-PCB)/(KBP+KB)*KBP* &
               (1.-EXP(-(KBP+KB)*LAI))-(1.-SCP)*(1.-EXP(-2.*KB*LAI))/2.)

      ISH    = IC-ISU

      RETURN
      END SUBROUTINE LIGAB














      SUBROUTINE KBEAM (SINB,BL, KB)
      IMPLICIT REAL (A-Z)
      

      B      = ASIN(SINB)
      

      IF (SINB.GE.SIN(BL)) THEN
          OAV = SINB*COS(BL)
      ELSE
          OAV = 2./3.141592654*(SINB*COS(BL)*ASIN(TAN(B)/TAN(BL)) &
          +((SIN(BL))**2-SINB**2)**0.5)
      ENDIF


      KB     = OAV/SINB

      RETURN
      END SUBROUTINE KBEAM















      SUBROUTINE KDIFF (LAI,BL,SCP, KDP)
      IMPLICIT REAL (A-Z)
      
      PI    = 3.141592654


      CALL KBEAM (SIN(15.*PI/180.),BL, KB15)
      CALL KBEAM (SIN(45.*PI/180.),BL, KB45)
      CALL KBEAM (SIN(75.*PI/180.),BL, KB75)


      KDP   = -1./LAI*LOG(0.178*EXP(-KB15*(1.-SCP)**0.5*LAI) &
                        +0.514*EXP(-KB45*(1.-SCP)**0.5*LAI) &
                        +0.308*EXP(-KB75*(1.-SCP)**0.5*LAI))

      RETURN
      END SUBROUTINE KDIFF












FUNCTION INTGRL(POOL, RATE, DT)
IMPLICIT NONE
real:: INTGRL, POOL, RATE, DT

INTGRL = POOL + RATE*DT

RETURN
END FUNCTION INTGRL










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
