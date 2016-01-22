MODULE module_Routing
#ifdef MPP_LAND
   use module_gw_baseflow, only: pix_ct_1
   use module_HYDRO_io, only: mpp_read_routedim, mpp_read_routing, mpp_read_chrouting_new, &
                              mpp_read_simp_gw
#else
   !yw use module_HYDRO_io, only: read_routedim, read_routing_old, read_chrouting,read_simp_gw
   use module_HYDRO_io, only: read_routedim, read_routing_seq, read_chrouting,read_simp_gw
#endif
   use module_HYDRO_io, only: readgw2d
   use module_HYDRO_utils 
   IMPLICIT NONE


CONTAINS


   subroutine rt_allocate(did,ix,jx,ixrt,jxrt,nsoil,CHANRTSWCRT)   
      use module_RT_data, only: rt_domain
      implicit none
      integer ixrt,jxrt, ix,jx,nsoil,NLINKS, CHANRTSWCRT, NLAKES
      integer istatus, did
      if(rt_domain(did)%allo_status .eq. 1) return
      rt_domain(did)%allo_status = 1

      rt_domain(did)%ix = ix
      rt_domain(did)%jx = jx
      rt_domain(did)%ixrt = ixrt
      rt_domain(did)%jxrt = jxrt
!     ixrt = rt_domain(did)%ixrt
!     jxrt = rt_domain(did)%jxrt
      
      NLINKS = rt_domain(did)%NLINKS
      NLAKES = rt_domain(did)%NLAKES

      if(rt_domain(did)%NLINKS .eq. 0) NLINKS = 1
      if(rt_domain(did)%NLAKES .eq. 0) NLAKES = 1

!DJG Allocate routing and disaggregation arrays

#ifdef HYDRO_D
  write(6,*) "  rt_allocate ***** ixrt,jxrt, nsoil", ixrt,jxrt, nsoil
#endif


  allocate( rt_domain(did)%DSMC   	(NSOIL) )
  rt_domain(did)%dsmc = 0 


  allocate( rt_domain(did)%SMCRTCHK    	(NSOIL) )
    rt_domain(did)%SMCRTCHK = 0

  allocate( rt_domain(did)%SH2OAGGRT   	(NSOIL) )
    rt_domain(did)%SH2OAGGRT = 0
  allocate( rt_domain(did)%STCAGGRT   	(NSOIL) )
  allocate( rt_domain(did)%SMCAGGRT   	(NSOIL) )
    rt_domain(did)%STCAGGRT = 0
  rt_domain(did)%SMCAGGRT = 0


  allocate( rt_domain(did)%SMCRT   	(IXRT,JXRT,NSOIL) )
  allocate( rt_domain(did)%ELRT   	(IXRT,JXRT) )
  allocate( rt_domain(did)%SOXRT  	(IXRT,JXRT) )
  allocate( rt_domain(did)%SOYRT   	(IXRT,JXRT) )
  allocate( rt_domain(did)%SO8RT   	(IXRT,JXRT,8) )
  allocate( rt_domain(did)%SO8RT_D   	(IXRT,JXRT,3) )
  allocate( rt_domain(did)%OVROUGHRT   (IXRT,JXRT) )
!  allocate( rt_domain(did)%QSUBBDRYTRT   (IXRT,JXRT) )
!  rt_domain(did)%QSUBBDRYTRT = 0.0

  allocate( rt_domain(did)%OVROUGHRTFAC   (IXRT,JXRT) )
  allocate( rt_domain(did)%RETDEPRT    (IXRT,JXRT) )
  allocate( rt_domain(did)%RETDEPRTFAC    (IXRT,JXRT) )
  allocate( rt_domain(did)%SFCHEADSUBRT(IXRT,JXRT) )
  allocate( rt_domain(did)%INFXSUBRT   (IXRT,JXRT) )
  allocate( rt_domain(did)%INFXSWGT    (IXRT,JXRT) )
  allocate( rt_domain(did)%LKSATRT     (IXRT,JXRT) )
  allocate( rt_domain(did)%LKSATFAC    (IXRT,JXRT) )
  allocate( rt_domain(did)%QSUBRT      (IXRT,JXRT) )
  allocate( rt_domain(did)%ZWATTABLRT  (IXRT,JXRT) )
  allocate( rt_domain(did)%QSUBBDRYRT  (IXRT,JXRT) )
  allocate( rt_domain(did)%SOLDEPRT    (IXRT,JXRT) )
  allocate( rt_domain(did)%q_sfcflx_x  (IXRT,JXRT) )
  allocate( rt_domain(did)%q_sfcflx_y  (IXRT,JXRT) )
  allocate( rt_domain(did)%SMCMAXRT   	(IXRT,JXRT,NSOIL) )
  allocate( rt_domain(did)%SMCWLTRT   	(IXRT,JXRT,NSOIL) )
  allocate( rt_domain(did)%SH2OWGT   	(IXRT,JXRT,NSOIL) )
  allocate( rt_domain(did)%INFXSAGGRT 	(IXRT,JXRT) )
  allocate( rt_domain(did)%DHRT   	(IXRT,JXRT) )
  allocate( rt_domain(did)%QSTRMVOLRT  (IXRT,JXRT) )


  allocate( rt_domain(did)%QSTRMVOLRT_TS  (IXRT,JXRT) )
  allocate( rt_domain(did)%QSTRMVOLRT_DUM  (IXRT,JXRT) )
  allocate( rt_domain(did)%QBDRYRT   	(IXRT,JXRT) )
  allocate( rt_domain(did)%CH_NETRT   	(IXRT,JXRT) )
  allocate( rt_domain(did)%LAKE_MSKRT 	(IXRT,JXRT) )
  allocate( rt_domain(did)%LAKE_INFLORT(IXRT,JXRT) )
  allocate( rt_domain(did)%LAKE_INFLORT_TS(IXRT,JXRT) )
  allocate( rt_domain(did)%LAKE_INFLORT_DUM(IXRT,JXRT) )

  allocate( rt_domain(did)%SUB_RESID (ixrt,jxrt) )
  allocate( rt_domain(did)%LATVAL (ixrt,jxrt) )
  allocate( rt_domain(did)%LONVAL (ixrt,jxrt) )
  allocate( rt_domain(did)%dist (ixrt,jxrt,9) )

! tmp array 
  allocate( rt_domain(did)%SMCREFRT    	(IXRT,JXRT,NSOIL) )
!!!! tmp

    rt_domain(did)%dist = -999  
    rt_domain(did)%SMCRT   	= 0.0                
    rt_domain(did)%ELRT   	= 0.0                
    rt_domain(did)%SOXRT  	= 0.0                
    rt_domain(did)%SOYRT   	= 0.0                
    rt_domain(did)%SO8RT   	= -999               
    rt_domain(did)%SO8RT_D   	= 0.0                
    rt_domain(did)%OVROUGHRT   = 0.0                
    rt_domain(did)%SFCHEADSUBRT= 0.0                
    rt_domain(did)%INFXSUBRT   = 0.0                
    rt_domain(did)%INFXSWGT    = 0.0                
    rt_domain(did)%LKSATRT     = 0.0                
    rt_domain(did)%LKSATFAC    = 0.0                
    rt_domain(did)%QSUBRT      = 0.0                
    rt_domain(did)%ZWATTABLRT  = 0.0                
    rt_domain(did)%QSUBBDRYRT  = 0.0                
    rt_domain(did)%SOLDEPRT    = 0.0                
    rt_domain(did)%q_sfcflx_x  = 0.0                
    rt_domain(did)%q_sfcflx_y  = 0.0                
    rt_domain(did)%SMCMAXRT   	= 0.0                
    rt_domain(did)%SMCWLTRT   	= 0.0                
    rt_domain(did)%SH2OWGT           = 0.0
    rt_domain(did)%INFXSAGGRT 	= 0.0                
    rt_domain(did)%DHRT   	= 0.0                
    rt_domain(did)%QSTRMVOLRT  = 0.0
    rt_domain(did)%QSTRMVOLRT_DUM  = 0.0                
    rt_domain(did)%QBDRYRT   	= 0.0                
    rt_domain(did)%CH_NETRT   	= 0.0                
    rt_domain(did)%LAKE_MSKRT 	= -9999              
    rt_domain(did)%LAKE_INFLORT= 0.0                
    rt_domain(did)%LAKE_INFLORT_DUM= 0.0                

    rt_domain(did)%SUB_RESID = 0.0                
    rt_domain(did)%LATVAL = 0.0                
    rt_domain(did)%LONVAL = 0.0


    rt_domain(did)%timestep_flag = 1    ! default is cold start

   IF (CHANRTSWCRT.EQ.1 .or. CHANRTSWCRT .eq. 2) THEN  !IF/then for channel routing
  allocate( rt_domain(did)%CH_NETLNK (IXRT,JXRT) )
            rt_domain(did)%CH_NETLNK = 0.0           
  allocate( rt_domain(did)%GCH_NETLNK (IXRT,JXRT) )
            rt_domain(did)%GCH_NETLNK = 0.0           

!DJG,DNY Allocate channel routing and lake routing arrays

#ifdef MPP_LAND
     allocate( rt_domain(did)%LAKE_INDEX(NLAKES) )
     allocate( rt_domain(did)%nlinks_INDEX(NLINKS) )
     allocate( rt_domain(did)%Link_location(ixrt,jxrt))
#endif
     

     allocate( rt_domain(did)%LINK(NLINKS) )
     allocate( rt_domain(did)%TO_NODE(NLINKS) )
     allocate( rt_domain(did)%FROM_NODE(NLINKS) )
     allocate( rt_domain(did)%TYPEL(NLINKS) )
     allocate( rt_domain(did)%ORDER(NLINKS) )
     allocate( rt_domain(did)%STRMFRXSTPTS(NLINKS) )
     allocate( rt_domain(did)%MUSK(NLINKS) )
     allocate( rt_domain(did)%MUSX(NLINKS) )
     allocate( rt_domain(did)%CHANXI(NLINKS) )
     allocate( rt_domain(did)%CHANYJ(NLINKS) )
     allocate( rt_domain(did)%CHLAT(NLINKS) )   !-latitutde of channel grid point
     allocate( rt_domain(did)%CHLON(NLINKS) )   !-longitude of channel grid point
     allocate( rt_domain(did)%CHANLEN(NLINKS) )
     allocate( rt_domain(did)%So(NLINKS) )
     allocate( rt_domain(did)%ChSSlp(NLINKS) )
     allocate( rt_domain(did)%Bw(NLINKS) )
     allocate( rt_domain(did)%ZELEV(NLINKS) )
     allocate( rt_domain(did)%CVOL(NLINKS) )
     allocate( rt_domain(did)%HRZAREA(NLAKES) )
     allocate( rt_domain(did)%LAKEMAXH(NLAKES) )
     allocate( rt_domain(did)%WEIRC(NLAKES) )
     allocate( rt_domain(did)%WEIRL(NLAKES) )
     allocate( rt_domain(did)%ORIFICEC(NLAKES) )
     allocate( rt_domain(did)%ORIFICEA(NLAKES) )
     allocate( rt_domain(did)%ORIFICEE(NLAKES) )
     allocate( rt_domain(did)%LATLAKE(NLAKES) )
     allocate( rt_domain(did)%LONLAKE(NLAKES) )
     allocate( rt_domain(did)%ELEVLAKE(NLAKES) )
     allocate( rt_domain(did)%LAKENODE(NLINKS) )
     allocate( rt_domain(did)%RESHT(NLAKES),STAT=istatus )
     allocate( rt_domain(did)%QLAKEI(NLAKES),STAT=istatus )
     allocate( rt_domain(did)%QLAKEO(NLAKES),STAT=istatus )
     allocate( rt_domain(did)%QLINK(NLINKS,2) )

     allocate( rt_domain(did)%HLINK(NLINKS) )  !--used for diffusion only
     allocate( rt_domain(did)%MannN(NLINKS))

     allocate( rt_domain(did)%node_area(NLINKS) )

!!!! tmp
      rt_domain(did)%LINK = 0.0        
      rt_domain(did)%TO_NODE = 0.0        
      rt_domain(did)%FROM_NODE = 0        
      rt_domain(did)%TYPEL = 0.0        
      rt_domain(did)%ORDER = 0.0        
      rt_domain(did)%STRMFRXSTPTS = 0.0        
      rt_domain(did)%MUSK = 0.0        
      rt_domain(did)%MUSX = 0.0        
      rt_domain(did)%CHANXI = 0.0        
      rt_domain(did)%CHANYJ = 0.0        
      rt_domain(did)%CHLAT = 0.0         !-latitutde of channel grid point
      rt_domain(did)%CHLON = 0.0         !-longitude of channel grid point
      rt_domain(did)%CHANLEN = 0.0        
      rt_domain(did)%ChSSlp = 0.0        
      rt_domain(did)%Bw = 0.0        
      rt_domain(did)%ZELEV = 0.0        
      rt_domain(did)%CVOL = 0.0        
      rt_domain(did)%HRZAREA = 0.0        
      rt_domain(did)%LAKEMAXH = 0.0        
      rt_domain(did)%WEIRC = 0.0        
      rt_domain(did)%WEIRL = 0.0        
      rt_domain(did)%ORIFICEC = 0.0        
      rt_domain(did)%ORIFICEA = 0.0        
      rt_domain(did)%ORIFICEE = 0.0        
      rt_domain(did)%LATLAKE = 0.0        
      rt_domain(did)%LONLAKE = 0.0        
      rt_domain(did)%ELEVLAKE = 0.0        
      rt_domain(did)%LAKENODE = 0.0        
      rt_domain(did)%RESHT = 0.0                    
      rt_domain(did)%QLAKEI = 0.0                     
      rt_domain(did)%QLAKEO = 0.0                     
      rt_domain(did)%QLINK = 0        

      rt_domain(did)%HLINK = 0.0        !--used for diffusion only
      rt_domain(did)%MannN = 0.0        

     rt_domain(did)%So = 0.01
  END IF   !IF/then for channel routing


  !DJG Allocate routing and disaggregation arrays
  allocate(rt_domain(did)%qinflowbase  (IXRT,JXRT) )
  allocate(rt_domain(did)%gw_strm_msk  (IXRT,JXRT) )

!!! allocate land surface grid variables
 allocate( rt_domain(did)%SMC  (IX,JX,NSOIL) )
 allocate( rt_domain(did)%SICE (IX,JX,NSOIL) )
! allocate( rt_domain(did)%dist_lsm (ixrt,jxrt,9) )
! allocate( rt_domain(did)%lat_lsm (ixrt,jxrt) )
! allocate( rt_domain(did)%lon_lsm (ixrt,jxrt) )
 allocate( rt_domain(did)%dist_lsm (ix,jx,9) )
 allocate( rt_domain(did)%lat_lsm (ix,jx) )
 allocate( rt_domain(did)%lon_lsm (ix,jx) )

! allocate( rt_domain(did)%SICE  (IX,JX,NSOIL) )
 allocate( rt_domain(did)%SMCMAX1  (IX,JX) )
  allocate( rt_domain(did)%STC  (IX,JX,NSOIL) )
  allocate( rt_domain(did)%SH2OX(IX,JX,NSOIL) )
  allocate( rt_domain(did)%SMCWLT1  (IX,JX) )
  allocate( rt_domain(did)%SMCREF1  (IX,JX) )
  allocate( rt_domain(did)%VEGTYP   (IX,JX) )
  allocate( rt_domain(did)%GWSUBBASMSK   (IX,JX) )
  allocate( rt_domain(did)%SLDPTH(NSOIL) )
  allocate( rt_domain(did)%SO8LD_D   (IX,JX,3) )
  allocate( rt_domain(did)%SO8LD_Vmax   (IX,JX) )
  allocate( rt_domain(did)%SFCHEADRT   (IX,JX) )
  allocate( rt_domain(did)%INFXSRT   (IX,JX) )
  allocate( rt_domain(did)%TERRAIN   (IX,JX) )
  allocate( rt_domain(did)%LKSAT   (IX,JX) )
  allocate( rt_domain(did)%SOLDRAIN   (IX,JX) )


  rt_domain(did)%dist_lsm = 0.0 

  rt_domain(did)%qinflowbase = 0.0           
  rt_domain(did)%gw_strm_msk   = 0         
  rt_domain(did)%SMC   = 0.25           
  rt_domain(did)%SICE  = 0.
! rt_domain(did)%SMCMAX1   = 0.434          
  rt_domain(did)%SMCMAX1   = 0.0
   rt_domain(did)%STC   = 282.0          
   rt_domain(did)%SH2OX = rt_domain(did)%SMC   
   rt_domain(did)%SMCWLT1   = 0.0            
   rt_domain(did)%SMCREF1   = 0.0            
   rt_domain(did)%VEGTYP    = 0            
   rt_domain(did)%GWSUBBASMSK    = 0              
   rt_domain(did)%SLDPTH = 0.0           
   rt_domain(did)%SO8LD_D    = 0.0           
   rt_domain(did)%SO8LD_Vmax    = 0.0            
   rt_domain(did)%SFCHEADRT    = 0.0            
   rt_domain(did)%INFXSRT    = 0.0            
   rt_domain(did)%TERRAIN    = 0.0            
   rt_domain(did)%LKSAT    = 0.0            
   rt_domain(did)%SOLDRAIN    = 0.0            

   rt_domain(did)%out_counts = 0
   rt_domain(did)%his_out_counts = 0
   rt_domain(did)%rst_counts = 1

#ifdef HYDRO_D
  write(6,*) "***** finish rt_allocate "
#endif

   end subroutine rt_allocate

   subroutine getChanDim(did)

  
      use module_namelist, only:  nlst_rt 
      use module_RT_data, only: rt_domain
      implicit none
      
      integer ixrt,jxrt, ix,jx, did
      INTEGER, allocatable,dimension(:,:) :: CH_NETLNK, GCH_NETLNK
!      INTEGER, dimension( rt_domain(did)%ixrt,GCH_NETLNK(ixrt,jxrt)) :: GCH_NETLNK, CH_NETLNK

      real :: Vmax
     
      ix = rt_domain(did)%ix 
      jx = rt_domain(did)%jx 
      ixrt = rt_domain(did)%ixrt 
      jxrt = rt_domain(did)%jxrt 

   
       allocate(CH_NETLNK(ixrt,jxrt)) 
       allocate(GCH_NETLNK(ixrt,jxrt)) 

      IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2) THEN  !IF/then for channel routing
#ifdef MPP_LAND
        CALL MPP_READ_ROUTEDIM(did, rt_domain(did)%g_IXRT,rt_domain(did)%g_JXRT, GCH_NETLNK, rt_domain(did)%GNLINKS, &
#else
        CALL READ_ROUTEDIM( &
#endif
              IXRT, JXRT, nlst_rt(did)%route_chan_f, nlst_rt(did)%route_link_f, &
              nlst_rt(did)%route_direction_f, nlst_rt(did)%route_lake_f, &
              rt_domain(did)%NLINKS, rt_domain(did)%NLAKES, &
              CH_NETLNK, nlst_rt(did)%channel_option, nlst_rt(did)%geo_finegrid_flnm)

#ifdef HYDRO_D
        write(6,*) "before rt_allocate after READ_ROUTEDIM"
#endif

#ifndef MPP_LAND
         GCH_NETLNK = CH_NETLNK
#endif

      end if

        call rt_allocate(did,rt_domain(did)%ix,rt_domain(did)%jx,&
                   rt_domain(did)%ixrt,rt_domain(did)%jxrt, nlst_rt(did)%nsoil,nlst_rt(did)%CHANRTSWCRT)

      IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2) THEN  !IF/then for channel routing
        rt_domain(did)%CH_NETLNK = CH_NETLNK
        rt_domain(did)%GCH_NETLNK = GCH_NETLNK
      endif

       deallocate(CH_NETLNK)
       deallocate(GCH_NETLNK)

   end subroutine getChanDim
   
   subroutine LandRT_ini(did)

      use module_noah_chan_param_init_rt
      use module_namelist, only:  nlst_rt
      use module_RT_data, only: rt_domain
      use module_gw_baseflow_data, only: gw2d

#ifdef MPP_LAND
      USE module_mpp_land
#endif
      implicit none 
      integer did
      real Vmax

     integer :: bas , bas_id
     CHARACTER(len=19)                      :: header
     CHARACTER(len=1)                       :: jnk

     REAL,  DIMENSION(50)     :: BOTWID,HLINK_INIT,CHAN_SS,CHMann !Channel parms from table
     integer :: i,j
 
     integer, allocatable, dimension(:) :: tmp_int
     real, allocatable, dimension(:) :: tmp_real
     
     
!------------------------------------------------------------------------
!DJG Routing Processing
!------------------------------------------------------------------------
!DJG IF/then to get routing terrain fields if either routing module is 
!DJG   activated


  IF (nlst_rt(did)%SUBRTSWCRT.EQ.1.OR.nlst_rt(did)%OVRTSWCRT.EQ.1 .or. nlst_rt(did)%GWBASESWCRT .ne. 0) THEN
#ifdef HYDRO_D
  print *, "Terrain routing initialization..."
#endif

#ifdef MPP_LAND
    CALL MPP_READ_ROUTING( &
#else
     CALL READ_ROUTING_seq  (  &
#endif
          rt_domain(did)%IXRT,rt_domain(did)%JXRT,rt_domain(did)%ELRT,rt_domain(did)%CH_NETRT, &
          rt_domain(did)%LKSATFAC,trim(nlst_rt(did)%route_topo_f),&
          nlst_rt(did)%route_chan_f,nlst_rt(did)%geo_finegrid_flnm  ,  &
#ifdef MPP_LAND
           rt_domain(did)%g_IXRT,rt_domain(did)%g_JXRT, &
#endif
          rt_domain(did)%OVROUGHRTFAC,rt_domain(did)%RETDEPRTFAC)

    !yw CALL READ_ROUTING_old(rt_domain(did)%IXRT,rt_domain(did)%JXRT,rt_domain(did)%ELRT,rt_domain(did)%CH_NETRT, &



    IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2) THEN  !IF/then for channel routing

#ifdef MPP_LAND
          CALL MPP_READ_CHROUTING_new(did,    &
#else
          CALL READ_CHROUTING(    &
#endif
               rt_domain(did)%IXRT,rt_domain(did)%JXRT,rt_domain(did)%ELRT,rt_domain(did)%CH_NETRT, &
               rt_domain(did)%LAKE_MSKRT, &
               rt_domain(did)%FROM_NODE, rt_domain(did)%TO_NODE, rt_domain(did)%TYPEL, rt_domain(did)%ORDER, &
               rt_domain(did)%MAXORDER,rt_domain(did)%NLINKS, &
               rt_domain(did)%NLAKES, rt_domain(did)%MUSK, rt_domain(did)%MUSX, rt_domain(did)%QLINK,&
               rt_domain(did)%CHANLEN, rt_domain(did)%MannN, rt_domain(did)%So, rt_domain(did)%ChSSlp, rt_domain(did)%Bw, &
               rt_domain(did)%HRZAREA, rt_domain(did)%LAKEMAXH, rt_domain(did)%WEIRC, rt_domain(did)%WEIRL, rt_domain(did)%ORIFICEC, &
               rt_domain(did)%ORIFICEA,  rt_domain(did)%ORIFICEE, rt_domain(did)%LATLAKE, rt_domain(did)%LONLAKE, &
               rt_domain(did)%ELEVLAKE , &
               nlst_rt(did)%route_link_f,nlst_rt(did)%route_lake_f, &
               nlst_rt(did)%route_direction_f, nlst_rt(did)%route_order_f, &
               nlst_rt(did)%CHANRTSWCRT,rt_domain(did)%dist, rt_domain(did)%ZELEV, rt_domain(did)%LAKENODE, rt_domain(did)%CH_NETLNK, &
               rt_domain(did)%CHANXI, rt_domain(did)%CHANYJ, &
               rt_domain(did)%CHLAT, rt_domain(did)%CHLON, nlst_rt(did)%channel_option,&
               rt_domain(did)%latval, rt_domain(did)%lonval,&
               rt_domain(did)%STRMFRXSTPTS,nlst_rt(did)%geo_finegrid_flnm        &
#ifdef MPP_LAND
               ,rt_domain(did)%g_IXRT,rt_domain(did)%g_JXRT   &
               ,rt_domain(did)%gnlinks,rt_domain(did)%GCH_NETLNK, rt_domain(did)%map_l2g, &
               rt_domain(did)%link_location, rt_domain(did)%yw_mpp_nlinks,rt_domain(did)%lake_index,rt_domain(did)%nlinks_index    &
#endif
               )


!yw #ifdef MPP_LAND
!yw          call MPP_CHROUTING_CONF(rt_domain(did)%g_ixrt,rt_domain(did)%g_jxrt,rt_domain(did)%ixrt,rt_domain(did)%jxrt, rt_domain(did)%NLAKES,rt_domain(did)%NLINKS,&
!yw               rt_domain(did)%lake_mskrt, rt_domain(did)%lake_index,rt_domain(did)%link_location,rt_domain(did)%HRZAREA,rt_domain(did)%LAKEMAXH,&
!yw               rt_domain(did)%WEIRC,rt_domain(did)%WEIRL,&
!yw               rt_domain(did)%ORIFICEC,rt_domain(did)%ORIFICEA,rt_domain(did)%ORIFICEE,rt_domain(did)%LATLAKE,rt_domain(did)%LONLAKE,rt_domain(did)%ELEVLAKE, &
!yw               rt_domain(did)%FROM_NODE,rt_domain(did)%TO_NODE,rt_domain(did)%ZELEV,rt_domain(did)%CHLAT,rt_domain(did)%CHLON,rt_domain(did)%TYPEL, &
!yw               rt_domain(did)%ORDER,rt_domain(did)%CHANLEN, &
!yw               rt_domain(did)%CHANXI,rt_domain(did)%CHANYJ, rt_domain(did)%lakenode,rt_domain(did)%mpp_nlinks, rt_domain(did)%nlinks_index, rt_domain(did)%maxorder, &
!yw               rt_domain(did)%yw_mpp_nlinks)
!!!!!   lake_index,Link_Location)
!yw#endif
!        call deriveFromNode(did)
    endif
  END IF

   
       allocate(tmp_int(rt_domain(did)%GNLINKS))
       allocate(tmp_real(rt_domain(did)%GNLINKS))
!      call write_chanel_int(rt_domain(did)%GCH_NETLNK,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,tmp_int)
!      call write_chanel_real(rt_domain(did)%chlon,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,rt_domain(did)%nlinks,tmp_real)
!       if(my_id .eq. IO_id) write(50,*) tmp_int
!      if(my_id .eq. IO_id) write(51,*) tmp_real
!      call hydro_stop("88888888")



!DJG Temporary hardwire of RETDEPRT,RETDEP_CHAN
!DJG    will later make this a function of SOLTYP and VEGTYP
!            OVROUGHRT(i,j) = 0.01

              rt_domain(did)%RETDEPRT = 0.001   ! units (mm)
              rt_domain(did)%RETDEP_CHAN = 0.001


!DJG Need to insert call for acquiring routing fields here...
!DJG     include as a subroutine in module module_Noahlsm_wrfcode_input.F
!DJG  Calculate terrain slopes 'SOXRT,SOYRT' from subgrid elevation 'ELRT'


        rt_domain(did)%so8rt = -999
        Vmax = 0.0
        do j=2,rt_domain(did)%JXRT-1
          do i=2,rt_domain(did)%IXRT-1
              rt_domain(did)%SOXRT(i,j)=(rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i+1,j))/rt_domain(did)%dist(i,j,3)
              rt_domain(did)%SOYRT(i,j)=(rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i,j+1))/rt_domain(did)%dist(i,j,1)
!DJG Introduce reduction in retention depth as a linear function of terrain slope
              IF (nlst_rt(did)%RT_OPTION.eq.2) then
                IF (rt_domain(did)%SOXRT(i,j).gt.rt_domain(did)%SOYRT(i,j)) then
                  Vmax=rt_domain(did)%SOXRT(i,j)
                ELSE
                  Vmax=rt_domain(did)%SOYRT(i,j)
                END IF

                IF (Vmax.gt.0.1) then
                  rt_domain(did)%RETDEPRT(i,j)=0.
                ELSE
                  rt_domain(did)%RETDEPFRAC=Vmax/0.1
                  rt_domain(did)%RETDEPRT(i,j)=rt_domain(did)%RETDEPRT(i,j)*(1.-rt_domain(did)%RETDEPFRAC)
                  IF (rt_domain(did)%RETDEPRT(i,j).lt.0.) rt_domain(did)%RETDEPRT(i,j)=0.
                END IF
              END IF

               rt_domain(did)%SO8RT(i,j,1) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i,j+1))/rt_domain(did)%dist(i,j,1)
               rt_domain(did)%SO8RT_D(i,j,1) = i
               rt_domain(did)%SO8RT_D(i,j,2) = j + 1 
               rt_domain(did)%SO8RT_D(i,j,3) = 1 
               Vmax = rt_domain(did)%SO8RT(i,j,1)
               
               rt_domain(did)%SO8RT(i,j,2) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i+1,j+1))/rt_domain(did)%dist(i,j,2)  
               if(rt_domain(did)%SO8RT(i,j,2) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i + 1
                 rt_domain(did)%SO8RT_D(i,j,2) = j + 1 
                 rt_domain(did)%SO8RT_D(i,j,3) = 2
                 Vmax = rt_domain(did)%SO8RT(i,j,2)
               end if

               rt_domain(did)%SO8RT(i,j,3) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i+1,j))/rt_domain(did)%dist(i,j,3)
               if(rt_domain(did)%SO8RT(i,j,3) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i + 1
                 rt_domain(did)%SO8RT_D(i,j,2) = j  
                 rt_domain(did)%SO8RT_D(i,j,3) = 3
                 Vmax = rt_domain(did)%SO8RT(i,j,3)
               end if

               rt_domain(did)%SO8RT(i,j,4) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i+1,j-1))/rt_domain(did)%dist(i,j,4)  
               if(rt_domain(did)%SO8RT(i,j,4) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i + 1
                 rt_domain(did)%SO8RT_D(i,j,2) = j - 1 
                 rt_domain(did)%SO8RT_D(i,j,3) = 4
                 Vmax = rt_domain(did)%SO8RT(i,j,4)
               end if

               rt_domain(did)%SO8RT(i,j,5) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i,j-1))/rt_domain(did)%dist(i,j,5)
               if(rt_domain(did)%SO8RT(i,j,5) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i 
                 rt_domain(did)%SO8RT_D(i,j,2) = j - 1 
                 rt_domain(did)%SO8RT_D(i,j,3) = 5
                 Vmax = rt_domain(did)%SO8RT(i,j,5)
               end if

               rt_domain(did)%SO8RT(i,j,6) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i-1,j-1))/rt_domain(did)%dist(i,j,6)  
               if(rt_domain(did)%SO8RT(i,j,6) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i - 1 
                 rt_domain(did)%SO8RT_D(i,j,2) = j - 1 
                 rt_domain(did)%SO8RT_D(i,j,3) = 6
                 Vmax = rt_domain(did)%SO8RT(i,j,6)
               end if

               rt_domain(did)%SO8RT(i,j,7) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i-1,j))/rt_domain(did)%dist(i,j,7)
               if(rt_domain(did)%SO8RT(i,j,7) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i - 1 
                 rt_domain(did)%SO8RT_D(i,j,2) = j  
                 rt_domain(did)%SO8RT_D(i,j,3) = 7
                 Vmax = rt_domain(did)%SO8RT(i,j,7)
               end if

               rt_domain(did)%SO8RT(i,j,8) = (rt_domain(did)%ELRT(i,j)-rt_domain(did)%ELRT(i-1,j+1))/rt_domain(did)%dist(i,j,8)  
               if(rt_domain(did)%SO8RT(i,j,8) .gt. Vmax ) then
                 rt_domain(did)%SO8RT_D(i,j,1) = i - 1 
                 rt_domain(did)%SO8RT_D(i,j,2) = j + 1 
                 rt_domain(did)%SO8RT_D(i,j,3) = 8
                 Vmax = rt_domain(did)%SO8RT(i,j,8)
               end if

!DJG Introduce reduction in retention depth as a linear function of terrain slope
              IF (nlst_rt(did)%RT_OPTION.eq.1) then
                IF (Vmax.gt.0.75) then
                  rt_domain(did)%RETDEPRT(i,j)=0.
                ELSE
                  rt_domain(did)%RETDEPFRAC=Vmax/0.75
                  rt_domain(did)%RETDEPRT(i,j)=rt_domain(did)%RETDEPRT(i,j)*(1.-rt_domain(did)%RETDEPFRAC)
                  IF (rt_domain(did)%RETDEPRT(i,j).lt.0.) rt_domain(did)%RETDEPRT(i,j)=0.
                END IF
              END IF


          end do
        end do


!Apply calibration scaling factors to sfc roughness and retention depth here...
                rt_domain(did)%RETDEPRT = rt_domain(did)%RETDEPRT * rt_domain(did)%RETDEPRTFAC
		rt_domain(did)%OVROUGHRT = rt_domain(did)%OVROUGHRT * rt_domain(did)%OVROUGHRTFAC



! calculate the slope for boundary        
#ifdef MPP_LAND
       if(right_id .lt. 0) rt_domain(did)%SOXRT(rt_domain(did)%IXRT,:)=rt_domain(did)%SOXRT(rt_domain(did)%IXRT-1,:)
       if(left_id  .lt. 0) rt_domain(did)%SOXRT(1,:)=rt_domain(did)%SOXRT(2,:)
       if(up_id    .lt. 0) rt_domain(did)%SOYRT(:,rt_domain(did)%JXRT)=rt_domain(did)%SOYRT(:,rt_domain(did)%JXRT-1)
       if(down_id  .lt. 0) rt_domain(did)%SOYRT(:,1)=rt_domain(did)%SOYRT(:,2)
#else
              rt_domain(did)%SOXRT(rt_domain(did)%IXRT,:)=rt_domain(did)%SOXRT(rt_domain(did)%IXRT-1,:)
              rt_domain(did)%SOXRT(1,:)=rt_domain(did)%SOXRT(2,:)
              rt_domain(did)%SOYRT(:,rt_domain(did)%JXRT)=rt_domain(did)%SOYRT(:,rt_domain(did)%JXRT-1)
              rt_domain(did)%SOYRT(:,1)=rt_domain(did)%SOYRT(:,2)
#endif

#ifdef MPP_LAND
! communicate the value to 
       call MPP_LAND_COM_REAL(rt_domain(did)%RETDEPRT,rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
       call MPP_LAND_COM_REAL(rt_domain(did)%SOXRT,rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
       call MPP_LAND_COM_REAL(rt_domain(did)%SOYRT,rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
       do i = 1, 8
          call MPP_LAND_COM_REAL(rt_domain(did)%SO8RT(:,:,i),rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
       end do
       do i = 1, 3
          call MPP_LAND_COM_INTEGER(rt_domain(did)%SO8RT_D(:,:,i),rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
       end do
#endif


!---------------------------------------------------------------------
!DJG  If GW/Baseflow activated...Read in req'd fields...
!----------------------------------------------------------------------
  IF (nlst_rt(did)%GWBASESWCRT.GE.1) THEN
      If (nlst_rt(did)%GWBASESWCRT.EQ.1.OR.nlst_rt(did)%GWBASESWCRT.EQ.2) THEN
#ifdef HYDRO_D
           print *, "new Simple GW-Bucket Scheme selected, retrieving files..."
#endif
#ifdef MPP_LAND
           CALL MPP_READ_SIMP_GW(              &
#else
           CALL READ_SIMP_GW(                  &
#endif
              rt_domain(did)%IX,rt_domain(did)%JX,rt_domain(did)%IXRT,&
              rt_domain(did)%JXRT,rt_domain(did)%GWSUBBASMSK,nlst_rt(did)%gwbasmskfil,&
              rt_domain(did)%gw_strm_msk,rt_domain(did)%numbasns,rt_domain(did)%ch_netrt,nlst_rt(did)%AGGFACTRT)
            allocate (rt_domain(did)%qout_gwsubbas (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%qin_gwsubbas (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%z_gwsubbas (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%gwbas_pix_ct (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%ct2_bas (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%bas_pcp (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%gw_buck_coeff (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%gw_buck_exp(rt_domain(did)%numbasns))
            allocate (rt_domain(did)%z_max (rt_domain(did)%numbasns))
            allocate (rt_domain(did)%basns_area (rt_domain(did)%numbasns))

#ifdef HYDRO_D
            write(6,*)  "end Simple GW-Bucket ..."
            print *, "Simple GW-Bucket Scheme selected, retrieving files..."
#endif

!Temporary hardwire...
      rt_domain(did)%z_gwsubbas = 1.     ! This gets updated with spun-up GW level in GWBUCKPARM.TBL

#ifdef MPP_LAND
   if(my_id .eq. IO_id) then
#endif
!Read in GW bucket params and Zinit from input file in Run directory...
     OPEN(81, FILE='GWBUCKPARM.TBL',FORM='FORMATTED',STATUS='OLD')
     read(81,811) header
811   FORMAT(A19)


     do bas = 1,rt_domain(did)%numbasns
       read(81,812) bas_id,jnk,rt_domain(did)%gw_buck_coeff(bas),jnk,rt_domain(did)%gw_buck_exp(bas),jnk,rt_domain(did)%z_max(bas),&
             jnk,rt_domain(did)%z_gwsubbas(bas)
!DJG...change bucket units to mm.... 812   FORMAT(I3,A1,F6.4,A1,F6.3,A1,F6.3,A1,F7.4)
812   FORMAT(I3,A1,F6.4,A1,F6.3,A1,F6.2,A1,F7.4)
     end do
     close(81)
#ifdef MPP_LAND
   endif
   call mpp_land_bcast_real(rt_domain(did)%numbasns,rt_domain(did)%gw_buck_coeff)
   call mpp_land_bcast_real(rt_domain(did)%numbasns,rt_domain(did)%gw_buck_exp  )
   call mpp_land_bcast_real(rt_domain(did)%numbasns,rt_domain(did)%z_max   )
   call mpp_land_bcast_real(rt_domain(did)%numbasns,rt_domain(did)%z_gwsubbas   )
#endif

!!! Determine number of stream pixels per GW basin for distribution...

#ifdef MPP_LAND
         call pix_ct_1(rt_domain(did)%gw_strm_msk,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%gwbas_pix_ct,rt_domain(did)%numbasns)
#else
         rt_domain(did)%gwbas_pix_ct = 0.
         do bas = 1, rt_domain(did)%numbasns
         do i=1,rt_domain(did)%ixrt
           do j=1,rt_domain(did)%jxrt
             if (rt_domain(did)%gw_strm_msk(i,j).eq.bas) then
                 rt_domain(did)%gwbas_pix_ct(rt_domain(did)%gw_strm_msk(i,j)) = & 
                 rt_domain(did)%gwbas_pix_ct(rt_domain(did)%gw_strm_msk(i,j))  + 1.0
             endif
           end do
         end do
         end do
#endif

#ifdef HYDRO_D
    print *, "Starting GW basin levels...",rt_domain(did)%z_gwsubbas
#endif
           

  ! BF gw2d model
      elseif (nlst_rt(did)%GWBASESWCRT.GE.3) THEN

        call readGW2d(gw2d(did)%ix, gw2d(did)%jx,     &
                      gw2d(did)%hycond, gw2d(did)%ho, &
		      gw2d(did)%bot, gw2d(did)%poros, &
		      gw2d(did)%ltype)

        gw2d(did)%elev = rt_domain(did)%elrt

    End if
    
  END IF
!---------------------------------------------------------------------
!DJG  End if GW/Baseflow activated...
!----------------------------------------------------------------------



!---------------------------------------------------------------------
!DJG,DNY  If channel routing activated...
!----------------------------------------------------------------------

  IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2) THEN

!---------------------------------------------------------------------
!DJG,DNY  Initalize lake and channel heights, this may be overwritten by RESTART
!--------------------------------------------------------------------

       if (nlst_rt(did)%channel_option .eq. 3) then
#ifdef MPP_LAND
        CALL mpp_CHAN_PARM_INIT (BOTWID,HLINK_INIT,CHAN_SS,CHMann)  !Read chan parms from table...
#else
        CALL CHAN_PARM_INIT (BOTWID,HLINK_INIT,CHAN_SS,CHMann)  !Read chan parms from table...
#endif
       end if

        do j=1,rt_domain(did)%NLINKS
           if (nlst_rt(did)%channel_option .ne. 3) then
            if (rt_domain(did)%TYPEL(j) .eq. 1) then !- for sparse network method this is a lake  (type 0 is river)
              rt_domain(did)%RESHT(j) = rt_domain(did)%LAKEMAXH(j) * 0.935  !-- assumes lake is ~90%
            endif
           else !-- parameterize according to order of diffusion scheme, or if read from hi res file, use its value
                !--  put condition within the if/then structure, which will assign a value if something is missing in hi res

             if (rt_domain(did)%ORDER(j) .eq. 1) then    !-- smallest stream reach
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .eq. 2) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .eq. 3) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .eq. 4) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .eq. 5) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .eq. 6) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             elseif (rt_domain(did)%ORDER(j) .ge. 7) then
               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(rt_domain(did)%ORDER(j))
               endif
               if(rt_domain(did)%MannN(j) .eq. 0.0) then 
                rt_domain(did)%MannN(j) = CHMann(rt_domain(did)%ORDER(j))
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(rt_domain(did)%ORDER(j))
             else   !-- the outlets won't have orders since there's no nodes, so
                    !-- assign the order 5 values

               if(rt_domain(did)%Bw(j) .eq. 0.0) then 
                rt_domain(did)%Bw(j) = BOTWID(5)
               endif
               if(rt_domain(did)%ChSSlp(j) .eq. 0.0) then  !if id didn't get set from the hi res file, use the  CHANPARAM
                rt_domain(did)%ChSSlp(j) = CHAN_SS(5)
               endif
              if(rt_domain(did)%MannN(j) .eq. 0.0) then 
               rt_domain(did)%MannN(j) = CHMann(5)
               endif
               rt_domain(did)%HLINK(j) = HLINK_INIT(5)
             endif
                
            rt_domain(did)%CVOL(j) = (rt_domain(did)%Bw(j)+ 1/rt_domain(did)%ChSSLP(j)*rt_domain(did)%HLINK(j))*rt_domain(did)%HLINK(j)*rt_domain(did)%CHANLEN(j) !-- initalize channel volume
           endif  !Endif channel option eq 3
        end do

        if (nlst_rt(did)%channel_option .eq. 3) then
           do j=1,rt_domain(did)%NLAKES
             rt_domain(did)%RESHT(j) = rt_domain(did)%LAKEMAXH(j) * 0.99   !-- lake is 99% full at start
           end do
        endif

!--------------------------------------------------------------------
  END IF     ! Endif for channel routing setup
!-----------------------------------------------------------------------

       rt_domain(did)%INFXSWGT = 1./(nlst_rt(did)%AGGFACTRT*nlst_rt(did)%AGGFACTRT)
       rt_domain(did)%SH2OWGT = 1.
  rt_domain(did)%SOLDEPRT = -1.0 * nlst_rt(did)%ZSOIL8(nlst_rt(did)%NSOIL)
  rt_domain(did)%QSUBRT = 0.0
  rt_domain(did)%ZWATTABLRT = 0.0
  rt_domain(did)%QSUBBDRYRT = 0.0
  rt_domain(did)%QSTRMVOLRT = 0.0
  rt_domain(did)%QSTRMVOLRT = 0.0
  rt_domain(did)%QBDRYRT = 0.0
  rt_domain(did)%SFCHEADSUBRT = 0.0
  rt_domain(did)%INFXSUBRT = 0.0
  rt_domain(did)%DHRT = 0.0
  rt_domain(did)%LAKE_INFLORT = 0.0
!  rt_domain(did)%LAKE_INFLORT_DUM = 0.0
  rt_domain(did)%LAKE_CT = 0
  rt_domain(did)%STRM_CT = 0
!  rt_domain(did)%QSTRMVOLRT_DUM = 0.0
  rt_domain(did)%SOLDRAIN = 0.0
  rt_domain(did)%qinflowbase = 0.0

!  rt_domain(did)%BASIN_MSK = 1
! !DJG Initialize mass balance check variables...
  rt_domain(did)%SMC_INIT=0.
  rt_domain(did)%DSMC=0.
  rt_domain(did)%DACRAIN=0.
  rt_domain(did)%DSFCEVP=0.
  rt_domain(did)%DCANEVP=0.
  rt_domain(did)%DEDIR=0.
  rt_domain(did)%DETT=0.
  rt_domain(did)%DEPND=0.
  rt_domain(did)%DESNO=0.
  rt_domain(did)%DSFCRNFF=0.
  rt_domain(did)%DQBDRY=0.
  rt_domain(did)%SUMINFXS1=0.
 


   end subroutine LandRT_ini

       subroutine deriveFromNode(did)
            implicit none
            integer :: did
            integer :: i,j, kk, maxv
            integer :: tmp(rt_domain(did)%nlinks)
            tmp = 0
            maxv = 1
            do i = 1, rt_domain(did)%nlinks
                if(rt_domain(did)%to_node(i) .gt. 0) then
                    kk = rt_domain(did)%to_node(i)
                    tmp(kk) = tmp(kk) + 1
                    if(maxv .lt. tmp(kk)) maxv = tmp(kk)
                end if
            end do
            allocate(rt_domain(did)%pnode(rt_domain(did)%nlinks,maxv+1) )
            rt_domain(did)%maxv_p = maxv+1
            rt_domain(did)%pnode = -99
            rt_domain(did)%pnode(:,1) = 1
            do i = 1, rt_domain(did)%nlinks
                if(rt_domain(did)%to_node(i) .gt. 0) then
                    j = rt_domain(did)%to_node(i)
                    rt_domain(did)%pnode(j,1) = rt_domain(did)%pnode(j,1) + 1
                    kk = rt_domain(did)%pnode(j,1)
                    rt_domain(did)%pnode(j,kk) = i
                end if
            end do

       end subroutine deriveFromNode



END MODULE module_Routing
