MODULE module_sf_bep_bem
use module_wrf_error

 USE module_sf_urban
 USE module_sf_bem









      integer nurbm           
      parameter (nurbm=11)

      integer ndm             
      parameter (ndm=2)

      integer nz_um           
      parameter(nz_um=16)

      integer ng_u            
      parameter (ng_u=10)
 
      integer ngr_u            
      parameter (ngr_u=10)

      integer nwr_u            
      parameter (nwr_u=10)

      integer nf_u             
      parameter (nf_u=10)

      integer ngb_u            
      parameter (ngb_u=10)

      real dz_u                
      parameter (dz_u=5.)

      integer nbui_max         
      parameter (nbui_max=7)   


      real h_water
      parameter(h_water=0.0009722) 







      integer p_num            
      parameter (p_num=2)
      integer q_num            
      parameter(q_num=4)       






           
      real vk                 
      real g_u                
      real pi                 
      real r                  
      real cp_u               
      real rcp_u              
      real sigma              
      real p0                 
      real latent             
      real dgmax              
      real drmax              

      parameter(vk=0.40,g_u=9.81,pi=3.141592653,r=287.,cp_u=1004.)        
      parameter(rcp_u=r/cp_u,sigma=5.67e-08,p0=1.e+5,latent=2.45e+06,dgmax=1.,drmax=1.)






   CONTAINS
 
      subroutine BEP_BEM(FRC_URB2D,UTYPE_URB2D,itimestep,dz8w,dt,u_phy,v_phy,      &
                      th_phy,rho,p_phy,swdown,glw,                    &
                      gmt,julday,xlong,xlat,                                       &
                      declin_urb,cosz_urb2d,omg_urb2d,                             &
                      num_urban_ndm,  urban_map_zrd,  urban_map_zwd, urban_map_gd, &
                      urban_map_zd,  urban_map_zdf,   urban_map_bd, urban_map_wd,  &
                      urban_map_gbd,  urban_map_fbd,                               &
                      urban_map_zgrd,  num_urban_hi,                               &
                      trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,                     &
                      tlev_urb3d,qlev_urb3d,tw1lev_urb3d,tw2lev_urb3d,             &
                      tglev_urb3d,tflev_urb3d,sf_ac_urb3d,lf_ac_urb3d,             &        
                      cm_ac_urb3d,                                                 & 
                      sfvent_urb3d,lfvent_urb3d,                                   &
                      sfwin1_urb3d,sfwin2_urb3d,                                   &
                      sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,                   &
		      ep_pv_urb3d,t_pv_urb3d,                                      &
		      trv_urb4d,qr_urb4d,qgr_urb3d,tgr_urb3d,                      &
                      drain_urb4d,draingr_urb3d,                                   &
		      sfrv_urb3d,lfrv_urb3d,                                       &
                      dgr_urb3d,dg_urb3d,                                          &
                      lfr_urb3d,lfg_urb3d,rainbl,swddir,swddif,                    &
                      lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,                        &
                      a_u,a_v,a_t,a_e,b_u,b_v,                                     &
                      b_t,b_e,b_q,dlg,dl_u,sf,vl,                                  &
                      rl_up,rs_abs,emiss,grdflx_urb,qv_phy,                        &
                      ids,ide, jds,jde, kds,kde,                                   &
                      ims,ime, jms,jme, kms,kme,                                   &
                      its,ite, jts,jte, kts,kte)                    

      implicit none




   INTEGER ::                       ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    itimestep
 

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   DZ8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   P_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   RHO
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   TH_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   T_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   U_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   V_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   U
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::   V
   REAL, DIMENSION( ims:ime , jms:jme )        ::   GLW
   REAL, DIMENSION( ims:ime , jms:jme )        ::   swdown
   REAL, DIMENSION( ims:ime , jms:jme )        ::   swddir
   REAL, DIMENSION( ims:ime , jms:jme )        ::   swddif
    REAL, DIMENSION( ims:ime, jms:jme )         ::   UST
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   UTYPE_URB2D
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   FRC_URB2D
   REAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, INTENT(IN  ) ::                               JULDAY
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                           XLAT, XLONG
   REAL, INTENT(IN) :: DECLIN_URB
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D
   INTEGER, INTENT(IN  ) :: urban_map_zrd
   INTEGER, INTENT(IN  ) :: urban_map_zwd
   INTEGER, INTENT(IN  ) :: urban_map_gd
   INTEGER, INTENT(IN  ) :: urban_map_zd
   INTEGER, INTENT(IN  ) :: urban_map_zdf
   INTEGER, INTENT(IN  ) :: urban_map_bd
   INTEGER, INTENT(IN  ) :: urban_map_wd
   INTEGER, INTENT(IN  ) :: urban_map_gbd
   INTEGER, INTENT(IN  ) :: urban_map_fbd
   INTEGER, INTENT(IN  ) :: num_urban_ndm
   INTEGER, INTENT(IN) :: num_urban_hi
   INTEGER , INTENT(IN)        ::     urban_map_zgrd
   REAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ), INTENT(INOUT) :: trv_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ), INTENT(INOUT) :: qr_urb4d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: qgr_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: tgr_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: drain_urb4d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: rainbl
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: draingr_urb3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ):: qv_phy
   REAL, DIMENSION( ims:ime, 1:urban_map_bd, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ep_pv_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: t_pv_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin2_urb3d


   REAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfrv_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: lfrv_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: dgr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: dg_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: lfg_urb3d 

   REAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL, DIMENSION( ims:ime,jms:jme), INTENT(IN) :: lp_urb2d
   REAL, DIMENSION( ims:ime,jms:jme), INTENT(IN) :: lb_urb2d
   REAL, DIMENSION( ims:ime,jms:jme), INTENT(IN) :: hgt_urb2d

   real z(ims:ime,kms:kme,jms:jme)            
   REAL, INTENT(IN )::   DT      







      real a_u(ims:ime,kms:kme,jms:jme)         
      real a_v(ims:ime,kms:kme,jms:jme)         
      real a_t(ims:ime,kms:kme,jms:jme)         
      real a_e(ims:ime,kms:kme,jms:jme)         
      real b_u(ims:ime,kms:kme,jms:jme)         
      real b_v(ims:ime,kms:kme,jms:jme)         
      real b_t(ims:ime,kms:kme,jms:jme)         
      real b_e(ims:ime,kms:kme,jms:jme)         
      real b_q(ims:ime,kms:kme,jms:jme)         
      real dlg(ims:ime,kms:kme,jms:jme)         
      real dl_u(ims:ime,kms:kme,jms:jme)        

      real sf(ims:ime,kms:kme,jms:jme)           
      real vl(ims:ime,kms:kme,jms:jme)             

      real rl_up(its:ite,jts:jte) 
      real rs_abs(its:ite,jts:jte) 
      real emiss(its:ite,jts:jte)  
      real grdflx_urb(its:ite,jts:jte)  



      real hi_urb(its:ite,1:nz_um,jts:jte) 
      real hi_urb1D(nz_um)                 
      real ss_urb(nz_um,nurbm)             
     real pb_urb(nz_um)                   
      real hb_u(nz_um)                     
      integer nz_urb(nurbm)                
      integer nzurban(nurbm)


      real alag_u(nurbm)                      
      real alaw_u(nurbm)                      
      real alar_u(nurbm)                      
      real csg_u(nurbm)                       
      real csw_u(nurbm)                       
      real csr_u(nurbm)                       
      real twini_u(nurbm)                     
      real trini_u(nurbm)                     
      real tgini_u(nurbm)                     





      real csg(ng_u)           
      real csw(nwr_u)          
      real csr(nwr_u)          
      real csgb(ngb_u)         
      real csf(nf_u)           
      real alar(nwr_u+1)       
      real alaw(nwr_u+1)       
      real alag(ng_u)          
      real alagb(ngb_u+1)      
      real alaf(nf_u+1)        
      real dzr(nwr_u)          
      real dzf(nf_u)           
      real dzw(nwr_u)          
      real dzgb(ngb_u)         





      real bs(ndm)              
      real ws(ndm)              
      real strd(ndm)            
      real drst(ndm)            
      real ss(nz_um)            
      real pb(nz_um)            
      real HFGR_D(nz_um)


      real z0(ndm,nz_um)        
      real bs_urb(ndm,nurbm)      
      real ws_urb(ndm,nurbm)      





      real albg_u(nurbm)                      
      real albw_u(nurbm)                      
      real albr_u(nurbm)                      
      real albwin_u(nurbm)                    
      real emwind_u(nurbm)                    
      real emg_u(nurbm)                       
      real emw_u(nurbm)                       
      real emr_u(nurbm)                       
      real gr_frac_roof_u(nurbm)
      real pv_frac_roof_u(nurbm)
      integer gr_flag_u
      integer gr_type_u


      real fww_u(nz_um,nz_um,ndm,nurbm)         
      real fwg_u(nz_um,ndm,nurbm)               
      real fgw_u(nz_um,ndm,nurbm)               
      real fsw_u(nz_um,ndm,nurbm)               
      real fws_u(nz_um,ndm,nurbm)               
      real fsg_u(ndm,nurbm)   





      real z0g_u(nurbm)         
      real z0r_u(nurbm)         


      integer nd_u(nurbm)       
      real strd_u(ndm,nurbm)    
      real drst_u(ndm,nurbm)    
      real ws_u(ndm,nurbm)      
      real bs_u(ndm,nurbm)      
      real h_b(nz_um,nurbm)     
      real d_b(nz_um,nurbm)     
      real ss_u(nz_um,nurbm)  
      real pb_u(nz_um,nurbm)  



      integer nz_u(nurbm)       
      
      real z_u(nz_um)         

      real bldac_frc_u(nurbm)
      real cooled_frc_u(nurbm)
      real cop_u(nurbm)
      real pwin_u(nurbm)
      real beta_u(nurbm)
      integer sw_cond_u(nurbm)
      real time_on_u(nurbm)
      real time_off_u(nurbm)
      real targtemp_u(nurbm)
      real gaptemp_u(nurbm)
      real targhum_u(nurbm)
      real gaphum_u(nurbm)
      real perflo_u(nurbm)
      real hsesf_u(nurbm)
      real hsequip(24)
      real irho(24)


      real z1D(kms:kme)               
      real ua1D(kms:kme)                
      real va1D(kms:kme)                
      real pt1D(kms:kme)                
      real da1D(kms:kme)                
      real pr1D(kms:kme)                
      real pt01D(kms:kme)               
      real zr1D                    
      real deltar1D                
      real ah1D                    
      real rs1D                    
      real rld1D                   
      real swddir1D
      real swddif1D                



      real tw1D(2*ndm,nz_um,nwr_u,nbui_max) 
      real tg1D(ndm,ng_u)                   
      real tr1D(ndm,nz_um,nwr_u)   
      real trv1D(ndm,nz_um,ngr_u)   
      real qr1D(ndm,nz_um,ngr_u)   




      real tlev1D(nz_um,nbui_max)            
      real qlev1D(nz_um,nbui_max)            
      real twlev1D(2*ndm,nz_um,nbui_max)     
      real tglev1D(ndm,ngb_u,nbui_max)       
      real tflev1D(ndm,nf_u,nz_um-1,nbui_max)
      real lflev1D(nz_um,nz_um)           
      real sflev1D(nz_um,nz_um)           
      real lfvlev1D(nz_um,nz_um)          
      real sfvlev1D(nz_um,nz_um)          
      real sfwin1D(2*ndm,nz_um,nbui_max)     
      real consumlev1D(nz_um,nz_um)       
      real eppvlev1D(nz_um)               
      real tair1D(nz_um)
      real tpvlev1D(ndm,nz_um)
      real qv1D(kms:kme)                  
      real meso_urb                       
      real meso_urb_ac
      real roof_frac                       
      real d_urb(nz_um)    
      real sf_ac
      integer ibui,nbui
      integer nlev(nz_um)
 




      real sfw1D(2*ndm,nz_um,nbui_max)      
      real sfg1D(ndm)              
      real sfr1D(ndm,nz_um)      
      real sfrpv1D(ndm,nz_um)

      real tpv1D(nbui_max)
      real sfr_indoor1D(nbui_max) 
      real sfrv1D(ndm,nz_um)      
      real lfrv1D(ndm,nz_um)      
      real dg1D(ndm)              
      real dgr1D(ndm,nz_um)      
      real lfg1D(ndm)              
      real lfr1D(ndm,nz_um)      
      real drain1D(ndm,nz_um)      
      real sf1D(kms:kme)              
      real vl1D(kms:kme)                
      real a_u1D(kms:kme)               
      real a_v1D(kms:kme)               
      real a_t1D(kms:kme)               
      real a_e1D(kms:kme)               
      real b_u1D(kms:kme)               
      real b_v1D(kms:kme)               
      real b_t1D(kms:kme)               
      real b_ac1D(kms:kme)
      real b_e1D(kms:kme)               
      real b_q1D(kms:kme)               
      real dlg1D(kms:kme)               
      real dl_u1D(kms:kme)              
      real gfr1D(ndm,nz_um)
      real time_bep

      integer ind_zwd(nbui_max,nz_um,nwr_u,ndm)
      integer ind_gd(ng_u,ndm)
      integer ind_zd(nbui_max,nz_um,ndm)
      integer ind_zdf(nz_um,ndm)
      integer ind_zrd(nz_um,nwr_u,ndm)
      integer ind_grd(nz_um,ngr_u,ndm)

      integer ind_bd(nbui_max,nz_um)
      integer ind_wd(nbui_max,nz_um,ndm)
      integer ind_gbd(nbui_max,ngb_u,ndm)  
      integer ind_fbd(nbui_max,nf_u,nz_um-1,ndm)

      integer ix,iy,iz,iurb,id,iz_u,iw,ig,ir,ix1,iy1,k
      integer it, nint
      integer iii
      logical first
      character(len=80) :: text
      data first/.true./
      save first,time_bep
       
      save alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,                       &
           albg_u,albw_u,albr_u,emg_u,emw_u,emr_u,                       &
           z0g_u,z0r_u, nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,ss_u,pb_u,  &
           nz_u,z_u,albwin_u,emwind_u,cop_u,pwin_u,beta_u,sw_cond_u,     &
           bldac_frc_u,cooled_frc_u,                                     &
           time_on_u,time_off_u,targtemp_u,gaptemp_u,targhum_u,gaphum_u, &
           perflo_u,gr_frac_roof_u,                    &
           pv_frac_roof_u,hsesf_u,hsequip,irho,gr_flag_u,gr_type_u




















     if(urban_map_zwd.lt.nbui_max*nz_um*ndm*max(nwr_u,ng_u))then
        write(*,*)'urban_map_zwd too small, please increase to at least ', nbui_max*nz_um*ndm*max(nwr_u,ng_u)
        stop
      endif



      if(urban_map_bd.lt.nbui_max*nz_um)then 
        write(*,*)'urban_map_bd too small, please increase to at least ', nbui_max*nz_um
        stop
      endif

      if(urban_map_wd.lt.nbui_max*nz_um*ndm)then 
        write(*,*)'urban_map_wd too small, please increase to at least ', nbui_max*nz_um*ndm
        stop
      endif

      if(urban_map_gbd.lt.nbui_max*ndm*ngb_u)then 
        write(*,*)'urban_map_gbd too small, please increase to at least ', nbui_max*ndm*ngb_u
        stop
      endif

      if(urban_map_fbd.lt.(nz_um-1)*nbui_max*ndm*nf_u)then 
        write(*,*)'urban_map_fbd too small, please increase to at least ', nbui_max*ndm*nf_u*(nz_um-1)
        stop
      endif

      if (ndm.ne.2)then
         write(*,*) 'number of directions is not correct',ndm
         stop
      endif






      ind_zwd=0       
      ind_gd=0
      ind_zd=0
      ind_zdf=0
      ind_zrd=0
      ind_grd=0
      ind_bd=0
      ind_wd=0
      ind_gbd=0
      ind_fbd=0




      iii=0
      do ibui=1,nbui_max
      do iz_u=1,nz_um
      do iw=1,nwr_u
      do id=1,ndm
       iii=iii+1
       ind_zwd(ibui,iz_u,iw,id)=iii
      enddo
      enddo
      enddo
      enddo

      iii=0
      do ig=1,ng_u
      do id=1,ndm
       iii=iii+1
       ind_gd(ig,id)=iii
      enddo
      enddo

      iii=0
      do ibui=1,nbui_max
      do iz_u=1,nz_um
      do id=1,ndm
       iii=iii+1
       ind_zd(ibui,iz_u,id)=iii
      enddo
      enddo
      enddo
  
      iii=0
      do iz_u=1,nz_um
      do iw=1,nwr_u
      do id=1,ndm
       iii=iii+1
       ind_zrd(iz_u,iw,id)=iii
      enddo
      enddo
      enddo

     iii=0
      do iz_u=1,nz_um
      do iw=1,ngr_u
      do id=1,ndm
       iii=iii+1
       ind_grd(iz_u,iw,id)=iii
      enddo
      enddo
      enddo
     


      
      iii=0
      do iz_u=1,nz_um
      do id=1,ndm
         iii=iii+1
         ind_zdf(iz_u,id)=iii
      enddo 
      enddo 

      iii=0
      do ibui=1,nbui_max  
      do iz_u=1,nz_um     
         iii=iii+1
         ind_bd(ibui,iz_u)=iii
      enddo 
      enddo 



      iii=0
      do ibui=1,nbui_max 
      do iz_u=1,nz_um 
      do id=1,ndm 
         iii=iii+1
         ind_wd(ibui,iz_u,id)=iii
      enddo 
      enddo 
      enddo 

      iii=0
      do ibui=1,nbui_max
      do iw=1,ngb_u 
      do id=1,ndm 
         iii=iii+1
         ind_gbd(ibui,iw,id)=iii  
      enddo 
      enddo 
      enddo 

      iii=0
      do ibui=1,nbui_max 
      do iw=1,nf_u 
      do iz_u=1,nz_um-1 
      do id=1,ndm  
         iii=iii+1
         ind_fbd(ibui,iw,iz_u,id)=iii
      enddo 
      enddo 
      enddo 
      enddo 


      
   
      if (num_urban_hi.ge.nz_um)then
          write(*,*)'nz_um too small, please increase to at least ', num_urban_hi+1
          stop         
      endif
   
      do ix=its,ite
      do iy=jts,jte
      do iz_u=1,nz_um
          hi_urb(ix,iz_u,iy)=0.
      enddo
      enddo
      enddo

      do ix=its,ite
      do iy=jts,jte
       z(ix,kts,iy)=0.
       do iz=kts+1,kte+1
        z(ix,iz,iy)=z(ix,iz-1,iy)+dz8w(ix,iz-1,iy)
       enddo
       iii=0
       do iz_u=1,num_urban_hi
          hi_urb(ix,iz_u,iy)= hi_urb2d(ix,iz_u,iy)
          if (hi_urb(ix,iz_u,iy)/=0.) then
             iii=iii+1
          endif
       enddo 
       if (iii.gt.nbui_max) then
          write(*,*) 'nbui_max too small, please increase to at least ',iii
          stop
       endif
      enddo
      enddo


      if (first) then                           

         call init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,albwin_u,emg_u,emw_u,&
                emr_u,emwind_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,  &
                cop_u,pwin_u,beta_u,sw_cond_u,time_on_u,time_off_u,targtemp_u,    &
                bldac_frc_u,cooled_frc_u,                                         &
                gaptemp_u,targhum_u,gaphum_u,perflo_u,                            &
                gr_frac_roof_u,pv_frac_roof_u,                   & 
                hsesf_u,hsequip,irho,gr_flag_u,gr_type_u)
 

        call icBEP(nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)
   
      first=.false.

      endif 

do ix=its,ite
      do iy=jts,jte
        if (FRC_URB2D(ix,iy).gt.0.) then    
	
         iurb=UTYPE_URB2D(ix,iy)

         hi_urb1D=0.
         do iz_u=1,nz_um
            hi_urb1D(iz_u)=hi_urb(ix,iz_u,iy)
           
         enddo

         call icBEPHI_XY(iurb,hb_u,hi_urb1D,ss_urb,pb_urb,    &
                         nz_urb(iurb),z_u)

         call param(iurb,nz_u(iurb),nz_urb(iurb),nzurban(iurb),      &
                    nd_u(iurb),csg_u,csg,alag_u,alag,csr_u,csr,      &
                    alar_u,alar,csw_u,csw,alaw_u,alaw,               &
                    ws_u,ws_urb,ws,bs_u,bs_urb,bs,z0g_u,z0r_u,z0,    &
                    strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,     &
                    pb_urb,pb,dzw,dzr,dzf,csf,alaf,dzgb,csgb,alagb,  &
                    lp_urb2d(ix,iy),lb_urb2d(ix,iy),                 &
                    hgt_urb2d(ix,iy),FRC_URB2D(ix,iy))
         




         call icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,fws_u,fsg_u,   &
                         nd_u(iurb),strd,ws,nzurban(iurb),z_u)   

         ibui=0
         nlev=0
         nbui=0
         d_urb=0.
         do iz=1,nz_um		   
         if(ss_urb(iz,iurb).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           d_urb(ibui)=ss_urb(iz,iurb)
           nbui=ibui
	 endif	  
         end do  

         if (nbui.gt.nbui_max) then
            write (*,*) 'nbui_max must be increased to',nbui
            stop
         endif



do iz= kts,kte
          ua1D(iz)=u_phy(ix,iz,iy)
          va1D(iz)=v_phy(ix,iz,iy)
	  pt1D(iz)=th_phy(ix,iz,iy)
	  da1D(iz)=rho(ix,iz,iy)
	  pr1D(iz)=p_phy(ix,iz,iy)
	  pt01D(iz)=300.
	  z1D(iz)=z(ix,iz,iy)
          qv1D(iz)=qv_phy(ix,iz,iy)
          a_u1D(iz)=0.
          a_v1D(iz)=0.
          a_t1D(iz)=0.
          a_e1D(iz)=0.
          b_u1D(iz)=0.
          b_v1D(iz)=0.
          b_t1D(iz)=0.
          b_ac1D(iz)=0.
          b_e1D(iz)=0.           
         enddo
	 z1D(kte+1)=z(ix,kte+1,iy)



         do id=1,ndm
         do iz_u=1,nz_um
         do iw=1,nwr_u
         do ibui=1,nbui_max
          tw1D(2*id-1,iz_u,iw,ibui)=tw1_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)
          tw1D(2*id,iz_u,iw,ibui)=tw2_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)
         enddo
         enddo
         enddo
         enddo
	
         do id=1,ndm
          do ig=1,ng_u
            tg1D(id,ig)=tgb_urb4d(ix,ind_gd(ig,id),iy)
         enddo
         
           do iz_u=1,nz_um
           do ir=1,nwr_u
            tr1D(id,iz_u,ir)=trb_urb4d(ix,ind_zrd(iz_u,ir,id),iy)
           enddo
           do ir=1,ngr_u
          if(gr_flag_u.eq.1)then
            trv1D(id,iz_u,ir)=trv_urb4d(ix,ind_grd(iz_u,ir,id),iy)
            qr1D(id,iz_u,ir)=qr_urb4d(ix,ind_grd(iz_u,ir,id),iy)
          else
            trv1D(id,iz_u,ir)=0.
            qr1D(id,iz_u,ir)=0.
          endif
          enddo
          enddo
           enddo





         tlev1D=0.  
         qlev1D=0.  

         twlev1D=0. 
         tglev1D=0. 
         tflev1D=0. 

         sflev1D=0.    
         lflev1D=0.    
         consumlev1D=0.
         eppvlev1D=0.  
         tpvlev1D=0.
         sfvlev1D=0.   
         lfvlev1D=0.   
         sfwin1D=0.    
         sfw1D=0.      

         do iz_u=1,nz_um    
         do ibui=1,nbui_max 
            tlev1D(iz_u,ibui)= tlev_urb3d(ix,ind_bd(ibui,iz_u),iy)  
            qlev1D(iz_u,ibui)= qlev_urb3d(ix,ind_bd(ibui,iz_u),iy)  
         enddo 
         enddo 



  do id=1,ndm  
            do iz_u=1,nz_um 
               do ibui=1,nbui_max 
                  twlev1D(2*id-1,iz_u,ibui)=tw1lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  twlev1D(2*id,iz_u,ibui)=tw2lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  sfwin1D(2*id-1,iz_u,ibui)=sfwin1_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
                  sfwin1D(2*id,iz_u,ibui)=sfwin2_urb3d(ix,ind_wd(ibui,iz_u,id),iy)
               enddo 
            enddo 
         enddo 

         do id=1,ndm 
            do iw=1,ngb_u 
               do ibui=1,nbui_max 
                  tglev1D(id,iw,ibui)=tglev_urb3d(ix,ind_gbd(ibui,iw,id),iy)
               enddo 
            enddo 
         enddo 
       
         do id=1,ndm 
            do iw=1,nf_u 
               do iz_u=1,nz_um-1 
                  do ibui=1,nbui_max 
                     tflev1D(id,iw,iz_u,ibui)=tflev_urb3d(ix,ind_fbd(ibui,iw,iz_u,id),iy)
                     
                  enddo 
               enddo 
             enddo 
         enddo 





        do id=1,ndm
	 do iz=1,nz_um
         do ibui=1,nbui_max 


	  sfw1D(2*id-1,iz,ibui)=sfw1_urb3d(ix,ind_zd(ibui,iz,id),iy)
	  sfw1D(2*id,iz,ibui)=sfw2_urb3d(ix,ind_zd(ibui,iz,id),iy)
         enddo
	 enddo
	 enddo
	 
	 do id=1,ndm
	  sfg1D(id)=sfg_urb3d(ix,id,iy)
          lfg1D(id)=lfg_urb3d(ix,id,iy)
          dg1D(id)=dg_urb3d(ix,id,iy)

	 enddo

	 do id=1,ndm
	 do iz=1,nz_um
	  tpvlev1D(id,iz)=t_pv_urb3d(ix,ind_zdf(iz,id),iy)
	  sfr1D(id,iz)=sfr_urb3d(ix,ind_zdf(iz,id),iy)
          lfr1D(id,iz)=lfr_urb3d(ix,ind_zdf(iz,id),iy)          
          dgr1D(id,iz)=dgr_urb3d(ix,ind_zdf(iz,id),iy)
          if(gr_flag_u.eq.1)then
          sfrv1D(id,iz)=sfrv_urb3d(ix,ind_zdf(iz,id),iy)
          lfrv1D(id,iz)=lfrv_urb3d(ix,ind_zdf(iz,id),iy)
          drain1D(id,iz)=drain_urb4d(ix,ind_zdf(iz,id),iy)
          else
          sfrv1D(id,iz)=0.
          lfrv1D(id,iz)=0.
          drain1D(id,iz)=0.
          endif
	 enddo
	 enddo



         rs1D=swdown(ix,iy)
         rld1D=glw(ix,iy)
         swddir1D=swddir(ix,iy)         
         swddif1D=swddif(ix,iy)         
         zr1D=acos(COSZ_URB2D(ix,iy))
         deltar1D=DECLIN_URB
         ah1D=OMG_URB2D(ix,iy)
         

         call BEP1D(itimestep,ix,iy,iurb,kms,kme,kts,kte,z1D,dt,ua1D,va1D,pt1D,da1D,pr1D,pt01D,  &
                   zr1D,deltar1D,ah1D,rs1D,rld1D,alagb,             & 
                   alag,alaw,alar,alaf,csgb,csg,csw,csr,csf,        & 
                   dzr,dzf,dzw,dzgb,xlat(ix,iy),swddir1D,swddif1D, &
                   albg_u(iurb),albw_u(iurb),albr_u(iurb),          &
                   albwin_u(iurb),emg_u(iurb),emw_u(iurb),          &
                   emr_u(iurb),emwind_u(iurb),fww_u,fwg_u,          &
                   fgw_u,fsw_u,fws_u,fsg_u,z0,                      & 
                   nd_u(iurb),strd,drst,ws,bs_urb,bs,ss,pb,         & 
                   nzurban(iurb),z_u,cop_u,pwin_u,beta_u,           & 
                   sw_cond_u,time_on_u,time_off_u,targtemp_u,       &
                   gaptemp_u,targhum_u,gaphum_u,perflo_u,           &
                   gr_frac_roof_u(iurb),pv_frac_roof_u(iurb),  & 
                   hsesf_u,hsequip,irho,gr_flag_u,gr_type_u,        &
                   tw1D,tg1D,tr1D,trv1D,sfw1D,sfg1D,sfr1D,    &
                   sfrv1D,lfrv1D,    &
                   dgr1D,dg1D,lfr1D,lfg1D,                       &
                   drain1D,rainbl(ix,iy),qr1D,                   &
                   a_u1D,a_v1D,a_t1D,a_e1D,                         & 
                   b_u1D,b_v1D,b_t1D,b_ac1D,b_e1D,b_q1D,            & 
                   dlg1D,dl_u1D,sf1D,vl1D,rl_up(ix,iy),             &
                   rs_abs(ix,iy),emiss(ix,iy),grdflx_urb(ix,iy),    &
                   qv1D,tlev1D,qlev1D,sflev1D,lflev1D,consumlev1D,  &
                   eppvlev1D,tpvlev1D,sfvlev1D,lfvlev1D,twlev1D,tglev1D,tflev1D,sfwin1D,tair1D,sfr_indoor1D,sfrpv1D,gfr1D) 
           
          do ibui=1,nbui_max 
	    do iz=1,nz_um   
               do id=1,ndm 
	          sfw1_urb3d(ix,ind_zd(ibui,iz,id),iy)=sfw1D(2*id-1,iz,ibui) 
	          sfw2_urb3d(ix,ind_zd(ibui,iz,id),iy)=sfw1D(2*id,iz,ibui) 
	       enddo
	    enddo
         enddo
 
	 do id=1,ndm
	  sfg_urb3d(ix,id,iy)=sfg1D(id)
          lfg_urb3d(ix,id,iy)=lfg1D(id)
          dg_urb3d(ix,id,iy)=dg1D(id) 
	 enddo
         
	 do id=1,ndm
	 do iz=1,nz_um
          t_pv_urb3d(ix,ind_zdf(iz,id),iy)=tpvlev1D(id,iz) 
	  sfr_urb3d(ix,ind_zdf(iz,id),iy)=sfr1D(id,iz)
          dgr_urb3d(ix,ind_zdf(iz,id),iy)=dgr1D(id,iz)
          lfr_urb3d(ix,ind_zdf(iz,id),iy)=lfr1D(id,iz)
          if(gr_flag_u.eq.1)then 
          sfrv_urb3d(ix,ind_zdf(iz,id),iy)=sfrv1D(id,iz)
          lfrv_urb3d(ix,ind_zdf(iz,id),iy)=lfrv1D(id,iz)
          drain_urb4d(ix,ind_zdf(iz,id),iy)=drain1D(id,iz)
          endif
	 enddo
	 enddo
         
        do ibui=1,nbui_max
         do iz_u=1,nz_um
         do iw=1,nwr_u
         do id=1,ndm
          tw1_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)=tw1D(2*id-1,iz_u,iw,ibui)
          tw2_urb4d(ix,ind_zwd(ibui,iz_u,iw,id),iy)=tw1D(2*id,iz_u,iw,ibui)
         enddo
         enddo
         enddo
         enddo


          do id=1,ndm
            do ig=1,ng_u
              
               tgb_urb4d(ix,ind_gd(ig,id),iy)=tg1D(id,ig)
            enddo
            do iz_u=1,nz_um
               do ir=1,nwr_u
                  trb_urb4d(ix,ind_zrd(iz_u,ir,id),iy)=tr1D(id,iz_u,ir)
               enddo
                if(gr_flag_u.eq.1)then
               do ir=1,ngr_u
                  trv_urb4d(ix,ind_grd(iz_u,ir,id),iy)=trv1D(id,iz_u,ir)
                  qr_urb4d(ix,ind_grd(iz_u,ir,id),iy)=qr1D(id,iz_u,ir)
               enddo
                endif
            enddo
          enddo

         



        
         do ibui=1,nbui_max 
         do iz_u=1,nz_um 
            tlev_urb3d(ix,ind_bd(ibui,iz_u),iy)=tlev1D(iz_u,ibui)  
            qlev_urb3d(ix,ind_bd(ibui,iz_u),iy)=qlev1D(iz_u,ibui)  
         enddo 
         enddo 
 
         do ibui=1,nbui_max 
         do iz_u=1,nz_um 
            do id=1,ndm 
               tw1lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=twlev1D(2*id-1,iz_u,ibui)
               tw2lev_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=twlev1D(2*id,iz_u,ibui)
               sfwin1_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=sfwin1D(2*id-1,iz_u,ibui)
               sfwin2_urb3d(ix,ind_wd(ibui,iz_u,id),iy)=sfwin1D(2*id,iz_u,ibui)
            enddo 
         enddo 
         enddo 
        
         do ibui=1,nbui_max  
            do iw=1,ngb_u 
               do id=1,ndm 
                  tglev_urb3d(ix,ind_gbd(ibui,iw,id),iy)=tglev1D(id,iw,ibui)
               enddo 
            enddo 
         enddo 

        do ibui=1,nbui_max 
        do iw=1,nf_u 
               do iz_u=1,nz_um-1 
                 do  id=1,ndm
                    tflev_urb3d(ix,ind_fbd(ibui,iw,iz_u,id),iy)=tflev1D(id,iw,iz_u,ibui)
                  enddo 
               enddo 
             enddo 
         enddo 



         sf_ac_urb3d(ix,iy)=0.
         lf_ac_urb3d(ix,iy)=0.
         cm_ac_urb3d(ix,iy)=0.
         ep_pv_urb3d(ix,iy)=0.
         sfvent_urb3d(ix,iy)=0.
         lfvent_urb3d(ix,iy)=0.
         draingr_urb3d(ix,iy)=0.
         qgr_urb3d(ix,iy)=0.
         tgr_urb3d(ix,iy)=0.
         meso_urb=(1./4.)*FRC_URB2D(ix,iy)/((bs_urb(1,iurb)+ws_urb(1,iurb))*bs_urb(2,iurb))+ &
                  (1./4.)*FRC_URB2D(ix,iy)/((bs_urb(2,iurb)+ws_urb(2,iurb))*bs_urb(1,iurb))
          meso_urb_ac=meso_urb*bldac_frc_u(iurb)*cooled_frc_u(iurb)
          roof_frac=FRC_URB2D(ix,iy)*bs_urb(1,iurb)/(bs_urb(1,iurb)+ws_urb(1,iurb))
         ibui=0
         nlev=0
         nbui=0
         d_urb=0.
         do iz=1,nz_um		   
         if(ss_urb(iz,iurb).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           d_urb(ibui)=ss_urb(iz,iurb)
           nbui=ibui
	 endif	  
         end do  

       


        do ibui=1,nbui       
            ep_pv_urb3d(ix,iy)=ep_pv_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*eppvlev1D(ibui)
         do iz_u=1,nlev(ibui) 
               sf_ac_urb3d(ix,iy)=sf_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*sflev1D(iz_u,ibui)
               lf_ac_urb3d(ix,iy)=lf_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*lflev1D(iz_u,ibui)
               cm_ac_urb3d(ix,iy)=cm_ac_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*consumlev1D(iz_u,ibui)
        


        
               sfvent_urb3d(ix,iy)=sfvent_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*sfvlev1D(iz_u,ibui)
               lfvent_urb3d(ix,iy)=lfvent_urb3d(ix,iy)+meso_urb_ac*d_urb(ibui)*lfvlev1D(iz_u,ibui)
         enddo 
         enddo 
       


       if(gr_flag_u.eq.1)then 
       do id=1,ndm
       do iz=2,nz_um-1
        draingr_urb3d(ix,iy)=draingr_urb3d(ix,iy)+d_urb(iz-1)*roof_frac*drain1D(id,iz)*1000
       do ig=1,ngr_u
        qgr_urb3d(ix,iy)=qgr_urb3d(ix,iy)+qr1D(id,iz,ig)/ndm/(nz_um-2)/ngr_u
        tgr_urb3d(ix,iy)=tgr_urb3d(ix,iy)+trv1D(id,iz,ig)/ndm/(nz_um-2)/ngr_u
        
       enddo
       enddo
       enddo
       endif


         
        sf_ac=0.
        sf(ix,kts:kte,iy)=0.
        vl(ix,kts:kte,iy)=0.
        a_u(ix,kts:kte,iy)=0.
        a_v(ix,kts:kte,iy)=0.
        a_t(ix,kts:kte,iy)=0.
        a_e(ix,kts:kte,iy)=0.
        b_u(ix,kts:kte,iy)=0.
        b_v(ix,kts:kte,iy)=0.
        b_t(ix,kts:kte,iy)=0.
        b_e(ix,kts:kte,iy)=0.
        b_q(ix,kts:kte,iy)=0.
        dlg(ix,kts:kte,iy)=0.
        dl_u(ix,kts:kte,iy)=0.

        do iz= kts,kte
          sf(ix,iz,iy)=sf1D(iz)
          vl(ix,iz,iy)=vl1D(iz)
          a_u(ix,iz,iy)=a_u1D(iz)
          a_v(ix,iz,iy)=a_v1D(iz)
          a_t(ix,iz,iy)=a_t1D(iz)
          a_e(ix,iz,iy)=a_e1D(iz)
          b_u(ix,iz,iy)=b_u1D(iz)
          b_v(ix,iz,iy)=b_v1D(iz)
          b_t(ix,iz,iy)=b_t1D(iz)
          sf_ac=sf_ac+b_ac1D(iz)*da1D(iz)*cp_u*dz8w(ix,iz,iy)*vl1D(iz)*FRC_URB2D(ix,iy)
          b_e(ix,iz,iy)=b_e1D(iz)
          b_q(ix,iz,iy)=b_q1D(iz)
          dlg(ix,iz,iy)=dlg1D(iz)
          dl_u(ix,iz,iy)=dl_u1D(iz)
        enddo
        sf(ix,kte+1,iy)=sf1D(kte+1)

         endif 


      enddo  
      enddo  


        time_bep=time_bep+dt





         
  
      return
      end subroutine BEP_BEM




      subroutine BEP1D(itimestep,ix,iy,iurb,kms,kme,kts,kte,z,dt,ua,va,pt,da,pr,pt0,   &  
                      zr,deltar,ah,rs,rld,alagb,                       & 
                      alag,alaw,alar,alaf,csgb,csg,csw,csr,csf,        & 
                      dzr,dzf,dzw,dzgb,xlat,swddir,swddif,             &
                      albg,albw,albr,albwin,emg,emw,emr,               & 
                      emwind,fww,fwg,fgw,fsw,fws,fsg,z0,               & 
                      ndu,strd,drst,ws,bs_u,bs,ss,pb,                  & 
                      nzu,z_u,cop_u,pwin_u,beta_u,sw_cond_u,           & 
                      time_on_u,time_off_u,targtemp_u,                 &
                      gaptemp_u,targhum_u,gaphum_u,perflo_u,           &
                      gr_frac_roof,pv_frac_roof,        & 
                      hsesf_u,hsequip,irho,gr_flag,gr_type,                    &
                      tw,tg,tr,trv,sfw,sfg,sfr,            &
                      sfrv,lfrv,dgr,dg,lfr,lfg,drain,rainbl,qr,      & 
                      a_u,a_v,a_t,a_e,                                 &
                      b_u,b_v,b_t,b_ac,b_e,b_q,                        & 
                      dlg,dl_u,sf,vl,rl_up,rs_abs,emiss,grdflx_urb,    &
                      qv,tlev,qlev,sflev,lflev,consumlev,              &
                      eppvlev,tpvlev,sfvlev,lfvlev,twlev,tglev,tflev,sfwin,tmp_u,sfr_indoor,sfrpv,gfr)    




      implicit none








      integer kms,kme,kts,kte,ix,iy,itimestep
      real z(kms:kme)               
      real ua(kms:kme)                
      real va(kms:kme)                
      real pt(kms:kme)                
      real da(kms:kme)                
      real pr(kms:kme)                
      real pt0(kms:kme)               
      real qv(kms:kme)              
      real dt                    
      real zr                    
      real deltar                
      real ah                    
      real rs                    
      real rld                   
      real xlat                  
      real swddir                
      real swddif                



      integer iurb               


      real albg                  
      real albw                  
      real albr                  
      real albwin                
      real emwind                
      real emg                   
      real emw                   
      real emr                   





      real fww(nz_um,nz_um,ndm,nurbm)  
      real fwg(nz_um,ndm,nurbm)        
      real fgw(nz_um,ndm,nurbm)        
      real fsw(nz_um,ndm,nurbm)        
      real fws(nz_um,ndm,nurbm)        
      real fsg(ndm,nurbm)              
      

      integer ndu                  
      real bs_u(ndm,nurbm)         
        

      integer nzu           
      real z_u(nz_um)       

      real cop_u(nurbm)
      real pwin_u(nurbm)
      real beta_u(nurbm)
      integer sw_cond_u(nurbm)
      real time_on_u(nurbm)
      real time_off_u(nurbm)
      real targtemp_u(nurbm)
      real gaptemp_u(nurbm)
      real targhum_u(nurbm)
      real gaphum_u(nurbm)
      real perflo_u(nurbm)
      real hsesf_u(nurbm)
      real hsequip(24)
      real irho(24)
      real gr_frac_roof
      real pv_frac_roof
      integer gr_flag
      integer gr_type
      real tpv(nbui_max)
     real  sfpv(nbui_max)
     real sfr_indoor(nbui_max)






      real tw(2*ndm,nz_um,nwr_u,nbui_max)  
      real tr(ndm,nz_um,nwr_u)  
      real tg(ndm,ng_u)          
      real trv(ndm,nz_um,ngr_u)  
      real sfw(2*ndm,nz_um,nbui_max)      
      real sfg(ndm)              
      real sfr(ndm,nz_um)      
      real sfrv(ndm,nz_um)      
      real lfrv(ndm,nz_um)      
      real dg(ndm)              
      real dgr(ndm,nz_um)      
      real lfr(ndm,nz_um)      
      real lfg(ndm)              
      real drain(ndm,nz_um)        
      real rainbl              
      real gfg(ndm)             
      real gfr(ndm,nz_um)     
      real gfw(2*ndm,nz_um,nbui_max)     
      real qr(ndm,nz_um,ngr_u)  




                         



      real sf(kms:kme)             
      real vl(kms:kme)               
     


      real a_u(kms:kme)              
      real a_v(kms:kme)              
      real a_t(kms:kme)              
      real a_e(kms:kme)              
      real b_u(kms:kme)              
      real b_v(kms:kme)              
      real b_t(kms:kme)              
      real b_ac(kms:kme)
      real b_e(kms:kme)              
      real b_q(kms:kme)              
      real dlg(kms:kme)              
      real dl_u(kms:kme)             




      real dz(kms:kme)               


      real ua_u(nz_um)          
      real va_u(nz_um)          
      real pt_u(nz_um)          
      real da_u(nz_um)          
      real pt0_u(nz_um)         
      real pr_u(nz_um)          
      real qv_u(nz_um)          



      real alag(ng_u)           
      
      real csg(ng_u)            
      real csr(nwr_u)            
      real csw(nwr_u)            

      real z0(ndm,nz_um)      
      real ws(ndm)              
      real bs(ndm)              
      real strd(ndm)            
      real drst(ndm)            
      real ss(nz_um)          
      real pb(nz_um)          
      real cdrag(nz_um)
      real alp



     real rsg(ndm)             
      real rsw(2*ndm,nz_um)     
      real rsd(2*ndm,nz_um)     
      real rlg(ndm)             
      real rlw(2*ndm,nz_um)     



      real ptg(ndm)             
      real ptr(ndm,nz_um)     
      real ptrv(ndm,nz_um)     
      real ptw(2*ndm,nz_um,nbui_max)     

      real tg_av(ndm) 





      real uhb_u(ndm,nz_um)   
      real uva_u(2*ndm,nz_um)   
      real uvb_u(2*ndm,nz_um)   
      real vhb_u(ndm,nz_um)   
      real vva_u(2*ndm,nz_um)   
      real vvb_u(2*ndm,nz_um)   
      real thb_u(ndm,nz_um)   
      real tva_u(2*ndm,nz_um)   
      real tvb_u(2*ndm,nz_um)   


 real tvb_ac(2*ndm,nz_um)
      real ehb_u(ndm,nz_um)   
      real evb_u(2*ndm,nz_um)   
      real qhb_u(ndm,nz_um)     
      real qvb_u(2*ndm,nz_um)   

      real rs_abs 
      real rl_up 
      real emiss 
      real grdflx_urb 
      real dt_int 
      integer nt_int 
      integer iz,id, it_int,it
      integer iw




   
      real tmp_u(nz_um)     

      real dzw(nwr_u)       
      real dzr(nwr_u)       
      real dzf(nf_u)        
      real dzgb(ngb_u)      

      real csgb(ngb_u)      

      real csf(nf_u)        
                            
      real alar(nwr_u+1)    
      real alaw(nwr_u+1)    
      real alaf(nf_u+1)     
      real alagb(ngb_u+1)   

      real sfrb(ndm,nbui_max)        
      real sfrbpv(ndm,nbui_max)      
      real sfrpv(ndm,nz_um)          
      real sfrvb(ndm,nbui_max)        
      real lfrvb(ndm,nbui_max)        
      real lfrb(ndm,nbui_max)        
  
      real gfrb(ndm,nbui_max)        
      real sfwb1D(2*ndm,nz_um)    
      real sfwin(2*ndm,nz_um,nbui_max)
      real sfwinb1D(2*ndm,nz_um)  
      real gfwb1D(2*ndm,nz_um)    

      real qlev(nz_um,nbui_max)      
      real qlevb1D(nz_um)         
      real tlev(nz_um,nbui_max)      
      real tlevb1D(nz_um)         
      real twb1D(2*ndm,nwr_u,nz_um)     
      real twlev(2*ndm,nz_um,nbui_max)     
      real twlevb1D(2*ndm,nz_um)        
      real tglev(ndm,ngb_u,nbui_max)        
      real tglevb1D(ngb_u)               
      real tflev(ndm,nf_u,nz_um-1,nbui_max)
      real tflevb1D(nf_u,nz_um-1)       
      real trb(ndm,nwr_u,nbui_max)         
      real trvb(ndm,ngr_u,nbui_max)         
      real trb1D(nwr_u) 

      real sflev(nz_um,nz_um)     
      real lflev(nz_um,nz_um)     
      real consumlev(nz_um,nz_um) 
      real sflev1D(nz_um)         
      real lflev1D(nz_um)         
      real consumlev1D(nz_um)     
      real eppvlev(nz_um)         
	   real tpvlev(ndm,nz_um)
      real tpvlevb(ndm,nbui_max)        
      real sfvlev(nz_um,nz_um)    
      real lfvlev(nz_um,nz_um)    
      real sfvlev1D(nz_um)        
      real lfvlev1D(nz_um)        

      real ptwin(2*ndm,nz_um,nbui_max)  
      real tw_av(2*ndm,nz_um)        
      real twlev_av(2*ndm,nz_um)     
      real sfw_av(2*ndm,nz_um)       
      real sfwind_av(2*ndm,nz_um)    
      integer flag_pvp
      integer nbui                
      integer nlev(nz_um)         
      integer ibui,ily  
      real :: nhourday   
      real :: st4,gamma,fp,lmr,smr,prova
      real hfgr(ndm,nz_um)
      real hfgrb(ndm,nbui_max)
      real irri_per_ts
      real irri_now 
      real tr_av(ndm,nz_um)
      real tr_avb(ndm,nbui_max)
      real sfr_avb(ndm,nbui_max)



    




        nhourday=ah/PI*180./15.+12.
        if (nhourday >= 24) nhourday = nhourday - 24
        if (nhourday < 0)  nhourday = nhourday + 24


      if(sum(irho).gt.0)then
        irri_per_ts=h_water/sum(irho)
       else
        irri_per_ts=0.
       endif
       
     if(irho(int(nhourday)+1).ne.0)then
       irri_now=irri_per_ts
     else
       irri_now=0.
     endif
      
      do iz=kts,kte
         dz(iz)=z(iz+1)-z(iz)
      end do

      call interpol(kms,kme,kts,kte,nzu,z,z_u,ua,ua_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,va,va_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt,pt_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt0,pt0_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pr,pr_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,da,da_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,qv,qv_u)

      

      call averaging_temp(tw,twlev,ss,pb,tw_av,twlev_av, &
                           sfw_av,sfwind_av,sfw,sfwin)
                           
     do id=1,ndu
       tg_av(id)=tg(id,ng_u)
     do iz=1,nz_um

       tr_av(id,iz)=((1-gr_frac_roof)*tr(id,iz,nwr_u)**4.+   &
       gr_frac_roof*trv(id,iz,ngr_u)**4.)**(1./4.)

     enddo
     enddo
     
     


   
     call modif_rad(iurb,ndu,nzu,z_u,ws,           &
                    drst,strd,ss,pb,                &
                    tw_av,tg_av,twlev_av,albg,albw,    &
                    emw,emg,pwin_u(iurb),albwin,    &
                    emwind,fww,fwg,fgw,fsw,fsg,     &
                    zr,deltar,ah,xlat,swddir,swddif,      &  
                    rs,rld,rsw,rsd,rsg,rlw,rlg)  


 




       call upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                 &
                       tg_av,emg,albg,rlg,rsg,sfg,lfg,                   & 
                       tw_av,emw,albw,rlw,rsw,sfw_av,             & 
                       tr_av,emr,albr,emwind,                        &
                       albwin,twlev_av,pwin_u(iurb),sfwind_av,rld,rs,sfr,sfrv,lfr,lfrv, & 
                       rs_abs,rl_up,emiss,grdflx_urb,gr_frac_roof,tpvlev,pv_frac_roof)          
   
    do id=1,ndu
    if(dg(id).le.dgmax) then
      dg(id)=dg(id)+(rainbl+(lfg(id)*dt)/latent)
     endif
    if (dg(id).lt.0) then
      dg(id)=0
    endif
    if (dg(id).gt.dgmax) then
      dg(id)=dgmax
    endif
   do iz=2,nz_um
    if(dgr(id,iz).le.drmax) then
     dgr(id,iz)=dgr(id,iz)+(rainbl+(lfr(id,iz)*dt)/latent)
    endif 
    if (dgr(id,iz).lt.0) then
     dgr(id,iz)=0
    endif
    if (dgr(id,iz).gt.drmax) then
     dgr(id,iz)=drmax
    endif
   enddo
  enddo 
 
  


     call surf_temp(ndu,pr_u,dt,                   & 
                    rld,rsg,rlg,                    &
                    tg,alag,csg,emg,albg,ptg,sfg,lfg,gfg)
     if(gr_flag.eq.1)then
     if(gr_frac_roof.gt.0.)then
     hfgr=0.
     call roof_temp_veg(ndu,pr_u,dt,                   &
                    rld,rs,                    &
                    trv,ptrv,sfrv,lfrv,gfr,qr,rainbl,drain,hfgr,tr,alar(5),dzr(5),csr(5),nzu,irri_now,gr_type,pv_frac_roof,tpvlev)
    
     endif
     endif


       
       do iz=1,nz_um 
	 tmp_u(iz)=pt_u(iz)*(pr_u(iz)/p0)**(rcp_u) 
       end do

       ibui=0
       nlev=0
       nbui=0
       hfgrb=0. 
       sfrb=0.     
       sfrbpv=0.   
       sfrpv=0.    
       lfrvb=0.
       lfrb=0.
       sfrvb=0.
       gfrb=0.     
       sfwb1D=0.   
       sfwinb1D=0. 
       gfwb1D=0.   


       twb1D=0.    
       twlevb1D=0. 
       tglevb1D=0. 
       tflevb1D=0. 
       trvb=0.     
       trb=0.      
       trb1D=0.    
       tr_avb=0.
       qlevb1D=0. 
       tlevb1D=0. 

       sflev1D=0.    
       lflev1D=0.    
       consumlev1D=0.
       tpvlevb=0.
       eppvlev=0.
       sfvlev1D=0.   
       lfvlev1D=0.   
       ptw=0.        
       ptwin=0.      
       ptr=0.        

       do iz=1,nz_um		   
         if(ss(iz).gt.0) then		
           ibui=ibui+1		                
           nlev(ibui)=iz-1
           nbui=ibui
           do id=1,ndm
              tr_avb(id,ibui)=tr_av(id,iz)
	      tpvlevb(id,ibui)=tpvlev(id,iz)
              hfgrb(id,ibui)=hfgr(id,iz)
              sfrb(id,ibui)=sfr(id,iz)
               sfrvb(id,ibui)=sfrv(id,iz)
              lfrvb(id,ibui)=lfrv(id,iz)
              lfrb(id,ibui)=lfr(id,iz)
              sfr_avb(id,ibui)=(1-gr_frac_roof)*sfr(id,iz)+gr_frac_roof*(sfrv(id,iz))
              do ily=1,nwr_u
                 trb(id,ily,ibui)=tr(id,iz,ily)
              enddo
              do ily=1,ngr_u
                 trvb(id,ily,ibui)=trv(id,iz,ily)
              enddo

           enddo
	 endif	  
       end do  
     





        do ibui=1,nbui
        do iz=1,nz_um
             qlevb1D(iz)=qlev(iz,ibui)
             tlevb1D(iz)=tlev(iz,ibui) 
          enddo
          
          do id=1,ndm

             do ily=1,nwr_u
                trb1D(ily)=trb(id,ily,ibui)
             enddo
             do ily=1,ngb_u
                tglevb1D(ily)=tglev(id,ily,ibui) 
             enddo

             do ily=1,nf_u
             do iz=1,nz_um-1
               tflevb1D(ily,iz)=tflev(id,ily,iz,ibui)
             enddo
             enddo

             do iz=1,nz_um
                sfwinb1D(2*id-1,iz)=sfwin(2*id-1,iz,ibui)
                sfwinb1D(2*id,iz)=sfwin(2*id,iz,ibui)
             enddo

             do iz=1,nz_um
                do ily=1,nwr_u
                   twb1D(2*id-1,ily,iz)=tw(2*id-1,iz,ily,ibui)
                   twb1D(2*id,ily,iz)=tw(2*id,iz,ily,ibui)
                enddo
                sfwb1D(2*id-1,iz)=sfw(2*id-1,iz,ibui)
                sfwb1D(2*id,iz)=sfw(2*id,iz,ibui)
                twlevb1D(2*id-1,iz)=twlev(2*id-1,iz,ibui)
                twlevb1D(2*id,iz)=twlev(2*id,iz,ibui)
             enddo
          enddo



                      call BEM(nz_um,nlev(ibui),nhourday,dt,bs_u(1,iurb),                &
                   bs_u(2,iurb),dz_u,nwr_u,nf_u,nwr_u,ngb_u,sfwb1D,gfwb1D,   &
                   sfwinb1D,sfr_avb(1,ibui),lfrb(1,ibui),gfrb(1,ibui),sfrbpv(1,ibui),   &
                   latent,sigma,albw,albwin,albr,                            &
     	           emr,emw,emwind,rsw,rlw,r,cp_u,                            &
     	           da_u,tmp_u,qv_u,pr_u,rs,rld,dzw,csw,alaw,pwin_u(iurb),    &
                   cop_u(iurb),beta_u(iurb),sw_cond_u(iurb),time_on_u(iurb), &
                   time_off_u(iurb),targtemp_u(iurb),gaptemp_u(iurb),        &
                   targhum_u(iurb),gaphum_u(iurb),perflo_u(iurb),            &
                   gr_frac_roof,pv_frac_roof,gr_flag,  & 
                   ua_u,va_u,                                                  &
                   hsesf_u(iurb),hsequip,                                    &
     	           dzf,csf,alaf,dzgb,csgb,alagb,dzr,csr,                     &
     	           alar,tlevb1D,qlevb1D,twb1D,twlevb1D,tflevb1D,tglevb1D,    &
     	           trb1D,sflev1D,lflev1D,consumlev1D,eppvlev(ibui),tpvlevb(1,ibui), &
                   sfvlev1D,lfvlev1D,hfgrb(1,ibui),tr_avb(1,ibui),           &
                    tpv(ibui),sfpv(ibui),sfr_indoor(ibui))
          




         tpvlevb(2,ibui)=tpvlevb(1,ibui)
         sfrb(2,ibui)=sfrb(1,ibui)
         sfrvb(2,ibui)=sfrvb(1,ibui)
         lfrvb(2,ibui)=lfrvb(1,ibui)
         lfrb(2,ibui)=lfrb(1,ibui)
         sfrbpv(2,ibui)=sfrbpv(1,ibui)
         gfrb(2,ibui)=gfrb(1,ibui)
         hfgrb(2,ibui)=hfgrb(1,ibui)


           do iz=1,nz_um
             qlev(iz,ibui)=qlevb1D(iz)
             tlev(iz,ibui)=tlevb1D(iz)
             sflev(iz,ibui)=sflev1D(iz)
             lflev(iz,ibui)=lflev1D(iz)
             consumlev(iz,ibui)=consumlev1D(iz)
             sfvlev(iz,ibui)=sfvlev1D(iz)
             lfvlev(iz,ibui)=lfvlev1D(iz)
           enddo
 
           do id=1,ndm
              do ily=1,nwr_u
                 trb(id,ily,ibui)=trb1D(ily)
              enddo   
              do ily=1,ngb_u
                 tglev(id,ily,ibui)=tglevb1D(ily) 
              enddo

              do ily=1,nf_u
              do iz=1,nz_um-1
                 tflev(id,ily,iz,ibui)=tflevb1D(ily,iz)
              enddo
              enddo
           

             do iz=1,nz_um
                do ily=1,nwr_u
                   tw(2*id-1,iz,ily,ibui)=twb1D(2*id-1,ily,iz)
                   tw(2*id,iz,ily,ibui)=twb1D(2*id,ily,iz)
                enddo
                gfw(2*id-1,iz,ibui)=gfwb1D(2*id-1,iz)
                gfw(2*id,iz,ibui)=gfwb1D(2*id,iz)
                twlev(2*id-1,iz,ibui)=twlevb1D(2*id-1,iz)
                twlev(2*id,iz,ibui)=twlevb1D(2*id,iz)
             enddo
           enddo       

        enddo 
   





       ibui=0

        do iz=1,nzu
	   
         if(ss(iz).gt.0) then		
           ibui=ibui+1	
           do id=1,ndm	
              gfr(id,iz)=gfrb(id,ibui)
	      tpvlev(id,iz)=tpvlevb(id,ibui)
              sfr(id,iz)=sfrb(id,ibui)
              hfgr(id,iz)=hfgrb(id,ibui)
              sfrpv(id,iz)=-sfrbpv(id,ibui)
              lfr(id,iz)=lfrb(id,ibui)
              do ily=1,nwr_u
                 tr(id,iz,ily)=trb(id,ily,ibui)
              enddo
              ptr(id,iz)=tr(id,iz,nwr_u)*(pr_u(iz)/p0)**(-rcp_u)
           enddo
         endif
        enddo 



       do id=1,ndm
          do iz=1,nzu
             do ibui=1,nbui
                ptw(2*id-1,iz,ibui)=tw(2*id-1,iz,nwr_u,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptw(2*id,iz,ibui)=tw(2*id,iz,nwr_u,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptwin(2*id-1,iz,ibui)=twlev(2*id-1,iz,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
                ptwin(2*id,iz,ibui)=twlev(2*id,iz,ibui)*(pr_u(iz)/p0)**(-rcp_u) 
              
             enddo
          enddo
       enddo

     do iz=1,nz_um
       alp=0.
       do id=1,ndu
        alp=alp+bs(id)/(ws(id)+bs(id))*pb(iz)
       enddo
       alp=alp/ndu
       if(alp.lt.0.29)then
        cdrag(iz)=3.32*alp**0.47
       else
        cdrag(iz)=1.85
       endif
     enddo

             
        


      call buildings(iurb,ndu,nzu,z0,cdrag,ua_u,va_u,                               & 
                     pt_u,pt0_u,ptg,ptr,ptrv,da_u,qv_u,pr_u,tmp_u,ptw,ptwin,pwin_u(iurb),drst,     &                      
                     uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,qvb_u,qhb_u,   & 
                     uhb_u,vhb_u,thb_u,ehb_u,ss,dt,sfw,sfg,sfr,sfrpv,sfrv,lfrv,   &
                     dgr,dg,lfr,lfg,                                                                    &
                     sfwin,pb,bs_u,dz_u,sflev,lflev,sfvlev,lfvlev,tvb_ac,ix,iy,rsg,rs,qr,gr_frac_roof,  &
                     pv_frac_roof,gr_flag,gr_type)  
      






      


      call urban_meso(ndu,kms,kme,kts,kte,nzu,z,dz,z_u,pb,ss,bs,ws,sf, & 
                     vl,uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,     &
                     uhb_u,vhb_u,thb_u,ehb_u,qhb_u,qvb_u,              &
                     a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e,b_q,tvb_ac,b_ac)                    
       



      call interp_length(ndu,kms,kme,kts,kte,nzu,z_u,z,ss,ws,bs,dlg,dl_u)
      
      return
      end subroutine BEP1D




       subroutine param(iurb,nzu,nzurb,nzurban,ndu,                   &
                       csg_u,csg,alag_u,alag,csr_u,csr,               &
                       alar_u,alar,csw_u,csw,alaw_u,alaw,             &
                       ws_u,ws_urb,ws,bs_u,bs_urb,bs,z0g_u,z0r_u,z0,  &  
                       strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,   &
                       pb_urb,pb,dzw,dzr,dzf,csf,alaf,dzgb,csgb,alagb,&
                       lp_urb,lb_urb,hgt_urb,frc_urb)        





      implicit none

  



      integer iurb                 
      integer nzu                  
      integer ndu                  
      integer nzurb                
      real alag_u(nurbm)           
      real alar_u(nurbm)           
      real alaw_u(nurbm)           
      real bs_u(ndm,nurbm)         
      real csg_u(nurbm)            
      real csr_u(nurbm)            
      real csw_u(nurbm)            
      real drst_u(ndm,nurbm)       
      real strd_u(ndm,nurbm)       
      real ws_u(ndm,nurbm)         
      real z0g_u(nurbm)            
      real z0r_u(nurbm)            
      real ss_u(nz_um,nurbm)       
      real pb_u(nz_um,nurbm)       
      real lp_urb                
      real lb_urb                
      real hgt_urb               
      real frc_urb               




      real alag(ng_u)           
      real csg(ng_u)            
      real bs(ndm)              
      real drst(ndm)            
      real strd(ndm)            
      real ws(ndm)              
      real z0(ndm,nz_um)      
      real ss(nz_um)          
      real pb(nz_um)          
      integer nzurban





      real dzw(nwr_u)       
      real dzr(nwr_u)       
      real dzf(nf_u)        
      real dzgb(ngb_u)      

      real csr(nwr_u)       
      real csw(nwr_u)       

      real csf(nf_u)        
                            
      real csgb(ngb_u)      
                            
      real alar(nwr_u+1)    
      real alaw(nwr_u+1)    
      real alaf(nf_u+1)     
      real alagb(ngb_u+1)   
      real bs_urb(ndm,nurbm)         
      real ws_urb(ndm,nurbm)         
      real ss_urb(nz_um,nurbm)       
      real pb_urb(nz_um)             



      integer id,ig,ir,iw,iz,iflo,ihu






      ss=0.
      pb=0.
      csg=0.
      alag=0.
      csgb=0.
      alagb=0.
      csf=0.
      alaf=0.
      csr=0.
      alar=0.
      csw=0.
      alaw=0.
      z0=0.
      ws=0.
      bs=0.
      bs_urb=0.
      ws_urb=0.
      strd=0.
      drst=0.
      nzurban=0



      dzgb=(/0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/)
      dzr=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/)   
      dzw=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/)
      dzf=(/0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02/) 
  
       ihu=0

       do iz=1,nz_um
          if (ss_urb(iz,iurb)/=0.) then
             ihu=1
             exit
          else
             continue
          endif
       enddo

       if (ihu==1) then
          do iz=1,nzurb+1
             ss(iz)=ss_urb(iz,iurb)
             pb(iz)=pb_urb(iz)
          enddo
          nzurban=nzurb
       else
          do iz=1,nzu+1
             ss(iz)=ss_u(iz,iurb)
             pb(iz)=pb_u(iz,iurb)
             ss_urb(iz,iurb)=ss_u(iz,iurb)
             pb_urb(iz)=pb_u(iz,iurb)
          end do 
          nzurban=nzu
       endif
      
      do ig=1,ngb_u
        csgb(ig) = csg_u(iurb)
        alagb(ig)= csg_u(iurb)*alag_u(iurb)
      enddo
      alagb(ngb_u+1)= csg_u(iurb)*alag_u(iurb)

      do iflo=1,nf_u
        csf(iflo) = csw_u(iurb)
        alaf(iflo)= csw_u(iurb)*alaw_u(iurb) 
      enddo
      alaf(nf_u+1)= csw_u(iurb)*alaw_u(iurb) 
     
      do ir=1,nwr_u
        csr(ir) = csr_u(iurb)
        alar(ir)= csr_u(iurb)*alar_u(iurb)
      enddo
      alar(nwr_u+1)= csr_u(iurb)*alar_u(iurb)

      do iw=1,nwr_u
        csw(iw) = csw_u(iurb)
        alaw(iw)= csw_u(iurb)*alaw_u(iurb)
      enddo
      alaw(nwr_u+1)=csw_u(iurb)*alaw_u(iurb) 


                 
       do ig=1,ng_u
        csg(ig)=csg_u(iurb)
        alag(ig)=alag_u(iurb)
       enddo
       
       do id=1,ndu
          z0(id,1)=z0g_u(iurb)
        do iz=2,nzurban+1
           z0(id,iz)=z0r_u(iurb)
        enddo
       enddo
      
       do id=1,ndu
          strd(id)=strd_u(id,iurb)
          drst(id)=drst_u(id,iurb)     
       enddo

       do id=1,ndu
          if ((hgt_urb<=0.).OR.(lp_urb<=0.).OR.(lb_urb<=0.)) then
              ws(id)=ws_u(id,iurb)
              bs(id)=bs_u(id,iurb)
              bs_urb(id,iurb)=bs_u(id,iurb)
              ws_urb(id,iurb)=ws_u(id,iurb)
          else if ((lp_urb/frc_urb<1.).and.(lp_urb<lb_urb)) then
                  bs(id)=2.*hgt_urb*lp_urb/(lb_urb-lp_urb)
                  ws(id)=2.*hgt_urb*lp_urb*((frc_urb/lp_urb)-1.)/(lb_urb-lp_urb)
                  bs_urb(id,iurb)=bs(id)
                  ws_urb(id,iurb)=ws(id)
               else
                  ws(id)=ws_u(id,iurb)
                  bs(id)=bs_u(id,iurb)
                  bs_urb(id,iurb)=bs_u(id,iurb)
                  ws_urb(id,iurb)=ws_u(id,iurb)
          endif
       enddo
       do id=1,ndu
          if ((bs(id)<=1.).OR.(bs(id)>=150.)) then
             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
             bs_urb(id,iurb)=bs_u(id,iurb)
             ws_urb(id,iurb)=ws_u(id,iurb)
          endif
          if ((ws(id)<=1.).OR.(ws(id)>=150.)) then
             ws(id)=ws_u(id,iurb)
             bs(id)=bs_u(id,iurb)
             bs_urb(id,iurb)=bs_u(id,iurb)
             ws_urb(id,iurb)=ws_u(id,iurb)
          endif
       enddo
       return
       end subroutine param
       



      subroutine interpol(kms,kme,kts,kte,nz_u,z,z_u,c,c_u)








      implicit none





      integer kts,kte,kms,kme            
      real z(kms:kme)          
      real c(kms:kme)            

      integer nz_u          

      real z_u(nz_um)      





      real c_u(nz_um)        
 


      integer iz_u,iz
      real ctot,dz





       do iz_u=1,nz_u
        ctot=0.
        do iz=kts,kte
         dz=max(min(z(iz+1),z_u(iz_u+1))-max(z(iz),z_u(iz_u)),0.)
         ctot=ctot+c(iz)*dz
        enddo
        c_u(iz_u)=ctot/(z_u(iz_u+1)-z_u(iz_u))
       enddo
       
       return
       end subroutine interpol
         



      subroutine  averaging_temp(tw,twlev,ss,pb,tw_av,twlev_av,       &
                                 sfw_av,sfwind_av,sfw,sfwin) 

      implicit none



      real tw(2*ndm,nz_um,nwr_u,nbui_max)        
      real twlev(2*ndm,nz_um,nbui_max)     
      real pb(nz_um)                    
      real ss(nz_um)                    
      real sfw(2*ndm,nz_um,nbui_max)             
      real sfwin(2*ndm,nz_um,nbui_max)     



      real tw_av(2*ndm,nz_um)           
      real twlev_av(2*ndm,nz_um)        
      real sfw_av(2*ndm,nz_um)          
      real sfwind_av(2*ndm,nz_um)       



      real d_urb(nz_um)    
      integer nlev(nz_um)            
      integer id,iz
      integer nbui,ibui



      tw_av=0.
      twlev_av=0.
      sfw_av=0.
      sfwind_av=0.
      ibui=0
      nbui=0
      nlev=0
      d_urb=0.

      do iz=1,nz_um		   
         if(ss(iz).gt.0) then		
           ibui=ibui+1
           d_urb(ibui)=ss(iz)
           nlev(ibui)=iz-1
           nbui=ibui		               
         endif
      enddo
      
      do id=1,ndm
         do iz=1,nz_um-1
            if (pb(iz+1).gt.0) then
                do ibui=1,nbui
                   if (iz.le.nlev(ibui)) then
                      tw_av(2*id-1,iz)=tw_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*&
                                       tw(2*id-1,iz,nwr_u,ibui)**4
                      tw_av(2*id,iz)=tw_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*&
                                     tw(2*id,iz,nwr_u,ibui)**4
                      twlev_av(2*id-1,iz)=twlev_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*&
                                          twlev(2*id-1,iz,ibui)**4
                      twlev_av(2*id,iz)=twlev_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*&
                                        twlev(2*id,iz,ibui)**4
                      sfw_av(2*id-1,iz)=sfw_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*sfw(2*id-1,iz,ibui)
                      sfw_av(2*id,iz)=sfw_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*sfw(2*id,iz,ibui)
                      sfwind_av(2*id-1,iz)=sfwind_av(2*id-1,iz)+(d_urb(ibui)/pb(iz+1))*sfwin(2*id-1,iz,ibui)
                      sfwind_av(2*id,iz)=sfwind_av(2*id,iz)+(d_urb(ibui)/pb(iz+1))*sfwin(2*id,iz,ibui)
                   endif
                enddo
                tw_av(2*id-1,iz)=tw_av(2*id-1,iz)**(1./4.)
                tw_av(2*id,iz)=tw_av(2*id,iz)**(1./4.)
                twlev_av(2*id-1,iz)=twlev_av(2*id-1,iz)**(1./4.)
                twlev_av(2*id,iz)=twlev_av(2*id,iz)**(1./4.)
            endif
         enddo 
      enddo 
      return
      end subroutine averaging_temp



      subroutine modif_rad(iurb,nd,nz_u,z,ws,drst,strd,ss,pb,    &
                          tw,tg_av,twlev,albg,albw,emw,emg,pwin,albwin,   &
                          emwin,fww,fwg,fgw,fsw,fsg,             &
                          zr,deltar,ah,xlat,swddir,swddif,           &    
                          rs,rl,rsw,rsd,rsg,rlw,rlg)                       
 





      implicit none
 
 



      integer iurb              
      integer nd                
      integer nz_u              
      real z(nz_um)           
      real ws(ndm)              
      real drst(ndm)            
      real strd(ndm)            
      real ss(nz_um)          
      real pb(nz_um)          
      real tw(2*ndm,nz_um)    
      real tg_av(ndm)         
      real albg                 
      real albw                 
      real emg                  
      real emw                  
      real fgw(nz_um,ndm,nurbm)       
      real fsg(ndm,nurbm)             
      real fsw(nz_um,ndm,nurbm)       
      real fws(nz_um,ndm,nurbm)       
      real fwg(nz_um,ndm,nurbm)       
      real fww(nz_um,nz_um,ndm,nurbm) 
      real ah                   
      real zr                   
      real deltar               
      real rs                   
      real rl                   
      real xlat                 
      real swddir               
      real swddif               




      real twlev(2*ndm,nz_um)         
      real pwin                       
      real albwin                     
      real emwin                      
      real alb_av                     



      real rlg(ndm)             
      real rlw(2*ndm,nz_um)     
      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     
      real rsd(2*ndm,nz_um)     





      integer id,iz



      call shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,        &
                     swddir,rsw,rsg,xlat)
      rsd=rsw


      do id=1,nd
         call long_rad(iurb,nz_u,id,emw,emg,emwin,pwin,twlev,      &
                      fwg,fww,fgw,fsw,fsg,tg_av,tw,rlg,rlw,rl,pb)

         alb_av=pwin*albwin+(1.-pwin)*albw
         
        call short_rad_dd(iurb,nz_u,id,alb_av,                        &
                           albg,swddif,fwg,fww,fgw,fsw,fsg,rsg,rsw,pb)
 
 
      enddo
      return
      end subroutine modif_rad






      subroutine surf_temp(nd,pr,dt,rl,rsg,rlg,              &
                           tg,alag,csg,emg,albg,ptg,sfg,lfg,gfg) 





      implicit none
                  




      integer nd                
      real alag(ng_u)           

      real albg                 

      real csg(ng_u)            

      real dt                   
      real emg                  

      real pr(nz_um)            
      
      real rl                   
      real rlg(ndm)             
     
      real rsg(ndm)             
      
      real sfg(ndm)             

      real lfg(ndm)             

      real gfg(ndm)             

      real tg(ndm,ng_u)         




      real ptg(ndm)             




      integer id,ig,ir,iw,iz

      real rtg(ndm)             

      real tg_tmp(ng_u)

      real dzg_u(ng_u)          
      
      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/

     




        
   
      do id=1,nd


       do ig=1,ng_u
        tg_tmp(ig)=tg(id,ig)
       end do



       call soil_temp(ng_u,dzg_u,tg_tmp,ptg(id),alag,csg,      &
                     rsg(id),rlg(id),pr(1),                    &
                     dt,emg,albg,                              &
                     rtg(id),sfg(id),lfg(id),gfg(id))    

       do ig=1,ng_u
        tg(id,ig)=tg_tmp(ig)
       end do
	
      end do 
      
      return
      end subroutine surf_temp






      subroutine roof_temp_veg(nd,pr,dt,rl,rsr,              &
                           trv,ptrv,sfrv,lfrv,gfr,qr,rainbl,drain,hfgroof,tr,alar,dzr,csr,nzu,irri_now,gr_type,pv_frac_roof,tpvlev)





      implicit none




      real rainbl
      integer nd                

      integer nzu                
      real irho(24)                

      real alar           
      real pv_frac_roof
      real csr

      real dzr          

      real dt                   

      real pr(nz_um)            

      real rl                   

      real rsr                 

     real tpvlev(ndm,nz_um)      

      real sfrv(ndm,nz_um)             

      real lfrv(ndm,nz_um)             

      real gfr(ndm,nz_um)             

      real trv(ndm,nz_um,ngr_u)         

      real qr(ndm,nz_um,ngr_u)         

      real tr(ndm,nz_um,nwr_u)         
 



      real ptrv(ndm,nz_um)             
      
      real hfgroof(ndm,nz_um)



      integer id,ig,ir,iw,iz

      real alagr(ngr_u)           

      real rtr(ndm,nz_um)             

      real tr_tmp(ngr_u)

      real qr_tmp(ngr_u)
      real qr_tmp_old(ngr_u)
      real dzgr_u(ngr_u)          

      data dzgr_u /0.1,0.003,0.06,0.003,0.05,0.04,0.02,0.0125,0.005,0.0025/
      real cs(ngr_u)  
      real cw
      parameter(cw=4.295e6)
      real s(ngr_u)
      real d(ngr_u)
      real k(ngr_u)
      real qr_m     
      real qrmax(ngr_u)
      real smax(ngr_u)
      real kmax(ngr_u)
      real b(ngr_u)
      real cd(ngr_u)
      real csa(4)
      real ka(4)
      real qref
      parameter(qref=0.37)
      data qrmax /0.0,0.0,0.0,0.0,0.439,0.37,0.37,0.37,0.37,0.37/
      data smax /0,0,0,0,-0.01,-0.1,-0.1,-0.1,-0.1,-0.1/
      data kmax /0,0,0,0,3.32e-3,2.162e-3,2.162e-3,2.162e-3,2.162e-3,2.162e-3/
      data b /0,0,0,0,2.7,3.9,3.9,3.9,3.9,3.9/
      data cd /0,0,0,0,331500,1.342e6,1.342e6,1.342e6,1.342e6,1.342e6/
      data csa /7.5e4,2.1e6,4.48e4,2.1e6/
      data ka /0.035,0.7,0.024,0.7/
      real em_gr(1)
      real alb_gr(1)
      real irri_now
      integer gr_type
      real drain(ndm,nz_um)



      if(gr_type.eq.1)then
      em_gr=0.95
      alb_gr=0.3
      elseif(gr_type.eq.2)then
       em_gr=0.83
       alb_gr=0.154
      endif


      do iz=2,nzu

      do id=1,nd






       do ig=1,ngr_u
        tr_tmp(ig)=trv(id,iz,ig)
        qr(id,iz,ig) = max(qr(id,iz,ig),1e-6) 
        qr_tmp(ig)=qr(id,iz,ig)
        qr_tmp_old(ig)=qr(id,iz,ig) 

      if(ig.le.4) then
 
       cs(ig)=csa(ig)
       alagr(ig)=ka(ig)/csa(ig)

      else
 
 
        if (ig.gt.5) then
        qr_m=(qr(id,iz,ig)*dzgr_u(ig-1)+qr(id,iz,ig-1)*dzgr_u(ig))/(dzgr_u(ig)+dzgr_u(ig-1))
        else
        qr_m=qr(id,iz,ig)
        endif
        cs(ig)=(1-qr_m)*cd(ig)+qr_m*cw
        s(ig)=smax(ig)*(qrmax(ig)/qr_m)**b(ig)
        k(ig)=kmax(ig)*(qr_m/qrmax(ig))**(2*b(ig)+3)
        d(ig)=-b(ig)*kmax(ig)*smax(ig)*((qr_m/qrmax(ig))**(b(ig)+3))/qr_m
        if (log10(abs(s(ig))).le.5.1) then
          alagr(ig)=exp(-(log10(abs(s(ig)))+2.7))*4.186e2/cs(ig)
        endif
        if (log10(abs(s(ig))).gt.5.1) then
          alagr(ig)=0.00041*4.186e2/cs(ig)
        endif

       endif

        end do
        hfgroof(id,iz)=(alar/csr+alagr(1))*(tr_tmp(1)-tr(id,iz,5))/(dzr+dzgr_u(1))
 
       call soil_temp_veg(hfgroof(id,iz),ngr_u,dzgr_u,tr_tmp,ptrv(id,iz),alagr,cs,      &
                     rsr,rl,pr(iz),                    &
                     dt,em_gr(1),alb_gr(1),                              &
                     rtr(id,iz),sfrv(id,iz),lfrv(id,iz),gfr(id,iz),pv_frac_roof,tpvlev(id,iz))
       do ig=1,ngr_u
        trv(id,iz,ig)=tr_tmp(ig)
       end do
        drain(id,iz)=kmax(5)*(qr(id,iz,5)/qrmax(5))**(2*b(5)+3)
        call soil_moist(ngr_u,dzgr_u,qr_tmp,dt,lfrv(id,iz),d,k,rainbl,drain(id,iz),irri_now)
 
     
        do ig=1,ngr_u
          
           qr(id,iz,ig)=max(min(qr_tmp(ig),qrmax(ig)),1e-6) 
         end do
   
      end do 
      end do 

      return
      end subroutine roof_temp_veg




      subroutine buildings(iurb,nd,nz,z0,cdrag,ua_u,va_u,pt_u,pt0_u,       &
                        ptg,ptr,ptrv,da_u,qv_u,pr_u,tmp_u,ptw,ptwin,pwin,                 &
                        drst,uva_u,vva_u,uvb_u,vvb_u,                &
                        tva_u,tvb_u,evb_u,qvb_u,qhb_u,               &
                        uhb_u,vhb_u,thb_u,ehb_u,ss,dt,sfw,sfg,sfr,sfrpv,sfrv,lfrv,   &
                        dgr,dg,lfr,lfg,                                               &
                        sfwin,pb,bs_u,dz_u,sflev,lflev,sfvlev,lfvlev,tvb_ac,ix,iy,rsg,rs,qr,gr_frac_roof,  &
                        pv_frac_roof,gr_flag,gr_type)                  







      implicit none

        



      integer nd                
      integer ix,iy
      integer nz                
      real ua_u(nz_um)          
      real va_u(nz_um)          
      real da_u(nz_um)          
      real qv_u(nz_um)          
      real pr_u(nz_um)          
      real tmp_u(nz_um)         
      real drst(ndm)            
      real dz
      real pt_u(nz_um)          
      real pt0_u(nz_um)         
      real ptg(ndm)             
      real ptr(ndm,nz_um)       
      real ptrv(ndm,nz_um)      
      real ptw(2*ndm,nz_um,nbui_max)     
      real ss(nz_um)            
      real pb(nz_um)
      real cdrag(nz_um)
      real z0(ndm,nz_um)        
      real dt 
      integer iurb              
      real rsg(ndm)             
      real rs                  
      real qr(ndm,nz_um,ngr_u)         
      real trv(ndm,nz_um,ngr_u)         
      real roof_frac
      real road_frac



      real bs_u(ndm,nurbm)    
      real dz_u               
      real sflev(nz_um,nz_um)     
      real lflev(nz_um,nz_um)     
      real sfvlev(nz_um,nz_um)    
      real lfvlev(nz_um,nz_um)    
      real qvb_u(2*ndm,nz_um)
      real qhb_u(ndm,nz_um)
      real ptwin(2*ndm,nz_um,nbui_max)  
      real pwin
      real tvb_ac(2*ndm,nz_um)
      real gr_frac_roof
      real pv_frac_roof
      integer gr_flag,gr_type









      real uhb_u(ndm,nz_um)   
      real uva_u(2*ndm,nz_um)   
      real uvb_u(2*ndm,nz_um)   
      real vhb_u(ndm,nz_um)   
      real vva_u(2*ndm,nz_um)   
      real vvb_u(2*ndm,nz_um)   
      real thb_u(ndm,nz_um)   
      real tva_u(2*ndm,nz_um)   
      real tvb_u(2*ndm,nz_um)   
      real ehb_u(ndm,nz_um)   
      real evb_u(2*ndm,nz_um)   
      real uhb(2*ndm,nz_um)
      real vhb(2*ndm,nz_um)
      real ehb(2*ndm,nz_um)
      real sfw(2*ndm,nz_um,nbui_max)   
      real sfwin(2*ndm,nz_um,nbui_max) 
      real sfr(ndm,nz_um)           
      real sfrv(ndm,nz_um)           
      real lfrv(ndm,nz_um)           
      real dgr(ndm,nz_um)           
      real dg(ndm)
      real lfr(ndm,nz_um)           
      real lfg(ndm)                 
      real sfrpv(ndm,nz_um)         
      real sfg(ndm)                 





      real d_urb(nz_um)
      real uva_tmp
      real vva_tmp
      real uvb_tmp
      real vvb_tmp 
      real evb_tmp     
      integer nlev(nz_um)
      integer id,iz,ibui,nbui,il
      real wfg     
      real wfr     
      real uhbv(2*ndm,nz_um)
      real vhbv(2*ndm,nz_um)
      real ehbv(2*ndm,nz_um)
      real z0v     
      parameter(z0v=0.01)
      real resg
      real rsveg
      real f1,f2,f3,f4
      integer rsv(2)
      real qr_tmp(ngr_u)
      data rsv /0,1/
      real fh,ric,utot



      dz=dz_u
      ibui=0
      nbui=0
      nlev=0
      d_urb=0.
      
      uva_u=0.
      uvb_u=0.
      vhb_u=0.
      vva_u=0.
      vvb_u=0.
      thb_u=0.
      tva_u=0.
      tvb_u=0.
      tvb_ac=0.
      ehb_u=0.
      evb_u=0.
      qvb_u=0.
      qhb_u=0.
      
      uhb=0.
      vhb=0.
      ehb=0.
      uhbv=0.
      vhbv=0.
      ehbv=0.


      do iz=1,nz_um		   
         if(ss(iz).gt.0)then		
           ibui=ibui+1
           d_urb(ibui)=ss(iz)
           nlev(ibui)=iz-1
           nbui=ibui		               
         endif
      enddo


      do id=1,nd
      
          call flux_flat(dz,z0(id,1),ua_u(1),va_u(1),pt_u(1),pt0_u(1),  &
                       ptg(id),qv_u(1),uhb(id,1),                            & 
                       vhb(id,1),sfg(id),lfg(id),ehb(id,1),da_u(1),pr_u(1))        
          if(dg(id).gt.0)then
           wfg=dg(id)/dgmax
           lfg(id)=-da_u(1)*latent*(-(wfg*lfg(id))/(da_u(1)*latent))
          else
           qhb_u(id,1)=0.
           lfg(id)=0.
          endif   
         thb_u(id,1)=-(sfg(id))/(da_u(1)*cp_u)
         vhb_u(id,1)=vhb(id,1)
         uhb_u(id,1)=uhb(id,1)
         ehb_u(id,1)=ehb(id,1)
         qhb_u(id,1)=-lfg(id)/(da_u(1)*latent)
         do iz=2,nz
            if(ss(iz).gt.0)then
            
               call flux_flat(dz,z0(id,iz),ua_u(iz),&
                       va_u(iz),pt_u(iz),pt0_u(iz), &
                       ptr(id,iz),qv_u(iz),uhb(id,iz),       &
                       vhb(id,iz),sfr(id,iz),lfr(id,iz),ehb(id,iz),da_u(iz),pr_u(iz))
         if(dgr(id,iz).gt.0)then
          wfr=dgr(id,iz)/drmax
          lfr(id,iz)=-da_u(iz)*latent*(-(wfr*lfr(id,iz))/(da_u(iz)*latent))
         else
          lfr(id,iz)=0.
         endif
         if(gr_flag.eq.1.and.gr_frac_roof.gt.0.)then  
         do il=1,ngr_u
           qr_tmp(il)=qr(id,iz,il)
         enddo
               call flux_flat_roof(dz,z0v,ua_u(iz),va_u(iz),pt_u(iz),pt0_u(iz),  &
                       ptrv(id,iz),uhbv(id,iz),                            &
                       vhbv(id,iz),sfrv(id,iz),lfrv(id,iz),ehbv(id,iz),da_u(iz),qv_u(iz),pr_u(iz),rs,qr_tmp,resg,rsveg,f1,f2,f3,f4,gr_type,pv_frac_roof)
         sfr(id,iz)=sfr(id,iz)+pv_frac_roof*sfrpv(id,iz) 
         thb_u(id,iz)=-((1.-gr_frac_roof)*sfr(id,iz)+gr_frac_roof*sfrv(id,iz))/(da_u(iz)*cp_u)
         vhb_u(id,iz)=(1.-gr_frac_roof)*vhb(id,iz)+gr_frac_roof*vhbv(id,iz)
         uhb_u(id,iz)=(1.-gr_frac_roof)*uhb(id,iz)+gr_frac_roof*uhbv(id,iz)
         ehb_u(id,iz)=(1.-gr_frac_roof)*ehb(id,iz)+gr_frac_roof*ehbv(id,iz)
         qhb_u(id,iz)=-(gr_frac_roof*lfrv(id,iz)+(1.-gr_frac_roof)*lfr(id,iz))/(da_u(iz)*latent)
         sfr(id,iz)=sfr(id,iz)-pv_frac_roof*sfrpv(id,iz)
         else
         sfr(id,iz)=sfr(id,iz)+pv_frac_roof*sfrpv(id,iz)
         thb_u(id,iz)=-sfr(id,iz)/(da_u(iz)*cp_u)
         vhb_u(id,iz)=vhb(id,iz)
         uhb_u(id,iz)=uhb(id,iz)
         ehb_u(id,iz)=ehb(id,iz)
         qhb_u(id,iz)=-lfr(id,iz)/(da_u(iz)*latent)
         sfr(id,iz)=sfr(id,iz)-pv_frac_roof*sfrpv(id,iz)
         endif
            else
               uhb_u(id,iz) = 0.0
               vhb_u(id,iz) = 0.0
               thb_u(id,iz) = 0.0
               ehb_u(id,iz) = 0.0
               qhb_u(id,iz) = 0.0
            endif
         enddo
         
         


 
         do ibui=1,nbui
         do iz=1,nlev(ibui)  
                   
            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),             &  
                        ptw(2*id-1,iz,ibui),ptwin(2*id-1,iz,ibui),          &   
                        uva_tmp,vva_tmp,                                    &   
                        uvb_tmp,vvb_tmp,                                    &   
                        sfw(2*id-1,iz,ibui),sfwin(2*id-1,iz,ibui),          &   
                        evb_tmp,drst(id),dt,cdrag(iz))      
   
            if (pb(iz+1).gt.0.) then

                    uva_u(2*id-1,iz)=uva_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*uva_tmp
                    vva_u(2*id-1,iz)=vva_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*vva_tmp
                    uvb_u(2*id-1,iz)=uvb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*uvb_tmp
                    vvb_u(2*id-1,iz)=vvb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*vvb_tmp
                    evb_u(2*id-1,iz)=evb_u(2*id-1,iz)+d_urb(ibui)/pb(iz+1)*evb_tmp
                    tvb_u(2*id-1,iz)=tvb_u(2*id-1,iz)-(d_urb(ibui)/pb(iz+1))*                       &
                                    (sfw(2*id-1,iz,ibui)*(1.-pwin)+sfwin(2*id-1,iz,ibui)*pwin)/     &
                                    da_u(iz)/cp_u-(1./4.)*(d_urb(ibui)/pb(iz+1))*(sfvlev(iz,ibui)-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    tvb_ac(2*id-1,iz)=tvb_ac(2*id-1,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    qvb_u(2*id-1,iz)=qvb_u(2*id-1,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(lfvlev(iz,ibui)-lflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/latent
                                     
            endif

            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),    &   
                        ptw(2*id,iz,ibui),ptwin(2*id,iz,ibui),     &    
                        uva_tmp,vva_tmp,                           &    
                        uvb_tmp,vvb_tmp,                           &    
                        sfw(2*id,iz,ibui),sfwin(2*id,iz,ibui),     &   
                        evb_tmp,drst(id),dt,cdrag(iz)) 

            if (pb(iz+1).gt.0.) then

                    uva_u(2*id,iz)=uva_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*uva_tmp
                    vva_u(2*id,iz)=vva_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*vva_tmp
                    uvb_u(2*id,iz)=uvb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*uvb_tmp
                    vvb_u(2*id,iz)=vvb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*vvb_tmp
                    evb_u(2*id,iz)=evb_u(2*id,iz)+d_urb(ibui)/pb(iz+1)*evb_tmp
                    tvb_u(2*id,iz)=tvb_u(2*id,iz)-(d_urb(ibui)/pb(iz+1))*                    &
                                    (sfw(2*id,iz,ibui)*(1.-pwin)+sfwin(2*id,iz,ibui)*pwin)/  &
                                     da_u(iz)/cp_u-(1./4.)*(d_urb(ibui)/pb(iz+1))*(sfvlev(iz,ibui)-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    tvb_ac(2*id,iz)=tvb_ac(2*id,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(-sflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/cp_u
                    qvb_u(2*id,iz)=qvb_u(2*id,iz)-(1./4.)*(d_urb(ibui)/pb(iz+1))*(lfvlev(iz,ibui)-lflev(iz,ibui))/&
                                    (dz*bs_u(id,iurb))/da_u(iz)/latent

            endif

          enddo 
         enddo 
         
      end do 
                
      return
      end subroutine buildings
      




        subroutine urban_meso(nd,kms,kme,kts,kte,nz_u,z,dz,z_u,pb,ss,bs,ws,sf,vl,    &
                             uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u, &       
                             uhb_u,vhb_u,thb_u,ehb_u,qhb_u,qvb_u,       &      
                             a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e,b_q,tvb_ac,b_ac)           







      implicit none





      integer kms,kme,kts,kte               
      real z(kms:kme)              
      real dz(kms:kme)               


      integer nz_u              
      integer nd                
      real bs(ndm)              
      real ws(ndm)              
      real z_u(nz_um)         
      real pb(nz_um)          
      real ss(nz_um)          
      real uhb_u(ndm,nz_um)   
      real uva_u(2*ndm,nz_um)   
      real uvb_u(2*ndm,nz_um)   
      real vhb_u(ndm,nz_um)   
      real vva_u(2*ndm,nz_um)   
      real vvb_u(2*ndm,nz_um)   
      real thb_u(ndm,nz_um)   
      real tva_u(2*ndm,nz_um)   
      real tvb_u(2*ndm,nz_um)   
      real tvb_ac(2*ndm,nz_um)
      real ehb_u(ndm,nz_um)   
      real evb_u(2*ndm,nz_um)   



      real qhb_u(ndm,nz_um)
      real qvb_u(2*ndm,nz_um)
     





      real sf(kms:kme)             
      real vl(kms:kme)               
      real a_u(kms:kme)              
      real a_v(kms:kme)              
      real a_t(kms:kme)              
      real a_e(kms:kme)              
      real b_u(kms:kme)              
      real b_v(kms:kme)              
      real b_t(kms:kme)              
      real b_ac(kms:kme)
      real b_e(kms:kme)              
      real b_q(kms:kme)               



      real dzz
      real fact
      integer id,iz,iz_u
      real se,sr,st,su,sv,sq
      real uet(kms:kme)                
      real veb,vta,vtb,vte,vtot,vua,vub,vva,vvb,vqb,vtb_ac








      do iz=kts,kte
         a_u(iz)=0.
         a_v(iz)=0.
         a_t(iz)=0.
         a_e(iz)=0.
         b_u(iz)=0.
         b_v(iz)=0.
         b_e(iz)=0.
         b_t(iz)=0.
         b_ac(iz)=0.
         uet(iz)=0.
         b_q(iz)=0.
      end do
            

      do iz=kts,kte
         sf(iz)=0.
         vl(iz)=0.
      enddo
      sf(kte+1)=0. 
      
      do id=1,nd      
         do iz=kts+1,kte+1
            sr=0.
            do iz_u=2,nz_u
               if(z(iz).lt.z_u(iz_u).and.z(iz).ge.z_u(iz_u-1))then
                  sr=pb(iz_u)
               endif
            enddo
            sf(iz)=sf(iz)+((ws(id)+(1.-sr)*bs(id))/(ws(id)+bs(id)))/nd
         enddo
      enddo


      do iz=kts,kte
         do id=1,nd
            vtot=0.
            do iz_u=1,nz_u
               dzz=max(min(z_u(iz_u+1),z(iz+1))-max(z_u(iz_u),z(iz)),0.)
               vtot=vtot+pb(iz_u+1)*dzz
            enddo
            vtot=vtot/(z(iz+1)-z(iz))
            vl(iz)=vl(iz)+(1.-vtot*bs(id)/(ws(id)+bs(id)))/nd
         enddo
      enddo
      


      do id=1,nd
      
         fact=1./vl(kts)/dz(kts)*ws(id)/(ws(id)+bs(id))/nd
         b_t(kts)=b_t(kts)+thb_u(id,1)*fact
         b_u(kts)=b_u(kts)+uhb_u(id,1)*fact
         b_v(kts)=b_v(kts)+vhb_u(id,1)*fact 
         b_e(kts)=b_e(kts)+ehb_u(id,1)*fact*(z_u(2)-z_u(1))
         b_q(kts)=b_q(kts)+qhb_u(id,1)*fact         

         do iz=kts,kte
            st=0.
            su=0.
            sv=0.
            se=0.
            sq=0.
            do iz_u=2,nz_u
               if(z(iz).le.z_u(iz_u).and.z(iz+1).gt.z_u(iz_u))then
                  st=st+ss(iz_u)*thb_u(id,iz_u)
                  su=su+ss(iz_u)*uhb_u(id,iz_u)
                  sv=sv+ss(iz_u)*vhb_u(id,iz_u)          
                  se=se+ss(iz_u)*ehb_u(id,iz_u)*(z_u(iz_u+1)-z_u(iz_u))
                  sq=sq+ss(iz_u)*qhb_u(id,iz_u)
               endif
            enddo
      
            fact=bs(id)/(ws(id)+bs(id))/vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+st*fact
            b_u(iz)=b_u(iz)+su*fact
            b_v(iz)=b_v(iz)+sv*fact
            b_e(iz)=b_e(iz)+se*fact
            b_q(iz)=b_q(iz)+sq*fact
         enddo
      enddo              


           
      do iz=kts,kte 
         uet(iz)=0.
         do id=1,nd              
            vtb=0.
            vtb_ac=0.
            vta=0.
            vua=0.
            vub=0.
            vva=0.
            vvb=0.
            veb=0.
	    vte=0.
            vqb=0.
            do iz_u=1,nz_u
               dzz=max(min(z_u(iz_u+1),z(iz+1))-max(z_u(iz_u),z(iz)),0.)
               fact=dzz/(ws(id)+bs(id))
               vtb=vtb+pb(iz_u+1)*                                  &        
                    (tvb_u(2*id-1,iz_u)+tvb_u(2*id,iz_u))*fact 
               vtb_ac=vtb_ac+pb(iz_u+1)*                            &        
                    (tvb_ac(2*id-1,iz_u)+tvb_ac(2*id,iz_u))*fact     
               vta=vta+pb(iz_u+1)*                                  &        
                   (tva_u(2*id-1,iz_u)+tva_u(2*id,iz_u))*fact
               vua=vua+pb(iz_u+1)*                                  &        
                    (uva_u(2*id-1,iz_u)+uva_u(2*id,iz_u))*fact
               vva=vva+pb(iz_u+1)*                                  &        
                    (vva_u(2*id-1,iz_u)+vva_u(2*id,iz_u))*fact
               vub=vub+pb(iz_u+1)*                                  &        
                    (uvb_u(2*id-1,iz_u)+uvb_u(2*id,iz_u))*fact
               vvb=vvb+pb(iz_u+1)*                                  &        
                    (vvb_u(2*id-1,iz_u)+vvb_u(2*id,iz_u))*fact
               veb=veb+pb(iz_u+1)*                                  &        
                    (evb_u(2*id-1,iz_u)+evb_u(2*id,iz_u))*fact
               vqb=vqb+pb(iz_u+1)*                                  &        
                    (qvb_u(2*id-1,iz_u)+qvb_u(2*id,iz_u))*fact   
            enddo
           
            fact=1./vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+vtb*fact
            b_ac(iz)=b_ac(iz)+vtb_ac*fact
            a_t(iz)=a_t(iz)+vta*fact
            a_u(iz)=a_u(iz)+vua*fact
            a_v(iz)=a_v(iz)+vva*fact
            b_u(iz)=b_u(iz)+vub*fact
            b_v(iz)=b_v(iz)+vvb*fact
            b_e(iz)=b_e(iz)+veb*fact
            uet(iz)=uet(iz)+vte*fact
            b_q(iz)=b_q(iz)+vqb*fact
         enddo              
      enddo
      

      
      return
      end subroutine urban_meso




      subroutine interp_length(nd,kms,kme,kts,kte,nz_u,z_u,z,ss,ws,bs,              &
                             dlg,dl_u)





     
      implicit none





      integer kms,kme,kts,kte                
      real z(kms:kme)              
      integer nd                
      integer nz_u              
      real z_u(nz_um)         
      real bs(ndm)              
      real ss(nz_um)          
      real ws(ndm)              





      real dlg(kms:kme)              
      real dl_u(kms:kme)             




      real dlgtmp
      integer id,iz,iz_u
      real sftot
      real ulu,ssl




   
        do iz=kts,kte
         ulu=0.
         ssl=0.
         do id=1,nd        
          do iz_u=2,nz_u
           if(z_u(iz_u).gt.z(iz))then
            ulu=ulu+ss(iz_u)/z_u(iz_u)/nd
            ssl=ssl+ss(iz_u)/nd
           endif
          enddo
         enddo

        if(ulu.ne.0)then
          dl_u(iz)=ssl/ulu
         else
          dl_u(iz)=0.
         endif
        enddo
       

        do iz=kts,kte
         dlg(iz)=0.
          do id=1,nd
           sftot=ws(id)  
           dlgtmp=ws(id)/((z(iz)+z(iz+1))/2.)
           do iz_u=1,nz_u
            if((z(iz)+z(iz+1))/2..gt.z_u(iz_u))then
             dlgtmp=dlgtmp+ss(iz_u)*bs(id)/                           &                
                    ((z(iz)+z(iz+1))/2.-z_u(iz_u))
             sftot=sftot+ss(iz_u)*bs(id)
            endif
           enddo
           dlg(iz)=dlg(iz)+dlgtmp/sftot/nd
         enddo
         dlg(iz)=1./dlg(iz)
        enddo
        
       return
       end subroutine interp_length




      subroutine shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,    &
                           swddir,rsw,rsg,xlat)
        





      implicit none
     



      integer nd                
      integer nz_u              
      real ah                   
      real deltar               
      real drst(ndm)            
      real swddir                   
      real ss(nz_um)          
      real pb(nz_um)          
      real ws(ndm)              
      real z(nz_um)           
      real zr                   
      real xlat
      real xlat_r



      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     




      integer id,iz,jz
      real aae,aaw,bbb,phix,rd,rtot,wsd
      




         xlat_r=xlat*pi/180

        if(swddir.eq.0.or.sin(zr).eq.1)then
           do id=1,nd
             rsg(id)=0.
             do iz=1,nz_u
               rsw(2*id-1,iz)=0.
               rsw(2*id,iz)=0.
            enddo
         enddo
        else


        if(abs(sin(zr)).gt.1.e-10)then
          if(cos(deltar)*sin(ah)/sin(zr).ge.1)then
           bbb=pi/2.
          elseif(cos(deltar)*sin(ah)/sin(zr).le.-1)then
           bbb=-pi/2.
          else
           bbb=asin(cos(deltar)*sin(ah)/sin(zr))                
           if(sin(deltar).lt.(cos(zr)*sin(xlat_r)))then         
           bbb=pi-bbb                                           
          endif
          endif
         else
          if(cos(deltar)*sin(ah).ge.0)then
           bbb=pi/2.
          elseif(cos(deltar)*sin(ah).lt.0)then
           bbb=-pi/2.
          endif
         endif
        phix=zr
           
         do id=1,nd
         
            rsg(id)=0.

            aae=bbb-drst(id)
            aaw=bbb-drst(id)+pi

            do iz=1,nz_u
               rsw(2*id-1,iz)=0.
               rsw(2*id,iz)=0.
            if(pb(iz+1).gt.0.)then
               do jz=1,nz_u
                if(abs(sin(aae)).gt.1.e-10)then
                  call shade_wall(z(iz),z(iz+1),z(jz+1),phix,aae,   &
                      ws(id),rd)
                  rsw(2*id-1,iz)=rsw(2*id-1,iz)+swddir*rd*ss(jz+1)/pb(iz+1)
                endif

                if(abs(sin(aaw)).gt.1.e-10)then
                  call shade_wall(z(iz),z(iz+1),z(jz+1),phix,aaw,   &
                      ws(id),rd)
                  rsw(2*id,iz)=rsw(2*id,iz)+swddir*rd*ss(jz+1)/pb(iz+1)
                endif
               enddo
            endif
            enddo
        if(abs(sin(aae)).gt.1.e-10)then
            wsd=abs(ws(id)/sin(aae))

            do jz=1,nz_u
               rd=max(0.,wsd-z(jz+1)*tan(phix))
               rsg(id)=rsg(id)+swddir*rd*ss(jz+1)/wsd
            enddo
            rtot=0.

            do iz=1,nz_u
               rtot=rtot+(rsw(2*id,iz)+rsw(2*id-1,iz))*            &
                         (z(iz+1)-z(iz))
            enddo
            rtot=rtot+rsg(id)*ws(id)
        else
            rsg(id)=swddir
        endif


            
         enddo
      endif
         
      return
      end subroutine shadow_mas




         



      subroutine shade_wall(z1,z2,hu,phix,aa,ws,rd)













      implicit none
      



      real aa                   
      real hu                   
      real phix                 
      real ws                   
      real z1                   
      real z2                   




      real rd                   
                                
                                
                                
                                



      real x1,x2                





      x1=min((hu-z1)*tan(phix),max(0.,ws/sin(aa)))
      
      x2=max((hu-z2)*tan(phix),0.)

      rd=max(0.,sin(aa)*(max(0.,x1-x2))/(z2-z1))
      
      return
      end subroutine shade_wall




      subroutine long_rad(iurb,nz_u,id,emw,emg,emwin,pwin,twlev,&
                         fwg,fww,fgw,fsw,fsg,tg_av,tw,rlg,rlw,rl,pb)










      implicit none

  
      



      real emg                        
      real emw                        
      real fgw(nz_um,ndm,nurbm)       
      real fsg(ndm,nurbm)             
      real fsw(nz_um,ndm,nurbm)       
      real fwg(nz_um,ndm,nurbm)       
      real fww(nz_um,nz_um,ndm,nurbm) 
      integer id                      
      integer iurb                    
      integer nz_u                    
      real pb(nz_um)                  
      real rl                         
      real tg_av(ndm)               
      real tw(2*ndm,nz_um)            



      real twlev(2*ndm,nz_um)         
      real emwin                      
      real pwin                       
      




      real rlg(ndm)                   
      real rlw(2*ndm,nz_um)           




      integer i,j
      real aaa(2*nz_um+1,2*nz_um+1)   
      real bbb(2*nz_um+1)             







       
      do i=1,nz_u
        
        do j=1,nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.        
       
        do j=nz_u+1,2*nz_u
         aaa(i,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                  fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
        enddo
        

        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)
        
        bbb(i)=fsw(i,id,iurb)*rl+emg*fgw(i,id,iurb)*sigma*tg_av(id)**4
        do j=1,nz_u
           bbb(i)=bbb(i)+pb(j+1)*sigma*fww(j,i,id,iurb)* &
                 (emw*(1.-pwin)*tw(2*id,j)**4+emwin*pwin*twlev(2*id,j)**4)+ &
                 fww(j,i,id,iurb)*rl*(1.-pb(j+1))
        enddo
        
       enddo
       


       do i=1+nz_u,2*nz_u
        
        do j=1,nz_u
         aaa(i,j)=-(1.-emw*(1.-pwin)-emwin*pwin)*fww(j,i-nz_u,id,iurb)*pb(j+1)
        enddo
        
        do j=1+nz_u,2*nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.
        

        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)
        
        bbb(i)=fsw(i-nz_u,id,iurb)*rl+  &     
               emg*fgw(i-nz_u,id,iurb)*sigma*tg_av(id)**4

        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*sigma*fww(j,i-nz_u,id,iurb)*  &   
                (emw*(1.-pwin)*tw(2*id-1,j)**4+emwin*pwin*twlev(2*id-1,j)**4)+&   
                fww(j,i-nz_u,id,iurb)*rl*(1.-pb(j+1))
        enddo
       
       enddo


       do j=1,nz_u
        aaa(2*nz_u+1,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                         fwg(j,id,iurb)*pb(j+1)
       enddo
       
       do j=nz_u+1,2*nz_u
        aaa(2*nz_u+1,j)=-(1.-emw*(1.-pwin)-emwin*pwin)* &
                         fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
       enddo
       
       aaa(2*nz_u+1,2*nz_u+1)=1.
       
       bbb(2*nz_u+1)=fsg(id,iurb)*rl
       
       do i=1,nz_u
        bbb(2*nz_u+1)=bbb(2*nz_u+1)+sigma*fwg(i,id,iurb)*pb(i+1)*         &
                      (emw*(1.-pwin)*(tw(2*id-1,i)**4+tw(2*id,i)**4)+     &
                      emwin*pwin*(twlev(2*id-1,i)**4+twlev(2*id,i)**4))+  &
                      2.*fwg(i,id,iurb)*(1.-pb(i+1))*rl                  
       enddo
   

     
       call gaussj(aaa,2*nz_u+1,bbb,2*nz_um+1)

       do i=1,nz_u
        rlw(2*id-1,i)=bbb(i)
       enddo
       
       do i=nz_u+1,2*nz_u
        rlw(2*id,i-nz_u)=bbb(i)
       enddo
       
       rlg(id)=bbb(2*nz_u+1)
  
       return
       end subroutine long_rad
             



 
      subroutine short_rad_dd(iurb,nz_u,id,albw,                        & 
                           albg,rsdif,fwg,fww,fgw,fsw,fsg,rsg,rsw,pb)










      implicit none

  
      



      real albg                 
      real albw                 
      real rsdif                
      real fgw(nz_um,ndm,nurbm)       
      real fwg(nz_um,ndm,nurbm)       
      real fww(nz_um,nz_um,ndm,nurbm) 
      real fsg(ndm,nurbm)             
      real fsw(nz_um,ndm,nurbm)       
      integer id                
      integer iurb              
      integer nz_u              
      real pb(nz_um)          




      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     




      integer i,j
      real aaa(2*nz_um+1,2*nz_um+1)  
      real bbb(2*nz_um+1)            





      

       
         
        do i=1,nz_u
         do j=1,nz_u
            aaa(i,j)=0.
         enddo

         aaa(i,i)=1.

         do j=nz_u+1,2*nz_u
            aaa(i,j)=-albw*fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
         enddo

         aaa(i,2*nz_u+1)=-albg*fgw(i,id,iurb)
         bbb(i)=rsw(2*id-1,i)+fsw(i,id,iurb)*rsdif

      enddo
 

       do i=1+nz_u,2*nz_u
         do j=1,nz_u
            aaa(i,j)=-albw*fww(j,i-nz_u,id,iurb)*pb(j+1)
         enddo

         do j=1+nz_u,2*nz_u
            aaa(i,j)=0.
         enddo

        aaa(i,i)=1.
        aaa(i,2*nz_u+1)=-albg*fgw(i-nz_u,id,iurb)
        bbb(i)=rsw(2*id,i-nz_u)+fsw(i-nz_u,id,iurb)*rsdif

      enddo



      do j=1,nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j,id,iurb)*pb(j+1)
      enddo

      do j=nz_u+1,2*nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
      enddo

      aaa(2*nz_u+1,2*nz_u+1)=1.
      bbb(2*nz_u+1)=rsg(id)+fsg(id,iurb)*rsdif

      call gaussj(aaa,2*nz_u+1,bbb,2*nz_um+1)

      do i=1,nz_u
         rsw(2*id-1,i)=bbb(i)
      enddo

      do i=nz_u+1,2*nz_u
         rsw(2*id,i-nz_u)=bbb(i)
      enddo

      rsg(id)=bbb(2*nz_u+1)

       
      return
      end subroutine short_rad_dd





      
      subroutine gaussj(a,n,b,np)









      implicit none




      integer np
      real a(np,np)




      real b(np)




      integer nmax
      parameter (nmax=150)

      real big,dum
      integer i,icol,irow
      integer j,k,l,ll,n
      integer ipiv(nmax)
      real pivinv




       
      do j=1,n
         ipiv(j)=0.
      enddo
       
      do i=1,n
         big=0.
         do j=1,n
            if(ipiv(j).ne.1)then
               do k=1,n
                  if(ipiv(k).eq.0)then
                     if(abs(a(j,k)).ge.big)then
                        big=abs(a(j,k))
                        irow=j
                        icol=k
                     endif
                  elseif(ipiv(k).gt.1)then
                     CALL wrf_error_fatal3("<stdin>",3717,&
'singular matrix in gaussj')
                  endif
               enddo
            endif
         enddo
         
         ipiv(icol)=ipiv(icol)+1
         
         if(irow.ne.icol)then
            do l=1,n
               dum=a(irow,l)
               a(irow,l)=a(icol,l)
               a(icol,l)=dum
            enddo
            
            dum=b(irow)
            b(irow)=b(icol)
            b(icol)=dum
          
         endif
         
         if(a(icol,icol).eq.0) CALL wrf_error_fatal3("<stdin>",3739,&
'singular matrix in gaussj')
         
         pivinv=1./a(icol,icol)
         a(icol,icol)=1
         
         do l=1,n
            a(icol,l)=a(icol,l)*pivinv
         enddo
         
         b(icol)=b(icol)*pivinv
         
         do ll=1,n
            if(ll.ne.icol)then
               dum=a(ll,icol)
               a(ll,icol)=0.
               do l=1,n
                  a(ll,l)=a(ll,l)-a(icol,l)*dum
               enddo
               
               b(ll)=b(ll)-b(icol)*dum
               
            endif
         enddo
      enddo
      
      return
      end subroutine gaussj






      subroutine soil_moist(nz,dz,qv,dt,lf,d,k,rainbl,drain,irri_now)









      implicit none






      integer nz                
      real dt                   
      real lf                   
      real qv(nz)               
      real dz(nz)               
      real rainbl               
      real d(nz)                
      real k(nz)                
      real gr                   
      real drain
      real irri_now



  




      integer iz
      real a(nz,3)
      real alpha
      real c(nz)
      real cddz(nz+2)
      real dw     
      parameter(dw=1000.)




      alpha=rainbl/(dw*dt)+lf/latent/dw+irri_now/dw
      cddz(1)=0.
      do iz=2,nz
         cddz(iz)=2.*d(iz)/(dz(iz)+dz(iz-1))
      enddo
      do iz=1,4
         a(iz,1)=0.
         a(iz,2)=1.
         a(iz,3)=0.
         c(iz)=qv(iz)
      enddo
      do iz=6,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=qv(iz)+dt*(k(iz+1)-k(iz))/dz(iz)
      enddo
         a(5,1)=0.
         a(5,2)=1.+dt*(cddz(5+1))/dz(5)
         a(5,3)=-cddz(5+1)*dt/dz(5)
         c(5)=qv(5)+dt*(k(5+1)-drain)/dz(5)
      

      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=qv(nz)+dt*alpha/dz(nz)-dt*k(nz-1)/dz(nz)

      call invert(nz,a,c,qv)

      return
      end subroutine soil_moist
         


  



       
      subroutine soil_temp_veg(heflro,nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,lf,gf,pv_frac_roof,tpv)









      implicit none

     
                



      integer nz                
      real ala(nz)              
      real alb                  
      real cs(nz)               
      real dt                   
      real em                   
      real press                
      real rl                   
      real rs                   
      real sf                   
      real lf                   
      real temp(nz)             
      real dz(nz)               
      real heflro                
      real rs_eff
      real rl_eff
      real tpv
      real pv_frac_roof



      real gf                   
      real pt                   
      real rt                   




      integer iz
      real a(nz,3)
      real alpha
      real c(nz)
      real cddz(nz+2)
      real tsig




     if(pv_frac_roof.gt.0)then 
     rl_eff=(1-pv_frac_roof)*em*rl+em*sigma*tpv**4*pv_frac_roof
      rs_eff=(1.-pv_frac_roof)*rs
     else
      rl_eff=em*rl
      rs_eff=rs
     endif
      tsig=temp(nz)
      alpha=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4)+sf+lf
      cddz(1)=ala(1)/dz(1)
      do iz=2,nz
         cddz(iz)=2.*ala(iz)/(dz(iz)+dz(iz-1))
      enddo
      
      a(1,1)=0.
      a(1,2)=1.
      a(1,3)=0.
      c(1)=temp(1)-heflro*dt/dz(1)
      do iz=2,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)          
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=temp(iz)
      enddo          
      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=temp(nz)+dt*alpha/cs(nz)/dz(nz) 
      
      call invert(nz,a,c,temp)

      pt=temp(nz)*(press/1.e+5)**(-rcp_u)

      rt=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4.)
                        
      gf=(1.-alb)*rs_eff+rl_eff-em*sigma*(tsig**4.)+sf                                   
      return
      end subroutine soil_temp_veg
      


       
      subroutine soil_temp(nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,lf,gf)









      implicit none

     
                



      integer nz                
      real ala(nz)              
      real alb                  
      real cs(nz)               
      real dt                   
      real em                   
      real press                
      real rl                   
      real rs                   
      real sf                   
      real lf                   
      real temp(nz)             
      real dz(nz)               




      real gf                   
      real pt                   
      real rt                   




      integer iz
      real a(nz,3)
      real alpha
      real c(nz)
      real cddz(nz+2)
      real tsig




       
      tsig=temp(nz)
      alpha=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf+lf

      cddz(1)=ala(1)/dz(1)
      do iz=2,nz
         cddz(iz)=2.*ala(iz)/(dz(iz)+dz(iz-1))
      enddo
      
      a(1,1)=0.
      a(1,2)=1.
      a(1,3)=0.
      c(1)=temp(1)
      do iz=2,nz-1
         a(iz,1)=-cddz(iz)*dt/dz(iz)
         a(iz,2)=1.+dt*(cddz(iz)+cddz(iz+1))/dz(iz)          
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=temp(iz)
      enddo          
      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=temp(nz)+dt*alpha/cs(nz)/dz(nz) 

      
      call invert(nz,a,c,temp)

      pt=temp(nz)*(press/1.e+5)**(-rcp_u)

      rt=(1.-alb)*rs+em*rl-em*sigma*(tsig**4.)
                        
       gf=(1.-alb)*rs+em*rl-em*sigma*(tsig**4.)+sf                                   
      return
      end subroutine soil_temp
      





      subroutine invert(n,a,c,x)






      implicit none
                



       integer n
       real a(n,3)              
                                
                                
       real c(n)




       real x(n)    




       integer i




                     
       do i=n-1,1,-1                 
          c(i)=c(i)-a(i,3)*c(i+1)/a(i+1,2)
          a(i,2)=a(i,2)-a(i,3)*a(i+1,1)/a(i+1,2)
       enddo
       
       do i=2,n        
          c(i)=c(i)-a(i,1)*c(i-1)/a(i-1,2)
       enddo
        
       do i=1,n
          x(i)=c(i)/a(i,2)
       enddo

       return
       end subroutine invert
  



  
      subroutine flux_wall(ua,va,pt,da,ptw,ptwin,uva,vva,uvb,vvb,  &
                           sfw,sfwin,evb,drst,dt,cdrag)         
       




      implicit none   
         


      real drst                 
      real da                   
      real pt                   
      real ptw                  
      real ptwin                
      real ua                   
      real va                   
      real dt                   
      real cdrag







      real uva                  
      real uvb                  
      real vva                  
      real vvb                  
      real tva                  
      real tvb                  
      real evb                  
      real sfw                  
      real sfwin                



      real hc
      real hcwin
      real u_ort
      real vett






      vett=(ua**2+va**2)**.5         
         
      u_ort=abs((cos(drst)*ua-sin(drst)*va))
       
      uva=-cdrag*u_ort/2.*cos(drst)*cos(drst)
      vva=-cdrag*u_ort/2.*sin(drst)*sin(drst)
         
      uvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*va
      vvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*ua         

      if (vett.lt.4.88) then   
         hc=5.678*(1.09+0.23*(vett/0.3048))  
      else
         hc=5.678*0.53*((vett/0.3048)**0.78)
      endif 

      if (hc.gt.da*cp_u/dt)then
         hc=da*cp_u/dt
      endif

       if (vett.lt.4.88) then
          hcwin=5.678*(0.99+0.21*(vett/0.3048))
       else
          hcwin=5.678*0.50*((vett/0.3048)**0.78)
       endif

       if (hcwin.gt.da*cp_u/dt) then
           hcwin=da*cp_u/dt
       endif
         





      sfw=hc*(pt-ptw)
      sfwin=hcwin*(pt-ptwin)  
       
         
      evb=cdrag*(abs(u_ort)**3.)/2.
              
      return
      end subroutine flux_wall
         



      subroutine flux_flat_ground(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,sf,ehb,da,qv,pr,rsg,qg,resg,rsveg,f1,f2,f3,f4,fh,ric,utot,gr_type)
                                





      implicit none

      real dz                   
      real pt                   
      real pt0                  
      real ptg                  
      real ua                   
      real va                   
      real z0                   
      real da                   
      real qv                   
      real pr                   
      real rsg                  
      real qg(ng_u)         

     








      real uhb                  
      real vhb                  

      real tva                  
      real tvb                  
      real ehb                  
      real sf
      real lf




      real aa,ah
      real z0t
      real al
      real buu
      real c
      real fbuw
      real fbpt
      real fh
      real fm
      real ric
      real tstar
      real qstar
      real ustar
      real utot
      real wstar
      real zz
      real qvsg,qvs,es,esa,fbqq
      real b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)

      real f
      real f1
      real f2
      real f3
      real f4
      real ta          
      real tmp                  
      real rsveg       
      real resg
      real lai         
      real sdlim       
      parameter(sdlim=100.)
      real rsmin 
      real rsmax 
      real qw
      parameter(qw=0.06)
      real qref
      parameter(qref=0.37)  
      real hs
      parameter(hs=36.35)
 
      real dzg_u(ng_u)          

      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/
      
      real gx,iz,dzg_tot
      integer gr_type



      z0t=z0/10.
      if(gr_type.eq.1)then
      rsmin=40.
      rsmax=5000.
      lai=2.
      elseif(gr_type.eq.2)then
      rsmin=150.
      rsmax=5000.
      lai=3.
      endif

         
      utot=(ua**2.+va**2.)**.5
        
      




      zz=dz/2.
   








      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)
      ah=vk/log(zz/z0t)



      if(ric.gt.0)then
         fm=1/(1+0.5*b*ric)**2.
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1-b*ric/(1+c*(-ric)**.5)
         c=b*cm*aa*ah*(zz/z0t)**.5
         c=c*ch/cm
         fh=1-b*ric/(1+c*(-ric)**.5)
      endif
      
      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*ah*utot*(pt-ptg)*fh/rr
      tmp=ptg*(pr/p0)**(rcp_u)-273.15 
      es=6.11*(10.**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)
      

      f=0.55*rsg/sdlim*2./lai
      
      f1=(f+rsmin/rsmax)/(1.+f)

      ta=pt*(pr/p0)**(rcp_u)-273.15
      esa=6.11*(10**(ta*7.5/(237.7+ta)))
      qvs=0.62197*esa/(0.01*pr-0.378*esa)

      f2= 1./(1.+hs*(qvs-qv))
      f3=1.-0.0016*(25.-ta)**2.
      f4=0.
      dzg_tot=0.
      do iz=1,ng_u
       gx=(qg(iz)-qw)/(qref-qw)
       if (gx.gt.1)gx=1.
       if (gx.lt.0)gx=0.
       f4=f4+gx*dzg_u(iz)
       dzg_tot=dzg_tot+dzg_u(iz)
      enddo
      f4=f4/dzg_tot

      rsveg=min(rsmin/max(lai*f1*f2*f3*f4,1e-9),rsmax)
      resg= rr/(aa*aa*utot*fh)


      fbqq=-(qv-qvsg)/(resg+rsveg)
      
               
      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar

      al=(vk*g_u*tstar)/(pt*ustar*ustar)                      
      
      buu=-g_u/pt0*ustar*tstar
       
      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot 
      sf= ustar*tstar*da*cp_u   
      lf= ustar*qstar*da*latent
       

      ehb=buu

         
      return
      end subroutine flux_flat_ground



      subroutine flux_flat_roof(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,sf,lf,ehb,da,qv,pr,rsg,qr,resg,rsveg,f1,f2,f3,f4,gr_type,pv_frac_roof)






      implicit none

      real dz                   
      real pt                   
      real pt0                  
      real ptg                  
      real ua                   
      real va                   
      real z0                   
      real da                   
      real qv                   
      real pr                   
      real rsg                  
      real qr(ngr_u)         
      real pv_frac_roof
      real rs_eff








      real uhb                  
      real vhb                  

      real tva                  
      real tvb                  
      real ehb                  
      real sf
      real lf




      real aa,ah
      real al
      real buu
      real c
      real fbuw
      real fbpt
      real fh
      real fm
      real ric
      real tstar
      real qstar
      real ustar
      real utot
      real wstar
      real zz
      real z0t
      real qvsg,qvs,es,esa,fbqq
      real b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)

      real f
      real f1
      real f2
      real f3
      real f4
      real ta          
      real tmp                  
      real rsveg       
      real resg
      real lai         
      real sdlim       
      parameter(sdlim=100.)
      real rsmin
      real rsmax 
      real qw    
      parameter(qw=0.06) 
      real qref  
      parameter(qref=0.37)
      real hs
      parameter(hs=36.35)

      real dzgr_u(ngr_u)          

      data dzgr_u /0.1,0.003,0.06,0.003,0.05,0.04,0.02,0.0125,0.005,0.0025/

      real gx,iz,dzgr_tot
      integer gr_type




      z0t=z0/10.
      if(gr_type.eq.1)then
      rsmin=40.
      rsmax=5000.
      lai=2.
      elseif(gr_type.eq.2)then
      rsmin=150.
      rsmax=5000.
      lai=3.
      endif
     rs_eff=(1-pv_frac_roof)*rsg


      utot=(ua**2.+va**2.)**.5





      zz=dz/2.


      utot=max(utot,0.01)

      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))

      aa=vk/log(zz/z0)
      ah=vk/log(zz/z0t)

      if(ric.gt.0.)then
         fm=1./(1.+0.5*b*ric)**2.
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1.-b*ric/(1.+c*(-ric)**.5)
         c=b*cm*aa*ah*(zz/z0t)**.5
         c=c*ch/cm
         fh=1.-b*ric/(1+c*(-ric)**.5)
      endif

      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*ah*utot*(pt-ptg)*fh/rr
      tmp=ptg*(pr/p0)**(rcp_u)-273.15
      es=6.11*(10.**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)


      f=0.55*rs_eff/sdlim*2./lai

      f1=(f+rsmin/rsmax)/(1.+f)

      ta=pt*(pr/p0)**(rcp_u)-273.15
      esa=6.11*(10**(ta*7.5/(237.7+ta)))
      qvs=0.62197*esa/(0.01*pr-0.378*esa)

      f2= 1./(1.+hs*(qvs-qv))
      f3=1.-0.0016*(25.-ta)**2.
      f4=0.
      dzgr_tot=0.
      do iz=5,ngr_u
       gx=(qr(iz)-qw)/(qref-qw)
       if (gx.gt.1)gx=1.
       if (gx.lt.0)gx=0.
       f4=f4+gx*dzgr_u(iz)
       dzgr_tot=dzgr_tot+dzgr_u(iz)
      enddo
      f4=f4/dzgr_tot

      rsveg=min(rsmin/max(lai*f1*f2*f3*f4,1e-9),rsmax)


      resg= rr/(aa*aa*utot*fh)


      fbqq=-(qv-qvsg)/(resg+rsveg)

      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar

      al=(vk*g_u*tstar)/(pt*ustar*ustar)

      buu=-g_u/pt0*ustar*tstar

      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot
      sf= ustar*tstar*da*cp_u
      lf= ustar*qstar*da*latent

      ehb=buu
      end subroutine flux_flat_roof
      





      subroutine flux_flat(dz,z0,ua,va,pt,pt0,ptg,qv,                   &
                          uhb,vhb,sf,lf,ehb,da,pr)
                                





      implicit none
      real pr
      real dz                   
      real pt                   
      real pt0                  
      real ptg                  
      real ua                   
      real va                   
      real z0                   
      real da                   
      real qv







      real uhb                  
      real vhb                  

      real tva                  
      real tvb                  
      real ehb                  
      real sf
      real lf
       



      real aa
      real al
      real buu
      real c
      real fbuw
      real fbpt
      real fh
      real fm
      real ric
      real tstar
      real ustar
      real qstar
      real utot
      real wstar
      real zz
      real qvsg,qvs,es,esa,fbqq,tmp,resg
      real b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)







         
      utot=(ua**2+va**2)**.5
        
      




      zz=dz/2.
   

      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)


     
      tmp=ptg*(pr/(1.e+5))**(rcp_u)-273.15 
      es=6.11*(10**(tmp*7.5/(237.7+tmp)))
      qvsg=0.62197*es/(0.01*pr-0.378*es)





      if(ric.gt.0.)then
         fm=1./(1.+0.5*b*ric)**2
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1.-b*ric/(1.+c*(-ric)**.5)
         c=c*ch/cm
         fh=1.-b*ric/(1.+c*(-ric)**.5)
      endif

      resg= rr/(aa*aa*utot*fh)
      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*aa*utot*(pt-ptg)*fh/rr
      fbqq=-(qv-qvsg)/(resg)
               
      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar
      qstar=-fbqq/ustar
      al=(vk*g_u*tstar)/(pt*ustar*ustar)                      
      
      buu=-g_u/pt0*ustar*tstar
       
      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot 
      sf= ustar*tstar*da*cp_u  
      lf= ustar*qstar*da*latent 
      ehb=buu

         
      return
      end subroutine flux_flat




      subroutine icBEP (nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)

      implicit none


      integer nd_u(nurbm)     
      real h_b(nz_um,nurbm)   
      real d_b(nz_um,nurbm)   




      real ss_u(nz_um,nurbm)     
      real pb_u(nz_um,nurbm)     


      integer nz_u(nurbm)     
      real z_u(nz_um)       






      integer iz_u,id,ilu,iurb

      real dtot
      real hbmax







 
      nz_u=0
      z_u=0.
      ss_u=0.
      pb_u=0.



      z_u(1)=0.

      do iz_u=1,nz_um-1
         z_u(iz_u+1)=z_u(iz_u)+dz_u
      enddo



      do iurb=1,nurbm
         dtot=0.
         do ilu=1,nz_um
            dtot=dtot+d_b(ilu,iurb)
         enddo
         do ilu=1,nz_um
            d_b(ilu,iurb)=d_b(ilu,iurb)/dtot
         enddo
      enddo



      do iurb=1,nurbm
         hbmax=0.
         nz_u(iurb)=0
         do ilu=1,nz_um
            if(h_b(ilu,iurb).gt.hbmax)hbmax=h_b(ilu,iurb)
         enddo

         do iz_u=1,nz_um-1
            if(z_u(iz_u+1).gt.hbmax)go to 10
         enddo

 10      continue
          nz_u(iurb)=iz_u+1

         do id=1,nd_u(iurb)

            do iz_u=1,nz_u(iurb)
               ss_u(iz_u,iurb)=0.
               do ilu=1,nz_um
                  if(z_u(iz_u).le.h_b(ilu,iurb)                      &
                    .and.z_u(iz_u+1).gt.h_b(ilu,iurb))then
                        ss_u(iz_u,iurb)=ss_u(iz_u,iurb)+d_b(ilu,iurb)
                  endif
               enddo
            enddo

            pb_u(1,iurb)=1.
            do iz_u=1,nz_u(iurb)
               pb_u(iz_u+1,iurb)=max(0.,pb_u(iz_u,iurb)-ss_u(iz_u,iurb))
            enddo

         enddo
      end do


      return
      end subroutine icBEP



    

      subroutine view_factors(iurb,nz_u,id,dxy,z,ws,fww,fwg,fgw,fsg,fsw,fws) 
     
      implicit none

 





      integer iurb            
      integer nz_u            
      integer id              
      real ws                 
      real z(nz_um)         
      real dxy                










      real fww(nz_um,nz_um,ndm,nurbm)            
      real fwg(nz_um,ndm,nurbm)                  
      real fgw(nz_um,ndm,nurbm)                  
      real fsw(nz_um,ndm,nurbm)                  
      real fws(nz_um,ndm,nurbm)                  
      real fsg(ndm,nurbm)                        






      integer jz,iz

      real hut
      real f1,f2,f12,f23,f123,ftot
      real fprl,fnrm
      real a1,a2,a3,a4,a12,a23,a123




        
      hut=z(nz_u+1)
        
      do jz=1,nz_u      
      

       
         do iz=1,nz_u
     
            call fprls (fprl,dxy,abs(z(jz+1)-z(iz  )),ws)
            f123=fprl
            call fprls (fprl,dxy,abs(z(jz+1)-z(iz+1)),ws)
            f23=fprl
            call fprls (fprl,dxy,abs(z(jz  )-z(iz  )),ws)
            f12=fprl
            call fprls (fprl,dxy,abs(z(jz  )-z(iz+1)),ws)
            f2 = fprl
       
            a123=dxy*(abs(z(jz+1)-z(iz  )))
            a12 =dxy*(abs(z(jz  )-z(iz  )))
            a23 =dxy*(abs(z(jz+1)-z(iz+1)))
            a1  =dxy*(abs(z(iz+1)-z(iz  )))
            a2  =dxy*(abs(z(jz  )-z(iz+1)))
            a3  =dxy*(abs(z(jz+1)-z(jz  )))
       
            ftot=0.5*(a123*f123-a23*f23-a12*f12+a2*f2)/a1
       
            fww(iz,jz,id,iurb)=ftot*a1/a3

         enddo 


       
         call fnrms (fnrm,z(jz+1),dxy,ws)
         f12=fnrm
         call fnrms (fnrm,z(jz)  ,dxy,ws)
         f1=fnrm
       
         a1 = ws*dxy
         
         a12= ws*dxy
       
         a4=(z(jz+1)-z(jz))*dxy
       
         ftot=(a12*f12-a12*f1)/a1
                    
         fgw(jz,id,iurb)=ftot*a1/a4
     

     
         call fnrms(fnrm,hut-z(jz)  ,dxy,ws)
         f12 = fnrm
         call fnrms (fnrm,hut-z(jz+1),dxy,ws)
         f1 =fnrm
       
         a1 = ws*dxy
       
         a12= ws*dxy
              
         a4 = (z(jz+1)-z(jz))*dxy
       
         ftot=(a12*f12-a12*f1)/a1
        
         fsw(jz,id,iurb)=ftot*a1/a4       
      
      enddo


       do iz=1,nz_u
       call fnrms(fnrm,ws,dxy,hut-z(iz))
       f12=fnrm
       call fnrms(fnrm,ws,dxy,hut-z(iz+1))
       f1=fnrm
       a1 = (z(iz+1)-z(iz))*dxy
       a2 = (hut-z(iz+1))*dxy
       a12= (hut-z(iz))*dxy
       a4 = ws*dxy
       ftot=(a12*f12-a2*f1)/a1
       fws(iz,id,iurb)=ftot*a1/a4 
 
      enddo



       do iz=1,nz_u


      
         call fnrms (fnrm,ws,dxy,z(iz+1))
         f12=fnrm
         call fnrms (fnrm,ws,dxy,z(iz  ))
         f1 =fnrm
         
         a1= (z(iz+1)-z(iz) )*dxy
       
         a2 = z(iz)*dxy
         a12= z(iz+1)*dxy
         a4 = ws*dxy

         ftot=(a12*f12-a2*f1)/a1        
                    
         fwg(iz,id,iurb)=ftot*a1/a4
        
      enddo


      
      call fprls (fprl,dxy,ws,hut)
      fsg(id,iurb)=fprl

      return
      end subroutine view_factors





      SUBROUTINE fprls (fprl,a,b,c)

      implicit none

     
            
      real a,b,c
      real x,y
      real fprl


      x=a/c
      y=b/c
      
      if(a.eq.0.or.b.eq.0.)then
       fprl=0.
      else
       fprl=log( ( (1.+x**2)*(1.+y**2)/(1.+x**2+y**2) )**.5)+  &
           y*((1.+x**2)**.5)*atan(y/((1.+x**2)**.5))+          &  
           x*((1.+y**2)**.5)*atan(x/((1.+y**2)**.5))-          &   
           y*atan(y)-x*atan(x)
       fprl=fprl*2./(pi*x*y)
      endif
      
      return
      end subroutine fprls




      SUBROUTINE fnrms (fnrm,a,b,c)

      implicit none



      real a,b,c
      real x,y,z,a1,a2,a3,a4,a5,a6
      real fnrm
      
      x=a/b
      y=c/b
      z=x**2+y**2
      
      if(y.eq.0.or.x.eq.0)then
       fnrm=0.
      else
       a1=log( (1.+x*x)*(1.+y*y)/(1.+z) )
       a2=y*y*log(y*y*(1.+z)/z/(1.+y*y) )
       a3=x*x*log(x*x*(1.+z)/z/(1.+x*x) )
       a4=y*atan(1./y)
       a5=x*atan(1./x)
       a6=sqrt(z)*atan(1./sqrt(z))
       fnrm=0.25*(a1+a2+a3)+a4+a5-a6
       fnrm=fnrm/(pi*y)
      endif
      
      return
      end subroutine fnrms
  
     
        SUBROUTINE init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
        twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,albwin_u,emg_u,emw_u,&
        emr_u,emwind_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,  &
        cop_u,pwin_u,beta_u,sw_cond_u,time_on_u,time_off_u,targtemp_u,    &
        bldac_frc_u,cooled_frc_u,                                         &
        gaptemp_u, targhum_u,gaphum_u,perflo_u,                           &
        gr_frac_roof_u,pv_frac_roof_u,                   &
        hsesf_u,hsequip,irho,gr_flag_u,gr_type_u)

 


      implicit none
      integer iurb            

      real alag_u(nurbm)      
      real alaw_u(nurbm)      
      real alar_u(nurbm)      
      real csg_u(nurbm)       
      real csw_u(nurbm)       
      real csr_u(nurbm)       
      real twini_u(nurbm)     
      real trini_u(nurbm)     
      real tgini_u(nurbm)     


      real albg_u(nurbm)      
      real albw_u(nurbm)      
      real albr_u(nurbm)      
      real albwin_u(nurbm)    
      real emg_u(nurbm)       
      real emw_u(nurbm)       
      real emr_u(nurbm)       
      real emwind_u(nurbm)    


      real z0g_u(nurbm)       
      real z0r_u(nurbm)       


      integer nd_u(nurbm)     

      real strd_u(ndm,nurbm)  
      real drst_u(ndm,nurbm)  
      real ws_u(ndm,nurbm)    
      real bs_u(ndm,nurbm)    
      real h_b(nz_um,nurbm)   
      real d_b(nz_um,nurbm)   

      integer i,iu
      integer nurb 
        real, intent(out) :: bldac_frc_u(nurbm)
      real, intent(out) :: cooled_frc_u(nurbm)
      real, intent(out) :: cop_u(nurbm)
      real, intent(out) :: pwin_u(nurbm)
      real, intent(out) :: beta_u(nurbm)
      integer, intent(out) :: sw_cond_u(nurbm)
      real, intent(out) :: time_on_u(nurbm)
      real, intent(out) :: time_off_u(nurbm)
      real, intent(out) :: targtemp_u(nurbm)
      real, intent(out) :: gaptemp_u(nurbm)
      real, intent(out) :: targhum_u(nurbm)
      real, intent(out) :: gaphum_u(nurbm)
      real, intent(out) :: perflo_u(nurbm)
      real, intent(out) :: gr_frac_roof_u(nurbm)
      real, intent(out) :: pv_frac_roof_u(nurbm)
      real, intent(out) :: hsesf_u(nurbm)
      real, intent(out) :: hsequip(24)
      real, intent(out) :: irho(24)
      integer, intent(out) :: gr_flag_u,gr_type_u



     
       h_b=0.
       d_b=0.

       nurb=ICATE
       do iu=1,nurb                         
          nd_u(iu)=0
       enddo

       csw_u=CAPB_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       csr_u=CAPR_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       csg_u=CAPG_TBL / (( 1.0 / 4.1868 ) * 1.E-6)
       do i=1,icate
         alaw_u(i)=AKSB_TBL(i) / csw_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
         alar_u(i)=AKSR_TBL(i) / csr_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
         alag_u(i)=AKSG_TBL(i) / csg_u(i) / (( 1.0 / 4.1868 ) * 1.E-2)
       enddo
       twini_u=TBLEND_TBL
       trini_u=TRLEND_TBL
       tgini_u=TGLEND_TBL
       albw_u=ALBB_TBL
       albr_u=ALBR_TBL
       albg_u=ALBG_TBL
       emw_u=EPSB_TBL
       emr_u=EPSR_TBL
       emg_u=EPSG_TBL
       z0r_u=Z0R_TBL
       z0g_u=Z0G_TBL
       nd_u=NUMDIR_TBL


       bldac_frc_u = bldac_frc_tbl
       cooled_frc_u = cooled_frc_tbl
       cop_u = cop_tbl
       pwin_u = pwin_tbl
       beta_u = beta_tbl
       sw_cond_u = sw_cond_tbl
       time_on_u = time_on_tbl
       time_off_u = time_off_tbl
       targtemp_u = targtemp_tbl
       gaptemp_u = gaptemp_tbl
       targhum_u = targhum_tbl
       gaphum_u = gaphum_tbl
       perflo_u = perflo_tbl
       gr_frac_roof_u =gr_frac_roof_tbl
       gr_flag_u=gr_flag_tbl
       pv_frac_roof_u = pv_frac_roof_tbl
       hsesf_u = hsesf_tbl
       hsequip = hsequip_tbl
       irho=irho_tbl
       gr_type_u=gr_type_tbl
       do iu=1,icate
              if(ndm.lt.nd_u(iu))then
                write(*,*)'ndm too small in module_sf_bep_bem, please increase to at least ', nd_u(iu)
                write(*,*)'remember also that num_urban_layers should be equal or greater than nz_um*ndm*nwr-u!'
                stop
              endif
         do i=1,nd_u(iu)
           drst_u(i,iu)=STREET_DIRECTION_TBL(i,iu) * pi/180.
           ws_u(i,iu)=STREET_WIDTH_TBL(i,iu)
           bs_u(i,iu)=BUILDING_WIDTH_TBL(i,iu)
         enddo
       enddo
       do iu=1,ICATE
          if(nz_um.lt.numhgt_tbl(iu)+3)then
              write(*,*)'nz_um too small in module_sf_bep, please increase to at least ',numhgt_tbl(iu)+3
              write(*,*)'remember also that num_urban_layers should be equal or greater than nz_um*ndm*nwr-u!'
              stop
          endif
         do i=1,NUMHGT_TBL(iu)
           h_b(i,iu)=HEIGHT_BIN_TBL(i,iu)
           d_b(i,iu)=HPERCENT_BIN_TBL(i,iu)
         enddo
       enddo

       do i=1,ndm
        do iu=1,nurbm
         strd_u(i,iu)=100000.
        enddo
       enddo

       do iu=1,nurb  
          emwind_u(iu)=0.9                       
          call albwindow(albwin_u(iu))  
       enddo
       
       return
       end subroutine init_para





       subroutine upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                &
                       tg_av,emg_u,albg_u,rlg,rsg,sfg,lfg,                   & 
                       tw,emw_u,albw_u,rlw,rsw,sfw,                   & 
                       tr_av,emr_u,albr_u,emwind,albwind,twlev,pwin,     &
                       sfwind,rld,rs, sfr,sfrv,lfr,lfrv,                            & 
                       rs_abs,rl_up,emiss,grdflx_urb,gr_frac_roof,tpvlev,pv_frac_roof)




      implicit none




      real rsw(2*ndm,nz_um)        
      real rlw(2*ndm,nz_um)         
      real rsg(ndm)                   
      real rlg(ndm)                   
      real rs                        
      real sfw(2*ndm,nz_um)      
      real sfg(ndm)              
      real lfg(ndm)
      real sfr(ndm,nz_um)      
      real lfr(ndm,nz_um)
      real lfrv(ndm,nz_um)
      real sfrv(ndm,nz_um)
      real gr_frac_roof
      real rld                        
      real albg_u                    
      real albw_u                    
      real albr_u                    
      real ws(ndm)                        
      real bs(ndm)
                        
      real pb(nz_um)                
      integer nzu
      real ss(nz_um)                
      real sigma                       
      real emg_u                       
      real emw_u                       
      real emr_u                       
      real tw(2*ndm,nz_um)  
      real tr_av(ndm,nz_um)  
      real tpvlev(ndm,nz_um)
      real pv_frac_roof
      real tg_av(ndm)          
      integer id 
      integer ndu 



      real emwind               
      real albwind              
      real twlev(2*ndm,nz_um)   
      real pwin                 
      real gflwin               
      real sfwind(2*ndm,nz_um)  


      real rs_abs  
      real rl_up   
      real emiss 
      real grdflx_urb 

      integer iz,iw
      real rl_inc,rl_emit
      real gfl
      integer ix,iy,iwrong

         iwrong=1
      do iz=1,nzu+1
      do id=1,ndu
      if(tr_av(id,iz).lt.100.)then
              write(203,*) tr_av(id,iz)
              write(*,*)'in upward_rad ',iz,id,iw,tr_av(id,iz)
              iwrong=0
      endif     
      enddo
      enddo
           if(iwrong.eq.0)stop

      rl_up=0.
 
      rs_abs=0.
      rl_inc=0.
      emiss=0.
      rl_emit=0.
      grdflx_urb=0.
      do id=1,ndu          
       rl_emit=rl_emit-( emg_u*sigma*(tg_av(id)**4.)+(1-emg_u)*rlg(id))*ws(id)/(ws(id)+bs(id))/ndu
       rl_inc=rl_inc+rlg(id)*ws(id)/(ws(id)+bs(id))/ndu       
       rs_abs=rs_abs+(1.-albg_u)*rsg(id)*ws(id)/(ws(id)+bs(id))/ndu
         gfl=(1.-albg_u)*rsg(id)+emg_u*rlg(id)-emg_u*sigma*(tg_av(id)**4.)+sfg(id)+lfg(id)
         grdflx_urb=grdflx_urb-gfl*ws(id)/(ws(id)+bs(id))/ndu  
 
         do iz=2,nzu
            rl_emit=rl_emit-(emr_u*sigma*((1.-pv_frac_roof)*tr_av(id,iz)**4.+pv_frac_roof*tpvlev(id,iz)**4)+ &
             (1-emr_u)*rld)*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
            rl_inc=rl_inc+(1-pv_frac_roof*rld+pv_frac_roof*emr_u*sigma*tpvlev(id,iz)**4)*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
            rs_abs=rs_abs+((1.-albr_u)*rs*(1.-pv_frac_roof))*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
            gfl=(1.-albr_u)*rs*(1-pv_frac_roof)+emr_u*rld*((1-pv_frac_roof)+pv_frac_roof*emr_u*sigma*tpvlev(id,iz)**4) &
                -emr_u*sigma*(tr_av(id,iz)**4.)+(1-gr_frac_roof)*sfr(id,iz)+(sfrv(id,iz)+lfrv(id,iz))*gr_frac_roof+(1.-gr_frac_roof)*lfr(id,iz)
            grdflx_urb=grdflx_urb-gfl*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
         enddo
           
         do iz=1,nzu 
           
            rl_emit=rl_emit-(emw_u*(1.-pwin)*sigma*(tw(2*id-1,iz)**4.+tw(2*id,iz)**4.)+ &
                            (emwind*pwin*sigma*(twlev(2*id-1,iz)**4.+twlev(2*id,iz)**4.))+ &
                ((1.-emw_u)*(1.-pwin)+pwin*(1.-emwind))*(rlw(2*id-1,iz)+rlw(2*id,iz)))* &
                            dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

            rl_inc=rl_inc+((rlw(2*id-1,iz)+rlw(2*id,iz)))*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

            rs_abs=rs_abs+(((1.-albw_u)*(1.-pwin)+(1.-albwind)*pwin)*(rsw(2*id-1,iz)+rsw(2*id,iz)))*&
                          dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu 

            gfl=(1.-albw_u)*(rsw(2*id-1,iz)+rsw(2*id,iz)) +emw_u*( rlw(2*id-1,iz)+rlw(2*id,iz) )   &
             -emw_u*sigma*( tw(2*id-1,iz)**4.+tw(2*id,iz)**4. )+(sfw(2*id-1,iz)+sfw(2*id,iz))   

            gflwin=(1.-albwind)*(rsw(2*id-1,iz)+rsw(2*id,iz)) +emwind*(rlw(2*id-1,iz)+rlw(2*id,iz))   &
             -emwind*sigma*( twlev(2*id-1,iz)**4.+twlev(2*id,iz)**4.)+(sfwind(2*id-1,iz)+sfwind(2*id,iz)) 
               
           
            grdflx_urb=grdflx_urb-(gfl*(1.-pwin)+pwin*gflwin)*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu

         enddo
          
      enddo
        emiss=(emg_u+emw_u+emr_u)/3.
        rl_up=(rl_inc+rl_emit)-rld
       
         
      return

      END SUBROUTINE upward_rad






         subroutine albwindow(albwin)
		

	 implicit none










        


         real albwin	        


	 real a,b,c		
	 real alfa,delta,gama	
	 real g0	        
                                
         real asup,ainf
	 real fonc



         
         real epsilon              
         parameter (epsilon=1.e-07) 
         real n1,n2                
         parameter(n1=1.,n2=1.5)
         integer intg,k

         if (q_num.eq.0) then
           write(*,*) 'Category parameter of the windows no valid'
           stop
         endif

         g0=4.*n1*n2/((n1+n2)*(n1+n2))
	 a=8.
	 b=0.25/q_num
         c=1.-a-b	
	 alfa =5.2 + (0.7*q_num)
	 delta =2.
	 gama =(5.26+0.06*p_num)+(0.73+0.04*p_num)*q_num

         intg=1



100      asup=0.
         ainf=0.

         do k=1,intg
          call foncs(fonc,(pi*k/intg),a,b,c,alfa,delta,gama)
          asup=asup+(pi/intg)*fonc
         enddo

         intg=intg+1

         do k=1,intg
          call foncs(fonc,(pi*k/intg),a,b,c,alfa,delta,gama)
          ainf=ainf+(pi/intg)*fonc
         enddo
	 
         if(abs(asup-ainf).lt.epsilon) then
           albwin=1-g0+(g0/2.)*asup
         else
           goto 100
         endif
        

	return
	end subroutine albwindow



        subroutine foncs(fonc,x,aa,bb,cc,alf,delt,gam)

        implicit none

        real x,aa,bb,cc
        real alf,delt,gam
        real fonc
  
        fonc=(((aa*(x**alf))/(pi**alf))+   &
             ((bb*(x**delt))/(pi**delt))+  &
             ((cc*(x**gam))/(pi**gam)))*sin(x)
        
        return
	end subroutine foncs



      subroutine icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,             &
                          fws_u,fsg_u,ndu,strd,ws,nzu,z_u)                               

      implicit none       
        

      integer ndu     
      integer iurb

      real strd(ndm)        
      real ws(ndm)          


      integer nzu          
      real z_u(nz_um)       








      real fww_u(nz_um,nz_um,ndm,nurbm)         
      real fwg_u(nz_um,ndm,nurbm)               
      real fgw_u(nz_um,ndm,nurbm)               
      real fsw_u(nz_um,ndm,nurbm)               
      real fws_u(nz_um,ndm,nurbm)               
      real fsg_u(ndm,nurbm)                     





      integer id







      fww_u=0.
      fwg_u=0.
      fgw_u=0.
      fsw_u=0.
      fws_u=0.
      fsg_u=0.
      
      do id=1,ndu

            call view_factors(iurb,nzu,id,strd(id),z_u,ws(id),  &    
                              fww_u,fwg_u,fgw_u,fsg_u,fsw_u,fws_u) 
      
      enddo               
      return       
      end subroutine icBEP_XY


      subroutine icBEPHI_XY(iurb,hb_u,hi_urb1D,ss_u,pb_u,nzu,z_u)

      implicit none   





      real hi_urb1D(nz_um)    
      integer iurb            



      real z_u(nz_um)         




      real ss_u(nz_um,nurbm)  
      real pb_u(nz_um)        



      integer nzu                




      real hb_u(nz_um)        
      integer iz_u,id,ilu

      real dtot
      real hbmax





      
      nzu=0
      ss_u=0.
      pb_u=0.
      


         dtot=0.
         hb_u=0.

         do ilu=1,nz_um
            dtot=dtot+hi_urb1D(ilu)
         enddo

         do ilu=1,nz_um
            if (hi_urb1D(ilu)<0.) then

               go to 20
            endif
         enddo

         if (dtot.gt.0.) then
            continue
         else

            go to 20
         endif

         do ilu=1,nz_um
            hi_urb1D(ilu)=hi_urb1D(ilu)/dtot
         enddo
         
         hb_u(1)=dz_u   
         do ilu=2,nz_um
            hb_u(ilu)=dz_u+hb_u(ilu-1)
         enddo
           


      
            
         hbmax=0.
       
         do ilu=1,nz_um
            if (hi_urb1D(ilu)>0.and.hi_urb1D(ilu)<=1.) then
                hbmax=hb_u(ilu)
            endif
         enddo
         
         do iz_u=1,nz_um-1
            if(z_u(iz_u+1).gt.hbmax)go to 10
         enddo

10       continue 
        
         nzu=iz_u+1
      
         if ((nzu+1).gt.nz_um) then 
             write(*,*) 'error, nz_um has to be increased to at least',nzu+1
             stop
         endif

            do iz_u=1,nzu
               ss_u(iz_u,iurb)=0.
               do ilu=1,nz_um
                  if(z_u(iz_u).le.hb_u(ilu)                      &    
                    .and.z_u(iz_u+1).gt.hb_u(ilu))then            
                        ss_u(iz_u,iurb)=ss_u(iz_u,iurb)+hi_urb1D(ilu)
                  endif 
               enddo
            enddo

            pb_u(1)=1.
            do iz_u=1,nzu
               pb_u(iz_u+1)=max(0.,pb_u(iz_u)-ss_u(iz_u,iurb))
            enddo

20     continue    
       return
       end subroutine icBEPHI_XY


END MODULE module_sf_bep_bem




      FUNCTION bep_bem_nurbm () RESULT (bep_bem_val_nurbm)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nurbm
         bep_bem_val_nurbm = nurbm
      END FUNCTION bep_bem_nurbm

      FUNCTION bep_bem_ndm () RESULT (bep_bem_val_ndm)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ndm
         bep_bem_val_ndm = ndm
      END FUNCTION bep_bem_ndm

      FUNCTION bep_bem_nz_um () RESULT (bep_bem_val_nz_um)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nz_um
         bep_bem_val_nz_um = nz_um
      END FUNCTION bep_bem_nz_um

      FUNCTION bep_bem_ng_u () RESULT (bep_bem_val_ng_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ng_u
         bep_bem_val_ng_u = ng_u
      END FUNCTION bep_bem_ng_u

      FUNCTION bep_bem_nwr_u () RESULT (bep_bem_val_nwr_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nwr_u
         bep_bem_val_nwr_u = nwr_u
      END FUNCTION bep_bem_nwr_u

      FUNCTION bep_bem_nf_u () RESULT (bep_bem_val_nf_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nf_u
         bep_bem_val_nf_u = nf_u
      END FUNCTION bep_bem_nf_u


      FUNCTION bep_bem_ngb_u () RESULT (bep_bem_val_ngb_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ngb_u
         bep_bem_val_ngb_u = ngb_u
      END FUNCTION bep_bem_ngb_u

      FUNCTION bep_bem_nbui_max () RESULT (bep_bem_val_nbui_max)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_nbui_max
         bep_bem_val_nbui_max = nbui_max
      END FUNCTION bep_bem_nbui_max

 
   FUNCTION bep_bem_ngr_u () RESULT (bep_bem_val_ngr_u)
         USE module_sf_bep_bem
         IMPLICIT NONE
         INTEGER :: bep_bem_val_ngr_u
         bep_bem_val_ngr_u = ngr_u
      END FUNCTION bep_bem_ngr_u

