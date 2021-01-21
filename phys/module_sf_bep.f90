MODULE module_sf_bep
use module_wrf_error

 USE module_sf_urban









      integer nurbm           
      parameter (nurbm=11)

      integer ndm             
      parameter (ndm=2)

      integer nz_um           
      parameter(nz_um=18)

      integer ng_u            
      parameter (ng_u=10)
      integer nwr_u            
      parameter (nwr_u=10)

      real dz_u                 
      parameter (dz_u=5.)






           
      real vk                 
      real g_u                  
      real pi                 
      real r                  
      real cp_u                 
      real rcp_u                
      real sigma              
      real p0                 
      real cdrag              
      parameter(vk=0.40,g_u=9.81,pi=3.141592653,r=287.,cp_u=1004.)        
      parameter(rcp_u=r/cp_u,sigma=5.67e-08,p0=1.e+5,cdrag=0.4)






   CONTAINS
 
      subroutine BEP(FRC_URB2D,UTYPE_URB2D,itimestep,dz8w,dt,u_phy,v_phy,      &
                      th_phy,rho,p_phy,swdown,glw,                    &
                      gmt,julday,xlong,xlat,                          &
                      declin_urb,cosz_urb2d,omg_urb2d,                &
                      num_urban_ndm,  urban_map_zrd,  urban_map_zwd, urban_map_gd, &
                       urban_map_zd,  urban_map_zdf,   urban_map_bd, urban_map_wd, &
                      urban_map_gbd,  urban_map_fbd,                               &
                       num_urban_hi,                                               &
                      trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,        &
                      sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,      &
                      lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,           &
                      a_u,a_v,a_t,a_e,b_u,b_v,                        &
                      b_t,b_e,b_q,dlg,dl_u,sf,vl,                     &
                      rl_up,rs_abs,emiss,grdflx_urb,                  &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
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
   INTEGER, INTENT(IN  ) :: num_urban_ndm
   INTEGER, INTENT(IN  ) :: urban_map_zrd
   INTEGER, INTENT(IN  ) :: urban_map_zwd
   INTEGER, INTENT(IN  ) :: urban_map_gd
   INTEGER, INTENT(IN  ) :: urban_map_zd
   INTEGER, INTENT(IN  ) :: urban_map_zdf
   INTEGER, INTENT(IN  ) :: urban_map_bd
   INTEGER, INTENT(IN  ) :: urban_map_wd
   INTEGER, INTENT(IN  ) :: urban_map_gbd
   INTEGER, INTENT(IN  ) :: urban_map_fbd
   INTEGER, INTENT(IN  ) :: num_urban_hi
   REAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: sfg_urb3d
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
      real hb_u(nz_um)                        
      real ss_urb(nz_um)  
      real pb_urb(nz_um)  
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
      real csr(nwr_u)                         
      real csw(nwr_u)                         
      real alag(ng_u)                         
      real alaw(nwr_u)                        
      real alar(nwr_u)                        




      real albg_u(nurbm)                      
      real albw_u(nurbm)                      
      real albr_u(nurbm)                      
      real emg_u(nurbm)                       
      real emw_u(nurbm)                       
      real emr_u(nurbm)                       



      real fww_u(nz_um,nz_um,ndm,nurbm)         
      real fwg_u(nz_um,ndm,nurbm)               
      real fgw_u(nz_um,ndm,nurbm)               
      real fsw_u(nz_um,ndm,nurbm)               
      real fws_u(nz_um,ndm,nurbm)               
      real fsg_u(ndm,nurbm)                     


      real z0g_u(nurbm)         
      real z0r_u(nurbm)         


      real z0(ndm,nz_um)           


      integer nd_u(nurbm)       
      real strd_u(ndm,nurbm)    
      real drst_u(ndm,nurbm)    
      real ws_u(ndm,nurbm)      
      real bs_u(ndm,nurbm)      
      real h_b(nz_um,nurbm)     
      real d_b(nz_um,nurbm)     
      real ss_u(nz_um,nurbm)  
      real pb_u(nz_um,nurbm)  



      real bs(ndm)                 
      real ws(ndm)                 
      real drst(ndm)               
      real strd(ndm)               
      real ss(nz_um)               
      real pb(nz_um)               



      integer nz_u(nurbm)       
      
      real z_u(nz_um)         



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


      real tw1D(2*ndm,nz_um,nwr_u)  
      real tg1D(ndm,ng_u)          
      real tr1D(ndm,nz_um,nwr_u)  
      real sfw1D(2*ndm,nz_um)      
      real sfg1D(ndm)              
      real sfr1D(ndm,nz_um)      
      real sf1D(kms:kme)              
      real vl1D(kms:kme)                
      real a_u1D(kms:kme)               
      real a_v1D(kms:kme)               
      real a_t1D(kms:kme)               
      real a_e1D(kms:kme)               
      real b_u1D(kms:kme)               
      real b_v1D(kms:kme)               
      real b_t1D(kms:kme)               
      real b_e1D(kms:kme)               
      real dlg1D(kms:kme)               
      real dl_u1D(kms:kme)              
      real time_bep

      integer ind_zwd(nz_um,nwr_u,ndm)
      integer ind_gd(ng_u,ndm)
      integer ind_zd(nz_um,ndm)

      integer ix,iy,iz,iurb,id,iz_u,iw,ig,ir,ix1,iy1,k
      integer it, nint
      integer iii
      real time_h,tempo
      logical first
      character(len=80) :: text
      data first/.true./
      save first,time_bep 

      save alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,                      &
           albg_u,albw_u,albr_u,emg_u,emw_u,emr_u,                      &
           z0g_u,z0r_u, nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b,ss_u,pb_u, &
           nz_u,z_u 












      if(urban_map_zrd.lt.nz_um*ndm*nwr_u)then
        write(*,*)'urban_map_zrd too small, please increase to at least ', nz_um*ndm*nwr_u
        stop
      endif
      iii=0
      do iz_u=1,nz_um
      do iw=1,nwr_u
      do id=1,ndm
       iii=iii+1
       ind_zwd(iz_u,iw,id)=iii
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
      do iz_u=1,nz_um
      do id=1,ndm
       iii=iii+1
       ind_zd(iz_u,id)=iii
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
       do iz_u=1,num_urban_hi
          hi_urb(ix,iz_u,iy)= hi_urb2d(ix,iz_u,iy)
       enddo 
      enddo
      enddo


      if (first) then                           
         
         call init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,emg_u,emw_u,&
                emr_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b)


             
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

           call icBEPHI_XY(hb_u,hi_urb1D,ss_urb,pb_urb,    &
                           nz_urb(iurb),z_u)
           
           call param(iurb,nz_u(iurb),nz_urb(iurb),nzurban(iurb),    &
                      nd_u(iurb),csg_u,csg,alag_u,alag,csr_u,csr,    &
                      alar_u,alar,csw_u,csw,alaw_u,alaw,             &
                      ws_u,ws,bs_u,bs,z0g_u,z0r_u,z0,                &
                      strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,   &
                      pb_urb,pb,lp_urb2d(ix,iy),                     &
                      lb_urb2d(ix,iy),hgt_urb2d(ix,iy),FRC_URB2D(ix,iy))  



         
           call icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,fws_u,fsg_u,   &
                         nd_u(iurb),strd,ws,nzurban(iurb),z_u)   

       do iz= kts,kte
          ua1D(iz)=u_phy(ix,iz,iy)
          va1D(iz)=v_phy(ix,iz,iy)
	  pt1D(iz)=th_phy(ix,iz,iy)
	  da1D(iz)=rho(ix,iz,iy)
	  pr1D(iz)=p_phy(ix,iz,iy)

	  pt01D(iz)=300.
	  z1D(iz)=z(ix,iz,iy)
          a_u1D(iz)=0.
          a_v1D(iz)=0.
          a_t1D(iz)=0.
          a_e1D(iz)=0.
          b_u1D(iz)=0.
          b_v1D(iz)=0.
          b_t1D(iz)=0.
          b_e1D(iz)=0.           
         enddo
	 z1D(kte+1)=z(ix,kte+1,iy)

         do id=1,ndm
         do iz_u=1,nz_um
         do iw=1,nwr_u


            if(ind_zwd(iz_u,iw,id).gt.urban_map_zwd)write(*,*)'ind_zwd too big w',ind_zwd(iz_u,iw,id)
          tw1D(2*id-1,iz_u,iw)=tw1_urb4d(ix,ind_zwd(iz_u,iw,id),iy)
          tw1D(2*id,iz_u,iw)=tw2_urb4d(ix,ind_zwd(iz_u,iw,id),iy)
         enddo
         enddo
         enddo
	
         do id=1,ndm
          do ig=1,ng_u

            tg1D(id,ig)=tgb_urb4d(ix,ind_gd(ig,id),iy)
          enddo
          do iz_u=1,nz_um
          do ir=1,nwr_u

            if(ind_zwd(iz_u,ir,id).gt.urban_map_zwd)write(*,*)'ind_zwd too big r',ind_zwd(iz_u,ir,id)
            tr1D(id,iz_u,ir)=trb_urb4d(ix,ind_zwd(iz_u,ir,id),iy)
          enddo
          enddo
         enddo

         do id=1,ndm
	 do iz=1,nz_um


	  sfw1D(2*id-1,iz)=sfw1_urb3d(ix,ind_zd(iz,id),iy)
	  sfw1D(2*id,iz)=sfw2_urb3d(ix,ind_zd(iz,id),iy)
	 enddo
	 enddo
	 
	 do id=1,ndm

	  sfg1D(id)=sfg_urb3d(ix,id,iy)
	 enddo
	 
	 do id=1,ndm
	 do iz=1,nz_um

	  sfr1D(id,iz)=sfr_urb3d(ix,ind_zd(iz,id),iy)
	 enddo
	 enddo

         
         rs1D=swdown(ix,iy)
         rld1D=glw(ix,iy)
         time_h=(itimestep*dt)/3600.+gmt

         zr1D=acos(COSZ_URB2D(ix,iy))
         deltar1D=DECLIN_URB
         ah1D=OMG_URB2D(ix,iy)


         call BEP1D(iurb,kms,kme,kts,kte,z1D,dt,ua1D,va1D,pt1D,da1D,pr1D,pt01D,  &
                   zr1D,deltar1D,ah1D,rs1D,rld1D,                   & 
                   alag,alaw,alar,csg,csw,csr,                      & 
                   albg_u(iurb),albw_u(iurb),albr_u(iurb),          &
                   emg_u(iurb),emw_u(iurb),emr_u(iurb),             & 
                   fww_u,fwg_u,fgw_u,fsw_u,                         &                    
                   fws_u,fsg_u,z0,                                  &                               
                   nd_u(iurb),strd,drst,ws,bs,ss,pb,                & 
                   nzurban(iurb),z_u,                               & 
                   tw1D,tg1D,tr1D,sfw1D,sfg1D,sfr1D,                & 
                   a_u1D,a_v1D,a_t1D,a_e1D,                         & 
                   b_u1D,b_v1D,b_t1D,b_e1D,                         & 
                   dlg1D,dl_u1D,sf1D,vl1D,rl_up(ix,iy),             &
                   rs_abs(ix,iy),emiss(ix,iy),grdflx_urb(ix,iy))                            

         do id=1,ndm
	 do iz=1,nz_um
	  sfw1_urb3d(ix,ind_zd(iz,id),iy)=sfw1D(2*id-1,iz) 
	  sfw2_urb3d(ix,ind_zd(iz,id),iy)=sfw1D(2*id,iz) 
	 enddo
	 enddo
 
	 do id=1,ndm
	  sfg_urb3d(ix,id,iy)=sfg1D(id) 
	 enddo
	
	 do id=1,ndm
	 do iz=1,nz_um
	  sfr_urb3d(ix,ind_zd(iz,id),iy)=sfr1D(id,iz)
	 enddo
	 enddo

         do id=1,ndm
         do iz_u=1,nz_um
         do iw=1,nwr_u
          tw1_urb4d(ix,ind_zwd(iz_u,iw,id),iy)=tw1D(2*id-1,iz_u,iw)
          tw2_urb4d(ix,ind_zwd(iz_u,iw,id),iy)=tw1D(2*id,iz_u,iw)
         enddo
         enddo
         enddo
        
         do id=1,ndm
          do ig=1,ng_u
           tgb_urb4d(ix,ind_gd(ig,id),iy)=tg1D(id,ig)
          enddo
          do iz_u=1,nz_um
          do ir=1,nwr_u
           trb_urb4d(ix,ind_zwd(iz_u,ir,id),iy)=tr1D(id,iz_u,ir)
          enddo
          enddo
         enddo
        
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
          b_e(ix,iz,iy)=b_e1D(iz)
          dlg(ix,iz,iy)=dlg1D(iz)
          dl_u(ix,iz,iy)=dl_u1D(iz)
         enddo
         sf(ix,kte+1,iy)=sf1D(kte+1)

         endif 
   
      enddo  
      enddo  


        time_bep=time_bep+dt
         
         
      return
      end subroutine BEP
            


      subroutine BEP1D(iurb,kms,kme,kts,kte,z,dt,ua,va,pt,da,pr,pt0,  &  
                      zr,deltar,ah,rs,rld,                            & 
                      alag,alaw,alar,csg,csw,csr,                     & 
                      albg,albw,albr,emg,emw,emr,                     & 
                      fww,fwg,fgw,fsw,fws,fsg,z0,                     &                                             
                      ndu,strd,drst,ws,bs,ss,pb,                      & 
                      nzu,z_u,                                        & 
                      tw,tg,tr,sfw,sfg,sfr,                           & 
                      a_u,a_v,a_t,a_e,                                &
                      b_u,b_v,b_t,b_e,                                & 
                      dlg,dl_u,sf,vl,rl_up,rs_abs,emiss,grdflx_urb)                             















 






















 


      implicit none

 








      integer kms,kme,kts,kte
      real z(kms:kme)               
      real ua(kms:kme)                
      real va(kms:kme)                
      real pt(kms:kme)                
      real da(kms:kme)                
      real pr(kms:kme)                
      real pt0(kms:kme)               
      real dt                    
      real zr                    
      real deltar                
      real ah                    
      real rs                    
      real rld                   



      integer iurb               


      real alag(ng_u)            
      real alaw(nwr_u)           
      real alar(nwr_u)           
      real csg(ng_u)             
      real csw(nwr_u)            
      real csr(nwr_u)            


      real albg                  
      real albw                  
      real albr                  
      real emg                   
      real emw                   
      real emr                   




      real fww(nz_um,nz_um,ndm,nurbm)  
      real fwg(nz_um,ndm,nurbm)        
      real fgw(nz_um,ndm,nurbm)        
      real fsw(nz_um,ndm,nurbm)        
      real fws(nz_um,ndm,nurbm)        
      real fsg(ndm,nurbm)              


      real z0(ndm,nz_um)           
      

      integer ndu                  
      real strd(ndm)               
      real drst(ndm)               
      real ws(ndm)                 
      real bs(ndm)                 
      real ss(nz_um)               
      real pb(nz_um)               
        

      integer nzu                  
      real z_u(nz_um)              








      real tw(2*ndm,nz_um,nwr_u)  
      real tr(ndm,nz_um,nwr_u)  
      real tg(ndm,ng_u)          
      real sfw(2*ndm,nz_um)      
      real sfg(ndm)              
      real sfr(ndm,nz_um)      
      real gfg(ndm)             
      real gfr(ndm,nz_um)     
      real gfw(2*ndm,nz_um)     






      real sf(kms:kme)             
      real vl(kms:kme)               
     


      real a_u(kms:kme)              
      real a_v(kms:kme)              
      real a_t(kms:kme)              
      real a_e(kms:kme)              
      real b_u(kms:kme)              
      real b_v(kms:kme)              
      real b_t(kms:kme)              
      real b_e(kms:kme)              
      real dlg(kms:kme)              
      real dl_u(kms:kme)             
      




      real dz(kms:kme)               



      real ua_u(nz_um)          
      real va_u(nz_um)          
      real pt_u(nz_um)          
      real da_u(nz_um)          
      real pt0_u(nz_um)         
      real pr_u(nz_um)          



      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     
      real rlg(ndm)             
      real rlw(2*ndm,nz_um)     



      real ptg(ndm)             
      real ptr(ndm,nz_um)     
      real ptw(2*ndm,nz_um)     

 





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
      

      real rs_abs 
      real rl_up 
      real emiss 
      real grdflx_urb 
      integer iz,id
      integer iw,ix,iy







      do iz=kts,kte
         dz(iz)=z(iz+1)-z(iz)
      end do


      call interpol(kms,kme,kts,kte,nzu,z,z_u,ua,ua_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,va,va_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt,pt_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt0,pt0_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pr,pr_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,da,da_u)

                   


      call modif_rad(iurb,ndu,nzu,z_u,ws,               &
                    drst,strd,ss,pb,                    &
                    tw,tg,albg,albw,emw,emg,            &
                    fww,fwg,fgw,fsw,fsg,                &
                    zr,deltar,ah,                       &
                    rs,rld,rsw,rsg,rlw,rlg)                       
 

       
       call upward_rad(ndu,nzu,ws,bs,                   &
                       sigma,pb,ss,                     &
                       tg,emg,albg,rlg,rsg,sfg,         & 
                       tw,emw,albw,rlw,rsw,sfw,         & 
                       tr,emr,albr,rld,rs,sfr,          & 
                       rs_abs,rl_up,emiss,grdflx_urb)               
        

     

      call surf_temp(nzu,ndu,pr_u,dt,ss,                & 
                    rs,rld,rsg,rlg,rsw,rlw,             &
                    tg,alag,csg,emg,albg,ptg,sfg,gfg,   &
                    tr,alar,csr,emr,albr,ptr,sfr,gfr,   &
                    tw,alaw,csw,emw,albw,ptw,sfw,gfw)  
      
      

       
      call buildings(ndu,nzu,z0,ua_u,va_u,                       & 
                     pt_u,pt0_u,ptg,ptr,da_u,ptw,drst,           &                      
                     uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,  & 
                     uhb_u,vhb_u,thb_u,ehb_u,ss,dt)                        
 
         





 
      do id=1,ndu         
         sfg(id)=-da_u(1)*cp_u*thb_u(id,1)
         do iz=2,nzu
            sfr(id,iz)=-da_u(iz)*cp_u*thb_u(id,iz)
         enddo
         
         do iz=1,nzu
            sfw(2*id-1,iz)=-da_u(iz)*cp_u*(tvb_u(2*id-1,iz)+     &
                tva_u(2*id-1,iz)*pt_u(iz))
            sfw(2*id,iz)=-da_u(iz)*cp_u*(tvb_u(2*id,iz)+         &
                tva_u(2*id,iz)*pt_u(iz)) 
         enddo
      enddo
      

          









      call urban_meso(ndu,kms,kme,kts,kte,nzu,z,dz,z_u,pb,ss,bs,ws,sf, & 
                     vl,uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,     &
                     uhb_u,vhb_u,thb_u,ehb_u,                          &
                     a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e)                    
       






      call interp_length(ndu,kms,kme,kts,kte,nzu,z_u,z,ss,ws,bs,dlg,dl_u)

      return
      end subroutine BEP1D




       subroutine param(iurb,nzu,nzurb,nzurban,ndu,                   &
                       csg_u,csg,alag_u,alag,csr_u,csr,               &
                       alar_u,alar,csw_u,csw,alaw_u,alaw,             &
                       ws_u,ws,bs_u,bs,z0g_u,z0r_u,z0,                &  
                       strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,   &
                       pb_urb,pb,lp_urb,lb_urb,hgt_urb,frc_urb)        





      implicit none

  



      integer iurb                 
      integer nzu                  
      integer nzurb                
      integer ndu                  
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
      real ss_urb(nz_um)     
      real pb_urb(nz_um)     
      real lp_urb                
      real lb_urb                
      real hgt_urb               
      real frc_urb               
     



      real alag(ng_u)           
      real alar(nwr_u)           
      real alaw(nwr_u)           
      real csg(ng_u)            
      real csr(nwr_u)            
      real csw(nwr_u)            
      real bs(ndm)              
      real drst(ndm)            
      real strd(ndm)            
      real ws(ndm)              
      real z0(ndm,nz_um)      
      real ss(nz_um)          
      real pb(nz_um)          
      integer nzurban




      integer id,ig,ir,iw,iz,ihu







      ss=0.
      pb=0.
      csg=0.
      alag=0.
      csr=0.
      alar=0.
      csw=0.
      alaw=0.
      z0=0.
      ws=0.
      bs=0.
      strd=0.
      drst=0.
      nzurban=0
      
      ihu=0

       do iz=1,nz_um
          if (ss_urb(iz)/=0.) then
             ihu=1
             exit
          else
             continue
          endif
       enddo
           
       if (ihu==1) then
          do iz=1,nzurb+1
             ss(iz)=ss_urb(iz)
             pb(iz)=pb_urb(iz)
          enddo
          nzurban=nzurb
       else
          do iz=1,nzu+1
             ss(iz)=ss_u(iz,iurb)
             pb(iz)=pb_u(iz,iurb)
          end do 
          nzurban=nzu
       endif

       do id=1,ndu
        z0(id,1)=z0g_u(iurb)
        do iz=2,nzurban+1
           z0(id,iz)=z0r_u(iurb)
        enddo
       enddo
                 
       do ig=1,ng_u
        csg(ig)=csg_u(iurb)
        alag(ig)=alag_u(iurb)
       enddo
       
       do ir=1,nwr_u
        csr(ir)=csr_u(iurb)
        alar(ir)=alar_u(iurb)
       enddo
       
       do iw=1,nwr_u
        csw(iw)=csw_u(iurb)
        alaw(iw)=alaw_u(iurb)
       enddo
      
       do id=1,ndu
        strd(id)=strd_u(id,iurb)
        drst(id)=drst_u(id,iurb)     
       enddo
    
       do id=1,ndu
          if ((hgt_urb<=0.).OR.(lp_urb<=0.).OR.(lb_urb<=0.)) then
             ws(id)=ws_u(id,iurb)
             bs(id)=bs_u(id,iurb)
          else if ((lp_urb/frc_urb<1.).and.(lp_urb<lb_urb)) then
                  bs(id)=2.*hgt_urb*lp_urb/(lb_urb-lp_urb)
                  ws(id)=2.*hgt_urb*lp_urb*((frc_urb/lp_urb)-1.)/(lb_urb-lp_urb)
               else
                  ws(id)=ws_u(id,iurb)
                  bs(id)=bs_u(id,iurb) 
          endif
       enddo
       do id=1,ndu
          if ((bs(id)<=1.).OR.(bs(id)>=150.)) then


             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
          endif
          if ((ws(id)<=1.).OR.(ws(id)>=150.)) then


             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
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
         



      subroutine modif_rad(iurb,nd,nz_u,z,ws,drst,strd,ss,pb,    &
                          tw,tg,albg,albw,emw,emg,               &
                          fww,fwg,fgw,fsw,fsg,                   &
                          zr,deltar,ah,                          &    
                          rs,rl,rsw,rsg,rlw,rlg)                       
 





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
      real tw(2*ndm,nz_um,nwr_u) 
      real tg(ndm,ng_u)         
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



      real rlg(ndm)             
      real rlw(2*ndm,nz_um)     
      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     





      integer id,iz



      call shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,           &
                     rs,rsw,rsg)


      do id=1,nd
         call long_rad(iurb,nz_u,id,emw,emg,                 &
                      fwg,fww,fgw,fsw,fsg,tg,tw,rlg,rlw,rl,pb)
         
         call short_rad(iurb,nz_u,id,albw,albg,fwg,fww,fgw,rsg,rsw,pb)
  
      enddo
      
      return
      end subroutine modif_rad





      subroutine surf_temp(nz_u,nd,pr,dt,ss,rs,rl,rsg,rlg,rsw,rlw,     &
                          tg,alag,csg,emg,albg,ptg,sfg,gfg,             &
                          tr,alar,csr,emr,albr,ptr,sfr,gfr,             &
                          tw,alaw,csw,emw,albw,ptw,sfw,gfw)             
                 





      implicit none
      
      
      



      integer nz_u              
      integer nd                
      real alag(ng_u)           
      real alar(nwr_u)           
      real alaw(nwr_u)           
      real albg                 
      real albr                 
      real albw                 
      real csg(ng_u)            
      real csr(nwr_u)            
      real csw(nwr_u)            
      real dt                   
      real emg                  
      real emr                  
      real emw                  
      real pr(nz_um)            
      real rs                   
      real rl                   
      real rlg(ndm)             
      real rlw(2*ndm,nz_um)     
      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     
      real sfg(ndm)             
      real sfr(ndm,nz_um)     
      real sfw(2*ndm,nz_um)     
      real gfg(ndm)             
      real gfr(ndm,nz_um)     
      real gfw(2*ndm,nz_um)     
      real ss(nz_um)          
      real tg(ndm,ng_u)         
      real tr(ndm,nz_um,nwr_u) 
      real tw(2*ndm,nz_um,nwr_u) 
      




      real ptg(ndm)             
      real ptr(ndm,nz_um)     
      real ptw(2*ndm,nz_um)     




      integer id,ig,ir,iw,iz

      real rtg(ndm)             
      real rtr(ndm,nz_um)     
      real rtw(2*ndm,nz_um)     
      real tg_tmp(ng_u)
      real tr_tmp(nwr_u)
      real tw_tmp(nwr_u)

      real dzg_u(ng_u)          

      real dzr_u(nwr_u)          
         
      real dzw_u(nwr_u)          
      

      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/
      data dzr_u /0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/   
      data dzw_u /0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/




        
   
      do id=1,nd


       do ig=1,ng_u
        tg_tmp(ig)=tg(id,ig)
       end do

       call soil_temp(ng_u,dzg_u,tg_tmp,ptg(id),alag,csg,        &
                     rsg(id),rlg(id),pr(1),                    &
                     dt,emg,albg,                              &
                     rtg(id),sfg(id),gfg(id))    
       do ig=1,ng_u
        tg(id,ig)=tg_tmp(ig)
       end do


         
       do iz=2,nz_u
            
        if(ss(iz).gt.0.)then
         do ir=1,nwr_u        
          tr_tmp(ir)=tr(id,iz,ir)
         end do
        
         call soil_temp(nwr_u,dzr_u,tr_tmp,ptr(id,iz),          &
                       alar,csr,rs,rl,pr(iz),dt,emr,albr,    &
                       rtr(id,iz),sfr(id,iz),gfr(id,iz))     
         do ir=1,nwr_u        
          tr(id,iz,ir)=tr_tmp(ir)
         end do
       
        end if
            
       end do 


         
       do iz=1,nz_u
            
        do iw=1,nwr_u        
         tw_tmp(iw)=tw(2*id-1,iz,iw)
        end do
        call soil_temp(nwr_u,dzw_u,tw_tmp,ptw(2*id-1,iz),alaw,          &
                      csw,                                           &     
                      rsw(2*id-1,iz),rlw(2*id-1,iz),                 &     
                      pr(iz),dt,emw,                                 &    
                albw,rtw(2*id-1,iz),sfw(2*id-1,iz),gfw(2*id-1,iz))   

        do iw=1,nwr_u        
         tw(2*id-1,iz,iw)=tw_tmp(iw)
        end do
            
        do iw=1,nwr_u        
         tw_tmp(iw)=tw(2*id,iz,iw)
        end do
            
        call soil_temp(nwr_u,dzw_u,tw_tmp,ptw(2*id,iz),alaw,          &      
                      csw,                                         &     
                      rsw(2*id,iz),rlw(2*id,iz),                   &     
                      pr(iz),dt,emw,                               &     
               albw,rtw(2*id,iz),sfw(2*id,iz),gfw(2*id,iz))        
         do iw=1,nwr_u        
          tw(2*id,iz,iw)=tw_tmp(iw)
         end do
                   
        end do 
	
      end do 
      
      return
      end subroutine surf_temp
     



      subroutine buildings(nd,nz,z0,ua_u,va_u,pt_u,pt0_u,         &
                        ptg,ptr,da_u,ptw,                            &
                        drst,uva_u,vva_u,uvb_u,vvb_u,                &
                        tva_u,tvb_u,evb_u,                           &
                        uhb_u,vhb_u,thb_u,ehb_u,ss,dt)                  







      implicit none

        



      integer nd                
      integer nz                
      real ua_u(nz_um)          
      real va_u(nz_um)          
      real da_u(nz_um)          
      real drst(ndm)            
      real dz
      real pt_u(nz_um)          
      real pt0_u(nz_um)         
      real ptg(ndm)             
      real ptr(ndm,nz_um)     
      real ptw(2*ndm,nz_um)     
      real ss(nz_um)          
      real z0(ndm,nz_um)      
      real dt 










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
  



      integer id,iz




       dz=dz_u

      do id=1,nd


         call flux_flat(dz,z0(id,1),ua_u(1),va_u(1),pt_u(1),pt0_u(1),  &
                       ptg(id),uhb_u(id,1),                            & 
                       vhb_u(id,1),thb_u(id,1),ehb_u(id,1))            


         do iz=2,nz
            if(ss(iz).gt.0)then
               call flux_flat(dz,z0(id,iz),ua_u(iz),                  &              
                       va_u(iz),pt_u(iz),pt0_u(iz),                   &   
                       ptr(id,iz),uhb_u(id,iz),                       &   
                       vhb_u(id,iz),thb_u(id,iz),ehb_u(id,iz))        
            else
               uhb_u(id,iz) = 0.0
               vhb_u(id,iz) = 0.0
               thb_u(id,iz) = 0.0
               ehb_u(id,iz) = 0.0
            endif
         end do


         do iz=1,nz         
            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),     &  
                        ptw(2*id-1,iz),                             &   
                        uva_u(2*id-1,iz),vva_u(2*id-1,iz),          &   
                        uvb_u(2*id-1,iz),vvb_u(2*id-1,iz),          &   
                        tva_u(2*id-1,iz),tvb_u(2*id-1,iz),          &   
                        evb_u(2*id-1,iz),drst(id),dt)                  
                    
            call flux_wall(ua_u(iz),va_u(iz),pt_u(iz),da_u(iz),    &   
                        ptw(2*id,iz),                              &    
                        uva_u(2*id,iz),vva_u(2*id,iz),             &    
                        uvb_u(2*id,iz),vvb_u(2*id,iz),             &    
                        tva_u(2*id,iz),tvb_u(2*id,iz),             &   
                        evb_u(2*id,iz),drst(id),dt) 

       
         end do
         
      end do
                
      return
      end subroutine buildings
      




        subroutine urban_meso(nd,kms,kme,kts,kte,nz_u,z,dz,z_u,pb,ss,bs,ws,sf,vl,    &
                             uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u, &       
                             uhb_u,vhb_u,thb_u,ehb_u,                   &      
                             a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e)           







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
      real ehb_u(ndm,nz_um)   
      real evb_u(2*ndm,nz_um)   





      real sf(kms:kme)             
      real vl(kms:kme)               
      real a_u(kms:kme)              
      real a_v(kms:kme)              
      real a_t(kms:kme)              
      real a_e(kms:kme)              
      real b_u(kms:kme)              
      real b_v(kms:kme)              
      real b_t(kms:kme)              
      real b_e(kms:kme)              
      



      real dzz
      real fact
      integer id,iz,iz_u
      real se,sr,st,su,sv
      real uet(kms:kme)                
      real veb,vta,vtb,vte,vtot,vua,vub,vva,vvb








      do iz=kts,kte
         a_u(iz)=0.
         a_v(iz)=0.
         a_t(iz)=0.
         a_e(iz)=0.
         b_u(iz)=0.
         b_v(iz)=0.
         b_e(iz)=0.
         b_t(iz)=0.
         uet(iz)=0.
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
         
         do iz=kts,kte
            st=0.
            su=0.
            sv=0.
            se=0.
            do iz_u=2,nz_u
               if(z(iz).le.z_u(iz_u).and.z(iz+1).gt.z_u(iz_u))then
                  st=st+ss(iz_u)*thb_u(id,iz_u)
                  su=su+ss(iz_u)*uhb_u(id,iz_u)
                  sv=sv+ss(iz_u)*vhb_u(id,iz_u)          
                  se=se+ss(iz_u)*ehb_u(id,iz_u)*(z_u(iz_u+1)-z_u(iz_u))
               endif
            enddo
      
            fact=bs(id)/(ws(id)+bs(id))/vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+st*fact
            b_u(iz)=b_u(iz)+su*fact
            b_v(iz)=b_v(iz)+sv*fact
            b_e(iz)=b_e(iz)+se*fact
         enddo
      enddo              


           
      do iz=kts,kte 
         uet(iz)=0.
         do id=1,nd              
            vtb=0.
            vta=0.
            vua=0.
            vub=0.
            vva=0.
            vvb=0.
            veb=0.
	    vte=0.
            do iz_u=1,nz_u
               dzz=max(min(z_u(iz_u+1),z(iz+1))-max(z_u(iz_u),z(iz)),0.)
               fact=dzz/(ws(id)+bs(id))
               vtb=vtb+pb(iz_u+1)*                                  &        
                    (tvb_u(2*id-1,iz_u)+tvb_u(2*id,iz_u))*fact   
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
            enddo
           
            fact=1./vl(iz)/dz(iz)/nd
            b_t(iz)=b_t(iz)+vtb*fact
            a_t(iz)=a_t(iz)+vta*fact
            a_u(iz)=a_u(iz)+vua*fact
            a_v(iz)=a_v(iz)+vva*fact
            b_u(iz)=b_u(iz)+vub*fact
            b_v(iz)=b_v(iz)+vvb*fact
            b_e(iz)=b_e(iz)+veb*fact
            uet(iz)=uet(iz)+vte*fact
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




      subroutine shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,         &
                           rs,rsw,rsg)
        





      implicit none
     



      integer nd                
      integer nz_u              
      real ah                   
      real deltar               
      real drst(ndm)            
      real rs                   
      real ss(nz_um)          
      real pb(nz_um)          
      real ws(ndm)              
      real z(nz_um)           
      real zr                   




      real rsg(ndm)             
      real rsw(2*ndm,nz_um)     




      integer id,iz,jz
      real aae,aaw,bbb,phix,rd,rtot,wsd
      




      if(rs.eq.0.or.sin(zr).eq.1)then
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
                  rsw(2*id-1,iz)=rsw(2*id-1,iz)+rs*rd*ss(jz+1)/pb(iz+1)
                endif
              
                if(abs(sin(aaw)).gt.1.e-10)then
                  call shade_wall(z(iz),z(iz+1),z(jz+1),phix,aaw,   &    
                      ws(id),rd)
                  rsw(2*id,iz)=rsw(2*id,iz)+rs*rd*ss(jz+1)/pb(iz+1)                  
                endif
               enddo             
            endif  
            enddo
        if(abs(sin(aae)).gt.1.e-10)then
            wsd=abs(ws(id)/sin(aae))
              
            do jz=1,nz_u           
               rd=max(0.,wsd-z(jz+1)*tan(phix))
               rsg(id)=rsg(id)+rs*rd*ss(jz+1)/wsd          
            enddo
            rtot=0.
           
            do iz=1,nz_u
               rtot=rtot+(rsw(2*id,iz)+rsw(2*id-1,iz))*            &
                         (z(iz+1)-z(iz))
            enddo
            rtot=rtot+rsg(id)*ws(id)
        else
            rsg(id)=rs
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




      subroutine long_rad(iurb,nz_u,id,emw,emg,                  &
                         fwg,fww,fgw,fsw,fsg,tg,tw,rlg,rlw,rl,pb)










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
      real tg(ndm,ng_u)               
      real tw(2*ndm,nz_um,nwr_u)       
      




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
         aaa(i,j)=-(1.-emw)*fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
        enddo
        

        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)
        
        bbb(i)=fsw(i,id,iurb)*rl+emg*fgw(i,id,iurb)*sigma*tg(id,ng_u)**4
        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*emw*sigma*fww(j,i,id,iurb)*       &
               tw(2*id,j,nwr_u)**4+                              &
               fww(j,i,id,iurb)*rl*(1.-pb(j+1))
        enddo
        
       enddo
       


       do i=1+nz_u,2*nz_u
        
        do j=1,nz_u
         aaa(i,j)=-(1.-emw)*fww(j,i-nz_u,id,iurb)*pb(j+1)
        enddo
        
        do j=1+nz_u,2*nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.
        

        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)
        
        bbb(i)=fsw(i-nz_u,id,iurb)*rl+                           &     
              emg*fgw(i-nz_u,id,iurb)*sigma*tg(id,ng_u)**4

        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*emw*sigma*fww(j,i-nz_u,id,iurb)*  &   
                tw(2*id-1,j,nwr_u)**4+                           &   
                fww(j,i-nz_u,id,iurb)*rl*(1.-pb(j+1))
        enddo
       
       enddo


       do j=1,nz_u
        aaa(2*nz_u+1,j)=-(1.-emw)*fwg(j,id,iurb)*pb(j+1)
       enddo
       
       do j=nz_u+1,2*nz_u
        aaa(2*nz_u+1,j)=-(1.-emw)*fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
       enddo
       
       aaa(2*nz_u+1,2*nz_u+1)=1.
       
       bbb(2*nz_u+1)=fsg(id,iurb)*rl
       
       do i=1,nz_u
        bbb(2*nz_u+1)=bbb(2*nz_u+1)+emw*sigma*fwg(i,id,iurb)*pb(i+1)*    &
                      (tw(2*id-1,i,nwr_u)**4+tw(2*id,i,nwr_u)**4)+       &
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
             



       subroutine short_rad(iurb,nz_u,id,albw,                        & 
                           albg,fwg,fww,fgw,rsg,rsw,pb)










      implicit none

  
      



      real albg                 
      real albw                 
      real fgw(nz_um,ndm,nurbm)       
      real fwg(nz_um,ndm,nurbm)       
      real fww(nz_um,nz_um,ndm,nurbm) 
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
         bbb(i)=rsw(2*id-1,i)
         
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
        bbb(i)=rsw(2*id,i-nz_u)
        
      enddo



      do j=1,nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j,id,iurb)*pb(j+1)
      enddo
       
      do j=nz_u+1,2*nz_u
         aaa(2*nz_u+1,j)=-albw*fwg(j-nz_u,id,iurb)*pb(j-nz_u+1)
      enddo
       
      aaa(2*nz_u+1,2*nz_u+1)=1.
      bbb(2*nz_u+1)=rsg(id)
       
      call gaussj(aaa,2*nz_u+1,bbb,2*nz_um+1)

      do i=1,nz_u
         rsw(2*id-1,i)=bbb(i)
      enddo
       
      do i=nz_u+1,2*nz_u
         rsw(2*id,i-nz_u)=bbb(i) 
      enddo
       
      rsg(id)=bbb(2*nz_u+1)
       
      return
      end subroutine short_rad
             



      
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
                     call wrf_error_fatal3("<stdin>",2260,&
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
         
         if(a(icol,icol).eq.0) call wrf_error_fatal3("<stdin>",2282,&
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
         


       
      subroutine soil_temp(nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,gf)









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
      alpha=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf

        
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
         a(iz,2)=1+dt*(cddz(iz)+cddz(iz+1))/dz(iz)          
         a(iz,3)=-cddz(iz+1)*dt/dz(iz)
         c(iz)=temp(iz)
      enddo          
                     
      a(nz,1)=-dt*cddz(nz)/dz(nz)
      a(nz,2)=1.+dt*cddz(nz)/dz(nz)
      a(nz,3)=0.
      c(nz)=temp(nz)+dt*alpha/cs(nz)/dz(nz)

      
      call invert(nz,a,c,temp)

           
      pt=temp(nz)*(press/1.e+5)**(-rcp_u)

      rt=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)
                        

       gf=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf                                   
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
  



  
      subroutine flux_wall(ua,va,pt,da,ptw,uva,vva,uvb,vvb,            &
                                  tva,tvb,evb,drst,dt)      
       





      implicit none

    
         


      real drst                 
      real da                   
      real pt                   
      real ptw                  
      real ua                   
      real va                   

      real dt                   






      real uva                  
      real uvb                  
      real vva                  
      real vvb                  
      real tva                  
      real tvb                  
      real evb                  



      real hc
      real u_ort
      real vett





      vett=(ua**2+va**2)**.5         
         
      u_ort=abs((cos(drst)*ua-sin(drst)*va))
       
      uva=-cdrag*u_ort/2.*cos(drst)*cos(drst)
      vva=-cdrag*u_ort/2.*sin(drst)*sin(drst)
         
      uvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*va
      vvb=cdrag*u_ort/2.*sin(drst)*cos(drst)*ua
         
      hc=5.678*(1.09+0.23*(vett/0.3048))  

      if(hc.gt.da*cp_u/dt)then
        hc=da*cp_u/dt
      endif
         




      tvb=hc*ptw/da/cp_u-hc/da/cp_u*pt 
      tva = 0.                  
         
      evb=cdrag*(abs(u_ort)**3.)/2.
              
      return
      end subroutine flux_wall
         




      subroutine flux_flat(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,thb,ehb)
                                





      implicit none

     




      real dz                   
      real pt                   
      real pt0                  
      real ptg                  
      real ua                   
      real va                   
      real z0                   








      real uhb                  
      real vhb                  
      real thb                  
      real tva                  
      real tvb                  
      real ehb                  





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
      real utot
      real wstar
      real zz
      
      real b,cm,ch,rr,tol
      parameter(b=9.4,cm=7.4,ch=5.3,rr=0.74,tol=.001)







         
      utot=(ua**2+va**2)**.5
        
      




      zz=dz/2.
   








      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)



      if(ric.gt.0)then
         fm=1/(1+0.5*b*ric)**2
         fh=fm
      else
         c=b*cm*aa*aa*(zz/z0)**.5
         fm=1-b*ric/(1+c*(-ric)**.5)
         c=c*ch/cm
         fh=1-b*ric/(1+c*(-ric)**.5)
      endif
      
      fbuw=-aa*aa*utot*utot*fm
      fbpt=-aa*aa*utot*(pt-ptg)*fh/rr
                     
      ustar=(-fbuw)**.5
      tstar=-fbpt/ustar

      al=(vk*g_u*tstar)/(pt*ustar*ustar)                      
      
      buu=-g_u/pt0*ustar*tstar
       
      uhb=-ustar*ustar*ua/utot
      vhb=-ustar*ustar*va/utot 
      thb=-ustar*tstar       

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


     







      z_u=0.
      nz_u=0
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
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,emg_u,emw_u,&
                emr_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b)



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
      real emg_u(nurbm)       
      real emw_u(nurbm)       
      real emr_u(nurbm)       


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
       do iu=1,icate
              if(ndm.lt.nd_u(iu))then
                write(*,*)'ndm too small in module_sf_bep, please increase to at least ', nd_u(iu)
                write(*,*)'remember also that urban_map_zrd should be equal or greater than nz_um*ndm*nwr-u!'
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
              write(*,*)'remember also that urban_map_zrd should be equal or greater than nz_um*ndm*nwr-u!'
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
         
       return
      END SUBROUTINE init_para



      subroutine angle(along,alat,day,realt,zr,deltar,ah)

















      implicit none
      real along,alat, realt, zr, deltar, ah, arg
      real rad,om,radh,initt, pii, drad, alongt, cphi, sphi
      real c1, c2, c3, s1, s2, s3, delta, rmsr2, cd, sid 
      real et, ahor, chor, coznt 
      integer day 


      data rad,om,radh,initt/0.0174533,0.0172142,0.26179939,0/
 
       zr=0.
       deltar=0.
       ah=0.

       pii = 3.14159265358979312
       drad = pii/180.
      
       alongt=along/15.
       cphi=cos(alat*drad)
       sphi=sin(alat*drad)



       arg=om*day
       c1=cos(arg)
       c2=cos(2.*arg)
       c3=cos(3.*arg)
       s1=sin(arg)
       s2=sin(2.*arg)
       s3=sin(3.*arg)
       delta=0.33281-22.984*c1-0.3499*c2-0.1398*c3+3.7872*s1+0.03205*s2+0.07187*s3
       rmsr2=(1./(1.-0.01673*c1))**2
       deltar=delta*rad
       cd=cos(deltar)
       sid=sin(deltar)



       et=0.0072*c1-0.0528*c2-0.0012*c3-0.1229*s1-0.1565*s2-0.0041*s3




      

      
 
      ahor=realt-12.+et+alongt
      ah=ahor*radh
      chor=cos(ah)



      coznt=sphi*sid+cphi*cd*chor
      
       zr=acos(coznt)

      return

      END SUBROUTINE angle




      subroutine upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                     &
                       tg,emg_u,albg_u,rlg,rsg,sfg,                        & 
                       tw,emw_u,albw_u,rlw,rsw,sfw,                        & 
                       tr,emr_u,albr_u,rld,rs, sfr,                        & 
                       rs_abs,rl_up,emiss,grdflx_urb)




                       implicit none




      real rsw(2*ndm,nz_um)        
      real rlw(2*ndm,nz_um)         
      real rsg(ndm)                   
      real rlg(ndm)                   
      real rs                        
      real sfw(2*ndm,nz_um)      
      real sfg(ndm)              
      real sfr(ndm,nz_um)      
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
      real tw(2*ndm,nz_um,nwr_u)  
      real tr(ndm,nz_um,nwr_u)  
      real tg(ndm,ng_u)          
      integer id 
      integer ndu 

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
      do iw=1,nwr_u
        if(tr(id,iz,iw).lt.100.)then
              write(*,*)'in upward_rad ',iz,id,iw,tr(id,iz,iw) 
              iwrong=0
        endif
        if(tw(2*id-1,iz,iw).lt.100.) then
           write(*,*)'in upward_rad ',iz,id,iw,tw(2*id-1,iz,iw) 
           iwrong=0
        endif
        if(tw(2*id,iz,iw).lt.100.) then
           write(*,*)'in upward_rad ',iz,id,iw,tw(2*id,iz,iw) 
           iwrong=0
        endif
      enddo
      enddo
      enddo
      do id=1,ndu
      do iw=1,ng_u
          if(tg(id,iw).lt.100.) then
            write(*,*)'in upward_rad ',id,iw,tg(id,iw) 
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
       rl_emit=rl_emit-( emg_u*sigma*(tg(id,ng_u)**4.)+(1-emg_u)*rlg(id))*ws(id)/(ws(id)+bs(id))/ndu
       rl_inc=rl_inc+rlg(id)*ws(id)/(ws(id)+bs(id))/ndu       
       rs_abs=rs_abs+(1.-albg_u)*rsg(id)*ws(id)/(ws(id)+bs(id))/ndu
         gfl=(1.-albg_u)*rsg(id)+emg_u*rlg(id)-emg_u*sigma*(tg(id,ng_u)**4.)+sfg(id)
         grdflx_urb=grdflx_urb-gfl*ws(id)/(ws(id)+bs(id))/ndu  
 
          do iz=2,nzu
            rl_emit=rl_emit-(emr_u*sigma*(tr(id,iz,nwr_u)**4.)+(1-emr_u)*rld)*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
            rl_inc=rl_inc+rld*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu            
            rs_abs=rs_abs+(1.-albr_u)*rs*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
            gfl=(1.-albr_u)*rs+emr_u*rld-emr_u*sigma*(tr(id,iz,nwr_u)**4.)+sfr(id,iz)
            grdflx_urb=grdflx_urb-gfl*ss(iz)*bs(id)/(ws(id)+bs(id))/ndu
         enddo
           
         do iz=1,nzu            
            rl_emit=rl_emit-(emw_u*sigma*( tw(2*id-1,iz,nwr_u)**4.+tw(2*id,iz,nwr_u)**4. )+          &
               (1-emw_u)*( rlw(2*id-1,iz)+rlw(2*id,iz) ) )*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu
            rl_inc=rl_inc+(( rlw(2*id-1,iz)+rlw(2*id,iz) ) )*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu
            rs_abs=rs_abs+((1.-albw_u)*( rsw(2*id-1,iz)+rsw(2*id,iz) ) )*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu 
            gfl=(1.-albw_u)*(rsw(2*id-1,iz)+rsw(2*id,iz)) +emw_u*( rlw(2*id-1,iz)+rlw(2*id,iz) )   &
             -emw_u*sigma*( tw(2*id-1,iz,nwr_u)**4.+tw(2*id,iz,nwr_u)**4. )+(sfw(2*id-1,iz)+sfw(2*id,iz))            
            grdflx_urb=grdflx_urb-gfl*dz_u*pb(iz+1)/(ws(id)+bs(id))/ndu
         enddo
          
      enddo
        emiss=(emg_u+emw_u+emr_u)/3.
        rl_up=(rl_inc+rl_emit)-rld
       
         
      return

      END SUBROUTINE upward_rad






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



      subroutine icBEPHI_XY(hb_u,hi_urb1D,ss_u,pb_u,nzu,z_u)

      implicit none   





      real hi_urb1D(nz_um)    



      real z_u(nz_um)         




      real ss_u(nz_um)   
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
               ss_u(iz_u)=0.
               do ilu=1,nz_um
                  if(z_u(iz_u).le.hb_u(ilu)                      &    
                    .and.z_u(iz_u+1).gt.hb_u(ilu))then            
                        ss_u(iz_u)=ss_u(iz_u)+hi_urb1D(ilu)
                  endif 
               enddo
            enddo

            pb_u(1)=1.
            do iz_u=1,nzu
               pb_u(iz_u+1)=max(0.,pb_u(iz_u)-ss_u(iz_u))
            enddo

20    continue    
      return
      end subroutine icBEPHI_XY


END MODULE module_sf_bep

      FUNCTION bep_nurbm () RESULT (bep_val_nurbm)
         USE module_sf_bep
         IMPLICIT NONE
         INTEGER :: bep_val_nurbm
         bep_val_nurbm = nurbm
      END FUNCTION bep_nurbm
      
      FUNCTION bep_ndm () RESULT (bep_val_ndm)
         USE module_sf_bep
         IMPLICIT NONE
         INTEGER :: bep_val_ndm
         bep_val_ndm = ndm
      END FUNCTION bep_ndm
      
      FUNCTION bep_nz_um () RESULT (bep_val_nz_um)
         USE module_sf_bep
         IMPLICIT NONE
         INTEGER :: bep_val_nz_um
         bep_val_nz_um = nz_um
      END FUNCTION bep_nz_um
      
      FUNCTION bep_ng_u () RESULT (bep_val_ng_u)
         USE module_sf_bep
         IMPLICIT NONE
         INTEGER :: bep_val_ng_u
         bep_val_ng_u = ng_u
      END FUNCTION bep_ng_u
      
      FUNCTION bep_nwr_u () RESULT (bep_val_nwr_u)
         USE module_sf_bep
         IMPLICIT NONE
         INTEGER :: bep_val_nwr_u
         bep_val_nwr_u = nwr_u
      END FUNCTION bep_nwr_u
      
