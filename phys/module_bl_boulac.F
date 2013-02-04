MODULE module_bl_boulac

!USE module_model_constants
    

!------------------------------------------------------------------------
!    Calculation of the tendency due to momentum, heat 
!    and moisture turbulent fluxes follwing the approach 
!    of Bougeault and Lacarrere, 1989 (MWR, 117, 1872-1890).
!    The scheme computes a prognostic ecuation for TKE and derives 
!    dissipation and turbulent coefficients using length scales.
!    
!    Subroutine written by Alberto Martilli, CIEMAT, Spain,
!    e-mail:alberto_martilli@ciemat.es
!    August 2006.
!------------------------------------------------------------------------
! IN THIS VERSION TKE IS NOT ADVECTED!!!!
! TO BE CHANGED IN THE FUTURE
! 
! -----------------------------------------------------------------------
!  Constant used in the module
!  ck_b=constant used in the compuation of diffusion coefficients
!  ceps_b=constant used inthe computation of dissipation
!  temin= minimum value allowed for TKE
!  vk=von karman constant
! -----------------------------------------------------------------------

      real ck_b,ceps_b,vk,temin    ! constant for Bougeault and Lacarrere    
      parameter(ceps_b=1/1.4,ck_b=0.4,temin=0.0001,vk=0.4) ! impose minimum values for tke similar to those of MYJ
! -----------------------------------------------------------------------     


   CONTAINS
 
      subroutine boulac(frc_urb2d,idiff,flag_bep,dz8w,dt,u_phy,v_phy   & 
                      ,th_phy,rho,qv_curr,qc_curr,hfx                                  &
                      ,qfx,ustar,cp,g                                          &
                      ,rublten,rvblten,rthblten                                &
                      ,rqvblten,rqcblten                        & 
                      ,tke,dlk,wu,wv,wt,wq,exch_h,exch_m,pblh        &
                      ,a_u_bep,a_v_bep,a_t_bep,a_q_bep          &
                      ,a_e_bep,b_u_bep,b_v_bep                  &
                      ,b_t_bep,b_q_bep,b_e_bep,dlg_bep          &
                      ,dl_u_bep,sf_bep,vl_bep                   &                 
                      ,ids,ide, jds,jde, kds,kde                &
                      ,ims,ime, jms,jme, kms,kme                &
                      ,its,ite, jts,jte, kts,kte)                    

      implicit none



!-----------------------------------------------------------------------
!     Input
!------------------------------------------------------------------------
   INTEGER::                        ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte
 
   integer, INTENT(IN) :: idiff
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   DZ8W      !vertical resolution       
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   qv_curr   !moisture  
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   qc_curr   !liquid water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   RHO       !air density
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   TH_PHY    !potential temperature
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   U_PHY     !x-component of wind
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   V_PHY     !y-component of wind
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   ustar              !friction velocity
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   hfx                !sensible heat flux (W/m2) at surface 
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   qfx                !moisture flux at surface
   real,  INTENT(IN   )    :: g,cp                                              !gravity and Cp
   REAL, INTENT(IN )::   DT                                                      ! Time step

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   FRC_URB2D          !fraction cover urban
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)    ::   PBLH          !PBL height
!
! variable added for urban
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_u_bep        ! Implicit component for the momemtum in X-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_v_bep        ! Implicit component for the momemtum in Y-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_t_bep        ! Implicit component for the Pot. Temp.
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_q_bep        ! Implicit component for Moisture
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_e_bep        ! Implicit component for the TKE
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_u_bep        ! Explicit component for the momemtum in X-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_v_bep        ! Explicit component for the momemtum in Y-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_t_bep        ! Explicit component for the Pot. Temp.
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_q_bep        ! Explicit component for Moisture
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_e_bep        ! Explicit component for the TKE

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT)    ::dlg_bep        ! Height above ground (L_ground in formula (24) of the BLM paper). 
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::dl_u_bep        ! Length scale (lb in formula (22) ofthe BLM paper).
! urban surface and volumes        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::sf_bep           ! surface of the urban grid cells
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::vl_bep             ! volume of the urban grid cells
   LOGICAL, INTENT(IN) :: flag_bep                                             !flag for BEP
                                                           
!
!-----------------------------------------------------------------------
!     Local, carried on from one timestep to the other
!------------------------------------------------------------------------
!      real, save, allocatable, dimension (:,:,:)::TKE  ! Turbulent kinetic energy
      real,  dimension (ims:ime, kms:kme, jms:jme)  ::th_0 ! reference state for potential temperature

!------------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------   
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  exch_h ! exchange coefficient for heat
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  exch_m ! exchange coefficient for momentum
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )    ::  tke  ! Turbulence Kinetic Energy 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wu  ! Turbulent flux of momentum (x) 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wv  ! Turbulent flux of momentum (y) 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wt  ! Turbulent flux of temperature
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wq  ! Turbulent flux of water vapor
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  dlk  ! Turbulent flux of water vapor
! only if idiff not equal 1:
        REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::   RUBLTEN  !tendency for U_phy
        REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::   RVBLTEN  !tendency for V_phy
        REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::   RTHBLTEN !tendency for TH_phy
        REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::   RQVBLTEN !tendency for QV_curr
        REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::   RQCBLTEN !tendency for QV_curr

!--------------------------------------------------------------
! Local
!--------------------------------------------------------------
! 1D array used for the input and output of the routine boulac1D

      real z1D(kms:kme)               ! vertical coordinates (faces of the grid)
      real dz1D(kms:kme)              ! vertical resolution
      real u1D(kms:kme)                 ! wind speed in the x directions
      real v1D(kms:kme)                 ! wind speed in the y directions
      real th1D(kms:kme)                ! potential temperature
      real q1D(kms:kme)                 ! moisture
      real qc1D(kms:kme)                 ! liquid water
      real rho1D(kms:kme)               ! air density
      real rhoz1D(kms:kme)            ! air density at the faces
      real tke1D(kms:kme)               ! air pressure
      real th01D(kms:kme)               ! reference potential temperature
      real dlk1D(kms:kme)               ! dlk
      real dls1D(kms:kme)               ! dls
      real exch1D(kms:kme)            ! exch
      real sf1D(kms:kme)              ! surface of the grid cells
      real vl1D(kms:kme)                ! volume of the  grid cells
      real a_u1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the X-direction
      real a_v1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the Y-direction
      real a_t1D(kms:kme)               ! Implicit component of the heat sources or sinks
      real a_q1D(kms:kme)               ! Implicit component of the moisture sources or sinks
      real a_qc1D(kms:kme)               ! Implicit component of the liquid water sources or sinks
      real a_e1D(kms:kme)               ! Implicit component of the TKE sources or sinks
      real b_u1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the X-direction
      real b_v1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the Y-direction
      real b_t1D(kms:kme)               ! Explicit component of the heat sources or sinks
      real b_q1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real b_qc1D(kms:kme)               ! Explicit component of the liquid water sources or sinks
      real b_e1D(kms:kme)               ! Explicit component of the TKE sources or sinks
      real dlg1D(kms:kme)               ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u1D(kms:kme)              ! Length scale (lb in formula (22) ofthe BLM paper)
      real sh1D(kms:kme)              ! shear
      real bu1D(kms:kme)              ! buoyancy
      real wu1D(kms:kme)              ! turbulent flux of momentum (x component)
      real wv1D(kms:kme)              ! turbulent flux of momentum (y component)
      real wt1D(kms:kme)              ! turbulent flux of temperature
      real wq1D(kms:kme)              ! turbulent flux of water vapor
      real wqc1D(kms:kme)              ! turbulent flux of liquid water 
      real gamma1D(kms:kme)              ! non local term
      real t2_1D(kms:kme)              ! temperature variance
      real w2_1D(kms:kme)              ! vertical velocity variance
! local added only for diagnostic output
      real a_e(ims:ime,kms:kme,jms:jme) ! implicit term in TKE
      real b_e(ims:ime,kms:kme,jms:jme) ! explicit term in TKE
      real bu(ims:ime,kms:kme,jms:jme) ! buoyancy term in TKE
      real sh(ims:ime,kms:kme,jms:jme) ! shear term in TKE
      real wrk(ims:ime) ! working array
      integer ix,iy,iz,id,iz_u,iw_u,ig,ir_u,ix1,iy1,igamma
      real ufrac_int                                              ! urban fraction     
      real vect,time_tke,hour,zzz
      real ustarf,wstar,wts,t2,w2,tstar_w,zzi
      real summ1,summ2,summ3
      save time_tke,hour
!
!    

!here I fix the value of the reference state equal to the value of the potnetial temperature
! the only use of this variable in the code is to compute the paramter BETA = g/th0
! I fix it to 300K. 
      
        do ix=its,ite
        do iy=jts,jte        
        do iz=kts,kte
!         th_0(ix,iz,iy)=th_phy(ix,iz,iy)
         th_0(ix,iz,iy)=300.
        enddo
        enddo
        enddo
! initialization
       z1D=0.               
       dz1D=0.              
       u1D =0.               
       v1D =0.                
       th1D=0.              
       q1D=0.                
       rho1D=0.              
       rhoz1D=0.            
       tke1D =0.             
       th01D =0.             
       dlk1D =0.              
       dls1D =0.              
       exch1D=0.            
       sf1D  =1.            
       vl1D  =1.             
       a_u1D =0.              
       a_v1D =0.              
       a_t1D =0.              
       a_q1D =0. 
       a_qc1D =0.              
       a_e1D =0.              
       b_u1D =0.             
       b_v1D =0.              
       b_t1D =0.            
       b_q1D =0.
       b_qc1D =0.              
       b_e1D =0.             
       dlg1D =0.             
       dl_u1D=0.              
       sh1D  =0.            
       bu1D  =0.            
       wu1D  =0.           
       wv1D  =0.            
       wt1D =0.              
       wq1D =0.             

! flag to choose the method for the calcaulation of the gamma non local term:
! igamma=0 - no term
! igamma=1 Troen and Mahrt
! igamma=2 Deardroff and Therry-Lacarrere
! igamma=3 Holstag and Moeng
    
      igamma=1    
! loop over the columns. 
! put variables in 1D temporary arrays
!     

       do ix=its,ite
       do iy=jts,jte
         z1d(kts)=0.
         do iz= kts,kte
	  u1D(iz)=u_phy(ix,iz,iy)
	  v1D(iz)=v_phy(ix,iz,iy)
	  th1D(iz)=th_phy(ix,iz,iy)
          q1D(iz)=qv_curr(ix,iz,iy)
          qc1D(iz)=qc_curr(ix,iz,iy)
          tke1D(iz)=tke(ix,iz,iy)
	  rho1D(iz)=rho(ix,iz,iy)	   
	  th01D(iz)=th_0(ix,iz,iy)	  
          dz1D(iz)=dz8w(ix,iz,iy)
          z1D(iz+1)=z1D(iz)+dz1D(iz)
         enddo
        rhoz1D(kts)=rho1D(kts)
        do iz=kts+1,kte
         rhoz1D(iz)=(rho1D(iz)*dz1D(iz-1)+rho1D(iz-1)*dz1D(iz))/(dz1D(iz-1)+dz1D(iz))
        enddo
        rhoz1D(kte+1)=rho1D(kte)
        if(flag_bep)then
         do iz=kts,kte          
          a_e1D(iz)=a_e_bep(ix,iz,iy)
          b_e1D(iz)=b_e_bep(ix,iz,iy)
          dlg1D(iz)=(z1D(iz)+z1D(iz+1))/2.*(1.-frc_urb2d(ix,iy))+dlg_bep(ix,iz,iy)*frc_urb2d(ix,iy)
          dl_u1D(iz)=dl_u_bep(ix,iz,iy)
          if((1.-frc_urb2d(ix,iy)).lt.1.)dl_u1D(iz)=dl_u1D(iz)/frc_urb2d(ix,iy)
          vl1D(iz)=vl_bep(ix,iz,iy)
          sf1D(iz)=sf_bep(ix,iz,iy)
         enddo
         ufrac_int=frc_urb2d(ix,iy)
         sf1D(kte+1)=sf_bep(ix,1,iy)
        else
         do iz=kts,kte          
          a_e1D(iz)=0.        
          b_e1D(iz)=0.
          dlg1D(iz)=(z1D(iz)+z1D(iz+1))/2.
          dl_u1D(iz)=0.
          vl1D(iz)=1.
          sf1D(iz)=1.
         enddo
         ufrac_int=0.
         sf1D(kte+1)=1.
        endif

! compute the pbl_height
        call pbl_height(kms,kme,kts,kte,dz1d,z1d,th1D,q1D,pblh(ix,iy))

! compute the values of wstar
        wts=max(0.,hfx(ix,iy)/rho1D(1)/cp)
        wstar=(g*wts*pblh(ix,iy)/th01D(1))**(1./3.)
        if (wts .ne. 0.0) then
          tstar_w=wts/wstar
        else
          tstar_w=0.0
        endif
        t2_1D=0.
        w2_1D=0.
        gamma1D=0.
! compute the variances
         do iz=kts+1,kte
	   zzi=z1D(iz)/pblh(ix,iy)
	   t2_1D(iz)=1.8*(zzi**(-2./3.))*(tstar_w**2.)
	   w2_1D(iz)=1.8*(zzi**(2./3.))*((1.-0.8*zzi)**2.)*(wstar**2.)
        enddo

! compute gamma 
     
 
       if(igamma.eq.1)then
! (Troen and Mahrt)
        do iz=kts+1,kte
         if(z1D(iz).le.1.0*pblh(ix,iy).and.wts.gt.0.)then
          gamma1D(iz)=10.*wts/wstar/pblh(ix,iy)
         else
          gamma1D(iz)=0.
         endif
        enddo

       elseif(igamma.eq.2)then
! Deardorff, and Therry -Lacarrere
         do iz=kts+1,kte
	  if(wts.gt.0)then
           if(z1D(iz).le.(1.0*pblh(ix,iy)).and.z1D(iz).gt.(0.1*pblh(ix,iy)))then
            gamma1D(iz)=g/th01D(iz)*t2_1D(iz)/w2_1D(iz)
           else
            gamma1D(iz)=0.
           endif
	  endif
         enddo

       elseif(igamma.eq.3)then! (Holtslag and Moeng)
         do iz=kts+1,kte
          if(z1D(iz).le.(1.0*pblh(ix,iy)).and.wts.gt.0)then
           gamma1D(iz)=2.*wstar*wts/w2_1D(iz)/pblh(ix,iy)
          else
           gamma1D(iz)=0.
          endif
         enddo
	  

       endif
       

         call boulac1D(ix,iy,ufrac_int,kms,kme,kts,kte,dz1d,z1D,dt,u1D,v1D,th1D,rho1D,rhoz1D,q1D,th01D,&
                   tke1D,ustar(ix,iy),hfx(ix,iy),qfx(ix,iy),cp,g,        & 
                   a_e1D,b_e1D,                              & 
                   dlg1D,dl_u1D,sf1D,vl1D,dlk1D,dls1D,exch1D,sh1D,bu1D,gamma1D)                    

         

! store turbulent exchange coefficients, TKE, and other variables
         
         do iz= kts,kte
          a_e(ix,iz,iy)=a_e1D(iz)
          b_e(ix,iz,iy)=b_e1D(iz)
          if(flag_bep)then
          dlg_bep(ix,iz,iy)=dlg1D(iz)
          endif
          tke(ix,iz,iy)=tke1D(iz)
          dlk(ix,iz,iy)=dlk1D(iz)
          sh(ix,iz,iy)=sh1D(iz)
          bu(ix,iz,iy)=bu1D(iz)
          exch_h(ix,iz,iy)=exch1D(iz)
          exch_m(ix,iz,iy)=exch1D(iz)
         enddo

         if(idiff.ne.1)then

! estimate the tendencies

        if(flag_bep)then         
         do iz=kts,kte          
          a_t1D(iz)=a_t_bep(ix,iz,iy)
          b_t1D(iz)=b_t_bep(ix,iz,iy)
          a_u1D(iz)=a_u_bep(ix,iz,iy)
          b_u1D(iz)=b_u_bep(ix,iz,iy)
          a_v1D(iz)=a_v_bep(ix,iz,iy)
          b_v1D(iz)=b_v_bep(ix,iz,iy)
          a_q1D(iz)=a_q_bep(ix,iz,iy)
          b_q1D(iz)=b_q_bep(ix,iz,iy)
         enddo
        else
         do iz=kts,kte          
          a_t1D(iz)=0.         
          b_t1D(iz)=0.
          a_u1D(iz)=0.        
          b_u1D(iz)=0.
          a_v1D(iz)=0.         
          b_v1D(iz)=0.
          a_q1D(iz)=0.        
          b_q1D(iz)=0.
         enddo
          b_t1D(1)=hfx(ix,iy)/dz1D(1)/rho1D(1)/cp         
          b_q1D(1)=qfx(ix,iy)/dz1D(1)/rho1D(1)
          a_u1D(1)=(-ustar(ix,iy)*ustar(ix,iy)/dz1D(1)/((u1D(1)**2.+v1D(1)**2.)**.5))
          a_v1D(1)=(-ustar(ix,iy)*ustar(ix,iy)/dz1D(1)/((u1D(1)**2.+v1D(1)**2.)**.5))
        endif

 
!
       
! compute the value of the extra term that will be added to b_t1D
        do iz=kts+1,kte
         if(z1D(iz).le.1.0*pblh(ix,iy).and.wts.gt.0.)then
          b_t1D(iz)=b_t1D(iz)-(exch1D(iz+1)*gamma1D(iz+1)-exch1D(iz)*gamma1D(iz))/dz1D(iz)
         endif
        enddo
       
    
!


         
! solve diffusion equation for momentum x component          
       call diff(kms,kme,kts,kte,1,1,dt,u1D,rho1D,rhoz1D,exch1D,a_u1D,b_u1D,sf1D,vl1D,dz1D,wu1D)
        
! solve diffusion equation for momentum y component          
       call diff(kms,kme,kts,kte,1,1,dt,v1D,rho1D,rhoz1D,exch1D,a_v1D,b_v1D,sf1D,vl1D,dz1D,wv1D)

! solve diffusion equation for potential temperature        
       call diff(kms,kme,kts,kte,1,1,dt,th1D,rho1D,rhoz1D,exch1D,a_t1D,b_t1D,sf1D,vl1D,dz1D,wt1D)

! solve diffusion equation for water vapor mixing ratio     
       call diff(kms,kme,kts,kte,1,1,dt,q1D,rho1D,rhoz1D,exch1D,a_q1D,b_q1D,sf1D,vl1D,dz1D,wq1D)

! solve diffusion equation for liquid water mixing ratio     
       call diff(kms,kme,kts,kte,1,1,dt,qc1D,rho1D,rhoz1D,exch1D,a_qc1D,b_qc1D,sf1D,vl1D,dz1D,wqc1D)

! compute the tendencies
                             
         do iz= kts,kte
          rthblten(ix,iz,iy)=rthblten(ix,iz,iy)+(th1D(iz)-th_phy(ix,iz,iy))/dt
          rqvblten(ix,iz,iy)=rqvblten(ix,iz,iy)+(q1D(iz)-qv_curr(ix,iz,iy))/dt
          rqcblten(ix,iz,iy)=rqcblten(ix,iz,iy)+(qc1D(iz)-qc_curr(ix,iz,iy))/dt
          rublten(ix,iz,iy)=rublten(ix,iz,iy)+(u1D(iz)-u_phy(ix,iz,iy))/dt
          rvblten(ix,iz,iy)=rvblten(ix,iz,iy)+(v1D(iz)-v_phy(ix,iz,iy))/dt  
          wu(ix,iz,iy)=wu1D(iz)
          wv(ix,iz,iy)=wv1D(iz) 
          wt(ix,iz,iy)=wt1D(iz)
          wq(ix,iz,iy)=wq1D(iz)      
        enddo
      endif
   
      enddo  ! iy
      enddo  ! ix

  
      return
      end subroutine boulac
            

! ===6=8===============================================================72

! ===6=8===============================================================72

      subroutine boulac1D(ix,iy,ufrac_int,kms,kme,kts,kte,dz,z,dt,u,v,th,rho,rhoz,qa,th0,te,    &
                   ustar,hfx,qfx,cp,g,                               & 
                   a_e,b_e,                        & 
                   dlg,dl_u,sf,vl,dlk,dls,exch,sh,bu,gamma)                           

! ----------------------------------------------------------------------
! 1D resolution of TKE following Bougeault and Lacarrere
! ----------------------------------------------------------------------

      implicit none

      integer iz,ix,iy

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------


      integer kms,kme,kts,kte                 
      real z(kms:kme)               ! Altitude above the ground of the cell interfaces.
      real dz(kms:kme)                ! vertical resolution
      real u(kms:kme)                ! Wind speed in the x direction
      real v(kms:kme)                ! Wind speed in the y direction
      real th(kms:kme)                ! Potential temperature
      real rho(kms:kme)                ! Air density
      real g                     ! gravity
      real cp                    !  
      real te(kms:kme)                ! turbulent kinetic energy
      real qa(kms:kme)                ! air humidity
      real th0(kms:kme)               ! Reference potential temperature 
      real dt                    ! Time step
      real ustar                 ! ustar
      real hfx                   ! sensbile heat flux
      real qfx                   ! kinematic latent heat flux
      real sf(kms:kme)              ! surface of the urban grid cells
      real vl(kms:kme)                ! volume of the urban grid cells
      real a_e(kms:kme)               ! Implicit component of the TKE sources or sinks
      real b_e(kms:kme)               ! Explicit component of the TKE sources or sinks
      real dlg(kms:kme)               ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u(kms:kme)              ! Length scale (lb in formula (22) ofthe BLM paper)
      real ufrac_int             ! urban fraction
! local variables not needed in principle, but that can be used to estimate the budget and turbulent fluxes
 
      real we(kms:kme),dwe(kms:kme)
     
! local variables
      real sh(kms:kme)    ! shear term in TKE eqn.
      real bu(kms:kme)    ! buoyancy term in TKE eqn.
      real gamma(kms:kme)    ! gamma term
      real td(kms:kme)    ! dissipation term in TKE eqn.
      real exch(kms:kme) ! turbulent diffusion coefficients (defined at the faces)
      real dls(kms:kme)   ! dissipation length scale
      real dlk(kms:kme)   ! length scale used to estimate exch
      real dlu(kms:kme)   ! l_up
      real dld(kms:kme)   ! l_down
      real rhoz(kms:kme) !air density at the faces of the cell                
      real tstar     ! derived from hfx and ustar
      real beta
      real summ1,summ2,summ3,summ4
! interpolate air density at the faces

       

! estimation of tstar

        tstar=-hfx/rho(1)/cp/ustar                                                
         
! first compute values of dlu and dld (length scales up and down). 
       
       call dissip_bougeault(ix,iy,g,kms,kme,kts,kte,z,dz,te,dlu,dld,th,th0)
          
!then average them to obtain dls and dlk (length scales for dissipation and eddy coefficients)

       call length_bougeault(ix,iy,kms,kme,kts,kte,dld,dlu,dlg,dl_u,dls,dlk)
            
! compute the turbulent diffusion coefficients exch

       call cdtur_bougeault(ix,iy,kms,kme,kts,kte,te,z,dz,exch,dlk)

! compute source and sink terms in the TKE equation (shear, buoyancy and dissipation)
       
       call tke_bougeault(ix,iy,g,kms,kme,kts,kte,z,dz,vl,u,v,th,te,th0,ustar,tstar,exch,dls,td,sh,bu,gamma,b_e,a_e,sf,ufrac_int)
        
! solve for tke 
      
       call diff(kms,kme,kts,kte,1,1,dt,te,rho,rhoz,exch,a_e,b_e,sf,vl,dz,we)
     
! avoid negative values for tke
  
       do iz=kts,kte
        if(te(iz).lt.temin) te(iz)=temin
       enddo 
      
       return
       end subroutine boulac1d
! 
! ===6=8===============================================================72

! ===6=8===============================================================72
         subroutine dissip_bougeault(ix,iy,g,kms,kme,kts,kte,z,dz,te,dlu,dld,th,th0)
! compute the length scales up and down
         implicit none
         integer kms,kme,kts,kte,iz,izz,ix,iy
         real dzt,zup,beta,zup_inf,bbb,tl,zdo,zdo_sup,zzz,g
         real te(kms:kme),dlu(kms:kme),dld(kms:kme),dz(kms:kme)
         real z(kms:kme),th(kms:kme),th0(kms:kme)

         do iz=kts,kte
          zup=0.
          dlu(iz)=z(kte+1)-z(iz)-dz(iz)/2.
          zzz=0.
          zup_inf=0.
          beta=g/th0(iz)      !Buoyancy coefficient
          do izz=iz,kte-1
           dzt=(dz(izz+1)+dz(izz))/2.
           zup=zup-beta*th(iz)*dzt
           zup=zup+beta*(th(izz+1)+th(izz))*dzt/2.
           zzz=zzz+dzt
           if(te(iz).lt.zup.and.te(iz).ge.zup_inf)then
            bbb=(th(izz+1)-th(izz))/dzt
            if(bbb.ne.0)then
             tl=(-beta*(th(izz)-th(iz))+sqrt( max(0.,(beta*(th(izz)-th(iz)))**2.+2.*bbb*beta*(te(iz)-zup_inf))))/bbb/beta
            else
             if(th(izz).ne.th(iz))then
              tl=(te(iz)-zup_inf)/(beta*(th(izz)-th(iz)))
             else
              tl=0.
             endif
            endif            
            dlu(iz)=zzz-dzt+tl
           endif
           zup_inf=zup
          enddo
                  
          zdo=0.
          zdo_sup=0.
          dld(iz)=z(iz)+dz(iz)/2.
          zzz=0.
          do izz=iz,kts+1,-1
           dzt=(dz(izz-1)+dz(izz))/2.
           zdo=zdo+beta*th(iz)*dzt
           zdo=zdo-beta*(th(izz-1)+th(izz))*dzt/2.
           zzz=zzz+dzt
           if(te(iz).lt.zdo.and.te(iz).ge.zdo_sup)then
            bbb=(th(izz)-th(izz-1))/dzt
            if(bbb.ne.0.)then
             tl=(beta*(th(izz)-th(iz))+sqrt( max(0.,(beta*(th(izz)-th(iz)))**2.+2.*bbb*beta*(te(iz)-zdo_sup))))/bbb/beta
            else
             if(th(izz).ne.th(iz))then
              tl=(te(iz)-zdo_sup)/(beta*(th(izz)-th(iz)))
             else
              tl=0.
             endif
            endif
            
            dld(iz)=zzz-dzt+tl
           endif
           zdo_sup=zdo
          enddo
         enddo
            
                   
         end subroutine dissip_bougeault
!
! ===6=8===============================================================72 
! ===6=8===============================================================72
         subroutine length_bougeault(ix,iy,kms,kme,kts,kte,dld,dlu,dlg,dl_u,dls,dlk)
! compute the length scales for dissipation and turbulent coefficients
         implicit none
         integer kms,kme,kts,kte,iz,ix,iy
         real dlu(kms:kme),dld(kms:kme),dl_u(kms:kme)
         real dls(kms:kme),dlk(kms:kme),dlg(kms:kme)
         
         do iz=kts,kte
          dld(iz)=min(dld(iz),dlg(iz))
          dls(iz)=sqrt(dlu(iz)*dld(iz))
          dlk(iz)=min(dlu(iz),dld(iz))

         if(dl_u(iz).gt.0.)then               
           dls(iz)=1./(1./dls(iz)+1./dl_u(iz))
           dlk(iz)=1./(1./dlk(iz)+1./dl_u(iz))             
          endif
         enddo 
                   
         return
         end subroutine length_bougeault
!

! ===6=8===============================================================72 
! ===6=8===============================================================72

       subroutine cdtur_bougeault(ix,iy,kms,kme,kts,kte,te,z,dz,exch,dlk)
! compute turbulent coefficients
       implicit none
       integer iz,kms,kme,kts,kte,ix,iy
       real te_m,dlk_m
       real te(kms:kme),exch(kms:kme)
       real dz(kms:kme),z(kms:kme)
       real dlk(kms:kme)
       real fact

       exch(kts)=0.

!       do iz=2,nz-1
       do iz=kts+1,kte
        te_m=(te(iz-1)*dz(iz)+te(iz)*dz(iz-1))/(dz(iz)+dz(iz-1))
        dlk_m=(dlk(iz-1)*dz(iz)+dlk(iz)*dz(iz-1))/(dz(iz)+dz(iz-1))
        exch(iz)=ck_b*dlk_m*sqrt(te_m)
!        exch(iz)=max(exch(iz),0.0001)    
        exch(iz)=max(exch(iz),0.1) 
       enddo

       exch(kte+1)=0.1

       return
       end subroutine cdtur_bougeault


! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine diff(kms,kme,kts,kte,iz1,izf,dt,co,rho,rhoz,cd,aa,bb,sf,vl,dz,fc)


!------------------------------------------------------------------------
!           Calculation of the diffusion in 1D        
!------------------------------------------------------------------------
!  - Input:
!       nz    : number of points
!       iz1   : first calculated point
!       co    : concentration of the variable of interest
!       dz    : vertical levels
!       cd    : diffusion coefficients
!       dtext : external time step
!       rho    : density of the air at the center
!       rhoz   : density of the air at the face
!       itest : if itest eq 1 then update co, else store in a flux array
!  - Output:
!       co :concentration of the variable of interest

!  - Internal:
!       cddz  : constant terms in the equations 
!       dt    : diffusion time step
!       nt    : number of the diffusion time steps
!       cstab : ratio of the stability condition for the time step
!---------------------------------------------------------------------

        implicit none
        integer iz,iz1,izf
        integer kms,kme,kts,kte
        real dt,dzv               
        real co(kms:kme),cd(kms:kme),dz(kms:kme)
        real rho(kms:kme),rhoz(kms:kme)
        real cddz(kms:kme+1),fc(kms:kme),df(kms:kme)
        real a(kms:kme,3),c(kms:kme)
        real sf(kms:kme),vl(kms:kme)
        real aa(kms:kme),bb(kms:kme)
        

! Compute cddz=2*cd/dz  
        
        cddz(kts)=sf(kts)*rhoz(kts)*cd(kts)/dz(kts)
        do iz=kts+1,kte
         cddz(iz)=2.*sf(iz)*rhoz(iz)*cd(iz)/(dz(iz)+dz(iz-1))
        enddo
        cddz(kte+1)=sf(kte+1)*rhoz(kte+1)*cd(kte+1)/dz(kte)

         do iz=kts,iz1-1
          a(iz,1)=0.
          a(iz,2)=1.
          a(iz,3)=0.
          c(iz)=co(iz)
         enddo
          
          do iz=iz1,kte-izf
           dzv=vl(iz)*dz(iz)
           a(iz,1)=-cddz(iz)*dt/dzv/rho(iz)
           a(iz,2)=1+dt*(cddz(iz)+cddz(iz+1))/dzv/rho(iz)-aa(iz)*dt
           a(iz,3)=-cddz(iz+1)*dt/dzv/rho(iz)
           c(iz)=co(iz)+bb(iz)*dt                     
          enddo
          
          do iz=kte-(izf-1),kte
           a(iz,1)=0.
           a(iz,2)=1
           a(iz,3)=0.
           c(iz)=co(iz)
          enddo
           
          call invert (kms,kme,kts,kte,a,c,co)
         
          do iz=kts,iz1 
           fc(iz)=0.
          enddo
                       
          do iz=iz1+1,kte 
           fc(iz)=-(cddz(iz)*(co(iz)-co(iz-1)))/rho(iz)
          enddo
        
!          do iz=1,iz1
!           df(iz)=0.
!          enddo
!          
!          do iz=iz1+1,nz-izf
!           dzv=vl(iz)*dz(iz)
!           df(iz)=+(co(iz-1)*cddz(iz)-co(iz)*(cddz(iz)+cddz(iz+1))+co(iz+1)*cddz(iz+1))/dzv/rho(iz)
!          enddo
!          
!          do iz=nz-izf,nz 
!           df(iz)=0.
!          enddo
                                        
       return
       end subroutine diff

! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine buoy(ix,iy,g,kms,kme,kts,kte,th,th0,exch,dz,bu,gamma,ustar,tstar,ufrac_int)
! compute buoyancy term
       implicit none
       integer kms,kme,kts,kte,iz,ix,iy
       real dtdz1,dtdz2,cdm,dtmdz,g      
       real th(kms:kme),exch(kms:kme),dz(kms:kme),bu(kms:kme),gamma(kms:kme)
       real th0(kms:kme),ustar,tstar,ufrac_int,gammam
        
!       bu(1)=-ustar*tstar*g/th0(1)*(1.-ufrac_int) 
      bu(kts)=0. 
       
        
       do iz=kts+1,kte-1       
        dtdz1=2.*(th(iz)-th(iz-1))/(dz(iz-1)+dz(iz))
        dtdz2=2.*(th(iz+1)-th(iz))/(dz(iz+1)+dz(iz))                  
        dtmdz=0.5*(dtdz1+dtdz2)
        cdm=0.5*(exch(iz+1)+exch(iz))
        gammam=0.5*(gamma(iz+1)+gamma(iz))
        bu(iz)=-cdm*(dtmdz-gammam)*g/th0(iz)         
       enddo
!
                 
       bu(kte)=0.
         
       return
       end subroutine buoy

! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine shear(ix,iy,g,kms,kme,kts,kte,u,v,cdua,dz,sh,ustar,tstar,th,ufrac_int)
! compute shear term
       implicit none
       integer kms,kme,kts,kte,iz,ix,iy
       real dudz1,dudz2,dvdz1,dvdz2,cdm,dumdz,ustar
       real tstar,th,al,phim,g      
       real u(kms:kme),v(kms:kme),cdua(kms:kme),dz(kms:kme),sh(kms:kme)
       real u1,u2,v1,v2,ufrac_int

!       al=vk*g*tstar/(th*(ustar**2.))
!       if(al.ge.0.)phim=1.+4.7*dz(1)/2.*al
!       if(al.lt.0.)phim=(1.-15*dz(1)/2.*al)**(-0.25)       
!        
!       sh(1)=(ustar**3.)/vk/(dz(1)/2.)*(1.-ufrac_int)       
       sh(kts)=0.
       do iz=kts+1,kte-1
        u2=(dz(iz+1)*u(iz)+dz(iz)*u(iz+1))/(dz(iz)+dz(iz+1))
        u1=(dz(iz)*u(iz-1)+dz(iz-1)*u(iz))/(dz(iz-1)+dz(iz))
        v2=(dz(iz+1)*v(iz)+dz(iz)*v(iz+1))/(dz(iz)+dz(iz+1))
        v1=(dz(iz)*v(iz-1)+dz(iz-1)*v(iz))/(dz(iz-1)+dz(iz))        
        cdm=0.5*(cdua(iz)+cdua(iz+1)) 
        dumdz=((u2-u1)/dz(iz))**2.+((v2-v1)/dz(iz))**2.            
        sh(iz)=cdm*dumdz                          
       enddo

!!!!!!!
       sh(kte)=0.
       
       return
       end subroutine shear

! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine invert(kms,kme,kts,kte,a,c,x)
       
!ccccccccccccccccccccccccccccccc       
! Aim: Inversion and resolution of a tridiagonal matrix
!          A X = C
! Input:
!  a(*,1) lower diagonal (Ai,i-1)
!  a(*,2) principal diagonal (Ai,i)
!  a(*,3) upper diagonal (Ai,i+1)
!  c      
! Output
!  x     results
!ccccccccccccccccccccccccccccccc

       implicit none
       integer in
       integer kts,kte,kms,kme
       real a(kms:kme,3),c(kms:kme),x(kms:kme)                       
        
        do in=kte-1,kts,-1                 
         c(in)=c(in)-a(in,3)*c(in+1)/a(in+1,2)
         a(in,2)=a(in,2)-a(in,3)*a(in+1,1)/a(in+1,2)
        enddo
        
        do in=kts+1,kte        
         c(in)=c(in)-a(in,1)*c(in-1)/a(in-1,2)
        enddo
        
        do in=kts,kte
          
         x(in)=c(in)/a(in,2)
          
        enddo

        return
        end subroutine invert
  
! ===6=8===============================================================72
! ===6=8===============================================================72
              
       subroutine tke_bougeault(ix,iy,g,kms,kme,kts,kte,z,dz,vl,u,v,th,te,th0,ustar,tstar,exch,   &
                         dls,td,sh,bu,gamma,b_e,a_e,sf,ufrac_int)
! in this routine the shear, buoyancy and part of the dissipation terms
! of the TKE equation are computed       

       implicit none
       integer kms,kme,kts,kte,iz,ix,iy
       real g,ustar,tstar,ufrac_int
       real z(kms:kme),dz(kms:kme),u(kms:kme),v(kms:kme),th(kms:kme),th0(kms:kme),te(kms:kme)
       real exch(kms:kme),dls(kms:kme),td(kms:kme),sh(kms:kme),bu(kms:kme),gamma(kms:kme)
       real a_e(kms:kme),b_e(kms:kme)
       real vl(kms:kme),sf(kms:kme)
       real te1,dl1
    
       call shear(ix,iy,g,kms,kme,kts,kte,u,v,exch,dz,sh,ustar,tstar,th(kts),ufrac_int)
      
       call buoy(ix,iy,g,kms,kme,kts,kte,th,th0,exch,dz,bu,gamma,ustar,tstar,ufrac_int)
     
       do iz=kts,kte          
        te1=max(te(iz),temin)
        dl1=max(dls(iz),0.1)
        td(iz)=-ceps_b*sqrt(te1)/dl1
        sh(iz)=sh(iz)*sf(iz)
        bu(iz)=bu(iz)*sf(iz)
        a_e(iz)=a_e(iz)+td(iz)       
        b_e(iz)=b_e(iz)+sh(iz)+bu(iz)
       enddo 

         
       return
       end subroutine tke_bougeault    

! ===6=8===============================================================72
      SUBROUTINE BOULACINIT(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,          &
     &                      TKE_PBL,EXCH_H,RESTART,ALLOWED_TO_READ,     &
     &                      IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE                 )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) ::    EXCH_H, &
     &                                                         RUBLTEN, &
     &                                                         RVBLTEN, &
     &                                                        RTHBLTEN, &
     &                                                        RQVBLTEN, &
     &                                                        RQCBLTEN, &
     &                                                         TKE_PBL
      INTEGER :: I,J,K,ITF,JTF,KTF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)

      IF(.NOT.RESTART)THEN
        DO J=JTS,JTF
        DO K=KTS,KTF
        DO I=ITS,ITF
          TKE_PBL(I,K,J)=0.0001
          RUBLTEN(I,K,J)=0.
          RVBLTEN(I,K,J)=0.
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          RQCBLTEN(I,K,J)=0.
          EXCH_H(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      END SUBROUTINE BOULACINIT
!######################################################################
       subroutine pbl_height(kms,kme,kts,kte,dz,z,th,q,pblh)

! this routine computes the PBL height
! with an approach similar to MYNN
       implicit none
       integer kms,kme,kts,kte,iz
       real z(kms:kme),dz(kms:kme),th(kms:kme),q(kms:kme)
       real pblh
!Local
       real thv(kms:kme),zc(kms:kme)
       real thsfc

! compute the height of the center of the grid cells
      do iz=kts,kte
       zc(iz)=z(iz)+dz(iz)/2.
      enddo

! compute the virtual potential temperature

       do iz=kts,kte
        thv(iz)=th(iz)*(1.+0.61*q(iz))
       enddo
! now compute the PBL height

       pblh=0.
       thsfc=thv(kts)+0.5
       do iz=kts+1,kte
        if(pblh.eq.0.and.thv(iz).gt.thsfc)then
         pblh=zc(iz-1)+(thsfc-thv(iz-1))/(max(0.01,thv(iz)-thv(iz-1)))*(zc(iz)-zc(iz-1))
!         pblh=z(iz-1)+(thsfc-thv(iz-1))/(max(0.01,thv(iz)-thv(iz-1)))*(z(iz)-z(iz-1))
        endif
       enddo

       return
       end subroutine pbl_height


! ===6=8===============================================================72



END MODULE module_bl_boulac

