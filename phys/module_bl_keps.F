MODULE module_bl_keps

      real      vk,temin,dmin,tpemin,c1,c2,c3,c_mu,Pr    ! constant from Zonato et al.,2022    
      parameter(temin=1E-4,dmin=1E-7,tpemin=1E-7,c_theta=0.09,  &
                vk=0.4,c_mu=0.09,c1=1.44,c2=1.92,c3=1.44,c4=0.27,c5=0.08,sigma_eps=1.3) ! impose minimum values for tke similar to those of MYJ
      parameter(temax=50.,dmax=50.,tpemax=5.)
      integer,private,parameter:: bl_keps_adv = 0
! -----------------------------------------------------------------------     


   CONTAINS
 
      subroutine keps(mol,tsk,xtime,frc_urb2d,flag_bep,dz8w,dt,u_phy,v_phy,th_phy &
                      ,PI_PHY,RTHRATEN,P8W                          &
                      ,rho,qv_curr,qc_curr,hfx                  &
                      ,qfx,ustar,cp,g                           &
                      ,rublten,rvblten,rthblten                 &
                      ,rqvblten,rqcblten                        & 
                      ,tke_pbl,diss_pbl,tpe_pbl                 &
                      ,tke_adv,diss_adv,tpe_adv                 &
                      ,pr_pbl                   &
                      ,wu,wv,wt,wq,exch_h,exch_m,pblh           &
                      ,a_u_bep,a_v_bep,a_t_bep,a_q_bep          &
                      ,b_u_bep,b_v_bep                          &
                      ,b_t_bep,b_q_bep                          &
                      ,b_e_bep                                  &
                      ,sf_bep,vl_bep                            &   
                      ,br,znt,psim,psih                         &             
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
 

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   DZ8W      !vertical resolution       
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   qv_curr   !moisture  
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   qc_curr   !liquid water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   RHO       !air density
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   TH_PHY    !potential temperature
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   PI_PHY    
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   RTHRATEN
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   P8W     
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   U_PHY     !x-component of wind
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::   V_PHY     !y-component of wind
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   ustar              !friction velocity
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   hfx                !sensible heat flux (W/m2) at surface 
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   qfx                !moisture flux at surface
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   tsk,mol                !moisture flux at surface
   real,  INTENT(IN   )    :: g,cp                                              !gravity and Cp
   REAL, INTENT(IN )::   DT                                                      ! Time step
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    :: br
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    :: znt
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   FRC_URB2D          !fraction cover urban
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)    ::   PBLH          !PBL height
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    ::   psim,psih

! variable added for urban
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_u_bep        ! Implicit component for the momemtum in X-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_v_bep        ! Implicit component for the momemtum in Y-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_t_bep        ! Implicit component for the Pot. Temp.
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::a_q_bep        ! Implicit component for Moisture
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_u_bep        ! Explicit component for the momemtum in X-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_v_bep        ! Explicit component for the momemtum in Y-direction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_t_bep        ! Explicit component for the Pot. Temp.
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_q_bep        ! Explicit component for Moisture
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   )    ::b_e_bep        ! Explicit component for Moisture

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
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  tke_pbl  ! Turbulence Kinetic Energy 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  diss_pbl  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  tpe_pbl  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  pr_pbl  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  tke_adv  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  diss_adv  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(INOUT   )  ::  tpe_adv  ! Dissipation Rate
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wu  ! Turbulent flux of momentum (x) 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wv  ! Turbulent flux of momentum (y) 
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wt  ! Turbulent flux of temperature
        real, dimension( ims:ime, kms:kme, jms:jme ), INTENT(OUT   )    ::  wq  ! Turbulent flux of water vapor

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
      real tke1D(kms:kme)               ! Turbulent Kinetic Energy
      real diss1D(kms:kme)              ! Dissipation Rate
      real tpe1D(kms:kme)
      real th01D(kms:kme)               ! reference potential temperature
      real exch1D(kms:kme)            ! exch
      real sf1D(kms:kme)              ! surface of the grid cells
      real vl1D(kms:kme)                ! volume of the  grid cells
      real a_u1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the X-direction
      real a_v1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the Y-direction
      real a_t1D(kms:kme)               ! Implicit component of the heat sources or sinks
      real a_q1D(kms:kme)               ! Implicit component of the moisture sources or sinks
      real a_qc1D(kms:kme)               ! Implicit component of the liquid water sources or sinks
      real b_u1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the X-direction
      real b_v1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the Y-direction
      real b_t1D(kms:kme)               ! Explicit component of the heat sources or sinks
      real b_q1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real b_qc1D(kms:kme)               ! Explicit component of the liquid water sources or sinks
      real b_e1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real a_e1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real b_d1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real a_d1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real b_tpe1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real a_tpe1D(kms:kme)               ! Explicit component of the moisture sources or sinks
      real sh1D(kms:kme)              ! shear
      real bu1D(kms:kme)              ! buoyancy
      real wu1D(kms:kme)              ! turbulent flux of momentum (x component)
      real wv1D(kms:kme)              ! turbulent flux of momentum (y component)
      real wt1D(kms:kme)              ! turbulent flux of temperature
      real wq1D(kms:kme)              ! turbulent flux of water vapor
      real wqc1D(kms:kme)              ! turbulent flux of liquid water 
      integer iz_pbl(ims:ime,jms:jme)
      real pr1D(kms:kme)
      real raten1D(kms:kme)
      real p8w1D(kms:kme)
      real pi_1D(kms:kme)
! local added only for diagnostic output
      real bu(ims:ime,kms:kme,jms:jme) ! buoyancy term in TKE
      real sh(ims:ime,kms:kme,jms:jme) ! shear term in TKE
      real wrk(ims:ime) ! working array
      integer ix,iy,iz,id
      real ufrac_int                                              ! urban fraction     
      real psim1D,psih1D,znt1D,br1D,sflux
      integer xtime
      
!
!    

 


!here I fix the value of the reference state equal to the value of the potnetial temperature
! the only use of this variable in the code is to compute the paramter BETA = g/th0
! I fix it to 300K. 
         rublten=0.
         rvblten=0.
         rthblten=0.
         rqvblten=0.
         rqcblten=0.
 
        do ix=its,ite
        do iy=jts,jte        
        do iz=kts,kte
         th_0(ix,iz,iy)=300.
        enddo
        enddo
        enddo
       

       z1D=0.               
       dz1D=0.              
       u1D =0.               
       v1D =0.                
       th1D=0.              
       q1D=0.                
       rho1D=0.              
       rhoz1D=0.   
       tke1D =0.  
       diss1D=0.
       tpe1D=0. 
       raten1D=0.
       pi_1D=0.  
       p8w1D=0.  
       th01D =0.             
       exch1D=0.            
       sf1D  =1.            
       vl1D  =1.             
       a_u1D =0.              
       a_v1D =0.              
       a_t1D =0.              
       a_q1D =0. 
       a_qc1D =0.              
       b_u1D =0.             
       b_v1D =0.              
       b_t1D =0.            
       b_q1D =0.
       b_qc1D =0.  
       b_e1D=0.
       a_e1D=0.
       b_d1D=0.
       a_d1D=0.
       b_tpe1D=0.
       a_tpe1D=0.
       sh1D  =0.            
       bu1D  =0.            
       wu1D  =0.           
       wv1D  =0.            
       wt1D =0.                        
       wq1D =0.          
       iz_pbl=0
       psim1D=0.
       psih1D=0.
 

       do ix=its,ite
       do iy=jts,jte
         z1d(kts)=0.
         br1D=br(ix,iy)
         sflux=hfx(ix,iy)/rho(ix,1,iy)/cp+qfx(ix,iy)/rho(ix,1,iy)*(461./287.-1)*th_phy(ix,1,iy)
         znt1D=znt(ix,iy)
         psim1D=psim(ix,iy)
         psih1D=psih(ix,iy)
         do iz= kts,kte
         if(xtime.le.2)then
          tke_pbl(ix,iz,iy)=temin
          diss_pbl(ix,iz,iy)=dmin
          tpe_pbl(ix,iz,iy)=tpemin
          tke_adv(ix,iz,iy)=temin
          diss_adv(ix,iz,iy)=dmin
          tpe_adv(ix,iz,iy)=tpemin
          pr_pbl(ix,iz,iy)=1.
         endif

	  u1D(iz)=u_phy(ix,iz,iy)
	  v1D(iz)=v_phy(ix,iz,iy)
	  th1D(iz)=th_phy(ix,iz,iy)
          q1D(iz)=qv_curr(ix,iz,iy)
          qc1D(iz)=qc_curr(ix,iz,iy)
	  rho1D(iz)=rho(ix,iz,iy)	   
	  th01D(iz)=th_0(ix,iz,iy)	  
          dz1D(iz)=dz8w(ix,iz,iy)
          pi_1D(iz)=pi_phy(ix,iz,iy)
          raten1D(iz)=rthraten(ix,iz,iy)
          p8w1D(iz)=p8w(ix,iz,iy)
          pr1D(iz)=pr_pbl(ix,iz,iy)
          z1D(iz+1)=z1D(iz)+dz1D(iz)
         if(bl_keps_adv==1)then
          tke_pbl(ix,iz,iy)=tke_adv(ix,iz,iy)
          diss_pbl(ix,iz,iy)=diss_adv(ix,iz,iy)
          tpe_pbl(ix,iz,iy)=tpe_adv(ix,iz,iy) 
         endif
          tke1D(iz)=min(temax,max(temin,tke_pbl(ix,iz,iy)))
          diss1D(iz)=min(dmax,max(dmin,diss_pbl(ix,iz,iy)))
          tpe1D(iz)=min(tpemax,max(tpemin,tpe_pbl(ix,iz,iy)))
         enddo
          rhoz1D(kts)=rho1D(kts)
         do iz=kts+1,kte
          rhoz1D(iz)=(rho1D(iz)*dz1D(iz-1)+rho1D(iz-1)*dz1D(iz))/(dz1D(iz-1)+dz1D(iz))
         enddo
          rhoz1D(kte+1)=rho1D(kte)
         do iz=kts,kte          
          vl1D(iz)=1.
          sf1D(iz)=1.
         enddo
         ufrac_int=0.
         sf1D(kte+1)=1.

! compute the pbl_height
          call GET_PBLH(kts,kte,pblh(ix,iy),th1D,tke1D,z1D,dz1D,q1D,iz_pbl(ix,iy))
          !call pbl_height(kms,kme,kts,kte,dz1D,z1D,th1D,q1D,pblh(ix,iy),iz_pbl(ix,iy))

        
! estimate the tendencies
         do iz=kts,kte          
          a_t1D(iz)=0.         
          b_t1D(iz)=0.
          a_u1D(iz)=0.        
          b_u1D(iz)=0.
          a_v1D(iz)=0.         
          b_v1D(iz)=0.
          a_q1D(iz)=0.        
          b_q1D(iz)=0.
          b_e1D(iz)=0.
          a_e1D(iz)=0.
          b_d1D(iz)=0.
          a_d1D(iz)=0.
          b_tpe1D(iz)=0.
          a_tpe1D(iz)=0.
         enddo
          b_t1D(1)=hfx(ix,iy)/dz1D(1)/rho1D(1)/cp         
          b_q1D(1)=qfx(ix,iy)/dz1D(1)/rho1D(1) 
          a_u1D(1)=(-ustar(ix,iy)*ustar(ix,iy)/dz1D(1)/((max(1.E-10,abs(u1D(1)))**2.+max(1.E-10,abs(v1D(1)))**2.)**.5))
          a_v1D(1)=(-ustar(ix,iy)*ustar(ix,iy)/dz1D(1)/((max(1.E-10,abs(u1D(1)))**2.+max(1.E-10,abs(v1D(1)))**2.)**.5))
   
        if(xtime.gt.2)then

        call keps1D(mol(ix,iy),tsk(ix,iy),xtime,ix,iy,ids,ide,jds,jde,kms,kme,kts,kte,dz1D,z1D,dt,                          &
                           u1D,v1D,th1D,pi_1D,raten1D,p8w1D,                                          &
                           rho1D,rhoz1D,q1D,qc1D,th01D,tke1D,diss1D,tpe1D,pr1D,                       &
                           a_u1D,b_u1D,a_v1D,b_v1D,a_t1D,b_t1D,a_q1D,b_q1D,a_qc1D,b_qc1D,             &
                           b_e1D,a_e1D,b_d1D,a_d1D,b_tpe1D,a_tpe1D,psim1D,psih1D,                     &
                           ustar(ix,iy),hfx(ix,iy),qfx(ix,iy),sflux,cp,g,                             &
                           sf1D,vl1D,exch1D,sh1D,bu1D,br1D,znt1D,                                     &
                           pblh(ix,iy),iz_pbl(ix,iy),ufrac_int)
        endif
        
         
         do iz= kts,kte
          tke_pbl(ix,iz,iy)=tke1D(iz)
          diss_pbl(ix,iz,iy)=diss1D(iz)
          tpe_pbl(ix,iz,iy)=tpe1D(iz)
       if(bl_keps_adv==1)then
        tke_adv(ix,iz,iy)=(tke_pbl(ix,iz,iy))
        diss_adv(ix,iz,iy)=diss_pbl(ix,iz,iy)
        tpe_adv(ix,iz,iy)=tpe_pbl(ix,iz,iy)
       endif
          pr_pbl(ix,iz,iy)=pr1D(iz)
          bu(ix,iz,iy)=bu1D(iz)
          exch_h(ix,iz,iy)=exch1D(iz)/pr1D(iz)
          exch_m(ix,iz,iy)=exch1D(iz)
        enddo


! compute the tendencies
                             
         do iz= kts,kte
          rthblten(ix,iz,iy)=rthblten(ix,iz,iy)+(th1D(iz)-th_phy(ix,iz,iy))/dt
          rqvblten(ix,iz,iy)=rqvblten(ix,iz,iy)+(q1D(iz)-qv_curr(ix,iz,iy))/dt
          rqcblten(ix,iz,iy)=rqcblten(ix,iz,iy)+(qc1D(iz)-qc_curr(ix,iz,iy))/dt
          rublten(ix,iz,iy)=rublten(ix,iz,iy)+(u1D(iz)-u_phy(ix,iz,iy))/dt
	  rvblten(ix,iz,iy)=rvblten(ix,iz,iy)+(v1D(iz)-v_phy(ix,iz,iy))/dt
        enddo
     ! endif
  
         wt(ix,kts,iy)=hfx(ix,iy)/rho1D(kts)/cp-rho1D(kts)*(th1D(kts)-th_phy(ix,kts,iy))/dt*dz1D(kts)
         wu(ix,kts,iy)=-ustar(ix,iy)*ustar(ix,iy)-rho1D(kts)*(u1D(kts)-u_phy(ix,kts,iy))/dt*dz1D(kts)
         wv(ix,kts,iy)=-ustar(ix,iy)*ustar(ix,iy)-rho1D(kts)*(u1D(kts)-u_phy(ix,kts,iy))/dt*dz1D(kts)
         do iz=kts+1,kte
          wt(ix,iz,iy)=wt(ix,iz-1,iy)-rho1D(iz)*(th1D(iz)-th_phy(ix,iz,iy))/dt*dz1D(iz)
          wu(ix,iz,iy)=wu(ix,iz-1,iy)-rho1D(iz)*(u1D(iz)-u_phy(ix,iz,iy))/dt*dz1D(iz)
          wv(ix,iz,iy)=wv(ix,iz-1,iy)-rho1D(iz)*(v1D(iz)-v_phy(ix,iz,iy))/dt*dz1D(iz)
        enddo

 
      enddo  ! iy


      enddo  ! ix

  
      return
      end subroutine keps
            

! ===6=8===============================================================72

! ===6=8===============================================================72

   
                                              
     subroutine  keps1D(mol,tsk,xtime,ix,iy,ids,ide,jds,jde,kms,kme,kts,kte,dz,z,dt,          &
                        u,v,th,pi1d,raten,p8w1D,rho,rhoz,qa,qc,th0,te,d,tpe,Pra,      &
                        a_u,b_u,a_v,b_v,a_t,b_t,a_q,b_q,a_qc,b_qc,b_e,a_e,            &
                        b_d,a_d,a_tpe,b_tpe,                                          &
                        psim,psih,ustar,hfx,qfx,sflux,cp,g,                          &
                        sf,vl,exch,sh,bu,b_ric,z0,pblh,iz_pbl,ufrac_int)

! ----------------------------------------------------------------------
! 1D resolution of TKE and dissipation rate following k epsilon turbulent closure
! ----------------------------------------------------------------------

      implicit none
      integer iz,ix,iy,xtime
      integer kms,kme,kts,kte 
      integer ids,ide,jds,jde              
      real z(kms:kme)               ! Altitude above the ground of the cell interfaces.
      real dz(kms:kme)                ! vertical resolution
      real dt,time                         ! Time step
      real pblh
      real tsk,mol
      real u(kms:kme)                ! Wind speed in the x direction
      real v(kms:kme)                ! Wind speed in the y direction
      real th(kms:kme)                ! Potential temperature
      real rho(kms:kme)                ! Air density
      real rhoz(kms:kme) !air density at the faces of the cell 
      real qa(kms:kme)                ! air humidity
      real qc(kms:kme)                ! air humidity
      real th0(kms:kme)               ! Reference potential temperature 
      real te(kms:kme)                ! turbulent kinetic energy
      real d(kms:kme)                 ! dissipation rate
      real tpe(kms:kme)
      real pi1d(kms:kme)
      real raten(kms:kme) 
      real p8w1D(kms:kme)  
      real a_u(kms:kme)
      real b_u(kms:kme)
      real a_v(kms:kme)
      real b_v(kms:kme)
      real a_t(kms:kme)
      real b_t(kms:kme)
      real a_q(kms:kme)
      real b_q(kms:kme)
      real a_qc(kms:kme)
      real b_qc(kms:kme)
      real a_e(kms:kme) 
      real b_e(kms:kme)
      real a_d(kms:kme)
      real b_d(kms:kme)
      real a_tpe(kms:kme)
      real b_tpe(kms:kme)
      real ufrac_int
      real ustar                 ! ustar
      real hfx                   ! sensbile heat flux
      real qfx                   ! kinematic latent heat flux
      real g                     ! gravity
      real cp                    !  
      real sf(kms:kme)              ! surface of the urban grid cells
      real vl(kms:kme)                ! volume of the urban grid cells
      real exch(kms:kme) ! turbulent diffusion coefficients (defined at the faces)
      real sh(kms:kme)    ! shear term in TKE eqn.
      real bu(kms:kme)    ! buoyancy term in TKE eqn.
      real b_ric,z0,sflux
      integer iz_pbl
      real we(kms:kme)
      real wd(kms:kme)
      real wtpe(kms:kme)
      real wu(kms:kme)
      real wv(kms:kme)
      real wt(kms:kme)
      real wq(kms:kme)
      real wqc(kms:kme)
      real Ri(kms:kme)
      real Pra(kms:kme),coeff(kms:kme)
      real Pra_st(kms:kme)
      real exch_h(kms:kme)
      real nsquare(kms:kme)
      real dls(kms:kme)
      real psim,psih
      real phim,phih,phieps
      real dtmdz(kms:kme)
       
       call shear(kms,kme,kts,kte,u,v,dz,sh,sf)
      
       call c_nsquare(Pra,ustar,sflux,z0,tsk,mol,pi1d,g,kms,kme,kts,kte,th,th0,dz,nsquare,dtmdz)
       
       call surface_bl_pra_ri(kms,kme,kts,kte,dz,z,rho,g,cp,z0,sflux,raten,pi1d,p8w1D,   &
                              th,qa,sh,nsquare,Ri,b_ric,psim,psih,pblh,ustar, &
                              iz_pbl,Pra,Pra_st,phim,phih,phieps,dtmdz(kts))  
       
       call  buoy(g,kms,kme,kts,kte,th,th0,dz,bu,te,tpe,Pra)
       
       call  cdtur_ke(kms,kme,kts,kte,te,d,z,dz,exch,exch_h,Pra_st)
       
       call  length_bougeault(g,kms,kme,kts,kte,z,dz,te,th,th0,dls)
!solve analytically the equation for tke and dissipation,following Zonato et
!al.2022
        call thetaphi_alphabeta(kms,kme,kts,kte,z0,dz,sh,bu,te,d,dt,Pra,dls,ustar,phieps,phim,Ri,nsquare,a_d)
! solve diffusion equation for tke
        call diff(kms,kme,kts,kte,2,1,dt,te,rho,rhoz,exch,a_e,b_e,sf,vl,dz,we)
! solve diffusion equation for dissipation rate
        call diff(kms,kme,kts,kte,2,1,dt,d,rho,rhoz,exch/sigma_eps,a_d,b_d,sf,vl,dz,wd)     

!calculation of the terms for turbulent potential energy equation and addition
!of the countergradient term to tke
       call terms_tpe(dtmdz,g,kms,kme,kts,kte,th0,te,d,tpe,a_tpe,b_tpe,Pra,coeff) 
       call calc_countergradient(dz,g,kms,kme,kts,kte,th0,te,d,tpe,Pra,b_t)
!solve diffusion equation for turbulent potential energy
       call diff(kms,kme,kts,kte,1,1,dt,tpe,rho,rhoz,exch,a_tpe,b_tpe,sf,vl,dz,wtpe)
     
! solve diffusion equation for momentum x component          
        call diff(kms,kme,kts,kte,1,1,dt,u,rho,rhoz,exch,a_u,b_u,sf,vl,dz,wu)

! solve diffusion equation for momentum y component          
        call diff(kms,kme,kts,kte,1,1,dt,v,rho,rhoz,exch,a_v,b_v,sf,vl,dz,wv)
! solve diffusion equation for potential temperature        
        call diff(kms,kme,kts,kte,1,1,dt,th,rho,rhoz,exch_h,a_t,b_t,sf,vl,dz,wt)

! solve diffusion equation for water vapor mixing ratio     
        call diff(kms,kme,kts,kte,1,1,dt,qa,rho,rhoz,exch_h,a_q,b_q,sf,vl,dz,wq)

! solve diffusion equation for liquid water mixing ratio     
        call diff(kms,kme,kts,kte,1,1,dt,qc,rho,rhoz,exch_h,a_qc,b_qc,sf,vl,dz,wqc)

do iz=kts,kte
te(iz)=min(max(temin,te(iz)),temax)
d(iz)=min(max(dmin,d(iz)),dmax)
tpe(iz)=min(max(tpemin,tpe(iz)),tpemax)
enddo

return
end subroutine keps1D

! ===6=8===============================================================72 
! ===6=8===============================================================72

subroutine cdtur_ke(kms,kme,kts,kte,te,d,z,dz,exch,exch_h,Pra_st)

! compute turbulent coefficients
implicit none
integer iz,kms,kme,kts,kte
real te(kms:kme),exch(kms:kme),exch_h(kms:kme),d(kms:kme)
real dz(kms:kme),z(kms:kme),Pra_st(kms:kme)
real fact

exch(kts)=0.
exch_h(kts)=0.
do iz=kts+1,kte
exch(iz)=(c_mu*te(iz-1)**2/d(iz-1)*dz(iz-1)+c_mu*te(iz)**2/d(iz)*dz(iz))/(dz(iz)+dz(iz-1))
exch(iz)=max(exch(iz),0.09)
exch_h(iz)=exch(iz)/Pra_st(iz)
enddo
exch(kte+1)=exch(kte)
exch_h(kte+1)=exch_h(kte)
return
end subroutine cdtur_ke


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
!--------------------------------------------------------------

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
                                
return
end subroutine diff

! ===6=8===============================================================72
! ===6=8===============================================================72

subroutine buoy(g,kms,kme,kts,kte,th,th0,dz,bu,te,tpe,Pra)
       implicit none
       integer kms,kme,kts,kte,iz
       real dtdz1,dtdz2,g,dtmdz
       real th(kms:kme),dz(kms:kme),bu(kms:kme),sf(kms:kme)
       real th0(kms:kme),phi_bu(kms:kme)
       real te(kms:kme),tpe(kms:kme),Pra(kms:kme)
       
        bu(kts)=0.      
       do iz=kts+1,kte-1
        phi_bu(iz)=g/th0(iz)*tpe(iz)/te(iz)*Pra(iz)*c_theta/c_mu
        dtdz1=2.*(th(iz)-th(iz-1))/(dz(iz-1)+dz(iz))
        dtdz2=2.*(th(iz+1)-th(iz))/(dz(iz+1)+dz(iz))
        dtmdz=0.5*(dtdz1+dtdz2)
        bu(iz)=-dtmdz*g/th0(iz)+(g/th0(iz))*phi_bu(iz)
       enddo
        bu(kte)=0.

return
end subroutine buoy

! ===6=8===============================================================72
! ===6=8===============================================================72

subroutine c_nsquare(Pra,ustar,sflux,z0,tsk,mol,pi,g,kms,kme,kts,kte,th,th0,dz,nsquare,dtmdz)
       implicit none
       integer kms,kme,kts,kte,iz
       real thetaz0,sflux,ustar,z0,dtdz1,dtdz2,g,tsk,mol,th0(kms:kme)
       real th(kms:kme),dz(kms:kme),nsquare(kms:kme),sf(kms:kme)
       real te(kms:kme),d(kms:kme),dtmdz(kms:kme),Pra(kms:kme)
       real pi(kms:kme)
         dtmdz(kts)=(-sflux/ustar)/vk/(dz(kts)/2.-z0)
         nsquare(kts)=-g/th0(kts)*dtmdz(kts)
        do iz=kts+1,kte-1
         dtdz1=2.*(th(iz)-th(iz-1))/(dz(iz-1)+dz(iz))
         dtdz2=2.*(th(iz+1)-th(iz))/(dz(iz+1)+dz(iz))
         dtmdz(iz)=0.5*(dtdz1+dtdz2)
         nsquare(iz)=-dtmdz(iz)*g/th0(iz)
        enddo
         nsquare(kte)=0.
       
return
end subroutine c_nsquare

! ===6=8===============================================================72
! ===6=8===============================================================72

subroutine shear(kms,kme,kts,kte,u,v,dz,sh,sf)
! compute shear term
       implicit none
       integer kms,kme,kts,kte,iz,ix,iy
       real dudz1,dudz2,dvdz1,dvdz2,dumdz
       real u(kms:kme),v(kms:kme),dz(kms:kme),sh(kms:kme),sf(kms:kme)
       real u1,u2,v1,v2,ufrac_int
        
        sh(kts)=0.
       do iz=kts+1,kte-1
        u2=(dz(iz+1)*u(iz)+dz(iz)*u(iz+1))/(dz(iz)+dz(iz+1))
        u1=(dz(iz)*u(iz-1)+dz(iz-1)*u(iz))/(dz(iz-1)+dz(iz))
        v2=(dz(iz+1)*v(iz)+dz(iz)*v(iz+1))/(dz(iz)+dz(iz+1))
        v1=(dz(iz)*v(iz-1)+dz(iz-1)*v(iz))/(dz(iz-1)+dz(iz))        
        dumdz=((u2-u1)/dz(iz))**2.+((v2-v1)/dz(iz))**2.            
        sh(iz)=dumdz                          
       enddo
        sh(kte)=0.
      
return
end subroutine shear

! ===6=8===============================================================72
! ===6=8===============================================================72

subroutine invert(kms,kme,kts,kte,a,c,x)
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
  


!######################################################################
       subroutine pbl_height(kms,kme,kts,kte,dz,z,th,q,pblh,iz_pbl)

! this routine computes the PBL height
! with an approach similar to MYNN
       implicit none
       integer kms,kme,kts,kte,iz
       real z(kms:kme),dz(kms:kme),th(kms:kme),q(kms:kme)
       real pblh
!Local
       real thv(kms:kme),zc(kms:kme)
       real thsfc
       integer iz_pbl
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
       iz_pbl=1
       do iz=kts+1,kte
        if(pblh.eq.0.and.thv(iz).gt.thsfc)then
         pblh=zc(iz-1)+(thsfc-thv(iz-1))/(max(0.01,thv(iz)-thv(iz-1)))*(zc(iz)-zc(iz-1))

        endif 
       enddo
       
       do iz=kts,kte
       if(zc(iz).le.pblh)then
       iz_pbl=iz_pbl+1
       endif
       enddo
       return
       
       end subroutine pbl_height


! ===6=8===============================================================72
! ===6=8===============================================================72 

      SUBROUTINE GET_PBLH(KTS,KTE,zi,theta1D,tke1D,zw1D,dz1D,q1D,iz_pbl)
! Copied from MYNN PBL

      !---------------------------------------------------------------
      !             NOTES ON THE PBLH FORMULATION
      !
      !The 1.5-theta-increase method defines PBL heights as the level at
      !which the potential temperature first exceeds the minimum potential
      !temperature within the boundary layer by 1.5 K. When applied to
      !observed temperatures, this method has been shown to produce PBL-
      !height estimates that are unbiased relative to profiler-based
      !estimates (Nielsen-Gammon et al. 2008). However, their study did not
      !include LLJs. Banta and Pichugina (2008) show that a TKE-based
      !threshold is a good estimate of the PBL height in LLJs. Therefore,
      !a hybrid definition is implemented that uses both methods, weighting
      !the TKE-method more during stable conditions (PBLH < 400 m).
      !A variable tke threshold (TKEeps) is used since no hard-wired
      !value could be found to work best in all conditions.
      !---------------------------------------------------------------

      INTEGER,INTENT(IN) :: KTS,KTE
      REAL, INTENT(OUT) :: zi
      REAL, DIMENSION(KTS:KTE), INTENT(IN) ::  tke1D, dz1D,theta1D,q1D
      REAL, DIMENSION(KTS:KTE) :: thetav1D
      REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
      !LOCAL VARS
      REAL ::  PBLH_TKE,tke,tkem1,wt,maxtke,TKEeps,minthv
      REAL :: delt_thv   !delta theta-v; dependent on land/sea point
      REAL, PARAMETER :: sbl_lim  = 200. !Theta-v PBL lower limit of trust (m).
      REAL, PARAMETER :: sbl_damp = 400. !Damping range for averaging with TKE-based PBLH (m).
      INTEGER :: I,J,K,kthv,ktke,iz_pbl

        do iz=kts,kte
        thetav1D(iz)=theta1D(iz)*(1.+0.61*q1D(iz))
       enddo



      !FIND MAX TKE AND MIN THETAV IN THE LOWEST 500 M
      k = kts+1
      kthv = 1
      ktke = 1
      maxtke = 0.
      minthv = 9.E9

      DO WHILE (zw1D(k) .LE. 500.)
        tke  =MAX(tke1D(k),0.)   ! maximum QKE
         IF (maxtke < ttke) then
            maxtke = ttke
            ktke = k
         ENDIF
         IF (minthv > thetav1D(k)) then
             minthv = thetav1D(k)
             kthv = k
         ENDIF
         k = k+1
      ENDDO
      TKEeps = maxtke/20.
      TKEeps = MAX(TKEeps,0.025/2.)
      TKEeps = MIN(TKEeps,0.25/2.)

      !FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
      zi=0.
      k = kthv+1
          delt_thv = 1.5
      DO WHILE (zi .EQ. 0.)
         IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
            zi = zw1D(k) - dz1D(k-1)* &
              & MIN((thetav1D(k)-(minthv+delt_thv))/MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
        ENDIF
        k = k+1
         IF (k .EQ. kte-1) zi = zw1D(kts+1) !EXIT SAFEGUARD
      ENDDO

      PBLH_TKE=0.
      k = ktke+1
     DO WHILE (PBLH_TKE .EQ. 0.)
         tke  =MAX(tke1D(k)/2.,0.)      ! maximum TKE
         tkem1=MAX(tke1D(k-1)/2.,0.)
         IF (tke .LE. TKEeps) THEN
               PBLH_TKE = zw1D(k) - dz1D(k-1)* &
               & MIN((TKEeps-tke)/MAX(tkem1-tke, 1E-6), 1.0)
             !IN CASE OF NEAR ZERO TKE, SET PBLH = LOWEST LEVEL.
             PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
             !print *,"PBLH_TKE:",i,j,PBLH_TKE, Qke1D(k)/2., zw1D(kts+1)
         ENDIF
         k = k+1
         IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) !EXIT SAFEGUARD
      ENDDO

    !BLEND THE TWO PBLH TYPES HERE:

      wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
      zi=PBLH_TKE*(1.-wt) + zi*wt


       iz_pbl=1
       do k=kts,kte
       if(zw1D(k).le.zi)then
       iz_pbl=iz_pbl+1
       endif
       enddo


   END SUBROUTINE GET_PBLH

! ===6=8===============================================================72
! ===6=8===============================================================72
subroutine thetaphi_alphabeta(kms,kme,kts,kte,z0,dz,sh,bu,te,d,dt,Pra,dls,ustar,phieps,phim,Ri,nsquare,a_d)
      
      implicit none
      integer iz,kms,kme,kts,kte
      real te(kms:kme)                ! turbulent kinetic energy
      real d(kms:kme)                 ! dissipation rate
      real a_d(kms:kme)
      real dt,z0                   ! Time step
      real sh(kms:kme)    ! shear term in TKE eqn.
      real bu(kms:kme)    ! buoyancy term in TKE eqn.
      real A(kms:kme)
      real B(kms:kme)
      real theta(kms:kme)
      real phi(kms:kme)
      real theta_new(kms:kme)
      real phi_new(kms:kme)
      real C(kms:kme)
      real alpha
      real beta
      real F
      real dz(kms:kme)
      real Pra(kms:kme)
      real dls(kms:kme)
      real Ri(kms:kme)
      real nsquare(kms:kme)
      real ustar,phieps,phim
       alpha=1.
       beta=-1./c2
       F=c2-1.

        do iz=kts+1,kte
        if(abs(te(iz))/abs(d(iz)).gt.1E4.and.d(iz).le.1E-6)then
        d(iz)=1/1.4*te(iz)**(3./2.)/dls(iz)
        endif
        theta(iz)=max(temin,te(iz))/max(dmin,d(iz))
        phi(iz)=max(dmin,d(iz))**beta*max(temin,te(iz))**alpha
        C(iz)=(c_mu*((c1-1.)*sh(iz)+((c3-1.)*bu(iz))/Pra(iz)))
        A(iz)=c_mu*(sh(iz)+bu(iz)/Pra(iz))
        B(iz)=c_mu*(c1*sh(iz)+c3*bu(iz)/Pra(iz))
        if(C(iz).lt.0.)then
        C(iz)=-C(iz)
        theta_new(iz)=(c2-1.)/(C(iz)*theta(iz))+((c2-1.)**(1./2.)*(C(iz)*theta(iz)**2-c2+1))/(C(iz)*theta(iz)*((c2-1.)**(1./2.)+C(iz)**(1./2.)*theta(iz)*tanh(C(iz)**(1./2.)*dt*(c2-1)**(1./2.))))
        elseif(C(iz).eq.0.)then
        theta_new(iz)=theta(iz)+F*dt;
        else
        theta_new(iz)=((F/C(iz))**(0.5)*(tanh(dt*(C(iz)*F)**(0.5)) +theta(iz)*(C(iz)/F)**(0.5)))/(theta(iz)*tanh(dt*(C(iz)*F)**(0.5))*(C(iz)/F)**(0.5) + 1.)
        endif
        theta_new(iz)=max(1.,theta_new(iz))
        phi_new(iz)=(phi(iz)*exp(theta_new(iz)*dt*(alpha*A(iz)+beta*B(iz))))
        te(iz)=((phi_new(iz)*theta_new(iz)**(beta))**(1./(alpha+beta)))
        d(iz)=(te(iz)/theta_new(iz))
        enddo
        do iz=kts,kte
       if(Ri(iz).gt.0.)then
       a_d(iz)=a_d(iz)+c4*min(1.,sqrt(Ri(iz)/c5))*sqrt(max(0.,-nsquare(iz)))
       endif
       enddo
       d(kts)=(ustar**3./vk/(dz(1)/2.-z0))*phieps*(1.+a_d(kts)*d(kts)*dt)
       te(kts)=(ustar**2.)/sqrt(c_mu)*sqrt(phieps/phim)

return
end subroutine thetaphi_alphabeta

! ===6=8===============================================================72
! ===6=8===============================================================72

subroutine length_bougeault(g,kms,kme,kts,kte,z,dz,te,th,th0,dls)
! compute the length scales up and down
         implicit none
         integer kms,kme,kts,kte,iz,izz,ix,iy
         real dzt,zup,beta,zup_inf,bbb,tl,zdo,zdo_sup,zzz,g
         real te(kms:kme),dlu(kms:kme),dld(kms:kme),dz(kms:kme)
         real z(kms:kme),th(kms:kme),th0(kms:kme),dls(kms:kme)

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
         do iz=kts,kte
          dld(iz)=min(dld(iz),(z(iz)+dz(iz)/2.))
          dls(iz)=sqrt(dlu(iz)*dld(iz))
         enddo


return
end subroutine length_bougeault

! ===6=8===============================================================72
! ===6=8===============================================================72

 subroutine surface_bl_pra_ri(kms,kme,kts,kte,dz,z,rho,g,cp,z0,sflux,raten,pi1d,p8w1D,  &
                           th,qa,sh,nsquare,Ri,b_ric,psim,psih,pblh,ustar,      &
                           iz_pbl,Pra,Pra_st,phim,phih,phieps,dtdzs)    

    implicit none
    integer kms,kme,kts,kte,iz,iz_pbl
    real cp,g,b_ric,psim,psih,pblh,ustar,sflux
    real phim,phih,phieps
    real radflux,radsum
    real z0,zz,dtdzs
    real dz(kms:kme),prnumfac(kms:kme),Pra(kms:kme),Pra_st(kms:kme),z(kms:kme+1)
    real th(kms:kme),qa(kms:kme),rho(kms:kme)
    real raten(kms:kme),pi1d(kms:kme),p8w1D(kms:kme)
    real sh(kms:kme),nsquare(kms:kme),Ri(kms:kme)
    real,parameter :: prmin=0.25,prmax=4.,sfcfrac=0.1,bfac=6.8
    real,parameter ::rimin=-100.,zfmin=1.e-8,aphi5 = 5.,aphi16 = 16.
    real prnum,prnum0,conpr,prfac,prfac2,wstar3,wstar3_2
    real zol1,zl1,hol1,phim_sl,phih_sl
    real bfx0,govrth,govrthv
    logical sfcflg
     Ri=1.
     Pra=4.   
    
    do iz=kts,kte
    if(iz.eq.kts)then
      Ri(iz)=b_ric
      else
      Ri(iz)=min(-nsquare(iz)/sh(iz),100.)
      if(sh(iz).lt.1e-15)then
      if(-nsquare(iz).gt.0.)then
      Ri(iz)=10.
      else
      Ri(iz)=-1.
      endif
      endif
    endif
    enddo
      Ri(kte)=Ri(kte-1)

     if(iz_pbl.ge.2)then
      do iz=1,iz_pbl-1
         radflux=raten(iz)*pi1d(iz)
         radflux=radflux*cp/g*(p8w1D(iz)-p8w1D(iz+1))
      if (radflux < 0.0 ) radsum=abs(radflux)+radsum
      enddo
      endif
      radsum=max(radsum,0.0)/rho(iz_pbl-1)/cp

     sfcflg=.true.
    if(b_ric.ge.0)sfcflg=.false.
     zl1=dz(1)/2.
     zz=(zl1+z0)/zl1
     zol1 = max(b_ric*psim*psim/psih,rimin)
    if(sfcflg)then
     zol1 = min(zol1,-zfmin)
    else
     zol1 = max(zol1,zfmin)
    endif
     hol1 = zol1*pblh/zl1*sfcfrac
    
    if(sfcflg)then
       phim = (1.-aphi16*zol1*zz)**(-1./4.)
       phih = (1.-aphi16*zol1*zz)**(-1./2.)
       phieps=1.-zol1*zz
       phim_sl = (1.-aphi16*hol1)**(-1./4.)
       phih_sl = (1.-aphi16*hol1)**(-1./2.)
       bfx0 = max(sflux,0.)
       govrth=g/th(1)
       govrthv=g/(th(iz_pbl-1)*(1+0.606271777*qa(iz_pbl-1)))
       wstar3 =(govrth*bfx0*pblh)
       wstar3_2 =(govrthv*radsum*pblh)
     else
       phim_sl = (1.+aphi5*hol1)
       phih_sl= phim_sl
       phim=(1.+aphi5*zol1*zz)
       phieps=(1+2.5*(zol1*zz)**0.6)**(3./2.)
       phih= phim_sl
       wstar3=0.
       wstar3_2=0.
     endif
       conpr=bfac*vk*sfcfrac
     if(iz_pbl.gt.1)then
     do iz=kts,kte+1
     if(sfcflg)then
       prnumfac(iz) = -3.*(max(z(iz+1)-sfcfrac*pblh,0.))**2./pblh**2.
       prfac=conpr
       prfac2 = 15.9*(wstar3+wstar3_2)/ustar**3/(1.+4.*vk*(wstar3+wstar3_2)/ustar**3)
       prnum0 =(phih_sl/phim_sl+prfac)
       prnum0= prnum0/(1.+prfac2*vk*sfcfrac)
       prnum0 = max(min(prnum0,prmax),prmin)
       Pra(iz)= 1. + (prnum0-1.)*exp(prnumfac(iz))
     else
       Pra(iz)=max(1.,min(4.,1.+3*Ri(iz)))
     endif
     enddo
     endif
      dtdzs=-sflux/ustar/vk*phih/(dz(kts)/2-z0)
      Pra_st(kts)=0.
     do iz=kts+1,kte
      Pra_st(iz)=(Pra(iz-1)*dz(iz)+Pra(iz)*dz(iz-1))/(dz(iz)+dz(iz-1))
     enddo
      Pra_st(kte+1)=Pra_st(kte)

return
end subroutine surface_bl_pra_ri

!==============================================================================
!==============================================================================

subroutine terms_tpe(dtmdz,g,kms,kme,kts,kte,th0,te,d,tpe,a_tpe,b_tpe,Pra,coeff)

     implicit none
     integer iz,kms,kme,kts,kte
     real g,th0(kms:kme),coeff(kms:kme)
     real te(kms:kme),d(kms:kme),tpe(kms:kme)
     real dtmdz(kms:kme),A_wt(kms:kme),a_tpe(kms:kme),b_tpe(kms:kme),Pra(kms:kme)
     do iz=kts,kte
      A_wt(iz)=1./te(iz)/tpe(iz)/2.*(c_mu/Pra(iz)*te(iz)**2./d(iz)*dtmdz(iz))**2.
      coeff(iz)= max(0.55,3./2.*(1.+A_wt(iz)))
      a_tpe(iz)=-g/th0(iz)*c_theta*te(iz)/d(iz)*dtmdz(iz)-coeff(iz)*d(iz)/te(iz)
      b_tpe(iz)=c_mu*te(iz)**2./d(iz)*dtmdz(iz)**2./Pra(iz)
     enddo
  !     tpe(kts)=(1./coeff(kts)*c_mu*dtmdz(kts)**2.*te(kts)**3./Pra(kts))/d(kts)**2.

return
end subroutine terms_tpe

!==============================================================================
!==============================================================================
 subroutine  calc_countergradient(dz,g,kms,kme,kts,kte,th0,te,d,tpe,Pra,b_t)
 implicit none
integer iz,kms,kme,kts,kte
real dz(kms:kme),g,te(kms:kme),d(kms:kme),Pra(kms:kme)
real phi(kms:kme),dphidz1,dphidz2,phiz1,dphidz(kms:kme)
real b_t(kms:kme),th0(kms:kme),tpe(kms:kme)
       phi=0.
       do iz=kts,kte
        phi(iz)=g/th0(iz)*te(iz)*tpe(iz)*c_theta/d(iz)
      enddo
       phiz1=(phi(kts)*dz(kts)+phi(kts+1)*dz(kts+1))/(dz(kts)+dz(kts+1))
       dphidz(kts)=phiz1/dz(kts)
      do iz=kts+1,kte-1
       dphidz1=2.*(phi(iz)-phi(iz-1))/(dz(iz-1)+dz(iz))
       dphidz2=2.*(phi(iz+1)-phi(iz))/(dz(iz+1)+dz(iz))
       dphidz(iz)=0.5*(dphidz1+dphidz2)
     enddo
      dphidz(kte)=0.
      dphidz(kts)=0. 
     do iz=kts,kte
      b_t(iz)=b_t(iz)-dphidz(iz)
     enddo


return
end subroutine calc_countergradient
END MODULE module_bl_keps
