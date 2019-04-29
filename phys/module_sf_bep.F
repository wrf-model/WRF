MODULE module_sf_bep

!USE module_model_constants
 USE module_sf_urban

! SGClarke 09/11/2008
! Access urban_param.tbl values through calling urban_param_init in module_physics_init
! for CASE (BEPSCHEME) select sf_urban_physics
!
! -----------------------------------------------------------------------
!  Dimension for the array used in the BEP module
! -----------------------------------------------------------------------

      integer nurbm           ! Maximum number of urban classes    
      parameter (nurbm=3)

      integer ndm             ! Maximum number of street directions 
      parameter (ndm=2)

      integer nz_um           ! Maximum number of vertical levels in the urban grid
      parameter(nz_um=18)

      integer ng_u            ! Number of grid levels in the ground
      parameter (ng_u=10)
      integer nwr_u            ! Number of grid levels in the walls or roofs
      parameter (nwr_u=10)

      real dz_u                 ! Urban grid resolution
      parameter (dz_u=5.)

! The change of ng_u, nwr_u should be done in agreement with the block data
!  in the routine "surf_temp" 
! -----------------------------------------------------------------------
!  Constant used in the BEP module
! -----------------------------------------------------------------------
           
      real vk                 ! von Karman constant
      real g_u                  ! Gravity acceleration
      real pi                 !
      real r                  ! Perfect gas constant
      real cp_u                 ! Specific heat at constant pressure
      real rcp_u                !
      real sigma              !
      real p0                 ! Reference pressure at the sea level
      real cdrag              ! Drag force constant
      parameter(vk=0.40,g_u=9.81,pi=3.141592653,r=287.,cp_u=1004.)        
      parameter(rcp_u=r/cp_u,sigma=5.67e-08,p0=1.e+5,cdrag=0.4)

! -----------------------------------------------------------------------     




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

!------------------------------------------------------------------------
!     Input
!------------------------------------------------------------------------
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

!      integer nx,ny,nz              ! Number of points in the mesocsale grid
      real z(ims:ime,kms:kme,jms:jme)            ! Vertical coordinates
      REAL, INTENT(IN )::   DT      ! Time step
!      real zr(ims:ime,jms:jme)                ! Solar zenith angle
!      real deltar(ims:ime,jms:jme)            ! Declination of the sun
!      real ah(ims:ime,jms:jme)                ! Hour angle
!      real rs(ims:ime,jms:jme)                ! Solar radiation
!------------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------ 
!      real tsk(ims:ime,jms:jme)           ! Average of surface temperatures (roads and roofs)
!
!    Implicit and explicit components of the source and sink terms at each levels,
!     the fluxes can be computed as follow: FX = A*X + B   example: t_fluxes = a_t * pt + b_t
      real a_u(ims:ime,kms:kme,jms:jme)         ! Implicit component for the momemtum in X-direction (center)
      real a_v(ims:ime,kms:kme,jms:jme)         ! Implicit component for the momemtum in Y-direction (center)
      real a_t(ims:ime,kms:kme,jms:jme)         ! Implicit component for the temperature
      real a_e(ims:ime,kms:kme,jms:jme)         ! Implicit component for the TKE
      real b_u(ims:ime,kms:kme,jms:jme)         ! Explicit component for the momemtum in X-direction (center)
      real b_v(ims:ime,kms:kme,jms:jme)         ! Explicit component for the momemtum in Y-direction (center)
      real b_t(ims:ime,kms:kme,jms:jme)         ! Explicit component for the temperature
      real b_e(ims:ime,kms:kme,jms:jme)         ! Explicit component for the TKE
      real b_q(ims:ime,kms:kme,jms:jme)         ! Explicit component for the humidity
      real dlg(ims:ime,kms:kme,jms:jme)         ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u(ims:ime,kms:kme,jms:jme)        ! Length scale (lb in formula (22) ofthe BLM paper).
! urban surface and volumes        
      real sf(ims:ime,kms:kme,jms:jme)           ! surface of the urban grid cells
      real vl(ims:ime,kms:kme,jms:jme)             ! volume of the urban grid cells
! urban fluxes
      real rl_up(its:ite,jts:jte) ! upward long wave radiation
      real rs_abs(its:ite,jts:jte) ! absorbed short wave radiation
      real emiss(its:ite,jts:jte)  ! emissivity averaged for urban surfaces
      real grdflx_urb(its:ite,jts:jte)  ! ground heat flux for urban areas
!------------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      real hi_urb(its:ite,1:nz_um,jts:jte)    ! Height histograms of buildings
      real hi_urb1D(nz_um)                    ! Height histograms of buildings
      real hb_u(nz_um)                        ! Bulding's heights
      real ss_urb(nz_um)  ! Probability that a building has an height equal to z
      real pb_urb(nz_um)  ! Probability that a building has an height greater or equal to z
      integer nz_urb(nurbm)                   ! Number of layer in the urban grid
      integer nzurban(nurbm)                  

!    Building parameters      
      real alag_u(nurbm)                      ! Ground thermal diffusivity [m^2 s^-1]
      real alaw_u(nurbm)                      ! Wall thermal diffusivity [m^2 s^-1]
      real alar_u(nurbm)                      ! Roof thermal diffusivity [m^2 s^-1]
      real csg_u(nurbm)                       ! Specific heat of the ground material [J m^3 K^-1]
      real csw_u(nurbm)                       ! Specific heat of the wall material [J m^3 K^-1]
      real csr_u(nurbm)                       ! Specific heat of the roof material [J m^3 K^-1]
      real twini_u(nurbm)                     ! Initial temperature inside the building's wall [K]
      real trini_u(nurbm)                     ! Initial temperature inside the building's roof [K]
      real tgini_u(nurbm)                     ! Initial road temperature
!
!   Building materials
!
      real csg(ng_u)                          ! Specific heat of the ground material [J m^3 K^-1]
      real csr(nwr_u)                         ! Specific heat of the roof material [J m^3 K^-1]
      real csw(nwr_u)                         ! Specific heat of the wall material [J m^3 K^-1]
      real alag(ng_u)                         ! Ground thermal diffusivity [m^2 s^-1]
      real alaw(nwr_u)                        ! Wall thermal diffusivity [m^2 s^-1]
      real alar(nwr_u)                        ! Roof thermal diffusivity [m^2 s^-1]
!
! for twini_u, and trini_u the initial value at the deepest level is kept constant during the simulation
!
!    Radiation parameters
      real albg_u(nurbm)                      ! Albedo of the ground
      real albw_u(nurbm)                      ! Albedo of the wall
      real albr_u(nurbm)                      ! Albedo of the roof
      real emg_u(nurbm)                       ! Emissivity of ground
      real emw_u(nurbm)                       ! Emissivity of wall
      real emr_u(nurbm)                       ! Emissivity of roof

!   fww_u,fwg_u,fgw_u,fsw_u,fsg_u are the view factors used to compute the long wave
!   and the short wave radation. 
      real fww_u(nz_um,nz_um,ndm,nurbm)         !  from wall to wall
      real fwg_u(nz_um,ndm,nurbm)               !  from wall to ground
      real fgw_u(nz_um,ndm,nurbm)               !  from ground to wall
      real fsw_u(nz_um,ndm,nurbm)               !  from sky to wall
      real fws_u(nz_um,ndm,nurbm)               !  from sky to wall
      real fsg_u(ndm,nurbm)                     !  from sky to ground

!    Roughness parameters
      real z0g_u(nurbm)         ! The ground's roughness length
      real z0r_u(nurbm)         ! The roof's roughness length

!    Roughness parameters
      real z0(ndm,nz_um)           ! Roughness lengths "profiles"

!    Street parameters
      integer nd_u(nurbm)       ! Number of street direction for each urban class 
      real strd_u(ndm,nurbm)    ! Street length (fix to greater value to the horizontal length of the cells)
      real drst_u(ndm,nurbm)    ! Street direction
      real ws_u(ndm,nurbm)      ! Street width
      real bs_u(ndm,nurbm)      ! Building width
      real h_b(nz_um,nurbm)     ! Bulding's heights
      real d_b(nz_um,nurbm)     ! Probability that a building has an height h_b
      real ss_u(nz_um,nurbm)  ! Probability that a building has an height equal to z
      real pb_u(nz_um,nurbm)  ! Probability that a building has an height greater or equal to z
!
!    Street parameters
!
      real bs(ndm)                 ! Building width 
      real ws(ndm)                 ! Street width
      real drst(ndm)               ! street directions 
      real strd(ndm)               ! Street lengths 
      real ss(nz_um)               ! Probability to have a building with height h
      real pb(nz_um)               ! Probability to have a building with an height equal

!    Grid parameters

      integer nz_u(nurbm)       ! Number of layer in the urban grid
      
      real z_u(nz_um)         ! Height of the urban grid levels

! 1D array used for the input and output of the routine "urban"

      real z1D(kms:kme)               ! vertical coordinates
      real ua1D(kms:kme)                ! wind speed in the x directions
      real va1D(kms:kme)                ! wind speed in the y directions
      real pt1D(kms:kme)                ! potential temperature
      real da1D(kms:kme)                ! air density
      real pr1D(kms:kme)                ! air pressure
      real pt01D(kms:kme)               ! reference potential temperature
      real zr1D                    ! zenith angle
      real deltar1D                ! declination of the sun
      real ah1D                    ! hour angle (it should come from the radiation routine)
      real rs1D                    ! solar radiation
      real rld1D                   ! downward flux of the longwave radiation


      real tw1D(2*ndm,nz_um,nwr_u)  ! temperature in each layer of the wall
      real tg1D(ndm,ng_u)          ! temperature in each layer of the ground
      real tr1D(ndm,nz_um,nwr_u)  ! temperature in each layer of the roof
      real sfw1D(2*ndm,nz_um)      ! sensible heat flux from walls
      real sfg1D(ndm)              ! sensible heat flux from ground (road)
      real sfr1D(ndm,nz_um)      ! sensible heat flux from roofs
      real sf1D(kms:kme)              ! surface of the urban grid cells
      real vl1D(kms:kme)                ! volume of the urban grid cells
      real a_u1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the X-direction
      real a_v1D(kms:kme)               ! Implicit component of the momentum sources or sinks in the Y-direction
      real a_t1D(kms:kme)               ! Implicit component of the heat sources or sinks
      real a_e1D(kms:kme)               ! Implicit component of the TKE sources or sinks
      real b_u1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the X-direction
      real b_v1D(kms:kme)               ! Explicit component of the momentum sources or sinks in the Y-direction
      real b_t1D(kms:kme)               ! Explicit component of the heat sources or sinks
      real b_e1D(kms:kme)               ! Explicit component of the TKE sources or sinks
      real dlg1D(kms:kme)               ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u1D(kms:kme)              ! Length scale (lb in formula (22) ofthe BLM paper)
      real time_bep
! arrays used to collapse indexes
      integer ind_zwd(nz_um,nwr_u,ndm)
      integer ind_gd(ng_u,ndm)
      integer ind_zd(nz_um,ndm)
!
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

!------------------------------------------------------------------------
!    Calculation of the momentum, heat and turbulent kinetic fluxes
!     produced by builgings
!
! Reference:
! Martilli, A., Clappier, A., Rotach, M.W.:2002, 'AN URBAN SURFACE EXCHANGE
! PARAMETERISATION FOR MESOSCALE MODELS', Boundary-Layer Meteorolgy 104:
! 261-304
!------------------------------------------------------------------------
!prepare the arrays to collapse indexes

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
       enddo !iz
       do iz_u=1,num_urban_hi
          hi_urb(ix,iz_u,iy)= hi_urb2d(ix,iz_u,iy)
       enddo !iz_u
      enddo
      enddo


      if (first) then                           ! True only on first call
         
         call init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,emg_u,emw_u,&
                emr_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b)

!      Initialisation of the urban parameters and calculation of the view factors
             
         call icBEP(nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)                                  
          
         first=.false.

      endif ! first

      do ix=its,ite
      do iy=jts,jte
        if (FRC_URB2D(ix,iy).gt.0.) then    ! Calling BEP only for existing urban classes.	
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
!
!We compute the view factors in the icBEP_XY routine
!           
         
           call icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,fws_u,fsg_u,   &
                         nd_u(iurb),strd,ws,nzurban(iurb),z_u)   

       do iz= kts,kte
          ua1D(iz)=u_phy(ix,iz,iy)
          va1D(iz)=v_phy(ix,iz,iy)
	  pt1D(iz)=th_phy(ix,iz,iy)
	  da1D(iz)=rho(ix,iz,iy)
	  pr1D(iz)=p_phy(ix,iz,iy)
!	  pt01D(iz)=th_phy(ix,iz,iy)
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
!          tw1D(2*id-1,iz_u,iw)=tw1_u(ix,iy,ind_zwd(iz_u,iw,id))
!          tw1D(2*id,iz_u,iw)=tw2_u(ix,iy,ind_zwd(iz_u,iw,id))
            if(ind_zwd(iz_u,iw,id).gt.urban_map_zwd)write(*,*)'ind_zwd too big w',ind_zwd(iz_u,iw,id)
          tw1D(2*id-1,iz_u,iw)=tw1_urb4d(ix,ind_zwd(iz_u,iw,id),iy)
          tw1D(2*id,iz_u,iw)=tw2_urb4d(ix,ind_zwd(iz_u,iw,id),iy)
         enddo
         enddo
         enddo
	
         do id=1,ndm
          do ig=1,ng_u
!           tg1D(id,ig)=tg_u(ix,iy,ind_gd(ig,id))
            tg1D(id,ig)=tgb_urb4d(ix,ind_gd(ig,id),iy)
          enddo
          do iz_u=1,nz_um
          do ir=1,nwr_u
!           tr1D(id,iz_u,ir)=tr_u(ix,iy,ind_zwd(iz_u,ir,id))
            if(ind_zwd(iz_u,ir,id).gt.urban_map_zwd)write(*,*)'ind_zwd too big r',ind_zwd(iz_u,ir,id)
            tr1D(id,iz_u,ir)=trb_urb4d(ix,ind_zwd(iz_u,ir,id),iy)
          enddo
          enddo
         enddo

         do id=1,ndm
	 do iz=1,nz_um
!	  sfw1D(2*id-1,iz)=sfw1(ix,iy,ind_zd(iz,id))
!	  sfw1D(2*id,iz)=sfw2(ix,iy,ind_zd(iz,id))
	  sfw1D(2*id-1,iz)=sfw1_urb3d(ix,ind_zd(iz,id),iy)
	  sfw1D(2*id,iz)=sfw2_urb3d(ix,ind_zd(iz,id),iy)
	 enddo
	 enddo
	 
	 do id=1,ndm
!	  sfg1D(id)=sfg(ix,iy,id)
	  sfg1D(id)=sfg_urb3d(ix,id,iy)
	 enddo
	 
	 do id=1,ndm
	 do iz=1,nz_um
!	  sfr1D(id,iz)=sfr(ix,iy,ind_zd(iz,id))
	  sfr1D(id,iz)=sfr_urb3d(ix,ind_zd(iz,id),iy)
	 enddo
	 enddo

         
         rs1D=swdown(ix,iy)
         rld1D=glw(ix,iy)
         time_h=(itimestep*dt)/3600.+gmt

         zr1D=acos(COSZ_URB2D(ix,iy))
         deltar1D=DECLIN_URB
         ah1D=OMG_URB2D(ix,iy)
!	 call angle(xlong(ix,iy),xlat(ix,iy),julday,time_h,zr1D,deltar1D,ah1D)
!        write(*,*) 'entro en BEP1D'
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
!        write(*,*) 'salgo de BEP1D'
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
!
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
!
         endif ! FRC_URB2D
   
      enddo  ! iy
      enddo  ! ix


        time_bep=time_bep+dt
         
         
      return
      end subroutine BEP
            
! ===6=8===============================================================72

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

! ----------------------------------------------------------------------
! This routine computes the effects of buildings on momentum, heat and
!  TKE (turbulent kinetic energy) sources or sinks and on the mixing length.
! It provides momentum, heat and TKE sources or sinks at different levels of a
!  mesoscale grid defined by the altitude of its cell interfaces "z" and
!  its number of levels "nz".
! The meteorological input parameters (wind, temperature, solar radiation)
!  are specified on the "mesoscale grid".
! The inputs concerning the building and street charateristics are defined
!  on a "urban grid". The "urban grid" is defined with its number of levels
!  "nz_u" and its space step "dz_u".
! The input parameters are interpolated on the "urban grid". The sources or sinks
!  are calculated on the "urban grid". Finally the sources or sinks are 
!  interpolated on the "mesoscale grid".
 

!  Mesoscale grid            Urban grid             Mesoscale grid
!  
! z(4)    ---                                               ---
!          |                                                 |
!          |                                                 |
!          |   Interpolation                  Interpolation  |
!          |            Sources or sinks calculation         |
! z(3)    ---                                               ---
!          | ua               ua_u  ---  uv_a         a_u    |
!          | va               va_u   |   uv_b         b_u    |
!          | pt               pt_u  ---  uh_b         a_v    |
! z(2)    ---                        |    etc...      etc...---
!          |                 z_u(1) ---                      |
!          |                         |                       |
! z(1) ------------------------------------------------------------

!     
! Reference:
! Martilli, A., Clappier, A., Rotach, M.W.:2002, 'AN URBAN SURFACE EXCHANGE
! PARAMETERISATION FOR MESOSCALE MODELS', Boundary-Layer Meteorolgy 104:
! 261-304
 
! ----------------------------------------------------------------------

      implicit none

 

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------

! Data relative to the "mesoscale grid"

!      integer nz                 ! Number of vertical levels
      integer kms,kme,kts,kte
      real z(kms:kme)               ! Altitude above the ground of the cell interfaces.
      real ua(kms:kme)                ! Wind speed in the x direction
      real va(kms:kme)                ! Wind speed in the y direction
      real pt(kms:kme)                ! Potential temperature
      real da(kms:kme)                ! Air density
      real pr(kms:kme)                ! Air pressure
      real pt0(kms:kme)               ! Reference potential temperature (could be equal to "pt")
      real dt                    ! Time step
      real zr                    ! Zenith angle
      real deltar                ! Declination of the sun
      real ah                    ! Hour angle
      real rs                    ! Solar radiation
      real rld                   ! Downward flux of the longwave radiation

! Data relative to the "urban grid"

      integer iurb               ! Current urban class

!    Building parameters      
      real alag(ng_u)            ! Ground thermal diffusivity [m^2 s^-1]
      real alaw(nwr_u)           ! Wall thermal diffusivity [m^2 s^-1]
      real alar(nwr_u)           ! Roof thermal diffusivity [m^2 s^-1]
      real csg(ng_u)             ! Specific heat of the ground material [J m^3 K^-1]
      real csw(nwr_u)            ! Specific heat of the wall material [J m^3 K^-1]
      real csr(nwr_u)            ! Specific heat of the roof material [J m^3 K^-1]

!    Radiation parameters
      real albg                  ! Albedo of the ground
      real albw                  ! Albedo of the wall
      real albr                  ! Albedo of the roof
      real emg                   ! Emissivity of ground
      real emw                   ! Emissivity of wall
      real emr                   ! Emissivity of roof

!    fww,fwg,fgw,fsw,fsg are the view factors used to compute the long and 
!    short wave radation. 
!    The calculation of these factor is explained in the Appendix A of the BLM paper
      real fww(nz_um,nz_um,ndm,nurbm)  !  from wall to wall
      real fwg(nz_um,ndm,nurbm)        !  from wall to ground
      real fgw(nz_um,ndm,nurbm)        !  from ground to wall
      real fsw(nz_um,ndm,nurbm)        !  from sky to wall
      real fws(nz_um,ndm,nurbm)        !  from wall to sky
      real fsg(ndm,nurbm)              !  from sky to ground

!    Roughness parameters
      real z0(ndm,nz_um)           ! Roughness lengths "profiles"
      
!    Street parameters
      integer ndu                  ! Number of street direction for each urban class 
      real strd(ndm)               ! Street length (set to a greater value then the horizontal length of the cells)
      real drst(ndm)               ! Street direction
      real ws(ndm)                 ! Street width
      real bs(ndm)                 ! Building width
      real ss(nz_um)               ! The probability that a building has an height equal to "z"
      real pb(nz_um)               ! The probability that a building has an height greater or equal to "z"
        
!    Grid parameters
      integer nzu                  ! Number of layer in the urban grid
      real z_u(nz_um)              ! Height of the urban grid levels


! ----------------------------------------------------------------------
! INPUT-OUTPUT
! ----------------------------------------------------------------------

! Data relative to the "urban grid" which should be stored from the current time step to the next one

      real tw(2*ndm,nz_um,nwr_u)  ! Temperature in each layer of the wall [K]
      real tr(ndm,nz_um,nwr_u)  ! Temperature in each layer of the roof [K]
      real tg(ndm,ng_u)          ! Temperature in each layer of the ground [K]
      real sfw(2*ndm,nz_um)      ! Sensible heat flux from walls
      real sfg(ndm)              ! Sensible heat flux from ground (road)
      real sfr(ndm,nz_um)      ! Sensible heat flux from roofs
      real gfg(ndm)             ! Heat flux transferred from the surface of the ground (road) towards the interior
      real gfr(ndm,nz_um)     ! Heat flux transferred from the surface of the roof towards the interior
      real gfw(2*ndm,nz_um)     ! Heat flux transfered from the surface of the walls towards the interior
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------

! Data relative to the "mesoscale grid"

      real sf(kms:kme)             ! Surface of the "mesoscale grid" cells taking into account the buildings
      real vl(kms:kme)               ! Volume of the "mesoscale grid" cells taking into account the buildings
     
!    Implicit and explicit components of the source and sink terms at each levels,
!     the fluxes can be computed as follow: FX = A*X + B   example: Heat fluxes = a_t * pt + b_t
      real a_u(kms:kme)              ! Implicit component of the momentum sources or sinks in the X-direction
      real a_v(kms:kme)              ! Implicit component of the momentum sources or sinks in the Y-direction
      real a_t(kms:kme)              ! Implicit component of the heat sources or sinks
      real a_e(kms:kme)              ! Implicit component of the TKE sources or sinks
      real b_u(kms:kme)              ! Explicit component of the momentum sources or sinks in the X-direction
      real b_v(kms:kme)              ! Explicit component of the momentum sources or sinks in the Y-direction
      real b_t(kms:kme)              ! Explicit component of the heat sources or sinks
      real b_e(kms:kme)              ! Explicit component of the TKE sources or sinks
      real dlg(kms:kme)              ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u(kms:kme)             ! Length scale (lb in formula (22) ofthe BLM paper).
      
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------

      real dz(kms:kme)               ! vertical space steps of the "mesoscale grid"

! Data interpolated from the "mesoscale grid" to the "urban grid"

      real ua_u(nz_um)          ! Wind speed in the x direction
      real va_u(nz_um)          ! Wind speed in the y direction
      real pt_u(nz_um)          ! Potential temperature
      real da_u(nz_um)          ! Air density
      real pt0_u(nz_um)         ! Reference potential temperature
      real pr_u(nz_um)          ! Air pressure

! Solar radiation at each level of the "urban grid"

      real rsg(ndm)             ! Short wave radiation from the ground
      real rsw(2*ndm,nz_um)     ! Short wave radiation from the walls
      real rlg(ndm)             ! Long wave radiation from the ground
      real rlw(2*ndm,nz_um)     ! Long wave radiation from the walls

! Potential temperature of the surfaces at each level of the "urban grid"

      real ptg(ndm)             ! Ground potential temperatures 
      real ptr(ndm,nz_um)     ! Roof potential temperatures 
      real ptw(2*ndm,nz_um)     ! Walls potential temperatures 

 
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
!  vertical surfaces (walls) ans horizontal surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = A*X + B
!  Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u

      real uhb_u(ndm,nz_um)   ! U (wind component) Horizontal surfaces, B (explicit) term
      real uva_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, A (implicit) term
      real uvb_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, B (explicit) term
      real vhb_u(ndm,nz_um)   ! V (wind component) Horizontal surfaces, B (explicit) term
      real vva_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, A (implicit) term
      real vvb_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, B (explicit) term
      real thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term
      real ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term
      
!
      real rs_abs ! solar radiation absorbed by urban surfaces 
      real rl_up ! longwave radiation emitted by urban surface to the atmosphere 
      real emiss ! mean emissivity of the urban surface
      real grdflx_urb ! ground heat flux
      integer iz,id
      integer iw,ix,iy

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

! Fix some usefull parameters for the computation of the sources or sinks

      do iz=kts,kte
         dz(iz)=z(iz+1)-z(iz)
      end do

! Interpolation on the "urban grid"
      call interpol(kms,kme,kts,kte,nzu,z,z_u,ua,ua_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,va,va_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt,pt_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pt0,pt0_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,pr,pr_u)
      call interpol(kms,kme,kts,kte,nzu,z,z_u,da,da_u)

                   
! Compute the modification of the radiation due to the buildings

      call modif_rad(iurb,ndu,nzu,z_u,ws,               &
                    drst,strd,ss,pb,                    &
                    tw,tg,albg,albw,emw,emg,            &
                    fww,fwg,fgw,fsw,fsg,                &
                    zr,deltar,ah,                       &
                    rs,rld,rsw,rsg,rlw,rlg)                       
 
! calculation of the urban albedo and the upward long wave radiation
       
       call upward_rad(ndu,nzu,ws,bs,                   &
                       sigma,pb,ss,                     &
                       tg,emg,albg,rlg,rsg,sfg,         & 
                       tw,emw,albw,rlw,rsw,sfw,         & 
                       tr,emr,albr,rld,rs,sfr,          & 
                       rs_abs,rl_up,emiss,grdflx_urb)               
        
! Compute the surface temperatures
     

      call surf_temp(nzu,ndu,pr_u,dt,ss,                & 
                    rs,rld,rsg,rlg,rsw,rlw,             &
                    tg,alag,csg,emg,albg,ptg,sfg,gfg,   &
                    tr,alar,csr,emr,albr,ptr,sfr,gfr,   &
                    tw,alaw,csw,emw,albw,ptw,sfw,gfw)  
      
      
! Compute the implicit and explicit components of the sources or sinks on the "urban grid"
       
      call buildings(ndu,nzu,z0,ua_u,va_u,                       & 
                     pt_u,pt0_u,ptg,ptr,da_u,ptw,drst,           &                      
                     uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,  & 
                     uhb_u,vhb_u,thb_u,ehb_u,ss,dt)                        
 
         
! Calculation of the sensible heat fluxes for the ground, the wall and roof
! Sensible Heat Flux = density * Cp_U * ( A* potential temperature + B )
! where A and B are the implicit and explicit components of the heat sources or sinks.
!  
! 
 
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
      
! calculation of the urban albedo and the upward long wave radiation
          
!!       call upward_rad(ndu,nzu,ws,bs,                      &
!!                       sigma,pb,ss,                        &
!!                       tg,emg,albg,rlg,rsg,sfg,            & 
!!                       tw,emw,albw,rlw,rsw,sfw,            & 
!!                       tr,emr,albr,rld,rs,sfr,             & 
!!                       rs_abs,rl_up,emiss,grdflx_urb)             

! Interpolation on the "mesoscale grid"

      call urban_meso(ndu,kms,kme,kts,kte,nzu,z,dz,z_u,pb,ss,bs,ws,sf, & 
                     vl,uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u,     &
                     uhb_u,vhb_u,thb_u,ehb_u,                          &
                     a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e)                    
       

! computation of the mean road temperature tsk (this value could be used 
! to replace the surface temperature in the radiation routines, if needed).

! Calculation of the length scale taking into account the buildings effects

      call interp_length(ndu,kms,kme,kts,kte,nzu,z_u,z,ss,ws,bs,dlg,dl_u)

      return
      end subroutine BEP1D

! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine param(iurb,nzu,nzurb,nzurban,ndu,                   &
                       csg_u,csg,alag_u,alag,csr_u,csr,               &
                       alar_u,alar,csw_u,csw,alaw_u,alaw,             &
                       ws_u,ws,bs_u,bs,z0g_u,z0r_u,z0,                &  
                       strd_u,strd,drst_u,drst,ss_u,ss_urb,ss,pb_u,   &
                       pb_urb,pb,lp_urb,lb_urb,hgt_urb,frc_urb)        

! ----------------------------------------------------------------------
!    This routine prepare some usefull parameters       
! ----------------------------------------------------------------------

      implicit none

  
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer iurb                 ! Current urban class
      integer nzu                  ! Number of vertical urban levels in the current class
      integer nzurb                ! Number of vertical urban levels in the current class
      integer ndu                  ! Number of street direction for the current urban class
      real alag_u(nurbm)           ! Ground thermal diffusivity [m^2 s^-1]
      real alar_u(nurbm)           ! Roof thermal diffusivity [m^2 s^-1]
      real alaw_u(nurbm)           ! Wall thermal diffusivity [m^2 s^-1]
      real bs_u(ndm,nurbm)         ! Building width
      real csg_u(nurbm)            ! Specific heat of the ground material [J m^3 K^-1]
      real csr_u(nurbm)            ! Specific heat of the roof material [J m^3 K^-1]
      real csw_u(nurbm)            ! Specific heat of the wall material [J m^3 K^-1]
      real drst_u(ndm,nurbm)       ! Street direction
      real strd_u(ndm,nurbm)       ! Street length 
      real ws_u(ndm,nurbm)         ! Street width
      real z0g_u(nurbm)            ! The ground's roughness length
      real z0r_u(nurbm)            ! The roof's roughness length
      real ss_u(nz_um,nurbm)     ! The probability that a building has an height equal to "z"
      real pb_u(nz_um,nurbm)     ! The probability that a building has an height greater or equal to "z"
      real ss_urb(nz_um)     ! The probability that a building has an height equal to "z"
      real pb_urb(nz_um)     ! The probability that a building has an height greater or equal to "z"
      real lp_urb                ! Building plan area density
      real lb_urb                ! Building surface area to plan area ratio
      real hgt_urb               ! Average building height weighted by building plan area [m]
      real frc_urb               ! Urban fraction
     
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real alag(ng_u)           ! Ground thermal diffusivity at each ground levels
      real alar(nwr_u)           ! Roof thermal diffusivity at each roof levels
      real alaw(nwr_u)           ! Wall thermal diffusivity at each wall levels
      real csg(ng_u)            ! Specific heat of the ground material at each ground levels
      real csr(nwr_u)            ! Specific heat of the roof material at each roof levels
      real csw(nwr_u)            ! Specific heat of the wall material at each wall levels
      real bs(ndm)              ! Building width for the current urban class
      real drst(ndm)            ! street directions for the current urban class
      real strd(ndm)            ! Street lengths for the current urban class
      real ws(ndm)              ! Street widths of the current urban class
      real z0(ndm,nz_um)      ! Roughness lengths "profiles"
      real ss(nz_um)          ! Probability to have a building with height h
      real pb(nz_um)          ! Probability to have a building with an height equal
      integer nzurban

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,ig,ir,iw,iz,ihu

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------     
!
!Initialize
!
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
!            write(*,*) 'WARNING, WIDTH OF THE BUILDING WRONG',id,bs(id)
!            write(*,*) 'WIDTH OF THE STREET',id,ws(id)
             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
          endif
          if ((ws(id)<=1.).OR.(ws(id)>=150.)) then
!            write(*,*) 'WARNING, WIDTH OF THE STREET WRONG',id,ws(id)
!            write(*,*) 'WIDTH OF THE BUILDING',id,bs(id)
             bs(id)=bs_u(id,iurb)
             ws(id)=ws_u(id,iurb)
          endif
       enddo
       return
       end subroutine param
       
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine interpol(kms,kme,kts,kte,nz_u,z,z_u,c,c_u)

! ----------------------------------------------------------------------
!  This routine interpolate para
!  meters from the "mesoscale grid" to
!  the "urban grid".
!  See p300 Appendix B.1 of the BLM paper.
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      integer kts,kte,kms,kme            
      real z(kms:kme)          ! Altitude of the cell interface
      real c(kms:kme)            ! Parameter which has to be interpolated
! Data relative to the "urban grid"
      integer nz_u          ! Number of levels
!!    real z_u(nz_u+1)      ! Altitude of the cell interface
      real z_u(nz_um)       ! Altitude of the cell interface
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
!!    real c_u(nz_u)        ! Interpolated paramters in the "urban grid"
      real c_u(nz_um)       ! Interpolated paramters in the "urban grid"

       
! LOCAL:
! ----------------------------------------------------------------------
      integer iz_u,iz
      real ctot,dz

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

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
         
! ===6=8===============================================================72       
! ===6=8===============================================================72     

      subroutine modif_rad(iurb,nd,nz_u,z,ws,drst,strd,ss,pb,    &
                          tw,tg,albg,albw,emw,emg,               &
                          fww,fwg,fgw,fsw,fsg,                   &
                          zr,deltar,ah,                          &    
                          rs,rl,rsw,rsg,rlw,rlg)                       
 
! ----------------------------------------------------------------------
! This routine computes the modification of the short wave and 
!  long wave radiation due to the buildings.
! ----------------------------------------------------------------------

      implicit none
 
 
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer iurb              ! current urban class
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! Number of layer in the urban grid
      real z(nz_um)           ! Height of the urban grid levels
      real ws(ndm)              ! Street widths of the current urban class
      real drst(ndm)            ! street directions for the current urban class
      real strd(ndm)            ! Street lengths for the current urban class
      real ss(nz_um)          ! probability to have a building with height h
      real pb(nz_um)          ! probability to have a building with an height equal
      real tw(2*ndm,nz_um,nwr_u) ! Temperature in each layer of the wall [K]
      real tg(ndm,ng_u)         ! Temperature in each layer of the ground [K]
      real albg                 ! Albedo of the ground for the current urban class
      real albw                 ! Albedo of the wall for the current urban class
      real emg                  ! Emissivity of ground for the current urban class
      real emw                  ! Emissivity of wall for the current urban class
      real fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real fsg(ndm,nurbm)             ! View factors from sky to ground
      real fsw(nz_um,ndm,nurbm)       ! View factors from sky to wall
      real fws(nz_um,ndm,nurbm)       ! View factors from wall to sky
      real fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      real ah                   ! Hour angle (it should come from the radiation routine)
      real zr                   ! zenith angle
      real deltar               ! Declination of the sun
      real rs                   ! solar radiation
      real rl                   ! downward flux of the longwave radiation
! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real rlg(ndm)             ! Long wave radiation at the ground
      real rlw(2*ndm,nz_um)     ! Long wave radiation at the walls
      real rsg(ndm)             ! Short wave radiation at the ground
      real rsw(2*ndm,nz_um)     ! Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------

      integer id,iz

!  Calculation of the shadow effects

      call shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,           &
                     rs,rsw,rsg)

! Calculation of the reflection effects          
      do id=1,nd
         call long_rad(iurb,nz_u,id,emw,emg,                 &
                      fwg,fww,fgw,fsw,fsg,tg,tw,rlg,rlw,rl,pb)
         
         call short_rad(iurb,nz_u,id,albw,albg,fwg,fww,fgw,rsg,rsw,pb)
  
      enddo
      
      return
      end subroutine modif_rad


! ===6=8===============================================================72  
! ===6=8===============================================================72     

      subroutine surf_temp(nz_u,nd,pr,dt,ss,rs,rl,rsg,rlg,rsw,rlw,     &
                          tg,alag,csg,emg,albg,ptg,sfg,gfg,             &
                          tr,alar,csr,emr,albr,ptr,sfr,gfr,             &
                          tw,alaw,csw,emw,albw,ptw,sfw,gfw)             
                 

! ----------------------------------------------------------------------
! Computation of the surface temperatures for walls, ground and roofs 
! ----------------------------------------------------------------------

      implicit none
      
      
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nz_u              ! Number of vertical layers defined in the urban grid
      integer nd                ! Number of street direction for the current urban class
      real alag(ng_u)           ! Ground thermal diffusivity for the current urban class [m^2 s^-1] 
      real alar(nwr_u)           ! Roof thermal diffusivity for the current urban class [m^2 s^-1]  
      real alaw(nwr_u)           ! Wall thermal diffusivity for the current urban class [m^2 s^-1]  
      real albg                 ! Albedo of the ground for the current urban class
      real albr                 ! Albedo of the roof for the current urban class
      real albw                 ! Albedo of the wall for the current urban class
      real csg(ng_u)            ! Specific heat of the ground material of the current urban class [J m^3 K^-1]
      real csr(nwr_u)            ! Specific heat of the roof material for the current urban class [J m^3 K^-1]
      real csw(nwr_u)            ! Specific heat of the wall material for the current urban class [J m^3 K^-1]
      real dt                   ! Time step
      real emg                  ! Emissivity of ground for the current urban class
      real emr                  ! Emissivity of roof for the current urban class
      real emw                  ! Emissivity of wall for the current urban class
      real pr(nz_um)            ! Air pressure
      real rs                   ! Solar radiation
      real rl                   ! Downward flux of the longwave radiation
      real rlg(ndm)             ! Long wave radiation at the ground
      real rlw(2*ndm,nz_um)     ! Long wave radiation at the walls
      real rsg(ndm)             ! Short wave radiation at the ground
      real rsw(2*ndm,nz_um)     ! Short wave radiation at the walls
      real sfg(ndm)             ! Sensible heat flux from ground (road)
      real sfr(ndm,nz_um)     ! Sensible heat flux from roofs
      real sfw(2*ndm,nz_um)     ! Sensible heat flux from walls
      real gfg(ndm)             ! Heat flux transferred from the surface of the ground (road) toward the interior
      real gfr(ndm,nz_um)     ! Heat flux transferred from the surface of the roof toward the interior
      real gfw(2*ndm,nz_um)     ! Heat flux transfered from the surface of the walls toward the interior
      real ss(nz_um)          ! Probability to have a building with height h
      real tg(ndm,ng_u)         ! Temperature in each layer of the ground [K]
      real tr(ndm,nz_um,nwr_u) ! Temperature in each layer of the roof [K]
      real tw(2*ndm,nz_um,nwr_u) ! Temperature in each layer of the wall [K]
      

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real ptg(ndm)             ! Ground potential temperatures 
      real ptr(ndm,nz_um)     ! Roof potential temperatures 
      real ptw(2*ndm,nz_um)     ! Walls potential temperatures 

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,ig,ir,iw,iz

      real rtg(ndm)             ! Total radiation at ground(road) surface (solar+incoming long+outgoing long)
      real rtr(ndm,nz_um)     ! Total radiation at roof surface (solar+incoming long+outgoing long)
      real rtw(2*ndm,nz_um)     ! Radiation at walls surface (solar+incoming long+outgoing long)
      real tg_tmp(ng_u)
      real tr_tmp(nwr_u)
      real tw_tmp(nwr_u)

      real dzg_u(ng_u)          ! Layer sizes in the ground

      real dzr_u(nwr_u)          ! Layers sizes in the roof
         
      real dzw_u(nwr_u)          ! Layer sizes in the wall
      

      data dzg_u /0.2,0.12,0.08,0.05,0.03,0.02,0.02,0.01,0.005,0.0025/
      data dzr_u /0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/   
      data dzw_u /0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.0025/
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

        
   
      do id=1,nd

!      Calculation for the ground surfaces
       do ig=1,ng_u
        tg_tmp(ig)=tg(id,ig)
       end do
!	        
       call soil_temp(ng_u,dzg_u,tg_tmp,ptg(id),alag,csg,        &
                     rsg(id),rlg(id),pr(1),                    &
                     dt,emg,albg,                              &
                     rtg(id),sfg(id),gfg(id))    
       do ig=1,ng_u
        tg(id,ig)=tg_tmp(ig)
       end do

!      Calculation for the roofs surfaces
         
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
            
       end do !iz

!      Calculation for the walls surfaces
         
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
                   
        end do !iz
	
      end do !id
      
      return
      end subroutine surf_temp
     
! ===6=8===============================================================72     
! ===6=8===============================================================72  

      subroutine buildings(nd,nz,z0,ua_u,va_u,pt_u,pt0_u,         &
                        ptg,ptr,da_u,ptw,                            &
                        drst,uva_u,vva_u,uvb_u,vvb_u,                &
                        tva_u,tvb_u,evb_u,                           &
                        uhb_u,vhb_u,thb_u,ehb_u,ss,dt)                  

! ----------------------------------------------------------------------
! This routine computes the sources or sinks of the different quantities 
! on the urban grid. The actual calculation is done in the subroutines 
! called flux_wall, and flux_flat.
! ----------------------------------------------------------------------

      implicit none

        
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nd                ! Number of street direction for the current urban class
      integer nz                ! number of vertical space steps
      real ua_u(nz_um)          ! Wind speed in the x direction on the urban grid
      real va_u(nz_um)          ! Wind speed in the y direction on the urban grid
      real da_u(nz_um)          ! air density on the urban grid
      real drst(ndm)            ! Street directions for the current urban class
      real dz
      real pt_u(nz_um)          ! Potential temperature on the urban grid
      real pt0_u(nz_um)         ! reference potential temperature on the urban grid
      real ptg(ndm)             ! Ground potential temperatures 
      real ptr(ndm,nz_um)     ! Roof potential temperatures 
      real ptw(2*ndm,nz_um)     ! Walls potential temperatures 
      real ss(nz_um)          ! probability to have a building with height h
      real z0(ndm,nz_um)      ! Roughness lengths "profiles"
      real dt ! time step


! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
! vertical surfaces (walls) and horizontal surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = A*X + B
!  Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u

      real uhb_u(ndm,nz_um)   ! U (wind component) Horizontal surfaces, B (explicit) term
      real uva_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, A (implicit) term
      real uvb_u(2*ndm,nz_um)   ! U (wind component)   Vertical surfaces, B (explicit) term
      real vhb_u(ndm,nz_um)   ! V (wind component) Horizontal surfaces, B (explicit) term
      real vva_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, A (implicit) term
      real vvb_u(2*ndm,nz_um)   ! V (wind component)   Vertical surfaces, B (explicit) term
      real thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term
      real ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term
  
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,iz

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       dz=dz_u

      do id=1,nd

!        Calculation at the ground surfaces
         call flux_flat(dz,z0(id,1),ua_u(1),va_u(1),pt_u(1),pt0_u(1),  &
                       ptg(id),uhb_u(id,1),                            & 
                       vhb_u(id,1),thb_u(id,1),ehb_u(id,1))            

!        Calculation at the roof surfaces    
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

!        Calculation at the wall surfaces         
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
!
       
         end do
         
      end do
                
      return
      end subroutine buildings
      

! ===6=8===============================================================72
! ===6=8===============================================================72

        subroutine urban_meso(nd,kms,kme,kts,kte,nz_u,z,dz,z_u,pb,ss,bs,ws,sf,vl,    &
                             uva_u,vva_u,uvb_u,vvb_u,tva_u,tvb_u,evb_u, &       
                             uhb_u,vhb_u,thb_u,ehb_u,                   &      
                             a_u,a_v,a_t,a_e,b_u,b_v,b_t,b_e)           

! ----------------------------------------------------------------------
!  This routine interpolates the parameters from the "urban grid" to the
!  "mesoscale grid".
!  See p300-301 Appendix B.2 of the BLM paper.  
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      integer kms,kme,kts,kte               
      real z(kms:kme)              ! Altitude above the ground of the cell interface
      real dz(kms:kme)               ! Vertical space steps

! Data relative to the "uban grid"
      integer nz_u              ! Number of layer in the urban grid
      integer nd                ! Number of street direction for the current urban class
      real bs(ndm)              ! Building widths of the current urban class
      real ws(ndm)              ! Street widths of the current urban class
      real z_u(nz_um)         ! Height of the urban grid levels
      real pb(nz_um)          ! Probability to have a building with an height equal
      real ss(nz_um)          ! Probability to have a building with height h
      real uhb_u(ndm,nz_um)   ! U (x-wind component) Horizontal surfaces, B (explicit) term
      real uva_u(2*ndm,nz_um)   ! U (x-wind component) Vertical surfaces, A (implicit) term
      real uvb_u(2*ndm,nz_um)   ! U (x-wind component) Vertical surfaces, B (explicit) term
      real vhb_u(ndm,nz_um)   ! V (y-wind component) Horizontal surfaces, B (explicit) term
      real vva_u(2*ndm,nz_um)   ! V (y-wind component) Vertical surfaces, A (implicit) term
      real vvb_u(2*ndm,nz_um)   ! V (y-wind component) Vertical surfaces, B (explicit) term
      real thb_u(ndm,nz_um)   ! Temperature        Horizontal surfaces, B (explicit) term
      real tva_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, A (implicit) term
      real tvb_u(2*ndm,nz_um)   ! Temperature          Vertical surfaces, B (explicit) term
      real ehb_u(ndm,nz_um)   ! Energy (TKE)       Horizontal surfaces, B (explicit) term
      real evb_u(2*ndm,nz_um)   ! Energy (TKE)         Vertical surfaces, B (explicit) term

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Data relative to the "mesoscale grid"
      real sf(kms:kme)             ! Surface of the "mesoscale grid" cells taking into account the buildings
      real vl(kms:kme)               ! Volume of the "mesoscale grid" cells taking into account the buildings
      real a_u(kms:kme)              ! Implicit component of the momentum sources or sinks in the X-direction
      real a_v(kms:kme)              ! Implicit component of the momentum sources or sinks in the Y-direction
      real a_t(kms:kme)              ! Implicit component of the heat sources or sinks
      real a_e(kms:kme)              ! Implicit component of the TKE sources or sinks
      real b_u(kms:kme)              ! Explicit component of the momentum sources or sinks in the X-direction
      real b_v(kms:kme)              ! Explicit component of the momentum sources or sinks in the Y-direction
      real b_t(kms:kme)              ! Explicit component of the heat sources or sinks
      real b_e(kms:kme)              ! Explicit component of the TKE sources or sinks
      
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real dzz
      real fact
      integer id,iz,iz_u
      real se,sr,st,su,sv
      real uet(kms:kme)                ! Contribution to TKE due to walls
      real veb,vta,vtb,vte,vtot,vua,vub,vva,vvb


! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ---------------------------------------------------------------------- 

! initialisation

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
            
! horizontal surfaces
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

! volume      
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
      
! horizontal surface impact  

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

! vertical surface impact
           
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

! ===6=8===============================================================72 

! ===6=8===============================================================72 

      subroutine interp_length(nd,kms,kme,kts,kte,nz_u,z_u,z,ss,ws,bs,              &
                             dlg,dl_u)

! ----------------------------------------------------------------------     
!    Calculation of the length scales
!    See p272-274 formula (22) and (24) of the BLM paper    
! ----------------------------------------------------------------------     
     
      implicit none


! ----------------------------------------------------------------------     
! INPUT:
! ----------------------------------------------------------------------     
      integer kms,kme,kts,kte                
      real z(kms:kme)              ! Altitude above the ground of the cell interface
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! Number of levels in the "urban grid"
      real z_u(nz_um)         ! Height of the urban grid levels
      real bs(ndm)              ! Building widths of the current urban class
      real ss(nz_um)          ! Probability to have a building with height h
      real ws(ndm)              ! Street widths of the current urban class


! ----------------------------------------------------------------------     
! OUTPUT:
! ----------------------------------------------------------------------     
      real dlg(kms:kme)              ! Height above ground (L_ground in formula (24) of the BLM paper). 
      real dl_u(kms:kme)             ! Length scale (lb in formula (22) ofthe BLM paper).

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real dlgtmp
      integer id,iz,iz_u
      real sftot
      real ulu,ssl

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
   
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

! ===6=8===============================================================72
! ===6=8===============================================================72   

      subroutine shadow_mas(nd,nz_u,zr,deltar,ah,drst,ws,ss,pb,z,         &
                           rs,rsw,rsg)
        
! ----------------------------------------------------------------------
!         Modification of short wave radiation to take into account
!         the shadow produced by the buildings
! ----------------------------------------------------------------------

      implicit none
     
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nd                ! Number of street direction for the current urban class
      integer nz_u              ! number of vertical layers defined in the urban grid
      real ah                   ! Hour angle (it should come from the radiation routine)
      real deltar               ! Declination of the sun
      real drst(ndm)            ! street directions for the current urban class
      real rs                   ! solar radiation
      real ss(nz_um)          ! probability to have a building with height h
      real pb(nz_um)          ! Probability that a building has an height greater or equal to h
      real ws(ndm)              ! Street width of the current urban class
      real z(nz_um)           ! Height of the urban grid levels
      real zr                   ! zenith angle

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real rsg(ndm)             ! Short wave radiation at the ground
      real rsw(2*ndm,nz_um)     ! Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer id,iz,jz
      real aae,aaw,bbb,phix,rd,rtot,wsd
      
! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      if(rs.eq.0.or.sin(zr).eq.1)then
         do id=1,nd
            rsg(id)=0.
            do iz=1,nz_u
               rsw(2*id-1,iz)=0.
               rsw(2*id,iz)=0.
            enddo
         enddo
      else            
!test              
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
         
! ===6=8===============================================================72     
! ===6=8===============================================================72     

      subroutine shade_wall(z1,z2,hu,phix,aa,ws,rd)

! ----------------------------------------------------------------------
! This routine computes the effects of a shadow induced by a building of 
! height hu, on a portion of wall between z1 and z2. See equation A10, 
! and correction described below formula A11, and figure A1. Basically rd
! is the ratio between the horizontal surface illuminated and the portion
! of wall. Referring to figure A1, multiplying radiation flux density on 
! a horizontal surface (rs) by x1-x2 we have the radiation energy per 
! unit time. Dividing this by z2-z1, we obtain the radiation flux 
! density reaching the portion of the wall between z2 and z1 
! (everything is assumed in 2D)
! ----------------------------------------------------------------------

      implicit none
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real aa                   ! Angle between the sun direction and the face of the wall (A12)
      real hu                   ! Height of the building that generates the shadow
      real phix                 ! Solar zenith angle
      real ws                   ! Width of the street
      real z1                   ! Height of the level z(iz)
      real z2                   ! Height of the level z(iz+1)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real rd                   ! Ratio between (x1-x2)/(z2-z1), see Fig. 1A. 
                                ! Multiplying rd by rs (radiation flux 
                                ! density on a horizontal surface) gives 
                                ! the radiation flux density on the 
                                ! portion of wall between z1 and z2. 
! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      real x1,x2                ! x1,x2 see Fig. A1.

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      x1=min((hu-z1)*tan(phix),max(0.,ws/sin(aa)))
      
      x2=max((hu-z2)*tan(phix),0.)

      rd=max(0.,sin(aa)*(max(0.,x1-x2))/(z2-z1))
      
      return
      end subroutine shade_wall

! ===6=8===============================================================72     
! ===6=8===============================================================72     

      subroutine long_rad(iurb,nz_u,id,emw,emg,                  &
                         fwg,fww,fgw,fsw,fsg,tg,tw,rlg,rlw,rl,pb)

! ----------------------------------------------------------------------
! This routine computes the effects of the reflections of long-wave 
! radiation in the street canyon by solving the system 
! of 2*nz_u+1 eqn. in 2*nz_u+1
! unkonwn defined in A4, A5 and A6 of the paper (pages 295 and 296).
! The system is solved by solving A X= B,
! with A matrix B vector, and X solution. 
! ----------------------------------------------------------------------

      implicit none

  
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real emg                        ! Emissivity of ground for the current urban class
      real emw                        ! Emissivity of wall for the current urban class
      real fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real fsg(ndm,nurbm)             ! View factors from sky to ground
      real fsw(nz_um,ndm,nurbm)       ! View factors from sky to wall
      real fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      integer id                      ! Current street direction
      integer iurb                    ! Current urban class
      integer nz_u                    ! Number of layer in the urban grid
      real pb(nz_um)                ! Probability to have a building with an height equal
      real rl                         ! Downward flux of the longwave radiation
      real tg(ndm,ng_u)               ! Temperature in each layer of the ground [K]
      real tw(2*ndm,nz_um,nwr_u)       ! Temperature in each layer of the wall [K]
      

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real rlg(ndm)                   ! Long wave radiation at the ground
      real rlw(2*ndm,nz_um)           ! Long wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer i,j
      real aaa(2*nz_um+1,2*nz_um+1)   ! terms of the matrix
      real bbb(2*nz_um+1)             ! terms of the vector

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------


! west wall
       
      do i=1,nz_u
        
        do j=1,nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.        
       
        do j=nz_u+1,2*nz_u
         aaa(i,j)=-(1.-emw)*fww(j-nz_u,i,id,iurb)*pb(j-nz_u+1)
        enddo
        
!!      aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)*pb(i+1)
        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i,id,iurb)
        
        bbb(i)=fsw(i,id,iurb)*rl+emg*fgw(i,id,iurb)*sigma*tg(id,ng_u)**4
        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*emw*sigma*fww(j,i,id,iurb)*       &
               tw(2*id,j,nwr_u)**4+                              &
               fww(j,i,id,iurb)*rl*(1.-pb(j+1))
        enddo
        
       enddo
       
! east wall

       do i=1+nz_u,2*nz_u
        
        do j=1,nz_u
         aaa(i,j)=-(1.-emw)*fww(j,i-nz_u,id,iurb)*pb(j+1)
        enddo
        
        do j=1+nz_u,2*nz_u
         aaa(i,j)=0.
        enddo
        
        aaa(i,i)=1.
        
!!      aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)*pb(i-nz_u+1)
        aaa(i,2*nz_u+1)=-(1.-emg)*fgw(i-nz_u,id,iurb)
        
        bbb(i)=fsw(i-nz_u,id,iurb)*rl+                           &     
              emg*fgw(i-nz_u,id,iurb)*sigma*tg(id,ng_u)**4

        do j=1,nz_u
         bbb(i)=bbb(i)+pb(j+1)*emw*sigma*fww(j,i-nz_u,id,iurb)*  &   
                tw(2*id-1,j,nwr_u)**4+                           &   
                fww(j,i-nz_u,id,iurb)*rl*(1.-pb(j+1))
        enddo
       
       enddo

! ground
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
             
! ===6=8===============================================================72
! ===6=8===============================================================72

       subroutine short_rad(iurb,nz_u,id,albw,                        & 
                           albg,fwg,fww,fgw,rsg,rsw,pb)

! ----------------------------------------------------------------------
! This routine computes the effects of the reflections of short-wave 
! (solar) radiation in the street canyon by solving the system 
! of 2*nz_u+1 eqn. in 2*nz_u+1
! unkonwn defined in A4, A5 and A6 of the paper (pages 295 and 296).
! The system is solved by solving A X= B,
! with A matrix B vector, and X solution. 
! ----------------------------------------------------------------------

      implicit none

  
      
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real albg                 ! Albedo of the ground for the current urban class
      real albw                 ! Albedo of the wall for the current urban class
      real fgw(nz_um,ndm,nurbm)       ! View factors from ground to wall
      real fwg(nz_um,ndm,nurbm)       ! View factors from wall to ground
      real fww(nz_um,nz_um,ndm,nurbm) ! View factors from wall to wall
      integer id                ! current street direction 
      integer iurb              ! current urban class
      integer nz_u              ! Number of layer in the urban grid
      real pb(nz_um)          ! probability to have a building with an height equal

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real rsg(ndm)             ! Short wave radiation at the ground
      real rsw(2*ndm,nz_um)     ! Short wave radiation at the walls

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer i,j
      real aaa(2*nz_um+1,2*nz_um+1)  ! terms of the matrix
      real bbb(2*nz_um+1)            ! terms of the vector

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------

      
! west wall
       
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
       
! east wall

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

! ground

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
             

! ===6=8===============================================================72     
! ===6=8===============================================================72     
      
      subroutine gaussj(a,n,b,np)

! ----------------------------------------------------------------------
! This routine solve a linear system of n equations of the form
!              A X = B
!  where  A is a matrix a(i,j)
!         B a vector and X the solution
! In output b is replaced by the solution     
! ----------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer np
      real a(np,np)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real b(np)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer nmax
      parameter (nmax=150)

      real big,dum
      integer i,icol,irow
      integer j,k,l,ll,n
      integer ipiv(nmax)
      real pivinv

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       
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
                     CALL wrf_error_fatal('singular matrix in gaussj')
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
         
         if(a(icol,icol).eq.0) CALL wrf_error_fatal('singular matrix in gaussj')
         
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
         
! ===6=8===============================================================72     
! ===6=8===============================================================72     
       
      subroutine soil_temp(nz,dz,temp,pt,ala,cs,                       &
                          rs,rl,press,dt,em,alb,rt,sf,gf)

! ----------------------------------------------------------------------
! This routine solves the Fourier diffusion equation for heat in 
! the material (wall, roof, or ground). Resolution is done implicitely.
! Boundary conditions are: 
! - fixed temperature at the interior
! - energy budget at the surface
! ----------------------------------------------------------------------

      implicit none

     
                
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      integer nz                ! Number of layers
      real ala(nz)              ! Thermal diffusivity in each layers [m^2 s^-1] 
      real alb                  ! Albedo of the surface
      real cs(nz)               ! Specific heat of the material [J m^3 K^-1]
      real dt                   ! Time step
      real em                   ! Emissivity of the surface
      real press                ! Pressure at ground level
      real rl                   ! Downward flux of the longwave radiation
      real rs                   ! Solar radiation
      real sf                   ! Sensible heat flux at the surface
      real temp(nz)             ! Temperature in each layer [K]
      real dz(nz)               ! Layer sizes [m]


! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
      real gf                   ! Heat flux transferred from the surface toward the interior
      real pt                   ! Potential temperature at the surface
      real rt                   ! Total radiation at the surface (solar+incoming long+outgoing long)

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
      integer iz
      real a(nz,3)
      real alpha
      real c(nz)
      real cddz(nz+2)
      real tsig

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
       
      tsig=temp(nz)
      alpha=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf
! Compute cddz=2*cd/dz  
        
      cddz(1)=ala(1)/dz(1)
      do iz=2,nz
         cddz(iz)=2.*ala(iz)/(dz(iz)+dz(iz-1))
      enddo
!        cddz(nz+1)=ala(nz+1)/dz(nz)
       
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
                        
!      gf=-cddz(nz)*(temp(nz)-temp(nz-1))*cs(nz)
       gf=(1.-alb)*rs+em*rl-em*sigma*(tsig**4)+sf                                   
      return
      end subroutine soil_temp

! ===6=8===============================================================72 
! ===6=8===============================================================72 

      subroutine invert(n,a,c,x)

! ----------------------------------------------------------------------
!        Inversion and resolution of a tridiagonal matrix
!                   A X = C
! ----------------------------------------------------------------------

      implicit none
                
! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
       integer n
       real a(n,3)              !  a(*,1) lower diagonal (Ai,i-1)
                                !  a(*,2) principal diagonal (Ai,i)
                                !  a(*,3) upper diagonal (Ai,i+1)
       real c(n)

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
       real x(n)    

! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
       integer i

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------
                     
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
  

! ===6=8===============================================================72  
! ===6=8===============================================================72
  
      subroutine flux_wall(ua,va,pt,da,ptw,uva,vva,uvb,vvb,            &
                                  tva,tvb,evb,drst,dt)      
       
! ----------------------------------------------------------------------
! This routine computes the surface sources or sinks of momentum, tke,
! and heat from vertical surfaces (walls).   
! ----------------------------------------------------------------------

      implicit none

    
         
! INPUT:
! -----
      real drst                 ! street directions for the current urban class
      real da                   ! air density
      real pt                   ! potential temperature
      real ptw                  ! Walls potential temperatures 
      real ua                   ! wind speed
      real va                   ! wind speed

      real dt                   !time step
! OUTPUT:
! ------
! Explicit and implicit component of the momentum, temperature and TKE sources or sinks on
! vertical surfaces (walls).
! The fluxes can be computed as follow: Fluxes of X = A*X + B
!  Example: Momentum fluxes on vertical surfaces = uva_u * ua_u + uvb_u
      real uva                  ! U (wind component)   Vertical surfaces, A (implicit) term
      real uvb                  ! U (wind component)   Vertical surfaces, B (explicit) term
      real vva                  ! V (wind component)   Vertical surfaces, A (implicit) term
      real vvb                  ! V (wind component)   Vertical surfaces, B (explicit) term
      real tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real evb                  ! Energy (TKE)         Vertical surfaces, B (explicit) term

! LOCAL:
! -----
      real hc
      real u_ort
      real vett

! -------------------------
! END VARIABLES DEFINITIONS
! -------------------------

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
         
!         tvb=hc*ptw/da/cp_u
!         tva=-hc/da/cp_u
!!!!!!!!!!!!!!!!!!!!
! explicit 
      tvb=hc*ptw/da/cp_u-hc/da/cp_u*pt !c
      tva = 0.                  !c
         
      evb=cdrag*(abs(u_ort)**3.)/2.
              
      return
      end subroutine flux_wall
         
! ===6=8===============================================================72

! ===6=8===============================================================72

      subroutine flux_flat(dz,z0,ua,va,pt,pt0,ptg,                     &
                          uhb,vhb,thb,ehb)
                                
! ----------------------------------------------------------------------
!           Calculation of the flux at the ground 
!           Formulation of Louis (Louis, 1979)       
! ----------------------------------------------------------------------

      implicit none

     

! ----------------------------------------------------------------------
! INPUT:
! ----------------------------------------------------------------------
      real dz                   ! first vertical level
      real pt                   ! potential temperature
      real pt0                  ! reference potential temperature
      real ptg                  ! ground potential temperature
      real ua                   ! wind speed
      real va                   ! wind speed
      real z0                   ! Roughness length

! ----------------------------------------------------------------------
! OUTPUT:
! ----------------------------------------------------------------------
! Explicit component of the momentum, temperature and TKE sources or sinks on horizontal 
!  surfaces (roofs and street)
! The fluxes can be computed as follow: Fluxes of X = B
!  Example: Momentum fluxes on horizontal surfaces =  uhb_u
      real uhb                  ! U (wind component) Horizontal surfaces, B (explicit) term
      real vhb                  ! V (wind component) Horizontal surfaces, B (explicit) term
      real thb                  ! Temperature        Horizontal surfaces, B (explicit) term
      real tva                  ! Temperature          Vertical surfaces, A (implicit) term
      real tvb                  ! Temperature          Vertical surfaces, B (explicit) term
      real ehb                  ! Energy (TKE)       Horizontal surfaces, B (explicit) term


! ----------------------------------------------------------------------
! LOCAL:
! ----------------------------------------------------------------------
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

! ----------------------------------------------------------------------
! END VARIABLES DEFINITIONS
! ----------------------------------------------------------------------


! computation of the ground temperature
         
      utot=(ua**2+va**2)**.5
        
      
!!!! Louis formulation
!
! compute the bulk Richardson Number

      zz=dz/2.
   
!        if(tstar.lt.0.)then
!         wstar=(-ustar*tstar*g*hii/pt)**(1./3.)
!        else
!         wstar=0.
!        endif
!        
!      if (utot.le.0.7*wstar) utot=max(0.7*wstar,0.00001)

      utot=max(utot,0.01)
          
      ric=2.*g_u*zz*(pt-ptg)/((pt+ptg)*(utot**2))
              
      aa=vk/log(zz/z0)

! determine the parameters fm and fh for stable, neutral and unstable conditions

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
!      thb= 0.      
      ehb=buu
!!!!!!!!!!!!!!!
         
      return
      end subroutine flux_flat

! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine icBEP (nd_u,h_b,d_b,ss_u,pb_u,nz_u,z_u)                               

      implicit none       
        

!    Street parameters
      integer nd_u(nurbm)     ! Number of street direction for each urban class
      real h_b(nz_um,nurbm)   ! Bulding's heights [m]
      real d_b(nz_um,nurbm)   ! The probability that a building has an height h_b
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

      real ss_u(nz_um,nurbm)     ! The probability that a building has an height equal to z
      real pb_u(nz_um,nurbm)     ! The probability that a building has an height greater or equal to z
        
!    Grid parameters
      integer nz_u(nurbm)     ! Number of layer in the urban grid
      real z_u(nz_um)       ! Height of the urban grid levels


! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer iz_u,id,ilu,iurb

      real dtot
      real hbmax

!------------------------------------------------------------------------
     

! -----------------------------------------------------------------------
!     This routine initialise the urban paramters for the BEP module
!------------------------------------------------------------------------
!
!Initialize variables
!
      z_u=0.
      nz_u=0
      ss_u=0.
      pb_u=0.

! Computation of the urban levels height

      z_u(1)=0.
     
      do iz_u=1,nz_um-1
         z_u(iz_u+1)=z_u(iz_u)+dz_u
      enddo
      
! Normalisation of the building density

      do iurb=1,nurbm
         dtot=0.
         do ilu=1,nz_um
            dtot=dtot+d_b(ilu,iurb)
         enddo
         do ilu=1,nz_um
            d_b(ilu,iurb)=d_b(ilu,iurb)/dtot
         enddo
      enddo      

! Compute the view factors, pb and ss 
      
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

! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine view_factors(iurb,nz_u,id,dxy,z,ws,fww,fwg,fgw,fsg,fsw,fws) 
     
      implicit none

 

! -----------------------------------------------------------------------
!     Input
!------------------------------------------------------------------------

      integer iurb            ! Number of the urban class
      integer nz_u            ! Number of levels in the urban grid
      integer id              ! Street direction number
      real ws                 ! Street width
      real z(nz_um)         ! Height of the urban grid levels
      real dxy                ! Street lenght


! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

!   fww,fwg,fgw,fsw,fsg are the view factors used to compute the long wave
!   and the short wave radation. They are the part of radiation from a surface
!   or from the sky to another surface.

      real fww(nz_um,nz_um,ndm,nurbm)            !  from wall to wall
      real fwg(nz_um,ndm,nurbm)                  !  from wall to ground
      real fgw(nz_um,ndm,nurbm)                  !  from ground to wall
      real fsw(nz_um,ndm,nurbm)                  !  from sky to wall
      real fws(nz_um,ndm,nurbm)                  !  from wall to sky
      real fsg(ndm,nurbm)                        !  from sky to ground


! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer jz,iz

      real hut
      real f1,f2,f12,f23,f123,ftot
      real fprl,fnrm
      real a1,a2,a3,a4,a12,a23,a123

! -----------------------------------------------------------------------
!     This routine calculates the view factors
!------------------------------------------------------------------------
        
      hut=z(nz_u+1)
        
      do jz=1,nz_u      
      
! radiation from wall to wall
       
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

! radiation from ground to wall
       
         call fnrms (fnrm,z(jz+1),dxy,ws)
         f12=fnrm
         call fnrms (fnrm,z(jz)  ,dxy,ws)
         f1=fnrm
       
         a1 = ws*dxy
         
         a12= ws*dxy
       
         a4=(z(jz+1)-z(jz))*dxy
       
         ftot=(a12*f12-a12*f1)/a1
                    
         fgw(jz,id,iurb)=ftot*a1/a4
     
!  radiation from sky to wall
     
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

! radiation from wall to sky      
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
!!!!!!!!!!!!!


       do iz=1,nz_u

! radiation from wall to ground
      
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

! radiation from sky to ground
      
      call fprls (fprl,dxy,ws,hut)
      fsg(id,iurb)=fprl

      return
      end subroutine view_factors

! ===6=8===============================================================72
! ===6=8===============================================================72

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

! ===6=8===============================================================72     
! ===6=8===============================================================72

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
  ! ===6=8===============================================================72  
     
      SUBROUTINE init_para(alag_u,alaw_u,alar_u,csg_u,csw_u,csr_u,&
                twini_u,trini_u,tgini_u,albg_u,albw_u,albr_u,emg_u,emw_u,&
                emr_u,z0g_u,z0r_u,nd_u,strd_u,drst_u,ws_u,bs_u,h_b,d_b)

! initialization routine, where the variables from the table are read

      implicit none
      
      integer iurb            ! urban class number
!    Building parameters      
      real alag_u(nurbm)      ! Ground thermal diffusivity [m^2 s^-1]
      real alaw_u(nurbm)      ! Wall thermal diffusivity [m^2 s^-1]
      real alar_u(nurbm)      ! Roof thermal diffusivity [m^2 s^-1]
      real csg_u(nurbm)       ! Specific heat of the ground material [J m^3 K^-1]
      real csw_u(nurbm)       ! Specific heat of the wall material [J m^3 K^-1]
      real csr_u(nurbm)       ! Specific heat of the roof material [J m^3 K^-1]
      real twini_u(nurbm)     ! Temperature inside the buildings behind the wall [K]
      real trini_u(nurbm)     ! Temperature inside the buildings behind the roof [K]
      real tgini_u(nurbm)     ! Initial road temperature

!    Radiation parameters
      real albg_u(nurbm)      ! Albedo of the ground
      real albw_u(nurbm)      ! Albedo of the wall
      real albr_u(nurbm)      ! Albedo of the roof
      real emg_u(nurbm)       ! Emissivity of ground
      real emw_u(nurbm)       ! Emissivity of wall
      real emr_u(nurbm)       ! Emissivity of roof

!    Roughness parameters
      real z0g_u(nurbm)       ! The ground's roughness length      
      real z0r_u(nurbm)       ! The roof's roughness length

!    Street parameters
      integer nd_u(nurbm)     ! Number of street direction for each urban class

      real strd_u(ndm,nurbm)  ! Street length (fix to greater value to the horizontal length of the cells)
      real drst_u(ndm,nurbm)  ! Street direction [degree]
      real ws_u(ndm,nurbm)    ! Street width [m]
      real bs_u(ndm,nurbm)    ! Building width [m]
      real h_b(nz_um,nurbm)   ! Bulding's heights [m]
      real d_b(nz_um,nurbm)   ! The probability that a building has an height h_b

      integer i,iu
      integer nurb ! number of urban classes used

!
!Initialize some variables
!
       
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
!==============================================================

!==============================================================
      subroutine angle(along,alat,day,realt,zr,deltar,ah)
!     ----------------     
!      
!         Computation of the solar angles
!         schayes (1982,atm.  env. , p1407)
! Inputs
!========================
! along=longitud
! alat=latitude
! day=julian day (from the beginning of the year)
! realt= time GMT in hours
! Outputs
!============================
! zr=solar zenith angle
! deltar=declination angle
! ah=hour angle
!===============================

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
!
!     declination
!
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
!
!     time equation in hours
!
       et=0.0072*c1-0.0528*c2-0.0012*c3-0.1229*s1-0.1565*s2-0.0041*s3
!
!
!   hour angle  
!
      
!      ifh=0
      
 !     ahor=realt-12.+ifh+et+alongt
      ahor=realt-12.+et+alongt
      ah=ahor*radh
      chor=cos(ah)
!
!    zenith angle
!
      coznt=sphi*sid+cphi*cd*chor
      
       zr=acos(coznt)

      return

      END SUBROUTINE angle
!
!====6=8===============================================================72         
!====6=8===============================================================72 

      subroutine upward_rad(ndu,nzu,ws,bs,sigma,pb,ss,                     &
                       tg,emg_u,albg_u,rlg,rsg,sfg,                        & 
                       tw,emw_u,albw_u,rlw,rsw,sfw,                        & 
                       tr,emr_u,albr_u,rld,rs, sfr,                        & 
                       rs_abs,rl_up,emiss,grdflx_urb)
!
! IN this surboutine we compute the upward longwave flux, and the albedo
! needed for the radiation scheme
!
                       implicit none

!
!INPUT VARIABLES
!
      real rsw(2*ndm,nz_um)        ! Short wave radiation at the wall for a given canyon direction [W/m2]
      real rlw(2*ndm,nz_um)         ! Long wave radiation at the walls for a given canyon direction [W/m2]
      real rsg(ndm)                   ! Short wave radiation at the canyon for a given canyon direction [W/m2]
      real rlg(ndm)                   ! Long wave radiation at the ground for a given canyon direction [W/m2]
      real rs                        ! Short wave radiation at the horizontal surface from the sun [W/m2]  
      real sfw(2*ndm,nz_um)      ! Sensible heat flux from walls [W/m2]
      real sfg(ndm)              ! Sensible heat flux from ground (road) [W/m2]
      real sfr(ndm,nz_um)      ! Sensible heat flux from roofs [W/m2]                      
      real rld                        ! Long wave radiation from the sky [W/m2]
      real albg_u                    ! albedo of the ground/street
      real albw_u                    ! albedo of the walls
      real albr_u                    ! albedo of the roof 
      real ws(ndm)                        ! width of the street
      real bs(ndm)
                        ! building size
      real pb(nz_um)                ! Probability to have a building with an height equal or higher   
      integer nzu
      real ss(nz_um)                ! Probability to have a building of a given height
      real sigma                       
      real emg_u                       ! emissivity of the street
      real emw_u                       ! emissivity of the wall
      real emr_u                       ! emissivity of the roof
      real tw(2*ndm,nz_um,nwr_u)  ! Temperature in each layer of the wall [K]
      real tr(ndm,nz_um,nwr_u)  ! Temperature in each layer of the roof [K]
      real tg(ndm,ng_u)          ! Temperature in each layer of the ground [K]
      integer id ! street direction
      integer ndu ! number of street directions
!OUTPUT/INPUT
      real rs_abs  ! absrobed solar radiationfor this street direction
      real rl_up   ! upward longwave radiation for this street direction
      real emiss ! mean emissivity
      real grdflx_urb ! ground heat flux 
!LOCAL
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

!====6=8===============================================================72         
!====6=8===============================================================72 
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine icBEP_XY(iurb,fww_u,fwg_u,fgw_u,fsw_u,             &
                          fws_u,fsg_u,ndu,strd,ws,nzu,z_u)                               

      implicit none       
        
!    Street parameters
      integer ndu     ! Number of street direction for each urban class
      integer iurb

      real strd(ndm)        ! Street length (fix to greater value to the horizontal length of the cells)
      real ws(ndm)          ! Street width [m]

!    Grid parameters
      integer nzu          ! Number of layer in the urban grid
      real z_u(nz_um)       ! Height of the urban grid levels
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

!   fww_u,fwg_u,fgw_u,fsw_u,fsg_u are the view factors used to compute the long wave
!   and the short wave radation. They are the part of radiation from a surface
!   or from the sky to another surface.

      real fww_u(nz_um,nz_um,ndm,nurbm)         !  from wall to wall
      real fwg_u(nz_um,ndm,nurbm)               !  from wall to ground
      real fgw_u(nz_um,ndm,nurbm)               !  from ground to wall
      real fsw_u(nz_um,ndm,nurbm)               !  from sky to wall
      real fws_u(nz_um,ndm,nurbm)               !  from sky to wall
      real fsg_u(ndm,nurbm)                     !  from sky to ground

! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------

      integer id

! -----------------------------------------------------------------------
!     This routine compute the view factors
!------------------------------------------------------------------------
!
!Initialize
!
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
! ===6=8===============================================================72
! ===6=8===============================================================72

      subroutine icBEPHI_XY(hb_u,hi_urb1D,ss_u,pb_u,nzu,z_u)

      implicit none   
!-----------------------------------------------------------------------
!    Inputs
!-----------------------------------------------------------------------
!    Street parameters
!
      real hi_urb1D(nz_um)    ! The probability that a building has an height h_b
!
!     Grid parameters
!
      real z_u(nz_um)         ! Height of the urban grid levels
! -----------------------------------------------------------------------
!     Output
!------------------------------------------------------------------------

      real ss_u(nz_um)   ! The probability that a building has an height equal to z
      real pb_u(nz_um)         ! The probability that a building has an height greater or equal to z
!        
!    Grid parameters
!
      integer nzu                ! Number of layer in the urban grid

! -----------------------------------------------------------------------
!     Local
!------------------------------------------------------------------------
      real hb_u(nz_um)        ! Bulding's heights [m]
      integer iz_u,id,ilu

      real dtot
      real hbmax

!------------------------------------------------------------------------

!Initialize variables
!
      
      nzu=0
      ss_u=0.
      pb_u=0.
      
! Normalisation of the building density

         dtot=0.
         hb_u=0.

         do ilu=1,nz_um
            dtot=dtot+hi_urb1D(ilu)
         enddo
         
         do ilu=1,nz_um
            if (hi_urb1D(ilu)<0.) then
!              write(*,*) 'WARNING, HI_URB1D(ilu) < 0 IN BEP'
               go to 20
            endif
         enddo

         if (dtot.gt.0.) then
            continue
         else
!           write(*,*) 'WARNING, HI_URB1D <= 0 IN BEP'
            go to 20
         endif

         do ilu=1,nz_um
            hi_urb1D(ilu)=hi_urb1D(ilu)/dtot
         enddo
         
         hb_u(1)=dz_u   
         do ilu=2,nz_um
            hb_u(ilu)=dz_u+hb_u(ilu-1)
         enddo
           

! Compute pb and ss 
      
            
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
! ===6=8===============================================================72
! ===6=8===============================================================72
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
      
