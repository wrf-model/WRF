!WRF:MEDIATION_LAYER:PHYSICS
! *** add new modules of schemes here
!
MODULE module_dust_emis
CONTAINS

!====================================================================================
! simple dust emission scheme outside of WRF_CHEM
!       revised from s/r mosaic_dust_gocartemis
!         Mei Xu 2017Jan31 and additions by G. Thompson 2017Sep19
!====================================================================================
  subroutine bulk_dust_emis (ktau,dt,num_soil_layers,u_phy,v_phy,          &
         rho_phy,alt,u10,v10,p8w,dz8w,smois,erod,                          &
         ivgtyp,isltyp,vegfra,albbck,xland,dx,g,                           &
         nifa2d,                                                           &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!====================================================================================
! INPUT:
! ktau          - simulation time
! dt            - size of timestep
! u_phy,v_phy   - state variables of wind
! rho_phy,alt   - state variable of density and inverse of density
! dz8w,p8w      - model layer depth 
! ivgtyp,isltyp - dominant vegetation, soil type
! vegfra        - vegetation fraction (0-100)
! albbck        - background sfc albedo used to increase erod where desert-like
! xland         - land mask (1: land, 2: water)
! dx, g         - model grid increment, gravity constant
! smois         - soil moisture
! erod          - fraction of erodible grid cell, for 1: Sand, 2: Silt, 3: Clay
!               - read in from wrfinput
! OUTPUT:
! nifa2d        - dust generation in #/kg/s - to be used to update QNIFA (# kg-1)
!------------------------------------------------------------------------------------
! Local variables:
! nmx           - number of dust size bins
! ilwi          - land/water flag
! bems          - binned dust emission in kg/timestep/cell 
! totalemis     - dust emission from 0.1-10 um in radius, in ug/m2/s
! erodin        - fraction of erodible grid cel,
! w10m          - total wind speed at 10 m
! gwet          - volumetric soil moisture over porosity
! dxy           - model grid box area
! airmas        - grid box air mass
! airden        - air density
! DSRC          - source of each dust type           (kg/timestep/cell)
! 
! USED from module_data_gocart_dust:
! porosity      -
! ch_dust       - Constant to fudge the total emission of dust  (s2/m2)
! frac_s        - mass fraction of dust
!====================================================================================

  USE module_data_gocart_dust

  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: ktau, num_soil_layers,           &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp,               &
                                                     isltyp
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) ::                        &
                                                     vegfra,               &
                                                     albbck
   REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) ,      &
      INTENT(INOUT) ::                               smois
   REAL,  DIMENSION( ims:ime , jms:jme, 3 )                   ,               &
          INTENT(IN   ) ::    erod
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN   ) ::                                                 &
                                                     u10,                  &
                                                     v10,                  &
                                                     xland
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::                                                 &
                                                        alt,               &
                                                     dz8w,p8w,             &
                                              u_phy,v_phy,rho_phy

  REAL, INTENT(IN   ) :: dt,dx,g
  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: NIFA2D
!
! local variables
!
  integer :: i,j,k, nmx, month, n, m, maxflux
  real*8  w10m,gwet,den,diam,airden,airmas,erodin
  real*8  dxy
  real*8  converi
  REAL*8  u_ts0, u_ts, dsrc, srce
  real rhoa, g0, totalemis, pi, dustmas

  converi=1.e9
  g0 = g*1.0E2
  pi = 3.14159
  dxy=dx*dx
  maxflux = 0

  do j=jts,jte
  do i=its,ite
     nifa2d(i,j) = 0.0
  enddo
  enddo

  nmx = 5
! consider 5 dust diameter/density as in module_data_gocart_dust
  do n = 1, nmx

  den = den_dust(n)*1.0D-3            ! in g/cm3
  diam = 2.0*reff_dust(n)*1.0D2       ! in cm
  dustmas = (pi/6.) * den * (diam)**3 ! in g
  ch_dust(n,:)=1.0D-9  ! default is 1 ug s2 m-5 == 1.e-9 kg s2 m-5
  month = 3   ! it doesn't matter, ch_dust is not a month dependent now, a constant
  m = ipoint(n)  ! which of the 3 classes of fractional erod data is to be used; ipoint(3)=2

  k=kts
  do j=jts,jte
  do i=its,ite

    dsrc = 0.0
!
! don't do dust over water!!!
! we still need to add in a check for snow cover on dusty ground. TO DO item

    if(xland(i,j).lt.1.5) then

      w10m=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
      airmas=-(p8w(i,kts+1,j)-p8w(i,kts,j))*dxy/g   ! kg 

! we don't trust the u10,v10 values, if model layers are very thin near surface
      if(dz8w(i,kts,j).lt.12.)w10m=sqrt(u_phy(i,kts,j)*u_phy(i,kts,j)+v_phy(i,kts,j)*v_phy(i,kts,j))

! consider using a gust factor of 33 percent higher than sustained wind (Cao and Fovell, 2018; also G. Bryan)
      w10m = w10m*1.33

      erodin=erod(i,j,m)

! increase erodability where the surface albedo is high to account better for real deserts
      if (erodin .gt. 1.E-8 .AND. albbck(i,j).gt.0.175 .and. vegfra(i,j).lt.12.5) then
         erodin = MIN(0.5, erodin + 0.1*albbck(i,j))
      endif

!  volumetric soil moisture over porosity
      gwet=smois(i,1,j)/porosity(isltyp(i,j))
      airden=rho_phy(i,kts,j)
      rhoa = airden*1.0D-3

! Threshold velocity as a function of the dust density and the diameter from Bagnold (1941)
      u_ts0 = 0.13*1.0D-2*SQRT(den*g0*diam/rhoa)* &
              SQRT(1.0+0.006/den/g0/(diam)**2.5)/ &
              SQRT(1.928*(1331.0*(diam)**1.56+0.38)**0.092-1.0)

! Case of surface dry enough to erode
      IF (gwet < 0.5) THEN  !  Pete's modified value
!     IF (gwet < 0.2) THEN
         u_ts = MAX(0.0D+0,u_ts0*(1.2D+0+2.0D-1*LOG10(MAX(1.0D-3, gwet))))
      ELSE
! Case of wet surface, no erosion
         u_ts = 100.0
      END IF
      srce = frac_s(n)*erodin*dxy  ! (m2)
!     srce = 1.1*erodin*dxy  ! (m2)
      dsrc = MAX(0.0, ch_dust(n,month)*srce*w10m*w10m *(w10m - u_ts)*dt) ! (kg)

! unit change from kg/timestep/cell to ug/m2/s
!   totalemis=((dsrc)/dt)*converi/dxy
!   totalemis=totalemis*1.01 !to account for the particles larger than 10 um based on assumed size distribution

! convert dust flux to number concentration (#/kg/s)
! nifa2d = dsrc/dt / dustParticle_mas /cell_air_mass

      nifa2d(i,j) = nifa2d(i,j) + dsrc/dt * 1000.0/dustmas/airmas

    endif ! xland
    maxflux = MAX(maxflux,int(nifa2d(i,j)))
  enddo ! i
  enddo ! j

  enddo ! n
! print*,'check maximum dust flux: ', maxflux

end subroutine bulk_dust_emis

END MODULE module_dust_emis
