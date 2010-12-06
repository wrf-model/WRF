!WRF:MODEL_LAYER:PHYSICS

MODULE module_wind_fitch

! implement the drag force for wind farm
! apply horizontal velocity tendencies in du,dv
! add source term for TKE in qke
! dz = vertical spacing in m
! phb = geopotential height
! theight = hub height in m
! tdiameter = turbine diameter in m
! thrustcof = standing thrust coeff.
! tpower = turbine power in MW
! cospeed = cut-out speed in m/s
! cispeed = cut-in speed in m/s
! lwfx = length of wind farm in X- direction in grid cells
! lwfy = length of wind farm in Y- direction in grid cells
! dcellx = distance between two wind turbines in X- direction in grid cells
! dcelly = distance between two wind turbines in Y- direction in grid cells
! pwfx = position of wind farm in X- direction in grid cells
! pwfy = position of wind farm in Y- direction in grid cells
! nturbine = no. of turbines per grid cell

  USE module_model_constants, only:  g

  IMPLICIT NONE

  PUBLIC  turbine_dragforce
  PRIVATE dragcof, wfdensityi

CONTAINS

  SUBROUTINE  turbine_dragforce( 
       & phb,u,v,dx,dz                           &
       &,theight,lwfx,lwfy,tdiameter,thrustcof   &
       &,cospeed,cispeed,dcellx,dcelly,tpower    &
       &,qke,du,dv,dt,pwfx,pwfy,nturbine         &
       &,ids,ide,jds,jde,kds,kde                 &
       &,ims,ime,jms,jme,kms,kme                 &
       &,its,ite,jts,jte,kts,kte                 &
       &)  

  INTEGER, INTENT(IN) :: its,ite,jts,jte,kts,kte
  INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme
  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde,itf,jtf,ktf
  REAL, INTENT(IN) :: dx,dt,tpower,nturbine
  REAL, INTENT(IN) :: tdiameter,theight,thrustcof,cospeed,cispeed
  INTEGER, INTENT(IN) :: lwfx,lwfy,pwfx,pwfy,dcellx,dcelly
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) ::dz,u,v,phb
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: du,dv,qke

! Local

  LOGICAL, DIMENSION(ims:ime,jms:jme) :: turbine
  REAL :: blade_l_point,blade_u_point,zheightl,zheightu,z1,z2,tarea,nt
  REAL :: l1,l2,speed,tkecof,powcof,thrcof,wfdensity
  INTEGER :: itf,jtf,ktf
  INTEGER :: i,j,k,swfindx,ewfindx,swfindy,ewfindy,n1,n2
  INTEGER, DIMENSION(ims:ime,1:2,jms:jme)::k_turbine

  LOGICAL :: kfound

  itf=MIN0(ite,ide-1)
  jtf=MIN0(jte,jde-1)
  ktf=MIN0(kte,kde-1)

  swfindx=pwfx ! x-coord of starting point of wind farm
  ewfindx=pwfx+lwfx-1 ! x-coord of ending point
  swfindy=pwfy ! y-coord of starting point
  ewfindy=pwfy+lwfy-1 ! y-coord of ending point

  wfdensity=nturbine/(dx*dx) ! no turbines per m2

  turbine = .false.

  blade_l_point=theight-tdiameter/2. ! height of lower point of blade (m) (theight=turbine height) 
  blade_u_point=theight+tdiameter/2. ! height of upper point of blade (m) 
! vertical layers cut by turbine blades
  k_turbine(:,1,:)=0      !bottom level
  k_turbine(:,2,:)=-1     !top level

! placing wind farm
  DO j=jts,jtf
     n1=mod(j-swfindy+1,dcelly)
      IF(j .GE. swfindy .AND. j .LE. ewfindy .AND. n1 .EQ. 0) THEN
          DO i=its,itf
             n2=mod(i-swfindx+1,dcellx)
              IF(i .GE. swfindx .and. i .LE. ewfindx .AND. n2 .EQ. 0) THEN
                 turbine(i,j) = .true.
              ENDIF
          ENDDO
      ENDIF
  ENDDO
! Finding vertical levels penetrated by wind turbine
  DO j=jts,jtf
     DO i=its,itf
        kfound = .false.
        zheightl=0.0
        IF(turbine(i,j)) THEN
           DO k=kts,ktf
              IF(kfound .EQ. .false.) THEN
                 zheightu = zheightl + dz(i,k,j) ! increment height

                 IF(blade_l_point .GE. zheightl .AND. blade_l_point .LE. zheightu) THEN
                    k_turbine(i,1,j)=k ! blade lower tip cuts this level
                 ENDIF

                 IF(blade_u_point .GE. zheightl .AND. blade_u_point .LE. zheightu) THEN
                    k_turbine(i,2,j)=k ! blade upper tip cuts this level
                    kfound = .TRUE.
                 ENDIF
                 zheightl = zheightu
              ENDIF
           ENDDO
        ENDIF
     ENDDO
  ENDDO
! Implementing drag force
  l1=theight-tdiameter/2. ! height of lower tip of blade above ground (m)
  l2=theight+tdiameter/2. ! height of upper tip of blade above ground (m)
  DO j=jts,jtf
     DO i=its,itf
        DO k=k_turbine(i,1,j),k_turbine(i,2,j) ! loop over turbine blade levels
           z1=phb(i,k,j)/g-l1-phb(i,1,j)/g  ! distance between k level and lower blade tip
           z2=phb(i,k+1,j)/g-l1-phb(i,1,j)/g ! distance between k+1 level and lower blade tip
           IF(z1 .LT. 0.) z1=0.0 ! k level lower than lower blade tip
           IF(z2 .GT. tdiameter) z2=tdiameter ! k+1 level higher than turbine upper blade tip
           ! magnitude of horizontal velocity
           speed=sqrt(u(i,k,j)*u(i,k,j)+v(i,k,j)*v(i,k,j))
           ! calculate TKE, power and thrust coeffs
           CALL dragcof(tkecof,powcof,thrcof,speed,cispeed,cospeed, &
           tpower,tdiameter,thrustcof)
           ! calculate area of turbine between z1 and z2 (fraction of level covering blade) * wind farm density
           CALL wfdensityi(z1,z2,tdiameter,wfdensity,tarea)
           ! output TKE tendency
           qke(i,k,j)     =    qke(i,k,j)+speed*speed*speed*tarea*tkecof*dt/dz(i,k,j)
           ! output u tendency
           du(i,k,j) = du(i,k,j)-.5*u(i,k,j)*thrcof*speed*tarea/dz(i,k,j)
           ! output v tendency
           dv(i,k,j) = dv(i,k,j)-.5*v(i,k,j)*thrcof*speed*tarea/dz(i,k,j)
        ENDDO
     ENDDO
  ENDDO
  END SUBROUTINE turbine_dragforce

! calculates area of turbine between two vertical levels
! Input variable : 
!            z1 = position of lower level in m
!            z2 = position of upper level in m
!            nt = wind farm density in m2
!     tdiameter = turbine diameter
! Output variable :
!         tarea = area of turbine between two levels * nt               
  SUBROUTINE wfdensityi(z1,z2,tdiameter,wfdensity,tarea)

  REAL, INTENT(IN) ::tdiameter,wfdensity
  REAL, INTENT(INOUT) ::z1,z2
  REAL, INTENT(OUT):: tarea
  REAL r,zc1,zc2

  r=tdiameter/2.              !r = turbine radius
  z1=r-z1                   !distance of kth level from turbine center 
  z2=r-z2                   !distance of k+1 th level from turbine center
  zc1=abs(z1)
  zc2=abs(z2)
  !turbine area between z1 and z2
  IF(z1 .GT. 0. .AND. z2 .GT. 0) THEN
     tarea=zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)- &
     (zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r))
  ELSE IF(z1 .LT. 0. .AND. z2 .LT. 0) THEN
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)- &
     (zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r))
  ELSE
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)+ &
     zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)
  ENDIF
  tarea=tarea*wfdensity      !turbine area * wind farm density 

  END SUBROUTINE wfdensityi

! Caculates tke, power and thrust coefficients by curve fitting as a function of wind speed -
! you can change accordingly
! It is based on cut-in speed, cut-out speed and standing thrust coefficient
! tkecof = tke coefficient
! powcof = power coefficient
! thrcof = thrust coefficient
! cispeed = cut-in speed in m/s
! cospeed = cut-out speed in m/s
! tpower = turbine power in MW
! speed = wind speed in m/s
! tdiameter = turbine diameter in m 
! thrustcof = standing thrust coefficents

  SUBROUTINE dragcof(tkecof,powcof,thrcof,speed,cispeed,cospeed, &
             tpower,tdiameter,thrustcof)
  REAL, PARAMETER :: pi=22./7.
  REAL, INTENT(IN):: speed, cispeed, cospeed, tpower,tdiameter,thrustcof
  REAL, INTENT(OUT):: tkecof,powcof,thrcof
  REAL :: power,area

  area=pi/4.*tdiameter*tdiameter          !swept area by turbine blades
! Thrust coefficient calculation
  IF (speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     thrcof=thrustcof
  ELSE IF(speed .GT. cispeed .AND. speed .LT. cispeed+2.) THEN
     thrcof=thrustcof+(.9-thrustcof)*(speed-cispeed)/2.
  ELSE
     thrcof=thrustcof+2.3/speed**.8
  ENDIF
! power coefficient calculation
  IF(speed .LE. cispeed .OR. speed .GE. cospeed+2.) THEN
     power=0.
     powcof=0.

  ELSE IF(speed .GT. cospeed  .AND. speed .LE. cospeed+2.) THEN
     power = abs(tpower - tpower/2.*(speed-cospeed))
     powcof = power * 2.e+6 / (speed*speed*speed*area)

  ELSE IF(speed .GT. cispeed  .AND. speed .LT. cospeed-12) THEN
     power=tpower/(cospeed-12-cispeed)*(speed-cispeed)
     powcof = power * 2.e+6 / (speed*speed*speed*area)

  ELSE
     power=tpower
     powcof = power * 2.e+6 / (speed*speed*speed*area)

  ENDIF
! tke coefficient calculation 
   tkecof=thrcof-powcof
   IF(tkecof .LT. 0.) tkecof=0.

  END SUBROUTINE dragcof
  
END MODULE module_wind_fitch
