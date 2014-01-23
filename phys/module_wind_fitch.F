!WRF:MODEL_LAYER:PHYSICS

MODULE module_wind_fitch

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! NOTICE
!! The following paper should be cited whenever presenting results using this scheme
!! (using either the original version or any modified versions of the scheme):
!! Fitch, A. C. et al. 2012: Local and Mesoscale Impacts of Wind Farms as Parameterized in a
!! Mesoscale NWP Model. Monthly Weather Review, doi:http://dx.doi.org/10.1175/MWR-D-11-00352.1
!!
!! Anna C. Fitch, National Center for Atmospheric Research (formerly University of Bergen)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Represents kinetic energy extracted by wind turbines and turbulence
! (TKE) they produce at model levels within the rotor area. The thrust and
! power coefficient curves included to calculate the momentum sink and source of TKE are 
! generic and an approximation to a real turbine. These coefficients should be
! obtained from the turbine manufacturer for the turbines of interest and incorporated
! into the code in subroutine dragcof (we could not include real curves since they
! are proprietary).
!
! References:
! Fitch, A. C. et al. 2012: Local and Mesoscale Impacts of Wind Farms as Parameterized in a
! Mesoscale NWP Model. Monthly Weather Review, doi:http://dx.doi.org/10.1175/MWR-D-11-00352.1
! Fitch, A. C. et al. 2013: Mesoscale Influences of Wind Farms Throughout a Diurnal Cycle.
! Monthly Weather Review, doi:http://dx.doi.org/10.1175/MWR-D-12-00185.1
! Fitch, A. C. et al. 2013: Parameterization of Wind Farms in Climate Models.
! Journal of Climate, doi:http://dx.doi.org/10.1175/JCLI-D-12-00376.1
!
! Output:
!   du, dv: horizontal velocity tendencies
!   qke: TKE
! Input: 
!   u, v: horizontal velocities
!   dz = dz between full levels (m)
!   !not yet:  z_at_w = height above sea level at layer interfaces (m)
!   !not yet:  ht = terrain height
!   phb = geopotential height
!   %hubheight = hub height (m)
!   %diameter = turbine diameter (m)
!   %stdthrcoef = standing thrust coeff. (thrust coeff of turbine when not operating)
!   %power = turbine power (MW)
!   %cutinspeed = cut-in speed (m/s)
!   %cutoutspeed = cut-out speed (m/s)
!   ewfx = x-extent of rectangular wind farm in grid cells
!   ewfy = y-extent of rectangular wind farm in grid cells
!   pwfx = x-coord of grid cell in SW corner of farm
!   pwfy = y-coord of grid cell in SW corner of farm
!   turbpercell = no. of turbines per grid cell

  USE module_wind_generic
  USE module_driver_constants, ONLY : max_domains
  USE module_model_constants, ONLY :  piconst
  USE module_model_constants, ONLY :  g

  IMPLICIT NONE

  LOGICAL, DIMENSION(max_domains) :: inited

  PUBLIC  turbine_drag
  PRIVATE dragcof, turbine_area, inited

CONTAINS

  SUBROUTINE  turbine_drag(                      &
       & id                                      &
       &,phb,u,v,xlat_u,xlong_u                  &
       &,xlat_v,xlong_v                          &
       &,dx,dz,dt,qke                            &
       &,qke_adv,bl_mynn_tkeadvect               &
       &,du,dv                                   &
       &,ids,ide,jds,jde,kds,kde                 &
       &,ims,ime,jms,jme,kms,kme                 &
       &,its,ite,jts,jte,kts,kte                 &
       &)  

  INTEGER, INTENT(IN) :: id  ! grid id
  INTEGER, INTENT(IN) :: its,ite,jts,jte,kts,kte
  INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme
  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde
  LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
  REAL, INTENT(IN) :: dx,dt
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz,u,v,phb
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN)         :: xlat_u, xlong_u
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN)         :: xlat_v, xlong_v
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: du,dv,qke,qke_adv
! Local
  TYPE(windturbine_specs), POINTER :: p
  INTEGER  turbgridid
  REAL     hubheight,diameter,power,cutinspeed,cutoutspeed,stdthrcoef,turbpercell
  INTEGER  ewfx,ewfy,pwfx,pwfy
  REAL     blade_l_point,blade_u_point,zheightl,zheightu,z1,z2,tarea
  REAL     speed,tkecof,powcof,thrcof,wfdensity
  INTEGER  itf,jtf,ktf
  INTEGER  i,j,k,swfindx,ewfindx,swfindy,ewfindy,n,n1,n2,iturbine
  INTEGER  k_turbine_bot, k_turbine_top

  LOGICAL :: kfound
  INTEGER :: allzero

  itf=MIN0(ite,ide-1)
  jtf=MIN0(jte,jde-1)
  ktf=MIN0(kte,kde-1)

!QKE should already == QKE_ADV coming out of PBL scheme
!ACF copy qke_adv into qke if using advection
!    IF (BL_MYNN_TKEADVECT) THEN
!       qke=qke_adv
!    ENDIF
!ACF-end

  CALL nl_get_td_turbpercell(1,turbpercell)
  CALL nl_get_td_turbgridid(1,turbgridid)
  IF ( .NOT. inited(id) ) THEN
    IF ( windspec .EQ. WIND_TURBINES_FROMLIST ) THEN 
! first check and see if xlat and xlong are all zero, if so, then use i,j directly
! (just check the u variables)
      allzero=1
      DO j=jts,jtf
        DO i=its,itf
          IF (xlat_u(i,j).NE.0. .OR. xlong_u(i,j).NE.0.)allzero=0
        ENDDO
      ENDDO
      CALL wrf_dm_bcast_integer(allzero,1)
      IF ( allzero .NE. 1 ) THEN
! if there are actual lats and lons available, find i and j based on lat and lon
! otherwise, it is an idealized case and the user has specified i and j in the
! turbines file read in by read_windturbines_in in module_wind_generic
        DO iturbine = 1,nwindturbines   ! nwindturbines defined in module_wind_generic
          p => windturbines(iturbine)
          IF ( id .EQ. p%id ) THEN
            DO j=jts,jtf
              DO i=its,itf
                IF (xlat_v(i,j) .LE. p%lat .AND. p%lat .LT. xlat_v(i,j+1) .AND. &
                    xlong_u(i,j).LE. p%lon .AND. p%lon .LT. xlong_u(i+1,j)) THEN
                  p%i=i
                  p%j=j
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ELSE IF ( windspec .EQ. WIND_TURBINES_IDEAL .AND. id .EQ. turbgridid ) THEN
      CALL nl_get_td_ewfx(1,ewfx)
      CALL nl_get_td_ewfy(1,ewfy)
      CALL nl_get_td_pwfx(1,pwfx)
      CALL nl_get_td_pwfy(1,pwfy)
      CALL nl_get_td_hubheight(1,hubheight)
      CALL nl_get_td_diameter(1,diameter)
      CALL nl_get_td_power(1,power)
      CALL nl_get_td_cutinspeed(1,cutinspeed)
      CALL nl_get_td_cutoutspeed(1,cutoutspeed)
      CALL nl_get_td_stdthrcoef(1,stdthrcoef)
! count the turbines
      n = 0
      DO j = jts,jtf
        IF ( pwfy .LE. j .AND. j .LE. (pwfy+ewfy-1) ) THEN
          DO i = its,itf
            IF ( pwfx .LE. i .AND. i .LE. (pwfx+ewfx-1) ) THEN
              n = n + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      nwindturbines = n
      ALLOCATE(windturbines(nwindturbines))
! set the turbines
      n = 0
      DO j = jts,jtf
        IF ( pwfy .LE. j .AND. j .LE. (pwfy+ewfy-1) ) THEN
          DO i = its,itf
            IF ( pwfx .LE. i .AND. i .LE. (pwfx+ewfx-1) ) THEN
              n = n + 1
              IF ( n .GT. nwindturbines ) THEN
                CALL wrf_error_fatal('would overrun windturbines array')
              ENDIF
              windturbines(n)%id = id
              windturbines(n)%lat = 0.0
              windturbines(n)%lon = 0.0
              windturbines(n)%i = i
              windturbines(n)%j = j
              windturbines(n)%hubheight = hubheight
              windturbines(n)%diameter = diameter
              windturbines(n)%stdthrcoef = stdthrcoef
              windturbines(n)%power = power
              windturbines(n)%cutinspeed = cutinspeed
              windturbines(n)%cutoutspeed = cutoutspeed
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    inited(id) = .TRUE.
  ENDIF

  IF ( windspec .EQ.  WIND_TURBINES_FROMLIST ) THEN
    wfdensity = 1.0/(dx*dx)   !  per turbine, so numerator is 1
  ELSE
    wfdensity = turbpercell/(dx*dx)
  ENDIF

  IF (inited(id) .AND.                                              &
      ((windspec .EQ. WIND_TURBINES_FROMLIST) .OR.       &
       (windspec .EQ. WIND_TURBINES_IDEAL .AND. id .EQ. turbgridid ))) THEN
    DO iturbine = 1,nwindturbines   ! nwindturbines defined in module_wind_generic
      p => windturbines(iturbine)
      IF ( id .EQ. p%id ) THEN
! vertical layers cut by turbine blades
        k_turbine_bot=0      !bottom level
        k_turbine_top=-1     !top level
        i = p%i
        j = p%j

        IF (( its .LE. i .AND. i .LE. itf ) .AND. &
            ( jts .LE. j .AND. j .LE. jtf )  ) THEN

          blade_l_point=p%hubheight-p%diameter/2. ! height of lower blade tip above ground (m)
          blade_u_point=p%hubheight+p%diameter/2. ! height of upper blade tip above ground (m)

          kfound = .false.
          zheightl=0.0
          ! find vertical levels cut by turbine blades
          DO k=kts,ktf
            IF(.NOT. kfound) THEN
              zheightu = zheightl + dz(i,k,j) ! increment height

              IF(blade_l_point .GE. zheightl .AND. blade_l_point .LE. zheightu) THEN
                k_turbine_bot=k ! lower blade tip cuts this level
              ENDIF

              IF(blade_u_point .GE. zheightl .AND. blade_u_point .LE. zheightu) THEN
                k_turbine_top=k ! upper blade tip cuts this level
                kfound = .TRUE.
              ENDIF

              zheightl = zheightu
            ENDIF
          ENDDO
          IF ( kfound ) THEN
            DO k=k_turbine_bot,k_turbine_top ! loop over turbine blade levels

              z1=phb(i,k,j)/g-blade_l_point-phb(i,1,j)/g  ! distance between k level and lower blade tip
              z2=phb(i,k+1,j)/g-blade_l_point-phb(i,1,j)/g ! distance between k+1 level and lower blade tip

              IF(z1 .LT. 0.) z1=0.0 ! k level lower than lower blade tip
              IF(z2 .GT. p%diameter) z2=p%diameter ! k+1 level higher than turbine upper blade tip

              ! horizontal wind speed
              speed=sqrt(u(i,k,j)**2.+v(i,k,j)**2.)

              ! calculate TKE, power and thrust coeffs
              CALL dragcof(tkecof,powcof,thrcof,               &
                           speed,p%cutinspeed,p%cutoutspeed,   &
                           p%power,p%diameter,p%stdthrcoef)

              CALL turbine_area(z1,z2,p%diameter,wfdensity,tarea)

              ! output TKE
              qke(i,k,j) = qke(i,k,j)+speed**3.*tarea*tkecof*dt/dz(i,k,j)
              ! output u tendency
              du(i,k,j) = du(i,k,j)-.5*u(i,k,j)*thrcof*speed*tarea/dz(i,k,j)
              ! output v tendency
              dv(i,k,j) = dv(i,k,j)-.5*v(i,k,j)*thrcof*speed*tarea/dz(i,k,j)

            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF

!ACF copy qke into qke_adv if using advection
   IF (BL_MYNN_TKEADVECT) THEN
      qke_adv=qke
   ENDIF
!ACF-end

  END SUBROUTINE turbine_drag

! calculates area of turbine between two vertical levels
! Input variables : 
!            z1 = distance between k level and lower blade tip
!            z2 = distance between k+1 level and lower blade tip
!            wfdensity = wind farm density in m^-2
!     tdiameter = turbine diameter
! Output variable :
!         tarea = area of turbine between two levels * wfdensity
  SUBROUTINE turbine_area(z1,z2,tdiameter,wfdensity,tarea)

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
  IF(z1 .GT. 0. .AND. z2 .GT. 0.) THEN
     tarea=zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)- &
     (zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r))
  ELSE IF(z1 .LT. 0. .AND. z2 .LT. 0.) THEN
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)- &
     (zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r))
  ELSE
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)+ &
     zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)
  ENDIF
  tarea=tarea*wfdensity      !turbine area * wind farm density 

  END SUBROUTINE turbine_area

! Caculates tke, power and thrust coefficients as function of horiz wind speed
! from fit to turbine power curve - needs to be changed for particular turbine used

! tkecof = tke coefficient
! powcof = power coefficient
! thrcof = thrust coefficient
! cispeed = cut-in speed in m/s
! cospeed = cut-out speed in m/s
! tpower = turbine power in MW
! speed = horiz wind speed in m/s
! tdiameter = turbine diameter in m 
! stdthrcoef = standing thrust coefficient

  SUBROUTINE dragcof(tkecof,powcof,thrcof,speed,cispeed,cospeed, &
                     tpower,tdiameter,stdthrcoef)

!  DISCLAIMER: The following power curve, power coefficients, and thrust
!  coefficients are meant for testing purposes only, and were formulated as 
!  an approximation to a real curve.  The user is strongly encouraged to 
!  incorporate their own curves for the particular turbine of interest 
!  to them.

  REAL, INTENT(IN):: speed, cispeed, cospeed, tpower,tdiameter,stdthrcoef
  REAL, INTENT(OUT):: tkecof,powcof,thrcof
  REAL :: power,area,mspeed,hspeed

  area=piconst/4.*tdiameter**2.          ! area swept by turbine blades

  ! GENERIC POWER CURVE - USE AT YOUR OWN RISK
  mspeed=0.5*(cospeed+cispeed)  !average of cispeed & cospeed
  hspeed=0.5*(cospeed-mspeed)   !this regulates the transition to full power
  power =tpower*(.5*tanh((speed - (mspeed-hspeed))/(hspeed*0.60)) + .5)*.8
  
  ! GENERIC power coefficient calculation - USE AT YOUR OWN RISK
  IF(speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     power=0.
     powcof=0.
  ELSE 
     powcof = power * 2.e+6 / (speed**3.*area)
     IF (speed .LT. cispeed*2.) THEN ! dampen artificial max near cispeed
        powcof = powcof * exp(-((speed-cispeed*2.)**2./(cispeed*2.)))
     end if
     powcof = MIN(powcof,.55)
  ENDIF

  ! GENERIC Thrust coefficient calculation - USE AT YOUR OWN RISK
  IF (speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     thrcof = stdthrcoef
  ELSE
     !thrcof= stdthrcoef+2.3/speed**.8
     thrcof = powcof + powcof*0.75
     thrcof = MIN(thrcof,.9)
     thrcof = MAX(thrcof,stdthrcoef)
  ENDIF

  ! tke coefficient calculation 
  tkecof=thrcof-powcof
  IF(tkecof .LT. 0.) tkecof=0.

  END SUBROUTINE dragcof

  SUBROUTINE init_module_wind_fitch
    inited = .FALSE.
  END SUBROUTINE init_module_wind_fitch
  
END MODULE module_wind_fitch
