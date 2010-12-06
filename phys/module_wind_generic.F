MODULE module_wind_generic

  IMPLICIT NONE

  TYPE windfarm_specs
     INTEGER id             ! grid id
     REAL    lat, lon       ! lat/lon of the individual turbine
     REAL    hubheight      ! hieght of the turbine hub
     REAL    diameter       ! diameter of the rotor
     REAL    thrust         ! standing thrust coefficient
     REAL    power          ! turbine power in MW
     REAL    cutinspeed     ! cut-in speed
     REAL    cutoutspeed    ! cut-out speed
  END TYPE windfarm_specs

  TYPE(windfarms_specs), ALLOCATABLE, DIMENSION(:) :: farmspec

  INTEGER, PARAMETER ( WIND_TURBINES_OFF      = 0 )
  INTEGER, PARAMETER ( WIND_TURBINES_IDEAL    = 1 )
  INTEGER, PARAMETER ( WIND_TURBINES_FROMLIST = 2 ) 

  INTEGER windspec

  LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

CONTAINS

  SUBROUTINE read_windfarms_in
! Check the namelist variable nl_get_windfarms_select.  If it is set to none,
! which is the default value, then do nothing.  If it is set to ideal, then
! a wind scheme is active but no extra information beyond what is in the namelist
! is needed.  If it is set to the name of a file, read the file to get position
! and characteristics of each turbine and store that in a datastructure here 
! (the array farmspec) that the parameterizations can refer to when initializing
! themselves.

    IMPLICIT NONE
    CHARACTER*256  fname, message
    INTEGER i,istat
    INTEGER id
    REAL  lat,lon,hubheight,diameter,thrust,power,cutinspeed,cutoutspeed
    CALL nl_get_windfarms_select( 1, fname )
    windspec = WIND_TURBINES_OFF
    IF ( TRIM(fname) .EQ. "none" ) THEN
      RETURN
    ELSE IF ( TRIM(fname) .EQ. "ideal" ) THEN
      RETURN   ! info is already available from namelist
      windspec = WIND_TURBINES_IDEAL
    ELSE
      !info is contained in a file named by fname
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(file=TRIM(fname),unit=19,FORM='FORMATTED',STATUS='OLD',IOSTAT=istat)
        IF ( istat .EQ. 0 ) THEN
          ! first time count things up
          n = 0
          DO WHILE (.true.)
            READ(19,*,END=30) 
            n = n + 1
          ENDDO
 30       CONTINUE
          ALLOCATE(farmspec(n))
          REWIND(19)
          DO I = 1, n 
            READ(19,*,ERR=130)id,lat,lon,hubheight,diameter,thrust,power,cutinspeed,cutoutspeed
            farmspec(i)%id = id
            farmspec(i)%lat = lat
            farmspec(i)%lon = lon
            farmspec(i)%hubheight = hubheight
            farmspec(i)%diameter = diameter
            farmspec(i)%thrust = thrust
            farmspec(i)%power = power
            farmspec(i)%cutinspeed = cutinspeed
            farmspec(i)%cutoutspeed = cutoutspeed
          ENDDO
        ENDIF
      ENDIF
      CALL wrf_dm_bcast_integer(istat,1) 
      IF ( istat .NE. 0 ) THEN
        WRITE(message,*)'Unable to open or read ',TRIM(fname),'. Proceeding without wind-turbine parameterization.'
        CALL wrf_message(message)
        IF ( ALLOCATED(farmspec) ) DEALLOCATE(farmspec)
        RETURN
      ENDIF
      CALL wrf_dm_bcast_integer(n,1) 
      IF ( .NOT. wrf_dm_on_monitor() ) ALLOCATE(farmspec(n))
      DO I = 1, n 
        CALL wrf_dm_bcast_integer(farmspec(i)%id,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%lat,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%lon,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%hubheight,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%diameter,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%thrust,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%power,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%cutinspeed,1) 
        CALL wrf_dm_bcast_real(farmspec(i)%cutoutspeed,1) 
      ENDDO
      windspec = WIND_TURBINES_FROMLIST
      RETURN
    ENDIF
  END SUBROUTINE read_windfarms_in

  SUBROUTINE init_module_wind_generic
    IMPLICIT NONE
    CALL read_windfarms_in 
  END SUBROUTINE init_module_wind_generic

END MODULE module_wind_generic
