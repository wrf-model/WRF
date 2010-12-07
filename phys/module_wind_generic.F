MODULE module_wind_generic

  IMPLICIT NONE

  TYPE windturbine_specs
     INTEGER id             ! grid id
     REAL    lat, lon       ! lat/lon of the individual turbine
     INTEGER i, j           ! coord in grid id (this is set internally, per grid)
     REAL    hubheight      ! hieght of the turbine hub
     REAL    diameter       ! diameter of the rotor
     REAL    thrustcof      ! standing thrust coefficient
     REAL    power          ! turbine power in MW
     REAL    cutinspeed     ! cut-in speed
     REAL    cutoutspeed    ! cut-out speed
  END TYPE windturbine_specs

  TYPE(windturbine_specs), TARGET, ALLOCATABLE, DIMENSION(:) :: windturbines
  INTEGER :: nwindturbines

  INTEGER, PARAMETER :: WIND_TURBINES_OFF      = 0
  INTEGER, PARAMETER :: WIND_TURBINES_IDEAL    = 1
  INTEGER, PARAMETER :: WIND_TURBINES_FROMLIST = 2

  INTEGER windspec

  LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

CONTAINS

  SUBROUTINE read_windturbines_in
! Check the namelist variable nl_get_windturbines_spec.  If it is set to none,
! which is the default value, then do nothing.  If it is set to ideal, then
! a wind scheme is active but no extra information beyond what is in the namelist
! is needed.  If it is set to the name of a file, read the file to get position
! and characteristics of each turbine and store that in a datastructure here 
! (the array turbinespec) that the parameterizations can refer to when initializing
! themselves.

    IMPLICIT NONE
! Local
    CHARACTER*256  fname, message
    INTEGER i,istat
    INTEGER id
    INTEGER n,lineno
    REAL  lat,lon,hubheight,diameter,thrustcof,power,cutinspeed,cutoutspeed
!
    CALL nl_get_windturbines_spec( 1, fname )
    windspec = WIND_TURBINES_OFF
    IF ( TRIM(fname) .EQ. "none" ) THEN
      RETURN
    ELSE IF ( TRIM(fname) .EQ. "ideal" ) THEN
     ! get the turbine specs from the namelist and initialize in 
     ! the specific turbine parameterization
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
          ALLOCATE(windturbines(n))
          REWIND(19)
          DO I = 1, n 
            lineno = i
            READ(19,*,ERR=130)id,lat,lon,hubheight,diameter,thrustcof,power,cutinspeed,cutoutspeed
            windturbines(i)%id = id
            windturbines(i)%lat = lat
            windturbines(i)%lon = lon
            windturbines(i)%hubheight = hubheight
            windturbines(i)%diameter = diameter
            windturbines(i)%thrustcof = thrustcof
            windturbines(i)%power = power
            windturbines(i)%cutinspeed = cutinspeed
            windturbines(i)%cutoutspeed = cutoutspeed
          ENDDO
          CLOSE(19)
          GOTO 150
 130      CONTINUE
          istat = 150150
          GOTO 150
        ENDIF
      ENDIF
 150  CONTINUE
      CALL wrf_dm_bcast_integer(istat,1) 
      IF ( istat .NE. 0 ) THEN
        WRITE(message,*)'Unable to open or read ',TRIM(fname),'. Proceeding without wind-turbine parameterization.'
        CALL wrf_message(message)
        IF ( istat .EQ. 150150 ) THEN
          WRITE(message,*)'Perhaps bad syntax at line ',lineno,' of ',TRIM(fname)
          CALL wrf_message(message)
        ENDIF
        IF ( ALLOCATED(windturbines) ) DEALLOCATE(windturbines)
        RETURN
      ENDIF
      CALL wrf_dm_bcast_integer(n,1) 
      IF ( .NOT. wrf_dm_on_monitor() ) ALLOCATE(windturbines(n))
      DO I = 1, n 
        CALL wrf_dm_bcast_integer(windturbines(i)%id,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%lat,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%lon,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%hubheight,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%diameter,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%thrustcof,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%power,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%cutinspeed,1) 
        CALL wrf_dm_bcast_real(windturbines(i)%cutoutspeed,1) 
        windturbines%i = -1   ! set to uninitialized
        windturbines%j = -1
      ENDDO
      windspec = WIND_TURBINES_FROMLIST
      RETURN
    ENDIF
  END SUBROUTINE read_windturbines_in

  SUBROUTINE init_module_wind_generic
    IMPLICIT NONE
    CALL read_windturbines_in 
  END SUBROUTINE init_module_wind_generic

END MODULE module_wind_generic
