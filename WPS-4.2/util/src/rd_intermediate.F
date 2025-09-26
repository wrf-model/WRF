PROGRAM rd_intermediate

   USE module_debug
   USE misc_definitions_module
   USE read_met_module

   IMPLICIT NONE

   !  Intermediate input and output from same source.

   INTEGER :: istatus
   TYPE (met_data)                   :: fg_data

   CHARACTER ( LEN =132 )            :: flnm


   !  Get the input file name from the command line.
   CALL getarg ( 1 , flnm  )

   IF ( flnm(1:1) == ' ' ) THEN
      print *,'USAGE: rd_intermediate.exe <filename>'
      print *,'       where <filename> is the name of an intermediate-format file'
      STOP
   END IF

   CALL set_debug_level(WARN)

   CALL read_met_init(trim(flnm), .true., '0000-00-00_00', istatus)

   IF ( istatus == 0 ) THEN

      CALL  read_next_met_field(fg_data, istatus)

      DO WHILE (istatus == 0)

         CALL mprintf(.true.,STDOUT, '================================================')
         CALL mprintf(.true.,STDOUT, 'FIELD = %s', s1=fg_data%field)
         CALL mprintf(.true.,STDOUT, 'UNITS = %s DESCRIPTION = %s', s1=fg_data%units, s2=fg_data%desc)
         CALL mprintf(.true.,STDOUT, 'DATE = %s FCST = %f', s1=fg_data%hdate, f1=fg_data%xfcst)
         CALL mprintf(.true.,STDOUT, 'SOURCE = %s', s1=fg_data%map_source)
         CALL mprintf(.true.,STDOUT, 'LEVEL = %f', f1=fg_data%xlvl)
         CALL mprintf(.true.,STDOUT, 'I,J DIMS = %i, %i', i1=fg_data%nx, i2=fg_data%ny)

         SELECT CASE ( fg_data%iproj )
            CASE (PROJ_LATLON)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i  PROJECTION = %s', i1=0, s1='LAT LON') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  DLAT, DLON = %f, %f', f1=fg_data%deltalat, f2=fg_data%deltalon)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE (PROJ_MERC)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i  PROJECTION = %s', i1=1, s1='MERCATOR') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=fg_data%dx, f2=fg_data%dy)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=fg_data%truelat1)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE (PROJ_LC)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i   PROJECTION = %s', i1=3, s1='LAMBERT CONFORMAL') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=fg_data%dx, f2=fg_data%dy)
               CALL mprintf(.true.,STDOUT,'  STAND_LON = %f', f1=fg_data%xlonc)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=fg_data%truelat1)
               CALL mprintf(.true.,STDOUT,'  TRUELAT2 = %f', f1=fg_data%truelat2)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE (PROJ_GAUSS)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i   PROJECTION = %s', i1=4, s1='GAUSSIAN') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  NLATS, DLON = %f, %f', f1=fg_data%deltalat, f2=fg_data%deltalon)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE (PROJ_PS)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i   PROJECTION = %s', i1=5, s1='POLAR STEREOGRAPHIC') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=fg_data%dx, f2=fg_data%dy)
               CALL mprintf(.true.,STDOUT,'  STAND_LON = %f', f1=fg_data%xlonc)
               CALL mprintf(.true.,STDOUT,'  TRUELAT1 = %f', f1=fg_data%truelat1)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE (PROJ_CASSINI)
               CALL mprintf(.true.,STDOUT, 'IPROJ = %i   PROJECTION = %s', i1=6, s1='PROJ_CASSINI') 
               CALL mprintf(.true.,STDOUT,'  REF_X, REF_Y = %f, %f', f1=fg_data%starti, f2=fg_data%startj)
               CALL mprintf(.true.,STDOUT,'  REF_LAT, REF_LON = %f, %f', f1=fg_data%startlat, f2=fg_data%startlon)
               CALL mprintf(.true.,STDOUT,'  DX, DY = %f, %f', f1=fg_data%dx, f2=fg_data%dy)
               CALL mprintf(.true.,STDOUT,'  STAND_LON = %f', f1=fg_data%xlonc)
               CALL mprintf(.true.,STDOUT,'  POLE_LAT = %f', f1=fg_data%pole_lat)
               CALL mprintf(.true.,STDOUT,'  POLE_LON = %f', f1=fg_data%pole_lon)
               CALL mprintf(.true.,STDOUT,'  CENTER_LAT = %f', f1=fg_data%centerlat)
               CALL mprintf(.true.,STDOUT,'  CENTER_LON = %f', f1=fg_data%centerlon)
               CALL mprintf(.true.,STDOUT,'  EARTH_RADIUS = %f', f1=fg_data%earth_radius)
            CASE default
               CALL mprintf(.true.,ERROR, '  Unknown iproj %i for version %i', i1=fg_data%iproj, i2=fg_data%version)
         END SELECT
         CALL mprintf(.true.,STDOUT,'DATA(1,1)=%f',f1=fg_data%slab(1,1))
         CALL mprintf(.true.,STDOUT,'')

         IF (ASSOCIATED(fg_data%slab)) DEALLOCATE(fg_data%slab)

         CALL  read_next_met_field(fg_data, istatus)

      END DO

      CALL read_met_close()

   ELSE
      print *, 'File = ',TRIM(flnm)
      print *, 'Problem with input file, I can''t open it'
      STOP 
   END IF

   print *,'SUCCESSFUL COMPLETION OF PROGRAM RD_INTERMEDIATE'
   STOP

END PROGRAM rd_intermediate
