   !---------------------------------------------------------------------------
   ! Code copied from SI
   !---------------------------------------------------------------------------

   !dis   
   !dis    Open Source License/Disclaimer, Forecast Systems Laboratory
   !dis    NOAA/OAR/FSL, 325 Broadway Boulder, CO 80305
   !dis    
   !dis    This software is distributed under the Open Source Definition,
   !dis    which may be found at http://www.opensource.org/osd.html.
   !dis    
   !dis    In particular, redistribution and use in source and binary forms,
   !dis    with or without modification, are permitted provided that the
   !dis    following conditions are met:
   !dis    
   !dis    - Redistributions of source code must retain this notice, this
   !dis    list of conditions and the following disclaimer.
   !dis    
   !dis    - Redistributions in binary form must provide access to this
   !dis    notice, this list of conditions and the following disclaimer, and
   !dis    the underlying source code.
   !dis    
   !dis    - All modifications to this software must be clearly documented,
   !dis    and are solely the responsibility of the agent making the
   !dis    modifications.
   !dis    
   !dis    - If significant modifications or enhancements are made to this
   !dis    software, the FSL Software Policy Manager
   !dis    (softwaremgr@fsl.noaa.gov) should be notified.
   !dis    
   !dis    THIS SOFTWARE AND ITS doCUMENTATION ARE in THE PUBLIC doMAin
   !dis    AND ARE FURNISHED "AS IS."  THE AUTHORS, THE unitED STATES
   !dis    GOVERNMENT, ITS inSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND
   !dis    AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE useFULNESS
   !dis    OF THE SOFTWARE AND doCUMENTATION FOR ANY Purpose.  THEY ASsumE
   !dis    NO RESPONSIBILITY (1) FOR THE use OF THE SOFTWARE AND
   !dis    doCUMENTATION; OR (2) TO PROVIDE TECHNICAL SUPPORT TO useRS.
   !dis   
   !dis 

   !module map_utils

   ! Module that defines constants, data structures, and
   ! subroutines used to convert grid indices to lat/lon
   ! and vice versa.   
   !
   ! SUPPORTED PROJECTIONS
   ! ---------------------
   ! Cylindrical Lat/Lon (code = PROJ_LATLON)
   ! Mercator (code = PROJ_MERC)
   ! Lambert Conformal (code = PROJ_LC)
   ! Polar Stereographic (code = PROJ_PS)
   !
   ! REMARKS
   ! -------
   ! The routines contained within were adapted from routines
   ! obtained from NCEP's w3 library.  The original NCEP routines were less
   ! flexible (e.g., polar-stereo routines only supported truelat of 60N/60S)
   ! than what we needed, so modifications based on equations in Hoke, Hayes, and
   ! Renninger (AFGWC/TN/79-003) were added to improve the flexibility.  
   ! Additionally, coding was improved to F90 standards and the routines were
   ! combined into this module.  
   !
   ! Assumptions
   ! -----------
   !  Grid Definition:
   !    For mercator, lambert conformal, and polar-stereographic projections,
   !    the routines within assume the following:
   !
   !       1.  Grid is dimensioned (i,j) where i is the East-West direction, 
   !           positive toward the east, and j is the north-south direction, 
   !           positive toward the north.  
   !       2.  Origin is at (1,1) and is located at the southwest corner,
   !           regardless of hemispere.
   !       3.  Grid spacing (dx) is always positive.
   !       4.  Values of true latitudes must be positive for NH domains
   !           and negative for SH domains.
   !
   !     For the latlon projection, the grid origin may be at any of the
   !     corners, and the deltalat and deltalon values can be signed to 
   !     account for this using the following convention:
   !       Origin Location        Deltalat Sign      Deltalon Sign
   !       ---------------        -------------      -------------
   !        SW Corner                  +                   +
   !        NE Corner                  -                   -
   !        NW Corner                  -                   +
   !        SE Corner                  +                   -
   !       
   !  Data Definitions:
   !       1. Any arguments that are a latitude value are expressed in 
   !          degrees north with a valid range of -90 -> 90
   !       2. Any arguments that are a longitude value are expressed in
   !          degrees east with a valid range of -180 -> 180
   !       3. Distances are in meters and are always positive.
   !       4. The standard longitude (stdlon) is defined as the longitude
   !          line which is parallel to the grid's y-axis (j-direction), along
   !          which latitude increases (NOT the absolute value of latitude, but
   !          the actual latitude, such that latitude increases continuously
   !          from the south pole to the north pole) as j increases.  
   !       5. One true latitude value is required for polar-stereographic and
   !          mercator projections, and defines at which latitude the 
   !          grid spacing is true.  For lambert conformal, two true latitude
   !          values must be specified, but may be set equal to each other to
   !          specify a tangent projection instead of a secant projection.  
   !       
   ! USAGE
   ! -----
   ! To use the routines in this module, the calling routines must have the 
   ! following statement at the beginning of its declaration block:
   !   use map_utils
   ! 
   ! The use of the module not only provides access to the necessary routines,
   ! but also defines a structure of type (proj_info) that can be used
   ! to declare a variable of the same type to hold your map projection
   ! information.  It also defines some integer parameters that contain
   ! the projection codes so one only has to use those variable names rather
   ! than remembering the acutal code when using them.  The basic steps are
   ! as follows:
   !  
   !   1.  Ensure the "use map_utils" is in your declarations.
   !   2.  Declare the projection information structure as type(proj_info):
   !         type(proj_info) :: proj
   !   3.  Populate your structure by calling the map_set routine:
   !         call map_set(code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,proj)
   !       where:
   !         code (input) = one of PROJ_LATLON, PROJ_MERC, PROJ_LC, or PROJ_PS
   !         lat1 (input) = Latitude of grid origin point (i,j)=(1,1) 
   !                         (see assumptions!)
   !         lon1 (input) = Longitude of grid origin 
   !         knowni (input) = origin point, x-location
   !         knownj (input) = origin point, y-location
   !         dx (input) = grid spacing in meters (ignored for LATLON projections)
   !         stdlon (input) = Standard longitude for PROJ_PS and PROJ_LC, 
   !               deltalon (see assumptions) for PROJ_LATLON, 
   !               ignored for PROJ_MERC
   !         truelat1 (input) = 1st true latitude for PROJ_PS, PROJ_LC, and
   !                PROJ_MERC, deltalat (see assumptions) for PROJ_LATLON
   !         truelat2 (input) = 2nd true latitude for PROJ_LC, 
   !                ignored for all others.
   !         proj (output) = The structure of type (proj_info) that will be fully 
   !                populated after this call
   !
   !   4.  Now that the proj structure is populated, you may call either
   !       of the following routines:
   !       
   !       da_latlon_to_ij(proj, lat, lon, i, j)
   !       da_xyll(proj, i, j, lat, lon)
   !
   !       It is incumbent upon the calling routine to determine whether or
   !       not the values returned are within your domain's bounds.  All values
   !       of i, j, lat, and lon are real values.
   !
   !
   ! REFERENCES
   ! ----------
   !  Hoke, Hayes, and Renninger, "Map Preojections and Grid Systems for
   !       Meteorological Applications." AFGWC/TN-79/003(Rev), Air Weather
   !       Service, 1985.
   !
   !  NCAR MM5v3 Modeling System, REGRIDDER program, module_first_guess_map.F
   !  NCEP routines w3fb06, w3fb07, w3fb08, w3fb09, w3fb11, w3fb12
   !
   ! HISTORY
   ! -------
   ! 27 Mar 2001 - Original Version
   !               Brent L. Shaw, NOAA/FSL (CSU/CIRA)
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Define some private constants

   real, private, parameter    :: deg_per_rad = 180.0/pi
   real, private, parameter    :: rad_per_deg = pi / 180.0

   ! Mean Earth Radius in m.  The value below is consistent
   ! with NCEP's routines and grids.
   ! real, public , parameter    :: earth_radius_m = 6371200.0 ! from Brent
   real, public , parameter    :: earth_radius_m = 6370000.0
   real, public , parameter    :: radians_per_degree = pi / 180.0

   ! Define public parameters
 
   ! Projection codes for proj_info structure:
   integer, public, parameter  :: PROJ_LATLON = 0
   integer, public, parameter  :: PROJ_MERC = 1
   integer, public, parameter  :: PROJ_LC = 3
   integer, public, parameter  :: PROJ_PS = 5

   
  ! Define data structures to define various projections

   type proj_info
      integer          :: code     ! integer code for projection type
      real             :: lat1     ! SW latitude (1,1) in degrees (-90->90N)
      real             :: lon1     ! SW longitude (1,1) in degrees (-180->180E)
      real             :: dx       ! Grid spacing in meters at truelats, used
                                   ! only for ps, lc, and merc projections
      real             :: dlat     ! Lat increment for lat/lon grids
      real             :: dlon     ! Lon increment for lat/lon grids
      real             :: stdlon   ! Longitude parallel to y-axis (-180->180E)
      real             :: truelat1 ! First true latitude (all projections)
      real             :: truelat2 ! Second true lat (LC only)
      real             :: hemi     ! 1 for NH, -1 for SH
      real             :: cone     ! Cone factor for LC projections
      real             :: polei    ! Computed i-location of pole point
      real             :: polej    ! Computed j-location of pole point
      real             :: rsw      ! Computed radius to SW corner
      real             :: rebydx   ! Earth radius divided by dx
      real             :: knowni   ! X-location of known lat/lon
      real             :: knownj   ! Y-location of known lat/lon
      real             :: latinc   ! Latitude increments in degrees 
      real             :: loninc   ! Longitude increments in degrees 
      logical          :: init     ! Flag to indicate if this struct is 
                                 ! ready for use
   end type proj_info

   type(proj_info) :: map_info, map_info_ens


