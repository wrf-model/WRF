MODULE MODULE_TYPE

!-------------------------------------------------------------------------
! Define the observation data structure
!
!  D. GILL,         April 1998
!  F. VANDENBERGHE, March 2001
!-------------------------------------------------------------------------

   TYPE location_type

      !  The fields in this record uniquely identify the source of the 
      !  data, so that duplicates of data can be merged or discarded.
      !  The horizontal location of this report (assumed constant, even
      !  for balloon ascent) is geven by the lat/lon of the site.
      
!  Records read in the input file (defined by the decoder

      REAL                   :: latitude  , &   ! latitude (+ degrees east)
                                longitude       ! longitude (+ degrees north)

      CHARACTER ( LEN = 40 ) :: id , &          ! 5 digit identifier, 
                                                ! consisting of a 2 digit block 
                                                ! number and a 3 digit 
                                                ! identifier (for soundings)
                                                ! for WMO sttn; non digit
                                                ! for other sources
                                name            ! The name corresponds to
                                                ! the id (is obtained from id
                                                ! in the program that is 
                                                ! source of data

! Additional records not present in the input file:

      REAL                   :: yic         , & ! MM5 i-index at cross point
                                xjc         , & ! MM5 j-index at cross point
                                yid         , & ! MM5 i-index at dot point
                                xjd             ! MM5 j-index at dot point


   END TYPE location_type


!---------------------------------------------------------------------------

   TYPE source_info

!  Records read in the input file (defined by the decoder):

      CHARACTER ( LEN = 40 ) :: platform , &    ! description of the 
                                                ! measurement device
                                source          ! GTS data, NCAR ADP files, 
                                                ! bogus information, etc
      REAL                   :: elevation       ! station elevation

      !  During the decoding process, how many errors (non conforming
      !  codes) were encountered, and how many warnings (this is a subjective
      !  call based on repeated incorrect -- but probably not garbled --
      !  GTS codes).  If a bulletin is completely garbled, the logical
      !  flag to not consider this report is set.

      INTEGER              :: num_vld_fld , & ! number of valid fields in the
                                              ! entire report; used as the
                                              ! first tie-breaker in deciding
                                              ! which conflicting data items
                                              ! to keep if have duplicate rpts
                              num_error , &   ! number of errors 
                                              ! encountered during the
                                              ! decoding process
                              num_warning , & ! number of warnings 
                                              ! encountered during the 
                                              ! decoding process
                              seq_num , &     ! sequence numbers that tell
                                              ! which of 2 reports is more
                                              ! recent.
                              num_dups        ! number of duplicates found of
                                              ! this observation 
      LOGICAL              :: is_sound        ! is-a-sounding tells whether
                                              ! the observation possibly has
                                              ! multiple levels vs having 
                                              ! only one level for srfc ob.
      LOGICAL              :: bogus           ! T/F if this is a bogus 
                                              ! observation
      LOGICAL              :: discard         ! Tells whether this observation
                                              ! has been found to be a dup
                                              ! AND has been discarded or
                                              ! merged.
      INTEGER              :: levels          ! Number of upper-air levels

   END TYPE source_info

!--------------------------------------------------------------------------

   TYPE field

      !  Defines a data type consisting of a paired data value (real) with a
      !  quality control flag that holds a binary-coded combination of error
      !  codes; the codes  identify possible problems with the data.

!  Records read in the input file (defined by the decoder):

      REAL                   :: data            ! Observation value y^o
      INTEGER                :: qc              !  Quality control flags
                                                !  that are 0 if data is
                                                !  good, or different 

! Additional records not present in the input file:

!     REAL                   :: inv             ! Innovation vector y^o - H*x^b
      REAL                   :: error           ! Observational error R
                                                !             0 for dot pt
   END TYPE field

!-------------------------------------------------------------------------

   TYPE terrestrial

      !  The data that will occur, at most, once during a report is 
      !  listed here.  These are typically terrestrial measured values.  The
      !  surface met fields are stored in a separate TYPE, to allow a 
      !  POINTER to the next level (if one exists).  This needs to be a 
      !  separate TYPE so as to allow a POINTER to it 

!  Records read in the input file (defined by the decoder):

      TYPE ( field )         :: slp       , &   ! sea level pressure
                                ref_pres  , &   ! reference pres level for
                                                ! the thickness
                                ground_t  , &   ! ground temperature
                                sst       , &   ! sea surface temperature
                                psfc      , &   ! surface pressure
                                precip    , &   ! precipitation accumulation
                                t_max     , &   ! daily temperature max
                                t_min     , &   ! daily temperature min
                                t_min_night , & ! min overnight temperature
                                p_tend03  , &   ! pressure tendency in 3hr
                                p_tend24  , &   ! pressure tendency in 24hr
                                cloud_cvr , &   ! total cloud cover (oktas)
                                ceiling   , &   ! height of lowest cloud base

! Additional records not present in the input file:

                                pw        , &   ! Integrated Precipitable water
                                tb19v     , &   ! Brihtness temperature
                                tb19h     , &   ! Channels
                                tb22v     , &  
                                tb37v     , &
                                tb37h     , &
                                tb85v     , &
                                tb85h
   END TYPE terrestrial 

!-------------------------------------------------------------------------

   TYPE time_info

      !  GTS report time: the valid time of the report.  
      !  The largest INTEGER values require only 8 digits, 
      !  so that this should function properly with 32-bit INTEGERS.  

!  Records read in the input file (defined by the decoder):

      INTEGER                :: sut      , &    ! number of seconds since 1 Jan
                                                ! 0000 UTC 1970
                                julian          ! Julian day
      CHARACTER ( LEN = 14 )    date_char       ! CCYYMMDDHHmmss date

! Additional records not present in the input file:

      CHARACTER (LEN = 19)   :: date_mm5

   END TYPE time_info

!--------------------------------------------------------------------------

   TYPE meas_data

      !  The met data involved with this program is defined in this TYPE.  The
      !  standard state variables (wind, temperature, moisture, with pressure
      !  and/or height to fix the vertical location) are stored here.  For 
      !  single level observations, only one of these records is used per     
      !  observation.   For multi-level reports, a linked list of these 
      !  measurement TYPEs is generated.

!  Records read in the input file (defined by the decoder):

      TYPE ( field )         :: pressure    , & ! pressure of observation
                                height      , & ! height (above sea level) 
                                temperature , & ! 
                                dew_point   , & ! 
                                speed       , & ! 
                                direction   , & ! 
                                u           , & ! u and v components of wind
                                v           , & ! are derived from spd and dir
                                rh          , & !
                                thickness   , & !
                                qv              ! mixing ratio

   END TYPE meas_data

!--------------------------------------------------------------------------

   TYPE measurement

      TYPE ( meas_data )               :: meas  ! contains data and qc code
      TYPE ( measurement ) ,  POINTER  :: next  ! the met info is handled
                                                ! as a linked list of the  
                                                ! measurement type

   END TYPE measurement

!-------------------------------------------------------------------------

   TYPE report                                 
                                               ! this is the entire report
      TYPE ( location_type ) :: location       ! for a single time, from a 
      TYPE ( source_info )   :: info           ! single reporting platform,
      TYPE ( time_info )     :: valid_time     ! a sounding, surface, buoy,
      TYPE ( terrestrial )   :: ground         ! aircraft or ship report
      TYPE ( measurement ) , &
               POINTER       :: surface        

   END TYPE report                            
                                             
!-------------------------------------------------------------------------

   TYPE crs_or_dot

        REAL :: crs
        REAL :: dot

   END TYPE crs_or_dot

!-------------------------------------------------------------------------

   TYPE model_profile

        TYPE (crs_or_dot) :: height
        TYPE (crs_or_dot) :: pressure
        REAL              :: speed
        REAL              :: direction
        REAL              :: u
        REAL              :: v
        REAL              :: t
        REAL              :: td
        REAL              :: rh
        REAL              :: qv

   END TYPE model_profile

!-------------------------------------------------------------------------

END MODULE MODULE_TYPE
