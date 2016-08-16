!WRF:DRIVER_LAYER:DOMAIN_OBJECT
MODULE module_domain_type

   USE module_driver_constants
   USE module_utility
   USE module_streams

   IMPLICIT NONE

   INTEGER, PARAMETER :: MAX_TILING_ZONES = 20

! needed to provide static definition of IO_MASK_SIZE
#include "../inc/streams.h"

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the 
   !  "next" domain that is to be stored, the other is the next domain which 
   !  happens to also be on the "same_level".

   TYPE streamrec
     INTEGER  ::  stream(IO_MASK_SIZE)
   END TYPE streamrec

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   TYPE tile_zone
      INTEGER, POINTER :: i_start(:)
      INTEGER, POINTER :: i_end(:)
      INTEGER, POINTER :: j_start(:)
      INTEGER, POINTER :: j_end(:)
      INTEGER num_tiles
      INTEGER num_tiles_x
      INTEGER num_tiles_y
   END TYPE tile_zone

   TYPE fieldlist
      CHARACTER*80    :: VarName
      CHARACTER*1     :: Type
      CHARACTER*1     :: ProcOrient  ! 'X' 'Y' or ' ' (X, Y, or non-transposed)
      CHARACTER*80    :: DataName
      CHARACTER*80    :: Description
      CHARACTER*80    :: Units
      CHARACTER*10    :: MemoryOrder
      CHARACTER*10    :: Stagger
      CHARACTER*80    :: dimname1
      CHARACTER*80    :: dimname2
      CHARACTER*80    :: dimname3
      LOGICAL         :: scalar_array
      LOGICAL         :: boundary_array
      LOGICAL         :: restart
   ! definition of IO_MASK_SIZE comes from build and must be the same as
   ! in both definitions of GET_MASK (frame/pack_utils.c and tools/misc.c)
      INTEGER, DIMENSION(IO_MASK_SIZE) :: streams
      INTEGER :: sd1,ed1,sd2,ed2,sd3,ed3
      INTEGER :: sm1,em1,sm2,em2,sm3,em3
      INTEGER :: sp1,ep1,sp2,ep2,sp3,ep3
      CHARACTER*80    :: MemberOf   ! only for 4+D tracer arrays
      INTEGER :: Ndim
      INTEGER :: Ntl                ! 0 single; 1, 2, ... if multi
      LOGICAL                                             :: subgrid_x, subgrid_y  ! true if has a subgrid dimension

      INTEGER, POINTER :: num_table(:)
      INTEGER, POINTER :: index_table(:,:)
      LOGICAL, POINTER :: boundary_table(:,:)
      CHARACTER*256, POINTER :: dname_table(:,:)
      CHARACTER*256, POINTER :: desc_table(:,:)
      CHARACTER*256, POINTER :: units_table(:,:)
      TYPE(streamrec), POINTER :: streams_table(:,:)

      TYPE ( fieldlist ) , POINTER :: next

      REAL, POINTER                                       :: rfield_0d
      REAL, POINTER, DIMENSION(:)                         :: rfield_1d
      REAL, POINTER, DIMENSION(:,:)                       :: rfield_2d
      REAL, POINTER, DIMENSION(:,:,:)                     :: rfield_3d
      REAL, POINTER, DIMENSION(:,:,:,:)                   :: rfield_4d
      REAL, POINTER, DIMENSION(:,:,:,:,:)                 :: rfield_5d
      REAL, POINTER, DIMENSION(:,:,:,:,:,:)               :: rfield_6d
      REAL, POINTER, DIMENSION(:,:,:,:,:,:,:)             :: rfield_7d

      DOUBLE PRECISION, POINTER                           :: dfield_0d
      DOUBLE PRECISION, POINTER, DIMENSION(:)             :: dfield_1d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:)           :: dfield_2d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:,:)         :: dfield_3d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:,:,:)       :: dfield_4d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:,:,:,:)     :: dfield_5d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:,:,:,:,:)   :: dfield_6d
      DOUBLE PRECISION, POINTER, DIMENSION(:,:,:,:,:,:,:) :: dfield_7d

      INTEGER, POINTER                                    :: ifield_0d
      INTEGER, POINTER, DIMENSION(:)                      :: ifield_1d
      INTEGER, POINTER, DIMENSION(:,:)                    :: ifield_2d
      INTEGER, POINTER, DIMENSION(:,:,:)                  :: ifield_3d
      INTEGER, POINTER, DIMENSION(:,:,:,:)                :: ifield_4d
      INTEGER, POINTER, DIMENSION(:,:,:,:,:)              :: ifield_5d
      INTEGER, POINTER, DIMENSION(:,:,:,:,:,:)            :: ifield_6d
      INTEGER, POINTER, DIMENSION(:,:,:,:,:,:,:)          :: ifield_7d

      LOGICAL, POINTER                                    :: lfield_0d
      LOGICAL, POINTER, DIMENSION(:)                      :: lfield_1d
      LOGICAL, POINTER, DIMENSION(:,:)                    :: lfield_2d
! save some space; you can still have these but will not be part of list
! so cannot do i/o, etc on 3d or greater logical arrays
!      LOGICAL, POINTER, DIMENSION(:,:,:)                  :: lfield_3d
!      LOGICAL, POINTER, DIMENSION(:,:,:,:)                :: lfield_4d
!      LOGICAL, POINTER, DIMENSION(:,:,:,:,:)              :: lfield_5d
!      LOGICAL, POINTER, DIMENSION(:,:,:,:,:,:)            :: lfield_6d
!      LOGICAL, POINTER, DIMENSION(:,:,:,:,:,:,:)          :: lfield_7d

   END TYPE fieldlist

#include "state_subtypes.inc"

   TYPE domain

      TYPE ( fieldlist ), POINTER :: head_statevars
      TYPE ( fieldlist ), POINTER :: tail_statevars

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
#include "state_struct.inc"

      INTEGER                                             :: comms( max_comms ), shift_x, shift_y

      INTEGER                                             :: id
      INTEGER                                             :: domdesc
      INTEGER                                             :: communicator
      INTEGER                                             :: iocommunicator
      INTEGER,POINTER                                     :: mapping(:,:)
      INTEGER,POINTER                                     :: i_start(:),i_end(:)
      INTEGER,POINTER                                     :: j_start(:),j_end(:)
      INTEGER                                             :: max_tiles
      INTEGER                                             :: num_tiles        ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_x      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_y      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_spec   ! place to store number of tiles computed from 
                                                                              ! externally specified params

      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents                            
      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests                            
      TYPE(domain) , POINTER                              :: sibling ! overlapped domains at same lev
      LOGICAL                                             :: allocated        ! has alloc_space_field been called on this domain?
      TYPE(domain) , POINTER                              :: intermediate_grid
      LOGICAL                                             :: is_intermediate
      INTEGER :: nids, nide, njds, njde  ! for intermediate domains, carry around the nest dimensions 
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active
      LOGICAL                                             :: active_this_task

      INTEGER      , DIMENSION(MAX_STREAMS)               :: nframes          ! frames per outfile for history 
                                                                              ! 1 is main history

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor
      LOGICAL                                             :: interp_mp        ! .true. = MOIST, SCALAR u,d,f,s will be called
      LOGICAL                                             :: first_force

      ! domain dimensions

      INTEGER    :: sd31,   ed31,   sd32,   ed32,   sd33,   ed33,         &
                    sd21,   ed21,   sd22,   ed22,                         &
                    sd11,   ed11

      INTEGER    :: sp31,   ep31,   sp32,   ep32,   sp33,   ep33,         &
                    sp21,   ep21,   sp22,   ep22,                         &
                    sp11,   ep11,                                         &
                    sm31,   em31,   sm32,   em32,   sm33,   em33,         &
                    sm21,   em21,   sm22,   em22,                         &
                    sm11,   em11,                                         &
                    sp31x,  ep31x,  sp32x,  ep32x,  sp33x,  ep33x,        &
                    sp21x,  ep21x,  sp22x,  ep22x,                        &
                    sm31x,  em31x,  sm32x,  em32x,  sm33x,  em33x,        &
                    sm21x,  em21x,  sm22x,  em22x,                        &
                    sp31y,  ep31y,  sp32y,  ep32y,  sp33y,  ep33y,        &
                    sp21y,  ep21y,  sp22y,  ep22y,                        &
                    sm31y,  em31y,  sm32y,  em32y,  sm33y,  em33y,        &
                    sm21y,  em21y,  sm22y,  em22y

      ! currently allocated domain dimesions
      INTEGER    :: alloced_sd31, alloced_ed31, &
                    alloced_sd32, alloced_ed32, &
                    alloced_sd33, alloced_ed33, &
                    alloced_sm31, alloced_em31, &
                    alloced_sm32, alloced_em32, &
                    alloced_sm33, alloced_em33, &
                    alloced_sm31x, alloced_em31x, &
                    alloced_sm32x, alloced_em32x, &
                    alloced_sm33x, alloced_em33x, &
                    alloced_sm31y, alloced_em31y, &
                    alloced_sm32y, alloced_em32y, &
                    alloced_sm33y, alloced_em33y

      Type(WRFU_Clock), POINTER                           :: domain_clock
      Type(WRFU_Time)                                     :: start_subtime, stop_subtime
      Type(WRFU_Time)                                     :: this_bdy_time, next_bdy_time
      Type(WRFU_Time)                                     :: this_emi_time, next_emi_time
      Type(WRFU_TimeInterval), DIMENSION(MAX_WRF_ALARMS)  :: io_intervals
      Type(WRFU_Alarm), POINTER :: alarms(:)
! This awful hackery accounts for the fact that ESMF2.2.0 objects cannot tell 
! us if they have ever been created or not.  So, we have to keep track of this 
! ourselves to avoid destroying an object that has never been created!  Rip 
! this out once ESMF has useful introspection for creation...  
      LOGICAL :: domain_clock_created
      LOGICAL, POINTER :: alarms_created(:)

      ! Have clocks and times been initialized yet?
      LOGICAL :: time_set
!
! The following are used by the adaptive time step
! T. Hutchinson, WSI  1/11/07
!
      REAL :: max_cfl_val
      REAL :: last_max_vert_cfl
      REAL :: last_max_horiz_cfl
      REAL :: max_vert_cfl
      REAL :: max_horiz_cfl
      Type(WRFU_TimeInterval) :: last_dtInterval

      ! Time series location information
      INTEGER :: ntsloc, ntsloc_domain
      INTEGER :: next_ts_time
      INTEGER, POINTER, DIMENSION(:) :: itsloc, jtsloc, id_tsloc
      REAL, POINTER, DIMENSION(:) :: lattsloc, lontsloc
      CHARACTER (LEN=5), POINTER, DIMENSION(:) :: nametsloc
      CHARACTER (LEN=25), POINTER, DIMENSION(:) :: desctsloc
      CHARACTER (LEN=256), POINTER, DIMENSION(:) :: ts_filename
      LOGICAL :: have_calculated_tslocs
      LOGICAL :: have_displayed_alloc_stats   ! used in module_alloc_space to display alloc stats; only do it once.

! Track location information
      CHARACTER (LEN=19), POINTER, DIMENSION(:) ::  track_time_in
      REAL, POINTER, DIMENSION(:) :: track_lat_in, track_lon_in

      INTEGER :: track_loc, track_loc_domain
      INTEGER :: track_next_time
      INTEGER, POINTER, DIMENSION(:) :: track_i, track_j

      CHARACTER (LEN=19), POINTER, DIMENSION(:) ::  track_time_domain
      REAL, POINTER, DIMENSION(:) :: track_lat_domain, track_lon_domain

      LOGICAL :: track_have_calculated
      LOGICAL :: track_have_input

! 20121003 jm  : for caching tiling
      TYPE( tile_zone ) :: tile_zones(MAX_TILING_ZONES)
      LOGICAL :: tiling_latch(MAX_TILING_ZONES)

   END TYPE domain
END MODULE module_domain_type
