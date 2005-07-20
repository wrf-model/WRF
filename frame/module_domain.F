!WRF:DRIVER_LAYER:DOMAIN_OBJECT
!
!  Following are the routines contained within this MODULE:

!  alloc_and_configure_domain        1. Allocate the space for a single domain (constants
!                                       and null terminate pointers).
!                                    2. Connect the domains as a linked list.
!                                    3. Store all of the domain constants.
!                                    4. CALL alloc_space_field.

!  alloc_space_field                 1. Allocate space for the gridded data required for
!                                       each domain.

!  dealloc_space_domain              1. Reconnect linked list nodes since the current
!                                       node is removed.
!                                    2. CALL dealloc_space_field.
!                                    3. Deallocate single domain.

!  dealloc_space_field               1. Deallocate each of the fields for a particular
!                                       domain.

!  first_loc_integer                 1. Find the first incidence of a particular
!                                       domain identifier from an array of domain
!                                       identifiers.

MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_state_description
   USE module_configure
   USE module_wrf_error
   USE module_utility

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the "next"
   !  domain that is to be stored, the other is the next domain which happens to
   !  also be on the "same_level".

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   INTEGER, PARAMETER :: HISTORY_ALARM=1, AUXHIST1_ALARM=2, AUXHIST2_ALARM=3,     &
                         AUXHIST3_ALARM=4, AUXHIST4_ALARM=5, AUXHIST5_ALARM=6,    &
                         AUXINPUT1_ALARM=7, AUXINPUT2_ALARM=8, AUXINPUT3_ALARM=9, &
                         AUXINPUT4_ALARM=10, AUXINPUT5_ALARM=11,                  &
                         RESTART_ALARM=12, BOUNDARY_ALARM=13, INPUTOUT_ALARM=14,  &  ! for outputing input (e.g. for 3dvar)
                         ALARM_SUBTIME=15,                                        &
#ifdef MOVE_NESTS
                         COMPUTE_VORTEX_CENTER_ALARM=16,                          &
#endif
                         MAX_WRF_ALARMS=20  ! WARNING:  MAX_WRF_ALARMS must be 
                                            ! large enough to include all of 
                                            ! the alarms declared above.  

#include <state_subtypes.inc>

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
#include <state_struct.inc>

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
      TYPE(domain) , POINTER                              :: intermediate_grid
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active

      INTEGER      , DIMENSION(0:5)                       :: nframes          ! frames per outfile for history 
                                                                              ! streams (0 is main history)                  

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor

      LOGICAL                                             :: first_force

#ifdef MOVE_NESTS
      REAL         :: xi , xj
      REAL         :: vc_i, vc_j    ! vortex center i and j
#endif

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
      Type(WRF_UTIL_Clock), POINTER                           :: domain_clock
      Type(WRF_UTIL_Time)                                     :: start_time, stop_time, current_time
      Type(WRF_UTIL_Time)                                     :: start_subtime, stop_subtime
      Type(WRF_UTIL_Time)                                     :: this_bdy_time, next_bdy_time
      Type(WRF_UTIL_Time)                                     :: this_emi_time, next_emi_time
      Type(WRF_UTIL_TimeInterval) :: step_time
      Type(WRF_UTIL_Alarm), pointer :: alarms(:)
! This awful hackery accounts for the fact that ESMF2.1.0+ alarms cannot tell 
! us if they have ever been created or not.  So, we have to keep track of this 
! ourselves to avoid destroying an alarm that has never been created!  Rip this 
! out once ESMF has useful introspection here...  
      LOGICAL, pointer :: alarms_created(:)

   END TYPE domain

   !  Now that a "domain" TYPE exists, we can use it to store a few pointers
   !  to this type.  These are primarily for use in traversing the linked list.
   !  The "head_grid" is always the pointer to the first domain that is
   !  allocated.  This is available and is not to be changed.  The others are
   !  just temporary pointers.

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   !  To facilitate an easy integration of each of the domains that are on the
   !  same level, we have an array for the head pointer for each level.  This
   !  removed the need to search through the linked list at each time step to
   !  find which domains are to be active.

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level
   
CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

    END SELECT data_ordering

#if 0
    CALL dealloc_space_field ( grid )

    CALL alloc_space_field ( grid, grid%id , 1 , 2 , .FALSE. ,     &
                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                             grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                             grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                             grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )
#endif

    RETURN

   END SUBROUTINE adjust_domain_dims_for_move


   SUBROUTINE get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe    )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_YXZ )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_ZXY )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_ZYX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_XZY )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp32 
           kpe             = grid%ep32 

       CASE  ( DATA_ORDER_YZX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp32 
           kpe             = grid%ep32 

    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid

! Default version ; Otherwise module containing interface to DM library will provide

   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
!<DESCRIPTION>
! Wrf_patch_domain is called as part of the process of initiating a new
! domain.  Based on the global domain dimension information that is
! passed in it computes the patch and memory dimensions on this
! distributed-memory process for parallel compilation when DM_PARALLEL is
! defined in configure.wrf.  In this case, it relies on an external
! communications package-contributed routine, wrf_dm_patch_domain. For
! non-parallel compiles, it returns the patch and memory dimensions based
! on the entire domain. In either case, the memory dimensions will be
! larger than the patch dimensions, since they allow for distributed
! memory halo regions (DM_PARALLEL only) and for boundary regions around
! the domain (used for idealized cases only).  The width of the boundary
! regions to be accommodated is passed in as bdx and bdy.
! 
! The bdy_mask argument is a four-dimensional logical array, each element
! of which is set to true for any boundaries that this process's patch
! contains (all four are true in the non-DM_PARALLEL case) and false
! otherwise. The indices into the bdy_mask are defined in
! frame/module_state_description.F. P_XSB corresponds boundary that
! exists at the beginning of the X-dimension; ie. the western boundary;
! P_XEB to the boundary that corresponds to the end of the X-dimension
! (east). Likewise for Y (south and north respectively).
! 
! The correspondence of the first, second, and third dimension of each
! set (domain, memory, and patch) with the coordinate axes of the model
! domain is based on the setting of the variable model_data_order, which
! comes into this routine through USE association of
! module_driver_constants in the enclosing module of this routine,
! module_domain.  Model_data_order is defined by the Registry, based on
! the dimspec entries which associate dimension specifiers (e.g. 'k') in
! the Registry with a coordinate axis and specify which dimension of the
! arrays they represent. For WRF, the sd1 , ed1 , sp1 , ep1 , sm1 , and
! em1 correspond to the starts and ends of the global, patch, and memory
! dimensions in X; those with 2 specify Z (vertical); and those with 3
! specify Y.  Note that the WRF convention is to overdimension to allow
! for staggered fields so that sd<em>n</em>:ed<em>n</em> are the starts
! and ends of the staggered domains in X.  The non-staggered grid runs
! sd<em>n</em>:ed<em>n</em>-1. The extra row or column on the north or
! east boundaries is not used for non-staggered fields.
! 
! The domdesc and parent_domdesc arguments are for external communication
! packages (e.g. RSL) that establish and return to WRF integer handles
! for referring to operations on domains.  These descriptors are not set
! or used otherwise and they are opaque, which means they are never
! accessed or modified in WRF; they are only only passed between calls to
! the external package.
!</DESCRIPTION>

   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  ! z-xpose (std)
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  ! x-xpose
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  ! y-xpose
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent

!local data

   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

#ifndef DM_PARALLEL

   bdy_mask = .true.     ! only one processor so all 4 boundaries are there

! this is a trivial version -- 1 patch per processor; 
! use version in module_dm to compute for DM
   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    ! just copy
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    ! just copy
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3
! assigns mostly just to suppress warning messages that INTENT OUT vars not assigned
   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3

#else
! This is supplied by the package specific version of module_dm, which
! is supplied by the external package and copied into the src directory
! when the code is compiled. The cp command will be found in the externals
! target of the configure.wrf file for this architecture.  Eg: for RSL
! routine is defined in external/RSL/module_dm.F .
! Note, it would be very nice to be able to pass parent to this routine;
! however, there doesn't seem to be a way to do that in F90. That is because
! to pass a pointer to a domain structure, this call requires an interface
! definition for wrf_dm_patch_domain (otherwise it will try to convert the
! pointer to something). In order to provide an interface definition, we
! would need to either USE module_dm or use an interface block. In either
! case it generates a circular USE reference, since module_dm uses
! module_domain.  JM 20020416

   CALL wrf_dm_patch_domain( id , domdesc , parent_id , parent_domdesc , &
                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                         sp1x , ep1x , sm1x , em1x , &
                                         sp2x , ep2x , sm2x , em2x , &
                                         sp3x , ep3x , sm3x , em3x , &
                                         sp1y , ep1y , sm1y , em1y , &
                                         sp2y , ep2y , sm2y , em2y , &
                                         sp3y , ep3y , sm3y , em3y , &
                             bdx , bdy )

   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_YXZ )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
      CASE ( DATA_ORDER_ZXY )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_ZYX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_XZY )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_YZX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   END SELECT

#endif

   RETURN
   END SUBROUTINE wrf_patch_domain
!
   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )

!<DESCRIPTION>
! This subroutine is used to allocate a domain data structure of
! TYPE(DOMAIN) pointed to by the argument <em>grid</em>, link it into the
! nested domain hierarchy, and set it's configuration information from
! the appropriate settings in the WRF namelist file. Specifically, if the
! domain being allocated and configured is nest, the <em>parent</em>
! argument will point to the already existing domain data structure for
! the parent domain and the <em>kid</em> argument will be set to an
! integer indicating which child of the parent this grid will be (child
! indices start at 1).  If this is the top-level domain, the parent and
! kid arguments are ignored.  <b>WRF domains may have multiple children
! but only ever have one parent.</b>
!
! The <em>domain_id</em> argument is the
! integer handle by which this new domain will be referred; it comes from
! the grid_id setting in the namelist, and these grid ids correspond to
! the ordering of settings in the namelist, starting with 1 for the
! top-level domain. The id of 1 always corresponds to the top-level
! domain.  and these grid ids correspond to the ordering of settings in
! the namelist, starting with 1 for the top-level domain.
! 
! Model_data_order is provide by USE association of
! module_driver_constants and is set from dimspec entries in the
! Registry.
! 
! The allocation of the TYPE(DOMAIN) itself occurs in this routine.
! However, the numerous multi-dimensional arrays that make up the members
! of the domain are allocated in the call to alloc_space_field, after
! wrf_patch_domain has been called to determine the dimensions in memory
! that should be allocated.  It bears noting here that arrays and code
! that indexes these arrays are always global, regardless of how the
! model is decomposed over patches. Thus, when arrays are allocated on a
! given process, the start and end of an array dimension are the global
! indices of the start and end of that process's subdomain.
! 
! Configuration information for the domain (that is, information from the
! namelist) is added by the call to <a href=med_add_config_info_to_grid.html>med_add_config_info_to_grid</a>, defined
! in share/mediation_wrfmain.F. 
!</DESCRIPTION>

      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)                           :: domain_id
      TYPE( domain ) , POINTER                       :: grid
      TYPE( domain ) , POINTER                       :: parent
      INTEGER , INTENT(IN)                           :: kid    ! which kid of parent am I?

      !  Local data.
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny


! This next step uses information that is listed in the registry as namelist_derived
! to properly size the domain and the patches; this in turn is stored in the new_grid
! data structure


      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering


#ifdef RSL
! Check domain size to be sure it is within RSLs limit
      IF ( nx .GE. 1024 .OR. ny .GE. 1024 ) THEN
        WRITE ( wrf_err_message , * ) 'domain too large for RSL. Use RSL_LITE or other comm package.'
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

#endif

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: Incorrect value for num_time_levels ', &
                                       num_time_levels
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF

! provided by application, WRF defines in share/module_bc.F
      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE ( new_grid%parents( max_parents ) )
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    ! initialize in setup_timekeeping
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO

#ifdef MOVE_NESTS
      new_grid%xi = -1.0
      new_grid%xj = -1.0
      new_grid%vc_i = -1.0
      new_grid%vc_j = -1.0
#endif

      ! set up the pointers that represent the nest hierarchy
      ! set this up *prior* to calling the patching or allocation
      ! routines so that implementations of these routines can
      ! traverse the nest hierarchy (through the root head_grid)
      ! if they need to 

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    ! note assumption that nest can have only 1 parent
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 ! this needs to be assigned prior to calling wrf_patch_domain

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     ! z-xpose dims
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     ! (standard)
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     ! x-xpose dims
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     ! y-xpose dims
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         ! initialize the number of frames per file (array assignment)

      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. ,      &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   ! x-xpose
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   ! y-xpose
      )

      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           ! this is a mediation layer routine

! Some miscellaneous state that is in the Registry but not namelist data

      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)

! This next set of includes causes all but the namelist_derived variables to be
! properly assigned to the new_grid record

      grid => new_grid

#ifdef DM_PARALLEL
      CALL wrf_get_dm_communicator ( grid%communicator )
      CALL wrf_dm_define_comms( grid )
#endif

   END SUBROUTINE alloc_and_configure_domain

!

!  This routine ALLOCATEs the required space for the meteorological fields
!  for a specific domain.  The fields are simply ALLOCATEd as an -1.  They
!  are referenced as wind, temperature, moisture, etc. in routines that are
!  below this top-level of data allocation and management (in the solve routine
!  and below).

   SUBROUTINE alloc_space_field ( grid,   id, setinitval ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      
      USE module_configure
      IMPLICIT NONE
 

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval   ! 3 = everything, 1 = arrays only, 0 = none
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.
      ! e.g. to set both 1st and second time level, use 3
      !      to set only 1st                        use 1
      !      to set only 2st                        use 2
      INTEGER , INTENT(IN)            :: tl_in
  
      ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated
      ! false otherwise (all allocated, modulo tl above)
      LOGICAL , INTENT(IN)            :: inter_domain_in

      !  Local data.
      INTEGER dyn_opt, idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop

#if 1
      tl = tl_in
      inter_domain = inter_domain_in
#else
      tl = 3
      inter_domain = .FALSE.
#endif

#if ( RWORDSIZE == 8 )
      initial_data_value = 0.
#else
      CALL get_initial_data_value ( initial_data_value )
#endif

      CALL nl_get_dyn_opt( 1, dyn_opt )
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 


      IF ( dyn_opt == DYN_NODYN ) THEN

        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: dynamics disabled ' )

#if ALLOW_NODYN
# include <nodyn_allocs.inc>
#else
        CALL wrf_error_fatal("To run the the NODYN option, recompile -DALLOW_NODYN in ARCHFLAGS settings of configure.wrf") 
#endif

#if (EM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_EM ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: Eulerian Mass Coordinate ')
# include <em_allocs.inc>
#endif
#if (NMM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_NMM ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )
# include <nmm_allocs.inc>
#endif
#if (COAMPS_CORE == 1)
      ELSE IF ( dyn_opt == DYN_COAMPS ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: coamps dyncore' )
# include <coamps_allocs.inc>
#endif
#if (EXP_CORE==1)
      ELSE IF ( dyn_opt == DYN_EXP ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( 'DYNAMICS OPTION: experimental dyncore' )
# include <exp_allocs.inc>
#endif

      ELSE
      
        WRITE( wrf_err_message , * )'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

      WRITE(message,*)'alloc_space_field: domain ',id,' ',num_bytes_allocated
      CALL  wrf_debug( 1, message )

   END SUBROUTINE alloc_space_field

!

!  This routine is used to DEALLOCATE space for a single domain.  First
!  the pointers in the linked list are fixed (so the one in the middle can
!  be removed).  Second, the field data are all removed through a CALL to 
!  the dealloc_space_domain routine.  Finally, the pointer to the domain
!  itself is DEALLOCATEd.

   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)            :: id

      !  Local data.

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found
      INTEGER                         :: alarmid

      !  Initializations required to start the routine.

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      !  The identity of the domain to delete is based upon the "id".
      !  We search all of the possible grids.  It is required to find a domain
      !  otherwise it is a fatal error.  

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL dealloc_space_field ( grid )
            DEALLOCATE( grid%parents )
            DEALLOCATE( grid%nests )
            ! clean up time manager bits
            CALL WRF_UTIL_ClockDestroy( grid%domain_clock )
            DO alarmid = 1, MAX_WRF_ALARMS
              IF ( grid%alarms_created( alarmid ) ) THEN
                CALL WRF_UTIL_AlarmDestroy( grid%alarms( alarmid ) )
                grid%alarms_created( alarmid ) = .FALSE.
              ENDIF
            ENDDO
            DEALLOCATE( grid%alarms )
            DEALLOCATE( grid%alarms_created )
            DEALLOCATE( grid%domain_clock )
            DEALLOCATE(grid)
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal ( TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain

!

!  This routine DEALLOCATEs each gridded field for this domain.  For each type of
!  different array (1d, 2d, 3d, etc.), the space for each pointer is DEALLOCATEd
!  for every -1 (i.e., each different meteorological field).

   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain)              , POINTER :: grid

      !  Local data.

      INTEGER                             :: dyn_opt, ierr

      CALL nl_get_dyn_opt( 1, dyn_opt )

      IF      ( .FALSE. )           THEN

#if (EM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_EM ) THEN
# include <em_deallocs.inc>
#endif
#if (NMM_CORE == 1)
      ELSE IF ( dyn_opt == DYN_NMM ) THEN
# include <nmm_deallocs.inc>
#endif
#if (COAMPS_CORE == 1)
      ELSE IF ( dyn_opt == DYN_COAMPS ) THEN
# include <coamps_deallocs.inc>
#endif
#if (EXP_CORE==1)
      ELSE IF ( dyn_opt == DYN_EXP ) THEN
# include <exp_deallocs.inc>
#endif
      ELSE
        WRITE( wrf_err_message , * )'dealloc_space_field: Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

   END SUBROUTINE dealloc_space_field

!
!
   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid
! <DESCRIPTION>
! This is a recursive subroutine that traverses the domain hierarchy rooted
! at the input argument <em>in_grid</em>, a pointer to TYPE(domain), and returns
! a pointer to the domain matching the integer argument <em>id</em> if it exists.
!
! </DESCRIPTION>
      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      IF ( ASSOCIATED( in_grid ) ) THEN
      IF ( in_grid%id .EQ. id ) THEN
         result_grid => in_grid
      ELSE
         grid_ptr => in_grid
         DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
            DO kid = 1, max_nests
               IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                  CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                  IF ( ASSOCIATED( result_grid ) ) THEN
                    IF ( result_grid%id .EQ. id ) found = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
         ENDDO
      ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      !  Output data.

      INTEGER                             :: loc

!<DESCRIPTION>
!  This routine is used to find a specific domain identifier in an array
!  of domain identifiers.
!
!</DESCRIPTION>
      
      !  Local data.

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer
!
   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain

END MODULE module_domain
