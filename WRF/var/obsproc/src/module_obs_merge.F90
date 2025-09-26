
MODULE module_obs_merge

!------------------------------------------------------------------------------!
! Merge space duplicate stations (same type, same location and same time)
! For each variable and each level, keep (as defined by function keep_best)
! the best variables.
!
!  D. GILL,         April 1998
!  F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

USE module_type
USE module_func


CONTAINS
!------------------------------------------------------------------------------!
! SUBROUTINE merge_obs ( first , second )
! SUBROUTINE keep_best ( field1 , field2 , best )
! SUBROUTINE link_levels ( list1 , list2 , info1 , info2 , best )
! SUBROUTINE merge_measurements ( first ,second , best )
!
!
! --------------------------------------------------------------------------

SUBROUTINE merge_obs ( first , second, print_duplicate, iunit)

!  Reports 'first' and 'second' have been found to have same location and
!  time, therefore they must be merged and one of them discarded.
!  The result of merge is put in 'first' report; second is discarded.
!  If either has data and other has 'missing', keep data; if both have data, take
!  data from one with greatest num_vld_fld or fewest 'num_error'.

   IMPLICIT NONE 

   TYPE ( report ) , INTENT ( INOUT )            :: first , &
                                                    second
! Guo..01/22/2007:
   TYPE (measurement), pointer                   :: surface1, surface2
  
   INTEGER, INTENT (in)                          :: iunit
   LOGICAL, INTENT (in)                          :: print_duplicate

   INTEGER                                       :: best
   CHARACTER (LEN =  80) :: sub_name = "merge_obs"
   CHARACTER (LEN = 160) :: error_message
   LOGICAL :: fatal

!  INCLUDE 'error.inc'
!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

   IF      ( first%info%num_vld_fld .GT. second%info%num_vld_fld ) THEN
      best = 1
   ELSE IF ( first%info%num_vld_fld .LT. second%info%num_vld_fld ) THEN
      best = 2
   ELSE IF ( first%info%num_error   .LT. second%info%num_error   ) THEN
      best = 1
   ELSE IF ( first%info%num_error   .GT. second%info%num_error   ) THEN
      best = 2
   ELSE IF ( first%info%num_warning .LT. second%info%num_warning ) THEN
      best = 1
   ELSE IF ( first%info%num_warning .GT. second%info%num_warning ) THEN
      best = 2
   ELSE IF ( first%info%seq_num     .GT. second%info%seq_num     ) THEN
      best = 1
   ELSE IF ( first%info%seq_num     .LT. second%info%seq_num     ) THEN
      best = 2
   ELSE
      best = 1

      IF (print_duplicate) THEN
      error_message = &
    " Arbitrarily assuming first obs is better than second for " // &
!     TRIM ( first%location%name ) // '  ' // &
!     TRIM ( first%location%id ) // ' ' // &
      TRIM ( first%info%platform) // '.'
      WRITE (UNIT = iunit, FMT = '(A)') TRIM (error_message)

!     fatal = .false.
!     CALL error_handler (sub_name , error_message, "", fatal)

      ENDIF

   END IF

   !  Will put all useful information in first report; discard second
   !  report.  Update report being kept with the num_vld_fld, num_error,
   !  num_warnings, etc, from best.

   IF ( best .EQ. 2 ) THEN
      first%info = second%info
   END IF

   ! Now look at all terrestrial fields, keeping the best values.

! foo
!  IF ( .NOT. ( first%ground .EQ. second%ground ) ) THEN

   IF ( .NOT. ground_eq ( first%ground , second%ground ) ) THEN
      CALL keep_best ( first%ground%slp         , second%ground%slp         , best )
      CALL keep_best ( first%ground%ref_pres    , second%ground%ref_pres    , best )
      CALL keep_best ( first%ground%ground_t    , second%ground%ground_t    , best )
      CALL keep_best ( first%ground%sst         , second%ground%sst         , best )
      CALL keep_best ( first%ground%psfc        , second%ground%psfc        , best )
      CALL keep_best ( first%ground%precip      , second%ground%precip      , best )
      CALL keep_best ( first%ground%t_max       , second%ground%t_max       , best )
      CALL keep_best ( first%ground%t_min       , second%ground%t_min       , best )
      CALL keep_best ( first%ground%t_min_night , second%ground%t_min_night , best )
      CALL keep_best ( first%ground%p_tend03    , second%ground%p_tend03    , best )
      CALL keep_best ( first%ground%p_tend24    , second%ground%p_tend24    , best )
      CALL keep_best ( first%ground%cloud_cvr   , second%ground%cloud_cvr   , best )
      CALL keep_best ( first%ground%ceiling     , second%ground%ceiling     , best )
      CALL keep_best ( first%ground%pw          , second%ground%pw           , best ) 
      CALL keep_best ( first%ground%tb19v       , second%ground%tb19v        , best ) 
      CALL keep_best ( first%ground%tb19h       , second%ground%tb19h        , best ) 
      CALL keep_best ( first%ground%tb22v       , second%ground%tb22v        , best ) 
      CALL keep_best ( first%ground%tb37v       , second%ground%tb37v        , best ) 
      CALL keep_best ( first%ground%tb37h       , second%ground%tb37h        , best ) 
      CALL keep_best ( first%ground%tb85v       , second%ground%tb85v        , best ) 
      CALL keep_best ( first%ground%tb85h       , second%ground%tb85h        , best ) 
!     WRITE (iunit,*) first%ground%pw,second%ground%pw 
   END IF

   !  Merge data at different levels, starting at ground (or lowest level).  On return
   !  all of linked list from second is deallocated, so not much additional memory
   !  is used by keeping the absorbed observation.  The info types are provided so 
   !  that it can be determined if these are both surface observations.
 
! Guo.. 01/22/2007: If either both of reports have no 'surface' data or the
!          "second" has no 'surface', no 'surface' data merging (link_levels)
!          need to do.
 
   if ( (.not.associated(first%surface) .and. &
         .not.associated(second%surface)) .or. &
        (associated(first%surface) .and. &
         .not.associated(second%surface)) )then
       return

! Guo.. 01/22/2007: If the "first" has no 'surface' but the "second" does,
!       link the second%surface to the first%surface because after merging,
!       all useful information is put in first report; discard second
!       
   else if (.not.associated(first%surface) .and. &
                 associated(second%surface)) then
        surface2 => second%surface
        first%surface => surface2
 !       nullify(second%surface)
        return 
   endif

   CALL link_levels ( first%surface , second%surface , &
   first%info , second%info , best )

END SUBROUTINE merge_obs
  
!
!---------------------------------------------------------------------------

SUBROUTINE keep_best ( field1 , field2 , best )

!  Use quality control (qc) info and keep the best value; if one is missing
!  keep the one that is known; if both present keep the one with better qc
!  flag;  if qc flags the same, keep the one chosen 'best'.

   IMPLICIT none

   TYPE ( field ) , INTENT ( INOUT )      :: field1
   TYPE ( field ) , INTENT ( IN )         :: field2
   INTEGER        , INTENT ( IN )         :: best

   CHARACTER ( LEN = 32 ) , PARAMETER     :: sub_name = 'keep_best'
   CHARACTER ( LEN = 80 )                 :: msg

   INCLUDE 'missing.inc'

!  INCLUDE 'error.inc'
!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

! foo
!  IF ( field1 .EQ. field2 ) THEN 
   IF ( field_eq ( field1 , field2 ) ) THEN 

      ! If there is no difference, have nothing to do.

   ELSE IF ( (       eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             ( .NOT. eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      !  Copy both data and quality control flag.

      field1 = field2

   ELSE IF ( ( .NOT. eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             (       eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      !  Already have data in report1, so do nothing.

   ELSE IF ( (       eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             (       eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      !  When both data fields are empty, do nothing.

   ELSE IF ( ( .NOT. eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             ( .NOT. eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      !  There are several cases to consider if both fields have data:

      !  First,  use quality control flags to differentiate the differences.

      IF ( field1%qc == missing ) THEN

         ! if field1 is a recovered value for a missing data

          field1 = field2

      ELSE IF ( field2%qc == missing ) THEN
       
         ! if field2 is a recovered value from the missing data
 
         ! Do nothing, keep field1.

      ELSE IF ( field1%qc .LT. field2%qc ) THEN

         !  In case of no "missing", since the data is already in field1, do nothing.

      ELSE IF ( field1%qc .GT. field2%qc ) THEN

         field1 = field2

      ELSE IF ( field1%qc .EQ. field2%qc ) THEN

         !  Second, if they have the same quality control values, use data 
         !  that was chosen 'best'

         IF ( best .EQ. 1 ) THEN

            !  Again, since the data is already in field1, do nothing.

         ELSE IF ( best .EQ. 2 ) THEN

            field1 = field2

         ELSE

            ! Should never execute this part; have invalid 'best' integer.
            msg = 'Internal logic error.  Invalid value of ''best'''
            CALL error_handler (sub_name, msg, "", .TRUE.)

         END IF

      ELSE  
 
         ! Should never execute this; if so, have fatal error
         msg = 'Internal logic error.  Either the QCs are different or the same.'
         CALL error_handler (sub_name, msg, "", .TRUE. )

      END IF

   ELSE  

      ! should never execute this; if so have fatal error
      msg = 'Internal logic error.  Only four combinations of fields missing are possible.'
      CALL error_handler (sub_name, msg , "", .TRUE.)

   END IF

END SUBROUTINE keep_best

!
!---------------------------------------------------------------------------

SUBROUTINE link_levels ( list1 , list2 , info1 , info2 , best )

!  Starting at the surface level, link levels into one list if pressure levels
!  are different;  if have two levels at the same pressure level (within 
!  epsilon) then keep the best data.  The resulting (output) linked list
!  starts from list1; list2 contains nothing useful on return.

   IMPLICIT NONE

   TYPE ( measurement ) , POINTER           :: list1 , list2
   INTEGER , INTENT ( IN )                  :: best

   TYPE ( measurement ) , POINTER           :: next1 , &
                                               next2 , &
                                               current , &
                                               delete_it

   TYPE ( source_info )                     :: info1 , info2

   INCLUDE 'missing.inc'

   !  Initialize both traversal pointers.

   next1 => list1
   next2 => list2
   NULLIFY ( current )

   !  Merge until the end of either list1 or list2 is reached.

   still_associated : DO WHILE ( ASSOCIATED ( next1 ) .AND. ASSOCIATED ( next2 ) )

      IF (    ( eps_equal ( next1%meas%pressure%data , & 
                            next2%meas%pressure%data , 1. ) ) &
                             .OR.  &
           (  ( eps_equal ( info1%elevation        , & 
                            next1%meas%height%data , .1 ) ) .AND. &
              ( eps_equal ( info2%elevation        , & 
                            next2%meas%height%data , .1 ) ) .AND. &
              ( .NOT. eps_equal ( info1%elevation , missing_r , 1. ) ) .AND. &
              ( eps_equal ( next1%meas%height%data , & 
                            next2%meas%height%data , .1 ) ) ) ) THEN

         !  There are two ways that cause us to merge the data into one level:
         !  1) Both levels are at same pressure level within precision of pressure,
         !  so merge data from both levels into one measurement.
         !  2) If both of the observations are surface reports, then the pressure
         !  may be different, but the height = terrain elevation, and the two
         !  heights are equal.

         CALL merge_measurements ( next1%meas , next2%meas , best )

         !  Advance the pointers.

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            ! are at the head of the output linked list;
            ! already have list1 => next1 so do nothing
         ELSE
            current%next => next1
         END IF 
         current => next1         !  set so current points to next output node 
         next1 => next1%next      !  get next node in list1
         delete_it => next2       !  record location of next2 to delete it
         next2 => next2%next      !  get next node in list2

         !  The bypassed observation can be deleted.

         DEALLOCATE ( delete_it )

         !  Because of the way that the data is merged (allowing the surface data
         !  to be recognized through the elevation = height), there may arise
         !  conditions that allow replicated pressure surfaces.  Those instances
         !  are what we check for in the next two IF blocks.

         duplicates_list1 : DO WHILE ( ASSOCIATED ( next1 ) ) 

            IF      (    ( eps_equal ( current%meas%pressure%data , next1%meas%pressure%data , 1. ) ) &
                                        .OR.  &
                      (  ( eps_equal ( current%meas%height%data   , next1%meas%height%data , .1 ) ) .AND. &
                         ( .NOT. eps_equal ( current%meas%height%data   , missing_r , 1. ) ) .AND. &
                         ( eps_equal ( info1%elevation            , next1%meas%height%data , .1 ) ) ) ) THEN
      
               CALL merge_measurements ( current%meas , next1%meas , best )
      
               !  Advance the next1 pointer, kill the old location.
      
               delete_it => next1       !  record location of next1 to delete it
               next1 => next1%next      !  get next node in list1
      
               !  The bypassed observation can be deleted.
      
               DEALLOCATE ( delete_it )
           
               !  We need to continue checking for more duplicates.
   
               CYCLE duplicates_list1
   
            ELSE
   
               !  There are no more duplicates.
   
               EXIT duplicates_list1 
   
            END IF
         
         END DO duplicates_list1

         duplicates_list2 : DO WHILE ( ASSOCIATED ( next2 ) ) 

            IF      (    ( eps_equal ( current%meas%pressure%data , next2%meas%pressure%data , 1. ) ) &
                                        .OR.  &
                      (  ( eps_equal ( current%meas%height%data   , next2%meas%height%data , .1 ) ) .AND. &
                         ( .NOT. eps_equal ( current%meas%height%data   , missing_r , 1. ) ) .AND. &
                         ( eps_equal ( info2%elevation            , next2%meas%height%data , .1 ) ) ) ) THEN
      
               CALL merge_measurements ( current%meas , next2%meas , best )
      
               !  Advance the next2 pointer, kill the old location.
      
               delete_it => next2       !  record location of next2 to delete it
               next2 => next2%next      !  get next node in list2
      
               !  The bypassed observation can be deleted.
      
               DEALLOCATE ( delete_it )

            ELSE
   
               !  There are no more duplicates.
   
               EXIT duplicates_list2 
   
            END IF
         
         END DO duplicates_list2

      ELSE IF ( next1%meas%pressure%data .LT. next2%meas%pressure%data ) THEN

         ! Link node from list2 in current list.                  

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            ! are at the head of the output list
            list1 => next2
         ELSE
            current%next => next2
         END IF
         current => next2
         next2 => next2%next

      ELSE

         ! Link node from list1 into the current list.

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            ! are at the head of the output list; list1 already points to next1
            !  have list1 => next1 so do nothing
         ELSE
            current%next => next1
         END IF
         current => next1
         next1 => next1%next

      END IF

   END DO still_associated

   !  The end of either list1 or list2 was reached.  The list that is still
   !  associated (not finished), still has data in the list tail.  Have the
   !  current list include that tail.  If both lists are exhausted, nullify 
   !  the last pointer.
   
   IF      ( ASSOCIATED ( next2 ) ) THEN
      current%next => next2
   ELSE IF ( ASSOCIATED ( next1 ) ) THEN
      current%next => next1
   ELSE
      NULLIFY ( current%next )
   END IF

END SUBROUTINE link_levels

!
! ------------------------------------------------------------------------

SUBROUTINE merge_measurements ( first ,second , best )

!  This takes two measurements that have been found to be at the same
!  pressure level and takes the best data from each.  Criterion for 
!  determining which to keep is which has better quality control value.

   IMPLICIT NONE 

   TYPE ( meas_data ) , INTENT ( INOUT )       :: first
   TYPE ( meas_data ) , INTENT ( IN )          :: second
   INTEGER , INTENT ( IN )                     :: best

   CALL keep_best ( first%pressure     , second%pressure     , best ) 
   CALL keep_best ( first%height       , second%height       , best ) 
!  CALL keep_best ( first%zkc          , second%zkc          , best ) 
!  CALL keep_best ( first%zkd          , second%zkd          , best ) 
!  CALL keep_best ( first%zkc_lg       , second%zkc_lg       , best ) 
!  CALL keep_best ( first%zkd_lg       , second%zkd_lg       , best ) 
   CALL keep_best ( first%temperature  , second%temperature  , best ) 
   CALL keep_best ( first%dew_point    , second%dew_point    , best ) 
   CALL keep_best ( first%speed        , second%speed        , best ) 
   CALL keep_best ( first%direction    , second%direction    , best ) 
   CALL keep_best ( first%u            , second%u            , best ) 
   CALL keep_best ( first%v            , second%v            , best ) 
   CALL keep_best ( first%rh           , second%rh           , best ) 
   CALL keep_best ( first%thickness    , second%thickness    , best ) 
   
END SUBROUTINE merge_measurements

END MODULE module_obs_merge
