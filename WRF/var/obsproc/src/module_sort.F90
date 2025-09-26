
MODULE module_sort

!------------------------------------------------------------------------------
! Sort observation by location and time
!
!  D. GILL,         April 1998
!  F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------

USE module_type
USE module_func

CONTAINS
!------------------------------------------------------------------------------
! SUBROUTINE sort_obs   (obs , array_size , index)
! SUBROUTINE merge_sort (obs , index , 1 , array_size )
!
! -----------------------------------------------------------------------------

 SUBROUTINE sort_obs ( obs , array_size , compare, index )
!SUBROUTINE sort_obs ( obs , array_size , index )

!  This takes an array of observations ('report' type) and sorts them,
!  or more precisely, creates an index giving their order.  This is the
!  driver that calls recursive merge_sort.

   IMPLICIT NONE

   INTEGER , INTENT ( IN )                            :: array_size
   TYPE ( report ) , INTENT ( IN ) , DIMENSION ( : )  :: obs
   INTEGER , INTENT ( OUT )        , DIMENSION ( : )  :: index
   
   INTEGER                                            :: i

   INTERFACE 
    LOGICAL FUNCTION compare (a, b, flag)
      USE module_type
      IMPLICIT NONE
      INTEGER         , INTENT ( IN )     :: flag
      TYPE ( report ) , INTENT ( IN )     :: a  ! the first data item compared
      TYPE ( report ) , INTENT ( IN )     :: b  ! the second data item compared
    END FUNCTION compare
   END INTERFACE 

   !  Iinitialize index for sorting.  These indices are sorted, not the
   !  array of observations.

!   index = (/ ( i , i = 1 , array_size ) /)
    do i = 1 , array_size
      index(i) = i
    enddo

   !  On entry, index is numerically sequential (1, 2, ... , array_size).  On
   !  return, index is the array that holds the obs sequential order by location
   !  (function compare).  The "ordered" observations may then be tested for
   !  duplicates (which is the purpose for this ordering).

   CALL merge_sort ( obs , index , 1 , array_size, compare )

  
END SUBROUTINE sort_obs

!
! -----------------------------------------------------------------------------

RECURSIVE SUBROUTINE merge_sort ( obs , index , low , high, compare )
!RECURSIVE SUBROUTINE merge_sort ( obs , index , low , high )

!  This is the recursive part of the merge sort routine.  This routine recurses
!  all the way down to a list of 2 items, which it sorts with no more
!  recursion.  It would be faster to stop at list of 10 or so and use n^2
!  sort like selection-sort; but this is easier and plenty fast.  This does 
!  not actually sort the observations, it creates an index to a sorted list
!  list of ovbservations.

   IMPLICIT NONE

   TYPE ( report )        , INTENT ( IN    ) , DIMENSION ( : ) :: obs 
   INTEGER                , INTENT ( INOUT ) , DIMENSION ( : ) :: index
   INTEGER                , INTENT ( IN    )                   :: low        , &
                                                                  high

   INTEGER , ALLOCATABLE                     , DIMENSION ( : ) :: tmp_old
   INTEGER                                                        mid        , &
                                                                  current    , & 
                                                                  first_ndx  , & 
                                                                  second_ndx , &
                                                                  temp
   INTERFACE 
    LOGICAL FUNCTION compare (a, b, flag)
      USE module_type
      IMPLICIT NONE
      INTEGER         , INTENT ( IN )     :: flag
      TYPE ( report ) , INTENT ( IN )     :: a  ! the first data item compared
      TYPE ( report ) , INTENT ( IN )     :: b  ! the second data item compared
    END FUNCTION compare
   END INTERFACE 


   !  The list is either small (2 items), or too big.  If it is too large, it
   !  is recursively broken in half.

   break_it_down : IF ( high - low .GE. 2 ) THEN

     !write(1001,'("In  RECURSIVE SUBROUTINE merge_sort: high and low:",2i10)')&
     ! high, low

      !  Half-way point for the list that is too large to handle.

      mid = ( low + high ) / 2

      !  Have merge_sort work on both halves of the list.

      CALL merge_sort ( obs , index , low , mid , compare)
!     CALL merge_sort ( obs , index , low , mid )
      CALL merge_sort ( obs , index , mid + 1 , high, compare)
!     CALL merge_sort ( obs , index , mid + 1 , high)

      !  Have now got two sorted lists.  They are now to be merged into one list.
      !  Allocate  additional temporary space for this list merging.

      ALLOCATE ( tmp_old ( low : mid ) )

      !  The lower half of the list is stored in the new space.

      tmp_old = index ( low : mid )

      !  Initialize the list sorting counters.

      first_ndx = low
      second_ndx = mid + 1
      current = low

      !  The lists are sorted until one of the indices is out of bounds.

      sort_two_groups : DO WHILE (first_ndx .LE. mid .AND. second_ndx .LE. high)

         !  Either the first index or the second index is the lowest.  The
         !  current counter keeps the correct choice, and the used index is
         !  incremented.

! foo
!        IF ( obs(tmp_old(first_ndx)) .LT. obs(index(second_ndx)) ) THEN
         IF ( compare (obs(tmp_old(first_ndx)) , obs(index(second_ndx)), 1 ) &
             ) THEN
            index(current) = tmp_old(first_ndx)
            first_ndx = first_ndx + 1
         ELSE 
            index(current) = index(second_ndx)
            second_ndx = second_ndx + 1
         ENDIF

         !  Increment the cureent counter, which is the sorted list 
         !  of observations.

         current = current + 1

      END DO sort_two_groups

      !  After the above sort, must still copy the tail end of tmp_old 
      !  (if any is left over from above).  There is NO need to move the tail 
      !  end if the tail is in index -- it is already there.

      tail_of_first_group : DO WHILE ( first_ndx .LE. mid )
         index(current) = tmp_old(first_ndx)
         current = current + 1
         first_ndx = first_ndx + 1
      END DO tail_of_first_group

      !  Free up the temporary memory before continuing.

      DEALLOCATE ( tmp_old )

   ELSE break_it_down

      !  Now we have a list that is EASY to sort, just 1 or 2 elements.  
      !  Either the indices need to be swapped (this if test), 
      !  or they are already ordered.

! foo
!     small_enough : IF ( obs(index(high)) .LT. obs(index(low)) ) THEN
      small_enough : IF ( compare ( obs(index(high)) , obs(index(low)), 0) &
                         ) THEN
         temp = index(low)
         index(low) = index(high)
         index(high) = temp       
      END IF small_enough

   END IF break_it_down

END SUBROUTINE merge_sort

END MODULE module_sort

