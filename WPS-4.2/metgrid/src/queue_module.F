!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: queue_module
!
! Description: This module implements a queue of user-defined data types and 
!   a set of routines related to the maintenance and manipulation of the queue.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module queue_module

   use module_debug

   type q_data         ! The user-defined datatype to store in the queue
#ifdef _GEOGRID
      real :: lat, lon
      integer :: x, y
      integer :: depth ! Used by 'search' interpolation method
#endif
#ifdef _METGRID
      integer :: x, y
      integer :: sr_x, sr_y
      character (len=128) :: units, description, stagger
      integer :: depth ! Used by 'search' interpolation method
#endif
   end type q_data
 
   type q_item         ! Wrapper for item to be stored in the queue
      type (q_data) :: data
      type (q_item), pointer :: next
   end type q_item
 
   type queue          ! The queue object, defined by a head and tail pointer
      type (q_item), pointer :: head, tail
      integer :: length
   end type queue
 

   contains
 
  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_init
   !
   ! Purpose: To initialize a queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine q_init(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      nullify(q%head)
      nullify(q%tail)
      q%length = 0
 
   end subroutine q_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_insert
   !
   ! Purpose: To insert an item in the tail of the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine q_insert(q, qitem)
    
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
      type (q_data), intent(in) :: qitem
  
      ! Local variables
      type (q_item), pointer :: newitem
  
      allocate(newitem)
      newitem%data = qitem
      nullify(newitem%next) 
      if (.not.associated(q%tail)) then
         q%head=>newitem
         q%tail=>newitem
      else
         q%tail%next=>newitem
         q%tail=>newitem
      end if
  
      q%length = q%length + 1
 
   end subroutine q_insert
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_isdata
   ! 
   ! Purpose: This function returns FALSE if the queue is empty and TRUE otherwise 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_isdata(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
      logical :: q_isdata
  
      q_isdata = .false.
    
      if (associated(q%head) .and. (q%length >= 1)) then 
         q_isdata = .true.
      end if
 
   end function q_isdata
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_peek
   ! 
   ! Purpose: To return the item in the head of the queue, without
   !    actually removing the item 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_peek(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
      type (q_data) :: q_peek
  
      if (associated(q%head)) then
         q_peek = q%head%data 
      else
         call mprintf(.true.,ERROR,'q_peek(): Trying to peek at an empty queue')
      end if
 
   end function q_peek
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_length
   ! 
   ! Purpose: To return the number of items currently in the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_length(q)
   
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
  !    type (q_item), pointer :: cursor
      integer :: q_length      
  
      q_length = q%length
  
  ! USE THE FOLLOWING TO COUNT THE LENGTH BY ACTUALLY TRAVERSING THE LINKED LIST
  ! REPRESENTATION OF THE QUEUE
  !    if (associated(q%head)) then
  !       q_length = q_length + 1
  !       cursor=>q%head
  !       do while(associated(cursor%next))
  !         cursor=>cursor%next
  !         q_length = q_length + 1
  !       end do
  !    end if
 
   end function q_length
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_remove
   ! 
   ! Purpose: To return the item stored at the head of the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_remove(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      ! Local variables
      type (q_data) :: q_remove
      type (q_item), pointer :: cursor
       
      if (associated(q%head)) then
         if (associated(q%head%next)) then
            cursor=>q%head%next
            q_remove = q%head%data
            deallocate(q%head)
            q%head=>cursor
         else
            q_remove = q%head%data
            deallocate(q%head)
            nullify(q%head)
            nullify(q%tail)
         end if 
         q%length = q%length - 1
      else
         call mprintf(.true.,ERROR,'q_remove(): Trying to remove from an empty queue')
      end if
 
   end function q_remove
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_destroy
   ! 
   ! Purpose: To free all memory allocated by the queue, thus destroying any 
   !    items that have not been removed
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine q_destroy(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      ! Local variables
      type (q_item), pointer :: cursor
  
      q%length = 0
  
      if (associated(q%head)) then
         do while(associated(q%head%next))
            cursor=>q%head
            q%head=>q%head%next
            deallocate(cursor)
         end do
         deallocate(q%head)
      end if
 
   end subroutine q_destroy

end module queue_module
