!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE LIST_MODULE
!
! Purpose: This module implements a list with insert, search, and
!   remove routines. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module list_module

   use module_debug

   type list_item
      integer :: ikey, ivalue
      character (len=128) :: ckey, cvalue
      type (list_item), pointer :: next, prev
   end type list_item
 
   type list
      integer :: l_len
      type (list_item), pointer :: head, tail
   end type list

   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_init
   !
   ! Purpose: To initialize a list type 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_init(l)
   
      implicit none
  
      ! Arguments
      type (list), intent(inout) :: l
  
      nullify(l%head)
      nullify(l%tail)
      l%l_len = 0
    
   end subroutine list_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_insert
   !
   ! Purpose: Given a list l, a key, and a value to be stored with that key,
   !   this routine adds (key, value) to the table. 
   !
   ! NOTE: If the key already exists in the list, a second copy of a list item 
   !   with that key is added, possibly with a different associated value. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_insert(l, ikey, ivalue, ckey, cvalue)
   
      implicit none
  
      ! Arguments
      integer, intent(in), optional :: ikey, ivalue
      character (len=128), intent(in), optional :: ckey, cvalue
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      allocate(lp)
      nullify(lp%prev)
      nullify(lp%next)
      if (present(ikey) .and. present(ivalue)) then
         lp%ikey   = ikey
         lp%ivalue = ivalue
      else if (present(ckey) .and. present(cvalue)) then
         lp%ckey   = ckey
         lp%cvalue = cvalue
      else
         call mprintf(.true.,ERROR,'list_insert() called without proper arguments.')
      end if
  
      if (associated(l%tail)) then
         l%tail%next => lp
         lp%prev => l%tail
         l%tail => lp
      else
         l%tail => lp
         l%head => lp
      end if

      l%l_len = l%l_len + 1
 
   end subroutine list_insert
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_get_keys
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function list_get_keys(l)

      implicit none

      ! Arguments
      type (list), intent(in) :: l

      ! Return value
      type (list_item), pointer, dimension(:) :: list_get_keys

      ! Local variables
      integer :: i
      type (list_item), pointer :: lp 

      allocate(list_get_keys(l%l_len)) 

      lp => l%head
  
      i = 1
      do while (associated(lp))
         list_get_keys(i)%ikey   = lp%ikey
         list_get_keys(i)%ivalue = lp%ivalue
         list_get_keys(i)%ckey   = lp%ckey
         list_get_keys(i)%cvalue = lp%cvalue
         lp => lp%next
         i = i + 1
      end do

      return

   end function list_get_keys
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_search
   !
   ! Purpose: If key k is found in the list, this function returns TRUE and sets 
   !   value equal to the value stored with k. If the k is not found, this
   !   function returns FALSE, and value is undefined.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function list_search(l, ikey, ivalue, ckey, cvalue)
   
      implicit none
  
      ! Arguments
      integer, intent(in), optional :: ikey
      integer, intent(out), optional :: ivalue
      character (len=128), intent(in), optional :: ckey
      character (len=128), intent(out), optional :: cvalue
      type (list), intent(inout) :: l
  
      ! Return value
      logical :: list_search
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      list_search = .false.
  
      lp => l%head
  
      do while (associated(lp))
         if (present(ikey) .and. present(ivalue)) then
            if (lp%ikey == ikey) then
               list_search = .true.
               ivalue = lp%ivalue
               exit
            end if
         else if (present(ckey) .and. present(cvalue)) then
            if (lp%ckey == ckey) then
               list_search = .true.
               cvalue = lp%cvalue
               exit
            end if
         else
            call mprintf(.true.,ERROR,'list_search() called without proper arguments.')
         end if
         lp => lp%next
      end do
 
   end function list_search
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: list_get_first_item
   !
   ! Purpose: Sets k and v equal to the key and value, respectively, of the
   !   first item in the list. The list should be thought of as a queue, so that
   !   the first item refers to the least recently inserted item that has not yet
   !   been removed or retrieved. This item is also removed from the list before 
   !   the subroutine returns.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine list_get_first_item(l, ikey, ivalue, ckey, cvalue)
 
      implicit none
  
      ! Arguments
      integer, intent(out), optional :: ikey, ivalue
      character (len=128), intent(out), optional :: ckey, cvalue
      type (list), intent(inout) :: l
 
      ! Local variables
      type (list_item), pointer :: lp
  
      lp => l%head
  
      if (associated(lp)) then
         if (present(ikey) .and. present(ivalue)) then
            ikey = lp%ikey
            ivalue = lp%ivalue
         else if (present(ckey) .and. present(cvalue)) then
            ckey = lp%ckey
            cvalue = lp%cvalue
         else
            call mprintf(.true.,ERROR,'list_get_first_item() called without proper arguments.')
         end if
         l%head => lp%next
         if (associated(lp%next)) nullify(lp%next%prev)
         deallocate(lp)
         l%l_len = l%l_len - 1
      end if
 
   end subroutine list_get_first_item
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_remove
   !
   ! Purpose: Deletes the entry with key k from the list. If multiple entries 
   !   have the specified key, only the first encountered entry is deleted.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_remove(l, ikey, ckey)
   
      implicit none
  
      ! Arguments
      integer, intent(in), optional :: ikey
      character (len=128), intent(in), optional :: ckey
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp 
  
      lp => l%head
  
      do while (associated(lp))
         if (present(ikey)) then
            if (lp%ikey == ikey) then
    
               if (.not. associated(lp%prev)) then
                  l%head => lp%next
                  if (.not. associated(l%head)) nullify(l%tail)
                  if (associated(lp%next)) nullify(lp%next%prev)
                  deallocate(lp)
               else if (.not. associated(lp%next)) then
                  l%tail => lp%prev
                  if (.not. associated(l%tail)) nullify(l%head)
                  if (associated(lp%prev)) nullify(lp%prev%next)
                  deallocate(lp)
               else
                  lp%prev%next => lp%next
                  lp%next%prev => lp%prev
                  deallocate(lp)
               end if
               l%l_len = l%l_len - 1
    
               exit
   
            end if

         else if (present(ckey)) then

            if (lp%ckey == ckey) then

               if (.not. associated(lp%prev)) then
                  l%head => lp%next
                  if (.not. associated(l%head)) nullify(l%tail)
                  if (associated(lp%next)) nullify(lp%next%prev)
                  deallocate(lp)
               else if (.not. associated(lp%next)) then
                  l%tail => lp%prev
                  if (.not. associated(l%tail)) nullify(l%head)
                  if (associated(lp%prev)) nullify(lp%prev%next)
                  deallocate(lp)
               else
                  lp%prev%next => lp%next
                  lp%next%prev => lp%prev
                  deallocate(lp)
               end if
               l%l_len = l%l_len - 1
    
               exit
   
            end if
         else
            call mprintf(.true.,ERROR,'list_remove() called without proper arguments.')
         end if

         lp => lp%next
      end do
 
   end subroutine list_remove
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_length
   !
   ! Purpose: Returns the number of items in the list l.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function list_length(l)
 
      implicit none
  
      ! Arguments
      type (list), intent(in) :: l
  
      ! Return value
      integer :: list_length
  
      list_length = l%l_len
  
      return
 
   end function list_length
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: list_destroy
   !
   ! Purpose: Frees all memory associated with list l. This routine may be
   !   used to remove all entries from a list.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine list_destroy(l)
   
      implicit none
  
      ! Arguments
      type (list), intent(inout) :: l
  
      ! Local variables
      type (list_item), pointer :: lp
  
      lp => l%head
  
      do while (associated(lp))
         l%head => lp%next
         deallocate(lp)
         lp => l%head
      end do
  
      l%l_len = 0
      nullify(l%head)
      nullify(l%tail)
    
   end subroutine list_destroy
 
end module list_module
