!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE HASH_MODULE
!
! Purpose: This module provides a dictionary/hashtable with insert, search, and
!   remove routines. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module hash_module

   ! Parameters
   integer, parameter :: TABLESIZE=53     ! Number of spaces in the table (the
                                          !   number of linked lists)
 
   type hashnode
      character (len=256) :: entry         ! The actual string to be stored 
      type (hashnode), pointer :: next
   end type hashnode
 
   type hashnode_ptr
      type (hashnode), pointer :: p        ! Pointer to a list of entries
   end type hashnode_ptr
 
   type hashtable
      type (hashnode_ptr), dimension(TABLESIZE) :: table ! The hashtable array
   end type hashtable
 
   contains
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: hash_init
   !
   ! Purpose: To initialize a hashtable
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine hash_init(h)
   
     implicit none
 
     ! Arguments
     type (hashtable), intent(inout) :: h
 
     ! Local variables
     integer :: i
 
     do i=1,TABLESIZE
        nullify(h%table(i)%p)
     end do
 
   end subroutine hash_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: hash_insert
   !
   ! Purpose: Given a hashtable h and a string to be inserted into the hashtable,
   !   this routine adds string to the table. 
   !
   ! NOTE: If the string already exists in the table, a second copy of the
   !   string is added to the table
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine hash_insert(h, string)
   
     implicit none
 
     ! Arguments
     character (len=256), intent(in) :: string
     type (hashtable), intent(inout) :: h
 
     ! Local variables
     integer :: hashval, i
     type (hashnode), pointer :: hn 
 
     hashval = 0
     do i=1,len(string)
        hashval = hashval + iachar(string(i:i))
     end do
     hashval = mod(hashval, TABLESIZE) + 1  
    
     allocate(hn) 
     hn%entry = string
     hn%next => h%table(hashval)%p
     h%table(hashval)%p => hn 
 
   end subroutine hash_insert
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: hash_search
   !
   ! Purpose: This function returns TRUE if the specified string was found in the
   !   hashtable h, and FALSE otherwise.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function hash_search(h, string)
   
      implicit none
  
      ! Arguments
      character (len=256), intent(in) :: string
      type (hashtable), intent(inout) :: h
  
      ! Return value
      logical :: hash_search
  
      ! Local variables
      integer :: hashval, i
      type (hashnode), pointer :: cursor 
  
      hash_search = .false.
  
      hashval = 0
      do i=1,len(string)
         hashval = hashval + iachar(string(i:i))
      end do
      hashval = mod(hashval, TABLESIZE) + 1  
     
      cursor => h%table(hashval)%p
      do while(associated(cursor))
         if (cursor%entry == string) then
            hash_search = .true.
            return 
         else
            cursor => cursor%next 
         end if
      end do
    
      return
 
   end function hash_search
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: hash_destroy
   !
   ! Purpose: Frees all memory associated with hashtable h. This routine may be
   !   used to remove all entries from a hashtable.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine hash_destroy(h)
   
      implicit none
  
      ! Arguments
      type (hashtable), intent(inout) :: h
  
      ! Local variables
      integer :: i
      type (hashnode), pointer :: cursor, cursor_prev
     
      do i=1,TABLESIZE
         cursor => h%table(i)%p
         do while(associated(cursor))
            cursor_prev => cursor
            cursor => cursor%next
            deallocate(cursor_prev)
         end do 
         nullify(h%table(i)%p)
      end do 
 
   end subroutine hash_destroy
 
end module hash_module
