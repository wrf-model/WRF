! Implements a heap using an array; top of the heap is the item 
!   with minimum key value
module minheap_module

   use datatype_module
   use module_debug

   ! Maximum heap size -- maybe make this magically dynamic somehow? 
   integer, parameter :: HEAPSIZE = 10000

   ! Type of item to be stored in the heap
   type heap_object
      type (data_node), pointer :: object
   end type heap_object

   ! The heap itself
   type (heap_object), allocatable, dimension(:) :: heap

   ! Index of last item in the heap
   integer :: end_of_heap
   
   contains


   ! Initialize the heap; current functionality can be had without
   !   the need for init function, but we may want more things later
   subroutine init_heap()

      implicit none

      end_of_heap = 0
      allocate(heap(HEAPSIZE))
 
   end subroutine init_heap


   subroutine heap_destroy()

      implicit none

      deallocate(heap)

   end subroutine heap_destroy


   subroutine add_to_heap(x)

      implicit none

      ! Arguments
      type (data_node), pointer :: x

      ! Local variables
      integer :: idx, parent

      call mprintf((end_of_heap == HEAPSIZE),ERROR, 'add_to_heap(): Maximum heap size exceeded') 
      
      end_of_heap = end_of_heap + 1
      idx = end_of_heap
      heap(idx)%object => x
      heap(idx)%object%heap_index = idx
      
      do while (idx > 1)
         parent = floor(real(idx)/2.)
         if (heap(idx)%object%last_used < heap(parent)%object%last_used) then
            heap(idx)%object => heap(parent)%object
            heap(idx)%object%heap_index = idx
            heap(parent)%object => x
            heap(parent)%object%heap_index = parent
            idx = parent
         else
            idx = 1
         end if
      end do 

   end subroutine add_to_heap


   subroutine remove_index(idx)

      implicit none

      ! Arguments
      integer, intent(in) :: idx

      ! Local variables
      integer :: indx, left, right
      type (data_node), pointer :: temp
 
      heap(idx)%object => heap(end_of_heap)%object
      heap(idx)%object%heap_index = idx
      end_of_heap = end_of_heap - 1

      indx = idx

      do while (indx <= end_of_heap)
         left = indx*2
         right = indx*2+1
         if (right <= end_of_heap) then
            if (heap(right)%object%last_used < heap(left)%object%last_used) then
               if (heap(right)%object%last_used < heap(indx)%object%last_used) then
                  temp => heap(indx)%object
                  heap(indx)%object => heap(right)%object
                  heap(indx)%object%heap_index = indx
                  heap(right)%object => temp
                  heap(right)%object%heap_index = right
                  indx = right 
               else
                  indx = end_of_heap + 1
               end if
            else
               if (heap(left)%object%last_used < heap(indx)%object%last_used) then
                  temp => heap(indx)%object
                  heap(indx)%object => heap(left)%object
                  heap(indx)%object%heap_index = indx
                  heap(left)%object => temp
                  heap(left)%object%heap_index = left
                  indx = left 
               else
                  indx = end_of_heap + 1
               end if
            end if
         else if (left <= end_of_heap) then
            if (heap(left)%object%last_used < heap(indx)%object%last_used) then
               temp => heap(indx)%object
               heap(indx)%object => heap(left)%object
               heap(indx)%object%heap_index = indx
               heap(left)%object => temp
               heap(left)%object%heap_index = left
               indx = left 
            else
               indx = end_of_heap + 1
            end if
         else
            indx = end_of_heap + 1
         end if
      end do
      
   end subroutine remove_index

 
   subroutine get_min(x)

      implicit none

      ! Arguments
      type (data_node), pointer :: x

      ! Local variables
      integer :: idx, left, right
      type (data_node), pointer :: temp

      call mprintf((end_of_heap <= 0),ERROR, 'get_min(): No items left in the heap.') 

      x => heap(1)%object

      heap(1)%object => heap(end_of_heap)%object
      heap(1)%object%heap_index = 1
      end_of_heap = end_of_heap - 1
      idx = 1

      do while (idx <= end_of_heap)
         left = idx*2
         right = idx*2+1
         if (right <= end_of_heap) then
            if (heap(right)%object%last_used < heap(left)%object%last_used) then
               if (heap(right)%object%last_used < heap(idx)%object%last_used) then
                  temp => heap(idx)%object
                  heap(idx)%object => heap(right)%object
                  heap(idx)%object%heap_index = idx
                  heap(right)%object => temp
                  heap(right)%object%heap_index = right
                  idx = right 
               else
                  idx = end_of_heap + 1
               end if
            else
               if (heap(left)%object%last_used < heap(idx)%object%last_used) then
                  temp => heap(idx)%object
                  heap(idx)%object => heap(left)%object
                  heap(idx)%object%heap_index = idx
                  heap(left)%object => temp
                  heap(left)%object%heap_index = left
                  idx = left 
               else
                  idx = end_of_heap + 1
               end if
            end if
         else if (left <= end_of_heap) then
            if (heap(left)%object%last_used < heap(idx)%object%last_used) then
               temp => heap(idx)%object
               heap(idx)%object => heap(left)%object
               heap(idx)%object%heap_index = idx
               heap(left)%object => temp
               heap(left)%object%heap_index = left
               idx = left 
            else
               idx = end_of_heap + 1
            end if
         else
            idx = end_of_heap + 1
         end if
      end do

   end subroutine get_min

end module minheap_module
