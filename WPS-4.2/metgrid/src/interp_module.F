!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE INTERP_MODULE
!
! This module provides routines for interpolation.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module interp_module

   use bitarray_module
   use misc_definitions_module
   use module_debug
   use queue_module
 
   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_array_from_string
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function interp_array_from_string(interp_string)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: interp_string

      ! Local variables
      integer :: j, p1, p2, iend, num_methods 

      ! Return value
      integer, pointer, dimension(:) :: interp_array_from_string

      ! Get an idea of how many interpolation methods are in the string
      !   so we can allocate an appropriately sized array
      num_methods = 1
      do j=1,len_trim(interp_string)
         if (interp_string(j:j) == '+') num_methods = num_methods + 1
      end do

      allocate(interp_array_from_string(num_methods+1))
      interp_array_from_string = 0

      iend = len_trim(interp_string)

      p1 = 1
      p2 = index(interp_string(1:iend),'+')
      j = 1
      do while(p2 >= p1)
         if (index(interp_string(p1:p2-1),'nearest_neighbor') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('nearest_neighbor')) then
            interp_array_from_string(j) = N_NEIGHBOR
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_4pt')) then
            interp_array_from_string(j) = AVERAGE4
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_16pt')) then
            interp_array_from_string(j) = AVERAGE16
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_4pt')) then
            interp_array_from_string(j) = W_AVERAGE4
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_16pt')) then
            interp_array_from_string(j) = W_AVERAGE16
            j = j + 1
         else if (index(interp_string(p1:p2-1),'four_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('four_pt')) then
            interp_array_from_string(j) = FOUR_POINT
            j = j + 1
         else if (index(interp_string(p1:p2-1),'sixteen_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('sixteen_pt')) then
            interp_array_from_string(j) = SIXTEEN_POINT
            j = j + 1
         else if (index(interp_string(p1:p2-1),'search') /= 0) then
            interp_array_from_string(j) = SEARCH
            j = j + 1
         else
            if (index(interp_string(p1:p2-1),'average_gcell') == 0) &
               call mprintf(.true.,WARN,'Unrecognized interpolation method %s.',s1=interp_string(p1:p2-1))
         end if
         p1 = p2 + 1
         p2 = index(interp_string(p1:iend),'+') + p1 - 1
      end do

      p2 = iend+1
      if (p1 < iend) then
         if (index(interp_string(p1:p2-1),'nearest_neighbor') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('nearest_neighbor')) then
            interp_array_from_string(j) = N_NEIGHBOR
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_4pt')) then
            interp_array_from_string(j) = AVERAGE4
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_16pt')) then
            interp_array_from_string(j) = AVERAGE16
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_4pt')) then
            interp_array_from_string(j) = W_AVERAGE4
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_16pt')) then
            interp_array_from_string(j) = W_AVERAGE16
            j = j + 1
         else if (index(interp_string(p1:p2-1),'four_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('four_pt')) then
            interp_array_from_string(j) = FOUR_POINT
            j = j + 1
         else if (index(interp_string(p1:p2-1),'sixteen_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('sixteen_pt')) then
            interp_array_from_string(j) = SIXTEEN_POINT
            j = j + 1
         else if (index(interp_string(p1:),'search') /= 0) then
            interp_array_from_string(j) = SEARCH
            j = j + 1
         else
            if (index(interp_string(p1:p2-1),'average_gcell') == 0) &
               call mprintf(.true.,WARN,'Unrecognized interpolation method %s.',s1=interp_string(p1:p2-1))
         end if
      end if

      return

   end function interp_array_from_string


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_options_from_string
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function interp_options_from_string(interp_string)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: interp_string

      ! Local variables
      integer :: j, p1, p2, iend, num_methods, istatus

      ! Return value
      integer, pointer, dimension(:) :: interp_options_from_string

      ! Get an idea of how many interpolation methods are in the string
      !   so we can allocate an appropriately sized array
      num_methods = 1
      do j=1,len_trim(interp_string)
         if (interp_string(j:j) == '+') num_methods = num_methods + 1
      end do

      allocate(interp_options_from_string(num_methods+1))
      interp_options_from_string(:) = 0

      iend = len_trim(interp_string)

      p1 = 1
      p2 = index(interp_string(1:iend),'+')
      j = 1
      do while(p2 >= p1)
         if (index(interp_string(p1:p2-1),'nearest_neighbor') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('nearest_neighbor')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_4pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_16pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_4pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_16pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'four_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('four_pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'sixteen_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('sixteen_pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'search') /= 0) then
            call get_search_depth(interp_string(p1:p2-1), interp_options_from_string(j), istatus)
            j = j + 1
         else
            if (index(interp_string(p1:p2-1),'average_gcell') == 0) &
               call mprintf(.true.,WARN,'Unrecognized interpolation method %s.',s1=interp_string(p1:p2-1))
         end if
         p1 = p2 + 1
         p2 = index(interp_string(p1:iend),'+') + p1 - 1
      end do

      p2 = iend+1
      if (p1 < iend) then
         if (index(interp_string(p1:p2-1),'nearest_neighbor') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('nearest_neighbor')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_4pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('average_16pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_4pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_4pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'wt_average_16pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('wt_average_16pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'four_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('four_pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:p2-1),'sixteen_pt') /= 0 .and. &
             len_trim(interp_string(p1:p2-1)) == len_trim('sixteen_pt')) then
            interp_options_from_string(j) = 0
            j = j + 1
         else if (index(interp_string(p1:),'search') /= 0) then
            call get_search_depth(interp_string(p1:), interp_options_from_string(j), istatus)
            j = j + 1
         else
            if (index(interp_string(p1:p2-1),'average_gcell') == 0) &
               call mprintf(.true.,WARN,'Unrecognized interpolation method %s.',s1=interp_string(p1:p2-1))
         end if
      end if

      return

   end function interp_options_from_string


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_search_depth
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_search_depth(interp_opt, depth, istatus)
   
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      integer, intent(out) :: depth
      character (len=*), intent(in) :: interp_opt

      ! Local variables
      integer :: i, p1, p2, p

      istatus = 1
      depth = 1200

      i = index(interp_opt,'search')
      if (i /= 0) then

         ! Check for a max search depth 
         p = index(interp_opt(i:),'+')
         if (p == 0) p = len_trim(interp_opt)
         p1 = index(interp_opt(i:p),'(')
         p2 = index(interp_opt(i:p),')')
         if (p1 /= 0 .and. p2 /= 0) then
            read(interp_opt(p1+1:p2-1),*,err=1000) depth
         else if (p1 == 0 .and. p2 == 0) then
            ! keep depth at 1200, no warning
         else
            call mprintf(.true., WARN, 'Problem with specified search depth '// &
                         'for search interp option. Setting max depth to 1200.')
            depth = 1200
         end if
      end if
      istatus = 0
    
      return

      1000 call mprintf(.true., ERROR, 'Search depth option to search interpolator '// &
                        'must be an integer value, enclosed in parentheses immediately '// &
                        'after keyword "search"')
  
   end subroutine get_search_depth
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_sequence
   !
   ! Purpose: Delegates the actual task of interpolation to specific 
   !   interpolation routines defined in the module.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function interp_sequence(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to 
                                                !   interpolate within
      real, intent(in) :: xx , yy               ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(in) :: array
      integer, intent(in) :: idx
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      real, dimension(start_x:end_x, start_y:end_y), intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational
  
      ! Return value
      real :: interp_sequence       
  
      ! No more interpolation methods to try
      if (interp_list(idx) == 0) then
         interp_sequence = msgval
         return
      end if
  
      if (interp_list(idx) == FOUR_POINT) then
         interp_sequence = four_pt(xx, yy, izz, array, start_x, end_x, &
                                   start_y, end_y, start_z, end_z, &
                                   msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == AVERAGE4) then
         interp_sequence = four_pt_average(xx, yy, izz, array, start_x, end_x, &
                                           start_y, end_y, start_z, end_z, &
                                           msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == W_AVERAGE4) then
         interp_sequence = wt_four_pt_average(xx, yy, izz, array, start_x, end_x, &
                                              start_y, end_y, start_z, end_z, &
                                              msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == N_NEIGHBOR) then
         interp_sequence = nearest_neighbor(xx, yy, izz, array, start_x, end_x, &
                                            start_y, end_y, start_z, end_z, &
                                            msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == SIXTEEN_POINT) then
         interp_sequence = sixteen_pt(xx, yy, izz, array, start_x, end_x, &
                                      start_y, end_y, start_z, end_z, &
                                      msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == SEARCH) then
         interp_sequence = search_extrap(xx, yy, izz, array, start_x, end_x, &
                                         start_y, end_y, start_z, end_z, &
                                         msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == AVERAGE16) then
         interp_sequence = sixteen_pt_average(xx, yy, izz, array, start_x, end_x, &
                                              start_y, end_y, start_z, end_z, &
                                              msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      else if (interp_list(idx) == W_AVERAGE16) then
         interp_sequence = wt_sixteen_pt_average(xx, yy, izz, array, start_x, end_x, &
                                                 start_y, end_y, start_z, end_z, &
                                                 msgval, interp_list, interp_opts, idx+1, mask_relational, maskval, mask_array)
      end if
 
   end function interp_sequence
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: nearest_neighbor
   !
   ! Purpose: Returns the point nearest to (xx,yy). If (xx,yy) is outside of the
   !   array, the point on the edge of the array nearest to (xx,yy) is returned.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function nearest_neighbor(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
    
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to 
                                                !   interpolate within
      real, intent(in) :: xx , yy               ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: nearest_neighbor
  
      ! Local variables
      integer :: ix, iy

      ix = nint(xx)
      iy = nint(yy)
  
      ! The first thing to do is to ensure that the point (xx,yy) is within the array
      if (ix < start_x .or. ix > end_x) then
         nearest_neighbor = msgval
         return
      end if
  
      if (iy < start_y .or. iy > end_y) then
         nearest_neighbor = msgval
         return
      end if

      if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
         if (mask_relational == '<' .and. mask_array(ix,iy) < maskval) then 
            nearest_neighbor = msgval
         else if (mask_relational == '>' .and. mask_array(ix,iy) > maskval) then 
            nearest_neighbor = msgval
         else if (mask_relational == ' ' .and. mask_array(ix,iy) == maskval) then
            nearest_neighbor = msgval
         else
            nearest_neighbor = array(ix,iy,izz)
         end if        
      else if (present(mask_array) .and. present(maskval)) then 
         if (maskval == mask_array(ix,iy)) then
            nearest_neighbor = msgval
         else
            nearest_neighbor = array(ix,iy,izz)
         end if        
      else
         nearest_neighbor = array(ix,iy,izz)
      end if
  
      ! If we have a missing value, try the next interpolation method in the sequence
      if (nearest_neighbor == msgval) then
         nearest_neighbor = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                             start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
      end if
  
   end function nearest_neighbor
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: search_extrap
   !
   ! Purpose: Returns the point nearest to (xx,yy) that has a non-missing value.
   !   If no valid value can be found in the array, msgval is returned.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function search_extrap(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to search
      real, intent(in) :: xx , yy               ! The location of the search origin 
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: search_extrap
  
      ! Local variables
      integer :: i, j
      real :: distance
      logical :: found_valid
      type (bitarray) :: b
      type (queue) :: q
      type (q_data) :: qdata

      ! We only search if the starting point is within the array
      if (nint(xx) < start_x .or. nint(xx) > end_x .or. &
          nint(yy) < start_y .or. nint(yy) > end_y) then
         search_extrap = msgval
         return 
      end if
      
      call bitarray_create(b, (end_x-start_x+1), (end_y-start_y+1))
      call q_init(q)
  
      found_valid = .false.
      qdata%x = nint(xx)
      qdata%y = nint(yy)
      qdata%depth = 0
      call q_insert(q, qdata)
      call bitarray_set(b, qdata%x-start_x+1, qdata%y-start_y+1)

      do while (q_isdata(q) .and. (.not. found_valid))
         qdata = q_remove(q) 
         i = qdata%x
         j = qdata%y

         if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
            if (mask_relational == '>' .and. mask_array(i,j) <= maskval .and. array(i,j,izz) /= msgval) then
               found_valid = .true.
            else if (mask_relational == '<' .and. mask_array(i,j) >= maskval .and. array(i,j,izz) /= msgval) then
               found_valid = .true.
            else if (mask_relational == ' ' .and. mask_array(i,j) /= maskval .and. array(i,j,izz) /= msgval) then
               found_valid = .true.
            end if
         else if (present(mask_array) .and. present(maskval)) then
            if (array(i,j,izz) /= msgval .and. mask_array(i,j) /= maskval) found_valid = .true.                  
         else
            if (array(i,j,izz) /= msgval) found_valid = .true. 
         end if

         if (i-1 >= start_x) then
            if (.not. bitarray_test(b, (i-1)-start_x+1, j-start_y+1)) then
               if (qdata%depth < interp_opts(idx-1)) then       ! idx-1, since idx was incremented before call to this subroutine
                  qdata%x = i-1
                  qdata%y = j
                  qdata%depth = qdata%depth+1
                  call q_insert(q, qdata)
                  call bitarray_set(b, (i-1)-start_x+1, j-start_y+1)
               end if
            end if
         end if  
         if (i+1 <= end_x) then
            if (.not. bitarray_test(b, (i+1)-start_x+1, j-start_y+1)) then
               if (qdata%depth < interp_opts(idx-1)) then       ! idx-1, since idx was incremented before call to this subroutine
                  qdata%x = i+1
                  qdata%y = j
                  qdata%depth = qdata%depth+1
                  call q_insert(q, qdata)
                  call bitarray_set(b, (i+1)-start_x+1, j-start_y+1)
               end if
            end if
         end if  
         if (j-1 >= start_y) then
            if (.not. bitarray_test(b, i-start_x+1, (j-1)-start_y+1)) then
               if (qdata%depth < interp_opts(idx-1)) then       ! idx-1, since idx was incremented before call to this subroutine
                  qdata%x = i
                  qdata%y = j-1
                  qdata%depth = qdata%depth+1
                  call q_insert(q, qdata)
                  call bitarray_set(b, i-start_x+1, (j-1)-start_y+1)
               end if
            end if
         end if  
         if (j+1 <= end_y) then
            if (.not. bitarray_test(b, i-start_x+1, (j+1)-start_y+1)) then
               if (qdata%depth < interp_opts(idx-1)) then       ! idx-1, since idx was incremented before call to this subroutine
                  qdata%x = i
                  qdata%y = j+1
                  qdata%depth = qdata%depth+1
                  call q_insert(q, qdata)
                  call bitarray_set(b, i-start_x+1, (j+1)-start_y+1)
               end if
            end if
         end if  
      end do
  
      if (found_valid) then
         distance = (real(i)-xx)*(real(i)-xx)+(real(j)-yy)*(real(j)-yy) 
         search_extrap = array(i,j,izz)
         do while (q_isdata(q))
            qdata = q_remove(q)
            if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
               if (mask_relational == '<' .and. mask_array(qdata%x,qdata%y) >= maskval &
                   .and. array(qdata%x,qdata%y,izz) /= msgval) then
                  if ((real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) < distance) then
                     distance = (real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) 
                     search_extrap = array(qdata%x, qdata%y, izz)
                  end if
               else if (mask_relational == '>' .and. mask_array(qdata%x,qdata%y) <= maskval &
                        .and. array(qdata%x,qdata%y,izz) /= msgval) then
                  if ((real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) < distance) then
                     distance = (real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) 
                     search_extrap = array(qdata%x, qdata%y, izz)
                  end if
               else if (mask_relational == ' ' .and. mask_array(qdata%x,qdata%y) /= maskval &
                        .and. array(qdata%x,qdata%y,izz) /= msgval) then
                  if ((real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) < distance) then
                     distance = (real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) 
                     search_extrap = array(qdata%x, qdata%y, izz)
                  end if
               end if
                
            else if (present(mask_array) .and. present(maskval)) then
               if (array(qdata%x,qdata%y,izz) /= msgval .and. mask_array(qdata%x,qdata%y) /= maskval) then
                  if ((real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) < distance) then
                     distance = (real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) 
                     search_extrap = array(qdata%x, qdata%y, izz)
                  end if
               end if
                
            else
               if (array(qdata%x,qdata%y,izz) /= msgval) then
                  if ((real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) < distance) then
                     distance = (real(qdata%x)-xx)*(real(qdata%x)-xx)+(real(qdata%y)-yy)*(real(qdata%y)-yy) 
                     search_extrap = array(qdata%x, qdata%y, izz)
                  end if
               end if
            end if
         end do
      else
         search_extrap = msgval
      end if
  
      call q_destroy(q)
      call bitarray_destroy(b)
 
   end function search_extrap
 
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: four_pt_average
   !
   ! Purpose: Average of four surrounding grid point values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function four_pt_average(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to 
                                                !   interpolate within
      real, intent(in) :: xx, yy                ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: four_pt_average
  
      ! Local variables
      integer :: ifx, ify, icx, icy
      real :: fxfy, fxcy, cxfy, cxcy

      fxfy = 1.0
      fxcy = 1.0
      cxfy = 1.0
      cxcy = 1.0
  
      ifx = floor(xx)
      icx = ceiling(xx)
      ify = floor(yy)
      icy = ceiling(yy)

      ! First, make sure that the point is contained in the source array
      if (ifx < start_x .or. icx > end_x .or. &
          ify < start_y .or. icy > end_y) then

         ! But if the point is at most half a grid point out, we can
         !   still proceed with modified ifx, icx, ify, and icy.
         if (xx > real(start_x)-0.5 .and. ifx < start_x) then
            ifx = start_x
            icx = start_x
         else if (xx < real(end_x)+0.5 .and. icx > end_x) then
            ifx = end_x
            icx = end_x
         end if

         if (yy > real(start_y)-0.5 .and. ify < start_y) then
            ify = start_y
            icy = start_y
         else if (yy < real(end_y)+0.5 .and. icy > end_y) then
            ify = end_y
            icy = end_y
         end if

         if (ifx < start_x .or. icx > end_x .or. &
             ify < start_y .or. icy > end_y) then
            four_pt_average = msgval
            return
         end if
      end if

      if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
         ! we determine which maskval is useable by... if the symbol > is found, then only 
         !    values less than 'maskval' can be used and if the symbol < is found,  
         !    then only the values greater than 'maskval' can be used.
         if (mask_relational == '>') then
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) > maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) > maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) > maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) > maskval) cxcy = 0.0 
         else if (mask_relational == '<') then
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) < maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) < maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) < maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) < maskval) cxcy = 0.0
         else  !equal
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) == maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) == maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) == maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) == maskval) cxcy = 0.0 
         end if 
      else if (present(mask_array) .and. present(maskval)) then
         if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) == maskval) fxfy = 0.0 
         if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) == maskval) fxcy = 0.0 
         if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) == maskval) cxfy = 0.0 
         if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) == maskval) cxcy = 0.0 
      else
         if (array(ifx, ify, izz) == msgval) fxfy = 0.0 
         if (array(ifx, icy, izz) == msgval) fxcy = 0.0 
         if (array(icx, ify, izz) == msgval) cxfy = 0.0 
         if (array(icx, icy, izz) == msgval) cxcy = 0.0 
      end if
  
      ! If all four points are missing, try the next interpolation method in the sequence
      if (fxfy == 0.0 .and. fxcy == 0.0 .and. cxfy == 0.0 .and. cxcy == 0.0) then
         four_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                             start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
      else
         four_pt_average = (fxfy * array(ifx, ify, izz) + &
                            fxcy * array(ifx, icy, izz) + &
                            cxfy * array(icx, ify, izz) + &
                            cxcy * array(icx, icy, izz) ) / (fxfy + fxcy + cxfy + cxcy)
      end if
 
   end function four_pt_average


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: wt_four_pt_average
   !
   ! Purpose: Weighted average of four surrounding grid point values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function wt_four_pt_average(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to 
                                                !   interpolate within
      real, intent(in) :: xx, yy                ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: wt_four_pt_average
  
      ! Local variables
      integer :: ifx, ify, icx, icy
      real :: fxfy, fxcy, cxfy, cxcy

      ifx = floor(xx)
      icx = ceiling(xx)
      ify = floor(yy)
      icy = ceiling(yy)

      fxfy = max(0., 1.0 - sqrt((xx-real(ifx))**2+(yy-real(ify))**2))
      fxcy = max(0., 1.0 - sqrt((xx-real(ifx))**2+(yy-real(icy))**2))
      cxfy = max(0., 1.0 - sqrt((xx-real(icx))**2+(yy-real(ify))**2))
      cxcy = max(0., 1.0 - sqrt((xx-real(icx))**2+(yy-real(icy))**2))

      ! First, make sure that the point is contained in the source array
      if (ifx < start_x .or. icx > end_x .or. &
          ify < start_y .or. icy > end_y) then

         ! But if the point is at most half a grid point out, we can
         !   still proceed with modified ifx, icx, ify, and icy.
         if (xx > real(start_x)-0.5 .and. ifx < start_x) then
            ifx = start_x
            icx = start_x
         else if (xx < real(end_x)+0.5 .and. icx > end_x) then
            ifx = end_x
            icx = end_x
         end if

         if (yy > real(start_y)-0.5 .and. ifx < start_y) then
            ify = start_y
            icy = start_y
         else if (yy < real(end_y)+0.5 .and. icy > end_y) then
            ify = end_y
            icy = end_y
         end if

         if (ifx < start_x .or. icx > end_x .or. &
             ify < start_y .or. icy > end_y) then
            wt_four_pt_average = msgval
            return
         end if
      end if

      if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then 
         ! we determine which maskval is useable by... if the symbol > is found, then only 
         !    values less than 'maskval' can be used and if the symbol < is found,  
         !    then only the values greater than 'maskval' can be used.
         if (mask_relational == '>') then
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) > maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) > maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) > maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) > maskval) cxcy = 0.0 
         else if (mask_relational == '<') then
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) < maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) < maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) < maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) < maskval) cxcy = 0.0
         else  !equal
            if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) == maskval) fxfy = 0.0 
            if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) == maskval) fxcy = 0.0 
            if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) == maskval) cxfy = 0.0 
            if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) == maskval) cxcy = 0.0 
         end if              
      else if (present(mask_array) .and. present(maskval)) then
         if (array(ifx, ify, izz) == msgval .or. mask_array(ifx,ify) == maskval) fxfy = 0.0 
         if (array(ifx, icy, izz) == msgval .or. mask_array(ifx,icy) == maskval) fxcy = 0.0 
         if (array(icx, ify, izz) == msgval .or. mask_array(icx,ify) == maskval) cxfy = 0.0 
         if (array(icx, icy, izz) == msgval .or. mask_array(icx,icy) == maskval) cxcy = 0.0 
      else
         if (array(ifx, ify, izz) == msgval) fxfy = 0.0 
         if (array(ifx, icy, izz) == msgval) fxcy = 0.0 
         if (array(icx, ify, izz) == msgval) cxfy = 0.0 
         if (array(icx, icy, izz) == msgval) cxcy = 0.0 
      end if

      ! If all four points are missing, try the next interpolation method in the sequence
      if (fxfy == 0.0 .and. fxcy == 0.0 .and. cxfy == 0.0 .and. cxcy == 0.0) then
         wt_four_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
      else
         wt_four_pt_average = (fxfy * array(ifx, ify, izz) + &
                               fxcy * array(ifx, icy, izz) + &
                               cxfy * array(icx, ify, izz) + &
                               cxcy * array(icx, icy, izz) ) / (fxfy + fxcy + cxfy + cxcy)
      end if
 
   end function wt_four_pt_average
 
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: sixteen_pt_average
   !
   ! Purpose: Average of sixteen surrounding grid point values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function sixteen_pt_average(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to
                                                !   interpolate within
      real, intent(in) :: xx , yy               ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: sixteen_pt_average
  
      ! Local variables
      integer :: i, j, ifx, ify
      real :: sum, sum_weight
      real, dimension(4,4) :: weights

      ifx = floor(xx)
      ify = floor(yy)
  
      ! First see whether the point is far enough within the array to 
      !   allow for a sixteen point average.
      if (ifx < start_x+1 .or. ifx > end_x-2 .or. &
          ify < start_y+1 .or. ify > end_y-2) then
         sixteen_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         return
      end if
  
      sum_weight = 0.0
      do i=1,4
         do j=1,4
            if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
               if (mask_relational == '>' .and. mask_array(ifx+3-i, ify+3-j) > maskval) then
                   weights(i,j) = 0.0
               else if (mask_relational == '<' .and. mask_array(ifx+3-i, ify+3-j) < maskval) then
                   weights(i,j) = 0.0
               else if (mask_relational == ' ' .and. mask_array(ifx+3-i, ify+3-j) == maskval) then
                   weights(i,j) = 0.0
               else
                   weights(i,j) = 1.0
               end if
               if (array(ifx+3-i, ify+3-j, izz) == msgval) weights(i,j) = 0.0
            else if (present(mask_array) .and. present(maskval)) then
               if (array(ifx+3-i, ify+3-j, izz) == msgval .or. mask_array(ifx+3-i, ify+3-j) == maskval) then
                  weights(i,j) = 0.0
               else
                  weights(i,j) = 1.0
               end if
            else
               if (array(ifx+3-i, ify+3-j, izz) == msgval) then
                  weights(i,j) = 0.0
               else
                  weights(i,j) = 1.0
               end if
            end if
    
            sum_weight = sum_weight + weights(i,j)
   
         end do
      end do
  
      ! If all points are missing, try the next interpolation method in the sequence
      if (sum_weight == 0.0) then
         sixteen_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
      else
         sum = 0.0
         do i=1,4
            do j=1,4
               sum = sum + weights(i,j) * array(ifx+3-i, ify+3-j, izz)
            end do
         end do
         sixteen_pt_average = sum / sum_weight
      end if
 
   end function sixteen_pt_average
 
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: wt_sixteen_pt_average
   !
   ! Purpose: Weighted average of sixteen surrounding grid point values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function wt_sixteen_pt_average(xx, yy, izz, array, start_x, end_x, &
              start_y, end_y, start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to
                                                !   interpolate within
      real, intent(in) :: xx , yy               ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: wt_sixteen_pt_average
  
      ! Local variables
      integer :: i, j, ifx, ify
      real :: sum, sum_weight
      real, dimension(4,4) :: weights

      ifx = floor(xx)
      ify = floor(yy)
  
      ! First see whether the point is far enough within the array to 
      !   allow for a sixteen point average.
      if (ifx < start_x+1 .or. ifx > end_x-2 .or. &
          ify < start_y+1 .or. ify > end_y-2) then
         wt_sixteen_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                   start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         return
      end if
  
      sum_weight = 0.0
      do i=1,4
         do j=1,4  
            if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
               if (mask_relational == '>' .and. mask_array(ifx+3-i, ify+3-j) > maskval) then
                   weights(i,j) = 0.0
               else if (mask_relational == '<' .and. mask_array(ifx+3-i, ify+3-j) < maskval) then
                   weights(i,j) = 0.0
               else if (mask_relational == ' ' .and. mask_array(ifx+3-i, ify+3-j) == maskval) then
                   weights(i,j) = 0.0
               else
                   weights(i,j) = max(0., 2.0 - sqrt((xx-real(ifx+3-i))**2+(yy-real(ify+3-j))**2))
               end if
               if (array(ifx+3-i, ify+3-j, izz) == msgval) weights(i,j) = 0.0
            else if (present(mask_array) .and. present(maskval)) then
               if (array(ifx+3-i, ify+3-j, izz) == msgval .or. mask_array(ifx+3-i, ify+3-j) == maskval) then
                  weights(i,j) = 0.0
               else
                  weights(i,j) = max(0., 2.0 - sqrt((xx-real(ifx+3-i))**2+(yy-real(ify+3-j))**2))
               end if
            else
               if (array(ifx+3-i, ify+3-j, izz) == msgval) then
                  weights(i,j) = 0.0
               else
                  weights(i,j) = max(0., 2.0 - sqrt((xx-real(ifx+3-i))**2+(yy-real(ify+3-j))**2))
               end if
            end if
    
            sum_weight = sum_weight + weights(i,j)
   
         end do
      end do
  
      ! If all points are missing, try the next interpolation method in the sequence
      if (sum_weight == 0.0) then
         wt_sixteen_pt_average = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                   start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
      else
         sum = 0.0
         do i=1,4
            do j=1,4
               sum = sum + weights(i,j) * array(ifx+3-i, ify+3-j, izz)
            end do
         end do
         wt_sixteen_pt_average = sum / sum_weight
      end if
 
   end function wt_sixteen_pt_average
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: four_pt
   !
   ! Purpose: Bilinear interpolation among four grid values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function four_pt(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                       start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: izz                ! The z-index of the 2d-array to 
                                                !   interpolate within
      real, intent(in) :: xx , yy               ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: four_pt
  
      ! Local variables
      integer :: min_x, min_y, max_x, max_y

      min_x = floor(xx)
      min_y = floor(yy)
      max_x = ceiling(xx)
      max_y = ceiling(yy)
  
      if (min_x < start_x .or. max_x > end_x) then
         four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                   msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         return
      end if
  
      if (min_y < start_y .or. max_y > end_y) then
         four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                   msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         return
      end if
  
      ! If we have a missing value, try the next interpolation method in the sequence
      if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
         ! we determine which maskval is useable by... if the symbol > is found, then only 
         !    values less than 'maskval' can be used and if the symbol < is found,  
         !    then only the values greater than 'maskval' can be used.
         if (mask_relational == '>' ) then                 
            if (array(min_x,min_y,izz) == msgval .or. mask_array(min_x,min_y) > maskval .or. &
                array(max_x,min_y,izz) == msgval .or. mask_array(max_x,min_y) > maskval .or. &
                array(min_x,max_y,izz) == msgval .or. mask_array(min_x,max_y) > maskval .or. &
                array(max_x,max_y,izz) == msgval .or. mask_array(max_x,max_y) > maskval) then 
               four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                     msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
               return
            end if
         else if (mask_relational == '<' ) then
            if (array(min_x,min_y,izz) == msgval .or. mask_array(min_x,min_y) < maskval .or. &
                array(max_x,min_y,izz) == msgval .or. mask_array(max_x,min_y) < maskval .or. &
                array(min_x,max_y,izz) == msgval .or. mask_array(min_x,max_y) < maskval .or. &
                array(max_x,max_y,izz) == msgval .or. mask_array(max_x,max_y) < maskval) then 
               four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                         msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
               return
             end if           
         else if (mask_relational == ' ' ) then
            if (array(min_x,min_y,izz) == msgval .or. mask_array(min_x,min_y) == maskval .or. &
                array(max_x,min_y,izz) == msgval .or. mask_array(max_x,min_y) == maskval .or. &
                array(min_x,max_y,izz) == msgval .or. mask_array(min_x,max_y) == maskval .or. &
                array(max_x,max_y,izz) == msgval .or. mask_array(max_x,max_y) == maskval) then 
               four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                         msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
               return
            end if
         end if
      else if (present(mask_array) .and. present(maskval)) then
         if (array(min_x,min_y,izz) == msgval .or. mask_array(min_x,min_y) == maskval .or. &
             array(max_x,min_y,izz) == msgval .or. mask_array(max_x,min_y) == maskval .or. &
             array(min_x,max_y,izz) == msgval .or. mask_array(min_x,max_y) == maskval .or. &
             array(max_x,max_y,izz) == msgval .or. mask_array(max_x,max_y) == maskval) then 
            four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                      msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         end if
      else
         if (array(min_x,min_y,izz) == msgval .or. &
             array(max_x,min_y,izz) == msgval .or. &
             array(min_x,max_y,izz) == msgval .or. &
             array(max_x,max_y,izz) == msgval ) then 
            four_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                start_z, end_z, msgval, interp_list, interp_opts, idx)
            return
         end if
      end if
  
      if (min_x == max_x) then
         if (min_y == max_y) then
            four_pt = array(min_x,min_y,izz)
         else
            four_pt = array(min_x,min_y,izz)*(real(max_y)-yy) + &
                      array(min_x,max_y,izz)*(yy-real(min_y)) 
         end if
      else if (min_y == max_y) then
         if (min_x == max_x) then
            four_pt = array(min_x,min_y,izz)
         else
            four_pt = array(min_x,min_y,izz)*(real(max_x)-xx) + &
                      array(max_x,min_y,izz)*(xx-real(min_x)) 
         end if
      else
         four_pt = (yy - min_y) * (array(min_x,max_y,izz)*(real(max_x)-xx) + &
                                   array(max_x,max_y,izz)*(xx-real(min_x))) + &
                   (max_y - yy) * (array(min_x,min_y,izz)*(real(max_x)-xx) + &
                                   array(max_x,min_y,izz)*(xx-real(min_x)));
      end if
 
   end function four_pt
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: sixteen_pt
   !
   ! Purpose: Overlapping parabolic interpolation among sixteen grid values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive function sixteen_pt(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                 msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
    
      implicit none
     
      ! Arguments
      integer, intent(in) :: izz    ! z-index of 2d-array to interpolate within
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      real, intent(in) :: xx , yy              ! The location to interpolate to
      real, intent(in) :: msgval
      real, intent(in), optional :: maskval
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
          intent(in) :: array 
      integer, dimension(:), intent(in) :: interp_list
      integer, dimension(:), intent(in) :: interp_opts
      integer, intent(in) :: idx
      real, dimension(start_x:end_x, start_y:end_y), &
          intent(in), optional :: mask_array 
      character (len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: sixteen_pt
  
      ! Local variables
      integer :: n , i , j , k , kk , l , ll
      real :: x , y , a , b , c , d , e , f , g , h
      real, dimension(4,4) :: stl
      logical :: is_masked

      is_masked = .false.

      if (int(xx) < start_x .or. int(xx) > end_x .or. &
          int(yy) < start_y .or. int(yy) > end_y) then
         sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                      start_z, end_z, msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
         return
      end if
  
      sixteen_pt = 0.0
      n = 0
      i = int(xx + 0.00001)
      j = int(yy + 0.00001)
      x = xx - i
      y = yy - j

      if ( ( abs(x) > 0.0001 ) .or. ( abs(y) > 0.0001 ) ) then
  
         loop_1 : do k = 1,4
            kk = i + k - 2
            if ( kk < start_x) then
               kk = start_x
            else if ( kk > end_x) then
               kk = end_x
            end if
            loop_2 : do l = 1,4
               stl(k,l) = 0.
               ll = j + l - 2
               if ( ll < start_y ) then
                  ll = start_y
               else if ( ll > end_y) then
                  ll = end_y
               end if
               stl(k,l) = array(kk,ll,izz)
               n = n + 1
               if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
                  if (mask_relational == '>' .and. mask_array(kk,ll) > maskval) then
                     is_masked = .true.
                  else if (mask_relational == '<' .and. mask_array(kk,ll) < maskval) then
                     is_masked = .true.
                  else if (mask_relational == ' ' .and. mask_array(kk,ll) == maskval) then
                     is_masked = .true.
                  end if
               else if (present(mask_array) .and. present(maskval)) then
                  if (mask_array(kk,ll) == maskval) is_masked = .true.
               end if
               if ( stl(k,l) == 0. .and. msgval /= 0.) then
                  stl(k,l) = 1.E-20
               end if
            end do loop_2
         end do loop_1
   
         ! If we have a missing value, try the next interpolation method in the sequence
         if (present(mask_array) .and. present(maskval)) then
            do k=1,4
               do l=1,4
                  if (stl(k,l) == msgval .or. is_masked) then
                     sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                                  start_z, end_z, msgval, interp_list, interp_opts, idx, &
                                                  mask_relational, maskval, mask_array)
                     return
                  end if
               end do
            end do
         else
            do k=1,4
               do l=1,4
                  if (stl(k,l) == msgval) then
                     sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, &
                                                  start_z, end_z, msgval, interp_list, interp_opts, idx)
                     return
                  end if
               end do
            end do
         end if
  
         a = oned(x,stl(1,1),stl(2,1),stl(3,1),stl(4,1))
         b = oned(x,stl(1,2),stl(2,2),stl(3,2),stl(4,2))
         c = oned(x,stl(1,3),stl(2,3),stl(3,3),stl(4,3))
         d = oned(x,stl(1,4),stl(2,4),stl(3,4),stl(4,4))
         sixteen_pt = oned(y,a,b,c,d)
   
         if (n /= 16) then
            e = oned(y,stl(1,1),stl(1,2),stl(1,3),stl(1,4))
            f = oned(y,stl(2,1),stl(2,2),stl(2,3),stl(2,4))
            g = oned(y,stl(3,1),stl(3,2),stl(3,3),stl(3,4))
            h = oned(y,stl(4,1),stl(4,2),stl(4,3),stl(4,4))
            sixteen_pt = (sixteen_pt+oned(x,e,f,g,h)) * 0.5
         end if
  
         if (sixteen_pt == 1.E-20) sixteen_pt = 0. 
   
      else
         if (present(mask_array) .and. present(maskval) .and. present(mask_relational)) then
            if (i >= start_x .and. i <= end_x .and. j >= start_y .and. j <= end_y .and. &
               mask_relational == '<' .and. mask_array(i,j) >= maskval .and. array(i,j,izz) /= msgval) then
               sixteen_pt = array(i,j,izz)
            else if (i >= start_x .and. i <= end_x .and. j >= start_y .and. j <= end_y .and. &
               mask_relational == '>' .and. mask_array(i,j) <= maskval .and. array(i,j,izz) /= msgval) then
               sixteen_pt = array(i,j,izz)
            else if (i >= start_x .and. i <= end_x .and. j >= start_y .and. j <= end_y .and. &
               mask_relational == ' ' .and. mask_array(i,j) /= maskval .and. array(i,j,izz) /= msgval) then
               sixteen_pt = array(i,j,izz)
            else
               sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                            msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
            end if
         else if (present(mask_array) .and. present(maskval)) then
            if (i >= start_x .and. i <= end_x .and. j >= start_y .and. j <= end_y .and. &
                mask_array(i,j) /= maskval .and. array(i,j,izz) /= msgval) then
               sixteen_pt = array(i,j,izz)
            else
               sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                            msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
            end if
         else
            if (i >= start_x .and. i <= end_x .and. j >= start_y .and. j <= end_y .and. array(i,j,izz) /= msgval) then
               sixteen_pt = array(i,j,izz)
            else
               sixteen_pt = interp_sequence(xx, yy, izz, array, start_x, end_x, start_y, end_y, start_z, end_z, &
                                            msgval, interp_list, interp_opts, idx, mask_relational, maskval, mask_array)
            end if
         end if
      end if
     
   end function sixteen_pt

 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: oned
   !
   ! Purpose: 1-dimensional overlapping parabolic interpolation
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function oned(x,a,b,c,d) 
   
      implicit none
   
      ! Arguments
      real, intent(in) :: x,a,b,c,d
 
      ! Return value 
      real :: oned
   
      oned = 0.                
   
      if ( x == 0. ) then
         oned = b      
      else if ( x == 1. ) then
         oned = c      
      end if
   
      if (b*c /= 0.) then
         if ( a*d == 0. ) then
            if ( ( a == 0 ) .and. ( d == 0 ) ) then
               oned = b*(1.0-x)+c*x                                        
            else if ( a /= 0. ) then
               oned = b+x*(0.5*(c-a)+x*(0.5*(c+a)-b))            
            else if ( d /= 0. ) then
               oned = c+(1.0-x)*(0.5*(b-d)+(1.0-x)*(0.5*(b+d)-c)) 
            end if
         else
            oned = (1.0-x)*(b+x*(0.5*(c-a)+x*(0.5*(c+a)-b)))+x*(c+(1.0-x)*(0.5*(b-d)+(1.0-x)*(0.5*(b+d)-c)))
         end if
      end if
    
   end function oned                                                       
 
end module interp_module
