module module_linked_list2
    type linked_list
        integer :: id                      ! id
        character(len=256) :: name         ! name
        real, pointer :: value(:)          ! data
        type(linked_list), pointer :: next ! pointer to the next element
    end type linked_list

    type(linked_list), pointer :: linklist_head, linklist_tail

   INTERFACE push4backup
     MODULE PROCEDURE push_1_array,  push_2_arrays, push_3_arrays, push_4_arrays, push_5_arrays, &
                      push_6_arrays, push_7_arrays, push_8_arrays, push_9_arrays, push_10_arrays, &
                      push_2d_1_array,  push_2d_2_arrays, push_2d_3_arrays, push_2d_4_arrays, push_2d_5_arrays, &
                      push_2d_6_arrays, push_2d_7_arrays, push_2d_8_arrays, push_2d_9_arrays, push_2d_10_arrays, &
                      push_3d_1_array,  push_3d_2_arrays, push_3d_3_arrays, push_3d_4_arrays, push_3d_5_arrays, &
                      push_3d_6_arrays, push_3d_7_arrays, push_3d_8_arrays, push_3d_9_arrays, push_3d_10_arrays, &
                      push_4d_1_array,  push_4d_2_arrays, push_4d_3_arrays, push_4d_4_arrays, push_4d_5_arrays, &
                      push_4d_6_arrays, push_4d_7_arrays, push_4d_8_arrays, push_4d_9_arrays, push_4d_10_arrays
   END INTERFACE
              
   INTERFACE pop2restore
     MODULE PROCEDURE pop_1_array,  pop_2_arrays, pop_3_arrays, pop_4_arrays, pop_5_arrays, &
                      pop_6_arrays, pop_7_arrays, pop_8_arrays, pop_9_arrays, pop_10_arrays, &
                      pop_2d_1_array,  pop_2d_2_arrays, pop_2d_3_arrays, pop_2d_4_arrays, pop_2d_5_arrays, &
                      pop_2d_6_arrays, pop_2d_7_arrays, pop_2d_8_arrays, pop_2d_9_arrays, pop_2d_10_arrays, &
                      pop_3d_1_array,  pop_3d_2_arrays, pop_3d_3_arrays, pop_3d_4_arrays, pop_3d_5_arrays, &
                      pop_3d_6_arrays, pop_3d_7_arrays, pop_3d_8_arrays, pop_3d_9_arrays, pop_3d_10_arrays, &
                      pop_4d_1_array,  pop_4d_2_arrays, pop_4d_3_arrays, pop_4d_4_arrays, pop_4d_5_arrays, &
                      pop_4d_6_arrays, pop_4d_7_arrays, pop_4d_8_arrays, pop_4d_9_arrays, pop_4d_10_arrays
   END INTERFACE

   INTERFACE backup_array
     MODULE PROCEDURE backup_1d_array, backup_2d_array, backup_3d_array, backup_4d_array
   END INTERFACE

   INTERFACE restore_array
     MODULE PROCEDURE restore_1d_array, restore_2d_array, restore_3d_array, restore_4d_array
   END INTERFACE

contains

subroutine linkedlist_initialize

    implicit none

    type(linked_list), pointer :: current

    current => linklist_head
    do while (associated(current))
        linklist_head => current%next
        deallocate(current%value)
        deallocate(current)
        current => linklist_head
    enddo

    nullify(linklist_head)

    print *, "linkedlist_initialized."

end subroutine linkedlist_initialize

subroutine check_linkedlist

    implicit none

    type(linked_list), pointer :: current

    current => linklist_head
    do while (associated(current))
        write(unit=6, fmt='(a,i4,3a)') 'check id:', current%id, ', name: <', trim(current%name), '>'
        current => current%next
    enddo

end subroutine check_linkedlist

subroutine push_1_array(a1, varname)

    implicit none

    real, dimension(:), intent(in) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(length))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(:) = a1(:)

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(length))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(:) = a1(:)
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_1_array

subroutine pop_1_array(a1, varname)

    implicit none

    real, dimension(:), intent(out) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:length) = current%value(1:length)

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_1_array

subroutine push_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(2)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(2)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2_arrays

subroutine pop_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2_arrays

subroutine push_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(3)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(3)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3_arrays

subroutine pop_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3_arrays

subroutine push_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(4)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(4)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4_arrays

subroutine pop_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4_arrays

subroutine push_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(5)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(5)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_5_arrays

subroutine pop_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_5_arrays

subroutine push_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(6)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))
        linklist_head%value(num(5)+1:num(6)) = a6(1:len(6))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(6)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%value(num(5)+1:num(6)) = a6(1:len(6))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_6_arrays

subroutine pop_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))
    a6(1:len(6)) = current%value(num(5)+1:num(6))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_6_arrays

subroutine push_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(7)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))
        linklist_head%value(num(5)+1:num(6)) = a6(1:len(6))
        linklist_head%value(num(6)+1:num(7)) = a7(1:len(7))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(7)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%value(num(5)+1:num(6)) = a6(1:len(6))
        current%value(num(6)+1:num(7)) = a7(1:len(7))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_7_arrays

subroutine pop_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))
    a6(1:len(6)) = current%value(num(5)+1:num(6))
    a7(1:len(7)) = current%value(num(6)+1:num(7))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_7_arrays

subroutine push_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(8)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))
        linklist_head%value(num(5)+1:num(6)) = a6(1:len(6))
        linklist_head%value(num(6)+1:num(7)) = a7(1:len(7))
        linklist_head%value(num(7)+1:num(8)) = a8(1:len(8))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(8)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%value(num(5)+1:num(6)) = a6(1:len(6))
        current%value(num(6)+1:num(7)) = a7(1:len(7))
        current%value(num(7)+1:num(8)) = a8(1:len(8))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_8_arrays

subroutine pop_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))
    a6(1:len(6)) = current%value(num(5)+1:num(6))
    a7(1:len(7)) = current%value(num(6)+1:num(7))
    a8(1:len(8)) = current%value(num(7)+1:num(8))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_8_arrays

subroutine push_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(9)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))
        linklist_head%value(num(5)+1:num(6)) = a6(1:len(6))
        linklist_head%value(num(6)+1:num(7)) = a7(1:len(7))
        linklist_head%value(num(7)+1:num(8)) = a8(1:len(8))
        linklist_head%value(num(8)+1:num(9)) = a9(1:len(9))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(9)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%value(num(5)+1:num(6)) = a6(1:len(6))
        current%value(num(6)+1:num(7)) = a7(1:len(7))
        current%value(num(7)+1:num(8)) = a8(1:len(8))
        current%value(num(8)+1:num(9)) = a9(1:len(9))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_9_arrays

subroutine pop_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))
    a6(1:len(6)) = current%value(num(5)+1:num(6))
    a7(1:len(7)) = current%value(num(6)+1:num(7))
    a8(1:len(8)) = current%value(num(7)+1:num(8))
    a9(1:len(9)) = current%value(num(8)+1:num(9))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_9_arrays

subroutine push_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(10)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        linklist_head%value(num(0)+1:num(1)) = a1(1:len(1))
        linklist_head%value(num(1)+1:num(2)) = a2(1:len(2))
        linklist_head%value(num(2)+1:num(3)) = a3(1:len(3))
        linklist_head%value(num(3)+1:num(4)) = a4(1:len(4))
        linklist_head%value(num(4)+1:num(5)) = a5(1:len(5))
        linklist_head%value(num(5)+1:num(6)) = a6(1:len(6))
        linklist_head%value(num(6)+1:num(7)) = a7(1:len(7))
        linklist_head%value(num(7)+1:num(8)) = a8(1:len(8))
        linklist_head%value(num(8)+1:num(9)) = a9(1:len(9))
        linklist_head%value(num(9)+1:num(10)) = a10(1:len(10))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(10)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        current%value(num(0)+1:num(1)) = a1(1:len(1))
        current%value(num(1)+1:num(2)) = a2(1:len(2))
        current%value(num(2)+1:num(3)) = a3(1:len(3))
        current%value(num(3)+1:num(4)) = a4(1:len(4))
        current%value(num(4)+1:num(5)) = a5(1:len(5))
        current%value(num(5)+1:num(6)) = a6(1:len(6))
        current%value(num(6)+1:num(7)) = a7(1:len(7))
        current%value(num(7)+1:num(8)) = a8(1:len(8))
        current%value(num(8)+1:num(9)) = a9(1:len(9))
        current%value(num(9)+1:num(10)) = a10(1:len(10))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_10_arrays

subroutine pop_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    a1(1:len(1)) = current%value(num(0)+1:num(1))
    a2(1:len(2)) = current%value(num(1)+1:num(2))
    a3(1:len(3)) = current%value(num(2)+1:num(3))
    a4(1:len(4)) = current%value(num(3)+1:num(4))
    a5(1:len(5)) = current%value(num(4)+1:num(5))
    a6(1:len(6)) = current%value(num(5)+1:num(6))
    a7(1:len(7)) = current%value(num(6)+1:num(7))
    a8(1:len(8)) = current%value(num(7)+1:num(8))
    a9(1:len(9)) = current%value(num(8)+1:num(9))
    a10(1:len(10)) = current%value(num(9)+1:num(10))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_10_arrays

subroutine push_2d_1_array(a1, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(length))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value, a1, length)

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(length))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value, a1, length)
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_1_array

subroutine pop_2d_1_array(a1, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value, length)

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_1_array

subroutine push_2d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(2)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(2)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_2_arrays

subroutine pop_2d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_2_arrays

subroutine push_2d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(3)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(3)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_3_arrays

subroutine pop_2d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_3_arrays

subroutine push_2d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(4)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(4)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_4_arrays

subroutine pop_2d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_4_arrays

subroutine push_2d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(5)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(5)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_5_arrays

subroutine pop_2d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_5_arrays

subroutine push_2d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(6)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(6)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_6_arrays

subroutine pop_2d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_6_arrays

subroutine push_2d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(7)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(7)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_7_arrays

subroutine pop_2d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_7_arrays

subroutine push_2d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(8)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(8)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_8_arrays

subroutine pop_2d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_8_arrays

subroutine push_2d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(9)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(9)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_9_arrays

subroutine pop_2d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_9_arrays

subroutine push_2d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(10)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(linklist_head%value(num(9)+1:num(10)), a10, len(10))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(10)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(current%value(num(9)+1:num(10)), a10, len(10))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_2d_10_arrays

subroutine pop_2d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))
    call restore_array(a10, current%value(num(9)+1:num(10)), len(10))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_2d_10_arrays

subroutine push_3d_1_array(a1, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(length))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value, a1, length)

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(length))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value, a1, length)
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_1_array

subroutine pop_3d_1_array(a1, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value, length)

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_1_array

subroutine push_3d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(2)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(2)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_2_arrays

subroutine pop_3d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_2_arrays

subroutine push_3d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(3)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(3)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_3_arrays

subroutine pop_3d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_3_arrays

subroutine push_3d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(4)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(4)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_4_arrays

subroutine pop_3d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_4_arrays

subroutine push_3d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(5)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(5)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_5_arrays

subroutine pop_3d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_5_arrays

subroutine push_3d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(6)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(6)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_6_arrays

subroutine pop_3d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_6_arrays

subroutine push_3d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(7)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(7)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_7_arrays

subroutine pop_3d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_7_arrays

subroutine push_3d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(8)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(8)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_8_arrays

subroutine pop_3d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_8_arrays

subroutine push_3d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(9)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(9)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_9_arrays

subroutine pop_3d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_9_arrays

subroutine push_3d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(10)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(linklist_head%value(num(9)+1:num(10)), a10, len(10))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(10)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(current%value(num(9)+1:num(10)), a10, len(10))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_3d_10_arrays

subroutine pop_3d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))
    call restore_array(a10, current%value(num(9)+1:num(10)), len(10))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_3d_10_arrays

subroutine push_4d_1_array(a1, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(length))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value, a1, length)

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(length))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value, a1, length)
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_1_array

subroutine pop_4d_1_array(a1, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1
    character(len=*), intent(in) :: varname

    integer :: length

    type(linked_list), pointer :: current

    length = size(a1)

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value, length)

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_1_array

subroutine push_4d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(2)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(2)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_2_arrays

subroutine pop_4d_2_arrays(a1, a2, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(2) :: len
    integer, dimension(0:2) :: num

    len(1) = size(a1)
    len(2) = size(a2)

    num(0) = 0
    do n=1, 2
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_2_arrays

subroutine push_4d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(3)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(3)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_3_arrays

subroutine pop_4d_3_arrays(a1, a2, a3, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(3) :: len
    integer, dimension(0:3) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)

    num(0) = 0
    do n=1, 3
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_3_arrays

subroutine push_4d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(4)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(4)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_4_arrays

subroutine pop_4d_4_arrays(a1, a2, a3, a4, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(4) :: len
    integer, dimension(0:4) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)

    num(0) = 0
    do n=1, 4
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_4_arrays

subroutine push_4d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(5)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(5)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_5_arrays

subroutine pop_4d_5_arrays(a1, a2, a3, a4, a5, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(5) :: len
    integer, dimension(0:5) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)

    num(0) = 0
    do n=1, 5
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_5_arrays

subroutine push_4d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(6)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(6)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_6_arrays

subroutine pop_4d_6_arrays(a1, a2, a3, a4, a5, a6, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(6) :: len
    integer, dimension(0:6) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)

    num(0) = 0
    do n=1, 6
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_6_arrays

subroutine push_4d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(7)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(7)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_7_arrays

subroutine pop_4d_7_arrays(a1, a2, a3, a4, a5, a6, a7, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(7) :: len
    integer, dimension(0:7) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)

    num(0) = 0
    do n=1, 7
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_7_arrays

subroutine push_4d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(8)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(8)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_8_arrays

subroutine pop_4d_8_arrays(a1, a2, a3, a4, a5, a6, a7, a8, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(8) :: len
    integer, dimension(0:8) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)

    num(0) = 0
    do n=1, 8
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_8_arrays

subroutine push_4d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(9)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(9)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_9_arrays

subroutine pop_4d_9_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(9) :: len
    integer, dimension(0:9) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)

    num(0) = 0
    do n=1, 9
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_9_arrays

subroutine push_4d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:,:,:), intent(in) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    if(.not. associated(linklist_head)) then
        nullify(linklist_head)

        allocate(linklist_head)
        allocate(linklist_head%value(num(10)))
        nullify(linklist_head%next)

        linklist_head%id = 1
        linklist_head%name = varname
        call backup_array(linklist_head%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(linklist_head%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(linklist_head%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(linklist_head%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(linklist_head%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(linklist_head%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(linklist_head%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(linklist_head%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(linklist_head%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(linklist_head%value(num(9)+1:num(10)), a10, len(10))

        linklist_tail => linklist_head
    else
        allocate(current)
        allocate(current%value(num(10)))
        nullify(current%next)
        current%id = linklist_head%id + 1
        current%name = varname
        call backup_array(current%value(num(0)+1:num(1)), a1, len(1))
        call backup_array(current%value(num(1)+1:num(2)), a2, len(2))
        call backup_array(current%value(num(2)+1:num(3)), a3, len(3))
        call backup_array(current%value(num(3)+1:num(4)), a4, len(4))
        call backup_array(current%value(num(4)+1:num(5)), a5, len(5))
        call backup_array(current%value(num(5)+1:num(6)), a6, len(6))
        call backup_array(current%value(num(6)+1:num(7)), a7, len(7))
        call backup_array(current%value(num(7)+1:num(8)), a8, len(8))
        call backup_array(current%value(num(8)+1:num(9)), a9, len(9))
        call backup_array(current%value(num(9)+1:num(10)), a10, len(10))
        current%next => linklist_head
        linklist_head => current
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Push id:', current%id, ', name: <', trim(current%name), '>'

end subroutine push_4d_10_arrays

subroutine pop_4d_10_arrays(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, varname)

    implicit none

    real, dimension(:,:,:,:), intent(out) :: a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
    character(len=*), intent(in) :: varname

    type(linked_list), pointer :: current

    integer :: n
    integer, dimension(10) :: len
    integer, dimension(0:10) :: num

    len(1) = size(a1)
    len(2) = size(a2)
    len(3) = size(a3)
    len(4) = size(a4)
    len(5) = size(a5)
    len(6) = size(a6)
    len(7) = size(a7)
    len(8) = size(a8)
    len(9) = size(a9)
    len(10) = size(a10)

    num(0) = 0
    do n=1, 10
       num(n) = num(n-1) + len(n)
    enddo

    current => linklist_head

    if(trim(current%name) /= trim(varname)) then
       write(unit=6, fmt='(3a)') "Want var: <", trim(varname), ">"
       write(unit=6, fmt='(3a)') "But first var in list is: <", trim(current%name), ">"
       return
    endif

!   write(unit=*, fmt='(a,i4,3a)') &
!        'Pop  id:', current%id, ', name: <', trim(current%name), '>'

    call restore_array(a1, current%value(num(0)+1:num(1)), len(1))
    call restore_array(a2, current%value(num(1)+1:num(2)), len(2))
    call restore_array(a3, current%value(num(2)+1:num(3)), len(3))
    call restore_array(a4, current%value(num(3)+1:num(4)), len(4))
    call restore_array(a5, current%value(num(4)+1:num(5)), len(5))
    call restore_array(a6, current%value(num(5)+1:num(6)), len(6))
    call restore_array(a7, current%value(num(6)+1:num(7)), len(7))
    call restore_array(a8, current%value(num(7)+1:num(8)), len(8))
    call restore_array(a9, current%value(num(8)+1:num(9)), len(9))
    call restore_array(a10, current%value(num(9)+1:num(10)), len(10))

    current => current%next

    nullify(linklist_head%next)
    deallocate(linklist_head%value)
    deallocate(linklist_head)
    linklist_head => current

end subroutine pop_4d_10_arrays

subroutine backup_1d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:), intent(in ) :: vin
    real, dimension(:), intent(out) :: vout

    vout(1:len) = vin(1:len)
end subroutine backup_1d_array

subroutine backup_2d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:), intent(out) :: vout
    real, dimension(:,:),   intent(in ) :: vin
    integer :: n1, n2, i, j, n
    n1 = size(vin, dim=1)
    n2 = size(vin, dim=2)
    do j=1,n2
       n = (j-1)*n1
       do i=1, n1
          vout(n+i) = vin(i,j)
       enddo
    enddo
end subroutine backup_2d_array

subroutine backup_3d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:), intent(out) :: vout
    real, dimension(:,:,:),   intent(in ) :: vin
    integer :: n1, n2, n3, i, j, k, n
    n1 = size(vin, dim=1)
    n2 = size(vin, dim=2)
    n3 = size(vin, dim=3)
    do k=1,n3
    do j=1,n2
       n = n1*(j-1 + n2*(k-1))
       do i=1, n1
          vout(n+i) = vin(i,j,k)
       enddo
    enddo
    enddo
end subroutine backup_3d_array

subroutine backup_4d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:), intent(out) :: vout
    real, dimension(:,:,:,:),   intent(in ) :: vin
    integer :: n1, n2, n3, n4, i, j, k, m, n
    n1 = size(vin, dim=1)
    n2 = size(vin, dim=2)
    n3 = size(vin, dim=3)
    n4 = size(vin, dim=4)
    do m=1,n4
    do k=1,n3
    do j=1,n2
       n = n1*(j-1 + n2*(k-1 + n3*(m-1)))
       do i=1, n1
          vout(n+i) = vin(i,j,k,m)
       enddo
    enddo
    enddo
    enddo
end subroutine backup_4d_array

subroutine restore_1d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:), intent(in ) :: vin
    real, dimension(:), intent(out) :: vout

    vout(1:len) = vin(1:len)
end subroutine restore_1d_array

subroutine restore_2d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:,:), intent(out) :: vout
    real, dimension(:),   intent(in ) :: vin
    integer :: n1, n2, i, j, n
    n1 = size(vout, dim=1)
    n2 = size(vout, dim=2)
    do j=1,n2
       n = (j-1)*n1
       do i=1, n1
          vout(i,j) = vin(n+i)
       enddo
    enddo
end subroutine restore_2d_array

subroutine restore_3d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:,:,:), intent(out) :: vout
    real, dimension(:),   intent(in ) :: vin
    integer :: n1, n2, n3, i, j, k, n
    n1 = size(vout, dim=1)
    n2 = size(vout, dim=2)
    n3 = size(vout, dim=3)
    do k=1,n3
    do j=1,n2
       n = n1*(j-1 + n2*(k-1))
       do i=1, n1
          vout(i,j,k) = vin(n+i)
       enddo
    enddo
    enddo
end subroutine restore_3d_array

subroutine restore_4d_array(vout, vin, len)
    implicit none
    integer, intent(in) :: len
    real, dimension(:,:,:,:), intent(out) :: vout
    real, dimension(:),   intent(in ) :: vin
    integer :: n1, n2, n3, n4, i, j, k, m, n
    n1 = size(vout, dim=1)
    n2 = size(vout, dim=2)
    n3 = size(vout, dim=3)
    n4 = size(vout, dim=4)
    do m=1,n4
    do k=1,n3
    do j=1,n2
       n = n1*(j-1 + n2*(k-1 + n3*(m-1)))
       do i=1, n1
          vout(i,j,k,m) = vin(n+i)
       enddo
    enddo
    enddo
    enddo
end subroutine restore_4d_array

end module module_linked_list2

