module storage_module
  use gridinfo
  use module_debug
  implicit none
  private
  public :: get_storage
  public :: get_dims
  public :: get_plvls
  public :: put_storage
  public :: llstor_start
  public :: clear_storage
  public :: refr_storage
  public :: refw_storage
  public :: is_there
  public :: print_storage
  public :: setll
  public :: getll

  integer, parameter :: idlen = 8
  integer :: verbose = 0  ! 0 = no prints; 1 = some prints; 2 = more; etc.

  type node2
     character(len=idlen) :: id
     real, pointer, dimension(:,:) :: data2d
     type(mapinfo) :: data_map
     type(node2), pointer :: next
  end type node2

  type node1
     integer :: id
     type(node1), pointer :: next
     type(node2), pointer :: first
  end type node1

  type(node1), target :: root
  type(node1), pointer :: nnode
  type(node2), pointer :: current
  type(node2), pointer :: hold
  type(node1), pointer :: holdnn

  integer, public :: iferr

contains

  subroutine llstor_start(icode)
    implicit none
    integer, intent(in) :: icode
!
! First, check to see that the list ICODE has not already been started:
!
    nnode => root
    SEARCH : do while (associated(nnode%next))
       nnode => nnode%next
       if (nnode%id == icode) then
          if (verbose.gt.0) write(*,&
               '(/,"LLSTOR_START: NNODE EXISTS, not starting ", I8, /)') icode
          return
       endif
    enddo SEARCH
!
! Since it is a new ICODE, add it to the list of lists:
!
    allocate(nnode%next)
    nnode => nnode%next
    nnode%id = icode
    if (verbose.gt.0) write(*, '(/,"NNODE%ID = ", I8, /)') nnode%id
    allocate(nnode%first)
    nnode%first%id = 'Root'
    nullify(nnode%first%next)
    nullify (nnode%next)
  end subroutine llstor_start

  subroutine clear_storage
    implicit none

    if (verbose > 0) then
       print*, 'Call clear_storage.'
    endif

    SEARCH : do

       nnode => root
       SCANF : do while (associated(nnode%next))
          holdnn => nnode
          nnode => nnode%next
       enddo SCANF
       if (nnode%id == 0) exit SEARCH

       N2: do 
          current => nnode%first
          do while (associated(current%next))
             hold => current
             current => current%next
          enddo
          if (current%id /= "Root") then
             if (associated(current%data2d)) then
                if (verbose > 0) then
                   print*, 'Deallocating and nullifying 2d.', &
                        nnode%id, current%id
                endif
                deallocate(current%data2d)
                nullify(current%data2d)
             endif
          endif
          nullify(hold%next)
          if (current%id == nnode%first%id) then
             deallocate(current)
             nullify(current)
             exit N2
          endif
       enddo N2
       nullify(holdnn%next)

    enddo SEARCH

  end subroutine clear_storage

  subroutine find_node1(inname)
    implicit none
    integer :: inname, name
    name = inname
    nnode => root
    SEARCH : do while (associated(nnode%next))
       nnode => nnode%next
       if (nnode%id == name) then
          iferr = 0
          return
       endif
    enddo SEARCH
    if (verbose > 0) then
       print '("FIND_NODE1: Name not found:  ", I8)',  name
    endif
    iferr = 1
  end subroutine find_node1


  subroutine get_plvls(plvl, maxlvl, nlvl)
    implicit none
    integer :: maxlvl, nlvl
    real, dimension(maxlvl) :: plvl
    integer :: nn

    nnode => root
    nlvl = 0
    plvl = -99999
    SEARCH : do while (associated(nnode%next))
       nnode => nnode%next
       nlvl = nlvl + 1
       LEVLOOP : do nn = 1, nlvl
          if (nnode%id > plvl(nn)) then
             plvl(nn+1:maxlvl) = plvl(nn:maxlvl-1)
             plvl(nn) = float(nnode%id)
             exit LEVLOOP
          endif
       enddo LEVLOOP
    enddo SEARCH
  end subroutine get_plvls

  subroutine put_storage(icode, inname, data, idum, jdum)
    implicit none
    character(len=*) :: inname
    character(len=idlen) :: name
    integer :: idum, jdum
    integer :: icode
    real, dimension(:,:) :: data

    name = inname

    if (verbose>0) print*, 'Put Storage: '

    call find_node1(icode)
    if (iferr /= 0) then
       call llstor_start(icode)
    endif
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          current%data2d = data
          current%data_map = map
          if (verbose.gt.0) write(*,'("PUT_STORAGE: Overwriting ", A,&
            &" to ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
            data(1,1)
          return
       endif
    enddo SEARCH
    allocate(current%next)
    current => current%next
    current%id = name
    allocate(current%data2d(size(data,1),size(data,2)))
    current%data2d = data
    current%data_map = map
    nullify (current%next)
    if (verbose.gt.0) write(*,'("PUT_STORAGE: Writing ", A,&
         &" to ID ", I8, "   Value: ", F16.6)') current%id, nnode%id, data(1,1)

  end subroutine put_storage

  subroutine refw_storage(icode, name, Pdata, idum, jdum)
    implicit none
    character(len=*) :: name
    integer :: icode
    integer :: idum, jdum
    real, pointer, dimension(:,:) :: Pdata

    call find_node1(icode)
    if (iferr /= 0) then
       call llstor_start(icode)
    endif
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          if (associated(current%data2d)) then
             deallocate(current%data2d)
             nullify(current%data2d)
          endif
          current%data2d => Pdata
          current%data_map = map
          if (verbose.gt.0) write(*,'("REFW_STORAGE: OverWriting ", A,&
               &" to ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
               current%data2d(1,1)
          return
       endif
    enddo SEARCH
    allocate(current%next)
    current => current%next
    current%id = name
    nullify(current%data2d)
    current%data2d => Pdata
    current%data_map = map
    nullify(current%next)

    if (verbose.gt.0) write(*,'("REFW_STORAGE: Writing ", A,&
         &" to ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
         current%data2d(1,1)

  end subroutine refw_storage

  subroutine get_storage(icode, name, data, idum, jdum)
    implicit none
    character(len=*) :: name
    integer :: icode
    integer :: idum, jdum
    real, dimension(:,:) :: data

    call find_node1(icode)
    if (iferr /= 0) then
       print*, 'Cannot find code ', icode, ' in routine GET_STORAGE.'
       stop 'GET_STORAGE_code'
    endif
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          data = current%data2d
          map = current%data_map
          if (verbose.gt.0) write(*,'("GET_STORAGE: READING ", A,&
              &" at ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
              & data(1,1)
          return
       endif
    enddo SEARCH
    write(*,'("GET_STORAGE : NAME not found: ", A)') name

  end subroutine get_storage

  subroutine refr_storage(icode, name, Pdata, idum, jdum)
    implicit none
    character(len=*) :: name
    integer :: icode
    integer :: idum, jdum
    real, pointer, dimension(:,:) :: Pdata

    call find_node1(icode)
    if (iferr /= 0) then
       print*, 'Cannot find code ', icode, ' in routine REFR_STORAGE.'
       STOP 'REFR_STORAGE_code'
    endif
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          Pdata => current%data2d
          map = current%data_map
          if (verbose.gt.0) write(*,'("REFR_STORAGE: Referencing ", A,&
         &" at ID ", I8, "   Value: ", F16.6)') current%id, nnode%id,&
         Pdata(1,1)
          return
       endif
    enddo SEARCH
    print '("REFR_STORAGE : NAME not found: ", A)', name

  end subroutine refr_storage

  subroutine llstor_remove(icode, name)
    implicit none
    character(len=*) :: name
    integer :: icode

    call find_node1(icode)
    if (iferr /= 0) then
       STOP 'find_node1'
    endif
    current => nnode%first

    do while (current%id /= name )
       if (.not. associated(current%next)) then
          print*, 'Not there : ', name
          return
       endif
       hold => current
       current => current%next
    enddo

    if (associated(current%data2d)) then
       deallocate(current%data2d)
    endif
    nullify(hold%next)
    hold%next => current%next
    nullify(current%next)
    nullify(current)
    
  end subroutine llstor_remove

  subroutine get_dims(icode, name)
    implicit none
    character(len=*) :: name
    integer :: icode

    call find_node1(icode)
    if (iferr /= 0) then
       STOP 'get_dims'
    end if
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          map = current%data_map
          return
       endif
    enddo SEARCH

  end subroutine get_dims

  subroutine print_storage(icode)
    implicit none
    integer :: isz
    integer, optional :: icode

    if (present(icode)) then
       call find_node1(icode)
       if (iferr /= 0) then
          STOP 'print_storage'
       end if
!      print '("PRINT_NODE1: id = ", I8)' , nnode%id
       call mprintf(.true.,DEBUG,"PRINT_NODE1: id = %i ",i1=nnode%id)
       current => nnode%first

!      print*
       call mprintf(.true.,DEBUG,' ',newline=.true.)
       if (.not. associated(current)) then
!         print '("Nothing there.")'
          call mprintf(.true.,DEBUG,"Nothing there. ")
          return
       endif
       do while ( associated(current%next))
          if (current%id == 'Root') then
!            print*, 'id = ', current%id
             call mprintf(.true.,DEBUG," id = %s ",s1=current%id)
          elseif (current%id /= 'Root') then

             if (associated(current%data2d)) then
                isz = size(current%data2d)
!               print*, current%id, ' = ', current%data2d(1,1)
                call mprintf(.true.,DEBUG," %s = %f ",s1=current%id,f1=current%data2d(1,1))
             endif
                
          endif
          current => current%next
       enddo
       if (current%id == 'Root') then
!         print*, 'id = ', current%id
          call mprintf(.true.,DEBUG," id = %s ",s1=current%id)
       elseif (current%id /= 'Root') then
          if (associated(current%data2d)) then
             isz = size(current%data2d)
!            print*, current%id, ' = ', current%data2d(1,1)
	     call mprintf(.true.,DEBUG," %s = %f ",s1=current%id,f1=current%data2d(1,1))
          endif
       endif
       current => current%next
!      print*
       call mprintf(.true.,DEBUG,' ',newline=.true.)

    else
       nnode => root
       do while (associated(nnode%next))
          nnode => nnode%next
!         print '("PRINT_NODE1: id = ", I8)' , nnode%id
          call mprintf(.true.,DEBUG,"PRINT_NODE1: id = %i ",i1=nnode%id)

          current => nnode%first

!         print*
          call mprintf(.true.,DEBUG,' ',newline=.true.)
          if (.not. associated(current)) then
!            print '("Nothing there.")'
             call mprintf(.true.,DEBUG,"Nothing there. ")
             return
          endif
          do while ( associated(current%next))
             if (current%id == 'Root') then
!               print*, 'id = ', current%id
                call mprintf(.true.,DEBUG," id = %s ",s1=current%id)
             elseif (current%id /= 'Root') then
                if (associated(current%data2d)) then
                   isz = size(current%data2d)
!                  print*, current%id, ' = ', current%data2d(1,1), isz
                   call mprintf(.true.,DEBUG," %s = %f  isz = %i", &
	             s1=current%id,f1=current%data2d(1,1),i1=isz)
                endif
             endif
             current => current%next
          enddo
          if (current%id == 'Root') then
!            print*, 'id = ', current%id
	     call mprintf(.true.,DEBUG," id = %s ",s1=current%id)
          elseif (current%id /= 'Root') then
             if (associated(current%data2d)) then
                isz = size(current%data2d)
!               print*, current%id, ' = ', current%data2d(1,1), isz
                call mprintf(.true.,DEBUG," %s = %f  isz = %i", &
	          s1=current%id,f1=current%data2d(1,1),i1=isz)
             endif
          endif
          current => current%next
!         print*
          call mprintf(.true.,DEBUG,' ',newline=.true.)

       enddo
    endif
  end subroutine print_storage

  logical function is_there(icode, name) RESULT(answer)
    implicit none
    character(len=*) :: name
    integer :: icode

    answer = .FALSE.

    if (verbose > 0) then
       write(*,'("Is there ",A," at ", i8, "?")', advance="NO") name, icode
    endif

    call find_node1(icode)
    if (iferr /= 0) go to 1000
    
    current => nnode%first
    
    SEARCH : do while (associated(current%next))
       current => current%next
       if (current%id == name) then
          answer = .TRUE.
          exit SEARCH
       endif
    enddo SEARCH

1000 continue

    if (verbose > 0) then
       write(*,*) answer
    endif


  end function is_there

  subroutine setll(ivrb)
    implicit none
    integer, optional :: ivrb
    if (present(ivrb)) verbose = ivrb
  end subroutine setll

  subroutine getll(ivrb)
    implicit none
    integer, optional :: ivrb
    if (present(ivrb)) ivrb = verbose
  end subroutine getll

end module storage_module

