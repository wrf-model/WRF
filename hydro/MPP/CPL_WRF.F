!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!   This is used as a coupler with the WRF model.
MODULE MODULE_CPL_LAND

    use mpi
    use, intrinsic :: iso_fortran_env, only: error_unit

  IMPLICIT NONE

  integer, public :: HYDRO_COMM_WORLD = MPI_COMM_NULL
  integer my_global_id
 
  integer total_pe_num
  integer global_ix,global_jx

  integer,allocatable,dimension(:,:) :: node_info

  logical initialized, cpl_land, time_step_read_rstart, &
           time_step_write_rstart, time_step_output
  character(len=19) cpl_outdate, cpl_rstdate

  integer, public :: cartGridComm
  integer, public :: np_up_down, np_left_right
  integer, public :: p_up_down, p_left_right

  contains

  ! sets incoming communicator and then calls CPL_LAND_INIT
  !subroutine CPL_LAND_INIT_COMM(istart,iend,jstart,jend,hydroCommunicator)
  !  implicit none
  !
  !  integer :: istart,iend,jstart,jend
  !  integer :: hydroCommunicator
  !
  !  HYDRO_COMM_WORLD = hydroCommunicator
  !  call CPL_LAND_INIT(istart,iend,jstart,jend)
  !end subroutine

  subroutine CPL_LAND_INIT(istart,iend,jstart,jend)
      implicit none
      integer ierr
      logical mpi_inited
      integer istart,iend,jstart,jend
      
      integer :: xx, ndim
      integer, dimension(0:1) :: dims, coords
      logical cyclic(0:1), reorder
      data cyclic/.false.,.false./  ! not cyclic
      data reorder/.false./   

      CALL mpi_initialized( mpi_inited, ierr )
      if ( .NOT. mpi_inited ) then
        call mpi_init(ierr)
        if (ierr /= MPI_SUCCESS) call fatal_error_stop("MPI Error: MPI_INIT failed")
      endif
      if (HYDRO_COMM_WORLD == MPI_COMM_NULL) then
        call MPI_COMM_DUP(MPI_COMM_WORLD, HYDRO_COMM_WORLD, ierr)
        if (ierr /= MPI_SUCCESS) call fatal_error_stop("MPI Error: MPI_COMM_DUP failed")
      endif

      call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_global_id, ierr )
      call MPI_COMM_SIZE( HYDRO_COMM_WORLD, total_pe_num, ierr )
      if (ierr /= MPI_SUCCESS) call fatal_error_stop("MPI Error: MPI_COMM_RANK and/or MPI_COMM_SIZE failed")

      allocate(node_info(9,total_pe_num))

      node_info = -99

! send node info to node 0
      node_info(1,my_global_id+1) = total_pe_num
      node_info(6,my_global_id+1) = istart
      node_info(7,my_global_id+1) = iend
      node_info(8,my_global_id+1) = jstart
      node_info(9,my_global_id+1) = jend


      call send_info()
      call find_left()
      call find_right()
      call find_up()
      call find_down()

      call send_info()

      ! initialize cartesian grid communicator
      dims(0) = 0
      dims(1) = 0
      do xx=1,total_pe_num
	if(node_info(2,xx) .eq. (-1)) then
	  dims(0) = dims(0)+1
	endif
	if(node_info(4,xx) .eq. (-1)) then
	  dims(1) = dims(1)+1
	endif
      enddo
      
      ndim = 2
      np_up_down = dims(0)
      np_left_right = dims(1)

      call MPI_Cart_create(HYDRO_COMM_WORLD, ndim, dims, &
                          cyclic, reorder, cartGridComm, ierr)
     
      call MPI_CART_GET(cartGridComm, 2, dims, cyclic, coords, ierr)
     
      p_up_down = coords(0)
      p_left_right = coords(1)

      initialized = .false.  ! land model need to be initialized. 
      return
  END subroutine CPL_LAND_INIT

     subroutine send_info()
        implicit none
        integer,allocatable,dimension(:,:) :: tmp_info
        integer  ierr, i,size, tag
        integer mpp_status(MPI_STATUS_SIZE)
        tag  = 9 
        size =  9

        if(my_global_id .eq. 0) then
           do i = 1, total_pe_num-1 
             call mpi_recv(node_info(:,i+1),size,MPI_INTEGER,  &
                i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
           enddo
        else
           call mpi_send(node_info(:,my_global_id+1),size,   &
               MPI_INTEGER,0,tag,HYDRO_COMM_WORLD,ierr)
        endif 

        call MPI_barrier( HYDRO_COMM_WORLD ,ierr)

        size = 9 * total_pe_num
        call mpi_bcast(node_info,size,MPI_INTEGER,   &
            0,HYDRO_COMM_WORLD,ierr)

        call MPI_barrier( HYDRO_COMM_WORLD ,ierr)

     return
     end  subroutine send_info

     subroutine find_left()
          implicit none
          integer i
          
          node_info(2,my_global_id+1) = -1

          do i = 1, total_pe_num 
               if( (node_info(8,i).eq.node_info(8,my_global_id+1)) .and. &
                   (node_info(9,i).eq.node_info(9,my_global_id+1)) .and. &
                   ((node_info(7,i)+1).eq.node_info(6,my_global_id+1)) ) then
                   node_info(2,my_global_id+1) = i - 1
                   return
               endif 
          end do
     return
     end subroutine find_left

     subroutine find_right()
          implicit none
          integer i
          
          node_info(3,my_global_id+1) = -1

          do i = 1, total_pe_num 
               if( (node_info(8,i).eq.node_info(8,my_global_id+1)) .and. &
                   (node_info(9,i).eq.node_info(9,my_global_id+1)) .and. &
                   ((node_info(6,i)-1).eq.node_info(7,my_global_id+1)) ) then
                   node_info(3,my_global_id+1) = i - 1
                   return
               endif 
          end do
     return
     end subroutine find_right

     subroutine find_up()
          implicit none
          integer i
          
          node_info(4,my_global_id+1) = -1

          do i = 1, total_pe_num 
               if( (node_info(6,i).eq.node_info(6,my_global_id+1)) .and. &
                   (node_info(7,i).eq.node_info(7,my_global_id+1)) .and. &
                   ((node_info(8,i)-1).eq.node_info(9,my_global_id+1)) ) then
                   node_info(4,my_global_id+1) = i - 1
                   return
               endif 
          end do
     return
     end subroutine find_up

     subroutine find_down()
          implicit none
          integer i
          
          node_info(5,my_global_id+1) = -1

          do i = 1, total_pe_num 
               if( (node_info(6,i).eq.node_info(6,my_global_id+1)) .and. &
                   (node_info(7,i).eq.node_info(7,my_global_id+1)) .and. &
                   ((node_info(9,i)+1).eq.node_info(8,my_global_id+1)) ) then
                   node_info(5,my_global_id+1) = i - 1
                   return
               endif 
          end do
     return
     end subroutine find_down

    ! stop the job due to the fatal error.
    subroutine fatal_error_stop(msg)
        character(len=*) :: msg
        integer :: ierr
        write(error_unit,*) "The job is stoped due to the fatal error. ", trim(msg)
        call flush(error_unit)
        CALL MPI_Abort(HYDRO_COMM_WORLD, 1, ierr)
        call MPI_Finalize(ierr)
        return
    end  subroutine fatal_error_stop
END MODULE MODULE_CPL_LAND
