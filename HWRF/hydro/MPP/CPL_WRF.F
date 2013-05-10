!   This is used as a coupler with the WRF model.
MODULE MODULE_CPL_LAND


  IMPLICIT NONE

  integer my_global_id
 
  integer total_pe_num
  integer global_ix,global_jx

  integer,allocatable,dimension(:,:) :: node_info

  logical initialized, cpl_land, time_step_read_rstart, &
           time_step_write_rstart, time_step_output
  character(len=19) cpl_outdate, cpl_rstdate



  contains

  subroutine CPL_LAND_INIT(istart,iend,jstart,jend)
      implicit none
   include "mpif.h"
      integer  ierr
      logical mpi_inited
      integer istart,iend,jstart,jend 

      CALL mpi_initialized( mpi_inited, ierr )
      if ( .NOT. mpi_inited ) then
          call mpi_init(ierr)
      endif

      call MPI_COMM_RANK( MPI_COMM_WORLD, my_global_id, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, total_pe_num, ierr )

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

      initialized = .false.  ! land model need to be initialized. 
      return
  END subroutine CPL_LAND_INIT

     subroutine send_info()
        implicit none
   include "mpif.h"
        integer,allocatable,dimension(:,:) :: tmp_info
        integer  ierr, i,size, tag
        integer mpp_status(MPI_STATUS_SIZE)
        tag  = 9 
        size =  9

        if(my_global_id .eq. 0) then
           do i = 1, total_pe_num-1 
             call mpi_recv(node_info(:,i+1),size,MPI_INTEGER,  &
                i,tag,MPI_COMM_WORLD,mpp_status,ierr) 
           enddo
        else
           call mpi_send(node_info(:,my_global_id+1),size,   &
               MPI_INTEGER,0,tag,MPI_COMM_WORLD,ierr) 
        endif 

        call MPI_barrier( MPI_COMM_WORLD ,ierr)

        size = 9 * total_pe_num
        call mpi_bcast(node_info,size,MPI_INTEGER,   &
            0,MPI_COMM_WORLD,ierr)

        call MPI_barrier( MPI_COMM_WORLD ,ierr)

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

END MODULE MODULE_CPL_LAND
