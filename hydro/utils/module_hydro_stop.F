module module_hydro_stop

implicit none

contains

! stop the job due to the fatal error.
      subroutine HYDRO_stop(msg)
#ifdef MPP_LAND
        use module_mpp_land
#endif
        character(len=*) :: msg
        integer :: ierr
        ierr = 1
#ifndef NCEP_WCOSS
!#ifdef HYDRO_D  !! PLEASE NEVER UNCOMMENT THIS IFDEF, it's just one incredibly useful string.
      write(6,*) "The job is stopped due to the fatal error. ", trim(msg)
      call flush(6)
!#endif
#else
     write(*,*) "FATAL ERROR: ", trim(msg)
     write(78,*) "FATAL ERROR: ", trim(msg)
      call flush(78)
      close(78)
#endif
#ifdef MPP_LAND
#ifndef HYDRO_D
      print*, "---"
      print*, "FATAL ERROR! Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
      print*, ""
#endif

!        call mpp_land_sync()
!        write(my_id+90,*) msg
!        call flush(my_id+90)

         call mpp_land_abort()
         call MPI_finalize(ierr)
#else
         stop "FATAL ERROR: Program stopped. Recompile with environment variable HYDRO_D set to 1 for enhanced debug information."
#endif

     return
     end  subroutine HYDRO_stop

end module
