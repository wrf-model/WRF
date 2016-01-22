      SUBROUTINE GLOB_ABORT(ie,s,rc)
#if defined( DM_PARALLEL ) 
      implicit none
!      include 'mpif.h'
      integer rc,ie,ierr
      character*(*) s
      character*500 message
      if (ie.ne.0) then
 3038    format("GLOB_ABORT: ",A," ie,rc:",I0,"  ",I0)
         write(message,3038) trim(s),ie,rc

         if(rc.eq.0) then
            call wrf_message(message)
         else
            call wrf_error_fatal(message)
         endif
!        write(0,*) 'GLOB_ABORT: '//s//' ie,rc:',ie,rc
!        if (rc.eq.0) RETURN
!        CALL MPI_ABORT(MPI_COMM_WORLD,rc,ierr)
      end if
#endif
      RETURN
      END
!
!***********************************************************************
!
