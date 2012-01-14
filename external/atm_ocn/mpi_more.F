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

#ifdef PRETEND_WRF_FOR_DMITRY
! For Dmitry's sake for testing this code outside of WRF, these are
! implementations of the WRF_MESSAGE and WRF_ERROR_FATAL functions.

      subroutine WRF_MESSAGE(s)
      character(*) :: s
      print *,trim(s)
      end subroutine WRF_MESSAGE

      subroutine WRF_ERROR_FATAL(s)
      include 'mpif.h'
      character(*) :: s
      print *,trim(s)
      write(0,*) trim(s)
      CALL MPI_ABORT(MPI_COMM_WORLD,rc,ierr)
      end subroutine WRF_ERROR_FATAL
#endif
!
!***********************************************************************
!
