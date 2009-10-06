      SUBROUTINE GLOB_ABORT(ie,s,rc)
#if defined( DM_PARALLEL ) 
      implicit none
      include 'mpif.h'
      integer rc,ie,ierr
      character*(*) s
      if (ie.ne.0) then
        print*,'GLOB_ABORT: '//s//' ie,rc:',ie,rc
        if (rc.eq.0) RETURN
        CALL MPI_ABORT(MPI_COMM_WORLD,rc,ierr)
      end if
#endif
      RETURN
      END
!
!***********************************************************************
!
