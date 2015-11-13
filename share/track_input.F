SUBROUTINE track_input ( grid , ierr )

    USE module_domain
    USE module_utility

    IMPLICIT NONE

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"

    TYPE(domain), INTENT(INOUT) :: grid
    INTEGER, INTENT(INOUT) :: ierr

#if ( EM_CORE == 1 )

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor
    INTEGER, EXTERNAL :: get_unused_unit

    INTEGER :: istatus, iunit, istatus2
    LOGICAL :: exists
    CHARACTER (LEN=256) :: errmess

    ierr = 0

#ifndef NETCDF
    call wrf_message( 'calc_track_locations: requires netcdf' )
    call wrf_abort
#endif
#if ( DA_CORE != 1 )
    IF ( grid%dfi_opt == DFI_NODFI .OR. (grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage == DFI_SETUP) ) THEN
#endif

       IF ( grid%track_have_input .or. grid%track_loc_in <= 0 ) then
         RETURN
       ENDIF

       grid%track_loc = 0
master_proc : &
       IF ( wrf_dm_on_monitor() ) THEN
         INQUIRE(FILE='wrfinput_track.txt', EXIST=exists)
have_input_file : &
         IF (exists) THEN
           iunit = get_unused_unit()
           IF ( iunit <= 0 ) THEN
              CALL wrf_error_fatal('Error in track_input: could not find a free Fortran unit.')
           END IF

! Input track locations
           OPEN(UNIT=iunit, FILE='wrfinput_track.txt', FORM='formatted', STATUS='old', IOSTAT=istatus)
           IF (istatus == 0) THEN
! Read in track locations
             istatus2 = 0
             DO WHILE (istatus2 == 0)
               READ(UNIT=iunit, FMT='(A19,1X,F7.3,1X,F8.3)', IOSTAT=istatus2)        &
                      grid%track_time_in(grid%track_loc+1),                          &
                      grid%track_lat_in(grid%track_loc+1),                           &
                      grid%track_lon_in(grid%track_loc+1)

               if (istatus2 == 0 ) then
                 grid%track_loc = grid%track_loc + 1
               elseif (istatus2 > 0) then
                 WRITE(errmess, FMT='(I4)') grid%track_loc + 1   ! One extra for the header of the file
                 CALL wrf_message('Error in track_input.txt, line '//trim(errmess))
                 EXIT    ! (technically unecessary, as we will exit the loop anyway)
               endif

               IF ( grid%track_loc >= grid%track_loc_in ) THEN
                 IF ( istatus2 == 0 ) THEN                 ! Assume there were more lines in the file
                   WRITE(errmess, FMT='(A,I4,A)') 'Ignoring all track locations beyond #', &
                                                  grid%track_loc,'. Increase track_loc_in in namelist.input'
                   CALL wrf_message(trim(errmess))   
                 ENDIF
                 EXIT
               ENDIF
             END DO  ! istatus2 == 0
             CLOSE(iunit) 
           ENDIF   ! istatus == 0
         ELSE have_input_file
           CALL wrf_error_fatal('Error in track_input: could not find wrfinput_track.txt file.')           
         ENDIF have_input_file

         write(errmess,*) 'track_input: total input locations = ',grid%track_loc
         call wrf_message( trim(errmess) )
!         print *,'track_input: track_loc_in         = ',grid%track_loc_in
       ENDIF master_proc

#ifdef DM_PARALLEL
       CALL wrf_dm_bcast_integer(grid%track_loc, 1)
       CALL wrf_dm_bcast_real(grid%track_time_in, grid%track_loc)
       CALL wrf_dm_bcast_real(grid%track_lat_in, grid%track_loc)
       CALL wrf_dm_bcast_real(grid%track_lon_in, grid%track_loc)
#endif

    grid%track_have_input = .TRUE.

#if ( DA_CORE != 1 )
    END IF
#endif
#endif

END SUBROUTINE track_input
