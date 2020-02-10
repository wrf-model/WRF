SUBROUTINE wrf_tsin ( grid , ierr )

    USE module_domain
    USE module_utility
    USE module_configure, ONLY : model_config_rec, grid_config_rec_type, model_to_grid_config_rec
    USE module_string_tools, ONLY : capitalize

    IMPLICIT NONE

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"

    TYPE(domain), INTENT(INOUT) :: grid
    TYPE (grid_config_rec_type) :: config_flags
    INTEGER, INTENT(INOUT) :: ierr

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor
    INTEGER, EXTERNAL :: get_unused_unit

    INTEGER :: istatus, iunit
    LOGICAL :: exists
    CHARACTER (LEN=256) :: errmess
    CHARACTER (LEN=256) :: IorLat,JorLon

    ierr = 0

#if ((EM_CORE == 1) && (DA_CORE != 1))
    IF ( grid%dfi_opt == DFI_NODFI .OR. (grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage == DFI_SETUP) ) THEN
#endif
    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags ) 
       grid%ntsloc = 0
       grid%have_calculated_tslocs = .FALSE.
   
       IF ( grid%max_ts_locs <= 0 ) RETURN
   
       IF ( wrf_dm_on_monitor() ) THEN

          CALL wrf_tsin_exist ( exists )
   
          IF (exists) THEN

             iunit = get_unused_unit()
             IF ( iunit <= 0 ) THEN
                CALL wrf_error_fatal('Error in wrf_tsin: could not find a free Fortran unit.')
             END IF

             ! Input time series locations
             OPEN(UNIT=iunit, FILE='tslist', FORM='formatted', STATUS='old', IOSTAT=istatus)
             IF (istatus == 0) THEN
   
                ! The header will be parsed to see if lat/lon or i/j is given...
                READ(UNIT=iunit, FMT='(1X)')
                READ(UNIT=iunit, FMT='(32X,A7,2X,A7)') IorLat, JorLon
                READ(UNIT=iunit, FMT='(1X)')
                grid%tslist_ij = .false.
                ! If the header contains "LAT" and "LON", 
                !    then it will expect lat/lon coords in the file
                ! If the header has "I" and "J", 
                !    then it will expect grid index coords in the file
                IorLat = capitalize(IorLat)
                JorLon = capitalize(JorLon)
                if(index(IorLat,'LAT').ne.0 .and. index(JorLon,'LON').ne.0) then
                    grid%tslist_ij = .false.
                elseif(index(IorLat,'I').ne.0 .and. index(JorLon,'J').ne.0) then
                    grid%tslist_ij = .true.
                else
                    CALL wrf_error_fatal('Error in wrf_tsin: Header line requires either LAT LON or I J')
                endif



   
                ! Read in time series locations
                istatus = 0
                DO WHILE (istatus == 0)
                   IF (config_flags%map_proj == 0 .OR. grid%tslist_ij) THEN !ideal run will only use i,j
                       READ(UNIT=iunit, FMT='(A25,1X,A5,1X,I7,1X,I8)', IOSTAT=istatus)            &
                       grid%desctsloc(grid%ntsloc+1), grid%nametsloc(grid%ntsloc+1), &
                       grid%itsloc(grid%ntsloc+1), grid%jtsloc(grid%ntsloc+1)
                   ELSE
                       READ(UNIT=iunit, FMT='(A25,1X,A5,1X,F7.3,1X,F8.3)', IOSTAT=istatus)            &
                       grid%desctsloc(grid%ntsloc+1), grid%nametsloc(grid%ntsloc+1), &
                       grid%lattsloc(grid%ntsloc+1), grid%lontsloc(grid%ntsloc+1)
                   END IF
                   IF (istatus == 0) grid%ntsloc = grid%ntsloc + 1
                   IF (istatus > 0) THEN
                      WRITE(errmess, FMT='(I4)') grid%ntsloc + 4   ! Three extra for the header of the file
                                                                   ! One extra for the line we incorrectly read
                      CALL wrf_message('Error in tslist, line '//trim(errmess))
                      CALL wrf_error_fatal('Error --- Maybe check that the header (I,J) vs (LAT,LON) matches the provided information')
                      EXIT    ! (technically unecessary, as we will exit the loop anyway)
                   END IF
                   IF ( ( grid%ntsloc == 1 ) .and. ( .NOT. grid%tslist_ij ) ) THEN
                      IF ( ( ABS(grid%lattsloc(grid%ntsloc)) <= 0.1 ) .and. &
                           ( ABS(grid%lontsloc(grid%ntsloc)) <= 0.1 ) ) THEN
                         CALL wrf_message('WARNING ')
                         CALL wrf_message('WARNING --- Maybe you have (I,J) locations for the (LAT,LON) values in tslist?')
                         CALL wrf_message('WARNING --- Are you purposefully studying Null Island?')
                         CALL wrf_message('WARNING ')
                      END IF
                   END IF
                   IF ( grid%ntsloc >= grid%max_ts_locs ) THEN
                      IF ( istatus == 0 ) THEN                 ! Assume there were more lines in the file
                         WRITE(errmess, FMT='(A,I4,A)') 'Ignoring all time series locations beyond #', &
                                                    grid%ntsloc,'. Increase max_ts_locs in namelist.input'
                         CALL wrf_message(trim(errmess))
                      END IF
                      EXIT
                   END IF
                END DO
      
                CLOSE(iunit)
   
             END IF

          END IF  ! tslist file exists
   
       END IF

#ifdef DM_PARALLEL
       CALL wrf_dm_bcast_integer(grid%ntsloc, 1)
       CALL wrf_dm_bcast_integer(grid%tslist_ij, 1)
       CALL wrf_dm_bcast_real(grid%lattsloc, grid%max_ts_locs)
       CALL wrf_dm_bcast_real(grid%lontsloc, grid%max_ts_locs)
       CALL wrf_dm_bcast_integer(grid%itsloc, grid%max_ts_locs)
       CALL wrf_dm_bcast_integer(grid%jtsloc, grid%max_ts_locs)
#endif
#if ((EM_CORE == 1) && (DA_CORE != 1))
    END IF
#endif

END SUBROUTINE wrf_tsin

SUBROUTINE wrf_tsin_exist ( exists )
   IMPLICIT NONE
   LOGICAL , INTENT(OUT) :: exists
   INQUIRE(FILE='tslist', EXIST=exists)
END SUBROUTINE wrf_tsin_exist


INTEGER FUNCTION get_unused_unit()

    IMPLICIT NONE

    INTEGER, PARAMETER :: min_unit_number = 30
    INTEGER, PARAMETER :: max_unit_number = 99 

    LOGICAL :: opened

    DO get_unused_unit = min_unit_number, max_unit_number
       INQUIRE(UNIT=get_unused_unit, OPENED=opened)
       IF ( .NOT. opened ) RETURN
    END DO

    get_unused_unit = -1

    RETURN
    
END FUNCTION get_unused_unit
