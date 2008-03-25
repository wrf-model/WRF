!WRF:DRIVER_LAYER:NESTING
!


MODULE module_nesting

   USE module_machine
   USE module_driver_constants
   USE module_domain
   USE module_configure
   USE module_utility

   LOGICAL, DIMENSION( max_domains )              :: active_domain

CONTAINS

   LOGICAL FUNCTION nests_to_open ( parent , nestid_ret , kid_ret )
      IMPLICIT NONE
      TYPE(domain) , INTENT(IN)  :: parent
      INTEGER, INTENT(OUT)       :: nestid_ret , kid_ret
      ! Local data
      INTEGER                    :: parent_id
      INTEGER                    :: nestid, kid
      INTEGER                    :: s_yr,s_mm,s_dd,s_h,s_m,s_s,rc
      INTEGER                    :: e_yr,e_mm,e_dd,e_h,e_m,e_s
      INTEGER                    :: max_dom
      LOGICAL                    :: grid_allowed
      TYPE (WRFU_Time)           :: nest_start, nest_stop
!#define STUB_FOR_NOW
#ifndef STUB_FOR_NOW
      nestid_ret = 0
      kid_ret = 0
      nests_to_open = .false.
      CALL nl_get_max_dom( 1, max_dom )
#if (NMM_CORE == 1)
# if (NMM_NEST == 0)
      IF ( max_dom > 1 ) THEN
        CALL wrf_error_fatal( 'WRF-NMM compiled without nesting; set max_dom to 1 in namelist.input' )
      END IF
# endif
#endif
      DO nestid = 2, max_dom
        CALL nl_get_grid_allowed( nestid, grid_allowed )
        IF ( .NOT. active_domain( nestid ) .AND. grid_allowed ) THEN
          CALL nl_get_parent_id( nestid, parent_id )  ! from namelist
          IF ( parent_id .EQ. parent%id ) THEN
            CALL nl_get_start_year(nestid,s_yr)   ; CALL nl_get_end_year(nestid,e_yr)
            CALL nl_get_start_month(nestid,s_mm)  ; CALL nl_get_end_month(nestid,e_mm)
            CALL nl_get_start_day(nestid,s_dd)    ; CALL nl_get_end_day(nestid,e_dd)
            CALL nl_get_start_hour(nestid,s_h)    ; CALL nl_get_end_hour(nestid,e_h)
            CALL nl_get_start_minute(nestid,s_m)  ; CALL nl_get_end_minute(nestid,e_m)
            CALL nl_get_start_second(nestid,s_s)  ; CALL nl_get_end_second(nestid,e_s)
            CALL WRFU_TimeSet( nest_start,YY=s_yr,MM=s_mm,DD=s_dd,H=s_h,M=s_m,S=s_s,rc=rc)
            CALL WRFU_TimeSet( nest_stop,YY=e_yr,MM=e_mm,DD=e_dd,H=e_h,M=e_m,S=e_s,rc=rc)
            IF ( nest_start .LE. domain_get_current_time(head_grid) .AND. &
                 nest_stop  .GT. domain_get_current_time(head_grid) ) THEN
              DO kid = 1 , max_nests
                IF ( .NOT. ASSOCIATED ( parent%nests(kid)%ptr ) ) THEN
                  active_domain( nestid ) = .true.
                  nestid_ret = nestid
                  kid_ret = kid
                  nests_to_open = .TRUE.
                  RETURN
                END IF
              END DO
            END IF
          END IF
        END IF
      END DO
#else
      nestid_ret = 0
      kid_ret = 0
      nests_to_open = .FALSE.
#endif
      RETURN
   END FUNCTION nests_to_open

   ! Descend tree rooted at grid and set sibling pointers for
   ! grids that overlap.  We need some kind of global point space
   ! for working this out.

   SUBROUTINE set_overlaps ( grid )
      IMPLICIT NONE
      TYPE (domain), INTENT(INOUT)    :: grid
      ! stub
   END SUBROUTINE set_overlaps

   SUBROUTINE init_module_nesting
      active_domain = .FALSE.
   END SUBROUTINE init_module_nesting

END MODULE module_nesting

