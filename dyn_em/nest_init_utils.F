SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)  :: parent , nest

   INTEGER iswater , map_proj, julyr, julday
   REAL    cen_lat, cen_lon, truelat1 , truelat2 , gmt , moad_cen_lat , stand_lon
   CHARACTER (LEN=4) :: char_junk

! single-value constants

   nest%p_top   = parent%p_top
   nest%cfn     = parent%cfn
   nest%cfn1    = parent%cfn1
   nest%epsts   = parent%epsts
   nest%rdx     = 1./nest%dx
   nest%rdy     = 1./nest%dy
!  nest%dts     = nest%dt/float(nest%time_step_sound)
   nest%dtseps  = parent%dtseps  ! used in height model only?
   nest%resm    = parent%resm    ! used in height model only?
   nest%zetatop = parent%zetatop ! used in height model only?
   nest%cf1     = parent%cf1
   nest%cf2     = parent%cf2
   nest%cf3     = parent%cf3
   nest%gmt     = parent%gmt
   nest%julyr   = parent%julyr
   nest%julday  = parent%julday

   CALL nl_get_mminlu ( 1,char_junk(1:4) )
   CALL nl_get_iswater (1, iswater )
   CALL nl_get_truelat1 ( 1 , truelat1 )
   CALL nl_get_truelat2 ( 1 , truelat2 )
   CALL nl_get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL nl_get_stand_lon ( 1 , stand_lon )
   CALL nl_get_map_proj ( 1 , map_proj )
   CALL nl_get_gmt ( 1 , gmt)
   CALL nl_get_julyr ( 1 , julyr)
   CALL nl_get_julday ( 1 , julday)
   IF ( nest%id .NE. 1 ) THEN
     CALL nl_set_gmt (nest%id, gmt)
     CALL nl_set_julyr (nest%id, julyr)
     CALL nl_set_julday (nest%id, julday)
     CALL nl_set_iswater (nest%id, iswater )
     CALL nl_set_truelat1 ( nest%id , truelat1 )
     CALL nl_set_truelat2 ( nest%id , truelat2 )
     CALL nl_set_moad_cen_lat ( nest%id , moad_cen_lat )
     CALL nl_set_stand_lon ( nest%id , stand_lon )
     CALL nl_set_map_proj ( nest%id , map_proj )
   END IF
   nest%gmt     = gmt 
   nest%julday  = julday
   nest%julyr   = julyr
   nest%iswater = iswater
   nest%cen_lat = cen_lat
   nest%cen_lon = cen_lon
   nest%truelat1= truelat1
   nest%truelat2= truelat2
   nest%moad_cen_lat= moad_cen_lat
   nest%stand_lon= stand_lon
   nest%map_proj= map_proj

   nest%step_number  = parent%step_number

! 1D constants (Z)

   nest%em_fnm    = parent%em_fnm
   nest%em_fnp    = parent%em_fnp
   nest%em_rdnw   = parent%em_rdnw
   nest%em_rdn    = parent%em_rdn
   nest%em_dnw    = parent%em_dnw
   nest%em_dn     = parent%em_dn
   nest%em_znu    = parent%em_znu
   nest%em_znw    = parent%em_znw
   nest%em_t_base = parent%em_t_base
   nest%u_base    = parent%u_base
   nest%v_base    = parent%v_base
   nest%qv_base   = parent%qv_base
   nest%z_base    = parent%z_base
   nest%dzs       = parent%dzs
   nest%zs        = parent%zs

END SUBROUTINE init_domain_constants_em

SUBROUTINE blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , & 
                           ims , ime , jms , jme , kms , kme , & 
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_configure
   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , & 
                                                 ims , ime , jms , jme , kms , kme , & 
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)    :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: ter_input

   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: ter_temp
   INTEGER :: i , j , k , spec_zone

   !  The fine grid elevation comes from the horizontally intterpolated
   !  parent elevation for the first spec_zone row/columns, so we need
   !  to get that value.

   CALL nl_get_spec_zone ( 1, spec_zone) 

   !  Initialize temp values to the nest ter elevation.  This fills in the values
   !  that will not be modified below.  

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_temp(i,k,j) = ter_input(i,k,j)
         END DO 
      END DO 
   END DO 

   !  To avoid some tricky indexing, we fill in the values inside out.  This allows
   !  us to overwrite incorrect assignments.  There are replicated assignments, and
   !  there is much unnecessary "IF test inside of a loop" stuff.  For a large
   !  domain, this is only a patch; for a small domain, this is not a biggy.

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
   
            !  3 points out of the spec zone, 25% parent, 75% nest
   
            IF      ( ( i .EQ.       spec_zone + 3 ) .OR.  ( j .EQ.       spec_zone + 3 ) .OR. &
                      ( i .EQ. ide - spec_zone - 3 ) .OR.  ( j .EQ. jde - spec_zone - 3 ) ) THEN
               ter_temp(i,k,j) = (    ter_interpolated(i,k,j) + 3.*ter_input(i,k,j) ) *0.25
            END IF
   
            !  2 points out of the spec zone, 50% parent, 50% nest
   
            IF      ( ( i .EQ.       spec_zone + 2 ) .OR.  ( j .EQ.       spec_zone + 2 ) .OR. &
                      ( i .EQ. ide - spec_zone - 2 ) .OR.  ( j .EQ. jde - spec_zone - 2 ) ) THEN
               ter_temp(i,k,j) = ( 2.*ter_interpolated(i,k,j) + 2.*ter_input(i,k,j) ) *0.25
            END IF
   
            !  1 point out of the spec zone, 75% parent, 25% nest
   
            IF      ( ( i .EQ.       spec_zone + 1 ) .OR.  ( j .EQ.       spec_zone + 1 ) .OR. &
                      ( i .EQ. ide - spec_zone - 1 ) .OR.  ( j .EQ. jde - spec_zone - 1 ) ) THEN
               ter_temp(i,k,j) = ( 3.*ter_interpolated(i,k,j) +    ter_input(i,k,j) ) *0.25
            END IF
   
            !  In the spec_zone: 100% of the terrain interpolated from the parent
   
            IF      ( ( i .LE.       spec_zone     ) .OR.  ( j .LE.       spec_zone     ) .OR. &
                      ( i .GE. ide - spec_zone     ) .OR.  ( j .GE. jde - spec_zone     ) ) THEN
               ter_temp(i,k,j) =      ter_interpolated(i,k,j)
            END IF
   
         END DO 
      END DO 
   END DO 

   !  Set nest elevation with temp values.  All values not overwritten in the above
   !  100%, 75%, 50%, 25% loops have been previously set in the initial assignment.

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_input(i,k,j) = ter_temp(i,k,j)
         END DO 
      END DO 
   END DO 

END SUBROUTINE blend_terrain

SUBROUTINE store_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , & 
                           ims , ime , jms , jme , kms , kme , & 
                           ips , ipe , jps , jpe , kps , kpe )

   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , & 
                                                 ims , ime , jms , jme , kms , kme , & 
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT) :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)  :: ter_input

   INTEGER :: i , j , k

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_interpolated(i,k,j) = ter_input(i,k,j)
         END DO 
      END DO 
   END DO 

END SUBROUTINE store_terrain


SUBROUTINE input_terrain_rsmas ( grid ,                        &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_domain
   IMPLICIT NONE
   TYPE ( domain ) :: grid

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe

   LOGICAL, EXTERNAL ::  wrf_dm_on_monitor

   INTEGER :: i , j , k , myproc
   INTEGER, DIMENSION(256) :: ipath  ! array for integer coded ascii for passing path down to get_terrain
   CHARACTER*256 :: message, message2
   CHARACTER*256 :: rsmas_data_path

#if DM_PARALLEL
! Local globally sized arrays
   REAL , DIMENSION(ids:ide,jds:jde) :: ht_g, xlat_g, xlon_g
#endif

   CALL wrf_get_myproc ( myproc ) 

#if 0
CALL wrf_timetoa ( grid%current_time, message2 )
WRITE ( message , FMT = '(A," HT before ",I3)' ) TRIM(message2), grid%id
write(30+myproc,*)ipe-ips+1,jpe-jps+1,trim(message)
do j = jps,jpe
do i = ips,ipe
write(30+myproc,*)grid%ht(i,j)
enddo
enddo
#endif

   CALL nl_get_rsmas_data_path(1,rsmas_data_path)
   do i = 1, LEN(TRIM(rsmas_data_path))
      ipath(i) = ICHAR(rsmas_data_path(i:i))
   enddo

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )

   CALL wrf_patch_to_global_real ( grid%xlat , xlat_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 
   CALL wrf_patch_to_global_real ( grid%xlong , xlon_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 

   IF ( wrf_dm_on_monitor() ) THEN
     CALL get_terrain ( grid%dx/1000., xlat_g(ids:ide,jds:jde), xlon_g(ids:ide,jds:jde), ht_g(ids:ide,jds:jde), &
                        ide-ids+1,jde-jds+1,ide-ids+1,jde-jds+1, ipath, LEN(TRIM(rsmas_data_path)) )
     WHERE ( ht_g(ids:ide,jds:jde) < -1000. ) ht_g(ids:ide,jds:jde) = 0.
   ENDIF

   CALL wrf_global_to_patch_real ( ht_g , grid%ht , grid%domdesc, ' ' , 'xy' ,         &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 
#else

   CALL get_terrain ( grid%dx/1000., grid%xlat(ids:ide,jds:jde), grid%xlong(ids:ide,jds:jde), grid%ht(ids:ide,jds:jde), &
                       ide-ids+1,jde-jds+1,ide-ids+1,jde-jds+1, ipath, LEN(TRIM(rsmas_data_path)) )
   WHERE ( grid%ht(ids:ide,jds:jde) < -1000. ) grid%ht(ids:ide,jds:jde) = 0.

#endif

#if 0
CALL wrf_timetoa ( grid%current_time, message2 )
WRITE ( message , FMT = '(A," HT after ",I3)' ) TRIM(message2), grid%id
write(30+myproc,*)ipe-ips+1,jpe-jps+1,trim(message)
do j = jps,jpe
do i = ips,ipe
write(30+myproc,*)grid%ht(i,j)
enddo
enddo
#endif
                       
END SUBROUTINE input_terrain_rsmas

SUBROUTINE update_after_feedback_em ( grid , &
!
#include "em_dummy_args.inc"
!
                 )
!
! perform core specific updates, exchanges after
! model feedback  (called from med_feedback_domain) -John
!

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
   USE module_bc
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   TYPE(domain) , TARGET         :: grid

   !  Definitions of dummy arguments
#include <em_dummy_decl.inc>

   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

  CALL wrf_debug( 500, "entering update_after_feedback_em" )

! Trick problematic compilers into not performing copy-in/copy-out by adding
! indices to array arguments in the CALL statements in this routine.
! It has the effect of passing only the first element of the array, rather
! than the entire array.  See:
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
#include "deref_kludge.h"

#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

!  Obtain dimension information stored in the grid data structure.
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  CALL wrf_debug( 500, "before HALO_EM_FEEDBACK.inc in update_after_feedback_em" )
#ifdef DM_PARALLEL
#include "HALO_EM_FEEDBACK.inc"
#endif
  CALL wrf_debug( 500, "leaving update_after_feedback_em" )

END SUBROUTINE update_after_feedback_em

