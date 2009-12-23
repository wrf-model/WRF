SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE(domain)  :: parent , nest

   INTEGER iswater , map_proj, julyr, julday
   REAL    truelat1 , truelat2 , gmt , moad_cen_lat , stand_lon, pole_lat, pole_lon
   CHARACTER (LEN=256) :: char_junk

! single-value constants

   nest%p_top   = parent%p_top
   nest%save_topo_from_real   = parent%save_topo_from_real
   nest%cfn     = parent%cfn
   nest%cfn1    = parent%cfn1
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

   CALL nl_get_mminlu ( 1,char_junk(1:256) )
   CALL nl_get_iswater (1, iswater )
   CALL nl_get_truelat1 ( 1 , truelat1 )
   CALL nl_get_truelat2 ( 1 , truelat2 )
   CALL nl_get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL nl_get_stand_lon ( 1 , stand_lon )
   CALL nl_get_pole_lat ( 1 , pole_lat )
   CALL nl_get_pole_lon ( 1 , pole_lon )
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
     CALL nl_set_pole_lat ( nest%id , pole_lat )
     CALL nl_set_pole_lon ( nest%id , pole_lon )
     CALL nl_set_map_proj ( nest%id , map_proj )
   END IF
   nest%gmt     = gmt
   nest%julday  = julday
   nest%julyr   = julyr
   nest%iswater = iswater
   nest%truelat1= truelat1
   nest%truelat2= truelat2
   nest%moad_cen_lat= moad_cen_lat
   nest%stand_lon= stand_lon
   nest%pole_lat= pole_lat
   nest%pole_lon= pole_lon
   nest%map_proj= map_proj

   nest%step_number  = parent%step_number

! 1D constants (Z)

   nest%fnm    = parent%fnm
   nest%fnp    = parent%fnp
   nest%rdnw   = parent%rdnw
   nest%rdn    = parent%rdn
   nest%dnw    = parent%dnw
   nest%dn     = parent%dn
   nest%znu    = parent%znu
   nest%znw    = parent%znw
   nest%t_base = parent%t_base
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
   INTEGER :: i , j , k , spec_bdy_width
   REAL    :: r_blend_zones
   INTEGER blend_cell, blend_width

   !  The fine grid elevation comes from the horizontally interpolated
   !  parent elevation for the first spec_bdy_width row/columns, so we need
   !  to get that value.  We blend the coarse and fine in the next blend_width
   !  rows and columns.  After that, in the interior, it is 100% fine grid.

   CALL nl_get_spec_bdy_width ( 1, spec_bdy_width)
   CALL nl_get_blend_width ( 1, blend_width)

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

   r_blend_zones = 1./(blend_width+1)
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            DO blend_cell = blend_width,1,-1
               IF   ( ( i .EQ.       spec_bdy_width + blend_cell ) .OR.  ( j .EQ.       spec_bdy_width + blend_cell ) .OR. &
                      ( i .EQ. ide - spec_bdy_width - blend_cell ) .OR.  ( j .EQ. jde - spec_bdy_width - blend_cell ) ) THEN
                  ter_temp(i,k,j) = ( (blend_cell)*ter_input(i,k,j) + (blend_width+1-blend_cell)*ter_interpolated(i,k,j) ) &
                                    * r_blend_zones
               END IF
            ENDDO
            IF      ( ( i .LE.       spec_bdy_width     ) .OR.  ( j .LE.       spec_bdy_width     ) .OR. &
                      ( i .GE. ide - spec_bdy_width     ) .OR.  ( j .GE. jde - spec_bdy_width     ) ) THEN
               ter_temp(i,k,j) =      ter_interpolated(i,k,j)
            END IF
         END DO
      END DO
   END DO

   !  Set nest elevation with temp values.  All values not overwritten in the above
   !  loops have been previously set in the initial assignment.

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_input(i,k,j) = ter_temp(i,k,j)
         END DO
      END DO
   END DO

END SUBROUTINE blend_terrain

SUBROUTINE copy_3d_field ( ter_interpolated , ter_input , &
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

END SUBROUTINE copy_3d_field

SUBROUTINE adjust_tempqv ( mub, save_mub, znw, p_top, &
                           th, pp, qv,  &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   !USE module_configure
   !USE module_domain
   USE module_model_constants

   !USE module_bc
   !USE module_io_domain
   !USE module_state_description
   !USE module_timing
   !USE module_soil_pre
   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: mub, save_mub
   REAL , DIMENSION(kms:kme) , INTENT(IN)    :: znw
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: th, pp, qv

   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: p_old, p_new, rh
   REAL :: es,dth,tc,e,dth1
   INTEGER :: i , j , k

   real p_top


! p_old = full pressure before terrain blending; also compute initial RH
! which is going to be conserved during terrain blending
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_old(i,k,j) = 0.5*(znw(k+1)+znw(k))*save_mub(i,j) + p_top + pp(i,k,j)
            tc = (th(i,k,j)+300.)*(p_old(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = qv(i,k,j)*p_old(i,k,j)/(0.622+qv(i,k,j))
            rh(i,k,j) = e/es
         END DO
      END DO
   END DO

! p_new = full pressure after terrain blending; also compute temperature correction and convert RH back to QV
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_new(i,k,j) = 0.5*(znw(k+1)+znw(k))*mub(i,j) + p_top + pp(i,k,j)
! 2*(g/cp-6.5e-3)*R_dry/g = -191.86e-3
            dth1 = -191.86e-3*(th(i,k,j)+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            dth = -191.86e-3*(th(i,k,j)+0.5*dth1+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            th(i,k,j) = th(i,k,j)+dth
            tc = (th(i,k,j)+300.)*(p_new(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = rh(i,k,j)*es
            qv(i,k,j) = 0.622*e/(p_new(i,k,j)-e)
         END DO
      END DO
   END DO


END SUBROUTINE adjust_tempqv

SUBROUTINE input_terrain_rsmas ( grid ,                        &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_domain, ONLY : domain
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
CALL domain_clock_get ( grid, current_timestr=message2 )
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
CALL domain_clock_get ( grid, current_timestr=message2 )
WRITE ( message , FMT = '(A," HT after ",I3)' ) TRIM(message2), grid%id
write(30+myproc,*)ipe-ips+1,jpe-jps+1,trim(message)
do j = jps,jpe
do i = ips,ipe
write(30+myproc,*)grid%ht(i,j)
enddo
enddo
#endif

END SUBROUTINE input_terrain_rsmas

SUBROUTINE update_after_feedback_em ( grid  &
!
#include "dummy_new_args.inc"
!
                 )
!
! perform core specific updates, exchanges after
! model feedback  (called from med_feedback_domain) -John
!

! Driver layer modules
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask
   USE module_comm_dm, ONLY : HALO_EM_FEEDBACK_sub
#else
   USE module_dm
#endif
   USE module_bc
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   TYPE(domain) , TARGET         :: grid

   !  Definitions of dummy arguments
#include <dummy_new_decl.inc>

   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

  CALL wrf_debug( 500, "entering update_after_feedback_em" )

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

