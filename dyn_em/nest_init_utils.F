SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain
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
   nest%dts     = nest%dt/float(nest%time_step_sound)
   nest%dtseps  = parent%dtseps  ! used in height model only?
   nest%resm    = parent%resm    ! used in height model only?
   nest%zetatop = parent%zetatop ! used in height model only?
   nest%cf1     = parent%cf1
   nest%cf2     = parent%cf2
   nest%cf3     = parent%cf3
   nest%gmt     = parent%gmt
   nest%julyr   = parent%julyr
   nest%julday  = parent%julday

   CALL get_mminlu ( char_junk(1:4) )
   CALL get_iswater (1, iswater )
   CALL get_cen_lat ( 1 , cen_lat )
   CALL get_cen_lon ( 1 , cen_lon )
   CALL get_truelat1 ( 1 , truelat1 )
   CALL get_truelat2 ( 1 , truelat2 )
   CALL get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL get_stand_lon ( 1 , stand_lon )
   CALL get_map_proj ( 1 , map_proj )
   CALL get_gmt ( 1 , gmt)
   CALL get_julyr ( 1 , julyr)
   CALL get_julday ( 1 , julday)
   IF ( nest%id .NE. 1 ) THEN
     CALL set_gmt (nest%id, gmt)
     CALL set_julyr (nest%id, julyr)
     CALL set_julday (nest%id, julday)
     CALL set_iswater (nest%id, iswater )
     CALL set_cen_lat ( nest%id , cen_lat )
     CALL set_cen_lon ( nest%id , cen_lon )
     CALL set_truelat1 ( nest%id , truelat1 )
     CALL set_truelat2 ( nest%id , truelat2 )
     CALL set_moad_cen_lat ( nest%id , moad_cen_lat )
     CALL set_stand_lon ( nest%id , stand_lon )
     CALL set_map_proj ( nest%id , map_proj )
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
   nest%dzs       = parent%dzs
   nest%zs        = parent%zs

END SUBROUTINE init_domain_constants_em

SUBROUTINE blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , & 
                           ims , ime , jms , jme , kms , kme , & 
                           ips , ipe , jps , jpe , kps , kpe )

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

   CALL get_spec_zone ( spec_zone) 

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
