!WRF:MODEL_LAYER:BOUNDARY
!
MODULE module_bc

   USE module_configure
   USE module_wrf_error
   IMPLICIT NONE

!   TYPE bcs
!
!     LOGICAL                     :: periodic_x
!     LOGICAL                     :: symmetric_xs
!     LOGICAL                     :: symmetric_xe
!     LOGICAL                     :: open_xs
!     LOGICAL                     :: open_xe
!     LOGICAL                     :: periodic_y
!     LOGICAL                     :: symmetric_ys
!     LOGICAL                     :: symmetric_ye
!     LOGICAL                     :: open_ys
!     LOGICAL                     :: open_ye
!     LOGICAL                     :: nested
!     LOGICAL                     :: specified
!     LOGICAL                     :: top_radiation
!
!   END TYPE bcs

!  set the bdyzone.  We are hardwiring this here and we'll
!  decide later where it should be set and stored

   INTEGER, PARAMETER            :: bdyzone = 3
   INTEGER, PARAMETER            :: bdyzone_x = bdyzone
   INTEGER, PARAMETER            :: bdyzone_y = bdyzone

CONTAINS

  SUBROUTINE boundary_condition_check ( config_flags, bzone, error, gn )

!  this routine checks the boundary condition logicals 
!  to make sure that the boundary conditions are not over
!  or under specified.  The routine also checks that the
!  boundary zone is sufficiently sized for the specified
!  boundary conditions

  IMPLICIT NONE

  TYPE( grid_config_rec_type ) config_flags

  INTEGER, INTENT(IN   ) :: bzone, gn
  INTEGER, INTENT(INOUT) :: error

! local variables

  INTEGER :: xs_bc, xe_bc, ys_bc, ye_bc, bzone_min
  INTEGER :: nprocx, nprocy

  CALL wrf_debug( 100 , ' checking boundary conditions for grid ' )

  error = 0
  xs_bc = 0
  xe_bc = 0
  ys_bc = 0
  ye_bc = 0

!  sum the number of conditions specified for each lateral boundary.
!  obviously, this number should be 1

  IF( config_flags%periodic_x ) THEN
    xs_bc = xs_bc+1
    xe_bc = xe_bc+1
  ENDIF

  IF( config_flags%periodic_y ) THEN
    ys_bc = ys_bc+1
    ye_bc = ye_bc+1
  ENDIF

  IF( config_flags%symmetric_xs ) xs_bc = xs_bc + 1
  IF( config_flags%symmetric_xe ) xe_bc = xe_bc + 1
  IF( config_flags%open_xs )      xs_bc = xs_bc + 1
  IF( config_flags%open_xe )      xe_bc = xe_bc + 1


  IF( config_flags%symmetric_ys ) ys_bc = ys_bc + 1
  IF( config_flags%symmetric_ye ) ye_bc = ye_bc + 1
  IF( config_flags%open_ys )      ys_bc = ys_bc + 1
  IF( config_flags%open_ye )      ye_bc = ye_bc + 1

  IF( config_flags%nested ) THEN
     xs_bc = xs_bc + 1
     xe_bc = xe_bc + 1
     ys_bc = ys_bc + 1
     ye_bc = ye_bc + 1
   ENDIF

  IF( config_flags%specified ) THEN
     xs_bc = xs_bc + 1
     xe_bc = xe_bc + 1
     ys_bc = ys_bc + 1
     ye_bc = ye_bc + 1
   ENDIF

!  check the number of conditions for each boundary

   IF( (xs_bc /= 1) .or. &
       (xe_bc /= 1) .or. &
       (ys_bc /= 1) .or. &
       (ye_bc /= 1)         ) THEN

     error = 1

     write( wrf_err_message ,*) ' *** Error in boundary condition specification '
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at xs ', xs_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at xe ', xe_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at ys ', ys_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at ye ', ye_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions logicals are '
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' periodic_x   ',config_flags%periodic_x
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' periodic_y   ',config_flags%periodic_y
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_xs ',config_flags%symmetric_xs
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_xe ',config_flags%symmetric_xe
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_ys ',config_flags%symmetric_ys
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_ye ',config_flags%symmetric_ye
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_xs      ',config_flags%open_xs
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_xe      ',config_flags%open_xe
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_ys      ',config_flags%open_ys
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_ye      ',config_flags%open_ye
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' nested       ',config_flags%nested
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' specified    ',config_flags%specified
     CALL wrf_error_fatal( ' *** Error in boundary condition specification ' )

   ENDIF

!  now check to see if boundary zone size is sufficient.
!  we could have the necessary boundary zone size be returned
!  to the calling routine.

   IF( config_flags%periodic_x   .or. &
       config_flags%periodic_y   .or. &
       config_flags%symmetric_xs .or. &
       config_flags%symmetric_xe .or. &
       config_flags%symmetric_ys .or. &
       config_flags%symmetric_ye        )  THEN

       bzone_min = MAX( 1,                                  &
                        (config_flags%h_mom_adv_order+1)/2, &
                        (config_flags%h_sca_adv_order+1)/2 )

       IF( bzone < bzone_min) THEN  

         error = 2
         WRITE ( wrf_err_message , * ) ' boundary zone not large enough '
         CALL wrf_message ( wrf_err_message )
         WRITE ( wrf_err_message , * ) ' boundary zone specified      ',bzone
         CALL wrf_message ( wrf_err_message )
         WRITE ( wrf_err_message , * ) ' minimum boundary zone needed ',bzone_min
         CALL wrf_error_fatal ( wrf_err_message )

       ENDIF
   ENDIF

#ifdef RSL_LITE
! add a check so that people do not try to specify peridic boundaries
! if they have compiled with RSL_LITE. It does not support periodic bcs
   CALL wrf_get_nprocx( nprocx )
   IF ( nprocx .GT. 1 .AND. config_flags%periodic_x ) THEN
     WRITE ( wrf_err_message , * ) 'RSL_LITE does not support periodic boundaries for decomposed X-dimension'
     CALL wrf_message ( TRIM( wrf_err_message ) )
   ENDIF
   CALL wrf_get_nprocy( nprocy )
   IF ( nprocy .GT. 1 .AND. config_flags%periodic_y ) THEN
     WRITE ( wrf_err_message , * ) 'RSL_LITE does not support periodic boundaries for decomposed Y-dimension'
     CALL wrf_message ( TRIM( wrf_err_message ) )
   ENDIF
   IF ( (nprocy .GT. 1 .AND. config_flags%periodic_y) .OR. (nprocx .GT. 1 .AND. config_flags%periodic_x) ) THEN
     CALL wrf_error_fatal ( 'Inappropriate selection of periodic bcs detected in boundary_condition_check' )
   ENDIF
#endif

   CALL wrf_debug ( 100 , ' boundary conditions OK for grid ' )

   END subroutine boundary_condition_check

!--------------------------------------------------------------------------
   SUBROUTINE set_physical_bc2d( data, variable_in,  &
                                 config_flags,           & 
                                 ids,ide, jds,jde,   & ! domain dims
                                 ims,ime, jms,jme,   & ! memory dims
                                 ips,ipe, jps,jpe,   & ! patch  dims
                                 its,ite, jts,jte   )      

!  This subroutine sets the data in the boundary region, by direct
!  assignment if possible, for periodic and symmetric (wall)
!  boundary conditions.  Currently, we are only doing 1 variable
!  at a time - lots of overhead, so maybe this routine can be easily 
!  inlined later or we could pass multiple variables -
!  would probably want a largestep and smallstep version.

!  15 Jan 99, Dave
!  Modified the incoming its,ite,jts,jte to truly be the tile size.
!  This required modifying the loop limits when the "istag" or "jstag"
!  is used, as this is only required at the end of the domain.

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , jms:jme ) :: data
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, istag, jstag, itime

      LOGICAL  :: debug, open_bc_copy

!------------

      debug = .false.

      open_bc_copy = .false.

      variable = variable_in
      IF ( variable_in .ge. 'A' .and. variable_in .le. 'Z' ) THEN
        variable = CHAR( ICHAR(variable_in) - ICHAR('A') + ICHAR('a') )
      ENDIF
      IF ((variable == 'u') .or. (variable == 'v') .or.  &
          (variable == 'w') .or. (variable == 't') .or.  &
          (variable == 'x') .or. (variable == 'y') .or.  &
          (variable == 'r')                        ) open_bc_copy = .true.

!  begin, first set a staggering variable

      istag = -1
      jstag = -1

      IF ((variable == 'u') .or. (variable == 'x')) istag = 0
      IF ((variable == 'v') .or. (variable == 'y')) jstag = 0

      if(debug) then
        write(6,*) ' in bc2d, var is ',variable, istag, jstag
        write(6,*) ' b.cs are ',  &
      config_flags%periodic_x,  &
      config_flags%periodic_y
      end if
      


!  periodic conditions.
!  note, patch must cover full range in periodic dir, or else
!  its intra-patch communication that is handled elsewheres.
!  symmetry conditions can always be handled here, because no
!  outside patch communication is needed

      periodicity_x:  IF( ( config_flags%periodic_x ) .and.  &
                          ( ids == ips )          .and.  &
                          ( ide == ipe )                ) THEN

        IF ( its == ids ) THEN

          DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
          DO i = 0,-(bdyzone-1),-1
            data(ids+i-1,j) = data(ide+i-1,j)
          ENDDO
          ENDDO

        ENDIF

        IF ( ite == ide ) THEN

          DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
!!          DO i = 1 , bdyzone
          DO i = -istag , bdyzone
            data(ide+i+istag,j) = data(ids+i+istag,j)
          ENDDO
          ENDDO

        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 1, bdyzone
              data(ids-i,j) = data(ids+i-1,j) !  here, data(0) = data(1), etc
            ENDDO                             !  symmetry about data(0.5) (u=0 pt)
            ENDDO

          ELSE

            IF( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                data(ids-i,j) = - data(ids+i,j) ! here, u(0) = - u(2), etc
              ENDDO                             !  normal b.c symmetry at u(1)
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                data(ids-i,j) =   data(ids+i,j) ! here, phi(0) = phi(2), etc
              ENDDO                             !  normal b.c symmetry at phi(1)
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs


!  now the symmetry boundary at xe

        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 1, bdyzone
              data(ide+i-1,j) = data(ide-i,j)  !  sym. about data(ide-0.5)
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                data(ide+i,j) = - data(ide-i,j)  ! u(ide+1) = - u(ide-1), etc.
              ENDDO
              ENDDO


            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                data(ide+i,j) = data(ide-i,j)  !  phi(ide+1) = phi(ide-1), etc.
              ENDDO
              ENDDO

            END IF

          END IF 

        END IF symmetry_xe


!  set open b.c in X copy into boundary zone here.  WCS, 19 March 2000

        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              data(ids-1,j) = data(ids,j) !  here, data(0) = data(1)
            ENDDO

        ENDIF open_xs


!  now the open boundary copy at xe

        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                          ( ite == ide ) .and. open_bc_copy  )  THEN

          IF ( variable /= 'u' .and. variable /= 'x') THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              data(ide,j) = data(ide-1,j) 
            ENDDO

          ELSE

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              data(ide+1,j) = data(ide,j)
            ENDDO

          END IF 

        END IF open_xe

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_x

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) .and.  &
                          ( jds == jps )          .and.  & 
                          ( jde == jpe )                  )  THEN 

        IF( jts == jds ) then

          DO j = 0, -(bdyzone-1), -1
          DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
            data(i,jds+j-1) = data(i,jde+j-1)
          ENDDO
          ENDDO

        END IF

        IF( jte == jde ) then

!!          DO j = 1, bdyzone
          DO j = -jstag, bdyzone
          DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
            data(i,jde+j+jstag) = data(i,jds+j+jstag)
          ENDDO
          ENDDO

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,jds-j) = data(i,jds+j-1) 
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,jds-j) = - data(i,jds+j) 
              ENDDO              
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,jds-j) = data(i,jds+j) 
              ENDDO              
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys

!  now the symmetry boundary at ye

        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,jde+j-1) = data(i,jde-j) 
            ENDDO                               
            ENDDO

          ELSE

            IF (variable == 'v' ) THEN

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,jde+j) = - data(i,jde-j)    ! bugfix: changed jds on rhs to jde , JM 20020410
              ENDDO                               
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,jde+j) = data(i,jde-j)
              ENDDO                               
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye

!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,jds-1) = data(i,jds) 
            ENDDO

        ENDIF open_ys

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF  (variable /= 'v' .and. variable /= 'y' ) THEN

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,jde) = data(i,jde-1) 
            ENDDO                               

          ELSE

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,jde+1) = data(i,jde) 
            ENDDO                               

          ENDIF

        END IF open_ye
      
!  end open b.c in Y copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_y

!  fix corners for doubly periodic domains

      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jts == jds) ) THEN  ! lower left corner fill
           DO j = 0, -(bdyzone-1), -1
           DO i = 0, -(bdyzone-1), -1
             data(ids+i-1,jds+j-1) = data(ide+i-1,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = 0, -(bdyzone-1), -1
           DO i = 1, bdyzone
             data(ide+i+istag,jds+j-1) = data(ids+i+istag,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = 1, bdyzone
           DO i = 1, bdyzone
             data(ide+i+istag,jde+j+jstag) = data(ids+i+istag,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = 1, bdyzone
           DO i = 0, -(bdyzone-1), -1
             data(ids+i-1,jde+j+jstag) = data(ide+i-1,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE set_physical_bc2d

!-----------------------------------

   SUBROUTINE set_physical_bc3d( data, variable_in,        &
                               config_flags,                   & 
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets the data in the boundary region, by direct
!  assignment if possible, for periodic and symmetric (wall)
!  boundary conditions.  Currently, we are only doing 1 variable
!  at a time - lots of overhead, so maybe this routine can be easily 
!  inlined later or we could pass multiple variables -
!  would probably want a largestep and smallstep version.

!  15 Jan 99, Dave
!  Modified the incoming its,ite,jts,jte to truly be the tile size.
!  This required modifying the loop limits when the "istag" or "jstag"
!  is used, as this is only required at the end of the domain.

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: data
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, k, istag, jstag, itime, k_end

      LOGICAL  :: debug, open_bc_copy

!------------

      debug = .false.

      open_bc_copy = .false.

      variable = variable_in
      IF ( variable_in .ge. 'A' .and. variable_in .le. 'Z' ) THEN
        variable = CHAR( ICHAR(variable_in) - ICHAR('A') + ICHAR('a') )
      ENDIF

      IF ((variable == 'u') .or. (variable == 'v') .or.     &
          (variable == 'w') .or. (variable == 't') .or.     &
          (variable == 'd') .or. (variable == 'e') .or. &
          (variable == 'x') .or. (variable == 'y') .or. &
          (variable == 'f') .or. (variable == 'r') ) open_bc_copy = .true.

!  begin, first set a staggering variable

      istag = -1
      jstag = -1
      k_end = max(1,min(kde-1,kte))


      IF ((variable == 'u') .or. (variable == 'x')) istag = 0
      IF ((variable == 'v') .or. (variable == 'y')) jstag = 0
      IF ((variable == 'd') .or. (variable == 'xy')) then
         istag = 0
         jstag = 0
      ENDIF
      IF ((variable == 'e') ) then
         istag = 0
         k_end = min(kde,kte)
      ENDIF

      IF ((variable == 'f') ) then
         jstag = 0
         k_end = min(kde,kte)
      ENDIF

      IF ( variable == 'w')  k_end = min(kde,kte)

!      k_end = kte

      if(debug) then
        write(6,*) ' in bc, var is ',variable, istag, jstag, kte, k_end
        write(6,*) ' b.cs are ',  &
      config_flags%periodic_x,  &
      config_flags%periodic_y
      end if
      


!  periodic conditions.
!  note, patch must cover full range in periodic dir, or else
!  its intra-patch communication that is handled elsewheres.
!  symmetry conditions can always be handled here, because no
!  outside patch communication is needed

      periodicity_x:  IF( ( config_flags%periodic_x ) .and.  &
                          ( ids == ips )          .and.  &
                          ( ide == ipe )                ) THEN

        IF ( its == ids ) THEN

          DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
          DO k = kts, k_end
          DO i = 0,-(bdyzone-1),-1
            data(ids+i-1,k,j) = data(ide+i-1,k,j)
          ENDDO
          ENDDO
          ENDDO

        ENDIF

        IF ( ite == ide ) THEN

          DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
          DO k = kts, k_end
!!          DO i = 1 , bdyzone
          DO i = -istag , bdyzone
            data(ide+i+istag,k,j) = data(ids+i+istag,k,j)
          ENDDO
          ENDDO
          ENDDO

        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 1, bdyzone
              data(ids-i,k,j) = data(ids+i-1,k,j) !  here, data(0) = data(1), etc
            ENDDO                                 !  symmetry about data(0.5) (u = 0 pt)
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                data(ids-i,k,j) = - data(ids+i,k,j) ! here, u(0) = - u(2), etc
              ENDDO                                 !  normal b.c symmetry at u(1)
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                data(ids-i,k,j) = data(ids+i,k,j) ! here, phi(0) = phi(2), etc
              ENDDO                               !  normal b.c symmetry at phi(1)
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs


!  now the symmetry boundary at xe

        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 1, bdyzone
              data(ide+i-1,k,j) = data(ide-i,k,j)  !  sym. about data(ide-0.5)
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u') THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                data(ide+i,k,j) = - data(ide-i,k,j)  ! u(ide+1) = - u(ide-1), etc.
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                data(ide+i,k,j) = data(ide-i,k,j)  ! phi(ide+1) = - phi(ide-1), etc.
              ENDDO
              ENDDO
              ENDDO

             END IF

          END IF 

        END IF symmetry_xe

!  set open b.c in X copy into boundary zone here.  WCS, 19 March 2000

        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = jts-bdyzone, MIN(jte,jde+jstag)+bdyzone
            DO k = kts, k_end
              data(ids-1,k,j) = data(ids,k,j) !  here, data(0) = data(1), etc
            ENDDO
            ENDDO

        ENDIF open_xs


!  now the open_xe boundary copy 

        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( ite == ide ) .and. open_bc_copy )  THEN

          IF (variable /= 'u' .and. variable /= 'x' ) THEN

            DO j = jts-bdyzone, MIN(jte,jde+jstag)+bdyzone
            DO k = kts, k_end
              data(ide,k,j) = data(ide-1,k,j)
            ENDDO
            ENDDO

          ELSE

!!!!!!! I am not sure about this one!  JM 20020402
            DO j = MAX(jds,jts-1)-bdyzone, MIN(jte+1,jde+jstag)+bdyzone
            DO k = kts, k_end
              data(ide+1,k,j) = data(ide,k,j)
            ENDDO
            ENDDO

          END IF 

        END IF open_xe

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_x

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) .and.  &
                          ( jds == jps )          .and.  & 
                          ( jde == jpe )                  )  THEN 

        IF( jts == jds ) then

          DO j = 0, -(bdyzone-1), -1
          DO k = kts, k_end
          DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
            data(i,k,jds+j-1) = data(i,k,jde+j-1)
          ENDDO
          ENDDO
          ENDDO

        END IF

        IF( jte == jde ) then

!!          DO j = 1, bdyzone
          DO j = -jstag, bdyzone
          DO k = kts, k_end
          DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
            data(i,k,jde+j+jstag) = data(i,k,jds+j+jstag)
          ENDDO
          ENDDO
          ENDDO

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,k,jds-j) = data(i,k,jds+j-1) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,k,jds-j) = - data(i,k,jds+j) 
              ENDDO              
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,k,jds-j) = data(i,k,jds+j) 
              ENDDO              
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys

!  now the symmetry boundary at ye

        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,k,jde+j-1) = data(i,k,jde-j) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'v' ) THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,k,jde+j) = - data(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                data(i,k,jde+j) = data(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye
      
!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,k,jds-1) = data(i,k,jds) 
            ENDDO
            ENDDO

        ENDIF open_ys

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF (variable /= 'v' .and. variable /= 'y' ) THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,k,jde) = data(i,k,jde-1) 
            ENDDO                               
            ENDDO

          ELSE

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              data(i,k,jde+1) = data(i,k,jde) 
            ENDDO                               
            ENDDO

          ENDIF

      END IF open_ye

!  end open b.c in Y copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_y

!  fix corners for doubly periodic domains

      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jts == jds) ) THEN  ! lower left corner fill
           DO j = 0, -(bdyzone-1), -1
           DO k = kts, k_end
           DO i = 0, -(bdyzone-1), -1
             data(ids+i-1,k,jds+j-1) = data(ide+i-1,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = 0, -(bdyzone-1), -1
           DO k = kts, k_end
           DO i = 1, bdyzone
             data(ide+i+istag,k,jds+j-1) = data(ids+i+istag,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 1, bdyzone
             data(ide+i+istag,k,jde+j+jstag) = data(ids+i+istag,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 0, -(bdyzone-1), -1
             data(ids+i-1,k,jde+j+jstag) = data(ide+i-1,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE set_physical_bc3d

   SUBROUTINE init_module_bc
   END SUBROUTINE init_module_bc

!------------------------------------------------------------------------

   SUBROUTINE relax_bdytend   ( field, field_tend,        &
                               field_bdy, field_bdy_tend, &
!                              field_xbdy, field_ybdy,    &
                               variable_in, config_flags, & 
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine adds the tendencies in the boundary relaxation region, for specified
!  boundary conditions.  
!  spec_bdy_width is only used to dimension the boundary arrays.
!  relax_zone is the inner edge of the boundary relaxation zone treated here.
!  spec_zone is the width of the outer specified b.c.s that are not changed here.
!  (JD July 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: ijds,ijde
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone, relax_zone
      REAL,         INTENT(IN   )    :: dtbc
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field_tend
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: field_bdy
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: field_bdy_tend
      REAL,  DIMENSION( spec_bdy_width ), INTENT(IN   ) :: fcx, gcx
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist
      REAL       :: fls0, fls1, fls2, fls3, fls4

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. relax_zone) THEN
! Y-start boundary
        DO j = max(jts,jbs+spec_zone), min(jtf,jbs+relax_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              fls0 = field_bdy(i, k, b_dist+1, P_YSB) &
                   + dtbc * field_bdy_tend(i, k, b_dist+1, P_YSB) &
                   - field(i,k,j)
              fls1 = field_bdy(i-1, k, b_dist+1, P_YSB) &
                   + dtbc * field_bdy_tend(i-1, k, b_dist+1, P_YSB) &
                   - field(i-1,k,j)
              fls2 = field_bdy(i+1, k, b_dist+1, P_YSB) &
                   + dtbc * field_bdy_tend(i+1, k, b_dist+1, P_YSB) &
                   - field(i+1,k,j)
              fls3 = field_bdy(i, k, b_dist, P_YSB) &
                   + dtbc * field_bdy_tend(i, k, b_dist, P_YSB) &
                   - field(i,k,j-1)
              fls4 = field_bdy(i, k, b_dist+2, P_YSB) &
                   + dtbc * field_bdy_tend(i, k, b_dist+2, P_YSB) &
                   - field(i,k,j+1)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (jbe - jtf .lt. relax_zone) THEN
! Y-end boundary
        DO j = max(jts,jbe-relax_zone+1), min(jtf,jbe-spec_zone)
          b_dist = jbe - j
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              fls0 = field_bdy(i, k, b_dist+1, P_YEB) &
                   + dtbc * field_bdy_tend(i, k, b_dist+1, P_YEB) &
                   - field(i,k,j)
              fls1 = field_bdy(i-1, k, b_dist+1, P_YEB) &
                   + dtbc * field_bdy_tend(i-1, k, b_dist+1, P_YEB) &
                   - field(i-1,k,j)
              fls2 = field_bdy(i+1, k, b_dist+1, P_YEB) &
                   + dtbc * field_bdy_tend(i+1, k, b_dist+1, P_YEB) &
                   - field(i+1,k,j)
              fls3 = field_bdy(i, k, b_dist, P_YEB) &
                   + dtbc * field_bdy_tend(i, k, b_dist, P_YEB) &
                   - field(i,k,j+1)
              fls4 = field_bdy(i, k, b_dist+2, P_YEB) &
                   + dtbc * field_bdy_tend(i, k, b_dist+2, P_YEB) &
                   - field(i,k,j-1)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)

            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (its - ibs .lt. relax_zone) THEN
! X-start boundary
        DO i = max(its,ibs+spec_zone), min(itf,ibs+relax_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              fls0 = field_bdy(j, k, b_dist+1, P_XSB) &
                   + dtbc * field_bdy_tend(j, k, b_dist+1, P_XSB) &
                   - field(i,k,j)
              fls1 = field_bdy(j-1, k, b_dist+1, P_XSB) &
                   + dtbc * field_bdy_tend(j-1, k, b_dist+1, P_XSB) &
                   - field(i,k,j-1)
              fls2 = field_bdy(j+1, k, b_dist+1, P_XSB) &
                   + dtbc * field_bdy_tend(j+1, k, b_dist+1, P_XSB) &
                   - field(i,k,j+1)
              fls3 = field_bdy(j, k, b_dist, P_XSB) &
                   + dtbc * field_bdy_tend(j, k, b_dist, P_XSB) &
                   - field(i-1,k,j)
              fls4 = field_bdy(j, k, b_dist+2, P_XSB) &
                   + dtbc * field_bdy_tend(j, k, b_dist+2, P_XSB) &
                   - field(i+1,k,j)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)

            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. relax_zone) THEN
! X-end boundary
        DO i = max(its,ibe-relax_zone+1), min(itf,ibe-spec_zone)
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              fls0 = field_bdy(j, k, b_dist+1, P_XEB) &
                   + dtbc * field_bdy_tend(j, k, b_dist+1, P_XEB) &
                   - field(i,k,j)
              fls1 = field_bdy(j-1, k, b_dist+1, P_XEB) &
                   + dtbc * field_bdy_tend(j-1, k, b_dist+1, P_XEB) &
                   - field(i,k,j-1)
              fls2 = field_bdy(j+1, k, b_dist+1, P_XEB) &
                   + dtbc * field_bdy_tend(j+1, k, b_dist+1, P_XEB) &
                   - field(i,k,j+1)
              fls3 = field_bdy(j, k, b_dist, P_XEB) &
                   + dtbc * field_bdy_tend(j, k, b_dist, P_XEB) &
                   - field(i+1,k,j)
              fls4 = field_bdy(j, k, b_dist+2, P_XEB) &
                   + dtbc * field_bdy_tend(j, k, b_dist+2, P_XEB) &
                   - field(i-1,k,j)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
            ENDDO
          ENDDO
        ENDDO
      ENDIF


   END SUBROUTINE relax_bdytend
!------------------------------------------------------------------------

   SUBROUTINE spec_bdytend   ( field_tend,                &
                               field_bdy, field_bdy_tend, &
                               variable_in, config_flags, & 
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets the tendencies in the boundary specified region.
!  spec_bdy_width is only used to dimension the boundary arrays.
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD July 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: ijds,ijde
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT  ) :: field_tend
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: field_bdy
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: field_bdy_tend
!     REAL,  DIMENSION( jms:jme , kms:kme , spec_bdy_width, 4 ), INTENT(IN   ) :: field_xbdy
!     REAL,  DIMENSION( ims:ime , kms:kme , spec_bdy_width, 4 ), INTENT(IN   ) :: field_ybdy
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              field_tend(i,k,j) = field_bdy_tend(i, k, b_dist+1, P_YSB)
!             field_tend(i,k,j) = field_ybdy(i, k, b_dist+1, P_SBT)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              field_tend(i,k,j) = field_bdy_tend(i, k, b_dist+1, P_YEB)
!             field_tend(i,k,j) = field_ybdy(i, k, b_dist+1, P_EBT)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field_tend(i,k,j) = field_bdy_tend(j, k, b_dist+1, P_XSB)
!             field_tend(i,k,j) = field_xbdy(i, k, b_dist+1, P_SBT)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field_tend(i,k,j) = field_bdy_tend(j, k, b_dist+1, P_XEB)
!             field_tend(i,k,j) = field_xbdy(i, k, b_dist+1, P_EBT)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE spec_bdytend
!------------------------------------------------------------------------

   SUBROUTINE spec_bdyupdate(  field,      &
                               field_tend, dt,            &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine adds the tendencies in the boundary specified region.
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE spec_bdyupdate
!------------------------------------------------------------------------

   SUBROUTINE zero_grad_bdy (  field,                     &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets zero gradient conditions in the boundary specified region.
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'w') ktf = kde

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              field(i,k,j) = field(i_inner,k,jbs+spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              field(i,k,j) = field(i_inner,k,jbe-spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              field(i,k,j) = field(ibs+spec_zone,k,j_inner)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              field(i,k,j) = field(ibe-spec_zone,k,j_inner)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE zero_grad_bdy
!------------------------------------------------------------------------

   SUBROUTINE flow_dep_bdy  (  field,                     &
                               u, v, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets zero gradient conditions for outflow and zero value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE flow_dep_bdy

!------------------------------------------------------------------------------

 SUBROUTINE stuff_bdy ( data3d , space_bdy , char_stagger , &
                             ijds , ijde , spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 !  This routine puts the data in the 3d arrays into the proper locations
 !  for the lateral boundary arrays.
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: ijds , ijde , spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3d
!    REAL , DIMENSION(:,:,:,:) , INTENT(OUT) :: space_bdy
    REAL , DIMENSION(ijds:ijde,kds:kde,spec_bdy_width,4,1) , INTENT(OUT) :: space_bdy
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    !  There are four lateral boundary locations that are stored.
 
    !  X start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    !  X end boundary
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy(j,k,ii,P_XEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    !  Y start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy(i,k,j,P_YSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    !  Y end boundary
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy(i,k,jj,P_YEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdy
 
 SUBROUTINE stuff_bdytend ( data3dnew , data3dold , time_diff , space_bdy , char_stagger , &
                             ijds , ijde , spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 !  This routine puts the tendency data into the proper locations
 !  for the lateral boundary arrays.
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: ijds , ijde , spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3dnew , data3dold
!    REAL , DIMENSION(:,:,:,:) , INTENT(OUT) :: space_bdy
    REAL , DIMENSION(ijds:ijde,kds:kde,spec_bdy_width,4,1) , INTENT(OUT) :: space_bdy
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff ! seconds
 
    INTEGER :: i , ii , j , jj , k
 
    !  There are four lateral boundary locations that are stored.
 
    !  X start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,i,P_XSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,i,P_XSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,i,P_XSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,i,P_XSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    END IF
 
    !  X end boundary
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy(j,k,ii,P_XEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,ii,P_XEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,ii,P_XEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,ii,P_XEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,ii,P_XEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(j,k,ii,P_XEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    END IF
 
    !  Y start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,j,P_YSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,j,P_YSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy(i,k,j,P_YSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,j,P_YSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,j,P_YSB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    END IF
 
    !  Y end boundary
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy(i,k,jj,P_YEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,jj,P_YEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,jj,P_YEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,jj,P_YEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,jj,P_YEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB,1) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
!         space_bdy(i,k,jj,P_YEB,1) = 0. ! zeroout
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdytend

END MODULE module_bc

SUBROUTINE get_bdyzone_x ( bzx )
  USE module_bc
  IMPLICIT NONE
  INTEGER bzx
  bzx = bdyzone_x
END SUBROUTINE get_bdyzone_x

SUBROUTINE get_bdyzone_y ( bzy)
  USE module_bc
  IMPLICIT NONE
  INTEGER bzy
  bzy = bdyzone_y
END SUBROUTINE get_bdyzone_y

SUBROUTINE get_bdyzone ( bz)
  USE module_bc
  IMPLICIT NONE
  INTEGER bz
  bz = bdyzone
END SUBROUTINE get_bdyzone

