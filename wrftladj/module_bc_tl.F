!WRF+/TL:MODEL_LAYER:BOUNDARY
!Created by Ning Pan, 2010-08
!

MODULE g_module_bc

   USE module_configure
   USE module_wrf_error
   USE module_model_constants

   IMPLICIT NONE

!  set the bdyzone.  We are hardwiring this here and we'll
!  decide later where it should be set and stored

   INTEGER, PARAMETER            :: bdyzone = 4
   INTEGER, PARAMETER            :: bdyzone_x = bdyzone
   INTEGER, PARAMETER            :: bdyzone_y = bdyzone

CONTAINS

!-----------------------------------

   SUBROUTINE g_set_physical_bc2d( dat,g_dat, variable_in,  &
                                 config_flags,           & 
                                 ids,ide, jds,jde,   & ! domain dims
                                 ims,ime, jms,jme,   & ! memory dims
                                 ips,ipe, jps,jpe,   & ! patch  dims
                                 its,ite, jts,jte   )      

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , jms:jme ) :: dat,g_dat
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
          (variable == 'r') .or. (variable == 'p') ) open_bc_copy = .true.

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
      
      IF ( variable == 'd' ) then  !JDM
         istag = 0
         jstag = 0
      ENDIF
      IF ( variable == 'e' ) then  !JDM
         istag = 0
      ENDIF
      IF ( variable == 'f' ) then  !JDM
         jstag = 0
      ENDIF

      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN 
        IF ( ( ids == ips ) .and.  ( ide == ipe ) ) THEN  ! test if east and west both on-processor 
          IF ( its == ids ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 0,-(bdyzone-1),-1
              g_dat(ids+i-1,j) = g_dat(ide+i-1,j)
              dat(ids+i-1,j) = dat(ide+i-1,j)
            ENDDO
            ENDDO

          ENDIF

          IF ( ite == ide ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
!!          DO i = 1 , bdyzone
            DO i = -istag , bdyzone
              g_dat(ide+i+istag,j) = g_dat(ids+i+istag,j)
              dat(ide+i+istag,j) = dat(ids+i+istag,j)
            ENDDO
            ENDDO

          ENDIF
        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 1, bdyzone
              g_dat(ids-i,j) = g_dat(ids+i-1,j)
              dat(ids-i,j) = dat(ids+i-1,j)
            ENDDO
            ENDDO

          ELSE

            IF( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                g_dat(ids-i,j) = - g_dat(ids+i,j)
                dat(ids-i,j) = - dat(ids+i,j)
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                g_dat(ids-i,j) =   g_dat(ids+i,j)
                dat(ids-i,j) =   dat(ids+i,j)
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
            DO i = 1, bdyzone
              g_dat(ide+i-1,j) = g_dat(ide-i,j)
              dat(ide+i-1,j) = dat(ide-i,j)
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                g_dat(ide+i,j) = - g_dat(ide-i,j)
                dat(ide+i,j) = - dat(ide-i,j)
              ENDDO
              ENDDO


            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                g_dat(ide+i,j) = g_dat(ide-i,j)
                dat(ide+i,j) = dat(ide-i,j)
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
              g_dat(ids-1,j) = g_dat(ids,j)
              dat(ids-1,j) = dat(ids,j)
              g_dat(ids-2,j) = g_dat(ids,j)
              dat(ids-2,j) = dat(ids,j)
              g_dat(ids-3,j) = g_dat(ids,j)
              dat(ids-3,j) = dat(ids,j)
            ENDDO

        ENDIF open_xs


!  now the open boundary copy at xe

        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                          ( ite == ide ) .and. open_bc_copy  )  THEN

          IF ( variable /= 'u' .and. variable /= 'x') THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              g_dat(ide  ,j) = g_dat(ide-1,j) 
              dat(ide  ,j) = dat(ide-1,j) 
              g_dat(ide+1,j) = g_dat(ide-1,j) 
              dat(ide+1,j) = dat(ide-1,j) 
              g_dat(ide+2,j) = g_dat(ide-1,j) 
              dat(ide+2,j) = dat(ide-1,j) 
            ENDDO

          ELSE

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              g_dat(ide+1,j) = g_dat(ide,j)
              dat(ide+1,j) = dat(ide,j)
              g_dat(ide+2,j) = g_dat(ide,j)
              dat(ide+2,j) = dat(ide,j)
              g_dat(ide+3,j) = g_dat(ide,j)
              dat(ide+3,j) = dat(ide,j)
            ENDDO

          END IF 

        END IF open_xe

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_x

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN
        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN    ! test of both north and south on processor

          IF( jts == jds ) then

            DO j = 0, -(bdyzone-1), -1
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jds+j-1) = g_dat(i,jde+j-1)
              dat(i,jds+j-1) = dat(i,jde+j-1)
            ENDDO
            ENDDO

          END IF

          IF( jte == jde ) then

            DO j = -jstag, bdyzone
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jde+j+jstag) = g_dat(i,jds+j+jstag)
              dat(i,jde+j+jstag) = dat(i,jds+j+jstag)
            ENDDO
            ENDDO

          END IF

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jds-j) = g_dat(i,jds+j-1) 
              dat(i,jds-j) = dat(i,jds+j-1) 
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,jds-j) = - g_dat(i,jds+j) 
                dat(i,jds-j) = - dat(i,jds+j) 
              ENDDO              
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,jds-j) = g_dat(i,jds+j) 
                dat(i,jds-j) = dat(i,jds+j) 
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
              g_dat(i,jde+j-1) = g_dat(i,jde-j) 
              dat(i,jde+j-1) = dat(i,jde-j) 
            ENDDO                               
            ENDDO

          ELSE

            IF (variable == 'v' ) THEN

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,jde+j) = - g_dat(i,jde-j)
                dat(i,jde+j) = - dat(i,jde-j)
              ENDDO                               
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,jde+j) = g_dat(i,jde-j)
                dat(i,jde+j) = dat(i,jde-j)
              ENDDO                               
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye

!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jds-1) = g_dat(i,jds) 
              dat(i,jds-1) = dat(i,jds) 
              g_dat(i,jds-2) = g_dat(i,jds) 
              dat(i,jds-2) = dat(i,jds) 
              g_dat(i,jds-3) = g_dat(i,jds) 
              dat(i,jds-3) = dat(i,jds) 
            ENDDO

        ENDIF open_ys

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF  (variable /= 'v' .and. variable /= 'y' ) THEN

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jde  ) = g_dat(i,jde-1) 
              dat(i,jde  ) = dat(i,jde-1) 
              g_dat(i,jde+1) = g_dat(i,jde-1) 
              dat(i,jde+1) = dat(i,jde-1) 
              g_dat(i,jde+2) = g_dat(i,jde-1) 
              dat(i,jde+2) = dat(i,jde-1) 
            ENDDO                               

          ELSE

            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,jde+1) = g_dat(i,jde) 
              dat(i,jde+1) = dat(i,jde) 
              g_dat(i,jde+2) = g_dat(i,jde) 
              dat(i,jde+2) = dat(i,jde) 
              g_dat(i,jde+3) = g_dat(i,jde) 
              dat(i,jde+3) = dat(i,jde) 
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
             g_dat(ids+i-1,jds+j-1) = g_dat(ide+i-1,jde+j-1)
             dat(ids+i-1,jds+j-1) = dat(ide+i-1,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = 0, -(bdyzone-1), -1
           DO i = 1, bdyzone
             g_dat(ide+i+istag,jds+j-1) = g_dat(ids+i+istag,jde+j-1)
             dat(ide+i+istag,jds+j-1) = dat(ids+i+istag,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = 1, bdyzone
           DO i = 1, bdyzone
             g_dat(ide+i+istag,jde+j+jstag) = g_dat(ids+i+istag,jds+j+jstag)
             dat(ide+i+istag,jde+j+jstag) = dat(ids+i+istag,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = 1, bdyzone
           DO i = 0, -(bdyzone-1), -1
             g_dat(ids+i-1,jde+j+jstag) = g_dat(ide+i-1,jds+j+jstag)
             dat(ids+i-1,jde+j+jstag) = dat(ide+i-1,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE g_set_physical_bc2d

!-----------------------------------

   SUBROUTINE g_set_physical_bc3d( dat,g_dat, variable_in,        &
                               config_flags,                   & 
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dat,g_dat
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
          (variable == 'f') .or. (variable == 'r') .or. &
          (variable == 'p')                        ) open_bc_copy = .true.

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

      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN

        IF ( ( ids == ips ) .and. ( ide == ipe ) ) THEN  ! test if both east and west on-processor
          IF ( its == ids ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 0,-(bdyzone-1),-1
              g_dat(ids+i-1,k,j) = g_dat(ide+i-1,k,j)
              dat(ids+i-1,k,j) = dat(ide+i-1,k,j)
            ENDDO
            ENDDO
            ENDDO

          ENDIF


          IF ( ite == ide ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = -istag , bdyzone
              g_dat(ide+i+istag,k,j) = g_dat(ids+i+istag,k,j)
              dat(ide+i+istag,k,j) = dat(ids+i+istag,k,j)
            ENDDO
            ENDDO
            ENDDO

          ENDIF

        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 1, bdyzone
              g_dat(ids-i,k,j) = g_dat(ids+i-1,k,j)
              dat(ids-i,k,j) = dat(ids+i-1,k,j)
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                g_dat(ids-i,k,j) = - g_dat(ids+i,k,j)
                dat(ids-i,k,j) = - dat(ids+i,k,j)
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                g_dat(ids-i,k,j) = g_dat(ids+i,k,j)
                dat(ids-i,k,j) = dat(ids+i,k,j)
              ENDDO
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
              g_dat(ide+i-1,k,j) = g_dat(ide-i,k,j)
              dat(ide+i-1,k,j) = dat(ide-i,k,j)
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u') THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                g_dat(ide+i,k,j) = - g_dat(ide-i,k,j)
                dat(ide+i,k,j) = - dat(ide-i,k,j)
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                g_dat(ide+i,k,j) = g_dat(ide-i,k,j)
                dat(ide+i,k,j) = dat(ide-i,k,j)
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
              g_dat(ids-1,k,j) = g_dat(ids,k,j)
              dat(ids-1,k,j) = dat(ids,k,j)
              g_dat(ids-2,k,j) = g_dat(ids,k,j)
              dat(ids-2,k,j) = dat(ids,k,j)
              g_dat(ids-3,k,j) = g_dat(ids,k,j)
              dat(ids-3,k,j) = dat(ids,k,j)
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
              g_dat(ide  ,k,j) = g_dat(ide-1,k,j)
              dat(ide  ,k,j) = dat(ide-1,k,j)
              g_dat(ide+1,k,j) = g_dat(ide-1,k,j)
              dat(ide+1,k,j) = dat(ide-1,k,j)
              g_dat(ide+2,k,j) = g_dat(ide-1,k,j)
              dat(ide+2,k,j) = dat(ide-1,k,j)
            ENDDO
            ENDDO

          ELSE

!!!!!!! I am not sure about this one!  JM 20020402
            DO j = MAX(jds,jts-1)-bdyzone, MIN(jte+1,jde+jstag)+bdyzone
            DO k = kts, k_end
              g_dat(ide+1,k,j) = g_dat(ide,k,j)
              dat(ide+1,k,j) = dat(ide,k,j)
              g_dat(ide+2,k,j) = g_dat(ide,k,j)
              dat(ide+2,k,j) = dat(ide,k,j)
              g_dat(ide+3,k,j) = g_dat(ide,k,j)
              dat(ide+3,k,j) = dat(ide,k,j)
            ENDDO
            ENDDO

          END IF 

        END IF open_xe

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

      END IF periodicity_x

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN
        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN      ! test if both north and south on processor
          IF( jts == jds ) then

            DO j = 0, -(bdyzone-1), -1
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jds+j-1) = g_dat(i,k,jde+j-1)
              dat(i,k,jds+j-1) = dat(i,k,jde+j-1)
            ENDDO
            ENDDO
            ENDDO

          END IF

          IF( jte == jde ) then

            DO j = -jstag, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jde+j+jstag) = g_dat(i,k,jds+j+jstag)
              dat(i,k,jde+j+jstag) = dat(i,k,jds+j+jstag)
            ENDDO
            ENDDO
            ENDDO

          END IF

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jds-j) = g_dat(i,k,jds+j-1) 
              dat(i,k,jds-j) = dat(i,k,jds+j-1) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,k,jds-j) = - g_dat(i,k,jds+j) 
                dat(i,k,jds-j) = - dat(i,k,jds+j) 
              ENDDO              
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,k,jds-j) = g_dat(i,k,jds+j) 
                dat(i,k,jds-j) = dat(i,k,jds+j) 
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
              g_dat(i,k,jde+j-1) = g_dat(i,k,jde-j) 
              dat(i,k,jde+j-1) = dat(i,k,jde-j) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'v' ) THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,k,jde+j) = - g_dat(i,k,jde-j) 
                dat(i,k,jde+j) = - dat(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                g_dat(i,k,jde+j) = g_dat(i,k,jde-j) 
                dat(i,k,jde+j) = dat(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye
      
!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jds-1) = g_dat(i,k,jds) 
              dat(i,k,jds-1) = dat(i,k,jds) 
              g_dat(i,k,jds-2) = g_dat(i,k,jds) 
              dat(i,k,jds-2) = dat(i,k,jds) 
              g_dat(i,k,jds-3) = g_dat(i,k,jds) 
              dat(i,k,jds-3) = dat(i,k,jds) 
            ENDDO
            ENDDO

        ENDIF open_ys

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF (variable /= 'v' .and. variable /= 'y' ) THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jde  ) = g_dat(i,k,jde-1) 
              dat(i,k,jde  ) = dat(i,k,jde-1) 
              g_dat(i,k,jde+1) = g_dat(i,k,jde-1) 
              dat(i,k,jde+1) = dat(i,k,jde-1) 
              g_dat(i,k,jde+2) = g_dat(i,k,jde-1) 
              dat(i,k,jde+2) = dat(i,k,jde-1) 
            ENDDO                               
            ENDDO

          ELSE

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              g_dat(i,k,jde+1) = g_dat(i,k,jde) 
              dat(i,k,jde+1) = dat(i,k,jde) 
              g_dat(i,k,jde+2) = g_dat(i,k,jde) 
              dat(i,k,jde+2) = dat(i,k,jde) 
              g_dat(i,k,jde+3) = g_dat(i,k,jde) 
              dat(i,k,jde+3) = dat(i,k,jde) 
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
             g_dat(ids+i-1,k,jds+j-1) = g_dat(ide+i-1,k,jde+j-1)
             dat(ids+i-1,k,jds+j-1) = dat(ide+i-1,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = 0, -(bdyzone-1), -1
           DO k = kts, k_end
           DO i = 1, bdyzone
             g_dat(ide+i+istag,k,jds+j-1) = g_dat(ids+i+istag,k,jde+j-1)
             dat(ide+i+istag,k,jds+j-1) = dat(ids+i+istag,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 1, bdyzone
             g_dat(ide+i+istag,k,jde+j+jstag) = g_dat(ids+i+istag,k,jds+j+jstag)
             dat(ide+i+istag,k,jde+j+jstag) = dat(ids+i+istag,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 0, -(bdyzone-1), -1
             g_dat(ids+i-1,k,jde+j+jstag) = g_dat(ide+i-1,k,jds+j+jstag)
             dat(ids+i-1,k,jde+j+jstag) = dat(ide+i-1,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE g_set_physical_bc3d

!------------------------------------------------------------------------

   SUBROUTINE g_init_module_bc
   END SUBROUTINE g_init_module_bc

!------------------------------------------------------------------------

   SUBROUTINE g_relax_bdytend ( field, g_field, field_tend, g_field_tend, &
                                field_bdy_xs, g_field_bdy_xs,  &
                                field_bdy_xe, g_field_bdy_xe,  &
                                field_bdy_ys, g_field_bdy_ys,  &
                                field_bdy_ye, g_field_bdy_ye,  &
                                field_bdy_tend_xs, g_field_bdy_tend_xs,  &
                                field_bdy_tend_xe, g_field_bdy_tend_xe,  &
                                field_bdy_tend_ys, g_field_bdy_tend_ys,  &
                                field_bdy_tend_ye, g_field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                its,ite, jts,jte, kts,kte  )

   IMPLICIT NONE

   INTEGER,   INTENT(IN) :: ids,ide, jds,jde, kds,kde
   INTEGER,   INTENT(IN) :: ims,ime, jms,jme, kms,kme
   INTEGER,   INTENT(IN) :: ips,ipe, jps,jpe, kps,kpe
   INTEGER,   INTENT(IN) :: its,ite, jts,jte, kts,kte
   INTEGER,   INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone
   REAL,      INTENT(IN) :: dtbc
   CHARACTER, INTENT(IN) :: variable_in

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: g_field
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: field
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: g_field_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: field_tend
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_xs, g_field_bdy_xe
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_xs, field_bdy_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_ys, g_field_bdy_ye
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_ys, field_bdy_ye
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_xs, g_field_bdy_tend_xe
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_xs, field_bdy_tend_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_ys, g_field_bdy_tend_ye
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_ys, field_bdy_tend_ye
   REAL, DIMENSION( spec_bdy_width ), INTENT(IN) :: fcx, gcx
   TYPE(grid_config_rec_type),        INTENT(IN) :: config_flags


   CALL g_relax_bdytend_core ( field, g_field, field_tend, g_field_tend, &
                       field_bdy_xs, g_field_bdy_xs,  &
                       field_bdy_xe, g_field_bdy_xe,  &
                       field_bdy_ys, g_field_bdy_ys,  &
                       field_bdy_ye, g_field_bdy_ye,  &
                       field_bdy_tend_xs, g_field_bdy_tend_xs,  &
                       field_bdy_tend_xe, g_field_bdy_tend_xe,  &
                       field_bdy_tend_ys, g_field_bdy_tend_ys,  &
                       field_bdy_tend_ye, g_field_bdy_tend_ye,  &
                       variable_in, config_flags,             &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte,  & ! patch  dims
                       ims,ime, jms,jme, kms,kme )  ! dimension of the field argument

   END SUBROUTINE g_relax_bdytend

! version that allows tile-sized version of field. Note, caller should define the
! field to be -+1 of tile size in each dimension because routine is going off onto halo
! for example, see relax_bdytend in dyn_em/module_bc_em.F 
   SUBROUTINE g_relax_bdytend_tile ( field, g_field, field_tend, g_field_tend, & 
                          field_bdy_xs, g_field_bdy_xs,  &
                          field_bdy_xe, g_field_bdy_xe,  &
                          field_bdy_ys, g_field_bdy_ys,  &
                          field_bdy_ye, g_field_bdy_ye,  &
                          field_bdy_tend_xs, g_field_bdy_tend_xs,  &
                          field_bdy_tend_xe, g_field_bdy_tend_xe,  &
                          field_bdy_tend_ys, g_field_bdy_tend_ys,  &
                          field_bdy_tend_ye, g_field_bdy_tend_ye,  &
                          variable_in, config_flags,             &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its,ite, jts,jte, kts,kte,  &
                          iXs,iXe, jXs,jXe, kXs,kXe   &  ! dims of first argument
                          )

   IMPLICIT NONE

   INTEGER,   INTENT(IN) :: ids,ide, jds,jde, kds,kde
   INTEGER,   INTENT(IN) :: ims,ime, jms,jme, kms,kme
   INTEGER,   INTENT(IN) :: ips,ipe, jps,jpe, kps,kpe
   INTEGER,   INTENT(IN) :: its,ite, jts,jte, kts,kte
   INTEGER,   INTENT(IN) :: iXs,iXe, jXs,jXe, kXs,kXe
   INTEGER,   INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone
   REAL,      INTENT(IN) :: dtbc
   CHARACTER, INTENT(IN) :: variable_in

   REAL, DIMENSION(iXs:iXe, kXs:kXe, jXs:jXe ), INTENT(IN   ) :: g_field
   REAL, DIMENSION(iXs:iXe, kXs:kXe, jXs:jXe ), INTENT(IN   ) :: field
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: g_field_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: field_tend
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_xs, g_field_bdy_xe
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_xs, field_bdy_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_ys, g_field_bdy_ye
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_ys, field_bdy_ye
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_xs, g_field_bdy_tend_xe
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_xs, field_bdy_tend_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_ys, g_field_bdy_tend_ye
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_ys, field_bdy_tend_ye
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
   TYPE(grid_config_rec_type),      INTENT(IN) :: config_flags


   CALL g_relax_bdytend_core ( field, g_field, field_tend, g_field_tend, &
                       field_bdy_xs, g_field_bdy_xs,  &
                       field_bdy_xe, g_field_bdy_xe,  &
                       field_bdy_ys, g_field_bdy_ys,  &
                       field_bdy_ye, g_field_bdy_ye,  &
                       field_bdy_tend_xs, g_field_bdy_tend_xs,  &
                       field_bdy_tend_xe, g_field_bdy_tend_xe,  &
                       field_bdy_tend_ys, g_field_bdy_tend_ys,  &
                       field_bdy_tend_ye, g_field_bdy_tend_ye,  &
                       variable_in, config_flags,             &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte,  &
                       iXs,iXe, jXs,jXe, kXs,kXe )  ! dimension of the field argument

   END SUBROUTINE g_relax_bdytend_tile

!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of relax_bdytend_core in forward (tangent) mode:
!   variations   of useful results: field_tend
!   with respect to varying inputs: field field_bdy_xe field_bdy_tend_xe
!                field_tend field_bdy_xs field_bdy_tend_xs field_bdy_ye
!                field_bdy_tend_ye field_bdy_ys field_bdy_tend_ys
!   RW status of diff variables: field:in field_bdy_xe:in field_bdy_tend_xe:in
!                field_tend:in-out field_bdy_xs:in field_bdy_tend_xs:in
!                field_bdy_ye:in field_bdy_tend_ye:in field_bdy_ys:in
!                field_bdy_tend_ys:in
! domain dims
! memory dims
! patch  dims
! patch  dims
! field (1st arg) dims; might be tile or patch
SUBROUTINE G_RELAX_BDYTEND_CORE(field, fieldd, field_tend, field_tendd, &
&  field_bdy_xs, field_bdy_xsd, field_bdy_xe, field_bdy_xed, field_bdy_ys&
&  , field_bdy_ysd, field_bdy_ye, field_bdy_yed, field_bdy_tend_xs, &
&  field_bdy_tend_xsd, field_bdy_tend_xe, field_bdy_tend_xed, &
&  field_bdy_tend_ys, field_bdy_tend_ysd, field_bdy_tend_ye, &
&  field_bdy_tend_yed, variable_in, config_flags, spec_bdy_width, &
&  spec_zone, relax_zone, dtbc, fcx, gcx, ids, ide, jds, jde, kds, kde, &
&  ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, its, ite, &
&  jts, jte, kts, kte, ixs, ixe, jxs, jxe, kxs, kxe)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
  INTEGER, INTENT(IN) :: ixs, ixe, jxs, jxe, kxs, kxe
  INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone
  REAL, INTENT(IN) :: dtbc
  CHARACTER, INTENT(IN) :: variable_in
  REAL, DIMENSION(ixs:ixe, kxs:kxe, jxs:jxe), INTENT(IN) :: field
  REAL, DIMENSION(ixs:ixe, kxs:kxe, jxs:jxe), INTENT(IN) :: fieldd
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: &
&  field_tend
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: &
&  field_tendd
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_xs, field_bdy_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_xsd, field_bdy_xed
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_ys, field_bdy_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_ysd, field_bdy_yed
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_tend_xs, field_bdy_tend_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_tend_xsd, field_bdy_tend_xed
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_tend_ys, field_bdy_tend_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
&  field_bdy_tend_ysd, field_bdy_tend_yed
  REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  CHARACTER :: variable
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
  INTEGER :: b_dist, b_limit
  REAL :: fls0, fls1, fls2, fls3, fls4
  REAL :: fls0d, fls1d, fls2d, fls3d, fls4d
  LOGICAL :: periodic_x
  INTEGER :: min8
  INTEGER :: min7
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  INTEGER :: max8
  INTEGER :: max7
  INTEGER :: max6
  INTEGER :: max5
  INTEGER :: max4
  INTEGER :: max3
  INTEGER :: max2
  INTEGER :: max1
  periodic_x = config_flags%periodic_x
  variable = variable_in
  IF (variable .EQ. 'U') variable = 'u'
  IF (variable .EQ. 'V') variable = 'v'
  IF (variable .EQ. 'M') variable = 'm'
  IF (variable .EQ. 'H') variable = 'h'
  ibs = ids
  ibe = ide - 1
  IF (ite .GT. ide - 1) THEN
    itf = ide - 1
  ELSE
    itf = ite
  END IF
  jbs = jds
  jbe = jde - 1
  IF (jte .GT. jde - 1) THEN
    jtf = jde - 1
  ELSE
    jtf = jte
  END IF
  ktf = kde - 1
  IF (variable .EQ. 'u') ibe = ide
  IF (variable .EQ. 'u') THEN
    IF (ite .GT. ide) THEN
      itf = ide
    ELSE
      itf = ite
    END IF
  END IF
  IF (variable .EQ. 'v') jbe = jde
  IF (variable .EQ. 'v') THEN
    IF (jte .GT. jde) THEN
      jtf = jde
    ELSE
      jtf = jte
    END IF
  END IF
  IF (variable .EQ. 'm') ktf = kte
  IF (variable .EQ. 'h') ktf = kte
  IF (jts - jbs .LT. relax_zone) THEN
    IF (jts .LT. jbs + spec_zone) THEN
      max1 = jbs + spec_zone
    ELSE
      max1 = jts
    END IF
    IF (jtf .GT. jbs + relax_zone - 1) THEN
      min1 = jbs + relax_zone - 1
    ELSE
      min1 = jtf
    END IF
! Y-start boundary
    DO j=max1,min1
      b_dist = j - jbs
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max2 = b_limit + ibs
        ELSE
          max2 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min2 = ibe - b_limit
        ELSE
          min2 = itf
        END IF
        DO i=max2,min2
          IF (i - 1 .LT. ibs) THEN
            im1 = ibs
          ELSE
            im1 = i - 1
          END IF
          IF (i + 1 .GT. ibe) THEN
            ip1 = ibe
          ELSE
            ip1 = i + 1
          END IF
          fls0d = field_bdy_ysd(i, k, b_dist+1) + dtbc*&
&            field_bdy_tend_ysd(i, k, b_dist+1) - fieldd(i, k, j)
          fls0 = field_bdy_ys(i, k, b_dist+1) + dtbc*field_bdy_tend_ys(i&
&            , k, b_dist+1) - field(i, k, j)
          fls1d = field_bdy_ysd(im1, k, b_dist+1) + dtbc*&
&            field_bdy_tend_ysd(im1, k, b_dist+1) - fieldd(im1, k, j)
          fls1 = field_bdy_ys(im1, k, b_dist+1) + dtbc*field_bdy_tend_ys&
&            (im1, k, b_dist+1) - field(im1, k, j)
          fls2d = field_bdy_ysd(ip1, k, b_dist+1) + dtbc*&
&            field_bdy_tend_ysd(ip1, k, b_dist+1) - fieldd(ip1, k, j)
          fls2 = field_bdy_ys(ip1, k, b_dist+1) + dtbc*field_bdy_tend_ys&
&            (ip1, k, b_dist+1) - field(ip1, k, j)
          fls3d = field_bdy_ysd(i, k, b_dist) + dtbc*field_bdy_tend_ysd(&
&            i, k, b_dist) - fieldd(i, k, j-1)
          fls3 = field_bdy_ys(i, k, b_dist) + dtbc*field_bdy_tend_ys(i, &
&            k, b_dist) - field(i, k, j-1)
          fls4d = field_bdy_ysd(i, k, b_dist+2) + dtbc*&
&            field_bdy_tend_ysd(i, k, b_dist+2) - fieldd(i, k, j+1)
          fls4 = field_bdy_ys(i, k, b_dist+2) + dtbc*field_bdy_tend_ys(i&
&            , k, b_dist+2) - field(i, k, j+1)
          field_tendd(i, k, j) = field_tendd(i, k, j) + fcx(b_dist+1)*&
&            fls0d - gcx(b_dist+1)*(fls1d+fls2d+fls3d+fls4d-4.*fls0d)
          field_tend(i, k, j) = field_tend(i, k, j) + fcx(b_dist+1)*fls0&
&            - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
        END DO
      END DO
    END DO
  END IF
  IF (jbe - jtf .LT. relax_zone) THEN
    IF (jts .LT. jbe - relax_zone + 1) THEN
      max3 = jbe - relax_zone + 1
    ELSE
      max3 = jts
    END IF
    IF (jtf .GT. jbe - spec_zone) THEN
      min3 = jbe - spec_zone
    ELSE
      min3 = jtf
    END IF
! Y-end boundary
    DO j=max3,min3
      b_dist = jbe - j
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max4 = b_limit + ibs
        ELSE
          max4 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min4 = ibe - b_limit
        ELSE
          min4 = itf
        END IF
        DO i=max4,min4
          IF (i - 1 .LT. ibs) THEN
            im1 = ibs
          ELSE
            im1 = i - 1
          END IF
          IF (i + 1 .GT. ibe) THEN
            ip1 = ibe
          ELSE
            ip1 = i + 1
          END IF
          fls0d = field_bdy_yed(i, k, b_dist+1) + dtbc*&
&            field_bdy_tend_yed(i, k, b_dist+1) - fieldd(i, k, j)
          fls0 = field_bdy_ye(i, k, b_dist+1) + dtbc*field_bdy_tend_ye(i&
&            , k, b_dist+1) - field(i, k, j)
          fls1d = field_bdy_yed(im1, k, b_dist+1) + dtbc*&
&            field_bdy_tend_yed(im1, k, b_dist+1) - fieldd(im1, k, j)
          fls1 = field_bdy_ye(im1, k, b_dist+1) + dtbc*field_bdy_tend_ye&
&            (im1, k, b_dist+1) - field(im1, k, j)
          fls2d = field_bdy_yed(ip1, k, b_dist+1) + dtbc*&
&            field_bdy_tend_yed(ip1, k, b_dist+1) - fieldd(ip1, k, j)
          fls2 = field_bdy_ye(ip1, k, b_dist+1) + dtbc*field_bdy_tend_ye&
&            (ip1, k, b_dist+1) - field(ip1, k, j)
          fls3d = field_bdy_yed(i, k, b_dist) + dtbc*field_bdy_tend_yed(&
&            i, k, b_dist) - fieldd(i, k, j+1)
          fls3 = field_bdy_ye(i, k, b_dist) + dtbc*field_bdy_tend_ye(i, &
&            k, b_dist) - field(i, k, j+1)
          fls4d = field_bdy_yed(i, k, b_dist+2) + dtbc*&
&            field_bdy_tend_yed(i, k, b_dist+2) - fieldd(i, k, j-1)
          fls4 = field_bdy_ye(i, k, b_dist+2) + dtbc*field_bdy_tend_ye(i&
&            , k, b_dist+2) - field(i, k, j-1)
          field_tendd(i, k, j) = field_tendd(i, k, j) + fcx(b_dist+1)*&
&            fls0d - gcx(b_dist+1)*(fls1d+fls2d+fls3d+fls4d-4.*fls0d)
          field_tend(i, k, j) = field_tend(i, k, j) + fcx(b_dist+1)*fls0&
&            - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
        END DO
      END DO
    END DO
  END IF
  IF (.NOT.periodic_x) THEN
    IF (its - ibs .LT. relax_zone) THEN
      IF (its .LT. ibs + spec_zone) THEN
        max5 = ibs + spec_zone
      ELSE
        max5 = its
      END IF
      IF (itf .GT. ibs + relax_zone - 1) THEN
        min5 = ibs + relax_zone - 1
      ELSE
        min5 = itf
      END IF
! X-start boundary
      DO i=max5,min5
        b_dist = i - ibs
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max6 = b_dist + jbs + 1
          ELSE
            max6 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min6 = jbe - b_dist - 1
          ELSE
            min6 = jtf
          END IF
          DO j=max6,min6
            fls0d = field_bdy_xsd(j, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xsd(j, k, b_dist+1) - fieldd(i, k, j)
            fls0 = field_bdy_xs(j, k, b_dist+1) + dtbc*field_bdy_tend_xs&
&              (j, k, b_dist+1) - field(i, k, j)
            fls1d = field_bdy_xsd(j-1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xsd(j-1, k, b_dist+1) - fieldd(i, k, j-1)
            fls1 = field_bdy_xs(j-1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xs(j-1, k, b_dist+1) - field(i, k, j-1)
            fls2d = field_bdy_xsd(j+1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xsd(j+1, k, b_dist+1) - fieldd(i, k, j+1)
            fls2 = field_bdy_xs(j+1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xs(j+1, k, b_dist+1) - field(i, k, j+1)
            fls3d = field_bdy_xsd(j, k, b_dist) + dtbc*&
&              field_bdy_tend_xsd(j, k, b_dist) - fieldd(i-1, k, j)
            fls3 = field_bdy_xs(j, k, b_dist) + dtbc*field_bdy_tend_xs(j&
&              , k, b_dist) - field(i-1, k, j)
            fls4d = field_bdy_xsd(j, k, b_dist+2) + dtbc*&
&              field_bdy_tend_xsd(j, k, b_dist+2) - fieldd(i+1, k, j)
            fls4 = field_bdy_xs(j, k, b_dist+2) + dtbc*field_bdy_tend_xs&
&              (j, k, b_dist+2) - field(i+1, k, j)
            field_tendd(i, k, j) = field_tendd(i, k, j) + fcx(b_dist+1)*&
&              fls0d - gcx(b_dist+1)*(fls1d+fls2d+fls3d+fls4d-4.*fls0d)
            field_tend(i, k, j) = field_tend(i, k, j) + fcx(b_dist+1)*&
&              fls0 - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
          END DO
        END DO
      END DO
    END IF
    IF (ibe - itf .LT. relax_zone) THEN
      IF (its .LT. ibe - relax_zone + 1) THEN
        max7 = ibe - relax_zone + 1
      ELSE
        max7 = its
      END IF
      IF (itf .GT. ibe - spec_zone) THEN
        min7 = ibe - spec_zone
      ELSE
        min7 = itf
      END IF
! X-end boundary
      DO i=max7,min7
        b_dist = ibe - i
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max8 = b_dist + jbs + 1
          ELSE
            max8 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min8 = jbe - b_dist - 1
          ELSE
            min8 = jtf
          END IF
          DO j=max8,min8
            fls0d = field_bdy_xed(j, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xed(j, k, b_dist+1) - fieldd(i, k, j)
            fls0 = field_bdy_xe(j, k, b_dist+1) + dtbc*field_bdy_tend_xe&
&              (j, k, b_dist+1) - field(i, k, j)
            fls1d = field_bdy_xed(j-1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xed(j-1, k, b_dist+1) - fieldd(i, k, j-1)
            fls1 = field_bdy_xe(j-1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xe(j-1, k, b_dist+1) - field(i, k, j-1)
            fls2d = field_bdy_xed(j+1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xed(j+1, k, b_dist+1) - fieldd(i, k, j+1)
            fls2 = field_bdy_xe(j+1, k, b_dist+1) + dtbc*&
&              field_bdy_tend_xe(j+1, k, b_dist+1) - field(i, k, j+1)
            fls3d = field_bdy_xed(j, k, b_dist) + dtbc*&
&              field_bdy_tend_xed(j, k, b_dist) - fieldd(i+1, k, j)
            fls3 = field_bdy_xe(j, k, b_dist) + dtbc*field_bdy_tend_xe(j&
&              , k, b_dist) - field(i+1, k, j)
            fls4d = field_bdy_xed(j, k, b_dist+2) + dtbc*&
&              field_bdy_tend_xed(j, k, b_dist+2) - fieldd(i-1, k, j)
            fls4 = field_bdy_xe(j, k, b_dist+2) + dtbc*field_bdy_tend_xe&
&              (j, k, b_dist+2) - field(i-1, k, j)
            field_tendd(i, k, j) = field_tendd(i, k, j) + fcx(b_dist+1)*&
&              fls0d - gcx(b_dist+1)*(fls1d+fls2d+fls3d+fls4d-4.*fls0d)
            field_tend(i, k, j) = field_tend(i, k, j) + fcx(b_dist+1)*&
&              fls0 - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
          END DO
        END DO
      END DO
    END IF
  END IF
END SUBROUTINE G_RELAX_BDYTEND_CORE
!------------------------------------------------------------------------

   SUBROUTINE g_spec_bdytend ( field_tend, g_field_tend,     &
                               field_bdy_xs, g_field_bdy_xs, &
                               field_bdy_xe, g_field_bdy_xe, &
                               field_bdy_ys, g_field_bdy_ys, &
                               field_bdy_ye, g_field_bdy_ye, &
                               field_bdy_tend_xs, g_field_bdy_tend_xs, &
                               field_bdy_tend_xe, g_field_bdy_tend_xe, &
                               field_bdy_tend_ys, g_field_bdy_tend_ys, &
                               field_bdy_tend_ye, g_field_bdy_tend_ye, &
                               variable_in, config_flags, & 
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  spec_bdy_width is only used to dimension the boundary arrays.
!  spec_zone is the width of the outer specified b.c.s that are set here.

      IMPLICIT NONE

      INTEGER,   INTENT(IN) :: ids,ide, jds,jde, kds,kde
      INTEGER,   INTENT(IN) :: ims,ime, jms,jme, kms,kme
      INTEGER,   INTENT(IN) :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,   INTENT(IN) :: its,ite, jts,jte, kts,kte
      INTEGER,   INTENT(IN) :: spec_bdy_width, spec_zone
      CHARACTER, INTENT(IN) :: variable_in

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: g_field_tend
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: field_tend

      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_xs, g_field_bdy_xe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_ys, g_field_bdy_ye 
      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_xs, g_field_bdy_tend_xe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_field_bdy_tend_ys, g_field_bdy_tend_ye 
      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_xs, field_bdy_xe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_ys, field_bdy_ye 
      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: field_bdy_tend_ys, field_bdy_tend_ye 
      TYPE(grid_config_rec_type), INTENT(IN) :: config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x


      periodic_x = config_flags%periodic_x

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
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              g_field_tend(i,k,j) = g_field_bdy_tend_ys(i, k, b_dist+1)
              field_tend(i,k,j) = field_bdy_tend_ys(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              g_field_tend(i,k,j) = g_field_bdy_tend_ye(i, k, b_dist+1)
              field_tend(i,k,j) = field_bdy_tend_ye(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              g_field_tend(i,k,j) = g_field_bdy_tend_xs(j, k, b_dist+1)
              field_tend(i,k,j) = field_bdy_tend_xs(j, k, b_dist+1)
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
              g_field_tend(i,k,j) = g_field_bdy_tend_xe(j, k, b_dist+1)
              field_tend(i,k,j) = field_bdy_tend_xe(j, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_spec_bdytend

!------------------------------------------------------------------------

   SUBROUTINE g_couple_bdy ( field, g_field,  &
                             variable_in, config_flags, & 
                             spec_zone,       &
                             mu, g_mu, msf,   &
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
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: g_mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msf
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: g_field
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'T') variable = 't'
      IF (variable == 'H') variable = 'h'
      IF (variable == 'W') variable = 'w'

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
      IF (variable == 'h') ktf = kte
      IF (variable == 'w') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)*mu(i,j) + field(i,k,j)*g_mu(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)
              else 
                 g_field(i,k,j) = (g_field(i,k,j)*mu(i,j)+field(i,k,j)*g_mu(i,j)) / msf(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)*mu(i,j) + field(i,k,j)*g_mu(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)
              else 
                 g_field(i,k,j) = (g_field(i,k,j)*mu(i,j)+field(i,k,j)*g_mu(i,j)) / msf(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)*mu(i,j) + field(i,k,j)*g_mu(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)
              else 
                 g_field(i,k,j) = (g_field(i,k,j)*mu(i,j)+field(i,k,j)*g_mu(i,j)) / msf(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)/msf(i,j)
              end if
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
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)*mu(i,j) + field(i,k,j)*g_mu(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)
              else 
                 g_field(i,k,j) = (g_field(i,k,j)*mu(i,j)+field(i,k,j)*g_mu(i,j)) / msf(i,j)
                 field(i,k,j) = field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_couple_bdy

!------------------------------------------------------------------------

   SUBROUTINE g_uncouple_bdy(  field, g_field,  &
                               variable_in, config_flags, & 
                               spec_zone,       &
                               mu, g_mu, msf,   &
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
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: g_mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msf
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: g_field
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'T') variable = 't'
      IF (variable == 'H') variable = 'h'
      IF (variable == 'W') variable = 'w'

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
      IF (variable == 'h') ktf = kte
      IF (variable == 'w') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)/mu(i,j) &
                                - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j))
                 field(i,k,j) = field(i,k,j)/mu(i,j)
              else 
                 g_field(i,k,j) = ( g_field(i,k,j)/mu(i,j) &
                                  - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j)) ) &
                                * msf(i,j)
                 field(i,k,j) = field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)/mu(i,j) &
                                - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j))
                 field(i,k,j) = field(i,k,j)/mu(i,j)
              else 
                 g_field(i,k,j) = ( g_field(i,k,j)/mu(i,j) &
                                  - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j)) ) &
                                * msf(i,j)
                 field(i,k,j) = field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)/mu(i,j) &
                                - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j))
                 field(i,k,j) = field(i,k,j)/mu(i,j)
              else 
                 g_field(i,k,j) = ( g_field(i,k,j)/mu(i,j) &
                                  - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j)) ) &
                                * msf(i,j)
                 field(i,k,j) = field(i,k,j)/mu(i,j)*msf(i,j)
              end if
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
              if (variable == 't' .or. variable == 'h') then 
                 g_field(i,k,j) = g_field(i,k,j)/mu(i,j) &
                                - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j))
                 field(i,k,j) = field(i,k,j)/mu(i,j)
              else 
                 g_field(i,k,j) = ( g_field(i,k,j)/mu(i,j) &
                                  - field(i,k,j)*g_mu(i,j)/(mu(i,j)*mu(i,j)) ) &
                                * msf(i,j)
                 field(i,k,j) = field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_uncouple_bdy 
!------------------------------------------------------------------------

   SUBROUTINE g_spec_bdyupdate(  field, g_field,  &
                                 field_tend, g_field_tend, dt,            &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: g_field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: g_field_tend
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

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
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              g_field(i,k,j) = g_field(i,k,j) + dt*g_field_tend(i,k,j) 
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              g_field(i,k,j) = g_field(i,k,j) + dt*g_field_tend(i,k,j) 
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              g_field(i,k,j) = g_field(i,k,j) + dt*g_field_tend(i,k,j) 
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
              g_field(i,k,j) = g_field(i,k,j) + dt*g_field_tend(i,k,j) 
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_spec_bdyupdate
!------------------------------------------------------------------------
!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.10 (r5498) - 20 Jan 2015 09:48
!
!  Differentiation of spec_bdy_final in forward (tangent) mode:
!   variations   of useful results: field
!   with respect to varying inputs: field field_bdy_xe field_bdy_tend_xe
!                field_bdy_xs field_bdy_tend_xs field_bdy_ye field_bdy_tend_ye
!                field_bdy_ys field_bdy_tend_ys mu
!   RW status of diff variables: field:in-out field_bdy_xe:in field_bdy_tend_xe:in
!                field_bdy_xs:in field_bdy_tend_xs:in field_bdy_ye:in
!                field_bdy_tend_ye:in field_bdy_ys:in field_bdy_tend_ys:in
!                mu:in
! domain dims
! memory dims
! patch  dims
SUBROUTINE g_SPEC_BDY_FINAL(field, fieldd, mu, mud, msf, field_bdy_xs, &
& field_bdy_xsd, field_bdy_xe, field_bdy_xed, field_bdy_ys, &
& field_bdy_ysd, field_bdy_ye, field_bdy_yed, field_bdy_tend_xs, &
& field_bdy_tend_xsd, field_bdy_tend_xe, field_bdy_tend_xed, &
& field_bdy_tend_ys, field_bdy_tend_ysd, field_bdy_tend_ye, &
& field_bdy_tend_yed, variable_in, config_flags, spec_bdy_width, &
& spec_zone, dtbc, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
& , kme, ips, ipe, jps, jpe, kps, kpe, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
  INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone
  REAL, INTENT(IN) :: dtbc
  CHARACTER, INTENT(IN) :: variable_in
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: field
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: fieldd
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mu, msf
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mud
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_xs, field_bdy_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_xsd, field_bdy_xed
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_ys, field_bdy_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_ysd, field_bdy_yed
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_xs, field_bdy_tend_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_xsd, field_bdy_tend_xed
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_ys, field_bdy_tend_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_ysd, field_bdy_tend_yed
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  CHARACTER :: variable
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
  INTEGER :: b_dist, b_limit
  REAL :: bfield, xmsf, xmu
  REAL :: bfieldd, xmud
  LOGICAL :: periodic_x, msfcouple, mucouple
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  INTEGER :: max6
  INTEGER :: max5
  INTEGER :: max4
  INTEGER :: max3
  INTEGER :: max2
  INTEGER :: max1
  periodic_x = config_flags%periodic_x
  variable = variable_in
  IF (variable .EQ. 'U') variable = 'u'
  IF (variable .EQ. 'V') variable = 'v'
  IF (variable .EQ. 'W') variable = 'w'
  IF (variable .EQ. 'M') variable = 'm'
  IF (variable .EQ. 'T') variable = 't'
  IF (variable .EQ. 'H') variable = 'h'
  ibs = ids
  ibe = ide - 1
  IF (ite .GT. ide - 1) THEN
    itf = ide - 1
  ELSE
    itf = ite
  END IF
  jbs = jds
  jbe = jde - 1
  IF (jte .GT. jde - 1) THEN
    jtf = jde - 1
  ELSE
    jtf = jte
  END IF
  ktf = kde - 1
  IF (variable .EQ. 'u') ibe = ide
  IF (variable .EQ. 'u') THEN
    IF (ite .GT. ide) THEN
      itf = ide
    ELSE
      itf = ite
    END IF
  END IF
  IF (variable .EQ. 'v') jbe = jde
  IF (variable .EQ. 'v') THEN
    IF (jte .GT. jde) THEN
      jtf = jde
    ELSE
      jtf = jte
    END IF
  END IF
  IF (variable .EQ. 'm') ktf = kde
  IF (variable .EQ. 'h') ktf = kde
  IF (variable .EQ. 'w') ktf = kde
  msfcouple = .false.
  mucouple = .true.
  IF ((variable .EQ. 'u' .OR. variable .EQ. 'v') .OR. variable .EQ. 'w'&
& ) msfcouple = .true.
  IF (variable .EQ. 'm') mucouple = .false.
  xmsf = 1.
  xmu = 1.
  IF (jts - jbs .LT. spec_zone) THEN
    IF (jtf .GT. jbs + spec_zone - 1) THEN
      min1 = jbs + spec_zone - 1
      xmud = 0.0
    ELSE
      min1 = jtf
      xmud = 0.0
    END IF
! Y-start boundary
    DO j=jts,min1
      b_dist = j - jbs
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max1 = b_limit + ibs
        ELSE
          max1 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min3 = ibe - b_limit
        ELSE
          min3 = itf
        END IF
        DO i=max1,min3
          bfieldd = field_bdy_ysd(i, k, b_dist+1) + dtbc*&
&           field_bdy_tend_ysd(i, k, b_dist+1)
          bfield = field_bdy_ys(i, k, b_dist+1) + dtbc*field_bdy_tend_ys&
&           (i, k, b_dist+1)
          IF (msfcouple) xmsf = msf(i, j)
          IF (mucouple) THEN
            xmud = mud(i, j)
            xmu = mu(i, j)
          END IF
          fieldd(i, k, j) = (xmsf*bfieldd*xmu-xmsf*bfield*xmud)/xmu**2
          field(i, k, j) = xmsf*bfield/xmu
        END DO
      END DO
    END DO
  ELSE
    xmud = 0.0
  END IF
  IF (jbe - jtf .LT. spec_zone) THEN
    IF (jts .LT. jbe - spec_zone + 1) THEN
      max2 = jbe - spec_zone + 1
    ELSE
      max2 = jts
    END IF
! Y-end boundary
    DO j=max2,jtf
      b_dist = jbe - j
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max3 = b_limit + ibs
        ELSE
          max3 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min4 = ibe - b_limit
        ELSE
          min4 = itf
        END IF
        DO i=max3,min4
          bfieldd = field_bdy_yed(i, k, b_dist+1) + dtbc*&
&           field_bdy_tend_yed(i, k, b_dist+1)
          bfield = field_bdy_ye(i, k, b_dist+1) + dtbc*field_bdy_tend_ye&
&           (i, k, b_dist+1)
          IF (msfcouple) xmsf = msf(i, j)
          IF (mucouple) THEN
            xmud = mud(i, j)
            xmu = mu(i, j)
          END IF
          fieldd(i, k, j) = (xmsf*bfieldd*xmu-xmsf*bfield*xmud)/xmu**2
          field(i, k, j) = xmsf*bfield/xmu
        END DO
      END DO
    END DO
  END IF
  IF (.NOT.periodic_x) THEN
    IF (its - ibs .LT. spec_zone) THEN
      IF (itf .GT. ibs + spec_zone - 1) THEN
        min2 = ibs + spec_zone - 1
      ELSE
        min2 = itf
      END IF
! X-start boundary
      DO i=its,min2
        b_dist = i - ibs
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max4 = b_dist + jbs + 1
          ELSE
            max4 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min5 = jbe - b_dist - 1
          ELSE
            min5 = jtf
          END IF
          DO j=max4,min5
            bfieldd = field_bdy_xsd(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xsd(j, k, b_dist+1)
            bfield = field_bdy_xs(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xs(j, k, b_dist+1)
            IF (msfcouple) xmsf = msf(i, j)
            IF (mucouple) THEN
              xmud = mud(i, j)
              xmu = mu(i, j)
            END IF
            fieldd(i, k, j) = (xmsf*bfieldd*xmu-xmsf*bfield*xmud)/xmu**2
            field(i, k, j) = xmsf*bfield/xmu
          END DO
        END DO
      END DO
    END IF
    IF (ibe - itf .LT. spec_zone) THEN
      IF (its .LT. ibe - spec_zone + 1) THEN
        max5 = ibe - spec_zone + 1
      ELSE
        max5 = its
      END IF
! X-end boundary
      DO i=max5,itf
        b_dist = ibe - i
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max6 = b_dist + jbs + 1
          ELSE
            max6 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min6 = jbe - b_dist - 1
          ELSE
            min6 = jtf
          END IF
          DO j=max6,min6
            bfieldd = field_bdy_xed(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xed(j, k, b_dist+1)
            bfield = field_bdy_xe(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xe(j, k, b_dist+1)
            IF (msfcouple) xmsf = msf(i, j)
            IF (mucouple) THEN
              xmud = mud(i, j)
              xmu = mu(i, j)
            END IF
            fieldd(i, k, j) = (xmsf*bfieldd*xmu-xmsf*bfield*xmud)/xmu**2
            field(i, k, j) = xmsf*bfield/xmu
          END DO
        END DO
      END DO
    END IF
  END IF
END SUBROUTINE g_SPEC_BDY_FINAL
!------------------------------------------------------------------------

   SUBROUTINE g_zero_grad_bdy (  field, g_field,                    &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: g_field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

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
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              g_field(i,k,j) = g_field(i_inner,k,jbs+spec_zone)
              field(i,k,j) = field(i_inner,k,jbs+spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              g_field(i,k,j) = g_field(i_inner,k,jbe-spec_zone)
              field(i,k,j) = field(i_inner,k,jbe-spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              g_field(i,k,j) = g_field(ibs+spec_zone,k,j_inner)
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
              g_field(i,k,j) = g_field(ibe-spec_zone,k,j_inner)
              field(i,k,j) = field(ibe-spec_zone,k,j_inner)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_zero_grad_bdy
!------------------------------------------------------------------------

   SUBROUTINE g_flow_dep_bdy ( field, g_field,     &
                               u, v, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: ids,ide, jds,jde, kds,kde
      INTEGER, INTENT(IN) :: ims,ime, jms,jme, kms,kme
      INTEGER, INTENT(IN) :: ips,ipe, jps,jpe, kps,kpe
      INTEGER, INTENT(IN) :: its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN) :: spec_zone

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: g_field
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: field
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: u
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: v
      TYPE(grid_config_rec_type), INTENT(IN) :: config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x


      periodic_x = config_flags%periodic_x

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
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j) .lt. 0.)THEN
                g_field(i,k,j) = g_field(i_inner,k,jbs+spec_zone)
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                g_field(i,k,j) = 0.
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
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j+1) .gt. 0.)THEN
                g_field(i,k,j) = g_field(i_inner,k,jbe-spec_zone)
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                g_field(i,k,j) = 0.
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                g_field(i,k,j) = g_field(ibs+spec_zone,k,j_inner)
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                g_field(i,k,j) = 0.
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
                g_field(i,k,j) = g_field(ibe-spec_zone,k,j_inner)
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                g_field(i,k,j) = 0.
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_flow_dep_bdy

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.6 (r4165) - 21 sep 2011 20:54
!
!  Differentiation of flow_dep_bdy_qnn in forward (tangent) mode:
!   variations   of useful results: field
!   with respect to varying inputs: field
!   RW status of diff variables: field:in-out
! domain dims
! memory dims
! patch  dims
SUBROUTINE G_FLOW_DEP_BDY_QNN(field, fieldd, u, v, config_flags, &
&  spec_zone, ccn_conc, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme&
&  , ips, ipe, jps, jpe, kps, kpe, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
  INTEGER, INTENT(IN) :: spec_zone
  REAL,    INTENT(IN) :: ccn_conc ! RAS
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: field
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: fieldd
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: v
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, &
&  j_inner
  INTEGER :: b_dist, b_limit
  LOGICAL :: periodic_x
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  INTRINSIC MAX
  INTRINSIC MIN
  INTEGER :: max6
  INTEGER :: max5
  INTEGER :: max4
  INTEGER :: max3
  INTEGER :: max2
  INTEGER :: max1
  periodic_x = config_flags%periodic_x
  ibs = ids
  ibe = ide - 1
  IF (ite .GT. ide - 1) THEN
    itf = ide - 1
  ELSE
    itf = ite
  END IF
  jbs = jds
  jbe = jde - 1
  IF (jte .GT. jde - 1) THEN
    jtf = jde - 1
  ELSE
    jtf = jte
  END IF
  ktf = kde - 1
  IF (jts - jbs .LT. spec_zone) THEN
    IF (jtf .GT. jbs + spec_zone - 1) THEN
      min1 = jbs + spec_zone - 1
    ELSE
      min1 = jtf
    END IF
! Y-start boundary
    DO j=jts,min1
      b_dist = j - jbs
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max1 = b_limit + ibs
        ELSE
          max1 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min3 = ibe - b_limit
        ELSE
          min3 = itf
        END IF
        DO i=max1,min3
          IF (i .LT. ibs + spec_zone) THEN
            i_inner = ibs + spec_zone
          ELSE
            i_inner = i
          END IF
          IF (i_inner .GT. ibe - spec_zone) THEN
            i_inner = ibe - spec_zone
          ELSE
            i_inner = i_inner
          END IF
          IF (periodic_x) i_inner = i
          IF (v(i, k, j) .LT. 0.) THEN
            fieldd(i, k, j) = fieldd(i_inner, k, jbs+spec_zone)
            field(i, k, j) = field(i_inner, k, jbs+spec_zone)
          ELSE
            fieldd(i, k, j) = 0.0
            field(i, k, j) = ccn_conc
          END IF
        END DO
      END DO
    END DO
  END IF
  IF (jbe - jtf .LT. spec_zone) THEN
    IF (jts .LT. jbe - spec_zone + 1) THEN
      max2 = jbe - spec_zone + 1
    ELSE
      max2 = jts
    END IF
! Y-end boundary
    DO j=max2,jtf
      b_dist = jbe - j
      b_limit = b_dist
      IF (periodic_x) b_limit = 0
      DO k=kts,ktf
        IF (its .LT. b_limit + ibs) THEN
          max3 = b_limit + ibs
        ELSE
          max3 = its
        END IF
        IF (itf .GT. ibe - b_limit) THEN
          min4 = ibe - b_limit
        ELSE
          min4 = itf
        END IF
        DO i=max3,min4
          IF (i .LT. ibs + spec_zone) THEN
            i_inner = ibs + spec_zone
          ELSE
            i_inner = i
          END IF
          IF (i_inner .GT. ibe - spec_zone) THEN
            i_inner = ibe - spec_zone
          ELSE
            i_inner = i_inner
          END IF
          IF (periodic_x) i_inner = i
          IF (v(i, k, j+1) .GT. 0.) THEN
            fieldd(i, k, j) = fieldd(i_inner, k, jbe-spec_zone)
            field(i, k, j) = field(i_inner, k, jbe-spec_zone)
          ELSE
            fieldd(i, k, j) = 0.0
            field(i, k, j) = ccn_conc
          END IF
        END DO
      END DO
    END DO
  END IF
  IF (.NOT.periodic_x) THEN
    IF (its - ibs .LT. spec_zone) THEN
      IF (itf .GT. ibs + spec_zone - 1) THEN
        min2 = ibs + spec_zone - 1
      ELSE
        min2 = itf
      END IF
! X-start boundary
      DO i=its,min2
        b_dist = i - ibs
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max4 = b_dist + jbs + 1
          ELSE
            max4 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min5 = jbe - b_dist - 1
          ELSE
            min5 = jtf
          END IF
          DO j=max4,min5
            IF (j .LT. jbs + spec_zone) THEN
              j_inner = jbs + spec_zone
            ELSE
              j_inner = j
            END IF
            IF (j_inner .GT. jbe - spec_zone) THEN
              j_inner = jbe - spec_zone
            ELSE
              j_inner = j_inner
            END IF
            IF (u(i, k, j) .LT. 0.) THEN
              fieldd(i, k, j) = fieldd(ibs+spec_zone, k, j_inner)
              field(i, k, j) = field(ibs+spec_zone, k, j_inner)
            ELSE
              fieldd(i, k, j) = 0.0
              field(i, k, j) = ccn_conc
            END IF
          END DO
        END DO
      END DO
    END IF
    IF (ibe - itf .LT. spec_zone) THEN
      IF (its .LT. ibe - spec_zone + 1) THEN
        max5 = ibe - spec_zone + 1
      ELSE
        max5 = its
      END IF
! X-end boundary
      DO i=max5,itf
        b_dist = ibe - i
        DO k=kts,ktf
          IF (jts .LT. b_dist + jbs + 1) THEN
            max6 = b_dist + jbs + 1
          ELSE
            max6 = jts
          END IF
          IF (jtf .GT. jbe - b_dist - 1) THEN
            min6 = jbe - b_dist - 1
          ELSE
            min6 = jtf
          END IF
          DO j=max6,min6
            IF (j .LT. jbs + spec_zone) THEN
              j_inner = jbs + spec_zone
            ELSE
              j_inner = j
            END IF
            IF (j_inner .GT. jbe - spec_zone) THEN
              j_inner = jbe - spec_zone
            ELSE
              j_inner = j_inner
            END IF
            IF (u(i+1, k, j) .GT. 0.) THEN
              fieldd(i, k, j) = fieldd(ibe-spec_zone, k, j_inner)
              field(i, k, j) = field(ibe-spec_zone, k, j_inner)
            ELSE
              fieldd(i, k, j) = 0.0
              field(i, k, j) = ccn_conc
            END IF
          END DO
        END DO
      END DO
    END IF
  END IF
END SUBROUTINE G_FLOW_DEP_BDY_QNN

END MODULE g_module_bc
