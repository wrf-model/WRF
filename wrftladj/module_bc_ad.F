!WRF+/AD:MODEL_LAYER:BOUNDARY
!Created by Ning Pan, 2010-08
!

MODULE a_module_bc

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

!------------------------------------------------------------------------

   SUBROUTINE a_set_physical_bc2d( a_dat, variable_in,  &
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

      REAL,  DIMENSION( ims:ime , jms:jme ) :: a_dat
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, istag, jstag, itime

      LOGICAL  :: debug, open_bc_copy 

      real :: a_aux

!------------

      a_aux = 0.0

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

!  fix corners for doubly periodic domains

      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = bdyzone,1,-1
           DO i = -(bdyzone-1),0,1
             a_aux = a_dat(ids+i-1,jde+j+jstag)
             a_dat(ids+i-1,jde+j+jstag) = 0.0
             a_dat(ide+i-1,jds+j+jstag) = a_dat(ide+i-1,jds+j+jstag) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = bdyzone,1,-1
           DO i = bdyzone,1,-1
             a_aux = a_dat(ide+i+istag,jde+j+jstag)
             a_dat(ide+i+istag,jde+j+jstag) = 0.0
             a_dat(ids+i+istag,jds+j+jstag) = a_dat(ids+i+istag,jds+j+jstag) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = -(bdyzone-1),0,1
           DO i = bdyzone,1,-1
             a_aux = a_dat(ide+i+istag,jds+j-1)
             a_dat(ide+i+istag,jds+j-1) = 0.0
             a_dat(ids+i+istag,jde+j-1) = a_dat(ids+i+istag,jde+j-1) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jts == jds) ) THEN  ! lower left corner fill
           DO j = -(bdyzone-1),0,1
           DO i = -(bdyzone-1),0,1
             a_aux = a_dat(ids+i-1,jds+j-1)
             a_dat(ids+i-1,jds+j-1) = 0.0
             a_dat(ide+i-1,jde+j-1) = a_dat(ide+i-1,jde+j-1) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
         END IF

       END IF

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN

        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN    ! test of both north and south on processor

          IF( jte == jde ) then

            DO j = bdyzone,-jstag,-1
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jde+j+jstag)
              a_dat(i,jde+j+jstag) = 0.0
              a_dat(i,jds+j+jstag) = a_dat(i,jds+j+jstag) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          END IF

          IF( jts == jds ) then

            DO j = -(bdyzone-1),0,1
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jds+j-1)
              a_dat(i,jds+j-1) = 0.0
              a_dat(i,jde+j-1) = a_dat(i,jde+j-1) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          END IF

        END IF

      ELSE

!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF  (variable /= 'v' .and. variable /= 'y' ) THEN

            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jde  )
              a_dat(i,jde  ) = 0.0
              a_dat(i,jde-1) = a_dat(i,jde-1) + a_aux
              a_aux = a_dat(i,jde+1)
              a_dat(i,jde+1) = 0.0
              a_dat(i,jde-1) = a_dat(i,jde-1) + a_aux
              a_aux = a_dat(i,jde+2)
              a_dat(i,jde+2) = 0.0
              a_dat(i,jde-1) = a_dat(i,jde-1) + a_aux
              a_aux = 0.0
            ENDDO                               

          ELSE

            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1
              a_aux = a_dat(i,jde+1)
              a_dat(i,jde+1) = 0.0
              a_dat(i,jde  ) = a_dat(i,jde  ) + a_aux
              a_aux = a_dat(i,jde+2)
              a_dat(i,jde+2) = 0.0
              a_dat(i,jde  ) = a_dat(i,jde  ) + a_aux
              a_aux = a_dat(i,jde+3)
              a_dat(i,jde+3) = 0.0
              a_dat(i,jde  ) = a_dat(i,jde  ) + a_aux
              a_aux = 0.0
            ENDDO                               

          ENDIF

        END IF open_ye

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jds-1)
              a_dat(i,jds-1) = 0.0
              a_dat(i,jds) = a_dat(i,jds) + a_aux
              a_aux = a_dat(i,jds-2)
              a_dat(i,jds-2) = 0.0
              a_dat(i,jds) = a_dat(i,jds) + a_aux
              a_aux = a_dat(i,jds-3)
              a_dat(i,jds-3) = 0.0
              a_dat(i,jds) = a_dat(i,jds) + a_aux
              a_aux = 0.0
            ENDDO

        ENDIF open_ys

!  now the symmetry boundary at ye

        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = bdyzone,1,-1
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jde+j-1)
              a_dat(i,jde+j-1) = 0.0
              a_dat(i,jde-j) = a_dat(i,jde-j) + a_aux
              a_aux = 0.0
            ENDDO                               
            ENDDO

          ELSE

            IF (variable == 'v' ) THEN

              DO j = bdyzone,1,-1
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1
                a_aux = - a_dat(i,jde+j)
                a_dat(i,jde+j) = 0.0
                a_dat(i,jde-j) = a_dat(i,jde-j) + a_aux
                a_aux = 0.0
              ENDDO                               
              ENDDO

            ELSE

              DO j = bdyzone,1,-1
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = a_dat(i,jde+j)
                a_dat(i,jde+j) = 0.0
                a_dat(i,jde-j) = a_dat(i,jde-j) + a_aux
                a_aux = 0.0
              ENDDO                               
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = bdyzone,1,-1
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,jds-j)
              a_dat(i,jds-j) = 0.0
              a_dat(i,jds+j-1) = a_dat(i,jds+j-1) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = bdyzone,1,-1
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = - a_dat(i,jds-j)
                a_dat(i,jds-j) = 0.0
                a_dat(i,jds+j) = a_dat(i,jds+j) + a_aux
                a_aux = 0.0
              ENDDO              
              ENDDO

            ELSE

              DO j = bdyzone,1,-1
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = a_dat(i,jds-j)
                a_dat(i,jds-j) = 0.0
                a_dat(i,jds+j) = a_dat(i,jds+j) + a_aux
                a_aux = 0.0
              ENDDO              
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys

      END IF periodicity_y

      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN 

        IF ( ( ids == ips ) .and.  ( ide == ipe ) ) THEN  ! test if east and west both on-processor 
          IF ( ite == ide ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1
            DO i = bdyzone,-istag,-1 
              a_aux = a_dat(ide+i+istag,j)
              a_dat(ide+i+istag,j) = 0.0
              a_dat(ids+i+istag,j) = a_dat(ids+i+istag,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ENDIF

          IF ( its == ids ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO i = -(bdyzone-1),0,1
              a_aux = a_dat(ids+i-1,j)
              a_dat(ids+i-1,j) = 0.0
              a_dat(ide+i-1,j) = a_dat(ide+i-1,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ENDIF
        ENDIF

      ELSE 

!  set open b.c in X copy into boundary zone here.  WCS, 19 March 2000

!  now the open boundary copy at xe

        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                          ( ite == ide ) .and. open_bc_copy  )  THEN

          IF ( variable /= 'u' .and. variable /= 'x') THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              a_aux = a_dat(ide  ,j)
              a_dat(ide  ,j) = 0.0
              a_dat(ide-1,j) = a_dat(ide-1,j) + a_aux
              a_aux = a_dat(ide+1,j)
              a_dat(ide+1,j) = 0.0
              a_dat(ide-1,j) = a_dat(ide-1,j) + a_aux
              a_aux = a_dat(ide+2,j)
              a_dat(ide+2,j) = 0.0
              a_dat(ide-1,j) = a_dat(ide-1,j) + a_aux
              a_aux = 0.0
            ENDDO

          ELSE

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              a_aux = a_dat(ide+1,j)
              a_dat(ide+1,j) = 0.0
              a_dat(ide,j) = a_dat(ide,j) + a_aux
              a_aux = a_dat(ide+2,j)
              a_dat(ide+2,j) = 0.0
              a_dat(ide,j) = a_dat(ide,j) + a_aux
              a_aux = a_dat(ide+3,j)
              a_dat(ide+3,j) = 0.0
              a_dat(ide,j) = a_dat(ide,j) + a_aux
              a_aux = 0.0
            ENDDO

          END IF 

        END IF open_xe

        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1
              a_aux = a_dat(ids-1,j)
              a_dat(ids-1,j) = 0.0
              a_dat(ids,j) = a_dat(ids,j) + a_aux
              a_aux = a_dat(ids-2,j)
              a_dat(ids-2,j) = 0.0
              a_dat(ids,j) = a_dat(ids,j) + a_aux
              a_aux = a_dat(ids-3,j)
              a_dat(ids-3,j) = 0.0
              a_dat(ids,j) = a_dat(ids,j) + a_aux
              a_aux = 0.0
            ENDDO

        ENDIF open_xs

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

!  now the symmetry boundary at xe

        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO i = bdyzone,1,-1
              a_aux = a_dat(ide+i-1,j)
              a_dat(ide+i-1,j) = 0.0
              a_dat(ide-i,j) = a_dat(ide-i,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u' ) THEN

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              DO i = bdyzone-1,0,-1
                a_aux = - a_dat(ide+i,j)
                a_dat(ide+i,j) = 0.0
                a_dat(ide-i,j) = a_dat(ide-i,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO


            ELSE

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              DO i = bdyzone-1,0,-1
                a_aux = a_dat(ide+i,j)
                a_dat(ide+i,j) = 0.0
                a_dat(ide-i,j) = a_dat(ide-i,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO

            END IF

          END IF 

        END IF symmetry_xe

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO i = bdyzone,1,-1
              a_aux = a_dat(ids-i,j)
              a_dat(ids-i,j) = 0.0
              a_dat(ide+i-1,j) = a_dat(ide+i-1,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ELSE

            IF( variable == 'u' ) THEN

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1
              DO i = bdyzone-1,0,-1
                a_aux = - a_dat(ids-i,j)
                a_dat(ids-i,j) = 0.0
                a_dat(ids+i,j) = a_dat(ids+i,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO

            ELSE

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1
              DO i = bdyzone-1,0,-1
                a_aux = a_dat(ids-i,j)
                a_dat(ids-i,j) = 0.0
                a_dat(ids+i,j) = a_dat(ids+i,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs

      END IF periodicity_x

   END SUBROUTINE a_set_physical_bc2d

!-----------------------------------

   SUBROUTINE a_set_physical_bc3d( a_dat, variable_in,        &
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

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: a_dat
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, k, istag, jstag, itime, k_end

      LOGICAL  :: debug, open_bc_copy

      real :: a_aux

!------------

      a_aux = 0.0

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
      
!  fix corners for doubly periodic domains

      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jte == jde) ) THEN  ! upper left corner fill
           DO j = bdyzone,1,-1
           DO k = kts, k_end
           DO i = -(bdyzone-1),0,1
             a_aux = a_dat(ids+i-1,k,jde+j+jstag)
             a_dat(ids+i-1,k,jde+j+jstag) = 0.0
             a_dat(ide+i-1,k,jds+j+jstag) = a_dat(ide+i-1,k,jds+j+jstag) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  ! upper right corner fill
           DO j = bdyzone,1,-1
           DO k = kts, k_end
           DO i = bdyzone,1,-1
             a_aux = a_dat(ide+i+istag,k,jde+j+jstag)
             a_dat(ide+i+istag,k,jde+j+jstag) = 0.0
             a_dat(ids+i+istag,k,jds+j+jstag) = a_dat(ids+i+istag,k,jds+j+jstag) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  ! lower right corner fill
           DO j = -(bdyzone-1),0,1
           DO k = kts, k_end
           DO i = bdyzone,1,-1
             a_aux = a_dat(ide+i+istag,k,jds+j-1)
             a_dat(ide+i+istag,k,jds+j-1) = 0.0
             a_dat(ids+i+istag,k,jde+j-1) = a_dat(ids+i+istag,k,jde+j-1) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jts == jds) ) THEN  ! lower left corner fill
           DO j = -(bdyzone-1),0,1
           DO k = kts, k_end
           DO i = -(bdyzone-1),0,1
             a_aux = a_dat(ids+i-1,k,jds+j-1)
             a_dat(ids+i-1,k,jds+j-1) = 0.0
             a_dat(ide+i-1,k,jde+j-1) = a_dat(ide+i-1,k,jde+j-1) + a_aux
             a_aux = 0.0
           ENDDO
           ENDDO
           ENDDO
         END IF

       END IF

!  same procedure in y

      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN
        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN      ! test if both north and south on processor
          IF( jte == jde ) then

            DO j = bdyzone,-jstag,-1
            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,k,jde+j+jstag)
              a_dat(i,k,jde+j+jstag) = 0.0
              a_dat(i,k,jds+j+jstag) = a_dat(i,k,jds+j+jstag) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          END IF

          IF( jts == jds ) then

            DO j = -(bdyzone-1),0,1
            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,k,jds+j-1)
              a_dat(i,k,jds+j-1) = 0.0
              a_dat(i,k,jde+j-1) = a_dat(i,k,jde+j-1) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          END IF

        END IF

      ELSE

!  set open b.c in Y copy into boundary zone here.  WCS, 19 March 2000

!  now the open boundary copy at ye

        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF (variable /= 'v' .and. variable /= 'y' ) THEN

            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1
              a_aux = a_dat(i,k,jde  )
              a_dat(i,k,jde  ) = 0.0
              a_dat(i,k,jde-1) = a_dat(i,k,jde-1) + a_aux
              a_aux = a_dat(i,k,jde+1)
              a_dat(i,k,jde+1) = 0.0
              a_dat(i,k,jde-1) = a_dat(i,k,jde-1) + a_aux
              a_aux = a_dat(i,k,jde+2)
              a_dat(i,k,jde+2) = 0.0
              a_dat(i,k,jde-1) = a_dat(i,k,jde-1) + a_aux
              a_aux = 0.0
            ENDDO                               
            ENDDO

          ELSE

            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1
              a_aux = a_dat(i,k,jde+1)
              a_dat(i,k,jde+1) = 0.0
              a_dat(i,k,jde) = a_dat(i,k,jde) + a_aux
              a_aux = a_dat(i,k,jde+2)
              a_dat(i,k,jde+2) = 0.0
              a_dat(i,k,jde) = a_dat(i,k,jde) + a_aux
              a_aux = a_dat(i,k,jde+3)
              a_dat(i,k,jde+3) = 0.0
              a_dat(i,k,jde) = a_dat(i,k,jde) + a_aux
              a_aux = 0.0
            ENDDO                               
            ENDDO

          ENDIF

        END IF open_ye

        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,k,jds-1)
              a_dat(i,k,jds-1) = 0.0
              a_dat(i,k,jds) = a_dat(i,k,jds) + a_aux
              a_aux = a_dat(i,k,jds-2)
              a_dat(i,k,jds-2) = 0.0
              a_dat(i,k,jds) = a_dat(i,k,jds) + a_aux
              a_aux = a_dat(i,k,jds-3)
              a_dat(i,k,jds-3) = 0.0
              a_dat(i,k,jds) = a_dat(i,k,jds) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

        ENDIF open_ys

!  end open b.c in Y copy into boundary zone addition.  WCS, 19 March 2000

!  now the symmetry boundary at ye

        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = bdyzone,1,-1
            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,k,jde+j-1)
              a_dat(i,k,jde+j-1) = 0.0
              a_dat(i,k,jde-j) = a_dat(i,k,jde-j) + a_aux
              a_aux = 0.0
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'v' ) THEN

              DO j = bdyzone,1,-1
              DO k = kts, k_end
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = - a_dat(i,k,jde+j)
                a_dat(i,k,jde+j) = 0.0
                a_dat(i,k,jde-j) = a_dat(i,k,jde-j) + a_aux
                a_aux = 0.0
              ENDDO                               
              ENDDO
              ENDDO

            ELSE

              DO j = bdyzone,1,-1
              DO k = kts, k_end
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1
                a_aux = a_dat(i,k,jde+j)
                a_dat(i,k,jde+j) = 0.0
                a_dat(i,k,jde-j) = a_dat(i,k,jde-j) + a_aux
                a_aux = 0.0
              ENDDO                               
              ENDDO
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye
      
        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = bdyzone,1,-1
            DO k = kts, k_end
            DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
              a_aux = a_dat(i,k,jds-j)
              a_dat(i,k,jds-j) = 0.0
              a_dat(i,k,jds+j-1) = a_dat(i,k,jds+j-1) + a_aux
              a_aux = 0.0
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = bdyzone,1,-1
              DO k = kts, k_end
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = - a_dat(i,k,jds-j)
                a_dat(i,k,jds-j) = 0.0
                a_dat(i,k,jds+j) = a_dat(i,k,jds+j) + a_aux
                a_aux = 0.0
              ENDDO              
              ENDDO
              ENDDO

            ELSE

              DO j = bdyzone,1,-1
              DO k = kts, k_end
              DO i = MIN(ite+1,ide+istag),MAX(ids,its-1),-1 
                a_aux = a_dat(i,k,jds-j)
                a_dat(i,k,jds-j) = 0.0
                a_dat(i,k,jds+j) = a_dat(i,k,jds+j) + a_aux
                a_aux = 0.0
              ENDDO              
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys

      END IF periodicity_y

      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN

        IF ( ( ids == ips ) .and. ( ide == ipe ) ) THEN  ! test if both east and west on-processor

          IF ( ite == ide ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO k = kts, k_end
            DO i = bdyzone,-istag,-1 
              a_aux = a_dat(ide+i+istag,k,j)
              a_dat(ide+i+istag,k,j) = 0.0
              a_dat(ids+i+istag,k,j) = a_dat(ids+i+istag,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          ENDIF

          IF ( its == ids ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO k = kts, k_end
            DO i = -(bdyzone-1),0,1
              a_aux = a_dat(ids+i-1,k,j)
              a_dat(ids+i-1,k,j) = 0.0
              a_dat(ide+i-1,k,j) = a_dat(ide+i-1,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          ENDIF

        ENDIF

      ELSE 

!  set open b.c in X copy into boundary zone here.  WCS, 19 March 2000

!  now the open_xe boundary copy 

        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( ite == ide ) .and. open_bc_copy )  THEN

          IF (variable /= 'u' .and. variable /= 'x' ) THEN

            DO j = MIN(jte,jde+jstag)+bdyzone,jts-bdyzone,-1 
            DO k = kts, k_end
              a_aux = a_dat(ide  ,k,j)
              a_dat(ide  ,k,j) = 0.0
              a_dat(ide-1,k,j) = a_dat(ide-1,k,j) + a_aux
              a_aux = a_dat(ide+1,k,j)
              a_dat(ide+1,k,j) = 0.0
              a_dat(ide-1,k,j) = a_dat(ide-1,k,j) + a_aux
              a_aux = a_dat(ide+2,k,j)
              a_dat(ide+2,k,j) = 0.0
              a_dat(ide-1,k,j) = a_dat(ide-1,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          ELSE

!!!!!!! I am not sure about this one!  JM 20020402
            DO j = MIN(jte+1,jde+jstag)+bdyzone,MAX(jds,jts-1)-bdyzone,-1 
            DO k = kts, k_end
              a_aux = a_dat(ide+1,k,j)
              a_dat(ide+1,k,j) = 0.0
              a_dat(ide,k,j) = a_dat(ide,k,j) + a_aux
              a_aux = a_dat(ide+2,k,j)
              a_dat(ide+2,k,j) = 0.0
              a_dat(ide,k,j) = a_dat(ide,k,j) + a_aux
              a_aux = a_dat(ide+3,k,j)
              a_dat(ide+3,k,j) = 0.0
              a_dat(ide,k,j) = a_dat(ide,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

          END IF 

        END IF open_xe

        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = MIN(jte,jde+jstag)+bdyzone,jts-bdyzone,-1 
            DO k = kts, k_end
              a_aux = a_dat(ids-1,k,j)
              a_dat(ids-1,k,j) = 0.0
              a_dat(ids,k,j) = a_dat(ids,k,j) + a_aux
              a_aux = a_dat(ids-2,k,j)
              a_dat(ids-2,k,j) = 0.0
              a_dat(ids,k,j) = a_dat(ids,k,j) + a_aux
              a_aux = a_dat(ids-3,k,j)
              a_dat(ids-3,k,j) = 0.0
              a_dat(ids,k,j) = a_dat(ids,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO

        ENDIF open_xs

!  end open b.c in X copy into boundary zone addition.  WCS, 19 March 2000

!  now the symmetry boundary at xe

        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO k = kts, k_end
            DO i = bdyzone,1,-1
              a_aux = a_dat(ide+i-1,k,j)
              a_dat(ide+i-1,k,j) = 0.0
              a_dat(ide-i,k,j) = a_dat(ide-i,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u') THEN

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              DO k = kts, k_end
              DO i = bdyzone,1,-1
                a_aux = - a_dat(ide+i,k,j)
                a_dat(ide+i,k,j) = 0.0
                a_dat(ide-i,k,j) = a_dat(ide-i,k,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              DO k = kts, k_end
              DO i = bdyzone,1,-1
                a_aux = a_dat(ide+i,k,j)
                a_dat(ide+i,k,j) = 0.0
                a_dat(ide-i,k,j) = a_dat(ide-i,k,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO
              ENDDO

             END IF

          END IF 

        END IF symmetry_xe

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
            DO k = kts, k_end
            DO i = bdyzone,1,-1
              a_aux = a_dat(ids-i,k,j)
              a_dat(ids-i,k,j) = 0.0
              a_dat(ids+i-1,k,j) = a_dat(ids+i-1,k,j) + a_aux
              a_aux = 0.0
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'u' ) THEN

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1 
              DO k = kts, k_end
              DO i = bdyzone,1,-1
                a_aux = - a_dat(ids-i,k,j)
                a_dat(ids-i,k,j) = 0.0
                a_dat(ids+i,k,j) = a_dat(ids+i,k,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MIN(jte+1,jde+jstag),MAX(jds,jts-1),-1
              DO k = kts, k_end
              DO i = bdyzone,1,-1
                a_aux = a_dat(ids-i,k,j)
                a_dat(ids-i,k,j) = 0.0
                a_dat(ids+i,k,j) = a_dat(ids+i,k,j) + a_aux
                a_aux = 0.0
              ENDDO
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs

      END IF periodicity_x

   END SUBROUTINE a_set_physical_bc3d

!------------------------------------------------------------------------

   SUBROUTINE a_init_module_bc
   END SUBROUTINE a_init_module_bc

!------------------------------------------------------------------------

! a couple versions of this call to allow a smaller-than-memory dimensioned field (e.g. tile sized) ! to be passed in as the first argument.  Both of these call the _core version defined below.
   SUBROUTINE a_relax_bdytend ( a_field, a_field_tend, &
                                a_field_bdy_xs, a_field_bdy_xe,  &
                                a_field_bdy_ys, a_field_bdy_ye,  &
                                a_field_bdy_tend_xs, a_field_bdy_tend_xe,  &
                                a_field_bdy_tend_ys, a_field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

   INTEGER,   INTENT(IN) :: ids,ide, jds,jde, kds,kde
   INTEGER,   INTENT(IN) :: ims,ime, jms,jme, kms,kme
   INTEGER,   INTENT(IN) :: ips,ipe, jps,jpe, kps,kpe
   INTEGER,   INTENT(IN) :: its,ite, jts,jte, kts,kte
   INTEGER,   INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone
   REAL,      INTENT(IN) :: dtbc
   CHARACTER, INTENT(IN) :: variable_in

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: a_field
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: a_field_tend
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_xs, a_field_bdy_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_ys, a_field_bdy_ye
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_xs, a_field_bdy_tend_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_ys, a_field_bdy_tend_ye
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
   TYPE(grid_config_rec_type),      INTENT(IN) :: config_flags


   CALL a_relax_bdytend_core ( a_field, a_field_tend, &
                    a_field_bdy_xs, a_field_bdy_xe, &
                    a_field_bdy_ys, a_field_bdy_ye, &
                    a_field_bdy_tend_xs, a_field_bdy_tend_xe,  &
                    a_field_bdy_tend_ys, a_field_bdy_tend_ye,  &
                    variable_in, config_flags,             &
                    spec_bdy_width, spec_zone, relax_zone, &
                    dtbc, fcx, gcx,             &
                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                    its,ite, jts,jte, kts,kte,  & ! patch  dims
                    ims,ime, jms,jme, kms,kme )  ! dimension of the field argument

   END SUBROUTINE a_relax_bdytend

! version that allows tile-sized version of field. Note, caller should define the
! field to be -+1 of tile size in each dimension because routine is going off onto halo
! for example, see relax_bdytend in dyn_em/module_bc_em.F 
   SUBROUTINE a_relax_bdytend_tile ( a_field, a_field_tend, & 
                       a_field_bdy_xs, a_field_bdy_xe,  &
                       a_field_bdy_ys, a_field_bdy_ye,  &
                       a_field_bdy_tend_xs, a_field_bdy_tend_xe,  &
                       a_field_bdy_tend_ys, a_field_bdy_tend_ye,  &
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

   REAL, DIMENSION(iXs:iXe, kXs:kXe, jXs:jXe), INTENT(INOUT) :: a_field
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: a_field_tend
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_xs, a_field_bdy_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_ys, a_field_bdy_ye
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_xs, a_field_bdy_tend_xe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_ys, a_field_bdy_tend_ye
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
   TYPE(grid_config_rec_type),      INTENT(IN) :: config_flags


   CALL a_relax_bdytend_core ( a_field, a_field_tend, &
                    a_field_bdy_xs, a_field_bdy_xe,  &
                    a_field_bdy_ys, a_field_bdy_ye,  &
                    a_field_bdy_tend_xs, a_field_bdy_tend_xe,  &
                    a_field_bdy_tend_ys, a_field_bdy_tend_ye,  &
                    variable_in, config_flags,             &
                    spec_bdy_width, spec_zone, relax_zone, &
                    dtbc, fcx, gcx,             &
                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                    its,ite, jts,jte, kts,kte,  &
                    iXs,iXe, jXs,jXe, kXs,kXe )  ! dimension of the field argument

   END SUBROUTINE a_relax_bdytend_tile

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of relax_bdytend_core in reverse (adjoint) mode:
!   gradient     of useful results: field field_bdy_xe field_bdy_tend_xe
!                field_tend field_bdy_xs field_bdy_tend_xs field_bdy_ye
!                field_bdy_tend_ye field_bdy_ys field_bdy_tend_ys
!   with respect to varying inputs: field field_bdy_xe field_bdy_tend_xe
!                field_tend field_bdy_xs field_bdy_tend_xs field_bdy_ye
!                field_bdy_tend_ye field_bdy_ys field_bdy_tend_ys
!   RW status of diff variables: field:incr field_bdy_xe:incr field_bdy_tend_xe:incr
!                field_tend:in-out field_bdy_xs:incr field_bdy_tend_xs:incr
!                field_bdy_ye:incr field_bdy_tend_ye:incr field_bdy_ys:incr
!                field_bdy_tend_ys:incr
! domain dims
! memory dims
! patch  dims
! patch  dims
! field (1st arg) dims; might be tile or patch
SUBROUTINE A_RELAX_BDYTEND_CORE(fieldb, field_tendb, &
&  field_bdy_xsb, field_bdy_xeb&
&  , field_bdy_ysb, field_bdy_yeb, &
&  field_bdy_tend_xsb, field_bdy_tend_xeb, &
&  field_bdy_tend_ysb, &
&  field_bdy_tend_yeb, variable_in, config_flags, spec_bdy_width, &
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
  REAL, DIMENSION(ixs:ixe, kxs:kxe, jxs:jxe) :: fieldb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: field_tendb
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width) :: field_bdy_xsb, &
&  field_bdy_xeb
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width) :: field_bdy_ysb, &
&  field_bdy_yeb
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width) :: &
&  field_bdy_tend_xsb, field_bdy_tend_xeb
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width) :: &
&  field_bdy_tend_ysb, field_bdy_tend_yeb
  REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  CHARACTER :: variable
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
  INTEGER :: b_dist, b_limit
  REAL :: fls0, fls1, fls2, fls3, fls4
  REAL :: fls0b, fls1b, fls2b, fls3b, fls4b
  LOGICAL :: periodic_x
  INTEGER :: branch
  INTEGER :: ad_from
  INTEGER :: ad_to
  INTEGER :: ad_from0
  INTEGER :: ad_to0
  INTEGER :: ad_from1
  INTEGER :: ad_to1
  INTEGER :: ad_from2
  INTEGER :: ad_to2
  INTEGER :: min8
  INTEGER :: min7
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  REAL :: tempb2
  REAL :: tempb1
  REAL :: tempb0
  REAL :: tempb
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
      CALL PUSHINTEGER4(b_dist)
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
        ad_from = max2
        DO i=ad_from,min2
          IF (i - 1 .LT. ibs) THEN
            CALL PUSHINTEGER4(im1)
            im1 = ibs
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(im1)
            im1 = i - 1
            CALL PUSHCONTROL1B(1)
          END IF
          IF (i + 1 .GT. ibe) THEN
            CALL PUSHINTEGER4(ip1)
            ip1 = ibe
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(ip1)
            ip1 = i + 1
            CALL PUSHCONTROL1B(1)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
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
      CALL PUSHINTEGER4(b_dist)
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
        ad_from0 = max4
        DO i=ad_from0,min4
          IF (i - 1 .LT. ibs) THEN
            CALL PUSHINTEGER4(im1)
            im1 = ibs
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(im1)
            im1 = i - 1
            CALL PUSHCONTROL1B(1)
          END IF
          IF (i + 1 .GT. ibe) THEN
            CALL PUSHINTEGER4(ip1)
            ip1 = ibe
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(ip1)
            ip1 = i + 1
            CALL PUSHCONTROL1B(1)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from0)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
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
        CALL PUSHINTEGER4(b_dist)
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
          ad_from1 = max6
          j = min6 + 1
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from1)
        END DO
      END DO
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
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
        CALL PUSHINTEGER4(b_dist)
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
          ad_from2 = max8
          j = min8 + 1
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from2)
        END DO
      END DO
      DO i=min7,max7,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from2)
          CALL POPINTEGER4(ad_to2)
          DO j=ad_to2,ad_from2,-1
            tempb2 = -(gcx(b_dist+1)*field_tendb(i, k, j))
            fls0b = fcx(b_dist+1)*field_tendb(i, k, j) - 4.*tempb2
            fls1b = tempb2
            fls2b = tempb2
            fls3b = tempb2
            fls4b = tempb2
            field_bdy_xeb(j, k, b_dist+2) = field_bdy_xeb(j, k, b_dist+2&
&              ) + fls4b
            field_bdy_tend_xeb(j, k, b_dist+2) = field_bdy_tend_xeb(j, k&
&              , b_dist+2) + dtbc*fls4b
            fieldb(i-1, k, j) = fieldb(i-1, k, j) - fls4b
            field_bdy_xeb(j, k, b_dist) = field_bdy_xeb(j, k, b_dist) + &
&              fls3b
            field_bdy_tend_xeb(j, k, b_dist) = field_bdy_tend_xeb(j, k, &
&              b_dist) + dtbc*fls3b
            fieldb(i+1, k, j) = fieldb(i+1, k, j) - fls3b
            field_bdy_xeb(j+1, k, b_dist+1) = field_bdy_xeb(j+1, k, &
&              b_dist+1) + fls2b
            field_bdy_tend_xeb(j+1, k, b_dist+1) = field_bdy_tend_xeb(j+&
&              1, k, b_dist+1) + dtbc*fls2b
            fieldb(i, k, j+1) = fieldb(i, k, j+1) - fls2b
            field_bdy_xeb(j-1, k, b_dist+1) = field_bdy_xeb(j-1, k, &
&              b_dist+1) + fls1b
            field_bdy_tend_xeb(j-1, k, b_dist+1) = field_bdy_tend_xeb(j-&
&              1, k, b_dist+1) + dtbc*fls1b
            fieldb(i, k, j-1) = fieldb(i, k, j-1) - fls1b
            field_bdy_xeb(j, k, b_dist+1) = field_bdy_xeb(j, k, b_dist+1&
&              ) + fls0b
            field_bdy_tend_xeb(j, k, b_dist+1) = field_bdy_tend_xeb(j, k&
&              , b_dist+1) + dtbc*fls0b
            fieldb(i, k, j) = fieldb(i, k, j) - fls0b
          END DO
        END DO
        CALL POPINTEGER4(b_dist)
      END DO
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      DO i=min5,max5,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from1)
          CALL POPINTEGER4(ad_to1)
          DO j=ad_to1,ad_from1,-1
            tempb1 = -(gcx(b_dist+1)*field_tendb(i, k, j))
            fls0b = fcx(b_dist+1)*field_tendb(i, k, j) - 4.*tempb1
            fls1b = tempb1
            fls2b = tempb1
            fls3b = tempb1
            fls4b = tempb1
            field_bdy_xsb(j, k, b_dist+2) = field_bdy_xsb(j, k, b_dist+2&
&              ) + fls4b
            field_bdy_tend_xsb(j, k, b_dist+2) = field_bdy_tend_xsb(j, k&
&              , b_dist+2) + dtbc*fls4b
            fieldb(i+1, k, j) = fieldb(i+1, k, j) - fls4b
            field_bdy_xsb(j, k, b_dist) = field_bdy_xsb(j, k, b_dist) + &
&              fls3b
            field_bdy_tend_xsb(j, k, b_dist) = field_bdy_tend_xsb(j, k, &
&              b_dist) + dtbc*fls3b
            fieldb(i-1, k, j) = fieldb(i-1, k, j) - fls3b
            field_bdy_xsb(j+1, k, b_dist+1) = field_bdy_xsb(j+1, k, &
&              b_dist+1) + fls2b
            field_bdy_tend_xsb(j+1, k, b_dist+1) = field_bdy_tend_xsb(j+&
&              1, k, b_dist+1) + dtbc*fls2b
            fieldb(i, k, j+1) = fieldb(i, k, j+1) - fls2b
            field_bdy_xsb(j-1, k, b_dist+1) = field_bdy_xsb(j-1, k, &
&              b_dist+1) + fls1b
            field_bdy_tend_xsb(j-1, k, b_dist+1) = field_bdy_tend_xsb(j-&
&              1, k, b_dist+1) + dtbc*fls1b
            fieldb(i, k, j-1) = fieldb(i, k, j-1) - fls1b
            field_bdy_xsb(j, k, b_dist+1) = field_bdy_xsb(j, k, b_dist+1&
&              ) + fls0b
            field_bdy_tend_xsb(j, k, b_dist+1) = field_bdy_tend_xsb(j, k&
&              , b_dist+1) + dtbc*fls0b
            fieldb(i, k, j) = fieldb(i, k, j) - fls0b
          END DO
        END DO
        CALL POPINTEGER4(b_dist)
      END DO
    END IF
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=min3,max3,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from0)
        CALL POPINTEGER4(ad_to0)
        DO i=ad_to0,ad_from0,-1
          tempb0 = -(gcx(b_dist+1)*field_tendb(i, k, j))
          fls0b = fcx(b_dist+1)*field_tendb(i, k, j) - 4.*tempb0
          fls1b = tempb0
          fls2b = tempb0
          fls3b = tempb0
          fls4b = tempb0
          field_bdy_yeb(i, k, b_dist+2) = field_bdy_yeb(i, k, b_dist+2) &
&            + fls4b
          field_bdy_tend_yeb(i, k, b_dist+2) = field_bdy_tend_yeb(i, k, &
&            b_dist+2) + dtbc*fls4b
          fieldb(i, k, j-1) = fieldb(i, k, j-1) - fls4b
          field_bdy_yeb(i, k, b_dist) = field_bdy_yeb(i, k, b_dist) + &
&            fls3b
          field_bdy_tend_yeb(i, k, b_dist) = field_bdy_tend_yeb(i, k, &
&            b_dist) + dtbc*fls3b
          fieldb(i, k, j+1) = fieldb(i, k, j+1) - fls3b
          field_bdy_yeb(ip1, k, b_dist+1) = field_bdy_yeb(ip1, k, b_dist&
&            +1) + fls2b
          field_bdy_tend_yeb(ip1, k, b_dist+1) = field_bdy_tend_yeb(ip1&
&            , k, b_dist+1) + dtbc*fls2b
          fieldb(ip1, k, j) = fieldb(ip1, k, j) - fls2b
          field_bdy_yeb(im1, k, b_dist+1) = field_bdy_yeb(im1, k, b_dist&
&            +1) + fls1b
          field_bdy_tend_yeb(im1, k, b_dist+1) = field_bdy_tend_yeb(im1&
&            , k, b_dist+1) + dtbc*fls1b
          fieldb(im1, k, j) = fieldb(im1, k, j) - fls1b
          field_bdy_yeb(i, k, b_dist+1) = field_bdy_yeb(i, k, b_dist+1) &
&            + fls0b
          field_bdy_tend_yeb(i, k, b_dist+1) = field_bdy_tend_yeb(i, k, &
&            b_dist+1) + dtbc*fls0b
          fieldb(i, k, j) = fieldb(i, k, j) - fls0b
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(ip1)
          ELSE
            CALL POPINTEGER4(ip1)
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(im1)
          ELSE
            CALL POPINTEGER4(im1)
          END IF
        END DO
      END DO
      CALL POPINTEGER4(b_dist)
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=min1,max1,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from)
        CALL POPINTEGER4(ad_to)
        DO i=ad_to,ad_from,-1
          tempb = -(gcx(b_dist+1)*field_tendb(i, k, j))
          fls0b = fcx(b_dist+1)*field_tendb(i, k, j) - 4.*tempb
          fls1b = tempb
          fls2b = tempb
          fls3b = tempb
          fls4b = tempb
          field_bdy_ysb(i, k, b_dist+2) = field_bdy_ysb(i, k, b_dist+2) &
&            + fls4b
          field_bdy_tend_ysb(i, k, b_dist+2) = field_bdy_tend_ysb(i, k, &
&            b_dist+2) + dtbc*fls4b
          fieldb(i, k, j+1) = fieldb(i, k, j+1) - fls4b
          field_bdy_ysb(i, k, b_dist) = field_bdy_ysb(i, k, b_dist) + &
&            fls3b
          field_bdy_tend_ysb(i, k, b_dist) = field_bdy_tend_ysb(i, k, &
&            b_dist) + dtbc*fls3b
          fieldb(i, k, j-1) = fieldb(i, k, j-1) - fls3b
          field_bdy_ysb(ip1, k, b_dist+1) = field_bdy_ysb(ip1, k, b_dist&
&            +1) + fls2b
          field_bdy_tend_ysb(ip1, k, b_dist+1) = field_bdy_tend_ysb(ip1&
&            , k, b_dist+1) + dtbc*fls2b
          fieldb(ip1, k, j) = fieldb(ip1, k, j) - fls2b
          field_bdy_ysb(im1, k, b_dist+1) = field_bdy_ysb(im1, k, b_dist&
&            +1) + fls1b
          field_bdy_tend_ysb(im1, k, b_dist+1) = field_bdy_tend_ysb(im1&
&            , k, b_dist+1) + dtbc*fls1b
          fieldb(im1, k, j) = fieldb(im1, k, j) - fls1b
          field_bdy_ysb(i, k, b_dist+1) = field_bdy_ysb(i, k, b_dist+1) &
&            + fls0b
          field_bdy_tend_ysb(i, k, b_dist+1) = field_bdy_tend_ysb(i, k, &
&            b_dist+1) + dtbc*fls0b
          fieldb(i, k, j) = fieldb(i, k, j) - fls0b
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(ip1)
          ELSE
            CALL POPINTEGER4(ip1)
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(im1)
          ELSE
            CALL POPINTEGER4(im1)
          END IF
        END DO
      END DO
      CALL POPINTEGER4(b_dist)
    END DO
  END IF
END SUBROUTINE A_RELAX_BDYTEND_CORE
!------------------------------------------------------------------------

   SUBROUTINE a_spec_bdytend ( a_field_tend,           &
                               a_field_bdy_tend_xs, a_field_bdy_tend_xe, &
                               a_field_bdy_tend_ys, a_field_bdy_tend_ye, &
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

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),        INTENT(INOUT) :: a_field_tend
      REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_xs, a_field_bdy_tend_xe
      REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_field_bdy_tend_ys, a_field_bdy_tend_ye 
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


    IF(.NOT.periodic_x)THEN
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = itf, max(its,ibe-spec_zone+1), -1
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              a_field_bdy_tend_xe(j, k, b_dist+1) = a_field_bdy_tend_xe(j, k, b_dist+1) &
                                                  + a_field_tend(i,k,j)
              a_field_tend(i,k,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = min(itf,ibs+spec_zone-1), its, -1
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              a_field_bdy_tend_xs(j, k, b_dist+1) = a_field_bdy_tend_xs(j, k, b_dist+1) &
                                                  + a_field_tend(i,k,j)
              a_field_tend(i,k,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = jtf, max(jts,jbe-spec_zone+1), -1
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              a_field_bdy_tend_ye(i, k, b_dist+1) = a_field_bdy_tend_ye(i, k, b_dist+1) &
                                                  + a_field_tend(i,k,j)
              a_field_tend(i,k,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = min(jtf,jbs+spec_zone-1), jts, -1
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              a_field_bdy_tend_ys(i, k, b_dist+1) = a_field_bdy_tend_ys(i, k, b_dist+1) &
                                                  + a_field_tend(i,k,j)
              a_field_tend(i,k,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_spec_bdytend

!------------------------------------------------------------------------

   SUBROUTINE a_spec_bdyupdate(  a_field,  &
                                 a_field_tend, dt,            &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: a_field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field_tend
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

    IF(.NOT.periodic_x)THEN
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) + dt * a_field(i,k,j) 
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
              a_field_tend(i,k,j) = a_field_tend(i,k,j) + dt * a_field(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) + dt * a_field(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) + dt * a_field(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_spec_bdyupdate
!------------------------------------------------------------------------
!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.10 (r5498) - 20 Jan 2015 09:48
!
!  Differentiation of spec_bdy_final in reverse (adjoint) mode:
!   gradient     of useful results: field field_bdy_xe field_bdy_tend_xe
!                field_bdy_xs field_bdy_tend_xs field_bdy_ye field_bdy_tend_ye
!                field_bdy_ys field_bdy_tend_ys mu
!   with respect to varying inputs: field field_bdy_xe field_bdy_tend_xe
!                field_bdy_xs field_bdy_tend_xs field_bdy_ye field_bdy_tend_ye
!                field_bdy_ys field_bdy_tend_ys mu
!   RW status of diff variables: field:in-out field_bdy_xe:incr
!                field_bdy_tend_xe:incr field_bdy_xs:incr field_bdy_tend_xs:incr
!                field_bdy_ye:incr field_bdy_tend_ye:incr field_bdy_ys:incr
!                field_bdy_tend_ys:incr mu:incr
! domain dims
! memory dims
! patch  dims
SUBROUTINE a_SPEC_BDY_FINAL(field, fieldb, mu, mub, msf, field_bdy_xs, &
& field_bdy_xsb, field_bdy_xe, field_bdy_xeb, field_bdy_ys, &
& field_bdy_ysb, field_bdy_ye, field_bdy_yeb, field_bdy_tend_xs, &
& field_bdy_tend_xsb, field_bdy_tend_xe, field_bdy_tend_xeb, &
& field_bdy_tend_ys, field_bdy_tend_ysb, field_bdy_tend_ye, &
& field_bdy_tend_yeb, variable_in, config_flags, spec_bdy_width, &
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
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: fieldb
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mu, msf
  REAL, DIMENSION(ims:ime, jms:jme) :: mub
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_xs, field_bdy_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width) :: field_bdy_xsb, &
& field_bdy_xeb
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_ys, field_bdy_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width) :: field_bdy_ysb, &
& field_bdy_yeb
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_xs, field_bdy_tend_xe
  REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width) :: &
& field_bdy_tend_xsb, field_bdy_tend_xeb
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: &
& field_bdy_tend_ys, field_bdy_tend_ye
  REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width) :: &
& field_bdy_tend_ysb, field_bdy_tend_yeb
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  CHARACTER :: variable
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
  INTEGER :: b_dist, b_limit
  REAL :: bfield, xmsf, xmu
  REAL :: bfieldb, xmub
  LOGICAL :: periodic_x, msfcouple, mucouple
  INTEGER :: branch
  INTEGER :: ad_from
  INTEGER :: ad_to
  INTEGER :: ad_from0
  INTEGER :: ad_to0
  INTEGER :: ad_from1
  INTEGER :: ad_to1
  INTEGER :: ad_from2
  INTEGER :: ad_to2
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  REAL :: tempb2
  REAL :: tempb1
  REAL :: tempb0
  REAL :: tempb
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
    ELSE
      min1 = jtf
    END IF
! Y-start boundary
    DO j=jts,min1
      CALL PUSHINTEGER4(b_dist)
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
        ad_from = max1
        DO i=ad_from,min3
          IF (msfcouple) THEN
            CALL PUSHREAL8(xmsf)
            xmsf = msf(i, j)
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
          IF (mucouple) THEN
            CALL PUSHREAL8(xmu)
            xmu = mu(i, j)
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
  END IF
  IF (jbe - jtf .LT. spec_zone) THEN
    IF (jts .LT. jbe - spec_zone + 1) THEN
      max2 = jbe - spec_zone + 1
    ELSE
      max2 = jts
    END IF
! Y-end boundary
    DO j=max2,jtf
      CALL PUSHINTEGER4(b_dist)
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
        ad_from0 = max3
        DO i=ad_from0,min4
          IF (msfcouple) THEN
            CALL PUSHREAL8(xmsf)
            xmsf = msf(i, j)
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
          IF (mucouple) THEN
            CALL PUSHREAL8(xmu)
            xmu = mu(i, j)
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from0)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
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
        CALL PUSHINTEGER4(b_dist)
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
          ad_from1 = max4
          DO j=ad_from1,min5
            IF (msfcouple) THEN
              CALL PUSHREAL8(xmsf)
              xmsf = msf(i, j)
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHCONTROL1B(1)
            END IF
            IF (mucouple) THEN
              CALL PUSHREAL8(xmu)
              xmu = mu(i, j)
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHCONTROL1B(1)
            END IF
          END DO
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from1)
        END DO
      END DO
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (ibe - itf .LT. spec_zone) THEN
      IF (its .LT. ibe - spec_zone + 1) THEN
        max5 = ibe - spec_zone + 1
      ELSE
        max5 = its
      END IF
! X-end boundary
      DO i=max5,itf
        CALL PUSHINTEGER4(b_dist)
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
          ad_from2 = max6
          DO j=ad_from2,min6
            IF (msfcouple) THEN
              CALL PUSHREAL8(xmsf)
              xmsf = msf(i, j)
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHCONTROL1B(1)
            END IF
            IF (mucouple) THEN
              CALL PUSHREAL8(xmu)
              xmu = mu(i, j)
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHCONTROL1B(1)
            END IF
          END DO
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from2)
        END DO
      END DO
      xmub = 0.0
      DO i=itf,max5,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from2)
          CALL POPINTEGER4(ad_to2)
          DO j=ad_to2,ad_from2,-1
            bfield = field_bdy_xe(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xe(j, k, b_dist+1)
            tempb2 = xmsf*fieldb(i, k, j)/xmu
            bfieldb = tempb2
            xmub = xmub - bfield*tempb2/xmu
            fieldb(i, k, j) = 0.0
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              CALL POPREAL8(xmu)
              mub(i, j) = mub(i, j) + xmub
              xmub = 0.0
            END IF
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) CALL POPREAL8(xmsf)
            field_bdy_xeb(j, k, b_dist+1) = field_bdy_xeb(j, k, b_dist+1&
&             ) + bfieldb
            field_bdy_tend_xeb(j, k, b_dist+1) = field_bdy_tend_xeb(j, k&
&             , b_dist+1) + dtbc*bfieldb
          END DO
        END DO
        CALL POPINTEGER4(b_dist)
      END DO
    ELSE
      xmub = 0.0
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      DO i=min2,its,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from1)
          CALL POPINTEGER4(ad_to1)
          DO j=ad_to1,ad_from1,-1
            bfield = field_bdy_xs(j, k, b_dist+1) + dtbc*&
&             field_bdy_tend_xs(j, k, b_dist+1)
            tempb1 = xmsf*fieldb(i, k, j)/xmu
            bfieldb = tempb1
            xmub = xmub - bfield*tempb1/xmu
            fieldb(i, k, j) = 0.0
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              CALL POPREAL8(xmu)
              mub(i, j) = mub(i, j) + xmub
              xmub = 0.0
            END IF
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) CALL POPREAL8(xmsf)
            field_bdy_xsb(j, k, b_dist+1) = field_bdy_xsb(j, k, b_dist+1&
&             ) + bfieldb
            field_bdy_tend_xsb(j, k, b_dist+1) = field_bdy_tend_xsb(j, k&
&             , b_dist+1) + dtbc*bfieldb
          END DO
        END DO
        CALL POPINTEGER4(b_dist)
      END DO
    END IF
  ELSE
    xmub = 0.0
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,max2,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from0)
        CALL POPINTEGER4(ad_to0)
        DO i=ad_to0,ad_from0,-1
          bfield = field_bdy_ye(i, k, b_dist+1) + dtbc*field_bdy_tend_ye&
&           (i, k, b_dist+1)
          tempb0 = xmsf*fieldb(i, k, j)/xmu
          bfieldb = tempb0
          xmub = xmub - bfield*tempb0/xmu
          fieldb(i, k, j) = 0.0
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(xmu)
            mub(i, j) = mub(i, j) + xmub
            xmub = 0.0
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) CALL POPREAL8(xmsf)
          field_bdy_yeb(i, k, b_dist+1) = field_bdy_yeb(i, k, b_dist+1) &
&           + bfieldb
          field_bdy_tend_yeb(i, k, b_dist+1) = field_bdy_tend_yeb(i, k, &
&           b_dist+1) + dtbc*bfieldb
        END DO
      END DO
      CALL POPINTEGER4(b_dist)
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=min1,jts,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from)
        CALL POPINTEGER4(ad_to)
        DO i=ad_to,ad_from,-1
          bfield = field_bdy_ys(i, k, b_dist+1) + dtbc*field_bdy_tend_ys&
&           (i, k, b_dist+1)
          tempb = xmsf*fieldb(i, k, j)/xmu
          bfieldb = tempb
          xmub = xmub - bfield*tempb/xmu
          fieldb(i, k, j) = 0.0
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(xmu)
            mub(i, j) = mub(i, j) + xmub
            xmub = 0.0
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) CALL POPREAL8(xmsf)
          field_bdy_ysb(i, k, b_dist+1) = field_bdy_ysb(i, k, b_dist+1) &
&           + bfieldb
          field_bdy_tend_ysb(i, k, b_dist+1) = field_bdy_tend_ysb(i, k, &
&           b_dist+1) + dtbc*bfieldb
        END DO
      END DO
      CALL POPINTEGER4(b_dist)
    END DO
  END IF
END SUBROUTINE a_SPEC_BDY_FINAL
!------------------------------------------------------------------------

   SUBROUTINE a_zero_grad_bdy (  a_field,                     &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x
      REAL       :: a_aux 


      a_aux = 0.

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

    IF(.NOT.periodic_x)THEN

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = itf, max(its,ibe-spec_zone+1), -1
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = min(jtf,jbe-b_dist-1), max(jts,b_dist+jbs+1), -1
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              a_aux = a_aux + a_field(i,k,j) 
              a_field(i,k,j) = 0.
              a_field(ibe-spec_zone,k,j_inner) = a_field(ibe-spec_zone,k,j_inner) + a_aux
              a_aux = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = min(itf,ibs+spec_zone-1), its, -1
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = min(jtf,jbe-b_dist-1), max(jts,b_dist+jbs+1), -1
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              a_aux = a_aux + a_field(i,k,j) 
              a_field(i,k,j) = 0.
              a_field(ibs+spec_zone,k,j_inner) = a_field(ibs+spec_zone,k,j_inner) + a_aux
              a_aux = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = jtf, max(jts,jbe-spec_zone+1), -1
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = min(itf,ibe-b_limit), max(its,b_limit+ibs), -1
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              a_aux = a_aux + a_field(i,k,j) 
              a_field(i,k,j) = 0.
              a_field(i_inner,k,jbe-spec_zone) = a_field(i_inner,k,jbe-spec_zone) + a_aux
              a_aux = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = min(jtf,jbs+spec_zone-1), jts, -1
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = min(itf,ibe-b_limit), max(its,b_limit+ibs), -1
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              a_aux = a_aux + a_field(i,k,j)
              a_field(i,k,j) = 0.
              a_field(i_inner,k,jbs+spec_zone) = a_field(i_inner,k,jbs+spec_zone) + a_aux
              a_aux = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_zero_grad_bdy

!------------------------------------------------------------------------

   SUBROUTINE a_couple_bdy ( field, a_field,  &
                             variable_in, config_flags, & 
                             spec_zone,       &
                             mu, a_mu, msf,   &
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
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: a_mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msf
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field
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

    IF(.NOT.periodic_x)THEN
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)/msf(i,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)/msf(i,j)
              end if
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
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)/msf(i,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
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
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)/msf(i,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) + field(i,k,j)*a_field(i,k,j)/msf(i,j)
                 a_field(i,k,j) = a_field(i,k,j)*mu(i,j)/msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
   
   END SUBROUTINE a_couple_bdy 
!------------------------------------------------------------------------

   SUBROUTINE a_uncouple_bdy(  field, a_field,  &
                               variable_in, config_flags, & 
                               spec_zone,       &
                               mu, a_mu, msf,   &
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
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: a_mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: mu
      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msf
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field
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

    IF(.NOT.periodic_x)THEN
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j)) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j))*msf(i,j) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)*msf(i,j)
              end if
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
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j)) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j))*msf(i,j) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

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
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j)) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j))*msf(i,j) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              if (variable == 't' .or. variable == 'h') then 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j)) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)
              else 
                 a_mu(i,j) = a_mu(i,j) &
                           - field(i,k,j)/(mu(i,j)*mu(i,j))*msf(i,j) * a_field(i,k,j)
                 a_field(i,k,j) = a_field(i,k,j)/mu(i,j)*msf(i,j)
              end if
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_uncouple_bdy 
!------------------------------------------------------------------------

   SUBROUTINE a_flow_dep_bdy ( a_field,            &
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


      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: a_field
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: u
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: v
      TYPE(grid_config_rec_type),INTENT(IN) :: config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x
      REAL       :: a_aux 


      a_aux = 0.0

      periodic_x = config_flags%periodic_x

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

    IF(.NOT.periodic_x)THEN
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                a_aux = a_aux + a_field(i,k,j) 
                a_field(i,k,j) = 0.
                a_field(ibe-spec_zone,k,j_inner) = a_field(ibe-spec_zone,k,j_inner) + a_aux
                a_aux = 0.
              ELSE
                a_field(i,k,j) = 0.
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
                a_aux = a_aux + a_field(i,k,j) 
                a_field(i,k,j) = 0.
                a_field(ibs+spec_zone,k,j_inner) = a_field(ibs+spec_zone,k,j_inner) + a_aux
                a_aux = 0.
              ELSE
                a_field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

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
                a_aux = a_aux + a_field(i,k,j) 
                a_field(i,k,j) = 0.
                a_field(i_inner,k,jbe-spec_zone) = a_field(i_inner,k,jbe-spec_zone) + a_aux
                a_aux = 0.
              ELSE
                a_field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

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
                a_aux = a_aux + a_field(i,k,j) 
                a_field(i,k,j) = 0.
                a_field(i_inner,k,jbs+spec_zone) = a_field(i_inner,k,jbs+spec_zone) + a_aux
                a_aux = 0.
              ELSE
                a_field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_flow_dep_bdy

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.6 (r4165) - 21 sep 2011 20:54
!
!  Differentiation of flow_dep_bdy_qnn in reverse (adjoint) mode:
!   gradient     of useful results: field
!   with respect to varying inputs: field
!   RW status of diff variables: field:in-out
! domain dims
! memory dims
! patch  dims
SUBROUTINE A_FLOW_DEP_BDY_QNN(field, fieldb, u, v, config_flags, &
&  spec_zone, ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme&
&  , ips, ipe, jps, jpe, kps, kpe, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER, INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
  INTEGER, INTENT(IN) :: spec_zone
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: field
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: fieldb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: v
  TYPE(GRID_CONFIG_REC_TYPE) :: config_flags
  INTEGER :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, &
&  j_inner
  INTEGER :: b_dist, b_limit
  LOGICAL :: periodic_x
  REAL :: tmp
  REAL :: tmp0
  REAL :: tmp1
  REAL :: tmp2
  INTEGER :: branch
  INTEGER :: ad_from
  INTEGER :: ad_to
  INTEGER :: ad_from0
  INTEGER :: ad_to0
  INTEGER :: ad_from1
  INTEGER :: ad_to1
  INTEGER :: ad_from2
  INTEGER :: ad_to2
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  INTRINSIC MAX
  REAL :: tmpb
  REAL :: tmp0b
  REAL :: tmp2b
  INTRINSIC MIN
  INTEGER :: max6
  INTEGER :: max5
  INTEGER :: max4
  INTEGER :: max3
  INTEGER :: max2
  INTEGER :: max1
  REAL :: tmp1b
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
        ad_from = max1
        DO i=ad_from,min3
          IF (i .LT. ibs + spec_zone) THEN
            CALL PUSHINTEGER4(i_inner)
            i_inner = ibs + spec_zone
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(i_inner)
            i_inner = i
            CALL PUSHCONTROL1B(1)
          END IF
          IF (i_inner .GT. ibe - spec_zone) THEN
            i_inner = ibe - spec_zone
          ELSE
            i_inner = i_inner
          END IF
          IF (periodic_x) i_inner = i
          IF (v(i, k, j) .LT. 0.) THEN
            CALL PUSHCONTROL1B(1)
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
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
        ad_from0 = max3
        DO i=ad_from0,min4
          IF (i .LT. ibs + spec_zone) THEN
            CALL PUSHINTEGER4(i_inner)
            i_inner = ibs + spec_zone
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHINTEGER4(i_inner)
            i_inner = i
            CALL PUSHCONTROL1B(1)
          END IF
          IF (i_inner .GT. ibe - spec_zone) THEN
            i_inner = ibe - spec_zone
          ELSE
            i_inner = i_inner
          END IF
          IF (periodic_x) i_inner = i
          IF (v(i, k, j+1) .GT. 0.) THEN
            CALL PUSHCONTROL1B(1)
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
        END DO
        CALL PUSHINTEGER4(i - 1)
        CALL PUSHINTEGER4(ad_from0)
      END DO
    END DO
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
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
          ad_from1 = max4
          DO j=ad_from1,min5
            IF (j .LT. jbs + spec_zone) THEN
              CALL PUSHINTEGER4(j_inner)
              j_inner = jbs + spec_zone
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHINTEGER4(j_inner)
              j_inner = j
              CALL PUSHCONTROL1B(1)
            END IF
            IF (j_inner .GT. jbe - spec_zone) THEN
              j_inner = jbe - spec_zone
            ELSE
              j_inner = j_inner
            END IF
            IF (u(i, k, j) .LT. 0.) THEN
              CALL PUSHCONTROL1B(1)
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
          END DO
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from1)
        END DO
      END DO
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
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
          ad_from2 = max6
          DO j=ad_from2,min6
            IF (j .LT. jbs + spec_zone) THEN
              CALL PUSHINTEGER4(j_inner)
              j_inner = jbs + spec_zone
              CALL PUSHCONTROL1B(0)
            ELSE
              CALL PUSHINTEGER4(j_inner)
              j_inner = j
              CALL PUSHCONTROL1B(1)
            END IF
            IF (j_inner .GT. jbe - spec_zone) THEN
              j_inner = jbe - spec_zone
            ELSE
              j_inner = j_inner
            END IF
            IF (u(i+1, k, j) .GT. 0.) THEN
              CALL PUSHCONTROL1B(1)
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
          END DO
          CALL PUSHINTEGER4(j - 1)
          CALL PUSHINTEGER4(ad_from2)
        END DO
      END DO
      DO i=itf,max5,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from2)
          CALL POPINTEGER4(ad_to2)
          DO j=ad_to2,ad_from2,-1
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              fieldb(i, k, j) = 0.0
            ELSE
              tmp2b = fieldb(i, k, j)
              fieldb(i, k, j) = 0.0
              fieldb(ibe-spec_zone, k, j_inner) = fieldb(ibe-spec_zone, &
&                k, j_inner) + tmp2b
            END IF
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              CALL POPINTEGER4(j_inner)
            ELSE
              CALL POPINTEGER4(j_inner)
            END IF
          END DO
        END DO
      END DO
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      DO i=min2,its,-1
        DO k=ktf,kts,-1
          CALL POPINTEGER4(ad_from1)
          CALL POPINTEGER4(ad_to1)
          DO j=ad_to1,ad_from1,-1
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              fieldb(i, k, j) = 0.0
            ELSE
              tmp1b = fieldb(i, k, j)
              fieldb(i, k, j) = 0.0
              fieldb(ibs+spec_zone, k, j_inner) = fieldb(ibs+spec_zone, &
&                k, j_inner) + tmp1b
            END IF
            CALL POPCONTROL1B(branch)
            IF (branch .EQ. 0) THEN
              CALL POPINTEGER4(j_inner)
            ELSE
              CALL POPINTEGER4(j_inner)
            END IF
          END DO
        END DO
      END DO
    END IF
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,max2,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from0)
        CALL POPINTEGER4(ad_to0)
        DO i=ad_to0,ad_from0,-1
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            fieldb(i, k, j) = 0.0
          ELSE
            tmp0b = fieldb(i, k, j)
            fieldb(i, k, j) = 0.0
            fieldb(i_inner, k, jbe-spec_zone) = fieldb(i_inner, k, jbe-&
&              spec_zone) + tmp0b
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(i_inner)
          ELSE
            CALL POPINTEGER4(i_inner)
          END IF
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=min1,jts,-1
      DO k=ktf,kts,-1
        CALL POPINTEGER4(ad_from)
        CALL POPINTEGER4(ad_to)
        DO i=ad_to,ad_from,-1
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            fieldb(i, k, j) = 0.0
          ELSE
            tmpb = fieldb(i, k, j)
            fieldb(i, k, j) = 0.0
            fieldb(i_inner, k, jbs+spec_zone) = fieldb(i_inner, k, jbs+&
&              spec_zone) + tmpb
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPINTEGER4(i_inner)
          ELSE
            CALL POPINTEGER4(i_inner)
          END IF
        END DO
      END DO
    END DO
  END IF
END SUBROUTINE A_FLOW_DEP_BDY_QNN

END MODULE a_module_bc
