!WRF+/AD:MODEL_LAYER:BOUNDARY
!Created by Ning Pan, 2010-08
!

MODULE a_module_bc

   USE module_configure
   USE module_wrf_error

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


   CALL a_relax_bdytend_core ( a_field, a_field_tend,  &
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

   SUBROUTINE a_relax_bdytend_core ( a_field, a_field_tend, &
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
                       its,ite, jts,jte, kts,kte,  & ! patch  dims
                       iXs,iXe, jXs,jXe, kXs,kXe   & ! field (1st arg) dims; might be tile or patch
                       )

!  spec_bdy_width is only used to dimension the boundary arrays.
!  relax_zone is the inner edge of the boundary relaxation zone
!  spec_zone is the width of the outer specified b.c.s

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

   CHARACTER  :: variable
   INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
   INTEGER    :: b_dist, b_limit
   REAL       :: a_fls0, a_fls1, a_fls2, a_fls3, a_fls4
   LOGICAL    :: periodic_x


   a_fls0 = 0.
   a_fls1 = 0.
   a_fls2 = 0.
   a_fls3 = 0.
   a_fls4 = 0.

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

   IF (.NOT.periodic_x) THEN
     IF (ibe - itf .lt. relax_zone) THEN
! X-end boundary
       DO i = min(itf,ibe-spec_zone), max(its,ibe-relax_zone+1), -1
         b_dist = ibe - i
         DO k = kts, ktf
           DO j = min(jtf,jbe-b_dist-1), max(jts,b_dist+jbs+1), -1
             a_fls0 = a_fls0 + ( fcx(b_dist+1)+gcx(b_dist+1)*4 ) * a_field_tend(i,k,j)
             a_fls1 = a_fls1 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls2 = a_fls2 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls3 = a_fls3 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls4 = a_fls4 - gcx(b_dist+1) * a_field_tend(i,k,j) 

             a_field_bdy_xe(j, k, b_dist+2) = a_field_bdy_xe(j, k, b_dist+2) + a_fls4
             a_field_bdy_tend_xe(j, k, b_dist+2) = a_field_bdy_tend_xe(j, k, b_dist+2) &
                                                 + dtbc * a_fls4
             a_field(i-1,k,j) = a_field(i-1,k,j) - a_fls4
             a_fls4 = 0.

             a_field_bdy_xe(j, k, b_dist) = a_field_bdy_xe(j, k, b_dist) + a_fls3
             a_field_bdy_tend_xe(j, k, b_dist) = a_field_bdy_tend_xe(j, k, b_dist) &
                                               + dtbc * a_fls3
             a_field(i+1,k,j) = a_field(i+1,k,j) - a_fls3
             a_fls3 = 0.

             a_field_bdy_xe(j+1, k, b_dist+1) = a_field_bdy_xe(j+1, k, b_dist+1) + a_fls2
             a_field_bdy_tend_xe(j+1,k,b_dist+1) = a_field_bdy_tend_xe(j+1,k,b_dist+1) &
                                                 + dtbc * a_fls2
             a_field(i,k,j+1) = a_field(i,k,j+1) - a_fls2
             a_fls2 = 0.

             a_field_bdy_xe(j-1, k, b_dist+1) = a_field_bdy_xe(j-1, k, b_dist+1) + a_fls1
             a_field_bdy_tend_xe(j-1,k,b_dist+1) = a_field_bdy_tend_xe(j-1,k,b_dist+1) &
                                                 + dtbc * a_fls1
             a_field(i,k,j-1) = a_field(i,k,j-1) - a_fls1
             a_fls1 = 0.

             a_field_bdy_xe(j, k, b_dist+1) = a_field_bdy_xe(j, k, b_dist+1) + a_fls0
             a_field_bdy_tend_xe(j, k, b_dist+1) = a_field_bdy_tend_xe(j, k, b_dist+1) &
                                                 + dtbc * a_fls0
             a_field(i,k,j) = a_field(i,k,j) - a_fls0
             a_fls0 = 0.
           ENDDO
         ENDDO
       ENDDO
     ENDIF

     IF (its - ibs .lt. relax_zone) THEN
! X-start boundary
       DO i = min(itf,ibs+relax_zone-1), max(its,ibs+spec_zone), -1
         b_dist = i - ibs
         DO k = kts, ktf
           DO j = min(jtf,jbe-b_dist-1), max(jts,b_dist+jbs+1), -1
             a_fls0 = a_fls0 + ( fcx(b_dist+1)+gcx(b_dist+1)*4 ) * a_field_tend(i,k,j)
             a_fls1 = a_fls1 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls2 = a_fls2 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls3 = a_fls3 - gcx(b_dist+1) * a_field_tend(i,k,j) 
             a_fls4 = a_fls4 - gcx(b_dist+1) * a_field_tend(i,k,j) 

             a_field_bdy_xs(j, k, b_dist+2) = a_field_bdy_xs(j, k, b_dist+2) + a_fls4
             a_field_bdy_tend_xs(j, k, b_dist+2) = a_field_bdy_tend_xs(j, k, b_dist+2) &
                                                 + dtbc * a_fls4
             a_field(i+1,k,j) = a_field(i+1,k,j) - a_fls4
             a_fls4 = 0.

             a_field_bdy_xs(j, k, b_dist) = a_field_bdy_xs(j, k, b_dist) + a_fls3
             a_field_bdy_tend_xs(j, k, b_dist) = a_field_bdy_tend_xs(j, k, b_dist) &
                                               + dtbc * a_fls3
             a_field(i-1,k,j) = a_field(i-1,k,j) - a_fls3
             a_fls3 = 0.

             a_field_bdy_xs(j+1, k, b_dist+1) = a_field_bdy_xs(j+1, k, b_dist+1) + a_fls2
             a_field_bdy_tend_xs(j+1,k,b_dist+1) = a_field_bdy_tend_xs(j+1,k,b_dist+1) &
                                                 + dtbc * a_fls2
             a_field(i,k,j+1) = a_field(i,k,j+1) - a_fls2
             a_fls2 = 0.

             a_field_bdy_xs(j-1, k, b_dist+1) = a_field_bdy_xs(j-1, k, b_dist+1) + a_fls1
             a_field_bdy_tend_xs(j-1,k,b_dist+1) = a_field_bdy_tend_xs(j-1,k,b_dist+1) &
                                                 + dtbc * a_fls1
             a_field(i,k,j-1) = a_field(i,k,j-1) - a_fls1
             a_fls1 = 0.

             a_field_bdy_xs(j, k, b_dist+1) = a_field_bdy_xs(j, k, b_dist+1) + a_fls0
             a_field_bdy_tend_xs(j, k, b_dist+1) = a_field_bdy_tend_xs(j, k, b_dist+1) &
                                                 + dtbc * a_fls0
             a_field(i,k,j) = a_field(i,k,j) - a_fls0
             a_fls0 = 0.
           ENDDO
         ENDDO
       ENDDO
     ENDIF

   ENDIF

   IF (jbe - jtf .lt. relax_zone) THEN
! Y-end boundary
     DO j = min(jtf,jbe-spec_zone), max(jts,jbe-relax_zone+1), -1
       b_dist = jbe - j
       b_limit = b_dist
       IF(periodic_x)b_limit = 0
       DO k = kts, ktf
         DO i = min(itf,ibe-b_limit), max(its,b_limit+ibs), -1
           im1 = max(i-1,ibs)
           ip1 = min(i+1,ibe)

           a_fls0 = a_fls0 + ( fcx(b_dist+1)+gcx(b_dist+1)*4 ) * a_field_tend(i,k,j)
           a_fls1 = a_fls1 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls2 = a_fls2 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls3 = a_fls3 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls4 = a_fls4 - gcx(b_dist+1) * a_field_tend(i,k,j) 

           a_field_bdy_ye(i, k, b_dist+2) = a_field_bdy_ye(i, k, b_dist+2) + a_fls4
           a_field_bdy_tend_ye(i, k, b_dist+2) = a_field_bdy_tend_ye(i, k, b_dist+2) &
                                               + dtbc * a_fls4
           a_field(i,k,j-1) = a_field(i,k,j-1) - a_fls4
           a_fls4 = 0.

           a_field_bdy_ye(i, k, b_dist) = a_field_bdy_ye(i, k, b_dist) + a_fls3
           a_field_bdy_tend_ye(i, k, b_dist) = a_field_bdy_tend_ye(i, k, b_dist) &
                                             + dtbc * a_fls3
           a_field(i,k,j+1) = a_field(i,k,j+1) - a_fls3
           a_fls3 = 0.

           a_field_bdy_ye(ip1, k, b_dist+1) = a_field_bdy_ye(ip1, k, b_dist+1) + a_fls2
           a_field_bdy_tend_ye(ip1,k,b_dist+1) = a_field_bdy_tend_ye(ip1,k,b_dist+1) &
                                               + dtbc * a_fls2
           a_field(ip1,k,j) = a_field(ip1,k,j) - a_fls2
           a_fls2 = 0.

           a_field_bdy_ye(im1, k, b_dist+1) = a_field_bdy_ye(im1, k, b_dist+1) + a_fls1
           a_field_bdy_tend_ye(im1,k,b_dist+1) = a_field_bdy_tend_ye(im1,k,b_dist+1) &
                                               + dtbc * a_fls1
           a_field(im1,k,j) = a_field(im1,k,j) - a_fls1
           a_fls1 = 0.

           a_field_bdy_ye(i, k, b_dist+1) = a_field_bdy_ye(i, k, b_dist+1) + a_fls0
           a_field_bdy_tend_ye(i, k, b_dist+1) = a_field_bdy_tend_ye(i, k, b_dist+1) &
                                               + dtbc * a_fls0
           a_field(i,k,j) = a_field(i,k,j) - a_fls0
           a_fls0 = 0.
         ENDDO
       ENDDO
     ENDDO
   ENDIF

   IF (jts - jbs .lt. relax_zone) THEN
! Y-start boundary
     DO j = min(jtf,jbs+relax_zone-1), max(jts,jbs+spec_zone), -1
       b_dist = j - jbs
       b_limit = b_dist
       IF(periodic_x)b_limit = 0
       DO k = kts, ktf
         DO i = min(itf,ibe-b_limit), max(its,b_limit+ibs), -1
           im1 = max(i-1,ibs)
           ip1 = min(i+1,ibe)

           a_fls0 = a_fls0 + ( fcx(b_dist+1)+gcx(b_dist+1)*4 ) * a_field_tend(i,k,j)
           a_fls1 = a_fls1 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls2 = a_fls2 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls3 = a_fls3 - gcx(b_dist+1) * a_field_tend(i,k,j) 
           a_fls4 = a_fls4 - gcx(b_dist+1) * a_field_tend(i,k,j) 

           a_field_bdy_ys(i, k, b_dist+2) = a_field_bdy_ys(i, k, b_dist+2) + a_fls4
           a_field_bdy_tend_ys(i, k, b_dist+2) = a_field_bdy_tend_ys(i, k, b_dist+2) &
                                               + dtbc * a_fls4
           a_field(i,k,j+1) = a_field(i,k,j+1) - a_fls4
           a_fls4 = 0.

           a_field_bdy_ys(i, k, b_dist) = a_field_bdy_ys(i, k, b_dist) + a_fls3
           a_field_bdy_tend_ys(i, k, b_dist) = a_field_bdy_tend_ys(i, k, b_dist) &
                                             + dtbc * a_fls3
           a_field(i,k,j-1) = a_field(i,k,j-1) - a_fls3
           a_fls3 = 0.

           a_field_bdy_ys(ip1, k, b_dist+1) = a_field_bdy_ys(ip1, k, b_dist+1) + a_fls2
           a_field_bdy_tend_ys(ip1,k,b_dist+1) = a_field_bdy_tend_ys(ip1,k,b_dist+1) &
                                               + dtbc * a_fls2
           a_field(ip1,k,j) = a_field(ip1,k,j) - a_fls2
           a_fls2 = 0.

           a_field_bdy_ys(im1, k, b_dist+1) = a_field_bdy_ys(im1, k, b_dist+1) + a_fls1
           a_field_bdy_tend_ys(im1,k,b_dist+1) = a_field_bdy_tend_ys(im1,k,b_dist+1) &
                                               + dtbc * a_fls1
           a_field(im1,k,j) = a_field(im1,k,j) - a_fls1
           a_fls1 = 0.

           a_field_bdy_ys(i, k, b_dist+1) = a_field_bdy_ys(i, k, b_dist+1) + a_fls0
           a_field_bdy_tend_ys(i, k, b_dist+1) = a_field_bdy_tend_ys(i, k, b_dist+1) &
                                               + dtbc * a_fls0
           a_field(i,k,j) = a_field(i,k,j) - a_fls0
           a_fls0 = 0.
         ENDDO
       ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE a_relax_bdytend_core

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

END MODULE a_module_bc
