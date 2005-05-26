!REAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_state_description
   USE module_timing
   USE module_soil_pre
#ifdef DM_PARALLEL
   USE module_dm
#endif


CONTAINS

!-------------------------------------------------------------------

   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid 
      TYPE (domain)          :: grid 

      !  Local data.

      INTEGER :: dyn_opt 
      INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

      CALL nl_get_dyn_opt ( head_grid%id, dyn_opt )
      
      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

      IF (      dyn_opt .eq. 1 &
           .or. dyn_opt .eq. 2 &
           .or. dyn_opt .eq. 3 &
                                          ) THEN
        CALL wrf_error_fatal ( "no RK version within dyn_nmm, dyn_opt wrong in namelist, wrf_error_fataling" )

     ELSEIF ( dyn_opt .eq. 4 ) THEN

        CALL init_domain_nmm (grid, &
!
#include <nmm_actual_args.inc>
!
      )

      ELSE
         WRITE(0,*)' init_domain: unknown or unimplemented dyn_opt = ',dyn_opt
        CALL wrf_error_fatal ( "ERROR-dyn_opt-wrong-in-namelist" )
      ENDIF

   END SUBROUTINE init_domain

!-------------------------------------------------------------------
!---------------------------------------------------------------------
   SUBROUTINE init_domain_nmm ( grid, &
!
# include <nmm_dummy_args.inc>
!
   )

      USE module_optional_si_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

# include <nmm_dummy_decl.inc>

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     i, j, k, NNXP, NNYP, ICOUNT

      !  Local data

        CHARACTER(LEN=19):: start_date

      CHARACTER (LEN=132) :: message

      INTEGER :: error
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a
      REAL    :: hold_znw, rmin,rmax

      LOGICAL :: stretch_grid, dry_sounding, debug, log_flag_sst
      LOGICAL :: wrf_dm_on_monitor

        REAL, ALLOCATABLE,DIMENSION(:,:):: ADUM2D,SNOWC,HT,TG_ALT

        INTEGER, ALLOCATABLE, DIMENSION(:):: KHL2,KVL2,KHH2,KVH2, &
                                             KHLA,KHHA,KVLA,KVHA

!        INTEGER, ALLOCATABLE, DIMENSION(:,:):: LU_INDEX

        REAL, ALLOCATABLE, DIMENSION(:):: DXJ,WPDARJ,CPGFUJ,CURVJ, &
                                          FCPJ,FDIVJ,EMJ,EMTJ,FADJ, &
                                          HDACJ,DDMPUJ,DDMPVJ

        REAL:: TPH0D,TLM0D
        REAL:: TPH0,WB,SB,DLM,DPH,TDLM,TDPH
        REAL:: WBI,SBI,EBI,ANBI,STPH0,CTPH0
        REAL:: TSPH,DTAD,DTCF
        REAL:: ACDT,CDDAMP,TPH,DXP,TLM,FP
        REAL:: CTPH,STPH
        REAL:: WBD,SBD
        REAL:: RSNOW,SNOFAC
        REAL, PARAMETER:: SALP=2.60
        REAL, PARAMETER:: SNUP=0.040
        REAL:: SMCSUM,STCSUM,SEAICESUM,FISX
        REAL:: TERM1,APH



        INTEGER:: KHH,KVH,JAM,JA, IHL, IHH, L
        INTEGER:: II,JJ,ISRCH,ISUM,ival,jval,ITER

        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: W_NMM=0.08
!0904        REAL, PARAMETER:: COAC=0.2
        REAL, PARAMETER:: COAC=0.75
        REAL, PARAMETER:: CODAMP=6.4
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: CP=1004.6
        REAL, PARAMETER:: DFC=1.0
        REAL, PARAMETER:: DDFC=1.0
        REAL, PARAMETER:: ROI=916.6
        REAL, PARAMETER:: R=287.04
        REAL, PARAMETER:: CI=2060.0
        REAL, PARAMETER:: ROS=1500.
        REAL, PARAMETER:: CS=1339.2
        REAL, PARAMETER:: DS=0.050
        REAL, PARAMETER:: AKS=.0000005
        REAL, PARAMETER:: DZG=2.85
        REAL, PARAMETER:: DI=.1000
        REAL, PARAMETER:: AKI=0.000001075
        REAL, PARAMETER:: DZI=2.0
        REAL, PARAMETER:: THL=210.
        REAL, PARAMETER:: PLQ=70000.
        REAL, PARAMETER:: ERAD=6371200.
        REAL, PARAMETER:: TG0=258.16
        REAL, PARAMETER:: TGA=30.0


#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

        if (ALLOCATED(ADUM2D)) DEALLOCATE(ADUM2D)
        if (ALLOCATED(TG_ALT)) DEALLOCATE(TG_ALT)

#define COPY_IN
#include <nmm_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <nmm_data_calls.inc>
#endif

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; ! tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

      END SELECT

        
        DT=float(TIME_STEP)

        

!       NNXP=IDE-1
!       NNYP=JDE-1

        NNXP=min(ITE,IDE-1)
        NNYP=min(JTE,JDE-1)

        write(0,*) 'nnxp,nnyp: ', nnxp,nnyp
        write(0,*) 'IDE, JDE: ', IDE,JDE

        JAM=6+2*(JDE-JDS-10)

        ALLOCATE(ADUM2D(grid%sm31:grid%em31,grid%sm33:grid%em33))
        ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
        ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
        ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),&
                 FADJ(JTS:NNYP))
        ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
        ALLOCATE(KHLA(JAM),KHHA(JAM))
        ALLOCATE(KVLA(JAM),KVHA(JAM))


    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

        write(0,*) 'cen_lat: ', config_flags%cen_lat
        write(0,*) 'cen_lon: ', config_flags%cen_lon
!       write(0,*) 'truelat?: ', config_flags%moad_stand_lats(1)
        write(0,*) 'dx: ', config_flags%dx
        write(0,*) 'dy: ', config_flags%dy
        write(0,*) 'config_flags%start_year: ', config_flags%start_year
        write(0,*) 'config_flags%start_month: ', config_flags%start_month
        write(0,*) 'config_flags%start_day: ', config_flags%start_day
        write(0,*) 'config_flags%start_hour: ', config_flags%start_hour


        write(start_date,435) config_flags%start_year, config_flags%start_month, &
                config_flags%start_day, config_flags%start_hour
  435   format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':00:00')
        
        dlmd=config_flags%dx
        dphd=config_flags%dy

!       tph0d=global_meta%moad_known_lat
!       tlm0d=global_meta%moad_known_lon

        tph0d=config_flags%cen_lat
        tlm0d=config_flags%cen_lon

        ival=ite-15
        jval=jte-15

!==========================================================================

!!

 !  Check to see if the boundary conditions are set 
 !  properly in the namelist file.
 !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      real_data_init_type = model_config_rec%real_data_init_type

!       DO j = jts, MIN(jte,jde-1)
!        DO k = kts, kde-1
!         DO i = its, MIN(ite,ide-1)
!        HTM(I,K,J)=1.
!        VTM(I,K,J)=1.
!         ENDDO
!        ENDDO
!       ENDDO


!!! WEASD has "snow water equivalent" in mm

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

      IF(SM(I,J).GT.0.9) THEN

       IF (XICE(I,J) .gt. 0) then
         SI(I,J)=1.0
       ENDIF

!  SEA
              EPSR(I,J)=.97
              GFFC(I,J)=0.
              ALBEDO(I,J)=.06
              ALBASE(I,J)=.06
              IF(SI (I,J).GT.0.    ) THEN
!  SEA-ICE
                 SM(I,J)=0.
                 SI(I,J)=0.
                 SICE(I,J)=1.
                 GFFC(I,J)=0. ! just leave zero as irrelevant
                ALBEDO(I,J)=.60
                 ALBASE(I,J)=.60
              ENDIF
           ELSE

        SI(I,J)=5.0*WEASD(I,J)/1000.
!  LAND
        EPSR(I,J)=1.0
        GFFC(I,J)=0.0 ! just leave zero as irrelevant
        SICE(I,J)=0.
        SNO(I,J)=SI(I,J)*.20
           ENDIF
        ENDDO
        ENDDO

! DETERMINE ALBEDO OVER LAND
       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
          IF(SM(I,J).LT.0.9.AND.SICE(I,J).LT.0.9) THEN
! SNOWFREE ALBEDO
            IF ( (SNO(I,J) .EQ. 0.0) .OR. &
                (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN
              ALBEDO(I,J) = ALBASE(I,J)
            ELSE
! MODIFY ALBEDO IF SNOWCOVER:
! BELOW SNOWDEPTH THRESHOLD...
              IF (SNO(I,J) .LT. SNUP) THEN
                RSNOW = SNO(I,J)/SNUP
                SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
! ABOVE SNOWDEPTH THRESHOLD...
              ELSE
                SNOFAC = 1.0
              ENDIF
! CALCULATE ALBEDO ACCOUNTING FOR SNOWDEPTH AND VGFRCK
              ALBEDO(I,J) = ALBASE(I,J) &
               + (1.0-VEGFRA(I,J))*SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
            ENDIF
          END IF

        ENDDO
      ENDDO

!new seaice stuff

!       write(0,*) 'skip seaice'
!       goto 979

       DO j = jts, MIN(jte,jde-1)
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

        DO ITER=1,10    

       DO j = jts+1, MIN(jte,jde-1)-1
         DO i = its+1, MIN(ite,ide-1)-1

! any sea ice around point in question?

        IF (SM(I,J) .eq. 1) THEN
        
        SEAICESUM=SICE(I+IHE(J),J+1)+SICE(I+IHW(J),J+1)+ &
                  SICE(I+IHE(J),J-1)+SICE(I+IHW(J),J-1)

        IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 3.) THEN

        IF ((SICE(I+IHE(J),J+1).eq.0 .and. SM(I+IHE(J),J+1).eq.0) .OR. &
            (SICE(I+IHW(J),J+1).eq.0 .and. SM(I+IHW(J),J+1).eq.0) .OR. &
            (SICE(I+IHE(J),J-1).eq.0 .and. SM(I+IHE(J),J-1).eq.0) .OR. &
            (SICE(I+IHW(J),J-1).eq.0 .and. SM(I+IHW(J),J-1).eq.0)) THEN 

!       HAVE SEA ICE AND A SURROUNDING LAND POINT - CONVERT TO SEA ICE

        write(0,*) 'MAKING SEA ICE AT I,J: ', I,J, 'on iter: ', iter
        SICE(I,J)=1.0
        SM(I,J)=0.

        ENDIF

        ELSEIF (SEAICESUM .ge. 3) THEN

!       WATER POINT SURROUNDED BY ICE  - CONVERT TO SEA ICE

        write(0,*) 'MAKING SEA ICE(2) AT I,J: ', I,J, 'on iter: ', iter
        SICE(I,J)=1.0
        SM(I,J)=0.

        ENDIF
        
        ENDIF

        ENDDO
        ENDDO

        ENDDO

        
        
  979   continue


!new new seaice stuff

! this block meant to guarantee land/sea agreement between SM and landmask

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

        if (SM(I,J) .gt. 0.5) then
                landmask(I,J)=0.0
        elseif (SM(I,J) .eq. 0 .and. SICE(I,J) .eq. 1) then
                landmask(I,J)=0.0
        elseif (SM(I,J) .lt. 0.5 .and. SICE(I,J) .eq. 0) then
                landmask(I,J)=1.0
        else
        write(0,*) 'missed point in landmask definition ' , I,J
        landmask(I,J)=0.0
        endif

        ENDDO
      ENDDO

      !  For sf_surface_physics = 1, we want to use close to a 10 cm value
      !  for the bottom level of the soil temps.

      IF      ( ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .AND. &
                ( flag_st000010 .EQ. 1 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               soiltb(i,j) = st000010(i,j)
            END DO
         END DO
!      ELSE IF ( ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .AND. &
!                ( flag_soilt020 .EQ. 1 ) ) THEN
!         DO j = jts , MIN(jde-1,jte)
!            DO i = its , MIN(ide-1,ite)
!               soiltb(i,j) = soilt020(i,j)
!            END DO
!         END DO
      END IF
!     write(0,*)' init_domain_nmm flag_toposoil=',flag_toposoil


  !  Adjust the various soil temperature values depending on the difference in
  !  in elevation between the current model's elevation and the incoming data's
  !  orography.

        write(0,*) 'flag_toposoil= ', flag_toposoil
         
      IF ( ( flag_toposoil .EQ. 1 ) ) THEN 

        ALLOCATE(HT(ims:ime,jms:jme))

        DO J=jms,jme
          DO I=ims,ime
              HT(I,J)=FIS(I,J)/9.81
          END DO
        END DO
           
!       if (maxval(toposoil) .gt. 100.) then

!  being avoided.   Something to revisit eventually.
!
!1219 might be simply a matter of including TOPOSOIL 
!
!    CODE NOT TESTED AT NCEP USING THIS FUNCTIONALITY, 
!    SO TO BE SAFE WILL AVOID FOR RETRO RUNS.

!       write(0,*) 'calling adjust_soil_temp_new'
!        CALL adjust_soil_temp_new ( soiltb , 2 , &
!                       nmm_tsk , ht , toposoil , landmask, flag_toposoil , &
!                       st000010 , st010040 , st040100 , st100200 , st010200 , &
!                       flag_st000010 , flag_st010040 , flag_st040100 , &
!                       flag_st100200 , flag_st010200 , &
!                       soilt000 , soilt005 , soilt020 , soilt040 , &
!                       soilt160 , soilt300 , &
!                       flag_soilt000 , flag_soilt005 , flag_soilt020 , &
!                       flag_soilt040 , flag_soilt160 , flag_soilt300 , &
!                       ids , ide , jds , jde , kds , kde , &
!                       ims , ime , jms , jme , kms , kme , &
!                       its , ite , jts , jte , kts , kte )
!       endif

      END IF

      !  Process the LSM data.
   
      IF ( real_data_init_type .EQ. 1 ) THEN
   
         num_veg_cat      = SIZE ( landusef , DIM=2 )
         num_soil_top_cat = SIZE ( soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )



!       sm (1=water, 0=land)
!       landmask(0=water, 1=land)



         CALL process_percent_cat_new ( landmask , &
                         landusef , soilctop , soilcbot , &
                         isltyp , ivgtyp , &
                         num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte , &
                         model_config_rec%iswater(grid%id) )


        DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (SICE(I,J) .eq. 0) THEN

        if (landmask(I,J) .gt. 0.5 .and. sm(I,J) .eq. 1.0) then
                write(0,*) 'land mask and SM both > 0.5: ', &
                                           I,J,landmask(I,J),sm(I,J)

        SM(I,J)=0.

        elseif (landmask(I,J) .lt. 0.5 .and. sm(I,J) .eq. 0.0) then
                write(0,*) 'land mask and SM both < 0.5: ', &
                                           I,J, landmask(I,J),sm(I,J)

        SM(I,J)=1.

        endif

        ELSE

        if (landmask(I,J) .gt. 0.5 .and. SM(I,J)+SICE(I,J) .eq. 1) then
        write(0,*) 'landmask says LAND, SM/SICE say SEAICE: ', I,J
        endif

        ENDIF

           ENDDO
        ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SICE(I,J) .eq. 1.0) then
!!!! change vegtyp and sltyp to fit seaice (desireable??)
        ISLTYP(I,J)=16
        IVGTYP(I,J)=24
        endif

            ENDDO
         ENDDO


! MOVE HERE

        write(0,*) 'flag_sst before define is: ', flag_sst
!     write(0,*)' init_domain_nmm flag_sst=',flag_sst
        FLAG_SST=1

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SM(I,J) .lt. 0.5) then
            SST(I,J)=0.
        endif

        if (SM(I,J) .gt. 0.5) then
          if (SST(I,J) .eq. 0) then
            SST(I,J)=NMM_TSK(I,J)
          endif
            NMM_TSK(I,J)=0.
        endif

                
        if ( (NMM_TSK(I,J)+SST(I,J)) .lt. 200. .or. &
             (NMM_TSK(I,J)+SST(I,J)) .gt. 350. ) then
        write(0,*) 'TSK, SST trouble at : ', I,J
        write(0,*) 'SM= ', SM(I,J)
        write(0,*) 'NMM_TSK(I,J), SST(I,J): ', NMM_TSK(I,J), SST(I,J)
        endif

            ENDDO
         ENDDO

        write(0,*) 'SM'
        do J=min(jde-1,jte),jts,-(jte-jts)/15
        write(0,635) (sm(i,J),I=its,ite,(ite-its)/10)
        enddo

!       write(0,*) 'SST/NMM_TSK'
!       do J=min(jde-1,jte),jts,-(jte-jts)/15
!       write(0,635) (SST(I,J)+NMM_TSK(I,J),I=ITS,min(ide-1,ite),(ite-its)/10)
!       enddo

  635   format(20(f5.1,1x))


         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  soiltb(i,j) = sst(i,j)
!curious               ELSE IF (  landmask(i,j) .LT. 0.5 ) THEN
               ELSE IF (  landmask(i,j) .GT. 0.5 ) THEN
                  soiltb(i,j) = nmm_tsk(i,j)
               END IF
            END DO
         END DO

!      END IF



! END MOVE HERE


   !  Land use categories, dominant soil and vegetation types (if available).

!       allocate(lu_index(ims:ime,jms:jme))
   
          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               lu_index(i,j) = ivgtyp(i,j)
            END DO
         END DO

      END IF


        if (flag_sst .eq. 1) log_flag_sst=.true.
        if (flag_sst .eq. 0) log_flag_sst=.false.

!       write(0,*) 'st_levels_input: ', st_levels_input
!       write(0,*) 'num_st_levels_input: ', num_st_levels_input


        write(0,*) 'maxval st_input(1): ', maxval(st_input(:,1,:))
        write(0,*) 'maxval st_input(2): ', maxval(st_input(:,2,:))
        write(0,*) 'maxval st_input(3): ', maxval(st_input(:,3,:))
        write(0,*) 'maxval st_input(4): ', maxval(st_input(:,4,:))

!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!

        ALLOCATE(TG_ALT(grid%sm31:grid%em31,grid%sm33:grid%em33))


      TPH0=TPH0D*DTR
!     WBD=-((nnxp-1)*DLMD)
      WBD=-(((ide-1)-1)*DLMD)
      WB= WBD*DTR
!     SBD=-((nnyp/2)*DPHD)
      SBD=-(((jde-1)/2)*DPHD)
      SB= SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      WBI=WB+TDLM
      SBI=SB+TDPH
!     EBI=WB+(nnxp-2)*TDLM
      EBI=WB+(ide-2)*TDLM
      ANBI=SB+(jde-2)*DPH
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
      TSPH=3600./DT
         DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J,2)*DLM   !For velocity points on the E grid
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
             FP=TWOM*(TERM1)
             F(I,J)=0.5*DT*FP
           ENDDO
         ENDDO
         DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J+1,2)*DLM   !For mass points on the E grid
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
             APH=ASIN(TERM1)
             TG_ALT(I,J)=TG0+TGA*COS(APH)-FIS(I,J)/3333.
           ENDDO
         ENDDO



            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
!                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
!                         SICE(I,J) .eq. 0. ) THEN
!                     TG(i,j) = sst(i,j)
!                   ELSEIF (SICE(I,J) .eq. 1) THEN
!                     TG(i,j) = 271.16
!                   END IF

        if (TG(I,J) .lt. 200.) then   ! only use default TG_ALT definition if
                                      ! not getting TGROUND from SI
                TG(I,J)=TG_ALT(I,J)
        endif

       if (TG(I,J) .lt. 200. .or. TG(I,J) .gt. 320.) then
               write(message,*) 'problematic TG point at : ', I,J
               CALL wrf_message( message )
       endif

        adum2d(i,j)=nmm_tsk(I,J)+sst(I,J)

               END DO
            END DO

        DEALLOCATE(TG_ALT)

  CALL process_soil_real ( adum2d, TG , &
     landmask, sst, &
     st_input, sm_input, sw_input, &
     st_levels_input , sm_levels_input , &
     sw_levels_input , &
     sldpth , dzsoil , stc , smc , sh2o,  &
     flag_sst , flag_soilt000, flag_soilm000, &
     ids , ide , jds , jde , kds , kde , &
     ims , ime , jms , jme , kms , kme , &
     its , ite , jts , jte , kts , kte , &
     model_config_rec%sf_surface_physics(grid%id) , &
     model_config_rec%num_soil_layers ,  &
     model_config_rec%real_data_init_type , &
     num_st_levels_input , num_sm_levels_input , &
     num_sw_levels_input , &
     num_st_levels_alloc , num_sm_levels_alloc , &
     num_sw_levels_alloc )
      write(0,*)' its=',its,' ite=',ite,' jts=',jts,' jte=',jte
      write(0,*)' ide=',ide,' jde=',jde

!!!     zero out NMM_TSK at water points again

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
        if (SM(I,J) .gt. 0.5) then
            NMM_TSK(I,J)=0.
        endif
            END DO
         END DO


!!      check on STC

          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (SICE(I,J) .eq. 1.0) then
          DO L = 1, grid%num_soil_layers
            STC(I,L,J)=271.16    ! TG value used by Eta/NMM
          END DO
        END IF
                
            END DO
          END DO

        write(0,*) 'STC(1)'
        do J=min(jde-1,jte),jts,-(jte-jts)/15
        write(0,635) (stc(I,1,J),I=its,ite,(ite-its)/12)
        enddo


        ICOUNT=0
          DO j = jts, MIN(jde-1,jte)
          DO i=  ITS, MIN(IDE-1,ITE)
                      IF (STC(I,1,J) .lt. 200. .and. SM(I,J) .eq. 0) then
              write(0,*) 'troublesome STC...I,J,STC,SM,SICE,SMC: ',& 
                   I,J,STC(I,1,J),SM(I,J),SICE(I,J),SMC(I,1,J)
        ICOUNT=ICOUNT+1
        if (ICOUNT .eq. 100) then
        call wrf_error_fatal("bad STC data...quit before gets worse")
        endif
            endif
        
         ENDDO
         ENDDO

!hardwire soil stuff for time being

        RTDPTH=0.
        RTDPTH(1)=0.1
        RTDPTH(2)=0.3
        RTDPTH(3)=0.6

        SLDPTH=0.
        SLDPTH(1)=0.1
        SLDPTH(2)=0.3
        SLDPTH(3)=0.6
        SLDPTH(4)=1.0

        write(0,*) 'SLDPTH: ', SLDPTH(1:4)
        write(0,*) 'RTDPTH: ', RTDPTH(1:4)

!!! main body of nmm_specific starts here

!
! Gopal's doing's : LMH and LMV should be flipped for start_domain_nmm.F
!                   this is consistent with Tom's version
!

       do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)
         LMH(I,J)= kme-1        !1
         LMV(I,J)= kme-1        !1
         RES(I,J)=1.
        enddo
        enddo



!! HBM2

        HBM2=0.

       do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)


        IF ( (J .ge. 3 .and. J .le. (jde-1)-2) .AND. &
             (I .ge. 2 .and. I .le. (ide-1)-2+mod(J,2)) ) THEN
       HBM2(I,J)=1.
        ENDIF
       enddo
       enddo


  636   format(60(f2.0))


!! HBM3
        HBM3=0.


!!      LOOP OVER LOCAL DIMENSIONS

       do J=jts,min(jte,jde-1)
          IHWG(J)=mod(J+1,2)-1
          IF (J .ge. 4 .and. J .le. (jde-1)-3) THEN
            IHL=(ids+1)-IHWG(J)
            IHH=(ide-1)-2
            do I=its,min(ite,ide-1)
              IF (I .ge. IHL  .and. I .le. IHH) HBM3(I,J)=1.
            enddo
          ENDIF
        enddo


!! VBM2

       VBM2=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 3 .and. J .le. (jde-1)-2)  .AND.  &
             (I .ge. 2 .and. I .le. (ide-1)-1-mod(J,2)) ) THEN

           VBM2(I,J)=1.

        ENDIF

       enddo
       enddo

!! VBM3

        VBM3=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 4 .and. J .le. (jde-1)-3)  .AND.  &
             (I .ge. 3-mod(J,2) .and. I .le. (ide-1)-2) ) THEN

       VBM3(I,J)=1.

        ENDIF

       enddo
       enddo

!       DTAD=1 in const.f of intrst code
      DTAD=1.0

!       IDTCF=DTCF, IDTCF=4
      DTCF=4.0 ! used?

      DY_NMM=ERAD*DPH
      CPGFV=-DT/(48.*DY_NMM)
      EN= DT/( 4.*DY_NMM)*DTAD
      ENT=DT/(16.*DY_NMM)*DTAD


        DO J=jts,nnyp
         KHL2(J)=(IDE-1)*(J-1)-(J-1)/2+2
         KVL2(J)=(IDE-1)*(J-1)-J/2+2
         KHH2(J)=(IDE-1)*J-J/2-1
         KVH2(J)=(IDE-1)*J-(J+1)/2-1
        ENDDO

        TPH=SB-DPH

!        DO J=jts,NNYP
        DO J=jts,min(jte,jde-1)
!       DO J=jds,jde-1
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         DXJ(J)=DXP
         WPDARJ(J)=-W_NMM * &
     ((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)/ &
                   (DT*32.*DXP*DY_NMM)

         CPGFUJ(J)=-DT/(48.*DXP)
         CURVJ(J)=.5*DT*TAN(TPH)/ERAD
         FCPJ(J)=DT/(CP*192.*DXP*DY_NMM)
         FDIVJ(J)=1./(12.*DXP*DY_NMM)
!         EMJ(J)= DT/( 4.*DXP)*DTAD
!         EMTJ(J)=DT/(16.*DXP)*DTAD
         FADJ(J)=-DT/(48.*DXP*DY_NMM)*DTAD
         ACDT=DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)
         CDDAMP=CODAMP*ACDT
         HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM)
         DDMPUJ(J)=CDDAMP/DXP
         DDMPVJ(J)=CDDAMP/DY_NMM
        ENDDO

!!! wrf_dm_on_monitor block was here, but was causing problems for unknown reasons


          DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J,2)*DLM
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             FP=TWOM*(CTPH0*STPH+STPH0*CTPH*COS(TLM))
             F(I,J)=0.5*DT*FP

           ENDDO
          ENDDO

! --------------DERIVED VERTICAL GRID CONSTANTS--------------------------

      EF4T=.5*DT/CP
      F4Q =   -DT*DTAD
      F4D =-.5*DT*DTAD

!       DO L=KDS,KDE
       DO L=KDS,KDE-1
        RDETA(L)=1./DETA(L)
        F4Q2(L)=-.25*DT*DTAD/DETA(L)
       ENDDO


! shouldnt need to be done globally, right?
        DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          DX_NMM(I,J)=DXJ(J)
          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
          CPGFU(I,J)=CPGFUJ(J)*VBM2(I,J)
          CURV(I,J)=CURVJ(J)*VBM2(I,J)
          FCP(I,J)=FCPJ(J)*HBM2(I,J)
          FDIV(I,J)=FDIVJ(J)*HBM2(I,J)
          FAD(I,J)=FADJ(J)
!       if (mod(I,5) .eq. 0 .and. mod(J,5) .eq. 0) then
!       write(0,*) 'I,J,FADJ,FAD(I,J): ', I,J,FADJ(J),FAD(I,J)
!       endif
          HDACV(I,J)=HDACJ(J)*VBM2(I,J)
          HDAC(I,J)=HDACJ(J)*1.25*HBM2(I,J)
        ENDDO
        ENDDO



!      DO J=3,(JDE-1)-2
        DO J=JTS, MIN(JDE-1,JTE)

        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN

               KHH=(IDE-1)-2+MOD(J,2) ! KHH is global...loop over I that have
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                   HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO

        ELSE

          KHH=2+MOD(J,2)
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                    HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO

          KHH=(IDE-1)-2+MOD(J,2)

!          DO I=(IDE-1)-2,KHH
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. (IDE-1)-2 .and. I .le. KHH) THEN
                   HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO
        ENDIF
      ENDDO


      DO J=JTS,min(JTE,JDE-1)
      DO I=ITS,min(ITE,IDE-1)
        DDMPU(I,J)=DDMPUJ(J)*VBM2(I,J)
        DDMPV(I,J)=DDMPVJ(J)*VBM2(I,J)
        HDACV(I,J)=HDACV(I,J)*VBM2(I,J)
      ENDDO
      ENDDO
! --------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------

!      DO J=3,JDE-1-2

        DO J=JTS,MIN(JDE-1,JTE)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             DDMPU(I,J)=DDMPU(I,J)*DDFC
             DDMPV(I,J)=DDMPV(I,J)*DDFC
             HDACV(I,J)=HDACV(I,J)* DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO

        write(0,*) ' grid%num_soil_layers = ',  grid%num_soil_layers

        write(0,*) 'STC(1)'
        do J=min(jde-1,jte),jts,-(jte-jts)/15
        write(0,635) (stc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12)
        enddo

        write(0,*) 'SMC(1)'
        do J=min(jde-1,jte),jts,-(jte-jts)/15
        write(0,635) (smc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12)
        enddo

!       write(0,*) 'SM'
!       do J=min(jde-1,jte),jts,-(jte-jts)/15
!       write(0,635) (smc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12)
!       enddo

          DO j = jts, MIN(jde-1,jte)
          DO i=  ITS, MIN(IDE-1,ITE)

        if (SM(I,J) .eq. 0 .and. SMC(I,1,J) .gt. 0.5 .and. SICE(I,J) .eq. 0) then
        write(0,*) 'wet on land point: ', I,J,SMC(I,1,J),SICE(I,J)
        endif

          enddo
        enddo


!!!! MOVE MONITOR BLOCK HERE

!!!   compute EMT, EM on global domain, and only on task 0.

        IF (wrf_dm_on_monitor()) THEN   !!!! NECESSARY TO LIMIT THIS TO TASK ZERO?

        ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))

!       write(0,*) 'FIGURING OUT EMJ, EMTJ ', JDS, JDE-1
        DO J=JDS,JDE-1
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         EMJ(J)= DT/( 4.*DXP)*DTAD
         EMTJ(J)=DT/(16.*DXP)*DTAD
!       write(0,*) 'J, EMTJ(J): ', J, EMTJ(J)
        ENDDO
        
          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 161      EMT(JA)=EMTJ(J)
          DO 162 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 162      EMT(JA)=EMTJ(J)
          DO 163 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=2+MOD(J,2)
 163      EMT(JA)=EMTJ(J)
          DO 164 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=(IDE-1)-2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 164      EMT(JA)=EMTJ(J)

! --------------SPREADING OF UPSTREAM VELOCITY-POINT ADVECTION FACTOR----

      JA=0
              DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 171      EM(JA)=EMJ(J)
              DO 172 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 172      EM(JA)=EMJ(J)
              DO 173 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=2+MOD(J+1,2)
 173      EM(JA)=EMJ(J)
              DO 174 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=(IDE-1)-2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 174      EM(JA)=EMJ(J)

   696  continue
        ENDIF ! wrf_dm_on_monitor


      call NMM_SH2O(IMS,IME,JMS,JME,ITS,NNXP,JTS,NNYP,4,ISLTYP, &
                             SM,SICE,STC,SMC,SH2O)

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)

        if (   abs(IDE-1-ITE) .eq. 1 ) THEN ! along eastern boundary
        write(0,*) 'zero phantom winds'
        do K=1,KDE-1
!         do J=JTS,JDE-1,2
        DO J=JDS,JDE-1,2
        if (J .ge. JTS .and. J .le. JTE) THEN
             u(IDE-1,K,J)=0.
             v(IDE-1,K,J)=0.
        endif
          enddo
        enddo
        endif

  969   continue

!       write(0,*) 'NMM_TSK leaving init_domain_nmm'
!       do J=min(jte,jde-1),jts,-(jte-jts)/15
!       write(0,635) (NMM_TSK(I,J),I=its,min(ide-1,ite),(ite-its)/12)
!       enddo

         DO j = jms, jme
            DO i = ims, ime

          fisx=max(fis(i,j),0.)
          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
     &                (Z0(I,J)*Z0MAX+FISx    *FCM+Z0LAND)

            ENDDO
          ENDDO

        write(0,*) 'Z0 over memory, leaving module_initialize_real'
        do J=JME,JMS,-(JME-JMS)/20
        write(0,635) (Z0(I,J),I=IMS,IME,(IME-IMS)/14)
        enddo



        write(0,*) 'leaving init_domain_nmm'
!
! Gopal's doing's : following lines  moved to namelist_input4 in Registry 
!
!       write(0,*) 'hardwire'
!        sigma=.true.
!       IDTAD=2  
!       NSOIL=4
!       NPHS=4
!       NCNVC=4
 write(0,*)'STUFF MOVED TO REGISTRY:',IDTAD,NSOIL,NRADL,NRADS,NPHS,NCNVC,sigma
!==================================================================================

#define COPY_OUT
#include <nmm_scalar_derefs.inc>
      RETURN

   END SUBROUTINE init_domain_nmm

!--------------------------------------------------------------------
      SUBROUTINE NMM_SH2O(IMS,IME,JMS,JME,ISTART,IM,JSTART,JM,&
                        NSOIL,ISLTPK, &
                        SM,SICE,STC,SMC,SH2O)

!!        INTEGER, PARAMETER:: NSOTYP=9
!        INTEGER, PARAMETER:: NSOTYP=16
        INTEGER, PARAMETER:: NSOTYP=19 !!!!!!!!MAYBE???

        REAL :: PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
        REAL :: STC(IMS:IME,NSOIL,JMS:JME), &
                SMC(IMS:IME,NSOIL,JMS:JME)
        REAL :: SH2O(IMS:IME,NSOIL,JMS:JME),SICE(IMS:IME,JMS:JME),&
                SM(IMS:IME,JMS:JME)
        REAL :: HLICE,GRAV,T0,BLIM
        INTEGER :: ISLTPK(IMS:IME,JMS:JME)

! Constants used in cold start SH2O initialization
      DATA HLICE/3.335E5/,GRAV/9.81/,T0/273.15/
      DATA BLIM/5.5/
!      DATA PSIS /0.04,0.62,0.47,0.14,0.10,0.26,0.14,0.36,0.04/
!      DATA BETA /4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
!      DATA SMCMAX /0.421,0.464,0.468,0.434,0.406, &
!                  0.465,0.404,0.439,0.421/

        
!!!      NOT SURE...PSIS=SATPSI, BETA=BB??

        DATA PSIS /0.069, 0.036, 0.141, 0.759, 0.759, 0.355,   &
                   0.135, 0.617, 0.263, 0.098, 0.324, 0.468,   &
                   0.355, 0.000, 0.069, 0.036, 0.468, 0.069, 0.069  /

        DATA BETA/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,    &
                  6.66,  8.72,  8.17, 10.73, 10.39, 11.55,    &
                  5.25,  0.00,  2.79,  4.26, 11.55, 2.79, 2.79 /

        DATA SMCMAX/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                    0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                    0.439, 1.000, 0.200, 0.421, 0.468, 0.200, 0.339/

        write(0,*) 'define SH2O over IM,JM: ', IM,JM
        DO K=1,NSOIL
         DO J=JSTART,JM
          DO I=ISTART,IM
        if(i==169.and.j==111.and.k==1)then
          write(0,*)' enter NMM_SH2O k=',k,' smc=',smc(i,k,j),' sh2o=',sh2o(i,k,j)
        endif
!tst
        IF (SMC(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
! if (K .eq. 1) then
!  write(0,*) 'I,J,reducing SMC from ' ,I,J,SMC(I,K,J), 'to ', SMCMAX(ISLTPK(I,J))
!  endif
        SMC(I,K,J)=SMCMAX(ISLTPK(I,J))
        ENDIF
!tst

        if(i==056.and.j==265.and.k==1)then
          write(0,*)' NMM_SH2O point 2 k=',k,' smc=',smc(i,k,j),' sh2o=',sh2o(i,k,j)
        endif
        IF ( (SM(I,J) .lt. 0.5) .and. (SICE(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(6,*) 'FORCING ISLTPK at : ', I,J
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(6,*) 'FORCING ISLTPK at : ', I,J
                ISLTPK(I,J)=1
        ENDIF


! cold start:  determine liquid soil water content (SH2O)
! SH2O <= SMC for T < 273.149K (-0.001C)

           IF (STC(I,K,J) .LT. 273.149) THEN

! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).

              BX = BETA(ISLTPK(I,J))
              IF ( BETA(ISLTPK(I,J)) .GT. BLIM ) BX = BLIM

        if ( GRAV*(-PSIS(ISLTPK(I,J))) .eq. 0 ) then
        write(0,*) 'TROUBLE'
        write(0,*) 'I,J: ', i,J
        write(0,*) 'grav, isltpk, psis(isltpk): ', grav,isltpk(I,J),&
                 psis(isltpk(I,J))
        endif

        if (BX .eq. 0 .or. STC(I,K,J) .eq. 0) then
                write(0,*) 'I,J,BX, STC: ', I,J,BX,STC(I,K,J)
        endif
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPK(I,J)))))* &
                  ((STC(I,K,J)-T0)/STC(I,K,J)))** &
                  (-1/BX))*SMCMAX(ISLTPK(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              SH2O(I,K,J) = MIN ( FK, SMC(I,K,J) )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O (from the Eta "NOAH" land-surface model) with the
! initial guess for SH2O from above explicit first guess.

              SH2O(I,K,J)=FRH2O_init(STC(I,K,J),SMC(I,K,J),SH2O(I,K,J), &
                         SMCMAX(ISLTPK(I,J)),BETA(ISLTPK(I,J)), &
                         PSIS(ISLTPK(I,J)))

            ELSE ! above freezing
              SH2O(I,K,J)=SMC(I,K,J)
            ENDIF


        ELSE   ! water point
              SH2O(I,K,J)=SMC(I,K,J)

        ENDIF ! test on land/ice/sea
        if (SH2O(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
        write(0,*) 'SH2O > THAN SMCMAX ', I,J,SH2O(I,K,J),SMCMAX(ISLTPK(I,J)),SMC(I,K,J)
        endif

        if(i==169.and.j==111.and.k==1)then
          write(0,*)' exit NMM_SH2O k=',k,' smc=',smc(i,k,j),' sh2o=',sh2o(i,k,j)
        endif

         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O

!-------------------------------------------------------------------

        subroutine zero2d(adum2d,nnxp,nnyp)

        integer I,J,NNXP,NNYP
        real adum2d(nnxp,nnyp)

        do J=1,nnyp
        do I=1,nnxp
        adum2d(I,J)=0.
        enddo
        enddo

        end subroutine zero2d


!-------------------------------------------------------------------
      FUNCTION FRH2O_init(TKELV,SMC,SH2O,SMCMAX,B,PSIS)

      IMPLICIT NONE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  PURPOSE:  CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT
!  IF TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION
!  TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF
!  KOREN ET AL. (1999, JGR, VOL 104(D16), 19569-19585).
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! New version (JUNE 2001): much faster and more accurate newton iteration
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  Also, explicit
! 1-step solution option for special case of parameter Ck=0, which reduces
! the original implicit equation to a simpler explicit form, known as the
! ""Flerchinger Eqn". Improved handling of solution in the limit of
! freezing point temperature T0.
!
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INPUT:
!
!   TKELV.........Temperature (Kelvin)
!   SMC...........Total soil moisture content (volumetric)
!   SH2O..........Liquid soil moisture content (volumetric)
!   SMCMAX........Saturation soil moisture content (from REDPRM)
!   B.............Soil type "B" parameter (from REDPRM)
!   PSIS..........Saturated soil matric potential (from REDPRM)
!
! OUTPUT:
!   FRH2O.........supercooled liquid water content.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL B
      REAL BLIM
      REAL BX
      REAL CK
      REAL DENOM
      REAL DF
      REAL DH2O
      REAL DICE
      REAL DSWL
      REAL ERROR
      REAL FK
      REAL FRH2O_init
      REAL GS
      REAL HLICE
      REAL PSIS
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SWL
      REAL SWLK
      REAL TKELV
      REAL T0

      INTEGER NLOG
      INTEGER KCOUNT
      PARAMETER (CK=8.0)
!      PARAMETER (CK=0.0)
      PARAMETER (BLIM=5.5)
!      PARAMETER (BLIM=7.0)
      PARAMETER (ERROR=0.005)

      PARAMETER (HLICE=3.335E5)
      PARAMETER (GS = 9.81)
      PARAMETER (DICE=920.0)
      PARAMETER (DH2O=1000.0)
      PARAMETER (T0=273.15)

!  ###   LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)  ####
!  ###   SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT  ####
!  ###   IS NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES    ####
!  ################################################################
!
      BX = B
      IF ( B .GT. BLIM ) BX = BLIM
! ------------------------------------------------------------------

! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
      NLOG=0
      KCOUNT=0

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O_init=SMC

      ELSE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (CK .NE. 0.0) THEN

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCCCCCC OPTION 1: ITERATED SOLUTION FOR NONZERO CK CCCCCCCCCCC
! CCCCCCCCCCCC IN KOREN ET AL, JGR, 1999, EQN 17 CCCCCCCCCCCCCCCCC

! INITIAL GUESS FOR SWL (frozen content)
        SWL = SMC-SH2O
! KEEP WITHIN BOUNDS.
         IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
         IF(SWL .LT. 0.) SWL=0.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  START OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * &
             ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
         SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
         IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
         IF(SWLK .LT. 0.) SWLK = 0.
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
         DSWL=ABS(SWLK-SWL)
         SWL=SWLK
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CC IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! CC WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         IF ( DSWL .LE. ERROR )  THEN
           KCOUNT=KCOUNT+1
         END IF
        END DO
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  END OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
        FRH2O_init = SMC - SWL

! CCCCCCCCCCCCCCCCCCCCCCCC END OPTION 1 CCCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

       IF (KCOUNT .EQ. 0) THEN
!         Print*,'Flerchinger used in NEW version. Iterations=',NLOG

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCC OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0 CCCCCCCC
! CCCCCCCCCCCCC IN KOREN ET AL., JGR, 1999, EQN 17  CCCCCCCCCCCCCCC

        FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
        IF (FK .LT. 0.02) FK = 0.02
        FRH2O_init = MIN ( FK, SMC )

! CCCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

      ENDIF

        RETURN

      END FUNCTION FRH2O_init


!--------------------------------------------------------------------

   SUBROUTINE const_module_initialize ( p00 , t00 , a ) 
      IMPLICIT NONE
      REAL , PARAMETER :: sea_level_pressure_base    = 100000.
      REAL , PARAMETER :: sea_level_temperature_base =    290.
      REAL , PARAMETER :: temp_diff_1000_to_300_mb   =     50.
      REAL , INTENT(OUT) :: p00 , t00 , a
      p00 = sea_level_pressure_base
      t00 = sea_level_temperature_base
      a   = temp_diff_1000_to_300_mb
   END SUBROUTINE const_module_initialize

!---------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize
