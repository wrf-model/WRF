#if ( ! NMM_CORE == 1 )

MODULE module_soil_pre

   USE module_date_time
   USE module_state_description

   CHARACTER (LEN=3) :: num_cat_count
   INTEGER , PARAMETER , DIMENSION(0:300) :: ints = &
   (/    0,    1,    2,    3,    4,    5,    6,    7,    8,    9, &
        10,   11,   12,   13,   14,   15,   16,   17,   18,   19, &
        20,   21,   22,   23,   24,   25,   26,   27,   28,   29, &
        30,   31,   32,   33,   34,   35,   36,   37,   38,   39, &
        40,   41,   42,   43,   44,   45,   46,   47,   48,   49, &
        50,   51,   52,   53,   54,   55,   56,   57,   58,   59, &
        60,   61,   62,   63,   64,   65,   66,   67,   68,   69, &
        70,   71,   72,   73,   74,   75,   76,   77,   78,   79, &
        80,   81,   82,   83,   84,   85,   86,   87,   88,   89, &
        90,   91,   92,   93,   94,   95,   96,   97,   98,   99, &
       100,  101,  102,  103,  104,  105,  106,  107,  108,  109, &
       110,  111,  112,  113,  114,  115,  116,  117,  118,  119, &
       120,  121,  122,  123,  124,  125,  126,  127,  128,  129, &
       130,  131,  132,  133,  134,  135,  136,  137,  138,  139, &
       140,  141,  142,  143,  144,  145,  146,  147,  148,  149, &
       150,  151,  152,  153,  154,  155,  156,  157,  158,  159, &
       160,  161,  162,  163,  164,  165,  166,  167,  168,  169, &
       170,  171,  172,  173,  174,  175,  176,  177,  178,  179, &
       180,  181,  182,  183,  184,  185,  186,  187,  188,  189, &
       190,  191,  192,  193,  194,  195,  196,  197,  198,  199, &
       200,  201,  202,  203,  204,  205,  206,  207,  208,  209, &
       210,  211,  212,  213,  214,  215,  216,  217,  218,  219, &
       220,  221,  222,  223,  224,  225,  226,  227,  228,  229, &
       230,  231,  232,  233,  234,  235,  236,  237,  238,  239, &
       240,  241,  242,  243,  244,  245,  246,  247,  248,  249, &
       250,  251,  252,  253,  254,  255,  256,  257,  258,  259, &
       260,  261,  262,  263,  264,  265,  266,  267,  268,  269, &
       270,  271,  272,  273,  274,  275,  276,  277,  278,  279, &
       280,  281,  282,  283,  284,  285,  286,  287,  288,  289, &
       290,  291,  292,  293,  294,  295,  296,  297,  298,  299, 300 /)

   !  Excluded middle processing

   LOGICAL , SAVE :: hold_ups
   INTEGER , SAVE :: em_width

   LOGICAL , EXTERNAL :: skip_middle_points_t

CONTAINS

   SUBROUTINE adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message

      INTEGER, INTENT(IN) :: fractional_seaice
      REAL :: XICE_THRESHOLD

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         xice_threshold = 0.02
      ENDIF

      num_seaice_changes = 0
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME ,CLMSCHEME,SSIBSCHEME)   !mchen add for ssib
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .GT. 0.5 ) THEN
                     if (xice(i,j).gt.0) num_seaice_changes = num_seaice_changes + 1
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of land location changes (seaice set to zero) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_pre

   SUBROUTINE adjust_for_seaice_post ( xice , landmask , tsk_old , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , vegfra , &
                                      tslb , smois , sh2o , &
                                      seaice_threshold , &
                                      sst , flag_sst , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      num_soil_layers , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme
      INTEGER , INTENT(IN) :: num_soil_layers

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      REAL , DIMENSION(ims:ime,1:num_soil_layers,jms:jme) , INTENT(INOUT):: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme)                   , INTENT(IN):: sst
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn , &
                                                           tsk_old , vegfra
      INTEGER , INTENT(IN) :: flag_sst
      REAL , INTENT(IN) :: seaice_threshold
      REAL :: total_depth , mid_point_depth

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message


      INTEGER, INTENT(IN) :: fractional_seaice
      real :: xice_threshold

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         xice_threshold = 0.02
      ENDIF

      num_seaice_changes = 0
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )

         CASE ( LSMSCHEME , NOAHMPSCHEME , CLMSCHEME, SSIBSCHEME )   !mchen add for ssib
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .GT. 170 ) .AND. ( tsk_old(i,j) .LT. 400 ) ) )THEN
                     tsk(i,j) = tsk_old(i,j)
                  END IF
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .LT. 170 ) .OR. ( tsk_old(i,j) .GT. 400 ) ) )THEN
                     print *,'TSK woes in seaice post, i,j=',i,j,'  tsk = ',tsk(i,j), tsk_old(i,j)
                     CALL wrf_error_fatal ( 'TSK is unrealistic, problems for seaice post')
                  ELSE IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     vegfra(i,j)=0.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     tsk_old(i,j) = tsk(i,j)

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                     total_depth = 3. ! ice is 3 m deep, num_soil_layers equispaced layers
                     DO loop = 1,num_soil_layers
                        mid_point_depth=(total_depth/num_soil_layers)/2. + &
                                        (loop-1)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE IF ( xice(i,j) .LT. xice_threshold ) THEN
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

        CASE ( RUCLSMSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .GT. 170 ) .AND. ( tsk_old(i,j) .LT. 400 ) ) )THEN
                     tsk(i,j) = tsk_old(i,j)
                  END IF
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .LT. 170 ) .OR. ( tsk_old(i,j) .GT. 400 ) ) )THEN
                     print *,'TSK woes in seaice post, i,j=',i,j,'  tsk = ',tsk(i,j), tsk_old(i,j)
                     CALL wrf_error_fatal ( 'TSK is unrealistic, problems for seaice post')
                  ELSE IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                       IF ( FRACTIONAL_SEAICE == 0 ) THEN
                          xice(i,j) = 1.0
                       ELSE
                          xice(i,j)=max(0.25,xice(i,j))
                       ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     vegfra(i,j)=0.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

!tgs - compute blended sea ice/water skin temperature
                   if(flag_sst.eq.1) then
                     tsk(i,j) = xice(i,j)*(min(271.4,tsk(i,j)))  &
                                +(1-xice(i,j))*sst(i,j)
                   else
                     tsk(i,j) = xice(i,j)*(min(271.4,tsk(i,j)))  &
                                +(1-xice(i,j))*tsk(i,j)
                   endif
                     tsk_old(i,j) = tsk(i,j)

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                 total_depth = 3. ! ice is 3 m deep, num_soil_layers equispaced layers
                       tslb(i,1,j) = tsk(i,j)
                       tslb(i,num_soil_layers,j) = tmn(i,j)
                     DO loop = 2,num_soil_layers-1
                        mid_point_depth=(total_depth/num_soil_layers)/4. + &
                                        (loop-2)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE IF ( xice(i,j) .LT. xice_threshold ) THEN
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF


      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_post

   SUBROUTINE process_percent_cat_new ( landmask ,  &
                                landuse_frac , soil_top_cat , soil_bot_cat , &
                                isltyp , ivgtyp , &
                                num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                iswater )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask

      INTEGER :: i , j , l , ll, dominant_index
      REAL :: dominant_value

#if ( WRF_CHEM == 1 )
!      REAL :: lwthresh = .99
      REAL :: lwthresh = .50
#else
      REAL :: lwthresh = .50
#endif

      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce
      CHARACTER (LEN=132) :: message
      CHARACTER(LEN=256) :: mminlu
      LOGICAL :: aggregate_lu
integer :: change_water , change_land
change_water = 0
change_land = 0

      !  Sanity check on the 50/50 points

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = landuse_frac(i,iswater,j)
            IF ( dominant_value .EQ. lwthresh ) THEN
               DO l = 1 , num_veg_cat
                  IF ( l .EQ. iswater ) CYCLE
                  IF ( ( landuse_frac(i,l,j) .EQ. lwthresh ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                     PRINT *,i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                     landuse_frac(i,l,j) = lwthresh - .01
                     landuse_frac(i,iswater,j) = lwthresh + 0.01
                  ELSE IF ( ( landuse_frac(i,l,j) .EQ. lwthresh ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                     PRINT *,i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                     landuse_frac(i,l,j) = lwthresh + .01
                     landuse_frac(i,iswater,j) = lwthresh - 0.01
                  END IF
               END DO
            END IF
         END DO
      END DO

      !  Compute the aggregate of the vegetation/land use categories.  Lump all of the grasses together,
      !  all of the shrubs, all of the trees, etc.  Choose the correct set of available land use
      !  categories.  Also, make sure that the user wants to actually avail themselves of aforementioned
      !  opportunity, as mayhaps they don't.
 
      CALL nl_get_mminlu       ( 1 , mminlu       )
      CALL nl_get_aggregate_lu ( 1 , aggregate_lu )
      IF ( aggregate_lu ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               CALL aggregate_categories_part1 ( landuse_frac , iswater , num_veg_cat , mminlu(1:4) )
            END DO
         END DO
      END IF

      !  Compute the dominant VEGETATION INDEX.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = landuse_frac(i,1,j)
            dominant_index = 1
            DO l = 2 , num_veg_cat
               IF        ( l .EQ. iswater ) THEN
                  ! wait a bit
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac(i,l,j) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               END IF
            END DO
            IF ( landuse_frac(i,iswater,j) .GT. lwthresh ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh) .AND. &
                      ( landmask(i,j) .LT. 0.5) .AND. &
                      ( dominant_value .EQ. lwthresh) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh) .AND. &
                      ( landmask(i,j) .GT. 0.5) .AND. &
                      ( dominant_value .EQ. lwthresh) ) THEN
               !no op
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh ) .AND. &
                      ( dominant_value .LT. lwthresh ) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            END IF
            IF      ( dominant_index .EQ. iswater ) THEN
if(landmask(i,j).gt.lwthresh) then
!print *,'changing to water at point ',i,j
!WRITE ( num_cat_count , FMT = '(I3)' ) num_veg_cat
!WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) ints(1:num_veg_cat)
!CALL wrf_debug(1,message)
!WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) nint(landuse_frac(i,:,j)*100)
!CALL wrf_debug(1,message)
change_water=change_water+1
endif
               landmask(i,j) = 0
            ELSE IF ( dominant_index .NE. iswater ) THEN
if(landmask(i,j).lt.lwthresh) then
!print *,'changing to land at point ',i,j
!WRITE ( num_cat_count , FMT = '(I3)' ) num_veg_cat
!WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) ints(1:num_veg_cat)
!CALL wrf_debug(1,message)
!WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) nint(landuse_frac(i,:,j)*100)
!CALL wrf_debug(1,message)
change_land=change_land+1
endif
               landmask(i,j) = 1
            END IF
            ivgtyp(i,j) = dominant_index
         END DO
      END DO

      !  Compute the dominant SOIL TEXTURE INDEX, TOP.

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = soil_top_cat(i,1,j)
            dominant_index = 1
            IF ( landmask(i,j) .GT. lwthresh ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,l,j) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,l,j)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
                  WRITE ( message , FMT = '(A,I4,I4)' ) &
                  'based on landuse, changing soil to land at point ',i,j
                  CALL wrf_debug(1,message)
                  WRITE ( num_cat_count , FMT = '(I3)' ) num_soil_top_cat
                  WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) (ints(l),l=1,num_soil_top_cat)
                  CALL wrf_debug(1,message)
                  WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) &
                    ((nint(soil_top_cat(i,ints(l),j)*100)), l=1,num_soil_top_cat)
                  CALL wrf_debug(1,message)
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp(i,j) = dominant_index
         END DO
      END DO

if(iforce.ne.0)then
WRITE(message,FMT='(A,I4,A,I6)' ) &
'forcing artificial silty clay loam at ',iforce,' points, out of ',&
(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
CALL wrf_debug(0,message)
endif
print *,'LAND  CHANGE = ',change_land
print *,'WATER CHANGE = ',change_water

   END SUBROUTINE process_percent_cat_new

   SUBROUTINE process_soil_real ( tsk , tmn , tavgsfc, &
                                landmask , sst , ht, toposoil, &
                                st_input , sm_input , sw_input , &
                                st_levels_input , sm_levels_input , sw_levels_input , &
                                zs , dzs , tslb , smois , sh2o , &
                                flag_sst , flag_tavgsfc, flag_soilhgt, &
                                flag_soil_layers, flag_soil_levels, &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                sf_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              sf_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc

      INTEGER , INTENT(IN) :: flag_sst, flag_tavgsfc
      INTEGER , INTENT(IN) :: flag_soil_layers, flag_soil_levels, flag_soilhgt

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tavgsfc, ht, toposoil
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk, tmn

      INTEGER :: i , j , k, l , dominant_index , num_soil_cat , num_veg_cat, closest_layer
      REAL :: dominant_value, closest_depth, diff_cm
      REAL , ALLOCATABLE , DIMENSION(:) :: depth     ! Soil layer thicknesses (cm)
      REAL, PARAMETER :: get_temp_closest_to = 30.   ! use soil temperature closest to this depth (cm)
      REAL, PARAMETER :: something_big = 1.e6        ! Initialize closest depth as something big (cm)
      INTEGER :: something_far = 1000                ! Soil array index far away
      CHARACTER (LEN=132) :: message

      !  Case statement for tmn initialization
      !  Need to have a reasonable default value for annual mean deeeeep soil temperature
      !  For sf_surface_physics = 1, we want to use close to a 30 cm value
      !  for the bottom level of the soil temps.
      ! NOTE:  We are assuming that soil_layers are the same for each grid point

      fix_bottom_level_for_temp : SELECT CASE ( sf_surface_physics )
         CASE (SLABSCHEME)
            IF ( flag_tavgsfc  .EQ. 1 ) THEN
               CALL wrf_debug ( 0 , 'Using average surface temperature for tmn')
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tmn(i,j) = tavgsfc(i,j)
                  END DO
               END DO
            ELSE
            ! Look for soil temp close to 30 cm
               closest_layer = something_far
               closest_depth = something_big
               DO k = 1, num_st_levels_input
                  diff_cm = abs( st_levels_input(k) - get_temp_closest_to )
                  IF ( diff_cm < closest_depth ) THEN
                     closest_depth = diff_cm
                     closest_layer = k
                  END IF
               END DO
               IF ( closest_layer == something_far ) THEN
                  CALL wrf_debug ( 0 , 'No soil temperature data for grid%tmn near 30 cm')
                  CALL wrf_debug ( 0 , 'Using 1 degree static annual mean temps' )
               ELSE
                  write(message, FMT='(A,F7.2,A,I3)')&
                     'Soil temperature closest to ',get_temp_closest_to, &
                     ' at level ',st_levels_input(closest_layer)
                  CALL wrf_debug ( 0 , message )
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tmn(i,j) = st_input(i,closest_layer+1,j)
                     END DO
                  END DO
               END IF
            END IF

         CASE (LSMSCHEME)

         CASE (NOAHMPSCHEME)

         CASE (CLMSCHEME)

         CASE (RUCLSMSCHEME)

         CASE (PXLSMSCHEME)
! When the input data from met_em is in layers, there is an additional level added to the beginning
! of the array to define the surface, which is why we add the extra value (closest_layer+1)
            IF ( flag_tavgsfc  .EQ. 1 ) THEN
               CALL wrf_debug ( 0 , 'Using average surface temperature for tmn')
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tmn(i,j) = tavgsfc(i,j)
                  END DO
               END DO
            ELSE
            ! Look for soil temp close to 30 cm
               closest_layer = num_st_levels_input+1
               closest_depth = something_big
               DO k = 1, num_st_levels_input
                  diff_cm = abs( st_levels_input(k) - get_temp_closest_to )
                  IF ( diff_cm < closest_depth ) THEN
                     closest_depth = diff_cm
                     closest_layer = k
                  END IF
               END DO
               IF ( closest_layer == num_st_levels_input + 1 ) THEN
                  CALL wrf_debug ( 0 , 'No soil temperature data for grid%tmn near 30 cm')
                  CALL wrf_debug ( 0 , 'Using 1 degree static annual mean temps' )
               ELSE
                  write(message, FMT='(A,F7.2,A,I3)')&
                     'Soil temperature closest to ',get_temp_closest_to, &
                     ' at level ',st_levels_input(closest_layer)
                  CALL wrf_debug ( 0 , message )
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tmn(i,j) = st_input(i,closest_layer+1,j)
                     END DO
                  END DO
               END IF
            END IF

#if 0
            ! Loop over layers and do a weighted mean
            IF ( ALLOCATED ( depth ) ) DEALLOCATE ( depth )
            ALLOCATE ( depth(num_st_levels_input) )
            IF ( flag_soil_layers == 1 ) THEN
               DO k = num_st_levels_input, 2, -1
                  depth(k) = st_levels_input(k) - st_levels_input(k-1)
               END DO
               depth(1) = st_levels_input(1)
            ELSE IF ( flag_soil_levels == 1 ) THEN
               DO k = 2, num_st_levels_input
                  depth(k) = st_levels_input(k) - st_levels_input(k-1)
               END DO
               depth(1) = 0.
            END IF

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  tmn(i,j) = 0.
                  DO k = 1, num_st_levels_input
                     tmn(i,j) = tmn(i,j) + depth(k) * st_input(i,k,j)
                  END DO
               END DO
            END DO
            DEALLOCATE ( depth )

#endif

      END SELECT fix_bottom_level_for_temp

      !  Adjust the various soil temperature values depending on the difference in
      !  elevation between the current model's elevation and the incoming data's
      !  orography.

      adjust_soil : SELECT CASE ( sf_surface_physics )

         CASE ( SLABSCHEME,LSMSCHEME,NOAHMPSCHEME,RUCLSMSCHEME,PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )
            CALL adjust_soil_temp_new ( tmn , sf_surface_physics , tsk , ht ,            &
                                        toposoil , landmask , st_input, st_levels_input, &
                                        flag_soilhgt , flag_tavgsfc ,                    &
                                        flag_soil_layers , flag_soil_levels,             &
                                        num_st_levels_input, num_st_levels_alloc,        &
                                        ids , ide , jds , jde , kds , kde ,              &
                                        ims , ime , jms , jme , kms , kme ,              &
                                        its , ite , jts , jte , kts , kte )

      END SELECT adjust_soil

      !  Initialize the soil depth, and the soil temperature and moisture.
   
      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
!CLM -- Jiming Jin 10/17/2012
      ELSE IF ( ( sf_surface_physics .EQ. CLMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_4 ( zs , dzs , num_soil_layers )
         CALL init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. PXLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_7 ( zs , dzs , num_soil_layers )
         CALL init_soil_7_real ( tsk , tmn , smois , sh2o, tslb , &
                                 st_input , sm_input , sw_input, landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input, &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. 8 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_8 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real

   SUBROUTINE process_soil_ideal ( xland,xice,vegfra,snow,canwat,  &
                                   ivgtyp,isltyp,tslb,smois, &
                                   tsk,tmn,zs,dzs,           &
                                   num_soil_layers,          &
                                   sf_surface_physics ,      &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) :: num_soil_layers , sf_surface_physics

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(INOUT) :: smois, tslb

      REAL, DIMENSION(num_soil_layers), INTENT(OUT) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT) :: tsk, tmn
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: xland, snow, canwat, xice, vegfra
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      !  Local variables.

      INTEGER :: itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_ideal(tsk,tmn,tslb,xland,                      &
                                ivgtyp,zs,dzs,num_soil_layers,           &
                                ids,ide, jds,jde, kds,kde,               &
                                ims,ime, jms,jme, kms,kme,               &
                                its,ite, jts,jte, kts,kte                )
      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )

      END IF

   END SUBROUTINE process_soil_ideal

   SUBROUTINE adjust_soil_temp_new ( tmn , sf_surface_physics , tsk , ter ,            &
                                     toposoil , landmask , st_input , st_levels_input, &
                                     flag_toposoil , flag_tavgsfc ,                    &
                                     flag_soil_layers , flag_soil_levels,              &
                                     num_st_levels_input, num_st_levels_alloc,         &
                                     ids , ide , jds , jde , kds , kde ,               &
                                     ims , ime , jms , jme , kms , kme ,               &
                                     its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 
      INTEGER , INTENT(IN) :: num_st_levels_input, num_st_levels_alloc

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: ter , toposoil , landmask
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(IN) :: st_levels_input

      INTEGER , INTENT(IN) :: sf_surface_physics , flag_toposoil , flag_tavgsfc
      INTEGER , INTENT(IN) :: flag_soil_layers , flag_soil_levels
 
      INTEGER :: i , j, k , st_near_sfc

      REAL :: soil_elev_min_val ,  soil_elev_max_val , soil_elev_min_dif , soil_elev_max_dif

      !  Adjust the annual mean temperature as if it is based on from a sea-level elevation
      !  if the value used is from the actual annula mean data set.  If the input field to
      !  be used for tmn is one of the first-guess input temp fields, need to do an adjustment
      !  only on the diff in topo from the model terrain and the first-guess terrain.

       SELECT CASE ( sf_surface_physics )

         CASE ( LSMSCHEME , NOAHMPSCHEME,CLMSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF (landmask(i,j) .GT. 0.5 ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                  END IF
               END DO
            END DO

         CASE (RUCLSMSCHEME)
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF (landmask(i,j) .GT. 0.5 ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                  END IF
               END DO
            END DO

      END SELECT


      !  Do we have a soil field with which to modify soil temperatures?

      IF ( flag_toposoil .EQ. 1 ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)

               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               !  Is the toposoil field OK, or is it a subversive soil elevation field.  We can tell
               !  usually by looking at values.  Anything less than -1000 m (lower than the Dead Sea) is
               !  bad.  Anything larger than 10 km (taller than Everest) is toast.  Also, anything where
               !  the difference between the soil elevation and the terrain is greater than 3 km means
               !  that the soil data is either all zeros or that the data are inconsistent.  Any of these
               !  three conditions is grievous enough to induce a WRF fatality.  However, if they are at
               !  a water point, then we can safely ignore them.

               soil_elev_min_val = toposoil(i,j)
               soil_elev_max_val = toposoil(i,j)
               soil_elev_min_dif = ter(i,j) - toposoil(i,j)
               soil_elev_max_dif = ter(i,j) - toposoil(i,j)

               IF      ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
!print *,'no soil temperature elevation adjustment, soil height too small = ',toposoil(i,j)
cycle
!                 CALL wrf_error_fatal ( 'TOPOSOIL values have large negative values < -1000 m, unrealistic.' )
               ENDIF

               IF      ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, soil height too high = ',toposoil(i,j)
cycle
                  CALL wrf_error_fatal ( 'TOPOSOIL values have large positive values > 10,000 m , unrealistic.' )
               ENDIF

               IF      ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, diff of soil height and terrain = ',ter(i,j) - toposoil(i,j)
cycle
                  CALL wrf_error_fatal ( 'TOPOSOIL difference with terrain elevation differs by more than 3000 m, unrealistic' )
               ENDIF

               !  For each of the fields that we would like to modify, check to see if it came in from the SI.
               !  If so, then use a -6.5 K/km lapse rate (based on the elevation diffs).  We only adjust when we
               !  are not at a water point.

               IF (landmask(i,j) .GT. 0.5 ) THEN
                  IF ( sf_surface_physics .EQ. SLABSCHEME ) THEN
                     st_near_sfc = 0                             ! Check if there is a soil layer above 40 cm
                     DO k = 1, num_st_levels_input
                        IF ( st_levels_input(k) .LE. 40 ) THEN
                           st_near_sfc = 1
                        END IF
                     END DO
                     IF ( ( flag_tavgsfc  == 1 ) .OR. ( st_near_sfc == 1 ) ) THEN
                        tmn(i,j) = tmn(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     ELSE
                        tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                     END IF
                  END IF

                  tsk(i,j) = tsk(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
      
                  IF ( flag_soil_layers == 1 ) THEN
                     DO k = 2, num_st_levels_input+1
                        st_input(i,k,j) = st_input(i,k,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     END DO
                  ELSE
                     DO k = 1, num_st_levels_input
                        st_input(i,k,j) = st_input(i,k,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     END DO
                  END IF

               END IF
            END DO
         END DO

      END IF

   END SUBROUTINE adjust_soil_temp_new


   SUBROUTINE init_soil_depth_1 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      !  Define layers (top layer = 0.01 m).  Double the thicknesses at each step (dzs values).
      !  The distance from the ground level to the midpoint of the layer is given by zs.

      !    -------   Ground Level   ----------      ||      ||   ||  ||
      !                                             ||      ||   ||  || zs(1) = 0.005 m
      !    --  --  --  --  --  --  --  --  --       ||      ||   ||  \/
      !                                             ||      ||   ||
      !    -----------------------------------      ||  ||  ||   \/   dzs(1) = 0.01 m
      !                                             ||  ||  ||
      !                                             ||  ||  || zs(2) = 0.02
      !    --  --  --  --  --  --  --  --  --       ||  ||  \/
      !                                             ||  ||
      !                                             ||  ||
      !    -----------------------------------  ||  ||  \/   dzs(2) = 0.02 m
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  || zs(3) = 0.05
      !    --  --  --  --  --  --  --  --  --   ||  \/
      !                                         ||
      !                                         ||
      !                                         ||
      !                                         ||
      !    -----------------------------------  \/   dzs(3) = 0.04 m

      IF ( num_soil_layers .NE. 5 ) THEN
         PRINT '(A)','Usually, the 5-layer diffusion uses 5 layers.  Change this in the namelist.'
         CALL wrf_error_fatal ( '5-layer_diffusion_uses_5_layers' )
      END IF

      dzs(1)=.01
      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         dzs(l)=2*dzs(l-1)
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_1

   SUBROUTINE init_soil_depth_2 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      dzs = (/ 0.1 , 0.3 , 0.6 , 1.0 /)

      IF ( num_soil_layers .NE. 4 ) THEN
         PRINT '(A)','Usually, the LSM uses 4 layers.  Change this in the namelist.'
         CALL wrf_error_fatal ( 'LSM_uses_4_layers' )
      END IF

      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_2

   SUBROUTINE init_soil_depth_3 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message

! in RUC LSM ZS - soil levels, and DZS - soil layer thicknesses, not used
! ZS is specified in the namelist: num_soil_layers = 6 or 9.
! Other options with number of levels are possible, but
! WRF users should change consistently the namelist entry with the
!    ZS array in this subroutine.

     IF ( num_soil_layers .EQ. 6) THEN
      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
     ELSEIF ( num_soil_layers .EQ. 9) THEN
      zs  = (/ 0.00 , 0.01 , 0.04 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)
!test3 in ppt      zs  = (/ 0.00 , 0.005 , 0.02 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)
!      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 0.60, 1.00, 1.60 , 2.20, 3.00 /)
     ENDIF

      IF ( num_soil_layers .EQ. 4 .OR. num_soil_layers .EQ. 5 ) THEN
         write (message, FMT='(A)') 'The RUC LSM uses 6, 9 or more levels.  Change this in the namelist.'
         CALL wrf_error_fatal ( message )
      END IF

   END SUBROUTINE init_soil_depth_3

   SUBROUTINE init_soil_depth_4 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL, PARAMETER :: scalez = 0.025

      INTEGER                   ::      l

      !  Define layers (top layer = 0.01 m).  Double the thicknesses at each
      !  step (dzs values).
      !  The distance from the ground level to the midpoint of the layer is
      !  given by zs.

      !    -------   Ground Level   ----------      ||      ||   ||  ||
      !                                             ||      ||   ||  || zs(1) =
      !                                             0.005 m
      !    --  --  --  --  --  --  --  --  --       ||      ||   ||  \/
      !                                             ||      ||   ||
      !    -----------------------------------      ||  ||  ||   \/   dzs(1) =
      !    0.01 m
      !                                             ||  ||  ||
      !                                             ||  ||  || zs(2) = 0.02
      !    --  --  --  --  --  --  --  --  --       ||  ||  \/
      !                                             ||  ||
      !                                             ||  ||
      !    -----------------------------------  ||  ||  \/   dzs(2) = 0.02 m
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  || zs(3) = 0.05
      !    --  --  --  --  --  --  --  --  --   ||  \/
      !                                         ||
      !                                         ||
      !                                         ||
      !                                         ||
      !    -----------------------------------  \/   dzs(3) = 0.04 m

      do l = 1, num_soil_layers
         zs(l) = scalez*(exp(0.5*(l-0.5))-1.)    !node depths
      enddo
       dzs(1) = 0.5*(zs(1)+zs(2))             !thickness b/n two interfaces
       do l = 2,num_soil_layers-1
         dzs(l)= 0.5*(zs(l+1)-zs(l-1))
       enddo
       dzs(num_soil_layers) = zs(num_soil_layers)-zs(num_soil_layers-1)

   END SUBROUTINE init_soil_depth_4

   SUBROUTINE init_soil_depth_7 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      dzs = (/ 0.01 , 0.99 /)

      IF ( num_soil_layers .NE. 2 ) THEN
         PRINT '(A)','Usually, the PX LSM uses 2 layers.  Change this in the namelist.'
         CALL wrf_error_fatal ( 'PXLSM_uses_2_layers' )
      END IF

      zs(1) = 0.5 * dzs(1)
      zs(2) = dzs(1) + 0.5 * dzs(2)

   END SUBROUTINE init_soil_depth_7

   SUBROUTINE init_soil_depth_8 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      zs(1) = 0.00
      zs(2) = 0.05
      zs(3) = 1.05

   END SUBROUTINE init_soil_depth_8

   SUBROUTINE init_soil_1_real ( tsk , tmn , tslb , zs , dzs , &
                                 num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , real_data_init_type , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tsk , tmn

      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb

      INTEGER :: i , j , l

      !  Soil temperature is linearly interpolated between the skin temperature (taken to be at a
      !  depth of 0.5 cm) and the deep soil, annual temperature (taken to be at a depth of 23 cm).
      !  The tslb(i,1,j) is the skin temperature, and the tslb(i,num_soil_layers,j) level is the
      !  annual mean temperature.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( landmask(i,j) .GT. 0.5 ) THEN
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= ( tsk(i,j) * ( zs(num_soil_layers) - zs(l) )   + &
                                 tmn(i,j) * ( zs(              l) - zs(1) ) ) / &
                                            ( zs(num_soil_layers) - zs(1) )
               END DO
            ELSE
               IF ( ( real_data_init_type .EQ. 1 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= sst(i,j)
                  END DO
               ELSE
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                  END DO
               END IF
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_1_real

   SUBROUTINE init_soil_1_ideal(tsk,tmn,tslb,xland,             &
                       ivgtyp,ZS,DZS,num_soil_layers,           &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

      IMPLICIT NONE

      INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN   )    ::      num_soil_layers

      REAL, DIMENSION( ims:ime , num_soil_layers , jms:jme ), INTENT(OUT) :: tslb
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: xland
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: ivgtyp

      REAL, DIMENSION(1:), INTENT(IN) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN) :: tsk, tmn

      !  Lcal variables.

      INTEGER :: l,j,i,itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF (num_soil_layers.NE.1)THEN
         DO j=jts,jtf
            DO l=1,num_soil_layers
               DO i=its,itf
                 tslb(i,l,j)=( tsk(i,j)*(zs(num_soil_layers)-zs(l)) + tmn(i,j)*(zs(l)-zs(1)) ) / &
                             ( zs(num_soil_layers)-zs(1) )
               ENDDO
            ENDDO
         ENDDO
      ENDIF

!     DO j=jts,jtf
!        DO i=its,itf
!          xland(i,j)  = 2
!          ivgtyp(i,j) = 7
!        ENDDO
!     ENDDO

   END SUBROUTINE init_soil_1_ideal

   SUBROUTINE init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      !  Are there any soil temp and moisture levels - ya know, they are mandatory.

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         !  Ordered levels that we have data for.

!tgs add option to initialize from RUCLSM
         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume Noah LSM input'
           CALL wrf_message ( message )
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
         END IF


         !  Sort the levels for temperature.

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF

         !  Sort the levels for moisture.

         outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         !  Sort the levels for liquid moisture.

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal ( &
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      !  Is it OK to continue?

      IF ( found_levels ) THEN

         !tgs:  Here are the levels that we have from the input for temperature.

         IF ( flag_soil_levels == 1 ) THEN
            DO l = 1 , num_st_levels_input
               zhave(l) = st_levels_input(l) / 100.
            END DO

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         ELSE

         !  Here are the levels that we have from the input for temperature.  The input levels plus
         !  two more: the skin temperature at 0 cm, and the annual mean temperature at 300 cm.

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt_2: DO lwant = 1 , num_soil_layers
            z_havet_2 : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet_2
               END IF
            END DO z_havet_2
         END DO z_wantt_2

      END IF

!tgs:
      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE
         !  Here are the levels that we have from the input for moisture.  The input levels plus
         !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is taken to be identical
         !  to the most shallow layer's value.  Similarly, the 300 cm value is taken to be the same
         !  as the most deep layer's value.

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantm_2 : DO lwant = 1 , num_soil_layers
            z_havem_2 : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havem_2
               END IF
            END DO z_havem_2
         END DO z_wantm_2
       ENDIF

         !  Any liquid soil moisture to worry about?

         IF ( num_sw_levels_input .GT. 1 ) THEN
      !  Interpolate between the layers we have (zhave) and those that we want
      !  (zs).
      IF ( flag_soil_levels == 1 ) THEN
!tgs: for RUC LSM
      z_wantw : DO lwant = 1 , num_soil_layers
         z_havew : DO lhave = 1 , num_sw_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     sh2o(i,lwant,j)= ( sw_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sw_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havew
            END IF
         END DO z_havew
      END DO z_wantw

     ELSE
            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            !  Interpolate between the layers we have (zhave) and those that we want (zs).

            z_wantw_2 : DO lwant = 1 , num_soil_layers
               z_havew_2 : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew_2
                  END IF
               END DO z_havew_2
            END DO z_wantw_2
       ENDIF

         END IF


         !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
         !  used, but they will make a more continuous plot.

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_2_real

   SUBROUTINE init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,     &
                     ivgtyp,isltyp,tslb,smois,tmn,                  &
                     num_soil_layers,                               &
                     ids,ide, jds,jde, kds,kde,                     &
                     ims,ime, jms,jme, kms,kme,                     &
                     its,ite, jts,jte, kts,kte                      )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) ::num_soil_layers

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(OUT) :: smois, tslb

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN)   :: xland
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT)  :: snow, canwat, xice, vegfra, tmn

      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      INTEGER :: icm,jcm,itf,jtf
      INTEGER ::  i,j,l

      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)

      icm = ide/2
      jcm = jde/2

      DO j=jts,jtf
         DO l=1,num_soil_layers
            DO i=its,itf
               if (xland(i,j) .lt. 1.5) then
               smois(i,1,j)=0.30
               smois(i,2,j)=0.30
               smois(i,3,j)=0.30
               smois(i,4,j)=0.30

               tslb(i,1,j)=290.
               tslb(i,2,j)=290.
               tslb(i,3,j)=290.
               tslb(i,4,j)=290.
               endif
            ENDDO
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_2_ideal

   SUBROUTINE init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask, sst, &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave, k
      REAL :: temp

      CHARACTER (LEN=132) :: message

      !  Allocate the soil layer array used for interpolating.

      IF ( ( num_st_levels_input .LE. 0 ) .OR. &
           ( num_sm_levels_input .LE. 0 ) ) THEN
         write (message, FMT='(A)')&
'No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         CALL wrf_error_fatal ( message )
      ELSE
         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume non-RUC LSM input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout)
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     temp = st_input(i,lout,j)
                     st_input(i,lout,j) = st_input(i,lin,j)
                     st_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      IF ( flag_soil_layers == 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            st_input(i,1,j) = tsk(i,j)
            st_input(i,num_st_levels_input+2,j) = tmn(i,j)
         END DO
      END DO
      END IF

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout)
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     temp = sm_input(i,lout,j)
                     sm_input(i,lout,j) = sm_input(i,lin,j)
                     sm_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      IF ( flag_soil_layers == 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            sm_input(i,1,j) = (sm_input(i,2,j)-sm_input(i,3,j))/   &
                              (st_levels_input(2)-st_levels_input(1))*st_levels_input(1)+  &
                              sm_input(i,2,j)
!           sm_input(i,1,j) = sm_input(i,2,j)
            sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
         END DO
      END DO
      END IF

      !  Here are the levels that we have from the input for temperature.

      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).


      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      END IF

      !  Here are the levels that we have from the input for moisture.

      IF ( flag_soil_levels .EQ. 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
            END IF
         END DO z_havem_2
      END DO z_wantm_2

      END IF
      !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
      !  used, but they will make a more continuous plot.

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst(i,j)
                     tsk(i,j)    = sst(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real

  SUBROUTINE init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      !  Are there any soil temp and moisture levels - ya know, they are
      !  mandatory.

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         !  Ordered levels that we have data for.  

!tgs add option to initialize from RUCLSM
         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume CLM input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume non-CLM input'
           CALL wrf_message ( message )
        ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
      !   ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF


         !  Sort the levels for temperature.

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF
        !  Sort the levels for moisture.

        outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         !  Sort the levels for liquid moisture.

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal ( &
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      !  Is it OK to continue?

      IF ( found_levels ) THEN

         !tgs:  Here are the levels that we have from the input for temperature.

         IF ( flag_soil_levels == 1 ) THEN
            DO l = 1 , num_st_levels_input
               zhave(l) = st_levels_input(l) / 100.
            END DO

         !  Interpolate between the layers we have (zhave) and those that we  want (zs).

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         ELSE

         !  Here are the levels that we have from the input for temperature.  The input levels plus
         !  two more: the skin temperature at 0 cm, and the annual mean  temperature at 300 cm.

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt_2: DO lwant = 1 , num_soil_layers
            z_havet_2 : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet_2
               END IF
            END DO z_havet_2
         END DO z_wantt_2

      END IF

!tgs:
      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                    if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE
         !  Here are the levels that we have from the input for moisture.  The  input levels plus
         !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is  taken to be identical
         !  to the most shallow layer's value.  Similarly, the 300 cm value is  taken to be the same
         !  as the most deep layer's value.

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantm_2 : DO lwant = 1 , num_soil_layers
            z_havem_2 : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                     END DO
                  END DO
                  EXIT z_havem_2
               END IF
            END DO z_havem_2
         END DO z_wantm_2
       ENDIF

         !  Any liquid soil moisture to worry about?

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            !  Interpolate between the layers we have (zhave) and those that we want (zs).

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


         !  Over water, put in reasonable values for soil temperature and  moisture.  These won't be
         !  used, but they will make a more continuous plot.

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_4_real

   SUBROUTINE init_soil_7_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst ,     &
                                 zs , dzs ,                                            &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input ,               &
                                 num_sm_levels_input ,  num_sw_levels_input ,          &
                                 num_st_levels_alloc , num_sm_levels_alloc ,           &
                                 num_sw_levels_alloc ,                                 &
                                 flag_sst , flag_soil_layers , flag_soil_levels ,      &
                                 ids , ide , jds , jde , kds , kde ,                   &
                                 ims , ime , jms , jme , kms , kme ,                   &
                                 its , ite , jts , jte , kts , kte )

      ! for soil temperature and moisture initialization for the PX LSM

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL    :: temp
      LOGICAL :: found_levels

      !  Are there any soil temp and moisture levels - ya know, they are mandatory.

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         !  Ordered levels that we have data for.

         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )

         !  Sort the levels for temperature.
         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO

         !  Sort the levels for moisture.

         outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
              IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO

         !  Sort the levels for liquid moisture.

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
        IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal ( &
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for PX LSM.' )
      END IF

      !  Is it OK to continue?

      IF ( found_levels ) THEN

         !  Here are the levels that we have from the input for temperature.  The input levels plus
         !  two more: the skin temperature at 0 cm, and the annual mean temperature at 300 cm.

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
        END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         !  Here are the levels that we have from the input for moisture.  The input levels plus
         !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is taken to be identical
         !  to the most shallow layer's value.  Similarly, the 300 cm value is taken to be the same
         !  as the most deep layer's value.

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
        zhave(num_sm_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantm : DO lwant = 1 , num_soil_layers
            z_havem : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havem
               END IF
            END DO z_havem
         END DO z_wantm

         !  Any liquid soil moisture to worry about?

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

          !  Interpolate between the layers we have (zhave) and those that we want (zs).

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


        !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
        !  used, but they will make a more continuous plot.

     DO j = jts , MIN(jde-1,jte)
        DO i = its , MIN(ide-1,ite)
             IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
             tslb(i,1,j)= tsk(i,j)
             tslb(i,2,j)= tmn(i,j)
        END DO
     END DO

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_7_real
!------------SSIB (fds 06/2010)-----------------------------
   SUBROUTINE init_soil_8_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
           num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                             num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 st000010, st010200, sm000010, sm010200, &
                                 st010040 , st040100 , st100200, &
                                 sm010040 , sm040100 , sm100200, &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers , flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: st000010, st010200, st010040 , st040100 , st100200
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: sm000010, sm010200, sm010040 , sm040100 , sm100200

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      found_levels = .false.

!      write(message, FMT='(A)') ' Prepare SSiB LSM soil fields'
!fds (06/2010)
!-- SSiB needs NCEP/NCAR reanalysis soil temperature and moisture at 0-10cm and 10-200cm --
      IF ( flag_soil_layers == 1 ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,1,j) = st000010(i,j)
                        tslb(i,2,j) = st010040(i,j)
                        tslb(i,3,j) = st040100(i,j)
                        smois(i,1,j) = sm000010(i,j)
                        smois(i,2,j) = sm010040(i,j)
                        smois(i,3,j) = sm040100(i,j)
! temporary fix (JD) for missing 4-layer input - use 2 instead
                        if(tslb(i,2,j) .lt. 10.)tslb(i,2,j) = st000010(i,j)
                        if(smois(i,2,j) .lt. 0.01)smois(i,2,j) = sm000010(i,j)
                        if(tslb(i,3,j) .lt. 10.)tslb(i,3,j) = st010200(i,j)
                        if(smois(i,3,j) .lt. 0.01)smois(i,3,j) = sm010200(i,j)
                     END DO
                  END DO
      ELSE
          CALL wrf_debug ( 0 , 'SSiB LSM needs 0-10 cm soil t')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 0-10 cm soil m')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 10-200 cm soil t')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 10-200 cm soil m')
      ENDIF

         !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
         !  used, but they will make a more continuous plot.

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

   END SUBROUTINE init_soil_8_real
!--------------------------------------------------------

   SUBROUTINE aggregate_categories_part1  ( landuse_frac , iswater , num_veg_cat , mminlu )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: iswater
      INTEGER , INTENT(IN) :: num_veg_cat
      CHARACTER (LEN=4) , INTENT(IN) :: mminlu
      REAL , DIMENSION(1:num_veg_cat) , INTENT(INOUT):: landuse_frac

      !  Local variables

      INTEGER , PARAMETER :: num_special_bins =  3 ! grass, shrubs, trees; add a new line in the "cib" array if updating this
      INTEGER , PARAMETER :: max_cats_per_bin =  8 ! larger than max num of cats inside each of the special bins
                                                   ! So, no more than 8 total tree categories that we need to consider.
      INTEGER , PARAMETER :: fl               = -1 ! Flag value so that we know this is not a valid category to consider.
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) :: cib_usgs = &
      (/  2 ,  3 ,  4 ,  5 ,  6 ,  7 , 10 , fl , & ! grass
          8 ,  9 , fl , fl , fl , fl , fl , fl , & ! shrubs 
         11 , 12 , 13 , 14 , 15 , fl , fl , fl /)  ! trees
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) :: cib_modis = &
      (/  9 , 10 , 12 , 14 , fl , fl , fl , fl , & ! grass
          6 ,  7 ,  8 , fl , fl , fl , fl , fl , & ! shrubs 
          1 ,  2 ,  3 ,  4 ,  5 , fl , fl , fl /)  ! trees
 
      IF      ( mminlu(1:4) .EQ. 'USGS' ) THEN
         CALL aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib_usgs  )
      ELSE IF ( mminlu(1:4) .EQ. 'MODI' ) THEN
         CALL aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib_modis )
      END IF

   END SUBROUTINE aggregate_categories_part1

   SUBROUTINE aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib )
      IMPLICIT NONE

      INTEGER , INTENT(IN) :: iswater
      INTEGER , INTENT(IN) :: num_veg_cat
      REAL , DIMENSION(1:num_veg_cat) , INTENT(INOUT):: landuse_frac
      INTEGER , INTENT(IN) :: num_special_bins , max_cats_per_bin , fl
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) , INTENT(IN) :: cib

      !  Local variables

      REAL , DIMENSION(1:num_veg_cat) :: landuse_frac_work ! copy of the input array, allows us to be wreckless
      INTEGER , DIMENSION ( max_cats_per_bin , num_special_bins ) :: cats_in_bin
      INTEGER :: ib , ic ! indexes for the bin and the category loops
      REAL    , DIMENSION ( num_special_bins ) :: bin_max_val , bin_sum ! max category value in this bin, sum of all cats in this bin
      INTEGER , DIMENSION ( num_special_bins ) :: bin_max_idx ! index of this maximum category in this bin
      INTEGER :: bin_work , bin_orig ! the bin from whence the maximum hails, respectively for the aggregated and the original data
      INTEGER :: max_cat_orig , max_cat_work
      REAL    :: max_val_orig , max_val_work

      !  Find the max in the original.  If it is >= 50%, no need to even be in here.

      DO ic = 1 , num_veg_cat
         IF ( landuse_frac(ic) .GE. 0.5 ) THEN
            RETURN
         END IF
      END DO

      !  Put the categories in the bin into a 2d array.

      cats_in_bin = RESHAPE ( cib , (/  max_cats_per_bin , num_special_bins /) )

      !  Make a copy of the incoming array so that we can eventually diff our working copy with
      !  the original.

      landuse_frac_work = landuse_frac

      !  Loop over each of the special bins that we know about.  Find the max values and the locations of such.

      DO ib = 1 , num_special_bins

         !  For this bin, we know about specific categories.  We get the sum of all of those
         !  categories that we know about for this bin, and we keep track of which category
         !  is dominant.

         bin_sum    (ib) =  0
         bin_max_val(ib) = fl

         cat_loop_accumulate : DO ic = 1 , max_cats_per_bin
            
            !  Have we run out of valid categories in this bin?  For example, we would not necessarily have
            !  the same number of tree categories as there are grass categories.
      
            IF      ( cats_in_bin(ic,ib) .EQ. fl ) THEN
               EXIT cat_loop_accumulate
            END IF

            bin_sum(ib) = bin_sum(ib) + landuse_frac(cats_in_bin(ic,ib))

            IF ( landuse_frac(cats_in_bin(ic,ib)) .GT. bin_max_val(ib) ) THEN
               bin_max_val(ib) = landuse_frac(cats_in_bin(ic,ib))
               bin_max_idx(ib) = cats_in_bin(ic,ib)
            END IF

         END DO cat_loop_accumulate

         cat_loop_assign : DO ic = 1 , max_cats_per_bin
            
            !  Plow through each cat in the bin.  If we find the dominant one, he gets the total sum.  If we land on
            !  the other guys in the bin, they get set back to zero to maintain the original aggregate influence.
      
            IF      ( cats_in_bin(ic,ib) .EQ. fl ) THEN
               EXIT cat_loop_assign
            ELSE IF ( cats_in_bin(ic,ib) .EQ. bin_max_idx(ib) ) THEN
               landuse_frac_work(cats_in_bin(ic,ib)) = bin_sum(ib)
            ELSE
               landuse_frac_work(cats_in_bin(ic,ib)) = 0
            END IF

         END DO cat_loop_assign

      END DO

      !  Now we loop through the categorical data, and get the max+location for the original input data and the
      !  modified work data.  Water is not allowed to be a "max" category unless it is greater than 50%.  We hit 
      !  that test up at the top already, so we can toss out water willy nilly here.

      max_cat_orig = fl
      max_val_orig =  0
      max_cat_work = fl
      max_val_work =  0

      DO ic = 1 , num_veg_cat
         IF ( ic .EQ. iswater ) THEN
            CYCLE
         END IF
         IF ( landuse_frac(ic) .GT. max_val_orig ) THEN
            max_val_orig = landuse_frac(ic)
            max_cat_orig = ic
         END IF
         IF ( landuse_frac_work(ic) .GT. max_val_work ) THEN
            max_val_work = landuse_frac_work(ic)
            max_cat_work = ic
         END IF
      END DO

      !  Find the bin for the maximimum value of the original data.

      bin_orig = -1
      bin_loop_orig : DO ib = 1 , num_special_bins
         cat_loop_orig : DO ic = 1 , max_cats_per_bin
            IF      ( cats_in_bin(ic,ib) .EQ. fl           ) THEN
               EXIT cat_loop_orig
            ELSE IF ( cats_in_bin(ic,ib) .EQ. max_cat_orig ) THEN
               bin_orig = ib
               EXIT bin_loop_orig
            END IF
         END DO cat_loop_orig
      END DO bin_loop_orig

      !  Find the bin for the maximimum value of the aggregated data.

      bin_work = -1
      bin_loop_work : DO ib = 1 , num_special_bins
         cat_loop_work : DO ic = 1 , max_cats_per_bin
            IF      ( cats_in_bin(ic,ib) .EQ. fl           ) THEN
               EXIT cat_loop_work
            ELSE IF ( cats_in_bin(ic,ib) .EQ. max_cat_work ) THEN
               bin_work = ib
               EXIT bin_loop_work
            END IF
         END DO cat_loop_work
      END DO bin_loop_work

      !  So the big question is, did the aggregation change the bin?  If the aggregation does not change the resulting
      !  bin, then we leave everything alone.  However, if there would be a change in the eventual bin chosen by
      !  the simple dominant-category method, then we become pro-active interventionists and rectify this heinous
      !  injustice foisted upon the lowly flora.

      IF ( bin_work .EQ. bin_orig ) THEN
         !  No op, we do nothing.
      ELSE
         DO ic = 1 , max_cats_per_bin
            landuse_frac(cats_in_bin(ic,bin_work)) = landuse_frac_work(cats_in_bin(ic,bin_work))
         END DO
      END IF
            
   END SUBROUTINE aggregate_categories_part2

END MODULE module_soil_pre

FUNCTION skip_middle_points_t ( ids , ide , jds , jde , &
                                i_in , j_in , width ,   &
                                subtleties_exist )      &
                       RESULT ( skip_it )
 
   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde
   INTEGER , INTENT(IN) :: i_in , j_in , width
   LOGICAL , INTENT(IN) :: subtleties_exist
   
   LOGICAL              :: skip_it

   INTEGER , PARAMETER :: slop = 0

   IF ( ( subtleties_exist ) .OR. ( ide .EQ. 3 ) .OR. ( jde .EQ. 3 ) ) THEN
      skip_it = .FALSE.
   ELSE
      IF ( ( i_in .GE. ids+width+slop ) .AND. ( i_in .LE. ide-1-width-slop )   .AND. &
           ( j_in .GE. jds+width+slop ) .AND. ( j_in .LE. jde-1-width-slop ) ) THEN
         skip_it = .TRUE.
      ELSE
         skip_it = .FALSE.
      END IF
   END IF

END FUNCTION skip_middle_points_t

#else

MODULE module_soil_pre

   USE module_date_time
   USE module_state_description

CONTAINS

   SUBROUTINE adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message


      integer, intent(in) :: fractional_seaice
      real                :: xice_threshold

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         CALL WRF_ERROR_FATAL("NMM cannot use FRACTIONAL_SEAICE = 1")
         xice_threshold = 0.02
      ENDIF

      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     lu_index(i,j)=ivgtyp(i,j)
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( LSMSCHEME )
         CASE ( RUCLSMSCHEME )

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_pre

   SUBROUTINE adjust_for_seaice_post ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      tslb , smois , sh2o , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      num_soil_layers , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme
      INTEGER , INTENT(IN) :: num_soil_layers

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      REAL , DIMENSION(ims:ime,1:num_soil_layers,jms:jme) , INTENT(INOUT):: tslb , smois , sh2o
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold
      REAL :: total_depth , mid_point_depth

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message

      integer, intent(in) :: fractional_seaice
      real                :: xice_threshold
      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         CALL WRF_ERROR_FATAL("NMM cannot use FRACTIONAL_SEAICE = 1")
         xice_threshold = 0.02
      ENDIF
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )

!SBAO
         CASE ( LSMSCHEME, GFDLSLAB )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     tmn(i,j) = 271.16
                     vegcat(i,j)=isice
                     lu_index(i,j)=ivgtyp(i,j)
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                     total_depth = 3. ! ice is 3 m deep, num_soil_layers equispaced layers
                     DO loop = 1,num_soil_layers
                        mid_point_depth=(total_depth/num_soil_layers)/2. + &
                                        (loop-1)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( RUCLSMSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN 

                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ELSE
                        xice(i,j)=max(0.25,xice(i,j))
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                 total_depth = 3. ! ice is 3 m deep, num_soil_layers equispaced layers
                       tslb(i,1,j) = tsk(i,j)
                       tslb(i,num_soil_layers,j) = tmn(i,j)
                     DO loop = 2,num_soil_layers-1
                        mid_point_depth=(total_depth/num_soil_layers)/4. + &
                                        (loop-2)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE IF ( xice(i,j) .LT. xice_threshold ) THEN
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_post

   SUBROUTINE process_percent_cat_new ( landmask ,  &
                                landuse_frac , soil_top_cat , soil_bot_cat , &
                                isltyp , ivgtyp , &
                                num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                iswater )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask

      INTEGER :: i , j , l , ll, dominant_index
      REAL :: dominant_value

#if ( WRF_CHEM == 1 )
!      REAL :: lwthresh = .99
      REAL :: lwthresh = .50
#else
      REAL :: lwthresh = .50
#endif

      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce
      CHARACTER (LEN=1024) :: message

      !  Sanity check on the 50/50 points

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            dominant_value = landuse_frac(i,iswater,j)
            IF ( dominant_value .EQ. lwthresh ) THEN
               DO l = 1 , num_veg_cat
                  IF ( l .EQ. iswater ) CYCLE
                  IF ( landuse_frac(i,l,j) .EQ. lwthresh ) THEN
                write(message, FMT='(I4,I4,A,I4,A,F12.4)') i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                call wrf_message ( message )
                     landuse_frac(i,l,j) = lwthresh - .01
                     landuse_frac(i,iswater,j) = 1. - (lwthresh + 0.01)
                  END IF
               END DO
            END IF
         END DO
      END DO

      !  Compute the dominant VEGETATION INDEX.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            dominant_value = landuse_frac(i,1,j)
            dominant_index = 1
            DO l = 2 , num_veg_cat
               IF        ( l .EQ. iswater ) THEN
                  ! wait a bit
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac(i,l,j) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               END IF
            END DO
            IF ( landuse_frac(i,iswater,j) .GT. lwthresh ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
#if 0
si needs to beef up consistency checks before we can use this part
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh) .AND. &
                      ( dominant_value .EQ. lwthresh) ) THEN
               ! no op
#else
ELSEIF((landuse_frac(i,iswater,j).EQ.lwthresh).AND.(dominant_value.EQ.lwthresh).and.(dominant_index.LT.iswater))THEN
write(message,*)'temporary SI landmask fix'
call wrf_message(trim(message))
! no op
ELSEIF((landuse_frac(i,iswater,j).EQ.lwthresh).AND.(dominant_value.EQ.lwthresh).and.(dominant_index.GT.iswater))THEN
write(message,*)'temporary SI landmask fix'
call wrf_message(trim(message))
dominant_value = landuse_frac(i,iswater,j)
dominant_index = iswater
#endif
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh ) .AND. &
                      ( dominant_value .LT. lwthresh ) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            END IF
            IF      ( dominant_index .EQ. iswater ) THEN
if(landmask(i,j).gt.lwthresh) then
write(message,*) 'changing to water at point ',i,j
call wrf_message(trim(message))
write(message,*)  nint(landuse_frac(i,:,j)*100)
call wrf_message(trim(message))
endif
               landmask(i,j) = 0
            ELSE IF ( dominant_index .NE. iswater ) THEN
if(landmask(i,j).lt.lwthresh) then
write(message,*) 'changing to land at point ',i,j
call wrf_message(trim(message))
write(message,*) nint(landuse_frac(i,:,j)*100)
call wrf_message(trim(message))
endif
               landmask(i,j) = 1
            END IF
            ivgtyp(i,j) = dominant_index
         END DO
      END DO

      !  Compute the dominant SOIL TEXTURE INDEX, TOP.

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            dominant_value = soil_top_cat(i,1,j)
            dominant_index = 1
            IF ( landmask(i,j) .GT. lwthresh ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,l,j) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,l,j)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
                  WRITE ( message , FMT = '(A,I4,I4)' ) &
                  'based on landuse, changing soil to land at point ',i,j
                  CALL wrf_debug(1,message)
!atec             WRITE ( message , FMT = '(16(i3,1x))' ) &
!atec             1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12, 13, 14, 15, 16
!atec             CALL wrf_debug(1,message)
!atec             WRITE ( message , FMT = '(16(i3,1x))' ) &
!atec             nint(soil_top_cat(i,:,j)*100)
!atec             CALL wrf_debug(1,message)
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp(i,j) = dominant_index
         END DO
      END DO

if(iforce.ne.0)then
WRITE(message,FMT='(A,I4,A,I6)' ) &
'forcing artificial silty clay loam at ',iforce,' points, out of ',&
(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
CALL wrf_debug(0,message)
endif

   END SUBROUTINE process_percent_cat_new

   SUBROUTINE process_soil_real ( tsk , tmn , &
                                landmask , sst , &
                                st_input , sm_input , sw_input , st_levels_input , sm_levels_input , sw_levels_input , &
                                zs , dzs , tslb , smois , sh2o , &
                                flag_sst , flag_soilt000, flag_soilm000, &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                sf_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              sf_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk

      INTEGER :: i , j , l , dominant_index , num_soil_cat , num_veg_cat
      REAL :: dominant_value

      !  Initialize the soil depth, and the soil temperature and moisture.

      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME .or. sf_surface_physics .eq. 88 ) .AND. &
                ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input ,&
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000, &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
!        CALL init_soil_old ( tsk , tmn , &
!                             smois , tslb , zs , dzs , num_soil_layers , &
!                             st000010_input , st010040_input , st040100_input , st100200_input , &
!                             st010200_input , &
!                             sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
!                             sm010200_input , &
!                             landmask_input , sst_input , &
!                             ids , ide , jds , jde , kds , kde , &
!                             ims , ime , jms , jme , kms , kme , &
!                             its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME .or. sf_surface_physics .eq. 88 ) .AND. &
                ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input ,&
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
#if 0
!CLM
      ELSE IF ( ( sf_surface_physics .EQ. CLMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_4 ( zs , dzs , num_soil_layers )
         CALL init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
!        CALL init_soil_old ( tsk , tmn , &
!                             smois , tslb , zs , dzs , num_soil_layers , &
!                             st000010_input , st010040_input , st040100_input , st100200_input , &
!                             st010200_input , &
!                             sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
!                             sm010200_input , &
!                             landmask_input , sst_input , &
!                             ids , ide , jds , jde , kds , kde , &
!                             ims , ime , jms , jme , kms , kme , &
!                             its , ite , jts , jte , kts , kte )
#endif
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real

   SUBROUTINE process_soil_ideal ( xland,xice,vegfra,snow,canwat,  &
                                   ivgtyp,isltyp,tslb,smois, &
                                   tsk,tmn,zs,dzs,           &
                                   num_soil_layers,          &
                                   sf_surface_physics ,      &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) :: num_soil_layers , sf_surface_physics

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(INOUT) :: smois, tslb

      REAL, DIMENSION(num_soil_layers), INTENT(OUT) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT) :: tsk, tmn
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: xland, snow, canwat, xice, vegfra
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      !  Local variables.

      INTEGER :: itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_ideal(tsk,tmn,tslb,xland,                      &
                                ivgtyp,zs,dzs,num_soil_layers,           &
                                ids,ide, jds,jde, kds,kde,               &
                                ims,ime, jms,jme, kms,kme,               &
                                its,ite, jts,jte, kts,kte                )
      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )

      END IF

   END SUBROUTINE process_soil_ideal

   SUBROUTINE adjust_soil_temp_new ( tmn , sf_surface_physics , &
                                 tsk , ter , toposoil , landmask , flag_toposoil , &
                                      st000010 ,      st010040 ,      st040100 ,      st100200 ,      st010200 , &
                                 flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , &
                                      soilt000 ,      soilt005 ,      soilt020 ,      soilt040 ,      soilt160 ,      soilt300 , &
                                 flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: ter , toposoil , landmask
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: st000010 , st010040 , st040100 , st100200 , st010200
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: soilt000 , soilt005 , soilt020 , soilt040 , soilt160 , soilt300

      INTEGER , INTENT(IN) :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200
      INTEGER , INTENT(IN) :: flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300
      INTEGER , INTENT(IN) :: sf_surface_physics , flag_toposoil

      INTEGER :: i , j

      REAL :: soil_elev_min_val ,  soil_elev_max_val , soil_elev_min_dif , soil_elev_max_dif

      !  Do we have a soil field with which to modify soil temperatures?

      IF ( flag_toposoil .EQ. 1 ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)

               !  Is the toposoil field OK, or is it a subversive soil elevation field.  We can tell
               !  usually by looking at values.  Anything less than -1000 m (lower than the Dead Sea) is
               !  bad.  Anything larger than 10 km (taller than Everest) is toast.  Also, anything where
               !  the difference between the soil elevation and the terrain is greater than 3 km means
               !  that the soil data is either all zeros or that the data are inconsistent.  Any of these
               !  three conditions is grievous enough to induce a WRF fatality.  However, if they are at
               !  a water point, then we can safely ignore them.

               soil_elev_min_val = toposoil(i,j)
               soil_elev_max_val = toposoil(i,j)
               soil_elev_min_dif = ter(i,j) - toposoil(i,j)
               soil_elev_max_dif = ter(i,j) - toposoil(i,j)

               IF      ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
!print *,'no soil temperature elevation adjustment, soil height too small = ',toposoil(i,j)
cycle
!                 CALL wrf_error_fatal ( 'TOPOSOIL values have large negative values < -1000 m, unrealistic.' )
               ENDIF

               IF      ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, soil height too high = ',toposoil(i,j)
cycle
                  CALL wrf_error_fatal ( 'TOPOSOIL values have large positive values > 10,000 m , unrealistic.' )
               ENDIF

               IF      ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, diff of soil height and terrain = ',ter(i,j) - toposoil(i,j)
cycle
                  CALL wrf_error_fatal ( 'TOPOSOIL difference with terrain elevation differs by more than 3000 m, unrealistic' )
               ENDIF

               !  For each of the fields that we would like to modify, check to see if it came in from the SI.
               !  If so, then use a -6.5 K/km lapse rate (based on the elevation diffs).  We only adjust when we
               !  are not at a water point.

               IF (landmask(i,j) .GT. 0.5 ) THEN

                  IF ( sf_surface_physics .EQ. SLABSCHEME .OR. sf_surface_physics .EQ. PXLSMSCHEME ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF

                  tsk(i,j) = tsk(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )

                  IF ( flag_st000010 .EQ. 1 ) THEN
                     st000010(i,j) = st000010(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st010040 .EQ. 1 ) THEN
                     st010040(i,j) = st010040(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st040100 .EQ. 1 ) THEN
                     st040100(i,j) = st040100(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st100200 .EQ. 1 ) THEN
                     st100200(i,j) = st100200(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st010200 .EQ. 1 ) THEN
                     st010200(i,j) = st010200(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF

                  IF ( flag_soilt000 .EQ. 1 ) THEN
                     soilt000(i,j) = soilt000(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt005 .EQ. 1 ) THEN
                     soilt005(i,j) = soilt005(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt020 .EQ. 1 ) THEN
                     soilt020(i,j) = soilt020(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt040 .EQ. 1 ) THEN
                     soilt040(i,j) = soilt040(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt160 .EQ. 1 ) THEN
                     soilt160(i,j) = soilt160(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt300 .EQ. 1 ) THEN
                     soilt300(i,j) = soilt300(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF

               END IF
            END DO
         END DO

      END IF

   END SUBROUTINE adjust_soil_temp_new


   SUBROUTINE init_soil_depth_1 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message

      !  Define layers (top layer = 0.01 m).  Double the thicknesses at each step (dzs values).
      !  The distance from the ground level to the midpoint of the layer is given by zs.

      !    -------   Ground Level   ----------      ||      ||   ||  ||
      !                                             ||      ||   ||  || zs(1) = 0.005 m
      !    --  --  --  --  --  --  --  --  --       ||      ||   ||  \/
      !                                             ||      ||   ||
      !    -----------------------------------      ||  ||  ||   \/   dzs(1) = 0.01 m
      !                                             ||  ||  ||
      !                                             ||  ||  || zs(2) = 0.02
      !    --  --  --  --  --  --  --  --  --       ||  ||  \/
      !                                             ||  ||
      !                                             ||  ||
      !    -----------------------------------  ||  ||  \/   dzs(2) = 0.02 m
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  || zs(3) = 0.05
      !    --  --  --  --  --  --  --  --  --   ||  \/
      !                                         ||
      !                                         ||
      !                                         ||
      !                                         ||
      !    -----------------------------------  \/   dzs(3) = 0.04 m

      IF ( num_soil_layers .NE. 5 ) THEN
         write(message,FMT= '(A)') 'Usually, the 5-layer diffusion uses 5 layers.  Change this in the namelist.'
         CALL wrf_error_fatal ( message )
      END IF

      dzs(1)=.01
      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         dzs(l)=2*dzs(l-1)
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_1

   SUBROUTINE init_soil_depth_2 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message

      dzs = (/ 0.1 , 0.3 , 0.6 , 1.0 /)

      IF ( num_soil_layers .NE. 4 ) THEN
         write(message,FMT='(A)') 'Usually, the LSM uses 4 layers.  Change this in the namelist.'
         CALL wrf_error_fatal ( message )
      END IF

      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_2

   SUBROUTINE init_soil_depth_3 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message

! in RUC LSM ZS - soil levels, and DZS - soil layer thicknesses, not used
! ZS is specified in the namelist: num_soil_layers = 6 or 9.
! Other options with number of levels are possible, but
! WRF users should change consistently the namelist entry with the
!    ZS array in this subroutine.

     IF ( num_soil_layers .EQ. 6) THEN
      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
!      dzs = (/ 0.00 , 0.125, 0.175 , 0.70 , 1.30 , 1.40 /)
     ELSEIF ( num_soil_layers .EQ. 9) THEN
      zs  = (/ 0.00 , 0.01 , 0.04 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)
!      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 0.60, 1.00, 1.60 , 2.20, 3.00 /)
!      dzs = (/ 0.00 , 0.125, 0.175 , 0.70 , 1.30 , 1.40 /)
     ENDIF

      IF ( num_soil_layers .EQ. 4 .OR. num_soil_layers .EQ. 5 ) THEN
         WRITE(message,FMT= '(A)')'Usually, the RUC LSM uses 6, 9 or more levels.  Change this in the namelist.'
         CALL wrf_error_fatal ( message )
      END IF

   END SUBROUTINE init_soil_depth_3

#if 0
!CLM  added by Jiming Jin for NMM
   SUBROUTINE init_soil_depth_4 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL, PARAMETER :: scalez = 0.025

      INTEGER                   ::      l

      !  Define layers (top layer = 0.01 m).  Double the thicknesses at each
      !  step (dzs values).
      !  The distance from the ground level to the midpoint of the layer is
      !  given by zs.

      !    -------   Ground Level   ----------      ||      ||   ||  ||
      !                                             ||      ||   ||  || zs(1) =
      !                                             0.005 m
      !    --  --  --  --  --  --  --  --  --       ||      ||   ||  \/
      !                                             ||      ||   ||
      !    -----------------------------------      ||  ||  ||   \/   dzs(1) =
      !    0.01 m
      !                                             ||  ||  ||
      !                                             ||  ||  || zs(2) = 0.02
      !    --  --  --  --  --  --  --  --  --       ||  ||  \/
      !                                             ||  ||
      !                                             ||  ||
      !    -----------------------------------  ||  ||  \/   dzs(2) = 0.02 m
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  ||
      !                                         ||  || zs(3) = 0.05
      !    --  --  --  --  --  --  --  --  --   ||  \/
      !                                         ||
      !                                         ||
      !                                         ||
      !                                         ||
      !    -----------------------------------  \/   dzs(3) = 0.04 m

      do l = 1, num_soil_layers
         zs(l) = scalez*(exp(0.5*(l-0.5))-1.)    !node depths
      enddo
       dzs(1) = 0.5*(zs(1)+zs(2))             !thickness b/n two interfaces
       do l = 2,num_soil_layers-1
         dzs(l)= 0.5*(zs(l+1)-zs(l-1))
       enddo
       dzs(num_soil_layers) = zs(num_soil_layers)-zs(num_soil_layers-1)

   END SUBROUTINE init_soil_depth_4
#endif

   SUBROUTINE init_soil_1_real ( tsk , tmn , tslb , zs , dzs , &
                                 num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , real_data_init_type , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tsk , tmn

      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb

      INTEGER :: i , j , l

      !  Soil temperature is linearly interpolated between the skin temperature (taken to be at a
      !  depth of 0.5 cm) and the deep soil, annual temperature (taken to be at a depth of 23 cm).
      !  The tslb(i,1,j) is the skin temperature, and the tslb(i,num_soil_layers,j) level is the
      !  annual mean temperature.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( landmask(i,j) .GT. 0.5 ) THEN
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= ( tsk(i,j) * ( zs(num_soil_layers) - zs(l) )   + &
                                 tmn(i,j) * ( zs(              l) - zs(1) ) ) / &
                                            ( zs(num_soil_layers) - zs(1) )
               END DO
            ELSE
               IF ( ( real_data_init_type .EQ. 1 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= sst(i,j)
                  END DO
               ELSE
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                  END DO
               END IF
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_1_real

   SUBROUTINE init_soil_1_ideal(tsk,tmn,tslb,xland,             &
                       ivgtyp,ZS,DZS,num_soil_layers,           &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

      IMPLICIT NONE

      INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN   )    ::      num_soil_layers

      REAL, DIMENSION( ims:ime , 1 , jms:jme ), INTENT(OUT) :: tslb
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: xland
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: ivgtyp

      REAL, DIMENSION(1:), INTENT(IN) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN) :: tsk, tmn

      !  Lcal variables.

      INTEGER :: l,j,i,itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF (num_soil_layers.NE.1)THEN
         DO j=jts,jtf
            DO l=1,num_soil_layers
               DO i=its,itf
                 tslb(i,l,j)=( tsk(i,j)*(zs(num_soil_layers)-zs(l)) + tmn(i,j)*(zs(l)-zs(1)) ) / &
                             ( zs(num_soil_layers)-zs(1) )
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      DO j=jts,jtf
         DO i=its,itf
           xland(i,j)  = 2
           ivgtyp(i,j) = 7
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_1_ideal

   SUBROUTINE init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input ,sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      !  Are there any soil temp and moisture levels - ya know, they are mandatory.

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         !  Ordered levels that we have data for.
         IF ( flag_soilt000 .eq. 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume Noah LSM input'
           CALL wrf_message ( message )
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
         END IF


         !  Sort the levels for temperature.
!print*,'num_st_levels_input, st_levels_input', num_st_levels_input, st_levels_input
!print*,'num_sm_levels_input,num_sw_levels_input',num_sm_levels_input,num_sw_levels_input

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
!tgs add IF
      IF ( flag_soilt000 .NE. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF


#if ( NMM_CORE == 1 )
!new
!       write(0,*) 'st_input(1) in init_soil_2_real'
!	DO J=MIN(jde-1,jte),JTS, -MIN(jde-1,jte)/15
!       write(0,616) (st_input(I,1,J),I=its , MIN(ide-1,ite),MIN(ide-1,ite)/10)
!	ENDDO

!       write(0,*) 'st_input(2) in init_soil_2_real'
!	DO J=MIN(jde-1,jte),JTS, -MIN(jde-1,jte)/15
!       write(0,616) (st_input(I,2,J),I=its , MIN(ide-1,ite),MIN(ide-1,ite)/10)
!	ENDDO

  616	format(20(f4.0,1x))
#endif

         !  Sort the levels for moisture.

         outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
!tgs add IF
      IF ( flag_soilm000 .NE. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         !  Sort the levels for liquid moisture.

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
           IF ( flag_soilm000 .NE. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
           ENDIF
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal ( &
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      !  Is it OK to continue?

      IF ( found_levels ) THEN

!tgs add IF
      IF ( flag_soilt000 .NE.1) THEN
!tgs initialize from Noah data
         !  Here are the levels that we have from the input for temperature.  The input levels plus
         !  two more: the skin temperature at 0 cm, and the annual mean temperature at 300 cm.

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

     ELSE

!tgs initialize from RUCLSM data
         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).


      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      ENDIF


      IF ( flag_soilm000 .NE. 1 ) THEN
!tgs initialize from Noah
         !  Here are the levels that we have from the input for moisture.  The input levels plus
         !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is taken to be identical
         !  to the most shallow layer's value.  Similarly, the 300 cm value is taken to be the same
         !  as the most deep layer's value.

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantm : DO lwant = 1 , num_soil_layers
            z_havem : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havem
               END IF
            END DO z_havem
         END DO z_wantm

     ELSE

!tgs  initialize from RUCLSM data
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
             END IF
         END DO z_havem_2
      END DO z_wantm_2

     ENDIF
         !  Any liquid soil moisture to worry about?

         IF ( num_sw_levels_input .GT. 1 ) THEN

      IF ( flag_soilm000 .NE. 1 ) THEN
            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            !  Interpolate between the layers we have (zhave) and those that we want (zs).

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw
     ELSE

!tgs  initialize from RUCLSM data
         DO l = 1 , num_sw_levels_input
            zhave(l) = sw_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want
      !  (zs).

      z_wantw_2 : DO lwant = 1 , num_soil_layers
         z_havew_2 : DO lhave = 1 , num_sw_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     sh2o(i,lwant,j)= ( sw_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sw_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havew_2
             END IF
         END DO z_havew_2
      END DO z_wantw_2

     ENDIF
         END IF


         !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
         !  used, but they will make a more continuous plot.

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
!tgs add line for tsk
                        tsk(i,j)    = sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_2_real

   SUBROUTINE init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,     &
                     ivgtyp,isltyp,tslb,smois,tmn,                  &
                     num_soil_layers,                               &
                     ids,ide, jds,jde, kds,kde,                     &
                     ims,ime, jms,jme, kms,kme,                     &
                     its,ite, jts,jte, kts,kte                      )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) ::num_soil_layers

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(OUT) :: smois, tslb

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT)  :: xland, snow, canwat, xice, vegfra, tmn

      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      INTEGER :: icm,jcm,itf,jtf
      INTEGER ::  i,j,l

      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)

      icm = ide/2
      jcm = jde/2

      DO j=jts,jtf
         DO l=1,num_soil_layers
            DO i=its,itf

               smois(i,1,j)=0.10
               smois(i,2,j)=0.10
               smois(i,3,j)=0.10
               smois(i,4,j)=0.10

               tslb(i,1,j)=295.
               tslb(i,2,j)=297.
               tslb(i,3,j)=293.
               tslb(i,4,j)=293.

            ENDDO
         ENDDO
      ENDDO

      DO j=jts,jtf
         DO i=its,itf
            xland(i,j)  =   2
            tmn(i,j)    = 294.
            xice(i,j)   =   0.
            vegfra(i,j) =   0.
            snow(i,j)   =   0.
            canwat(i,j) =   0.
            ivgtyp(i,j) =   7
            isltyp(i,j) =   8
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_2_ideal

   SUBROUTINE init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask, sst, &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      !  Allocate the soil layer array used for interpolating.

      IF ( ( num_st_levels_input .LE. 0 ) .OR. &
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         CALL wrf_error_fatal ( 'no soil data' )
      ELSE
         IF ( flag_soilt000 .eq. 1 ) THEN
           PRINT '(A)',' Assume RUC LSM 6-level input'
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input)  ) )
         ELSE
           PRINT '(A)',' Assume non-RUC LSM input'
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout)
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,lout,j)
                     st_input(i,lout,j) = st_input(i,lin,j)
                     st_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      IF ( flag_soilt000 .NE. 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            st_input(i,1,j) = tsk(i,j)
            st_input(i,num_st_levels_input+2,j) = tmn(i,j)
         END DO
      END DO
      END IF

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout)
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,lout,j)
                     sm_input(i,lout,j) = sm_input(i,lin,j)
                     sm_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      IF ( flag_soilm000 .NE. 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            sm_input(i,1,j) = sm_input(i,2,j)
            sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
         END DO
      END DO
      END IF

      !  Here are the levels that we have from the input for temperature.

      IF ( flag_soilt000 .EQ. 1 ) THEN
         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      END IF

      !  Here are the levels that we have from the input for moisture.

      IF ( flag_soilm000 .EQ. 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
            END IF
         END DO z_havem_2
      END DO z_wantm_2

      END IF
      !  Over water, put in reasonable values for soil temperature and moisture.  These won't be
      !  used, but they will make a more continuous plot.

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst(i,j)
                     tsk(i,j)    = sst(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real

#if 0
! CLM  added by Jiming Jin

  SUBROUTINE init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      !  Are there any soil temp and moisture levels - ya know, they are
      !  mandatory.

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         !  Ordered levels that we have data for.  

!tgs add option to initialize from RUCLSM
         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume CLM input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume non-CLM input'
           CALL wrf_message ( message )
        ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
      !   ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF


         !  Sort the levels for temperature.

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF
        !  Sort the levels for moisture.

        outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
!tgs add IF
      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         !  Sort the levels for liquid moisture.

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal ( &
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      !  Is it OK to continue?

      IF ( found_levels ) THEN

         !tgs:  Here are the levels that we have from the input for temperature.

         IF ( flag_soil_levels == 1 ) THEN
            DO l = 1 , num_st_levels_input
               zhave(l) = st_levels_input(l) / 100.
            END DO

         !  Interpolate between the layers we have (zhave) and those that we  want (zs).

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         ELSE

         !  Here are the levels that we have from the input for temperature.  The input levels plus
         !  two more: the skin temperature at 0 cm, and the annual mean  temperature at 300 cm.

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantt_2: DO lwant = 1 , num_soil_layers
            z_havet_2 : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet_2
               END IF
            END DO z_havet_2
         END DO z_wantt_2

      END IF

!tgs:
      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                    if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE
         !  Here are the levels that we have from the input for moisture.  The  input levels plus
         !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is  taken to be identical
         !  to the most shallow layer's value.  Similarly, the 300 cm value is  taken to be the same
         !  as the most deep layer's value.

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         !  Interpolate between the layers we have (zhave) and those that we want (zs).

         z_wantm_2 : DO lwant = 1 , num_soil_layers
            z_havem_2 : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                     END DO
                  END DO
                  EXIT z_havem_2
               END IF
            END DO z_havem_2
         END DO z_wantm_2
       ENDIF

         !  Any liquid soil moisture to worry about?

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            !  Interpolate between the layers we have (zhave) and those that we want (zs).

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


         !  Over water, put in reasonable values for soil temperature and  moisture.  These won't be
         !  used, but they will make a more continuous plot.

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_4_real
#endif

END MODULE module_soil_pre

#endif
