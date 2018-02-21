#if (NMM_NEST == 1)
!===========================================================================
!
! E-GRID NESTING UTILITIES: This is gopal's doing
!
!===========================================================================

SUBROUTINE med_nest_egrid_configure ( parent , nest )
 USE module_dm
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
 TYPE(domain), POINTER  :: grid
 REAL, PARAMETER                    :: ERAD=6371200.
 REAL, PARAMETER                    :: DTR=0.01745329
 REAL, PARAMETER                    :: DTAD=1.0
 REAL, PARAMETER                    :: CP=1004.6
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 CHARACTER(LEN=255)                 :: message
 CHARACTER (LEN=256) :: char_junk

 INTEGER :: comzilla


!----------------------------------------------------------------------------
!  PURPOSE: 
!         - Initialize nested domain configurations including setting up 
!           wbd,sbd and some other variables and 1D arrays. 
!         - Note that in order to obtain coincident grid points, which  
!           is a basic requirement for RSL, WRF infrastructure, we use 
!           western and southern boundaries of nested domain (nest%wbd0
!           and nest%sbd0 derived from the parent domain. In this case
!           the nested domain may be considered as a part of the parent 
!           domain with a higher resolution (telescoping ?). 
!         - Also note that in this case, the central lat/lons for nested 
!           domain should coincide with the central lat/lons of the parent,
!           although the nested domain NEED NOT be located at the center of
!           the domain.
!----------------------------------------------------------------------------
!
!   BASIC TEST FOR PARENT DOMAIN: CHECK IF JMAX IS ODD. SINCE JDE IN THE NAMELIST
!   IS JMAX + 1, WE NEED TO CHECK IF JDE IS EVEN IN WRF CONTEXT

    IF(MOD(parent%ed32,2) .NE. 0)THEN
     CALL wrf_error_fatal("PARENT DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF

!   BASIC TEST FOR NESTED DOMAIN: CHECK IF JMAX IS ODD. SINCE JDE IN THE NAMELIST
!   IS JMAX + 1, WE NEED TO CHECK IF JDE IS EVEN IN WRF CONTEXT

    IF(MOD(nest%ed32,2) .NE. 0)THEN
     CALL wrf_error_fatal("NESTED DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF

!   Parent grid configuration, including, western and southern boundary

    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS = parent%sp31
    ITE = parent%ep31
    JTS = parent%sp32
    JTE = parent%ep32
    KTS = parent%sp33
    KTE = parent%ep33

!   grid configuration

    ! calculate wbd0 and sbd0 only for MOAD i.e. grid with parent_id == 0
    if (parent%parent_id == 0 ) then        ! Dusan's doing
       parent%wbd0 = -(IDE-2)*parent%dx     ! WBD0: in degrees;factor 2 takes care of dummy last column
       parent%sbd0 = -((JDE-1)/2)*parent%dy ! SBD0: in degrees; note that JDE-1 should be odd  
       parent%wbd0var = parent%wbd0
       parent%sbd0var = parent%sbd0
    end if
    nest%wbd0   = parent%wbd0 + (nest%i_parent_start-1)*2.*parent%dx + mod(nest%j_parent_start+1,2)*parent%dx
    nest%sbd0   = parent%sbd0 + (nest%j_parent_start-1)*parent%dy
    nest%wbd0var = nest%wbd0
    nest%sbd0var = nest%sbd0
    nest%dx     = parent%dx/nest%parent_grid_ratio
    nest%dy     = parent%dy/nest%parent_grid_ratio

    write(message,*)" -     nest%id    = ",nest%id
    CALL wrf_message(trim(message))
    write(message,*)" -   parent%id    = ",nest%id
    CALL wrf_message(trim(message))
    write(message,*)" - i_parent_start = ",nest%i_parent_start
    CALL wrf_message(trim(message))
    write(message,*)" - j_parent_start = ",nest%j_parent_start
    CALL wrf_message(trim(message))
    write(message,*)" - parent%wbd0    = ",parent%wbd0
    CALL wrf_message(trim(message))
    write(message,*)" - parent%sbd0    = ",parent%sbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%wbd0      = ",nest%wbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%sbd0      = ",nest%sbd0
    CALL wrf_message(trim(message))
    write(message,*)" - nest%dx        = ",nest%dx
    CALL wrf_message(trim(message))
    write(message,*)" - nest%dy        = ",nest%dy
    CALL wrf_message(trim(message))
!
    CALL nl_set_dx (nest%id , nest%dx)   ! for output purpose
    CALL nl_set_dy (nest%id , nest%dy)   ! for output purpose

!   set lat-lons; parent set to nested domain

    CALL nl_get_cen_lat (parent%id, parent%cen_lat) ! cen_lat of parent set to nested domain
    CALL nl_get_cen_lon (parent%id, parent%cen_lon) ! cen_lon of parent set to nested domain

   IF ( parent%active_this_task .AND. nest%active_this_task ) THEN
    nest%cen_lat=parent%cen_lat
    nest%cen_lon=parent%cen_lon
!
    CALL nl_set_cen_lat ( nest%id , nest%cen_lat)  ! for output purpose
    CALL nl_set_cen_lon ( nest%id , nest%cen_lon)  ! for output purpose



!   soil configuration

#if ( HWRF == 1 )
!zhang 
    if ( .not.nest%analysis ) then
#endif
    nest%sldpth  = parent%sldpth
    nest%dzsoil  = parent%dzsoil
    nest%rtdpth  = parent%rtdpth
#if ( HWRF == 1 ) 
    endif
#endif

!   numerical set up

    nest%deta        = parent%deta
    nest%aeta        = parent%aeta
    nest%etax        = parent%etax
    nest%dfl         = parent%dfl
    nest%deta1       = parent%deta1
    nest%aeta1       = parent%aeta1
    nest%eta1        = parent%eta1
    nest%deta2       = parent%deta2
    nest%aeta2       = parent%aeta2
    nest%eta2        = parent%eta2
    nest%pdtop       = parent%pdtop
    nest%pt          = parent%pt
    nest%dfrlg       = parent%dfrlg
    nest%num_soil_layers = parent%num_soil_layers
    nest%num_moves       = parent%num_moves

! Unfortunately, some of the single value constants in used in module_initialize have 
! to be defiend here instead of the usual spot in med_initialize_nest_nmm. There
! appears to be a problem in Registry and related code in this area.
!
! state  logical upstrm   -      dyn_nmm     -      -      -


    nest%dlmd   = nest%dx
    nest%dphd   = nest%dy
    nest%dy_nmm = erad*(nest%dphd*dtr)
    nest%cpgfv  = -nest%dt/(48.*nest%dy_nmm)
    nest%en     = nest%dt/( 4.*nest%dy_nmm)*dtad
    nest%ent    = nest%dt/(16.*nest%dy_nmm)*dtad
    nest%f4d    = -.5*nest%dt*dtad
    nest%f4q    = -nest%dt*dtad
    nest%ef4t   = .5*nest%dt/cp

!  Other output configurations that will make grads happy 

   CALL nl_get_truelat1 (parent%id, parent%truelat1 )
   CALL nl_get_truelat2 (parent%id, parent%truelat2 )
#if ( HWRF == 1 )
! bao : to make the restart output identical at the restart initial time for stand_lon
   CALL nl_get_stand_lon (parent%id, parent%stand_lon )
#endif
   CALL nl_get_map_proj (parent%id, parent%map_proj )
   CALL nl_get_iswater (parent%id, parent%iswater )

   nest%truelat1=parent%truelat1
   nest%truelat2=parent%truelat2
!bao
   nest%stand_lon=parent%stand_lon
!bao
   nest%map_proj=parent%map_proj
   nest%iswater=parent%iswater

   CALL nl_set_truelat1(nest%id, nest%truelat1)
   CALL nl_set_truelat2(nest%id, nest%truelat2)
!bao
   CALL nl_set_stand_lon(nest%id, nest%stand_lon)
!bao
   CALL nl_set_map_proj(nest%id, nest%map_proj)
   CALL nl_set_iswater(nest%id, nest%iswater)

   CALL nl_get_mminlu ( 1, char_junk )
   IF ( nest%id .GT. 1 ) THEN
     CALL nl_set_mminlu ( nest%id, char_junk )
   ENDIF

  ENDIF
!!!!!!!!!!!!!
! handle case where task computes parent or nest but not both (case of both is handled above)
  IF ( parent%active_this_task .OR. nest%active_this_task ) THEN
     IF ( parent%active_this_task ) THEN
       comzilla = mpi_comm_to_kid( which_kid( nest%id ) , parent%id )
!debug write(0,*)__FILE__,__LINE__,'Patt: mpi_comm_to_kid ',comzilla,which_kid( nest%id ) ,nest%id, parent%id
       grid => parent
     ELSE
       comzilla = mpi_comm_to_mom( nest%id )
!debug write(0,*)__FILE__,__LINE__,'Natt: mpi_comm_to_mom ',comzilla,nest%id
       grid => nest
     ENDIF

#define BCAST_RARRAY(X) BYTE_BCAST( X, size(X)*RWORDSIZE, comzilla )  
!   soil configuration
#if ( HWRF == 1 )
!zhang 
    if ( .not.nest%analysis ) then
#endif
      CALL BCAST_RARRAY( grid%sldpth ) !  nest%sldpth  = parent%sldpth
      CALL BCAST_RARRAY( grid%dzsoil ) !  nest%dzsoil  = parent%dzsoil
      CALL BCAST_RARRAY( grid%rtdpth ) !  nest%rtdpth  = parent%rtdpth
#if ( HWRF == 1 ) 
    endif
#endif

     IF ( parent%active_this_task ) THEN
       CALL nl_get_mminlu ( 1, char_junk )
     ENDIF
     CALL wrf_dm_bcast_string_comm( char_junk, LEN(char_junk),comzilla) ! nest%mminlu  = trim(parent%mminlu)
     IF ( nest%active_this_task .AND. nest%id .GT. 1 ) THEN
       CALL nl_set_mminlu ( nest%id, char_junk )
     ENDIF

!   numerical set up

      CALL BYTE_BCAST( grid%cen_lat, RWORDSIZE, comzilla )  !
      CALL BYTE_BCAST( grid%cen_lon, RWORDSIZE, comzilla )  !
      CALL nl_set_cen_lat ( nest%id , nest%cen_lat)  ! for output purpose
      CALL nl_set_cen_lon ( nest%id , nest%cen_lon)  ! for output purpose
    ! "For output purposes" is not entirely true, because 
    ! the central lat (parent) is needed in the call to EARTH_LATLON
    ! but it is derived (read from the input) so if this task isn't active
    ! on the parent, it won't know it, so set it here.
    ! Other way to have done this would have been to pass the nested cen_lat/lon
    ! into EARTH_LATLON instead of the parent's.  Flipped a coin.
      IF ( .NOT. parent%active_this_task ) THEN
        CALL nl_set_cen_lat ( parent%id , nest%cen_lat)  ! for output purpose
        CALL nl_set_cen_lon ( parent%id , nest%cen_lon)  ! for output purpose
      ENDIF

      IF ( nest%active_this_task ) THEN
        write(message,*)" - nest%cen_lat   = ",nest%cen_lat
        CALL wrf_message(trim(message))
        write(message,*)" - nest%cen_lon   = ",nest%cen_lon
        CALL wrf_message(trim(message))
      ENDIF

      CALL BYTE_BCAST( grid%truelat1, RWORDSIZE, comzilla )  !
      CALL BYTE_BCAST( grid%truelat2, RWORDSIZE, comzilla )  !
      CALL nl_set_truelat1(nest%id, nest%truelat1)
      CALL nl_set_truelat2(nest%id, nest%truelat2)

      CALL BYTE_BCAST( grid%stand_lon, RWORDSIZE, comzilla )  !
      CALL nl_set_stand_lon(nest%id, nest%stand_lon)

      CALL BYTE_BCAST( grid%map_proj, IWORDSIZE, comzilla )  !
      CALL BYTE_BCAST( grid%iswater, IWORDSIZE, comzilla )  !
      CALL nl_set_map_proj(nest%id, nest%map_proj)
      CALL nl_set_iswater(nest%id, nest%iswater)

      CALL BCAST_RARRAY( grid%deta )  !  nest%deta        = parent%deta
      CALL BCAST_RARRAY( grid%aeta )  !  nest%aeta        = parent%aeta
      CALL BCAST_RARRAY( grid%etax )  !  nest%etax        = parent%etax
      CALL BCAST_RARRAY( grid%dfl )   !  nest%dfl         = parent%dfl
      CALL BCAST_RARRAY( grid%deta1 ) !  nest%deta1       = parent%deta1
      CALL BCAST_RARRAY( grid%aeta1 ) !  nest%aeta1       = parent%aeta1
      CALL BCAST_RARRAY( grid%eta1 )  !  nest%eta1        = parent%eta1
      CALL BCAST_RARRAY( grid%deta2 ) !  nest%deta2       = parent%deta2
      CALL BCAST_RARRAY( grid%aeta2 ) !  nest%aeta2       = parent%aeta2
      CALL BCAST_RARRAY( grid%eta2 )  !  nest%eta2        = parent%eta2
      CALL BYTE_BCAST( grid%pdtop, RWORDSIZE, comzilla ) !  nest%pdtop       = parent%pdtop
      CALL BYTE_BCAST( grid%pt, RWORDSIZE, comzilla )    !  nest%pt          = parent%pt
      CALL BYTE_BCAST( grid%dfrlg, RWORDSIZE, comzilla ) !  nest%dfrlg       = parent%dfrlg
      CALL BYTE_BCAST( grid%num_soil_layers, IWORDSIZE, comzilla ) !  nest%num_soil_layers = parent%num_soil_layers
      CALL BYTE_BCAST( grid%num_moves, IWORDSIZE, comzilla )       !  nest%num_moves       = parent%num_moves

! Unfortunately, some of the single value constants in used in module_initialize have
! to be defiend here instead of the usual spot in med_initialize_nest_nmm. There
! appears to be a problem in Registry and related code in this area.
!
! state  logical upstrm   -      dyn_nmm     -      -      -

  ENDIF

  IF ( nest%active_this_task ) THEN

    nest%dlmd   = nest%dx
    nest%dphd   = nest%dy
    nest%dy_nmm = erad*(nest%dphd*dtr)
    nest%cpgfv  = -nest%dt/(48.*nest%dy_nmm)
    nest%en     = nest%dt/( 4.*nest%dy_nmm)*dtad
    nest%ent    = nest%dt/(16.*nest%dy_nmm)*dtad
    nest%f4d    = -.5*nest%dt*dtad
    nest%f4q    = -nest%dt*dtad
    nest%ef4t   = .5*nest%dt/cp

  ENDIF


!   physics and other configurations
!   CALL nl_get_iswater (parent%id, nest%iswater) ! iswater is just based on parents
!   CALL nl_get_bl_surface_physics (nest%id,  nest%bl_surface_physics )
!   CALL nl_get_num_soil_layers( parent%num_soil_layers )
!   CALL nl_get_real_data_init_type (parent%real_data_init_type)

END SUBROUTINE med_nest_egrid_configure

SUBROUTINE med_construct_egrid_weights ( parent , nest )
 USE module_dm
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
 TYPE(domain), POINTER  :: grid
 LOGICAL, EXTERNAL                  :: wrf_dm_on_monitor
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER                            :: I,J,II,JJ,NII,NJJ
 REAL                               :: parent_CLAT,parent_CLON,parent_WBD,parent_SBD,parent_DLMD,parent_DPHD
 REAL                               :: nest_WBD,nest_SBD,nest_DLMD,nest_DPHD
 REAL                               :: SW_LATD,SW_LOND
 REAL                               :: ADDSUM1,ADDSUM2
 REAL                               :: xr,zr,xc
 INTEGER :: comzilla
!-----------------------------------------------------------------------------------------------------------
!   PURPOSE: 
!           - Initialize lat-lons and determine weights 
!
!----------------------------------------------------------------------------------------------------------

!   First obtain central latitude and longitude for the parent domain

  IF ( parent%active_this_task .OR.  nest%active_this_task ) THEN
    IF ( parent%active_this_task ) THEN
       comzilla = mpi_comm_to_kid( which_kid( nest%id ) , parent%id )
       grid => parent
    ELSE
       comzilla = mpi_comm_to_mom( nest%id )
       grid => nest
    ENDIF
  ENDIF

    CALL nl_get_cen_lat (parent%ID, parent_CLAT)
    CALL nl_get_cen_lon (parent%ID, parent_CLON)

!   Parent grid configuration, including, western and southern boundary

    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    JTS  = parent%sp32
    JTE  = parent%ep32
    KTS  = parent%sp33
    KTE  = parent%ep33
!
    parent_DLMD = parent%dx                ! DLMD: dlamda in degrees 
    parent_DPHD = parent%dy                ! DPHD: dphi in degrees 
    parent_WBD  = parent%wbd0
    parent_SBD  = parent%sbd0

!   Now compute Geodetic lat/lon (Positive East) of parent grid in degrees

  IF ( parent%active_this_task ) THEN

    CALL EARTH_LATLON ( parent%HLAT,parent%HLON,parent%VLAT,parent%VLON,  & !output
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                    & !inputs 
                        parent_CLAT,parent_CLON,                                          &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                          &
                        IMS,IME,JMS,JME,KMS,KME,                                          &
                        ITS,ITE,JTS,JTE,KTS,KTE                                           )
  ENDIF

!   Nested grid configuration, including, western and southern boundary

    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    ITS  = nest%sp31
    ITE  = nest%ep31
    JTS  = nest%sp32
    JTE  = nest%ep32
    KTS  = nest%sp33
    KTE  = nest%ep33
!
    nest_DLMD = nest%dx
    nest_DPHD = nest%dy
    nest_WBD  = nest%wbd0
    nest_SBD  = nest%sbd0

!
!   Now compute Geodetic lat/lon (Positive East) of nest in degrees, with the same central lat-lon
!   as the parent grid
!

  IF ( nest%active_this_task ) THEN
    CALL EARTH_LATLON ( nest%HLAT,nest%HLON,nest%VLAT,nest%VLON, & ! output
                        nest_DLMD,nest_DPHD,nest_WBD,nest_SBD,                   & ! nest inputs
                        parent_CLAT,parent_CLON,                                 & ! parent central lat/lon
                        IDS,IDE,JDS,JDE,KDS,KDE,                                 & ! nested domain dimension
                        IMS,IME,JMS,JME,KMS,KME,                                 &
                        ITS,ITE,JTS,JTE,KTS,KTE                                  )

!   Determine the weights of nested grid h points nearest to H points of parent domain 

  if(nest%vortex_tracker /= 1) then
    CALL G2T2H_new(    nest%IIH,nest%JJH,                            & ! output grid index in parent grid
                       nest%HBWGT1,nest%HBWGT2,                      & ! output weights in terms of parent grid
                       nest%HBWGT3,nest%HBWGT4,                      &
                       nest%I_PARENT_START,nest%J_PARENT_START,      & ! nest start I, J in parent domain
                       3,                              & ! Ratio of parent and child grid ( always = 3 for NMM)
                       IDS,IDE,JDS,JDE,KDS,KDE,            & ! target (nest) dimensions
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
  else
    CALL G2T2H( nest%IIH,nest%JJH,                       & ! output grid index on nested grid
                nest%HBWGT1,nest%HBWGT2,                 & ! output weights on the nested grid 
                nest%HBWGT3,nest%HBWGT4,                 &
                nest%HLAT,nest%HLON,                     & ! target (nest) input lat lon in degrees
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & ! parent res, western and south boundaries
                parent_CLAT,parent_CLON,                         & ! parent central lat,lon, all in degrees
                parent%ed31,parent%ed32,                         & ! parent imax and jmax
                IDS,IDE,JDS,JDE,KDS,KDE,                         & ! 
                IMS,IME,JMS,JME,KMS,KME,                         & ! nested grid configuration
                ITS,ITE,JTS,JTE,KTS,KTE                          ) !
  endif


!   Determine the weights of nested grid v points nearest to V points of parent domain

  if(nest%vortex_tracker /= 1) then
    CALL G2T2V_new(    nest%IIV,nest%JJV,                            & ! output grid index in parent grid
                       nest%VBWGT1,nest%VBWGT2,                      & ! output weights in terms of parent grid
                       nest%VBWGT3,nest%VBWGT4,                      &
                       nest%I_PARENT_START,nest%J_PARENT_START,      & ! nest start I, J in parent domain
                       3,                              & ! Ratio of parent and child grid ( always = 3 for NMM)
                       IDS,IDE,JDS,JDE,KDS,KDE,            & ! target (nest) dimensions
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
  else
    CALL G2T2V( nest%IIV,nest%JJV,                       & ! output grid index on nested grid
                nest%VBWGT1,nest%VBWGT2,                 & ! output weights on the nested grid
                nest%VBWGT3,nest%VBWGT4,                 &
                nest%VLAT,nest%VLON,                     & ! target (nest) input lat lon in degrees
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & ! parent res, western and south boundaries
                parent_CLAT,parent_CLON,                         & ! parent central lat,lon, all in degrees
                parent%ed31,parent%ed32,                         & ! parent imax and jmax
                IDS,IDE,JDS,JDE,KDS,KDE,                         & !
                IMS,IME,JMS,JME,KMS,KME,                         & ! nested grid configuration
                ITS,ITE,JTS,JTE,KTS,KTE                          ) !
  endif

! Generate nearest neighbor information for parent->nest nearest
! neighbor interpolation:
  call INIT_HNEAR(nest%iih, nest%jjh, nest%hbwgt1, nest%hbwgt2, &
                  nest%hbwgt3, nest%hbwgt4,                     &
                  nest%hnear_i, nest%hnear_j,                   &
                  IDS,IDE,JDS,JDE,KDS,KDE,                      &
                  IMS,IME,JMS,JME,KMS,KME,                      &
                  ITS,ITE,JTS,JTE,KTS,KTE)


!*** CHECK WEIGHTS AT MASS AND VELOCITY POINTS

     CALL WEIGTS_CHECK(nest%HBWGT1,nest%HBWGT2,          & ! output weights on the nested grid
                       nest%HBWGT3,nest%HBWGT4,          &
                       nest%VBWGT1,nest%VBWGT2,          & ! output weights on the nested grid
                       nest%VBWGT3,nest%VBWGT4,          &
                       IDS,IDE,JDS,JDE,KDS,KDE,                  & !
                       IMS,IME,JMS,JME,KMS,KME,                  & ! nested grid configuration
                       ITS,ITE,JTS,JTE,KTS,KTE                   )

!*** CHECK DOMAIN BOUNDS BEFORE PROCEEDING TO INTERPOLATION
!
    CALL BOUNDS_CHECK( nest%IIH,nest%JJH,nest%IIV,nest%JJV,   &
                       nest%i_parent_start,nest%j_parent_start,nest%shw,      &
                       IDS,IDE,JDS,JDE,KDS,KDE,                               & !
                       IMS,IME,JMS,JME,KMS,KME,                               & ! nested grid configuration
                       ITS,ITE,JTS,JTE,KTS,KTE                                )
  ENDIF

!------------------------------------------------------------------------------------------

END SUBROUTINE med_construct_egrid_weights 

SUBROUTINE med_set_egrid_locs ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain)             :: parent , nest
 LOGICAL, EXTERNAL                  :: wrf_dm_on_monitor
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER                            :: I,J,II,JJ,NII,NJJ
 REAL                               :: parent_CLAT,parent_CLON,parent_WBD,parent_SBD,parent_DLMD,parent_DPHD
 REAL                               :: nest_WBD,nest_SBD,nest_DLMD,nest_DPHD
 REAL                               :: SW_LATD,SW_LOND
 REAL                               :: ADDSUM1,ADDSUM2
 REAL                               :: xr,zr,xc
character*255 :: message
!-----------------------------------------------------------------------------------------------------------
!   PURPOSE: 
!           - Initialize lat-lons and determine weights 
!
!----------------------------------------------------------------------------------------------------------

!   First obtain central latitude and longitude for the parent domain

    CALL nl_get_cen_lat (parent%ID, parent_CLAT)
    CALL nl_get_cen_lon (parent%ID, parent_CLON)

!   Parent grid configuration, including, western and southern boundary

    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    JTS  = parent%sp32
    JTE  = parent%ep32
    KTS  = parent%sp33
    KTE  = parent%ep33
!
    parent_DLMD = parent%dx                ! DLMD: dlamda in degrees 
    parent_DPHD = parent%dy                ! DPHD: dphi in degrees 
    parent_WBD  = parent%wbd0
    parent_SBD  = parent%sbd0

!   Now compute Geodetic lat/lon (Positive East) of parent grid in degrees

  IF ( parent%active_this_task ) THEN

    CALL EARTH_LATLON ( parent%HLAT,parent%HLON,parent%VLAT,parent%VLON,  & !output
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                    & !inputs 
                        parent_CLAT,parent_CLON,                                          &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                          &
                        IMS,IME,JMS,JME,KMS,KME,                                          &
                        ITS,ITE,JTS,JTE,KTS,KTE                                           )
  ENDIF

    if(nest%id == parent%id) return ! nothing more to do; same data structure

!   Nested grid configuration, including, western and southern boundary

    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    ITS  = nest%sp31
    ITE  = nest%ep31
    JTS  = nest%sp32
    JTE  = nest%ep32
    KTS  = nest%sp33
    KTE  = nest%ep33
!
    nest_DLMD = nest%dx
    nest_DPHD = nest%dy
    nest_WBD  = nest%wbd0
    nest_SBD  = nest%sbd0

!
!   Now compute Geodetic lat/lon (Positive East) of nest in degrees, with the same central lat-lon
!   as the parent grid
!

  IF ( nest%active_this_task ) THEN

    CALL EARTH_LATLON ( nest%HLAT,nest%HLON,nest%VLAT,nest%VLON, & ! output
                        nest_DLMD,nest_DPHD,nest_WBD,nest_SBD,                   & ! nest inputs
                        parent_CLAT,parent_CLON,                                 & ! parent central lat/lon
                        IDS,IDE,JDS,JDE,KDS,KDE,                                 & ! nested domain dimension
                        IMS,IME,JMS,JME,KMS,KME,                                 &
                        ITS,ITE,JTS,JTE,KTS,KTE                                  )
  ENDIF
  end SUBROUTINE med_set_egrid_locs
!======================================================================================
!
! compute earth lat-lons for parent and the nest before interpolations
!------------------------------------------------------------------------------

SUBROUTINE EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                          DLMD1,DPHD1,WBD1,SBD1,   & !input res,west & south boundaries,
                          CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees   
                          IDS,IDE,JDS,JDE,KDS,KDE, &  
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE  )
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME 
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE  
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON

! local


 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13) 
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
 REAL(KIND=KNUM) :: DLMD8,DPHD8,WBD8,SBD8,CLAT8,CLON8
 REAL(KIND=KNUM) :: CPHH, CPHV
!-------------------------------------------------------------------------
 DLMD8=DLMD1
 DPHD8=DPHD1
 WBD8=WBD1
 SBD8=SBD1
 CLAT8=CENTRAL_LAT
 CLON8=CENTRAL_LON
!
      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD8 * DTR                 ! WB:   western boundary in radians
      SB   = SBD8 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD8 * DTR                ! DLM:  dlamda in radians 
      DPH  = DPHD8 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                  ! TDLM: 2.0*dlamda 
      TDPH = DPH + DPH                  ! TDPH: 2.0*DPH 

!     For earth lat lon only

      TPH0  = CLAT8*DTR                ! TPH0: central lat in radians 
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

                                                !    .H
      DO J = JTS,MIN(JTE,JDE-1)                 ! H./    This loop takes care of zig-zag 
!                                               !   \.H  starting points along j  
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   !  ./    TLMH (rotated lats at H points)
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     !  H     (//ly for V points) 
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
                                                              !   .H
         DO I = ITS,MIN(ITE,IDE-1)                            !  / 
           TLMH = TLMH0 + I*TDLM                              !  \.H   .U   .H 
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           CPHH = sqrt(1-SPHH**2)
           GLATH(I,J)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians 
           !CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
           !     - TAN(GLATH(I,J))*TAN(TPH0)
           CLMH = (CTPH*COS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CLON8*DTR + FACTH*ACOS(CLMH)

         ENDDO                                    

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           CPHV = sqrt(1-SPHV**2)
           GLATV(I,J) = ASIN(SPHV)
           !CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
           !     - TAN(GLATV(I,J))*TAN(TPH0)
           CLMV = (CTPV*COS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CLON8*DTR + FACTV*ACOS(CLMV)

         ENDDO

      ENDDO

!     Conversion to degrees (may not be required, eventually)

      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J)  - 360.
          IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.
!
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J)  - 360.
          IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.

       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON

!-----------------------------------------------------------------------------  
SUBROUTINE EARTH_LATLON_r8 ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                            DLMD1,DPHD1,WBD1,SBD1,   & !input res,west & south boundaries,
                            CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            ITS,ITE,JTS,JTE,KTS,KTE  )
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13)
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON

! local


 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
 REAL(KIND=KNUM) :: DLMD8,DPHD8,WBD8,SBD8,CLAT8,CLON8
 REAL(KIND=KNUM) :: CPHH, CPHV
!-------------------------------------------------------------------------
 DLMD8=DLMD1
 DPHD8=DPHD1
 WBD8=WBD1
 SBD8=SBD1
 CLAT8=CENTRAL_LAT
 CLON8=CENTRAL_LON
!
      PI_2 = DACOS(0.D0)
      DTR  = PI_2/90.D0
      WB   = WBD8 * DTR                 ! WB:   western boundary in radians
      SB   = SBD8 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD8 * DTR                ! DLM:  dlamda in radians
      DPH  = DPHD8 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                  ! TDLM: 2.0*dlamda
      TDPH = DPH + DPH                  ! TDPH: 2.0*DPH

!     For earth lat lon only

      TPH0  = CLAT8*DTR                ! TPH0: central lat in radians
      STPH0 = DSIN(TPH0)
      CTPH0 = DCOS(TPH0)

                                                !    .H
      DO J = JTS,MIN(JTE,JDE-1)                 ! H./    This loop takes care of zig-zag
!                                               !   \.H  starting points along j
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   !  ./    TLMH (rotated lats at H points)
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     !  H     (//ly for V points)
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = DSIN(TPHH)
         CTPH = DCOS(TPHH)
         STPV = DSIN(TPHV)
         CTPV = COS(TPHV)
                                                              !   .H
         DO I = ITS,MIN(ITE,IDE-1)                            !  /
           TLMH = TLMH0 + I*TDLM                              !  \.H   .U   .H
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           CPHH = dsqrt(1-SPHH**2)
           GLATH(I,J)=DASIN(SPHH)                              ! GLATH: Earth Lat in radians
           !CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
           !     - TAN(GLATH(I,J))*TAN(TPH0)
           CLMH = (CTPH*DCOS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)
           IF(CLMH .GT. 1.D0) CLMH = 1.0D0
           IF(CLMH .LT. -1.D0) CLMH = -1.0D0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.D0
           GLONH(I,J) = -CLON8*DTR + FACTH*DACOS(CLMH)

         ENDDO

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * DCOS(TLMV)
           CPHV = dsqrt(1.D0-SPHV**2)
           GLATV(I,J) = DASIN(SPHV)
           !CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
           !     - TAN(GLATV(I,J))*TAN(TPH0)
           CLMV = (CTPV*DCOS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.D0) CLMV = 1.D0
           IF(CLMV .LT. -1.D0) CLMV = -1.D0
           FACTV = 1.D0
           IF(TLMV .GT. 0.D0) FACTV = -1.D0
           GLONV(I,J) = -CLON8*DTR + FACTV*DACOS(CLMV)

         ENDDO

      ENDDO

!     Conversion to degrees (may not be required, eventually)

      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .GT. 180.D0) HLON(I,J) = HLON(I,J)  - 360.D0
          IF(HLON(I,J) .LT. -180.D0) HLON(I,J) = HLON(I,J) + 360.D0
!
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.D0) VLON(I,J) = VLON(I,J)  - 360.D0
          IF(VLON(I,J) .LT. -180.D0) VLON(I,J) = VLON(I,J) + 360.D0

       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON_r8

subroutine init_hnear(iih,jjh,hbwgt1,hbwgt2,hbwgt3,hbwgt4, &
                      hnear_i,hnear_j,                     &
                      IDS,IDE,JDS,JDE,KDS,KDE,             &
                      IMS,IME,JMS,JME,KMS,KME,             &
                      ITS,ITE,JTS,JTE,KTS,KTE)
  implicit none
  integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
                         ims,ime,jms,jme,kms,kme, &
                         its,ite,jts,jte,kts,kte, &
                         iih(ims:ime,jms:jme), jjh(ims:ime,jms:jme)
  integer, dimension(ims:ime,jms:jme), intent(out) :: hnear_i,hnear_j
  real, dimension(ims:ime,jms:jme), intent(in) :: hbwgt1, hbwgt2, hbwgt3, hbwgt4

  integer :: i,j,iweight,a
  real :: rweight

  do j=jts,min(jte,jde-1)
     do i=its,min(ite,ide-1)
        a=1-mod(JJH(i,j),2)

        iweight=1
        rweight=hbwgt1(i,j)

        if(hbwgt2(i,j)>rweight) then
           iweight=2 ; rweight=hbwgt2(i,j)
        endif

        if(hbwgt3(i,j)>rweight) then
           iweight=3 ; rweight=hbwgt3(i,j)
        endif

        if(hbwgt4(i,j)>rweight) then
           iweight=4
        endif

        select case(iweight)
        case(1)
           hnear_i(i,j)=IIH(I,J)   ; hnear_j(i,j)=JJH(I,J)
        case(2)
           hnear_i(i,j)=IIH(I,J)+1 ; hnear_j(i,j)=JJH(I,J)
        case(3)
           hnear_i(i,j)=IIH(I,J)+a ; hnear_j(i,j)=JJH(I,J)-1
        case default ! case(4)
           hnear_i(i,j)=IIH(I,J)+a ; hnear_j(i,j)=JJH(I,J)+1
        end select
     enddo
  enddo
end subroutine init_hnear

!-----------------------------------------------------------------------------  

  SUBROUTINE G2T2H( IIH,JJH,                     & ! output grid index and weights 
                    HBWGT1,HBWGT2,               & ! output weights in terms of parent grid
                    HBWGT3,HBWGT4,               & 
                    HLAT,HLON,                   & ! target (nest) input lat lon in degrees
                    DLMD1,DPHD1,WBD1,SBD1,       & ! parent res, west and south boundaries
                    CENTRAL_LAT,CENTRAL_LON,     & ! parent central lat,lon, all in degrees
                    P_IDE,P_JDE,                 & ! parent imax and jmax
                    IDS,IDE,JDS,JDE,KDS,KDE,     & ! target (nest) dIMEnsions
                    IMS,IME,JMS,JME,KMS,KME,     &
                    ITS,ITE,JTS,JTE,KTS,KTE      )

! 
!***  Tom Black   - Initial Version
!***  Gopal       - Revised Version for WRF (includes coincident grid points) 
!*** 
!***  GIVEN PARENT CENTRAL LAT-LONS, RESOLUTION AND WESTERN AND SOUTHERN BOUNDARY,
!***  AND THE NESTED GRID LAT-LONS AT H POINTS, THIS ROUTINE FIRST LOCATES THE 
!***  INDICES,IIH,JJH, OF THE PARENT DOMAIN'S H POINTS THAT LIES CLOSEST TO THE
!***  h POINTS OF THE NESTED DOMAIN   
!
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: P_IDE,P_JDE
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(IN)   :: HLAT,HLON
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH

! local

 INTEGER,PARAMETER :: KNUM=SELECTED_REAL_KIND(13)
 INTEGER           :: IMT,JMT,N2R,MK,K,I,J,DSLP0,DSLOPE
 INTEGER           :: NROW,NCOL,KROWS
 REAL(KIND=KNUM)   :: X,Y,Z,TLAT,TLON
 REAL(KIND=KNUM)   :: PI_2,D2R,R2D,GLAT,GLON,DPH,DLM,TPH0,TLM0,WB,SB                
 REAL(KIND=KNUM)   :: ROW,COL,SLP0,TLATHC,TLONHC,DENOM,SLOPE
 REAL(KIND=KNUM)   :: TLAT1,TLAT2,TLON1,TLON2,DLM1,DLM2,DLM3,DLM4,D1,D2
 REAL(KIND=KNUM)   :: DLA1,DLA2,DLA3,DLA4,S1,R1,DS1,AN1,AN2,AN3                    ! Q
 REAL(KIND=KNUM)   :: DL1,DL2,DL3,DL4,DL1I,DL2I,DL3I,DL4I,SUMDL,TLONO,TLATO 
 REAL(KIND=KNUM)   :: DTEMP
 REAL   , DIMENSION(IMS:IME,JMS:JME)    :: TLATHX,TLONHX
 INTEGER, DIMENSION(IMS:IME,JMS:JME)    :: KOUTB
!------------------------------------------------------------------------------- 

  IMT=2*P_IDE-2             ! parent i dIMEnsions 
  JMT=P_JDE/2               ! parent j dIMEnsions 
  PI_2=ACOS(0.)
  D2R=PI_2/90.
  R2D=1./D2R
  DPH=DPHD1*D2R
  DLM=DLMD1*D2R
  TPH0= CENTRAL_LAT*D2R
  TLM0=-CENTRAL_LON*D2R        ! NOTE THE MINUS HERE
  WB=WBD1*D2R                   ! CONVERT NESTED GRID H POINTS FROM GEODETIC
  SB=SBD1*D2R
  SLP0=DPHD1/DLMD1
  DSLP0=NINT(R2D*ATAN(SLP0))
  DS1=SQRT(DPH*DPH+DLM*DLM)    ! Q
  AN1=ASIN(DLM/DS1)
  AN2=ASIN(DPH/DS1)

  DO J =  JTS,MIN(JTE,JDE-1) 
    DO I = ITS,MIN(ITE,IDE-1) 

!***
!***  LOCATE TARGET h POINTS (HLAT AND HLON) ON THE PARENT DOMAIN AND
!***  DETERMINE THE INDICES IN TERMS OF THE PARENT DOMAIN. FIRST 
!***  CONVERT NESTED GRID h POINTS FROM GEODETIC TO TRANSFORMED 
!***  COORDINATE ON THE PARENT GRID
!

      GLAT=HLAT(I,J)*D2R
      GLON= (360. - HLON(I,J))*D2R
      X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
      Y=-COS(GLAT)*SIN(GLON-TLM0)
      Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
      TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLON=R2D*ATAN(Y/X)

!      ROW=TLAT/DPHD1+JMT         ! JMT IS THE CENTRAL ROW OF THE PARENT DOMAIN
!      COL=TLON/DLMD1+P_IDE-1     ! (P_IDE-1) IS THE CENTRAL COLUMN OF THE PARENT DOMAIN 

      ROW=(TLAT-SBD1)/DPHD1+1     ! Dusan's doing
      COL=(TLON-WBD1)/DLMD1+1     ! Dusan's doing

      NROW=INT(ROW + 0.001)     ! ROUND-OFF IS AVOIDED WITHOUT USING NINT ON PURPOSE
      NCOL=INT(COL + 0.001)     
      TLAT=TLAT*D2R
      TLON=TLON*D2R

!***  
!***
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT h IS AT
!***
!***              V      H
!***
!***
!***                 H           
!***              H      V
!***
!***  THEN LOCATE THE NEAREST H POINT ON THE PARENT GRID
!***
      IF(MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.1.OR.     &     
         MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.0)THEN        
           TLAT1=(NROW-JMT)*DPH                           
           TLAT2=TLAT1+DPH                              
           TLON1=(NCOL-(P_IDE-1))*DLM                                   
           TLON2=TLON1+DLM                                 
           DLM1=TLON-TLON1                                 
           DLM2=TLON-TLON2
!           D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
!           D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
            IF(D1.GT.D2)THEN
             NROW=NROW+1                    ! FIND THE NEAREST H ROW
             NCOL=NCOL+1                    ! FIND THE NEAREST H COLUMN
            ENDIF 
      ELSE
!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT h IS AT
!***
!***              H      V
!***
!***
!***                 H 
!***              V      H
!***
!***  THEN LOCATE THE NEAREST H POINT ON THE PARENT GRID
!***
!***
           TLAT1=(NROW+1-JMT)*DPH
           TLAT2=TLAT1-DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2
!           D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
!           D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
             IF(D1.LT.D2)THEN
              NROW=NROW+1                    ! FIND THE NEAREST H ROW
             ELSE
              NCOL=NCOL+1                    ! FIND THE NEAREST H COLUMN
             ENDIF
      ENDIF

      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2).EQ.1)THEN
        K=KROWS+(NCOL+1)/2
      ELSE
        K=KROWS+P_IDE-1+NCOL/2
      ENDIF

!***
!***  WE NOW KNOW THAT THE INNER GRID POINT IN QUESTION IS
!***  NEAREST TO THE CENTER K AS SEEN BELOW.  WE MUST FIND
!***  WHICH OF THE FOUR H-BOXES (OF WHICH THIS H POINT IS
!***  A VERTEX) SURROUNDS THE INNER GRID h POINT IN QUESTION.
!***
!**
!***                  H
!***
!***
!***
!***            H     V     H
!***
!***
!***               H
!***      H     V     H     V     H
!***
!***
!***
!***            H     V     H
!***
!***
!***
!***                  H
!***
!***
!***  FIND THE SLOPE OF THE LINE CONNECTING h AND THE CENTER H.
!***
    N2R=K/IMT
    MK=MOD(K,IMT)
!
    IF(MK.EQ.0)THEN
      TLATHC=SB+(2*N2R-1)*DPH
    ELSE
      TLATHC=SB+(2*N2R+(MK-1)/(P_IDE-1))*DPH
    ENDIF
!
    IF(MK.LE.(P_IDE-1))THEN
      TLONHC=WB+2*(MK-1)*DLM
    ELSE
      TLONHC=WB+(2*(MK-(P_IDE-1))-1)*DLM
    ENDIF
  
!
!***  EXECUTE CAUTION IF YOU NEED TO CHANGE THESE CONDITIONS. SINCE WE ARE
!***  DEALING WITH SLOPES TO GENERATE DIAMOND SHAPE H BOXES, WE NEED TO BE
!***  CAREFUL HERE      
!

    IF(ABS(TLON-TLONHC) .LE. 1.E-4)TLONHC=TLON
    IF(ABS(TLAT-TLATHC) .LE. 1.E-4)TLATHC=TLAT
    DENOM=(TLON-TLONHC)
!
!***
!***STORE THE LOCATION OF THE WESTERNMOST VERTEX OF THE H-BOX ON
!***THE OUTER GRID THAT SURROUNDS THE h POINT ON THE INNER GRID.
!***
!*** COINCIDENT CONDITIONS

    IF(DENOM.EQ.0.0)THEN

      IF(TLATHC.EQ.TLAT)THEN
        KOUTB(I,J)=K
        IIH(I,J) = NCOL
        JJH(I,J) = NROW
        TLATHX(I,J)=TLATHC
        TLONHX(I,J)=TLONHC
        HBWGT1(I,J)=1.0
        HBWGT2(I,J)=0.0
        HBWGT3(I,J)=0.0
        HBWGT4(I,J)=0.0
      ELSE                                      ! SAME LONGITUDE BUT DIFFERENT LATS
!
         IF(TLATHC .GT. TLAT)THEN      ! NESTED POINT SOUTH OF PARENT
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
         ELSE                                   ! NESTED POINT NORTH OF PARENT
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
         ENDIF
!***
!***
!***                  4
!***
!***                  H
!***             1         2
!***
!***                  3
!***  DL 1-4 ARE THE ANGULAR DISTANCES FROM h TO EACH VERTEX

       TLATO=TLATHX(I,J)
       TLONO=TLONHX(I,J)
       DLM1=TLON-TLONO
       DLA1=TLAT-TLATO                                               ! Q
!      DL1=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM1)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 ! Q 
!
       TLATO=TLATHX(I,J)
       TLONO=TLONHX(I,J)+2.*DLM
       DLM2=TLON-TLONO
       DLA2=TLAT-TLATO                                               ! Q
!      DL2=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM2)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 ! Q
! 
       TLATO=TLATHX(I,J)-DPH
       TLONO=TLONHX(I,J)+DLM
       DLM3=TLON-TLONO
       DLA3=TLAT-TLATO                                               ! Q
!      DL3=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM3)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 ! Q
 
       TLATO=TLATHX(I,J)+DPH
       TLONO=TLONHX(I,J)+DLM
       DLM4=TLON-TLONO
       DLA4=TLAT-TLATO                                               ! Q
!      DL4=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM4)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 ! Q


!      THE BILINEAR WEIGHTS
!***
!***
       AN3=ATAN2(DLA1,DLM1)                                          ! Q
       R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
       S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
       R1=R1/DS1
       S1=S1/DS1
       DL1I=(1.-R1)*(1.-S1)
       DL2I=R1*S1
       DL3I=R1*(1.-S1)
       DL4I=(1.-R1)*S1
!
       HBWGT1(I,J)=DL1I
       HBWGT2(I,J)=DL2I
       HBWGT3(I,J)=DL3I
       HBWGT4(I,J)=DL4I
! 
      ENDIF

    ELSE
!
!*** NON-COINCIDENT POINTS   
!
      SLOPE=(TLAT-TLATHC)/DENOM
      DSLOPE=NINT(R2D*ATAN(SLOPE))

      IF(DSLOPE.LE.DSLP0.AND.DSLOPE.GE.-DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLONHC.GE.-WB-DLM)CALL wrf_error_fatal("1H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K
          IIH(I,J) = NCOL
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC
        ELSE
          IF(TLONHC.LE.WB+DLM)CALL wrf_error_fatal("2H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIH(I,J) = NCOL-2
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC -2.*DLM
        ENDIF

!
      ELSEIF(DSLOPE.GT.DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal("3H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
        ELSE
          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal("4H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
        ENDIF

!
      ELSEIF(DSLOPE.LT.-DSLP0)THEN
        IF(TLON.GT.TLONHC)THEN
          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal("5H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
        ELSE
          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal("6H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
        ENDIF
      ENDIF

!
!***  NOW WE WILL MOVE AS FOLLOWS:
!***
!***
!***                      4
!***
!***
!***  
!***                   H
!***             1                 2
!***
!***
!***
!***
!***                      3
!***
!***
!***
!***  DL 1-4 ARE THE ANGULAR DISTANCES FROM h TO EACH VERTEX
      
      TLATO=TLATHX(I,J)
      TLONO=TLONHX(I,J)
      DLM1=TLON-TLONO
      DLA1=TLAT-TLATO                                               ! Q
!     DL1=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM1)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 ! Q
!
      TLATO=TLATHX(I,J)                                             ! redundant computations
      TLONO=TLONHX(I,J)+2.*DLM
      DLM2=TLON-TLONO
      DLA2=TLAT-TLATO                                               ! Q
!     DL2=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM2)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 ! Q
!
      TLATO=TLATHX(I,J)-DPH
      TLONO=TLONHX(I,J)+DLM
      DLM3=TLON-TLONO
      DLA3=TLAT-TLATO                                               ! Q
!     DL3=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM3)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 ! Q
!
      TLATO=TLATHX(I,J)+DPH
      TLONO=TLONHX(I,J)+DLM
      DLM4=TLON-TLONO
      DLA4=TLAT-TLATO                                               ! Q
!     DL4=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM4)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 ! Q

!     THE BILINEAR WEIGHTS
!***
      AN3=ATAN2(DLA1,DLM1)                                          ! Q
      R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
      S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
      R1=R1/DS1
      S1=S1/DS1
      DL1I=(1.-R1)*(1.-S1)
      DL2I=R1*S1
      DL3I=R1*(1.-S1)
      DL4I=(1.-R1)*S1
!
      HBWGT1(I,J)=DL1I
      HBWGT2(I,J)=DL2I
      HBWGT3(I,J)=DL3I
      HBWGT4(I,J)=DL4I
!
    ENDIF 

!
!***  FINALLY STORE IIH IN TERMS OF E-GRID INDEX
!
      IIH(I,J)=NINT(0.5*IIH(I,J))

      HBWGT1(I,J)=MAX(HBWGT1(I,J),0.0)   ! all weights must be GE zero (postive def)
      HBWGT2(I,J)=MAX(HBWGT2(I,J),0.0)   ! all weights must be GE zero (postive def)
      HBWGT3(I,J)=MAX(HBWGT3(I,J),0.0)   ! all weights must be GE zero (postive def)
      HBWGT4(I,J)=MAX(HBWGT4(I,J),0.0)   ! all weights must be GE zero (postive def)

!      write(message,105)"H WEIGHTS:",I,J,HBWGT1(I,J),HBWGT2(I,J),HBWGT3(I,J),HBWGT4(I,J), &
!                               HBWGT1(I,J)+HBWGT2(I,J)+HBWGT3(I,J)+HBWGT4(I,J),IIH(i,j),JJH(i,j)
!      CALL wrf_message(trim(message))
! 105  format(a,2i4,5f7.3,2i4)

   ENDDO
  ENDDO


  RETURN 
  END SUBROUTINE G2T2H
!========================================================================================


  SUBROUTINE G2T2V( IIV,JJV,                     & ! output grid index and weights
                    VBWGT1,VBWGT2,               & ! output weights in terms of parent grid
                    VBWGT3,VBWGT4,               &
                    VLAT,VLON,                   & ! target (nest) input lat lon in degrees
                    DLMD1,DPHD1,WBD1,SBD1,       & ! parent res, west and south boundaries
                    CENTRAL_LAT,CENTRAL_LON,     & ! parent central lat,lon, all in degrees
                    P_IDE,P_JDE,                 & ! parent imax and jmax
                    IDS,IDE,JDS,JDE,KDS,KDE,     & ! target (nest) dIMEnsions
                    IMS,IME,JMS,JME,KMS,KME,     &
                    ITS,ITE,JTS,JTE,KTS,KTE      )

!
!***  Tom Black   - Initial Version 
!***  Gopal       - Revised Version for WRF (includes coincIDEnt grid points)
!***
!***  GIVEN PARENT CENTRAL LAT-LONS, RESOLUTION AND WESTERN AND SOUTHERN BOUNDARY,
!***  AND THE NESTED GRID LAT-LONS AT V POINTS, THIS ROUTINE FIRST LOCATES THE
!***  INDICES,IIV,JJV, OF THE PARENT DOMAIN'S V POINTS THAT LIES CLOSEST TO THE
!***  V POINTS OF THE NESTED DOMAIN
!
!============================================================================

 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: P_IDE,P_JDE
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)    :: VLAT,VLON
 REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(OUT)   :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),   INTENT(OUT)   :: IIV,JJV

! local

 INTEGER,PARAMETER :: KNUM=SELECTED_REAL_KIND(13)     ! (6) single precision
 INTEGER           :: IMT,JMT,N2R,MK,K,I,J,DSLP0,DSLOPE
 INTEGER           :: NROW,NCOL,KROWS
 REAL(KIND=KNUM)   :: X,Y,Z,TLAT,TLON
 REAL(KIND=KNUM)   :: PI_2,D2R,R2D,GLAT,GLON,DPH,DLM,TPH0,TLM0,WB,SB
 REAL(KIND=KNUM)   :: ROW,COL,SLP0,TLATVC,TLONVC,DENOM,SLOPE
 REAL(KIND=KNUM)   :: TLAT1,TLAT2,TLON1,TLON2,DLM1,DLM2,DLM3,DLM4,D1,D2
 REAL(KIND=KNUM)   :: DLA1,DLA2,DLA3,DLA4,S1,R1,DS1,AN1,AN2,AN3                    ! Q
 REAL(KIND=KNUM)   :: DL1,DL2,DL3,DL4,DL1I,DL2I,DL3I,DL4I,SUMDL,TLONO,TLATO
 REAL(KIND=KNUM)   :: DTEMP
 REAL  , DIMENSION(IMS:IME,JMS:JME)      :: TLATVX,TLONVX
 INTEGER, DIMENSION(IMS:IME,JMS:JME)     :: KOUTB
!-------------------------------------------------------------------------------------

  IMT=2*P_IDE-2             ! parent i dIMEnsions
  JMT=P_JDE/2               ! parent j dIMEnsions
  PI_2=ACOS(0.)
  D2R=PI_2/90.
  R2D=1./D2R
  DPH=DPHD1*D2R
  DLM=DLMD1*D2R
  TPH0= CENTRAL_LAT*D2R
  TLM0=-CENTRAL_LON*D2R        ! NOTE THE MINUS HERE
  WB=WBD1*D2R                   ! DEGREES TO RADIANS
  SB=SBD1*D2R
  SLP0=DPHD1/DLMD1
  DSLP0=NINT(R2D*ATAN(SLP0))
  DS1=SQRT(DPH*DPH+DLM*DLM)    ! Q
  AN1=ASIN(DLM/DS1)
  AN2=ASIN(DPH/DS1)

  DO J =  JTS,MIN(JTE,JDE-1)
    DO I = ITS,MIN(ITE,IDE-1)
!***
!***  LOCATE TARGET V POINTS (VLAT AND VLON) ON THE PARENT DOMAIN AND
!***  DETERMINE THE INDICES IN TERMS OF THE PARENT DOMAIN. FIRST
!***  CONVERT NESTED GRID V POINTS FROM GEODETIC TO TRANSFORMED
!***  COORDINATE ON THE PARENT GRID
!

      GLAT=VLAT(I,J)*D2R
      GLON=(360. - VLON(I,J))*D2R
      X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
      Y=-COS(GLAT)*SIN(GLON-TLM0)
      Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
      TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLON=R2D*ATAN(Y/X)

!      ROW=TLAT/DPHD1+JMT         ! JMT IS THE CENTRAL ROW OF THE PARENT DOMAIN
!      COL=TLON/DLMD1+P_IDE-1     ! (P_IDE-1) IS THE CENTRAL COLUMN OF THE PARENT DOMAIN

      ROW=(TLAT-SBD1)/DPHD1+1     ! Dusan's doing
      COL=(TLON-WBD1)/DLMD1+1     ! Dusan's doing

      NROW=INT(ROW + 0.001)     ! ROUND-OFF IS AVOIDED WITHOUT USING NINT ON PURPOSE
      NCOL=INT(COL + 0.001)
      TLAT=TLAT*D2R
      TLON=TLON*D2R

!***
!***
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT V IS AT
!***
!***              H      V
!***
!***
!***                 V
!***              V      H
!***
!***  THEN LOCATE THE NEAREST V POINT ON THE PARENT GRID
!***

      IF(MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.1.OR.     &
         MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.0)THEN
           TLAT1=(NROW-JMT)*DPH
           TLAT2=TLAT1+DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2
!           D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
!           D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
            IF(D1.GT.D2)THEN
             NROW=NROW+1                    ! FIND THE NEAREST V ROW
             NCOL=NCOL+1                    ! FIND THE NEAREST V COLUMN
            ENDIF
      ELSE

!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT V IS AT
!***
!***              V      H
!***
!***
!***                 V
!***              H      V
!***
!*** THEN LOCATE THE NEAREST V POINT ON THE PARENT GRID
!***
           TLAT1=(NROW+1-JMT)*DPH
           TLAT2=TLAT1-DPH
           TLON1=(NCOL-(P_IDE-1))*DLM
           TLON2=TLON1+DLM
           DLM1=TLON-TLON1
           DLM2=TLON-TLON2
!           D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
!           D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
           D1=ACOS(DTEMP)
           DTEMP=MIN(1.0_KNUM,COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
           D2=ACOS(DTEMP)
             IF(D1.LT.D2)THEN
              NROW=NROW+1                    ! FIND THE NEAREST H ROW
             ELSE
              NCOL=NCOL+1                    ! FIND THE NEAREST H COLUMN
             ENDIF

      ENDIF

      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2).EQ.1)THEN
        K=KROWS+NCOL/2
      ELSE
        K=KROWS+P_IDE-2+(NCOL+1)/2     ! check this one should this not be P_IDE-2 ????
      ENDIF

!***
!***  WE NOW KNOW THAT THE INNER GRID POINT IN QUESTION IS
!***  NEAREST TO THE CENTER K AS SEEN BELOW.  WE MUST FIND
!***  WHICH OF THE FOUR v-BOXES (OF WHICH THIS V POINT IS
!***  A VERTEX) SURROUNDS THE INNER GRID V POINT IN QUESTION.
!***
!***
!***                  V
!***
!***
!***
!***            V     H     V
!***
!***
!***               V
!***      V     H     V     H     V
!***
!***
!***
!***            V     H     V
!***
!***
!***
!***                  V
!***
!***
!***  FIND THE SLOPE OF THE LINE CONNECTING V AND THE CENTER v.
!***
      N2R=K/IMT
      MK=MOD(K,IMT)
!
      IF(MK.EQ.0)THEN
        TLATVC=SB+(2*N2R-1)*DPH
      ELSE
        TLATVC=SB+(2*N2R+MK/(P_IDE-1))*DPH
      ENDIF
!
      IF(MK.LE.(P_IDE-1)-1)THEN
        TLONVC=WB+(2*MK-1)*DLM
      ELSE
        TLONVC=WB+2*(MK-(P_IDE-1))*DLM
      ENDIF

!
!***  EXECUTE CAUTION IF YOU NEED TO CHANGE THESE CONDITIONS. SINCE WE ARE
!***  DEALING WITH SLOPES TO GENERATE DIAMOND SHAPE V BOXES, WE NEED TO BE
!***  CAREFUL HERE
!
       IF(ABS(TLON-TLONVC) .LE. 1.E-4)TLONVC=TLON
       IF(ABS(TLAT-TLATVC) .LE. 1.E-4)TLATVC=TLAT
       DENOM=(TLON-TLONVC)
!
!***
!***STORE THE LOCATION OF THE WESTERNMOST VERTEX OF THE H-BOX ON
!***THE OUTER GRID THAT SURROUNDS THE h POINT ON THE INNER GRID.
!***
!*** COINCIDENT CONDITIONS

     IF(DENOM.EQ.0.0)THEN

       IF(TLATVC.EQ.TLAT)THEN
         KOUTB(I,J)=K
         IIV(I,J) = NCOL
         JJV(I,J) = NROW
         TLATVX(I,J)=TLATVC
         TLONVX(I,J)=TLONVC
         VBWGT1(I,J)=1.0
         VBWGT2(I,J)=0.0
         VBWGT3(I,J)=0.0
         VBWGT4(I,J)=0.0
       ELSE                              ! SAME LONGITUDE BUT DIFFERENT LATS 
          
         IF(TLATVC .GT. TLAT)THEN      ! NESTED POINT SOUTH OF PARENT
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
         ELSE                                   ! NESTED POINT NORTH OF PARENT
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
         ENDIF

!***
!***
!***                  4
!***
!***                  V 
!***             1         2
!***
!***                  3
!***  DL 1-4 ARE THE ANGULAR DISTANCES FROM h TO EACH VERTEX

       TLATO=TLATVX(I,J)
       TLONO=TLONVX(I,J)
       DLM1=TLON-TLONO
       DLA1=TLAT-TLATO                                               ! Q
!      DL1=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM1)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 ! Q
!
       TLATO=TLATVX(I,J)
       TLONO=TLONVX(I,J)+2.*DLM
       DLM2=TLON-TLONO
       DLA2=TLAT-TLATO                                               ! Q
!      DL2=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM2)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 ! Q

       TLATO=TLATVX(I,J)-DPH
       TLONO=TLONVX(I,J)+DLM
       DLM3=TLON-TLONO
       DLA3=TLAT-TLATO                                               ! Q
!      DL3=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM3)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 ! Q

       TLATO=TLATVX(I,J)+DPH
       TLONO=TLONVX(I,J)+DLM
       DLM4=TLON-TLONO
       DLA4=TLAT-TLATO                                               ! Q
!      DL4=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM4)+SIN(TLAT)*SIN(TLATO)) ! Q
       DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 ! Q
                 
!      THE BILINEAR WEIGHTS
!***
       AN3=ATAN2(DLA1,DLM1)                                          ! Q
       R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
       S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
       R1=R1/DS1
       S1=S1/DS1
       DL1I=(1.-R1)*(1.-S1)
       DL2I=R1*S1
       DL3I=R1*(1.-S1)
       DL4I=(1.-R1)*S1
!
       VBWGT1(I,J)=DL1I
       VBWGT2(I,J)=DL2I
       VBWGT3(I,J)=DL3I
       VBWGT4(I,J)=DL4I      

      ENDIF

    ELSE

!
!*** NON-COINCIDENT POINTS
!
      SLOPE=(TLAT-TLATVC)/DENOM
      DSLOPE=NINT(R2D*ATAN(SLOPE))

      IF(DSLOPE.LE.DSLP0.AND.DSLOPE.GE.-DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLONVC.GE.-WB-DLM)CALL wrf_error_fatal("1V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K
          IIV(I,J)=NCOL
          JJV(I,J)=NROW
          TLATVX(I,J)=TLATVC
          TLONVX(I,J)=TLONVC
        ELSE
          IF(TLONVC.LE.WB+DLM)CALL wrf_error_fatal("2V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIV(I,J) = NCOL-2
          JJV(I,J) = NROW
          TLATVX(I,J)=TLATVC
          TLONVX(I,J)=TLONVC-2.*DLM
        ENDIF
 
      ELSEIF(DSLOPE.GT.DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal("3V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
        ELSE
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal("4V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
        ENDIF
 
      ELSEIF(DSLOPE.LT.-DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal("5V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
        ELSE
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal("6V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
        ENDIF
      ENDIF
!
!***  NOW WE WILL MOVE AS FOLLOWS:
!***
!***
!***                      4
!***
!***
!*** 
!***                   V 
!***             1                 2
!***
!***
!***
!***
!***                      3
!***
!***
!***
!***  DL 1-4 ARE THE ANGULAR DISTANCES FROM V TO EACH VERTEX

      TLATO=TLATVX(I,J)
      TLONO=TLONVX(I,J)
      DLM1=TLON-TLONO
      DLA1=TLAT-TLATO                                               ! Q
!     DL1=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM1)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL1=SQRT(DLM1*DLM1+DLA1*DLA1)                                 ! Q
!
      TLATO=TLATVX(I,J)
      TLONO=TLONVX(I,J)+2.*DLM
      DLM2=TLON-TLONO
      DLA2=TLAT-TLATO                                               ! Q
!     DL2=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM2)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL2=SQRT(DLM2*DLM2+DLA2*DLA2)                                 ! Q
!
      TLATO=TLATVX(I,J)-DPH
      TLONO=TLONVX(I,J)+DLM
      DLM3=TLON-TLONO
      DLA3=TLAT-TLATO                                               ! Q
!     DL3=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM3)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL3=SQRT(DLM3*DLM3+DLA3*DLA3)                                 ! Q
!
      TLATO=TLATVX(I,J)+DPH
      TLONO=TLONVX(I,J)+DLM
      DLM4=TLON-TLONO
      DLA4=TLAT-TLATO                                               ! Q
!     DL4=ACOS(COS(TLAT)*COS(TLATO)*COS(DLM4)+SIN(TLAT)*SIN(TLATO)) ! Q
      DL4=SQRT(DLM4*DLM4+DLA4*DLA4)                                 ! Q
 
!     THE BILINEAR WEIGHTS
!***
      AN3=ATAN2(DLA1,DLM1)                                          ! Q
      R1=DL1*SIN(AN2-AN3)/SIN(2.*AN1)
      S1=DL1*SIN(2.*PI_2-2*AN1-AN2+AN3)/SIN(2.*AN1)
      R1=R1/DS1
      S1=S1/DS1
      DL1I=(1.-R1)*(1.-S1)
      DL2I=R1*S1
      DL3I=R1*(1.-S1)
      DL4I=(1.-R1)*S1
!
      VBWGT1(I,J)=DL1I
      VBWGT2(I,J)=DL2I
      VBWGT3(I,J)=DL3I
      VBWGT4(I,J)=DL4I

     ENDIF

!
!***  FINALLY STORE IIH IN TERMS OF E-GRID INDEX
!
      IIV(I,J)=NINT(0.5*IIV(I,J))

      VBWGT1(I,J)=MAX(VBWGT1(I,J),0.0)   ! all weights must be GE zero (postive def)
      VBWGT2(I,J)=MAX(VBWGT2(I,J),0.0)   ! all weights must be GE zero (postive def)
      VBWGT3(I,J)=MAX(VBWGT3(I,J),0.0)   ! all weights must be GE zero (postive def)
      VBWGT4(I,J)=MAX(VBWGT4(I,J),0.0)   ! all weights must be GE zero (postive def)

    ENDDO
  ENDDO

 RETURN
 END SUBROUTINE G2T2V

!-----------------------------------------------------------------------------  

 SUBROUTINE G2T2H_new( IIH,JJH,                            & ! output grid index and weights 
                       HBWGT1,HBWGT2,                      & ! output weights in terms of parent grid
                       HBWGT3,HBWGT4,                      &
                       I_PARENT_START,J_PARENT_START,      & ! nest start I and J in parent domain  
                       RATIO,                              & ! Ratio of parent and child grid ( always = 3 for NMM)
                       IDS,IDE,JDS,JDE,KDS,KDE,            & ! target (nest) dimensions
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
!
!*** XUEJIN ZHANG --- Initial version (09/08/2008)
!*** XUEJIN ZHANG --- Modified for parallel purpose (09/10/2009)
!*** XUEJIN ZHANG --- Modified for parallel purpose (09/29/2009)
!Function: Bilnear interpolation weight and indexing for E-grid
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: JP


!*** ARRANGEMENT OF 4 VERTEXES FROM PARENT DOMAIN
!***
!***                  4
!***
!***                  h
!***             1         2
!***
!***
!***                  3
!
!************************************************************* 
!***  POINT (i,j) SPANS 9 NESTED POINTS 
!***  A VERTEX IN THE NEST DOMAIN AND INDEXING FOR PARENT DOMAIN 
!***
!***
!***                  H
!***                
!***               h     h
!***              / \
!***            h     h     h
!***           / \   / \
!***         H     h     h     H  
!***          \   / \   /   
!***            h     h     h
!***             \   /
!***               h     h
!***      
!***                  H
!***                 
!***
!***
!************************************************************* 
!*** MOVING NEST ALWAYS STARTING FROM MASS GRID H
!*** PLEASE REFER TO E-GRID ARRANGEMENT FIGURE FOR MASS POINTS


 DO J=JTS,MIN(JTE,JDE-1)
 DO I=ITS,MIN(ITE,IDE-1)
    JP = MOD(J,RATIO*2)
    SELECT CASE(JP)
    CASE ( 1 )
      CALL SUB1H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 2 )
      CALL SUB2H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 3 )
      CALL SUB3H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 4 )
      CALL SUB4H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 5 )
      CALL SUB5H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 0 )
      CALL SUB6H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    END SELECT
  105  format(a,4i4,5f7.3)
 END DO
 END DO

 RETURN
 END SUBROUTINE G2T2H_new
 SUBROUTINE SUB1H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 4.0/9.0
    HBWGT2(I,J) = 1.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/9.0
    HBWGT2(I,J) = 4.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB1H
 SUBROUTINE SUB2H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 1.0/3.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 1.0/9.0
    HBWGT4(I,J) = 4.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 2.0/3.0
    HBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB2H
 SUBROUTINE SUB3H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 4.0/9.0
    HBWGT4(I,J) = 1.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 2.0/3.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 1.0/3.0
    HBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB3H
 SUBROUTINE SUB4H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/9.0
    HBWGT2(I,J) = 4.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 4.0/9.0
    HBWGT2(I,J) = 1.0/9.0
    HBWGT3(I,J) = 2.0/9.0
    HBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB4H
 SUBROUTINE SUB5H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)-1
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 1.0/9.0
    HBWGT4(I,J) = 4.0/9.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)+1
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 2.0/3.0
    HBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 1.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB5H
 SUBROUTINE SUB6H(I,J,IIH,JJH,HBWGT1,HBWGT2,HBWGT3,HBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP

  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    HBWGT1(I,J) = 2.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 1.0/3.0
    HBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    HBWGT1(I,J) = 2.0/9.0
    HBWGT2(I,J) = 2.0/9.0
    HBWGT3(I,J) = 4.0/9.0
    HBWGT4(I,J) = 1.0/9.0
   CASE ( 0 )
    IIH(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJH(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    HBWGT1(I,J) = 1.0/3.0
    HBWGT2(I,J) = 0.0
    HBWGT3(I,J) = 0.0
    HBWGT4(I,J) = 2.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB6H

!-----------------------------------------------------------------------------  

 SUBROUTINE G2T2V_new( IIV,JJV,                            & ! output grid index and weights 
                       VBWGT1,VBWGT2,                      & ! output weights in terms of parent grid
                       VBWGT3,VBWGT4,                      &
                       I_PARENT_START,J_PARENT_START,      & ! nest start I and J in parent domain  
                       RATIO,                              & ! Ratio of parent and child grid ( always = 3 for NMM)
                       IDS,IDE,JDS,JDE,KDS,KDE,            & ! target (nest) dimensions
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
!
!*** XUEJIN ZHANG --- Initial version (09/08/2008)
!*** XUEJIN ZHANG --- Modified for parallel purpose (09/10/2009)
!*** XUEJIN ZHANG --- Modified for parallel purpose (09/29/2009)
!Function: Bilnear interpolation weight and indexing for E-grid
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: JP

!*** ARRANGEMENT OF 4 VERTEXES FROM PARENT DOMAIN
!***
!***                  4
!***
!***                  v
!***             1         2
!***
!***
!***                  3
!
!************************************************************* 
!***  POINT (i,j) SPANS 9 NESTED POINTS 
!***  A VERTEX IN THE NEST DOMAIN AND INDEXING FOR PARENT DOMAIN
!***
!***
!***                  V
!***                
!***               v  h  v
!***              / \
!***            v  h  v  h  v
!***           / \   / \
!***         V  H  v  h  v  h  V  H
!***          \   / \   /   
!***            v  h  v  h  v
!***             \   /
!***               v  h  v
!***      
!***                  V
!***                 
!***
!***
!************************************************************* 
!*** MOVING NEST ALWAYS STARTING FROM MASS GRID H
!*** PLEASE REFER TO E-GRID ARRANGEMENT FIGURE FOR WIND POINTS

 DO J=JTS,MIN(JTE,JDE-1)
 DO I=ITS,MIN(ITE,IDE-1)
    JP = MOD(J,RATIO*2)
    SELECT CASE(JP)
    CASE ( 1 )
      CALL SUB1V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 2 )
      CALL SUB2V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 3 )
      CALL SUB3V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 4 )
      CALL SUB4V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 5 )
      CALL SUB5V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    CASE ( 0 )
      CALL SUB6V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &    
                I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                IMS,IME,JMS,JME,KMS,KME,                 &
                ITS,ITE,JTS,JTE,KTS,KTE      )
    END SELECT
 END DO
 END DO

 RETURN
 END SUBROUTINE G2T2V_new
 SUBROUTINE SUB1V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/9.0
    VBWGT2(I,J) = 4.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) 
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 4.0/9.0
    VBWGT2(I,J) = 1.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB1V
 SUBROUTINE SUB2V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 1.0/9.0
    VBWGT4(I,J) = 4.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 2.0/3.0
    VBWGT4(I,J) = 0.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 1.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB2V
 SUBROUTINE SUB3V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 1.0/3.0
    VBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 4.0/9.0
    VBWGT4(I,J) = 1.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 2.0/3.0
   END SELECT
 RETURN
 END SUBROUTINE SUB3V
 SUBROUTINE SUB4V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 0.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 4.0/9.0
    VBWGT2(I,J) = 1.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 1.0/9.0
    VBWGT2(I,J) = 4.0/9.0
    VBWGT3(I,J) = 2.0/9.0
    VBWGT4(I,J) = 2.0/9.0
   END SELECT
 RETURN
 END SUBROUTINE SUB4V
 SUBROUTINE SUB5V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 1.0/3.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO)
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 1.0/9.0
    VBWGT4(I,J) = 4.0/9.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 2.0/3.0
    VBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB5V
 SUBROUTINE SUB6V(I,J,IIV,JJV,VBWGT1,VBWGT2,VBWGT3,VBWGT4, &
                 I_PARENT_START,J_PARENT_START,           & ! nest start I and J in parent domain  
                 RATIO,                                   & ! Ratio of parent and child grid ( always = 3 for NMM)
                 IDS,IDE,JDS,JDE,KDS,KDE,                 & ! target (nest) dimensions
                 IMS,IME,JMS,JME,KMS,KME,                 &
                 ITS,ITE,JTS,JTE,KTS,KTE      )
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
 INTEGER,    INTENT(IN   )                            :: RATIO
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
!LOCAL VARIABLES
 INTEGER                                              :: I,J
 INTEGER                                              :: IP
  IP = MOD(I,RATIO)
  SELECT CASE(IP)
   CASE ( 1 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO) - 1
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/9.0
    VBWGT2(I,J) = 2.0/9.0
    VBWGT3(I,J) = 4.0/9.0
    VBWGT4(I,J) = 1.0/9.0
   CASE ( 2 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) 
    VBWGT1(I,J) = 1.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 0.0
    VBWGT4(I,J) = 2.0/3.0
   CASE ( 0 )
    IIV(I,J)    = I_PARENT_START + INT((I-1)/RATIO)
    JJV(I,J)    = J_PARENT_START + INT((J-1)/RATIO) + 1
    VBWGT1(I,J) = 2.0/3.0
    VBWGT2(I,J) = 0.0
    VBWGT3(I,J) = 1.0/3.0
    VBWGT4(I,J) = 0.0
   END SELECT
 RETURN
 END SUBROUTINE SUB6V


!------------------------------------------------------------------------------
!
SUBROUTINE WEIGTS_CHECK(HBWGT1,HBWGT2,HBWGT3,HBWGT4,       & 
                        VBWGT1,VBWGT2,VBWGT3,VBWGT4,       &
                        IDS,IDE,JDS,JDE,KDS,KDE,           &
                        IMS,IME,JMS,JME,KMS,KME,           & 
                        ITS,ITE,JTS,JTE,KTS,KTE            )

  IMPLICIT NONE
  INTEGER, INTENT(IN)                                 :: IDS,IDE,JDS,JDE,KDS,KDE,  &
                                                         IMS,IME,JMS,JME,KMS,KME,  &
                                                         ITS,ITE,JTS,JTE,KTS,KTE
  REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)   :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
  REAL,    DIMENSION(IMS:IME,JMS:JME),   INTENT(IN)   :: VBWGT1,VBWGT2,VBWGT3,VBWGT4

! local 

  REAL , PARAMETER :: EPSI=1.0E-3
  INTEGER          :: I,J
  REAL             :: ADDSUM
 CHARACTER(LEN=255):: message

!-------------------------------------------------------------------------------------

! DUE TO THE NEED FOR HALO EXCHANGES IN PARALLEL RUNS ONE HAS TO ENSURE CONSISTENT 
! USAGE OF NUMBER OF PROCESSORS BEFORE ANY FURTHER COMPUTATIONS. WE INTRODUCE THIS
! CHECK FIRST

  IF((ITE-ITS) .LE. 4 .OR. (JTE-JTS) .LE. 4)THEN
   WRITE(message,*)'ITE-ITS=',ITE-ITS,'JTE-JTS=',JTE-JTS
   CALL wrf_message(trim(message))
   CALL wrf_error_fatal ('NESTED DOMAIN:PLEASE OPTIMIZE THE NUMBER OF PROCESSES; TRY SQUARES OF NUMBERS') 
  ENDIF

!  
! NOW CHECK WEIGHTS
!

  ADDSUM=0.
  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      ADDSUM=HBWGT1(I,J)+HBWGT2(I,J)+HBWGT3(I,J)+HBWGT4(I,J)
      IF(ABS(1.0-ADDSUM) .GE. EPSI)THEN
       WRITE(message,*)'I=',I,'J=',J,'WEIGHTS=',HBWGT1(I,J),HBWGT2(I,J),HBWGT3(I,J),HBWGT4(I,J),1-ADDSUM
       CALL wrf_message(trim(message))
       CALL wrf_error_fatal ('NESTED DOMAIN:SOMETHING IS WRONG WITH WEIGHTS COMPUTATION AT MASS POINTS') 
      ENDIF
   ENDDO
  ENDDO

  ADDSUM=0.
  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      ADDSUM=VBWGT1(I,J)+VBWGT2(I,J)+VBWGT3(I,J)+VBWGT4(I,J)
      IF(ABS(1.0-ADDSUM) .GE. EPSI)THEN 
       WRITE(message,*)'I=',I,'J=',J,'WEIGHTS=',VBWGT1(I,J),VBWGT2(I,J),VBWGT3(I,J),VBWGT4(I,J),1-ADDSUM
       CALL wrf_message(trim(message))
       CALL wrf_error_fatal ('NESTED DOMAIN:SOMETHING IS WRONG WITH WEIGHTS COMPUTATION AT VELOCITY POINTS')
      ENDIF
   ENDDO
  ENDDO

END SUBROUTINE WEIGTS_CHECK

!-----------------------------------------------------------------------------------

SUBROUTINE BOUNDS_CHECK( IIH,JJH,IIV,JJV,          &
                         IPOS,JPOS,SHW,            &
                         IDS,IDE,JDS,JDE,KDS,KDE,  & !
                         IMS,IME,JMS,JME,KMS,KME,  & ! nested grid configuration
                         ITS,ITE,JTS,JTE,KTS,KTE   )

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: IPOS,JPOS,SHW,            &
                        IDS,IDE,JDS,JDE,KDS,KDE,  & 
                        IMS,IME,JMS,JME,KMS,KME,  & 
                        ITS,ITE,JTS,JTE,KTS,KTE   

 INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IIH,JJH,IIV,JJV

! local variables

 INTEGER :: I,J
 CHARACTER(LEN=255)                 :: message

!***  Gopal       - Initial version 
!***
!*** CHECK DOMAIN BOUNDS BEFORE PROCEEDING TO INTERPOLATION
!
!============================================================================

  IF(IPOS .LE. SHW)CALL wrf_error_fatal('NESTED DOMAIN TOO CLOSE TO PARENTs X-BOUNDARY')
  IF(JPOS .LE. SHW)CALL wrf_error_fatal('NESTED DOMAIN TOO CLOSE TO PARENTs Y-BOUNDARY')

  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      IF(IIH(I,J) .EQ. 0)CALL wrf_error_fatal ('IIH=0: SOMETHING IS WRONG')
      IF(JJH(I,J) .EQ. 0)CALL wrf_error_fatal ('JJH=0: SOMETHING IS WRONG')
   ENDDO
  ENDDO

  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      IF(IIH(I,J) .LT. (IPOS-SHW) .OR. JJH(I,J) .LT. (JPOS-SHW) .OR.   &
         IIV(I,J) .LT. (IPOS-SHW) .OR. JJV(I,J) .LT. (JPOS-SHW))THEN
         WRITE(message,*)I,J,IIH(I,J),IPOS,JJH(I,J),JPOS,SHW
         CALL wrf_message(trim(message))
         WRITE(message,*)I,J,IIV(I,J),IPOS,JJV(I,J),JPOS,SHW
         CALL wrf_message(trim(message))
         CALL wrf_error_fatal ('CHECK NESTED DOMAIN BOUNDS: TRY INCREASING STENCIL WIDTH') 
      ENDIF
   ENDDO
  ENDDO

END SUBROUTINE BOUNDS_CHECK

!==========================================================================================


SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                               PINT,T,Q,CWM,            &
                               FIS,QS,PD,PDTOP,PTOP,    &
                               ETA1,ETA2,               &
                               DETA1,DETA2,             &
                               IDS,IDE,JDS,JDE,KDS,KDE, &
                               IMS,IME,JMS,JME,KMS,KME, &
                               ITS,ITE,JTS,JTE,KTS,KTE  )
!

 USE MODULE_MODEL_CONSTANTS
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 REAL,       INTENT(IN   )                            :: PDTOP,PTOP
 REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
 REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QS
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
 REAL, DIMENSION(KMS:KME),                 INTENT(OUT):: PSTD
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d

! local

 INTEGER,PARAMETER                                    :: JTB=134
 INTEGER                                              :: I,J,K,ILOC,JLOC
 REAL, PARAMETER                                      :: LAPSR=6.5E-3, GI=1./G,D608=0.608
 REAL, PARAMETER                                      :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
 REAL, PARAMETER                                      :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
 REAL, PARAMETER                                      :: P_REF=103000.
 REAL                                                 :: A,B,APELP,RTOPP,DZ,ZMID
 REAL, DIMENSION(IMS:IME,JMS:JME)                     :: SLP,TSFC,ZMSLP
 REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME)             :: Z3d_IN
 REAL,DIMENSION(JTB)                                  :: PIN,ZIN,Y2,PIO,ZOUT,DUM1,DUM2
 REAL,DIMENSION(JTB)                                  :: QIN,QOUT,TIN,TOUT
!-------------------------------------------------------------------------------------- 

!  CLEAN Z3D ARRAY FIRST

    DO K=KDS,KDE
     DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d(I,J,K)=0.0
       T3d(I,J,K)=0.0
       Q3d(I,J,K)=0.0
      ENDDO
     ENDDO
    ENDDO 


!  DETERMINE THE HEIGHTS ON THE PARENT DOMAIN

    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d_IN(I,J,1)=FIS(I,J)*GI
      ENDDO
    ENDDO 

    DO K = KDS,KDE-1
     DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
        APELP    = (PINT(I,J,K+1)+PINT(I,J,K))
!       RTOPP    = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608-CWM(I,J,K))/APELP
        RTOPP    = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
        DZ       = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))   ! (RTv/P_TOT)*D(P_HYDRO)
        Z3d_IN(I,J,K+1) = Z3d_IN(I,J,K) + DZ
      ENDDO
     ENDDO
    ENDDO


!  CONSTRUCT STANDARD ISOBARIC SURFACES 

    DO K=KDS,KDE                         ! target points in model interface levels (pint)
       PSTD(K) = ETA1(K)*PDTOP + ETA2(K)*(P_REF -PDTOP - PTOP) + PTOP
    ENDDO

!   DETERMINE THE MSLP USE THAT TO CREATE HEIGHTS AT 1000. mb LEVEL. THESE HEIGHTS  
!   MAY ONLY BE USED IN VERTICAL INTERPOLATION TO ISOBARIC SURFACES WHICH ARE LOCATED
!   BELOW GROUND LEVEL. 

    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
        TSFC(I,J) = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z3d_IN(I,J,1)+Z3d_IN(I,J,2))*0.5
        A         = LAPSR*Z3d_IN(I,J,1)/TSFC(I,J)
        SLP(I,J)  = PINT(I,J,1)*(1-A)**COEF2    ! sea level pressure 
        B         = (PSTD(1)/SLP(I,J))**COEF3
        ZMSLP(I,J)= TSFC(I,J)*LAPSI*(1.0 - B)   ! Height at 1000. mb level
      ENDDO
    ENDDO

!   INTERPOLATE Z3d_IN TO STANDARD PRESSURE INTERFACES. FOR LEVELS BELOW
!   GROUND USE ZMSLP(I,J)

    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
!    
!     clean local array before use of spline

      PIN=0.;ZIN=0.;Y2=0;PIO=0.;ZOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS,KDE                           ! inputs at model interfaces 
         PIN(K) = PINT(I,J,KDE-K+1)
         ZIN(K) = Z3d_IN(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
          PIN(KDE) = PSTD(1)
          ZIN(KDE) = ZMSLP(I,J)
       ENDIF
!
       Y2(1  )=0.
       Y2(KDE)=0.
!
       DO K=KDS,KDE
          PIO(K)=PSTD(K)
       ENDDO
!
       CALL SPLINE1(I,J,JTB,KDE,PIN,ZIN,Y2,KDE,PIO,ZOUT,DUM1,DUM2)  ! interpolate
!

       DO K=KDS,KDE                           ! inputs at model interfaces
         Z3d(I,J,K)=ZOUT(K)
       ENDDO

      ENDDO
    ENDDO
!
!   INTERPOLATE TEMPERATURE ONTO THE STANDARD PRESSURE LEVELS. FOR LEVELS BELOW  
!   GROUND USE A LAPSE RATE ATMOSPHERE 
!
    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
!  
!     clean local array before use of spline or linear interpolation
!
      PIN=0.;TIN=0.;Y2=0;PIO=0.;TOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS+1,KDE                           ! inputs at model levels
         PIN(K-1) = EXP((ALOG(PINT(I,J,KDE-K+1))+ALOG(PINT(I,J,KDE-K+2)))*0.5)
         TIN(K-1) = T(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
         PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)
         ZMID     = 0.5*(Z3d_IN(I,J,1)+Z3d_IN(I,J,2))
         TIN(KDE-1) = T(I,J,1) + LAPSR*(ZMID-ZMSLP(I,J))
       ENDIF
!
       Y2(1    )=0.
       Y2(KDE-1)=0.
!
       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,TIN,Y2,KDE-1,PIO,TOUT,DUM1,DUM2)  ! interpolate


       DO K=KDS,KDE-1                           ! inputs at model levels
         T3d(I,J,K)=TOUT(K)
       ENDDO

      ENDDO
    ENDDO

!
!   INTERPOLATE MOISTURE ONTO THE STANDARD PRESSURE LEVELS. FOR LEVELS BELOW 
!   GROUND USE THE SURFACE MOISTURE 
!
    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
!
!     clean local array before use of spline or linear interpolation


      PIN=0.;QIN=0.;Y2=0;PIO=0.;QOUT=0.;DUM1=0.;DUM2=0.

       DO K=KDS+1,KDE                           ! inputs at model levels
         PIN(K-1) = EXP((ALOG(PINT(I,J,KDE-K+1))+ALOG(PINT(I,J,KDE-K+2)))*0.5)
         QIN(K-1) = Q(I,J,KDE-K+1)
       ENDDO

       IF(PINT(I,J,1) .LE. PSTD(1))THEN
          PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)
!         QIN(KDE-1) =  QS(I,J)
       ENDIF

       Y2(1    )=0.
       Y2(KDE-1)=0.
!
       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,QIN,Y2,KDE-1,PIO,QOUT,DUM1,DUM2)  ! interpolate

       DO K=KDS,KDE-1                          ! inputs at model levels
         Q3d(I,J,K)=QOUT(K)
       ENDDO

      ENDDO
    ENDDO

END SUBROUTINE BASE_STATE_PARENT
!=============================================================================
      SUBROUTINE SPLINE1(I,J,JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!
!   ******************************************************************
!   *                                                                *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE        *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                         *
!   *                                                                *
!   *  PROGRAMER Z. JANJIC                                           *
!   *                                                                *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3. *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.       *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.   *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE      *
!   *         SPECIFIED.                                             *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.     *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)   *
!   *         AND LE XOLD(NOLD).                                     *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.           *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                *
!   *                                                                *
!   ******************************************************************
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: I,J,JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW
!
      INTEGER :: II,JJ,K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                 &
             ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
      CHARACTER(LEN=255) :: message
!---------------------------------------------------------------------

!     debug

      II=9999 !67 !35 !50   !4
      JJ=9999 !31 !73 !115  !192
#if ( HWRF == 1 )
      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(message,*)'DEBUG in SPLINE1:HSO= ',xnew(1:nold)
        CALL wrf_debug(1,trim(message))
        DO K=1,NOLD
         WRITE(message,*)'DEBUG in SPLINE1:L,ZETAI,PINTI= ' &
                        ,K,YOLD(K),XOLD(K)
         CALL wrf_debug(1,trim(message))
        ENDDO 
      ENDIF 
#endif

!
      NOLDM1=NOLD-1
!
      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)
!
      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR
!
      IF(NOLD.EQ.3)GO TO 150
!---------------------------------------------------------------------
      K=3
!
  100 DXL=DXR
      DYDXL=DYDXR
      DXR=XOLD(K+1)-XOLD(K)
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR
      DXC=DXL+DXR
      DEN=1./(DXL*Q(K-2)+DXC+DXC)
!
      P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
      Q(K-1)=-DEN*DXR
!
      K=K+1
      IF(K.LT.NOLD)GO TO 100
!-----------------------------------------------------------------------
  150 K=NOLDM1
!
  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
!
      K=K-1
      IF(K.GT.1)GO TO 200
!-----------------------------------------------------------------------
      K1=1
!
  300 XK=XNEW(K1)
!
      DO 400 K2=2,NOLD
!
      IF(XOLD(K2).GT.XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF
!
  400 CONTINUE
!
      YNEW(K1)=YOLD(NOLD)
      GO TO 600
!
  450 IF(K1.EQ.1)GO TO 500
      IF(K.EQ.KOLD)GO TO 550
!
  500 K=KOLD
!
      Y2K=Y2(K)
      Y2KP1=Y2(K+1)
      DX=XOLD(K+1)-XOLD(K)
      RDX=1./DX
!
      AK=.1666667*RDX*(Y2KP1-Y2K)
      BK=0.5*Y2K
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
!
  550 X=XK-XOLD(K)
      XSQ=X*X
!
      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)

!  debug

#if ( HWRF == 1 )
      if(i.eq.ii.and.j.eq.jj)then
        write(message,*) 'DEBUG:: k1,xnew(k1),ynew(k1): ', k1,xnew(k1),ynew(k1)
        CALL wrf_debug(1,trim(message))
      endif
#endif

!
  600 K1=K1+1
      IF(K1.LE.NNEW)GO TO 300

      RETURN
      END SUBROUTINE SPLINE1
!---------------------------------------------------------------------

SUBROUTINE NEST_TERRAIN ( nest, config_flags )

 USE module_domain
 USE module_configure
 USE module_timing
 USE module_TERRAIN
 USE wrfsi_static
 USE module_SMOOTH_TERRAIN

 IMPLICIT NONE

 TYPE(domain) , POINTER                        :: nest
 TYPE(grid_config_rec_type) , INTENT(IN)       :: config_flags

!
! Local variables
!

 LOGICAL, EXTERNAL                 :: wrf_dm_on_monitor
 INTEGER                           :: ids,ide,jds,jde,kds,kde
 INTEGER                           :: ims,ime,jms,jme,kms,kme
 INTEGER                           :: its,ite,jts,jte,kts,kte 
 INTEGER                           :: i_parent_start, j_parent_start
 INTEGER                           :: parent_grid_ratio
 INTEGER                           :: n,i,j,ii,jj,nnxp,nnyp
 INTEGER                           :: i_start,j_start,level
 REAL, DIMENSION(1,1), TARGET      :: nothing
 REAL                              :: lah_nest_11, loh_nest_11
 INTEGER                           :: im_big, jm_big, i_add
 INTEGER                           :: im, jm
 CHARACTER(LEN=6)                  :: nestpath

 integer                           :: input_type
 character(len=128)                :: input_fname
 character (len=32)                :: cname
 integer                           :: ndim
 character (len=3)                 :: memorder
 character (len=32)                :: stagger
 integer, dimension(3)             :: domain_start, domain_end
 integer                           :: wrftype
 character (len=128), dimension(3) :: dimnames

 integer :: istatus
 integer :: handle
 integer :: comm_1, comm_2

 real, allocatable, dimension(:,:,:) :: real_domain

 character (len=10), dimension(24)  :: name = (/ "XLAT_M    ", &
                                                "XLONG_M   ", &
                                                "XLAT_V    ", &
                                                "XLONG_V   ", &
                                                "E         ", &
                                                "F         ", &
                                                "LANDMASK  ", &
                                                "LANDUSEF  ", &
                                                "LU_INDEX  ", &
                                                "HCNVX     ", &
                                                "HSTDV     ", &
                                                "HASYW     ", &
                                                "HASYS     ", &
                                                "HASYSW    ", &
                                                "HASYNW    ", &
                                                "HLENW     ", &
                                                "HLENS     ", &
                                                "HLENSW    ", &
                                                "HLENNW    ", &
                                                "HANIS     ", &
                                                "HSLOP     ", &
                                                "HANGL     ", &
                                                "HZMAX     ", & 
                                                "HGT_M     " /)

 integer, parameter :: IO_BIN=1, IO_NET=2

 integer :: io_form_auxinput2
 integer :: itsok,iteok,jtsok,jteok

 CHARACTER(LEN=512) :: message

 write(message,'("Nest d",I2," entering nest_terrain")') nest%id
 call wrf_debug(1,trim(message))

 call START_TIMING()

 write(message,*)"in NEST_TERRAIN config_flags%io_form_auxinput2 = ", config_flags%io_form_auxinput2
 CALL wrf_debug(2,trim(message))
 write(message,*)"in NEST_TERRAIN config_flags%auxinput2_inname = ", config_flags%auxinput2_inname
 CALL wrf_debug(2,trim(message))

 io_form_auxinput2 = config_flags%io_form_auxinput2
 ! NOTE: input_type (WRFSI vs. WPS) triggers based on auxinput1
 ! (metgrid) because that is the only way we have of differentiating
 ! between WRFSI and WPS
 if (config_flags%auxinput1_inname(1:7) == "met_nmm") then
    input_type = 2
 else
    input_type = 1
 end if

!----------------------------------------------------------------------------------

      IDS = nest%sd31
      IDE = nest%ed31
      JDS = nest%sd32
      JDE = nest%ed32
      KDS = nest%sd33
      KDE = nest%ed33

      IMS = nest%sm31
      IME = nest%em31
      JMS = nest%sm32
      JME = nest%em32
      KMS = nest%sm33
      KME = nest%em33

      ITS = nest%sp31
      ITE = nest%ep31
      JTS = nest%sp32
      JTE = nest%ep32
      KTS = nest%sp33
      KTE = nest%ep33

      i_parent_start = nest%i_parent_start
      j_parent_start = nest%j_parent_start
      parent_grid_ratio = nest%parent_grid_ratio

      NNXP=IDE-1
      NNYP=JDE-1
      im = NNXP
      jm = NNYP

       ! Find nesting depth:
       call find_ijstart_level (nest,i_start,j_start,level)
       write(message,*)" nest%id =", nest%id , " i_start,j_start,level =", i_start,j_start,level
       CALL wrf_debug(2,trim(message))
       if ( level <= 0 ) then
          CALL wrf_error_fatal('this routine NEST_TERRAIN should not be called for top-level domain')
       end if

      ! Monitor process stores high-resolution topography:
      IF ( nest%active_this_task ) THEN
        CALL push_communicators_for_domain(nest%id)
#ifdef DM_PARALLEL
      monitor_only: IF ( wrf_dm_on_monitor() ) THEN
#endif
         call wrf_debug(1,'NEST_TERRAIN MASTER PROCESS')
         call MASTER(IDS,IDE,JDS,JDE)
#ifdef DM_PARALLEL
      ELSE
         call wrf_debug(1,'NEST_TERRAIN CLIENT PROCESS')
         call CLIENT(IDS,IDE,JDS,JDE)
      ENDIF monitor_only
#endif

      if(config_flags%terrain_smoothing==2) then     
         call wrf_debug(1,'Call fast smoother (smooth_terrain)')
         call smooth_terrain(nest,12,12, &
              IDS,IDE,JDS,JDE,KDS,KDE, &
              IMS,IME,JMS,JME,KMS,KME, &
              ITS,ITE,JTS,JTE,KTS,KTE)
      elseif(config_flags%terrain_smoothing==1) then
         continue ! already handled this case in the call to MASTER
      elseif(config_flags%terrain_smoothing==0) then
         call wrf_debug(1,'Terrain smoothing is disabled.')
      else
         write(message,*) 'Invalid option for terrain_smoothing: ',config_flags%terrain_smoothing
         call wrf_error_fatal(message)
      endif

     DO J = jts,jte
        DO I = its,ite
#ifdef IDEAL_NMM_TC
           nest%hres_fis(I,J)= 0.0 ! idealized
#else
           nest%hres_fis(i,j)=9.81*nest%hres_avc(i,j)
#endif
        ENDDO
     ENDDO

     write(message,'("Nest d",I0," nest_terrain")') nest%id
        CALL pop_communicators_for_domain
      ENDIF
     call END_TIMING(trim(message))

#ifdef IDEAL_NMM_TC
           return ! move return after end_timing to prevent cnmax KWON
#endif

CONTAINS
#ifdef DM_PARALLEL
  SUBROUTINE CLIENT(IDS,IDE,JDS,JDE)
    IMPLICIT NONE
    integer, intent(in) :: IDS,IDE,JDS,JDE
    REAL, DIMENSION(1,1) :: avc_nest,lnd_nest

     call wrf_debug(1,'call wrf_global_to_patch_real in nest_terrain')
     call wrf_global_to_patch_real(avc_nest,nest%hres_avc,nest%domdesc,'z','xy', &
                                   ids,   ide-1, jds,   jde-1, 1, 1, &
                                   ims,   ime,   jms,   jme,   1, 1, &
                                   its,   ite,   jts,   jte,   1, 1)
     call wrf_global_to_patch_real(lnd_nest,nest%hres_lnd,nest%domdesc,'z','xy', &
                                   ids,   ide-1, jds,   jde-1, 1, 1, &
                                   ims,   ime,   jms,   jme,   1, 1, &
                                   its,   ite,   jts,   jte,   1, 1)
     call wrf_debug(1,'back from wrf_global_to_patch_real in nest_terrain')


  END SUBROUTINE CLIENT
#endif
  SUBROUTINE MASTER(IDS,IDE,JDS,JDE)
    IMPLICIT NONE
    integer, intent(in) :: IDS,IDE,JDS,JDE
    REAL, DIMENSION(IDS:IDE,JDS:JDE)     :: avc_nest, lnd_nest
    type(nmm_terrain), pointer :: tr

    nullify(tr)
    avc_nest = 0.0
    lnd_nest = 0.0
    
    tr=>terrain_for(level,input_type,io_form_auxinput2)

    ! select subdomain from big fine grid
    i_add = mod(j_start+1,2) 
    DO j=jds,jde
       DO i=ids,ide
          avc_nest(i,j) = tr%avc(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
          lnd_nest(i,j) = tr%lnd(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
       END DO
    END DO

    i=1 ; j=1
    lah_nest_11 = tr%lah(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
    loh_nest_11 = tr%loh(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)

    IF(ABS(lah_nest_11-nest%HLAT(1,1)) .GE. 0.5 .OR.  & 
         ABS(loh_nest_11-nest%HLON(1,1)) .GE. 0.5)THEN 

       WRITE(message,*)'SOME MATCHING TEST i_parent_start, j_parent_start',i_parent_start,j_parent_start
       CALL wrf_message(trim(message))
       CALL wrf_message('WRFSI LAT      COMPUTED LAT')
       WRITE(message,*)lah_nest_11,nest%HLAT(1,1)
       CALL wrf_message(trim(message))
       CALL wrf_message('WRFSI LON      COMPUTED LON')
       WRITE(message,*)loh_nest_11,nest%HLON(1,1)
       CALL wrf_message(trim(message))

       CALL wrf_message('CHECK WRFSI CONFIGURATION AND INPUT HIGH RESOLUTION TOPOGRAPHY AND/OR GRID RATIO') 
#ifndef IDEAL_NMM_TC
       CALL wrf_error_fatal('LATLON MISMATCH: ERROR READING static FILE FOR THE NEST')
#endif
    ENDIF

    if(config_flags%terrain_smoothing==1) then
       call wrf_debug(1,'Call slow smoother (smdhld).')
       call smdhld(ids,ide,jds,jde,avc_nest,lnd_nest,12,12)
    endif

#ifdef DM_PARALLEL
    call wrf_debug(1,'call wrf_global_to_patch_real in nest_terrain')
    call wrf_global_to_patch_real(avc_nest,nest%hres_avc,nest%domdesc,'z','xy', &
                                  ids,   ide-1, jds,   jde-1, 1, 1, &
                                  ims,   ime,   jms,   jme,   1, 1, &
                                  its,   ite,   jts,   jte,   1, 1)
    call wrf_global_to_patch_real(lnd_nest,nest%hres_lnd,nest%domdesc,'z','xy', &
                                  ids,   ide-1, jds,   jde-1, 1, 1, &
                                  ims,   ime,   jms,   jme,   1, 1, &
                                  its,   ite,   jts,   jte,   1, 1)
    call wrf_debug(1,'back from wrf_global_to_patch_real in nest_terrain')
#endif
  END SUBROUTINE MASTER
     
END SUBROUTINE NEST_TERRAIN


!===========================================================================================


SUBROUTINE med_init_domain_constants_nmm ( parent, nest)   !, config_flags)
  ! Driver layer
   USE module_domain
   USE module_configure
   USE module_timing
   USE module_dm, ONLY : intercomm_active

   IMPLICIT NONE
   TYPE(domain) , POINTER                     :: parent, nest, grid
!
!
   INTERFACE
     SUBROUTINE med_initialize_nest_nmm ( grid  &   
!
# include "dummy_new_args.inc"
!
                           )
        USE module_domain
        USE module_configure
        USE module_timing
        IMPLICIT NONE
        TYPE(domain) , POINTER                  :: grid
#include "dummy_new_decl.inc"
     END SUBROUTINE med_initialize_nest_nmm 
   END INTERFACE

!------------------------------------------------------------------------------
!  PURPOSE: 
!         - initialize some data, mainly 2D & 3D nmm arrays  very similar to 
!           those done in ./dyn_nmm/module_initialize_real.f 
!-----------------------------------------------------------------------------
!

   grid => nest

   IF ( intercomm_active( grid%id ) ) THEN
     CALL med_initialize_nest_nmm( grid &   
!
# include "actual_new_args.inc"
!
                       )
   ENDIF

END SUBROUTINE med_init_domain_constants_nmm

SUBROUTINE med_initialize_nest_nmm( grid &
!
# include "dummy_new_args.inc"
!
                           )

 USE module_domain
 USE module_configure
 USE module_timing
 IMPLICIT NONE

! Local domain indices and counters.

 INTEGER :: ids, ide, jds, jde, kds, kde, &
            ims, ime, jms, jme, kms, kme, &
            its, ite, jts, jte, kts, kte, &
            i, j, k, nnxp, nnyp 

 TYPE(domain) , POINTER                     :: grid

! Local data

 LOGICAL, EXTERNAL                   :: wrf_dm_on_monitor
 INTEGER                             :: KHH,KVH,JAM,JA,IHL, IHH, L
 INTEGER                             :: II,JJ,ISRCH,ISUM
 INTEGER, ALLOCATABLE, DIMENSION(:)  :: KHL2,KVL2,KHH2,KVH2,KHLA,KHHA,KVLA,KVHA
 INTEGER,PARAMETER                   :: KNUM=SELECTED_REAL_KIND(13)
!
 REAL(KIND=KNUM)                     :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                     :: STPH,CTPH,TDLM,TDPH,FP,TPH,TLM,TLM0
 REAL                                :: TPH0D,TLM0D,ANBI,TSPH,DTAD,DTCF,DT
 REAL                                :: ACDT,CDDAMP,DXP
 REAL                                :: WBD,SBD,WBI,SBI,EBI
 REAL                                :: DY_NMM0
 REAL                                :: RSNOW,SNOFAC
 REAL, ALLOCATABLE, DIMENSION(:)     :: DXJ,WPDARJ,CPGFUJ,CURVJ,   &
                                        FCPJ,FDIVJ,EMJ,EMTJ,FADJ,  &
                                        HDACJ,DDMPUJ,DDMPVJ
!
 REAL, PARAMETER:: SALP=2.60
 REAL, PARAMETER:: SNUP=0.040
 REAL, PARAMETER:: W_NMM=0.08
! REAL, PARAMETER:: COAC=0.75
! REAL, PARAMETER:: CODAMP=6.4
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
 REAL, PARAMETER:: DTR=0.01745329

 REAL :: COAC
 REAL :: CODAMP

 CHARACTER(LEN=255) :: message

   !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"

!#define COPY_IN
!#include "scalar_derefs.inc"
#ifdef DM_PARALLEL
#      include "data_calls.inc"
#endif

   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             its, ite, jts, jte, kts, kte     )


!=================================================================================
!
!

   call nl_get_coac(grid%id,coac)
   call nl_get_codamp(grid%id,codamp)

    DT=grid%dt         !float(TIME_STEP)/parent_time_step_ratio
    NNXP=min(ITE,IDE-1)
    NNYP=min(JTE,JDE-1)
    JAM=6+2*((JDE-1)-10)         ! this should be the fix instead of JAM=6+2*(NNYP-10)

    WRITE(message,*)'TIME STEP ON DOMAIN',grid%id,'==',dt
    CALL wrf_message(trim(message))

    WRITE(message,*)'IDS,IDE ON DOMAIN',grid%id,'==',ids,ide
    CALL wrf_message(trim(message))
!
IF ( grid%active_this_task ) THEN
    ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
    ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
    ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),FADJ(JTS:NNYP))
    ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
    ALLOCATE(KHLA(JAM),KHHA(JAM))
    ALLOCATE(KVLA(JAM),KVHA(JAM))

!   INITIALIZE SOME LAND/WATER SURFACE DATA ON THE BASIS OF INPUTS: SM, XICE, WEASD, 
!   INTERPOLATED FROM MOTHER (WRFSI) DOMAIN. THIS PART OF THE CODE HAS TO BE REVISITED
!   LATER ON

!   Since SM has been changed on parent domain to be 0 over sea ice it can not be used here
!   to find where sea ice is. That's why alogirthm here is slightly different than the
!   one used in module_initalize_real.f

#if ( HWRF == 1 )
!zhang's doing: added to AVOID THIS COMPUTATION IF THE NEST IS STARTED USING ANALYSIS FILE
   IF(.not. grid%analysis)THEN
#endif
    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)

      IF (grid%sm(I,J).GT.0.9) THEN                              ! OVER WATER SURFACE
         grid%epsr(I,J)= 0.97
         grid%embck(I,J)= 0.97
         grid%gffc(I,J)= 0.
         grid%albedo(I,J)=.06
         grid%albase(I,J)=.06
      ENDIF

      IF (grid%sice(I,J).GT.0.9) THEN                            ! OVER SEA-ICE
         grid%sm(I,J)=0.
         grid%si(I,J)=0.
         grid%gffc(I,J)=0.
         grid%albedo(I,J)=.60
         grid%albase(I,J)=.60
      ENDIF

      IF (grid%sm(I,J).LT.0.5.AND.grid%sice(I,J).LT.0.5) THEN         ! OVER LAND SURFACE
           grid%si(I,J)=5.0*grid%weasd(I,J)/1000. ! SNOW WATER EQ (mm) OBTAINED FROM PARENT (grid%si) IS INTERPOLATED
           grid%epsr(I,J)=1.0                ! EMISSIVITY DEFINED OVER LAND IN THE NESTED DOMAIN
           grid%embck(I,J)=1.0               ! EMISSIVITY DEFINED OVER LAND IN THE NESTED DOMAIN
           grid%gffc(I,J)=0.0                ! just leave zero as irrelevant
           grid%sno(I,J)=grid%si(I,J)*.20         ! LAND-SNOW COVER
      ENDIF

     ENDDO
    ENDDO

!   This may just be a fix and may need some Registry related changes, later on

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
         grid%vegfra(I,J)=grid%vegfrc(I,J)
     ENDDO
    ENDDO

!   DETERMINE ALBEDO OVER LAND ON THE BASIS OF INPUTS: SM, ALBASE, MXSNAL & VEGFRA
!   INTERPOLATED FROM MOTHER (WRFSI) DOMAIN

 
    DO J = JTS, MIN(JTE,JDE-1) 
     DO I = ITS, MIN(ITE,IDE-1)

          IF(grid%sm(I,J).LT.0.9.AND.grid%sice(I,J).LT.0.9) THEN
!
            IF ( (grid%sno(I,J) .EQ. 0.0) .OR. &            ! SNOWFREE ALBEDO
                           (grid%albase(I,J) .GE. grid%mxsnal(I,J) ) ) THEN
              grid%albedo(I,J) = grid%albase(I,J)
            ELSE
              IF (grid%sno(I,J) .LT. SNUP) THEN             ! MODIFY ALBEDO IF SNOWCOVER:
                  RSNOW = grid%sno(I,J)/SNUP                ! BELOW SNOWDEPTH THRESHOLD
                  SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
              ELSE
                  SNOFAC = 1.0                         ! ABOVE SNOWDEPTH THRESHOLD
              ENDIF
              grid%albedo(I,J) = grid%albase(I,J) &
                          + (1.0-grid%vegfra(I,J))*SNOFAC*(grid%mxsnal(I,J)-grid%albase(I,J))
            ENDIF
!
          END IF

          grid%si(I,J)=5.0*grid%weasd(I,J)
          grid%sno(I,J)=grid%weasd(I,J)
! this block probably superfluous.  Meant to guarantee land/sea agreement

        IF (grid%sm(I,J) .gt. 0.5)THEN 
           grid%landmask(I,J)=0.0
        ELSE 
           grid%landmask(I,J)=1.0
        ENDIF 

        IF (grid%sice(I,J) .eq. 1.0) then !!!! change vegtyp and sltyp to fit seaice (desireable??)
         grid%isltyp(I,J)=16
         grid%ivgtyp(I,J)=24
        ENDIF 

     ENDDO
    ENDDO

!  Check land water interface

!     The write(20,*) statements below were very slow on Jet when using
!     the intel compiler (added hours to the runtime).  The fix
!     suggested by Chris Harrop was to send messages to wrf_message
!     instead:

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS,MIN(ITE,IDE-1)
      IF(grid%sm(I,J).GT.0.9 .AND. grid%vegfra(I,J) .NE. 0) THEN
#if defined(USE_SLOW_INTERFACE_CHECK)
        WRITE(20,*)'PROBLEM AT THE LAND-WATER INTERFACE (VEGFRA):', &
             I,J,grid%sm(I-1,J),grid%vegfra(I-1,j),grid%sm(I,J),grid%vegfra(I,J)
#else
        WRITE(message,*)'PROBLEM AT THE LAND-WATER INTERFACE (VEGFRA):', &
             I,J,grid%sm(I-1,J),grid%vegfra(I-1,j),grid%sm(I,J),grid%vegfra(I,J)
        CALL wrf_message(trim(message))
#endif
      ENDIF
!
#if ( HWRF == 1 )
      ! HWRF should not perform the check below because the nmm_tsk is
      ! update with the correct skin temperature every timestep (on both
      ! land and sea points).
#else
      IF(grid%sm(I,J).GT.0.9 .AND. grid%nmm_tsk(I,J) .NE. 0) THEN
#if defined(USE_SLOW_INTERFACE_CHECK)
        WRITE(20,*)'PROBLEM AT THE LAND-WATER INTERFACE (NMM_TSK):', &
             I,J,grid%sm(I-1,J),grid%nmm_tsk(I-1,J),grid%sm(I,J),grid%nmm_tsk(I,J)
#else
        WRITE(message,*)'PROBLEM AT THE LAND-WATER INTERFACE (NMM_TSK):', &
             I,J,grid%sm(I-1,J),grid%nmm_tsk(I-1,J),grid%sm(I,J),grid%nmm_tsk(I,J)
        CALL wrf_message(trim(message))
#endif
      ENDIF
#endif
     ENDDO
    ENDDO


!   hardwire root depth for time being

        grid%rtdpth=0.
        grid%rtdpth(1)=0.1
        grid%rtdpth(2)=0.3
        grid%rtdpth(3)=0.6

!   hardwire soil depth for time being

        grid%sldpth=0.
        grid%sldpth(1)=0.1
        grid%sldpth(2)=0.3
        grid%sldpth(3)=0.6
        grid%sldpth(4)=1.0

#if ( HWRF == 1 )
!zhang's doing: added to AVOID THIS COMPUTATION IF THE NEST IS STARTED USING ANALYSIS FILE
   ENDIF ! <------ for analysis set to false
#endif 
!-----------  END OF LAND SURFACE INITIALIZATION -------------------------------------
!
    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
       grid%res(I,J)=1.
     ENDDO
    ENDDO

!   INITIALIZE 2D BOUNDARY MASKS

!! grid%hbm2:
 
    grid%hbm2=0.
    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       IF((J .GE. 3 .and. J .LE. (JDE-1)-2) .AND.   &
          (I .GE. 2 .and. I .LE. (IDE-1)-2+MOD(J,2))) THEN
          grid%hbm2(I,J)=1.
        ENDIF
      ENDDO
    ENDDO   

!! grid%hbm3:

    grid%hbm3=0.
    DO J=JTS,MIN(JTE,JDE-1)
     grid%ihwg(J)=mod(J+1,2)-1
      IF (J .ge. 4 .and. J .le. (JDE-1)-3) THEN
        IHL=(IDS+1)-grid%ihwg(J) 
        IHH=(IDE-1)-2
        DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IHL  .and. I .le. IHH) grid%hbm3(I,J)=1.
        ENDDO 
      ENDIF
    ENDDO 
   
!! grid%vbm2

    grid%vbm2=0.
    DO J=JTS,MIN(JTE,JDE-1)
     DO I=ITS,MIN(ITE,IDE-1)
       IF((J .ge. 3 .and. J .le. (JDE-1)-2)  .AND.  &
          (I .ge. 2 .and. I .le. (IDE-1)-1-MOD(J,2))) THEN
           grid%vbm2(I,J)=1.
       ENDIF
     ENDDO
    ENDDO

!! grid%vbm3

    grid%vbm3=0.
    DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
        IF((J .ge. 4 .and. J .le. (JDE-1)-3)  .AND.  &
           (I .ge. 3-MOD(J,2) .and. I .le. (IDE-1)-2)) THEN
           grid%vbm3(I,J)=1.
        ENDIF
      ENDDO
    ENDDO

    TPH0D  = grid%CEN_LAT
    TLM0D  = grid%CEN_LON
    TPH0   = TPH0D*DTR
    WBD    = grid%WBD0   ! gopal's doing: may use Registry WBD0 now
    WB     = WBD*DTR
    SBD    = grid%SBD0   ! gopal's doing: may use Registry SBD0 now
    SB     = SBD*DTR
    DLM    = grid%dlmd*DTR  ! input now from med_nest_egrid_configure
    DPH    = grid%dphd*DTR  ! input now from med_nest_egrid_configure
    TDLM   = DLM+DLM
    TDPH   = DPH+DPH
    WBI    = WB+TDLM
    SBI    = SB+TDPH
    EBI    = WB+((ide-1)-2)*TDLM  ! gopal's doing: check this for nested domain
    ANBI   = SB+((jde-1)-3)*DPH   ! gopal's doing: check this for nested domain
    STPH0  = SIN(TPH0)
    CTPH0  = COS(TPH0)
    TSPH   = 3600./grid%DT
    DTAD   = 1.0
    DTCF   = 4.0    
    DY_NMM0= grid%dy_nmm ! ERAD*DPH; input now from med_nest_egrid_configure

!   CORIOLIS PARAMETER  (There appears to be some roundoff in computing TLM & STPH and other terms,
!   in the nested domain. The problem needs to be revisited

    DO J=JTS,MIN(JTE,JDE-1)
      TLM0=WB-TDLM+MOD(J,2)*DLM           ! remember this is a wind point
      TPH =SB+float(J-1)*DPH
      STPH=SIN(TPH)
      CTPH=COS(TPH)
      DO I=ITS,MIN(ITE,IDE-1)
         TLM=TLM0 + I*TDLM
         FP=TWOM*(CTPH0*STPH+STPH0*CTPH*COS(TLM))
         grid%f(I,J)=0.5*grid%DT*FP
      ENDDO
    ENDDO


    DO J=JTS,MIN(JTE,JDE-1)
      KHL2(J)=(IDE-1)*(J-1)-(J-1)/2+2
      KVL2(J)=(IDE-1)*(J-1)-J/2+2
      KHH2(J)=(IDE-1)*J-J/2-1
      KVH2(J)=(IDE-1)*J-(J+1)/2-1
    ENDDO


    TPH=SB-DPH
    DO J=JTS,MIN(JTE,JDE-1)
      TPH=SB+float(J-1)*DPH
      DXP=ERAD*DLM*COS(TPH)
      DXJ(J)=DXP
      WPDARJ(J)=-W_NMM*((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM0**2)/  & 
                (grid%DT*32.*DXP*DY_NMM0)
      CPGFUJ(J)=-grid%DT/(48.*DXP)
      CURVJ(J)=.5*grid%DT*TAN(TPH)/ERAD
      FCPJ(J)=grid%DT/(CP*192.*DXP*DY_NMM0)
      FDIVJ(J)=1./(12.*DXP*DY_NMM0)
      FADJ(J)=-grid%DT/(48.*DXP*DY_NMM0)*DTAD
      ACDT=grid%DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM0**2)
      CDDAMP=CODAMP*ACDT
      HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM0)
      DDMPUJ(J)=CDDAMP/DXP
      DDMPVJ(J)=CDDAMP/DY_NMM0
    ENDDO

! --------------DERIVED VERTICAL GRID CONSTANTS--------------------------

     WRITE(message,*)'NEW CHANGE',grid%f4d,grid%ef4t,grid%f4q
     CALL wrf_message(trim(message))

      DO L=KDS,KDE-1
        grid%rdeta(L)=1./grid%deta(L)
        grid%f4q2(L)=-.25*grid%DT*DTAD/grid%deta(L)
      ENDDO

       DO J=JTS,MIN(JTE,JDE-1)
        DO I=ITS,MIN(ITE,IDE-1)
          grid%dx_nmm(I,J)=DXJ(J)
          grid%wpdar(I,J)=WPDARJ(J)*grid%hbm2(I,J)
          grid%cpgfu(I,J)=CPGFUJ(J)*grid%vbm2(I,J)
          grid%curv(I,J)=CURVJ(J)*grid%vbm2(I,J)
          grid%fcp(I,J)=FCPJ(J)*grid%hbm2(I,J)
          grid%fdiv(I,J)=FDIVJ(J)*grid%hbm2(I,J)
          grid%fad(I,J)=FADJ(J)
          grid%hdacv(I,J)=HDACJ(J)*grid%vbm2(I,J)
          grid%hdac(I,J)=HDACJ(J)*1.25*grid%hbm2(I,J)
        ENDDO
       ENDDO

       DO J=JTS, MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN
          KHH=(IDE-1)-2+MOD(J,2) ! KHH is global...loop over I that have
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
          KHH=(IDE-1)-2+MOD(J,2)

          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. (IDE-1)-2 .and. I .le. KHH) THEN
               grid%hdac(I,J)=grid%hdac(I,J)* DFC
             ENDIF
          ENDDO
        ENDIF
      ENDDO

      DO J=JTS,min(JTE,JDE-1)
      DO I=ITS,min(ITE,IDE-1)
        grid%ddmpu(I,J)=DDMPUJ(J)*grid%vbm2(I,J)
        grid%ddmpv(I,J)=DDMPVJ(J)*grid%vbm2(I,J)
        grid%hdacv(I,J)=grid%hdacv(I,J)*grid%vbm2(I,J)
      ENDDO
      ENDDO

! --------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------

        DO J=JTS,MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
             grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
             grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)*DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO

! This one was left over for nested domain
 
     DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          grid%GLAT(I,J)=grid%HLAT(I,J)*DTR
          grid%GLON(I,J)=grid%HLON(I,J)*DTR
       ENDDO
     ENDDO

!!   compute EMT, EM on global domain, and only on task 0.

!    IF (wrf_dm_on_monitor()) THEN   !!!! NECESSARY TO LIMIT THIS TO TASK ZERO?
      
     ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))
     DO J=JDS,JDE-1
       TPH=SB+float(J-1)*DPH
       DXP=ERAD*DLM*COS(TPH)
       EMJ(J)= grid%DT/( 4.*DXP)*DTAD
       EMTJ(J)=grid%DT/(16.*DXP)*DTAD
     ENDDO

          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 161      grid%emt(JA)=EMTJ(J)
          DO 162 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 162      grid%emt(JA)=EMTJ(J)
          DO 163 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=2+MOD(J,2)
 163      grid%emt(JA)=EMTJ(J)
          DO 164 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=(IDE-1)-2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 164      grid%emt(JA)=EMTJ(J)

! --------------SPREADING OF UPSTREAM VELOCITY-POINT ADVECTION FACTOR----

          JA=0
          DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 171      grid%em(JA)=EMJ(J)
          DO 172 J=(JDE-1)-4,(JDE-2)-2
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 172      grid%em(JA)=EMJ(J)
          DO 173 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=2+MOD(J+1,2)
 173      grid%em(JA)=EMJ(J)
          DO 174 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=(IDE-1)-2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 174      grid%em(JA)=EMJ(J)

!        ENDIF ! wrf_dm_on_monitor

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)
!!
                                                                !   phantom
        IF (ABS(IDE-1-ITE) .eq. 1 ) THEN                        !      | 
         CALL wrf_message('zero phantom winds')                 !  H  [x]    H    V
         DO K=KDS,KDE-1                                         !  
          DO J=JDS,JDE-1,2                                      !  V  [H]    V    H
           IF (J .ge. JTS .and. J .le. JTE) THEN                !
             grid%u(IDE-1,J,K)=0.                                    !  H  [x]    H    V
             grid%v(IDE-1,J,K)=0.                                    !  ------    ------  
           ENDIF                                                !   ide-1      ide
          ENDDO                                                 !   NMM/si     WRF
         ENDDO                                                  !   domain    domain
        ENDIF                                                   !             (dummy)   


! just a test for gravity waves

!  PD=62000.
!   grid%u=0.0
!   grid%v=0.0
!   T=300.
!   Q=0.0
!   Q2=0.0
!   CWM=0.0
!   FIS=0.0

! testx
!  DO J = JTS, MIN(JTE,JDE-1)
!   DO K = KTS,KTE
!    DO I = ITS, MIN(ITE,IDE-1)
!      grid%sm(I,J)=I
!       grid%u(I,K,J)=J
!    ENDDO
!   ENDDO
!  ENDDO
!

!   deallocs

    DEALLOCATE(KHL2,KVL2,KHH2,KVH2)
    DEALLOCATE(DXJ,WPDARJ,CPGFUJ,CURVJ)
    DEALLOCATE(FCPJ,FDIVJ,FADJ)
    DEALLOCATE(HDACJ,DDMPUJ,DDMPVJ)
    DEALLOCATE(KHLA,KHHA)
    DEALLOCATE(KVLA,KVHA)
ENDIF


END SUBROUTINE med_initialize_nest_nmm
!======================================================================

!--------------------------------------------------------------------------------------
#if 0
SUBROUTINE initial_nest_pivot ( parent , nest, iloc, jloc )

!==========================================================================================
!
! This program produces i_start and j_start for the nested domain depending on the
! central lat-lon of the storm.
!
!==========================================================================================

 USE module_domain
 USE module_configure
 USE module_timing
 USE module_dm 

 IMPLICIT NONE
 TYPE(domain) , POINTER              :: parent , nest
 INTEGER, INTENT(OUT)                :: ILOC,JLOC
 INTEGER                             :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                             :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                             :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                             :: ITS,ITE,JTS,JTE,KTS,KTE
 INTEGER                             :: NIDE,NJDE              ! nest dimension
 INTEGER                             :: I,J,ITER,IDUM,JDUM
 REAL                                :: ALAT,ALON,DIFF1,DIFF2,ERR
 REAL                                :: parent_CLAT,parent_CLON,parent_SLAT,parent_SLON
 REAL                                :: parent_WBD,parent_SBD,parent_DLMD,parent_DPHD 
!========================================================================================

!   First obtain central latitude and longitude for the parent domain

    CALL nl_get_cen_lat (parent%ID, parent_CLAT)
    CALL nl_get_cen_lon (parent%ID, parent_CLON)
!    CALL nl_get_storm_lat (parent%ID, parent_SLAT)
!    CALL nl_get_storm_lon (parent%ID, parent_SLON)

!   Parent grid configuration, including, western and southern boundary

    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    JTS  = parent%sp32
    JTE  = parent%ep32
    KTS  = parent%sp33
    KTE  = parent%ep33

    NIDE = nest%ed31
    NJDE = nest%ed32

    parent_DLMD = parent%dx          ! DLMD: dlamda in degrees
    parent_DPHD = parent%dy          ! DPHD: dphi in degrees
    parent_WBD  = -(IDE-2)*parent%dx ! WBD0: in deg;factor 2 takes care of dummy last column 
    parent_SBD  = -((JDE-1)/2)*parent%dy ! SBD0: in degrees; note that JDE-1 should be odd 
    ALAT  = parent_SLAT - 0.5*(NJDE-2)*parent_DPHD/nest%parent_grid_ratio
    ALON  = parent_SLON - 1.0*(NIDE-2)*parent_DLMD/nest%parent_grid_ratio

    CALL EARTH_LATLON ( parent%HLAT,parent%HLON,parent%VLAT,parent%VLON, & !output
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                   & !inputs
                        parent_CLAT,parent_CLON,                                         &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                         &
                        IMS,IME,JMS,JME,KMS,KME,                                         &
                        ITS,ITE,JTS,JTE,KTS,KTE                          )

!   start iteration

      ILOC=-99
      JLOC=-99
      ERR=0.1
      ITER=1
100   CONTINUE

     DO J = JTS,min(JTE,JDE-1)
      DO I = ITS,min(ITE,IDE-1)
        DIFF1 = ABS(ALAT - parent%HLAT(I,J))
        DIFF2 = ABS(ALON - parent%HLON(I,J))
        IF(DIFF1 .LE. ERR .AND. DIFF2 .LE. ERR)THEN
         ILOC=I
         JLOC=J
        ENDIF
      ENDDO
     ENDDO

     CALL wrf_dm_maxval_integer ( ILOC, idum, jdum )
     CALL wrf_dm_maxval_integer ( JLOC, idum, jdum ) 

     IF(ILOC .EQ. -99 .AND. JLOC .EQ. -99)THEN
       ERR=ERR+0.1
       ITER=ITER+1
       IF(ITER .LE. 100)GO TO 100
     ENDIF

     IF(ILOC .NE. -99 .AND. JLOC .NE. -99)THEN
       WRITE(message,*)'NOTE: I_PARENT_START AND J_PARENT_START FOUND FOR THE NESTED DOMAIN CONFIGURATION AT ITER=',ITER
       CALL wrf_message(trim(message))
       WRITE(message,*)'istart=',ILOC
       CALL wrf_message(trim(message))
       WRITE(message,*)'jstart=',JLOC
       CALL wrf_message(trim(message))
     ELSE
       ILOC=IDE/3
       JLOC=JDE/3
!
       WRITE(message,*)'WARNING: COULD NOT LOCATE I_PARENT_START AND J_PARENT_START FROM INPUT STORM INFO'
       CALL wrf_message(trim(message))
       WRITE(message,*)'ISTART=',IDE/3
       CALL wrf_message(trim(message))
       WRITE(message,*)'JSTART=',JDE/3
       CALL wrf_message(trim(message))
     ENDIF

     RETURN
END SUBROUTINE initial_nest_pivot

!============================================================================================
#endif

LOGICAL FUNCTION cd_feedback_mask_orig( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask_orig = ( pig .ge. ips_save+2 .and. &
                            pjg .ge. jps_save+3 .and. &
                            pig .le. ipe_save-2-mod(pjg-jps_save,2) .and. &
                            pjg .le. jpe_save-3 )

END FUNCTION cd_feedback_mask_orig

LOGICAL FUNCTION cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask = ( pig .ge. ips_save+1 .and. &
                            pjg .ge. jps_save+2 .and. &
                            pig .le. ipe_save-1-mod(pjg-jps_save,2) .and. &
                            pjg .le. jpe_save-2 )

END FUNCTION cd_feedback_mask

LOGICAL FUNCTION cd_feedback_mask_v( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1
   
   cd_feedback_mask_v = ( pig .ge. ips_save+2 .and. &
                            pjg .ge. jps_save+3 .and. &
                            pig .le. ipe_save-2-mod(pjg-jps_save+1,2) .and. &
                            pjg .le. jpe_save-3 )

END FUNCTION cd_feedback_mask_v


!----------------------------------------------------------------------------
#else
SUBROUTINE stub_nmm_nest_stub
END SUBROUTINE stub_nmm_nest_stub
#endif

RECURSIVE SUBROUTINE find_ijstart_level ( grid, i_start, j_start, level )

! Dusan Jovic

   USE module_domain

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) :: grid
   INTEGER, INTENT (OUT) :: i_start, j_start, level
   INTEGER :: iadd

      if (grid%parent_id == 0 ) then
         i_start = 1
         j_start = 1
         level = 0
      else
         call find_ijstart_level ( grid%parents(1)%ptr, i_start, j_start, level )
         if (level > 0) then
             iadd = (i_start-1)*3
             if ( mod(j_start,2).ne.0 .and. mod(grid%j_parent_start,2).ne.0 ) iadd = iadd - 1
             if ( mod(j_start,2).eq.0 .and. mod(grid%j_parent_start,2).eq.0 ) iadd = iadd + 2
         else
             iadd = -mod(grid%j_parent_start,2)
         end if
         i_start = iadd + grid%i_parent_start*3 - 1
         j_start = ( (j_start-1) + (grid%j_parent_start-1) ) * 3 + 1
         level = level + 1
      end if

END SUBROUTINE find_ijstart_level
