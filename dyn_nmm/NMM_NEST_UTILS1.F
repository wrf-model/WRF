#if (NMM_NEST == 1)
!===========================================================================
!
! E-GRID NESTING UTILITIES: This is gopal's doing
!
!===========================================================================

SUBROUTINE med_nest_egrid_configure ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
 REAL, PARAMETER                    :: ERAD=6371200.
 REAL, PARAMETER                    :: DTR=0.01745329
 REAL, PARAMETER                    :: DTAD=1.0
 REAL, PARAMETER                    :: CP=1004.6
 INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE

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

    IF(MOD(parent%ed33,2) .NE. 0)THEN
     CALL wrf_error_fatal("PARENT DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF

!   BASIC TEST FOR NESTED DOMAIN: CHECK IF JMAX IS ODD. SINCE JDE IN THE NAMELIST
!   IS JMAX + 1, WE NEED TO CHECK IF JDE IS EVEN IN WRF CONTEXT

    IF(MOD(nest%ed33,2) .NE. 0)THEN
     CALL wrf_error_fatal("NESTED DOMAIN: JMAX IS EVEN, INCREASE e_sn IN THE namelist.input BY 1")
    ENDIF

!   Parent grid configuration, including, western and southern boundary

    IDS = parent%sd31
    IDE = parent%ed31
    KDS = parent%sd32
    KDE = parent%ed32
    JDS = parent%sd33
    JDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    KMS = parent%sm32
    KME = parent%em32
    JMS = parent%sm33
    JME = parent%em33

    ITS = parent%sp31
    ITE = parent%ep31
    KTS = parent%sp32
    KTE = parent%ep32
    JTS = parent%sp33
    JTE = parent%ep33

!   grid configuration

    ! calculate wbd0 and sbd0 only for MOAD i.e. grid with parent_id == 0
    if (parent%parent_id == 0 ) then        ! Dusan's doing
       parent%wbd0 = -(IDE-2)*parent%dx     ! WBD0: in degrees;factor 2 takes care of dummy last column
       parent%sbd0 = -((JDE-1)/2)*parent%dy ! SBD0: in degrees; note that JDE-1 should be odd  
    end if
    nest%wbd0   = parent%wbd0 + (nest%i_parent_start-1)*2.*parent%dx + mod(nest%j_parent_start+1,2)*parent%dx
    nest%sbd0   = parent%sbd0 + (nest%j_parent_start-1)*parent%dy
    nest%dx     = parent%dx/nest%parent_grid_ratio
    nest%dy     = parent%dy/nest%parent_grid_ratio

    write(0,*)" - i_parent_start = ",nest%i_parent_start
    write(0,*)" - j_parent_start = ",nest%j_parent_start
    write(0,*)" - parent%wbd0    = ",parent%wbd0
    write(0,*)" - parent%sbd0    = ",parent%sbd0
    write(0,*)" - nest%wbd0      = ",nest%wbd0
    write(0,*)" - nest%sbd0      = ",nest%sbd0
    write(0,*)" - nest%dx        = ",nest%dx
    write(0,*)" - nest%dy        = ",nest%dy
!
    CALL nl_set_dx (nest%id , nest%dx)   ! for output purpose
    CALL nl_set_dy (nest%id , nest%dy)   ! for output purpose

!   set lat-lons; parent set to nested domain

    CALL nl_get_cen_lat (parent%id, parent%cen_lat) ! cen_lat of parent set to nested domain
    CALL nl_get_cen_lon (parent%id, parent%cen_lon) ! cen_lon of parent set to nested domain
    nest%cen_lat=parent%cen_lat
    nest%cen_lon=parent%cen_lon
!
    CALL nl_set_cen_lat ( nest%id , nest%cen_lat)  ! for output purpose
    CALL nl_set_cen_lon ( nest%id , nest%cen_lon)  ! for output purpose

    write(0,*)" - nest%cen_lat   = ",nest%cen_lat
    write(0,*)" - nest%cen_lon   = ",nest%cen_lon


!   soil configuration

    nest%nmm_sldpth  = parent%nmm_sldpth
    nest%nmm_dzsoil  = parent%nmm_dzsoil
    nest%nmm_rtdpth  = parent%nmm_rtdpth

!   numerical set up

    nest%nmm_deta        = parent%nmm_deta
    nest%nmm_aeta        = parent%nmm_aeta
    nest%nmm_etax        = parent%nmm_etax
    nest%nmm_dfl         = parent%nmm_dfl
    nest%nmm_deta1       = parent%nmm_deta1
    nest%nmm_aeta1       = parent%nmm_aeta1
    nest%nmm_eta1        = parent%nmm_eta1
    nest%nmm_deta2       = parent%nmm_deta2
    nest%nmm_aeta2       = parent%nmm_aeta2
    nest%nmm_eta2        = parent%nmm_eta2
    nest%nmm_pdtop       = parent%nmm_pdtop
    nest%nmm_pt          = parent%nmm_pt
    nest%nmm_dfrlg       = parent%nmm_dfrlg
    nest%num_soil_layers = parent%num_soil_layers
    nest%num_moves       = parent%num_moves

! Unfortunately, some of the single value constants in used in module_initialize have 
! to be defiend here instead of the usual spot in med_initialize_nest_nmm. There
! appears to be a problem in Registry and related code in this area.
!
! state  logical upstrm   -      dyn_nmm     -      -      -


    nest%nmm_dlmd   = nest%dx
    nest%nmm_dphd   = nest%dy
    nest%nmm_dy_nmm = erad*(nest%nmm_dphd*dtr)
    nest%nmm_cpgfv  = -nest%dt/(48.*nest%nmm_dy_nmm)
    nest%nmm_en     = nest%dt/( 4.*nest%nmm_dy_nmm)*dtad
    nest%nmm_ent    = nest%dt/(16.*nest%nmm_dy_nmm)*dtad
    nest%nmm_f4d    = -.5*nest%dt*dtad
    nest%nmm_f4q    = -nest%dt*dtad
    nest%nmm_ef4t   = .5*nest%dt/cp

!  Other output configurations that will make grads happy 

   CALL nl_get_truelat1 (parent%id, parent%truelat1 )
   CALL nl_get_truelat2 (parent%id, parent%truelat2 )
   CALL nl_get_map_proj (parent%id, parent%map_proj )
   CALL nl_get_iswater (parent%id, parent%iswater )

   nest%truelat1=parent%truelat1
   nest%truelat2=parent%truelat2
   nest%map_proj=parent%map_proj
   nest%iswater=parent%iswater

   CALL nl_set_truelat1(nest%id, nest%truelat1)
   CALL nl_set_truelat2(nest%id, nest%truelat2)
   CALL nl_set_map_proj(nest%id, nest%map_proj)
   CALL nl_set_iswater(nest%id, nest%iswater)

!   physics and other configurations
!   CALL nl_get_iswater (parent%id, nest%iswater) ! iswater is just based on parents
!   CALL nl_get_bl_surface_physics (nest%id,  nest%bl_surface_physics )
!   CALL nl_get_num_soil_layers( parent%num_soil_layers )
!   CALL nl_get_real_data_init_type (parent%real_data_init_type)

END SUBROUTINE med_nest_egrid_configure

SUBROUTINE med_construct_egrid_weights ( parent , nest )
 USE module_domain
 USE module_configure
 USE module_timing

 IMPLICIT NONE
 TYPE(domain) , POINTER             :: parent , nest
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
    KDS = parent%sd32
    KDE = parent%ed32
    JDS = parent%sd33
    JDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    KMS = parent%sm32
    KME = parent%em32
    JMS = parent%sm33
    JME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    KTS  = parent%sp32
    KTE  = parent%ep32
    JTS  = parent%sp33
    JTE  = parent%ep33
!
    parent_DLMD = parent%dx                ! DLMD: dlamda in degrees 
    parent_DPHD = parent%dy                ! DPHD: dphi in degrees 
    parent_WBD  = parent%wbd0
    parent_SBD  = parent%sbd0

!   Now compute Geodetic lat/lon (Positive East) of parent grid in degrees

    CALL EARTH_LATLON ( parent%nmm_HLAT,parent%nmm_HLON,parent%nmm_VLAT,parent%nmm_VLON,  & !output
                        parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,                    & !inputs 
                        parent_CLAT,parent_CLON,                                          &
                        IDS,IDE,JDS,JDE,KDS,KDE,                                          &
                        IMS,IME,JMS,JME,KMS,KME,                                          &
                        ITS,ITE,JTS,JTE,KTS,KTE                                           )

!   Nested grid configuration, including, western and southern boundary

    IDS = nest%sd31
    IDE = nest%ed31
    KDS = nest%sd32
    KDE = nest%ed32
    JDS = nest%sd33
    JDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    KMS = nest%sm32
    KME = nest%em32
    JMS = nest%sm33
    JME = nest%em33

    ITS  = nest%sp31
    ITE  = nest%ep31
    KTS  = nest%sp32
    KTE  = nest%ep32
    JTS  = nest%sp33
    JTE  = nest%ep33
!
    nest_DLMD = nest%dx
    nest_DPHD = nest%dy
    nest_WBD  = nest%wbd0
    nest_SBD  = nest%sbd0

!
!   Now compute Geodetic lat/lon (Positive East) of nest in degrees, with the same central lat-lon
!   as the parent grid
!

    CALL EARTH_LATLON ( nest%nmm_HLAT,nest%nmm_HLON,nest%nmm_VLAT,nest%nmm_VLON, & ! output
                        nest_DLMD,nest_DPHD,nest_WBD,nest_SBD,                   & ! nest inputs
                        parent_CLAT,parent_CLON,                                 & ! parent central lat/lon
                        IDS,IDE,JDS,JDE,KDS,KDE,                                 & ! nested domain dimension
                        IMS,IME,JMS,JME,KMS,KME,                                 &
                        ITS,ITE,JTS,JTE,KTS,KTE                                  )

!   Determine the weights of nested grid h points nearest to H points of parent domain 

    CALL G2T2H( nest%nmm_IIH,nest%nmm_JJH,                       & ! output grid index on nested grid
                nest%nmm_HBWGT1,nest%nmm_HBWGT2,                 & ! output weights on the nested grid 
                nest%nmm_HBWGT3,nest%nmm_HBWGT4,                 &
                nest%nmm_HLAT,nest%nmm_HLON,                     & ! target (nest) input lat lon in degrees
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & ! parent res, western and south boundaries
                parent_CLAT,parent_CLON,                         & ! parent central lat,lon, all in degrees
                parent%ed31,parent%ed33,                         & ! parent imax and jmax
                IDS,IDE,JDS,JDE,KDS,KDE,                         & ! 
                IMS,IME,JMS,JME,KMS,KME,                         & ! nested grid configuration
                ITS,ITE,JTS,JTE,KTS,KTE                          ) !


!   Determine the weights of nested grid v points nearest to V points of parent domain

    CALL G2T2V( nest%nmm_IIV,nest%nmm_JJV,                       & ! output grid index on nested grid
                nest%nmm_VBWGT1,nest%nmm_VBWGT2,                 & ! output weights on the nested grid
                nest%nmm_VBWGT3,nest%nmm_VBWGT4,                 &
                nest%nmm_VLAT,nest%nmm_VLON,                     & ! target (nest) input lat lon in degrees
                parent_DLMD,parent_DPHD,parent_WBD,parent_SBD,   & ! parent res, western and south boundaries
                parent_CLAT,parent_CLON,                         & ! parent central lat,lon, all in degrees
                parent%ed31,parent%ed33,                         & ! parent imax and jmax
                IDS,IDE,JDS,JDE,KDS,KDE,                         & !
                IMS,IME,JMS,JME,KMS,KME,                         & ! nested grid configuration
                ITS,ITE,JTS,JTE,KTS,KTE                          ) !

!*** CHECK WEIGHTS AT MASS AND VELOCITY POINTS

     CALL WEIGTS_CHECK(nest%nmm_HBWGT1,nest%nmm_HBWGT2,          & ! output weights on the nested grid
                       nest%nmm_HBWGT3,nest%nmm_HBWGT4,          &
                       nest%nmm_VBWGT1,nest%nmm_VBWGT2,          & ! output weights on the nested grid
                       nest%nmm_VBWGT3,nest%nmm_VBWGT4,          &
                       IDS,IDE,JDS,JDE,KDS,KDE,                  & !
                       IMS,IME,JMS,JME,KMS,KME,                  & ! nested grid configuration
                       ITS,ITE,JTS,JTE,KTS,KTE                   )

!*** CHECK DOMAIN BOUNDS BEFORE PROCEEDING TO INTERPOLATION
!
    CALL BOUNDS_CHECK( nest%nmm_IIH,nest%nmm_JJH,nest%nmm_IIV,nest%nmm_JJV,   &
                       nest%i_parent_start,nest%j_parent_start,nest%shw,      &
                       IDS,IDE,JDS,JDE,KDS,KDE,                               & !
                       IMS,IME,JMS,JME,KMS,KME,                               & ! nested grid configuration
                       ITS,ITE,JTS,JTE,KTS,KTE                                )

!------------------------------------------------------------------------------------------

END SUBROUTINE med_construct_egrid_weights 

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
!-------------------------------------------------------------------------

!
      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD1 * DTR                 ! WB:   western boundary in radians
      SB   = SBD1 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD1 * DTR                ! DLM:  dlamda in radians 
      DPH  = DPHD1 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                  ! TDLM: 2.0*dlamda 
      TDPH = DPH + DPH                  ! TDPH: 2.0*DPH 

!     For earth lat lon only

      TPH0  = CENTRAL_LAT*DTR                ! TPH0: central lat in radians 
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

!      WRITE(0,*) 'WB,SB,DLM,DPH,DTR: ',WBD1,SBD1,DLM,DPH,DTR    
!      WRITE(0,*) 'IMS,IME,JMS,JME,KMS,KME',IMS,IME,JMS,JME,KMS,KME 
!      WRITE(0,*) 'IDS,IDE,JDS,JDE,KDS,KDE',IDS,IDE,JDS,JDE,KDS,KDE
!      WRITE(0,*) 'ITS,ITE,JTS,JTE,KTS,KTE',ITS,ITE,JTS,JTE,KTS,KTE 

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
           GLATH(I,J)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians 
           CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
                - TAN(GLATH(I,J))*TAN(TPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CENTRAL_LON*DTR + FACTH*ACOS(CLMH)

         ENDDO                                    

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           GLATV(I,J) = ASIN(SPHV)
           CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
                - TAN(GLATV(I,J))*TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CENTRAL_LON*DTR + FACTV*ACOS(CLMV)

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
!***                 h           
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
!            WRITE(60,*)'NEAREST PARENT IS:','col=',COL,'row=',ROW,'ncol=',NCOL,'nrow=',NROW
      ELSE
!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT h IS AT
!***
!***              H      V
!***
!***
!***                 h 
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
!             WRITE(60,*)'NEAREST PARENT IS:','col=',COL,'row=',ROW,'ncol=',NCOL,'nrow=',NROW
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
!***               h
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
!        WRITE(60,*)'TRIVIAL SOLUTION'
      ELSE                                      ! SAME LONGITUDE BUT DIFFERENT LATS
!
         IF(TLATHC .GT. TLAT)THEN      ! NESTED POINT SOUTH OF PARENT
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'VANISHING SLOPE, -ve: TLATHC-DPH, TLONHC-DLM'
         ELSE                                   ! NESTED POINT NORTH OF PARENT
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'VANISHING SLOPE, +ve: TLATHC+DPH, TLONHC-DLM' 
         ENDIF
!***
!***
!***                  4
!***
!***                  h
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
!          WRITE(60,*)'HERE WE GO1: TLATHC, TLONHC'
        ELSE
          IF(TLONHC.LE.WB+DLM)CALL wrf_error_fatal("2H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIH(I,J) = NCOL-2
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC -2.*DLM
!          WRITE(60,*)'HERE WE GO2: TLATHC, TLONHC -2.*DLM'
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
!          WRITE(60,*)'HERE WE GO3:  TLATHC+DPH, TLONHC-DLM'
        ELSE
          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal("4H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'HERE WE GO4:  TLATHC-DPH, TLONHC-DLM' 
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
!          WRITE(60,*)'HERE WE GO5:  TLATHC-DPH, TLONHC-DLM'
        ELSE
          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal("6H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'HERE WE GO6: TLATHC+DPH,  TLONHC-DLM'
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
!***                   h
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

!      write(0,105)"H WEIGHTS:",I,J,HBWGT1(I,J),HBWGT2(I,J),HBWGT3(I,J),HBWGT4(I,J), &
!                               HBWGT1(I,J)+HBWGT2(I,J)+HBWGT3(I,J)+HBWGT4(I,J),IIH(i,j),JJH(i,j)
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
!***  AND THE NESTED GRID LAT-LONS AT v POINTS, THIS ROUTINE FIRST LOCATES THE
!***  INDICES,IIV,JJV, OF THE PARENT DOMAIN'S v POINTS THAT LIES CLOSEST TO THE
!***  v POINTS OF THE NESTED DOMAIN
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
!***  LOCATE TARGET v POINTS (VLAT AND VLON) ON THE PARENT DOMAIN AND
!***  DETERMINE THE INDICES IN TERMS OF THE PARENT DOMAIN. FIRST
!***  CONVERT NESTED GRID v POINTS FROM GEODETIC TO TRANSFORMED
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
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT v IS AT
!***
!***              H      V
!***
!***
!***                 v
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
!            WRITE(61,*)'NEAREST PARENT IS:','col=',COL,'row=',ROW,'ncol=',NCOL,'nrow=',NROW
      ELSE

!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT v IS AT
!***
!***              V      H
!***
!***
!***                 v
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
!             WRITE(61,*)'NEAREST PARENT IS:','col=',COL,'row=',ROW,'ncol=',NCOL,'nrow=',NROW

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
!***  WHICH OF THE FOUR V-BOXES (OF WHICH THIS V POINT IS
!***  A VERTEX) SURROUNDS THE INNER GRID v POINT IN QUESTION.
!***
!***
!***                  V
!***
!***
!***
!***            V     H     V
!***
!***
!***               v
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
!***  FIND THE SLOPE OF THE LINE CONNECTING v AND THE CENTER V.
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
!         WRITE(61,*)'TRIVIAL SOLUTION'
       ELSE                              ! SAME LONGITUDE BUT DIFFERENT LATS 
          
         IF(TLATVC .GT. TLAT)THEN      ! NESTED POINT SOUTH OF PARENT
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'VANISHING SLOPE, -ve: TLATVC-DPH, TLONVC-DLM'
         ELSE                                   ! NESTED POINT NORTH OF PARENT
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'VANISHING SLOPE, +ve: TLATVC+DPH, TLONVC-DLM'
         ENDIF

!***
!***
!***                  4
!***
!***                  v 
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
!          WRITE(61,*)'HERE WE GO1: TLATHC, TLONHC'
        ELSE
          IF(TLONVC.LE.WB+DLM)CALL wrf_error_fatal("2V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-1
          IIV(I,J) = NCOL-2
          JJV(I,J) = NROW
          TLATVX(I,J)=TLATVC
          TLONVX(I,J)=TLONVC-2.*DLM
!          WRITE(61,*)'HERE WE GO2: TLATHC, TLONHC -2.*DLM'
        ENDIF
 
      ELSEIF(DSLOPE.GT.DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal("3V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'HERE WE GO3:  TLATHC+DPH, TLONHC-DLM'
        ELSE
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal("4V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'HERE WE GO4:  TLATHC-DPH, TLONHC-DLM'
        ENDIF
 
      ELSEIF(DSLOPE.LT.-DSLP0)THEN
        IF(TLON.GT.TLONVC)THEN
          IF(TLATVC.LE.SB+DPH)CALL wrf_error_fatal("5V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW-1
          TLATVX(I,J)=TLATVC-DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'HERE WE GO5:  TLATHC-DPH, TLONHC-DLM'
        ELSE
          IF(TLATVC.GE.-SB-DPH)CALL wrf_error_fatal("6V:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIV(I,J) = NCOL-1
          JJV(I,J) = NROW+1
          TLATVX(I,J)=TLATVC+DPH
          TLONVX(I,J)=TLONVC-DLM
!          WRITE(61,*)'HERE WE GO6: TLATHC+DPH,  TLONHC-DLM'
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
!***                   v 
!***             1                 2
!***
!***
!***
!***
!***                      3
!***
!***
!***
!***  DL 1-4 ARE THE ANGULAR DISTANCES FROM v TO EACH VERTEX

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

!      WRITE(61,*)I,J,VBWGT1(I,J),VBWGT2(I,J),VBWGT3(I,J),VBWGT4(I,J), &
!                    VBWGT1(I,J)+VBWGT2(I,J)+VBWGT3(I,J)+VBWGT4(I,J),IIV(i,j),JJV(i,j)

    ENDDO
  ENDDO

 RETURN
 END SUBROUTINE G2T2V

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

!-------------------------------------------------------------------------------------

! DUE TO THE NEED FOR HALO EXCHANGES IN PARALLEL RUNS ONE HAS TO ENSURE CONSISTENT 
! USAGE OF NUMBER OF PROCESSORS BEFORE ANY FURTHER COMPUTATIONS. WE INTRODUCE THIS
! CHECK FIRST

  IF((ITE-ITS) .LE. 5 .OR. (JTE-JTS) .LE. 5)THEN
   WRITE(0,*)'ITE-ITS=',ITE-ITS,'JTE-JTS=',JTE-JTS
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
       WRITE(0,*)'I=',I,'J=',J,'WEIGHTS=',HBWGT1(I,J),HBWGT2(I,J),HBWGT3(I,J),HBWGT4(I,J),1-ADDSUM
       CALL wrf_error_fatal ('NESTED DOMAIN:SOMETHING IS WRONG WITH WEIGHTS COMPUTATION AT MASS POINTS') 
      ENDIF
   ENDDO
  ENDDO

  ADDSUM=0.
  DO J = JTS, MIN(JTE,JDE-1)
   DO I = ITS, MIN(ITE,IDE-1)
      ADDSUM=VBWGT1(I,J)+VBWGT2(I,J)+VBWGT3(I,J)+VBWGT4(I,J)
      IF(ABS(1.0-ADDSUM) .GE. EPSI)THEN 
       WRITE(0,*)'I=',I,'J=',J,'WEIGHTS=',VBWGT1(I,J),VBWGT2(I,J),VBWGT3(I,J),VBWGT4(I,J),1-ADDSUM
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
         WRITE(0,*)I,J,IIH(I,J),IPOS,JJH(I,J),JPOS,SHW
         WRITE(0,*)I,J,IIV(I,J),IPOS,JJV(I,J),JPOS,SHW
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
 REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(IN) :: PINT,T,Q,CWM
 REAL, DIMENSION(KMS:KME),                 INTENT(OUT):: PSTD
 REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(OUT):: Z3d,Q3d,T3d

! local

 INTEGER,PARAMETER                                    :: JTB=134
 INTEGER                                              :: I,J,K,ILOC,JLOC
 REAL, PARAMETER                                      :: LAPSR=6.5E-3, GI=1./G,D608=0.608
 REAL, PARAMETER                                      :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
 REAL, PARAMETER                                      :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
 REAL, PARAMETER                                      :: P_REF=103000.
 REAL                                                 :: A,B,APELP,RTOPP,DZ,ZMID
 REAL, DIMENSION(IMS:IME,JMS:JME)                     :: SLP,TSFC,ZMSLP
 REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME)             :: Z3d_IN
 REAL,DIMENSION(JTB)                                  :: PIN,ZIN,Y2,PIO,ZOUT,DUM1,DUM2
 REAL,DIMENSION(JTB)                                  :: QIN,QOUT,TIN,TOUT
!-------------------------------------------------------------------------------------- 

!  CLEAN Z3D ARRAY FIRST

    DO J = JTS, MIN(JTE,JDE-1)
     DO K=KDS,KDE
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d(I,K,J)=0.0
       T3d(I,K,J)=0.0
       Q3d(I,K,J)=0.0
      ENDDO
     ENDDO
    ENDDO 


!  DETERMINE THE HEIGHTS ON THE PARENT DOMAIN

    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       Z3d_IN(I,1,J)=FIS(I,J)*GI
      ENDDO
    ENDDO 

    DO J = JTS, MIN(JTE,JDE-1)
     DO K = KDS,KDE-1
      DO I = ITS, MIN(ITE,IDE-1)
        APELP    = (PINT(I,K+1,J)+PINT(I,K,J))
!       RTOPP    = TRG*T(I,K,J)*(1.0+Q(I,K,J)*P608-CWM(I,K,J))/APELP
        RTOPP    = TRG*T(I,K,J)*(1.0+Q(I,K,J)*P608)/APELP
        DZ       = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))   ! (RTv/P_TOT)*D(P_HYDRO)
        Z3d_IN(I,K+1,J) = Z3d_IN(I,K,J) + DZ
!       IF(I==2 .AND. J==2)WRITE(0,*)'INSIDE BASE_STATE',K,T(I,K,J)
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
        TSFC(I,J) = T(I,1,J)*(1.+D608*Q(I,1,J)) + LAPSR*(Z3d_IN(I,1,J)+Z3d_IN(I,2,J))*0.5
        A         = LAPSR*Z3d_IN(I,1,J)/TSFC(I,J)
        SLP(I,J)  = PINT(I,1,J)*(1-A)**COEF2    ! sea level pressure 
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
         PIN(K) = PINT(I,KDE-K+1,J)
         ZIN(K) = Z3d_IN(I,KDE-K+1,J)
       ENDDO

       IF(PINT(I,1,J) .LE. PSTD(1))THEN
          PIN(KDE) = PSTD(1)                 
          ZIN(KDE) = ZMSLP(I,J)             
       ENDIF
!
       Y2(1   )=0.
       Y2(KDE)=0.
!
       DO K=KDS,KDE
          PIO(K)=PSTD(K)
       ENDDO
!
       CALL SPLINE1(I,J,JTB,KDE,PIN,ZIN,Y2,KDE,PIO,ZOUT,DUM1,DUM2)  ! interpolate
!

       DO K=KDS,KDE                           ! inputs at model interfaces
         Z3d(I,K,J)=ZOUT(K)
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
         PIN(K-1) = EXP((ALOG(PINT(I,KDE-K+1,J))+ALOG(PINT(I,KDE-K+2,J)))*0.5)
         TIN(K-1) = T(I,KDE-K+1,J)
       ENDDO

       IF(PINT(I,1,J) .LE. PSTD(1))THEN
         PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)
         ZMID     = 0.5*(Z3d_IN(I,1,J)+Z3d_IN(I,2,J))
         TIN(KDE-1) = T(I,1,J) + LAPSR*(ZMID-ZMSLP(I,J)) 
       ENDIF
!
       Y2(1   )=0.
       Y2(KDE-1)=0.
!
       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,TIN,Y2,KDE-1,PIO,TOUT,DUM1,DUM2)  ! interpolate


       DO K=KDS,KDE-1                           ! inputs at model levels
         T3d(I,K,J)=TOUT(K)
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
         PIN(K-1) = EXP((ALOG(PINT(I,KDE-K+1,J))+ALOG(PINT(I,KDE-K+2,J)))*0.5)
         QIN(K-1) = Q(I,KDE-K+1,J)
       ENDDO

       IF(PINT(I,1,J) .LE. PSTD(1))THEN
          PIN(KDE-1) = EXP((ALOG(PSTD(1))+ALOG(PSTD(2)))*0.5)
!         QIN(KDE-1) =  QS(I,J)
       ENDIF

       Y2(1   )=0.
       Y2(KDE-1)=0.
!
       DO K=KDS,KDE-1
          PIO(K)=EXP((ALOG(PSTD(K))+ALOG(PSTD(K+1)))*0.5)
       ENDDO

       CALL SPLINE1(I,J,JTB,KDE-1,PIN,QIN,Y2,KDE-1,PIO,QOUT,DUM1,DUM2)  ! interpolate

       DO K=KDS,KDE-1                          ! inputs at model levels
         Q3d(I,K,J)=QOUT(K)
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
!---------------------------------------------------------------------

!     debug

      II=9999 !67 !35 !50   !4
      JJ=9999 !31 !73 !115  !192
      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(0,*)'DEBUG in SPLINE1:HSO= ',xnew(1:nold)
        DO K=1,NOLD
         WRITE(0,*)'DEBUG in SPLINE1:L,ZETAI,PINTI= ' &
                        ,K,YOLD(K),XOLD(K)
        ENDDO 
      ENDIF 

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

      if(i.eq.ii.and.j.eq.jj)then
        write(0,*) 'DEBUG:: k1,xnew(k1),ynew(k1): ', k1,xnew(k1),ynew(k1)
      endif

!
  600 K1=K1+1
      IF(K1.LE.NNEW)GO TO 300

      RETURN
      END SUBROUTINE SPLINE1
!---------------------------------------------------------------------

SUBROUTINE NEST_TERRAIN ( nest )

 USE module_domain
 USE module_configure
 USE module_timing

 USE wrfsi_static

 IMPLICIT NONE

 TYPE(domain) , POINTER                        :: nest

!
! Local variables
!

 LOGICAL, EXTERNAL                 :: wrf_dm_on_monitor
 INTEGER                           :: ids,ide,jds,jde,kds,kde
 INTEGER                           :: ims,ime,jms,jme,kms,kme
 INTEGER                           :: its,ite,jts,jte,kts,kte 
 INTEGER                           :: i_parent_start, j_parent_start
 INTEGER                           :: parent_grid_ratio
 INTEGER                           :: i,j,ii,jj,nnxp,nnyp
 INTEGER                           :: i_start,j_start,level
 REAL, ALLOCATABLE, DIMENSION(:,:) :: data1     ! for highres topo
 REAL, ALLOCATABLE, DIMENSION(:,:) :: avc_big, lnd_big, lah_big, loh_big
 REAL, ALLOCATABLE, DIMENSION(:,:) :: avc_nest, lnd_nest, lah_nest, loh_nest
 INTEGER                           :: im_big, jm_big, i_add
 INTEGER                           :: im, jm
 CHARACTER(LEN=6)                  :: nestpath
 
!----------------------------------------------------------------------------------

      IDS = nest%sd31
      IDE = nest%ed31
      KDS = nest%sd32
      KDE = nest%ed32
      JDS = nest%sd33
      JDE = nest%ed33

      IMS = nest%sm31
      IME = nest%em31
      KMS = nest%sm32
      KME = nest%em32
      JMS = nest%sm33
      JME = nest%em33

      ITS = nest%sp31
      ITE = nest%ep31
      KTS = nest%sp32
      KTE = nest%ep32
      JTS = nest%sp33
      JTE = nest%ep33

      i_parent_start = nest%i_parent_start
      j_parent_start = nest%j_parent_start
      parent_grid_ratio = nest%parent_grid_ratio

      NNXP=IDE-1
      NNYP=JDE-1

      ALLOCATE(DATA1(1:NNXP,1:NNYP))
!
!
!--- Read in high resolution topography
!
      IF ( wrf_dm_on_monitor() ) THEN                    ! first assign a status
!
!      This part of the code is Dusan's doing. Extended by gopal for multiple nest (Feb 19,2005)
!
       call find_ijstart_level (nest,i_start,j_start,level)
       write(0,*)" nest%id =", nest%id , " i_start,j_start,level =", i_start,j_start,level

       write(nestpath,"(a4,i1,a1)") 'nest',level,'/'

       if ( level > 0 ) then
          CALL get_wrfsi_static_dims(nestpath, im_big, jm_big)
          ALLOCATE (avc_big(im_big,jm_big))
          ALLOCATE (lnd_big(im_big,jm_big))
          ALLOCATE (lah_big(im_big,jm_big))
          ALLOCATE (loh_big(im_big,jm_big))
          CALL get_wrfsi_static_2d(nestpath, 'avc', avc_big)
          CALL get_wrfsi_static_2d(nestpath, 'lnd', lnd_big)
          CALL get_wrfsi_static_2d(nestpath, 'lah', lah_big)
          CALL get_wrfsi_static_2d(nestpath, 'loh', loh_big)
       else
          CALL wrf_error_fatal('this routine NEST_TERRAIN should nou be called for top-level domain')
       end if

! select subdomain from big fine grid

        im = NNXP
        jm = NNYP

        ALLOCATE (avc_nest(im,jm))
        ALLOCATE (lnd_nest(im,jm))
        ALLOCATE (lah_nest(im,jm))
        ALLOCATE (loh_nest(im,jm))

        i_add = mod(j_start+1,2) 
        DO j=1,jm
        DO i=1,im
           avc_nest(i,j) = avc_big(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
           lnd_nest(i,j) = lnd_big(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
           lah_nest(i,j) = lah_big(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
           loh_nest(i,j) = loh_big(i_start+i-1 + mod(j+1,2)*i_add, j_start+j-1)
        END DO
        END DO

        WRITE(0,*)'SOME MATCHING TEST i_parent_start, j_parent_start',i_parent_start,j_parent_start
        WRITE(0,*)'WRFSI LAT      COMPUTED LAT'
        WRITE(0,*)lah_nest(1,1),nest%nmm_hlat(1,1)
        WRITE(0,*)'WRFSI LON      COMPUTED LON'
        WRITE(0,*)loh_nest(1,1),nest%nmm_hlon(1,1)

        IF(ABS(lah_nest(1,1)-nest%nmm_hlat(1,1)) .GE. 0.5 .OR.  & 
           ABS(loh_nest(1,1)-nest%nmm_hlon(1,1)) .GE. 0.5)THEN 
            WRITE(0,*)'CHECK WRFSI CONFIGURATION AND INPUT HIGH RESOLUTION TOPOGRAPHY AND/OR GRID RATIO' 
            CALL wrf_error_fatal('LATLON MISMATCH: ERROR READING static.wrfsi.rotlat FOR THE NEST')
        ENDIF

        call smdhld(im,jm,avc_nest,1.0-lnd_nest,12,12)

!-------------4-point averaging of mountains along inner boundary-------

        do i=1,im-1
            avc_nest(i,2)=0.25*(avc_nest(i,1)+avc_nest(i+1,1)+ &
           &                    avc_nest(i,3)+avc_nest(i+1,3))
        enddo

        do i=1,im-1
            avc_nest(i,jm-1)=0.25*(avc_nest(i,jm-2)+avc_nest(i+1,jm-2)+ &
           &                       avc_nest(i,jm)+avc_nest(i+1,jm))
        enddo

        do j=4,jm-3,2
            avc_nest(1,j)=0.25*(avc_nest(1,j-1)+avc_nest(2,j-1)+ &
           &                    avc_nest(1,j+1)+avc_nest(2,j+1))
        enddo

        do j=4,jm-3,2
            avc_nest(im,j)=0.25*(avc_nest(im-1,j-1)+avc_nest(im,j-1)+ &
           &                     avc_nest(im-1,j+1)+avc_nest(im,j+1))
        enddo
        
        DO J = 1,NNYP
         DO I = 1,NNXP
            DATA1(I,J) = 9.81*avc_nest(I,J)
         ENDDO
        ENDDO

        DEALLOCATE (avc_big,lnd_big)
        DEALLOCATE (avc_nest,lnd_nest)
!
      ENDIF

      CALL wrf_dm_bcast_bytes (DATA1,NNXP*NNYP*RWORDSIZE)

      DO J=JDS,JDE
       DO I =IDS,IDE
        IF(I.GE.ITS .AND. I .LE. MIN(ide-1,ite) .AND. J.GE.JTS  .AND. J .LE. MIN(jde-1,jte))THEN
          nest%nmm_hres_fis(I,J)=DATA1(I,J)
        ENDIF
       ENDDO
      ENDDO

      DEALLOCATE(DATA1)
      WRITE(0,*)'end of NEST_TERRAIN'

END SUBROUTINE NEST_TERRAIN
!===========================================================================================


SUBROUTINE med_init_domain_constants_nmm ( parent, nest)   !, config_flags)
  ! Driver layer
   USE module_domain
   USE module_configure
   USE module_timing
   IMPLICIT NONE
   TYPE(domain) , POINTER                     :: parent, nest, grid
!
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
!
   INTERFACE
     SUBROUTINE med_initialize_nest_nmm ( grid  &   
!
# include <nmm_dummy_args.inc>
!
                           )
        USE module_domain
        USE module_configure
        USE module_timing
        IMPLICIT NONE
        TYPE(domain) , POINTER                  :: grid
#include <nmm_dummy_decl.inc>
     END SUBROUTINE med_initialize_nest_nmm 
   END INTERFACE

!------------------------------------------------------------------------------
!  PURPOSE: 
!         - initialize some data, mainly 2D & 3D nmm arrays  very similar to 
!           those done in ./dyn_nmm/module_initialize_real.F 
!-----------------------------------------------------------------------------
!

   grid => nest

#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
   sm31x            = grid%sm31x
   em31x            = grid%em31x
   sm32x            = grid%sm32x
   em32x            = grid%em32x
   sm33x            = grid%sm33x
   em33x            = grid%em33x
   sm31y            = grid%sm31y
   em31y            = grid%em31y
   sm32y            = grid%sm32y
   em32y            = grid%em32y
   sm33y            = grid%sm33y
   em33y            = grid%em33y
#endif

   CALL med_initialize_nest_nmm( grid &   
!
# include <nmm_actual_args.inc>
!
                       )

END SUBROUTINE med_init_domain_constants_nmm

SUBROUTINE med_initialize_nest_nmm( grid &
!
# include <nmm_dummy_args.inc>
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
 REAL, PARAMETER:: DTR=0.01745329

   !  Definitions of dummy arguments to solve
#include <nmm_dummy_decl.inc>

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
   sm31x            = grid%sm31x
   em31x            = grid%em31x
   sm32x            = grid%sm32x
   em32x            = grid%em32x
   sm33x            = grid%sm33x
   em33x            = grid%em33x
   sm31y            = grid%sm31y
   em31y            = grid%em31y
   sm32y            = grid%sm32y
   em32y            = grid%em32y
   sm33y            = grid%sm33y
   em33y            = grid%em33y
#endif

#define COPY_IN
#include <nmm_scalar_derefs.inc>
#ifdef DM_PARALLEL
#      include <nmm_data_calls.inc>
#endif

   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             its, ite, jts, jte, kts, kte     )


!=================================================================================
!
!

    DT=grid%dt         !float(TIME_STEP)/parent_time_step_ratio
    NNXP=min(ITE,IDE-1)
    NNYP=min(JTE,JDE-1)
    JAM=6+2*((JDE-1)-10)         ! this should be the fix instead of JAM=6+2*(NNYP-10)

    WRITE(0,*)'TIME STEP ON DOMAIN',grid%id,'==',dt

!
    ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
    ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
    ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),FADJ(JTS:NNYP))
    ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
    ALLOCATE(KHLA(JAM),KHHA(JAM))
    ALLOCATE(KVLA(JAM),KVHA(JAM))

!   INITIALIZE SOME LAND/WATER SURFACE DATA ON THE BASIS OF INPUTS: SM, XICE, WEASD, 
!   INTERPOLATED FROM MOTHER (WRFSI) DOMAIN. THIS PART OF THE CODE HAS TO BE REVISITED
!   LATER ON

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
      IF(SM(I,J).GT.0.9) THEN           ! OVER WATER SURFACE
!
           IF (XICE(I,J) .gt. 0)THEN    ! XICE: SI INPUT ON PARENT, INTERPOLATED ONTO NEST
             SI(I,J)=1.0                ! INITIALIZE SI BASED ON XICE FROM INTERPOLATED INPUT
           ENDIF
!
           EPSR(I,J)= 0.97              ! VALID OVER SEA SURFACE
           GFFC(I,J)= 0.
           ALBEDO(I,J)=.06
           ALBASE(I,J)=.06
!
              IF(SI (I,J) .GT. 0.)THEN  ! VALID OVER SEA-ICE 
                 SM(I,J)=0.
                 SI(I,J)=0.             ! 
                 SICE(I,J)=1.
                 GFFC(I,J)=0.           ! just leave zero as irrelevant
                 ALBEDO(I,J)=.60        ! DEFINE ALBEDO 
                 ALBASE(I,J)=.60
              ENDIF
!
      ELSE                              ! OVER LAND SURFACE
!
           SI(I,J)=5.0*WEASD(I,J)/1000. ! SNOW WATER EQ (mm) OBTAINED FROM PARENT (SI) IS INTERPOLATED 
           EPSR(I,J)=1.0                ! EMISSIVITY DEFINED OVER LAND IN THE NESTED DOMAIN
           GFFC(I,J)=0.0                ! just leave zero as irrelevant
           SICE(I,J)=0.                 ! SEA ICE
           SNO(I,J)=SI(I,J)*.20         ! LAND-SNOW COVER 
!
      ENDIF
!
     ENDDO
    ENDDO

!   This may just be a fix and may need some Registry related changes, later on

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
         VEGFRA(I,J)=VEGFRC(I,J)
     ENDDO
    ENDDO

!   DETERMINE ALBEDO OVER LAND ON THE BASIS OF INPUTS: SM, ALBASE, MXSNAL & VEGFRA
!   INTERPOLATED FROM MOTHER (WRFSI) DOMAIN

 
    DO J = JTS, MIN(JTE,JDE-1) 
     DO I = ITS, MIN(ITE,IDE-1)

          IF(SM(I,J).LT.0.9.AND.SICE(I,J).LT.0.9) THEN
!
            IF ( (SNO(I,J) .EQ. 0.0) .OR. &            ! SNOWFREE ALBEDO
                           (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN
              ALBEDO(I,J) = ALBASE(I,J)
            ELSE
              IF (SNO(I,J) .LT. SNUP) THEN             ! MODIFY ALBEDO IF SNOWCOVER:
                  RSNOW = SNO(I,J)/SNUP                ! BELOW SNOWDEPTH THRESHOLD
                  SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
              ELSE
                  SNOFAC = 1.0                         ! ABOVE SNOWDEPTH THRESHOLD
              ENDIF
              ALBEDO(I,J) = ALBASE(I,J) &
                          + (1.0-VEGFRA(I,J))*SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
            ENDIF
!
          END IF

          SI(I,J)=5.0*WEASD(I,J)
          SNO(I,J)=WEASD(I,J)
! this block probably superfluous.  Meant to guarantee land/sea agreement

        IF (SM(I,J) .gt. 0.5)THEN 
           landmask(I,J)=0.0
        ELSE 
           landmask(I,J)=1.0
        ENDIF 

        IF (SICE(I,J) .eq. 1.0) then !!!! change vegtyp and sltyp to fit seaice (desireable??)
         ISLTYP(I,J)=16
         IVGTYP(I,J)=24
        ENDIF 

     ENDDO
    ENDDO

!  Check land water interface

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS,MIN(ITE,IDE-1)
      IF(SM(I,J).GT.0.9 .AND. VEGFRA(I,J) .NE. 0) THEN
        WRITE(20,*)'PROBLEM AT THE LAND-WATER INTERFACE:',I,J,SM(I-1,J),VEGFRA(I-1,j),SM(I,J),VEGFRA(I,J)
      ENDIF
!
      IF(SM(I,J).GT.0.9 .AND. NMM_TSK(I,J) .NE. 0) THEN
        WRITE(20,*)'PROBLEM AT THE LAND-WATER INTERFACE:',I,J,SM(I-1,J),NMM_TSK(I-1,J),SM(I,J),NMM_TSK(I,J)
      ENDIF
     ENDDO
    ENDDO


!   hardwire root depth for time being

        RTDPTH=0.
        RTDPTH(1)=0.1
        RTDPTH(2)=0.3
        RTDPTH(3)=0.6

!   hardwire soil depth for time being

        SLDPTH=0.
        SLDPTH(1)=0.1
        SLDPTH(2)=0.3
        SLDPTH(3)=0.6
        SLDPTH(4)=1.0

!-----------  END OF LAND SURFACE INITIALIZATION -------------------------------------
!
!   INITIALIZE 3D HEIGHT MASK AND VELOCITY FIELDS (HTM AND VTM),
!   AND LOWEST ABV GROUND LEVEL (LMH AND LMV) AND RECIPROCAL 
!   ETAS (RES) OVER THE NESTED DOMAIN  


    DO J = JTS, MIN(JTE,JDE-1)
     DO K = KTS,KTE
      DO I = ITS, MIN(ITE,IDE-1)
       HTM(I,K,J)=1.0
       VTM(I,K,J)=1.0
      ENDDO
     ENDDO
    ENDDO

    DO J = JTS, MIN(JTE,JDE-1)
     DO I = ITS, MIN(ITE,IDE-1)
       LMH(I,J)= KME-1        ! note the flipping for start_domain_nmm.F
       LMV(I,J)= KME-1        ! this is consistent with Tom's version
       RES(I,J)=1.
     ENDDO
    ENDDO

!   INITIALIZE 2D BOUNDARY MASKS

!! HBM2:
 
    HBM2=0.
    DO J = JTS, MIN(JTE,JDE-1)
      DO I = ITS, MIN(ITE,IDE-1)
       IF((J .GE. 3 .and. J .LE. (JDE-1)-2) .AND.   &
          (I .GE. 2 .and. I .LE. (IDE-1)-2+MOD(J,2))) THEN
          HBM2(I,J)=1.
        ENDIF
      ENDDO
    ENDDO   

!! HBM3:

    HBM3=0.
    DO J=JTS,MIN(JTE,JDE-1)
     IHWG(J)=mod(J+1,2)-1
      IF (J .ge. 4 .and. J .le. (JDE-1)-3) THEN
        IHL=(IDS+1)-IHWG(J) 
        IHH=(IDE-1)-2
        DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IHL  .and. I .le. IHH) HBM3(I,J)=1.
        ENDDO 
      ENDIF
    ENDDO 
   
!! VBM2

    VBM2=0.
    DO J=JTS,MIN(JTE,JDE-1)
     DO I=ITS,MIN(ITE,IDE-1)
       IF((J .ge. 3 .and. J .le. (JDE-1)-2)  .AND.  &
          (I .ge. 2 .and. I .le. (IDE-1)-1-MOD(J,2))) THEN
           VBM2(I,J)=1.
       ENDIF
     ENDDO
    ENDDO

!! VBM3

    VBM3=0.
    DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
        IF((J .ge. 4 .and. J .le. (JDE-1)-3)  .AND.  &
           (I .ge. 3-MOD(J,2) .and. I .le. (IDE-1)-2)) THEN
           VBM3(I,J)=1.
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
    DLM    = DLMD*DTR  ! input now from med_nest_egrid_configure
    DPH    = DPHD*DTR  ! input now from med_nest_egrid_configure
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
    DY_NMM0= DY_NMM ! ERAD*DPH; input now from med_nest_egrid_configure

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
         F(I,J)=0.5*grid%DT*FP
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

     WRITE(0,*)'NEW CHANGE',F4D,EF4T,F4Q

      DO L=KDS,KDE-1
        RDETA(L)=1./DETA(L)
        F4Q2(L)=-.25*grid%DT*DTAD/DETA(L)
      ENDDO

       DO J=JTS,MIN(JTE,JDE-1)
        DO I=ITS,MIN(ITE,IDE-1)
          DX_NMM(I,J)=DXJ(J)
          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
          CPGFU(I,J)=CPGFUJ(J)*VBM2(I,J)
          CURV(I,J)=CURVJ(J)*VBM2(I,J)
          FCP(I,J)=FCPJ(J)*HBM2(I,J)
          FDIV(I,J)=FDIVJ(J)*HBM2(I,J)
          FAD(I,J)=FADJ(J)
          HDACV(I,J)=HDACJ(J)*VBM2(I,J)
          HDAC(I,J)=HDACJ(J)*1.25*HBM2(I,J)
        ENDDO
       ENDDO

       DO J=JTS, MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN
          KHH=(IDE-1)-2+MOD(J,2) ! KHH is global...loop over I that have
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               HDAC(I,J)=HDAC(I,J)* DFC
             ENDIF
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
             IF (I .ge. 2 .and. I .le. KHH) THEN
               HDAC(I,J)=HDAC(I,J)* DFC
             ENDIF
          ENDDO
          KHH=(IDE-1)-2+MOD(J,2)

          DO I=ITS,MIN(ITE,IDE-1)
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

        DO J=JTS,MIN(JTE,JDE-1)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             DDMPU(I,J)=DDMPU(I,J)*DDFC
             DDMPV(I,J)=DDMPV(I,J)*DDFC
             HDACV(I,J)=HDACV(I,J)*DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)*DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,MIN(ITE,IDE-1)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)*DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO

! This one was left over for nested domain
 
     DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          GLAT(I,J)=HLAT(I,J)*DTR
          GLON(I,J)=HLON(I,J)*DTR
       ENDDO
     ENDDO

!!   compute EMT, EM on global domain, and only on task 0.

!    IF (wrf_dm_on_monitor()) THEN   !!!! NECESSARY TO LIMIT THIS TO TASK ZERO?
      
     ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))
     write(0,*) 'FIGURING OUT EMJ, EMTJ ', JDS, JDE-1
     DO J=JDS,JDE-1
       TPH=SB+float(J-1)*DPH
       DXP=ERAD*DLM*COS(TPH)
       EMJ(J)= grid%DT/( 4.*DXP)*DTAD
       EMTJ(J)=grid%DT/(16.*DXP)*DTAD
!       write(0,*) 'J, EMTJ(J): ', J, EMTJ(J)
     ENDDO

          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 161      EMT(JA)=EMTJ(J)
          DO 162 J=(JDE-1)-4,(JDE-2)-2
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
          DO 172 J=(JDE-1)-4,(JDE-2)-2
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

!        ENDIF ! wrf_dm_on_monitor

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)
!!
                                                                !   phantom
        IF (ABS(IDE-1-ITE) .eq. 1 ) THEN                        !      | 
         WRITE(0,*)'zero phantom winds'                         !  H  [x]    H    V
         DO K=KDS,KDE-1                                         !  
          DO J=JDS,JDE-1,2                                      !  V  [H]    V    H
           IF (J .ge. JTS .and. J .le. JTE) THEN                !
             U(IDE-1,K,J)=0.                                    !  H  [x]    H    V
             V(IDE-1,K,J)=0.                                    !  ------    ------  
           ENDIF                                                !   ide-1      ide
          ENDDO                                                 !   NMM/SI     WRF
         ENDDO                                                  !   domain    domain
        ENDIF                                                   !             (dummy)   


! just a test for gravity waves

!  PD=62000.
!   U=0.0
!   V=0.0
!   T=300.
!   Q=0.0
!   Q2=0.0
!   CWM=0.0
!   FIS=0.0

! testx
!  DO J = JTS, MIN(JTE,JDE-1)
!   DO K = KTS,KTE
!    DO I = ITS, MIN(ITE,IDE-1)
!      SM(I,J)=I
!       U(I,K,J)=J
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


END SUBROUTINE med_initialize_nest_nmm
!======================================================================

      subroutine smdhld(ime,jme,h,s,lines,nsmud)
      dimension ihw(jme),ihe(jme)
      dimension h(ime,jme),s(ime,jme) &
     &         ,hbms(ime,jme),hne(ime,jme),hse(ime,jme)
!-----------------------------------------------------------------------
          do j=1,jme
      ihw(j)=-mod(j,2)
      ihe(j)=ihw(j)+1
          enddo
!-----------------------------------------------------------------------

              do j=1,jme
          do i=1,ime
      hbms(i,j)=1.-s(i,j)
          enddo
              enddo
!
      jmelin=jme-lines+1
      ibas=lines/2
      m2l=mod(lines,2)
!
              do j=lines,jmelin
          ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
          ihh=ime-ibas-m2l*mod(j+1,2)

!       write(6,*) 'no smooth limits for J: ', J, 'are  ', ihl,ihh
!
          do i=ihl,ihh
      hbms(i,j)=0.
          enddo
              enddo
!-----------------------------------------------------------------------
                  do ks=1,nsmud

                write(6,*) 'H(1,1): ', h(1,1)
                write(6,*) 'H(3,1): ', h(1,1)
!-----------------------------------------------------------------------
              do j=1,jme-1
          do i=1,ime-1
      hne(i,j)=h(i+ihe(j),j+1)-h(i,j)
          enddo
              enddo
              do j=2,jme
          do i=1,ime-1
      hse(i,j)=h(i+ihe(j),j-1)-h(i,j)
          enddo
              enddo
!
              do j=2,jme-1
          do i=1+mod(j,2),ime-1
      h(i,j)=(hne(i,j)-hne(i+ihw(j),j-1) &
     &       +hse(i,j)-hse(i+ihw(j),j+1))*hbms(i,j)*0.125+h(i,j)
          enddo
              enddo
!-----------------------------------------------------------------------

!!!         smooth around boundary somehow?

!       special treatment for four corners

        if (hbms(1,1) .eq. 1) then
        h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+ &
     &                            0.0625*(h(2,1)+h(1,3))
        endif

        if (hbms(ime,1) .eq. 1) then
        h(ime,1)=0.75*h(ime,1)+0.125*h(ime+ihw(1),2)+ &
     &                            0.0625*(h(ime-1,1)+h(ime,3))
        endif

        if (hbms(1,jme) .eq. 1) then
        h(1,jme)=0.75*h(1,jme)+0.125*h(1+ihe(jme),jme-1)+ &
     &                            0.0625*(h(2,jme)+h(1,jme-2))
        endif

        if (hbms(ime,jme) .eq. 1) then
        h(ime,jme)=0.75*h(ime,jme)+0.125*h(ime+ihw(jme),jme-1)+ &
     &                            0.0625*(h(ime-1,jme)+h(ime,jme-2))
        endif


!       S bound

        J=1
        do I=2,ime-1
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
        endif
        enddo

!       N bound

        J=JME
        do I=2,ime-1
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
        endif
        enddo

!       W bound

        I=1
        do J=3,jme-2
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
        endif
        enddo

!       E bound

        I=IME
        do J=3,jme-2
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
        endif
        enddo


                      enddo   ! end ks loop

!!      (light touch) with 5-point filter over untouched interior?

!       do ks=1,5
!       do J=lines-1,jme-(lines-1)
!       do I=lines-1,ime-(lines-1)
!       if (s(I,J) .eq. 0 .and.
!     &      h(I,J) .gt. h(i+ihw(J),J+1) .and.
!     &      h(I,J) .gt. h(I+ihe(J),J+1) .and.
!     &      h(I,J) .gt. h(i+ihw(J),J-1) .and.
!     &      h(I,J) .gt. h(I+ihe(J),J-1)) then
!       write(6,*) 'smoothing topo at I,J...', I,J,H(I,J)
!       h(I,J)=h(I,J)+0.125*( h(i+ihw(J),J+1) + h(I+ihe(J),J+1) +
!     &                      h(i+ihw(J),J-1) + h(I+ihe(J),J-1) -
!     &                       4*h(I,J) )
!       write(6,*) 'post smoothing val', ks,H(I,J)
!       endif
!       enddo
!       enddo
!       enddo

!-----------------------------------------------------------------------
      return
      end subroutine smdhld

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
    KDS = parent%sd32
    KDE = parent%ed32
    JDS = parent%sd33
    JDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    KMS = parent%sm32
    KME = parent%em32
    JMS = parent%sm33
    JME = parent%em33

    ITS  = parent%sp31
    ITE  = parent%ep31
    KTS  = parent%sp32
    KTE  = parent%ep32
    JTS  = parent%sp33
    JTE  = parent%ep33

    NIDE = nest%ed31
    NJDE = nest%ed33

    parent_DLMD = parent%dx          ! DLMD: dlamda in degrees
    parent_DPHD = parent%dy          ! DPHD: dphi in degrees
    parent_WBD  = -(IDE-2)*parent%dx ! WBD0: in deg;factor 2 takes care of dummy last column 
    parent_SBD  = -((JDE-1)/2)*parent%dy ! SBD0: in degrees; note that JDE-1 should be odd 
    ALAT  = parent_SLAT - 0.5*(NJDE-2)*parent_DPHD/nest%parent_grid_ratio
    ALON  = parent_SLON - 1.0*(NIDE-2)*parent_DLMD/nest%parent_grid_ratio

!    WRITE(0,*)'ALAT AND ALON=',ALAT,ALON

    CALL EARTH_LATLON ( parent%nmm_HLAT,parent%nmm_HLON,parent%nmm_VLAT,parent%nmm_VLON, & !output
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
        DIFF1 = ABS(ALAT - parent%nmm_HLAT(I,J))
        DIFF2 = ABS(ALON - parent%nmm_HLON(I,J))
        IF(DIFF1 .LE. ERR .AND. DIFF2 .LE. ERR)THEN
         ILOC=I
         JLOC=J
!         WRITE(0,*)'ITERATED',ERR,ITER,I,J,parent%nmm_HLAT(I,J),ALAT,parent%nmm_HLON(I,J),ALON
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
       WRITE(0,*)'NOTE: I_PARENT_START AND J_PARENT_START FOUND FOR THE NESTED DOMAIN CONFIGURATION AT ITER=',ITER
       WRITE(0,*)'istart=',ILOC
       WRITE(0,*)'jstart=',JLOC
     ELSE
       ILOC=IDE/3
       JLOC=JDE/3
!
       WRITE(0,*)'WARNING: COULD NOT LOCATE I_PARENT_START AND J_PARENT_START FROM INPUT STORM INFO'
       WRITE(0,*)'ISTART=',IDE/3
       WRITE(0,*)'JSTART=',JDE/3
     ENDIF

     RETURN
END SUBROUTINE initial_nest_pivot

!============================================================================================
#endif

LOGICAL FUNCTION nmm_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   nmm_cd_feedback_mask = ( pig .ge. ips_save+1        .and.      &
                            pjg .ge. jps_save+1        .and.      &
                            pig .le. ipe_save-1  +ioff .and.      &
                            pjg .le. jpe_save-1  +joff           )

END FUNCTION nmm_cd_feedback_mask

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
