MODULE module_cpl

   USE module_domain          , ONLY : domain, get_ijk_from_grid
   USE module_configure       , ONLY : grid_config_rec_type
   USE module_model_constants , ONLY : stbolt
   USE module_driver_constants, ONLY : max_domains, max_cplfld, max_extdomains
   USE module_cpl_oasis3 

   IMPLICIT NONE
   PRIVATE

   PUBLIC cpl_init
   PUBLIC cpl_set_dm_communicator
   PUBLIC cpl_defdomain
   PUBLIC cpl_settime
   PUBLIC cpl_snd
   PUBLIC cpl_rcv
   PUBLIC cpl_store_input
   PUBLIC cpl_finalize
   PUBLIC cpl_abort

#ifdef key_cpp_oasis3
   LOGICAL     , PARAMETER, PUBLIC :: coupler_on = .TRUE.
   CHARACTER(5), PARAMETER         :: coupler_name = 'oasis'
#else
   LOGICAL     , PARAMETER, PUBLIC :: coupler_on = .FALSE.
   CHARACTER(4), PARAMETER         :: coupler_name = 'none'
#endif
   INTEGER :: nsecrun             ! current time in seconds since simulation restart
   INTEGER, PARAMETER :: charlen = 64
   CHARACTER(charlen), DIMENSION(max_domains,max_extdomains,max_cplfld) :: rcvname, sndname   ! coupling fields names for each nest

   CHARACTER(256) :: cltxt        ! messages or debug string
   INTEGER :: nlevdbg  = 1        ! verbosity level
   INTEGER :: nlevdbg2 = 10       ! verbosity level

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
   INCLUDE 'mpif.h'               ! only for MPI_COMM_NULL
#else
   INTEGER :: MPI_COMM_NULL = -1  ! define a fake (and not used) MPI_COMM_NULL, so it is compiling
#endif

CONTAINS

   SUBROUTINE cpl_init( kl_comm ) 
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_init  ***
      !!
      !! ** Purpose :   initialise coupling field names and WRF-coupler MPI communications
      !!--------------------------------------------------------------------
      INTEGER, INTENT(OUT) :: kl_comm       ! local MPI communicator of the model
      !
      INTEGER       :: jwrf,jext,jfld       ! local loop indicees
      CHARACTER( 3) :: clwrfdom, clextdom   ! d<domain>
      CHARACTER(16) :: clprefix             ! 'WRF_d??_EXT_d??_'
      !!--------------------------------------------------------------------

      ! coupling field name default definition
      rcvname(:,:,:) = 'not defined'
      sndname(:,:,:) = 'not defined'
      
      ! we could imagine to define rcvname and sndname through the namelist...
      ! define all possible coupling names with _d<domain> of WRF and the external model(s)
      DO jext = 1, max_extdomains
         
         WRITE(clextdom, fmt="('d',i2.2)") jext
         
         DO jwrf = 1, max_domains
            
            WRITE(clwrfdom, fmt="('d',i2.2)") jwrf          
            ! do not change following syntaxe as it is used in routines bellow 
            clprefix = 'WRF_'//clwrfdom//'_EXT_'//clextdom//'_' 
            
            ! Variables that can be received by WRF
            rcvname(jwrf,jext,1) = clprefix//'SST'                  ! receive Sea surface temperature
            rcvname(jwrf,jext,2) = clprefix//'UOCE'                 ! receive ocean zonal surface current 
            rcvname(jwrf,jext,3) = clprefix//'VOCE'                 ! receive ocean meridional surface current 
            
            ! Variables that can be sent by WRF
            sndname(jwrf,jext,1) = clprefix//'EVAP-PRECIP'          ! send net fresh water budget: evaporation - total précipitation
            sndname(jwrf,jext,2) = clprefix//'SURF_NET_SOLAR'       ! send net short wave flux at ground surface
            sndname(jwrf,jext,3) = clprefix//'SURF_NET_NON-SOLAR'   ! send net non-solar heat flux at ground surface
            sndname(jwrf,jext,4) = clprefix//'TAUX'                 ! send zonal wind tress at atmosphere-ocean interface
            sndname(jwrf,jext,5) = clprefix//'TAUY'                 ! send meridional wind tress at atmosphere-ocean interface
            sndname(jwrf,jext,6) = clprefix//'TAUMOD'               ! send the wind tress module at atmosphere-ocean interface
            
         END DO
      END DO
      
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_init( kl_comm ) 
      
   END SUBROUTINE cpl_init
   

   SUBROUTINE cpl_set_dm_communicator( kdm_comm )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_initquilt  ***
      !!
      !! ** Purpose : provide the computing nodes communicator to the coupler
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdm_comm       ! MPI communicator between the computing nodes
      !!--------------------------------------------------------------------

      IF ( coupler_name == 'oasis' ) THEN 
         IF ( kdm_comm == MPI_COMM_NULL ) THEN
            CALL cpl_oasis_define( sndname, rcvname )   ! define io_quilting to OASIS
         ELSE
            CALL cpl_oasis_def_dmcomm( kdm_comm )       ! send the computing nodes communicator to OASIS
         END IF
      END IF

   END SUBROUTINE cpl_set_dm_communicator


   SUBROUTINE cpl_defdomain( grid )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_defdomain  ***
      !!
      !! ** Purpose : define each variable involved in the coupling and the grid partitioning
      !!--------------------------------------------------------------------
      TYPE(domain), INTENT(IN), POINTER ::   grid
      !
      INTEGER :: jwrf,jext,jfld          ! local loop indicees
      REAL    :: zmin,zmax               ! min/max of grid*cplmask
      INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
      INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
      INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
      !!--------------------------------------------------------------------
#if (EM_CORE == 1)

      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )

      ! first do some checks and prints. note that this could not be done in cpl_init
      ! which is called too early in the code

      ! some control prints on potential sent/received fields...
      CALL wrf_debug(nlevdbg, 'cpl_init: defined variables to be potentially received' )
      DO jfld = 1, max_cplfld
         DO jext = 1, grid%num_ext_model_couple_dom
            DO jwrf = 1, grid%max_dom
               IF( TRIM(sndname(jwrf,jext,jfld)) /= 'not defined' ) THEN
                  WRITE(cltxt,*) '   jwrf, jext, jfld: ', jwrf, jext, jfld ,' name: ', TRIM(sndname(jwrf,jext,jfld))
                  CALL wrf_debug(nlevdbg2, cltxt)
               END IF
            END DO
         END DO
      END DO
      CALL wrf_debug(nlevdbg, 'cpl_init: defined variables to be potentially sent' )
      DO jfld = 1, max_cplfld
         DO jext = 1, grid%num_ext_model_couple_dom
            DO jwrf = 1, grid%max_dom
               IF( TRIM(rcvname(jwrf,jext,jfld)) /= 'not defined' ) THEN
                  WRITE(cltxt,*) '   jwrf, jext, jfld: ', jwrf, jext, jfld ,' name: ', TRIM(rcvname(jwrf,jext,jfld))
                  CALL wrf_debug(nlevdbg2, cltxt)
               END IF
            END DO
         END DO
      END DO
      
      ! some checks on grid%cplmask...
      DO jext = 1, grid%num_ext_model_couple_dom

         WRITE(cltxt,*) 'checks on cplmask of external model domain: ', jext               ;   CALL wrf_debug(nlevdbg, cltxt)

         zmin = MINVAL(grid%cplmask(ips:ipe,jext,jps:jpe))
         IF( zmin < 0. ) THEN
            WRITE(cltxt,*) 'min of external model domain cplmask: ',jext,' < 0. : ',zmin   ;   CALL cpl_abort('cpl_defdomain',cltxt)
         END IF
         WRITE(cltxt,*) '   minval(grid%cplmask(ips:ipe,jext,jps:jpe)): ', zmin            ;   CALL wrf_debug(nlevdbg, cltxt)

         zmax = MAXVAL(grid%cplmask(ips:ipe,jext,jps:jpe))
         IF( zmax > 1. ) THEN
            WRITE(cltxt,*) 'max of external model domain cplmask: ',jext,' > 1. : ',zmax   ;   CALL cpl_abort('cpl_defdomain',cltxt)
         END IF
         IF( zmax == 0. ) THEN
            WRITE(cltxt,*) 'max of external model domain cplmask: ',jext,' = 0 '           ;   CALL wrf_message(cltxt)
            WRITE(cltxt,*) '  => no coupling between this external model domain and this WRF patch'   ;   CALL wrf_message(cltxt)
         END IF
         WRITE(cltxt,*) '   maxval(grid%cplmask(ips:ipe,jext,jps:jpe)): ', zmax            ;   CALL wrf_debug(nlevdbg, cltxt)

      END DO
#endif       
      
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_define( sndname, rcvname, grid )

   END SUBROUTINE cpl_defdomain


   SUBROUTINE cpl_settime( psec )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_settime  ***
      !!
      !! ** Purpose :   update and store the number of second since the beginning of the job.  
      !!--------------------------------------------------------------------
      REAL, INTENT(in) :: psec
      !!--------------------------------------------------------------------

      nsecrun = NINT( psec )
      WRITE(cltxt,*) 'store number of second since the beginning of the job: ', nsecrun   ;   CALL wrf_debug(nlevdbg2, cltxt)

   END SUBROUTINE cpl_settime


   FUNCTION cpl_toreceive( kdomwrf, kdomext, kfldid )
      !!-------------------------------------------------------------------
      !!             ***  FUNCTION cpl_toreceive  ***
      !!
      !! ** Purpose :   send back a logical to tell if a variable must be received or not
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kdomext   ! external model domain index
      INTEGER, INTENT(IN) :: kfldid    ! field index
      !
      LOGICAL :: cpl_toreceive
      !!--------------------------------------------------------------------

      IF ( coupler_name == 'oasis' ) cpl_toreceive = cpl_oasis_toreceive( kdomwrf, kdomext, kfldid ) 

   END FUNCTION cpl_toreceive


   FUNCTION cpl_tosend( kdomwrf, kfldid, max_edom )
      !!-------------------------------------------------------------------
      !!             ***  FUNCTION cpl_tosend  ***
      !!
      !! ** Purpose :   send back a logical array to tell if a variable must be
      !!                sent or not to each of the external model domains
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kfldid    ! variable index
      INTEGER, INTENT(IN) :: max_edom  ! max number of external model domains
      !
      LOGICAL,DIMENSION(max_edom) :: cpl_tosend
      INTEGER                     :: jext          ! local loop indicees
      !!--------------------------------------------------------------------

      DO jext = 1, max_edom
         IF ( coupler_name == 'oasis' )   cpl_tosend(jext) = cpl_oasis_tosend( kdomwrf, jext, kfldid ) 
      END DO
      
   END FUNCTION cpl_tosend


   FUNCTION cpl_get_fldid( cdsuffix )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_get_fldid  ***
      !!
      !! ** Purpose : send back the field id corresponding to the suffix of a coupling variable name
      !!--------------------------------------------------------------------
      CHARACTER(*), INTENT(IN) :: cdsuffix   ! field name suffix
      !
      INTEGER       :: cpl_get_fldid     ! field index
      INTEGER       :: jfld              ! local loop indicees
      CHARACTER(16) :: clprefix          ! 'WRF_d01_EXT_d01_'
      !!--------------------------------------------------------------------
      cpl_get_fldid = -1   ! default value
         
      clprefix = 'WRF_d01_EXT_d01_' 
      DO jfld = 1, max_cplfld
         IF( clprefix//TRIM(cdsuffix) == TRIM(sndname(1,1,jfld)) )   cpl_get_fldid = jfld
         IF( clprefix//TRIM(cdsuffix) == TRIM(rcvname(1,1,jfld)) )   cpl_get_fldid = jfld
      END DO
          
      IF( cpl_get_fldid == -1 )   CALL cpl_abort( 'cpl_get_fldid', 'variable suffix not found '//TRIM(cdsuffix) )
      WRITE(cltxt,*) 'The id of variable'//TRIM(cdsuffix)//' is: ', cpl_get_fldid   ;   CALL wrf_debug(nlevdbg2, cltxt)

   END FUNCTION cpl_get_fldid

   
   SUBROUTINE cpl_snd( grid )
         !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_snd  ***
      !!
      !! ** Purpose : compute coupling data to be sent and call cpl_sndfield
      !!--------------------------------------------------------------------
      TYPE(domain), INTENT(IN), POINTER :: grid
      !
      INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
      INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
      INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
      !!--------------------------------------------------------------------
      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )

#if (EM_CORE == 1)
      CALL cpl_snd2( grid, grid%num_ext_model_couple_dom,   &
         &                 ids, ide, jds, jde, kds, kde,    &
         &                 ims, ime, jms, jme, kms, kme,    &
         &                 ips, ipe, jps, jpe, kps, kpe )
#endif

   END SUBROUTINE cpl_snd


   SUBROUTINE cpl_snd2( grid, max_edom                &
      &                     , ids,ide,jds,jde,kds,kde &
      &                     , ims,ime,jms,jme,kms,kme &
      &                     , ips,ipe,jps,jpe,kps,kpe )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_snd2  ***
      !!
      !! ** Purpose : compute coupling data to be sent and call cpl_sndfield
      !!--------------------------------------------------------------------
      TYPE(domain), INTENT(IN), POINTER :: grid
      INTEGER,      INTENT(IN)          :: max_edom    ! max number of external model domains
      INTEGER,      INTENT(IN)          :: ids,ide,jds,jde,kds,kde
      INTEGER,      INTENT(IN)          :: ims,ime,jms,jme,kms,kme
      INTEGER,      INTENT(IN)          :: ips,ipe,jps,jpe,kps,kpe
      !
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: cplsnd
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: u_uo
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: v_vo
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: wspd
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: taut
      INTEGER :: icnt
      INTEGER :: ifldid
      LOGICAL,DIMENSION(max_edom) :: lltosend
      !!--------------------------------------------------------------------

#if (EM_CORE == 1)

      ! we use ipe and not min(ipe, ide-1) the variable we are using are coming from grid and are therefore initialized to 0  
      
      ifldid      = cpl_get_fldid( 'EVAP-PRECIP' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = grid%QFX(ips:ipe,jps:jpe) &
            &                  - ( grid%RAINCV(ips:ipe,jps:jpe)+grid%RAINNCV(ips:ipe,jps:jpe) ) / grid%DT
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'SURF_NET_SOLAR' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, grid%GSW(ips:ipe,jps:jpe) )
      END IF
      
      ifldid      = cpl_get_fldid( 'SURF_NET_NON-SOLAR' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = grid%GLW(ips:ipe,jps:jpe) &
            &                      - STBOLT * grid%EMISS(ips:ipe,jps:jpe) * grid%SST(ips:ipe,jps:jpe)**4 &
            &                      - grid%LH(ips:ipe,jps:jpe) - grid%HFX(ips:ipe,jps:jpe) 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ! test if we need to compute the module of the wind speed and stres
      icnt   =        COUNT( cpl_tosend( grid%id, cpl_get_fldid( 'TAUMOD' ), max_edom ) )
      icnt   = icnt + COUNT( cpl_tosend( grid%id, cpl_get_fldid( 'TAUX'   ), max_edom ) )
      icnt   = icnt + count( cpl_tosend( grid%id, cpl_get_fldid( 'TAUY'   ), max_edom ) )
      IF ( icnt > 0 ) THEN 
         u_uo(ips:ipe,jps:jpe) = grid%u_phy(ips:ipe,kps,jps:jpe) - grid%uoce(ips:ipe,jps:jpe)
         v_vo(ips:ipe,jps:jpe) = grid%v_phy(ips:ipe,kps,jps:jpe) - grid%voce(ips:ipe,jps:jpe)
         wspd(ips:ipe,jps:jpe) = MAX( SQRT( u_uo(ips:ipe,jps:jpe)**2 + v_vo(ips:ipe,jps:jpe)**2 ), 1.e-7 )
         taut(ips:ipe,jps:jpe) = grid%rho(ips:ipe,kps,jps:jpe) * grid%ust(ips:ipe,jps:jpe)**2
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUX' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = taut(ips:ipe,jps:jpe) * u_uo(ips:ipe,jps:jpe) / wspd(ips:ipe,jps:jpe)
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUY' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = taut(ips:ipe,jps:jpe) * v_vo(ips:ipe,jps:jpe) / wspd(ips:ipe,jps:jpe)
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUMOD' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, taut )
      END IF
      
#endif       
   END SUBROUTINE cpl_snd2


   SUBROUTINE cpl_sndfield( kdomwrf, ldtosend, kfldid, pdata )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_rcv  ***
      !!
      !! ** Purpose :   send coupling data
      !!--------------------------------------------------------------------
      INTEGER,              INTENT(IN) :: kdomwrf   ! wrf domain index
      LOGICAL,DIMENSION(:), INTENT(IN) :: ldtosend
      INTEGER,              INTENT(IN) :: kfldid    ! field index
      REAL, DIMENSION(:,:), INTENT(IN) :: pdata     ! data to be sent
      !
      INTEGER :: jext          ! local loop indicees
      !!--------------------------------------------------------------------

      DO jext = 1, SIZE(ldtosend)
         IF( ldtosend(jext) ) THEN
            IF ( coupler_name == 'oasis' ) CALL cpl_oasis_snd( kdomwrf, jext, kfldid, nsecrun, pdata )
         END IF
      END DO

   END SUBROUTINE cpl_sndfield


   SUBROUTINE cpl_rcv( kdomwrf, cdsuffix,            &
      &                ids, ide, jds, jde, kds, kde, &
      &                ims, ime, jms, jme, kms, kme, &
      &                ips, ipe, jps, jpe, kps, kpe, &
      &                max_edom, pcplmask, pdatacpl, pdataobs )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_rcv  ***
      !!
      !! ** Purpose :   receive coupling data
      !!--------------------------------------------------------------------
      INTEGER,                                                   INTENT(IN   ) :: kdomwrf     ! wrf domain index
      CHARACTER(*),                                              INTENT(IN   ) :: cdsuffix    ! field name suffix
      INTEGER,                                                   INTENT(IN   ) :: ids,ide,jds,jde,kds,kde
      INTEGER,                                                   INTENT(IN   ) :: ims,ime,jms,jme,kms,kme
      INTEGER,                                                   INTENT(IN   ) :: ips,ipe,jps,jpe,kps,kpe
      INTEGER,                                                   INTENT(IN   ) :: max_edom    ! max number of external model domains
      REAL, DIMENSION( ims:ime, 1:max_edom, jms:jme ),           INTENT(IN   ) :: pcplmask    ! coupling mask
      REAL, DIMENSION( ims:ime,             jms:jme ),           INTENT(  OUT) :: pdatacpl    ! coupling data
      REAL, DIMENSION( ims:ime,             jms:jme ), OPTIONAL, INTENT(IN   ) :: pdataobs    ! observed data to be merged
      !
      INTEGER :: jext                                ! external domain index
      INTEGER :: ifldid                              ! field index
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: zdata   ! data received from the coupler
      !!--------------------------------------------------------------------

      ifldid = cpl_get_fldid( cdsuffix )
         
      IF( PRESENT(pdataobs) ) THEN
         pdatacpl(ips:ipe,jps:jpe) = pdataobs(ips:ipe,jps:jpe) * ( 1.0 - SUM( pcplmask(ips:ipe,1:max_edom,jps:jpe), dim = 2 ) )
      ELSE 
         pdatacpl(ips:ipe,jps:jpe) = 0.0
      END IF

      DO jext = 1, max_edom
         IF( cpl_toreceive( kdomwrf, jext, ifldid ) ) THEN
            IF( coupler_name == 'oasis' )   CALL cpl_oasis_rcv( kdomwrf, jext, ifldid, nsecrun, zdata )
            pdatacpl(ips:ipe,jps:jpe) = pdatacpl(ips:ipe,jps:jpe) + zdata(ips:ipe,jps:jpe) * pcplmask(ips:ipe,jext,jps:jpe)
         END IF
      END DO

   END SUBROUTINE cpl_rcv


   SUBROUTINE cpl_store_input( grid, config_flags )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_store_input  ***
      !!
      !! ** Purpose : Store input data that will be merged later with data received from the coupler
      !!--------------------------------------------------------------------
      TYPE(domain)                , INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type) , INTENT(IN   ) :: config_flags
      !
      INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
      INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
      INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
      LOGICAL :: llmust_store
      INTEGER :: jext          ! local loop indicees     
      !!--------------------------------------------------------------------

#if (EM_CORE == 1)
      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )
      
      ! take care of variables read in AUXINPUT4... 
      ! AUXINPUT4 was just read if:
      ! 1) We asked (legally) for an AUXINPUT4 input AND this is the first time step AFTER an auxinput4_alarm was ringing
      ! OR
      ! 2) This is the first time step
      IF( ( config_flags%auxinput4_interval .NE. 0 .AND. config_flags%io_form_auxinput4 .NE. 0 .AND. grid%just_read_auxinput4 ) &
         .OR. grid%itimestep .EQ. 1 ) THEN
         
         ! if we receive the SST, we need to store it in SST_INPUT
         llmust_store = .FALSE.
         DO jext = 1, grid%num_ext_model_couple_dom
            llmust_store = llmust_store .OR. cpl_toreceive( grid%id, jext, cpl_get_fldid( 'SST' ) )
         END DO
         IF( llmust_store )   grid%sst_input(ips:ipe,jps:jpe) = grid%sst(ips:ipe,jps:jpe)   ! store SST into SST_INPUT 
         
         grid%just_read_auxinput4 = .FALSE.  ! the work as been done and not me done again until we reread data from AUXINPUT4
      
      END IF
#endif      
 
   END SUBROUTINE cpl_store_input


   SUBROUTINE cpl_finalize()
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_finalize  ***
      !!
      !! ** Purpose :   cpl_finalize MPI communications with the coupler
      !!--------------------------------------------------------------------
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_finalize()

   END SUBROUTINE cpl_finalize


   SUBROUTINE cpl_abort( cdroutine, cdtxt )
      !!-------------------------------------------------------------------
      !!             ***  SUBROUTINE cpl_abort  ***
      !!
      !! ** Purpose :   abort coupling simulation
      !!--------------------------------------------------------------------
      CHARACTER(*), INTENT(IN) :: cdroutine   ! name of the subroutine calling cpl_oasis_abort
      CHARACTER(*), INTENT(IN) :: cdtxt       ! aborting text
      !!--------------------------------------------------------------------

      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_abort( cdroutine, cdtxt )

   END SUBROUTINE cpl_abort


END MODULE module_cpl
