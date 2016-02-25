 MODULE module_cpl_oasis3
#ifdef key_cpp_oasis3
   !!======================================================================
   !!                    ***  MODULE cpl_oasis  ***
   !! Coupled O/A : coupled ocean-atmosphere case using OASIS3-MCT
   !!=====================================================================
   !!----------------------------------------------------------------------
   !!   cpl_oasis_init     : initialization of coupled mode communication
   !!   cpl_oasis_define   : definition of grid and fields
   !!   cpl_oasis_snd      : send out fields in coupled mode
   !!   cpl_oasis_rcv      : receive fields in coupled mode
   !!   cpl_oasis_finaliz : finalize the coupled mode communication
   !!----------------------------------------------------------------------

   USE module_domain          , ONLY : domain, get_ijk_from_grid
   USE module_driver_constants, ONLY : max_domains, max_cplfld, max_extdomains
   USE mod_oasis              ! OASIS3-MCT module

   IMPLICIT NONE
   PRIVATE

   TYPE ::   FLD_CPL                                  ! Coupling field information
      CHARACTER(len = 64) ::   clname                    ! Name of the coupling field, jpeighty defined in oasis
      INTEGER             ::   nid                       ! Id of the field
#if ( RWORDSIZE == 8 )
      REAL        , POINTER, DIMENSION(:,:) ::   dbl2d   ! 2d array to store received field 
#else
      REAL(kind=8), POINTER, DIMENSION(:,:) ::   dbl2d   ! 2d array to store received field 
#endif
   END TYPE FLD_CPL
   TYPE(FLD_CPL), DIMENSION(max_domains,max_extdomains,max_cplfld) :: srcv, ssnd   ! Coupling fields informations
   INTEGER :: ndm_comm                                                         ! MPI communicator between the computing nodes
   INTEGER :: ncomp_id                                                         ! id returned by oasis_init_comp
   INTEGER :: nlevdbg  = 1                                                     ! verbosity level
   INTEGER :: nlevdbg2 = 10                                                    ! verbosity level
   CHARACTER(len = 256) :: cltxt                                               ! messages or debug string

   !! Routine accessibility
   PUBLIC cpl_oasis_init
   PUBLIC cpl_oasis_def_dmcomm
   PUBLIC cpl_oasis_define
   PUBLIC cpl_oasis_toreceive
   PUBLIC cpl_oasis_tosend
   PUBLIC cpl_oasis_snd
   PUBLIC cpl_oasis_rcv
   PUBLIC cpl_oasis_finalize
   PUBLIC cpl_oasis_abort

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
   INCLUDE 'mpif.h'               ! only for MPI_COMM_NULL
#else
   INTEGER :: MPI_COMM_NULL = -1  ! define a fake (and not used) MPI_COMM_NULL, so it is compiling 
#endif

CONTAINS

   SUBROUTINE cpl_oasis_init( kl_comm ) 
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_oasis_init  ***
      !!
      !! ** Purpose :   Initialize coupled mode communication for WRF
      !!--------------------------------------------------------------------
      INTEGER, INTENT(OUT) :: kl_comm       ! local communicator of the model
      !
      INTEGER :: ierror   ! return error code
      !!--------------------------------------------------------------------

      ! Initialize OASIS for the application
      CALL oasis_init_comp( ncomp_id, 'wrfexe', ierror )
      IF( ierror /= OASIS_Ok )   CALL cpl_oasis_abort( 'cpl_oasis_init', 'Failure in oasis_init_comp' )

      ! Get an MPI communicator for WRF local communication
      CALL oasis_get_localcomm( kl_comm, ierror )
      IF( ierror /= OASIS_Ok )   CALL cpl_oasis_abort( 'cpl_oasis_init','Failure in oasis_get_localcomm' )

      srcv(:,:,:)%nid = -1            ! default definition
      ssnd(:,:,:)%nid = -1            ! default definition
      ndm_comm        = MPI_COMM_NULL ! default definition, will be redefined by cpl_oasis_def_dmcomm if computing node

   END SUBROUTINE cpl_oasis_init


   SUBROUTINE cpl_oasis_def_dmcomm( kdm_comm ) 
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_oasis_def_dmcomm  ***
      !!
      !! ** Purpose :   define ndm_comm: the MPI communicator between the computing nodes
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdm_comm       ! computing nodes communicator
      !!--------------------------------------------------------------------
      ndm_comm = kdm_comm   ! store it to used it in cpl_oasis_define

      WRITE(cltxt,*) 'cpl_oasis_def_dmcomm : ', kdm_comm
      CALL wrf_debug(nlevdbg, cltxt)
      CALL wrf_debug(nlevdbg, '~~~~~~~~~~~~~~~~~~~~~~~')

   END SUBROUTINE cpl_oasis_def_dmcomm


   SUBROUTINE cpl_oasis_define( cdsndname, cdrcvname, pgrid )
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_oasis_define  ***
      !!
      !! ** Purpose :   Define grid and coupling field information for WRF
      !!--------------------------------------------------------------------
      CHARACTER(*), INTENT(IN), DIMENSION(:,:,:)  :: cdsndname, cdrcvname   ! coupling field names
      TYPE(domain), INTENT(IN), OPTIONAL, POINTER :: pgrid                  ! grid structure
      !
      INTEGER :: ierror                  ! return error code
      INTEGER :: idwrf1,idwrf2           ! loop index over wrf domain number (start and end)
      INTEGER :: idext1,idext2           ! loop index over external model domain number (start and end)
      INTEGER :: id_part                 ! partition id in oasis
      INTEGER :: iparal(5)               ! OASIS box partition
      INTEGER :: ishape(2,2)             ! shape of arrays passed to PSMILe
      INTEGER :: jw,je,jf                ! local loop indicees
      INTEGER :: ips,ipe,jps,jpe,kps,kpe ! domain dimension on 1 processor
      INTEGER :: ims,ime,jms,jme,kms,kme ! memory domain dimension on 1 processor 
      INTEGER :: ids,ide,jds,jde,kds,kde ! domain dimension
      LOGICAL :: llcompute_core          ! is it a compiting core?
      !!--------------------------------------------------------------------

      CALL wrf_message('cpl_oasis_define : initialization in coupled ocean/atmosphere case')
      CALL wrf_debug(nlevdbg, '~~~~~~~~~~~~~~~~~~~~~~~')

      llcompute_core = PRESENT(pgrid)

      ! -----------------------------------------------------------------
      ! ... Define communicator used between computing cores      
      CALL oasis_set_couplcomm( ndm_comm, ierror )               ! provide this communicator to OASIS3-MCT
      IF ( ierror /= OASIS_Ok ) CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in oasis_set_couplcomm')
      
      ! -----------------------------------------------------------------
      ! ... Define the partition 
      ! -----------------------------------------------------------------
      IF( llcompute_core ) THEN
         
         ! ... get mpi domain position
         CALL get_ijk_from_grid( pgrid, ids, ide, jds, jde, kds, kde, &
            &                           ims, ime, jms, jme, kms, kme, &
            &                           ips, ipe, jps, jpe, kps, kpe  )

         ishape(:,1) = (/1, ipe-ips+1 /)
         ishape(:,2) = (/1, jpe-jps+1 /)
         
         ! ... Define the partition parameteres
         iparal(1) = 2                                      ! box partitioning
         iparal(2) = ide * ( jps - 1 ) + (ips -1)
         iparal(3) = ipe - ips + 1                          ! local extent in i 
         iparal(4) = jpe - jps + 1                          ! local extent in j
         iparal(5) = ide                                    ! global extent in x
      
         WRITE(cltxt,*) 'Define the partition for computing cores'   ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: iparal (1:5)', iparal          ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: ips, ipe =', ips, ipe          ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: jps, jpe =', jps, jpe          ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: ids, jds =', ids, jds          ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: ide, jde =', ide, jde          ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: ishape(:,1) =', ishape(:,1)    ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) ' multiexchg: ishape(:,2) =', ishape(:,2)    ;   CALL wrf_debug(nlevdbg, cltxt)

      ELSE
         CALL wrf_debug(nlevdbg, 'no partition for IO cores')
         iparal(:) = 0   ! "fake" partition for IO cores
      ENDIF
      
      CALL oasis_def_partition( id_part, iparal, ierror )
      IF( ierror /= OASIS_Ok ) CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in oasis_def_partition')

      ! -----------------------------------------------------------------
      ! ... Define the variables that can be send/received by WRF
      ! -----------------------------------------------------------------
      IF( llcompute_core ) THEN   ;   idwrf1 = pgrid%id   ;   idwrf2 = pgrid%id      ! coupling field related to this nest
      ELSE                        ;   idwrf1 = 1          ;   idwrf2 = max_domains   ! define all (dummy) coupling fields
      ENDIF

      ! -----------------------------------------------------------------
      ! ... Define sent variables. 
      ! -----------------------------------------------------------------
      DO jf = 1, max_cplfld
         DO je = 1, max_extdomains
            DO jw = idwrf1, idwrf2
               ssnd(jw,je,jf)%clname = TRIM(cdsndname(jw,je,jf))
               CALL oasis_def_var(ssnd(jw,je,jf)%nid, ssnd(jw,je,jf)%clname, id_part, (/2,1/), OASIS_Out, ishape, OASIS_Real,ierror)
               IF( ierror /= OASIS_Ok ) THEN
                  WRITE(cltxt,*) 'wrf domain ',jw,' external model domain ',je,   &
                     ' field ',jf,' (',TRIM(ssnd(jw,je,jf)%clname),'): oasis_def_var failed'
                  CALL wrf_message( cltxt )
                  CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in oasis_def_var')
               ENDIF
               WRITE(cltxt,*) 'cpl_oasis_define ok for :', TRIM(ssnd(jw,je,jf)%clname), ssnd(jw,je,jf)%nid 
               CALL wrf_debug(nlevdbg2, cltxt)
               IF( ssnd(jw,je,jf)%nid /= -1 ) THEN 
                  WRITE(cltxt,*) ' var snd: ', ssnd(jw,je,jf)%nid, ' ', TRIM(ssnd(jw,je,jf)%clname), id_part 
                  CALL wrf_debug(nlevdbg, cltxt)
               ENDIF
            END DO
         END DO
      END DO
         
      ! -----------------------------------------------------------------
      ! ... Define received variables. 
      ! -----------------------------------------------------------------
      DO jf = 1, max_cplfld
         DO je = 1, max_extdomains
            DO jw = idwrf1, idwrf2
               srcv(jw,je,jf)%clname = TRIM(cdrcvname(jw,je,jf))
               CALL oasis_def_var(srcv(jw,je,jf)%nid, srcv(jw,je,jf)%clname, id_part, (/2,1/), OASIS_In , ishape, OASIS_Real,ierror)
               IF( ierror /= OASIS_Ok ) THEN
                  WRITE(cltxt,*) 'wrf domain ',jw,' external model domain ',je,   &
                     ' field ',jf,' (',TRIM(srcv(jw,je,jf)%clname),'): oasis_def_var failed'
                  CALL wrf_message( cltxt )
                  CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in oasis_def_var')
               ENDIF
               WRITE(cltxt,*) 'cpl_oasis_define ok for :', TRIM(srcv(jw,je,jf)%clname), srcv(jw,je,jf)%nid
               CALL wrf_debug(nlevdbg2, cltxt)
               IF( srcv(jw,je,jf)%nid /= -1 ) THEN
                  WRITE(cltxt,*) ' var rcv: ', srcv(jw,je,jf)%nid, ' ', TRIM(srcv(jw,je,jf)%clname), id_part
                  CALL wrf_debug(nlevdbg, cltxt)
               END IF
               IF( srcv(jw,je,jf)%nid /= -1 .AND. llcompute_core ) THEN   ! allocate received array
                  ALLOCATE( srcv(jw,je,jf)%dbl2d( iparal(3), iparal(4) ), stat = ierror)
                  IF (ierror > 0)   CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in allocating srcv')
               END IF
            END DO
         END DO
      END DO

      ! -----------------------------------------------------------------
      ! ... End definition
      ! -----------------------------------------------------------------
      IF (llcompute_core) THEN 
         IF ( pgrid%id == pgrid%max_dom ) CALL cpl_oasis_enddef()
         CALL wrf_message('cpl_oasis_define (compute_core) : cpl_oasis_enddef done')
      ELSE
         CALL cpl_oasis_enddef()
         CALL wrf_message('cpl_oasis_define (io_core) : cpl_oasis_enddef done')
      ENDIF
      
   END SUBROUTINE cpl_oasis_define
   
   
   SUBROUTINE cpl_oasis_enddef()
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_oasis_enddef  ***
      !!
      !! ** Purpose :   tells to OASIS that exchanged field definition is finished
      !!--------------------------------------------------------------------
      INTEGER :: ierror         ! return error code
      CALL oasis_enddef(ierror)
      IF( ierror /= OASIS_Ok )   CALL cpl_oasis_abort( 'cpl_oasis_define', 'Failure in oasis_enddef')
      
   END SUBROUTINE cpl_oasis_enddef
   
   
   FUNCTION cpl_oasis_toreceive( kdomwrf, kdomext, kfldid )
      !!-------------------------------------------------------------------
      !!             ***  FUNCTION cpl_oasis_toreceive  ***
      !!
      !! ** Purpose :   send back a logical to tell if a variable is received or not
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kdomext   ! external model domain index
      INTEGER, INTENT(IN) :: kfldid    ! field index
      !
      LOGICAL :: cpl_oasis_toreceive
      !!--------------------------------------------------------------------
      
      cpl_oasis_toreceive = srcv(kdomwrf,kdomext,kfldid)%nid /= -1

   END FUNCTION cpl_oasis_toreceive


   FUNCTION cpl_oasis_tosend( kdomwrf, kdomext, kfldid )
      !!-------------------------------------------------------------------
      !!             ***  FUNCTION cpl_oasis_tosend  ***
      !!
      !! ** Purpose :   send back a logical to tell if a variable is tosend or not
      !!--------------------------------------------------------------------
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kdomext   ! external model domain index
      INTEGER, INTENT(IN) :: kfldid    ! field index
      !
      LOGICAL :: cpl_oasis_tosend
      !!--------------------------------------------------------------------
      
      cpl_oasis_tosend = ssnd(kdomwrf,kdomext,kfldid)%nid /= -1

   END FUNCTION cpl_oasis_tosend


   SUBROUTINE cpl_oasis_snd( kdomwrf, kdomext, kfldid, ksec, pdata )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_oasis_snd  ***
      !!
      !! ** Purpose : - At each coupling time-step,this routine sends fields to the coupler
      !!----------------------------------------------------------------------
      INTEGER,              INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER,              INTENT(IN) :: kdomext   ! external model domain index
      INTEGER,              INTENT(IN) :: kfldid    ! field index
      INTEGER,              INTENT(IN) :: ksec      ! time-step in seconds
      REAL, DIMENSION(:,:), INTENT(IN) :: pdata     ! data to be sent
      !!
      INTEGER :: info              ! OASIS3 info argument
      LOGICAL :: llaction          ! true if we sent data to the coupler
      !!--------------------------------------------------------------------
      !
      WRITE(cltxt,*) 'OASIS_PUT in: kdomwrf, kdomext, kfldid, name, ksec', &
         kdomwrf, kdomext, kfldid, ' ', TRIM(ssnd(kdomwrf,kdomext,kfldid)%clname), ksec
      CALL wrf_debug(nlevdbg, cltxt)

#if ( RWORDSIZE == 8 )
      CALL oasis_put(ssnd(kdomwrf,kdomext,kfldid)%nid, ksec,      pdata(:,:) , info)
#else
      CALL oasis_put(ssnd(kdomwrf,kdomext,kfldid)%nid, ksec, DBLE(pdata(:,:)), info)
#endif
      
      WRITE(cltxt,*) 'OASIS_PUT out: info', info   ;   CALL wrf_debug(nlevdbg, cltxt)

      llaction = info == OASIS_Sent     .OR. info == OASIS_ToRest .OR.   &
         &       info == OASIS_SentOut  .OR. info == OASIS_ToRestOut 

      WRITE(cltxt,*) "llaction : ", llaction       ;   CALL wrf_debug(nlevdbg, cltxt)

      IF( llaction ) THEN
         WRITE(cltxt,*) '****************'                                                  ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) 'oasis_put: Incoming ', TRIM(ssnd(kdomwrf,kdomext,kfldid)%clname)   ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) 'oasis_put:    varid ', ssnd(kdomwrf,kdomext,kfldid)%nid            ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) 'oasis_put:     ksec ', ksec                                        ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) 'oasis_put:     info ', info                                        ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) '    - shape         ', SHAPE(pdata)                                ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) '    - minimum       ', MINVAL(pdata)                               ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) '    - maximum       ', MAXVAL(pdata)                               ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) '    - sum           ', SUM(pdata)                                  ;   CALL wrf_debug(nlevdbg, cltxt)
         WRITE(cltxt,*) '****************'                                                  ;   CALL wrf_debug(nlevdbg, cltxt)
      ELSE
         WRITE(cltxt,*) 'field not sent as info=', info                                     ;   CALL wrf_debug(nlevdbg, cltxt)
      ENDIF
      
    END SUBROUTINE cpl_oasis_snd


   SUBROUTINE cpl_oasis_rcv( kdomwrf, kdomext, kfldid, ksec, pcplrcv )

      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_oasis_rcv  ***
      !!
      !! ** Purpose : - At each coupling time-step, this routine check if it is the good time
      !!                to receive field from the coupler
      !!----------------------------------------------------------------------
      INTEGER,              INTENT(IN   ) :: kdomwrf   ! wrf domain index
      INTEGER,              INTENT(IN   ) :: kdomext   ! external model domain index
      INTEGER,              INTENT(IN   ) :: kfldid    ! variable index
      INTEGER,              INTENT(IN   ) :: ksec      ! number of seconds since the last restart
      REAL, DIMENSION(:,:), INTENT(  OUT) :: pcplrcv   ! output data
      !!
      INTEGER :: info              ! OASIS3 info argument
      LOGICAL :: llaction          ! true if we received data from the coupler
      !!--------------------------------------------------------------------
      !
      WRITE(cltxt,*) 'OASIS_GET in: kdomwrf, kdomext, kfldid, name, ksec', &
         kdomwrf, kdomext, kfldid, ' ', TRIM(srcv(kdomwrf,kdomext,kfldid)%clname), ksec
      CALL wrf_debug(nlevdbg, cltxt)

      CALL oasis_get( srcv(kdomwrf,kdomext,kfldid)%nid, ksec, srcv(kdomwrf,kdomext,kfldid)%dbl2d, info )
#if ( RWORDSIZE == 8 )
      pcplrcv(:,:) =      srcv(kdomwrf,kdomext,kfldid)%dbl2d
#else
      pcplrcv(:,:) = REAL(srcv(kdomwrf,kdomext,kfldid)%dbl2d, kind=4)
#endif

      WRITE(cltxt,*) 'OASIS_GET out: info', info   ;   CALL wrf_debug(nlevdbg, cltxt)

      llaction = info == OASIS_Recvd   .OR. info == OASIS_FromRest .OR.   &
         &       info == OASIS_RecvOut .OR. info == OASIS_FromRestOut 

      WRITE(cltxt,*) "llaction : ", llaction       ;   CALL wrf_debug(nlevdbg, cltxt)

      IF( llaction ) THEN
            WRITE(cltxt,*) '****************'                                                   ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) 'oasis_get: Incoming ', TRIM(srcv(kdomwrf,kdomext,kfldid)%clname)    ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) 'oasis_get:    varid ', srcv(kdomwrf,kdomext,kfldid)%nid             ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) 'oasis_get:     ksec ', ksec                                         ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) 'oasis_get:     info ', info                                         ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - shape         ', SHAPE(pcplrcv)                               ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local shape   ', SHAPE(srcv(kdomwrf, kdomext,kfldid)%dbl2d)   ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local minimum ', MINVAL(pcplrcv)                              ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local maximum ', MAXVAL(pcplrcv)                              ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local sum     ', SUM(pcplrcv)                                 ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '****************'                                                   ;   CALL wrf_debug(nlevdbg, cltxt)
      ELSE
            WRITE(cltxt,*) '****************'                                                   ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) 'oasis_get: field not received as info = ', info                     ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local minimum ', MINVAL(pcplrcv)                              ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local maximum ', MAXVAL(pcplrcv)                              ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '    - local sum     ', SUM(pcplrcv)                                 ;   CALL wrf_debug(nlevdbg, cltxt)
            WRITE(cltxt,*) '****************'                                                   ;   CALL wrf_debug(nlevdbg, cltxt)
      ENDIF

   END SUBROUTINE cpl_oasis_rcv


   SUBROUTINE cpl_oasis_finalize()
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_oasis_finalize  ***
      !!
      !! ** Purpose : - Finalizes the coupling. If MPI_init has not been
      !!      called explicitly before cpl_oasis_init it will also close
      !!      MPI communication.
      !!----------------------------------------------------------------------
      INTEGER :: ierror         ! return error code
      INTEGER :: jw,je,jf       ! local loop indicees
      !!--------------------------------------------------------------------
      DO jf = 1, max_cplfld
         DO je = 1, max_extdomains
            DO jw = 1, max_domains
               ierror = 0
               IF ( ASSOCIATED(srcv(jw,je,jf)%dbl2d) ) DEALLOCATE( srcv(jw,je,jf)%dbl2d, stat = ierror )
               IF (ierror > 0) THEN
                  CALL cpl_oasis_abort( 'cpl_oasis_finalize', 'Failure in deallocating ')
                  RETURN
               ENDIF
            END DO
         END DO
      END DO
      CALL oasis_terminate ( ierror )         

   END SUBROUTINE cpl_oasis_finalize


   SUBROUTINE cpl_oasis_abort( cdroutine, cdtxt )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_oasis_abort  ***
      !!
      !! ** Purpose :   abort coupling simulation
      !!----------------------------------------------------------------------
      CHARACTER(*), INTENT(IN) :: cdroutine   ! name of the subroutine calling cpl_oasis_abort
      CHARACTER(*), INTENT(IN) :: cdtxt       ! aborting text
      !!--------------------------------------------------------------------

      CALL wrf_message( '     ==== ABORTING ====' )
      CALL wrf_message( 'cpl_abort called by '//TRIM(cdroutine) )
      CALL wrf_message( '   ==> '//TRIM(cdtxt) )
      CALL oasis_abort( ncomp_id, cdroutine, cdtxt )
      
   END SUBROUTINE cpl_oasis_abort
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy modules just for compilation...
   !!----------------------------------------------------------------------
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   PRIVATE
   PUBLIC cpl_oasis_init
   PUBLIC cpl_oasis_def_dmcomm
   PUBLIC cpl_oasis_define
   PUBLIC cpl_oasis_toreceive
   PUBLIC cpl_oasis_tosend
   PUBLIC cpl_oasis_snd
   PUBLIC cpl_oasis_rcv
   PUBLIC cpl_oasis_finalize
   PUBLIC cpl_oasis_abort

CONTAINS

   SUBROUTINE cpl_oasis_init( kl_comm ) 
      INTEGER, INTENT(OUT) :: kl_comm       ! local communicator of the model
      IF (.FALSE.) kl_comm = -1 ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_init

   SUBROUTINE cpl_oasis_def_dmcomm( kdm_comm ) 
      INTEGER, INTENT(IN) :: kdm_comm       ! computing nodes communicator
      IF (.FALSE.) WRITE(*,*) kdm_comm ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_def_dmcomm

   SUBROUTINE cpl_oasis_define( cdsndname, cdrcvname, pgrid )
      CHARACTER(*), INTENT(IN), DIMENSION(:,:,:)  :: cdsndname, cdrcvname   ! coupling field names
      TYPE(domain), INTENT(IN), OPTIONAL, POINTER :: pgrid                  ! grid structure
      IF (.FALSE.) WRITE(*,*) cdsndname, cdrcvname, pgrid%id  ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_define

   FUNCTION cpl_oasis_toreceive( kdomwrf, kdomext, kfldid )
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kdomext   ! external model domain index
      INTEGER, INTENT(IN) :: kfldid    ! field index
      LOGICAL :: cpl_oasis_toreceive
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid  ! to avoid compilation warning
      IF (.FALSE.) cpl_oasis_toreceive = .false.  ! to avoid compilation warning
   END FUNCTION cpl_oasis_toreceive

   FUNCTION cpl_oasis_tosend( kdomwrf, kdomext, kfldid )
      INTEGER, INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER, INTENT(IN) :: kdomext   ! external model domain index
      INTEGER, INTENT(IN) :: kfldid    ! field index
      LOGICAL :: cpl_oasis_tosend
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid  ! to avoid compilation warning
      IF (.FALSE.) cpl_oasis_tosend = .false.  ! to avoid compilation warning
   END FUNCTION cpl_oasis_tosend

   SUBROUTINE cpl_oasis_snd( kdomwrf, kdomext, kfldid, ksec, pdata )
      !!----------------------------------------------------------------------
      INTEGER,              INTENT(IN) :: kdomwrf   ! wrf domain index
      INTEGER,              INTENT(IN) :: kdomext   ! external model domain index
      INTEGER,              INTENT(IN) :: kfldid    ! field index
      INTEGER,              INTENT(IN) :: ksec      ! time-step in seconds
      REAL, DIMENSION(:,:), INTENT(IN) :: pdata     ! data to be sent
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid, ksec, pdata ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_snd

   SUBROUTINE cpl_oasis_rcv( kdomwrf, kdomext, kfldid, ksec, pcplrcv )
      INTEGER,              INTENT(IN   ) :: kdomwrf   ! wrf domain index
      INTEGER,              INTENT(IN   ) :: kdomext   ! external model domain index
      INTEGER,              INTENT(IN   ) :: kfldid    ! variable index
      INTEGER,              INTENT(IN   ) :: ksec      ! number of seconds since the last restart
      REAL, DIMENSION(:,:), INTENT(  OUT) :: pcplrcv   ! output data
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid, ksec ! to avoid compilation warning
      IF (.FALSE.) pcplrcv(:,:) = -1. ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_rcv

   SUBROUTINE cpl_oasis_finalize()
      IF (.FALSE.) WRITE(*,*) 'You should not be there...'
   END SUBROUTINE cpl_oasis_finalize

   SUBROUTINE cpl_oasis_abort( cdroutine, cdtxt )
      CHARACTER(*), INTENT(IN) :: cdroutine   ! name of the subroutine calling cpl_oasis_abort
      CHARACTER(*), INTENT(IN) :: cdtxt       ! aborting text
      IF (.FALSE.) WRITE(*,*) cdroutine, cdtxt   ! to avoid compilation warning
   END SUBROUTINE cpl_oasis_abort
#endif

END MODULE module_cpl_oasis3
