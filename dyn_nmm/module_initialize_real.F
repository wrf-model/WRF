!REAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize_real

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
!   USE module_si_io_nmm
   USE module_state_description
   USE module_timing
   USE module_soil_pre
#ifdef DM_PARALLEL
   USE module_dm
   USE module_ext_internal
#endif

   INTEGER :: internal_time_loop
      INTEGER:: MPI_COMM_COMP,MYPE

CONTAINS

!-------------------------------------------------------------------

   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

      !  Local data.

      INTEGER :: idum1, idum2

      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

        CALL init_domain_nmm (grid &
!
#include <actual_args.inc>
!
      )

   END SUBROUTINE init_domain

!-------------------------------------------------------------------
!---------------------------------------------------------------------
   SUBROUTINE init_domain_nmm ( grid &
!
# include <dummy_args.inc>
!
   )

      USE module_optional_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

# include <dummy_decl.inc>

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, NNXP, NNYP

      !  Local data

        CHARACTER(LEN=19):: start_date

#ifdef DM_PARALLEL

        LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR

!              INTEGER :: DOMDESC
              REAL,ALLOCATABLE    :: SICE_G(:,:), SM_G(:,:)
              INTEGER, ALLOCATABLE::  IHE_G(:),IHW_G(:)
              INTEGER, ALLOCATABLE::  ITEMP(:,:)
              INTEGER :: my_e,my_n,my_s,my_w,my_ne,my_nw,my_se,my_sw,myi,myj,npe
              INTEGER :: istat,inpes,jnpes
#endif


      CHARACTER (LEN=255) :: message

      INTEGER :: error
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a
      REAL    :: hold_znw, rmin,rmax

      REAL :: p_top_requested , ptsgm
      INTEGER :: num_metgrid_levels, ICOUNT
      REAL , DIMENSION(max_eta) :: eta_levels

      LOGICAL :: stretch_grid, dry_sounding, debug, log_flag_sst, hyb_coor

        REAL, ALLOCATABLE,DIMENSION(:,:):: ADUM2D,SNOWC,HT,TG_ALT, &
                                           PDVP,PSFC_OUTV

        REAL, ALLOCATABLE,DIMENSION(:,:,:):: P3D_OUT,P3DV_OUT,P3DV_IN, &
                                             QTMP,QTMP2

        INTEGER, ALLOCATABLE, DIMENSION(:):: KHL2,KVL2,KHH2,KVH2, &
                                             KHLA,KHHA,KVLA,KVHA

!        INTEGER, ALLOCATABLE, DIMENSION(:,:):: LU_INDEX

        REAL, ALLOCATABLE, DIMENSION(:):: DXJ,WPDARJ,CPGFUJ,CURVJ, &
                                          FCPJ,FDIVJ,EMJ,EMTJ,FADJ, &
                                          HDACJ,DDMPUJ,DDMPVJ

        REAL, ALLOCATABLE,DIMENSION(:),SAVE:: SG1,SG2,DSG1,DSG2, &
                                              SGML1,SGML2

!-- Carsel and Parrish [1988]
         REAL , DIMENSION(100) :: lqmi
         integer iicount

        REAL:: TPH0D,TLM0D
        REAL:: TPH0,WB,SB,TDLM,TDPH
        REAL:: WBI,SBI,EBI,ANBI,STPH0,CTPH0
        REAL:: TSPH,DTAD,DTCF
        REAL:: ACDT,CDDAMP,DXP,FP
        REAL:: WBD,SBD
        REAL:: RSNOW,SNOFAC
        REAL, PARAMETER:: SALP=2.60
        REAL, PARAMETER:: SNUP=0.040
        REAL:: SMCSUM,STCSUM,SEAICESUM,FISX
        REAL:: cur_smc, aposs_smc

        INTEGER,PARAMETER:: DOUBLE=SELECTED_REAL_KIND(15,300)
        REAL(KIND=DOUBLE):: TERM1,APH,TLM,TPH,DLM,DPH,STPH,CTPH

        INTEGER:: KHH,KVH,JAM,JA, IHL, IHH, L
        INTEGER:: II,JJ,ISRCH,ISUM,ITER,Ilook,Jlook

        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: W_NMM=0.08
        REAL, PARAMETER:: COAC=1.6
        REAL, PARAMETER:: CODAMP=6.4
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: CP=1004.6
        REAL, PARAMETER:: DFC=1.0
        REAL, PARAMETER:: DDFC=8.0
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

        if (ALLOCATED(ADUM2D)) DEALLOCATE(ADUM2D)
        if (ALLOCATED(TG_ALT)) DEALLOCATE(TG_ALT)
       IF ( CONFIG_FLAGS%FRACTIONAL_SEAICE == 1 ) THEN
          CALL WRF_ERROR_FATAL("NMM cannot use FRACTIONAL_SEAICE = 1")
       ENDIF

      if ( config_flags%bl_pbl_physics == BOULACSCHEME ) then
         call wrf_error_fatal("Cannot use BOULAC PBL with NMM")
      endif

#define COPY_IN
#include <scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <data_calls.inc>
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

#ifdef DM_PARALLEL
      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      call wrf_get_nprocx(inpes)
      call wrf_get_nprocy(jnpes)
!
      allocate(itemp(inpes,jnpes),stat=istat)
      npe=0
!
      do j=1,jnpes
      do i=1,inpes
        itemp(i,j)=npe
        if(npe==mype)then
          myi=i
          myj=j
        endif
        npe=npe+1
      enddo
      enddo
!
      my_n=-1
      if(myj+1<=jnpes)my_n=itemp(myi,myj+1)
!
      my_e=-1
      if(myi+1<=inpes)my_e=itemp(myi+1,myj)
!
      my_s=-1
      if(myj-1>=1)my_s=itemp(myi,myj-1)
!
      my_w=-1
      if(myi-1>=1)my_w=itemp(myi-1,myj)
!
      my_ne=-1
      if((myi+1<=inpes).and.(myj+1<=jnpes)) &
         my_ne=itemp(myi+1,myj+1)
!
      my_se=-1
      if((myi+1<=inpes).and.(myj-1>=1)) &
         my_se=itemp(myi+1,myj-1)
!
      my_sw=-1
      if((myi-1>=1).and.(myj-1>=1)) &
         my_sw=itemp(myi-1,myj-1)
!
      my_nw=-1
      if((myi-1>=1).and.(myj+1<=jnpes)) &
         my_nw=itemp(myi-1,myj+1)
!
!     my_neb(1)=my_n
!     my_neb(2)=my_e
!     my_neb(3)=my_s
!     my_neb(4)=my_w
!     my_neb(5)=my_ne
!     my_neb(6)=my_se
!     my_neb(7)=my_sw
!     my_neb(8)=my_nw
!
      deallocate(itemp)
#endif

        grid%DT=float(grid%TIME_STEP)

        NNXP=min(ITE,IDE-1)
        NNYP=min(JTE,JDE-1)

        write(message,*) 'IDE, JDE: ', IDE,JDE
        write(message,*) 'NNXP, NNYP: ', NNXP,NNYP
        CALL wrf_message(message)

        JAM=6+2*(JDE-JDS-10)

        if (internal_time_loop .eq. 1) then
          ALLOCATE(ADUM2D(grid%sm31:grid%em31,jms:jme))
          ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
          ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
          ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),&
                   FADJ(JTS:NNYP))
          ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
          ALLOCATE(KHLA(JAM),KHHA(JAM))
          ALLOCATE(KVLA(JAM),KVHA(JAM))
        endif


    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

        write(message,*) 'cen_lat: ', config_flags%cen_lat
        CALL wrf_debug(100,message)
        write(message,*) 'cen_lon: ', config_flags%cen_lon
        CALL wrf_debug(100,message)
        write(message,*) 'dx: ', config_flags%dx
        CALL wrf_debug(100,message)
        write(message,*) 'dy: ', config_flags%dy
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_year: ', config_flags%start_year
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_month: ', config_flags%start_month
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_day: ', config_flags%start_day
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_hour: ', config_flags%start_hour
        CALL wrf_debug(100,message)

        write(start_date,435) config_flags%start_year, config_flags%start_month, &
                config_flags%start_day, config_flags%start_hour
  435   format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':00:00')

        dlmd=config_flags%dx
        dphd=config_flags%dy
        tph0d=config_flags%cen_lat
        tlm0d=config_flags%cen_lon

!==========================================================================

!!

 !  Check to see if the boundary conditions are set
 !  properly in the namelist file.
 !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      grid%real_data_init_type = model_config_rec%real_data_init_type
      write(message,*) 'what is flag_metgrid: ', flag_metgrid
      CALL wrf_message(message)

     IF ( flag_metgrid .EQ. 1 ) THEN  !   <----- START OF VERTICAL INTERPOLATION PART ---->

     num_metgrid_levels = grid%num_metgrid_levels

	IF (ght_gc(its,jts,num_metgrid_levels/2) .lt. ght_gc(its,jts,num_metgrid_levels/2+1)) THEN


           write(message,*) 'normal ground up file order'
           hyb_coor=.false.
           CALL wrf_message(message)

        ELSE

           hyb_coor=.true.
           write(message,*) 'reverse the order of coordinate'
           CALL wrf_message(message)

           CALL reverse_vert_coord(ght_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(p_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(t_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(u_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(v_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(rh_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

        endif


        IF (hyb_coor) THEN
                           ! limit extreme deviations from source model topography
                           ! due to potential for nasty extrapolation/interpolation issues
                           !
          write(message,*) 'min, max of ht_gc before adjust: ', minval(ht_gc), maxval(ht_gc)
          CALL wrf_debug(100,message)
          ICOUNT=0
          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
             IF ((ht_gc(I,J) - ght_gc(I,J,2)) .LT. -150.) THEN
               ht_gc(I,J)=ght_gc(I,J,2)-150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'increasing NMM topo toward RUC  ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ELSEIF ((ht_gc(I,J) - ght_gc(I,J,2)) .GT. 150.) THEN
               ht_gc(I,J)=ght_gc(I,J,2)+150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'decreasing NMM topo toward RUC ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ENDIF
           END DO
          END DO

          write(message,*) 'min, max of ht_gc after correct: ', minval(ht_gc), maxval(ht_gc)
          CALL wrf_debug(100,message)
        ENDIF

      CALL boundary_smooth(ht_gc,landmask, grid, 12 , 12 &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE )

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           if (LANDMASK(I,J) .gt. 0.5) SM(I,J)=0.
           if (LANDMASK(I,J) .le. 0.5) SM(I,J)=1.
        if (tsk_gc(I,J) .gt. 0.) then
           NMM_TSK(I,J)=tsk_gc(I,J)
        else
           NMM_TSK(I,J)=t_gc(I,J,1) ! stopgap measure
        endif
!
           GLAT(I,J)=hlat_gc(I,J)*DEGRAD
           GLON(I,J)=hlon_gc(I,J)*DEGRAD
           WEASD(I,J)=SNOW(I,J)
           XICE(I,J)=XICE_gc(I,J)
         ENDDO
       ENDDO
! First item is to define the target vertical coordinate

     num_metgrid_levels = grid%num_metgrid_levels
     eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
     ptsgm             = model_config_rec%ptsgm
     p_top_requested = grid%p_top_requested
     pt=p_top_requested

        if (internal_time_loop .eq. 1) then

        if (eta_levels(1) .ne. 1.0) then
          write(message,*) '********************************************************************* '
          CALL wrf_message(message)
          write(message,*) '**  eta_levels appears not to be specified in the namelist'
          CALL wrf_message(message)
          write(message,*) '**  We will call compute_nmm_levels to define layer thicknesses.'
          CALL wrf_message(message)
          write(message,*) '**  These levels should be reasonable for running the model, '
          CALL wrf_message(message)
          write(message,*) '**  but may not be ideal for the simulation being made.  Consider   '
          CALL wrf_message(message)
          write(message,*) '**  defining your own levels by specifying eta_levels in the model   '
          CALL wrf_message(message)
          write(message,*) '**  namelist. '
          CALL wrf_message(message)
          write(message,*) '********************************************************************** '
          CALL wrf_message(message)

          CALL compute_nmm_levels(KDE,p_top_requested,eta_levels)

          DO L=1,KDE
            write(message,*) 'L, eta_levels(L) returned :: ', L,eta_levels(L)
            CALL wrf_message(message)
          ENDDO

        endif

        write(message,*) 'KDE-1: ', KDE-1
        CALL wrf_debug(1,message)
        allocate(SG1(1:KDE-1))
        allocate(SG2(1:KDE-1))
        allocate(DSG1(1:KDE-1))
        allocate(DSG2(1:KDE-1))
        allocate(SGML1(1:KDE))
        allocate(SGML2(1:KDE))

      CALL define_nmm_vertical_coord (kde-1, ptsgm, pt,pdtop, eta_levels, &
                                      ETA1,DETA1,AETA1,             &
                                      ETA2,DETA2,AETA2, DFL, DFRLG         )

       DO L=KDS,KDE-1
        DETA(L)=eta_levels(L)-eta_levels(L+1)
       ENDDO
        endif

        if (.NOT. allocated(PDVP))     allocate(PDVP(IMS:IME,JMS:JME))
        if (.NOT. allocated(P3D_OUT))  allocate(P3D_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
        if (.NOT. allocated(PSFC_OUTV)) allocate(PSFC_OUTV(IMS:IME,JMS:JME))
        if (.NOT. allocated(P3DV_OUT)) allocate(P3DV_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
        if (.NOT. allocated(P3DV_IN))  allocate(P3DV_IN(IMS:IME,JMS:JME,num_metgrid_levels))

        write(message,*) 'num_metgrid_levels: ', num_metgrid_levels
        CALL wrf_message(message)

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           FIS(I,J)=ht_gc(I,J)*g
!
!       IF ( p_gc(I,J,1) .ne. 200100. .AND.  (ht_gc(I,J) .eq. ght_gc(I,J,1)) .AND. ht_gc(I,J) .ne. 0) THEN
        IF ( p_gc(I,J,1) .ne. 200100. .AND.  (abs(ht_gc(I,J)-ght_gc(I,J,1)) .lt. 0.01) .AND. ht_gc(I,J) .ne. 0) THEN
          IF (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) THEN
            write(message,*) 'ht_gc and toposoil to swap, flag_soilhgt ::: ', &
                          I,J, ht_gc(I,J),toposoil(I,J),flag_soilhgt
            CALL wrf_debug(10,message)
          ENDIF
                IF ( ( flag_soilhgt.EQ. 1 ) ) THEN
                 ght_gc(I,J,1)=toposoil(I,J)
                ENDIF
        ENDIF

         ENDDO
       ENDDO

      CALL compute_nmm_surfacep (ht_gc, ght_gc, p_gc , t_gc               &
     &,                          psfc_out, num_metgrid_levels  &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE ) ! H points

      CALL compute_3d_pressure (psfc_out,AETA1,AETA2   &
     &,            pdtop,pt,pd,p3d_out                 &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE )

#ifdef DM_PARALLEL
        ips=its ; ipe=ite ;  jps=jts ; jpe=jte ; kps=kts ; kpe=kte
# include "HALO_NMM_MG2.inc"
#endif

#ifdef DM_PARALLEL
# include "HALO_NMM_MG3.inc"
#endif

       do K=1,num_metgrid_levels
        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)

         IF (K .eq. KTS) THEN
           IF (J .eq. JDS .and. I .lt. IDE-1) THEN  ! S boundary
             PDVP(I,J)=0.5*(PD(I,J)+PD(I+1,J))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN ! N boundary
             PDVP(I,J)=0.5*(PD(I,J)+PD(I+1,J))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN ! W boundary
             PDVP(I,J)=0.5*(PD(I,J-1)+PD(I,J+1))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J-1)+PSFC_OUT(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN ! E boundary
             PDVP(I,J)=0.5*(PD(I,J-1)+PD(I,J+1))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J-1)+PSFC_OUT(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN ! phantom E boundary
             PDVP(I,J)=PD(I,J)
             PSFC_OUTV(I,J)=PSFC_OUT(I,J)
           ELSEIF (mod(J,2) .eq. 0) THEN ! interior even row
             PDVP(I,J)=0.25*(PD(I,J)+PD(I-1,J)+PD(I,J+1)+PD(I,J-1))
             PSFC_OUTV(I,J)=0.25*(PSFC_OUT(I,J)+PSFC_OUT(I-1,J)+ &
                                  PSFC_OUT(I,J+1)+PSFC_OUT(I,J-1))
           ELSE ! interior odd row
             PDVP(I,J)=0.25*(PD(I,J)+PD(I+1,J)+PD(I,J+1)+PD(I,J-1))
             PSFC_OUTV(I,J)=0.25*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J)+ &
                                  PSFC_OUT(I,J+1)+PSFC_OUT(I,J-1))
           ENDIF
          ENDIF

           IF (J .eq. JDS .and. I .lt. IDE-1) THEN  ! S boundary
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J,K)+p_gc(I+1,J,K))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN ! N boundary
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J,K)+p_gc(I+1,J,K))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN ! W boundary
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J-1,K)+p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN ! E boundary
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J-1,K)+p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN ! phantom E boundary
             P3DV_IN(I,J,K)=p_gc(I,J,K)
           ELSEIF (mod(J,2) .eq. 0) THEN ! interior even row
             P3DV_IN(I,J,K)=0.25*(p_gc(I,J,K)+p_gc(I-1,J,K) + &
                                  p_gc(I,J+1,K)+p_gc(I,J-1,K))
           ELSE ! interior odd row
             P3DV_IN(I,J,K)=0.25*(p_gc(I,J,K)+p_gc(I+1,J,K) + &
                                  p_gc(I,J+1,K)+p_gc(I,J-1,K))
           ENDIF

         enddo
        enddo
       enddo

      CALL compute_3d_pressure (psfc_outv,AETA1,AETA2   &
     &,            pdtop,pt,pdvp,p3dv_out              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE )

      CALL interp_press2press_lin(p_gc, p3d_out        &
     &,            t_gc, T,num_metgrid_levels          &
     &,            .TRUE.,.TRUE.,.TRUE.               & ! extrap, ignore_lowest, t_field
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )


      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            u_gc, U,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            V_gc, V,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

       IF (hyb_coor) THEN
       CALL wind_adjust(p3dv_in,p3dv_out,U_gc,V_gc,U,V  &
     &,                 num_metgrid_levels,5000.        &
     &,                 IDS,IDE,JDS,JDE,KDS,KDE           &
     &,                 IMS,IME,JMS,JME,KMS,KME           &
     &,                 ITS,ITE,JTS,JTE,KTS,KTE )
       ENDIF


         ALLOCATE(qtmp(IMS:IME,JMS:JME,num_metgrid_levels))
         ALLOCATE(qtmp2(IMS:IME,JMS:JME,num_metgrid_levels))

            CALL rh_to_mxrat (rh_gc, t_gc, p_gc, qtmp , .TRUE. , &
                        ids , ide , jds , jde , 1 , num_metgrid_levels , &
                        ims , ime , jms , jme , 1 , num_metgrid_levels , &
                        its , ite , jts , jte , 1 , num_metgrid_levels )

       do K=1,num_metgrid_levels
        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           QTMP2(I,J,K)=QTMP(I,J,K)/(1.0+QTMP(I,J,K))
         end do
        end do
       end do

      CALL interp_press2press_log(p_gc, p3d_out        &
     &,            QTMP2, Q,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.                      &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

      IF (ALLOCATED(QTMP)) DEALLOCATE(QTMP)
      IF (ALLOCATED(QTMP)) DEALLOCATE(QTMP2)

         !  Get the monthly values interpolated to the current date
         !  for the traditional monthly
         !  fields of green-ness fraction and background albedo.

        if (internal_time_loop .eq. 1) then

         CALL monthly_interp_to_date ( greenfrac_gc , current_date , vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( albedo12m_gc , current_date , albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         !  Get the min/max of each i,j for the monthly green-ness fraction.

         CALL monthly_min_max ( greenfrac_gc , shdmin , shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness values in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
!!              vegfra(i,j) = vegfra(i,j) * 100.
              shdmax(i,j) = shdmax(i,j) * 100.
              shdmin(i,j) = shdmin(i,j) * 100.
              VEGFRC(I,J)=VEGFRA(I,J)
           END DO
         END DO

         !  The model expects the albedo fields as
         !  a fraction, not a percent.  Set the water values to 8%.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              if (albbck(i,j) .lt. 5.) then
                  write(message,*) 'reset albedo to 8%...  I,J,albbck:: ', I,J,albbck(I,J)
                  CALL wrf_debug(10,message)
                  albbck(I,J)=8.
              endif
              albbck(i,j) = albbck(i,j) / 100.
              snoalb(i,j) = snoalb(i,j) / 100.
              IF ( landmask(i,j) .LT. 0.5 ) THEN
                 albbck(i,j) = 0.08
                 snoalb(i,j) = 0.08
              END IF
              albase(i,j)=albbck(i,j)
              mxsnal(i,j)=snoalb(i,j)
           END DO
         END DO

         endif

!  new deallocs
        DEALLOCATE(p3d_out,p3dv_out,p3dv_in)

     END IF     !   <----- END OF VERTICAL INTERPOLATION PART ---->

        if (internal_time_loop .eq. 1) then

!!! WEASD has "snow water equivalent" in mm

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

      IF(SM(I,J).GT.0.9) THEN

       IF (XICE(I,J) .gt. 0) then
         SI(I,J)=1.0
       ENDIF

!  SEA
              EPSR(I,J)=.97
              EMBCK(I,J)=.97
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
! LAND
        EPSR(I,J)=1.0
        EMBCK(I,J)=1.0
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
          SI(I,J)=5.0*WEASD(I,J)
          SNO(I,J)=WEASD(I,J)

!!  convert VEGFRA
          VEGFRA(I,J)=VEGFRA(I,J)*100.
!
        ENDDO
      ENDDO

#ifdef DM_PARALLEL

        ALLOCATE(SM_G(IDS:IDE,JDS:JDE),SICE_G(IDS:IDE,JDS:JDE))

        CALL WRF_PATCH_TO_GLOBAL_REAL( SICE(IMS,JMS)           &
     &,                                SICE_G,grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_PATCH_TO_GLOBAL_REAL( SM(IMS,JMS)           &
     &,                                SM_G,grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )


        IF (WRF_DM_ON_MONITOR()) THEN

  637   format(40(f3.0,1x))

        allocate(IHE_G(JDS:JDE-1),IHW_G(JDS:JDE-1))
       DO j = JDS, JDE-1
          IHE_G(J)=MOD(J+1,2)
          IHW_G(J)=IHE_G(J)-1
       ENDDO

      DO ITER=1,10
       DO j = jds+1, (jde-1)-1
         DO i = ids+1, (ide-1)-1

! any sea ice around point in question?

        IF (SM_G(I,J) .ge. 0.9) THEN
          SEAICESUM=SICE_G(I+IHE_G(J),J+1)+SICE_G(I+IHW_G(J),J+1)+ &
                    SICE_G(I+IHE_G(J),J-1)+SICE_G(I+IHW_G(J),J-1)
          IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 3.) THEN

            IF ((SICE_G(I+IHE_G(J),J+1).lt.0.1 .and. SM_G(I+IHE_G(J),J+1).lt.0.1) .OR. &
                (SICE_G(I+IHW_G(J),J+1).lt.0.1 .and. SM_G(I+IHW_G(J),J+1).lt.0.1) .OR. &
                (SICE_G(I+IHE_G(J),J-1).lt.0.1 .and. SM_G(I+IHE_G(J),J-1).lt.0.1) .OR. &
                (SICE_G(I+IHW_G(J),J-1).lt.0.1 .and. SM_G(I+IHW_G(J),J-1).lt.0.1)) THEN

!             HAVE SEA ICE AND A SURROUNDING LAND POINT - CONVERT TO SEA ICE

              write(message,*) 'making seaice (1): ', I,J
              CALL wrf_debug(100,message)
              SICE_G(I,J)=1.0
              SM_G(I,J)=0.

            ENDIF

          ELSEIF (SEAICESUM .ge. 3) THEN

!       WATER POINT SURROUNDED BY ICE  - CONVERT TO SEA ICE

            write(message,*) 'making seaice (2): ', I,J
            CALL wrf_debug(100,message)
            SICE_G(I,J)=1.0
            SM_G(I,J)=0.
          ENDIF

        ENDIF

        ENDDO
       ENDDO
      ENDDO

        ENDIF

        CALL WRF_GLOBAL_TO_PATCH_REAL( SICE_G, SICE           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_GLOBAL_TO_PATCH_REAL( SM_G,SM           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        IF (WRF_DM_ON_MONITOR()) THEN

        DEALLOCATE(SM_G,SICE_G)
        DEALLOCATE(IHE_G,IHW_G)

        ENDIF

!        write(message,*) 'revised sea ice on patch'
!        CALL wrf_debug(100,message)
!        DO J=JTE,JTS,-(((JTE-JTS)/25)+1)
!          write(message,637) (SICE(I,J),I=ITS,ITE,ITE/20)
!          CALL wrf_debug(100,message)
!        END DO

#else
! serial sea ice reprocessing

       DO j = jts, MIN(jte,jde-1)
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

      DO ITER=1,10
       DO j = jts+1, MIN(jte,jde-1)-1
         DO i = its+1, MIN(ite,ide-1)-1

! any sea ice around point in question?

        IF (SM(I,J) .gt. 0.9) THEN
          SEAICESUM=SICE(I+IHE(J),J+1)+SICE(I+IHW(J),J+1)+ &
                  SICE(I+IHE(J),J-1)+SICE(I+IHW(J),J-1)
          IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 3.) THEN
            IF ((SICE(I+IHE(J),J+1).lt.0.1 .and. SM(I+IHE(J),J+1).lt.0.1) .OR. &
                (SICE(I+IHW(J),J+1).lt.0.1 .and. SM(I+IHW(J),J+1).lt.0.1) .OR. &
                (SICE(I+IHE(J),J-1).lt.0.1 .and. SM(I+IHE(J),J-1).lt.0.1) .OR. &
                (SICE(I+IHW(J),J-1).lt.0.1 .and. SM(I+IHW(J),J-1).lt.0.1)) THEN

!       HAVE SEA ICE AND A SURROUNDING LAND POINT - CONVERT TO SEA ICE
              SICE(I,J)=1.0
              SM(I,J)=0.
            ENDIF
          ELSEIF (SEAICESUM .ge. 3) THEN
!       WATER POINT SURROUNDED BY ICE  - CONVERT TO SEA ICE
        SICE(I,J)=1.0
        SM(I,J)=0.
          ENDIF
        ENDIF

        ENDDO
       ENDDO
      ENDDO

#endif

! this block meant to guarantee land/sea agreement between SM and landmask

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

          IF (SM(I,J) .gt. 0.5) THEN
                landmask(I,J)=0.0
          ELSEIF (SM(I,J) .lt. 0.5 .and. SICE(I,J) .gt. 0.9) then
                landmask(I,J)=0.0
          ELSEIF (SM(I,J) .lt. 0.5 .and. SICE(I,J) .lt. 0.1) then
                landmask(I,J)=1.0
          ELSE
                write(message,*) 'missed point in landmask definition ' , I,J
                CALL wrf_message(message)
                landmask(I,J)=0.0
          ENDIF
!
        IF (SICE(I,J) .gt. 0.5 .and. NMM_TSK(I,J) .lt. 0.1 .and. SST(I,J) .gt. 0.) THEN
           write(message,*) 'set NMM_TSK to: ', SST(I,J)
           CALL wrf_message(message)
           NMM_TSK(I,J)=SST(I,J)
           SST(I,J)=0.
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
      END IF

  !  Adjust the various soil temperature values depending on the difference in
  !  in elevation between the current model's elevation and the incoming data's
  !  orography.

      IF ( ( flag_toposoil .EQ. 1 ) ) THEN

        ALLOCATE(HT(ims:ime,jms:jme))

        DO J=jms,jme
          DO I=ims,ime
              HT(I,J)=FIS(I,J)/9.81
          END DO
        END DO

!       if (maxval(toposoil) .gt. 100.) then
!
!  Being avoided.   Something to revisit eventually.
!
!1219 might be simply a matter of including TOPOSOIL
!
!    CODE NOT TESTED AT NCEP USING THIS FUNCTIONALITY,
!    SO TO BE SAFE WILL AVOID FOR RETRO RUNS.
!
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

      !  surface_input_source=1 => use data from static file
      !                            (fractional category as input)
      !  surface_input_source=2 => use data from grib file
      !                            (dominant category as input)

      IF ( config_flags%surface_input_source .EQ. 1 ) THEN
         vegcat (its,jts) = 0
         soilcat(its,jts) = 0
      END IF

      !  Generate the vegetation and soil category information
      !  from the fractional input
      !  data, or use the existing dominant category fields if they exist.

      IF ((soilcat(its,jts) .LT. 0.5) .AND. (vegcat(its,jts) .LT. 0.5)) THEN

         num_veg_cat      = SIZE ( landusef_gc , DIM=3 )
         num_soil_top_cat = SIZE ( soilctop_gc , DIM=3 )
         num_soil_bot_cat = SIZE ( soilcbot_gc , DIM=3 )

        do J=JMS,JME
        do K=1,num_veg_cat
        do I=IMS,IME
           landusef(I,K,J)=landusef_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_top_cat
        do I=IMS,IME
           soilctop(I,K,J)=soilctop_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_bot_cat
        do I=IMS,IME
           soilcbot(I,K,J)=soilcbot_gc(I,J,K)
        enddo
        enddo
        enddo

!       sm (1=water, 0=land)
!       landmask(0=water, 1=land)


        write(message,*) 'landmask into process_percent_cat_new'

        CALL wrf_debug(1,message)
        do J=JTE,JTS,-(((JTE-JTS)/20)+1)
        write(message,641) (landmask(I,J),I=ITS,min(ITE,IDE-1),((ITE-ITS)/15)+1)
        CALL wrf_debug(1,message)
        enddo
  641   format(25(f3.0,1x))

         CALL process_percent_cat_new ( landmask , &
                         landusef , soilctop , soilcbot , &
                         isltyp , ivgtyp , &
                         num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte , &
                         model_config_rec%iswater(grid%id) )

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               vegcat(i,j)  = ivgtyp(i,j)
               soilcat(i,j) = isltyp(i,j)
            END DO
         END DO

       ELSE

         !  Do we have dominant soil and veg data from the input already?

         IF ( soilcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  isltyp(i,j) = NINT( soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( vegcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  ivgtyp(i,j) = NINT( vegcat(i,j) )
               END DO
            END DO
         END IF

       ENDIF

        DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (SICE(I,J) .lt. 0.1) THEN
          IF (landmask(I,J) .gt. 0.5 .and. sm(I,J) .gt. 0.5) THEN
                write(message,*) 'land mask and SM both > 0.5: ', &
                                           I,J,landmask(I,J),sm(I,J)
                CALL wrf_message(message)
                SM(I,J)=0.
          ELSEIF (landmask(I,J) .lt. 0.5 .and. sm(I,J) .lt. 0.5) THEN
                write(message,*) 'land mask and SM both < 0.5: ', &
                                           I,J, landmask(I,J),sm(I,J)
                CALL wrf_message(message)
                SM(I,J)=1.
          ENDIF
        ELSE
          IF (landmask(I,J) .gt. 0.5 .and. SM(I,J)+SICE(I,J) .gt. 0.9) then
            write(message,*) 'landmask says LAND, SM/SICE say SEAICE: ', I,J
          ENDIF
        ENDIF

           ENDDO
        ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SICE(I,J) .gt. 0.9) then
        ISLTYP(I,J)=16
        IVGTYP(I,J)=24
        endif

            ENDDO
         ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SM(I,J) .lt. 0.5) then
            SST(I,J)=0.
        endif

        if (SM(I,J) .gt. 0.5) then
          if (SST(I,J) .lt. 0.1) then
            SST(I,J)=NMM_TSK(I,J)
          endif
            NMM_TSK(I,J)=0.
        endif

        IF ( (NMM_TSK(I,J)+SST(I,J)) .lt. 200. .or. &
             (NMM_TSK(I,J)+SST(I,J)) .gt. 350. ) THEN
          write(message,*) 'TSK, SST trouble at : ', I,J
          CALL wrf_message(message)
          write(message,*) 'SM, NMM_TSK,SST ', SM(I,J),NMM_TSK(I,J),SST(I,J)
          CALL wrf_message(message)
        ENDIF

            ENDDO
         ENDDO

        write(message,*) 'SM'
        CALL wrf_message(message)

        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (sm(i,J),I=its,ite,((ite-its)/10)+1)
          CALL wrf_message(message)
        END DO

        write(message,*) 'SST/NMM_TSK'
        CALL wrf_debug(10,message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (SST(I,J)+NMM_TSK(I,J),I=ITS,min(ide-1,ite),((ite-its)/10)+1)
          CALL wrf_debug(10,message)
        END DO

  635   format(20(f5.1,1x))

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  soiltb(i,j) = sst(i,j)
               ELSE IF (  landmask(i,j) .GT. 0.5 ) THEN
                  soiltb(i,j) = nmm_tsk(i,j)
               END IF
            END DO
         END DO

!      END IF

   !  Land use categories, dominant soil and vegetation types (if available).

!       allocate(lu_index(ims:ime,jms:jme))

          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               lu_index(i,j) = ivgtyp(i,j)
            END DO
          END DO

        if (flag_sst .eq. 1) log_flag_sst=.true.
        if (flag_sst .eq. 0) log_flag_sst=.false.

        write(message,*) 'st_input dimensions: ', size(st_input,dim=1), &
                                  size(st_input,dim=2),size(st_input,dim=3)
        CALL wrf_debug(100,message)

!        write(message,*) 'maxval st_input(1): ', maxval(st_input(:,1,:))
!          CALL wrf_message(message)
!        write(message,*) 'maxval st_input(2): ', maxval(st_input(:,2,:))
!          CALL wrf_message(message)
!        write(message,*) 'maxval st_input(3): ', maxval(st_input(:,3,:))
!          CALL wrf_message(message)
!        write(message,*) 'maxval st_input(4): ', maxval(st_input(:,4,:))
!          CALL wrf_message(message)

! =============================================================

   IF (.NOT. ALLOCATED(TG_ALT))ALLOCATE(TG_ALT(grid%sm31:grid%em31,jms:jme))

      TPH0=TPH0D*DTR
      WBD=-(((ide-1)-1)*DLMD)
      WB= WBD*DTR
      SBD=-(((jde-1)/2)*DPHD)
      SB= SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      WBI=WB+TDLM
      SBI=SB+TDPH
      EBI=WB+(ide-2)*TDLM
      ANBI=SB+(jde-2)*DPH
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
      TSPH=3600./GRID%DT
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
             F(I,J)=0.5*GRID%DT*FP
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
             TERM1=MIN(TERM1,1.0D0)
             TERM1=MAX(TERM1,-1.0D0)
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

        write(message,*) 'call process_soil_real with num_st_levels_input: ',  num_st_levels_input
        CALL wrf_message( message )

! =============================================================

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

! =============================================================

!  Minimum soil values, residual, from RUC LSM scheme.
!  For input from Noah and using
!  RUC LSM scheme, this must be subtracted from the input
!  total soil moisture.  For  input RUC data and using the Noah LSM scheme,
!  this value must be added to the soil moisture_input.

       lqmi(1:num_soil_top_cat) = &
       (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
         0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
         0.004, 0.065 /) !dusan , 0.020, 0.004, 0.008 /)

!  At the initial time we care about values of soil moisture and temperature,
!  other times are ignored by the model, so we ignore them, too.

          account_for_zero_soil_moisture : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

             CASE ( LSMSCHEME )
                iicount = 0
                IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         IF ((landmask(i,j).gt.0.5) .and. (stc(i,1,j) .gt. 200) .and. &
                            (stc(i,1,j) .lt. 400) .and. (smc(i,1,j) .lt. 0.005)) then
                            write(message,*) 'Noah > Noah: bad soil moisture at i,j = ',i,j,smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            smc(i,:,j) = 0.005
                         END IF
                      END DO
                   END DO
                   IF ( iicount .GT. 0 ) THEN
                   write(message,*) 'Noah -> Noah: total number of small soil moisture locations= ',&
                                                                                        iicount
                   CALL wrf_message(message)
                   END IF
                ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         smc(i,:,j) = smc(i,:,j) + lqmi(isltyp(i,j))
                      END DO
                   END DO
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         IF ((landmask(i,j).gt.0.5) .and. (stc(i,1,j) .gt. 200) .and. &
                            (stc(i,1,j) .lt. 400) .and. (smc(i,1,j) .lt. 0.004)) then
                            write(message,*) 'RUC -> Noah: bad soil moisture at i,j = ' &
                                                                     ,i,j,smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            smc(i,:,j) = 0.004
                         END IF
                      END DO
                   END DO
                   IF ( iicount .GT. 0 ) THEN
                   write(message,*) 'RUC -> Noah: total number of small soil moisture locations = ',&
                                                                                       iicount
                   CALL wrf_message(message)
                   END IF
                END IF
               CASE ( RUCLSMSCHEME )
                iicount = 0
                IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         smc(i,:,j) = MAX ( smc(i,:,j) - lqmi(isltyp(i,j)) , 0. )
                      END DO
                   END DO
                ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                   ! no op
                END IF

          END SELECT account_for_zero_soil_moisture

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

        IF (SICE(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            STC(I,L,J)=271.16    ! TG value used by Eta/NMM
          END DO
        END IF

        IF (SM(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            STC(I,L,J)=273.16    ! TG value used by Eta/NMM
          END DO
        END IF

            END DO
          END DO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SM(I,J) .lt. 0.1 .and. STC(I,1,J) .lt. 0.1) THEN
          write(message,*) 'troublesome SM,STC,SMC value: ', I,J,SM(I,J), stc(I,1,J),smc(I,1,J)
          CALL wrf_message(message)
        do JJ=J-1,J+1
        do L=1, grid%num_soil_layers
        do II=I-1,I+1

	if (II .ge. its .and. II .le. MIN(ide-1,ite) .and. &
            JJ .ge. jts .and. JJ .le. MIN(jde-1,jte)) then

        STC(I,L,J)=amax1(STC(I,L,J),STC(II,L,JJ))
        cur_smc=SMC(I,L,J)

        if ( SMC(II,L,JJ) .gt. 0.005 .and. SMC(II,L,JJ) .lt. 1.0) then
		aposs_smc=SMC(II,L,JJ)

        if ( cur_smc .eq. 0 ) then
                cur_smc=aposs_smc
                SMC(I,L,J)=cur_smc
        else
                cur_smc=amin1(cur_smc,aposs_smc)
                cur_smc=amin1(cur_smc,aposs_smc)
                SMC(I,L,J)=cur_smc
        endif
	endif

	endif ! bounds check

        enddo
        enddo
        enddo
        write(message,*) 'STC, SMC(1) now: ',  stc(I,1,J),smc(I,1,J)
        CALL wrf_message(message)
        endif

        if (STC(I,1,J) .lt. 0.1) then
          write(message,*) 'QUITTING DUE TO STILL troublesome STC value: ', I,J, stc(I,1,J),smc(I,1,J)
          call wrf_error_fatal(message)
        endif

        ENDDO
        ENDDO

!hardwire soil stuff for time being

!       RTDPTH=0.
!       RTDPTH(1)=0.1
!       RTDPTH(2)=0.3
!       RTDPTH(3)=0.6

!       SLDPTH=0.
!       SLDPTH(1)=0.1
!       SLDPTH(2)=0.3
!       SLDPTH(3)=0.6
!       SLDPTH(4)=1.0

!!! main body of nmm_specific starts here
!
        do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)
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

      DTAD=1.0
!       IDTCF=DTCF, IDTCF=4
      DTCF=4.0 ! used?

      DY_NMM=ERAD*DPH
      CPGFV=-GRID%DT/(48.*DY_NMM)
      EN= GRID%DT/( 4.*DY_NMM)*DTAD
      ENT=GRID%DT/(16.*DY_NMM)*DTAD

        DO J=jts,nnyp
         KHL2(J)=(IDE-1)*(J-1)-(J-1)/2+2
         KVL2(J)=(IDE-1)*(J-1)-J/2+2
         KHH2(J)=(IDE-1)*J-J/2-1
         KVH2(J)=(IDE-1)*J-(J+1)/2-1
        ENDDO

        TPH=SB-DPH

        DO J=jts,min(jte,jde-1)
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         DXJ(J)=DXP
         WPDARJ(J)=-W_NMM * &
     ((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)/ &
                   (GRID%DT*32.*DXP*DY_NMM)

         CPGFUJ(J)=-GRID%DT/(48.*DXP)
         CURVJ(J)=.5*GRID%DT*TAN(TPH)/ERAD
         FCPJ(J)=GRID%DT/(CP*192.*DXP*DY_NMM)
         FDIVJ(J)=1./(12.*DXP*DY_NMM)
!         EMJ(J)= GRID%DT/( 4.*DXP)*DTAD
!         EMTJ(J)=GRID%DT/(16.*DXP)*DTAD
         FADJ(J)=-GRID%DT/(48.*DXP*DY_NMM)*DTAD
         ACDT=GRID%DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)
         CDDAMP=CODAMP*ACDT
         HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM)
         DDMPUJ(J)=CDDAMP/DXP
         DDMPVJ(J)=CDDAMP/DY_NMM
        ENDDO

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
             F(I,J)=0.5*GRID%DT*FP

           ENDDO
          ENDDO

! --------------DERIVED VERTICAL GRID CONSTANTS--------------------------

      EF4T=.5*GRID%DT/CP
      F4Q =   -GRID%DT*DTAD
      F4D =-.5*GRID%DT*DTAD

       DO L=KDS,KDE-1
        RDETA(L)=1./DETA(L)
        F4Q2(L)=-.25*GRID%DT*DTAD/DETA(L)
       ENDDO

        DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
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

        write(message,*) 'STC(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (stc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

        write(message,*) 'SMC(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (smc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

          DO j = jts, MIN(jde-1,jte)
          DO i=  ITS, MIN(IDE-1,ITE)

          if (SM(I,J) .lt. 0.1 .and. SMC(I,1,J) .gt. 0.5 .and. SICE(I,J) .lt. 0.1) then
            write(message,*) 'very moist on land point: ', I,J,SMC(I,1,J)
            CALL wrf_debug(10,message)
          endif

          enddo
        enddo

!!!   compute EMT, EM on global domain, and only on task 0.

#ifdef DM_PARALLEL
        IF (wrf_dm_on_monitor()) THEN   !!!! NECESSARY TO LIMIT THIS TO TASK ZERO?
#else
        IF (JDS .eq. JTS) THEN !! set unfailable condition for serial job
#endif

        ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))

        DO J=JDS,JDE-1
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         EMJ(J)= GRID%DT/( 4.*DXP)*DTAD
         EMTJ(J)=GRID%DT/(16.*DXP)*DTAD
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
        ENDIF ! wrf_dm_on_monitor/serial job

      call NMM_SH2O(IMS,IME,JMS,JME,ITS,NNXP,JTS,NNYP,grid%num_soil_layers,ISLTYP, &
                             SM,SICE,STC,SMC,SH2O)

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)

        IF (   abs(IDE-1-ITE) .eq. 1 ) THEN ! along eastern boundary
          write(message,*) 'zero phantom winds'
          CALL wrf_message(message)
          DO K=1,KDE-1
            DO J=JDS,JDE-1,2
              IF (J .ge. JTS .and. J .le. JTE) THEN
                u(IDE-1,J,K)=0.
                v(IDE-1,J,K)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDIF

  969   continue

         DO j = jms, jme
            DO i = ims, ime
             fisx=max(fis(i,j),0.)
             Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
     &                (0.*Z0MAX+FISx    *FCM+Z0LAND)
            ENDDO
          ENDDO

        write(message,*) 'Z0 over memory, leaving module_initialize_real'
        CALL wrf_message(message)
        DO J=JME,JMS,-((JME-JMS)/20+1)
          write(message,635) (Z0(I,J),I=IMS,IME,(IME-IMS)/14+1)
          CALL wrf_message(message)
        ENDDO


        endif ! on first_time check

        write(message,*) 'leaving init_domain_nmm'
        CALL wrf_message( TRIM(message) )
!
       write(message,*)'STUFF MOVED TO REGISTRY:',grid%IDTAD,          &
     & grid%NSOIL,grid%NRADL,grid%NRADS,grid%NPHS,grid%NCNVC,grid%sigma
       CALL wrf_message( TRIM(message) )

!==================================================================================

#define COPY_OUT
#include <scalar_derefs.inc>
      RETURN

   END SUBROUTINE init_domain_nmm

!------------------------------------------------------

  SUBROUTINE define_nmm_vertical_coord ( LM, PTSGM, PT, PDTOP,HYBLEVS, &
                                         SG1,DSG1,SGML1,         &
                                         SG2,DSG2,SGML2,DFL, DFRLG            )

        IMPLICIT NONE

!        USE module_model_constants

!!! certain physical parameters here probably don't need to be defined, as defined
!!! elsewhere within WRF.  Done for initial testing purposes.

        INTEGER ::  LM, LPT2, L
        REAL    ::  PTSGM, PT, PL, PT2, PDTOP
        REAL    ::  RGOG, PSIG,PHYB,PHYBM
        REAL, PARAMETER  :: Rd           =  287.04  ! J deg{-1} kg{-1}
        REAL, PARAMETER :: CP=1004.6,GAMMA=.0065,PRF0=101325.,T0=288.
        REAL, PARAMETER :: g=9.81

        REAL, DIMENSION(LM)   :: DSG,DSG1,DSG2
        REAL, DIMENSION(LM)   :: SGML1,SGML2
        REAL, DIMENSION(LM+1) :: SG1,SG2,HYBLEVS,DFL,DFRLG

        CHARACTER(LEN=255)    :: message

        LPT2=LM+1

        write(message,*) 'pt= ', pt
        CALL wrf_message(message)

        DO L=LM+1,1,-1
          pl=HYBLEVS(L)*(101325.-pt)+pt
          if(pl.lt.ptSGm) LPT2=l
        ENDDO

      IF(LPT2.lt.LM+1) THEN
        pt2=HYBLEVS(LPT2)*(101325.-pt)+pt
      ELSE
        pt2=pt
      ENDIF

      write(message,*) '*** Sigma system starts at ',pt2,' Pa, from level ',LPT2
      CALL wrf_message(message)

      pdtop=pt2-pt

        write(message,*) 'allocating DSG,DSG1,DSG2 as ', LM
        CALL wrf_debug(10,message)

        DSG=-99.

      DO L=1,LM
        DSG(L)=HYBLEVS(L)- HYBLEVS(L+1)
      ENDDO

        DSG1=0.
        DSG2=0.

      DO L=LM,1,-1

       IF(L.ge.LPT2) then
        DSG1(L)=DSG(L)
       ELSE
        DSG2(L)=DSG(L)
       ENDIF

      ENDDO

        SGML1=-99.
        SGML2=-99.

       IF(LPT2.le.LM+1) THEN

        DO L=LM+1,LPT2,-1
        SG2(L)=0.
        ENDDO

       DO L=LPT2,2,-1
        SG2(L-1)=SG2(L)+DSG2(L-1)
       ENDDO

        DO L=LPT2-1,1,-1
        SG2(L)=SG2(L)/SG2(1)
        ENDDO
        SG2(1)=1.

       DO L=LPT2-1,1,-1
        DSG2(L)=SG2(L)-SG2(L+1)
        SGML2(l)=(SG2(l)+SG2(l+1))*0.5
       ENDDO

      ENDIF

      DO L=LM,LPT2,-1
        DSG2(L)=0.
        SGML2(L)=0.
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SG1(LM+1)=0.

      DO L=LM+1,LPT2,-1
       SG1(L-1)=SG1(L)+DSG1(L-1)
      ENDDO

      DO L=LM,LPT2,-1
       SG1(L)=SG1(L)/SG1(LPT2-1)
      ENDDO

        SG1(LPT2-1)=1.

       do l=LPT2-2,1,-1
        SG1(l)=1.
       enddo


      DO L=LM,LPT2,-1
       DSG1(L)=SG1(L)-SG1(L+1)
       SGML1(L)=(SG1(L)+SG1(L+1))*0.5
      ENDDO

      DO L=LPT2-1,1,-1
               DSG1(L)=0.
               SGML1(L)=1.
      ENDDO

 1000 format('l,hyblevs,psig,SG1,SG2,phyb,phybm')
 1100 format(' ',i4,f7.4,f10.2,2f7.4,2f10.2)

      write(message,1000)
      CALL wrf_debug(100,message)

      do l=1,LM+1
        psig=HYBLEVS(L)*(101325.-pt)+pt
        phyb=SG1(l)*pdtop+SG2(l)*(101325.-pdtop-pt)+pt
        if(l.lt.LM+1) then
          phybm=SGML1(l)*pdtop+SGML2(l)*(101325.-pdtop-pt)+pt
        else
          phybm=-99.
        endif

        write(message,1100) l,HYBLEVS(L),psig &
                      ,SG1(l),SG2(l),phyb,phybm
        CALL wrf_debug(100,message)
      enddo


  632   format(f9.6)

       write(message,*) 'SG1'
       CALL wrf_debug(100,message)
       do L=LM+1,1,-1
       write(message,632) SG1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SG2'
       CALL wrf_debug(100,message)
       do L=LM+1,1,-1
       write(message,632) SG2(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'DSG1'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) DSG1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'DSG2'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) DSG2(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SGML1'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) SGML1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SGML2'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) SGML2(L)
       CALL wrf_debug(100,message)
       enddo

      rgog=(rd*gamma)/g
      DO L=1,LM+1
        DFL(L)=g*T0*(1.-((pt+SG1(L)*pdtop+SG2(L)*(101325.-pt2)) &
                       /101325.)**rgog)/gamma
        DFRLG(L)=DFL(L)/g
       write(message,*) 'L, DFL(L): ', L, DFL(L)
       CALL wrf_debug(100,message)
      ENDDO

  END SUBROUTINE define_nmm_vertical_coord

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE compute_nmm_surfacep ( TERRAIN_HGT_T, Z3D_IN, PRESS3D_IN, T3D_IN   &
     &,                             psfc_out,generic           &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE  )

	
       IMPLICIT NONE

       real, allocatable:: dum2d(:,:),DUM2DB(:,:)

       integer :: IDS,IDE,JDS,JDE,KDS,KDE
       integer :: IMS,IME,JMS,JME,KMS,KME
       integer :: ITS,ITE,JTS,JTE,KTS,KTE,Ilook,Jlook
       integer :: I,J,II,generic,L,KINSERT,K,bot_lev,LL
       integer :: IHE(JMS:JME),IHW(JMS:JME), loopinc,iloopinc
	
       real :: TERRAIN_HGT_T(IMS:IME,JMS:JME)
       real :: Z3D_IN(IMS:IME,JMS:JME,generic)
       real :: T3D_IN(IMS:IME,JMS:JME,generic)
       real :: PRESS3D_IN(IMS:IME,JMS:JME,generic)
       real :: PSFC_IN(IMS:IME,JMS:JME),TOPO_IN(IMS:IME,JMS:JME)
       real :: psfc_out(IMS:IME,JMS:JME),rincr(IMS:IME,JMS:JME)
       real :: dif1,dif2,dif3,dif4,dlnpdz,BOT_INPUT_HGT,BOT_INPUT_PRESS,dpdz,rhs
       real :: zin(generic),pin(generic)

       character (len=255) :: message
	
       logical :: DEFINED_PSFC(IMS:IME,JMS:JME), DEFINED_PSFCB(IMS:IME,JMS:JME)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	Ilook=25
        Jlook=25

       DO j = JMS, JME
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

       DO J=JMS,JME
       DO I=IMS,IME
          DEFINED_PSFC(I,J)=.FALSE.
          DEFINED_PSFCB(I,J)=.FALSE.
        IF (PRESS3D_IN(I,J,1) .ne. 200100.) THEN
          PSFC_IN(I,J)=PRESS3D_IN(I,J,1)
          TOPO_IN(I,J)=Z3D_IN(I,J,1)
        ELSE
          PSFC_IN(I,J)=PRESS3D_IN(I,J,2)
          TOPO_IN(I,J)=Z3D_IN(I,J,2)
        ENDIF
       ENDDO
       ENDDO

! input surface pressure smoothing over the ocean - still needed for NAM?

        II_loop: do II=1,8

        CYCLE II_loop

	do J=JTS+1,min(JTE,JDE-1)-1
         do I=ITS+1,min(ITE,IDE-1)-1
         rincr(I,J)=0.

       if (PSFC_IN(I,J) .gt. 100000.          .and. &
           PSFC_IN(I+IHE(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHE(J),J-1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J-1) .gt. 100000. ) then

       dif1=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J+1))
       dif2=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J-1))
       dif3=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J+1))
       dif4=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J-1))

        if (max(dif1,dif2,dif3,dif4) .lt. 200. .and. TOPO_IN(I,J).le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J-1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J-1) .lt. 0.5) then

        rincr(I,J)=0.125*( 4.*PSFC_IN(I,J)+ &
                            PSFC_IN(I+IHE(J),J+1)+PSFC_IN(I+IHE(J),J-1)+ &
                            PSFC_IN(I+IHW(J),J+1)+PSFC_IN(I+IHW(J),J-1) ) &
                          - PSFC_IN(I,J)

!        if (rincr(I,J) .ne. 0 .and. abs(rincr(I,J)) .gt. 20.) then
!          write(message,*) 'II, I,J,rincr: ', II, I,J,rincr(I,J)
!          CALL wrf_message(message)
!        endif

         endif
         endif

        ENDDO
        ENDDO

       DO J=JTS+1,min(JTE,JDE-1)-1
         DO I=ITS+1,min(ITE,IDE-1)-1
           PSFC_IN(I,J)=PSFC_IN(I,J) + rincr(I,J)
         ENDDO
       ENDDO

!        write(message,*) ' -------------------------------------------------- '
!        CALL wrf_message(message)

         end do II_loop

       ALLOCATE(DUM2D(IMS:IME,JMS:JME))

       DO J=JMS,JME
        DO I=IMS,IME
         DUM2D(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
        I_loop: DO I=ITS,min(ITE,IDE-1)

         IF (PSFC_IN(I,J) .lt. 0.1) THEN
           write(message,*) 'QUITTING BECAUSE I,J, PSFC_IN: ', I,J,PSFC_IN(I,J)
           call wrf_error_fatal(message)
         ENDIF

         BOT_INPUT_PRESS=PSFC_IN(I,J)
         BOT_INPUT_HGT=TOPO_IN(I,J)

         IF (I .eq. Ilook .AND. J .eq. Jlook) THEN

	   write(message,*) ' TERRAIN_HGT_T: ', I,J, TERRAIN_HGT_T(I,J)
           CALL wrf_message(message)
	   write(message,*) ' PSFC_IN, TOPO_IN: ', &
                            I, J, PSFC_IN(I,J),TOPO_IN(I,J)
           CALL wrf_message(message)

           DO L=1,generic
	     write(message,*) ' L,PRESS3D_IN, Z3D_IN: ', &
                             I,J,L, PRESS3D_IN(I,J,L),Z3D_IN(I,J,L)
             CALL wrf_debug(10,message)
           END DO
         ENDIF

       DO L=2,generic-1

         IF ( PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
             Z3D_IN(I,J,L) .lt. TERRAIN_HGT_T(I,J) .AND. &
             Z3D_IN(I,J,L+1) .gt. TERRAIN_HGT_T(I,J) ) THEN

           BOT_INPUT_PRESS=PRESS3D_IN(i,j,L)
           BOT_INPUT_HGT=Z3D_IN(I,J,L)

!           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
!             write(message,*) 'BOT_INPUT_PRESS, BOT_INPUT_HGT NOW : ', &
!                         Ilook,Jlook, BOT_INPUT_PRESS, BOT_INPUT_HGT
!             CALL wrf_message(message)
!           ENDIF

          ENDIF
       END DO	

!!!!!!!!!!!!!!!!!!!!!! START HYDRO CHECK

       IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
          (PSFC_IN(I,J) .gt. PRESS3D_IN(i,j,2) .OR. &
           TOPO_IN(I,J) .lt. Z3D_IN(I,J,2)) ) THEN        ! extrapolate downward

         IF (J .eq. JTS .AND. I .eq. ITS) THEN
            write(message,*) 'hydro check - should only be for isobaric input'
            CALL wrf_message(message)
         ENDIF

	 IF (Z3D_IN(I,J,2) .ne. TOPO_IN(I,J)) THEN
           dpdz=(PRESS3D_IN(i,j,2)-PSFC_IN(I,J))/(Z3D_IN(I,J,2)-TOPO_IN(I,J))
           rhs=-9.81*((PRESS3D_IN(i,j,2)+ PSFC_IN(I,J))/2.)/(287.04* T3D_IN(I,J,2))

	   IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
             IF (dpdz .lt. 1.05*rhs .OR. dpdz .gt. 0.95*rhs) THEN
                write(message,*) 'I,J,P(2),Psfc,Z(2),Zsfc: ', &
                    I,J,PRESS3D_IN(i,j,2),PSFC_IN(I,J),Z3D_IN(I,J,2),TOPO_IN(I,J)
               IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) CALL wrf_debug(50,message)
	      CYCLE I_loop
             ENDIF

           ENDIF

         ELSE ! z(2) equals TOPO_IN

	  IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN
!	    write(message,*) 'all equal at I,J: ', I,J
!           CALL wrf_message(message)
          ELSE
!           write(message,*) 'heights equal, pressures not: ', &
!                           PRESS3D_IN(i,j,2), PSFC_IN(I,J)
!           CALL wrf_message(message)
	    CYCLE I_loop
	  ENDIF

         ENDIF

         IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
           IF (PRESS3D_IN(i,j,2) .lt. PSFC_IN(I,J) .and. &
                          Z3D_IN(I,J,2) .lt. TOPO_IN(I,J)) THEN
!            write(message,*) 'surface data mismatch(a) at I,J: ', I,J
!            CALL wrf_message(message)
	     CYCLE I_loop
           ELSEIF (PRESS3D_IN(i,j,2) .gt. PSFC_IN(I,J) .AND.  &
                  Z3D_IN(I,J,2) .gt. TOPO_IN(I,J)) THEN
!             write(message,*) 'surface data mismatch(b) at I,J: ', I,J
!             CALL wrf_message(message)
             CYCLE I_loop
           ENDIF
         ENDIF
       ENDIF

!!!!!!! loop over a few more levels

        DO L=3,6
          IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
             (((PSFC_IN(I,J)-PRESS3D_IN(i,j,L)) .lt. 400.) .OR. &
               TOPO_IN(I,J) .lt. Z3D_IN(I,J,L))) then

	    IF (Z3D_IN(I,J,L) .ne. TOPO_IN(I,J)) THEN
              dpdz=(PRESS3D_IN(i,j,L)-PSFC_IN(I,J))/ &
                   (Z3D_IN(I,J,L)-TOPO_IN(I,J))
              rhs=-9.81*((PRESS3D_IN(i,j,L)+ PSFC_IN(I,J))/2.)/ &
                        (287.04*T3D_IN(I,J,L))
              IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
                IF (dpdz .lt. 1.05*rhs .or. dpdz .gt. 0.95*rhs) THEN
                  write(message,*) 'I,J,L,Piso,Psfc,Ziso,Zsfc: ', &
                                    I,J,L,PRESS3D_IN(i,j,L),PSFC_IN(I,J),&
                                    Z3D_IN(I,J,L),TOPO_IN(I,J)
                  IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) &
                                               CALL wrf_debug(50,message)
	          CYCLE I_loop
                ENDIF
              ENDIF
            ELSE
	      IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN
!	        write(message,*) 'all equal at I,J: ', I,J
!               CALL wrf_message(message)
              ELSE
	        CYCLE I_loop
              ENDIF
            ENDIF
          ENDIF

	  IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
            IF (PRESS3D_IN(i,j,L) .lt. PSFC_IN(I,J) .AND. &
                    Z3D_IN(I,J,L) .lt. TOPO_IN(I,J)) THEN
              CYCLE I_loop
            ELSEIF (PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
                    Z3D_IN(I,J,L) .gt. TOPO_IN(I,J)) THEN
             CYCLE I_loop
            ENDIF
          ENDIF
        END DO
!!!!!!!!!!!!!!!!!!!!!! END HYDRO CHECK

        IF (TERRAIN_HGT_T(I,J) .eq. BOT_INPUT_HGT ) THEN
           dum2d(I,J)=BOT_INPUT_PRESS

	  IF (BOT_INPUT_HGT .ne. 0. .and. (BOT_INPUT_HGT-INT(BOT_INPUT_HGT) .ne. 0.) ) THEN
	    write(message,*) 'with BOT_INPUT_HGT: ', BOT_INPUT_HGT, &
                             'set dum2d to bot_input_pres: ', I,J,dum2d(I,J)
            CALL wrf_message(message)
          ENDIF

          IF (dum2d(I,J) .lt. 50000. .OR. dum2d(I,J) .gt. 109000.) THEN
            write(message,*) 'bad dum2d(a): ', I,J,DUM2D(I,J)
            CALL wrf_message(message)
          ENDIF

        ELSEIF (TERRAIN_HGT_T(I,J) .lt. BOT_INPUT_HGT ) THEN

!         target is below lowest possible input...extrapolate

          IF ( BOT_INPUT_PRESS-PRESS3D_IN(I,J,2) .gt. 500. ) THEN
            dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,2)) ) / &
                     (BOT_INPUT_HGT-Z3D_IN(i,j,2))
            IF (I .eq. Ilook .and. J .eq. Jlook) THEN
              write(message,*) 'I,J,dlnpdz(a): ', I,J,dlnpdz
              CALL wrf_message(message)
            ENDIF

          ELSE

!! thin layer and/or just have lowest level - difference with 3rd level data
            IF ( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,3)) .gt. 290. ) THEN

              dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,3)) ) / &
                      (BOT_INPUT_HGT-Z3D_IN(i,j,3))

              IF (I .eq. Ilook .and. J .eq. Jlook) then
               write(message,*) 'p diff: ', BOT_INPUT_PRESS, PRESS3D_IN(i,j,3)
               CALL wrf_message(message)
               write(message,*) 'z diff: ', BOT_INPUT_HGT, Z3D_IN(i,j,3)
               CALL wrf_message(message)
              ENDIF
	
            ELSE

!! Loop up to level 7 looking for a sufficiently thick layer

              FIND_THICK:  DO LL=4,7
               IF( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,LL)) .gt. 290.) THEN
                 dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,LL)) ) / &
                   (BOT_INPUT_HGT-Z3D_IN(i,j,LL))
                EXIT FIND_THICK
               ENDIF
              END DO FIND_THICK

            ENDIF

          ENDIF

        dum2d(I,J)= exp(log(BOT_INPUT_PRESS) + dlnpdz * &
                        (TERRAIN_HGT_T(I,J) - BOT_INPUT_HGT) )

         IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
           write(message,*) 'bad dum2d(b): ', I,J,DUM2D(I,J)
           CALL wrf_message(message)
           write(message,*) 'BOT_INPUT_PRESS, dlnpdz, TERRAIN_HGT_T, BOT_INPUT_HGT: ', &
                BOT_INPUT_PRESS, dlnpdz, TERRAIN_HGT_T(I,J), BOT_INPUT_HGT
           CALL wrf_message(message)
           write(message,*) 'Z3D_IN: ', Z3D_IN(I,J,1:10)
           CALL wrf_message(message)
           write(message,*) 'PRESS3D_IN: ', PRESS3D_IN(I,J,1:10)
           CALL wrf_message(message)
         ENDIF

        ELSE ! target level bounded by input levels

          DO L=2,generic-1
            IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
                  TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN
               dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                       (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))
               dum2d(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                           dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
               dum2d(i,j)=exp(dum2d(i,j))
               IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                 write(message,*) 'bad dum2d(c): ', I,J,DUM2D(I,J)
                 CALL wrf_message(message)
               ENDIF
            ENDIF
          ENDDO

!!! account for situation where BOT_INPUT_HGT < TERRAIN_HGT_T < Z3D_IN(:,2,:)
          IF (dum2d(I,J) .eq. -9 .AND. BOT_INPUT_HGT .lt. TERRAIN_HGT_T(I,J) &
              .AND. TERRAIN_HGT_T(I,J) .lt. Z3D_IN(I,J,2)) then

            IF (mod(I,50) .eq. 0 .AND. mod(J,50) .eq. 0) THEN
              write(message,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                 I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
              CALL wrf_message(message)
            ENDIF

            dlnpdz= (log(PSFC_IN(i,j))-log(PRESS3D_IN(i,j,2)) ) / &
                    (TOPO_IN(i,j)-Z3D_IN(i,j,2))
            dum2d(I,J)= log(PSFC_IN(i,j)) +   &
                        dlnpdz * (TERRAIN_HGT_T(I,J) - TOPO_IN(i,j) )
            dum2d(i,j)= exp(dum2d(i,j))
            IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
              write(message,*) 'bad dum2d(d): ', I,J,DUM2D(I,J)
              CALL wrf_message(message)
            ENDIF
          ENDIF

          IF (dum2d(I,J) .eq. -9.) THEN
            write(message,*) 'must have flukey situation in new ', I,J
            CALL wrf_message(message)
            write(message,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                       I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
            CALL wrf_message(message)

            DO L=1,generic-1
              IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
! problematic with HGT_M substitution for "input" surface height?
                dum2d(i,j)=PRESS3D_IN(I,J,L)
                IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                  write(message,*) 'bad dum2d(e): ', I,J,DUM2D(I,J)
                  CALL wrf_message(message)
                ENDIF
              ENDIF
            ENDDO

            IF ( TERRAIN_HGT_T(I,J) .eq. TOPO_IN(I,J)) THEN
              dum2d(I,J)=PSFC_IN(I,J)
              IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                write(message,*) 'bad dum2d(f): ', I,J,DUM2D(I,J)
                CALL wrf_message(message)
              ENDIF
             write(message,*) 'matched input topo, psfc: ', I,J,TOPO_IN(I,J),PSFC_IN(I,J)
             CALL wrf_message(message)
            ENDIF

            IF (dum2d(I,J) .eq. -9.) THEN
              CALL wrf_error_fatal("quitting due to undefined surface pressure")
            ENDIF
          ENDIF

          DEFINED_PSFC(I,J)=.TRUE.

	  IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	    write(message,*) 'newstyle psfc: ', I,J,dum2d(I,J)
            CALL wrf_message(message)
          ENDIF

        ENDIF

        ENDDO I_loop
        ENDDO

        write(message,*) 'psfc points (new style)'
        CALL wrf_message(message)
	loopinc=max( (JTE-JTS)/20,1)
	iloopinc=max( (ITE-ITS)/10,1)

        DO J=min(JTE,JDE-1),JTS,-loopinc
          write(message,633) (dum2d(I,J)/100.,I=ITS,min(ITE,IDE-1),iloopinc)
        END DO

  633   format(35(f5.0,1x))

        write(message,*) 'PSFC extremes (new style)'
        CALL wrf_message(message)
        write(message,*) minval(dum2d,MASK=DEFINED_PSFC),maxval(dum2d,MASK=DEFINED_PSFC)
        CALL wrf_message(message)

!         IF (minval(dum2d,MASK=DEFINED_PSFC) .lt. 50000. .or. maxval(dum2d,MASK=DEFINED_PSFC) .gt. 108000.) THEN

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)

          IF (DEFINED_PSFC(I,J) .AND. dum2d(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (DEFINED_PSFC(I,J) .AND. dum2d(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

         END DO
        END DO



!! "traditional" isobaric only approach ------------------------------------------------

       ALLOCATE (DUM2DB(IMS:IME,JMS:JME))
       DO J=JMS,JME
        DO I=IMS,IME
         DUM2DB(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)

        IF (TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,2)) THEN ! targ below lowest

          IF ( abs(PRESS3D_IN(i,j,2)-PRESS3D_IN(i,j,3)) .gt. 290.) THEN
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,3)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,3))
          ELSE
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,4)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,4))
          ENDIF

          DUM2DB(I,J)= exp( log(PRESS3D_IN(i,j,2)) + dlnpdz * &
                           (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,2)) )

	  IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	    write(message,*) 'I,K, trad: dlnpdz, press_in(2), terrain_t, Z3D_IN(2): ', I,J,dlnpdz, &
                             PRESS3D_IN(i,j,2), TERRAIN_HGT_T(I,J), Z3D_IN(i,j,2)
            CALL wrf_message(message)
          ENDIF

          DEFINED_PSFCB(i,j)=.true.

        ELSEIF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,2)) THEN ! target level bounded by input levels

        DO L=2,generic-1
          IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
              TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN

            dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                    (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))

            DUM2DB(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
            DUM2DB(i,j)=exp(DUM2DB(i,j))

	    DEFINED_PSFCB(i,j)=.true.

            IF (DUM2DB(I,J) .lt. 13000.) THEN
              write(message,*) 'I,J,L,terrain,Z3d(L),z3d(L+1),p3d(L),p3d(l+1): ', I,J,L, &
                                TERRAIN_HGT_T(I,J),Z3D_IN(I,J,L),Z3D_IN(I,J,L+1),PRESS3D_IN(I,J,L), &
                                PRESS3D_IN(I,J,L+1)
              CALL wrf_error_fatal(message)
            ENDIF
          ENDIF
        ENDDO

        ELSEIF (TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,2)) THEN
          DUM2DB(i,j)=PRESS3D_IN(I,J,2)
	  DEFINED_PSFCB(i,j)=.true.
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(message,*) 'must have flukey situation in trad ', I,J
          CALL wrf_message(message)
          DO L=1,generic-1
            IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
              DUM2DB(i,j)=PRESS3D_IN(I,J,L)
              DEFINED_PSFCB(i,j)=.true.
            ENDIF
          ENDDO
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(message,*) 'HOPELESS PSFC, I QUIT'
          CALL wrf_error_fatal(message)
        ENDIF

	if (I .eq. Ilook .and. J .eq. Jlook) THEN
	  write(message,*) ' traditional psfc: ', I,J,DUM2DB(I,J)
          CALL wrf_message(message)
        ENDIF

       ENDDO
       ENDDO

       write(message,*) 'psfc points (traditional)'
       CALL wrf_message(message)
       DO J=min(JTE,JDE-1),JTS,-loopinc
         write(message,633) (DUM2DB(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
         CALL wrf_message(message)
       ENDDO

       write(message,*) 'PSFC extremes (traditional)'
       CALL wrf_message(message)
       write(message,*) minval(DUM2DB,MASK=DEFINED_PSFCB),maxval(DUM2DB,MASK=DEFINED_PSFCB)
       CALL wrf_message(message)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)

          IF (DEFINED_PSFCB(I,J) .AND. dum2db(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (DEFINED_PSFCB(I,J) .AND. dum2db(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

!          IF (DEFINED_PSFCB(I,J) .AND. ( dum2db(I,J) .lt. 50000. .OR. dum2db(I,J) .gt. 108000. )) THEN
!          IF (TERRAIN_HGT_T(I,J) .gt. -10. .and. TERRAIN_HGT_T(I,J) .lt. 5000.) THEN
!            write(0,*) 'I,J, terrain_hgt_t, dum2db: ', I,J, terrain_hgt_t(I,J),dum2db(I,J)
!	    CALL wrf_error_fatal("quit due to unrealistic surface pressure")
!          ELSE
!            WRITE(message,*) 'surface pressure allowed because surface height is extreme value of: ', TERRAIN_HGT_T(I,J)
!            CALL wrf_debug(2,message)
!          ENDIF
!          ENDIF

         ENDDO
        ENDDO

!!!!! end traditional

       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)
         IF (DEFINED_PSFCB(I,J) .and. DEFINED_PSFC(I,J)) THEN

          IF (  abs(dum2d(I,J)-DUM2DB(I,J)) .gt. 400.) THEN
	     write(message,*) 'BIG DIFF I,J, dum2d, DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
             CALL wrf_message(message)
          ENDIF

!! do we have enough confidence in new style to give it more than 50% weight?
          psfc_out(I,J)=0.5*(dum2d(I,J)+DUM2DB(I,J))

         ELSEIF (DEFINED_PSFC(I,J)) THEN
           psfc_out(I,J)=dum2d(I,J)
         ELSEIF (DEFINED_PSFCB(I,J)) THEN
           psfc_out(I,J)=DUM2DB(I,J)
         ELSE
	   write(message,*) 'I,J,dum2d,DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
           CALL wrf_message(message)
	   write(message,*) 'I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J): ', I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J)
           CALL wrf_message(message)
	   call wrf_error_fatal("psfc_out completely undefined")
         ENDIF

	IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	  write(message,*) ' combined psfc: ', I,J,psfc_out(I,J)
          CALL wrf_message(message)
        ENDIF

          IF (PSFC_OUT(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
	      write(message,*) 'possibly bad combo on psfc_out: ', I,J, psfc_out(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)
              CALL wrf_debug(2,message)
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (PSFC_OUT(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
	      write(message,*) 'possibly bad combo on psfc_out: ', I,J, psfc_out(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)
              CALL wrf_debug(2,message)
              CALL wrf_error_fatal("quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

       ENDDO
       ENDDO
	
	deallocate(dum2d,dum2db)

	END SUBROUTINE compute_nmm_surfacep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE compute_3d_pressure(psfc_out,SGML1,SGML2,pdtop,pt       &
     &,                              pd,p3d_out                          &
     &,                              IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                              IMS,IME,JMS,JME,KMS,KME             &
     &,                              ITS,ITE,JTS,JTE,KTS,KTE )


        INTEGER          :: IDS,IDE,JDS,JDE,KDS,KDE
        INTEGER          :: IMS,IME,JMS,JME,KMS,KME
        INTEGER          :: ITS,ITE,JTS,JTE,KTS,KTE

        REAL, INTENT(IN) :: psfc_out(IMS:IME,JMS:JME)
        REAL, INTENT(IN) :: SGML1(KDE),SGML2(KDE),pdtop,pt

        REAL, INTENT(OUT):: p3d_out(IMS:IME,JMS:JME,KDS:KDE-1)
        REAL, INTENT(OUT):: PD(IMS:IME,JMS:JME)

        CHARACTER (len=255) :: message

!	write(message,*) 'pdtop, pt, psfc_out(1,1): ', pdtop, pt, psfc_out(1,1)
!        CALL wrf_message(message)

        DO J=JTS,min(JTE,JDE-1)
          DO I=ITS,min(ITE,IDE-1)
             PD(I,J)=psfc_out(I,J)-PDTOP-PT
          ENDDO
        ENDDO

        DO J=JTS,min(JTE,JDE-1)
         DO K=KDS,KDE-1
          DO I=ITS,min(ITE,IDE-1)
           p3d_out(I,J,K)=PD(I,J)*SGML2(K)+PDTOP*SGML1(K)+PT

	IF (p3d_out(I,J,K) .ge. psfc_out(I,J) .or. p3d_out(I,J,K) .le. pt) THEN
           write(message,*) 'I,K,J,p3d_out: ', I,K,J,p3d_out(I,J,K)
           CALL wrf_error_fatal(message)
 	ENDIF

          ENDDO
         ENDDO
        ENDDO

	END SUBROUTINE compute_3d_pressure

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE interp_press2press_lin(press_in,press_out, &
                                    data_in, data_out,generic          &
     &,                             extrapolate,ignore_lowest,TFIELD    &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, internal_time )

    ! Interpolates data from one set of pressure surfaces to
    ! another set of pressures

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: internal_time

!    REAL, INTENT(IN)                   :: press_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)
!    REAL, INTENT(IN)                   :: data_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest, TFIELD
    LOGICAL                            :: col_smooth

    INTEGER                            :: i,j
    INTEGER                            :: k,kk
    REAL                               :: desired_press
    REAL                               :: dvaldlnp,dlnp,tadiabat,tiso

    REAL, PARAMETER                    :: ADIAFAC=9.81/1004.
    REAL, PARAMETER                    :: TSTEXTRAPFAC=.0065



      DO K=KMS,KME
      DO J=JMS,JME
      DO I=IMS,IME
        DATA_OUT(I,J,K)=-99999.9
      ENDDO
      ENDDO
      ENDDO

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
      test_i: DO i = ITS, min(ITE,IDE-1)

     IF (internal_time_loop .gt. 1) THEN
        IF (J .ne. JDS .and. J .ne. JDE-1 .and. &
          I .ne. IDS .and. I .ne. IDE-1 ) THEN
!! not on boundary
          CYCLE test_i
        ENDIF
     ENDIF


       col_smooth=.false.

        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,j,k)

        if (K .gt. KDS) then
	if (TFIELD .and. col_smooth .and. desired_press .le. press_in(i,j,LMIN) &
                                    .and. press_out(i,j,k-1) .ge. press_in(i,j,LMIN)) then
	  MAX_SMOOTH=K
!	  write(message,*) 'I,J, MAX_SMOOTH: ', I,J, MAX_SMOOTH
!         CALL wrf_debug(100,message)
        endif
        endif

! keep track of where the extrapolation begins

          IF (desired_press .GT. press_in(i,j,LMIN)) THEN
           IF (TFIELD .and. K .eq. 1  .and. (desired_press - press_in(i,j,LMIN)) .gt. 3000.) then
            col_smooth=.TRUE.   ! due to large extrapolation distance
           ENDIF
	

            IF ((desired_press - press_in(i,j,LMIN)).LT. 50.) THEN ! 0.5 mb
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate downward because desired P level is below
                ! the lowest level in our input data.  Extrapolate using simple
                ! 1st derivative of value with respect to ln P for the bottom 2
                ! input layers.

                ! Add a check to make sure we are not using the gradient of
                ! a very thin layer

                if (TFIELD) then
                  tiso=0.5*(data_in(i,j,1)+data_in(i,j,2))
                endif


                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 500.) THEN ! likely isobaric data
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+1)) / dlnp
                ELSE                                                           ! assume terrain following
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+5))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+5)) / dlnp
                ENDIF
                data_out(i,j,k) = data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

	if (TFIELD .and. data_out(i,j,k) .lt. tiso-0.2) then

! restrict slope to -1K/10 hPa
          dvaldlnp=max(dvaldlnp, -1.0/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

        elseif (TFIELD .and. data_out(i,j,k) .gt. tiso+0.2) then

! restrict slope to +0.8K/10 hPa
          dvaldlnp=min(dvaldlnp, 0.8/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

         endif

              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate upward
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-1))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-2))/dlnp
                ENDIF
                data_out(i,j,k) =  data_in(i,j,generic) + &
                  dvaldlnp * (log(desired_press)-log(press_in(i,j,generic)))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            ! We can trap between two levels and linearly interpolate

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN

!       do trapped in lnp

         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dvaldlnp = (data_in(i,j,kk)-data_in(i,j,kk+1))/dlnp
         data_out(i,j,k) = data_in(i,j,kk+1)+ &
                           dvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1)))

                EXIT input_loop
              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop

	if (col_smooth) then
       do K=max(KDS,MAX_SMOOTH-4),MAX_SMOOTH+4
       data_out(I,J,K)=0.5*(data_out(I,J,K)+data_out(I,J,K+1))
       enddo
        endif

      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_lin

  SUBROUTINE wind_adjust(press_in,press_out, &
                                    U_in, V_in,U_out,V_out           &
     &,                             generic,depth_replace    &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: MAXLIN,MAXLOUT

    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)
    REAL, INTENT(IN)                   :: U_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: V_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(INOUT)                :: U_out(IMS:IME,KMS:KME,JMS:JME)
    REAL, INTENT(INOUT)                :: V_out(IMS:IME,KMS:KME,JMS:JME)
    REAL                               :: p1d_in(generic)
    REAL                               :: p1d_out(KDS:KDE-1)


    DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)

!        IF (press_out(I,J,1) .lt. press_in(I,J,2)) then
         IF(  (press_in(I,J,2)-press_out(I,J,1)) .gt. 200.) then

        U_out(I,1,J)=U_in(I,J,2)
        V_out(I,1,J)=V_in(I,J,2)

   INLOOP: DO L=2,generic
	p1d_in(L)=-9999.
        IF (  (press_in(I,J,2)-press_in(I,J,L)) .lt. depth_replace) THEN
          p1d_in(L)=(press_in(I,J,2)-press_in(I,J,L))
          MAXLIN=L
        ELSE
          p1d_in(L)=(press_in(I,J,2)-press_in(I,J,L))
          EXIT INLOOP
        ENDIF
    END DO INLOOP

   OUTLOOP: DO L=KDS,KDE-1
	p1d_out(L)=-9999.
        IF (  (press_out(I,J,1)-press_out(I,J,L)) .lt. depth_replace) THEN
          p1d_out(L)=(press_out(I,J,1)-press_out(I,J,L))
          MAXLOUT=L
        ELSE
          EXIT OUTLOOP
        ENDIF
    END DO OUTLOOP

        DO L=1,MAXLOUT
	ptarg=p1d_out(L)

    FINDLOOP:   DO LL=2,MAXLIN

         if (p1d_in(LL) .lt. ptarg .and. p1d_in(LL+1) .gt. ptarg) then

           dlnp=log(p1d_in(LL))-log(p1d_in(LL+1))
           dudlnp=(U_in(I,J,LL)-U_in(I,J,LL+1))/dlnp
           dvdlnp=(V_in(I,J,LL)-V_in(I,J,LL+1))/dlnp
           U_out(I,L,J)=U_in(I,J,LL)+dudlnp*(log(ptarg)-log(p1d_in(LL)))
           V_out(I,L,J)=V_in(I,J,LL)+dvdlnp*(log(ptarg)-log(p1d_in(LL)))

           EXIT FINDLOOP
         endif

   END DO FINDLOOP
        END DO   ! MAXLOUT loop


        ENDIF

      ENDDO
    ENDDO



  END SUBROUTINE wind_adjust
!--------------------------------------------------------------------

  SUBROUTINE interp_press2press_log(press_in,press_out, &
                                    data_in, data_out, generic          &
     &,                             extrapolate,ignore_lowest           &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, internal_time )

    ! Interpolates ln(data) from one set of pressure surfaces to
    ! another set of pressures

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: internal_time

!    REAL, INTENT(IN)                   :: press_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)
!    REAL, INTENT(IN)                   :: data_in(IMS:IME,generic,JMS:JME)
!    REAL, INTENT(IN)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL                               :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest

    INTEGER                            :: i,j
    INTEGER                            :: k,kk
    REAL                               :: desired_press
    REAL                               :: dlnvaldlnp,dlnp


      DO K=1,generic
      DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)
        DATA_IN(I,J,K)=max(DATA_in(I,J,K),1.e-13)
      ENDDO
      ENDDO
      ENDDO

      DO K=KMS,KME
      DO J=JMS,JME
      DO I=IMS,IME
        DATA_OUT(I,J,K)=-99999.9
      ENDDO
      ENDDO
      ENDDO

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
     test_i:  DO i = ITS, min(ITE,IDE-1)

      IF (internal_time .gt. 1) THEN
        IF (J .ne. JDS .and. J .ne. JDE-1 .and. &
            I .ne. IDS .and. I .ne. IDE-1 ) THEN
!! not on boundary
          CYCLE test_i
        ENDIF
      ENDIF


        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,j,k)

          IF (desired_press .GT. press_in(i,j,LMIN)) THEN

            IF ((desired_press - press_in(i,j,LMIN)).LT. 10.) THEN ! 0.1 mb
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate downward because desired P level is below
                ! the lowest level in our input data.  Extrapolate using simple
                ! 1st derivative of value with respect to ln P for the bottom 2
                ! input layers.

                ! Add a check to make sure we are not using the gradient of
                ! a very thin layer

                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 100.) THEN
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dlnvaldlnp = ( log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+1)) ) / dlnp

                ELSE

                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+2))
                  dlnvaldlnp = (log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+2))) / dlnp

                ENDIF

                data_out(i,j,k) = exp(log(data_in(i,j,LMIN)) + dlnvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN))))
              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate upward
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-1)))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-2)))/dlnp
                ENDIF
                data_out(i,j,k) =  exp(log(data_in(i,j,generic)) + &
                          dlnvaldlnp * (log(desired_press)-log(press_in(i,j,generic))))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            ! We can trap between two levels and linearly interpolate

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN

!       do trapped in lnp

         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dlnvaldlnp = (log(data_in(i,j,kk))-log(data_in(i,j,kk+1)))/dlnp
         data_out(i,j,k) = exp(log(data_in(i,j,kk+1))+ &
                          dlnvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1))))

                EXIT input_loop

              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop
      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_log

!-------------------------------------------------------------------
   SUBROUTINE rh_to_mxrat (rh, t, p, q , wrt_liquid , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

!      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
!      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(OUT)    :: q

      !  Local vars

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      !  This subroutine computes mixing ratio (q, kg/kg) from basic variables
      !  pressure (p, Pa), temperature (t, K) and relative humidity (rh, 1-100%).
      !  The reference temperature (t_ref, C) is used to describe the temperature
      !  at which the liquid and ice phase change occurs.

         DO k = kts , kte
      DO j = jts , MIN ( jde-1 , jte )
            DO i = its , MIN (ide-1 , ite )
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  1. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  es=svp1*10.*EXP(svp2*(t(i,j,k)-svpt0)/(t(i,j,k)-svp3))
                  qs=eps*es/(p(i,j,k)/100.-es)
                  q(i,j,k)=MAX(.01*rh(i,j,k)*qs,0.0)
               END DO
            END DO
         END DO

      ELSE
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )

                  t1 = t(i,j,k) - 273.16

                  !  Obviously dry.

                  IF ( t1 .lt. -200. ) THEN
                     q(i,j,k) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    ! liq phase ESLO
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then !liq phas poor ES
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     ELSE
                        tk = t(i,j,k)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     !  Now sat vap pres obtained compute local vapor pressure

                     ew = MAX ( ew , 0. ) * rh(i,j,k) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).  The
                     !  constants assume that the pressure is in hPa, so we divide
                     !  the pressures by 100.

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,j,k)/100. - ew))

                     q(i,j,k) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE rh_to_mxrat

!--=------------------------------------------------------------------

      SUBROUTINE  boundary_smooth(h, landmask, grid, nsmth , nrow &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE )

	implicit none

      TYPE (domain)          :: grid

        integer :: IDS,IDE,JDS,JDE,KDS,KDE
        integer :: IMS,IME,JMS,JME,KMS,KME
        integer :: ITS,ITE,JTS,JTE,KTS,KTE
        integer:: ihw(JDS:JDE-1),ihe(JDS:JDE-1),nsmth,nrow
        real::    h(IMS:IME,JMS:JME),landmask(IMS:IME,JMS:JME)
        real ::   h_old(IMS:IME,JMS:JME)
        real ::   hbms(IDS:IDE-1,JDS:JDE-1)
        real ::   hse(IDS:IDE-1,JDS:JDE-1)
        real ::   hne(IDS:IDE-1,JDS:JDE-1)
        integer :: IPS,IPE,JPS,JPE,KPS,KPE
        integer :: ihl, ihh, m2l, ibas,jmelin
        integer :: I,J,KS,IOFFSET,JSTART,JEND
        character (len=255) :: message

	ips=its
        ipe=ite
	jps=jts
        jpe=jte
	kps=kts
        kpe=kte

        do j= JTS,min(JTE,JDE-1)
         ihw(J)=-mod(J,2)
         ihe(j)=ihw(J)+1
        end do

        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           hbms(I,J)=landmask(I,J)
         enddo
        enddo

        jmelin=(JDE-1)-nrow+1
        ibas=nrow/2
        m2l=mod(nrow,2)

        do j=jts,min(jte,jde-1)
         ihl=ibas+mod(j,2)+m2l*mod(J+1,2)
         ihh=(IDE-1)-ibas-m2l*mod(J+1,2)
         do i=its,min(ite,ide-1)
          if (I .ge. ihl .and. I .le. ihh .and. J .ge. nrow .and. J .le. jmelin) then
           hbms(I,J)=0.
          endif
         end do
        end do

  634	format(30(f2.0,1x))

        do KS=1,nsmth

         grid%ht_gc=h
#ifdef DM_PARALLEL
# include "HALO_NMM_MG.inc"
#endif
         h=grid%ht_gc
         h_old=grid%ht_gc

          do J=JTS,min(JTE,JDE-1)
           do I=ITS, min(ITE,IDE-1)
              if (I .ge. (IDS+mod(J,2)) .and. J .gt. JDS .and. J .lt. JDE-1 .and. I .lt. IDE-1) then
                h(i,j)= ( h_old(i+ihe(j),j+1) + h_old(i+ihw(j),j-1) + h_old(i+ihe(j),j-1) + h_old(i+ihw(j),j+1) - &
                        4. *h_old(i,j) )*hbms(i,j)*0.125+h_old(i,j)
              endif

           enddo
          enddo

!       special treatment for four corners

        if (hbms(1,1) .eq. 1 .and. ITS .le. 1 .and. JTS .le. 1) then
        h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+  &
                                 0.0625*(h(2,1)+h(1,3))
        endif

        if (hbms(IDE-1,1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTS .le. 1) then
        h(IDE-1,1)=0.75*h(IDE-1,1)+0.125*h(IDE-1+ihw(1),2)+ &
                                 0.0625*(h(IDE-1-1,1)+h(IDE-1,3))
        endif

        if (hbms(1,JDE-1) .eq. 1 .and. ITS .le. 1 .and. JTE .ge. JDE-2) then
        h(1,JDE-1)=0.75*h(1,JDE-1)+0.125*h(1+ihe(JDE-1),JDE-1-1)+ &
                                 0.0625*(h(2,JDE-1)+h(1,JDE-1-2))
        endif

        if (hbms(IDE-1,JDE-1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTE .ge. JDE-2) then
        h(IDE-1,JDE-1)=0.75*h(IDE-1,JDE-1)+0.125*h(IDE-1+ihw(JDE-1),JDE-1-1)+ &
                                 0.0625*(h(IDE-1-1,JDE-1)+h(IDE-1,JDE-1-2))
        endif

        do J=JMS,JME
         do I=IMS,IME
         grid%ht_gc(I,J)=h(I,J)
         enddo
        enddo
#ifdef DM_PARALLEL
# include "HALO_NMM_MG.inc"
#endif
         do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%ht_gc(I,J)
         enddo
        enddo


!       S bound
	if (JTS .eq. JDS) then
        J=JTS

        do I=ITS,ITE
        if (I .ge. IDS+1 .and. I .le. IDE-2) then
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
        endif
        endif
        enddo

        endif

!       N bound
        if (JTE .eq. JDE) then
        J=JDE-1
	write(message,*) 'DOING N BOUND SMOOTHING for J= ', J
        CALL wrf_message(message)
         do I=ITS,min(ITE,IDE-1)
          if (hbms(I,J) .eq. 1 .and. I .ge. IDS+1 .and. I .le. IDE-2) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
          endif
         enddo
	endif

!       W bound
        if (ITS .eq. IDS) then
         I=ITS
         do J=JTS,min(JTE,JDE-1)
          if (hbms(I,J) .eq. 1 .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
          endif
         enddo
	endif

!       E bound
	if (ITE .eq. IDE) then
	write(message,*) 'DOING E BOUND SMOOTHING for I= ', min(ITE,IDE-1)
        CALL wrf_message(message)
         I=min(ITE,IDE-1)
         do J=JTS,min(JTE,JDE-1)
          if (hbms(I,J) .eq. 1  .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
          endif
         enddo
	endif

        enddo   ! end ks loop

        do J=JMS,JME
         do I=IMS,IME
         grid%ht_gc(I,J)=h(I,J)
         enddo
        enddo
#ifdef DM_PARALLEL
#include "HALO_NMM_MG.inc"
#endif
        do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%ht_gc(I,J)
        enddo
        enddo

! extra smoothing along inner boundary

          if (JTS .eq. JDS) then
           if (ITE .eq. IDE) then
              IOFFSET=1
           else
              IOFFSET=0
           endif
!  Southern Boundary
           do i=its,min(ITE,IDE-1)-IOFFSET
             h(i,2)=0.25*(h(i,1)+h(i+1,1)+ &
                          h(i,3)+h(i+1,3))
           enddo
          endif


          if (JTE .eq. JDE) then
           if (ITE .eq. IDE) then
              IOFFSET=1
           else
              IOFFSET=0
           endif
           do i=its,min(ITE,IDE-1)-IOFFSET
             h(i,(JDE-1)-1)=0.25*(h(i,(JDE-1)-2)+h(i+1,(JDE-1)-2)+ &
                                h(i,JDE-1)+h(i+1,JDE-1))
           enddo
          endif

           if (JTS .eq. 1) then
             JSTART=4
           else
             JSTART=JTS+mod(JTS,2) ! needs to be even
           endif

           if (JTE .eq. JDE) then
             JEND=(JDE-1)-3
           else
             JEND=JTE
           endif

          if (ITS .eq. IDS) then

!  Western Boundary
          do j=JSTART,JEND,2
            h(1,j)=0.25*(h(1,j-1)+h(2,j-1)+ &
                         h(1,j+1)+h(2,j+1))

          enddo
          endif
	

          if (ITE .eq. IDE) then
!  Eastern Boundary
          do j=JSTART,JEND,2
            h((IDE-1)-1,j)=0.25*(h((IDE-1)-1,j-1)+h((IDE-1),j-1)+        &
                                 h((IDE-1)-1,j+1)+h((IDE-1),j+1))
          enddo
          endif


       END SUBROUTINE boundary_smooth

!--------------------------------------------------------------------

   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Linrarly in time interpolate data to a current valid time.  The data is
   !  assumed to come in "monthly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month, next_month
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 12
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(l) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 31

      l = 13
      middle(l) = middle(12) + 31

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  int_month = MOD ( l , 12 )
                  IF ( int_month .EQ. 0 ) int_month = 12

	IF (int_month == 12) THEN
            next_month=1
        ELSE
            next_month=int_month+1
        ENDIF

                  field_out(i,j) =  ( field_in(i,j,next_month) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_month  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month
   END SUBROUTINE monthly_interp_to_date

!---------------------------------------------------------------------
   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Plow through each month, find the max, min values for each i,j.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      !  Local vars

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,j,1)
            maxxer = field_in(i,j,1)
            DO l = 2 , 12
               IF ( field_in(i,j,l) .LT. minner ) THEN
                  minner = field_in(i,j,l)
               END IF
               IF ( field_in(i,j,l) .GT. maxxer ) THEN
                  maxxer = field_in(i,j,l)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO

   END SUBROUTINE monthly_min_max

!-----------------------------------------------------------------------

  SUBROUTINE reverse_vert_coord  ( field, start_z, end_z                &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

	IMPLICIT NONE

        INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte,  &
                                       start_z, end_z

        REAL, INTENT(INOUT)         :: field(IMS:IME,JMS:JME,end_z)
! local

        INTEGER                     :: I,J,L
        REAL, ALLOCATABLE           :: dum3d(:,:,:)

        allocate(dum3d(IMS:IME,JMS:JME,end_z))

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
	      dum3d(I,J,L)=field(I,J,end_z-L+start_z)
            END DO
          END DO
        END DO

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
              field(I,J,L)=dum3d(I,J,L)
            END DO
          END DO
        END DO

        DEALLOCATE(dum3d)

        END SUBROUTINE reverse_vert_coord


!--------------------------------------------------------------------

        SUBROUTINE compute_nmm_levels(ninterface, ptop, eta_levels)

        USE module_model_constants

        IMPLICIT NONE

        character(len=132):: message
        integer        ::  ninterface,Lthick,L
        real, parameter:: gamma=.0065
        real, parameter:: t_stand=288.
        real, parameter:: p_stand=101325.

        real           ::  maxdz_compute, ptop
        real           ::  plower,pupper,tlay, sum

        real             :: eta_levels(ninterface)
        real, allocatable:: Z(:)
        real, allocatable:: deta_levels_spline(:)

        logical:: print_pbl_warn

!----------------------------------------------------

        allocate(Z(ninterface))
        allocate(deta_levels_spline(ninterface-1))

        CALL compute_eta_spline(ninterface-1,deta_levels_spline,ptop)

        sum=0.
        DO L=1,ninterface-1
          sum=sum+deta_levels_spline(L)
        ENDDO

        eta_levels(1)=1.00

        DO L=2,ninterface
          eta_levels(L)=eta_levels(L-1)-deta_levels_spline(L-1)
        ENDDO

        eta_levels(ninterface)=0.00

        DO L=2,ninterface-1
          eta_levels(L)=0.5*(eta_levels(L))+0.25*(eta_levels(L-1)+eta_levels(L+1))
        ENDDO

        Z(1)=0.
        maxdz_compute=0.
        print_pbl_warn=.false.

        DO L=2,ninterface
          tlay=max( t_stand-gamma*Z(L-1), 216.5)
          plower=ptop+(p_stand-ptop)*eta_levels(L-1)
          pupper=ptop+(p_stand-ptop)*eta_levels(L)
          Z(L)=Z(L-1)+(tlay*r_d/g)*(log(plower)-log(pupper))

          if (plower .gt. 85000. .and. pupper .lt. 85000. .and. L .lt. 10) then
            print_pbl_warn=.true.
          endif

          write(message,*) 'L, eta(l), pupper, Z(L): ', L, eta_levels(L),pupper,Z(L)
          CALL wrf_debug(100,message)

          if (Z(L)-Z(L-1) .gt. maxdz_compute) then
            Lthick=L
          endif

          maxdz_compute=max(maxdz_compute,Z(L)-Z(L-1))
        ENDDO

        if (print_pbl_warn) then
          write(message,*) 'WARNING - PBL MAY BE POORLY RESOLVED WITH NUMBER OF VERTICAL LEVELS'
          CALL wrf_message(message)
          write(message,*) '        - CONSIDER INCREASING THE VERTICAL RESOLUTION'
          CALL wrf_message(message)
        endif

        write(message,*) 'thickest layer was: ', maxdz_compute , 'meters thick at level: ', Lthick
        CALL wrf_message(message)

        END SUBROUTINE compute_nmm_levels

!---------------------------

     SUBROUTINE compute_eta_spline(LM, dsg, ptop)

     IMPLICIT NONE

     real:: dsg(LM), ptop, sum, rsum
     real, allocatable:: xold(:),dold(:)
     real, allocatable:: xnew(:),sgm(:)
     real, allocatable:: pps(:),qqs(:),y2s(:)
     integer nlev,LM,L,KOLD

    IF (LM .ge. 46) THEN
     KOLD=9
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.13
     dold(2)=.009
     xold(3)=.19
     dold(3)=.012
     xold(4)=.30
     dold(4)=.036
     xold(5)=.42
     dold(5)=.041
     xold(6)=.56
     dold(6)=.040
     xold(7)=.69
     dold(7)=.018

     if (ptop .ge. 2000.) then
      xold(8)=.90
      dold(8)=.012
      xold(9)=1.0
      dold(9)=.006
     else
      xold(8)=.90
      dold(8)=.008
      xold(9)=1.0
      dold(9)=.003
     endif

    ELSE

     KOLD=8
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.18
     dold(2)=.015
     xold(3)=.32
     dold(3)=.035
     xold(4)=.50
     dold(4)=.040
     xold(5)=.68
     dold(5)=.030
     xold(6)=.75
     dold(6)=.017
     xold(7)=.85
     dold(7)=.012

     if (ptop .ge. 2000.) then
      xold(8)=1.0
      dold(8)=.013
     else
      xold(8)=1.0
      dold(8)=.008
     endif

    ENDIF

        allocate(xnew(lm))
        allocate(sgm(lm))
        allocate(pps(lm))
        allocate(qqs(lm))
        allocate(y2s(lm))

    DO L=1,LM
       xnew(l)=float(l-1)/float(lm-1)
    ENDDO

    y2s=0.

    CALL spline(kold,kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)

    sum=0.
    DO l=1,lm
       sum=sum+dsg(l)
    ENDDO

    rsum=1./sum
    sgm(1)=0.

    DO L=1,lm-1
     dsg(l)=dsg(l)*rsum
     sgm(l+1)=sgm(l)+dsg(l)
    ENDDO
    sgm(lm+1)=1.
    dsg(lm)=sgm(lm+1)-sgm(lm)

    END SUBROUTINE compute_eta_spline

! -------------------------------------------------------------------

     subroutine spline(JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!   ********************************************************************
!   *                                                                  *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE          *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                           *
!   *                                                                  *
!   *  PROGRAMER Z. JANJIC                                             *
!   *                                                                  *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3.   *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.         *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.     *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL   *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE        *
!   *         SPECIFIED.                                               *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.       *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)     *
!   *         AND LE XOLD(NOLD).                                       *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.             *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                  *
!   *                                                                  *
!   ********************************************************************
!
!   LOG:
!
!     PYLE - June 2007 - eliminated use of GO TO statements.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW
!
      INTEGER :: K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                   &
     &       ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
!-----------------------------------------------------------------------
      NOLDM1=NOLD-1

      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)

      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR

      first_loop: DO K=3,NOLD-1
        IF(NOLD==3) exit first_loop   ! should be impossible to execute
        DXL=DXR
        DYDXL=DYDXR
        DXR=XOLD(K+1)-XOLD(K)
        DYDXR=(YOLD(K+1)-YOLD(K))/DXR
        DXC=DXL+DXR
        DEN=1./(DXL*Q(K-2)+DXC+DXC)
        P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
        Q(K-1)=-DEN*DXR
      END DO first_loop

      DO K=NOLDM1,2,-1
         Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
      END DO

!-----------------------------------------------------------------------
      second_loop:  DO K1=1,NNEW+1
        XK=XNEW(K1)
        third_loop:  DO K2=2,NOLD
          IF(XOLD(K2)>XK)THEN
            KOLD=K2-1
            exit third_loop
          ENDIF
        END DO third_loop

        IF (XOLD(K2) .le. XK) THEN
          YNEW(K1)=YOLD(NOLD)
          CYCLE second_loop
        ENDIF

        IF (K1 .eq. 1 .or. K .ne. KOLD) THEN
          K=KOLD
          Y2K=Y2(K)
          Y2KP1=Y2(K+1)
          DX=XOLD(K+1)-XOLD(K)
          RDX=1./DX
          AK=.1666667*RDX*(Y2KP1-Y2K)
          BK=0.5*Y2K
          CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
        ENDIF

        X=XK-XOLD(K)
        XSQ=X*X
        YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)
      END DO second_loop

      END SUBROUTINE SPLINE

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
        CHARACTER(LEN=255) :: message

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

        DO K=1,NSOIL
         DO J=JSTART,JM
          DO I=ISTART,IM

!tst
        IF (SMC(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
  if (K .eq. 1) then
    write(message,*) 'I,J,reducing SMC from ' ,I,J,SMC(I,K,J), 'to ', SMCMAX(ISLTPK(I,J))
    CALL wrf_debug(100,message)
  endif
        SMC(I,K,J)=SMCMAX(ISLTPK(I,J))
        ENDIF
!tst

        IF ( (SM(I,J) .lt. 0.5) .and. (SICE(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
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
        write(message,*) 'TROUBLE'
        CALL wrf_message(message)
        write(message,*) 'I,J: ', i,J
        CALL wrf_message(message)
        write(message,*) 'grav, isltpk, psis(isltpk): ', grav,isltpk(I,J),&
                 psis(isltpk(I,J))
        CALL wrf_message(message)
        endif

        if (BX .eq. 0 .or. STC(I,K,J) .eq. 0) then
                write(message,*) 'TROUBLE -- I,J,BX, STC: ', I,J,BX,STC(I,K,J)
                CALL wrf_message(message)
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
          write(message,*) 'SH2O > THAN SMCMAX ', I,J,SH2O(I,K,J),SMCMAX(ISLTPK(I,J)),SMC(I,K,J)
          CALL wrf_message(message)
        endif

         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O

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

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize_real
