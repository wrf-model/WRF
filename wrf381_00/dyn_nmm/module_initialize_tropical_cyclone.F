!REAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for individual domains utilizing the NMM dynamical core.

!-----------------------------------------------------------------------

MODULE module_initialize_ideal

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
   USE module_dm,                    ONLY : LOCAL_COMMUNICATOR       &
                                           ,MYTASK,NTASKS,NTASKS_X   &
                                           ,NTASKS_Y
   USE module_comm_dm
   USE module_ext_internal
#endif

   INTEGER :: internal_time_loop
   INTEGER:: MPI_COMM_COMP,MYPE
   INTEGER:: loopinc, iloopinc

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
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE init_domain

!-------------------------------------------------------------------
!---------------------------------------------------------------------
   SUBROUTINE init_domain_nmm ( grid &
!
# include "dummy_new_args.inc"
!
   )

      USE module_optional_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

# include "dummy_new_decl.inc"


real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: ght_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: rh_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: t_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: u_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: v_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_land_cat)           :: landusef_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilcbot_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilctop_out

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      INTEGER :: num_veg_gc , num_soil_top_gc , num_soil_bot_gc

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
       logical :: test

!              INTEGER :: DOMDESC
              REAL,ALLOCATABLE    :: SICE_G(:,:), SM_G(:,:)
              INTEGER, ALLOCATABLE::  IHE_G(:),IHW_G(:)
              INTEGER, ALLOCATABLE::  ITEMP(:,:)
              INTEGER :: my_e,my_n,my_s,my_w,my_ne,my_nw,my_se,my_sw,myi,myj,npe
              INTEGER :: istat,inpes,jnpes
#else
        integer, allocatable:: ihw(:),ihe(:)
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

!        INTEGER, ALLOCATABLE, DIMENSION(:,:):: grid%lu_index

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

        REAL:: COAC, CODAMP

        INTEGER,PARAMETER:: DOUBLE=SELECTED_REAL_KIND(15,300)
        REAL(KIND=DOUBLE):: TERM1,APH,TLM,TPH,DLM,DPH,STPH,CTPH

        INTEGER:: KHH,KVH,JAM,JA, IHL, IHH, L
        INTEGER:: II,JJ,ISRCH,ISUM,ITER,Ilook,Jlook

        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: W_NMM=0.08
#if ( HWRF == 1 )
        REAL, PARAMETER:: DDFC=1.0
#else
        REAL, PARAMETER:: DDFC=8.0
#endif
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: CP=1004.6
        REAL, PARAMETER:: DFC=1.0
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
    integer :: numzero,numexamined
#if ( HWRF == 1 )
!============================================================================
!   gopal's doing for ocean coupling
!============================================================================

 REAL, DIMENSION(:,:),    ALLOCATABLE :: NHLAT,NHLON,NVLAT,NVLON,HRES_SM
 REAL                                 :: NDLMD,NDPHD,NWBD,NSBD
 INTEGER                              :: NIDE,NJDE,ILOC,JLOC

      INTEGER fid, ierr, nprocs
      CHARACTER*255 f65name, SysString

!============================================================================
! end gopal's doing for ocean coupling
!============================================================================
#endif

        if (ALLOCATED(ADUM2D)) DEALLOCATE(ADUM2D)
        if (ALLOCATED(TG_ALT)) DEALLOCATE(TG_ALT)

!#define COPY_IN
!#include "scalar_derefs.inc"
#ifdef DM_PARALLEL
#    include "data_calls.inc"
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

       IF ( CONFIG_FLAGS%FRACTIONAL_SEAICE == 1 ) THEN
          CALL WRF_ERROR_FATAL("NMM cannot use FRACTIONAL_SEAICE = 1")
       ENDIF

      if ( config_flags%bl_pbl_physics == BOULACSCHEME ) then
         call wrf_error_fatal("Cannot use BOULAC PBL with NMM")
      endif

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

        grid%dlmd=config_flags%dx
        grid%dphd=config_flags%dy
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
!      write(message,*) 'what is flag_metgrid: ', flag_metgrid
!      CALL wrf_message(message)

     IF ( flag_metgrid .EQ. 1 ) THEN  !   <----- START OF VERTICAL INTERPOLATION PART ---->

     num_metgrid_levels = grid%num_metgrid_levels

!---------------------------------------------------------------------
!
! gopal's doing for ideal cases
!

 num_veg_gc      = SIZE ( grid%landusef_gc , DIM=3 )
 num_soil_top_gc = SIZE ( grid%soilctop_gc , DIM=3 )
 num_soil_bot_gc = SIZE ( grid%soilcbot_gc , DIM=3 )

#ifdef DM_PARALLEL
ips=its ; ipe=ite ;  jps=jts ; jpe=jte ; kps=kts ; kpe=kte
!JWB # include "HALO_NMM_IDEAL_1.inc"
#endif
!
 WRITE(message,*)'--------------- ght_gc before calling vortex --------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)grid%ght_gc(100,100,:)

  CALL vortex ( grid%ght_gc,grid%rh_gc,grid%t_gc,grid%u_gc,grid%v_gc,grid%p_gc &
     &,         ght_out,rh_out,t_out,u_out,v_out      &
     &,         grid%ht_gc,grid%tsk_gc,grid%xice_gc                          &
     &,          grid%hlat_gc,grid%hlon_gc,grid%vlat_gc,grid%vlon_gc               &
     &,          grid%greenfrac_gc,grid%albedo12m_gc,grid%landusef_gc    &
     &,          grid%soilctop_gc,grid%soilcbot_gc             &
     &,          landusef_out,soilctop_out,soilcbot_out        &
     &,          num_veg_gc,num_soil_top_gc,num_soil_bot_gc    &
     &,          config_flags%dx,internal_time_loop            &
     &,          1,grid%num_metgrid_levels                          &
     &,          ids,ide,jds,jde,kds,kde                       &
     &,          ims,ime,jms,jme,kms,kme                       &
     &,          its,ite,jts,jte,kts,kte                       )

!----------------------------------------------------------------------

	IF (grid%ght_gc(its,jts,num_metgrid_levels/2) .lt. grid%ght_gc(its,jts,num_metgrid_levels/2+1)) THEN

           write(message,*) 'normal ground up file order'
           hyb_coor=.false.
           CALL wrf_message(message)

        ELSE

           hyb_coor=.true.
           write(message,*) 'reverse the order of coordinate'
           CALL wrf_message(message)

           CALL reverse_vert_coord(grid%ght_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

#if ( HWRF == 1 )
          if(.not. grid%use_prep_hybrid) then
#endif

           CALL reverse_vert_coord(grid%p_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(grid%t_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(grid%u_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(grid%v_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(grid%rh_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

#if ( HWRF == 1 )
          endif
#endif

        endif


        IF (hyb_coor) THEN
                           ! limit extreme deviations from source model topography
                           ! due to potential for nasty extrapolation/interpolation issues
                           !
          write(message,*) 'min, max of grid%ht_gc before adjust: ', minval(grid%ht_gc), maxval(grid%ht_gc)
          CALL wrf_debug(100,message)
          ICOUNT=0
          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
             IF ((grid%ht_gc(I,J) - grid%ght_gc(I,J,2)) .LT. -150.) THEN
               grid%ht_gc(I,J)=grid%ght_gc(I,J,2)-150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'increasing NMM topo toward RUC  ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ELSEIF ((grid%ht_gc(I,J) - grid%ght_gc(I,J,2)) .GT. 150.) THEN
               grid%ht_gc(I,J)=grid%ght_gc(I,J,2)+150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'decreasing NMM topo toward RUC ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ENDIF
           END DO
          END DO

          write(message,*) 'min, max of ht_gc after correct: ', minval(grid%ht_gc), maxval(grid%ht_gc)
          CALL wrf_debug(100,message)
        ENDIF

      CALL boundary_smooth(grid%ht_gc,grid%landmask, grid, 12 , 12 &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE )

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           if (grid%landmask(I,J) .gt. 0.5) grid%sm(I,J)=0.
           if (grid%landmask(I,J) .le. 0.5) grid%sm(I,J)=1.
        if (grid%tsk_gc(I,J) .gt. 0.) then
           grid%nmm_tsk(I,J)=grid%tsk_gc(I,J)
        else
#if ( HWRF == 1 )
                if(grid%use_prep_hybrid) then
                   if(grid%t(I,J,1)<100) then
                      write(message,*) 'NO VALID SURFACE TEMPERATURE: I,J,TSK_GC(I,J),T(I,J,1) = ', &
                           I,J,grid%TSK_GC(I,J),grid%T(I,J,1)
                      call wrf_debug(1,message)
                   else
                      grid%nmm_tsk(I,J)=grid%t(I,J,1) ! stopgap measure
                   end if
                else
#endif
           grid%nmm_tsk(I,J)=grid%t_gc(I,J,1) ! stopgap measure
#if ( HWRF == 1 )
                endif
#endif
        endif
!
           grid%glat(I,J)=grid%hlat_gc(I,J)*DEGRAD
           grid%glon(I,J)=grid%hlon_gc(I,J)*DEGRAD
           grid%weasd(I,J)=grid%snow(I,J)
           grid%xice(I,J)=grid%xice_gc(I,J)
         ENDDO
       ENDDO
! First item is to define the target vertical coordinate

     num_metgrid_levels = grid%num_metgrid_levels
     eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
     ptsgm             = model_config_rec%ptsgm
     p_top_requested = grid%p_top_requested
     grid%pt=p_top_requested

        if (internal_time_loop .eq. 1) then

        if (eta_levels(1) .ne. 1.0) then
#if ( HWRF == 1 )
             if(grid%use_prep_hybrid) then
                call wrf_error_fatal('PREP_HYBRID ERROR: eta_levels is not specified, but use_prep_hybrid=.true.')
             end if
#endif

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

      CALL define_nmm_vertical_coord (kde-1, ptsgm, grid%pt,grid%pdtop, eta_levels, &
                                      grid%eta1,grid%deta1,grid%aeta1,             &
                                      grid%eta2,grid%deta2,grid%aeta2, grid%dfl, grid%dfrlg         )

       DO L=KDS,KDE-1
        grid%deta(L)=eta_levels(L)-eta_levels(L+1)
       ENDDO
        endif

        write(message,*) 'num_metgrid_levels: ', num_metgrid_levels
        CALL wrf_message(message)

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           grid%fis(I,J)=grid%ht_gc(I,J)*g
!
!       IF ( grid%p_gc(I,J,1) .ne. 200100. .AND.  (grid%ht_gc(I,J) .eq. grid%ght_gc(I,J,1)) .AND. grid%ht_gc(I,J) .ne. 0) THEN
        IF ( grid%p_gc(I,J,1) .ne. 200100. .AND.  (abs(grid%ht_gc(I,J)-grid%ght_gc(I,J,1)) .lt. 0.01) .AND. grid%ht_gc(I,J) .ne. 0) THEN
          IF (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) THEN
            write(message,*) 'grid%ht_gc and grid%toposoil to swap, flag_soilhgt ::: ', &
                          I,J, grid%ht_gc(I,J),grid%toposoil(I,J),flag_soilhgt
            CALL wrf_debug(10,message)
          ENDIF
                IF ( ( flag_soilhgt.EQ. 1 ) ) THEN
                 grid%ght_gc(I,J,1)=grid%toposoil(I,J)
                ENDIF
        ENDIF

         ENDDO
       ENDDO

       numzero=0
       numexamined=0
       DO j = jts, MIN(jte,jde-1)
          DO i = its, MIN(ite,ide-1)
             numexamined=numexamined+1
             if(grid%fis(i,j)<1e-5 .and. grid%fis(i,j)>-1e5 ) then
                numzero=numzero+1
             end if
          enddo
       enddo
       write(message,*) 'TOTAL NEAR-ZERO FIS POINTS: ',numzero,' OF ',numexamined
       call wrf_debug(10,message)
#if ( HWRF == 1 )
       interp_notph: if(.not. grid%use_prep_hybrid) then
#endif
      if (.NOT. allocated(PDVP))     allocate(PDVP(IMS:IME,JMS:JME))
      if (.NOT. allocated(P3D_OUT))  allocate(P3D_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
      if (.NOT. allocated(PSFC_OUTV)) allocate(PSFC_OUTV(IMS:IME,JMS:JME))
      if (.NOT. allocated(P3DV_OUT)) allocate(P3DV_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
      if (.NOT. allocated(P3DV_IN))  allocate(P3DV_IN(IMS:IME,JMS:JME,num_metgrid_levels))

      CALL compute_nmm_surfacep (grid%ht_gc, grid%ght_gc, grid%p_gc , grid%t_gc               &
     &,                          grid%psfc_out, num_metgrid_levels  &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE ) ! H points

       if (internal_time_loop .eq. 1) then

       write(message,*) 'psfc points (final combined)'
	loopinc=max( (JTE-JTS)/20,1)
	iloopinc=max( (ITE-ITS)/10,1)
       CALL wrf_message(message)
       DO J=min(JTE,JDE-1),JTS,-loopinc
         write(message,633) (grid%psfc_out(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
         CALL wrf_message(message)
       ENDDO
       
       endif

  633   format(35(f5.0,1x))

      CALL compute_3d_pressure (grid%psfc_out,grid%aeta1,grid%aeta2   &
     &,            grid%pdtop,grid%pt,grid%pd,p3d_out                 &
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
             PDVP(I,J)=0.5*(grid%pd(I,J)+grid%pd(I+1,J))
             PSFC_OUTV(I,J)=0.5*(grid%psfc_out(I,J)+grid%psfc_out(I+1,J))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN ! N boundary
             PDVP(I,J)=0.5*(grid%pd(I,J)+grid%pd(I+1,J))
             PSFC_OUTV(I,J)=0.5*(grid%psfc_out(I,J)+grid%psfc_out(I+1,J))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN ! W boundary
             PDVP(I,J)=0.5*(grid%pd(I,J-1)+grid%pd(I,J+1))
             PSFC_OUTV(I,J)=0.5*(grid%psfc_out(I,J-1)+grid%psfc_out(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN ! E boundary
             PDVP(I,J)=0.5*(grid%pd(I,J-1)+grid%pd(I,J+1))
             PSFC_OUTV(I,J)=0.5*(grid%psfc_out(I,J-1)+grid%psfc_out(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN ! phantom E boundary
             PDVP(I,J)=grid%pd(I,J)
             PSFC_OUTV(I,J)=grid%psfc_out(I,J)
           ELSEIF (mod(J,2) .eq. 0) THEN ! interior even row
             PDVP(I,J)=0.25*(grid%pd(I,J)+grid%pd(I-1,J)+grid%pd(I,J+1)+grid%pd(I,J-1))
             PSFC_OUTV(I,J)=0.25*(grid%psfc_out(I,J)+grid%psfc_out(I-1,J)+ &
                                  grid%psfc_out(I,J+1)+grid%psfc_out(I,J-1))
           ELSE ! interior odd row
             PDVP(I,J)=0.25*(grid%pd(I,J)+grid%pd(I+1,J)+grid%pd(I,J+1)+grid%pd(I,J-1))
             PSFC_OUTV(I,J)=0.25*(grid%psfc_out(I,J)+grid%psfc_out(I+1,J)+ &
                                  grid%psfc_out(I,J+1)+grid%psfc_out(I,J-1))
           ENDIF
          ENDIF

           IF (J .eq. JDS .and. I .lt. IDE-1) THEN  ! S boundary
             P3DV_IN(I,J,K)=0.5*(grid%p_gc(I,J,K)+grid%p_gc(I+1,J,K))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN ! N boundary
             P3DV_IN(I,J,K)=0.5*(grid%p_gc(I,J,K)+grid%p_gc(I+1,J,K))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN ! W boundary
             P3DV_IN(I,J,K)=0.5*(grid%p_gc(I,J-1,K)+grid%p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN ! E boundary
             P3DV_IN(I,J,K)=0.5*(grid%p_gc(I,J-1,K)+grid%p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN ! phantom E boundary
             P3DV_IN(I,J,K)=grid%p_gc(I,J,K)
           ELSEIF (mod(J,2) .eq. 0) THEN ! interior even row
             P3DV_IN(I,J,K)=0.25*(grid%p_gc(I,J,K)+grid%p_gc(I-1,J,K) + &
                                  grid%p_gc(I,J+1,K)+grid%p_gc(I,J-1,K))
           ELSE ! interior odd row
             P3DV_IN(I,J,K)=0.25*(grid%p_gc(I,J,K)+grid%p_gc(I+1,J,K) + &
                                  grid%p_gc(I,J+1,K)+grid%p_gc(I,J-1,K))
           ENDIF

         enddo
        enddo
       enddo

      CALL compute_3d_pressure (psfc_outv,grid%aeta1,grid%aeta2   &
     &,            grid%pdtop,grid%pt,pdvp,p3dv_out              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE )

      CALL interp_press2press_lin(grid%p_gc, p3d_out        &
     &,            grid%t_gc, grid%t,num_metgrid_levels          &
     &,            .TRUE.,.TRUE.,.TRUE.               & ! extrap, ignore_lowest, t_field
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )


      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            grid%u_gc, grid%u,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            grid%v_gc, grid%v,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

       IF (hyb_coor) THEN
       CALL wind_adjust(p3dv_in,p3dv_out,grid%u_gc,grid%v_gc,grid%u,grid%v  &
     &,                 num_metgrid_levels,5000.        &
     &,                 IDS,IDE,JDS,JDE,KDS,KDE           &
     &,                 IMS,IME,JMS,JME,KMS,KME           &
     &,                 ITS,ITE,JTS,JTE,KTS,KTE )
       ENDIF


         ALLOCATE(qtmp(IMS:IME,JMS:JME,num_metgrid_levels))
         ALLOCATE(qtmp2(IMS:IME,JMS:JME,num_metgrid_levels))

            CALL rh_to_mxrat (grid%rh_gc, grid%t_gc, grid%p_gc, qtmp , .TRUE. , &
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

      CALL interp_press2press_log(grid%p_gc, p3d_out        &
     &,            QTMP2, grid%q,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.                      &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

      IF (ALLOCATED(QTMP)) DEALLOCATE(QTMP)
      IF (ALLOCATED(QTMP)) DEALLOCATE(QTMP2)
#if ( HWRF == 1 )
       else ! we are using prep_hybrid
          ! Compute surface pressure:
          grid%psfc_out=grid%pdtop+grid%pd
       end if interp_notph
#endif

         !  Get the monthly values interpolated to the current date
         !  for the traditional monthly
         !  fields of green-ness fraction and background grid%albedo.

        if (internal_time_loop .eq. 1 .or. config_flags%sst_update .eq. 1) then

         CALL monthly_interp_to_date ( grid%greenfrac_gc , current_date , grid%vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( grid%albedo12m_gc , current_date , grid%albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         !  Get the min/max of each i,j for the monthly green-ness fraction.

         CALL monthly_min_max ( grid%greenfrac_gc , grid%shdmin , grid%shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness values in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
!!              grid%vegfra(i,j) = grid%vegfra(i,j) * 100.
              grid%shdmax(i,j) = grid%shdmax(i,j) * 100.
              grid%shdmin(i,j) = grid%shdmin(i,j) * 100.
              grid%vegfrc(I,J)=grid%vegfra(I,J)
           END DO
         END DO

         !  The model expects the albedo fields as
         !  a fraction, not a percent.  Set the water values to 8%.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              if (grid%albbck(i,j) .lt. 5.) then
                  write(message,*) 'reset albedo to 8%...  I,J,albbck:: ', I,J,grid%albbck(I,J)
                  CALL wrf_debug(10,message)
                  grid%albbck(I,J)=8.
              endif
              grid%albbck(i,j) = grid%albbck(i,j) / 100.
              grid%snoalb(i,j) = grid%snoalb(i,j) / 100.
              IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                 grid%albbck(i,j) = 0.08
                 grid%snoalb(i,j) = 0.08
              END IF
              grid%albase(i,j)=grid%albbck(i,j)
              grid%mxsnal(i,j)=grid%snoalb(i,j)
           END DO
         END DO

         endif

#if ( HWRF == 1 )
       if(.not.grid%use_prep_hybrid) then
#endif
!  new deallocs
        DEALLOCATE(p3d_out,p3dv_out,p3dv_in)
#if ( HWRF == 1 )
       end if
#endif

     END IF     !   <----- END OF VERTICAL INTERPOLATION PART ---->


!! compute SST at each time if updating SST
        if (config_flags%sst_update == 1) then

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (grid%SM(I,J) .lt. 0.5) then
            grid%SST(I,J)=0.
        endif

        if (grid%SM(I,J) .gt. 0.5) then
            grid%SST(I,J)=grid%NMM_TSK(I,J)
            grid%NMM_TSK(I,J)=0.
        endif

        IF ( (grid%NMM_TSK(I,J)+grid%SST(I,J)) .lt. 200. .or. &
             (grid%NMM_TSK(I,J)+grid%SST(I,J)) .gt. 350. ) THEN
          write(message,*) 'TSK, SST trouble at : ', I,J
          CALL wrf_message(message)
          write(message,*) 'SM, NMM_TSK,SST ', grid%SM(I,J),grid%NMM_TSK(I,J),grid%SST(I,J)
          CALL wrf_message(message)
        ENDIF

            ENDDO
         ENDDO

        endif ! sst_update test

        if (internal_time_loop .eq. 1) then

!!! weasd has "snow water equivalent" in mm

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

      IF(grid%sm(I,J).GT.0.9) THEN

       IF (grid%xice(I,J) .gt. 0) then
         grid%si(I,J)=1.0
       ENDIF

!  SEA
              grid%epsr(I,J)=.97
              grid%embck(I,J)=.97
              grid%gffc(I,J)=0.
              grid%albedo(I,J)=.06
              grid%albase(I,J)=.06
              IF(grid%si (I,J).GT.0.    ) THEN
!  SEA-ICE
                 grid%sm(I,J)=0.
                 grid%si(I,J)=0.
                 grid%sice(I,J)=1.
                 grid%gffc(I,J)=0. ! just leave zero as irrelevant
                 grid%albedo(I,J)=.60
                 grid%albase(I,J)=.60
              ENDIF
           ELSE

        grid%si(I,J)=5.0*grid%weasd(I,J)/1000.
! LAND
        grid%epsr(I,J)=1.0
        grid%embck(I,J)=1.0
        grid%gffc(I,J)=0.0 ! just leave zero as irrelevant
        grid%sice(I,J)=0.
        grid%sno(I,J)=grid%si(I,J)*.20
           ENDIF
        ENDDO
        ENDDO

! DETERMINE grid%albedo OVER LAND
       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
          IF(grid%sm(I,J).LT.0.9.AND.grid%sice(I,J).LT.0.9) THEN
! SNOWFREE albedo
            IF ( (grid%sno(I,J) .EQ. 0.0) .OR. &
                (grid%albase(I,J) .GE. grid%mxsnal(I,J) ) ) THEN
              grid%albedo(I,J) = grid%albase(I,J)
            ELSE
! MODIFY albedo IF SNOWCOVER:
! BELOW SNOWDEPTH THRESHOLD...
              IF (grid%sno(I,J) .LT. SNUP) THEN
                RSNOW = grid%sno(I,J)/SNUP
                SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
! ABOVE SNOWDEPTH THRESHOLD...
              ELSE
                SNOFAC = 1.0
              ENDIF
! CALCULATE grid%albedo ACCOUNTING FOR SNOWDEPTH AND VGFRCK
              grid%albedo(I,J) = grid%albase(I,J) &
               + (1.0-grid%vegfra(I,J))*SNOFAC*(grid%mxsnal(I,J)-grid%albase(I,J))
            ENDIF
          END IF
          grid%si(I,J)=5.0*grid%weasd(I,J)
          grid%sno(I,J)=grid%weasd(I,J)

!!  convert vegfra
          grid%vegfra(I,J)=grid%vegfra(I,J)*100.
!
        ENDDO
      ENDDO

#ifdef DM_PARALLEL

        ALLOCATE(SM_G(IDS:IDE,JDS:JDE),SICE_G(IDS:IDE,JDS:JDE))

        CALL WRF_PATCH_TO_GLOBAL_REAL( grid%sice(IMS,JMS)           &
     &,                                SICE_G,grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_PATCH_TO_GLOBAL_REAL( grid%sm(IMS,JMS)           &
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

        CALL WRF_GLOBAL_TO_PATCH_REAL( SICE_G, grid%sice           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_GLOBAL_TO_PATCH_REAL( SM_G,grid%sm           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        IF (WRF_DM_ON_MONITOR()) THEN
#if ( HWRF == 1 )
          ! SM_G is still needed for the high-res grid
#else
          DEALLOCATE(SM_G)
#endif
          deallocate(SICE_G)
        DEALLOCATE(IHE_G,IHW_G)

        ENDIF

!        write(message,*) 'revised sea ice on patch'
!        CALL wrf_debug(100,message)
!        DO J=JTE,JTS,-(((JTE-JTS)/25)+1)
!          write(message,637) (grid%sice(I,J),I=ITS,ITE,ITE/20)
!          CALL wrf_debug(100,message)
!        END DO

#else
! serial sea ice reprocessing
        allocate(IHE(JDS:JDE-1),IHW(JDS:JDE-1))

       DO j = jts, MIN(jte,jde-1)
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

      DO ITER=1,10
       DO j = jts+1, MIN(jte,jde-1)-1
         DO i = its+1, MIN(ite,ide-1)-1

! any sea ice around point in question?

        IF (grid%sm(I,J) .gt. 0.9) THEN
          SEAICESUM=grid%sice(I+IHE(J),J+1)+grid%sice(I+IHW(J),J+1)+ &
                  grid%sice(I+IHE(J),J-1)+grid%sice(I+IHW(J),J-1)
          IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 3.) THEN
            IF ((grid%sice(I+IHE(J),J+1).lt.0.1 .and. grid%sm(I+IHE(J),J+1).lt.0.1) .OR. &
                (grid%sice(I+IHW(J),J+1).lt.0.1 .and. grid%sm(I+IHW(J),J+1).lt.0.1) .OR. &
                (grid%sice(I+IHE(J),J-1).lt.0.1 .and. grid%sm(I+IHE(J),J-1).lt.0.1) .OR. &
                (grid%sice(I+IHW(J),J-1).lt.0.1 .and. grid%sm(I+IHW(J),J-1).lt.0.1)) THEN

!       HAVE SEA ICE AND A SURROUNDING LAND POINT - CONVERT TO SEA ICE
              grid%sice(I,J)=1.0
              grid%sm(I,J)=0.
            ENDIF
          ELSEIF (SEAICESUM .ge. 3) THEN
!       WATER POINT SURROUNDED BY ICE  - CONVERT TO SEA ICE
        grid%sice(I,J)=1.0
        grid%sm(I,J)=0.
          ENDIF
        ENDIF

        ENDDO
       ENDDO
      ENDDO

        DEALLOCATE(IHE,IHW)
#endif

! this block meant to guarantee land/sea agreement between sm and landmask

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

          IF (grid%sm(I,J) .gt. 0.5) THEN
                grid%landmask(I,J)=0.0
          ELSEIF (grid%sm(I,J) .lt. 0.5 .and. grid%sice(I,J) .gt. 0.9) then
                grid%landmask(I,J)=0.0
          ELSEIF (grid%sm(I,J) .lt. 0.5 .and. grid%sice(I,J) .lt. 0.1) then
                grid%landmask(I,J)=1.0
          ELSE
                write(message,*) 'missed point in grid%landmask definition ' , I,J
                CALL wrf_message(message)
                grid%landmask(I,J)=0.0
          ENDIF
!
        IF (grid%sice(I,J) .gt. 0.5 .and. grid%nmm_tsk(I,J) .lt. 0.1 .and. grid%sst(I,J) .gt. 0.) THEN
           write(message,*) 'set grid%nmm_tsk to: ', grid%sst(I,J)
           CALL wrf_message(message)
           grid%nmm_tsk(I,J)=grid%sst(I,J)
           grid%sst(I,J)=0.
        endif

        ENDDO
      ENDDO

      !  For sf_surface_physics = 1, we want to use close to a 10 cm value
      !  for the bottom level of the soil temps.

      IF      ( ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .AND. &
                ( flag_st000010 .EQ. 1 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               grid%soiltb(i,j) = grid%st000010(i,j)
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
              HT(I,J)=grid%fis(I,J)/9.81
          END DO
        END DO

!       if (maxval(grid%toposoil) .gt. 100.) then
!
!  Being avoided.   Something to revisit eventually.
!
!1219 might be simply a matter of including toposoil
!
!    CODE NOT TESTED AT NCEP USING THIS FUNCTIONALITY,
!    SO TO BE SAFE WILL AVOID FOR RETRO RUNS.
!
!        CALL adjust_soil_temp_new ( grid%soiltb , 2 , &
!                       grid%nmm_tsk , ht , grid%toposoil , grid%landmask, flag_toposoil , &
!                       grid%st000010 , st010040 , st040100 , st100200 , st010200 , &
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
         grid%vegcat (its,jts) = 0
         grid%soilcat(its,jts) = 0
      END IF

      !  Generate the vegetation and soil category information
      !  from the fractional input
      !  data, or use the existing dominant category fields if they exist.

      IF ((grid%soilcat(its,jts) .LT. 0.5) .AND. (grid%vegcat(its,jts) .LT. 0.5)) THEN

         num_veg_cat      = SIZE ( grid%landusef_gc , DIM=3 )
         num_soil_top_cat = SIZE ( grid%soilctop_gc , DIM=3 )
         num_soil_bot_cat = SIZE ( grid%soilcbot_gc , DIM=3 )

        do J=JMS,JME
        do K=1,num_veg_cat
        do I=IMS,IME
           grid%landusef(I,K,J)=grid%landusef_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_top_cat
        do I=IMS,IME
           grid%soilctop(I,K,J)=grid%soilctop_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_bot_cat
        do I=IMS,IME
           grid%soilcbot(I,K,J)=grid%soilcbot_gc(I,J,K)
        enddo
        enddo
        enddo

!       grid%sm (1=water, 0=land)
!       grid%landmask(0=water, 1=land)


        write(message,*) 'landmask into process_percent_cat_new'

        CALL wrf_debug(1,message)
        do J=JTE,JTS,-(((JTE-JTS)/20)+1)
        write(message,641) (grid%landmask(I,J),I=ITS,min(ITE,IDE-1),((ITE-ITS)/15)+1)
        CALL wrf_debug(1,message)
        enddo
  641   format(25(f3.0,1x))

         CALL process_percent_cat_new ( grid%landmask , &
                         grid%landusef , grid%soilctop , grid%soilcbot , &
                         grid%isltyp , grid%ivgtyp , &
                         num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte , &
                         model_config_rec%iswater(grid%id) )

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               grid%vegcat(i,j)  = grid%ivgtyp(i,j)
               grid%soilcat(i,j) = grid%isltyp(i,j)
            END DO
         END DO

       ELSE

         !  Do we have dominant soil and veg data from the input already?

         IF ( grid%soilcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%isltyp(i,j) = NINT( grid%soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( grid%vegcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%ivgtyp(i,j) = NINT( grid%vegcat(i,j) )
               END DO
            END DO
         END IF

       ENDIF

        DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (grid%sice(I,J) .lt. 0.1) THEN
          IF (grid%landmask(I,J) .gt. 0.5 .and. grid%sm(I,J) .gt. 0.5) THEN
                write(message,*) 'land mask and grid%sm both > 0.5: ', &
                                           I,J,grid%landmask(I,J),grid%sm(I,J)
                CALL wrf_message(message)
                grid%sm(I,J)=0.
          ELSEIF (grid%landmask(I,J) .lt. 0.5 .and. grid%sm(I,J) .lt. 0.5) THEN
                write(message,*) 'land mask and grid%sm both < 0.5: ', &
                                           I,J, grid%landmask(I,J),grid%sm(I,J)
                CALL wrf_message(message)
                grid%sm(I,J)=1.
          ENDIF
        ELSE
          IF (grid%landmask(I,J) .gt. 0.5 .and. grid%sm(I,J)+grid%sice(I,J) .gt. 0.9) then
            write(message,*) 'landmask says LAND, sm/sice say SEAICE: ', I,J
          ENDIF
        ENDIF

           ENDDO
        ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (grid%sice(I,J) .gt. 0.9) then
        grid%isltyp(I,J)=16
        grid%ivgtyp(I,J)=24
        endif

            ENDDO
         ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (grid%sm(I,J) .lt. 0.5) then
            grid%sst(I,J)=0.
        endif

        if (grid%sm(I,J) .gt. 0.5) then
          if (grid%sst(I,J) .lt. 0.1) then
            grid%sst(I,J)=grid%nmm_tsk(I,J)
          endif
            grid%nmm_tsk(I,J)=0.
        endif

        IF ( (grid%nmm_tsk(I,J)+grid%sst(I,J)) .lt. 200. .or. &
             (grid%nmm_tsk(I,J)+grid%sst(I,J)) .gt. 350. ) THEN
          write(message,*) 'TSK, sst trouble at : ', I,J
          CALL wrf_message(message)
          write(message,*) 'sm, nmm_tsk,sst ', grid%sm(I,J),grid%nmm_tsk(I,J),grid%sst(I,J)
          CALL wrf_message(message)
        ENDIF

            ENDDO
         ENDDO

        write(message,*) 'grid%sm'
        CALL wrf_message(message)

        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (grid%sm(i,J),I=its,ite,((ite-its)/10)+1)
          CALL wrf_message(message)
        END DO

        write(message,*) 'sst/nmm_tsk'
        CALL wrf_debug(10,message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (grid%sst(I,J)+grid%nmm_tsk(I,J),I=ITS,min(ide-1,ite),((ite-its)/10)+1)
          CALL wrf_debug(10,message)
        END DO

  635   format(20(f5.1,1x))

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  grid%soiltb(i,j) = grid%sst(i,j)
               ELSE IF (  grid%landmask(i,j) .GT. 0.5 ) THEN
                  grid%soiltb(i,j) = grid%nmm_tsk(i,j)
               END IF
            END DO
         END DO

!      END IF

   !  Land use categories, dominant soil and vegetation types (if available).

!       allocate(grid%lu_index(ims:ime,jms:jme))

          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               grid%lu_index(i,j) = grid%ivgtyp(i,j)
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
      WBD=-(((ide-1)-1)*grid%dlmd)
      WB= WBD*DTR
      SBD=-(((jde-1)/2)*grid%dphd)
      SB= SBD*DTR
      DLM=grid%dlmd*DTR
      DPH=grid%dphd*DTR
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
! jbao orig             grid%f(I,J)=0.5*GRID%DT*FP
! jbao Coriolis correction for idealized!
             grid%f(I,J)=3.15656e-5*0.5*GRID%DT
! jbao Coriolis correction for idealized!

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
             TG_ALT(I,J)=TG0+TGA*COS(APH)-grid%fis(I,J)/3333.
           ENDDO
         ENDDO

            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
!                  IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
!                         grid%sice(I,J) .eq. 0. ) THEN
!                     grid%tg(i,j) = grid%sst(i,j)
!                   ELSEIF (grid%sice(I,J) .eq. 1) THEN
!                     grid%tg(i,j) = 271.16
!                   END IF

        if (grid%tg(I,J) .lt. 200.) then   ! only use default TG_ALT definition if
                                      ! not getting TGROUND from grid%si
                grid%tg(I,J)=TG_ALT(I,J)
        endif

       if (grid%tg(I,J) .lt. 200. .or. grid%tg(I,J) .gt. 320.) then
               write(message,*) 'problematic grid%tg point at : ', I,J
               CALL wrf_message( message )
       endif

        adum2d(i,j)=grid%nmm_tsk(I,J)+grid%sst(I,J)

               END DO
            END DO

        DEALLOCATE(TG_ALT)

        write(message,*) 'call process_soil_real with num_st_levels_input: ',  num_st_levels_input
        CALL wrf_message( message )

! =============================================================

  CALL process_soil_real ( adum2d, grid%tg , &
     grid%landmask, grid%sst, &
     st_input, sm_input, sw_input, &
     st_levels_input , sm_levels_input , &
     sw_levels_input , &
     grid%sldpth , grid%dzsoil , grid%stc , grid%smc , grid%sh2o,  &
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
                         IF ((grid%landmask(i,j).gt.0.5) .and. (grid%stc(i,1,j) .gt. 200) .and. &
                            (grid%stc(i,1,j) .lt. 400) .and. (grid%smc(i,1,j) .lt. 0.005)) then
                            write(message,*) 'Noah > Noah: bad soil moisture at i,j = ',i,j,grid%smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            grid%smc(i,:,j) = 0.005
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
                         grid%smc(i,:,j) = grid%smc(i,:,j) + lqmi(grid%isltyp(i,j))
                      END DO
                   END DO
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         IF ((grid%landmask(i,j).gt.0.5) .and. (grid%stc(i,1,j) .gt. 200) .and. &
                            (grid%stc(i,1,j) .lt. 400) .and. (grid%smc(i,1,j) .lt. 0.004)) then
                            write(message,*) 'RUC -> Noah: bad soil moisture at i,j = ' &
                                                                     ,i,j,grid%smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            grid%smc(i,:,j) = 0.004
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
                         grid%smc(i,:,j) = MAX ( grid%smc(i,:,j) - lqmi(grid%isltyp(i,j)) , 0. )
                      END DO
                   END DO
                ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                   ! no op
                END IF

          END SELECT account_for_zero_soil_moisture

!!!     zero out grid%nmm_tsk at water points again

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
        if (grid%sm(I,J) .gt. 0.5) then
            grid%nmm_tsk(I,J)=0.
        endif
            END DO
         END DO

!!      check on grid%stc

          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (grid%sice(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            grid%stc(I,L,J)=271.16    ! grid%tg value used by Eta/NMM
          END DO
        END IF

        IF (grid%sm(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            grid%stc(I,L,J)=273.16    ! grid%tg value used by Eta/NMM
          END DO
        END IF

            END DO
          END DO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (grid%sm(I,J) .lt. 0.1 .and. grid%stc(I,1,J) .lt. 0.1) THEN
          write(message,*) 'troublesome grid%sm,grid%stc,grid%smc value: ', I,J,grid%sm(I,J), grid%stc(I,1,J),grid%smc(I,1,J)
          CALL wrf_message(message)
        do JJ=J-1,J+1
        do L=1, grid%num_soil_layers
        do II=I-1,I+1

	if (II .ge. its .and. II .le. MIN(ide-1,ite) .and. &
            JJ .ge. jts .and. JJ .le. MIN(jde-1,jte)) then

        grid%stc(I,L,J)=amax1(grid%stc(I,L,J),grid%stc(II,L,JJ))
        cur_smc=grid%smc(I,L,J)

        if ( grid%smc(II,L,JJ) .gt. 0.005 .and. grid%smc(II,L,JJ) .lt. 1.0) then
		aposs_smc=grid%smc(II,L,JJ)

        if ( cur_smc .eq. 0 ) then
                cur_smc=aposs_smc
                grid%smc(I,L,J)=cur_smc
        else
                cur_smc=amin1(cur_smc,aposs_smc)
                cur_smc=amin1(cur_smc,aposs_smc)
                grid%smc(I,L,J)=cur_smc
        endif
	endif

	endif ! bounds check

        enddo
        enddo
        enddo
        write(message,*) 'grid%stc, grid%smc(1) now: ',  grid%stc(I,1,J),grid%smc(I,1,J)
        CALL wrf_message(message)
        endif

        if (grid%stc(I,1,J) .lt. 0.1) then
          write(message,*) 'QUITTING DUE TO STILL troublesome grid%stc value: ', I,J, grid%stc(I,1,J),grid%smc(I,1,J)
          call wrf_error_fatal(message)
        endif

        ENDDO
        ENDDO

!hardwire soil stuff for time being

!       RTDPTH=0.
!       RTDPTH(1)=0.1
!       RTDPTH(2)=0.3
!       RTDPTH(3)=0.6

!       grid%sldpth=0.
!       grid%sldpth(1)=0.1
!       grid%sldpth(2)=0.3
!       grid%sldpth(3)=0.6
!       grid%sldpth(4)=1.0

!!! main body of nmm_specific starts here
!
        do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)
         grid%res(I,J)=1.
        enddo
        enddo

!! grid%hbm2

        grid%hbm2=0.

       do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)

        IF ( (J .ge. 3 .and. J .le. (jde-1)-2) .AND. &
             (I .ge. 2 .and. I .le. (ide-1)-2+mod(J,2)) ) THEN
       grid%hbm2(I,J)=1.
        ENDIF
       enddo
       enddo

!! grid%hbm3
        grid%hbm3=0.

!!      LOOP OVER LOCAL DIMENSIONS

       do J=jts,min(jte,jde-1)
          grid%ihwg(J)=mod(J+1,2)-1
          IF (J .ge. 4 .and. J .le. (jde-1)-3) THEN
            IHL=(ids+1)-grid%ihwg(J)
            IHH=(ide-1)-2
            do I=its,min(ite,ide-1)
              IF (I .ge. IHL  .and. I .le. IHH) grid%hbm3(I,J)=1.
            enddo
          ENDIF
        enddo

!! grid%vbm2

       grid%vbm2=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 3 .and. J .le. (jde-1)-2)  .AND.  &
             (I .ge. 2 .and. I .le. (ide-1)-1-mod(J,2)) ) THEN

           grid%vbm2(I,J)=1.

        ENDIF

       enddo
       enddo

!! grid%vbm3

        grid%vbm3=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 4 .and. J .le. (jde-1)-3)  .AND.  &
             (I .ge. 3-mod(J,2) .and. I .le. (ide-1)-2) ) THEN
         grid%vbm3(I,J)=1.
        ENDIF

       enddo
       enddo

      COAC=model_config_rec%coac(grid%id)
      CODAMP=model_config_rec%codamp(grid%id)

      DTAD=1.0
!       IDTCF=DTCF, IDTCF=4
      DTCF=4.0 ! used?

      grid%dy_nmm=ERAD*DPH
      grid%cpgfv=-GRID%DT/(48.*grid%dy_nmm)
      grid%en= GRID%DT/( 4.*grid%dy_nmm)*DTAD
      grid%ent=GRID%DT/(16.*grid%dy_nmm)*DTAD

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
     ((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+grid%dy_nmm**2)/ &
                   (GRID%DT*32.*DXP*grid%dy_nmm)

         CPGFUJ(J)=-GRID%DT/(48.*DXP)
         CURVJ(J)=.5*GRID%DT*TAN(TPH)/ERAD
         FCPJ(J)=GRID%DT/(CP*192.*DXP*grid%dy_nmm)
         FDIVJ(J)=1./(12.*DXP*grid%dy_nmm)
!         EMJ(J)= GRID%DT/( 4.*DXP)*DTAD
!         EMTJ(J)=GRID%DT/(16.*DXP)*DTAD
         FADJ(J)=-GRID%DT/(48.*DXP*grid%dy_nmm)*DTAD
         ACDT=GRID%DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+grid%dy_nmm**2)
         CDDAMP=CODAMP*ACDT
         HDACJ(J)=COAC*ACDT/(4.*DXP*grid%dy_nmm)
         DDMPUJ(J)=CDDAMP/DXP
         DDMPVJ(J)=CDDAMP/grid%dy_nmm
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
!jbao orig             grid%f(I,J)=0.5*GRID%DT*FP
! jbao Coriolis correction for idealized!
             grid%f(I,J)=3.15656e-5*0.5*GRID%DT
             print*,'real 2griddt is ',grid%dt
! jbao Coriolis correction for idealized!


           ENDDO
          ENDDO

! --------------DERIVED VERTICAL GRID CONSTANTS--------------------------

      grid%ef4t=.5*GRID%DT/CP
      grid%f4q =   -GRID%DT*DTAD
      grid%f4d =-.5*GRID%DT*DTAD

       DO L=KDS,KDE-1
        grid%rdeta(L)=1./grid%deta(L)
        grid%f4q2(L)=-.25*GRID%DT*DTAD/grid%deta(L)
       ENDDO

        DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
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

        DO J=JTS, MIN(JDE-1,JTE)

        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN

               KHH=(IDE-1)-2+MOD(J,2) ! KHH is global...loop over I that have
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                   grid%hdac(I,J)=grid%hdac(I,J)* DFC
                 ENDIF
               ENDDO

        ELSE

          KHH=2+MOD(J,2)
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                    grid%hdac(I,J)=grid%hdac(I,J)* DFC
                 ENDIF
               ENDDO

          KHH=(IDE-1)-2+MOD(J,2)

               DO I=ITS,MIN(IDE-1,ITE)
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

        DO J=JTS,MIN(JDE-1,JTE)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
             grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
             grid%hdacv(I,J)=grid%hdacv(I,J)* DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)* DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            grid%ddmpu(I,J)=grid%ddmpu(I,J)*DDFC
            grid%ddmpv(I,J)=grid%ddmpv(I,J)*DDFC
            grid%hdacv(I,J)=grid%hdacv(I,J)* DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO

        write(message,*) 'grid%stc(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (grid%stc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

        write(message,*) 'grid%smc(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (grid%smc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

          DO j = jts, MIN(jde-1,jte)
          DO i=  ITS, MIN(IDE-1,ITE)

          if (grid%sm(I,J) .lt. 0.1 .and. grid%smc(I,1,J) .gt. 0.5 .and. grid%sice(I,J) .lt. 0.1) then
            write(message,*) 'very moist on land point: ', I,J,grid%smc(I,1,J)
            CALL wrf_debug(10,message)
          endif

          enddo
        enddo

!!!   compute grid%emt, grid%em on global domain, and only on task 0.

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
              DO 172 J=(JDE-1)-4,(JDE-1)-2
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

   696  continue
        ENDIF ! wrf_dm_on_monitor/serial job

      call NMM_SH2O(IMS,IME,JMS,JME,ITS,NNXP,JTS,NNYP,grid%num_soil_layers,grid%isltyp, &
                             grid%sm,grid%sice,grid%stc,grid%smc,grid%sh2o)

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)

        IF (   abs(IDE-1-ITE) .eq. 1 ) THEN ! along eastern boundary
          write(message,*) 'zero phantom winds'
          CALL wrf_message(message)
          DO K=1,KDE-1
            DO J=JDS,JDE-1,2
              IF (J .ge. JTS .and. J .le. JTE) THEN
                grid%u(IDE-1,J,K)=0.
                grid%v(IDE-1,J,K)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDIF

  969   continue

         DO j = jms, jme
            DO i = ims, ime
             fisx=max(grid%fis(i,j),0.)
             grid%z0(I,J)    =grid%sm(I,J)*Z0SEA+(1.-grid%sm(I,J))*                      &
     &                (0.*Z0MAX+FISx    *FCM+Z0LAND)
            ENDDO
          ENDDO

        write(message,*) 'grid%z0 over memory, leaving module_initialize_real'
        CALL wrf_message(message)
        DO J=JME,JMS,-((JME-JMS)/20+1)
          write(message,635) (grid%z0(I,J),I=IMS,IME,(IME-IMS)/14+1)
          CALL wrf_message(message)
        ENDDO


        endif ! on first_time check

        write(message,*) 'leaving init_domain_nmm'
        CALL wrf_message( TRIM(message) )
!
       write(message,*)'STUFF MOVED TO REGISTRY:',grid%IDTAD,          &
     & grid%NSOIL,grid%NRADL,grid%NRADS,grid%NPHS,grid%NCNVC,grid%sigma
       CALL wrf_message( TRIM(message) )
#if ( HWRF == 1 )
!=========================================================================================
!   gopal's doing for ocean coupling. Produce a high resolution grid for the entire domain
!=========================================================================================

    if(internal_time_loop.eq.1) then      !Kwon's doing

     NDLMD=grid%dlmd/3.
     NDPHD=grid%dphd/3.
     NIDE=3*(IDE-1)-2
     NJDE=3*(JDE-1)-2
     ILOC=1
     JLOC=1
     NWBD= WBD  ! + (ILOC -1)*2.*grid%dlmd + MOD(JLOC+1,2)*grid%dlmd
     NSBD= SBD  ! + (JLOC -1)*grid%dphd

     ALLOCATE (NHLAT(NIDE,NJDE))
     ALLOCATE (NHLON(NIDE,NJDE))
     ALLOCATE (NVLAT(NIDE,NJDE))
     ALLOCATE (NVLON(NIDE,NJDE))
     ALLOCATE (HRES_SM(NIDE,NJDE))

#if defined(DM_PARALLEL)
       if(wrf_dm_on_monitor()) then
          ! Only the monitor process does the actual work (kinda
          ! stupid; should be parallelized, but it's better than
          ! writing garbage like it did before with >1 process)

          ! Get high-res lat & lon:
     CALL EARTH_LATLON_hwrf (  NHLAT,NHLON,NVLAT,NVLON,  &    ! rotated lat,lon at H and V points
                          NDLMD,NDPHD,NWBD,NSBD,    &
                          tph0d,tlm0d,              &
                          1,NIDE,1,NJDE,1,1,        &
                          1,NIDE,1,NJDE,1,1,        &
                          1,NIDE,1,NJDE,1,1         )

          ! Interpolate landmask to high-res grid:
          CALL G2T2H_hwrf ( SM_G,HRES_SM,   & ! output grid index and weights
                  NHLAT,NHLON,                 & ! target (hres) input lat lon in degrees
                  grid%DLMD,grid%DPHD,WBD,SBD,           & ! parent res, west and south boundaries
                  tph0d,tlm0d,                 & ! parent central lat,lon, all in degrees
               IDE,JDE,IDS,IDE,JDS,JDE,     & ! parent imax and jmax, ime,jme
                  1,NIDE,1,NJDE,1,1,           &
                  1,NIDE,1,NJDE,1,1,           &
                  1,NIDE,1,NJDE,1,1            )

          ! We're done with the low-res sm grid now:
          deallocate(SM_G)

          ! Write out high-res grid for coupler:
         WRITE(65)NHLAT(1:NIDE,1:NJDE)
         WRITE(65)NHLON(1:NIDE,1:NJDE)
         WRITE(65)NVLAT(1:NIDE,1:NJDE)
         WRITE(65)NVLON(1:NIDE,1:NJDE)
         WRITE(65)HRES_SM(1:NIDE,1:NJDE)
      endif
#else
       ! This code is the same as above, but for the non-mpi version:
       CALL EARTH_LATLON_hwrf (  NHLAT,NHLON,NVLAT,NVLON,  &    ! rotated lat,lon at H and V points
            NDLMD,NDPHD,NWBD,NSBD,    &
            tph0d,tlm0d,              &
            1,NIDE,1,NJDE,1,1,        &
            1,NIDE,1,NJDE,1,1,        &
            1,NIDE,1,NJDE,1,1         )
       CALL G2T2H_hwrf ( grid%SM,HRES_SM,                  & ! output grid index and weights
            NHLAT,NHLON,                 & ! target (hres) input lat lon in degrees
            grid%DLMD,grid%DPHD,WBD,SBD,           & ! parent res, west and south boundaries
            tph0d,tlm0d,                 & ! parent central lat,lon, all in degrees
            IDE,JDE,IMS,IME,JMS,JME,     & ! parent imax and jmax, ime,jme
            1,NIDE,1,NJDE,1,1,           &
            1,NIDE,1,NJDE,1,1,           &
            1,NIDE,1,NJDE,1,1            )

         WRITE(65)NHLAT(1:NIDE,1:NJDE)
         WRITE(65)NHLON(1:NIDE,1:NJDE)
         WRITE(65)NVLAT(1:NIDE,1:NJDE)
         WRITE(65)NVLON(1:NIDE,1:NJDE)
         WRITE(65)HRES_SM(1:NIDE,1:NJDE)
#endif

     DEALLOCATE (NHLAT)
     DEALLOCATE (NHLON)
     DEALLOCATE (NVLAT)
     DEALLOCATE (NVLON)
     DEALLOCATE (HRES_SM)

     endif                 !Kwon's doing

!==================================================================================
!   end gopal's doing for ocean coupling.
!==================================================================================
#endif

!#define COPY_OUT
!#include "scalar_derefs.inc"
      RETURN

   END SUBROUTINE init_domain_nmm

!------------------------------------------------------

  SUBROUTINE define_nmm_vertical_coord ( LM, PTSGM, pt, pdtop,HYBLEVS, &
                                         SG1,DSG1,SGML1,         &
                                         SG2,DSG2,SGML2,dfl, dfrlg            )

        IMPLICIT NONE

!        USE module_model_constants

!!! certain physical parameters here probably don't need to be defined, as defined
!!! elsewhere within WRF.  Done for initial testing purposes.

        INTEGER ::  LM, LPT2, L
        REAL    ::  PTSGM, pt, PL, PT2, pdtop
        REAL    ::  RGOG, PSIG,PHYB,PHYBM
        REAL, PARAMETER  :: Rd           =  287.04  ! J deg{-1} kg{-1}
        REAL, PARAMETER :: CP=1004.6,GAMMA=.0065,PRF0=101325.,T0=288.
        REAL, PARAMETER :: g=9.81

        REAL, DIMENSION(LM)   :: DSG,DSG1,DSG2
        REAL, DIMENSION(LM)   :: SGML1,SGML2
        REAL, DIMENSION(LM+1) :: SG1,SG2,HYBLEVS,dfl,dfrlg

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
        dfl(L)=g*T0*(1.-((pt+SG1(L)*pdtop+SG2(L)*(101325.-pt2)) &
                       /101325.)**rgog)/gamma
        dfrlg(L)=dfl(L)/g
       write(message,*) 'L, dfl(L): ', L, dfl(L)
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
       integer :: IHE(JMS:JME),IHW(JMS:JME)

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
                write(message,*) 'bad dum2d(grid%f): ', I,J,DUM2D(I,J)
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

!        write(message,*) 'psfc points (new style)'
!        CALL wrf_message(message)

!        DO J=min(JTE,JDE-1),JTS,-loopinc
!          write(message,633) (dum2d(I,J)/100.,I=ITS,min(ITE,IDE-1),iloopinc)
!          CALL wrf_message(message)
!        END DO

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

!       write(message,*) 'psfc points (traditional)'
!       CALL wrf_message(message)
!       DO J=min(JTE,JDE-1),JTS,-loopinc
!         write(message,633) (DUM2DB(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
!         CALL wrf_message(message)
!       ENDDO

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

          IF (psfc_out(I,J) .lt. 50000. ) THEN
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

          IF (psfc_out(I,J) .gt. 108000. ) THEN
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
        REAL, INTENT(OUT):: pd(IMS:IME,JMS:JME)

        CHARACTER (len=255) :: message

!	write(message,*) 'pdtop, pt, psfc_out(1,1): ', pdtop, pt, psfc_out(1,1)
!        CALL wrf_message(message)

        DO J=JTS,min(JTE,JDE-1)
          DO I=ITS,min(ITE,IDE-1)
             pd(I,J)=psfc_out(I,J)-pdtop-pt
          ENDDO
        ENDDO

        DO J=JTS,min(JTE,JDE-1)
         DO K=KDS,KDE-1
          DO I=ITS,min(ITE,IDE-1)
           p3d_out(I,J,K)=pd(I,J)*SGML2(K)+pdtop*SGML1(K)+pt

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
        CALL wrf_debug(100,message)
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
        CALL wrf_debug(100,message)
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
        allocate(sgm(lm+1))
        allocate(pps(lm))
        allocate(qqs(lm))
        allocate(y2s(lm))

    DO L=1,LM
       xnew(l)=float(l-1)/float(lm-1)
    ENDDO

    y2s=0.

    CALL spline(kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)

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

     subroutine spline(NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,q)

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
!   *  P, q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                  *
!   *                                                                  *
!   ********************************************************************
!
!   LOG:
!
!     JOVIC - July 2008 - fixed incorrectly dimensioned arrays,
!     PYLE                and do loop leading to out of bound array
!                         reference
!------
!
!     PYLE - June 2007 - eliminated use of GO TO statements.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NNEW,NOLD
      REAL,DIMENSION(NOLD),INTENT(IN) :: XOLD,YOLD
      REAL,DIMENSION(NNEW),INTENT(IN)  :: XNEW
      REAL,DIMENSION(NNEW),INTENT(OUT) :: YNEW
      REAL,DIMENSION(NOLD+2),INTENT(INOUT) :: P,q,Y2
!
      INTEGER :: K,K1,K2,KOLD,NOLDM1, K2_hold, K_hold
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
      q(1)=-RTDXC*DXR

      K=3
      first_loop: DO K=3,NOLD-1
        DXL=DXR
        DYDXL=DYDXR
        DXR=XOLD(K+1)-XOLD(K)
        DYDXR=(YOLD(K+1)-YOLD(K))/DXR
        DXC=DXL+DXR
        DEN=1./(DXL*q(K-2)+DXC+DXC)
        P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
        q(K-1)=-DEN*DXR
      END DO first_loop

      DO K=NOLDM1,2,-1
         Y2(K)=P(K-1)+q(K-1)*Y2(K+1)
         K_hold=K
      END DO

      K=K_hold

!-----------------------------------------------------------------------
      second_loop:  DO K1=1,NNEW
        XK=XNEW(K1)
        third_loop:  DO K2=2,NOLD

          IF(XOLD(K2)>XK)THEN
            KOLD=K2-1
            K2_hold=K2
            exit third_loop
          ENDIF
        K2_hold=K2
        END DO third_loop

        IF (XOLD(K2_hold) .le. XK) THEN
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
                        sm,sice,stc,smc,sh2o)

!!        INTEGER, PARAMETER:: NSOTYP=9
!        INTEGER, PARAMETER:: NSOTYP=16
        INTEGER, PARAMETER:: NSOTYP=19 !!!!!!!!MAYBE???

        REAL :: PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
        REAL :: stc(IMS:IME,NSOIL,JMS:JME), &
                smc(IMS:IME,NSOIL,JMS:JME)
        REAL :: sh2o(IMS:IME,NSOIL,JMS:JME),sice(IMS:IME,JMS:JME),&
                sm(IMS:IME,JMS:JME)
        REAL :: HLICE,GRAV,T0,BLIM
        INTEGER :: ISLTPK(IMS:IME,JMS:JME)
        CHARACTER(LEN=255) :: message

! Constants used in cold start sh2o initialization
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
        IF (smc(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
  if (K .eq. 1) then
    write(message,*) 'I,J,reducing smc from ' ,I,J,smc(I,K,J), 'to ', SMCMAX(ISLTPK(I,J))
    CALL wrf_debug(100,message)
  endif
        smc(I,K,J)=SMCMAX(ISLTPK(I,J))
        ENDIF
!tst

        IF ( (sm(I,J) .lt. 0.5) .and. (sice(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
                ISLTPK(I,J)=1
        ENDIF


! cold start:  determine liquid soil water content (sh2o)
! sh2o <= smc for t < 273.149K (-0.001C)

           IF (stc(I,K,J) .LT. 273.149) THEN

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

        if (BX .eq. 0 .or. stc(I,K,J) .eq. 0) then
                write(message,*) 'TROUBLE -- I,J,BX, stc: ', I,J,BX,stc(I,K,J)
                CALL wrf_message(message)
        endif
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPK(I,J)))))* &
                  ((stc(I,K,J)-T0)/stc(I,K,J)))** &
                  (-1/BX))*SMCMAX(ISLTPK(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              sh2o(I,K,J) = MIN ( FK, smc(I,K,J) )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O (from the Eta "NOAH" land-surface model) with the
! initial guess for sh2o from above explicit first guess.

              sh2o(I,K,J)=FRH2O_init(stc(I,K,J),smc(I,K,J),sh2o(I,K,J), &
                         SMCMAX(ISLTPK(I,J)),BETA(ISLTPK(I,J)), &
                         PSIS(ISLTPK(I,J)))

            ELSE ! above freezing
              sh2o(I,K,J)=smc(I,K,J)
            ENDIF


        ELSE   ! water point
              sh2o(I,K,J)=smc(I,K,J)

        ENDIF ! test on land/ice/sea
        if (sh2o(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
          write(message,*) 'sh2o > THAN SMCMAX ', I,J,sh2o(I,K,J),SMCMAX(ISLTPK(I,J)),smc(I,K,J)
          CALL wrf_message(message)
        endif

         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O

!-------------------------------------------------------------------

      FUNCTION FRH2O_init(TKELV,smc,sh2o,SMCMAX,B,PSIS)

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
!   smc...........Total soil moisture content (volumetric)
!   sh2o..........Liquid soil moisture content (volumetric)
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
      REAL sh2o
      REAL smc
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
! C  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), sh2o = smc
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O_init=smc

      ELSE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (CK .NE. 0.0) THEN

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCCCCCC OPTION 1: ITERATED SOLUTION FOR NONZERO CK CCCCCCCCCCC
! CCCCCCCCCCCC IN KOREN ET AL, JGR, 1999, EQN 17 CCCCCCCCCCCCCCCCC

! INITIAL GUESS FOR SWL (frozen content)
        SWL = smc-sh2o
! KEEP WITHIN BOUNDS.
         IF (SWL .GT. (smc-0.02)) SWL=smc-0.02
         IF(SWL .LT. 0.) SWL=0.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  START OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * &
             ( SMCMAX/(smc-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( smc - SWL )
         SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
         IF (SWLK .GT. (smc-0.02)) SWLK = smc - 0.02
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
        FRH2O_init = smc - SWL

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
        FRH2O_init = MIN ( FK, smc )

! CCCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

      ENDIF

        RETURN

      END FUNCTION FRH2O_init


!--------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------
!-------------------------------------------------------------------------------

 SUBROUTINE vortex ( ght_gc,rh_gc,t_gc,u_gc,v_gc,p_gc              &
     &,              ght_out,rh_out,t_out,u_out,v_out              &
     &,              ht_gc,tsk_gc,xice_gc                          &
     &,              hlat_gc,hlon_gc,vlat_gc,vlon_gc               &
     &,              greenfrac_gc,albedo12m_gc,landusef_gc         &
     &,              soilctop_gc,soilcbot_gc                       &
     &,              landusef_out,soilctop_out,soilcbot_out        &
     &,              num_veg_cat,num_soil_top_cat,num_soil_bot_cat &
     &,              dx,internal_time_loop                         &
     &,              start_z,end_z                                 &
     &,              ids,ide,jds,jde,kds,kde                       &
     &,              ims,ime,jms,jme,kms,kme                       &
     &,              its,ite,jts,jte,kts,kte                       )


 USE module_dm
 IMPLICIT NONE

 LOGICAL,EXTERNAL   :: WRF_DM_ON_MONITOR

INTEGER, INTENT(IN):: ids,ide,jds,jde,kds,kde, &
                       ims,ime,jms,jme,kms,kme, &
                       its,ite,jts,jte,kts,kte, &
                       internal_time_loop

 INTEGER, INTENT(IN):: num_veg_cat,num_soil_top_cat,num_soil_bot_cat,start_z, end_z

 REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z),INTENT(INOUT)   :: ght_gc,rh_gc,t_gc,u_gc,v_gc,p_gc
 REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z),INTENT(INOUT)   :: ght_out,rh_out,t_out,u_out,v_out
 REAL, DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT)                 :: ht_gc,tsk_gc,xice_gc
 REAL, DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT)                 :: hlat_gc,hlon_gc,vlat_gc,vlon_gc

 REAL, DIMENSION(ims:ime,jms:jme,12) , INTENT(INOUT)            :: greenfrac_gc,albedo12m_gc
 REAL, DIMENSION(ims:ime,jms:jme,num_veg_cat), INTENT(INOUT)    :: landusef_gc,landusef_out

 REAL, DIMENSION(ims:ime,jms:jme,num_soil_top_cat),INTENT(INOUT):: soilctop_gc,soilctop_out
 REAL, DIMENSION(ims:ime,jms:jme,num_soil_bot_cat),INTENT(INOUT):: soilcbot_gc,soilcbot_out
 REAL,                                             INTENT(INOUT):: dx

 REAL, PARAMETER                                 :: eradius=6371221.3
 INTEGER                                         :: i,j,l,iter,nmax,nres,nfilter
 INTEGER                                         :: id0,jd0,id1,jd1
 REAL                                            :: factor,glon0,glat0,dist,cavlat,minval_gh
 REAL                                            :: beta1,vmax,rmax,dx_km
 REAL,  DIMENSION(start_z:end_z)                 :: vmax1,rmax1
 REAL,  DIMENSION(IMS:IME,JMS:JME)               :: dist1,dir1,diffx,diffy
 REAL,  DIMENSION(IMS:IME,JMS:JME,start_z:end_z) :: pert_ght,pert_rh,pert_t,pert_u,pert_v,pert_p,temp0
!New Vortex Related
 DATA beta1 /0.0/   ! 0.0 sets non-linear balance equation vortex. GFDL vortex for beta1 between 0.1 and 1.5
 character*255 :: message
 REAL, ALLOCATABLE,DIMENSION(:,:)   :: psc
 REAL, ALLOCATABLE,DIMENSION(:,:,:) :: u_temp,v_temp,t_temp,ght_temp,rh_temp

!----------------------------------------------------------------------------
!  PURPOSE:
!         - HURRICANE VORTEX FILTERING
!         - HURRICANE VORTEX INITIALIZATION
!
!         This is gopal's doing
!----------------------------------------------------------------------------

 WRITE(0,*)'---------------- IDEAL CASE -------------------------'
 call wrf_debug(1,message)
 WRITE(message,*)'number of pressure levels',end_z
 call wrf_debug(1,message)
 WRITE(message,*)'number of vegcat   levels',num_veg_cat
 call wrf_debug(1,message)
 WRITE(message,*)'number of soiltop  levels',num_soil_top_cat
 call wrf_debug(1,message)
 WRITE(message,*)'number of soilbot  levels',num_soil_bot_cat
 call wrf_debug(1,message)
 WRITE(message,*)'--------------- ght_gc --------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)ght_gc(100,100,:)
! call wrf_debug(1,message)
 WRITE(message,*)'--------------- p_gc --------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)p_gc(100,100,:)
! call wrf_debug(1,message)
 WRITE(message,*)'--------------- rh_gc ---------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)rh_gc(100,100,:)
! call wrf_debug(1,message)
 WRITE(message,*)'--------------- t_gc --------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)t_gc(100,100,:)
! call wrf_debug(1,message)
 WRITE(message,*)'------------------------------------------------'
! call wrf_debug(1,message)
! WRITE(message,*)'------------------------------------------------'
! call wrf_debug(1,message)

!
!   SET UP IDEAL CONDITIONS
!

    do l = 1,num_veg_cat
     do j = jts, MIN(jte,jde-1)
      do i = its, MIN(ite,ide-1)
        if(l==16)THEN
           landusef_out(i,j,l)=1                    ! create ocean everywhere
        else
            landusef_out(i,j,l)=0
        endif
      enddo
     enddo
    enddo

    do l = 1,num_soil_top_cat
     do j = jts, MIN(jte,jde-1)
      do i = its, MIN(ite,ide-1)
        if(l==14)THEN
           soilctop_out(i,j,l)=1                    ! create ocean everywhere
        else
           soilctop_out(i,j,l)=0
        endif
      enddo
     enddo
    enddo
   do l = 1,num_soil_bot_cat
     do j = jts, MIN(jte,jde-1)
      do i = its, MIN(ite,ide-1)
        if(l==14)THEN
           soilcbot_out(i,j,l)=1                    ! create ocean everywhere
        else
           soilcbot_out(i,j,l)=0
        endif
      enddo
     enddo
    enddo


    landusef_gc=landusef_out       !=landusef_gc
    soilcbot_gc=soilcbot_out       !=soilcbot_gc
    soilctop_gc=soilctop_out       !=soilctop_gc


    do j = jts, MIN(jte,jde-1)
     do i = its, MIN(ite,ide-1)
      xice_gc(i,j)=0.
      ht_gc(i,j)=0.                                 ! uniform terrain
      ght_gc(i,j,1)=0.                              ! uniform gpm at level 1
      tsk_gc(i,j)=302.0                             ! uniform SSTs
     enddo
    enddo

!
! Make sure the GFDL analysis does not have temperature problems especially
! near the surface. I noticed some problems with the wps outputs (met_nmm file.)
!
    do j=jts,min(jte,jde-1)
     do i=its,min(ite,ide-1)
       if(t_gc(i,j,1) .le. 250.0)t_gc(i,j,1)=302.5
     enddo
    enddo
!
!     READ IN LAT-LONS FROM STORM MESSAGE FILE
!
  IF (WRF_DM_ON_MONITOR()) THEN
!orig   OPEN(21,file='../MESSAGES/storm.center',status='old')
!repository messages
   OPEN(21,file='storm.center',status='old')
   read(21,*)glat0
   read(21,*)glon0
   close(21)
  ENDIF

  CALL wrf_dm_bcast_bytes (glat0,IWORDSIZE)
  CALL wrf_dm_bcast_bytes (glon0,IWORDSIZE)

  WRITE(message,*)'glat0 and glon0',glat0,glon0
  call wrf_debug(1,message)

!
!
! FIND THE CENTER OF THE HURRICANE BASED ON THE STORM MESSAGE FILE
!
  id0  = -999
  jd0  = -999
  factor = 4.0*ATAN(1.)/180.
  dist  = -1
  do j=jts,min(jte,jde-1)
    do i=its,min(ite,ide-1)
     diffx(i,j) = (hlon_gc(i,j) - glon0)*factor
     diffy(i,j) = (hlat_gc(i,j) - glat0)*factor
     cavlat= cos((hlat_gc(i,j) + glat0)*0.5*factor)
     dist1(i,j) = eradius*sqrt(cavlat*cavlat*diffx(i,j)*diffx(i,j) + diffy(i,j)*diffy(i,j))
     if(dist1(i,j).LE.dist .OR. dist.LT.0.)then
       dist  = dist1(i,j)
       id0 = i
       jd0 = j
     endif
    enddo
  enddo

!
! SECONDARY SEARCH FOR THE CENTER
!

  id1=-999
  jd1=-999
  minval_gh=MINVAL(ght_gc(ids:ide-1,jds:jde-1,10))
  write(message,*)'MIN VAL OF GH NEW=',minval_gh
  call wrf_debug(1,message)
  do j=jts,min(jte,jde-1)
   do i=its,min(ite,ide-1)
      if(minval_gh .EQ. ght_gc(i,j,10))then
       id1=i
       jd1=j
      endif
    enddo
  enddo
! jbao-put storm in center
        id1=ide/2
        jd1=jde/2
! end jbao-put storm in center


  IF(abs(ID0-ID1) .GE. 5 .OR. abs(JD0-JD1) .GE. 5)THEN
!   call wrf_error_fatal("LAT LON INFO IN STROM MESSAGE FILE IN INCORRECT")
    write(message,*) 'WARNING - ACTUAL STORM LOCATION DIFFERENT FROM MODEL LOCATION'
    call wrf_debug(1,message)
    WRITE(message,*)'OLD CENTER',ID0,JD0
    call wrf_debug(1,message)
    WRITE(message,*)'NEW CENTER',ID1,JD1
    call wrf_debug(1,message)
  ELSE
    WRITE(message,*)'OLD CENTER',ID0,JD0
    call wrf_debug(1,message)
    WRITE(message,*)'NEW CENTER',ID1,JD1
    call wrf_debug(1,message)
  ENDIF

!
! RECOMPUTE RADIUS FROM THE CENTER BASED ON MODEL/ANALYSED STORM CENTER
!
  do j=jts,min(jte,jde-1)
    do i=its,min(ite,ide-1)
     diffx(i,j) = (hlon_gc(i,j) - hlon_gc(id1,jd1))*factor
     diffy(i,j) = (hlat_gc(i,j) - hlat_gc(id1,jd1))*factor
     cavlat= cos((hlat_gc(i,j) + hlat_gc(id1,jd1))*0.5*factor)
     dist1(i,j) = eradius*sqrt(cavlat*cavlat*diffx(i,j)*diffx(i,j) + diffy(i,j)*diffy(i,j))
   enddo
  enddo


 ght_out=ght_gc
 t_out=t_gc
 rh_out=rh_gc
 u_out=u_gc
 v_out=v_gc

!
! Beta is the intensity parameter
!

  IF(BETA1 .NE. 0.)THEN   ! Adjustment of the GFDL vortex

!
! DEFINE THE NUMBER OF RECRUSSIVE OPERATIONS (NMAX) REQUIRED TO
! REMOVE OR MODIFY THE INITIAL GRIB ANALYSIS. IF beta1 IS SET TO
! < 0.01, THE VORTEX IS COMPLETELY FILTERED OUT. ALSO, FILTERING
! RELATED TO COMPLETE VORTEX REMOVAL IS ONLY CALLED AT THE INITIAL
! TIME ASSUMING THAT THE VORTEX IS FAR AWAY FROM THE BOUNDARIES.
!

    nres=0.18/dx
    nfilter=1
!
    if(nres==3.and. internal_time_loop .eq. 1)then
      nmax=9*nfilter
    else
      nmax=1*nfilter
    endif

    WRITE(message,*)'CALLING HBFILTER'
    call wrf_debug(1,message)

    CALL hbfilter ( ght_gc,ght_out               &
        &,          nmax,start_z,end_z           &
        &,          ids,ide,jds,jde              &
        &,          ims,ime,jms,jme              &
        &,          its,ite,jts,jte              )

    WRITE(message,*)'COMPLETED FILTERING ght_gc'
    call wrf_debug(1,message)

    CALL hbfilter ( t_gc,t_out                   &
        &,          nmax,start_z,end_z           &
        &,          ids,ide,jds,jde              &
        &,          ims,ime,jms,jme              &
        &,          its,ite,jts,jte              )

    WRITE(message,*)'COMPLETED FILTERING t_gc'
    call wrf_debug(1,message)

    CALL hbfilter ( rh_gc,rh_out                 &
        &,          nmax,start_z,end_z           &
        &,          ids,ide,jds,jde              &
        &,          ims,ime,jms,jme              &
        &,          its,ite,jts,jte              )

    WRITE(message,*)'COMPLETED FILTERING rh_gc'
    call wrf_debug(1,message)

    CALL hbfilter ( u_gc,u_out                   &
        &,          nmax,start_z,end_z           &
        &,          ids,ide,jds,jde              &
        &,          ims,ime,jms,jme              &
        &,          its,ite,jts,jte              )

    WRITE(message,*)'COMPLETED FILTERING u_gc'
    call wrf_debug(1,message)

    CALL hbfilter ( v_gc,v_out                   &
        &,          nmax,start_z,end_z           &
        &,          ids,ide,jds,jde              &
        &,          ims,ime,jms,jme              &
        &,          its,ite,jts,jte              )

    WRITE(message,*)'COMPLETED FILTERING v_gc'
    call wrf_debug(1,message)

    WRITE(message,*)'END OF HBFILTER'
    call wrf_debug(1,message)
!
    do l=start_z,end_z
     do j=jts,min(jte,jde-1)
      do i=its,min(ite,ide-1)
         pert_ght(i,j,l) =  (ght_gc(i,j,l) - ght_out(i,j,l))      ! vortex only
         pert_t(i,j,l)   =  (t_gc(i,j,l)   - t_out(i,j,l))
         pert_rh(i,j,l)  =  (rh_gc(i,j,l)  - rh_out(i,j,l))
         pert_u(i,j,l)   =  (u_gc(i,j,l)   - u_out(i,j,l))
         pert_v(i,j,l)   =  (v_gc(i,j,l)   - v_out(i,j,l))
      enddo
     enddo
    enddo
!
    do l=start_z,end_z
     do j=jts,min(jte,jde-1)
      do i=its,min(ite,ide-1)
         ght_gc(i,j,l) = ght_out(i,j,l) +  beta1*pert_ght(i,j,l)     ! vortex only
         t_gc(i,j,l)   = t_out(i,j,l)   +  beta1*pert_t(i,j,l)
         rh_gc(i,j,l)  = rh_out(i,j,l)  +  beta1*pert_rh(i,j,l)
         u_gc(i,j,l)   = u_out(i,j,l)   +  beta1*pert_u(i,j,l)
         v_gc(i,j,l)   = v_out(i,j,l)   +  beta1*pert_v(i,j,l)
      enddo
     enddo
    enddo
!
!    DIAGNOSTICS

     ght_out=0.;t_out=0.;rh_out=0.;u_out=0.;v_out=0.
     do l=start_z,end_z
      do j=jts,min(jte,jde-1)
        do i=its,min(ite,ide-1)
         ght_out(i,j,l)= beta1*pert_ght(i,j,l)
         t_out(i,j,l)  = beta1*pert_t(i,j,l)
         rh_out(i,j,l) = beta1*pert_rh(i,j,l)
         u_out(i,j,l)  = beta1*pert_u(i,j,l)
         v_out(i,j,l)  = beta1*pert_v(i,j,l)
       enddo
      enddo
   enddo

  ELSE !  Balanced vortex option

!jwb   IF(internal_time_loop .eq. 1)THEN  ! use balancing only at the initial time
   IF(internal_time_loop .le. 2)THEN  ! use balancing only at the initial time


!
! INSERT NEW VORTEX USING THE BALANCING ALGORITHM DEVELOPED BY ZHIHUA ZENG &
! JIAN-WEN BAO. THIS ALGORITHM IS BASED ON INVERSE BALANCE EQUATION
!

      WRITE(message,*)'CALLING TCBOGUS'
      call wrf_debug(1,message)

      allocate(u_temp(ide,jde,end_z));allocate(v_temp(ide,jde,end_z))
      allocate(t_temp(ide,jde,end_z));allocate(ght_temp(ide,jde,end_z))
      allocate(rh_temp(ide,jde,end_z));allocate(psc(ide,jde))

      dx_km=110000.0*dx    ! dx in kilometers

      CALL  tcbogus( u_temp,v_temp,t_temp,ght_temp,rh_temp,psc,  &
                     dx_km,id1,jd1,                              &
                     start_z,end_z+1,                            &
                     ids,ide,jds,jde,                            &
                     ids,ide,jds,jde,                            &
                     ids,ide,jds,jde                             )

!    DIAGNOSTICS AND REVERSAL OF LOOP

     ght_out=0.;t_out=0.;rh_out=0.;u_out=0.;v_out=0.
     do l=start_z,end_z
      do j=jts,min(jte,jde-1)
        do i=its,min(ite,ide-1)
         if(l .lt.end_z)ght_out(i,j,l+1)= ght_temp(i,j,end_z-l+1)
         t_out(i,j,l)  = t_temp(i,j,end_z-l+1)
         rh_out(i,j,l) = rh_temp(i,j,end_z-l+1)
         u_out(i,j,l)  = u_temp(i,j,end_z-l+1)
         v_out(i,j,l)  = v_temp(i,j,end_z-l+1)
        enddo
      enddo
     enddo

      do l=start_z,end_z
       do j=jts,min(jte,jde-1)
        do i=its,min(ite,ide-1)
         ght_gc(i,j,l) = ght_out(i,j,l)
         t_gc(i,j,l)   = t_out(i,j,l)
         rh_gc(i,j,l)  = rh_out(i,j,l)
         u_gc(i,j,l)   = u_out(i,j,l)
         v_gc(i,j,l)   = v_out(i,j,l)
         if(l .eq. 1)p_gc(i,j,1)=psc(i,j)
        enddo
       enddo
     enddo

      WRITE(message,*)'--------------- new ght_gc --------------------------'
!      call wrf_debug(1,message)
!      WRITE(message,*)ght_gc(100,100,:)
!      call wrf_debug(1,message)
      WRITE(message,*)'--------------- new p_gc --------------------------'
!      call wrf_debug(1,message)
!      WRITE(message,*)p_gc(100,100,:)
!      call wrf_debug(1,message)
      WRITE(message,*)'--------------- new rh_gc ---------------------------'
!      call wrf_debug(1,message)
!      WRITE(message,*)rh_gc(100,100,:)
!      call wrf_debug(1,message)
      WRITE(message,*)'--------------- new t_gc --------------------------'
!      call wrf_debug(1,message)
!      WRITE(message,*)t_gc(100,100,:)
!      call wrf_debug(1,message)
!      WRITE(message,*)'---------------------------------------------------'
!      call wrf_debug(1,message)

      WRITE(0,*)'tcbogus completed'
      WRITE(message,*)'tcbogus completed'
      call wrf_debug(1,message)

   ENDIF  ! for internal_time_loop .eq. 1

  ENDIF  ! endif for beta=0.- 1.5

 END SUBROUTINE vortex

!-------------------------------------------------------------------------
!   SUBROUTINE hbfilter
!   -------------------
!   input              - field to be filtered
!   output             - filtered output
!   nmax               - recrussive application of filter ( suggested 3 for 9 km and 27 for 3 km)
!   start_z,end_zstart - usually 1 to ktop
!   ids,ide,jds,jde    - domain index, namely,imin,imax,jmin,jmax
!   ims,ime,jms,jme    - memory index, for 1 processor set this to above values
!   its,ite,jts,jte    - tile index, for 1 processor set this to above values


 SUBROUTINE hbfilter ( input, output        &
     &,                nmax,start_z,end_z   &
     &,                ids,ide,jds,jde      &
     &,                ims,ime,jms,jme      &
     &,                its,ite,jts,jte      )
 IMPLICIT NONE

 INTEGER, INTENT(IN)                                          :: ids,ide,jds,jde,ims,ime,jms,jme,its,ite,jts,jte
 INTEGER, INTENT(IN)                                          :: start_z,end_z,nmax
 REAL                                                         :: dx
 REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z),INTENT(IN)    :: INPUT
 REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z),INTENT(INOUT) :: OUTPUT

! local

 INTEGER :: I,J,K,N,JMIN,JMAX,IMIN,IMAX,NRES,REPEAT
 REAL    :: PI
 REAL, DIMENSION(11) :: M
 DATA M /2,3,4,2,5,6,7,2,8,9,2/
 INTEGER, PARAMETER          :: NF=11
 REAL, DIMENSION(NF)         :: FK
 REAL, DIMENSION(IMS:IME,NF) :: XTU
 REAL, DIMENSION(JMS:JME,NF) :: YTU


!------------------------------------------------------------------
!
!     Purpose: This routine filters and removes
!              hurricane signals (Kurihara et al., 1993, MWR)
!
!     Called from: vortex
!
!     Original Code: Qingfu Liu (EMC)
!     Modification history: This is gopal's doing
!-----------------------------------------------------------------
!
!   DOMAIN

    IMIN=1     !(1,ID0-40)
    IMAX=IDE-1 !min(IDE-1,ID0+40)
    JMIN=1     !max(1,JD0-67)
    JMAX=JDE-1 !min(JDE-1,JD0+67)

!   INPUT:  Local variable defined for testing

    PI=4.0* ATAN(1.0)
    DO K=start_z,end_z
    DO J=JMIN,JMAX
    DO I=IMIN,IMAX
      OUTPUT(I,J,K)= INPUT(I,J,K) !SIN(2.0*PI/(21-1)*(I-1)) ! INPUT(I,J,K)
    ENDDO
    ENDDO
    ENDDO

!   DEFINE FILTER FUNCTION

    DO N=1,NF
      FK(N)=0.5/(1-COS(2.*PI/M(N)))
    ENDDO

    DO K=start_z,end_z

!.. DO ZONAL FILTER

     DO REPEAT=1,nmax
       DO J=JMIN,JMAX-1
        DO N=1,NF
         XTU(IMIN,N)   = OUTPUT(IMIN,J,K)
         XTU(IMAX,N)   = OUTPUT(IMAX,J,K)
        ENDDO

        DO I=IMIN+1,IMAX-1
         XTU(I,1) = OUTPUT(I,J,K)+FK(1)*(OUTPUT(I-1,J,K)+OUTPUT(I+1,J,K)-2.*OUTPUT(I,J,K))
        ENDDO

        DO N=2,NF
        DO I=IMIN+1,IMAX-1
           XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.*XTU(I,N-1))
        ENDDO
        ENDDO

        DO I=IMIN,IMAX-1
           OUTPUT(I,J,K) = XTU(I,NF)
        ENDDO
       ENDDO ! J loop
     ENDDO   ! End recrussive repeat

!.. DO MERIDIONAL FILTER

     DO REPEAT=1,nmax
       DO I=IMIN,IMAX
        DO N=1,NF
         YTU(JMIN,N) = OUTPUT(I,JMIN,K)
         YTU(JMAX,N) = OUTPUT(I,JMAX,K)
        ENDDO

        DO J=JMIN+1,JMAX-1
         YTU(J,1) = OUTPUT(I,J,K) + FK(1)*(OUTPUT(I,J-1,K) + OUTPUT(I,J+1,K) -2.*OUTPUT(I,J,K))
        ENDDO

        DO N = 2,NF
        DO J = JMIN+1,JMAX-1
         YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  + YTU(J+1,N-1) - 2.*YTU(J,N-1))
        ENDDO
        ENDDO

        DO J = JMIN,JMAX-1
         OUTPUT(I,J,K)   =  YTU(J,NF)
        ENDDO
       ENDDO ! I loop
     ENDDO   ! End recrussive repeat

    ENDDO  ! K loop
    RETURN


 END SUBROUTINE hbfilter
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++c
!  This program performs the idealized vortex initialization for ARW_WRF    c
!  was written by Zhihua Zeng (Shanghai Typhoon Institute/CMA) and Jian-Wen c
!  Bao (NOAA/PSD) in 2009.                                                  c
!                                                                           c
!  Contact Eamil:                                                           c
!           zengzh@mail.typhoon.gov.cn or Jian-Wen.Bao@noaa.gov             c
!                                                                           c
!  References:                                                              c
!           Wang Y.,1995:On inverse balance equation in sigma-coordinates   c
!                   for model initialization, MWR, 123, 482-488.            c
!           Zeng Z. et al,2009: Simulation of spiral rainbands and vertical c
!                   resolution, 10th annual WRF user's workshop,23-26 June, c
!                   Boulder,CO.                                             c
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  This program performs the vortex initialisation using a nonlinear
!  balance equation in sigma coordinate following  Wang  (1995, MWR)
!  but with some modifications.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine  tcbogus( uw,vw,tw,ghw,rhw,psc         &
     &,                    dsc,id1,jd1                  &
     &,                    start_z,end_z1               &
     &,                    ids,ide,jds,jde              &
     &,                    ims,ime,jms,jme              &
     &,                    its,ite,jts,jte              )
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'

      INTEGER, INTENT(IN):: start_z,end_z1,id1,jd1
      INTEGER, INTENT(IN):: ids,ide,jds,jde,ims,ime,jms,jme,its,ite,jts,jte
!
      REAL,                                          INTENT(IN)    :: dsc
      REAL, DIMENSION(IMS:IME,JMS:JME),              INTENT(INOUT) :: psc
      REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z1),INTENT(INOUT) :: uw,vw,tw,ghw,rhw

! local variables
      INTEGER, PARAMETER                                :: nv=30
      INTEGER                                           :: i,j,k,km,kd,indx,jtc,itc
      INTEGER                                           :: ji,ii,itime,jjt,nwest
      REAL                                              :: f0,beta0,beta,pia,betk,txc,tyc,sigma
      REAL                                              :: tk,t6,rh,rm,vm0,rm0,ts
      REAL, DIMENSION(JMS:JME)                          :: fc,fe
      REAL, DIMENSION(start_z:end_z1)                   :: up1,pp1,pp1o
      REAL, DIMENSION(start_z:end_z1-1)                 :: up,tvb
      REAL, DIMENSION(IMS:IME,JMS:JME)                  :: betkc
      REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z1-1) :: uc,vc,tc,rqc,ppc
      REAL, DIMENSION(IMS:IME,JMS:JME,start_z:end_z1-1) :: tvbc,ghc,rhc
      character*255 message
!
      common /comp1/tk(kmx),t6(kmx1),rh(kmx),rm,vm0,ts
      km=end_z1-1
!
!*******************************************************************
      open(22,file='sigma.d',form='formatted')
      do k=1,end_z1
      read(22,*) kd, sig(k)
      enddo
  5   format(2x,i2,4x,f10.6)
      close(22)

      do k=1,km
      sig1(k)=0.5*(sig(k)+sig(k+1))
      enddo
!
      open(20,file='input.d',form='formatted',status='unknown')
      read(20,1) indx
      read(20,2) nenv
      read(20,3) vm0
      read(20,3) rm0
!     read(20,4) tsf0
      close(20)
  1   format(i1)
  2   format(i2)
  3   format(f5.1)
  4   format(f8.2)
!
      rm=rm0*1000.0
!
!     write(6,*) vm0,rm0
!     write(6,*) nenv
!
      do k=1,km
      up(k)=0.0
      enddo
      do k=1,end_z1
      up1(k)=0.0
      enddo
      jjt=0.0
!
      nwest=-(ide-1)/2+25
      jjt=max(jjt,nwest)
      write(6,*) jjt
!
      txc=id1  !(float(ide)+1.0)/3.0 !2.0
      tyc=jd1  !(float(jde)+1.0)/3.0 !2.0
      write(6,*)'txc= ', txc,'tyc= ',tyc
!
      call tem(tk,t6,rh,km,end_z1,pp1)
!
      ts=t6(end_z1)
      write(6,*) ts
!-------------------------------------
!--to calculate the Coriolis parameter
!-------------------------------------
      pia=4.0*atan(1.0)/180.0
      f0=14.584e-5*sin(pia*fi0)
      beta=14.584e-5*cos(pia*fi0)/6.3712e6
      if(indx.eq.0) beta=0.0
      do i=1,jde
      fc(i)=f0+beta*(float(i)-tyc)*dsc
      enddo
!---------------------------------------------------
      call env(tc,psc,fc,ide,jde,dsc,tyc,up1,km,end_z1)
!---------------------------------------------------
      do k=1,km
      do i=1,jde
      do j=1,ide
      tc(j,i,k)=tk(k)+tc(j,i,k)
      enddo
      enddo
      enddo
!
      call result(uc,vc,tc,rqc,psc,fc,ide,jde,dsc,tyc,txc, &
              up1,up,km,end_z1,beta)

      write(message,*)'out of result'
      call wrf_debug(1,message)
!
      itime=0.0
!
      do k=1,km
       qb(k)=rqc(1,1,k)
      enddo
!
      do i=1,jde
      do k=1,km
      do j=1,ide
      ppc(j,i,k)=sig1(k)*psc(j,i)
      enddo
      enddo
      enddo
!
!--------------------------------------
!--to calculate the height of the u,v,t
!--------------------------------------
      do k=1,km
      tvb(k)=tk(k)*(1.0+0.608*qb(k))
      enddo
      betk=(rgas/grv)*log(1.0/sig1(km))
      poz(km)=betk*tvb(km)
!     write(10,*) poz(km)
      do k=km-1,1,-1
      betk=0.5*(rgas/grv)*log(sig1(k+1)/sig1(k))
      poz(k)=poz(k+1)+betk*(tvb(k)+tvb(k+1))
!     write(10,*) poz(k)
      enddo
!
      write(message,*)'before interpolation'
      call wrf_debug(1,message)
      call interpz(uc,vc,tc,ppc,rqc,ide,jde,km)
!
      if(vm0.gt.1.0) then
!
!--------------------------------------
      do k=1,km
       do i=1,jde
        do j=1,ide
      rhc(j,i,k)=rh(k)
      fe(i)=0.0
        enddo
       enddo
      enddo

!--------------------------------------
      do k=1,km
       do i=1,jde
        do j=1,ide
      tvbc(j,i,k)=tc(j,i,k)*(1.0+0.608*rqc(j,i,k))
        enddo
       enddo
      enddo
       do i=1,jde
        do j=1,ide
      betkc(j,i)=(rgas/grv)*log(0.01*psc(j,i)/pp1(km))  !! psc is hPa
      ghc(j,i,km)=betkc(j,i)*tvbc(j,i,km)
        enddo
       enddo
      do k=km-1,1,-1
       do i=1,jde
        do j=1,ide
!        write(63,*)k,i,j,pp1(k+1),pp1(k)
        if(pp1(k) .gt. 0.)then
         betkc(j,i)=0.5*(rgas/grv)*log(pp1(k+1)/pp1(k))
        else
         betkc(j,i)=0.0
        endif
         ghc(j,i,k)=ghc(j,i,k+1)+betkc(j,i)*(tvbc(j,i,k)+tvbc(j,i,k+1))
        enddo
       enddo
      enddo
!
      write(message,*)'---------- test write out -----------'
      call wrf_debug(1,message)
      do k=1,km
       do i=1,jde
        do j=1,ide
         uw(j,i,k)=uc(j,i,k)
         vw(j,i,k)=vc(j,i,k)
         tw(j,i,k)=tc(j,i,k)
         ghw(j,i,k)=ghc(j,i,k)
         rhw(j,i,k)=rhc(j,i,k)
         if(i .eq. 10 .and. j .eq. 10) then 
            write(message,*)k,uw(j,i,k),vw(j,i,k),ghw(j,i,k),tw(j,i,k)
            call wrf_debug(1,message)
         endif
         if(i .eq. 100 .and. j .eq. 98) then
            write(message,*)k,uw(j,i,k),vw(j,i,k),ghw(j,i,k),tw(j,i,k)
            call wrf_debug(1,message)
         endif
        enddo
       enddo
      enddo

      endif
!
      return
      end subroutine  tcbogus
!
!-----------------------------------------------------------------
!
      subroutine result(u,v,t,rq,ps,f,lq,lp,ds,ty0,tx0,up1, &
                       up,km,end_z1,beta)
!
!-----------------------------------------------------------------
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'
      integer i,j,k,lq,lp,km,end_z1,nnt,jti,iti
      real ds,ds2,err,uee,tx0,ty0,beta,ajac,vor,forc1,forc2,forc3
      real ees,rqs,sgg,reh,ta,pp,alpi
      real u(lq,lp,km),v(lq,lp,km),t(lq,lp,km),rq(lq,lp,km),ps(lq,lp)
      real alp(lq,lp),tt(lq,lp,end_z1),tt2(2:km),xx(lq,lp),forc(lq,lp)
      real ux(lq,lp),vy(lq,lp),tt1(lq,lp,km),gg1(lq,lp,end_z1),f(lp)
      real up1(end_z1),up(km)
      real tk,t6,rh,rm,vm0,ts
      character*255 message
      common /comp1/tk(kmx),t6(kmx1),rh(kmx),rm,vm0,ts
!
      ds2=ds*ds
      err=3.0e-6
      do i=1,lp
      do j=1,lq
       forc(j,i)=0.0
       alp(j,i)=log(ps(j,i))
      enddo
      enddo
!
      uee=up1(end_z1)

      WRITE(message,*)'CALLING VORTEX 1'
      call wrf_debug(1,message)

      call vortex1(ux,vy,1.0,ds,lq,lp,ty0,tx0,uee)
      do 1 i=2,lp-1
      do 1 j=2,lq-1
      ajac=((ux(j+1,i)-ux(j-1,i))*(vy(j,i+1)-vy(j,i-1))-(ux(j,i+1) &
         -ux(j,i-1))*(vy(j+1,i)-vy(j-1,i)))/(2.0*ds2)
      vor=f(i)*(vy(j+1,i)-vy(j-1,i)-ux(j,i+1)+ux(j,i-1))/(2.0*ds)
      forc(j,i)=ds2*(ajac+vor-beta*ux(j,i))/(rgas*ts)
  1   continue
      call posn(alp,forc,5.0e-10,1,lq,lp)

      do i=1,lp
      do j=1,lq
        ps(j,i)=exp(alp(j,i))
      enddo
      enddo
     do 2 i=1,lp
      do 2 j=1,lq
      do 2 k=1,km
      t(j,i,k)=t(j,i,k)-tk(k)
   2  continue
!
      do 5 k=1,end_z1
!
      sgg=sig(k)
      uee=up1(k)

      WRITE(message,*)'CALLING VORTEX 2',k
      call wrf_debug(1,message)

      call vortex1(ux,vy,sgg,ds,lq,lp,ty0,tx0,uee)
      do 3 i=2,lp-1
      do 3 j=2,lq-1
      ajac=((ux(j+1,i)-ux(j-1,i))*(vy(j,i+1)-vy(j,i-1))-(ux(j,i+1) &
         -ux(j,i-1))*(vy(j+1,i)-vy(j-1,i)))/(2.0*ds2)
      vor=f(i)*(vy(j+1,i)-vy(j-1,i)-ux(j,i+1)+ux(j,i-1))/(2.0*ds)
      gg1(j,i,k)=ajac+vor-beta*ux(j,i)
  3   continue
!
  5   continue
!
      nnt=0
   6  nnt=nnt+1
!
      do 8 j=1,lq
      do 8 i=1,lp
      do k=2,km
      tt2(k)=t(j,i,k-1)+(t(j,i,k)-t(j,i,k-1))*(sig(k)-sig1(k-1)) &
              /(sig1(k)-sig1(k-1))
      enddo
      do k=1,km
      if(k.eq.1) then
      tt(j,i,k)=(tt2(k+1)-t(j,i,k))/log(sig(k+1)/sig1(k))
      else if(k.eq.km) then
      tt(j,i,k)=(t(j,i,k)-tt2(k))/log(sig1(k)/sig(k))
      else
      tt(j,i,k)=(tt2(k+1)-tt2(k))/log(sig(k+1)/sig(k))
      endif
      enddo
   8  continue
!
      do 15 k=1,km
!
      do 9 j=2,lq-1
      do 9 i=2,lp-1
      alpi=alp(j-1,i)+alp(j,i-1)-4.0*alp(j,i)+alp(j+1,i)+alp(j,i+1)
      if(k.eq.1) then
      forc1=(t6(k+1)-tk(k))*alpi/log(sig(k+1)/sig1(k))
      forc3=(gg1(j,i,k+1)-gg1(j,i,k+2))*ds2/ &
                     (rgas*log(sig(k+2)/sig(k+1)))
      else
      forc1=(t6(k+1)-t6(k))*alpi/log(sig(k+1)/sig(k))
      forc3=(gg1(j,i,k)-gg1(j,i,k+1))*ds2/(rgas*log(sig(k+1)/sig(k)))
      endif
      forc2=(tt(j+1,i,k)+tt(j,i,k))*(alp(j+1,i)-alp(j,i))-(tt(j,i,k)+ &
        tt(j-1,i,k))*(alp(j,i)-alp(j-1,i))+(tt(j,i+1,k)+tt(j,i,k))*(alp &
        (j,i+1)-alp(j,i))-(tt(j,i,k)+tt(j,i-1,k))*(alp(j,i)-alp(j,i-1))
      forc(j,i)=forc1+0.5*forc2+forc3
  9   continue
      do 10 j=1,lq
      do 10 i=1,lp
      xx(j,i)=t(j,i,k)
  10  continue
!
      call posn(xx,forc,err,nnt,lq,lp)
!
      do 12 j=1,lq
      do 12 i=1,lp
  12  t(j,i,k)=xx(j,i)
!
  15  continue
!
      if(nnt.ne.4) goto 6
!
      jti=int(tx0+0.5)
      iti=int(ty0+0.5)
!
      do k=1,km
      write(6,*) t(jti,iti,k)
      enddo
!
      do 18 k=1,km
      do 18 i=1,lp
      do 18 j=1,lq
      t(j,i,k)=t(j,i,k)+tk(k)
      reh=amin1(1.0,rh(k)*0.01)
      if(nphy.eq.0) reh=0.0
      pp=sig1(k)*ps(j,i)
      ta=t(j,i,k)
      ees=611.2*exp(cs2*(ta-cs3)/(ta-cs4))
      ees=amin1(0.5*pp,ees)
      rqs=0.622*ees/(pp-ees)
      rq(j,i,k)=reh*rqs
  18  continue
!
      do 20 k=1,km
      sgg=sig1(k)
      uee=up(k)

      WRITE(message,*)'CALLING VORTEX 3',k
      call wrf_debug(1,message)

      call vortex1(ux,vy,sgg,ds,lq,lp,ty0,tx0,uee)
      do 20 i=1,lp
      do 20 j=1,lq
     u(j,i,k)=ux(j,i)
      v(j,i,k)=vy(j,i)
  20  continue
!
      return
      end subroutine result
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      subroutine vortex1(ux,vy,sgg,ds,lq,lp,ty0,tx0,uee)
!
!----------------------------------------------------------------
      implicit none
      include 'BALANCE_PARS.F'
      character*255 message
      integer i,j,lq,lp
      real ahem,pi,sgt,sgb,sgg,ds,ty0,tx0,uee,vm,b,r0,rr,rd,ty,tx
      real ux(lq,lp),vy(lq,lp)
      real tk,t6,rh,rm,vm0,ts,vt
      common /comp1/tk(kmx),t6(kmx1),rh(kmx),rm,vm0,ts
!
      WRITE(message,*)'INSIDE VORTEX'
      call wrf_debug(1,message)
      ahem=1.0
      if(fi0.lt.0.0) ahem=-1.0
      pi=2.0*atan(1.0)
      sgt=0.15
      sgb=0.95
      if(sgg.ge.sgb) then
        vm=vm0
      else if(sgg.le.sgt) then
        vm=0.0
      else
        vm=vm0*sin(pi*(sgg-sgt)/(sgb-sgt))
      endif
!
      b=1.0
      r0=10.0*rm
      do i=1,lp
      ty=(float(i)-ty0)*ds
      do j=1,lq
      tx=(float(j)-tx0)*2.0*ds  ! factor 2.0: gopal's doing for E grid
      rr=sqrt(ty*ty+tx*tx)
      rd=rr/rm
      vt=vm/rm*(exp((1.0-rd**b)/b)-abs(rr-rm)/(r0-rm) &
          *exp((1.0-(r0/rm)**b)/b))
      if(rr.ge.r0) vt=0.0
      ux(j,i)=-vt*ty*ahem+uee
      vy(j,i)=vt*tx*ahem
     enddo
      enddo
!
      return
      end subroutine vortex1
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      subroutine posn(xx,forc,err,nnt,lq,lp)
!
!--------------------------------------------------------
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'
!
      integer i,j,nnt,lq,lp
      real xx(lq,lp),forc(lq,lp),err
      real*8 rji,ane,arm,anorm
      real*8 tem(lq,lp),wk(lq,lp),fc(lq,lp)
!
      do i=1,lp
      do j=1,lq
      tem(j,i)=xx(j,i)
      fc(j,i)=forc(j,i)
      enddo
      enddo
!
      if(nnt.eq.1.and.nenv.ne.0) then
      do 1 i=2,lp-1
      do 1 j=2,lq-1
      tem(j,i)=tem(j,i)-0.25*fc(j,i)
  1   continue
      endif
!
      nt=0
  2   nt=nt+1
!
      anorm=0.0d0
!
      do 10 i=2,lp-1
      do 10 j=2,lq-1
      rji=fc(j,i)-(tem(j-1,i)+tem(j,i-1)+ &
        tem(j+1,i)+tem(j,i+1)-4.0d0*tem(j,i))
      ane=-0.25d0*rji
      wk(j,i)=tem(j,i)+ane
      arm=abs(ane)
      if(arm.gt.anorm) anorm=arm
  10  continue
!
      do 20 i=2,lp-1
      do 20 j=2,lq-1
  20  tem(j,i)=wk(j,i)
!
      !Gopal's doing on June 17th, 2015. The line below was erroneously removed from the code.
      if(anorm.gt.err) goto 2
      write(6,*) nt,anorm,err
!
      do i=1,lp
      do j=1,lq
      xx(j,i)=tem(j,i)
      enddo
      enddo
!
      return
      end subroutine posn
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      subroutine tem(ttk,ttk1,rh,km,end_z1,pp1)
!
!----------------------------------------------------------------
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'
      integer km,end_z1,nv,k,n1
      parameter(nv=30)
      real ppk(km),ttk(km),ttk1(end_z1),tt(nv),pp(nv),rh(km)
      real pp1(end_z1)
      real rho(nv),t1,t2,p1,p2,pk,r1,r2,b,rk
!
!  Wilis Island sounding (January)
!
      open(30,file='sound.d',form='formatted',status='old')
      do k=1,nv
      read(30,*) pp(k),tt(k),rho(k)
      enddo
!  2   format(1x,f7.1,7x,f7.2,8x,f4.1)
      close(30)
!
      do k=1,nv
      tt(k)=tt(k)+273.15
      enddo
!
      do k=1,km
      ppk(k)=sig1(k)*pse/100.0
      pp1(k)=sig(k)*pse/100.0
      enddo
      pp1(end_z1)=pse/100.0
!
      do k=1,km
      pk=ppk(k)
!
      do n1=1,nv-1
      if(pp(n1).le.pk.and.pp(n1+1).gt.pk) then
      t1=tt(n1)
      t2=tt(n1+1)
      p1=pp(n1)
      p2=pp(n1+1)
      endif
      enddo
      write(6,*) '     p1, p2=',p1,p2
!
      b=(t2-t1)/(p2-p1)
      ttk(k)=t1+b*(pk-p1)
      enddo
!
      do k=2,end_z1
      pk=pp1(k)
!
      do n1=1,nv-1
      if(pp(n1).le.pk.and.pp(n1+1).gt.pk) then
      t1=tt(n1)
      t2=tt(n1+1)
      p1=pp(n1)
     p2=pp(n1+1)
      endif
      enddo
!
      b=(t2-t1)/(p2-p1)
      ttk1(k)=t1+b*(pk-p1)
      enddo
      ttk1(1)=ttk(1)
!
      do k=1,km
      pk=ppk(k)
!
      do n1=1,nv-1
      if(pp(n1).le.pk.and.pp(n1+1).gt.pk) then
      r1=rho(n1)
      r2=rho(n1+1)
      p1=pp(n1)
      p2=pp(n1+1)
      endif
      enddo
!
      b=(r2-r1)/(p2-p1)
      rk=r1+b*(pk-p1)
      rh(k)=rk
      enddo
!
     open(23,file='pp.d',form='formatted')
      do k=1,end_z1
      write(23,3) k,sig(k),pp1(k),ttk1(k)-273.15
      enddo
!
      do k=1,km
      write(23,4) k,sig1(k),ppk(k),ttk(k)-273.15,rh(k)
      enddo
  3   format(2x,'k=',i2,',   sig=',f8.4,', p=',f8.3,', t=',f7.2)
  4   format(2x,'k=',i2,',   sig=',f8.4,', p=',f8.3,', t=',f7.2,', rh=', &
         f6.3)
      close(23)
!
      return
      end subroutine tem

      subroutine env(t,ps,f,lq,lp,ds,ty0,up1,km,end_z1)
!
!------------------------------------------------------------
!  This subroutine performs initialization for a zonal flow
!  using a nonlinear balance equation in sigma coordinate.
!------------------------------------------------------------
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'
      integer i,j,k,lq,lp,km,end_z1,nnt
      real ds,ty0,forc1,forc2,forc3
      real alp(lp),tt(lp,end_z1),xx(lp),forc(lp)
      real alpy(lp),f(lp),t(lq,lp,km),ps(lq,lp)
      real tt2(2:km),up1(end_z1)
      real tk,t6,rh,rm,vm0,ts
      common /comp1/tk(kmx),t6(kmx1),rh(kmx),rm,vm0,ts
!
      it0=int(ty0+0.5)
!
      do 1 k=1,km
      do 1 i=1,lp
      do 1 j=1,lq
 1    t(j,i,k)=0.0
!
      do i=1,lp
      alpy(i)=-f(i)*up1(end_z1)/(rgas*ts)
      enddo
      alp(it0)=log(pse)
      do i=it0-1,1,-1
      alp(i)=alp(i+1)-0.5*(alpy(i)+alpy(i+1))*ds
      enddo
      do i=it0+1,lp
      alp(i)=alp(i-1)+0.5*(alpy(i)+alpy(i-1))*ds
      enddo
!
      do 2 j=1,lq
      do 2 i=1,lp
      ps(j,i)=exp(alp(i))
   2  continue
!
      nnt=0
   6  nnt=nnt+1
!
      do 8 i=1,lp
      do k=2,km
      tt2(k)=t(1,i,k-1)+(t(1,i,k)-t(1,i,k-1))*(sig(k)-sig1(k-1)) &
              /(sig1(k)-sig1(k-1))
      enddo
      do k=1,km
      if(k.eq.1) then
      tt(i,k)=(tt2(k+1)-t(1,i,k))/log(sig(k+1)/sig1(k))
      else if(k.eq.km) then
      tt(i,k)=(t(1,i,k)-tt2(k))/log(sig1(k)/sig(k))
      else
      tt(i,k)=(tt2(k+1)-tt2(k))/log(sig(k+1)/sig(k))
      endif
      enddo
   8  continue
!
      do 15 k=1,km
! Bug fix for idealized simulation by Lin Zhu. forc2. The bug is associated with
! thermal wind relationship in sheared environment.
      do 9 i=1,lp
      forc1=tt(i,k)*alpy(i)
      if(k.eq.1) then
      forc2= - f(i)*(up1(3)-up1(2))/(rgas*log(sig(3)/sig(2)))
      forc3=(t6(2)-tk(1))*alpy(i)/log(sig(2)/sig1(1))
      else
      forc2= - f(i)*(up1(3)-up1(2))/(rgas*log(sig(3)/sig(2)))
      forc3=(t6(k+1)-t6(k))*alpy(i)/log(sig(k+1)/sig(k))
      endif
      forc(i)=forc1+forc2+forc3
  9   continue
!
      xx(it0)=0.0
      do i=it0-1,1,-1
      xx(i)=xx(i+1)-0.5*(forc(i)+forc(i+1))*ds
      enddo
      do i=it0+1,lp
      xx(i)=xx(i-1)+0.5*(forc(i)+forc(i-1))*ds
      enddo
!
      do i=1,lp
      do j=1,lq
      t(j,i,k)=xx(i)
      enddo
      enddo
!
!
  15  continue
!
      if(nnt.ne.6) goto 6
!
      return
      end subroutine env
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      real function splin(x,a,b,c,d)
      implicit none
      real a,b,c,d
      real x(4)
      splin=(-a*x(1)+b*x(2)+c*x(3)-d*x(4))/81.0
      return
      end function splin
!

!------------------------------------------------------
!
      subroutine interpz(u,v,t,pp,qv,lq,lp,km)
!
!******************************************************
      implicit none
      include 'BALANCE_PARS.F'
      include 'BALANCE_COMS.F'
      integer i,j,k,lq,lp,km
      real ees,pa,ta,rqs,betk
      real u(lq,lp,km),v(lq,lp,km),qv(lq,lp,km),t(lq,lp,km)
      real tv(lq,lp,km),pp(lq,lp,km),pzz(lq,lp,km)
!***************************************************
!
      do i=1,lp
      do k=1,km
      do j=1,lq
      tv(j,i,k)=t(j,i,k)*(1.0+0.608*qv(j,i,k))
      enddo
      enddo
      enddo
!
      do i=1,lp
      do j=1,lq
      betk=(rgas/grv)*log(1.0/sig1(km))
      pzz(j,i,km)=betk*tv(j,i,km)
      enddo
      enddo
      do i=1,lp
      do k=km-1,1,-1
      do j=1,lq
      betk=0.5*(rgas/grv)*log(sig1(k+1)/sig1(k))
      pzz(j,i,k)=pzz(j,i,k+1)+betk*(tv(j,i,k)+tv(j,i,k+1))
      enddo
      enddo
      enddo
!
      do i=1,lp
      do k=1,km
      do j=1,lq
      pa=pp(j,i,k)
      ta=t(j,i,k)
      ees=611.2*exp(cs2*(ta-cs3)/(ta-cs4))
      rqs=0.622*ees/(pa-ees)
      qv(j,i,k)=qv(j,i,k)/rqs
      enddo
      enddo
      enddo
!
      call interps(u,pzz,poz,lq,lp,km)
      call interps(v,pzz,poz,lq,lp,km)
      call interps(t,pzz,poz,lq,lp,km)
      call interps(pp,pzz,poz,lq,lp,km)
     call interps(qv,pzz,poz,lq,lp,km)
!
      do i=1,lp
      do k=1,km
      do j=1,lq
      pa=pp(j,i,k)
      ta=t(j,i,k)
      ees=611.2*exp(cs2*(ta-cs3)/(ta-cs4))
      rqs=0.622*ees/(pa-ees)
      qv(j,i,k)=qv(j,i,k)*rqs
      enddo
      enddo
      enddo
!
      return
      end subroutine interpz
!
!-------------------------------------------------
!  this subroutine performs vertical interpotation
!-------------------------------------------------
!
      subroutine interps(as,zz,poz,lq,lp,km)
!
!-------------------------------------------------
     implicit none
      integer i,j,k,lq,lp,km,n1,nn
      real a1,a2,sg,sg1,sg2,b
      real as(lq,lp,km),poz(km),zz(lq,lp,km),wk(lq,lp,km)
!
      do 100 i=1,lp
      do 100 j=1,lq
!
      do 100 k=1,km
      sg=poz(k)
      if(sg.le.zz(j,i,km)) then
      n1=km-1
      else if(sg.gt.zz(j,i,1)) then
      n1=1
      else
      do nn=1,km-1
      if(sg.gt.zz(j,i,nn+1).and.sg.le.zz(j,i,nn)) then
      n1=nn
      goto 20
      endif
      enddo
  20  continue
      endif
      a1=as(j,i,n1+1)
      a2=as(j,i,n1)
      sg1=zz(j,i,n1+1)
      sg2=zz(j,i,n1)
    sg2=zz(j,i,n1)
      b=(a2-a1)/(sg2-sg1)
      wk(j,i,k)=a1+b*(sg-sg1)
!
  100 continue
!
      do 200 i=1,lp
      do 200 k=1,km
      do 200 j=1,lq
      as(j,i,k)=wk(j,i,k)
  200 continue
!
      return
      end subroutine interps



#if ( HWRF == 1 )
! compute earth lat-lons for before interpolations. This is gopal's doing for ocean coupling
!============================================================================================

SUBROUTINE EARTH_LATLON_hwrf ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
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

      DO J = JTS,MIN(JTE,JDE) !-1)                 ! H./    This loop takes care of zig-zag
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
         DO I = ITS,MIN(ITE,IDE) !-1)                         !  /
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

         DO I = ITS,MIN(ITE,IDE) !-1)
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

      DO J = JTS, MIN(JTE,JDE)  !-1)
       DO I = ITS, MIN(ITE,IDE) !-1)
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

END SUBROUTINE EARTH_LATLON_hwrf

  SUBROUTINE G2T2H_hwrf( SM,HRES_SM,                          & ! output grid index and weights
                    HLAT,HLON,                           & ! target (nest) input lat lon in degrees
                    DLMD1,DPHD1,WBD1,SBD1,               & ! parent res, west and south boundaries
                    CENTRAL_LAT,CENTRAL_LON,             & ! parent central lat,lon, all in degrees
                    P_IDE,P_JDE,P_IMS,P_IME,P_JMS,P_JME, & ! parent imax and jmax
                    IDS,IDE,JDS,JDE,KDS,KDE,             & ! target (nest) dIMEnsions
                    IMS,IME,JMS,JME,KMS,KME,             &
                    ITS,ITE,JTS,JTE,KTS,KTE              )

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
 INTEGER,    INTENT(IN   )                            :: P_IDE,P_JDE,P_IMS,P_IME,P_JMS,P_JME
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL,    DIMENSION(P_IMS:P_IME,P_JMS:P_JME), INTENT(IN)   :: SM
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(IN)   :: HLAT,HLON
 REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HRES_SM

! local

 INTEGER,PARAMETER :: KNUM=SELECTED_REAL_KIND(13)
 INTEGER           :: IMT,JMT,N2R,MK,K,I,J,DSLP0,DSLOPE,N
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
 REAL     SUM,AMAXVAL
 REAL,    DIMENSION (4, ims:ime, jms:jme ) :: NBWGT
 LOGICAL  FLIP
 REAL,    DIMENSION(IMS:IME,JMS:JME)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
 INTEGER, DIMENSION(IMS:IME,JMS:JME)  :: IIH,JJH
 character*255 message
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


  DO J =  JTS,MIN(JTE,JDE)  !-1)
    DO I = ITS,MIN(ITE,IDE) !-1)

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

!    
      ROW=TLAT/DPHD1+JMT         ! JMT IS THE CENTRAL ROW OF THE PARENT DOMAIN
      COL=TLON/DLMD1+P_IDE-1     ! (P_IDE-1) IS THE CENTRAL COLUMN OF THE PARENT DOMAIN
      NROW=INT(ROW + 0.001)     ! ROUND-OFF IS AVOIDED WITHOUT USING NINT ON PURPOSE
      NCOL=INT(COL + 0.001)
      TLAT=TLAT*D2R
      TLON=TLON*D2R

!      WRITE(60,*)'============================================================'
!      WRITE(60,*)'              ','i=',i,'j=',j
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
!          IF(TLONHC.GE.-WB-DLM)CALL wrf_error_fatal("1H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K
          IIH(I,J) = NCOL
          JJH(I,J) = NROW
          TLATHX(I,J)=TLATHC
          TLONHX(I,J)=TLONHC
!          WRITE(60,*)'HERE WE GO1: TLATHC, TLONHC'
        ELSE
!          IF(TLONHC.LE.WB+DLM)CALL wrf_error_fatal("2H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
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
!          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal("3H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K+(P_IDE-1)-1
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW+1
          TLATHX(I,J)=TLATHC+DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'HERE WE GO3:  TLATHC+DPH, TLONHC-DLM'
        ELSE
!          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal("4H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
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
!          IF(TLATHC.LE.SB+DPH)CALL wrf_error_fatal("5H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
          KOUTB(I,J)=K-(P_IDE-1)
          IIH(I,J) = NCOL-1
          JJH(I,J) = NROW-1
          TLATHX(I,J)=TLATHC-DPH
          TLONHX(I,J)=TLONHC-DLM
!          WRITE(60,*)'HERE WE GO5:  TLATHC-DPH, TLONHC-DLM'
        ELSE
!          IF(TLATHC.GE.-SB-DPH)CALL wrf_error_fatal("6H:NESTED DOMAIN TOO CLOSE TO THE BOUNDARY OF PARENT")
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

   ENDDO
  ENDDO

!
!*** EXTENSION TO NEAREST NEIGHBOR
!
     DO J =  JTS,MIN(JTE,JDE) !-1)
      DO I = ITS,MIN(ITE,IDE) !-1)
         NBWGT(1,I,J)=HBWGT1(I,J)
         NBWGT(2,I,J)=HBWGT2(I,J)
         NBWGT(3,I,J)=HBWGT3(I,J)
         NBWGT(4,I,J)=HBWGT4(I,J)
      ENDDO
     ENDDO

     DO J =  JTS,MIN(JTE,JDE) !-1)
      DO I = ITS,MIN(ITE,IDE) !-1)
       AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO
!
          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal ( "horizontal interp error - interp_hnear_nmm" )
          ENDDO


          IF((NBWGT(1,I,J)+NBWGT(2,I,J)+NBWGT(3,I,J)+NBWGT(4,I,J)) .NE. 1)THEN
            WRITE(message,*)'------------------------------------------------------------------------'
            call wrf_debug(1,message)
            WRITE(message,*)'FATAL: SOMETHING IS WRONG WITH THE WEIGHTS IN module_initialize_real.F'
            call wrf_debug(1,message)
            WRITE(message,*)'------------------------------------------------------------------------'
            call wrf_debug(1,message)
            STOP
          ENDIF

!       WRITE(66,*)I,J,NBWGT(1,I,J),NBWGT(2,I,J),NBWGT(3,I,J),NBWGT(4,I,J)

      ENDDO
     ENDDO


     DO J=MAX(3,JTS),MIN(JTE,JDE)  !-1)
      DO I=MAX(3,ITS),MIN(ITE,IDE) !-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
                HRES_SM(I,J) = NBWGT(1,I,J)*SM(IIH(I,J),JJH(I,J)  )    &
                             + NBWGT(2,I,J)*SM(IIH(I,J)+1, JJH(I,J)  )    &
                             + NBWGT(3,I,J)*SM(IIH(I,J),   JJH(I,J)-1)    &
                             + NBWGT(4,I,J)*SM(IIH(I,J),   JJH(I,J)+1)
!                WRITE(68,*)I,J,SM(IIH(I,J),JJH(I,J)),SM(IIH(I,J)+1, JJH(I,J)), &
!                               SM(IIH(I,J),   JJH(I,J)-1),SM(IIH(I,J), JJH(I,J)+1),HRES_SM(I,J)
            ELSE
                HRES_SM(I,J) = NBWGT(1,I,J)*SM(IIH(I,J),   JJH(I,J)  )  &
                             + NBWGT(2,I,J)*SM(IIH(I,J)+1, JJH(I,J)  )  &
                             + NBWGT(3,I,J)*SM(IIH(I,J)+1, JJH(I,J)-1)  &
                             + NBWGT(4,I,J)*SM(IIH(I,J)+1, JJH(I,J)+1)

!                WRITE(68,*)I,J,SM(IIH(I,J),JJH(I,J)),SM(IIH(I,J)+1, JJH(I,J)), &
!                               SM(IIH(I,J)+1, JJH(I,J)-1),SM(IIH(I,J)+1, JJH(I,J)+1),HRES_SM(I,J)

            ENDIF

      ENDDO
     ENDDO
!    Boundary treatment in J direction
     DO J=MAX(3,JTS),MIN(JTE,JDE)
       HRES_SM(2,J)=HRES_SM(3,J)
       HRES_SM(1,J)=HRES_SM(2,J)
     END DO
!    Boundary treatment in J direction and 4 corners
     DO I=ITS,MIN(ITE,IDE) 
       HRES_SM(I,2)=HRES_SM(I,3)
       HRES_SM(I,1)=HRES_SM(I,2)
     END DO


  RETURN
  END SUBROUTINE G2T2H_hwrf
!========================================================================================
! end gopal's doing for ocean coupling
!============================================================================================
#endif
END MODULE module_initialize_ideal
