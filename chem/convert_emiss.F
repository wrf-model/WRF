! This program that converts the following emissions data into 
!    into WRF input data data files. The potential fields are:
!       1) anthropogenic emissions data
!       2) biogenic emissions data
!       3) GOCART dust errosion factor
!       4) GOCART sea salt emissions factor
!       5) GOCART dimethyl sulfide emissions
!       6) Wild fire biomass burn location obtained from GOES-ABBA data 
!
!    The WRF namelist.input file is used to decide which data 
!    is to be converted when this program is executed.
!

PROGRAM convert_emissions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   USE module_machine
   USE module_domain
   USE module_io
   USE module_wrf_error
   USE module_io_wrf
   USE module_integrate 
   USE module_driver_constants
   USE module_state_description
   USE module_configure
   USE module_timing
   USE module_utility
   USE module_initialize_real
#ifdef DM_PARALLEL
   USE module_dm
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   USE module_bc
   USE module_get_file_names
#if (EM_CORE ==1)
   USE module_big_step_utilities_em
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   IMPLICIT NONE

   INTERFACE
     SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
       USE module_domain
       USE module_configure
       TYPE(domain), POINTER  :: parent , nest
     END SUBROUTINE init_domain_constants_em_ptr

   END INTERFACE


   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   INTEGER, PARAMETER  :: rnum8=4
   REAL    :: time 

   INTEGER :: loop , levels_to_process
   INTEGER :: rc

   TYPE(domain) , POINTER      :: keep_grid, grid_ptr, null_domain, grid, ingrid
   TYPE (grid_config_rec_type) :: config_flags, config_flags_in
   INTEGER                     :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: id1, id , fid, ierr
   INTEGER :: idum1, idum2 , ihour, icnt
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   REAL    :: dt_from_file, tstart_from_file, tend_from_file
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: i , j , k , idts, ntsd, emi_frame, nemi_frames
   integer     ::  kw, kg
   real        ::  top, bot,fac
   INTEGER :: debug_level = 0
   INTEGER, PARAMETER  :: iklev=55
   real,dimension(iklev) :: p_g,p_wrf
   REAL        :: p00, t00, a, p_surf, pd_surf,p_top
   integer :: kbot,ktop
   REAL, ALLOCATABLE, DIMENSION(:) :: gocart_lev
   REAL, ALLOCATABLE, DIMENSION(:,:) :: tmp2
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: tmp_oh,tmp_no3,tmp_h2o2,tmp3,interpolate
   REAL, ALLOCATABLE :: dumc0(:,:,:),dumc4(:,:,:)
   REAL, ALLOCATABLE :: dumc1(:,:)
   real mmax

#if (NMM_CORE == 1)
   REAL, ALLOCATABLE :: plvl(:,:,:)
   REAL, ALLOCATABLE :: pdsl(:,:)
#endif

   CHARACTER (LEN=80)     :: message

   CHARACTER(LEN=24) :: previous_date , this_date , next_date
   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN= 4) :: loop_char

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   REAL :: cen_lat, cen_lon, moad_cen_lat, truelat1, truelat2, gmt, stand_lon, dum1
   INTEGER :: map_proj, julyr, julday, iswater, isice, isurban, isoilwater
   INTEGER :: iswaterr,itest
   INTEGER :: itime = 0
   INTEGER :: nv = 0
   INTEGER :: nv_f = 0
   INTEGER :: nv_g = 0
   INTEGER :: itime_f = 0

   REAL :: dx,dy

   CHARACTER(LEN= 8) :: chlanduse


   CHARACTER (LEN=80) :: inpname , eminame, dum_str, wrfinname
   CHARACTER (LEN=80)       :: bdyname, bdyname2
   CHARACTER (LEN=20)       :: dname

! Number of reference emission and LAI files to open
   INTEGER, PARAMETER :: numfil=19
   INTEGER            :: status,system
! Filenames of reference emissions and LAI
   CHARACTER*100 onefil
   CHARACTER*12 emfil(numfil)
   DATA emfil/'ISO','OLI','API','LIM','XYL','HC3','ETE','OLT',  &
              'KET','ALD','HCHO','ETH','ORA2','CO','NR',        &
              'NOAG_GROW','NOAG_NONGROW','NONONAG','ISOP'/


! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_emissions, below
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

#include "version_decl"

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * rnum8 )

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "WRF-CHEM " // TRIM(release_version) // " EMISSIONS PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

!  CALL init_modules
   CALL       wrf_debug ( 100 , 'convert_emiss: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)


#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )
   
   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )
   
   CALL  wrf_message ( program_name )
   write(message,FMT='(A)') ' allocate for wrfinput_d01 '
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )
   grid => head_grid

   !  Set up time initializations.

   CALL Setup_Timekeeping ( grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL       wrf_debug ( 100 , 'convert_gocart: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'convert_gocart: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   !  Initialize the I/O for WRF.

   CALL       wrf_debug ( 100 , 'convert_gocart: calling init_wrfio' )
   CALL init_wrfio

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

!     !  Get the grid info from the wrfinput file

   CALL  wrf_debug ( 100, message )
   write(message,FMT='(A)') ' set scalars for wrfinput_d01 '
   CALL  wrf_debug ( 100, message )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   write(message,FMT='(A)') ' construct filename for wrfinput_d01 '
   CALL  wrf_debug ( 100, message )
   CALL construct_filename1( wrfinname , 'wrfinput' , grid%id , 2 )

   write(message,FMT='(A,A)') ' open file ',TRIM(wrfinname)
   CALL  wrf_message ( message )
   CALL open_r_dataset ( fid, TRIM(wrfinname) , head_grid , config_flags , "DATASET=INPUT", ierr )

! read wrfinput
!
   CALL med_initialdata_input( head_grid , config_flags )
!
   write(message,FMT='(A)') ' wrfinput open error check '
   CALL  wrf_debug ( 100, message )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' )  &
          'program convert_emiss: error opening ',TRIM(wrfinname),' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF
   write(message,FMT='(A)') ' past opening wrfinput_d01 '
   CALL  wrf_debug ( 00, message )


      CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )


   !  An available simple timer from the timing module.

   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL domain_clock_get ( grid, current_timestr=message )
   write(message,FMT='(A,A)') ' current_time ',Trim(message)
   CALL  wrf_debug ( 100, message )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   !  Figure out the starting and ending dates in a character format.

   start_year   = model_config_rec%start_year  (grid%id)
   start_month  = model_config_rec%start_month (grid%id)
   start_day    = model_config_rec%start_day   (grid%id)
   start_hour   = model_config_rec%start_hour  (grid%id)
   start_minute = model_config_rec%start_minute(grid%id)
   start_second = model_config_rec%start_second(grid%id)

   end_year   = model_config_rec%  end_year  (grid%id)
   end_month  = model_config_rec%  end_month (grid%id)
   end_day    = model_config_rec%  end_day   (grid%id)
   end_hour   = model_config_rec%  end_hour  (grid%id)
   end_minute = model_config_rec%  end_minute(grid%id)
   end_second = model_config_rec%  end_second(grid%id)

   interval_seconds    = config_flags%interval_seconds

   real_data_init_type = model_config_rec%real_data_init_type

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second
   print *,'START DATE ',start_date_char
   print *,'END DATE ',end_date_char

! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_emissions, below
#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif
       CALL get_ijk_from_grid (  grid ,                        &
                                 ids, ide, jds, jde, kds, kde,    &
                                 ims, ime, jms, jme, kms, kme,    &
                                 ips, ipe, jps, jpe, kps, kpe    )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Allocate scratch arrays
!
     ALLOCATE (dumc0(ids:ide,kds:grid%kemit,jds:jde))
!    ALLOCATE (dumc4(ids:ide-1,kds:grid%kemit,jds:jde-1))
     ALLOCATE (dumc1(ids:ide,jds:jde))

     ALLOCATE( tmp_oh(ids:ide,jds:jde,iklev) )
     ALLOCATE( tmp_h2o2(ids:ide,jds:jde,iklev) )
     ALLOCATE( tmp_no3(ids:ide,jds:jde,iklev) )
     ALLOCATE( tmp2(ids:ide,jds:jde) )
     ALLOCATE( tmp3(ids:ide,jds:jde,3) )
     ALLOCATE( interpolate(ids:ide,kds:kde,jds:jde) )
     ALLOCATE( gocart_lev(iklev))


   ihour = start_hour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   read reference fields that some of the GOCART modules require (can be run with other modules too?)
!
   if(config_flags%chem_opt == GOCART_SIMPLE               &
      .or.    config_flags%chem_opt == GOCARTRACM_KPP      &
      .or.    config_flags%dmsemis_opt == DMSGOCART        &
      .or.    config_flags%dust_opt  ==   DUSTGOCART       &
                                                           ) then
   write(message,FMT='(A)') ' READ GOCART BACKGROUND DATA '
   CALL  wrf_debug ( 00, message )
! 
#ifdef DM_PARALLEL
   IF (wrf_dm_on_monitor()) THEN
     OPEN(19,FILE='wrf_gocart_backg',FORM='UNFORMATTED')
   endif
#else
     OPEN(19,FILE='wrf_gocart_backg',FORM='UNFORMATTED')
#endif     

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)tmp2(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(tmp2)
#else
       read(19)tmp2(ids:ide  ,jds:jde  )
#endif
      grid%dms_0(ips:ipe,jps:jpe) = tmp2(ips:ipe,jps:jpe)

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)tmp3(ids:ide  ,jds:jde  ,1:3)
       ENDIF
       DM_BCAST_MACRO(tmp3)
#else
       read(19)tmp3(ids:ide  ,jds:jde  ,1:3)
#endif
      do i=1,3 
       grid%erod(ips:ipe,jps:jpe,i)=tmp3(ips:ipe,jps:jpe,i)
      enddo

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)gocart_lev(1:iklev)
       ENDIF
       DM_BCAST_MACRO(gocart_lev)
#else
       read(19)gocart_lev(1:iklev)
#endif

       write(*,*) 'GOCART_LEV: ',gocart_lev(:)

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)tmp_h2o2(ids:ide,jds:jde,1:iklev)
       ENDIF
       DM_BCAST_MACRO(tmp_h2o2)
#else
       read(19)tmp_h2o2(ids:ide,jds:jde,1:iklev)
#endif

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)tmp_oh(ids:ide,jds:jde,1:iklev)
       ENDIF
       DM_BCAST_MACRO(tmp_oh)
#else
       read(19)tmp_oh(ids:ide,jds:jde,1:iklev)
#endif

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(19)tmp_no3(ids:ide,jds:jde,1:iklev)
       ENDIF
       DM_BCAST_MACRO(tmp_no3)
#else
       read(19)tmp_no3(ids:ide,jds:jde,1:iklev)
#endif

     CLOSE(19)

!  CALL input_ext_chem_gocart_bg ( grid ) 
   write(message,FMT='(A)') ' PAST  GOCART BACKGROUND DATA'
   CALL  wrf_debug ( 00, message )

   grid%input_from_file = .false.

   write(message,FMT='(A)') ' OPEN  GOCART BACKGROUND DATA WRF file'
   CALL  wrf_debug ( 00, message )

   CALL construct_filename1( inpname , 'wrfchemi_gocart_bg' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input8 , "DATASET=AUXINPUT8", ierr )
   write(message,FMT='(A,A)') '  GOCART BACKGROUND DATA file name: ',TRIM(inpname)
   CALL  wrf_message ( message )
   
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfchem emissions file for writing' )
   ENDIF

   write(message,FMT='(A)') ' PAST OPEN  GOCART BACKGROUND DATA WRF file '
   CALL  wrf_debug ( 100, message )

   CALL calc_current_date ( grid%id , 0. )
   CALL geth_newdate ( current_date_char, current_date, 3600 )
   current_date = current_date_char // '.0000'

!     if( stand_lon  == 0. ) then
!        stand_lon = cen_lon
!     endif
 
!     if( moad_cen_lat  == 0. ) then
!        moad_cen_lat = cen_lat
!     endif


    idum1 = 1
    IF( (config_flags%chem_opt == GOCART_SIMPLE) .OR. &
        (config_flags%chem_opt == GOCARTRACM_KPP) ) then
! interpolate 3d fields to WRF in vertical
!
     do k=1,iklev
        p_g(k) = log10( gocart_lev(k) * 1023.) 
     enddo
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      call nl_get_p_top_requested (1,p_top)
     t00=290.
     a=50.
!    p00=100000.
!    p_top=5000.

#if (EM_CORE == 1) 
     do j=jps,jpe
     do i=ips,ipe
        p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
        do k=kps,kpe
           grid%pb(i,k,j) = grid%znu(k)*(p_surf - p_top) + p_top
        enddo
        do kw=kpe-1,kps,-1
         do kg=iklev-1,1,-1
            if( p_g(kg) <= log10(.01*grid%pb(i,kw,j)) ) then
               kbot = max(1,kg-1)
               ktop = max(min(kg,iklev-1),2)
            endif
         enddo ! kg loop

         fac= (tmp_no3(i  ,j  ,ktop)-tmp_no3(i  ,j  ,kbot))/ (p_g(ktop) - p_g(kbot))
         grid%backg_no3(i, kw, j) = tmp_no3(i  ,j  ,kbot)+fac*(log10(.01*grid%pb(i,kw,j))-p_g(kbot))

! OH
         fac= (tmp_oh(i  ,j  ,ktop)-tmp_oh(i  ,j  ,kbot))/ (p_g(ktop) - p_g(kbot))
         grid%backg_oh(i, kw, j) = tmp_oh(i  ,j  ,kbot)+fac*(log10(.01*grid%pb(i,kw,j))-p_g(kbot))

! H2O2
         fac= (tmp_h2o2(i  ,j  ,ktop)-tmp_h2o2(i  ,j  ,kbot) )/(p_g(ktop) - p_g(kbot))
         grid%backg_h2o2(i, kw, j) = tmp_h2o2(i  ,j  ,kbot)+fac*(log10(.01*grid%pb(i,kw,j))-p_g(kbot))

        enddo  ! kw loop

     enddo
     enddo
#endif
#if ( NMM_CORE == 1 )

   ! Get scalar grid point heights
   ALLOCATE(  plvl(ips:ipe,kps:kpe,jps:jpe) )
   ALLOCATE(  pdsl(ips:ipe,jps:jpe) )

       IF(grid%sigma.EQ. 1)THEN
         do j=jps,jpe
         do i=ips,ipe
           pdsl(i,j)=grid%pd(i,j)
         ENDDO
         ENDDO
       ELSE
         do j=jps,jpe
         do i=ips,ipe
           pdsl(i,j)=grid%res(i,j)*grid%pd(i,j)
         enddo
         enddO
       ENDIF

      do j=jps,jpe
        do k=kps,kpe
        do i=ips,ipe
          plvl(i,k,j)=grid%eta1(k)*grid%pdtop+grid%eta2(k)*pdsl(i,j)+grid%pt
        ENDDO
        ENDDO
      ENDDO

     do j=jps,jpe
     do i=ips,ipe

      do kw=kpe-1,kps,-1
       do kg=iklev-1,1,-1
          if( p_g(kg) <= log10(.01*plvl(i,kw,j)) ) then
             kbot = max(1,kg-1)
             ktop = max(min(kg,iklev-1),2)
          endif
       enddo ! kg loop

       fac= (tmp_no3(i  ,j  ,ktop)-tmp_no3(i  ,j  ,kbot))/ (p_g(ktop) - p_g(kbot))
       grid%backg_no3(i, kw, j) = tmp_no3(i  ,j  ,kbot)+fac*(log10(.01*plvl(i,kw,j))-p_g(kbot))

! OH
       fac= (tmp_oh(i  ,j  ,ktop)-tmp_oh(i  ,j  ,kbot))/ (p_g(ktop) - p_g(kbot))
       grid%backg_oh(i, kw, j) = tmp_oh(i  ,j  ,kbot)+fac*(log10(.01*plvl(i,kw,j))-p_g(kbot))

! H2O2
       fac= (tmp_h2o2(i  ,j  ,ktop)-tmp_h2o2(i  ,j  ,kbot) )/(p_g(ktop) - p_g(kbot))
       grid%backg_h2o2(i, kw, j) = tmp_h2o2(i  ,j  ,kbot)+fac*(log10(.01*plvl(i,kw,j))-p_g(kbot))

      enddo  ! kw loop

     enddo
     enddo

#endif

   ENDIF ! chem_opt gocart_simple or gocartracm

   CALL output_aux_model_input8 ( id1 , grid , config_flags , ierr )

   CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT8" )

   ENDIF ! chem_opt gocart_simple,dmsgocart,dustgocart

   DEALLOCATE( tmp_oh )
   DEALLOCATE( tmp_h2o2 )
   DEALLOCATE( tmp_no3 )
   DEALLOCATE( tmp2)
   DEALLOCATE( tmp3)
   DEALLOCATE( interpolate )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! now do fire and/or global emissions
!
     if(config_flags%emiss_opt == ecptec .or. config_flags%emiss_opt == gocart_ecptec ) then
     write(message, FMT='(A,4I6)') ' I am reading global anthropogenic emissions, dims: =',ids, ide-1, jds, jde-1
     call wrf_message( TRIM( message ) )

     CALL construct_filename1 ( bdyname , 'emissopt3' , grid%id , 2 )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
        open (92,file=bdyname,form='unformatted')
     ENDIf
#else
     OPEN(92,FILE=bdyname,FORM='UNFORMATTED')
#endif
   write(message, FMT='(A,A)') ' OPENED FILE: ',TRIM(bdyname)
   call wrf_message( TRIM( message ) )

         itest=0
         if(config_flags%emiss_opt == ecptec)itest=1
         if(config_flags%emiss_opt == gocart_ecptec)then
           itest=0
           write(message, FMT='(A)') ' I am reading emissions for gocart only (aerosols)'
           call wrf_message( TRIM( message ) )
         endif
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
       read(92)nv_g
       read(92)dname
       read(92)itime
     ENDIF
     CALL wrf_dm_bcast_bytes ( nv_g , rnum8 )
     CALL wrf_dm_bcast_bytes ( dname , rnum8 )
     CALL wrf_dm_bcast_bytes ( itime , rnum8 )
#else
     read(92)nv_g
     read(92)dname
     read(92)itime
#endif
   
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_so2)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_no)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_ald)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_hcho)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_ora2)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_nh3)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_hc3)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_hc5)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_hc8)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_eth)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_co)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_ol2)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_olt)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_oli)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_tol)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_xyl)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_ket)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_csl)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         if(itest.eq.1)grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_iso)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_pm_25)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_pm_10)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_oc)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_bc)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
!        grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_dms)=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(92)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(92)dumc1(ids:ide  ,jds:jde  )
#endif
         grid%emis_ant(ips:ipe  ,kps,jps:jpe  ,p_e_sulf)=dumc1(ips:ipe  ,jps:jpe  )

     CLOSE(92)
!
! simple file dump here....
!
         write(message,FMT='(A)') ' OPEN EMISSIONS WRF files for emissions coming from global data set'
         call wrf_message( TRIM( message ) )
         write(message, FMT='(A)') ' NO TIME DEPENDENCE IN THIS DATASET'

!
! emissions from retro/edgar/gocart
!
         CALL construct_filename1( inpname , 'wrfchemi' , grid%id , 2 )
         CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input5 , "DATASET=AUXINPUT5", ierr )

         CALL output_aux_model_input5 ( id1 , grid , config_flags , ierr )

         CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT5" )

     endif  ! emiss_opt = ecptec or gocart_ecptec
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
     if(config_flags%biomass_burn_opt == biomassb ) then

        CALL construct_filename1 ( bdyname2 , 'emissfire' , grid%id , 2 )
        write(message, FMT='(A,A)') ' TRY TO OPEN FILE: ',TRIM(bdyname2)
        call wrf_message( TRIM( message ) )
        CALL wrf_debug( 00 , 'calling fire emissions' )

#ifdef DM_PARALLEL
        IF (wrf_dm_on_monitor()) THEN
           open (93,file=bdyname2,form='unformatted',status='old')
        ENDIf
#else
        OPEN(93,FILE=bdyname2,FORM='UNFORMATTED',status='old')
#endif

#ifdef DM_PARALLEL
        IF (wrf_dm_on_monitor()) THEN
           read(93)nv_f
           read(93)dname 
           read(93)itime_f
        ENDIF
        CALL wrf_dm_bcast_bytes ( nv_f , rnum8 )
        CALL wrf_dm_bcast_bytes ( dname , rnum8 )
        CALL wrf_dm_bcast_bytes ( itime_f , rnum8 )
#else
        read(93)nv_f
        read(93)dname 
        read(93)itime_f
#endif
     write(message, FMT='(A,I10)') ' Number of fire emissions: ',nv_f
     call wrf_message( TRIM( message ) )
     write(message, '(A,I8,A,I8)') ' FIRE EMISSIONS INPUT FILE TIME PERIOD (GMT): ',itime_f-1,' TO ',itime_f
     call wrf_message( TRIM( message ) )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif

!    write(*,*) 'MAXVAL2 ',MAXVAL(dumc1(:,:))
!    write(*,*) 'MINVAL2 ',MINVAL(dumc1(:,:))

     write(message, FMT='(A)') ' put dumc1 into ebu_in_so2'
     call wrf_debug(100, TRIM( message ) )
     grid%ebu_in_so2(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
       DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_no(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_ald(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_hcho(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_ora2(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
   !   grid%ebu_in_nh3(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_hc3(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_hc5(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_hc8(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_eth(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_co(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
   !   grid%ebu_in_ol2(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_olt(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_oli(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_tol(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_xyl(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_ket(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_csl(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_iso(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_pm25(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_pm10(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_oc(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_bc(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_dms(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%ebu_in_sulf(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )

#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%mean_fct_agtf(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%mean_fct_agef(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%mean_fct_agsv(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%mean_fct_aggr(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%firesize_agtf(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%firesize_agef(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%firesize_agsv(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
#ifdef DM_PARALLEL
       IF (wrf_dm_on_monitor()) THEN
           read(93)dumc1(ids:ide  ,jds:jde  )
       ENDIF
     DM_BCAST_MACRO(dumc1)
#else
       read(93)dumc1(ids:ide  ,jds:jde  )
#endif
       grid%firesize_aggr(ips:ipe  ,jps:jpe  )=dumc1(ips:ipe  ,jps:jpe  )
!
         CLOSE(93)
!
! fire stuff
!
         CALL construct_filename1( inpname , 'wrffirechemi' , grid%id , 2 )
         write(message, FMT='(A,A)') ' NOW OPEN FILE FOR WRITE: ',TRIM(inpname)
         call wrf_message( TRIM( message ) )

         CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input7 , "DATASET=AUXINPUT7", ierr )
         write(message,FMT='(A,A)') ' FIRE EMISSIONS OUTPUT file name: ',TRIM(inpname)
         CALL  wrf_message ( message )
         IF ( ierr .NE. 0 ) THEN
            write(message, FMT='(A,I10)') ' convert_emiss: error opening wrfchem fire emissions file for writing',ierr
            CALL wrf_error_fatal( TRIM( message ) )
         ENDIF

         CALL output_aux_model_input7 ( id1 , grid , config_flags , ierr )
 
         CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT7" )

     endif ! biomassburn
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    NOW DO BIG TIMEDEPENDENT EMISSIONS FILE
!
    if(CONFIG_FLAGS%EMISS_OPT == ECBMZ_MOSAIC ) then
       write(message, FMT='(A,I10)') ' convert_emiss: error: conversion for all CBMZ emission arrays not available '
       CALL wrf_error_fatal( TRIM( message ) )
    endif
    if(CONFIG_FLAGS%EMISS_OPT == ERADM .or. CONFIG_FLAGS%EMISS_OPT == ERADMSORG .or.
       CONFIG_FLAGS%EMISS_OPT == ECBMZ_MOSAIC ) then
   !  Figure out our loop count for the processing times.
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
#endif

   time_loop = 1
   write(message,FMT='(A,I4,A,A)') 'Time period #',time_loop,' to process = ',start_date_char
   CALL  wrf_message ( message )
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate ( next_date_char , current_date_char , interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1
         write(message,FMT='(A,I4,A,A)') 'Time period #',time_loop,' to process = ',next_date_char
         CALL  wrf_message ( message )
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1
         write(message,FMT='(A,I4,A,A)') 'Time period #',time_loop,' to process = ',next_date_char
         CALL  wrf_message ( message )
         write(message,FMT='(A,I4)') 'Total analysis times to input = ',time_loop
         CALL  wrf_message ( message )
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         write(message,FMT='(A,I4)') 'Total analysis times to input = ',time_loop
         CALL  wrf_message ( message )
         time_loop_max = time_loop
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
   write(message,FMT='(A,I4,A,I4)') 'Total number of times to input = ',time_loop,' ',time_loop_max
   CALL  wrf_message ( message )
#ifdef DM_PARALLEL
     ENDIF 
   CALL wrf_dm_bcast_bytes ( time_loop , rnum8 )
   CALL wrf_dm_bcast_bytes ( time_loop_max , rnum8 )
#endif

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date

#ifdef DM_PARALLEL
   CALL wrf_dm_bcast_bytes ( start_hour , rnum8 )
#endif

   ihour = start_hour
   write(message,FMT='(A)') ' READ EMISSIONS 1'
   CALL  wrf_debug ( 100, message )
! do the ugly stuff
!
     if(start_hour.eq.0)CALL construct_filename1 ( bdyname , 'wrfem_00to12z' , grid%id , 2 )
     if(start_hour.eq.12)CALL construct_filename1 ( bdyname , 'wrfem_12to24z' , grid%id , 2 )

#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
        open (91,file=bdyname,form='unformatted',status='old')

        write(*,*) TRIM( bdyname )
     ENDIf
#else
!  write(message, FMT='(A,A)') ' OPEN FILE: ',bdyname
!  call wrf_message( TRIM( message ) )
        open (91,file=bdyname,form='unformatted',status='old')
#endif

!  write(message, FMT='(A,A)') ' OPENED FILE: ',bdyname
!  call wrf_message( TRIM( message ) )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
       read(91)nv
       read(91)dname 
       read(91)itime
     ENDIF
     CALL wrf_dm_bcast_bytes ( nv , rnum8 )
     CALL wrf_dm_bcast_bytes ( dname , rnum8 )
     CALL wrf_dm_bcast_bytes ( itime , rnum8 )
#else
     read(91)nv
     read(91)dname 
     read(91)itime
#endif
     write(message, FMT='(A,I10)') ' Number of emissions: ',nv
     call wrf_message( TRIM( message ) )
     write(message, '(A,I8,A,I8)') ' EMISSIONS INPUT FILE TIME PERIOD (GMT): ',itime-1,' TO ',itime
     call wrf_message( TRIM( message ) )

#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
     write(message, FMT='(A)') ' read dumc0 '
     call wrf_debug(100, TRIM( message ) )
        read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
     write(message, FMT='(A)') ' write dumc0 to emiss_ant'
     call wrf_debug(100, TRIM( message ) )
     write(message, FMT='(A,3I10)') ' dims:',ips  ,kps,jps
     call wrf_message( TRIM( message ) )
     write(message, FMT='(A,4I10)') ' dims:',ipe-1,kpe,jpe-1,grid%kemit
     call wrf_message( TRIM( message ) )
     write(message, FMT='(A,3I10)') ' dims:',ids  ,kds,jds
     call wrf_message( TRIM( message ) )
     write(message, FMT='(A,4I10)') ' dims:',ide-1,kde,jde-1
     call wrf_message( TRIM( message ) )
     write(message, FMT='(A,5I10)') ' dims:',size(grid%emis_ant,1),size(grid%emis_ant,2),size(grid%emis_ant,3),size(grid%emis_ant,4),p_e_so2
     call wrf_message( TRIM( message ) )
     write(message, FMT='(A,5I10)') ' dims:',size(dumc0,1),size(dumc0,2),size(dumc0,3)
     call wrf_message( TRIM( message ) )
!    write(*,*) dumc0(ids:ide-1,1,7)
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
!    write(*,*) grid%emis_ant(ips:ipe-1,1,7,p_e_so2)
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else

         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ald)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hcho)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ora2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_nh3)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc3)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc5)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc8)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_eth)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_co)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ol2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_olt)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_oli)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_tol)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_xyl)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ket)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_csl)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_iso)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm25i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm25j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so4i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so4j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no3i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no3j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_orgi)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_orgj)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_eci)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ecj)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm_10)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )

   write(message,FMT='(A)') ' PAST READ EMISSIONS 1'
   CALL  wrf_message ( message )

!  grid%input_from_file = .false.

   CALL construct_filename1( inpname , 'wrfchemi' , grid%id , 2 )

   write(message,FMT='(A,A,I10)') ' OPEN FILE ',TRIM(inpname),config_flags%io_form_auxinput5
   CALL  wrf_debug ( 100, message )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input5 , "DATASET=AUXINPUT5", ierr )
   write(message,FMT='(A,A)') ' EMISSIONS OUTPUT file name: ',TRIM(inpname)
   CALL  wrf_message ( message )
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfchem emissions file for writing' )
   ENDIF

   CALL calc_current_date ( grid%id , 0. )
   CALL geth_newdate ( current_date_char, current_date, 3600 )
   current_date = current_date_char // '.0000'
         if( stand_lon  == 0. ) then
         stand_lon = cen_lon
      endif
   
      if( moad_cen_lat  == 0. ) then
         moad_cen_lat = cen_lat
      endif

   write(message,FMT='(A)') ' WRITE EMISSIONS 1'
   CALL  wrf_message ( message )
    CALL output_aux_model_input5 ( id1 , grid , config_flags, ierr )

   CALL wrf_dm_bcast_bytes ( time_loop , rnum8 )
   nemi_frames = time_loop
  
    write(message,FMT='(A,4I10)') 'FRAMES: ',int(nemi_frames),time_loop
   CALL  wrf_message ( message )
    current_date_char = start_date_char
    current_date = current_date_char
!  if( debug_level >= -100) print *,'NEMI_FRAMES ', nemi_frames,time_loop

!  DO emi_frame = 2,1
   DO emi_frame = 2,nemi_frames
    write(message,FMT='(A,4I10)') 'LOOP: ',emi_frame,nemi_frames
   CALL  wrf_message ( message )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
     write(message,FMT='(A,I4)') 'emi_frame: ',emi_frame
     CALL  wrf_debug ( 100, message )
     CALL domain_clock_get ( grid, current_timestr=message )
     write(message,FMT='(A,A)') ' Current time ',Trim(message)
     CALL  wrf_debug ( 100, message )

     current_date_char = current_date(1:19)
     CALL geth_newdate ( next_date_char, current_date_char, int(interval_seconds) )
     current_date = next_date_char // '.0000'

     write(message,FMT='(A,A)') ' Date &  time ',Trim(current_date)
     CALL  wrf_message ( message )
     CALL domain_clockadvance( grid )

     write(message,FMT='(A,I4)') ' Read emissions ',emi_frame
     CALL  wrf_debug ( 100, message )
     write(message,FMT='(A,I4)') ' Hour ',ihour
     CALL  wrf_message ( message )
     ihour = mod(ihour + 1,24)
     if(ihour.eq.0)then
         close(91)
         CALL construct_filename1 ( bdyname , 'wrfem_00to12z' , grid%id , 2 )
         open (91,file=bdyname,form='unformatted')
     endif
     if(ihour.eq.12) then 
         close(91)
         CALL construct_filename1 ( bdyname , 'wrfem_12to24z' , grid%id , 2 )
         open (91,file=bdyname,form='unformatted')
     endif


     ENDIF
     CALL wrf_dm_bcast_bytes ( ihour , rnum8 )
!    CALL wrf_dm_bcast_bytes ( bdyname , rnum8 )
#else
     write(message,FMT='(A,I4)') 'emi_frame: ',emi_frame
     CALL  wrf_debug ( 100, message )
     CALL domain_clock_get ( grid, current_timestr=message )
     write(message,FMT='(A,A)') ' Current time ',Trim(message)
     CALL  wrf_debug ( 100, message )

     current_date_char = current_date(1:19)
     CALL geth_newdate ( next_date_char, current_date_char, int(interval_seconds) )
     current_date = next_date_char // '.0000'

     write(message,FMT='(A,A)') ' Date &  time ',Trim(current_date)
     CALL  wrf_message ( message )
     CALL domain_clockadvance( grid ) 
     write(message,FMT='(A,I4)') ' Read emissions ',emi_frame
     CALL  wrf_debug ( 100, message )
     ihour = mod(ihour + 1,24)
     if(ihour.eq.0)then
         close(91)
         CALL construct_filename1 ( bdyname , 'wrfem_00to12z' , grid%id , 2 )
         open (91,file=bdyname,form='unformatted')
     endif
     if(ihour.eq.12) then 
         close(91)
         CALL construct_filename1 ( bdyname , 'wrfem_12to24z' , grid%id , 2 )
         open (91,file=bdyname,form='unformatted')
     endif
#endif

     write(message, '(A,A,I10)') ' USING FILE: ',TRIM(bdyname),ihour
     call wrf_message( TRIM( message ) )

     if(ihour.eq.0.or.ihour.eq.12)then
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
       read(91)nv
       read(91)dname 
     ENDIF
     CALL wrf_dm_bcast_bytes ( nv , rnum8 )
     CALL wrf_dm_bcast_bytes ( dname , rnum8 )
#else
     read(91)nv
     read(91)dname 
#endif
     write(message, '(A,I10)') ' Reading FILE header: ',nv
     call wrf_message( TRIM( message ) )

     endif

#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
        read(91)itime
     ENDIF
     CALL wrf_dm_bcast_bytes ( itime , rnum8 )
#else
     read(91)itime
#endif

     write(message, '(A,I10)') ' Reading data from file: ',itime
     call wrf_message( TRIM( message ) )

#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
        read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else

         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ald)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hcho)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ora2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_nh3)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc3)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc5)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_hc8)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_eth)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_co)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ol2)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_olt)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_oli)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_tol)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_xyl)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ket)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_csl)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_iso)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm25i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm25j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so4i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_so4j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no3i)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_no3j)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_orgi)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_orgj)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_eci)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_ecj)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )
#ifdef DM_PARALLEL
     IF (wrf_dm_on_monitor()) THEN
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
     ENDIF
     DM_BCAST_MACRO(dumc0)
#else
         read(91)dumc0(ids:ide-1,kds:grid%kemit,jds:jde-1)
#endif
         grid%emis_ant(ips:ipe  ,kps:grid%kemit,jps:jpe  ,p_e_pm_10)=dumc0(ips:ipe  ,kps:grid%kemit,jps:jpe  )

     write(message,FMT='(A)') ' Past reading emissions '
     CALL  wrf_debug ( 100, message ) 

   ! write global atributes into wrf emissions file

     write(message,FMT='(A)') ' Output emissions '
     CALL  wrf_debug ( 100, message ) 
     CALL output_aux_model_input5 ( id1 , grid , config_flags , ierr )

!   idum1 = 1
   END DO   ! NEMI_FRAMES loop

!
   CLOSE(91)
   CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT5" )

     write(message,FMT='(A)') ' DONE WRITING TIME DEPENDENT EMISSIONS FILE'
     CALL  wrf_message ( message )

    endif  !    ERADM .or.ERADMSORG .or. ECBMZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    NOW DO BIOGENIC EMISSIONS FILE
!
    if(CONFIG_FLAGS%BIO_EMISS_OPT == BEIS313 ) then

   write(message,FMT='(A)') ' READ BIOGENIC EMISSIONS '
   CALL  wrf_debug ( 100, message )

!!!!!-------------------------------------------------------------------

! Loop over the file names
      DO i=1,numfil

!   Remove scratch unzipped file
       status=system('rm -f scratem*')

!   All reference emissions except NO
       IF(i.LE.15)THEN 
        onefil='../../run/BIOREF_'//             &
         TRIM(ADJUSTL(emfil(i)))//'.gz'
!   NO reference emissions
       ELSE IF(i.GE.16.AND.i.LE.18)THEN 
        onefil='../../run/AVG_'//                &
         TRIM(ADJUSTL(emfil(i)))//'.gz'
!   LAI
       ELSE
        onefil='../../run/LAI_'//                &
         TRIM(ADJUSTL(emfil(i)))//'S.gz'
       ENDIF

!   Copy selected file to scratch
       status=system('cp '//TRIM(ADJUSTL(onefil))//' scratem.gz')

!   Unzip scratch
       status=system('gunzip scratem')

!   Open scratch and read into appropriate array
       OPEN(26,FILE='scratem',FORM='FORMATTED')
       IF(i.EQ. 1) then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
             grid%sebio_iso(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 2)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_oli(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 3)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_api(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 4)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_lim(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 5)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_xyl(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 6)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_hc3(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 7)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_ete(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 8)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_olt(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ. 9)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_ket(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.10)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_ald(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.11)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_hcho(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.12)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_eth(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.13)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_ora2(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.14)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_co(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.15)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%sebio_nr(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.16)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%noag_grow(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.17)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%noag_nongrow(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.18)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%nononag(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       IF(i.EQ.19)then
#ifdef DM_PARALLEL
           IF (wrf_dm_on_monitor()) THEN
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
           ENDIF
         DM_BCAST_MACRO(dumc1)
#else
             READ(26,'(12E9.2)') dumc1(ids:ide-1,jds:jde-1)
#endif
              grid%slai(ips:ipe  ,jps:jpe  ) = dumc1(ips:ipe  ,jps:jpe  )
       ENDIF
       CLOSE(26)

      ENDDO

   write(message,FMT='(A)') ' PAST READ BIOGENIC EMISSIONS '
   CALL  wrf_debug ( 100, message )

   write(message,FMT='(A)') ' OPEN BIOGENIC EMISSIONS WRF file'
   CALL  wrf_debug ( 100, message )

   CALL construct_filename1( inpname , 'wrfbiochemi' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input6 , "DATASET=AUXINPUT6", ierr )
   write(message,FMT='(A,A)') ' BIOGENIC EMISSIONS file name: ',TRIM(inpname)
   CALL  wrf_message ( message )

   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfchem emissions file for writing' )
   ENDIF

   write(message,FMT='(A)') ' WRITE BIOGENIC EMISSIONS WRF file '
   CALL  wrf_debug ( 100, message )

   CALL output_aux_model_input6 ( id1 , grid , config_flags , ierr )

   CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT6" )

    endif  !    BIO_EMIS_OPT = BEIS313
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DEALLOCATE ( dumc0 )
    DEALLOCATE ( dumc1 )

   CALL med_shutdown_io ( grid , config_flags )

   CALL wrf_debug ( 0 , ' EMISSIONS CONVERSION : end of program ')

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM  convert_emissions
