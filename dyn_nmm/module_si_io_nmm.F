MODULE module_si_io_nmm

   !  Input 3D meteorological fields.

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: u_input , v_input , &
                                            q_input , t_input

   !  Input 3D LSM fields.

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , &
                                            soil_top_cat_input , &
                                            soil_bot_cat_input

   REAL, ALLOCATABLE:: htm_in(:,:,:),vtm_in(:,:,:)

   !  Input 2D surface fields.

   REAL , DIMENSION(:,:)   , ALLOCATABLE :: tg_input , res_input , &
                                            t_annual_avg_input , pd_input , &
                                            st000010_input , st010040_input , &
                                            st040100_input , st100200_input , &
                                            sm000010_input , sm010040_input , &
                                            sm040100_input , sm100200_input , &
                                            st010200_input , &
                                            sm010200_input , &
                                            soilt010_input , soilt040_input , &
                                            soilt100_input , soilt200_input , &
                                            soilm010_input , soilm040_input , &
                                            soilm100_input , soilm200_input , &
                                            seaice_input , snow_input , &
                                            canwat_input , &
                                            landuse_input , landmask_input , &
                                            skintemp_input , sst_input , &
                                            green_frac_input , green_frac_min, &
                                            green_frac_max, ter_input ,  &
                                            toposoil_input, albase_input, &
                                            mxsnal_input, psfc_in,pmsl,cmc_in,sm_in, &
                                            islope_input

   INTEGER , DIMENSION(:,:), ALLOCATABLE :: isltyp_input , ivgtyp_input

   !  Input 2D map information.

   REAL , DIMENSION(:,:)   , ALLOCATABLE :: lat_mass, lon_mass, &
                                            lat_wind, lon_wind 

   REAL , DIMENSION(:)     , ALLOCATABLE :: DETA_in, AETA_in, ETAX_in
   REAL , DIMENSION(:)     , ALLOCATABLE :: DETA1_in, AETA1_in, ETA1_in
   REAL , DIMENSION(:)     , ALLOCATABLE :: DETA2_in, AETA2_in, ETA2_in, dfl_input

   LOGICAL , SAVE :: first_time_in = .TRUE.

   INTEGER :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , &
              flag_sm000010 , flag_sm010040 , flag_sm040100 , flag_sm100200 , &
              flag_st010200 , &
              flag_sm010200
   INTEGER :: flag_soilt010 , flag_soilt040 , flag_soilt100 , flag_soilt200 , &
              flag_soilm010 , flag_soilm040 , flag_soilm100 , flag_soilm200

   INTEGER :: flag_sst , flag_toposoil

   INTEGER                  :: num_st_levels_input , num_sm_levels_input
   INTEGER , DIMENSION(100) ::     st_levels_input ,     sm_levels_input
   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: st_input , sm_input

   REAL                     :: pdtop_in, TLM0D, TPH0D

!   Some constants to allow simple dimensions in the defined types
!   given below.

   INTEGER, PARAMETER          :: max_domains = 10
   INTEGER, PARAMETER          :: var_maxdims = 5
   INTEGER, PARAMETER          :: max_staggers_xy_new = 4
   INTEGER, PARAMETER          :: max_staggers_xy_old = 3
   INTEGER, PARAMETER          :: max_staggers_z = 2
   INTEGER, PARAMETER          :: max_standard_lats = 4
   INTEGER, PARAMETER          :: max_standard_lons = 4  
   INTEGER, PARAMETER          :: max_fg_variables = 200
   INTEGER, PARAMETER          :: max_vertical_levels = 2000

!   This module defines the items needed for the WRF metadata
!   which is broken up into three levels:  
!      Global metadata:  Those things which apply to the
!                        entire simulation that are 
!                        independent of time, domain, or
!                        variable
!
!      Domain metadata:  Those things which apply to 
!                        a single domain (this may
!                        or may not be time dependent)
!
!      Variable metadata: Those things which apply to 
!                        a specific variable at a 
!                        specific time
!
!      The variable names and definitions can be 
!      found in the wrf_metadata spec, which is still
!      a living document as coding goes on.   The names
!      may not match exactly, but you should be able 
!      to figure things out.  
!

   TYPE wrf_var_metadata
      CHARACTER (LEN=8)         :: name 
      CHARACTER (LEN=16)        :: units
      CHARACTER (LEN=80)        :: description
      INTEGER                   :: domain_id
      INTEGER                   :: ndim
      INTEGER                   :: dim_val (var_maxdims)
      CHARACTER(LEN=4)          :: dim_desc (var_maxdims)
      INTEGER                   :: start_index(var_maxdims)
      INTEGER                   :: stop_index(var_maxdims)
      INTEGER                   :: h_stagger_index
      INTEGER                   :: v_stagger_index
      CHARACTER(LEN=8)          :: array_order
      CHARACTER(LEN=4)          :: field_type
      CHARACTER(LEN=8)          :: field_source_prog
      CHARACTER(LEN=80)         :: source_desc
      CHARACTER(LEN=8)          :: field_time_type
      INTEGER                   :: vt_date_start
      REAL                      :: vt_time_start
      INTEGER                   :: vt_date_stop
      REAL                      :: vt_time_stop
   END TYPE wrf_var_metadata

   TYPE(wrf_var_metadata)  :: var_meta , var_info

   TYPE wrf_domain_metadata
      INTEGER                   :: id
      INTEGER                   :: parent_id
      CHARACTER(LEN=8)          :: dyn_init_src
      CHARACTER(LEN=8)          :: static_init_src 
      INTEGER                   :: vt_date
      REAL                      :: vt_time
      INTEGER                   :: origin_parent_x
      INTEGER                   :: origin_parent_y
      INTEGER                   :: ratio_to_parent
      REAL                      :: delta_x
      REAL                      :: delta_y
      REAL                      :: top_level
      INTEGER                   :: origin_parent_z
      REAL                      :: corner_lats_new(4,max_staggers_xy_new)
      REAL                      :: corner_lons_new(4,max_staggers_xy_new)
      REAL                      :: corner_lats_old(4,max_staggers_xy_old)
      REAL                      :: corner_lons_old(4,max_staggers_xy_old)
      INTEGER                   :: xdim
      INTEGER                   :: ydim
      INTEGER                   :: zdim
   END TYPE wrf_domain_metadata
   TYPE(wrf_domain_metadata) :: dom_meta

   TYPE wrf_global_metadata
      CHARACTER(LEN=80)         :: simulation_name
      CHARACTER(LEN=80)         :: user_desc
      INTEGER                   :: si_version
      INTEGER                   :: analysis_version  
      INTEGER                   :: wrf_version
      INTEGER                   :: post_version
      CHARACTER(LEN=32)         :: map_projection
      REAL                      :: moad_known_lat
      REAL                      :: moad_known_lon
      CHARACTER(LEN=8)          :: moad_known_loc
      REAL                      :: moad_stand_lats(max_standard_lats)
      REAL                      :: moad_stand_lons(max_standard_lons)
      REAL                      :: moad_delta_x
      REAL                      :: moad_delta_y
      CHARACTER(LEN=4)          :: horiz_stagger_type
      INTEGER                   :: num_stagger_xy
      REAL                      :: stagger_dir_x_new(max_staggers_xy_new)
      REAL                      :: stagger_dir_y_new(max_staggers_xy_new)
      REAL                      :: stagger_dir_x_old(max_staggers_xy_old)
      REAL                      :: stagger_dir_y_old(max_staggers_xy_old)
      INTEGER                   :: num_stagger_z    
      REAL                      :: stagger_dir_z(max_staggers_z)
      CHARACTER(LEN=8)          :: vertical_coord
      INTEGER                   :: num_domains
      INTEGER                   :: init_date
      REAL                      :: init_time
      INTEGER                   :: end_date
      REAL                      :: end_time
      CHARACTER(LEN=4)          :: lu_source
      INTEGER                   :: lu_water
      INTEGER                   :: lu_ice  
   END TYPE wrf_global_metadata
   TYPE(wrf_global_metadata)   :: global_meta

CONTAINS

   SUBROUTINE read_si ( grid , dx , pt , file_date_string &
                       ,numx_sm_levels_input              &
                       ,numx_st_levels_input              &
                       ,smx_levels_input                  &
                       ,stx_levels_input                  &
                       ,ids,ide,jds,jde,kds,kde           &
                       ,ims,ime,jms,jme,kms,kme           &
                       ,its,ite,jts,jte,kts,kte)

      USE module_soil_pre
      USE module_domain

      IMPLICIT NONE

      TYPE(domain) , INTENT(INOUT)  :: grid
      INTEGER , INTENT(IN) :: ids,ide,jds,jde,kds,kde           &
                             ,ims,ime,jms,jme,kms,kme           &
                             ,its,ite,jts,jte,kts,kte
      REAL , INTENT(IN) :: dx
      REAL , INTENT(OUT) :: pt
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string

      REAL,DIMENSION(IDS:IDE-1,JDS:JDE-1) :: dum2d
      REAL,DIMENSION(IDS:IDE-1,JDS:JDE-1,KDS:KDE-1) :: dum3d
     
      INTEGER :: i , j , k , loop

      REAL :: dummy

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open
      integer,intent(out) :: numx_sm_levels_input,numx_st_levels_input
      real,dimension(100),intent(out) :: smx_levels_input,stx_levels_input

      write(0,*)' enter read_si'
      !  Initialize what soil temperature and moisture is available.

      flag_st000010 = 0
      flag_st010040 = 0
      flag_st040100 = 0
      flag_st100200 = 0
      flag_sm000010 = 0 
      flag_sm010040 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0
      flag_st010200 = 0
      flag_sm010200 = 0

      flag_soilt010 = 0
      flag_soilt040 = 0
      flag_soilt100 = 0
      flag_soilt200 = 0 
      flag_soilm010 = 0 
      flag_soilm040 = 0
      flag_soilm100 = 0
      flag_soilm200 = 0

      flag_sst      = 0
      flag_toposoil = 0

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

      !  Get the space for the data if this is the first time here.

	write(6,*) 'enter read_si...first_time_in:: ', first_time_in

      IF ( first_time_in ) THEN

         CLOSE(12)
         OPEN ( FILE   = 'real_input_nm.global.metadata' , &
                UNIT   = 12                              , &
                STATUS = 'OLD'                           , &
                ACCESS = 'SEQUENTIAL'                    , &
                FORM   = 'UNFORMATTED'                   , &
                IOSTAT = ok_open                           )

         IF ( ok_open .NE. 0 ) THEN
            PRINT '(A)','You asked for WRF SI data, but no real_input_nm.global.metadata file exists.'
            STOP 'No_real_input_nm.global.metadata_exists'
         END IF

         READ(12) global_meta%simulation_name, global_meta%user_desc, &
                  global_meta%si_version, global_meta%analysis_version, &
                  global_meta%wrf_version, global_meta%post_version
   
         REWIND (12)

         IF      ( global_meta%si_version .EQ. 1 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_old, global_meta%stagger_dir_y_old, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_new, global_meta%stagger_dir_y_new, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time , &
                     global_meta%lu_source, global_meta%lu_water, global_meta%lu_ice
         END IF
         CLOSE (12)
   
         print *,'GLOBAL METADATA'
         print *,'global_meta%simulation_name', global_meta%simulation_name
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%si_version', global_meta%si_version
         print *,'global_meta%analysis_version', global_meta%analysis_version
         print *,'global_meta%wrf_version', global_meta%wrf_version
         print *,'global_meta%post_version', global_meta%post_version
         print *,'global_meta%map_projection', global_meta%map_projection
         print *,'global_meta%moad_known_lat', global_meta%moad_known_lat
         print *,'global_meta%moad_known_lon', global_meta%moad_known_lon
         print *,'global_meta%moad_known_loc', global_meta%moad_known_loc
         print *,'global_meta%moad_stand_lats', global_meta%moad_stand_lats
         print *,'global_meta%moad_stand_lons', global_meta%moad_stand_lons
         print *,'global_meta%moad_delta_x', global_meta%moad_delta_x
         print *,'global_meta%moad_delta_y', global_meta%moad_delta_y
         print *,'global_meta%horiz_stagger_type', global_meta%horiz_stagger_type
         print *,'global_meta%num_stagger_xy', global_meta%num_stagger_xy
         IF      ( global_meta%si_version .EQ. 1 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_old
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_old
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_new
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_new
         END IF
         print *,'global_meta%num_stagger_z', global_meta%num_stagger_z
         print *,'global_meta%stagger_dir_z', global_meta%stagger_dir_z
         print *,'global_meta%vertical_coord', global_meta%vertical_coord
         print *,'global_meta%num_domains', global_meta%num_domains
         print *,'global_meta%init_date', global_meta%init_date
         print *,'global_meta%init_time', global_meta%init_time
         print *,'global_meta%end_date', global_meta%end_date
         print *,'global_meta%end_time', global_meta%end_time
         IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%lu_source', global_meta%lu_source
            print *,'global_meta%lu_water', global_meta%lu_water
            print *,'global_meta%lu_ice', global_meta%lu_ice
         END IF
         print *,' '


!!!!!
	TLM0D=global_meta%moad_known_lon
	TPH0D=global_meta%moad_known_lat

	write(6,*) 'defined tlm0d, tph0d:: ', tlm0d, tph0d
!!!!!



!
!	STOP
!
         
         !  1D - this is the definition of the vertical coordinate.

	IF ( .NOT. ALLOCATED (DETA_in)) ALLOCATE(DETA_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (AETA_in)) ALLOCATE(AETA_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (ETAX_in)) ALLOCATE(ETAX_in(kds:kde))

	IF ( .NOT. ALLOCATED (DETA1_in)) ALLOCATE(DETA1_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (AETA1_in)) ALLOCATE(AETA1_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (ETA1_in))  ALLOCATE(ETA1_in(kds:kde))

	IF ( .NOT. ALLOCATED (DETA2_in)) ALLOCATE(DETA2_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (AETA2_in)) ALLOCATE(AETA2_in(kds:kde-1))
	IF ( .NOT. ALLOCATED (ETA2_in)) ALLOCATE(ETA2_in(kds:kde))

	IF ( .NOT. ALLOCATED (dfl_input)) ALLOCATE(dfl_input(kds:kde))

	
	write(6,*) 'ix,jx,kx: ', ide-1,jde-1,kde-1

!	call wrf_error_fatal ("just want to quit here")	

         !  3D met

         IF ( .NOT. ALLOCATED ( u_input)  ) ALLOCATE ( u_input(its:ite,jts:jte,kts:kte) )
         IF ( .NOT. ALLOCATED ( v_input)  ) ALLOCATE ( v_input(its:ite,jts:jte,kts:kte) )
         IF ( .NOT. ALLOCATED ( q_input)  ) ALLOCATE ( q_input(its:ite,jts:jte,kts:kte) )
         IF ( .NOT. ALLOCATED ( t_input)  ) ALLOCATE ( t_input(its:ite,jts:jte,kts:kte) )
	write(6,*) 'allocating all 3d inputs with ite,jte,kte:: ', ite,jte,kte
         IF ( .NOT. ALLOCATED ( htm_in)  ) ALLOCATE ( htm_in(its:ite,jts:jte,kts:kte) )
         IF ( .NOT. ALLOCATED ( vtm_in)  ) ALLOCATE ( vtm_in(its:ite,jts:jte,kts:kte) )

         !  2D pressure fields

         IF ( .NOT. ALLOCATED ( pd_input)           ) ALLOCATE ( pd_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( pmsl)           )     ALLOCATE ( pmsl(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( psfc_in)           )     ALLOCATE ( psfc_in(its:ite,jts:jte) )

         !  2D - skin temperature and some sort of soil temp, usually 0-10 cm

!         IF ( .NOT. ALLOCATED ( tg_input)           ) ALLOCATE ( tg_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( res_input)          ) ALLOCATE ( res_input(its:ite,jts:jte) )

         !  2D - for LSM, these are computed from the categorical precentage values.

!         IF ( .NOT. ALLOCATED ( isltyp_input)       ) ALLOCATE ( isltyp_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( ivgtyp_input)       ) ALLOCATE ( ivgtyp_input(its:ite,jts:jte) )
          IF ( .NOT. ALLOCATED ( islope_input)       ) ALLOCATE ( islope_input(its:ite,jts:jte) )

         !  2D - for LSM, this is a new field from the SI

!         IF ( .NOT. ALLOCATED ( t_annual_avg_input) ) ALLOCATE ( t_annual_avg_input(its:ite,jts:jte) )

         !  2D - for LSM, the various soil temperature and moisture levels that are available.

         IF ( .NOT. ALLOCATED ( st000010_input)    ) ALLOCATE ( st000010_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( st010040_input)    ) ALLOCATE ( st010040_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( st040100_input)    ) ALLOCATE ( st040100_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( st100200_input)    ) ALLOCATE ( st100200_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( st010200_input)    ) ALLOCATE ( st010200_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( st_input      )    ) ALLOCATE ( st_input      (its:ite,jts:jte,10) )
         IF ( .NOT. ALLOCATED ( sm000010_input)    ) ALLOCATE ( sm000010_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( sm010040_input)    ) ALLOCATE ( sm010040_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( sm040100_input)    ) ALLOCATE ( sm040100_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( sm100200_input)    ) ALLOCATE ( sm100200_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( sm010200_input)    ) ALLOCATE ( sm010200_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( sm_input      )    ) ALLOCATE ( sm_input      (its:ite,jts:jte,10) )

         IF ( .NOT. ALLOCATED ( soilt010_input)    ) ALLOCATE ( soilt010_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilt040_input)    ) ALLOCATE ( soilt040_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilt100_input)    ) ALLOCATE ( soilt100_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilt200_input)    ) ALLOCATE ( soilt200_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilm010_input)   ) ALLOCATE ( soilm010_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilm040_input)    ) ALLOCATE ( soilm040_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilm100_input)    ) ALLOCATE ( soilm100_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( soilm200_input)    ) ALLOCATE ( soilm200_input(its:ite,jts:jte) )

         ! 2D - for LSM, seaice and snow, maybe runoff is later.

         IF ( .NOT. ALLOCATED ( seaice_input)      ) ALLOCATE ( seaice_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( snow_input)        ) ALLOCATE ( snow_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( canwat_input)      ) ALLOCATE ( canwat_input(its:ite,jts:jte) )

         IF ( .NOT. ALLOCATED ( landuse_input)     ) ALLOCATE ( landuse_input(its:ite,jts:jte) )
         IF ( .NOT. ALLOCATED ( landmask_input)    ) ALLOCATE ( landmask_input(its:ite,jts:jte) )

         IF ( .NOT. ALLOCATED ( skintemp_input)    ) ALLOCATE ( skintemp_input(its:ite,jts:jte)  )
         IF ( .NOT. ALLOCATED ( sst_input)         ) ALLOCATE ( sst_input(its:ite,jts:jte)  )
         IF ( .NOT. ALLOCATED ( green_frac_min)  ) ALLOCATE ( green_frac_min(its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( green_frac_input)  ) ALLOCATE ( green_frac_input(its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( green_frac_max)  ) ALLOCATE ( green_frac_max(its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( albase_input)  ) ALLOCATE ( albase_input(its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( mxsnal_input)  ) ALLOCATE ( mxsnal_input(its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( cmc_in )  ) ALLOCATE ( cmc_in (its:ite,jts:jte))
         IF ( .NOT. ALLOCATED ( sm_in )  ) ALLOCATE ( sm_in (its:ite,jts:jte))


!!!! FIS
        IF ( .NOT. ALLOCATED ( ter_input)         ) ALLOCATE ( ter_input(its:ite,jts:jte) )

!       IF ( .NOT. ALLOCATED ( ter_std_input)     ) ALLOCATE ( ter_std_input(its:ite,jts:jte) )
!       IF ( .NOT. ALLOCATED ( ter_slpx_input)    ) ALLOCATE ( ter_slpx_input(its:ite,jts:jte) )
!       IF ( .NOT. ALLOCATED ( ter_slpy_input)    ) ALLOCATE ( ter_slpy_input(its:ite,jts:jte) )
!       IF ( .NOT. ALLOCATED ( toposoil_input)    ) ALLOCATE ( toposoil_input(its:ite,jts:jte) )

         !  2D terrestrial fields other than elevation stuff.

!         IF ( .NOT. ALLOCATED ( msft_input)         ) ALLOCATE ( msft_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( msfu_input)         ) ALLOCATE ( msfu_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( msfv_input)         ) ALLOCATE ( msfv_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( sina_input)         ) ALLOCATE ( sina_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( cosa_input)         ) ALLOCATE ( cosa_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( e_input)            ) ALLOCATE ( e_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( f_input)            ) ALLOCATE ( f_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( lat_input)          ) ALLOCATE ( lat_input(its:ite,jts:jte) )
!         IF ( .NOT. ALLOCATED ( lon_input)          ) ALLOCATE ( lon_input(its:ite,jts:jte) )
	
	IF (.NOT. ALLOCATED (lat_mass)) ALLOCATE (lat_mass(its:ite,jts:jte))
	IF (.NOT. ALLOCATED (lat_wind)) ALLOCATE (lat_wind(its:ite,jts:jte))
	IF (.NOT. ALLOCATED (lon_mass)) ALLOCATE (lon_mass(its:ite,jts:jte))
	IF (.NOT. ALLOCATED (lon_wind)) ALLOCATE (lon_wind(its:ite,jts:jte))


      END IF

      CLOSE(13)

	write(6,*) 'file_date_string: ', file_date_string
	write(6,*) 'opening real_input_nm.d01... as unit 13'
      OPEN ( FILE   = 'real_input_nm.d01.'//file_date_string , &
             UNIT   = 13                                     , &
             STATUS = 'OLD'                                  , &
             ACCESS = 'SEQUENTIAL'                           , &
             FORM   = 'UNFORMATTED'                            )

      IF      ( global_meta%si_version .EQ. 1 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_old, dom_meta%corner_lons_old, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_new, dom_meta%corner_lons_new, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      END IF

      print *,'DOMAIN METADATA'
      print *,'dom_meta%id=', dom_meta%id
      print *,'dom_meta%parent_id=', dom_meta%parent_id
      print *,'dom_meta%dyn_init_src=', dom_meta%dyn_init_src
      print *,'dom_meta%static_init_src=', dom_meta%static_init_src
      print *,'dom_meta%vt_date=', dom_meta%vt_date
      print *,'dom_meta%vt_time=', dom_meta%vt_time
      print *,'dom_meta%origin_parent_x=', dom_meta%origin_parent_x
      print *,'dom_meta%origin_parent_y=', dom_meta%origin_parent_y
      print *,'dom_meta%ratio_to_parent=', dom_meta%ratio_to_parent
      print *,'dom_meta%delta_x=', dom_meta%delta_x
      print *,'dom_meta%delta_y=', dom_meta%delta_y
      print *,'dom_meta%top_level=', dom_meta%top_level
      print *,'dom_meta%origin_parent_z=', dom_meta%origin_parent_z
      IF      ( global_meta%si_version .EQ. 1 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_old
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_old
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_new
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_new
      END IF
      print *,'dom_meta%xdim=', dom_meta%xdim
      print *,'dom_meta%ydim=', dom_meta%ydim
      print *,'dom_meta%zdim=', dom_meta%zdim
      print *,' '

      !  A simple domain size test.
    

!!	relax constraint, as model namelist has +1 for i and j, while
!!	si data has true dimensions

      IF (  abs(dom_meta%xdim - (ide-1)) .gt. 1 &
       .OR. abs(dom_meta%ydim - (jde-1)) .gt. 1 &
       .OR. abs(dom_meta%zdim - (kde-1)) .gt. 1) THEN
         PRINT '(A)','Namelist does not match the input data.'
         PRINT '(A,3I5,A)','Namelist dimensions =',ide-1,jde-1,kde-1,'.'
         PRINT '(A,3I5,A)','Input data dimensions =',dom_meta%xdim,dom_meta%ydim,dom_meta%zdim,'.'
         STOP 'Wrong_data_size'
      END IF

      ! How about the grid distance?  Is it the same as in the namelist?

!      IF ( ABS ( dom_meta%delta_x - dx ) .GT. 1 ) THEN
!         PRINT '(A)','Grid distance differs between input file and namelist.'
!         PRINT '(A,F8.1,A)','Grid distance in input file = ',dom_meta%delta_x,' m.'
!         PRINT '(A,F8.1,A)','Grid distance in namelist = ',dx,' m.'
!         STOP 'Wrong_grid_distance'
!      END IF

      IF        ( global_meta%si_version .EQ. 1 ) THEN
         CALL nl_set_cen_lat ( grid%id , ( dom_meta%corner_lats_old(1,1) + dom_meta%corner_lats_old(2,1) +        &
                                        dom_meta%corner_lats_old(3,1) + dom_meta%corner_lats_old(4,1) ) * 0.25 ) 
      ELSE IF ( ( global_meta%si_version .EQ. 2 ) .AND. ( global_meta%moad_known_loc(1:6) .EQ. 'CENTER' ) ) THEN
         CALL nl_set_cen_lat ( grid%id , global_meta%moad_known_lat )
      ELSE IF   ( global_meta%si_version .EQ. 2 ) THEN
         CALL nl_set_cen_lat ( grid%id , ( dom_meta%corner_lats_new(1,1) + dom_meta%corner_lats_new(2,1) +        &
                                        dom_meta%corner_lats_new(3,1) + dom_meta%corner_lats_new(4,1) ) * 0.25 ) 
      END IF


!!!	might be trouble here

      CALL nl_set_cen_lon ( grid%id , global_meta%moad_stand_lons(1) )
!!!!!
	write(6,*) 'set_cen_lat... global_meta%moad_stand_lats(1): ', global_meta%moad_stand_lats(1)
      CALL nl_set_cen_lat ( grid%id , global_meta%moad_stand_lats(1) )
!!!!!
      CALL nl_set_truelat1 ( grid%id , global_meta%moad_stand_lats(1) )
      CALL nl_set_truelat2 ( grid%id , global_meta%moad_stand_lats(2) )

      pt = dom_meta%top_level

      IF      ( global_meta%map_projection(1:17) .EQ. 'LAMBERT CONFORMAL'   ) THEN
         map_proj = 1
      ELSE IF ( global_meta%map_projection(1:19) .EQ. 'POLAR STEREOGRAPHIC' ) THEN
         map_proj = 2
      ELSE IF ( global_meta%map_projection(1: 8) .EQ. 'MERCATOR'            ) THEN
         map_proj = 3
      ELSE IF ( global_meta%map_projection(1:14) .EQ. 'ROTATED LATLON' ) THEN
	map_proj = 203 !?
      ELSE
         PRINT '(A,A,A)','Undefined map projection: ',TRIM(global_meta%map_projection(1:20)),'.'
         STOP 'Undefined_map_proj_si'
      END IF
      CALL nl_set_map_proj ( grid%id , map_proj ) 
     
      IF      ( global_meta%si_version .EQ. 1 ) THEN
         CALL nl_set_mminlu ( 'USGS' )
         CALL nl_set_iswater (grid%id, 16 )
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         CALL nl_set_mminlu ( global_meta%lu_source )
         CALL nl_set_iswater (grid%id, global_meta%lu_water )
         CALL nl_set_isice (grid%id, global_meta%lu_ice )
      END IF

      CALL nl_set_gmt (grid%id, dom_meta%vt_time / 3600. )
      CALL nl_set_julyr (grid%id, dom_meta%vt_date / 1000 )
      CALL nl_set_julday (grid%id, dom_meta%vt_date - ( dom_meta%vt_date / 1000 ) * 1000 )

	write(6,*) 'start reading from unit 13'
      read_all_the_data : DO

         READ (13,IOSTAT=OK) var_info%name, var_info%units, &
                             var_info%description, var_info%domain_id, var_info%ndim, &
                             var_info%dim_val, var_info%dim_desc, var_info%start_index, &
                             var_info%stop_index, var_info%h_stagger_index, var_info%v_stagger_index,&
                             var_info%array_order, var_info%field_type, var_info%field_source_prog, &
                             var_info%source_desc, var_info%field_time_type, var_info%vt_date_start, &
                             var_info%vt_time_start, var_info%vt_date_stop, var_info%vt_time_stop

         IF ( OK .NE. 0 ) THEN
            PRINT '(A,A,A)','End of file found for real_input_nm.d01.',file_date_string,'.'
            EXIT read_all_the_data
         END IF

!        print *,'VARIABLE METADATA'
         PRINT '(A,A)','var_info%name=', var_info%name 
!        print *,'var_info%units=', var_info%units 
!        print *,'var_info%description=', var_info%description 
!        print *,'var_info%domain_id=', var_info%domain_id 
!        print *,'var_info%ndim=', var_info%ndim 
!        print *,'var_info%dim_val=', var_info%dim_val 
!        print *,'var_info%dim_desc=', var_info%dim_desc 
!        print *,'var_info%start_index=', var_info%start_index 
!        print *,'var_info%stop_index=', var_info%stop_index 
!        print *,'var_info%h_stagger_index=', var_info%h_stagger_index 
!        print *,'var_info%v_stagger_index=', var_info%v_stagger_index
!        print *,'var_info%array_order=', var_info%array_order 
!        print *,'var_info%field_type=', var_info%field_type 
!        print *,'var_info%field_source_prog=', var_info%field_source_prog 
!        print *,'var_info%source_desc=', var_info%source_desc 
!        print *,'var_info%field_time_type=', var_info%field_time_type 
!        print *,'var_info%vt_date_start=', var_info%vt_date_start 
!        print *,'var_info%vt_time_start=', var_info%vt_time_start 
!        print *,'var_info%vt_date_stop=', var_info%vt_date_stop 
!        print *,'var_info%vt_time_stop=', var_info%vt_time_stop

         !  3D meteorological fields.

      write(0,*)' read_si var_info%name=',var_info%name(1:8)
         IF      ( var_info%name(1:8) .EQ. 'T       ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              t_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
	do K=1,kde-1
	write(6,*) 't_input, K: ', K, t_input(1,1,K)
	enddo
	
	write(6,*) 't_input(5,5,5): ', t_input(5,5,5)
         ELSE IF      ( var_info%name(1:8) .EQ. 'U       ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              u_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
	do K=1,kde-1
	write(6,*) 'u_input, K: ', K, u_input(1,1,K)
	enddo
	write(6,*) 'u_input(5,5,5): ', u_input(5,5,5)

         ELSE IF ( var_info%name(1:8) .EQ. 'V       ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              v_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
	do K=1,kde-1
	write(6,*) 'v_input, K: ', K, v_input(1,1,K)
	enddo
	write(6,*) 'v_input(5,5,5): ', v_input(5,5,5)

         ELSE IF ( var_info%name(1:8) .EQ. 'Q      ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              q_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
	do K=1,kde-1
	write(6,*) 'q_input, K: ', K, q_input(1,1,K)
	enddo
	write(6,*) 'q_input(5,5,5): ', q_input(5,5,5)
!         ELSE IF ( var_info%name(1:8) .EQ. 'THETA   ' ) THEN
!            READ (13) theta_input

         !  3D LSM fields.  Don't know the 3rd dimension until we read it in.

         ELSE IF ( var_info%name(1:8) .EQ. 'LANDUSEF' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( landuse_frac_input) ) ) THEN
               ALLOCATE (landuse_frac_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,jte
            do i=its,ite
              landuse_frac_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCTOP' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_top_cat_input) ) ) THEN
               ALLOCATE (soil_top_cat_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,jte
            do i=its,ite
              soil_top_cat_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCBOT' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_bot_cat_input) ) ) THEN
               ALLOCATE (soil_bot_cat_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,jte
            do i=its,ite
              soil_bot_cat_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         !  2D dry pressure minus ptop.

         ELSE IF ( var_info%name(1:8) .EQ. 'PD      ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              pd_input(i,j)=dum2d(i,j)
            enddo
            enddo
	write(6,*) 'pd_input(1,1):: ', pd_input(1,1)
         ELSE IF ( var_info%name(1:8) .EQ. 'PSFC    ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              psfc_in(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'PMSL    ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              pmsl(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'PDTOP   ' ) THEN
            READ (13) pdtop_in
         ELSE IF ( var_info%name(1:8) .EQ. 'PT      ' ) THEN
            READ (13) pt

         !  2D surface fields.

	ELSE IF ( var_info%name(1:8) .eq. 'GLAT    ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              lat_mass(i,j)=dum2d(i,j)
            enddo
            enddo
	ELSE IF ( var_info%name(1:8) .eq. 'GLON    ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              lon_mass(i,j)=dum2d(i,j)
            enddo
            enddo
	ELSE IF ( var_info%name(1:8) .eq. 'LAT_V   ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              lat_wind(i,j)=dum2d(i,j)
            enddo
            enddo
	ELSE IF ( var_info%name(1:8) .eq. 'LON_V   ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              lon_wind(i,j)=dum2d(i,j)
            enddo
            enddo

!         ELSE IF ( var_info%name(1:8) .EQ. 'T_AVGANN' ) THEN
!            READ (13) t_annual_avg_input

         ELSE IF ( var_info%name(1:8) .EQ. 'ST000010' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              st000010_input(i,j)=dum2d(i,j)
!             res_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st000010 = 1
            num_st_levels_input = num_st_levels_input + 1
	write(6,*) 'num_st_levels_input=',num_st_levels_input
	write(6,*) 'st000010_input(50,50): ', st000010_input(50,50)
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st000010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST010040' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              st010040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st010040 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
	write(6,*) 'num_st_levels_input=',num_st_levels_input
	write(6,*) 'st_levels_input: ', st_levels_input
            st_input(:,:,num_st_levels_input + 1) = st010040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST040100' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              st040100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st040100 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
	write(6,*) 'num_st_levels_input=',num_st_levels_input
	write(6,*) 'st_levels_input: ', st_levels_input
            st_input(:,:,num_st_levels_input + 1) = st040100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST100200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              st100200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st100200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
	write(6,*) 'num_st_levels_input=',num_st_levels_input
	write(6,*) 'st_levels_input: ', st_levels_input
            st_input(:,:,num_st_levels_input + 1) = st100200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST010200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              st010200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st010200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
	write(6,*) 'num_st_levels_input=',num_st_levels_input
	write(6,*) 'num_st_levels_input:: ', num_st_levels_input
            st_input(:,:,num_st_levels_input + 1) = st010200_input
        ELSE IF ( var_info%name(1:8) .EQ. 'SM000010' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm000010_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm000010 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm000010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM010040' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm010040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm010040 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm010040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM040100' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm040100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm040100 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm040100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM100200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm100200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm100200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm100200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM010200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm010200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm010200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm010200_input

         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT010' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilt010_input(i,j)=dum2d(i,j)
!             res_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt010 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt010_input
	write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT040' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilt040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt040 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt040_input
	write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT100' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilt100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt100 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt100_input
	write(6,*) 'num_st_levels_input=',num_st_levels_input
        ELSE IF ( var_info%name(1:8) .EQ. 'SOILT200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilt200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt200_input
	write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM010' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilm010_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm010 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM040' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilm040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm040 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM100' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilm100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm100 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM200' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              soilm200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm200_input

         ELSE IF ( var_info%name(1:8) .EQ. 'SEAICE  ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              seaice_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'WEASD   ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              snow_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'CANWAT  ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              canwat_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'LANDMASK' ) THEN
	write(6,*) 'size of landmask_input in read_si: ', size(landmask_input,dim=1),&
                                                          size(landmask_input,dim=2)
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              landmask_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SKINTEMP' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              skintemp_input(i,j)=dum2d(i,j)
!             tg_input(i,j)=dum2d(i,j)
              tg_input=skintemp_input
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SST     ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sst_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sst = 1
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENFRC' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              green_frac_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'ISLOPE  ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              islope_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENMAX' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              green_frac_max(i,j)=dum2d(i,j)
            enddo
            enddo
	write(6,*) 'green_frac_max(5,5): ', green_frac_max(5,5)
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENMIN' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              green_frac_min(i,j)=dum2d(i,j)
            enddo
            enddo
	write(6,*) 'green_frac_min(5,5): ', green_frac_min(5,5)
         ELSE IF ( var_info%name(1:8) .EQ. 'FIS     ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              ter_input(i,j)=dum2d(i,j)
            enddo
            enddo
	write(6,*) 'maxval of ter_input: ', maxval(ter_input)
         ELSE IF ( var_info%name(1:8) .EQ. 'CMC     ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              cmc_in(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'HTM     ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              htm_in(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'VTM     ' ) THEN
            READ (13) dum3d
            do k=kts,kte
            do j=jts,jte
            do i=its,ite
              vtm_in(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SM      ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              sm_in(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'ALBASE  ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              albase_input(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'MXSNAL  ' ) THEN
            READ (13) dum2d
            do j=jts,jte
            do i=its,ite
              mxsnal_input(i,j)=dum2d(i,j)
            enddo
            enddo
!         ELSE IF(( var_info%name(1:8) .EQ. 'USGSLAND' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
!            READ (13) landuse_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'TERRAIN ' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN 
!            READ (13) ter_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'TOPO_M  ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
!            READ (13) ter_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSTDV' ) THEN
!            READ (13) ter_std_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSLPX' ) THEN
!            READ (13) ter_slpx_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSLPY' ) THEN
!            READ (13) ter_slpy_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'SOILHGT ' ) THEN
!            READ (13) toposoil_input
!            flag_toposoil = 1

         !  2D map input fields.

!         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_M' ) THEN
!            READ (13) msft_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_U' ) THEN
!            READ (13) msfu_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_V' ) THEN
!            READ (13) msfv_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'SINALPHA' ) THEN
!            READ (13) sina_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'COSALPHA' ) THEN
!            READ (13) cosa_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'H_CORIOL' ) THEN
!            READ (13) f_input
!         ELSE IF ( var_info%name(1:8) .EQ. 'V_CORIOL' ) THEN
!            READ (13) e_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'LATITUDE' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
!            READ (13) lat_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'LONGITUD' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
!            READ (13) lon_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'LAT_M   ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
!            READ (13) lat_input
!         ELSE IF(( var_info%name(1:8) .EQ. 'LON_M   ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
!            READ (13) lon_input

         !  1D vertical coordinate.

          ELSE IF ( var_info%name(1:8) .EQ. 'DETA    ' ) THEN
	     READ(13) DETA_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DETA1   ' ) THEN
	     READ(13) DETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DETA2   ' ) THEN
	     READ(13) DETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETAX    ' ) THEN
	     READ(13) ETAX_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETA1    ' ) THEN
	     READ(13) ETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETA2    ' ) THEN
	     READ(13) ETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA    ' ) THEN
	     READ(13) AETA_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA1   ' ) THEN
	     READ(13) AETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA2   ' ) THEN
	     READ(13) AETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DFL     ' ) THEN
	     READ(13) dfl_input

!         ELSE IF ( var_info%name(1:8) .EQ. 'ETAPHALF' ) THEN
!            READ (13) etahalf
!         ELSE IF ( var_info%name(1:8) .EQ. 'ETAPFULL' ) THEN
!            READ (13) etafull

         !  wrong input data.

         ELSE IF ( var_info%name(1:8) .EQ. 'ZETAFULL' ) THEN
            PRINT '(A)','Oops, you put in the height data.'
            STOP 'this_is_mass_not_height'
 

         !  Stuff that we do not want or need is just skipped over.

         ELSE
print *,'------------------> skipping ', var_info%name(1:8)
            READ (13) dummy
         END IF

      END DO read_all_the_data

      CLOSE (13)

      first_time_in = .FALSE.

      write(6,*)' exit read_si num_st_levels_input=',num_st_levels_input
      write(0,*)' exit read_si num_st_levels_input=',num_st_levels_input
      numx_sm_levels_input=num_sm_levels_input
      numx_st_levels_input=num_st_levels_input
      smx_levels_input=sm_levels_input
      stx_levels_input=st_levels_input
      write(6,*) 'end of read_si...st_levels_input:: ', st_levels_input

   END SUBROUTINE read_si

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION char2int1( string3 ) RESULT ( int1 )
      CHARACTER (LEN=3) , INTENT(IN) :: string3
      INTEGER :: i1 , int1
      READ(string3,fmt='(I3)') i1
      int1 = i1
   END FUNCTION char2int1

   FUNCTION char2int2( string6 ) RESULT ( int1 )
      CHARACTER (LEN=6) , INTENT(IN) :: string6
      INTEGER :: i2 , i1 , int1
      READ(string6,fmt='(I3,I3)') i1,i2
      int1 = ( i2 + i1 ) / 2
   END FUNCTION char2int2

END MODULE module_si_io_nmm
