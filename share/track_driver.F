!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This routine prints out the current value of variables at all specified
!   track locations that are within the current patch.
!
! Jeff Lee -- 25 June 2009
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE track_driver( grid )

   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_scalar_tables
   USE module_model_constants
   USE module_date_time

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(INOUT) :: grid
#if ( EM_CORE == 1 )
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ! Local variables

   INTEGER :: level
   INTEGER :: level_stag
   INTEGER :: level_zref
   INTEGER :: num_tuv
   INTEGER :: num_rad
   INTEGER :: m, n, i
   INTEGER :: ix, iy
   TYPE(WRFU_Time) :: xcurrent_time, track_time_test 
   CHARACTER (LEN=132) :: message
   CHARACTER (LEN=19) :: xcurrent_timestr
   CHARACTER (LEN=19) :: chem_name
   
!============================================================================================
   
   IF ( grid%track_loc_domain <= 0 ) RETURN     ! no valid location

#if ( DA_CORE != 1 )
   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN
#endif

! get the next valid track time

   n = grid%track_next_time

   IF ( grid%track_next_time > grid%track_loc_domain ) RETURN   ! all track locations done

! get the track time

   CALL wrf_atotime( grid%track_time_domain(n), track_time_test )

! get the current time

   CALL domain_clock_get( grid, current_time=xcurrent_time, current_timestr=xcurrent_timestr )

   IF ( track_time_test .NE. xcurrent_time ) RETURN  ! track time does not match current time

! track time matches current time

   write(message,*) 'track_driver: current_time = ',xcurrent_timestr 
   call wrf_message( trim(message) )

   level      = grid%em32-grid%sm32
   level_stag = grid%em32-grid%sm32+1
#if (WRF_CHEM == 1)
   level_zref = model_config_rec%track_tuv_lev
   num_tuv    = model_config_rec%track_tuv_num
   num_rad    = model_config_rec%track_rad_num 
#endif

   ix = grid%track_i(n)
   iy = grid%track_j(n)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN

!-- output chemical species

#if ( WRF_CHEM == 1 )
         IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN
           do m= 1,grid%track_chem_num
            chem_name = TRIM(model_config_rec%track_chem_name(m))
              do i = 1, num_chem
                if (chem_name .eq. TRIM(chem_dname_table( grid%id, i ))) then
                  grid%track_chem(m,n,grid%sm32:grid%em32-1) = grid%chem(ix,grid%sm32:grid%em32-1,iy,i)

!                 print*,'track_chem_name,pointer = ',chem_name, i
!                 print*,'track_chem =',grid%track_chem(m,n,grid%sm32:grid%em32-1)
                  exit
                end if
              end do
           end do

           grid%track_o31d  (n,grid%sm32:grid%em32-1) = grid%ph_o31d  (ix,grid%sm32:grid%em32-1,iy)
           grid%track_o33p  (n,grid%sm32:grid%em32-1) = grid%ph_o33p  (ix,grid%sm32:grid%em32-1,iy)
           grid%track_no2   (n,grid%sm32:grid%em32-1) = grid%ph_no2   (ix,grid%sm32:grid%em32-1,iy)
           grid%track_hno2  (n,grid%sm32:grid%em32-1) = grid%ph_hno2  (ix,grid%sm32:grid%em32-1,iy)
           grid%track_hno3  (n,grid%sm32:grid%em32-1) = grid%ph_hno3  (ix,grid%sm32:grid%em32-1,iy)
           grid%track_h2o2  (n,grid%sm32:grid%em32-1) = grid%ph_h2o2  (ix,grid%sm32:grid%em32-1,iy)
           grid%track_ch3o2h(n,grid%sm32:grid%em32-1) = grid%ph_ch3o2h(ix,grid%sm32:grid%em32-1,iy)

           if (model_config_rec%phot_opt(grid%id) == 3) then
             do i = 1, num_rad
               grid%track_radfld(n,i,1:level_zref) = grid%radfld(ix,1:level_zref,iy,i)
             end do

             do i = 1, num_tuv
               grid%track_adjcoe(n,i,1:level_zref) = grid%adjcoe(ix,1:level_zref,iy,i)
               grid%track_phrate(n,i,1:level_zref) = grid%phrate(ix,1:level_zref,iy,i)
             end do
           endif
         END IF         
#endif

!-- output met

         grid%track_z(n,grid%sm32:grid%em32-1)      = grid%z(ix,grid%sm32:grid%em32-1,iy)
         grid%track_p(n,grid%sm32:grid%em32-1)      = grid%p(ix,grid%sm32:grid%em32-1,iy) + &
                                                    grid%pb(ix,grid%sm32:grid%em32-1,iy)
         grid%track_t(n,grid%sm32:grid%em32-1)      = (grid%t_2(ix,grid%sm32:grid%em32-1,iy) + t0 ) * &
                                                    (grid%track_p(n,grid%sm32:grid%em32-1)/p1000mb)**rcp
         grid%track_u(n,grid%sm32:grid%em32-1)      = (grid%u_2(ix,grid%sm32:grid%em32-1,iy) + &
                                                     grid%u_2(ix+1,grid%sm32:grid%em32-1,iy) )*0.5
         grid%track_v(n,grid%sm32:grid%em32-1)      = (grid%v_2(ix,grid%sm32:grid%em32-1,iy) + &
                                                     grid%v_2(ix,grid%sm32:grid%em32-1,iy+1) )*0.5
         grid%track_w(n,grid%sm32:grid%em32)      = grid%w_2(ix,grid%sm32:grid%em32,iy)
         grid%track_rh(n,grid%sm32:grid%em32-1)     = MIN( 1.00,grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QV) /      &
                                                    (3.80*exp(17.27*(grid%track_t(n,grid%sm32:grid%em32-1)-273.)/ &
                                                    (grid%track_t(n,grid%sm32:grid%em32-1)-36.))/                 &
                                                    (.01*grid%track_p(n,grid%sm32:grid%em32-1)))   )
         grid%track_alt(n,grid%sm32:grid%em32-1)    = grid%alt(ix,grid%sm32:grid%em32-1,iy)
         grid%track_qcloud(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QC)
         grid%track_qrain (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QR)
         grid%track_qice  (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QI)
         grid%track_qsnow (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QS)
         grid%track_qgraup(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QG)
         grid%track_qvapor(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QV)

!        print*,'track_z =',grid%track_z(n,grid%sm32:grid%em32-1)
       
      ELSE

!--      this section must have.

!-- output chem

#if ( WRF_CHEM == 1 )

         IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN
           do m= 1,grid%track_chem_num
            grid%track_chem(m,n,grid%sm32:grid%em32-1) = 1.E30
           end do

           grid%track_o31d  (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_o33p  (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_no2   (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_hno2  (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_hno3  (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_h2o2  (n,grid%sm32:grid%em32-1) = 1.E30
           grid%track_ch3o2h(n,grid%sm32:grid%em32-1) = 1.E30

           if (model_config_rec%phot_opt(grid%id) == 3) then
             grid%track_radfld(n,1:num_rad,1:level_zref) = 1.E30
             grid%track_adjcoe(n,1:num_tuv,1:level_zref) = 1.E30
             grid%track_phrate(n,1:num_tuv,1:level_zref) = 1.E30
           end if
         ENDIF     
#endif

!-- output met

         grid%track_z     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_p     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_t     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_u     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_v     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_w     (n,grid%sm32:grid%em32) = 1.E30
         grid%track_rh    (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_alt   (n,grid%sm32:grid%em32-1) = 1.E30

         grid%track_qcloud(n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qrain (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qice  (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qsnow (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qgraup(n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qvapor(n,grid%sm32:grid%em32-1) = 1.E30
 
      END IF
 
!-- write output to file

!     write (*,*) 'grid%track_next_time = ', grid%track_next_time

   if ( grid%track_next_time == grid%track_loc_domain ) then
!     write (*,*) 'grid%track_loc_domain = ', grid%track_loc_domain
!     write (*,*) 'track_driver: calling write_track'

      call write_track(grid)

      write (*,*) 'track_driver: DONE write_track'
   end if
 
   grid%track_next_time = grid%track_next_time + 1

#endif

END SUBROUTINE track_driver


SUBROUTINE write_track( grid )

   USE module_dm
   USE module_domain
   USE module_configure

   IMPLICIT NONE

   ! Arguments

   TYPE (domain), INTENT(INOUT) :: grid
#if ( EM_CORE == 1 )
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   ! Local variables

   INTEGER :: level
   INTEGER :: level_stag
   INTEGER :: level_zref
   INTEGER :: num_tuv
   INTEGER :: num_rad
   INTEGER :: m,n
   INTEGER :: i
   INTEGER :: ncid
   INTEGER :: astat
   CHARACTER (LEN=19) :: track_output
   CHARACTER (LEN=19) :: chem_name

   character (len=40) :: description
   character (len=40) :: units

   integer, parameter :: DateStrLen = 19
   integer            :: time_dim
   integer            :: level_dim
   integer            :: level_stag_dim
   integer            :: level_zref_dim
   integer            :: rad_dim
   integer            :: tuv_dim
   integer            :: Times_dim
   integer            :: var_dim(3)
   integer            :: var_id
   integer            :: start(3)
   integer            :: count(3) 

#ifdef DM_PARALLEL
   REAL, ALLOCATABLE, DIMENSION(:,:)   :: track_buf2
#if ( WRF_CHEM == 1 )
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: track_buf3
#endif
#endif

!====================================================================================
#if 1
!We actually always need to include 'netcdf.inc',
!as this routine won't compile without netcdf.
include 'netcdf.inc'
#else
#ifdef NETCDF
include 'netcdf.inc'
#endif
#endif


   IF ( grid%track_loc_domain .LE. 0 ) RETURN

#if ( DA_CORE != 1 )
   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN
#endif

   level      = grid%em32 - grid%sm32
   level_stag = grid%em32 - grid%sm32 + 1  
#if (WRF_CHEM == 1)
   level_zref = model_config_rec%track_tuv_lev
   num_tuv    = model_config_rec%track_tuv_num
   num_rad    = model_config_rec%track_rad_num
#endif

#ifdef DM_PARALLEL

   ALLOCATE(track_buf2(grid%track_loc_in, level))

!--put z output in grid%track_z(:,:)
!z 
   track_buf2(:,:) = grid%track_z(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_z(:,:),grid%track_loc_in*level)
!p
   track_buf2(:,:) = grid%track_p(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_p(:,:),grid%track_loc_in*level)
!t
   track_buf2(:,:) = grid%track_t(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_t(:,:),grid%track_loc_in*level)
!u
   track_buf2(:,:) = grid%track_u(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_u(:,:),grid%track_loc_in*level)
!v
   track_buf2(:,:) = grid%track_v(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_v(:,:),grid%track_loc_in*level)
!w
!  track_buf2(:,:) = grid%track_w(:,:)
!  CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_w(:,:),grid%track_loc_in*level)
!rh
   track_buf2(:,:) = grid%track_rh(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_rh(:,:),grid%track_loc_in*level)
!alt
   track_buf2(:,:) = grid%track_alt(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_alt(:,:),grid%track_loc_in*level)
!qcloud
   track_buf2(:,:) = grid%track_qcloud(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qcloud(:,:),grid%track_loc_in*level)
!qrain
   track_buf2(:,:) = grid%track_qrain(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qrain(:,:),grid%track_loc_in*level)
!qice
   track_buf2(:,:) = grid%track_qice(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qice(:,:),grid%track_loc_in*level)
!qsnow
   track_buf2(:,:) = grid%track_qsnow(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qsnow(:,:),grid%track_loc_in*level)
!qgraup
   track_buf2(:,:) = grid%track_qgraup(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qgraup(:,:),grid%track_loc_in*level)
!qvapor
   track_buf2(:,:) = grid%track_qvapor(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qvapor(:,:),grid%track_loc_in*level)

#if ( WRF_CHEM == 1 )
   IF (model_config_rec%chem_opt(grid%id) > 0) THEN
!o31d
   track_buf2(:,:) = grid%track_o31d(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_o31d(:,:),grid%track_loc_in*level)
!o33p
   track_buf2(:,:) = grid%track_o33p(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_o33p(:,:),grid%track_loc_in*level)
!no2
   track_buf2(:,:) = grid%track_no2(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_no2(:,:),grid%track_loc_in*level)
!hno2
   track_buf2(:,:) = grid%track_hno2(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_hno2(:,:),grid%track_loc_in*level)
!hno3
   track_buf2(:,:) = grid%track_hno3(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_hno3(:,:),grid%track_loc_in*level)
!h2o2
   track_buf2(:,:) = grid%track_h2o2(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_h2o2(:,:),grid%track_loc_in*level)
!ch3o2h
   track_buf2(:,:) = grid%track_ch3o2h(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_ch3o2h(:,:),grid%track_loc_in*level)

   END IF
#endif

   DEALLOCATE(track_buf2)

   ALLOCATE(track_buf2(grid%track_loc_in, level_stag))
!w
   track_buf2(:,:) = grid%track_w(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_w(:,:),grid%track_loc_in*level_stag)

   DEALLOCATE(track_buf2)

#if ( WRF_CHEM == 1 )

!--put chem output in grid%track_chem(:,:,:)
!chem
   IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN

   ALLOCATE(track_buf3(grid%track_chem_num, grid%track_loc_in, level))

   track_buf3(:,:,:) = grid%track_chem(:,:,:)
   CALL wrf_dm_min_reals(track_buf3(:,:,:),grid%track_chem(:,:,:),grid%track_chem_num*grid%track_loc_in*level)

   DEALLOCATE(track_buf3)

   if (model_config_rec%phot_opt(grid%id) == 3) then
!radfld
   ALLOCATE(track_buf3(grid%track_loc_in, num_rad, level_zref))

   track_buf3(:,:,:) = grid%track_radfld(:,:,:)
   CALL wrf_dm_min_reals(track_buf3(:,:,:),grid%track_radfld(:,:,:),grid%track_loc_in*num_rad*level_zref)

   DEALLOCATE(track_buf3)

   ALLOCATE(track_buf3(grid%track_loc_in, num_tuv, level_zref))
!adjcoe
   track_buf3(:,:,:) = grid%track_adjcoe(:,:,:)
   CALL wrf_dm_min_reals(track_buf3(:,:,:),grid%track_adjcoe(:,:,:),grid%track_loc_in*num_tuv*level_zref)
!phrate
   track_buf3(:,:,:) = grid%track_phrate(:,:,:)
   CALL wrf_dm_min_reals(track_buf3(:,:,:),grid%track_phrate(:,:,:),grid%track_loc_in*num_tuv*level_zref)

   DEALLOCATE(track_buf3)
   
   end if

   END IF
#endif

#endif

   IF ( wrf_dm_on_monitor() ) THEN

!--   get output unit

!     ncid = get_unused_unit()
!     if ( ncid <= 0 ) then
!        call wrf_error_fatal('write_track: ERROR: could not find a free Fortran unit.')
!     end if

!--   get output file name

      write (track_output,'(A)') trim('wrfout_track_d00')
      i = len_trim(track_output)
      write ( track_output(i-1:i), '(I2.2)') grid%id

!--   create necdf file

      astat = NF_CREATE(track_output, NF_CLOBBER, ncid)
      if (astat .ne. NF_NOERR) then
         call wrf_abort
      end if

!--   define dimensions

      astat = NF_DEF_DIM(ncid, 'time'       , NF_UNLIMITED , time_dim )
      astat = NF_DEF_DIM(ncid, 'level'      , level        , level_dim)
      astat = NF_DEF_DIM(ncid, 'DateStrLen' , DateStrLen   , Times_dim)
      astat = NF_DEF_DIM(ncid, 'level_stag' , level_stag   , level_stag_dim)

#if ( WRF_CHEM == 1 )
      IF ( model_config_rec%chem_opt(grid%id) > 0 .and. model_config_rec%phot_opt(grid%id) == 3 ) THEN
      astat = NF_DEF_DIM(ncid, 'level_zref' , level_zref   , level_zref_dim)
      astat = NF_DEF_DIM(ncid, 'num_rad'    , num_rad      , rad_dim)
      astat = NF_DEF_DIM(ncid, 'num_tuv'    , num_tuv      , tuv_dim)
      END IF
#endif

!--   define Times variable

      var_dim(1) = Times_dim
      var_dim(2) = time_dim

      astat = NF_DEF_VAR(ncid,'Times', NF_CHAR, 2, var_dim(1:2), var_id)

!--   define 1-D variables

#if ( WRF_CHEM == 1 )
      IF ( model_config_rec%chem_opt(grid%id) > 0 .and. model_config_rec%phot_opt(grid%id) == 3 ) THEN
!wc
      description = 'Wavelength'
      units       = ''
      astat = NF_DEF_VAR(ncid, 'wc'   , NF_REAL, 1, rad_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!zref
      description = 'Reference height'
      units       = 'km'
      astat = NF_DEF_VAR(ncid, 'zref'   , NF_REAL, 1, level_zref_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      END IF
#endif

!lat
      description = 'LATITUDE, SOUTH IS NEGATIVE'
      units       = 'degree_north'
      astat = NF_DEF_VAR(ncid, 'lat'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!lon
      description = 'LONGITUDE, WEST IS NEGATIVE'
      units       = 'degree_east'
      astat = NF_DEF_VAR(ncid, 'lon'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!grid_i
      description = 'grid_i, longitude direction '
      units       = ''
      astat = NF_DEF_VAR(ncid, 'grid_i'   , NF_INT, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!grid_j
      description = 'grid_j, latitude direction '
      units       = ''
      astat = NF_DEF_VAR(ncid, 'grid_j'   , NF_INT, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!ele
      description = 'elevation'
      units       = 'm'
      astat = NF_DEF_VAR(ncid, 'ele'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

!--   define 2-D variables

      var_dim(1) = level_dim
      var_dim(2) = time_dim
!z
      description = 'height'
      units       = 'm'
      astat = NF_DEF_VAR(ncid, 'z', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!p
      description = 'pressure'
      units       = 'Pa'
      astat = NF_DEF_VAR(ncid, 'p', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!t
      description = 'temperature'
      units       = 'K'
      astat = NF_DEF_VAR(ncid, 't', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!u
      description = 'x-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'u', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!v
      description = 'y-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'v', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!w
!     description = 'z-wind component'
!     units       = 'm s-1'
!     astat = NF_DEF_VAR(ncid, 'w', NF_REAL, 2 , var_dim(1:2), var_id )
!     astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
!     astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!rh
      description = 'relative humidity'
      units       = 'fraction'
      astat = NF_DEF_VAR(ncid, 'rh', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!alt
      description = 'inverse density'
      units       = 'm3 Kg-1'
      astat = NF_DEF_VAR(ncid, 'alt', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qcloud
      description = 'Cloud water mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qcloud', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qrain
      description = 'Rain water mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qrain', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qice
      description = 'Ice mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qice', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qsnow
      description = 'Snow mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qsnow', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qgraup
      description = 'Graupel mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qgraup', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!qvapor
      description = 'Water vapor mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qvapor', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

#if ( WRF_CHEM == 1 )

      IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN
!chem
      units       = 'ppmv'
      do m= 1,grid%track_chem_num
         chem_name = trim(model_config_rec%track_chem_name(m))
         description = trim(chem_name) // ' concentration'
         astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
         astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
         astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
      end do

      units       = 'min-1'
!o31d
      chem_name   = 'photr_o31d' 
      description = 'O31D Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!o33p
      chem_name   = 'photr_o33p' 
      description = 'O33P Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!no2
      chem_name   = 'photr_no2' 
      description = 'NO2 Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!hno2
      chem_name   = 'photr_hno2' 
      description = 'HNO2 Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!hno3
      chem_name   = 'photr_hno3' 
      description = 'HNO3 Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!h2o2
      chem_name   = 'photr_h2o2' 
      description = 'H2O2 Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!ch3o2h
      chem_name   = 'photr_ch3o2h' 
      description = 'CH3O2H Photolysis Rate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 2, var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      if (model_config_rec%phot_opt(grid%id) == 3 ) then

      units       = ''
!radfld
      var_dim(1) = level_zref_dim
      var_dim(2) = rad_dim
      var_dim(3) = time_dim
      chem_name   = 'radfld' 
      description = 'radfld'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 3, var_dim(1:3), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!adjcoe
      var_dim(1) = level_zref_dim
      var_dim(2) = tuv_dim
      var_dim(3) = time_dim
      chem_name   = 'adjcoe' 
      description = 'adjcoe'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 3, var_dim(1:3), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )
!phrate
      var_dim(1) = level_zref_dim
      var_dim(2) = tuv_dim
      var_dim(3) = time_dim
      chem_name   = 'phrate' 
      description = 'phrate'
      astat = NF_DEF_VAR(ncid, trim(chem_name), NF_REAL, 3, var_dim(1:3), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      end if

      END IF
#endif

!w
      var_dim(1) = level_stag_dim
      var_dim(2) = time_dim
      description = 'z-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'w', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

! --  end define

      astat = NF_ENDDEF(ncid)

!--   write Times variable
!Times
      start(1) = 1
      start(2) = 1
      count(1) = DateStrLen
      count(2) = 1

      astat = NF_INQ_VARID(ncid,'Times',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_TEXT(ncid,var_id,start,count,grid%track_time_domain(m))
      end do
      write (*,*) 'var_id,grid%track_time_domain = ', var_id,grid%track_time_domain

!--   write 1-D variables

#if ( WRF_CHEM == 1 )
      IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN
      if ( model_config_rec%phot_opt(grid%id) == 3 )then
!wc
      start(2) = 1
      count(2) = num_rad

      astat = NF_INQ_VARID(ncid,'wc',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_wc)
!zref
      start(2) = 1
      count(2) = level_zref

      astat = NF_INQ_VARID(ncid,'zref',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_zref)

      end if

      END IF
#endif

!lat
      start(2) = 1
      count(2) = grid%track_loc_domain

      astat = NF_INQ_VARID(ncid,'lat',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_lat_domain)
      write (*,*) 'var_id,grid%track_lat_domain = ', var_id,grid%track_lat_domain
!lon
      astat = NF_INQ_VARID(ncid,'lon',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_lon_domain)
      write (*,*) 'var_id,grid%track_lon_domain = ', var_id,grid%track_lon_domain
!grid_i
      astat = NF_INQ_VARID(ncid,'grid_i',var_id)
      astat = NF_PUT_VARA_INT(ncid,var_id,start(2),count(2),grid%track_i)
!grid_j
      astat = NF_INQ_VARID(ncid,'grid_j',var_id)
      astat = NF_PUT_VARA_INT(ncid,var_id,start(2),count(2),grid%track_j)
!ele
      astat = NF_INQ_VARID(ncid,'ele',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_ele)
      write (*,*) 'var_id,grid%track_ele = ', var_id,grid%track_ele

!--   write 2-D variables

      start(1) = 1
      start(2) = 1
      count(1) = level
      count(2) = 1
!z
      astat = NF_INQ_VARID(ncid,'z',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_z(m,:))
      end do
!p
      astat = NF_INQ_VARID(ncid,'p',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_p(m,:))
      end do
!t
      astat = NF_INQ_VARID(ncid,'t',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_t(m,:))
      end do
!u
      astat = NF_INQ_VARID(ncid,'u',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_u(m,:))
      end do
!v
      astat = NF_INQ_VARID(ncid,'v',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_v(m,:))
      end do
!w
!     astat = NF_INQ_VARID(ncid,'w',var_id)
!     do m= 1,grid%track_loc_domain
!        start(2) = m
!        astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_w(m,:))
!     end do
!rh
      astat = NF_INQ_VARID(ncid,'rh',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_rh(m,:))
      end do
!alt
      astat = NF_INQ_VARID(ncid,'alt',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_alt(m,:))
      end do
!qcloud
      astat = NF_INQ_VARID(ncid,'qcloud',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qcloud(m,:))
      end do
!qrain
      astat = NF_INQ_VARID(ncid,'qrain',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qrain(m,:))
      end do
!qice
      astat = NF_INQ_VARID(ncid,'qice',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qice(m,:))
      end do
!qsnow
      astat = NF_INQ_VARID(ncid,'qsnow',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qsnow(m,:))
      end do
!qgraup
      astat = NF_INQ_VARID(ncid,'qgraup',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qgraup(m,:))
      end do
!qvapor
      astat = NF_INQ_VARID(ncid,'qvapor',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qvapor(m,:))
      end do

#if ( WRF_CHEM == 1 )
!chem
      IF ( model_config_rec%chem_opt(grid%id) > 0 .and. grid%track_chem_num > 0 ) THEN

      do n= 1,grid%track_chem_num
         chem_name = trim(model_config_rec%track_chem_name(n))
         astat = NF_INQ_VARID(ncid,trim(chem_name),var_id)

         do m= 1,grid%track_loc_domain
            start(2) = m
            astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_chem(n,m,:))          
         end do

!        write (*,*) 'n, var_id, chem_name = ', n, var_id, trim(chem_name)
      end do

!o31d
      astat = NF_INQ_VARID(ncid,'photr_o31d',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_o31d(m,:))
      end do
!o33p
      astat = NF_INQ_VARID(ncid,'photr_o33p',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_o33p(m,:))
      end do
!no2
      astat = NF_INQ_VARID(ncid,'photr_no2',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_no2(m,:))
      end do
!hno2
      astat = NF_INQ_VARID(ncid,'photr_hno2',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_hno2(m,:))
      end do
!hno3
      astat = NF_INQ_VARID(ncid,'photr_hno3',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_hno3(m,:))
      end do
!h2o2
      astat = NF_INQ_VARID(ncid,'photr_h2o2',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_h2o2(m,:))
      end do
!ch3o2h
      astat = NF_INQ_VARID(ncid,'photr_ch3o2h',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_ch3o2h(m,:))
      end do

      if (model_config_rec%phot_opt(grid%id) == 3 ) then
!radfld
      start(1) = 1
      start(2) = 1
      start(3) = 1
      count(1) = level_zref
      count(2) = 1
      count(3) = 1

      astat = NF_INQ_VARID(ncid,'radfld',var_id)
      do m= 1,grid%track_loc_domain
      do n= 1,num_rad
         start(2) = n
         start(3) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:3),count(1:3),grid%track_radfld(m,n,:))
      end do
      end do
!adjcoe
      astat = NF_INQ_VARID(ncid,'adjcoe',var_id)
      do m= 1,grid%track_loc_domain
      do n= 1,num_tuv
         start(2) = n
         start(3) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:3),count(1:3),grid%track_adjcoe(m,n,:))
      end do
      end do
!phrate
      astat = NF_INQ_VARID(ncid,'phrate',var_id)
      do m= 1,grid%track_loc_domain
      do n= 1,num_tuv
         start(2) = n
         start(3) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:3),count(1:3),grid%track_phrate(m,n,:))
      end do
      end do

      end if

      END IF
#endif

!w
      start(1) = 1
      start(2) = 1
      count(1) = level_stag
      count(2) = 1

      astat = NF_INQ_VARID(ncid,'w',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_w(m,:))
      end do

!--   close output unit

      astat = NF_CLOSE(ncid)

   END IF

   grid%track_next_time = 1
#endif

END SUBROUTINE write_track

SUBROUTINE calc_track_locations( grid )

   USE module_domain
   USE module_configure
   USE module_dm
   USE module_llxy

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(INOUT) :: grid
#if ( EM_CORE == 1 )
   ! Externals
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ! Local variables
   INTEGER :: track_loc_temp
   INTEGER :: i, k, iunit
   REAL :: track_rx, track_ry
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   TYPE (PROJ_INFO) :: track_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey

   IF ( grid%track_loc <= 0 ) then
     RETURN
   ENDIF

#if ( DA_CORE != 1 )
   IF ( grid%dfi_stage == DFI_FST ) THEN
#endif
     CALL get_ijk_from_grid ( grid ,                               &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              imsx, imex, jmsx, jmex, kmsx, kmex,  &
                              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                              imsy, imey, jmsy, jmey, kmsy, kmey,  &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
! Set up map transformation structure
     CALL map_init(track_proj)
   
     IF (ips <= 1 .AND. 1 <= ipe .AND. &
         jps <= 1 .AND. 1 <= jpe) THEN
        known_lat = grid%xlat(1,1)
        known_lon = grid%xlong(1,1)
     ELSE
        known_lat = 9999.
        known_lon = 9999.
     ENDIF
     known_lat = wrf_dm_min_real(known_lat)
     known_lon = wrf_dm_min_real(known_lon)
   
! Mercator
   IF (config_flags%map_proj == PROJ_MERC) THEN
     CALL map_set(PROJ_MERC, track_proj,                &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   
! Lambert conformal
   ELSE IF (config_flags%map_proj == PROJ_LC) THEN
     CALL map_set(PROJ_LC, track_proj,                   &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
! Polar stereographic
   ELSE IF (config_flags%map_proj == PROJ_PS) THEN
     CALL map_set(PROJ_PS, track_proj,                   &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
! Cassini (global ARW)
   ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
     CALL map_set(PROJ_CASSINI, track_proj,                             &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &
! We still need to get POLE_LAT and POLE_LON metadata variables before
!   this will work for rotated poles.
                      lat0     = 90.0,                                  &
                      lon0     = 0.0,                                   &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)

! Rotated latitude-longitude
   ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
     CALL map_set(PROJ_ROTLL, track_proj,                       &
! I have no idea how this should work for NMM nested domains
                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
   ENDIF

   IF (.NOT. grid%track_have_calculated) THEN
     grid%track_have_calculated = .TRUE.
     WRITE(message, '(A46,I3)') 'Computing track locations inside model domain ', grid%id
     CALL wrf_message(message)

!--------------------------------------------------------
! initialize
!--------------------------------------------------------

     grid%track_next_time = 1
!--------------------------------------------------------
! Determine track locations inside model domain and the corresponding model grid
!--------------------------------------------------------
     track_loc_temp = 0
     DO k = 1,grid%track_loc
       CALL latlon_to_ij(track_proj, grid%track_lat_in(k), grid%track_lon_in(k), track_rx, track_ry)
       track_loc_temp = track_loc_temp + 1
!--------------------------------------------------------
! found the corresponding model grid
!--------------------------------------------------------
       grid%track_i(track_loc_temp) = NINT(track_rx)
       grid%track_j(track_loc_temp) = NINT(track_ry)
!--------------------------------------------------------
! found the corresponding track time
!--------------------------------------------------------
       grid%track_time_domain(track_loc_temp) = grid%track_time_in(k)
!--------------------------------------------------------
! Is point outside of domain (or on the edge of domain)? -- don't count
!--------------------------------------------------------
       IF (grid%track_i(track_loc_temp) < ids .OR. grid%track_i(track_loc_temp) > ide .OR. &
           grid%track_j(track_loc_temp) < jds .OR. grid%track_j(track_loc_temp) > jde) THEN
         track_loc_temp = track_loc_temp - 1   
       ENDIF
     ENDDO

!--------------------------------------------------------
! put the total valid track locations into grid%track_loc_domain
!--------------------------------------------------------
     grid%track_loc_domain = track_loc_temp
!--------------------------------------------------------
! found the corresponding model lat and lon and elevation
!--------------------------------------------------------
     DO k = 1,grid%track_loc_domain
!--------------------------------------------------------
! If location is outside of patch, we need to get lat/lon of track grid cell from another patch
!--------------------------------------------------------
       IF (grid%track_i(k) < ips .OR. grid%track_i(k) > ipe .OR. &
           grid%track_j(k) < jps .OR. grid%track_j(k) > jpe) THEN
         grid%track_lat_domain(k) = 1.E30
         grid%track_lon_domain(k) = 1.E30
         grid%track_ele(k) = 1.E30
       ELSE
         grid%track_lat_domain(k) = grid%xlat(grid%track_i(k),grid%track_j(k))
         grid%track_lon_domain(k) = grid%xlong(grid%track_i(k),grid%track_j(k))
         grid%track_ele(k) = grid%ht(grid%track_i(k),grid%track_j(k))
       ENDIF

#if DM_PARALLEL
       grid%track_ele(k)         = wrf_dm_min_real(grid%track_ele(k))
       grid%track_lat_domain(k)  = wrf_dm_min_real(grid%track_lat_domain(k))
       grid%track_lon_domain(k)  = wrf_dm_min_real(grid%track_lon_domain(k))

       call wrf_dm_bcast_string(grid%track_time_domain(k), 19)
#endif
     END DO

     write(message,*) 'calc_track_locations: valid track locations in the model domain ', grid%track_loc_domain
     call wrf_message( trim(message) )
   
    ENDIF
#if ( DA_CORE != 1 )
   ENDIF
#endif
#endif

END SUBROUTINE calc_track_locations
