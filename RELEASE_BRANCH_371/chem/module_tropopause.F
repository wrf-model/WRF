! This module is used to diagnose the location of the tropopause.
! Two routines are specifed, a primary routine (twmo) which is tried first 
! and a backup routine (climate) which will be tried only if the first routine fails. 
! If the tropopause can not be identified by either routine, then a NOTFOUND is returned.
!
! These routines are based upon code in the WACCM chemistry module
! including mo_tropoause.F90 and llnl_set_chem_trop.F90. The code
! for the Reichler et al. [2003] algorithm is from:
!
!   http://www.gfdl.noaa.gov/~tjr/TROPO/tropocode.htm
!
! Author: Jeff Lee
! Created: Feb., 2011

 module module_tropopause

      implicit none

      private

      public  :: tropopause_init
      public  :: tropopause_driver

      save

      integer,  parameter :: r8    = selected_real_kind(12) 

      real(r8), parameter :: pi    =3.14159265358979323846_r8
      real(r8), parameter :: mb2Pa = 100._r8
      real(r8), parameter :: d2r   = pi/180._r8
      real(r8), parameter :: zero  = 0._r8
      real(r8), parameter :: twopi = pi*2._r8

      integer,  parameter :: NOTFOUND = -1
 
      integer :: iend 
      integer :: jend

      integer :: tropo_month_n      ! # of month in tropopause file
      integer :: tropo_lon_n        ! # of lon   in tropopause file
      integer :: tropo_lat_n        ! # of lat   in tropopause file

      type tropo_type
          real(r8), pointer :: tropo_p_loc(:,:,:)  ! climatological tropopause pressures(Pa)
                                                   ! on wrf model grids
          logical           :: is_allocated 
      end type tropo_type 

      type(tropo_type), allocatable :: tropo_bc(:)

      ! physical constants
      ! These constants are set in module variables rather than as parameters 
      ! to support the aquaplanet mode in which the constants have values determined
      ! by the experiment protocol

      real(r8),parameter :: CONST_BOLTZ  = 1.38065e-23_r8  ! Boltzmann's constant ~ J/K/molecule
      real(r8),parameter :: CONST_AVOGAD = 6.02214e26_r8   ! Avogadro's number ~ molecules/kmole
      real(r8),parameter :: CONST_RGAS   = CONST_AVOGAD*CONST_BOLTZ ! Universal gas constant ~ J/K/kmole
      real(r8),parameter :: CONST_MWDAIR = 28.966_r8       ! molecular weight dry air ~ kg/kmole
      real(r8),parameter :: CONST_RDAIR  = CONST_RGAS/CONST_MWDAIR ! Dry air gas constant ~ J/K/kg
      real(r8),parameter :: CONST_CPDAIR  = 1.00464e3_r8    ! specific heat of dry air   ~ J/kg/K

      real(r8),parameter :: gravit       = 9.80616_r8
      real(r8),parameter :: rair         = CONST_RDAIR
      real(r8),parameter :: cappa        = CONST_RDAIR/CONST_CPDAIR   !R/CP

      real(r8),parameter :: cnst_kap    = cappa
      real(r8),parameter :: cnst_faktor = -gravit/rair
      real(r8),parameter :: cnst_ka1    = cnst_kap - 1._r8

      contains

      subroutine tropopause_driver( id, dtstep, current_date_char,  &
                                    t_phy, p_phy, p8w, zmid, z8w,   &
                                    tropo_lev, tropo_p, tropo_z,    &
                                    ids, ide, jds, jde, kds, kde,   &
                                    ims, ime, jms, jme, kms, kme,   &
                                    its, ite, jts, jte, kts, kte    )

      implicit none

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in   ) :: id,                             &
                                ids,ide, jds,jde, kds,kde,      &
                                ims,ime, jms,jme, kms,kme,      &
                                its,ite, jts,jte, kts,kte

      real, intent(in  ) :: dtstep

      real, dimension( ims:ime, kms:kme, jms:jme ),             &
               intent(in   ) :: t_phy,  & ! t at mid-level 
                                p_phy,  & ! p at mid_level (Pa)
                                zmid,   & ! z at mid_level (meters)
                                z8w,    & ! z at interface (meters)
                                p8w       ! p at interface (Pa)

      real, dimension( ims:ime, jms:jme ),             &
               intent(inout) :: tropo_p,  & ! tropopause pressure (Pa) 
                                tropo_z     ! tropopause height   (meters)
      integer, dimension( ims:ime, jms:jme ),          &
               intent(inout) :: tropo_lev   ! tropopause level

      CHARACTER (LEN=256),intent(in) :: current_date_char
!----------------------------------------------------
!     local scalars
!---------------------------------------------------- 

      integer :: i, j, k

!----------------------------------------------------
!----------------------------------------------------
! ... tile dimensions, needed for nest domains
!----------------------------------------------------
      iend = min(ite,ide-1)
      jend = min(jte,jde-1)

      tropo_lev(its:iend,jts:jend) = NOTFOUND

! --  This is the primary routine 
       
      call tropopause_twmo (id, t_phy, p_phy, p8w, zmid, z8w, &
                            tropo_lev, tropo_p, tropo_z,      &
                            ids,ide, jds,jde, kds,kde,        &
                            ims,ime, jms,jme, kms,kme,        &
                            its,ite, jts,jte, kts,kte         )

! --  This is the backup routine

      if ( any(tropo_lev(its:iend,jts:jend) == NOTFOUND) ) then

         call tropopause_climate (id, current_date_char,       &
                                  p_phy, p8w, zmid, z8w,       &
                                  tropo_lev, tropo_p, tropo_z, &
                                  ids,ide, jds,jde, kds,kde,   &
                                  ims,ime, jms,jme, kms,kme,   &
                                  its,ite, jts,jte, kts,kte    )
      end if

   end subroutine tropopause_driver

!===========================================================================

      subroutine tropopause_init (id, xlat, xlon, config_flags, &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte     )
!---------------------------------------------------------------------
!       ... new initialization routine for tropopause
!---------------------------------------------------------------------
      use module_interpolate,  only : lininterp_init, lininterp, interp_type, lininterp_finish
      use module_configure,    only : grid_config_rec_type

      implicit none

!---------------------------------------------------------------------
! ... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: id,                          &
                             ids,ide, jds,jde, kds,kde,   &
                             ims,ime, jms,jme, kms,kme,   &
                             its,ite, jts,jte, kts,kte

      real,    intent(in) :: xlat(ims:ime, jms:jme)
      real,    intent(in) :: xlon(ims:ime, jms:jme)
      type(grid_config_rec_type), intent(in) :: config_flags

!---------------------------------------------------------------------
! ... local variables
!---------------------------------------------------------------------

      type(interp_type) :: lon_wgts, lat_wgts

      integer :: max_dom
      integer :: astat
      integer :: ncid
      integer :: varid
      integer :: dimid(3)
      integer :: start(3)
      integer :: count(3)
      integer :: dimid_lon
      integer :: dimid_lat
      integer :: dimid_month
      integer :: ndims  

      character(len=128) :: err_msg
      character(len=64)  :: filename
      character(len=3)   :: id_num

      character(len=80) :: attribute

      real(r8), allocatable :: tropo_p_in(:,:,:)  ! values of pressure levels (Pa) in tropopause file
      real(r8), allocatable :: tropo_lat(:)       ! values of lat (-90~90)in tropopause file
      real(r8), allocatable :: tropo_lon(:)       ! values of lon (0~360) in tropopause file

      real(r8) :: wrf_lon(1)       ! to match kind, values of lon (0~360)
      real(r8) :: wrf_lat(1)       ! input to lininterp_init needs to be an array, not scalar
      real(r8) :: tmp_out(1)       

      integer  :: i, j, m
      CHARACTER(LEN=132) :: message

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

include 'netcdf.inc'
!---------------------------------------------------------------------
! ... tile dimensions
!---------------------------------------------------------------------
      iend = min(ite,ide-1)
      jend = min(jte,jde-1)

!---------------------------------------------------------------------
! ... allocate tropo_bc
!---------------------------------------------------------------------
   if( id == 1 .and. .not. allocated(tropo_bc) ) then
       CALL nl_get_max_dom( 1,max_dom )

       allocate(tropo_bc(max_dom),stat=astat )
       if( astat /= 0 ) then
          CALL wrf_message( 'tropopause_init: failed to allocate tropo_bc' )
          CALL wrf_abort
       end if

       tropo_bc(:)%is_allocated = .false.
  endif

tropo_bc_allocated : &
  if( .not. tropo_bc(id)%is_allocated ) then

!---------------------------------------------------------------------
!... allocate tropo_bc type
!--------------------------------------------------------------------

master_proc : &
   IF( wrf_dm_on_monitor() ) THEN
      write(id_num,'(i3)') 100+id
      write(message,*) 'tropopause_init: intializing domain ' // id_num(2:3)
      call wrf_message( trim(message) )
       
!---------------------------------------------------------------------
! ... open climate tropopause netcdf file
!---------------------------------------------------------------------
!     filename = 'clim_p_trop.nc' 
      filename = config_flags%trop_lev_inname
      if( filename == ' ' ) then
        call wrf_message( 'tropopause_init: input filename not specified in namelist' )
        call wrf_abort
      endif

      err_msg = 'tropopause_init: failed to open file ' // trim(filename)
      call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
      write(message,*) 'tropopause_init: open filename= ',filename
      call wrf_message( trim(message) )
!---------------------------------------------------------------------
! ... get dimensions
!---------------------------------------------------------------------
      err_msg = 'tropopause_init: failed to get time id'
      call handle_ncerr( nf_inq_dimid( ncid, 'time', dimid_month ), trim(err_msg) )
      err_msg = 'tropopause_init: failed to get time'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_month, tropo_month_n ), trim(err_msg) )
      if( tropo_month_n /= 12 )then
       write(message,*) 'tropopause_init: number of months = ',tropo_month_n,'; expecting 12'
       call wrf_message( trim(message) )
       call wrf_abort
      end if

      err_msg = 'tropopause_init: failed to get lat id'
      call handle_ncerr( nf_inq_dimid( ncid, 'lat', dimid_lat ), trim(err_msg) )
      err_msg = 'tropopause_init: failed to get lat'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_lat, tropo_lat_n ), trim(err_msg) )

      err_msg = 'tropopause_init: failed to get lon id'
      call handle_ncerr( nf_inq_dimid( ncid, 'lon', dimid_lon ), trim(err_msg) )
      err_msg = 'tropopause_init: failed to get lon'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_lon, tropo_lon_n ), trim(err_msg) )

   END IF master_proc
!---------------------------------------------------------------------
! ... bcast the dimensions
!---------------------------------------------------------------------
#ifdef DM_PARALLEL
      CALL wrf_dm_bcast_integer ( tropo_month_n , 1 )
      CALL wrf_dm_bcast_integer ( tropo_lat_n   , 1 )
      CALL wrf_dm_bcast_integer ( tropo_lon_n   , 1 )
#endif

!---------------------------------------------------------------------
! ... allocate local arrays 
!---------------------------------------------------------------------

      allocate( tropo_lat(tropo_lat_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'tropopause_init: failed to allocate tropo_lat' )
         call wrf_abort
      end if

      allocate( tropo_lon(tropo_lon_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'tropopause_init: failed to allocate tropo_lon' )
         call wrf_abort
      end if

      allocate( tropo_p_in(tropo_lon_n,tropo_lat_n,tropo_month_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'tropopause_init: failed to allocate tropo_p_in' )
         call wrf_abort
      end if
 
!---------------------------------------------------------------------
! ... allocate tropo_bc(id) component arrays 
!---------------------------------------------------------------------

      allocate( tropo_bc(id)%tropo_p_loc(its:iend,jts:jend,tropo_month_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'tropopause_init: failed to allocate tropo_bc(id)%tropo_p_loc' )
         call wrf_abort
      end if  

      tropo_bc(id)%is_allocated = .true.

!---------------------------------------------------------------------
! ... read arrays
!---------------------------------------------------------------------
master_proc_a : &
   IF ( wrf_dm_on_monitor() ) THEN
!---------------------------
!lat
      err_msg = 'tropopause_init: failed to get lat variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'lat', varid ), trim(err_msg) )
      err_msg = 'tropopause_init: failed to read lat variable'
      call handle_ncerr( nf_get_var_double( ncid, varid, tropo_lat ), trim(err_msg) )

      !-------- check unit
     tropo_lat(:) = tropo_lat(:) * d2r

!---------------------------
!lon
      err_msg = 'tropopause_init: failed to get lon variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'lon', varid ), trim(err_msg) )
      err_msg = 'tropopause_init: failed to read lon variable'
      call handle_ncerr( nf_get_var_double( ncid, varid, tropo_lon ), trim(err_msg) )

      !-------- check unit and note: 0-360 degree
      tropo_lon(:) = tropo_lon(:) * d2r
!---------------------------
!tropo_p_in    
      err_msg = 'tropopause_init: failed to get trop_p variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'trop_p', varid ), trim(err_msg) )

      ! check dimensions

      err_msg = 'tropopause_init: failed to get ndims of trop_p variable'
      call handle_ncerr( nf_inq_varndims( ncid, varid, ndims ), trim(err_msg) )

      if( ndims /= 3 ) then
         write(message,*) 'tropopause_init: error! variable trop_p has ndims = ',ndims,', expecting 3'
         call wrf_message( trim(message) )
         call wrf_abort
      end if

      err_msg = 'tropopause_init: failed to get dimid of vmr variable'
      call handle_ncerr( nf_inq_vardimid( ncid, varid, dimid ), trim(err_msg) )

      if( dimid(1) /= dimid_lon   .or. dimid(2) /= dimid_lat .or. &
          dimid(3) /= dimid_month )then
          write(message,*) 'tropopause_init: error! dimensions in wrong order for variable trop_p,'// &
               'expecting (lon,lat,month)'
          call wrf_message( trim(message) )
          call wrf_abort
      end if

      !------------------------------------------------------------------
      ! ... read in the tropo_p_in values
      !------------------------------------------------------------------
      err_msg = 'tropopause_init: failed to read trop_p variable'
      call handle_ncerr( nf_get_var_double( ncid, varid, tropo_p_in ), trim(err_msg) )

!---------------------------------------------------------------------
!... close input netcdf file
!---------------------------------------------------------------------
      err_msg = 'tropopause_init: failed to close file ' // trim(filename)
      call handle_ncerr( nf_close( ncid ), trim(err_msg) )

   END IF master_proc_a

!---------------------------------------------------------------------
! ... bcast the variables
!---------------------------------------------------------------------
#ifdef DM_PARALLEL
      CALL wrf_dm_bcast_double ( tropo_lat, size(tropo_lat) )
      CALL wrf_dm_bcast_double ( tropo_lon, size(tropo_lon) )
      CALL wrf_dm_bcast_double ( tropo_p_in, size(tropo_p_in) )
#endif

!------------------------------------------------------------------
! ... linear interpolation from tropo_p_in to tropo_p_loc 
!------------------------------------------------------------------
      !-------------------------------------
      !... get wrf_lat_1d and wrf_lon_1d
      !... note: grid%XLAT(ide,:)  =0.
      !... note: grid%XLONG(:,jde) =0.
      !...       => iend = min(ite,ide-1)
      !             jend = min(jte,jde-1)
      !-------------------------------------

      !-------------------------------------------
      !... for every point in the tile
      !-------------------------------------------
      do i = its,iend
        do j = jts,jend   ! input to lininterp_init needs to be an array, not scalar
         !-------------------------------------------
         !...  from degrees to radians
         !-------------------------------------------
          wrf_lat(1) = xlat(i,j)*d2r

          wrf_lon(1) = xlon(i,j)     
          if( wrf_lon(1)  < 0.0_r8 ) wrf_lon(1) = wrf_lon(1) + 360.0_r8
          wrf_lon(1) = wrf_lon(1)*d2r
   
         !-------------------------------------
         !...initialization interp routine
         !-------------------------------------
          call lininterp_init( tropo_lon, tropo_lon_n, wrf_lon, 1, 2, lon_wgts, zero, twopi )
          call lininterp_init( tropo_lat, tropo_lat_n, wrf_lat, 1, 1, lat_wgts )

         !-------------------------------------
         !... linear interpolation
         !-------------------------------------
          do m = 1,tropo_month_n
            call lininterp( tropo_p_in(:,:,m), tropo_lon_n, tropo_lat_n, &
                            tmp_out, 1 , lon_wgts, lat_wgts)
            tropo_bc(id)%tropo_p_loc(i,j,m) = tmp_out(1)    
          end do

        end do
      end do

      call lininterp_finish( lon_wgts )
      call lininterp_finish( lat_wgts )

      deallocate(tropo_lon)
      deallocate(tropo_lat)
      deallocate(tropo_p_in)
      
      call wrf_message( ' ' )
      write(message,*) 'tropopause_init: DONE intialized domain ',id
      call wrf_message( trim(message) )
      call wrf_message( ' ' )

endif tropo_bc_allocated

      end subroutine tropopause_init

!===========================================================================

      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!       ... netcdf error handling routine
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!       ... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: ret
      character(len=*), intent(in) :: mes

include 'netcdf.inc'

      if( ret /= nf_noerr ) then
         call wrf_message( trim(mes) )
         call wrf_message( trim(nf_strerror(ret)) )
         call wrf_abort
      end if

      end subroutine handle_ncerr

!===========================================================================

  ! This routine uses an implementation of Reichler et al. [2003] done by
  ! Reichler and downloaded from his web site. This is similar to the WMO
  ! routines, but is designed for GCMs with a coarse vertical grid.

  subroutine tropopause_twmo (id, t_phy, p_phy, p8w, zmid, z8w, & 
                              tropo_lev, tropo_p,tropo_z,       &
                              ids,ide, jds,jde, kds,kde,        &
                              ims,ime, jms,jme, kms,kme,        &
                              its,ite, jts,jte, kts,kte         )
      implicit none

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in) :: id,                         &
                             ids,ide, jds,jde, kds,kde,  &
                             ims,ime, jms,jme, kms,kme,  &
                             its,ite, jts,jte, kts,kte

      real, dimension( ims:ime , kms:kme , jms:jme ),    &
            intent(in)    :: t_phy,  & ! t at mid-level 
                             p_phy,  & ! p at mid_level (Pa)
                             zmid,   & ! z at mid_level (meters)
                             z8w,    & ! z at interface (meters)
                             p8w       ! p at interface (Pa)

      real, dimension( ims:ime, jms:jme ),             &
               intent(inout) :: tropo_p,  & ! tropopause pressure (Pa) 
                                tropo_z     ! tropopause height   (meters)
      integer, dimension( ims:ime, jms:jme ),          &
               intent(inout) :: tropo_lev   ! tropopause level
!----------------------------------------------------
!     local scalars
!----------------------------------------------------
    real, dimension( kts:kte) :: t_temp    ! store t from top to bottom
    real, dimension( kts:kte) :: p_temp    ! store p from top to bottom
 
    real(r8), parameter     :: gamma    = -0.002_r8    ! K/m
    real(r8), parameter     :: plimu    = 45000._r8    ! Pa
    real(r8), parameter     :: pliml    = 7500._r8     ! Pa
     
    integer                 :: i
    integer                 :: j
    integer                 :: k
    integer                 :: kk
    integer                 :: pLev

    real(r8)                :: tP                       ! tropopause pressure (Pa)
    real(r8)                :: dZdlogP
!---------------------------------------------------- 

    pLev = kte - kts + 1

    ! Iterate over all of the grids.
    do i = its,iend
    do j = jts,jend

       ! subroutine twmo expects variables from top to bottom
       ! t_phy and p_phy are from bottom to top

       do k = kts,kte
          kk = pLev - k + 1
          t_temp(kk) = t_phy(i,k,j)
          p_temp(kk) = p_phy(i,k,j)
       end do
 
       ! Use the routine from Reichler.

       call twmo(pLev, t_temp, p_temp, plimu, pliml, gamma, tP)
     
       ! if successful, store the results and find the level and temperature.
       if (tP > 0) then        
          ! Find the associated level, from bottom to top
          do k = kts,kte-1
             if (tP >= p8w(i,k,j)) then
                tropo_lev(i,j) = k - 1      ! needed for wrf, from bottom to top
                tropo_p  (i,j) = tP
                exit
             end if
          end do
          !----------------------------------------------------------
          ! get tropopause height
          ! Intrepolate the geopotential height linearly against log(P)

          k = k - 1     ! needed for wrf, from bottom to top
    
          ! Is the tropoause at the midpoint?
          if (tropo_p(i,j) == p_phy(i,k,j)) then
             tropo_z(i,j)= zmid(i,k,j)
          else if (tropo_p(i,j) < p_phy(i,k,j)) then
             ! It is below the midpoint. Make sure we aren't at the bottom.
             if ( k > kts ) then 
                dZdlogP = (zmid(i,k,j) - z8w(i,k-1,j)) / &
                          (log(p_phy(i,k,j)) - log(p8w(i,k-1,j)) )
                tropo_z(i,j)= zmid(i,k,j) + (log(tropo_p(i,j)) - log(p_phy(i,k,j))) * dZdlogP
             end if
          else
             ! It is above the midpoint? Make sure we aren't at the top.
             if ( k < kte ) then
                dZdlogP = (zmid(i,k,j) - z8w(i,k,j)) / &
                          (log(p_phy(i,k,j)) - log(p8w(i,k,j)) )
                tropo_z(i,j)= zmid(i,k,j) + (log(tropo_p(i,j)) - log(p_phy(i,k,j))) * dZdlogP
             end if
          end if
          !----------------------------------------------------------      
       end if

!      if ( tropo_lev(i,j) == NOTFOUND ) then
!         write (*,*) 'tropopause_twmo: NOTFOUND at id, i, j = ', id, i, j
!      end if

    end do
    end do

  end subroutine tropopause_twmo

!===========================================================================

  ! This routine is an implementation of Reichler et al. [2003] done by
  ! Reichler and downloaded from his web site. Minimal modifications were
  ! made to have the routine work within the CAM framework (i.e. using
  ! CAM constants and types).
  !
  ! NOTE: I am not a big fan of the goto's and multiple returns in this
  ! code, but for the moment I have left them to preserve as much of the
  ! original and presumably well tested code as possible.
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! determination of tropopause height from gridded temperature data
  !
  ! reference: Reichler, T., M. Dameris, and R. Sausen (2003)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine twmo(level, t, p, plimu, pliml, gamma, trp)

    integer, intent(in)                     :: level
!jeff
!   real(r8), intent(in), dimension(level)  :: t, p
    real,     intent(in), dimension(level)  :: t, p

    real(r8), intent(in)                    :: plimu, pliml, gamma
    real(r8), intent(out)                   :: trp
    
    real(r8), parameter                     :: deltaz = 2000.0_r8

    real(r8)                                :: pmk, pm, a, b, tm, dtdp, dtdz
    real(r8)                                :: ag, bg, ptph
    real(r8)                                :: pm0, pmk0, dtdz0
    real(r8)                                :: p2km, asum, aquer
    real(r8)                                :: pmk2, pm2, a2, b2, tm2, dtdp2, dtdz2
    integer                                 :: icount, jj
    integer                                 :: j
    

    trp=-99.0_r8                           ! negative means not valid

lev_loop : &
    do j=level,2,-1
    
      ! dt/dz
      pmk= .5_r8 * (p(j-1)**cnst_kap+p(j)**cnst_kap)
      pm = pmk**(1/cnst_kap)               
      a = (t(j-1)-t(j))/(p(j-1)**cnst_kap-p(j)**cnst_kap)
      b = t(j)-(a*p(j)**cnst_kap)
      tm = a * pmk + b               
      dtdp = a * cnst_kap * (pm**cnst_ka1)
      dtdz = cnst_faktor*dtdp*pm/tm
      ! dt/dz valid?
!     if (j.eq.level)    go to 999     ! no, start level, initialize first
!     if (dtdz.le.gamma) go to 999     ! no, dt/dz < -2 K/km
!     if (pm.gt.plimu)   go to 999     ! no, too low
      if( j == level .or. dtdz <= gamma .or. pm > plimu ) then
        pm0   = pm
        pmk0  = pmk
        dtdz0 = dtdz
        cycle lev_loop
      endif
  
      ! dtdz is valid, calculate tropopause pressure
      if (dtdz0 < gamma) then
        ag = (dtdz-dtdz0) / (pmk-pmk0)     
        bg = dtdz0 - (ag * pmk0)          
        ptph = exp(log((gamma-bg)/ag)/cnst_kap)
      else
        ptph = pm
      endif
  
!     if (ptph.lt.pliml) go to 999     
!     if (ptph.gt.plimu) go to 999           
      if( ptph < pliml .or. ptph > plimu ) then
        pm0   = pm
        pmk0  = pmk
        dtdz0 = dtdz
        cycle lev_loop
      endif
  
      ! 2nd test: dtdz above 2 km must not exceed gamma
      p2km = ptph + deltaz*(pm/tm)*cnst_faktor     ! p at ptph + 2km
      asum = 0.0_r8                                ! dtdz above
      icount = 0                                   ! number of levels above
  
      ! test until apm < p2km
lev_loop_a : &
      do jj=j,2,-1
    
        pmk2 = .5_r8 * (p(jj-1)**cnst_kap+p(jj)**cnst_kap) ! p mean ^kappa
        pm2 = pmk2**(1/cnst_kap)                           ! p mean
        if( pm2 > ptph ) then                     ! doesn't happen
          cycle lev_loop_a
        endif
        if( pm2 < p2km ) then                     ! ptropo is valid
          trp = ptph
          exit lev_loop
        endif

        a2 = (t(jj-1)-t(jj))                     ! a
        a2 = a2/(p(jj-1)**cnst_kap-p(jj)**cnst_kap)
        b2 = t(jj)-(a2*p(jj)**cnst_kap)          ! b
        tm2 = a2 * pmk2 + b2                     ! T mean
        dtdp2 = a2 * cnst_kap * (pm2**(cnst_kap-1))  ! dt/dp
        dtdz2 = cnst_faktor*dtdp2*pm2/tm2
        asum = asum+dtdz2
        icount = icount+1
        aquer = asum/float(icount)               ! dt/dz mean
   
        ! discard ptropo ?
        if( aquer <= gamma ) then                ! dt/dz above < gamma
          pm0   = pm
          pmk0  = pmk
          dtdz0 = dtdz
          exit lev_loop_a
        endif
    
      enddo lev_loop_a
    
    enddo lev_loop

  end subroutine twmo

!===========================================================================

  ! Read the tropopause pressure in from a file containging a climatology. The
  ! data is interpolated to the current dat of year and latitude.
  !
  ! NOTE: The data is read in during tropopause_init and stored in the module
  ! variable tropo_bc(id)%tropo_p_loc

  subroutine tropopause_climate (id, current_date_char,        &
                                 p_phy, p8w, zmid, z8w,        &
                                 tropo_lev, tropo_p,  tropo_z, &
                                 ids,ide, jds,jde, kds,kde,    &
                                 ims,ime, jms,jme, kms,kme,    &
                                 its,ite, jts,jte, kts,kte     )
      implicit none

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in) :: id,                         &
                             ids,ide, jds,jde, kds,kde,  &
                             ims,ime, jms,jme, kms,kme,  &
                             its,ite, jts,jte, kts,kte

      real, dimension( ims:ime , kms:kme , jms:jme ),    &
            intent(in)    :: p_phy,  & ! p at mid-level (Pa)
                             zmid,   & ! z at mid_level (meters)
                             z8w,    & ! z at interface (meters) 
                             p8w       ! p at interface (Pa)

      real, dimension( ims:ime, jms:jme ),             &
               intent(inout) :: tropo_p,  & ! tropopause pressure (Pa) 
                                tropo_z     ! tropopause height   (meters)
      integer, dimension( ims:ime, jms:jme ),          &
               intent(inout) :: tropo_lev   ! tropopause level

      CHARACTER (LEN=256),intent(in) :: current_date_char
!----------------------------------------------------

    ! Local Variables

    real      :: del_time
    integer   :: last
    integer   :: next

    real(r8)  :: tP                       ! tropopause pressure (Pa)
    real(r8)  :: dZdlogP

    integer   :: i
    integer   :: j
    integer   :: k
    CHARACTER(LEN=132) :: message
!-----------------------------------------------------------------------
    
    if (any(tropo_lev(its:iend,jts:jend) == NOTFOUND)) then

       !------------------------------------------------------------------------
       ! setup time interpolation
       !------------------------------------------------------------------------

       call get_time_interp_factors( current_date_char, last, next, del_time )

       ! Iterate over all of the grids.

       do i = its,iend
       do j = jts,jend
       
          ! Skip grids in which the tropopause has already been found.

          if (tropo_lev(i,j) == NOTFOUND) then
        
             !--------------------------------------------------------
             ! ... get tropopause level from climatology
             !--------------------------------------------------------
             ! Interpolate the tropopause pressure.
             tP =  tropo_bc(id)%tropo_p_loc(i,j,last) &
                + (tropo_bc(id)%tropo_p_loc(i,j,next) &
                -  tropo_bc(id)%tropo_p_loc(i,j,last))*del_time

             if (tP > 0) then        
                ! Find the associated level, from bottom to top
                do k = kts,kte-1
                   if (tP >= p8w(i,k,j)) then
                      tropo_lev(i,j) = k - 1   ! needed for wrf, from bottom to top
                      tropo_p  (i,j) = tP
                      exit
                   end if
                end do

                !----------------------------------------------------------
                ! get tropopause height
                ! Intrepolate the geopotential height linearly against log(P)

                k = k - 1     ! needed for wrf
    
                ! Is the tropoause at the midpoint?
                if (tropo_p(i,j) == p_phy(i,k,j)) then
                   tropo_z(i,j)= zmid(i,k,j)
    
                else if (tropo_p(i,j) < p_phy(i,k,j)) then
                   ! It is below the midpoint. Make sure we aren't at the bottom.
                   if ( k > kts ) then 
                      dZdlogP = (zmid(i,k,j) - z8w(i,k-1,j)) / &
                                (log(p_phy(i,k,j)) - log(p8w(i,k-1,j)) )
                      tropo_z(i,j)= zmid(i,k,j) + (log(tropo_p(i,j)) - log(p_phy(i,k,j))) * dZdlogP
                   end if
                else
                   ! It is above the midpoint? Make sure we aren't at the top.
                   if ( k < kte ) then 
                      dZdlogP = (zmid(i,k,j) - z8w(i,k,j)) / &
                                (log(p_phy(i,k,j)) - log(p8w(i,k,j)) )
                      tropo_z(i,j)= zmid(i,k,j) + (log(tropo_p(i,j)) - log(p_phy(i,k,j))) * dZdlogP
                   end if
                end if
                !----------------------------------------------------------      
             end if
          end if
       end do
       end do
    end if

    if (any(tropo_lev(its:iend,jts:jend) == NOTFOUND)) then       
       write(message,*) 'tropopause_climate: Warning: some tropopause levels still NOTFOUND' 
    else        
       write(message,*) 'tropopause_climate: Warning: Done finding tropopause'
    end if
    call wrf_message( trim(message) )
  
  end subroutine tropopause_climate
 
!===========================================================================

      subroutine get_time_interp_factors(current_date_char , last, next, del_time )
!-----------------------------------------------------------------------------
! ... setup the time interpolation
!-----------------------------------------------------------------------------

      use module_date_time, only : get_julgmt

      implicit none

!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      CHARACTER (LEN=256),intent(in) :: current_date_char

      integer, intent(out) :: next, last
      real,    intent(out) :: del_time
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------

      integer, parameter :: day_of_year(12) = (/ 16, 45, 75, 105, 136, 166, 197, &
                                                 228, 258, 289, 319, 350        /)
      integer :: julyr , julday 
      real    :: gmt
      real    :: calday

      integer  :: m
!---------------------------------------------------------------------------------

      call get_julgmt ( current_date_char, julyr, julday, gmt )

      calday = real(julday) + gmt

      if( calday < day_of_year(1) ) then
         next = 1
         last = 12
         del_time = (365. + calday - day_of_year(12)) &
                / (365. + day_of_year(1) - day_of_year(12))
      else if( calday >= day_of_year(12) ) then
         next = 1
         last = 12
         del_time = (calday - day_of_year(12)) &
                / (365. + day_of_year(1) - day_of_year(12))
      else
         do m = 11,1,-1
            if( calday >= day_of_year(m) ) then
               exit
            end if
         end do
         last = m
         next = m + 1
         del_time = (calday - day_of_year(m)) / (day_of_year(m+1) - day_of_year(m))
      end if

      del_time = max( min( 1.,del_time ),0. )

      end subroutine get_time_interp_factors

!===========================================================================
      end module module_tropopause

