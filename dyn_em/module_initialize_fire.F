!IDEAL:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains.  

!  This MODULE CONTAINS the following routines:

!  initialize_field_test - 1. Set different fields to different constant
!                             values.  This is only a test.  If the correct
!                             domain is not found (based upon the "id")
!                             then a fatal error is issued.               

!-----------------------------------------------------------------------

MODULE module_initialize_ideal

   USE module_domain
   USE module_io_domain
   USE module_state_description
   USE module_model_constants
   USE module_bc
   USE module_timing
   USE module_configure
   USE module_init_utilities
   USE module_soil_pre        !AK/ak for full surface initialization
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_fr_fire_util, ONLY: continue_at_boundary,crash,read_array_2d_real, &
     read_array_2d_integer,interpolate_2d,set_ideal_coord 

CONTAINS


!-------------------------------------------------------------------
! this is a wrapper for the solver-specific init_domain routines.
! Also dereferences the grid variables and passes them down as arguments.
! This is crucial, since the lower level routines may do message passing
! and this will get fouled up on machines that insist on passing down
! copies of assumed-shape arrays (by passing down as arguments, the 
! data are treated as assumed-size -- ie. f77 -- arrays and the copying
! business is avoided).  Fie on the F90 designers.  Fie and a pox.

   SUBROUTINE init_domain ( grid )

   IMPLICIT NONE

   !  Input data.
   TYPE (domain), POINTER :: grid 
   !  Local data.
   INTEGER :: idum1, idum2

   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

     CALL init_domain_rk( grid &
!
#include "actual_new_args.inc"
!
                        )

   END SUBROUTINE init_domain

!-------------------------------------------------------------------

   SUBROUTINE init_domain_rk ( grid &
!
# include "dummy_new_args.inc"
!
)
   IMPLICIT NONE

   !  Input data.
   TYPE (domain), POINTER :: grid

# include "dummy_new_decl.inc"

   TYPE (grid_config_rec_type)              :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte, &
                                  i, j, k

   INTEGER, PARAMETER :: nl_max = 1000
   REAL, DIMENSION(nl_max) :: zk, p_in, theta, rho, u, v, qv, pd_in
   INTEGER :: nl_in


   INTEGER :: icm,jcm, ii, im1, jj, jm1, loop, error, fid, nxc, nyc
   REAL    :: u_mean,v_mean, f0, p_surf, p_level, qvf, z_at_v, z_at_u
   REAL    :: z_scale, xrad, yrad, zrad, rad, delt, cof1, cof2
   REAL    :: x_rad, y_rad, z_rad, hght_pert   !Ak/ak
   character (len=256) :: mminlu2              !AK/ak land use scheme (USGS)
!   REAL, EXTERNAL :: interp_0
   REAL    :: hm
   REAL    :: pi

!  stuff from original initialization that has been dropped from the Registry 
   REAL    :: vnu, xnu, xnus, dinit0, cbh, p0_temp, t0_temp, zd, zt
   REAL    :: qvf1, qvf2, pd_surf
   INTEGER :: it
   real :: thtmp, ptmp, temp(3)

   LOGICAL :: moisture_init
   LOGICAL :: stretch_grd, dry_sounding
   LOGICAL :: stretch_hyp, sfc_init           !AK/ak switches for hyperbolic grid streching and surface initialization

   INTEGER :: xs , xe , ys , ye
   INTEGER :: mtn_type
   INTEGER :: &  ! fire mesh sizes
              iots,iote,jots,jote, &            ! tile dims out
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifts,ifte, kfts,kfte, jfts,jfte

   REAL :: mtn_ht, mtn_max, mtn_x, mtn_y, mtn_z, grad_max
   REAL :: mtn_axs, mtn_ays, mtn_axe, mtn_aye
   REAL :: mtn_fxs, mtn_fys, mtn_fxe, mtn_fye
   REAL :: mtn_xs, mtn_ys, mtn_xe, mtn_ye
   REAL :: fdx,fdy ! fire mesh step
   INTEGER:: ir,jr ! refinement factors

   logical have_fire_ht,have_fire_grad,have_atm_grad

!*** executable

   SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
   kds = grid%sd31 ; kde = grid%ed31 ;
   ids = grid%sd32 ; ide = grid%ed32 ;
   jds = grid%sd33 ; jde = grid%ed33 ;

   kms = grid%sm31 ; kme = grid%em31 ;
   ims = grid%sm32 ; ime = grid%em32 ;
   jms = grid%sm33 ; jme = grid%em33 ;

   kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
   its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
   jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
         CASE ( DATA_ORDER_XYZ )
   ids = grid%sd31 ; ide = grid%ed31 ;
   jds = grid%sd32 ; jde = grid%ed32 ;
   kds = grid%sd33 ; kde = grid%ed33 ;

   ims = grid%sm31 ; ime = grid%em31 ;
   jms = grid%sm32 ; jme = grid%em32 ;
   kms = grid%sm33 ; kme = grid%em33 ;

   its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
   jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
   kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch
         CASE ( DATA_ORDER_XZY )
   ids = grid%sd31 ; ide = grid%ed31 ;
   kds = grid%sd32 ; kde = grid%ed32 ;
   jds = grid%sd33 ; jde = grid%ed33 ;

   ims = grid%sm31 ; ime = grid%em31 ;
   kms = grid%sm32 ; kme = grid%em32 ;
   jms = grid%sm33 ; jme = grid%em33 ;

   its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
   kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
   jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

   END SELECT

!   z_scale = .40
   pi = 2.*asin(1.0)
   write(6,*) ' pi is ',pi
   nxc = (ide-ids)/2
   nyc = (jde-jds)/2

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )
 
    delt = config_flags%delt_perturbation
    x_rad = config_flags%xrad_perturbation
    y_rad = config_flags%yrad_perturbation
    z_rad = config_flags%zrad_perturbation
    hght_pert = config_flags%hght_perturbation

    stretch_grd = config_flags%stretch_grd   
    stretch_hyp = config_flags%stretch_hyp
        z_scale = config_flags%z_grd_scale
       sfc_init = config_flags%sfc_full_init
  
 moisture_init = .true.   !AK/ak

    grid%itimestep=0

#ifdef DM_PARALLEL
   CALL wrf_dm_bcast_bytes( icm , IWORDSIZE )
   CALL wrf_dm_bcast_bytes( jcm , IWORDSIZE )
#endif

!AK/ak land use initialization (USGS) 
   IF (sfc_init) THEN  
    mminlu2=' '
    mminlu2(1:4) = 'USGS'                !Ak/ak
    CALL nl_set_mminlu(1, mminlu2)       !Ak/ak
    CALL nl_get_iswater(1,grid%iswater) ! Ak/ak
   ENDIF

    CALL nl_set_iswater(1,0)
    CALL nl_set_cen_lat(1,40.)
    CALL nl_set_cen_lon(1,-105.)
    CALL nl_set_truelat1(1,0.)
    CALL nl_set_truelat2(1,0.)
    CALL nl_set_moad_cen_lat (1,0.)
    CALL nl_set_stand_lon (1,0.)
    CALL nl_set_pole_lon (1,0.)
    CALL nl_set_pole_lat (1,90.)
    CALL nl_set_map_proj(1,0)

!  here we initialize data we currently is not initialized 
!  in the input data

    DO j = jts, jte
      DO i = its, ite
         grid%msftx(i,j)    = 1.
         grid%msfty(i,j)    = 1.
         grid%msfux(i,j)    = 1.
         grid%msfuy(i,j)    = 1.
         grid%msfvx(i,j)    = 1.
         grid%msfvx_inv(i,j)= 1.
         grid%msfvy(i,j)    = 1.
         grid%sina(i,j)     = 0.
         grid%cosa(i,j)     = 1.
         grid%e(i,j)        = 0.
         grid%f(i,j)        = 0.
      END DO
    END DO     

!AK/ak surface initialization latitude, longitude, landuse index from from LANDUSE.TBL skin temperature and soil temperature
write(6,*) '*************************************'
   IF (sfc_init) THEN
    DO j = jts, jte
      DO i = its, ite
          grid%xlat(i,j) = config_flags%fire_lat_init     !Ak/sk (35)
         grid%xlong(i,j) = config_flags%fire_lon_init     !Ak/ak (-111)
         grid%xland(i,j) = 1.                             !Ak/ak
      grid%lu_index(i,j) = config_flags%sfc_lu_index      !AK/ak land use index (28)
           grid%tsk(i,j) = config_flags%sfc_tsk           !AK/ak  surface skin temperature [K] (285)
           grid%tmn(i,j) = config_flags%sfc_tmn           !AK/ak  soil temperature at lower boundary [K] (285)
      END DO
    END DO
      ! read land use data from files, overwriting the constant 
      if(config_flags%fire_read_lu) &
          call read_array_2d_real('input_lu',grid%lu_index,ids,ide,jds,jde,ims,ime,jms,jme) 
      if(config_flags%fire_read_tsk) &
          call read_array_2d_real   ('input_tsk',grid%tsk,    ids,ide,jds,jde,ims,ime,jms,jme) 
      if(config_flags%fire_read_tmn) &
          call read_array_2d_real   ('input_tmn',grid%tmn,    ids,ide,jds,jde,ims,ime,jms,jme) 

! for Noah LSM, additional variables need to be initializedi  !AK/ak |

  other_masked_fields : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

      CASE (SLABSCHEME)
      write(6,*) ' SLAB surface scheme activated'

      CASE (LSMSCHEME)
      write(6,*) ' Noah unified LSM scheme activated with:'
      write(6,*) '    vegetation fraction=',config_flags%sfc_vegfra
      write(6,*) '          canopy  water=',config_flags%sfc_canwat
      write(6,*) '     dominant veg. type=',config_flags%sfc_ivgtyp
      write(6,*) '     dominant soil type=',config_flags%sfc_isltyp

        DO j = jts , MIN(jde-1,jte)
           DO i = its , MIN(ide-1,ite)
               grid%vegfra(i,j) = config_flags%sfc_vegfra  !0.5
               grid%canwat(i,j) = config_flags%sfc_canwat  !0.
               grid%ivgtyp(i,j) = config_flags%sfc_ivgtyp  !18
               grid%isltyp(i,j) = config_flags%sfc_isltyp  !7
               grid%xice(i,j) = 0.
               grid%snow(i,j) = 0.
           END DO
        END DO

      CASE (RUCLSMSCHEME)
       write(6,*) ' RUS surface scheme activated'
    END SELECT other_masked_fields                         !AK/ak |

    ENDIF

    DO j = jts, jte
    DO k = kts, kte
      DO i = its, ite
         grid%ww(i,k,j)     = 0.
      END DO
   END DO
   END DO

   grid%step_number = 0

   IF (sfc_init) THEN

      write(6,*) ' full surface initialization activated '
      ! write(6,*) ' land use index =', config_flags%sfc_lu_index
      ! write(6,*) ' skin temperature=',grid%tsk(10,10),&
      !         '[K] soil temperature=', grid%tmn(10,10),'[K]'    
! Process the soil; note that there are some things hard-wired into share/module_soil_pre.F
      CALL process_soil_ideal(grid%xland,grid%xice,grid%vegfra,grid%snow,grid%canwat, &
                     grid%ivgtyp,grid%isltyp,grid%tslb,grid%smois, &
                     grid%tsk,grid%tmn,grid%zs,grid%dzs,model_config_rec%num_soil_layers, &
                     model_config_rec%sf_surface_physics(grid%id), &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )


     ELSE
      write(6,*) 'full surface initialization is turned off!! '
    ENDIF    !end of surface initialization

! set up the grid
   write(6,*) '*************************************'

   IF (stretch_grd) THEN ! exponential or hyperbolic tangential stretch for eta

    IF (stretch_hyp) THEN ! hyperbolic tangential stretch (more levels at the surface)
     write(6,*) ' hyperbolic tangential stretching activated with z_scale =',z_scale
     DO k=1, kde          
      grid%znw(k) = -1.* (tanh(z_scale*(float(k-1) / float(kde-1) -1.)))/ &
                                 (tanh(z_scale))
     ENDDO
    ELSE                 ! exponential stretch for eta (nearly constant dz)
    write(6,*) ' exponential grid stretching activated with z_scale =',z_scale
     DO k=1, kde 
     grid%znw(k) = (exp(-(k-1)/float(kde-1)/z_scale) - exp(-1./z_scale))/ &
                            (1.-exp(-1./z_scale))
     ENDDO
    ENDIF 
   ELSE
   write(6,*) ' no grid stretching'
     DO k=1, kde 
      grid%znw(k) = 1. - float(k-1)/float(kde-1)
     ENDDO
   ENDIF
   write(6,*) '*************************************'

   DO k=1, kde-1
    grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
    grid%rdnw(k) = 1./grid%dnw(k)
    grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
   ENDDO
   DO k=2, kde-1
    grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
    grid%rdn(k) = 1./grid%dn(k)
    grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
    grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
   ENDDO

   cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2) 
   cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3) 
   grid%cf1  = grid%fnp(2) + cof1
   grid%cf2  = grid%fnm(2) - cof1 - cof2
   grid%cf3  = cof2       

   grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
   grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)
   grid%rdx = 1./config_flags%dx
   grid%rdy = 1./config_flags%dy

!  get the sounding from the ascii sounding file, first get dry sounding and 
!  calculate base state

  dry_sounding = .true.
  IF ( wrf_dm_on_monitor() ) THEN
  write(6,*) ' getting dry sounding for base state '

  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )
  ENDIF
  CALL wrf_dm_bcast_real( zk , nl_max )
  CALL wrf_dm_bcast_real( p_in , nl_max )
  CALL wrf_dm_bcast_real( pd_in , nl_max )
  CALL wrf_dm_bcast_real( theta , nl_max )
  CALL wrf_dm_bcast_real( rho , nl_max )
  CALL wrf_dm_bcast_real( u , nl_max )
  CALL wrf_dm_bcast_real( v , nl_max )
  CALL wrf_dm_bcast_real( qv , nl_max )
  CALL wrf_dm_bcast_integer ( nl_in , 1 ) 

  write(6,*) ' returned from reading sounding, nl_in is ',nl_in

!  find ptop for the desired ztop (ztop is input from the namelist),
!  and find surface pressure

  grid%p_top = interp_0( p_in, zk, config_flags%ztop, nl_in )

! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
       ifds,ifde, jfds,jfde,kfds,kfde, & 
       ifms,ifme, jfms,jfme,kfms,kfme,  & 
       ifts,ifte, jfts,jfte,kfts,kfte)

  write (6,*)' ******** SFIRE ideal initialization ********'

  ! fire grid step size
  fdx = grid%dx/grid%sr_x
  fdy = grid%dy/grid%sr_y
  ! refinement ratios
  ir = grid%sr_x
  jr = grid%sr_y

  write (6,*)' atm  mesh step ',grid%dx,grid%dy
  write (6,*)' fire mesh step ',fdx,fdy
  write (6,*)' refinement ratio ',grid%sr_x,grid%sr_y
  write (6,*)' atm  domain bounds ',ids,ide, jds,jde,kds,kde
  write (6,*)' atm  memory bounds ',ims,ime, jms,jme,kms,kme
  write (6,*)' atm  tile   bounds ',its,ite, jts,jte,kts,kte
  write (6,*)' fire domain bounds ',ifds,ifde, jfds,jfde,kfds,kfde
  write (6,*)' fire memory bounds ',ifms,ifme, jfms,jfme,kfms,kfme
  write (6,*)' fire tile   bounds ',ifts,ifte, jfts,jfte,kfts,kfte
  write (6,*)' Note that atm mesh and fire mesh are cell-centered'

! set ideal coordinates
  call set_ideal_coord( fdx,fdy, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                grid%fxlong,grid%fxlat          )
  call set_ideal_coord( grid%dx,grid%dy, &
                ids,ide,jds,jde,  &
                ims,ime,jms,jme,  &
                its,ite,jts,jte,  &
                grid%xlong,grid%xlat          )

! set terrain height

  DO j=jts,jte
  DO i=its,ite
    grid%ht(i,j) = 0.
  ENDDO
  ENDDO

  if(config_flags%fire_fuel_read.eq.2) &
      call read_array_2d_real('input_fc',grid%nfuel_cat,ifds,ifde,jfds,jfde,ifms,ifme,jfms,jfme) 


  have_fire_grad=.false.
  have_atm_grad=.false.
  have_fire_ht=.false.

  !******* set terrain height

  ! copy params from the namelist
  mtn_type = config_flags%fire_mountain_type
  mtn_xs   = config_flags%fire_mountain_start_x
  mtn_ys   = config_flags%fire_mountain_start_y
  mtn_xe   = config_flags%fire_mountain_end_x
  mtn_ye   = config_flags%fire_mountain_end_y
  mtn_ht   = config_flags%fire_mountain_height

  IF(mtn_type .ne. 0)THEN

    ! idealized mountain

    ! atmospheric grid coordinates of the mountain
    mtn_axs = mtn_xs/grid%dx + ids - 0.5 
    mtn_axe = mtn_xe/grid%dx + ids - 0.5
    mtn_ays = mtn_ys/grid%dy + jds - 0.5
    mtn_aye = mtn_ye/grid%dy + jds - 0.5

    ! fire grid coordinates of the mountain
    mtn_fxs = mtn_xs/fdx + ifds - 0.5 
    mtn_fxe = mtn_xe/fdx + ifds - 0.5
    mtn_fys = mtn_ys/fdy + jfds - 0.5
    mtn_fye = mtn_ye/fdy + jfds - 0.5

    write(6,*)' Mountain height ',mtn_ht,' type',mtn_type
    write(6,*)' Mountain (m) LL=(0,0) ',mtn_xs,':',mtn_xe,' ',mtn_ys,':',mtn_ye
    write(6,*)' Mountain on atm grid  ',mtn_axs,':',mtn_axe,' ',mtn_ays,':',mtn_aye
    write(6,*)' Mountain on fire grid ',mtn_fxs,':',mtn_fxe,' ',mtn_fys,':',mtn_fye

    mtn_max = 0.
    DO j=jts,jte
    DO i=its,ite
      mtn_x = pi + 2*pi* max(0. , min( (i - mtn_axs)/(mtn_axe - mtn_axs), 1. ))
      mtn_y = pi + 2*pi* max(0. , min( (j - mtn_ays)/(mtn_aye - mtn_ays), 1. ))
      SELECT CASE(mtn_type)
        CASE (1) ! circ/elliptic mountain
          mtn_z = mtn_ht * 0.25 * (1. + COS(mtn_x))*(1. + COS(mtn_y))
        CASE (2)  ! EW ridge
          mtn_z = mtn_ht * 0.5 * (1. + COS(mtn_y))
        CASE (3)  ! NS ridge
          mtn_z = mtn_ht * 0.5 * (1. + COS(mtn_x))
        CASE DEFAULT
          call wrf_error_fatal ( ' bad fire_mountain_type ' )
      END SELECT
      mtn_max = max(mtn_max, mtn_z)
      grid%ht(i,j) = mtn_z
    ENDDO
    ENDDO

    write(6, *)' Atm  tile ',its,':',ite,' ',jts,':',jte,' max terrain height ',mtn_max

    DO j=jfts,jfte
        DO i=ifts,ifte
          mtn_x = pi + 2*pi* max(0. , min( (i - mtn_fxs)/(mtn_fxe - mtn_fxs), 1. ))
          mtn_y = pi + 2*pi* max(0. , min( (j - mtn_fys)/(mtn_fye - mtn_fys), 1. ))
          SELECT CASE(mtn_type)
            CASE (1) ! circ/elliptic mountain
              mtn_z = mtn_ht * 0.25 * (1. + COS(mtn_x))*(1. + COS(mtn_y))
            CASE (2)  ! EW ridge
              mtn_z = mtn_ht * 0.5 * (1. + COS(mtn_y))
            CASE (3)  ! NS ridge
              mtn_z = mtn_ht * 0.5 * (1. + COS(mtn_x))
            CASE DEFAULT
              call wrf_error_fatal ( ' bad fire_mountain_type ' )
          END SELECT
          grid%zsf(i,j) = mtn_z
        ENDDO
    ENDDO
 
    have_fire_ht=.true.

  ELSE ! mtn_type 

    if(config_flags%fire_read_atm_ht)then !
      call read_array_2d_real('input_ht',grid%ht,ids,ide,jds,jde,ims,ime,jms,jme) 
      ! no flag - we always have the terrain height on atm mesh, zero if not set
    endif

    if(config_flags%fire_read_fire_ht)then !
      call read_array_2d_real('input_zsf',grid%zsf,ifds,ifde,jfds,jfde,ifms,ifme,jfms,jfme) 
      have_fire_ht=.true.
    endif

    if(config_flags%fire_read_atm_grad)then !
      call crash('Reading terrain gradient on atm mesh from file not supported.')
      have_atm_grad=.true.
    endif

    if(config_flags%fire_read_fire_grad)then !
      call read_array_2d_real('input_dzdxf',grid%dzdxf,ifds,ifde,jfds,jfde,ifms,ifme,jfms,jfme) 
      call read_array_2d_real('input_dzdyf',grid%dzdyf,ifds,ifde,jfds,jfde,ifms,ifme,jfms,jfme) 
      have_fire_grad=.true.
    endif
  ENDIF  ! mtn_type

  if(have_fire_ht)then 
      write(6, *)'Fine-resolution terrain height on the fire mesh used.'
  else
      write(6,*)'Interpolating the terrain height from the atm mesh to the fire mesh'
      call interpolate_2d(  &
        ims,ime,jms,jme, & ! memory dims atm grid tile
        its,ite,jts,jte, & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts,ifte,jfts,jfte,  & ! dimensions fire grid tile
        ir,jr,                & ! refinement ratio
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
        grid%ht,                                      & ! atm grid arrays in
        grid%zsf)                                      ! fire grid arrays out
      have_fire_ht=.true.
  endif


  if(have_fire_grad)then 
     write(6, *)'Fine-resolution terrain gradient on the fire mesh used.'
  else

     write(6,*)'Computing the terrain gradient from fire mesh height'
     if(.not.have_fire_ht)then
        write(6,*)'WARNING: Fire mesh terrain height not given, setting to zero'
        do j=jfts,jfte
           do i=ifts,ifte
              grid%zsf(i,j) = 0.
           enddo
        enddo
      endif
   
      ! extend the terrain height one beyond the domain
      call continue_at_boundary(1,1,0., & ! do x direction or y direction
              ifms,ifme,jfms,jfme, &            ! memory dims
              ifds,ifde,jfds,jfde, &            ! domain dims
              ifds,ifde,jfds,jfde, &            ! patch dims = domain, not parallel!
              ifts,ifte,jfts,jfte, &            ! tile dims
              iots,iote,jots,jote, &            ! tile dims out
              grid%zsf)                         ! array
  
      ! compute the terrain gradient by differencing
      do j=jfts,jfte
         do i=ifts,ifte
            grid%dzdxf(i,j) = (grid%zsf(i+1,j)-grid%zsf(i-1,j))/(2.*fdx)
            grid%dzdyf(i,j) = (grid%zsf(i,j+1)-grid%zsf(i,j-1))/(2.*fdy)
         enddo
      enddo
      have_fire_grad=.true.
   endif ! have_fire_grad
   
   if(.not.have_fire_grad)call crash('Fire mesh terrain gradient not set')
 
    mtn_max = 0.
    DO j=jts,jte
      DO i=its,ite
        mtn_max = max(mtn_max, grid%ht(i,j))
      ENDDO
    ENDDO
    write(6, *)' Max terrain height on the atmosphere mesh ',mtn_max

    mtn_max = 0.
    grad_max =0.
    DO j=jfts,jfte
      DO i=ifts,ifte
        mtn_max = max(mtn_max, grid%zsf(i,j))
        grad_max = max( grad_max, sqrt(grid%dzdxf(i,j)**2+grid%dzdyf(i,j)**2) )
      ENDDO
    ENDDO
    write(6, *)' Max terrain height on the fire mesh       ',mtn_max
    write(6, *)' Max terrain gradient on the fire mesh     ',grad_max
  
! the rest of initialization dependent on the atmosphere grid terrain height set

  DO j=jts,jte
  DO i=its,ite
    grid%phb(i,1,j) = g * grid%ht(i,j)
    grid%ph0(i,1,j) = g * grid%ht(i,j)
  ENDDO
  ENDDO

  DO J = jts, jte
  DO I = its, ite

    p_surf = interp_0( p_in, zk, grid%phb(i,1,j)/g, nl_in )
    grid%mub(i,j) = p_surf-grid%p_top

!  this is dry hydrostatic sounding (base state), so given grid%p (coordinate),
!  interp theta (from interp) and compute 1/rho from eqn. of state

    DO K = 1, kte-1
      p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
      grid%pb(i,k,j) = p_level
      grid%t_init(i,k,j) = interp_0( theta, p_in, p_level, nl_in ) - t0
      grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
    ENDDO

!  calc hydrostatic balance (alternatively we could interp the geopotential from the
!  sounding, but this assures that the base state is in exact hydrostatic balance with
!  respect to the model eqns.

    DO k  = 2,kte
      grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
    ENDDO

  ENDDO
  ENDDO

  IF ( wrf_dm_on_monitor() ) THEN
    write(6,*) ' ptop is ',grid%p_top
    write(6,*) ' base state grid%mub(1,1), p_surf is ',grid%mub(1,1),grid%mub(1,1)+grid%p_top
  ENDIF

!  calculate full state for each column - this includes moisture.

  write(6,*) ' getting moist sounding for full state '
  dry_sounding = .false.
  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)

!  At this point grid%p_top is already set. find the DRY mass in the column 
!  by interpolating the DRY pressure.  

   pd_surf = interp_0( pd_in, zk, grid%phb(i,1,j)/g, nl_in )

!  compute the perturbation mass and the full mass

    grid%mu_1(i,j) = pd_surf-grid%p_top - grid%mub(i,j)
    grid%mu_2(i,j) = grid%mu_1(i,j)
    grid%mu0(i,j) = grid%mu_1(i,j) + grid%mub(i,j)

! given the dry pressure and coordinate system, interp the potential
! temperature and qv

    do k=1,kde-1

      p_level = grid%znu(k)*(pd_surf - grid%p_top) + grid%p_top

      moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
      grid%t_1(i,k,j)          = interp_0( theta, pd_in, p_level, nl_in ) - t0
      grid%t_2(i,k,j)          = grid%t_1(i,k,j)
      

    enddo

!  integrate the hydrostatic equation (from the RHS of the bigstep
!  vertical momentum equation) down from the top to get grid%p.
!  first from the top of the model to the top pressure

    k = kte-1  ! top level

    qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
    qvf2 = 1./(1.+qvf1)
    qvf1 = qvf1*qvf2

!    grid%p(i,k,j) = - 0.5*grid%mu_1(i,j)/grid%rdnw(k)
    grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
    qvf = 1. + rvovrd*moist(i,k,j,P_QV)
    grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
    grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)

!  down the column

    do k=kte-2,1,-1
      qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
      qvf2 = 1./(1.+qvf1)
      qvf1 = qvf1*qvf2
      grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_1(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
      qvf = 1. + rvovrd*moist(i,k,j,P_QV)
      grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                  (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
      grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
    enddo

!  this is the hydrostatic equation used in the model after the
!  small timesteps.  In the model, grid%al (inverse density)
!  is computed from the geopotential.


    grid%ph_1(i,1,j) = 0.
    DO k  = 2,kte
      grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (grid%dnw(k-1))*(       &
                   (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j)+ &
                    grid%mu_1(i,j)*grid%alb(i,k-1,j)  )
                                                   
      grid%ph_2(i,k,j) = grid%ph_1(i,k,j) 
      grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
    ENDDO

    IF ( wrf_dm_on_monitor() ) THEN
    if((i==2) .and. (j==2)) then
     write(6,*) ' grid%ph_1 calc ',grid%ph_1(2,1,2),grid%ph_1(2,2,2),&
                              grid%mu_1(2,2)+grid%mub(2,2),grid%mu_1(2,2), &
                              grid%alb(2,1,2),grid%al(1,2,1),grid%rdnw(1)
    endif
    ENDIF

  ENDDO
  ENDDO

! checking if the perturbation (bubble) is to be applied
  IF ((delt/=0.) .and. (x_rad > 0.) &
                 .and. (y_rad > 0.) &
                 .and. (z_rad > 0.)) THEN
!  thermal perturbation to kick off convection

  write(6,*) ' nxc, nyc for perturbation ',nxc,nyc
  write(6,'(A23,f18.16)') ' delt for perturbation ',delt
  write(6,'(A30,f18.12)') ' x radius of the perturbation ' ,x_rad
  write(6,'(A30,f18.12)') ' y radius of the perturbation ' ,y_rad
  write(6,'(A30,f18.12)') ' z radius of the perturbation ' ,z_rad
  write(6,'(A30,f18.12)') ' height of the perturbation   ' ,hght_pert  

  DO J = jts, min(jde-1,jte)
    yrad = config_flags%dy*float(j-nyc)/y_rad
!   yrad = 0.
    DO I = its, min(ide-1,ite)
      xrad = config_flags%dx*float(i-nxc)/x_rad
!     xrad = 0.
      DO K = 1, kte-1

!  put in preturbation theta (bubble) and recalc density.  note,
!  the mass in the column is not changing, so when theta changes,
!  we recompute density and geopotential

        zrad = 0.5*(grid%ph_1(i,k,j)+grid%ph_1(i,k+1,j)  &
                   +grid%phb(i,k,j)+grid%phb(i,k+1,j))/g
        zrad = (zrad-hght_pert)/z_rad
        RAD=SQRT(xrad*xrad+yrad*yrad+zrad*zrad)
        IF(RAD <= 1.) THEN
           grid%t_1(i,k,j)=grid%t_1(i,k,j)+delt*COS(.5*PI*RAD)**2
           grid%t_2(i,k,j)=grid%t_1(i,k,j)
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                        (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
           grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
        ENDIF
      ENDDO
   
!  rebalance hydrostatically

      DO k  = 2,kte
        grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (grid%dnw(k-1))*(       &
                     (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j)+ &
                      grid%mu_1(i,j)*grid%alb(i,k-1,j)  )
                                                   
        grid%ph_2(i,k,j) = grid%ph_1(i,k,j) 
        grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
      ENDDO

    ENDDO
  ENDDO

!End of setting up the perturbation (bubble)
ENDIF

   IF ( wrf_dm_on_monitor() ) THEN
   write(6,*) ' grid%mu_1 from comp ', grid%mu_1(1,1)
   write(6,*) ' full state sounding from comp, ph, grid%p, grid%al, grid%t_1, qv '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1)+grid%phb(1,k,1), &
                                      grid%p(1,k,1)+grid%pb(1,k,1), grid%alt(1,k,1), &
                                      grid%t_1(1,k,1)+t0, moist(1,k,1,P_QV)
   enddo

   write(6,*) ' pert state sounding from comp, grid%ph_1, pp, alp, grid%t_1, qv '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1), &
                                      grid%p(1,k,1), grid%al(1,k,1), &
                                      grid%t_1(1,k,1), moist(1,k,1,P_QV)
   enddo
   ENDIF

! interp v

  DO J = jts, jte
  DO I = its, min(ide-1,ite)

    IF (j == jds) THEN
      z_at_v = grid%phb(i,1,j)/g
    ELSE IF (j == jde) THEN
      z_at_v = grid%phb(i,1,j-1)/g
    ELSE
      z_at_v = 0.5*(grid%phb(i,1,j)+grid%phb(i,1,j-1))/g
    END IF
    p_surf = interp_0( p_in, zk, z_at_v, nl_in )

    DO K = 1, kte-1
      p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
      grid%v_1(i,k,j) = interp_0( v, p_in, p_level, nl_in )
      grid%v_2(i,k,j) = grid%v_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

! interp u

  DO J = jts, min(jde-1,jte)
  DO I = its, ite

    IF (i == ids) THEN
      z_at_u = grid%phb(i,1,j)/g
    ELSE IF (i == ide) THEN
      z_at_u = grid%phb(i-1,1,j)/g
    ELSE
      z_at_u = 0.5*(grid%phb(i,1,j)+grid%phb(i-1,1,j))/g
    END IF

    p_surf = interp_0( p_in, zk, z_at_u, nl_in )

    DO K = 1, kte-1
      p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
      grid%u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
      grid%u_2(i,k,j) = grid%u_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

!  set w

  DO J = jts, min(jde-1,jte)
  DO K = kts, kte
  DO I = its, min(ide-1,ite)
    grid%w_1(i,k,j) = 0.
    grid%w_2(i,k,j) = 0.
  ENDDO
  ENDDO
  ENDDO

!  set a few more things

  DO J = jts, min(jde-1,jte)
  DO K = kts, kte-1
  DO I = its, min(ide-1,ite)
    grid%h_diabatic(i,k,j) = 0.
  ENDDO
  ENDDO
  ENDDO

  IF ( wrf_dm_on_monitor() ) THEN
  DO k=1,kte-1
    grid%t_base(k) = grid%t_1(1,k,1)
    grid%qv_base(k) = moist(1,k,1,P_QV)
    grid%u_base(k) = grid%u_1(1,k,1)
    grid%v_base(k) = grid%v_1(1,k,1)
    grid%z_base(k) = 0.5*(grid%phb(1,k,1)+grid%phb(1,k+1,1)+grid%ph_1(1,k,1)+grid%ph_1(1,k+1,1))/g
  ENDDO
  ENDIF
  CALL wrf_dm_bcast_real( grid%t_base , kte )
  CALL wrf_dm_bcast_real( grid%qv_base , kte )
  CALL wrf_dm_bcast_real( grid%u_base , kte )
  CALL wrf_dm_bcast_real( grid%v_base , kte )
  CALL wrf_dm_bcast_real( grid%z_base , kte )

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)
     thtmp   = grid%t_2(i,1,j)+t0
     ptmp    = grid%p(i,1,j)+grid%pb(i,1,j)
     temp(1) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = grid%t_2(i,2,j)+t0
     ptmp    = grid%p(i,2,j)+grid%pb(i,2,j)
     temp(2) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = grid%t_2(i,3,j)+t0
     ptmp    = grid%p(i,3,j)+grid%pb(i,3,j)
     temp(3) = thtmp * (ptmp/p1000mb)**rcp

!     grid%tsk(I,J)=grid%cf1*temp(1)+grid%cf2*temp(2)+grid%cf3*temp(3) !AK/AK it is already declared via namelist.input if sfc_init=.true.
!     grid%tmn(I,J)=grid%tsk(I,J)-0.5                                  !AK/AK it is already declared via namelist.input if sfc_init=.true.
  ENDDO
  ENDDO

  IF (.NOT.sfc_init) THEN
  write(6,*) ' setting tsk and tmn default'
    DO J = jts, min(jde-1,jte)
    DO I = its, min(ide-1,ite)
     grid%tsk(I,J)=grid%cf1*temp(1)+grid%cf2*temp(2)+grid%cf3*temp(3)
     grid%tmn(I,J)=grid%tsk(I,J)-0.5
   ENDDO
   ENDDO
 ENDIF

 END SUBROUTINE init_domain_rk

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

!  test driver for get_sounding
!
!      implicit none
!      integer n
!      parameter(n = 1000)
!      real zk(n),p(n),theta(n),rho(n),u(n),v(n),qv(n),pd(n)
!      logical dry
!      integer nl,k
!
!      dry = .false.
!      dry = .true.
!      call get_sounding( zk, p, pd, theta, rho, u, v, qv, dry, n, nl )
!      write(6,*) ' input levels ',nl
!      write(6,*) ' sounding '
!      write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
!      do k=1,nl
!        write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), pd(k), theta(k), rho(k), u(k), v(k), qv(k)
!      enddo
!      end
!
!---------------------------------------------------------------------------

      subroutine get_sounding( zk, p, p_dry, theta, rho, &
                               u, v, qv, dry, nl_max, nl_in )
      implicit none

      integer nl_max, nl_in
      real zk(nl_max), p(nl_max), theta(nl_max), rho(nl_max), &
           u(nl_max), v(nl_max), qv(nl_max), p_dry(nl_max)
      logical dry

      integer n
      parameter(n=1000)
      logical debug
      parameter( debug = .true.)

! input sounding data

      real p_surf, th_surf, qv_surf
      real pi_surf, pi(n)
      real h_input(n), th_input(n), qv_input(n), u_input(n), v_input(n)

! diagnostics

      real rho_surf, p_input(n), rho_input(n)
      real pm_input(n)  !  this are for full moist sounding

! local data

      real r
      parameter (r = r_d)
      integer k, it, nl
      real qvf, qvf1, dz

!  first, read the sounding

      call read_sounding( p_surf, th_surf, qv_surf, &
                          h_input, th_input, qv_input, u_input, v_input,n, nl, debug )

      if(dry) then
       do k=1,nl
         qv_input(k) = 0.
       enddo
      endif

      if(debug) write(6,*) ' number of input levels = ',nl

        nl_in = nl
        if(nl_in .gt. nl_max ) then
          write(6,*) ' too many levels for input arrays ',nl_in,nl_max
          call wrf_error_fatal ( ' too many levels for input arrays ' )
        end if

!  compute diagnostics,
!  first, convert qv(g/kg) to qv(g/g)

      do k=1,nl
        qv_input(k) = 0.001*qv_input(k)
      enddo

      p_surf = 100.*p_surf  ! convert to pascals
      qvf = 1. + rvovrd*qv_input(1) 
      rho_surf = 1./((r/p1000mb)*th_surf*qvf*((p_surf/p1000mb)**cvpm))
      pi_surf = (p_surf/p1000mb)**(r/cp)

      if(debug) then
        write(6,*) ' surface density is ',rho_surf
        write(6,*) ' surface pi is      ',pi_surf
      end if


!  integrate moist sounding hydrostatically, starting from the
!  specified surface pressure
!  -> first, integrate from surface to lowest level

          qvf = 1. + rvovrd*qv_input(1) 
          qvf1 = 1. + qv_input(1)
          rho_input(1) = rho_surf
          dz = h_input(1)
          do it=1,10
            pm_input(1) = p_surf &
                    - 0.5*dz*(rho_surf+rho_input(1))*g*qvf1
            rho_input(1) = 1./((r/p1000mb)*th_input(1)*qvf*((pm_input(1)/p1000mb)**cvpm))
          enddo

! integrate up the column

          do k=2,nl
            rho_input(k) = rho_input(k-1)
            dz = h_input(k)-h_input(k-1)
            qvf1 = 0.5*(2.+(qv_input(k-1)+qv_input(k)))
            qvf = 1. + rvovrd*qv_input(k)   ! qv is in g/kg here
 
            do it=1,10
              pm_input(k) = pm_input(k-1) &
                      - 0.5*dz*(rho_input(k)+rho_input(k-1))*g*qvf1
              rho_input(k) = 1./((r/p1000mb)*th_input(k)*qvf*((pm_input(k)/p1000mb)**cvpm))
            enddo
          enddo

!  we have the moist sounding

!  next, compute the dry sounding using p at the highest level from the
!  moist sounding and integrating down.

        p_input(nl) = pm_input(nl)

          do k=nl-1,1,-1
            dz = h_input(k+1)-h_input(k)
            p_input(k) = p_input(k+1) + 0.5*dz*(rho_input(k)+rho_input(k+1))*g
          enddo


        do k=1,nl

          zk(k) = h_input(k)
          p(k) = pm_input(k)
          p_dry(k) = p_input(k)
          theta(k) = th_input(k)
          rho(k) = rho_input(k)
          u(k) = u_input(k)
          v(k) = v_input(k)
          qv(k) = qv_input(k)

        enddo

     if(debug) then
      write(6,*) ' sounding '
      write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
      do k=1,nl
        write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), p_dry(k), theta(k), rho(k), u(k), v(k), qv(k)
      enddo

     end if

      end subroutine get_sounding

!-------------------------------------------------------

      subroutine read_sounding( ps,ts,qvs,h,th,qv,u,v,n,nl,debug )
      implicit none
      integer n,nl
      real ps,ts,qvs,h(n),th(n),qv(n),u(n),v(n)
      logical end_of_file
      logical debug

      integer k

      open(unit=10,file='input_sounding',form='formatted',status='old')
      rewind(10)
      read(10,*) ps, ts, qvs
      if(debug) then
        write(6,*) ' input sounding surface parameters '
        write(6,*) ' surface pressure (mb) ',ps
        write(6,*) ' surface pot. temp (K) ',ts
        write(6,*) ' surface mixing ratio (g/kg) ',qvs
      end if

      end_of_file = .false.
      k = 0

      do while (.not. end_of_file)

        read(10,*,end=100) h(k+1), th(k+1), qv(k+1), u(k+1), v(k+1)
        k = k+1
        if(debug) write(6,'(1x,i3,5(1x,e10.3))') k, h(k), th(k), qv(k), u(k), v(k)
        go to 110
 100    end_of_file = .true.
 110    continue
      enddo

      nl = k

      close(unit=10,status = 'keep')

      end subroutine read_sounding

END MODULE module_initialize_ideal
