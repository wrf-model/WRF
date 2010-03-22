! WRF:MEDIATION_LAYER:FIRE_MODEL

!*** Jan Mandel August 2007 - February 2008
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu

! For support please subscribe to the wrf-fire mailing list at NCAR at
! http://mailman.ucar.edu/mailman/listinfo/wrf-fire
! or go to http://www.openwfm.org/wiki/index.php5/Talk:WRF-Fire

! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This model is described in [1]. The fire model is coupled with WRF 
! but the fire code itself is not dependent on WRF in any way other 
! than calls to few WRF utilities from module_fr_sfire_util. This 
! model uses a level set function method for advancing the fireline. 
! It is a reimplementation of an earlier model, which used fireline 
! propagation by tracers and was coupled with the Clark-Hall 
! atmospheric code, described in [2]. For WRF documentation see [3].

! If you use this code, please acknowledge our work by citing [1].
! Thank you.

! Acknowledgements
!
! Contributions to development of the level set 
! method by Mijeong Kim. Contribution to fuel calculation by Volodymyr
! Kondratenko.
!
! The fire physics is adapted from an earlier code by Terry  
! L. Clark, Janice L. Coen, and Don Latham. The coupling with WRF is 
! adapted from a code by Ned Patton for coupling of the earlier fire
! model with WRF. The changes in WRF infrastructure and support of
! refined fire grid was provided by John Michalakes.

! Jonathan D. Beezley has set up and maintained the WRF build and
! execution environment, provided software engineering infrastructure 
! including synchronization with the WRF repository, and was responsibe
! for all aspects of WRF modification.

! Refefences
!
! [1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, and Minjeong Kim,
! Data Asimilation for Wildland Fires: Ensemble Kalman filters in 
! coupled atmosphere-surface models, IEEE Control Systems Magazine 29,
! Issue 3, June 2009, 47-65. 

! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49-64, 
! 2004
!
! [3] http://www.mmm.ucar.edu/wrf/OnLineTutorial/Introduction/index.html

#define DEBUG_OUT

module module_fr_sfire_driver

use module_model_constants, only: cp,xlv,reradius,pi2
use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_atm
use module_fr_sfire_util

private
public :: sfire_driver_em_init, sfire_driver_em_step


contains

subroutine sfire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe)

    ! stub to call sfire_driver_em with irun=0 and omit last 3 args

    USE module_domain
    USE module_configure
    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe

    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    ! dummies

    real,dimension(1,1,1)::rho,z_at_w,dz8w

    call message('sfire_driver_em_init: SFIRE initialization start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call sfire_driver_em ( grid , config_flags               & 
            ,1,2,0                        & ! ifun start, end, test steps
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

    call message('sfire_driver_em_init: SFIRE initialization complete')

end subroutine sfire_driver_em_init

!
!***
!

subroutine sfire_driver_em_step (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,z_at_w,dz8w ) 

    ! stub to call sfire_driver_em 

    USE module_domain
    USE module_configure
    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe

    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    ! dummies

    real,dimension(ims:ime, kms:kme, jms:jme)::rho,z_at_w,dz8w

    call message('sfire_driver_em_step: SFIRE step start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call sfire_driver_em ( grid , config_flags               & 
            ,3,6,fire_test_steps                                &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

    call message('sfire_driver_em_step: SFIRE step complete')

end subroutine sfire_driver_em_step

!
!***
!

subroutine sfire_driver_em ( grid , config_flags                    & 
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

!*** purpose: driver from grid structure

! Driver layer modules
    USE module_domain
    USE module_configure
    USE module_driver_constants
    USE module_machine
    USE module_tiles
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm   , ONLY : halo_fire_lfn_sub,halo_fire_longlat_sub,halo_fire_ht_sub   &
                                ,halo_fire_zsf_sub, halo_fire_fuel_sub,halo_fire_wind_a_sub &
                                ,halo_fire_wind_f_sub,halo_fire_tign_sub
#endif

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                             ! state 
    TYPE (grid_config_rec_type) , INTENT(IN)  :: config_flags ! namelist
    integer, intent(in)::     fire_ifun_start,fire_ifun_end,tsteps ! driver cycle controls
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde,                              &
             ims,ime, kms,kme, jms,jme,                              &
             ips,ipe, kps,kpe, jps,jpe,                              &
             ifds,ifde, jfds,jfde,                                   &
             ifms,ifme, jfms,jfme,                                   &
             ifps,ifpe, jfps,jfpe 
    real,intent(in),dimension(ims:ime, kms:kme, jms:jme)::rho,  &! air density  (kg/m^3) 
             z_at_w,dz8w                 ! ????????

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    REAL, DIMENSION(fire_max_ignitions)::  fire_ignition_start_x, &
        fire_ignition_start_y, &
        fire_ignition_end_x, &
        fire_ignition_end_y, &
        fire_ignition_time, &
        fire_ignition_radius
    integer::fire_ifun,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::need_lfn_update,restart
    !real, dimension(ifms:ifme, jfms:jfme)::uf,vf,lfn_out  
    ! uf vf only do not need to be in the state but we need halo on them
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real::lat_ctr,lon_ctr
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    real:: unit_fxlong ,unit_fxlat


!*** executable

    call print_id

! populate our structures from wrf
    ! get ignition data 
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_time,fire_num_ignitions)
    ! copy configuration flags
    call set_flags(config_flags)

    if(fire_ignition_longlat .eq.0)then
       ! ideal
       !  ignition is in m
       unit_fxlong=1.  
       unit_fxlat=1.
       ! will set fire mesh coordinates to uniform mesh below
    else
       ! real
       lat_ctr=config_flags%cen_lat
       lon_ctr=config_flags%cen_lon
       ! 1 degree in m (approximate OK)
       unit_fxlat=pi2/(360.*reradius)  ! earth circumference in m / 360 degrees
       unit_fxlong=cos(lat_ctr*pi2/360.)*unit_fxlat  ! latitude
    endif

    ! refinement r
    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart

    

    write(msg,'(a,i1,a,i1,a,i4)') &
       'sfire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' test steps',tsteps
    call message(msg)

    do istep=0,tsteps ! istep >0 is for testing only, exit after the first call
      itimestep = grid%itimestep + istep ! in the first call, do fire_test_steps steps of the fire model

      need_lfn_update=.false.
      do fire_ifun=fire_ifun_start,fire_ifun_end

        ! 1 = initialize run pass 1: interpolate height to zsf=terrain
        ! 2 = initialize run pass 2: set fuel data, terrain gradient
        ! 3 = initialize timestep: interpolate winds, check for ignition
        ! 4 = do one timestep 
        ! 5 = copy timestep output to input
        ! 6 = compute output fluxes

#ifdef DM_PARALLEL
        if(need_lfn_update)then
!           halo exchange on lfn width 2
#include "HALO_FIRE_LFN.inc"
        endif

        if(fire_ifun.eq.1)then
!       halo exchange on topography
#include "HALO_FIRE_LONGLAT.inc"
            if(fire_topo_from_atm.eq.1)then
#include "HALO_FIRE_HT.inc"
            endif 

        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"

        elseif(fire_ifun.eq.3)then
!           halo exchange on atm winds width 1 for interpolation
#include "HALO_FIRE_WIND_A.inc"

        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 2 for a 2-step RK method
#include "HALO_FIRE_WIND_F.inc"

        elseif(fire_ifun.eq.6)then
!           computing fuel_left needs ignition time from neighbors
#include "HALO_FIRE_TIGN.inc"

        endif
#endif
        ! need domain by 1 smaller, in last row.col winds are not set properly
        call sfire_driver_phys ( &
            fire_ifun,need_lfn_update,                  &
            ids,ide-1, kds,kde, jds,jde-1,                          &
            ims,ime, kms,kme, jms,jme,                          &
            ips,min(ipe,ide-1), kps,kpe, jps,min(jpe,jde-1),                          & 
            ifds,ifde-ir, jfds,jfde-jr,                    &
            ifms,ifme, jfms,jfme,                    &
            ifps,min(ifpe,ifde-ir), jfps,min(jfpe,jfde-jr),      &
            ir,jr,                                      & ! atm/fire grid ratio
            grid%num_tiles,                             & ! atm grid tiling
            grid%i_start,min(grid%i_end,ide-1),                    &
            grid%j_start,min(grid%j_end,jde-1),                    &                 
            itimestep,restart,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  ! in scalars
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            unit_fxlong,unit_fxlat,                           & ! coordinates of grid center
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            fire_num_ignitions,                                & 
            fire_ignition_longlat,      &
            fire_ignition_start_x,fire_ignition_start_y, & ! ignition - small arrays
            fire_ignition_end_x,fire_ignition_end_y,     &
            fire_ignition_radius,fire_ignition_time,     &
            grid%u_2,grid%v_2,grid%mut,rho,grid%ht,      & ! in arrays, on atm grid
            z_at_w,dz8w,                                  &
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location           
            grid%lfn,grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%fire_area,                               & ! redundant, for display, fire grid
            grid%uf,grid%vf,                              & ! in - one step only
            lfn_out,                                      & ! work - one timestep    
            grid%rthfrten,grid%rqvfrten,                & ! out arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%ros,                                   & ! rate of spread
            grid%fxlong,grid%fxlat,                           &       
            grid%nfuel_cat,                               & ! input, or internal for safekeeping
            grid%fuel_time,grid%zsf,                      & 
            grid%bbb,grid%betafl,grid%phiwc,grid%r_0,grid%fgip,grid%ischap&
        )

#ifdef DM_PARALLEL
            if(fire_ifun.eq.2)then
!               halo exchange on all fuel data width 2
#include "HALO_FIRE_FUEL.inc"
            endif
#endif

                

      enddo
    enddo
    if(tsteps>0)call crash('sfire_driver_em: test run of uncoupled fire model completed')

end subroutine sfire_driver_em

!
!*******************
!

! module_fr_sfire_driver%%sfire_driver
subroutine sfire_driver_phys (ifun,need_lfn_update,    &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    itimestep,restart,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_start_x,ignition_start_y,            & ! ignition - small arrays
    ignition_end_x,ignition_end_y,                &
    ignition_radius,                              &
    ignition_time,                                &
    u,v,mu,rho,zs,                                & ! in arrays, atm grid
    z_at_w,dz8w,                                  &
    xlong,xlat,                                   &
    lfn,tign,fuel_frac,                           & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    uf,vf,lfn_out,                                & ! fire wind velocities, out level set function    
    rthfrten,rqvfrten,                            & ! out arrays, atm grid
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,                                          &
    fxlong,fxlat,                                 & !  
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,zsf,                                & ! save constant internal data, fire grid
    bbb,betafl,phiwc,r_0,fgip,ischap&
    )
USE module_dm, only:wrf_dm_maxval

implicit none

!*** arguments

integer, intent(in)::ifun,                        &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe, kps,kpe, jps,jpe,                    & ! atm patch bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ir,jr,                                        & ! atm/fire grid refinement ratio
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                       ! -1=not at all, done outside 
                                                       ! 0=from nfuel_cat0
                                                       ! 1=from altitude
                                                       ! 2=from file
    nfuel_cat0,                                   & ! fuel category to initialize everything to
    num_tiles                                       ! number of tiles

logical, intent(in)::restart
    

logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of ground fire heat (m)
    fire_ext_crwn                                   !  extinction depth of crown fire heat (m)


integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
real, dimension(num_ignitions), intent(in):: &   
    ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius, & ! start, end, radius, time
    ignition_time                           !  of ignition lines
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v ! wind velocity (m/s) (node based, atm grid) 
real,intent(in),dimension(ims:ime,jms:jme)::mu          ! dry air mass (Pa)  pressure??  (cell based, atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::  zs ! terrain height  
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::rho, &  ! air density  (kg/m^3) (cell based, atm grid) 
                z_at_w,dz8w                         ! height of some sort??

real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat ! inout because of extension at bdry
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    uf,vf,lfn_out                                    ! fire wind velocities

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        ! fraction of each cell burning

real, intent(out), dimension(ims:ime, kms:kme, jms:jme):: &
    rthfrten,rqvfrten                              ! temperature and humidity tendencies (atm grid)

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    grnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  ! redundant arrays, for display only, fire grid
    fgrnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    fgrnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    fcanhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                      &   ! moisture flux from crown fire (W/m^2) 
    ros                                             ! fire rate of spread (m/s)

!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat ! fire mesh coordinates

real, intent(inout), dimension(ifms:ifme, jfms:jfme):: &
    fuel_time,zsf,                               &
    bbb,betafl,phiwc,r_0,fgip
integer, intent(inout), dimension(ifms:ifme, jfms:jfme):: ischap
    
!*** local
real :: dxf,dyf,time_start,latm
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,kpe1, &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex


!*** executable

! time - assume dt does not change
time_start = itimestep * dt

! fire mesh step
dxf=dx/ir
dyf=dy/jr



write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',dxf,dyf
call message(msg)
write(msg,7001)'atm domain      ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory      ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch       ','ips',ips,ipe,jps,jpe
call message(msg)                    
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'fire patch      ','ifps',ifps,ifpe,jfps,jfpe
call message(msg)                    

! check mesh dimensions
call check_fmesh(ids,ide,ifds,ifde,ir,'id')           ! check if atm and fire grids line up
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        ! check if atm patch fits in atm array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) ! check if fire patch fits in fire array
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        ! check if atm patch fits in atm domain
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) ! check if fire patch fits in fire domain

!$OMP SINGLE
! init rest of fuel tables with derived constants
if(ifun.eq.1) then
   call init_fuel_cats  ! common for all threads
endif
!$OMP END SINGLE


pid=0
if(itimestep.le.10.or.mod(itimestep,10).eq.0)pid=itimestep ! print 1-10 then every 10th
!pid=itimestep


! fake atm tile bounds
kts=kps
kte=kpe

! set up fire tiles & interpolate to fire grid
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id=0  ! do not print/write anything
    if(itimestep.le.10.or.mod(itimestep,10).eq.0)id=itimestep+ij*10000

    ignitions_done_tile(ij)=0

    ! set up tile bounds    
    its = i_start(ij)  ! start atmospheric tile in i
    ite = i_end(ij)    ! end atmospheric tile in i
    jts = j_start(ij)  ! start atmospheric tile in j
    jte = j_end(ij)    ! end atmospheric tile in j
    ifts= (its-ids)*ir+ifds       ! start fire tile in i
    ifte= (ite-ids+1)*ir+ifds-1   ! end fire tile in i
    jfts= (jts-jds)*jr+jfds       ! start fire tile in j
    jfte= (jte-jds+1)*jr+jfds-1   ! end fire tile in j
        
    write(msg,*)'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory


    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr

    if(ifun.eq.1)then   ! set terrain

      if(restart)then
          
          call message('restart - topo initialization skipped')

      else

        call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'driver:zs')
    
        ! interpolate terrain height
        
        if(fire_topo_from_atm.eq.1)then
            call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
                ids,ide,  jds,jde,                    & ! atm grid dimensions
                ims,ime,  jms,jme,                    &
                ips,ipe,jps,jpe,                              &
                its,ite,jts,jte,                              &
                ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
                ifms, ifme, jfms, jfme,                       &
                ifts,ifte,jfts,jfte,                          &
                ir,jr,                                        & ! atm/fire grid ratio
                zs,                                       & ! atm grid arrays in
                zsf)                                      ! fire grid arrays out
        else
           write(msg,'(a,i3,a)')'fire_topo_from_atm=',fire_topo_from_atm,' assuming ZSF set, interpolation skipped'
        endif

        if(ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates - used for ignition only
            ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop
            call set_ideal_coord( dxf,dyf, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                fxlong,fxlat          )
        else
            ! assume halo xlong xlat
            ! interpolate nodal coordinates

#ifdef DEBUG_OUT
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlat,'xlat',id)
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlong,'xlong',id)
#endif
        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            xlat,                                       & ! atm grid arrays in
            fxlat)                                      ! fire grid arrays out

        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            xlong,                                       & ! atm grid arrays in
            fxlong)                                      ! fire grid arrays out

        endif

     endif

    elseif(ifun.eq.2)then  
               
        ! after the loop where zsf created exited and all synced
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,zsf,'driver_phys:zsf')        

    elseif(ifun.eq.3)then  ! interpolate winds to the fire grid
    
        call interpolate_atm2fire(id,                     & ! flag for debug output
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,                                       & ! atm grid arrays in
            uf,vf)                                      ! fire grid arrays out
    
    endif

!   the following executes in any case

    call sfire_model (id,ifun,restart,need_lfn_update,  &
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        ignition_start_x,ignition_start_y,      & ! ignition - small arrays
        ignition_end_x,ignition_end_y,          &
        ignition_radius,                        &
        ignition_time,                          &
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        zsf,                                    & ! terrain height (for gradient)
        uf,vf,                                  & ! input: wind
        lfn,lfn_out,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,                                    & ! output: rate of spread for display
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,                              & ! save derived internal data
        bbb,betafl,phiwc,r_0,fgip,ischap &
    )
    
    if(ifun.eq.6)then ! heat fluxes into the atmosphere    
    
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fire_driver:fgrnqfx')
    
        ! sum the fluxes over atm cells
        call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnhfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnhfx)
!comment out the next call to get results as before commit 55fd92051196b796891b60cb7ec1c4bdb8800078
        call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnqfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnqfx)

        write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',fire_atm_feedback
        do j=jts,jte
            do i=its,ite
                ! scale ground fluxes to get the averages
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)/(ir*jr)
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)/(ir*jr)
                ! we do not have canopy fluxes yet...
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
        enddo

        do j=jts,jte
            do k=kts,min(kte+1,kde)
               do i=its,ite
                   rthfrten(i,k,j)=0.
                   rqvfrten(i,k,j)=0.
               enddo
            enddo
        enddo


        ! --- add heat and moisture fluxes to tendency variables by postulated decay

       call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
       call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')

       call fire_tendency(                 &
            ids,ide, kds,kde, jds,jde,      & ! dimensions
            ims,ime, kms,kme, jms,jme,      &
            its,ite, kts,kte, jts,jte,      & ! 
            grnhfx,grnqfx,canhfx,canqfx,        & ! fluxes on atm grid 
            fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,                &
            zs,z_at_w,dz8w,mu,rho,          &
            rthfrten,rqvfrten)                ! out

       ! debug print to compare

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,'fire_driver_phys:rqvfrten')

            
    endif ! ifun=6

enddo ! tiles
!$OMP END PARALLEL DO

if (ifun.eq.3)then
    ignitions_done=ignitions_done_tile(1) ! all should be the same
    do i=1,ignitions_done
        write(msg,*)'sfire_driver_phys: checking ignition ',i,' of ',ignitions_done
        call message(msg)
        ignited_patch(i)=0
        do ij=1,num_tiles
            ignited_patch(i)=ignited_patch(i)+ignited_tile(i,ij)
        enddo
#ifdef DM_PARALLEL
        call message('sfire_driver_phys: checking ignitions, collect counts')
        call wrf_dm_maxval(ignited_patch(i),idex,jdex)
        call message('sfire_driver_phys: collected')
#endif
        if(ignited_patch(i).eq.0)then
            call crash('sfire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo
endif

#ifdef DEBUG_OUT
if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,zsf,'zsf',pid)
    endif
#endif
elseif(ifun.eq.3)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,uf,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,vf,'vf',pid)
    endif
#endif
elseif(ifun.eq.5)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
#endif
elseif(ifun.eq.6)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,'rthfrten',pid)
        call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,'rqvfrten',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
#endif
    !call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,mu,'driver:mu')
    !call print_3d_stats(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rho,'driver:rho')
    kpe1=min(kps+1,kpe)
    !kpe1=kps-1
    do k=kts,min(kte,kts+3)
        write(kk,'(i2)')k
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,kk//'driver_phys:rthfrten')
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,kk//'driver_phys:rqvfrten')
    enddo
endif

end subroutine sfire_driver_phys
!
!*******************
!

subroutine fire_ignition_convert (config_flags,fire_max_ignitions,ignition_longlat, &
    fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
    fire_ignition_radius,fire_ignition_time,fire_num_ignitions)
    USE module_configure
    implicit none
! create ignition arrays from scalar flags
!*** arguments
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    real, dimension(fire_max_ignitions), intent(out):: &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_time
    integer, intent(out)::fire_num_ignitions,ignition_longlat
!*** local
    integer::i
    logical:: real,ideal
!*** executable
    ! this is only until I figure out how to input arrays through the namelist...
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')
    ! figure out which kind of coordinates from the first given
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')

    if(ideal)then
        ! use values from _x and _y variables
        ignition_longlat=0
        fire_ignition_start_x(1)=config_flags%fire_ignition_start_x1
        fire_ignition_start_y(1)=config_flags%fire_ignition_start_y1
        fire_ignition_end_x(1)=config_flags%fire_ignition_end_x1
        fire_ignition_end_y(1)=config_flags%fire_ignition_end_y1
        fire_ignition_start_x(2)=config_flags%fire_ignition_start_x2
        fire_ignition_start_y(2)=config_flags%fire_ignition_start_y2
        fire_ignition_end_x(2)=config_flags%fire_ignition_end_x2
        fire_ignition_end_y(2)=config_flags%fire_ignition_end_y2
        fire_ignition_start_x(3)=config_flags%fire_ignition_start_x3
        fire_ignition_start_y(3)=config_flags%fire_ignition_start_y3
        fire_ignition_end_x(3)=config_flags%fire_ignition_end_x3
        fire_ignition_end_y(3)=config_flags%fire_ignition_end_y3
        fire_ignition_start_x(4)=config_flags%fire_ignition_start_x4
        fire_ignition_start_y(4)=config_flags%fire_ignition_start_y4
        fire_ignition_end_x(4)=config_flags%fire_ignition_end_x4
        fire_ignition_end_y(4)=config_flags%fire_ignition_end_y4
        fire_ignition_start_x(5)=config_flags%fire_ignition_start_x5
        fire_ignition_start_y(5)=config_flags%fire_ignition_start_y5
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_x5
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_y5
    endif
    if(real)then
        ! use values from _long and _lat
        ignition_longlat=1
        fire_ignition_start_x(1)=config_flags%fire_ignition_start_lon1
        fire_ignition_start_y(1)=config_flags%fire_ignition_start_lat1
        fire_ignition_end_x(1)=config_flags%fire_ignition_end_lon1
        fire_ignition_end_y(1)=config_flags%fire_ignition_end_lat1
        fire_ignition_start_x(2)=config_flags%fire_ignition_start_lon2
        fire_ignition_start_y(2)=config_flags%fire_ignition_start_lat2
        fire_ignition_end_x(2)=config_flags%fire_ignition_end_lon2
        fire_ignition_end_y(2)=config_flags%fire_ignition_end_lat2
        fire_ignition_start_x(3)=config_flags%fire_ignition_start_lon3
        fire_ignition_start_y(3)=config_flags%fire_ignition_start_lat3
        fire_ignition_end_x(3)=config_flags%fire_ignition_end_lon3
        fire_ignition_end_y(3)=config_flags%fire_ignition_end_lat3
        fire_ignition_start_x(4)=config_flags%fire_ignition_start_lon4
        fire_ignition_start_y(4)=config_flags%fire_ignition_start_lat4
        fire_ignition_end_x(4)=config_flags%fire_ignition_end_lon4
        fire_ignition_end_y(4)=config_flags%fire_ignition_end_lat4
        fire_ignition_start_x(5)=config_flags%fire_ignition_start_lon5
        fire_ignition_start_y(5)=config_flags%fire_ignition_start_lat5
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_lon5
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_lat5
    endif
    ! common to both cases
        fire_ignition_radius(1)=config_flags%fire_ignition_radius1 
        fire_ignition_time(1)=config_flags%fire_ignition_time1 
        fire_ignition_radius(2)=config_flags%fire_ignition_radius2 
        fire_ignition_time(2)=config_flags%fire_ignition_time2 
        fire_ignition_radius(3)=config_flags%fire_ignition_radius3 
        fire_ignition_time(3)=config_flags%fire_ignition_time3 
        fire_ignition_radius(4)=config_flags%fire_ignition_radius4 
        fire_ignition_time(4)=config_flags%fire_ignition_time4 
        fire_ignition_radius(5)=config_flags%fire_ignition_radius5 
        fire_ignition_time(5)=config_flags%fire_ignition_time5

    ! 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            ! count the ignitions 
            if(fire_ignition_radius(i).gt.0.)fire_num_ignitions=i
            ! expand the point ignitions given as zero
            if(fire_ignition_end_x(i).eq.0.)fire_ignition_end_x(i)=fire_ignition_start_x(i)
            if(fire_ignition_end_y(i).eq.0.)fire_ignition_end_y(i)=fire_ignition_start_y(i)
        enddo

end subroutine fire_ignition_convert

!
!*****************************
!
!module_fr_sfire_driver%%interpolate_atm2fire

subroutine interpolate_atm2fire(id,               & ! for debug output, <= 0 no output
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    u_frame, v_frame,                             & ! velocity frame correction
    u,v,                                          & ! atm grid arrays in
    uf,vf)                                          ! fire grid arrays out
    
implicit none
!*** purpose: interpolate winds and height

!*** arguments
integer, intent(in)::id,                          &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in):: u_frame, v_frame                 ! velocity frame correction
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
    u,v                                             ! atm wind velocity, staggered  
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    uf,vf                                           ! wind velocity fire grid nodes 
    
    
!*** local
#define TDIMS its-1,ite+2,jts-1,jte+2
real, dimension(its-1:ite+2,jts-1:jte+2):: ua,va   ! atm winds, averaged over height
integer:: i,j,k,ifts1,ifte1,jfts1,jfte1

!*** executable

    k=kds             ! the ground
    do j = jts-1,jte+2
        do i = its-1,ite+2 
            ! average 1st 2 layers, correct const shift
            ua(i,j)=0.5*( u(i,k,j) + u(i,k+1,j)) + u_frame
            va(i,j)=0.5*( v(i,k,j) + v(i,k+1,j)) + v_frame
        enddo
    enddo

    ! extend the winds by one beyond the domain boundary 
    call continue_at_boundary(1,0,0., & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    ips,ipe+1,jps,jpe+1, &            ! patch dims - winds defined up to +1
    its,ite+1,jts,jte+1, &                ! tile dims
    va)                               ! array

    call continue_at_boundary(0,1,0., & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    ips,ipe+1,jps,jpe+1, &            ! patch dims - winds defined up to +1
    its,ite+1,jts,jte+1, &                ! tile dims
    ua)                               ! array

!if (id.gt.0) then
!    call write_array_m(TDIMS,TDIMS,ua,'ua',id)
!    call write_array_m(TDIMS,TDIMS,va,'va',id)
!endif

call print_2d_stats_vec(its,ite+1,jts,jte+1,TDIMS,ua,va, &
    'driver: atm wind (m/s)')
    

!      ---------------
!     | F | F | F | F |   Example of atmospheric and fire grid with
!     |-------|-------|   ir=jr=4.
!     | F | F | F | F |   Winds are given at the midpoints of the sides of the coarse grid,
!     u-------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
!     | F | F | F | F |
!     |---------------|
!     | * | F | F | F |
!      -------v-------
!
! Meshes are aligned by the lower left cell of the domain. Then in the above figure
! u = node with the ua component of the wind at (ids,jds), midpoint of side
! v = node with the va component of the wind at (ids,jds), midpoint of side
! * = fire grid node at (ifds,jfds)
! z = node with height, midpoint of cell
! 
! ua(ids,jds)=uf(ifds-0.5,jfds+jr/2+0.5)
! va(ids,jds)=vf(ifds+ir/2+0.5,jfds-0.5)
! za(ids,jds)=zsf(ifds+ir/2+0.5,jfds+jr/2+0.5)
    
    ifts1=snode(ifts,ifds,-1) ! go 1 beyond domain boundary but not between tiles
    ifte1=snode(ifte,ifde,+1)
    jfts1=snode(jfts,jfds,-1)
    jfte1=snode(jfte,jfde,+1)
    
    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        TDIMS,                  & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds-.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid     
        uf)                      ! out fire grid

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        TDIMS,                  & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir+1)*.5,jfds-0.5, & ! line up by lower left corner of domain
        va,                     & ! in atm grid     
        vf)                      ! out fire grid

!call print_2d_stats_vec(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme,uf,vf,'fire wind (m/s)')


end subroutine interpolate_atm2fire

!
!*****************************
!

subroutine interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
    ids,ide, jds,jde,                    & ! atm grid dimensions
    ims,ime, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    zs,                                       & ! atm grid arrays in
    zsf)                                      ! fire grid arrays out
    
implicit none
!*** purpose: interpolate height

!*** arguments
integer, intent(in)::id,                          &
    ids,ide, jds,jde,                    & ! atm domain bounds
    ims,ime,jms,jme,                    & ! atm memory bounds 
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in), dimension(ims:ime, jms:jme):: zs  ! terrain height at atm cell centers                                        & ! terrain height  
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    zsf                                             ! terrain height fire grid nodes
    
    
!*** local
real, dimension(its-2:ite+2,jts-2:jte+2):: za      ! terrain height
integer:: i,j,jts1,jte1,its1,ite1,jfts1,jfte1,ifts1,ifte1

! terrain height

    jts1=max(jts-1,jds) ! lower loop limit by one less when at end of domain
    its1=max(its-1,ids) ! ASSUMES THE HALO IS THERE if patch != domain
    jte1=min(jte+1,jde) 
    ite1=min(ite+1,ide)
    do j = jts1,jte1
        do i = its1,ite1 
            ! copy to local array
            za(i,j)=zs(i,j)           
        enddo
    enddo

    call continue_at_boundary(1,1,0., & ! do x direction or y direction
    its-2,ite+2,jts-2,jte+2,           &                ! memory dims
    ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
    ips,ipe,jps,jpe, &            ! patch dims - winds defined up to +1
    its1,ite1,jts1,jte1, &                ! tile dims
    za)                               ! array

    ! interpolate to tile plus strip along domain boundary if at boundary
    jfts1=snode(jfts,jfds,-1) ! lower loop limit by one less when at end of domain
    ifts1=snode(ifts,ifds,-1)
    jfte1=snode(jfte,jfde,+1) 
    ifte1=snode(ifte,ifde,+1)
                     
    call interpolate_2d(  &
        its-2,ite+2,jts-2,jte+2, & ! memory dims atm grid tile
        its1-1,ite1+1,jts1-1,jte1+1, & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,  & ! dimensions fire grid tile
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir+1)*.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
        za,                     & ! in atm grid     
        zsf)                      ! out fire grid

end subroutine interpolate_z2fire
!
!*****************************
!

subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)
!*** purpose: check if fire and atm meshes line up
implicit none
!*** arguments
integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s
!*** local
character(len=128)msg
!*** executable
if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_sfire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!
subroutine set_flags(config_flags)
USE module_configure
use module_fr_sfire_util
implicit none
TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
! copy flags from wrf to module_fr_sfire_util
! for instructions how to add a flag see the top of module_fr_sfire_util.F


fire_print_msg          = config_flags%fire_print_msg
fire_print_file         = config_flags%fire_print_file
fuel_left_method        = config_flags%fire_fuel_left_method
fuel_left_irl           = config_flags%fire_fuel_left_irl
fuel_left_jrl           = config_flags%fire_fuel_left_jrl
fire_const_time         = config_flags%fire_const_time
fire_const_grnhfx       = config_flags%fire_const_grnhfx
fire_const_grnqfx       = config_flags%fire_const_grnqfx
fire_atm_feedback       = config_flags%fire_atm_feedback
boundary_guard          = config_flags%fire_boundary_guard
fire_back_weight        = config_flags%fire_back_weight
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
fire_topo_from_atm      = config_flags%fire_topo_from_atm
fire_advection          = config_flags%fire_advection




end subroutine set_flags

subroutine print_id
character(len=128)::id,msg
id='b6fe89aeb71d941e91530aafcf2f5b183a44fc37'
msg=id
call message(msg)
end subroutine print_id

end module module_fr_sfire_driver
