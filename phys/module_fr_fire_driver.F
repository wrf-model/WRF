
! SFIRE - Spread fire model in WRF-Fire
!
! This module is the entry point from WRF ARW to the wildland 
! fire module. The call to fire_driver advances the fire module by 
! one timestep. The fire module inputs the wind and outputs 
! temperature and humidity tendencies. The fire module also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.
!
! Contributions to this wildland fire module have come from individuals at
! NCAR, the U.S.D.A. Forest Service, the Australian Bureau of Meteorology, 
! and the University of Colorado at Denver. 
!

module module_fr_fire_driver
! use this module for standalone call, you only need to provide some mock-up wrf modules  

use module_fr_fire_model
use module_fr_fire_phys, only : fire_params , init_fuel_cats
use module_fr_fire_util
use module_fr_fire_core, only: ignition_line_type
use module_fr_fire_atm, only: add_fire_tracer_emissions 

USE module_domain, only: domain
USE module_configure, only: grid_config_rec_type
use module_model_constants, only: reradius,    & ! 1/earth radiusw
                                  g,           & ! gravitation acceleration
                                  pi2            ! 2*pi

implicit none


private
public fire_driver_em,use_atm_vars

logical:: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back

logical:: fmoist_run, fmoist_interp, fire_run  ! which kind of model to run overall

contains

#define DEBUG_OUT
subroutine fire_driver_em ( grid , config_flags                    & 
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w           &  
            )

!*** purpose: driver from grid structure

! Driver layer modules
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_fuel_sub, halo_fire_tign_sub, halo_fire_wind_f_sub, &
halo_fire_wind_a_sub, halo_fire_ph_sub, halo_fire_zsf_sub, halo_fire_longlat_sub, &
halo_fire_phb_sub, halo_fire_z0_sub, halo_fire_lfn_sub, halo_fire_mfg_sub, &
halo_fire_mag_sub
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
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in), optional::rho,z_at_w,dz8w 

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    TYPE(ignition_line_type), DIMENSION(fire_max_ignitions):: ignition_line
    integer::fire_ifun,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::need_lfn_update,restart
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    real:: unit_fxlong ,unit_fxlat
    type(fire_params)::fp
    real:: time_start, dt
    real:: moisture_time
    logical:: run_advance_moisture,run_fuel_moisture, moisture_initializing
    real::    dt_moisture



!*** executable

! populate our structures from wrf

    ! pointers to be passed to fire rate of spread formulas
    fp%vx => grid%uf         ! W-E winds used in fire module
    fp%vy => grid%vf         ! S-N winds used in fire module
    fp%zsf => grid%zsf       ! terrain height 
    fp%dzdxf => grid%dzdxf   ! terrain grad 
    fp%dzdyf => grid%dzdyf   ! terrain grad 
    fp%bbb => grid%bbb       ! a rate of spread formula coeff 
    fp%betafl => grid%betafl ! a rate of spread formula variable 
    fp%phiwc => grid%phiwc   ! a rate of spread formula coeff 
    fp%r_0 => grid%r_0       ! a rate of spread formula variable
    fp%fgip => grid%fgip     ! a rate of spread formula coeff 
    fp%ischap => grid%ischap ! a rate of spread formula switch
    fp%iboros => grid%iboros ! Ib divided by ROS
    fp%fmc_g => grid%fmc_g   ! fuel moisture, ground
            
    ! get ignition data 
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)

    ! copy configuration flags to fire internal structures
    call set_flags(config_flags)

    ! refinement r
    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart

    ! time - assume dt does not change
    dt = grid%dt
    time_start = itimestep * dt

    ! **** moisture model

    ! decide what to run - moisture, interpolation, or fire model itself
    fmoist_run    = config_flags%fmoist_run
    fmoist_interp = config_flags%fmoist_interp 
    if(fire_fmc_read.ne.0.and.fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')
    fire_run = .not. config_flags%fmoist_only

    !decide what to run
    moisture_time = time_start
    run_advance_moisture = .false. ! default
    run_fuel_moisture = .false. ! default
    moisture_initializing = fire_ifun_start < 3
    
    
    
    if(fmoist_run)then
        if(moisture_initializing)then
            if(fire_ifun_end>2)call crash('initialization must be run separately')
            grid%fmoist_lasttime=moisture_time ! initialize the last time the model has run to start of run
            grid%fmoist_nexttime=moisture_time 
            call message('moisture initialization')
            run_advance_moisture = .true.
        else ! regular timestep
            if(config_flags%fmoist_freq > 0)then  ! regular timestep. go by multiples?
                if(mod(grid%itimestep,config_flags%fmoist_freq) .eq. 0)then
                    write(msg,'(a,i10,a,i10)')'moisture model runs because timestep ',grid%itimestep,' is a multiple of ',config_flags%fmoist_freq
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            else
                if(.not. moisture_time  < grid%fmoist_nexttime) then ! no, by time interval
                    write(msg,'(a,f12.2,a)')'moisture model runs because time ',grid%fmoist_nexttime,'s has arrived'
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            endif
            if(run_advance_moisture)then ! decide on timing
                dt_moisture  = moisture_time - grid%fmoist_lasttime  ! Time since moisture model run the last time. Should be long.
                grid%fmoist_lasttime = moisture_time
                if(config_flags%fmoist_freq > 0)then
                    write(msg,'(a,f12.2,a,i10,a)')'moisture time step is ',dt_moisture,'s running every ',config_flags%fmoist_freq,' steps'
                    call message(msg)
                else
                    grid%fmoist_nexttime = moisture_time + config_flags%fmoist_dt
                    write(msg,'(a,f12.2,a,f12.2,a)')'moisture time step is ',dt_moisture,'s next run at ',grid%fmoist_nexttime,'s'
                    call message(msg)
                endif
                if(fmoist_interp)then
                    call message('moisture interpolation to fuels will run because moisture model does')
                    run_fuel_moisture=.true.
                endif
            endif
        endif
    elseif(itimestep.eq.1.and.fmoist_interp)then
            call message('initializing, moisture interpolation to fuels will run from input data')
            run_fuel_moisture=.true.
    endif

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,i4)') &
       'fire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' test steps',tsteps
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call message(msg)

    do istep=0,tsteps ! istep >0 is for testing only, exit after the first call
      itimestep = grid%itimestep + istep ! in the first call, do fire_test_steps steps of the fire model

      need_lfn_update=.false.
      do fire_ifun=fire_ifun_start,fire_ifun_end

        ! 1 = run pass 1: interpolate height to zsf=terrain, moisture_model initialize
        ! 2 = initialize run pass 2: set fuel data, terrain gradient
        ! 3 = initialize timestep: interpolate winds, check for ignition, time step on moisture model
        ! 4 = do one timestep 
        ! 5 = copy timestep output to input
        ! 6 = compute output fluxes

#ifdef DM_PARALLEL

       if(fire_run)then
        if(need_lfn_update)then
!           halo exchange on lfn width 3
#include "HALO_FIRE_LFN.inc"
        endif

        if(fire_ifun.eq.1)then
!       halo exchange on topography
#include "HALO_FIRE_LONGLAT.inc"
!!            if(fire_topo_from_atm.eq.1)then
!!#include "HALO_FIRE_HT.inc"
!!            endif 
! base geopotential and roughness
#include "HALO_FIRE_PHB.inc"

#include "HALO_FIRE_Z0.inc"

        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"

        elseif(fire_ifun.eq.3)then
!           halo exchange on atm winds and geopotential, width 1 for interpolation
#include "HALO_FIRE_WIND_A.inc"
#include "HALO_FIRE_PH.inc"

        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 2 for a 2-step RK method
#include "HALO_FIRE_WIND_F.inc"

            if(run_fuel_moisture)then
            ! have interpolated to the fire grid
#include "HALO_FIRE_MFG.inc"
            endif

        elseif(fire_ifun.eq.6)then
!           computing fuel_left needs ignition time from neighbors
#include "HALO_FIRE_TIGN.inc"

        endif
       endif
#endif

        
        ! need domain by 1 smaller, in last row.col winds are not set properly
        call fire_driver_phys ( &
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
            time_start,                                 &
            itimestep,restart,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  ! in scalars
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            unit_fxlong,unit_fxlat,                           & ! coordinates of grid center
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            config_flags%fire_wind_height,              &  ! height of wind input to fire spread formula
            fire_num_ignitions,                                & 
            fire_ignition_longlat,      &
            ignition_line,              &
            grid%u_2,grid%v_2,           &          ! atm arrays in
            grid%ph_2,grid%phb,               & ! geopotential
            grid%z0,                        & ! roughness height
            grid%ht,                        &                         ! terrain height
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location
            grid%lfn, &
            grid%lfn_hist, &  ! PAJ: to initialize fire perimeter from obs
            config_flags%fire_is_real_perim,  & ! PAJ: use obs fire perimeter. 
            grid%lfn_0,grid%lfn_1,grid%lfn_2,grid%lfn_s0,grid%lfn_s1,grid%lfn_s2,grid%lfn_s3,grid%flame_length,grid%ros_front, & 
            grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%fire_area,                               & ! redundant, for display, fire grid
            grid%burnt_area_dt,                               & ! redundant, for display
            lfn_out,                                      & ! work - one timestep    
            grid%avg_fuel_frac,                           & ! out redundant arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%grnhfx_fu,grid%grnqfx_fu, & ! out redundant arrays, atm grid
            grid%uah,grid%vah,                          &
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%ros,                                   & ! rate of spread
            grid%fxlong,grid%fxlat,grid%fz0,                           &       
            grid%nfuel_cat,                               & ! input, or internal for safekeeping
            grid%fuel_time,                      & 
            config_flags%nfmc,         & ! moisture model variables start
            run_advance_moisture,run_fuel_moisture,dt_moisture,     &    ! moisture model control
            config_flags%fmep_decay_tlag,                               & ! moisture extended model assim. diffs decay time lag
            grid%rainc, grid%rainnc,                       & ! accumulated rain from different sources
            grid%t2, grid%q2, grid%psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
            grid%rain_old,                   & ! previous value of accumulated rain
            grid%t2_old, grid%q2_old, grid%psfc_old,   & ! previous values of the atmospheric state at surface
            grid%rh_fire,                    & ! relative humidity, diagnostics
            grid%fmc_gc,                      & ! fuel moisture fields updated, by class, assumed set to something reasonable
            grid%fmep,                      & ! fuel moisture extended model parameters
            grid%fmc_equi,                      & ! fuel moisture fields updated, by class, equilibrium diagnostic 
            grid%fmc_lag,                      & ! fuel moisture fields updated, by class, tendency diagnostic
            fp%fmc_g , &                             !  write-only alias. need to exit before using fp again
            fp, &
            grid, & ! DME added for halo update
            ids,ide,jds,jde,kds,kde, & 
            ims,ime,jms,jme,kms,kme, & 
            ips,ipe,jps,jpe,kps,kpe, &
            config_flags & 
        )

   
#ifdef DM_PARALLEL
          if(fire_run)then
            if(fire_ifun.eq.2)then
!               halo exchange on all fuel data width 2
#include "HALO_FIRE_FUEL.inc"
!               fire state was initialized
                call message('halo exchange on lfn width 2')
#include "HALO_FIRE_LFN.inc"
          endif
          if(run_fuel_moisture)then
            if(fire_ifun.eq.3)then
                     ! prepare for interpolation to the fire grid
#include "HALO_FIRE_MAG.inc"
                endif
            endif
            endif
#endif

! DME update tracer emissions
     if(fire_ifun.eq.6 .AND. config_flags%tracer_opt.eq.3)then
       call add_fire_tracer_emissions(config_flags%tracer_opt,grid%dt,grid%dx,grid%dy, &
                 ifms,ifme,jfms,jfme,    &
                 ifps,ifpe,jfps,jfpe,    &
                 ids,ide,kds,kde,jds,jde,          &
                 ims,ime,kms,kme,jms,jme,          &
                 ips,ipe,kps,kpe,jps,jpe,          &
                 rho,dz8w,                         &
                 grid%burnt_area_dt,grid%fgip, &
                 grid%tracer,config_flags%fire_tracer_smoke)
     endif
! DME
      enddo
    enddo
    if(tsteps>0)call crash('fire_driver_em: test run of uncoupled fire model completed')

end subroutine fire_driver_em

!
!*******************
!

! module_fr_fire_driver%%fire_driver
subroutine fire_driver_phys (ifun,need_lfn_update,    &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    time_start,                                   & ! start of the timestep
    itimestep,restart,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    fire_wind_height,                             & ! for vertical wind interpolation
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_line,                                &
    u,v,                                       & ! in arrays, atm grid
    ph,phb,                                       &
    z0,zs,                                        & 
    xlong,xlat,                                   &
    lfn,                                      &
    lfn_hist,                                      & ! PAJ to init perimeter from obs
    fire_is_real_perim,                            & ! PAJ to init perimeter from obs
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front, & 
    tign,fuel_frac,                           & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    burnt_area_dt,                                    & ! redundant state, fire grid
    lfn_out,                                      & ! out level set function    
    avg_fuel_frac,                                &
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    grnhfx_fu,grnqfx_fu,                 & ! out redundant arrays, atm grid
    uah,vah,                                      & ! out atm grid
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,                                          &
    fxlong,fxlat,fz0,                                 & 
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,                                & ! save constant internal data, fire grid
    nfmc,                                     & ! number of fuel moisture classes
    run_advance_moisture,run_fuel_moisture,dt_moisture,& ! moisture model control
    fmep_decay_tlag,                              & ! moist. extended model assim. diffs time lag
    rainc,rainnc,              & ! accumulated rain from different sources
    t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    rain_old,                   & ! previous value of accumulated rain
    t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
    rh_fire,                    & ! relative humidity, diagnostics
    fmc_gc,                    &  ! fuel moisture fields updated, by class, assumed set to something reasonable
    fmep,                       &  ! fuel moisture extended model parameters
    fmc_equi,                    &  ! fuel moisture fields updated, by class equilibrium diagnostic
    fmc_lag,                    &  ! fuel moisture fields updated, by class tendency diagnostic
    fmc_g, &                          ! fuel moisture, alias of fp%fmc_g
    fp,                                           & ! fire params
    grid,                                         & 
    ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, &
    ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, &
    ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu, &
    config_flags & 
    )

USE module_dm, only:wrf_dm_maxval
USE module_domain, only: domain

implicit none

    type(domain) , target :: grid                             ! state 

!*** arguments

integer, intent(in)::ifun,                        &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe, kps,kpe, jps,jpe,                    & ! atm patch bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ir,jr,                                        & ! atm/fire grid refinement ratio
    nfmc,                                     & ! number of fuel moisture classes
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                       ! -1=not at all, done outside 
                                                       ! 0=from nfuel_cat0
                                                       ! 1=from altitude
                                                       ! 2=from file
    nfuel_cat0,                                   & ! fuel category to initialize everything to
    num_tiles                                       ! number of tiles

integer, intent(in)::ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
                     ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
                     ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu   

type (grid_config_rec_type), intent(in)          :: config_flags 

logical, intent(in)::restart
    

logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    time_start,                                   & ! start of this time step from the beginning of the simulation
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of surface fire heat flux (m)
    fire_ext_crwn,                                & ! wind height for vertical interploation to fire spread
    fire_wind_height 


integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner
TYPE (ignition_line_type), DIMENSION(num_ignitions), intent(out):: ignition_line

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v, & ! wind velocity (m/s) (staggered atm grid) 
                              ph, phb                      ! geopotential (w-points atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::   z0, &    ! roughness height
                                                zs       ! terrain height  
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & ! atm wind at fire_wind_height, diagnostics
    vah                                              ! atm wind at fire_wind_height, diagnostics

real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat ! inout because of extension at bdry
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    lfn_hist,                                  &     ! PAJ: to init obs. fira perimeter
    lfn_out                                    ! fire wind velocities

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front  ! level set function and reinitialization steps

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        ! fraction of each cell burning

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    burnt_area_dt                                        
real, intent(out), dimension(ims:ime, jms:jme)::  &
   grnhfx_fu,grnqfx_fu                                        

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    avg_fuel_frac,                               &  ! average fuel fraction
    grnhfx,                                      &  ! heat flux from surface fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from surface fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  ! redundant arrays, for display only, fire grid
    fgrnhfx,                                      &  ! heat flux from surface fire (W/m^2) 
    fgrnqfx,                                      &  ! moisture flux from surface fire (W/m^2) 
    fcanhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                      &   ! moisture flux from crown fire (W/m^2) 
    ros                                             ! fire rate of spread (m/s)
    
! moisture model arguments
logical, intent(in)::run_advance_moisture,run_fuel_moisture
real, intent(in)::dt_moisture
real, intent(in)::fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(out),dimension(ims:ime,jms:jme):: rh_fire
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi,fmc_lag
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fmc_g



!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat,fz0 ! fire mesh coordinates
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays

type(fire_params),intent(inout)::fp
    
!*** local
real :: dxf,dyf,latm, s
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,ipe1,jpe1,ite1,jte1, &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex

    ! PAJ:
  logical :: fire_is_real_perim

!*** executable

! fire mesh step
dxf=dx/ir
dyf=dy/jr



!$OMP CRITICAL(FIRE_DRIVER_CRIT)
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
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

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
   ! properties of fuel categories and moisture classes from namelist.fire
   call init_fuel_cats(fmoist_run .or. fmoist_interp) ! common for all threads
endif
!$OMP END SINGLE


pid=0
if(fire_print_file.gt.0)then
    if(itimestep.le.fire_print_file.or.mod(itimestep,fire_print_file).eq.0)pid=itimestep ! print 1-fire_print_file then every fire_print_file-th
endif

if(ifun.eq.3)then
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,1,0,0,u,'u')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,0,1,v,'v')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,1,0,ph,'ph')
endif

! fake atm tile bounds
kts=kps
kte=kpe

! staggered atm patch bounds
ipe1=ifval(ipe.eq.ide,ipe+1,ipe)
jpe1=ifval(jpe.eq.jde,jpe+1,jpe)

! set up fire tiles & interpolate to fire grid
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ite1,jte1,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id = ifval(pid.ne.0,pid+ij*10000,0) ! for print

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
        
! staggered atm tile bounds
    ite1=ifval(ite.eq.ide,ite+1,ite)
    jte1=ifval(jte.eq.jde,jte+1,jte)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i3,1x,a,i7,1x,a,i3)')'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory


!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    if(run_advance_moisture)then
      if(ifun.eq.3)then 
      
      ! one timestep of the moisture model
          call message('advance_moisture start')
          call advance_moisture(    &
              itimestep.eq.1,             & ! initialize?
              ims,ime,  jms,jme,          & ! memory dimensions
              its,ite,  jts,jte,          & ! tile dimensions
              nfmc,                       & ! number of moisture fields
              dt_moisture,                & ! moisture model time step
              fmep_decay_tlag,            & ! moisture extended model assim. diffs decay tlag
              rainc, rainnc,              & ! accumulated rain 
              t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
              rain_old,                   & ! previous value of accumulated rain
              t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
              rh_fire,                    & ! relative humidity, diagnostics
              fmc_gc,                     & ! fuel moisture fields updated, by class, assumed set to something reasonable
              fmep,                       & ! fuel moisture extended model parameters
              fmc_equi,                   &  ! fuel moisture fields updated, by class equilibrium diagnostic
              fmc_lag                    &  ! fuel moisture fields updated, by class tendency diagnostic
          )
          call message('advance_moisture end')
      endif
    endif
        
    if(fire_run)then

     if(ifun.eq.1)then   ! set terrain

      if(restart)then
          
          call message('restart - topo initialization skipped')

      else
!
!        call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'driver:zs')
!    
!        ! interpolate terrain height
!        if(fire_topo_from_atm.eq.1)then
!            call interpolate_z2fire(id,1,                 & ! for debug output, <= 0 no output
!                ids,ide,  jds,jde,                    & ! atm grid dimensions
!                ims,ime,  jms,jme,                    &
!                ips,ipe,jps,jpe,                              &
!                its,ite,jts,jte,                              &
!                ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
!                ifms, ifme, jfms, jfme,                       &
!                ifts,ifte,jfts,jfte,                          &
!                ir,jr,                                        & ! atm/fire grid ratio
!                zs,                                       & ! atm grid arrays in
!                fp%zsf)                                      ! fire grid arrays out
!        else
!!$OMP CRITICAL(FIRE_DRIVER_CRIT)
!           write(msg,'(a,i3,a)')'fire_topo_from_atm=',fire_topo_from_atm,' assuming ZSF set, interpolation skipped'
!!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
!        endif

        if(ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates - used for ignition only
            ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop

            ! DME: Next call added to fixe a bug when
            ! initializing a nested domain (before, fxlong & fxlat where not
            ! assigned, they were supposed to be set by a mod in WPS 
            ! but here we use standard WPS therefore were zero and fire does not ignite)
            call set_ideal_coord( dxf,dyf, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                fxlong,fxlat          )
            call interpolate_z2fire(id,             & 
                ids,ide,jds,jde,                    &
                ims,ime,jms,jme,                    &
                ips,ipe,jps,jpe,                    &
                its,ite,jts,jte,                    &
                ifds,ifde,jfds,jfde,                &
                ifms,ifme,jfms,jfme,                &
                ifts,ifte,jfts,jfte,                &
                ir,jr,                              &
                z0,                                 &
                fz0,1)
            !call set_ideal_coord( dx,dy, &
            !    ids,ide,jds,jde,  &
            !    ims,ime,jms,jme,  &
            !    its,ite,jts,jte,  &
            !    xlong,xlat          )
        elseif(use_atm_vars)then
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
            fxlat,0)                                      ! fire grid arrays out

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
            fxlong,0)                                      ! fire grid arrays out

        call interpolate_z2fire(id,                       & 
            ids,ide,  jds,jde,                            &
            ims,ime,  jms,jme,                            &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       &
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        &
            z0,                                           &
            fz0,1)

        endif

     endif

    elseif(ifun.eq.2)then  
               
        ! after the loop where zsf created exited and all synced
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fp%zsf,'driver_phys:zsf')        

    elseif(ifun.eq.3)then  ! interpolate winds to the fire grid


      if(use_atm_vars)then                                  

        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,z0,'z0',id)
        call write_array_m3(its,ite1,kts,kde-1,jts,jte,ims,ime,kms,kme,jms,jme,u,'u_2',id)
        call write_array_m3(its,ite,kts,kde-1,jts,jte1,ims,ime,kms,kme,jms,jme,v,'v_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,ph,'ph_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,phb,'phb',id)
        call interpolate_atm2fire(id,                     & ! flag for debug output
            fire_wind_height,                             & ! height to interpolate to
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe, jps,jpe,                             &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
            ifts, ifte, jfts, jfte,                       &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,                                          & ! 3D atm grid arrays in
            ph,phb,                                       &
            z0,zs,                                        & ! 2D atm grid arrays in
            uah,vah,                                      & ! 2D atm grid out
            fp%vx,fp%vy,fz0)                                    ! fire grid arrays out

      endif

    ! endif ! ifun
    ! endif ! fire_run
     
    elseif(ifun.eq.4)then

      ! interpolate and compute weighted average to get the fuel moisture
      !! print *,'ifun=4, run_fuel_moisture=',run_fuel_moisture
      if(run_fuel_moisture)then
        call message('fuel_moisture start')
        call fuel_moisture(                &
        id,                                     & ! for prints and maybe file names
        nfmc,                                &
        ids,ide, jds,jde,               & ! atm grid dimensions
        ims,ime, jms,jme,           &
        ips,ipe,jps,jpe,                &
        its,ite,jts,jte,                     &
        ifds, ifde, jfds, jfde,         & ! fire grid dimensions
        ifms, ifme, jfms, jfme,     &
        ifts,ifte,jfts,jfte,                 &
        ir,jr,                                   & ! atm/fire grid ratio
        nfuel_cat,                         & ! fuel data
        fmc_gc,                             & ! moisture contents by class on atmospheric grid
        fmc_g                                & ! weighted fuel moisture contents on fire grid
        )
        call message('fuel_moisture end')
      endif ! run_fuel_moisture`

     endif ! ifun

    endif ! fire_run

!   the following executes in any case

    if(fire_run)then
      call fire_model (id,ifun,restart,need_lfn_update,  &
        run_fuel_moisture,                      & ! if fuel moisture needs to be updated
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        ignition_line,                          & ! description of ignition lines
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        lfn,                                     & ! state: level function
        lfn_hist,                                & ! PAJ: to init obs fire perimeter
        fire_is_real_perim,                      & ! PAJ: to init obs fire perimeter
        lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front,  & ! state
        lfn_out,tign,fuel_frac,                     & ! state: ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        burnt_area_dt,                              & 
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,                                    & ! output: rate of spread for display
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,                              & ! save derived internal data
        fp,                                      & ! fire coefficients
        grid,            &  ! DME added for halo update
        ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
        ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
        ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  &
    )

    if(ifun.eq.6)then ! heat fluxes into the atmosphere    
    
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fire_driver:fgrnqfx')
    
        ! sum the fluxes over atm cells
        if(use_atm_vars)then                                  
          call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fuel_frac,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            avg_fuel_frac)
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

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
          write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',fire_atm_feedback
!$OMP end CRITICAL(FIRE_DRIVER_CRIT)
	  call message(msg)
          s = 1./(ir*jr)
          do j=jts,jte
            do i=its,ite
                ! DME heat fluxes contribution for the case wiythout feedback
                grnhfx_fu(i,j)=grnhfx(i,j)*s
                grnqfx_fu(i,j)=grnqfx(i,j)*s
                ! scale surface fluxes to get the averages
                avg_fuel_frac(i,j)=avg_fuel_frac(i,j)*s
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)*s
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)*s
                ! we do not have canopy fluxes yet...
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
          enddo

          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')
       endif

    endif ! ifun=6
  endif ! fire_run
enddo ! tiles
!$OMP END PARALLEL DO

#ifdef DEBUG_OUT
if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
endif
#endif

if (ifun.eq.3)then
    ignitions_done=ignitions_done_tile(1) ! all should be the same
    do i=1,ignitions_done
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
        write(msg,'(2(a,i4,1x))')'fire_driver_phys: checking ignition ',i,' of ',ignitions_done
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
        call message(msg)
        ignited_patch(i)=0
        do ij=1,num_tiles
            ignited_patch(i)=ignited_patch(i)+ignited_tile(i,ij)
        enddo
#ifdef DM_PARALLEL
        call message('fire_driver_phys: checking ignitions, collect counts')
        call wrf_dm_maxval(ignited_patch(i),idex,jdex)
        call message('fire_driver_phys: collected')
#endif
        if(ignited_patch(i).eq.0)then
            call crash('fire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo

 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,1,0,0,uah,'uah')
 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,1,vah,'vah')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vx,'uf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vy,'vf')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe1,jps,jpe,ims,ime,jms,jme,uah,'uah',pid)
        call write_array_m(ips,ipe,jps,jpe1,ims,ime,jms,jme,vah,'vah',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe1,kds,kde+1,jps,jpe,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe,kds,kde+1,jps,jpe1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
#endif
endif

if(ifun.eq.5)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
#endif
endif

if(ifun.eq.6)then
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnhfx,'fgrnhfx')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnqfx,'fgrnqfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnhfx,'grnhfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnqfx,'grnqfx')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
#endif
endif

end subroutine fire_driver_phys
!
!*******************
!

subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)
    USE module_configure, only : grid_config_rec_type
    implicit none
! create ignition arrays from scalar flags
!*** arguments
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    TYPE (ignition_line_type), DIMENSION(fire_max_ignitions), intent(out):: ignition_line ! any values from input discarded 
    integer, intent(out)::fire_num_ignitions,fire_ignition_longlat
    real, intent(out)::unit_fxlong,unit_fxlat
!*** local
    integer::i
    logical:: real,ideal
    real::lat_ctr,lon_ctr
!*** executable
    ! this is only until I figure out how to input arrays through the namelist...
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')
    ! figure out which kind of coordinates from the first given
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')

    fire_ignition_longlat=0  ! default, if no ignition
    if(ideal)then
       ! use values from _x and _y variables
       fire_ignition_longlat=0
       ignition_line(1)%start_x=config_flags%fire_ignition_start_x1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_y1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_x1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_y1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_x2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_y2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_x2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_y2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_x3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_y3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_x3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_y3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_x4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_y4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_x4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_y4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_x5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_y5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_x5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_y5
    endif
    if(real)then
        ! use values from _long and _lat
       fire_ignition_longlat=1
       ignition_line(1)%start_x=config_flags%fire_ignition_start_lon1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_lat1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_lon1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_lat1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_lon2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_lat2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_lon2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_lat2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_lon3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_lat3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_lon3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_lat3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_lon4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_lat4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_lon4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_lat4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_lon5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_lat5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_lon5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_lat5
    endif
    ! common to both cases
       ignition_line(1)%ros=config_flags%fire_ignition_ros1 
       ignition_line(1)%radius=config_flags%fire_ignition_radius1 
       ignition_line(1)%start_time=config_flags%fire_ignition_start_time1 
       ignition_line(1)%end_time=config_flags%fire_ignition_end_time1 
       ignition_line(2)%ros=config_flags%fire_ignition_ros2 
       ignition_line(2)%radius=config_flags%fire_ignition_radius2 
       ignition_line(2)%start_time=config_flags%fire_ignition_start_time2 
       ignition_line(2)%end_time=config_flags%fire_ignition_end_time2 
       ignition_line(3)%ros=config_flags%fire_ignition_ros3 
       ignition_line(3)%radius=config_flags%fire_ignition_radius3 
       ignition_line(3)%start_time=config_flags%fire_ignition_start_time3 
       ignition_line(3)%end_time=config_flags%fire_ignition_end_time3 
       ignition_line(4)%ros=config_flags%fire_ignition_ros4 
       ignition_line(4)%radius=config_flags%fire_ignition_radius4 
       ignition_line(4)%start_time=config_flags%fire_ignition_start_time4 
       ignition_line(4)%end_time=config_flags%fire_ignition_end_time4 
       ignition_line(5)%ros=config_flags%fire_ignition_ros5 
       ignition_line(5)%radius=config_flags%fire_ignition_radius5 
       ignition_line(5)%start_time=config_flags%fire_ignition_start_time5
       ignition_line(5)%end_time=config_flags%fire_ignition_end_time5

    ! 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            ! count the ignitions 
            if(ignition_line(i)%radius.gt.0.)fire_num_ignitions=i
            ! expand ignition data given as zero
            if(ignition_line(i)%end_x.eq.0.)ignition_line(i)%end_x=ignition_line(i)%start_x
            if(ignition_line(i)%end_y.eq.0.)ignition_line(i)%end_y=ignition_line(i)%start_y
            if(ignition_line(i)%end_time.eq.0.)ignition_line(i)%end_time=ignition_line(i)%start_time
        enddo

    if(fire_ignition_longlat .eq. 0)then
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

end subroutine fire_ignition_convert

!
!*****************************
!

subroutine interpolate_atm2fire(id,               & ! for debug output, <= 0 no output
    fire_wind_height,                             & ! interpolation height
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    u_frame, v_frame,                             & ! velocity frame correction
    u,v,                                          & ! atm grid arrays in
    ph,phb,                                       &
    z0,zs,                                        &
    uah,vah,                                      &
    uf,vf,z0f)                                          ! fire grid arrays out
    
implicit none
!*** purpose: interpolate winds and height

!*** arguments
integer, intent(in)::id                           
real, intent(in):: fire_wind_height                 ! height above the terrain for vertical interpolation
integer, intent(in)::                             &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in):: u_frame, v_frame                 ! velocity frame correction
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
    u,v,                                          & ! atm wind velocity, staggered  
    ph,phb                                          ! geopotential
real,intent(in),dimension(ims:ime,jms:jme)::&
    z0,                                           & ! roughness height
    zs                                              ! terrain height
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & ! atm wind at fire_wind_height, diagnostics
    vah                                              ! 
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    uf,vf                                           ! wind velocity fire grid nodes 
real,intent(in),dimension(ifms:ifme,jfms:jfme)::z0f ! roughness length in fire grid 
    
    
!*** local
character(len=256)::msg
#define TDIMS its-2,ite+2,jts-2,jte+2
real, dimension(its-2:ite+2,jts-2:jte+2):: ua,va   ! atm winds, interpolated over height, still staggered grid
real, dimension(its-2:ite+2,kds:kde,jts-2:jte+2):: altw,altub,altvb,hgtu,hgtv ! altitudes
integer:: i,j,k,ifts1,ifte1,jfts1,jfte1,ite1,jte1
integer::itst,itet,jtst,jtet,itsu,iteu,jtsu,jteu,itsv,itev,jtsv,jtev
integer::kdmax,its1,jts1,ips1,jps1
integer::itsou,iteou,jtsou,jteou,itsov,iteov,jtsov,jteov
real:: ground,loght,loglast,logz0,logfwh,ht,zr
real::r_nan
integer::i_nan
equivalence (i_nan,r_nan)
real::fire_wind_height_local,z0fc
real::ust_d,wsf,wsf1,uf_temp,vf_temp
real,parameter::vk_kappa=0.4

!*** executable

! debug init local arrays
i_nan=2147483647
ua=r_nan
va=r_nan
altw=r_nan
altub=r_nan
hgtu=r_nan
hgtv=r_nan


if(kds.ne.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,*)'WARNING: bottom index kds=',kds,' should be 1?'
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif

!                            ^ j
!        ------------        |
!        |          |         ----> i
!        u    p     |
!        |          |    nodes in cell (i,j)
!        ------v-----    view from top     
!
!             v(ide,jde+1)
!            -------x------        
!            |            |      
!            |            |      
! u(ide,jde) x      x     x u(ide+1,jde) 
!            | p(ide,hde) |   
!            |            |   p=ph,phb,z0,...
!            -------x------        
!              v(ide,jde)
!
! staggered values set u(ids:ide+1,jds:jde) v(ids:ide,jds:jde+1)
! p=ph+phb set at (ids:ide,jds:jde) 
! location of u(i,j) needs p(i-1,j) and p(i,j)
! location of v(i,j) needs p(i,j-1) and p(i,j)
! *** NOTE need HALO in ph, phb
! so we can compute only u(ids+1:ide,jds:jde) v(ids:ide,jds+1,jde)
! unless we extend p at the boundary
! but because we care about the fire way in the inside it does not matter
! if the fire gets close to domain boundary the simulation is over anyway

    ite1=snode(ite,ide,1)
    jte1=snode(jte,jde,1)
    ! do this in any case to check for nans
    call print_3d_stats(its,ite1,kds,kde,jts,jte,ims,ime,kms,kme,jms,jme,u,'wind U in')
    call print_3d_stats(its,ite,kds,kde,jts,jte1,ims,ime,kms,kme,jms,jme,v,'wind V in')

    if(fire_print_msg.gt.0)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
       write(msg,'(a,f7.2,a)')'interpolate_atm2fire: log-interpolation of wind to',fire_wind_height,' m'
       call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    endif

    ! indexing
       
    ! file for u
    itst=ifval(ids.eq.its,its,its-1)
    itet=ifval(ide.eq.ite,ite,ite+1)
    jtst=ifval(jds.ge.jts,jts,jts-1)
    jtet=ifval(jde.eq.jte,jte,jte+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'atm input  ','tile',its,ite,jts,jte
  call message(msg)
  write(msg,7001)'altw       ','tile',itst,itet,jtst,jtet
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)

    kdmax=kde-1   ! max layer to interpolate from, can be less

    do j = jtst,jtet
      do k=kds,kdmax+1
        do i = itst,itet       
          altw(i,k,j) = (ph(i,k,j)+phb(i,k,j))/g             ! altitude of the bottom w-point 
        enddo
      enddo
    enddo

! values at u points
    itsu=ifval(ids.eq.its,its+1,its)  ! staggered direction
    iteu=ifval(ide.eq.ite,ite,ite+1)  ! staggered direction
    jtsu=ifval(jds.ge.jts,jts,jts-1)
    jteu=ifval(jde.eq.jte,jte,jte+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'u interp at','tile',itsu,iteu,jtsu,jteu
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif

    do j = jtsu,jteu          
      do k=kds,kdmax+1
        do i = itsu,iteu       
          altub(i,k,j)= 0.5*(altw(i-1,k,j)+altw(i,k,j))      ! altitude of the bottom point under u-point
        enddo
      enddo
      do k=kds,kdmax
        do i = itsu,iteu       
          hgtu(i,k,j) =  0.5*(altub(i,k,j)+altub(i,k+1,j)) - altub(i,kds,j)  ! height of the u-point above the ground
        enddo
      enddo
    enddo

! values at v points
    jtsv=ifval(jds.eq.jts,jts+1,jts)  ! staggered direction
    jtev=ifval(jde.eq.jte,jte,jte+1)  ! staggered direction
    itsv=ifval(ids.ge.its,its,its-1)
    itev=ifval(ide.eq.ite,ite,ite+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'v interp at','tile',itsv,itev,jtsv,jtev
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
    do j = jtsv,jtev          
      do k=kds,kdmax+1
        do i = itsv,itev       
          altvb(i,k,j)= 0.5*(altw(i,k,j-1)+altw(i,k,j))      ! altitude of the bottom point under v-point
        enddo
      enddo
      do k=kds,kdmax
        do i = itsv,itev       
          hgtv(i,k,j) =  0.5*(altvb(i,k,j)+altvb(i,k+1,j)) - altvb(i,kds,j)  ! height of the v-point above the ground
        enddo
      enddo
    enddo

#ifdef DEBUG_OUT
        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,altub,'altub',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,altvb,'altvb',id)
        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,hgtu,'hgtu',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,hgtv,'hgtv',id)
#endif
! DME
if (fire_lsm_zcoupling) then
  logfwh = log(fire_lsm_zcoupling_ref)
  fire_wind_height_local = fire_lsm_zcoupling_ref
else
  logfwh = log(fire_wind_height)
  fire_wind_height_local = fire_wind_height
endif

    ! interpolate u, staggered in X

    do j = jtsu,jteu            ! compute on domain by 1 smaller, otherwise z0 and ph not available
!      do i = itsu,iteu        ! compute with halo 2
      do i = itsu,iteu       
        zr = 0.5*(z0(i,j)+z0(i-1,j)) ! interpolated roughness length under this u point
        if(fire_wind_height_local > zr)then       !  
          do k=kds,kdmax
            ht = hgtu(i,k,j)      ! height of this u point above the ground
            if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
              loght = log(ht)
              if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                logz0 = log(zr)
                ua(i,j)= u(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           ! log linear interpolation
                loglast=log(hgtu(i,k-1,j))
                ua(i,j)= u(i,k-1,j) + (u(i,k,j) - u(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 10
            endif
            if(k.eq.kdmax)then                 ! last layer, still not high enough
              ua(i,j)=u(i,k,j) 
            endif
          enddo
10        continue
        else  ! roughness higher than the fire wind height
          ua(i,j)=0.
        endif
      enddo
    enddo

    do j = jtsu,jteu 
      ua(itsu-1,j)=ua(itsu,j)
    enddo


    ! interpolate v, staggered in Y

    do j = jtsv,jtev
      do i = itsv,itev
        zr = 0.5*(z0(i,j-1)+z0(i,j)) ! roughness length under this v point
        if(fire_wind_height_local > zr)then       !  
          do k=kds,kdmax
            ht = hgtv(i,k,j)      ! height of this u point above the ground
            if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
              loght = log(ht)
              if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                logz0 = log(zr)
                va(i,j)= v(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           ! log linear interpolation
                loglast=log(hgtv(i,k-1,j))
                va(i,j)= v(i,k-1,j) + (v(i,k,j) - v(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 11
            endif
            if(k.eq.kdmax)then                 ! last layer, still not high enough
              va(i,j)=v(i,k,j) 
            endif
          enddo
11        continue
        else  ! roughness higher than the fire wind height
          va(i,j)=0.
        endif
      enddo
    enddo

    do i = itsv,itev 
      va(i,jtsv-1)=va(i,jtsv)
    enddo

#ifdef DEBUG_OUT
!   store the output for diagnostics
    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

    call write_array_m(its,ite1,jts,jte,ims,ime,jms,jme,uah,'uah_n',id) ! no reflection
    call write_array_m(its,ite,jts,jte1,ims,ime,jms,jme,vah,'vah_n',id)
#endif

    ips1 = ifval(ips.eq.ids,ips+1,ips)

    call continue_at_boundary(1,1,0., & ! x direction 
       TDIMS,                  &! memory dims atm grid tile
       ids+1,ide,jds,jde, &     ! domain dims - where u defined
       ips1,ipe,jps,jpe, &     ! patch dims 
       itsu,iteu,jtsu,jteu, & ! tile dims - in nonextended direction one beyond if at patch boundary but not domain
       itsou,iteou,jtsou,jteou, & ! out, where set
       ua)                           ! array

    jps1 = ifval(jps.eq.jds,jps+1,jps)

    call continue_at_boundary(1,1,0., & ! y direction 
       TDIMS,                  & ! memory dims atm grid tile
       ids,ide,jds+1,jde, &      ! domain dims - where v wind defined
       ips,ipe,jps1,jpe, &        ! patch dims 
       itsv,itev,jtsv,jtev, & ! tile dims
       itsov,iteov,jtsov,jteov, & ! where set
       va)                           ! array

!   store the output for diagnostics
    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

#ifdef DEBUG_OUT
        call write_array_m(itsou,iteou,jtsou,jteou,TDIMS,ua,'ua',id)
        call write_array_m(itsov,iteov,jtsov,jteov,TDIMS,va,'va',id)
#endif

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    ! don't have all values valid, don't check
     write(msg,12)'atm mesh wind U at',fire_wind_height,' m'
     call print_2d_stats(itsou,iteou,jtsou,jteou,TDIMS,ua,msg)
     write(msg,12)'atm mesh wind V at',fire_wind_height,' m'
     call print_2d_stats(itsov,iteov,jtsov,jteov,TDIMS,va,msg)
12  format(a,f6.2,a)
    call print_2d_stats(its,ite1,jts,jte,ims,ime,jms,jme,uah,'UAH')
    call print_2d_stats(its,ite,jts,jte1,ims,ime,jms,jme,vah,'VAH')
    !call write_array_m(its,ite1,jts,jte,ims,ime,jms,jme,uah,'uah',id)
    !call write_array_m(its,ite,jts,jte1,ims,ime,jms,jme,vah,'vah',id)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

!      ---------------
!     | F | F | F | F |   Example of atmospheric and fire grid with
!     |-------|-------|   ir=jr=4.
!     | F | F | F | F |   Winds are given at the midpoints of the sides of the atmosphere grid,
!     ua------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
!     | F | F | F | F |   This is (1,1) cell of atmosphere grid, and [*] is the (1,1) cell of the fire grid.
!     |---------------|   ua(1,1) <--> uf(0.5,2.5)
!     | * | F | F | F |   va(1,1) <--> vf(2.5,0.5)
!      -------va------    za(1,1) <--> zf(2.5,2.5)
!
!   ^ x2
!   |  --------va(1,2)---------
!   | |            |           |   Example of atmospheric and fire grid with
!   | |            |           |   ir=jr=1.
!   | |          za,zf         |   Winds are given at the midpoints of the sides of the atmosphere grid,
!   | ua(1,1)----uf,vf-----ua(2,1) interpolated to midpoints of the cells of the (the same) fire grid 
!   | |           (1,1)        |   ua(1,1) <--> uf(0.5,1) 
!   | |            |           |   va(1,1) <--> vf(1,0.5) 
!   | |            |           |   za(1,1) <--> zf(1,1)
!   |  --------va(1,1)---------
!   |--------------------> x1 
!
! Meshes are aligned by the lower left cell of the domain. Then in the above figure
! u = node with the ua component of the wind at (ids,jds), midpoint of side
! v = node with the va component of the wind at (ids,jds), midpoint of side
! * = fire grid node at (ifds,jfds)
! z = node with height, midpoint of cell
! 
! ua(ids,jds)=uf(ifds-0.5,jfds+jr*0.5-0.5)         = uf(ifds-0.5,jfds+(jr-1)*0.5)
! va(ids,jds)=vf(ifds+ir*0.5-0.5,jfds-0.5)         = vf(ifds+(ir-1)*0.5,jfds-0.5)
! za(ids,jds)=zf(ifds+ir*0.5-0.5,jfds+jr*0.5-0.5)  = zf(ifds+(ir-1)*0.5,jfds+(jr-1)*0.5)
    
    ! ifts1=max(snode(ifts,ifps,-1),ifds) ! go 1 beyond patch boundary but not at domain boundary
    ! ifte1=min(snode(ifte,ifpe,+1),ifde)
    ! jfts1=max(snode(jfts,jfps,-1),jfds)
    ! jfte1=min(snode(jfte,jfpe,+1),jfde)

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        itsou,iteou,jtsou,jteou,& ! where set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds-0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid     
        uf)                      ! out fire grid

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        itsov,iteov,jtsov,jteov,& ! where set 
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds-0.5, & ! line up by lower left corner of domain
        va,                     & ! in atm grid     
        vf)                      ! out fire grid

! DME here code to extrapolate mid-flame height velocity -> fire_lsm_zcoupling = .true.
if (fire_lsm_zcoupling) then
  do j = jfts,jfte
    do i = ifts,ifte
      uf_temp=uf(i,j)
      vf_temp=vf(i,j)
      wsf=max(sqrt(uf_temp**2.+vf_temp**2.),0.1)
      z0fc=z0f(i,j)
      ust_d=wsf*vk_kappa/log(fire_lsm_zcoupling_ref/z0fc)
      wsf1=(ust_d/vk_kappa)*log((fire_wind_height+z0fc)/z0fc)
      uf(i,j)=wsf1*uf_temp/wsf
      vf(i,j)=wsf1*vf_temp/wsf
    enddo
  enddo
endif


call print_2d_stats_vec(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,vf,'fire wind (m/s)')
! call print_2d_stats_vec(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,uf,vf,'fire wind extended')
#ifdef DEBUG_OUT
        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,'uf1',id)
        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,vf,'vf1',id)
        ! call write_array_m(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,uf,'uf1',id)
        ! call write_array_m(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,vf,'vf1',id)
#endif
return

end subroutine interpolate_atm2fire

!
!***
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
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_fire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!
subroutine set_flags(config_flags)
implicit none
TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
! copy flags from wrf to module_fr_fire_util
! for instructions how to add a flag see the top of module_fr_fire_util.F


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
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
fire_advection          = config_flags%fire_advection
fire_lsm_reinit         = config_flags%fire_lsm_reinit
fire_lsm_reinit_iter    = config_flags%fire_lsm_reinit_iter 
fire_upwinding_reinit   = config_flags%fire_upwinding_reinit 
fire_lsm_band_ngp       = config_flags%fire_lsm_band_ngp
fire_lsm_zcoupling      = config_flags%fire_lsm_zcoupling
fire_lsm_zcoupling_ref  = config_flags%fire_lsm_zcoupling_ref 
fire_viscosity_bg       = config_flags%fire_viscosity_bg 
fire_viscosity_band     = config_flags%fire_viscosity_band 
fire_viscosity_ngp      = config_flags%fire_viscosity_ngp
fire_slope_factor       = config_flags%fire_slope_factor 
fire_fmc_read           = config_flags%fire_fmc_read




end subroutine set_flags

end module module_fr_fire_driver
