!
#define DEBUG_OUT

module module_fr_fire_model

use module_fr_fire_core
use module_fr_fire_util
use module_fr_fire_phys

contains

subroutine fire_model (                    &
    id,                                     & ! unique number for prints and debug
    ifun,                                   & ! what to do see below
    restart,                                &
    need_lfn_update,                          & ! if lfn needs to be synced between tiles
    num_ignitions,                          & ! number of ignitions before advancing
    ifuelread,nfuel_cat0,                   & ! initialize fuel categories
    ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
    ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
    ifps,ifpe,jfps,jfpe,                    & ! patch - nodes owned by this process
    ifts,ifte,jfts,jfte,                    & ! fire tile dims  - this thread
    time_start,dt,                          & ! time and increment
    fdx,fdy,                                & ! fire mesh spacing,
    ignition_line,                          & ! small array of ignition line descriptions
    ignitions_done,ignited_tile,            &
    coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
    lfn,                                & ! state: level function
    lfn_hist,                           & ! PAJ: to init obs fire perimeter.
    fire_is_real_perim,                 & ! PAJ: to init obs fire perimeter.
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front, & ! state
    lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left, area burning
    burnt_area_dt,                          & 
    grnhfx,grnqfx,                          & ! output: heat fluxes
    ros,                                    & ! output: rate of spread
    nfuel_cat,                              & ! fuel data per point 
    fuel_time,                              & ! save derived internal data
    fp, &
    grid, &                
    ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
    ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
    ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  & 
) 

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of any atmospheric model.
! This code was written to conform with the WRF parallelism model, however it
! does not depend on it. It can be called with domain equal to tile.
! Wind and height must be given on 1 more node beyond the domain bounds. 
! The subroutine changes only array entries of the arguments in the tile.
! Upon exit with ifun=2 (time step), lfn_out is to be copied into lfn by the caller.
! When this subroutine is used on separate tiles that make a domain the value, the
! it uses lfn on a strip of width 2 from neighboring tiles.
!
! All computation is done on one tile. 
!
! This subroutine is intended to be called in a loop like
!
! 
! do ifun=1,6 (if initizalize run, otherwise 3,6)
!   start parallel loop over tiles
!       if ifun=1, set z and fuel data
!       if ifun=3, set the wind arrays
!       call fire_model(....)
!   end parallel loop over tiles
!
!   if need_lfn_update, halo exchange on lfn width 2
!   
!   if ifun=0
!       halo exchange on z width 2
!       halo exchange on fuel data width 1
!   endif
!   
!   if ifun=3, halo exchange on winds width 2
!    
! enddo

USE module_domain , only: domain
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_lfn_sub
#endif
USE module_configure, only: grid_config_rec_type

implicit none

!*** arguments

! DME added for halo update
    type(domain) , target :: grid                            
    integer, intent(in):: ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
                          ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
                          ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu
!

! control switches
integer, intent(in) :: id
integer, intent(in) :: ifun                 ! 1 = initialize run pass 1
                                            ! 2 = initialize run pass 2
                                            ! 3 = initialize timestep
                                            ! 4 = do one timestep 
                                            ! 5 = copy timestep output to input
                                            ! 6 = compute output fluxes
logical, intent(in):: restart               ! if true, use existing state
logical, intent(out)::need_lfn_update       ! if true, halo update on lfn afterwards
! scalar data
integer, intent(in) :: num_ignitions        ! number of ignition lines
integer, intent(in) :: ifuelread,nfuel_cat0 ! for set_fire_params
integer, intent(in) :: ifds,ifde,jfds,jfde,&  ! fire domain bounds
        ifps,ifpe,jfps,jfpe                ! patch - nodes owned by this process
integer, intent(in) :: ifts,ifte,jfts,jfte  ! fire tile bounds         
integer, intent(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
! array data
type(ignition_line_type), dimension (num_ignitions), intent(in):: ignition_line ! descriptions of ignition lines
integer, intent(out):: ignited_tile(num_ignitions),ignitions_done
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       !  node coordinates  
real, intent(in):: unit_xf,unit_yf          !  coordinate units in m
    
! state
    ! PAJ: to init obs fire perimeter
  real, intent(in), dimension(ifms:ifme,jfms:jfme):: lfn_hist   
  logical, intent(in) :: fire_is_real_perim

REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn   , &                               ! level function: fire is where lfn<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant

REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front           ! level function stages

REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    fire_area                               ! fraction of each cell burning
    
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    burnt_area_dt 

! output
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    lfn_out, &                              !                              
    grnhfx,grnqfx, &                        ! heat fluxes J/m^2/s  (cell)             
    ros                                     ! output: rate of spread
 
! constant arrays - set at initialization
real, intent(inout), dimension(ifms:ifme, jfms:jfme)::nfuel_cat ! cell based, data, constant
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
type(fire_params),intent(inout)::fp

!*** local

integer :: xifms,xifme,xjfms,xjfme  ! memory bounds for pass-through arguments to normal spread
real, dimension(ifts:ifte,jfts:jfte)::fuel_frac_burnt,fuel_frac_end
integer::ignited,ig,i,j,itso,iteo,jtso,jteo
real::tbound,err,erri,errj,maxgrad,grad,tfa,thf,mhf,tqf,mqf,aw,mw
character(len=128)::msg
logical:: freeze_fire
integer:: stat_lev=1

    ! PAJ:
  real :: start_time_ig, end_time_ig
  real, parameter :: EPSILON = 0.00001

!*** executable

call check_mesh_2dim(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme)

xifms=ifms  ! dimensions for the include file
xifme=ifme
xjfms=jfms
xjfme=jfme


! init flags
need_lfn_update=.false.
ignitions_done=0
freeze_fire = fire_const_time > 0. .and. time_start < fire_const_time

if(ifun.eq.1)then       ! do nothing, init pass 1 is outside only
elseif(ifun.eq.2)then   
        ! initialize all arrays that the model will not change later

        ! assuming halo on zsf done
        ! extrapolate on 1 row of cells beyond the domain boundary
        ! including on the halo regions 

        call continue_at_boundary(1,1,0., & ! do x direction or y direction
            ifms,ifme,jfms,jfme,           &                ! memory dims
            ifds,ifde,jfds,jfde, &                     ! domain dims 
            ifps,ifpe,jfps,jfpe, &            ! patch dims - winds defined up to +1
            ifts,ifte,jfts,jfte, &                ! tile dims
            itso,iteo,jtso,jteo, &              ! where set now
            fp%zsf)                               ! array

!       compute the gradients once for all
        err=0.
        maxgrad=0.
        do j=jfts,jfte
            do i=ifts,ifte
                erri = fp%dzdxf(i,j) - (fp%zsf(i+1,j)-fp%zsf(i-1,j))/(2.*fdx)
                errj = fp%dzdyf(i,j) - (fp%zsf(i,j+1)-fp%zsf(i,j-1))/(2.*fdy)
                err=max(err,abs(erri),abs(errj))
                grad=sqrt(fp%dzdxf(i,j)**2+fp%dzdyf(i,j)**2)
                maxgrad=max(maxgrad,grad)
            enddo
        enddo
!$OMP CRITICAL(FIRE_MODEL_CRIT)
        write(msg,*)'max gradient ',maxgrad,' max error against zsf',err
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
        call message(msg)

        if(.not.restart)call set_nfuel_cat( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            ifuelread,nfuel_cat0,&
            fp%zsf,nfuel_cat)            ! better not use the extrapolated zsf!!

        ! uses nfuel_cat to set the other fuel data arrays
        ! needs zsf on halo width 1 to compute the terrain gradient
        if(.not.restart)call set_fire_params(   & 
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,nfuel_cat0,  &
            nfuel_cat,fuel_time, &
            fp  &
)

        ! initialize model state to no fire
        if(.not.restart)then
            call init_no_fire  ( &
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,time_start,  &
            fuel_frac,fire_area,lfn,tign)
            
            need_lfn_update=.true. ! because we have set lfn 

        endif

elseif(ifun.eq.3)then   ! ignition if so specified

    
elseif (ifun.eq.4) then  ! do the timestep

    if(fire_print_msg.ge.stat_lev)then
      aw=fun_real(RNRM_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fp%vx,fp%vy)/((ifde-ifds+1)*(jfde-jfds+1))
      mw=fun_real(RNRM_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fp%vx,fp%vy)
!$OMP MASTER 
      write(msg,91)time_start,'Average wind        ',aw,'m/s'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Maximum wind        ',mw,'m/s'
      call message(msg,stat_lev)
!$OMP END MASTER 
    endif

!   compute fuel fraction at start
!    call fuel_left( &
!        ifms,ifme,jfms,jfme, &
!        ifts,ifte,jfts,jfte, &
!        ifms,ifme,jfms,jfme, &
!        lfn,tign,fuel_time,time_start,fuel_frac,fire_area) ! fuel frac is shared

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   fuel_frac,'model: fuel_frac start')

    ! advance the model from time_start to time_start+dt
    ! return the fuel fraction burnt this call in each fire cell
    ! will call module_fr_fire_speed::normal_spread for propagation speed
    ! We cannot simply compute the spread rate here because that will change with the
    ! angle of the wind and the direction of propagation, thus it is done in subroutine
    ! normal_spread at each fire time step. Instead, we pass arguments that 
    ! the speed function may use as fp. 

!   propagate level set function in time
!   set lfn_out tign
!   lfn does not change, tign has no halos

    if(.not. freeze_fire)then

         call prop_ls_rk3(id,                           &
             ifds,ifde,jfds,jfde,                       & 
             ifms,ifme,jfms,jfme,                       &
             ifps,ifpe,jfps,jfpe,                       &          
             ifts,ifte,jfts,jfte,                       &
             time_start,dt,fdx,fdy,tbound,              &
             lfn,                                       &
             lfn_0,lfn_1,lfn_2,                         & 
             lfn_out,tign,ros, fp,                      &
             grid,                                      & 
             ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
             ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
             ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  &
         ) 

       call tign_update(ifts,ifte,jfts,jfte, & 
                        ifms,ifme,jfms,jfme, &                    
                        ifds,jfds,ifde,jfde, &
                        time_start,dt,       &  
                        lfn,lfn_out,tign     &             
                       )

       call calc_flame_length(ifts,ifte,jfts,jfte, & 
                              ifms,ifme,jfms,jfme, &
                              ros,fp%iboros,flame_length,ros_front,fire_area)

       if (fire_lsm_reinit) then ! DME added call to reinitialize level-set function
          
           call reinit_ls_rk3(id,                                    &
                          ifts,ifte,jfts,jfte,                       &                  
                          ifms,ifme,jfms,jfme,                       &                     
                          ifds,ifde,jfds,jfde,                       &                     
                          ifps,ifpe,jfps,jfpe,                       &                     
                          time_start,dt,fdx,fdy,                     &               
                          lfn,                                       &
                          lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,         &
                          lfn_out,tign,                              &                      
                          grid,                                      &          
                          ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
                          ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
                          ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  &
                         )
       endif ! fire_lsm_reinit

    else
        call message('fire_model: EXPERIMENTAL: skipping fireline propagation')

    endif
    
elseif (ifun.eq.5) then ! copy the result of timestep back to input
    ! this cannot be done in the time step itself because of race condition
    ! some thread may still be using lfn as input in their tile halo

    if (.not. freeze_fire) then

       do j=jfts,jfte
           do i=ifts,ifte
               lfn(i,j)=lfn_out(i,j)
               ! if want to try timestep again treat tign the same way here
               ! even if tign does not need a halo
           enddo
       enddo

    endif

    ! check for ignitions
!paj
  ig = 1
  start_time_ig = ignition_line(ig)%start_time 
  end_time_ig  = ignition_line(ig)%end_time

  if ( fire_is_real_perim .and. time_start >= start_time_ig .and. time_start < start_time_ig + dt) then
    ignited = 0
    do j = jfts, jfte
      do i = ifts, ifte
        lfn(i, j) = lfn_hist(i, j)
        if (abs(lfn(i, j)) < EPSILON) then
          tign(i, j)= time_start
          ignited = ignited + 1
        end if
      enddo
    enddo
  elseif (.not. fire_is_real_perim) then
    do ig = 1,num_ignitions
    
!  for now, check for ignition every time step...
!        if(ignition_line(ig)%end_time>=time_start.and.ignition_line(ig)%start_time<time_start+dt)then 
            call ignite_fire(                             &
                ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                      &
                ifts,ifte,jfts,jfte,                      &
                ignition_line(ig),                        &
                time_start,time_start+dt,                 &
                coord_xf,coord_yf,unit_xf,unit_yf,        & 
                lfn,tign,ignited)

            ignitions_done=ignitions_done+1
            ignited_tile(ignitions_done)=ignited
                
!            need_lfn_update=.true. ! if ignition, lfn changed
#ifdef DEBUG_OUT    
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,lfn,'lfn_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_xf,'coord_xf_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_yf,'coord_yf_ig',id)
#endif
!        endif
        
    enddo
 end if
            

    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, &
                   lfn,'fire_model: lfn out')

    
    need_lfn_update=.true. ! duh

elseif (ifun.eq.6) then ! timestep postprocessing

  if(.not. freeze_fire)then

    ! compute the heat fluxes from the fuel burned
    ! needs lfn and tign from neighbors so halo must be updated before
    call fuel_left(&
        ifms,ifme,jfms,jfme, &
        ifts,ifte,jfts,jfte, &
        ifts,ifte,jfts,jfte, &
        lfn,tign,fuel_time,time_start+dt,fuel_frac_end,fire_area) !fuel_frac_end is private and tile based

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_end,'model: fuel_frac end')
    
    do j=jfts,jfte
        do i=ifts,ifte
            fuel_frac_burnt(i,j)=fuel_frac(i,j)-fuel_frac_end(i,j) ! fuel lost this timestep
            burnt_area_dt(i,j)=fuel_frac_burnt(i,j)
            fuel_frac(i,j)=fuel_frac_end(i,j) ! copy new value to state array
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_burnt,'model: fuel_frac burned')
        
    call heat_fluxes(dt,                          &
        ifms,ifme,jfms,jfme,                      &
        ifts,ifte,jfts,jfte,                      &
        ifts,ifte,jfts,jfte,                      &  ! fuel_frac_burned is tile dimensioned
        fp%fgip,                                     &
        fuel_frac_burnt,                          & !
        grnhfx,grnqfx)                              !out

    if(fire_print_msg.ge.stat_lev)then
      tfa=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fire_area,fire_area) * fdx * fdy
      thf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnhfx,grnhfx) * fdx * fdy
      mhf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnhfx,grnhfx) 
      tqf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnqfx,grnqfx) * fdx * fdy
      mqf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnqfx,grnqfx) 
!$OMP MASTER 
      write(msg,91)time_start,'Fire area           ',tfa,'m^2'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Heat output         ',thf,'W'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Max heat flux       ',mhf,'W/m^2'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Latent heat output  ',tqf,'W'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Max latent heat flux',mqf,'W/m^2'
      call message(msg,stat_lev)
!$OMP END MASTER
91  format('Time ',f11.3,' s ',a,e12.3,1x,a)
    endif
        

  else
     call message('fire_model: EXPERIMENTAL: skipping fuel burnt computation')

     if (fire_const_grnhfx >= 0. .and. fire_const_grnqfx >= 0.) then

!$OMP CRITICAL(FIRE_MODEL_CRIT)
        write(msg,'(a,2e12.3,a)')'fire_model: EXPERIMENTAL output constant heat flux', &
           fire_const_grnhfx, fire_const_grnqfx, ' W/s'
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
        call message(msg)
        
        do j=jfts,jfte
            do i=ifts,ifte
                grnhfx(i,j)=fire_const_grnhfx
                grnqfx(i,j)=fire_const_grnqfx
            enddo
        enddo

      endif

   endif

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   grnhfx,'model: heat flux(J/m^2/s)')

else
!$OMP CRITICAL(FIRE_MODEL_CRIT)
    write(msg,*)'fire_model: bad ifun=',ifun
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
    call crash(msg)
endif

end subroutine fire_model

!
!*****************
!
            
end module module_fr_fire_model
