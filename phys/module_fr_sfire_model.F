!
!*** Jan Mandel October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!

#define DEBUG_OUT

module module_fr_sfire_model

use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_phys

contains

subroutine sfire_model (                    &
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
    ignition_start_x,ignition_start_y,      & ! ignition - small arrays
    ignition_end_x,ignition_end_y,          &
    ignition_radius,                        &
    ignition_time,                          &
    ignitions_done,ignited_tile,            &
    coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
    zsf,                                    & ! terrain height (for gradient)
    vx,vy,                                  & ! input: wind
    lfn,lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left, area burning
    grnhfx,grnqfx,                          & ! output: heat fluxes
    ros,                                    & ! output: rate of spread
    nfuel_cat,                              & ! fuel data per point 
    fuel_time,                              & ! save derived internal data
    bbb,betafl,phiwc,r_0,fgip,ischap &
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
!       call sfire_model(....)
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

implicit none

!*** arguments

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
integer, intent(in) :: num_ignitions        ! number of ignition locations/times 
integer, intent(in) :: ifuelread,nfuel_cat0 ! for set_fire_params
integer, intent(in) :: ifds,ifde,jfds,jfde,&  ! fire domain bounds
        ifps,ifpe,jfps,jfpe                ! patch - nodes owned by this process
integer, intent(in) :: ifts,ifte,jfts,jfte  ! fire tile bounds
integer, intent(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
! array data
real, dimension(num_ignitions), intent(in):: &   
    ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius, & ! start, end, radius, time
    ignition_time                           !  of ignition lines
integer, intent(out):: ignited_tile(num_ignitions),ignitions_done
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       !  node coordinates  
real, intent(in):: unit_xf,unit_yf          !  coordinate units in m
REAL, INTENT(in), dimension(ifms:ifme,jfms:jfme):: & 
    vx,vy                                   ! wind m/s (node based), data, variable
    
! state
REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme):: &
    zsf,   &                                ! terrain height, node based, data, constant after extr
    lfn   , &                               ! level function: fire is where lfn<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant

REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    fire_area                               ! fraction of each cell burning
    
! output
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    lfn_out, &                              !                              
    grnhfx,grnqfx, &                        ! heat fluxes J/m^2/s  (cell)             
    ros                                     ! output: rate of spread
 
! constant arrays - set at initialization
real, intent(inout), dimension(ifms:ifme, jfms:jfme)::nfuel_cat ! cell based, data, constant
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: bbb,betafl,phiwc,r_0 ! (node) spread formula coefficients
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fgip                 ! (cell) init mass of surface fuel (kg/m^2)
integer,intent(inout),dimension(ifms:ifme,jfms:jfme):: ischap            ! (node) .ne.0 if chapparal

!*** local

integer :: xifms,xifme,xjfms,xjfme  ! memory bounds for pass-through arguments to normal spread
real, dimension(ifts:ifte,jfts:jfte)::fuel_frac_burnt,fuel_frac_end
integer::ignited,ig,i,j
real::tbound
character(len=128)::msg
logical:: freeze_fire

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
            zsf)                               ! array

        if(.not.restart)call set_nfuel_cat( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            ifuelread,nfuel_cat0,&
            zsf,nfuel_cat)            ! better not use the extrapolated zsf!!

        ! uses nfuel_cat to set the other fuel data arrays
        ! needs zsf on halo width 1 to compute the terrain gradient
        if(.not.restart)call set_fire_params(   & 
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,nfuel_cat0,  &
            nfuel_cat,fuel_time &
#           include "fr_sfire_params_args.h" 
)

        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,zsf,'model: terrain height')        
                                        
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

    ! check for ignitions
    do ig = 1,num_ignitions
    
        if(ignition_time(ig)>=time_start.and.ignition_time(ig)<time_start+dt)then 
            call ignite_fire(                             &
                ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                      &
                ifts,ifte,jfts,jfte,                      &
                ignition_start_x(ig),ignition_start_y(ig),&
                ignition_end_x(ig),ignition_end_y(ig),    &
                ignition_radius(ig),                      &
                ignition_time(ig),                        &  
                coord_xf,coord_yf,unit_xf,unit_yf,        & 
                lfn,tign,ignited)
            ignitions_done=ignitions_done+1
            ignited_tile(ignitions_done)=ignited
                
            need_lfn_update=.true. ! if ignition, lfn changed
#ifdef DEBUG_OUT    
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,lfn,'lfn_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_xf,'coord_xf_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_yf,'coord_yf_ig',id)
#endif
        endif
        
    enddo
    
elseif (ifun.eq.4) then  ! do the timestep

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
    ! will call module_fr_sfire_speed::normal_spread for propagation speed
    ! We cannot simply compute the spread rate here because that will change with the
    ! angle of the wind and the direction of propagation, thus it is done in subroutine
    ! normal_spread at each fire time step. Instead, we pass arguments that 
    ! the speed function may use. The include is to guarantee this is done consistently
    ! over the call chain.

!   propagate level set function in time
!   set lfn_out tign
!   lfn does not change, tign has no halos

    if(.not. freeze_fire)then

    call prop_ls(id,     &
        ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
        ifms,ifme,jfms,jfme,                      &
        ifps,ifpe,jfps,jfpe, &                ! patch - nodes owned by this process
        ifts,ifte,jfts,jfte,                      &
        time_start,dt,fdx,fdy,tbound,  &
        lfn,lfn_out,tign,ros &
#       include "fr_sfire_params_args.h" 
    ) 

    else
        call message('sfire_model: EXPERIMENTAL: skipping fireline propagation')

    endif
    
elseif (ifun.eq.5) then ! copy the result of timestep back to input
    ! this cannot be done in the time step itself because of race condition
    ! some thread may still be using lfn as input in their tile halo

    if(.not. freeze_fire)then
    
    do j=jfts,jfte
        do i=ifts,ifte
            lfn(i,j)=lfn_out(i,j)
            ! if want to try timestep again treat tign the same way here
            ! even if tign does not need a halo
        enddo
    enddo

    endif
            
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, &
                   lfn,'sfire_model: lfn out')

    
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
        fgip,                                     &
        fuel_frac_burnt,                          & !
        grnhfx,grnqfx)                              !out

    else
        call message('sfire_model: EXPERIMENTAL: skipping fuel burnt computation')

        if (fire_const_grnhfx >= 0. .and. fire_const_grnqfx >= 0.) then

        write(msg,'(a,2e12.3,a)')'sfire_model: EXPERIMENTAL output constant heat flux', &
           fire_const_grnhfx, fire_const_grnqfx, ' W/s'
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
    write(msg,*)'sfire_model: bad ifun=',ifun
    call crash(msg)
endif

end subroutine sfire_model

!
!*****************
!
            
end module module_fr_sfire_model
