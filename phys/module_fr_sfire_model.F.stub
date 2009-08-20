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
    coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
    zsf,                                    & ! terrain height (for gradient)
    vx,vy,                                  & ! input: wind
    lfn,lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left, area burning
    grnhfx,grnqfx,                          & ! output: heat fluxes
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
    grnhfx,grnqfx                           ! heat fluxes J/m^2/s  (cell)             
 
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


end subroutine sfire_model

!
!*****************
!
            
end module module_fr_sfire_model
