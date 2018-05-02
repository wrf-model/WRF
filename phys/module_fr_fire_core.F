!
#define DEBUG_OUT
#define DEBUG_PRINT
!#define FUEL_LEFT
!#define DEBUG_OUT_FUEL_LEFT    

module module_fr_fire_core

use module_fr_fire_phys, only: fire_params , fire_ros
use module_fr_fire_util

! The mathematical core of the fire spread model. No physical constants here.
! 
! subroutine fire_core: only this routine should be called from the outside.
! subroutine fuel_left:  compute remaining fuel from time of ignition.
! subroutine prop_ls: propagation of curve in normal direction.

! describe one ignition line
type ignition_line_type
  REAL  ros, &          ! subscale rate of spread during the ignition process
        stop_time, &    ! when the ignition process stops from ignition start (s)
        wind_red,  &    ! wind reduction factor at the ignition line
        wrdist,   &     ! distance from the ignition line when the wind reduction stops
        wrupwind, &     ! use distance interpolated between 0. = nearest 1. = upwind
        start_x, &      ! x coordinate of the ignition line start point (m, or long/lat)
        start_y, &      ! y coordinate of the ignition line start point
        end_x, &        ! x coordinate of the ignition line end point
        end_y, &        ! y coordinate of the ignition line end point
        start_time, &   ! ignition time for the start point from simulation start (s)
        end_time, &     ! ignition time for the end poin from simulation start (s)
        radius          ! all within this radius ignites immediately
end type ignition_line_type

contains

!
!****************************************
!
    
subroutine init_no_fire(&
    ifds,ifde,jfds,jfde, &
    ifms,ifme,jfms,jfme, &
    ifts,ifte,jfts,jfte, &
    fdx,fdy,time_now,    & ! scalars in
    fuel_frac,fire_area,lfn,tign)    ! arrays out            
implicit none
             
!*** purpose: initialize model to no fire

!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(in) :: fdx,fdy,time_now        ! mesh spacing, time
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   fuel_frac,fire_area,lfn,tign       ! model state

!*** calls
intrinsic epsilon
                                                
!*** local
integer:: i,j
real lfn_init,time_init

lfn_init = 2*max((ifde-ifds+1)*fdx,(jfde-jfds+1)*fdy)      ! more than domain diameter
time_init=time_now + max(time_now,1.0)*epsilon(time_now) ! a bit in future
 
do j=jfts,jfte
    do i=ifts,ifte
        fuel_frac(i,j)=1.          ! fuel at start is 1 by definition
        fire_area(i,j)=0.          ! nothing burning
        tign(i,j) = time_init      ! ignition in future
        lfn(i,j) = lfn_init        ! no fire 
    enddo
enddo
call message('init_no_fire: state set to no fire')

end subroutine init_no_fire

!
!******************
!
 

subroutine ignite_fire( ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
                        ifms,ifme,jfms,jfme,                      &
                        ifts,ifte,jfts,jfte,                      &
                        ignition_line,                            &
                        start_ts,end_ts,                    &
                        coord_xf,coord_yf,                &     
                        unit_xf,unit_yf,                  &
                        lfn,tign,ignited)
implicit none

!*** purpose: ignite a circular/line/prescribed fire 

!*** description
! ignite fire in the region within radius r from the line (sx,sy) to (ex,ey).
! the coordinates of nodes are given as the arrays coord_xf and coord_yf
! r is given in m
! one unit of coord_xf is unit_xf m 
! one unit of coord_yf is unit_yf m 
! so a node (i,j) will be ignited iff for some (x,y) on the line
! || ( (coord_xf(i,j) - x)*unit_xf , (coord_yf(i,j) - y)*unit_yf ) || <= r 


!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
type(ignition_line_type), intent(in):: ignition_line    ! description of the ignition line
real, intent(in):: start_ts,end_ts          ! the time step start and end 
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       !  node coordinates  
real, intent(in):: unit_xf,unit_yf          !  coordinate units in m
real, intent(inout), dimension (ifms:ifme,jfms:jfme) :: & 
                   lfn, tign                ! level function, ignition time (state)
integer, intent(out):: ignited              ! number of nodes newly ignited
                        
!*** local
integer:: i,j
real::lfn_new,time_ign,ax,ay,rels,rele,d
real:: sx,sy                    ! start of ignition line, from lower left corner
real:: ex,ey                    ! end of ignition line, or zero
real:: st,et                    ! start and end of time of the ignition line
character(len=128):: msg
real::cx2,cy2,dmax,axmin,axmax,aymin,aymax,dmin
real:: start_x,start_y          ! start of ignition line, from lower left corner
real:: end_x,end_y              ! end of ignition line, or zero
real:: radius                   ! all within the radius of the line will ignite
real:: start_time,end_time      ! the ignition time for the start and the end of the line
real:: ros,tos                  ! ignition rate and time of spread

!*** executable

! copy ignition line fields to local variables
start_x    = ignition_line%start_x ! x coordinate of the ignition line start point (m, or long/lat)
start_y    = ignition_line%start_y ! y coordinate of the ignition line start point
end_x      = ignition_line%end_x   ! x coordinate of the ignition line end point
end_y      = ignition_line%end_y   ! y coordinate of the ignition line end point
start_time = ignition_line%start_time ! ignition time for the start point from simulation start (s)
end_time   = ignition_line%end_time! ignition time for the end poin from simulation start (s)
radius     = ignition_line%radius  ! all within this radius ignites immediately
ros        = ignition_line%ros     ! rate of spread
tos        = radius/ros            ! time of spread to the given radius
st         = start_time            ! the start time of ignition considered in this time step
et         = min(end_ts,end_time)  ! the end time of the ignition segment in this time step

! this should be called whenever (start_ts, end_ts) \subset (start_time, end_time + tos) 
if(start_ts>et+tos .or. end_ts<st)return   ! too late or too early, nothing to do
!print *, 'IGNITING'
if(start_time < end_time)then  ! we really want to test start_time .ne. end_time, but avoiding test of floats on equality
        ! segment of nonzero length
        !rels =  (st - start_time) / (end_time - start_time)  ! relative position of st in the segment (start,end)
	!sx = start_x + rels * (end_x - start_x)
	!sy = start_y + rels * (end_y - start_y)
!paj        rels = 0.
        sx = start_x
        sy = start_y
        rele =  (et - start_time) / (end_time - start_time)    ! relative position of et in the segment (start,end)
	ex = start_x + rele * (end_x - start_x)
	ey = start_y + rele * (end_y - start_y)
else
        ! just a point
!paj        rels = 0.
!paj        rele = 1.
	sx = start_x
	sy = start_y
	ex = end_x
	ey = end_y
endif


cx2=unit_xf*unit_xf
cy2=unit_yf*unit_yf

axmin=coord_xf(ifts,jfts)
aymin=coord_yf(ifts,jfts)
axmax=coord_xf(ifte,jfte)
aymax=coord_yf(ifte,jfte)
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2f11.6,a,2f11.6)')'IGN from ',sx,sy,' to ',ex,ey
call message(msg)
write(msg,'(a,2f10.2,a,2f10.2,a)')'IGN timestep [',start_ts,end_ts,'] in [',start_time,end_time,']'
call message(msg)
write(msg,'(a,2g13.6,a,2g13.6)')'IGN tile coord from  ',axmin,aymin,' to ',axmax,aymax
call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
ignited=0
dmax=0
dmin=huge(dmax)
11      format('IGN ',6(a,g17.7,1x)) 
12      format('IGN ',4(a,2g13.7,1x))
do j=jfts,jfte   
    do i=ifts,ifte
        ax=coord_xf(i,j)
        ay=coord_yf(i,j)

        ! get d= distance from the nearest point on the ignition segment
        ! and time_ign = the ignition time there
        call nearest(d,time_ign,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2)
        dmax=max(d,dmax)
        dmin=min(d,dmin)

        lfn_new=d - min( radius, ros*(end_ts - time_ign) )  ! lft at end_ts
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN1 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg)
            write(msg,*)'IGN2 i,j=',i,j,' lfn_new= ',lfn_new, ' time_ign= ',time_ign,' d=',d
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
        if(.not.lfn_new>0.) then
            ignited=ignited+1   ! count
        endif
        if(lfn(i,j)>0. .and. .not. lfn_new > 0.) then 
            tign(i,j)=time_ign + d/ros  ! newly ignited now
            if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,2g13.6,a,f10.2,a,2f10.2,a)')'IGN ignited cell ',i,j,' at',ax,ay, &
                    ' time',tign(i,j),' in [',start_ts,end_ts,']'
                call message(msg)
                write(msg,'(a,g10.3,a,f10.2,a,2f10.2,a)')'IGN distance',d,' from ignition line at',time_ign,' in [',st,et,']'
                call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
            endif
            if(tign(i,j) < start_ts .or. tign(i,j) > end_ts )then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,f11.6,a,2f11.6,a)')'WARNING ',i,j, &
                ' fixing ignition time ',tign(i,j),' outside of the time step [',start_ts,end_ts,']'
                call message (msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
                tign(i,j) = min(max(tign(i,j),start_ts),end_ts)
            endif
        endif
        lfn(i,j)=min(lfn(i,j),lfn_new)  ! update the level set function
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN3 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
    enddo
enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2g13.2,a,g10.2,a,g10.2)')'IGN units ',unit_xf,unit_yf,' m max dist ',dmax,' min',dmin
call message(msg)
write(msg,'(a,f6.1,a,f8.1,a,i10)')'IGN radius ',radius,' time of spread',tos,' ignited nodes',ignited
call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)

return
99 continue

end subroutine ignite_fire

! called from the inside of a loop, inline if possible
!DEC$ ATTRIBUTES FORCEINLINE
SUBROUTINE nearest(d,t,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2)
        implicit none
!***    arguments
        real, intent(out):: d,t
        real, intent(in):: ax,ay,sx,sy,st,ex,ey,et,cx2,cy2
        ! input:
        ! ax, ay       coordinates of point a
        ! sx,sy,ex,ey  coordinates of endpoints of segment [x,y]
        ! st,et        values at the endpoints x,y
        ! cx2,cy2      x and y unit squared for computing distance 

        ! output
        ! d            the distance of a and the nearest point z on the segment [x,y]
        ! t            linear interpolation from the values st,et to the point z
        !
        ! method: compute d as the distance (ax,ay) from the midpoint (mx,my)
        ! minus a correction (because of rounding errors): |a-c|^2 = |a-m|^2 - |m-c|^2
        ! when |m-c| >= |s-e|/2 the nearest point is one of the endpoints
        ! the computation work also for the case when s=e exactly or approximately 
        !
        !
        !           a    
        !          /| \
        !     s---m-c--e
        !
        ! |m-c| = |a-m| cos (a-m,e-s) 
        !       = |a-m| (a-m).(e-s))/(|a-m|*|e-s|)
!***    local
        real:: mx,my,dam2,dames,am_es,cos2,dmc2,mcrel,mid_t,dif_t,des2,cx,cy
        character(len=128):: msg
!***    executable

11      format('IGN ',6(a,g17.7,1x))
12      format('IGN ',4(a,2g13.7,1x))

        ! midpoint m = (mx,my)
        mx = (sx + ex)*0.5
        my = (sy + ey)*0.5
        dam2=(ax-mx)*(ax-mx)*cx2+(ay-my)*(ay-my)*cy2      ! |a-m|^2
        des2 = (ex-sx)*(ex-sx)*cx2+(ey-sy)*(ey-sy)*cy2          ! des2 = |e-s|^2
        dames = dam2*des2
        am_es=(ax-mx)*(ex-sx)*cx2+(ay-my)*(ey-sy)*cy2       ! am_es = (a-m).(e-s)
        if(dames>0)then
            cos2 = (am_es*am_es)/dames                  ! cos2 = cos^2 (a-m,e-s)
        else ! point a already is the midpoint
            cos2 = 0.
        endif
        dmc2 = dam2*cos2                                ! dmc2 = |m-c|^2
        if(4.*dmc2 < des2)then                          ! if |m-c|<=|e-s|/2
            ! d = sqrt(max(dam2 - dmc2,0.))               ! d=|a-m|^2 - |m-c|^2, guard rounding
            mcrel = sign(sqrt(4.*dmc2/des2),am_es)      ! relative distance of c from m 
        elseif(am_es>0)then                             ! if cos > 0, closest is e
            mcrel = 1.0 
        else                                            ! closest is s
            mcrel = -1.0
        endif
	cx = (ex + sx)*0.5 + mcrel*(ex - sx)*0.5     ! interpolate to c by going from m
	cy = (ey + sy)*0.5 + mcrel*(ey - sy)*0.5     ! interpolate to c by going from m
        d=sqrt((ax-cx)*(ax-cx)*cx2+(ay-cy)*(ay-cy)*cy2) ! |a-c|^2
	t = (et + st)*0.5 + mcrel*(et - st)*0.5     ! interpolate to c by going from m
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,12)'find nearest to [',ax,ay,'] from [',sx,sy,'] [',ex,ey,']' ! DEB
            call message(msg)
            write(msg,12)'end times',st,et,' scale squared',cx2,cy2 ! DEB
            call message(msg)
            write(msg,11)'nearest at mcrel=',mcrel,'from the midpoint, t=',t ! DEB
            call message(msg)
            write(msg,12)'nearest is [',cx,cy,'] d=',d ! DEB
            call message(msg)
            write(msg,11)'dam2=',dam2,'des2=',des2,'dames=',dames
            call message(msg)
            write(msg,11)'am_es=',am_es,'cos2=',cos2,'dmc2=',dmc2 ! DEB
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
END SUBROUTINE nearest


!
!**********************
!            

subroutine fuel_left(&
    ims,ime,jms,jme, &
    its,ite,jts,jte, &
    ifs,ife,jfs,jfe, &
    lfn, tign, fuel_time, time_now, fuel_frac, fire_area)
implicit none

!*** purpose: determine fraction of fuel remaining
!*** NOTE: because variables are cell centered, need halo/sync width 1 before

!*** arguments

integer, intent(in) :: its,ite,jts,jte,ims,ime,jms,jme,ifs,ife,jfs,jfe
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: time_now
real, intent(out), dimension(ifs:ife,jfs:jfe)::fuel_frac
real, intent(out), dimension(ims:ime,jms:jme):: fire_area

! ims,ime,jms,jme   in   memory dimensions
! its,ite,jts,jte   in   tile dimensions (cells where fuel_frac computed)
! ifs,ife,jfs,jfe   in   fuel_frac memory dimensions
! lfn               in   level function, at nodes at midpoints of cells
! tign              in   ignition time, at nodes at nodes at midpoints of cells
! fuel_time         in   time constant of fuel, per cell
! time_now          in   time now
! fuel_frac         out  fraction of fuel remaining, per cell
! fire_area         out  fraction of cell area on fire

!*** local

integer::i,j,ir,jr,icl,jcl,isubcl,jsubcl,i2,j2,ii,jj
real::fmax,frat,helpsum1,helpsum2,fuel_left_ff,fire_area_ff,rx,ry,tignf(2,2)
! help variables instead of arrays fuel_left_f and fire_area_f 
real::lffij,lffi1j,lffij1,lffi1j1,tifij,tifi1j,tifij1,tifi1j1,tx,ty,txx,tyy
! variables for calculation instead of lff(i,j) and tif(i,j)is lffij,tifij etc..#define IFCELLS (ite-its+1)*fuel_left_irl
#define JFCELLS (jte-jts+1)*fuel_left_jrl
character(len=128)::msg
integer::m,omp_get_thread_num
     

! refinement
ir=fuel_left_irl
jr=fuel_left_jrl

if ((ir.ne.2).or.(jr.ne.2)) then 
   call crash('fuel_left: ir.ne.2 or jr.ne.2 ')
endif

rx=1./ir 
ry=1./jr

! interpolate level set function to finer grid
#ifdef DEBUG_OUT_FUEL_LEFT    
    call write_array_m(1,IFCELLS+1,1,JFCELLS+1,1,IFCELLS+1,1,JFCELLS+1,lff,'lff',0)
    call write_array_m(1,IFCELLS+1,1,JFCELLS+1,1,IFCELLS+1,1,JFCELLS+1,tif,'tif',0)
#endif

!
! example for ir=2:
!
!                     |      coarse cell      |
!      its-1                     its                                   ite             ite+1
! -------X------------|-----.-----X-----.-----|--........----|----------X----------|---------X
!           fine node 1           2           3                   2*(ite-its+1) 
!                fine cell  1           2                             cell 2*(ite-its+1)



!  Loop over cells in Tile 
!  Changes made by Volodymyr Kondratenko 09/24/2009
do icl=its,ite
  do jcl=jts,jte
    helpsum1=0
    helpsum2=0
!   Loop over subcells in cell #(icl,jcl)
    do isubcl=1,ir
      do jsubcl=1,jr 
        i=(icl-its)*ir+isubcl
        j=(jcl-jts)*jr+jsubcl

!       Direct calculation tif and lff, avoiding arrays, just for case ir=jr=2
        if ((isubcl.eq.1).and.(jsubcl.eq.1)) then
           i2=icl-1
           j2=jcl-1
           ty=0.5
           tx=0.5
           tyy=1.0
           txx=1.0
        else if ((isubcl.eq.2).and.(jsubcl.eq.1)) then
           i2=icl
           j2=jcl-1
           ty=0.5
           tx=0
           tyy=1.0
           txx=0.5
        else if ((isubcl.eq.1).and.(jsubcl.eq.2)) then
           i2=icl-1
           j2=jcl
           tx=0.5
           ty=0
           txx=1.0
           tyy=0.5
        else if ((isubcl.eq.2).and.(jsubcl.eq.2)) then
           i2=icl
           j2=jcl
           tx=0
           ty=0
           txx=0.5
           tyy=0.5
        else
           call crash('fuel_left: isubcl,jsubcl should be only 1 or 2')
        endif 

! calculation lff and tif in 4 endpoints of subcell                    
        lffij=                             &    
                  (1-tx)*(1-ty)*lfn(i2,j2)      &
             +    (1-tx)*ty  *lfn(i2,j2+1)      &
             +     tx*(1-ty)*lfn(i2+1,j2)       &
             +       tx*ty  *lfn(i2+1,j2+1)
        lffi1j=                            &
                    (1-txx)*(1-ty)*lfn(i2,j2)   &
             +      (1-txx)*ty  *lfn(i2,j2+1)   &
             +      (txx)*(1-ty)*lfn(i2+1,j2)   &
             +      (txx)*ty  *lfn(i2+1,j2+1)
        lffij1=                            &
                    (1-tx)*(1-tyy)*lfn(i2,j2)   &
             +      (1-tx)*(tyy)  *lfn(i2,j2+1) &
             +      tx*(1-tyy)*lfn(i2+1,j2)     &
             +      tx*(tyy)  *lfn(i2+1,j2+1)
        lffi1j1 =                               &
                      (1-txx)*(1-tyy)*lfn(i2,j2)     &
             +      (1-txx)*(tyy)  *lfn(i2,j2+1)   &        
             +      (txx)*(1-tyy)*lfn(i2+1,j2)     &
             +      (txx)*(tyy)  *lfn(i2+1,j2+1)

        ! get ready to fix up tign values to be interpolated
        do ii=1,2
          do jj=1,2
            tignf(ii,jj)=tign(i2+ii-1,j2+jj-1)
          enddo
        enddo
        tifij=                                 &
                   (1-tx)*(1-ty)*tignf(1,1)        &
             +     (1-tx)*ty*tignf(1,1+1)          &
             +     tx*(1-ty)*tignf(1+1,1)          &
             +     tx*ty*tignf(1+1,1+1)
        tifi1j=                               &
                   (1-txx)*(1-ty)*tignf(1,1)      &
             +     (1-txx)*ty*tignf(1,1+1)        &
             +     (txx)*(1-ty)*tignf(1+1,1)      &
             +     (txx)*(ty)*tignf(1+1,1+1)            
        tifij1=                               &
                   (1-tx)*(1-tyy)*tignf(1,1)      &
             +     (1-tx)*(tyy)*tignf(1,1+1)      &
             +      tx*(1-tyy)*tignf(1+1,1)       &
             +      tx*(tyy)*tignf(1+1,1+1)
        tifi1j1=                               &
                   (1-txx)*(1-tyy)*tignf(1,1)     &
             +     (1-txx)*(tyy)*tignf(1,1+1)     &
             +     (txx)*(1-tyy)*tignf(1+1,1)     &
             +     (txx)*(tyy)*tignf(1+1,1+1) 

         
        if(fuel_left_method.eq.1)then
          call fuel_left_cell_1( fuel_left_ff, fire_area_ff, &
             lffij,lffij1,lffi1j,lffi1j1,&
             tifij,tifij1,tifi1j,tifi1j1,&
             time_now, fuel_time(icl,jcl))
        elseif(fuel_left_method.eq.2)then
          fire_area_ff=0  ! initialize to something until computed in fuel_left_f(i,j)
          fuel_left_ff=fuel_left_cell_2( &
             lffij,lffij1,lffi1j,lffi1j1,&
             tifij,tifij1,tifi1j,tifi1j1,&
             time_now, fuel_time(icl,jcl)) 
        else
          call crash('fuel_left: unknown fuel_left_method')
        endif

        ! consistency check
        if(fire_area_ff.lt.-1e-6 .or.  &
          (fire_area_ff.eq.0. .and. fuel_left_ff.lt.1.-1e-6))then
!$OMP CRITICAL(FIRE_CORE_CRIT)
           write(msg,'(a,2i6,2(a,f11.8))')'fuel_left: at node',i,j, &
              ' of refined mesh fuel burnt',1-fuel_left_ff,' fire area',fire_area_ff
!$OMP END CRITICAL(FIRE_CORE_CRIT)
           call crash(msg)
        endif

        helpsum1=helpsum1+fuel_left_ff
        helpsum2=helpsum2+fire_area_ff
      enddo
    enddo
    fuel_frac(icl,jcl)=helpsum1 
    fire_area(icl,jcl)=helpsum2
  enddo 
enddo
  



#ifdef DEBUG_OUT_FUEL_LEFT
    call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,fire_area,'fire_area',0)
    call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,fuel_frac,'fuel_frac',0)
    call write_array_m(1,IFCELLS,1,JFCELLS,1,IFCELLS,1,JFCELLS,fuel_left_f,'fuel_left_f',0)
    call write_array_m(1,IFCELLS,1,JFCELLS,1,IFCELLS,1,JFCELLS,fire_area_f,'fire_area_f',0)
#endif

! finish the averaging
do j=jts,jte
    do i=its,ite        
        fuel_frac(i,j) = fuel_frac(i,j) /(ir*jr) ! multiply by weight for averaging over coarse cell
        fire_area(i,j) = fire_area(i,j) /(ir*jr) ! 
    enddo
enddo

! consistency check after sum
fmax=0
do j=jts,jte
    do i=its,ite        
       if(fire_area(i,j).eq.0.)then
           if(fuel_frac(i,j).lt.1.-1e-6)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
               write(msg,'(a,2i6,2(a,f11.8))')'fuel_left: at node',i,j, &
                   ' fuel burnt',1-fuel_frac(i,j),' but fire area',fire_area(i,j)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
               call crash(msg)
           endif
       else
           frat=(1-fuel_frac(i,j))/fire_area(i,j)
           fmax=max(fmax,frat)
       endif
    enddo
enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,4i6,a,f10.7)')'fuel_left: tile',its,ite,jts,jte,' max fuel burnt/area',fmax 
!$OMP END CRITICAL(FIRE_CORE_CRIT)
call message(msg)
return


end subroutine fuel_left

!
!************************
!

subroutine fuel_left_cell_1( fuel_frac_left, fire_frac_area, &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
!*** purpose: compute the fuel fraction left in one cell
implicit none
!*** arguments
real, intent(out):: fuel_frac_left, fire_frac_area ! 
real, intent(in)::lfn00,lfn01,lfn10,lfn11    ! level set function at 4 corners of the cell
real, intent(in)::tign00,tign01,tign10,tign11! ignition time at the  4 corners of the cell
real, intent(in)::time_now                   ! the time now
real, intent(in)::fuel_time_cell            ! time to burns off to 1/e
!*** Description
! The area burning is given by the condition L <= 0, where the function P is
! interpolated from lfn(i,j)
!
! The time since ignition is the function T, interpolated in from tign(i,j)-time_now.
! The values of tign(i,j) where lfn(i,j)>=0 are ignored, tign(i,j)=0 is taken 
! when lfn(i,j)=0.
!
! The function computes an approxmation  of the integral
!
!
!                                  /\
!                                  |              
! fuel_frac_left  =      1   -     | 1 -  exp(-T(x,y)/fuel_time_cell)) dxdy
!                                  |            
!                                 \/
!                                0<x<1
!                                0<y<1
!                             L(x,y)<=0
!
! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
! Because of symmetries, the result should not depend on the mesh spacing dx dy
! so dx=1 and dy=1 assumed.
!
! Example:
!
!        lfn<0         lfn>0
!      (0,1) -----O--(1,1)            O = points on the fireline, T=tnow
!            |      \ |               A = the burning area for computing
!            |       \|                        fuel_frac(i,j)
!            |   A    O 
!            |        |
!            |        |
!       (0,0)---------(1,0)
!       lfn<0          lfn<0
!
! Approximations allowed: 
! The fireline can be approximated by straight line(s).
! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
! 
! Requirements:
! 1. The output should be a continuous function of the arrays lfn and
!  tign whenever lfn(i,j)=0 implies tign(i,j)=tnow.  
! 2. The output should be invariant to the symmetries of the input in each cell.
! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
! 4. The result should be at least 1st order accurate in the sense that it is
!    exact if the time from ignition is a linear function.
!
! If time from ignition is approximated by polynomial in the burnt
! region of the cell, this is integral of polynomial times exponential
! over a polygon, which can be computed exactly.
!
! Requirement 4 is particularly important when there is a significant decrease
! of the fuel fraction behind the fireline on the mesh scale, because the
! rate of fuel decrease right behind the fireline is much larger 
! (exponential...). This will happen when
!
! change of time from ignition within one mesh cell / fuel_time_cell is not << 1
!
! This is the same as
!
!               mesh cell size
!  X =    -------------------------      is not << 1
!       fireline speed * fuel_time_cell
!         
!
! When X is large then the fuel burnt in one timestep in the cell is
! approximately proportional to length of  fireline in that cell.
!
! When X is small then the fuel burnt in one timestep in the cell is
! approximately proportional to the area of the burning region.
!

!*** calls
intrinsic tiny

!*** local
real::ps,aps,area,ta,out
real::t00,t01,t10,t11
real,parameter::safe=tiny(aps)
character(len=128)::msg

! the following algorithm is a very crude approximation

! minus time since ignition, 0 if no ignition yet
! it is possible to have 0 in fire region when ignitin time falls in 
! inside the time step because lfn is updated at the beginning of the time step

t00=tign00-time_now
if(lfn00>0. .or. t00>0.)t00=0.
t01=tign01-time_now
if(lfn01>0. .or. t01>0.)t01=0.
t10=tign10-time_now
if(lfn10>0. .or. t10>0.)t10=0.
t11=tign11-time_now
if(lfn11>0. .or. t11>0.)t11=0.

! approximate burning area, between 0 and 1   
ps = lfn00+lfn01+lfn10+lfn11   
aps = abs(lfn00)+abs(lfn01)+abs(lfn10)+abs(lfn11)
aps=max(aps,safe)
area =(-ps/aps+1.)/2.
area = max(area,0.) ! make sure area is between 0 and 1
area = min(area,1.)
    
! average negative time since ignition
ta=0.25*(t00+t01+t10+t11)

! exp decay in the burning area
out=1.
!if(area>0.)out=1. - area*(1. - exp(ta/fuel_time_cell))
if(area>0)out=area*exp(ta/fuel_time_cell) + (1. - area)

if(out>1.)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,*)'out=',out,'>1 area=',area,' ta=',ta
    call message(msg)
    write(msg,*)'tign=',tign00,tign01,tign10,tign11,' time_now=',time_now
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)
    !call message('WARNING: fuel_left_cell_1: fuel fraction > 1')
    call crash('fuel_left_cell_1: fuel fraction > 1')
endif

!out = max(out,0.) ! make sure out is between 0 and 1
!out = min(out,1.)

fuel_frac_left = out
fire_frac_area = area

end subroutine fuel_left_cell_1

!
!****************************************
!

real function fuel_left_cell_2(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
!*** purpose: compute the fuel fraction left in one cell
implicit none
!*** arguments
real, intent(in)::lfn00,lfn01,lfn10,lfn11    ! level set function at 4 corners of the cell
real, intent(in)::tign00,tign01,tign10,tign11! ignition time at the  4 corners of the cell
real, intent(in)::time_now                   ! the time now
real, intent(in)::fuel_time_cell            ! time to burns off to 1/e
!*** Description
! The area burning is given by the condition L <= 0, where the function P is
! interpolated from lfn(i,j)
!
! The time since ignition is the function T, interpolated in from tign(i,j)-time_now.
! The values of tign(i,j) where lfn(i,j)>=0 are ignored, tign(i,j)=0 is taken 
! when lfn(i,j)=0.
!
! The function computes an approxmation  of the integral
!
!
!                                  /\
!                                  |              
! fuel_frac_left  =      1   -     | 1 -  exp(-T(x,y)/fuel_time_cell)) dxdy
!                                  |            
!                                 \/
!                                0<x<1
!                                0<y<1
!                             L(x,y)<=0
!
! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
! Because of symmetries, the result should not depend on the mesh spacing dx dy
! so dx=1 and dy=1 assumed.
!
! Example:
!
!        lfn<0         lfn>0
!      (0,1) -----O--(1,1)            O = points on the fireline, T=tnow
!            |      \ |               A = the burning area for computing
!            |       \|                        fuel_frac(i,j)
!            |   A    O 
!            |        |
!            |        |
!       (0,0)---------(1,0)
!       lfn<0          lfn<0
!
! Approximations allowed: 
! The fireline can be approximated by straight line(s).
! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
! 
! Requirements:
! 1. The output should be a continuous function of the arrays lfn and
!  tign whenever lfn(i,j)=0 implies tign(i,j)=tnow.  
! 2. The output should be invariant to the symmetries of the input in each cell.
! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
! 4. The result should be at least 1st order accurate in the sense that it is
!    exact if the time from ignition is a linear function.
!
! If time from ignition is approximated by polynomial in the burnt
! region of the cell, this is integral of polynomial times exponential
! over a polygon, which can be computed exactly.
!
! Requirement 4 is particularly important when there is a significant decrease
! of the fuel fraction behind the fireline on the mesh scale, because the
! rate of fuel decrease right behind the fireline is much larger 
! (exponential...). This will happen when
! 
! change of time from ignition within one mesh cell * fuel speed is not << 1
!
! This is the same as
!
!         mesh cell size*fuel_speed 
!         -------------------------      is not << 1
!             fireline speed
!         
!
! When X is large then the fuel burnt in one timestep in the cell is
! approximately proportional to length of  fireline in that cell.
!
! When X is small then the fuel burnt in one timestep in the cell is
! approximately proportional to the area of the burning region.
!

#ifndef FUEL_LEFT
call crash('fuel_left_cell_2: not implemented, please use fire_fuel_left_method=1')
fuel_left_cell_2=0.  ! to avoid compiler warning about value not set
end function fuel_left_cell_2
#else

!*** calls
intrinsic tiny

!*** local
real::ps,aps,area,ta,out
real::t00,t01,t10,t11
real,parameter::safe=tiny(aps)
character(len=128)::msg

!*** local
integer::i,j,k

! least squares
integer::mmax,nb,nmax,pmax,nin,nout
parameter(mmax=3,nb=64,nmax=8,pmax=8)
integer lda, ldb, lwork, info
parameter (lda=nmax, ldb=nmax, lwork=mmax+nmax+nb*(nmax+pmax))
integer n,m,p
real,dimension(lda,mmax):: mA
real,dimension(nmax):: vecD
real,dimension(lwork):: WORK
real,dimension(pmax):: vecY
real,dimension(mmax):: vecX
real,dimension(ldb,pmax)::mB
real,dimension(mmax)::u

real::tweight,tdist
integer::kk,ll,ss
real::rnorm
real,dimension(8,2)::xylist,xytlist
real,dimension(8)::tlist,llist,xt
real,dimension(5)::xx,yy
real,dimension(5)::lfn,tign

integer:: npoint
real::tt,x0,y0,xts,xte,yts,yte,xt1,xt2
real::lfn0,lfn1,dist,nr,c,s,errQ,ae,ce,ceae,a0,a1,a2,d,cet
real::s1,s2,s3
real::upper,lower,ah,ch,aa,cc,aupp,cupp,alow,clow
real,dimension(2,2)::mQ
real,dimension(2)::ut

!calls
intrinsic epsilon

real, parameter:: zero=0.,one=1.,eps=epsilon(zero)


! external functions    
real::snrm2
double precision :: dnrm2
external dnrm2
external snrm2
! external subroutines
external sggglm
external dggglm
!executable statements

! a very crude approximation - replace by a better code
! call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)
t00=tign00-time_now
if(lfn00>=0. .or. t00>0.)t00=0.
t01=tign01-time_now
if(lfn01>=0. .or. t01>0.)t01=0.
t10=tign10-time_now
if(lfn10>=0. .or. t10>0.)t10=0.
t11=tign11-time_now
if(lfn11>=0. .or. t11>0.)t11=0.

!if (t00<0 .or. t01 <0 .or. t10<0 .or. t11<0) then
!   print *,'tign00=',tign00,'tign10=',tign10,&
!          'tign01=',tign01,'tign11=',tign11
!end if



!*** case0 Do nothing
if ( lfn00>=0 .and. lfn10>=0 .and. lfn01>=0 .and. lfn11>=0 ) then
    out = 1.0 !  fuel_left, no burning
!*** case4 all four coners are burning
else if (lfn00<=0 .and. lfn10<=0 .and. lfn01<=0 .and. lfn11<=0) then
    ! least squares, A matrix for points
    mA(1,1)=0.0
    mA(2,1)=1.0
    mA(3,1)=0.0
    mA(4,1)=1.0
    mA(1,2)=0.0
    mA(2,2)=0.0
    mA(3,2)=1.0
    mA(4,2)=1.0
    mA(1,3)=1.0
    mA(2,3)=1.0
    mA(3,3)=1.0
    mA(4,3)=1.0
    ! D vector, time from ignition
    vecD(1)=time_now-tign00
    vecD(2)=time_now-tign10
    vecD(3)=time_now-tign01
    vecD(4)=time_now-tign11
    ! B matrix, weights
    do kk=1,4
    do ll=1,4
        mB(kk,ll)=0.0
    end do
        mB(kk,kk)=2.0
    end do
    ! set the m,n,p
    n=4 ! rows of matrix A and B
    m=3 ! columns of matrix A
    p=4 ! columns of matrix B
    ! call least squqres in LAPACK            
    call SGGGLM(N,M,P,mA,LDA,mB,LDB,vecD,vecX,vecY, &
                WORK,LWORK,INFO)
    rnorm=snrm2(p,vecY,1)            
    ! integrate
    u(1)=-vecX(1)/fuel_time_cell
    u(2)=-vecX(2)/fuel_time_cell
    u(3)=-vecX(3)/fuel_time_cell            
    !fuel_burn(i,j)=1-exp(u(3))*intexp(u(1)*dx)*intexp(u(2)*dy)
    s1=u(1)
    s2=u(2)            
    out=1-exp(u(3))*intexp(s1)*intexp(s2)
    !print *,'intexp
    if ( out<0 .or. out>1.0 ) then
        print *,'case4, out should be between 0 and 1'
    end if
!*** case 1,2,3
else
    ! set xx, yy for the coner points
    ! move these values out of i and j loop to speed up
    xx(1) = -0.5
    xx(2) =  0.5
    xx(3) =  0.5
    xx(4) = -0.5
    xx(5) = -0.5
    yy(1) = -0.5
    yy(2) = -0.5
    yy(3) =  0.5
    yy(4) =  0.5
    yy(5) = -0.5     
    lfn(1)=lfn00
    lfn(2)=lfn10
    lfn(3)=lfn11
    lfn(4)=lfn01
    lfn(5)=lfn00
    tign(1)=t00
    tign(2)=t10
    tign(3)=t11
    tign(4)=t01
    tign(5)=t00
    npoint = 0 ! number of points in polygon
    !print *,'time_now=',time_now
    !print *,'lfn00=',lfn00,'lfn10=',lfn10,&
    !        'lfn01=',lfn01,'lfn11=',lfn11
    !print *,'t00=',t00,'t10=',t10,&
    !        't01=',t01,'t11=',t11

    do k=1,4
        lfn0=lfn(k  )
        lfn1=lfn(k+1)
        if ( lfn0 <= 0.0 ) then
            npoint = npoint + 1
            xylist(npoint,1)=xx(k)
            xylist(npoint,2)=yy(k)
            tlist(npoint)=-tign(k)
            llist(npoint)=lfn0
        end if
        if ( lfn0*lfn1 < 0 ) then
            npoint = npoint + 1
            tt=lfn0/(lfn0-lfn1)
            x0=xx(k)+( xx(k+1)-xx(k) )*tt
            y0=yy(k)+( yy(k+1)-yy(k) )*tt
            xylist(npoint,1)=x0
            xylist(npoint,2)=y0
            tlist(npoint)=0 ! on fireline
            llist(npoint)=0
        end if
    end do

    ! make the list circular
    tlist(npoint+1)=tlist(1)
    llist(npoint+1)=llist(1)   
    xylist(npoint+1,1)=xylist(1,1)
    xylist(npoint+1,2)=xylist(1,2)
    !* least squares, A matrix for points
    do kk=1,npoint
        mA(kk,1)=xylist(kk,1)
        mA(kk,2)=xylist(kk,2)
        mA(kk,3)=1.0
        vecD(kk)=tlist(kk) ! D vector,time from ignition
    end do
    ! B matrix, weights
    do kk=1,ldb
    do ll=1,pmax
        mB(kk,ll)=0.0 ! clear
    end do
    end do
        
    do kk=1,npoint
        mb(kk,kk) = 10 ! large enough
        do ll=1,npoint
            if ( kk .ne. ll ) then
                dist = sqrt( (xylist(kk,1)-xylist(ll,1))**2+ &
                             (xylist(kk,2)-xylist(ll,2))**2 )                   
                mB(kk,kk)=min( mB(kk,kk) , dist )
            end if              
        end do !ll
        mB(kk,kk)=mB(kk,kk)+1.
    end do ! kk
    ! set the m,n,p
    n=npoint ! rows of matrix A and B
    m=3 ! columns of matrix A
    p=npoint ! columns of matrix B
    !* call least squqres in LAPACK                  
    call SGGGLM(N,M,P,mA,LDA,mB,LDB,vecD,vecX,vecY, &
                        WORK,LWORK,INFO)
    !print *,'after LS in case3'
    !print *,'vecX from LS',vecX
    !print *,'tign inputed',tign00,tign10,tign11,tign01
    rnorm=snrm2(p,vecY,1)
    u(1)=vecX(1)
    u(2)=vecX(2)
    u(3)=vecX(3)            
    ! rotate to gradient on x only
    nr = sqrt(u(1)**2+u(2)**2)
    if(.not.nr.gt.eps)then
        out=1.
        goto 900
    endif
    c = u(1)/nr
    s = u(2)/nr
    mQ(1,1)=c
    mQ(1,2)=s
    mQ(2,1)=-s
    mQ(2,2)=c            
    ! mat vec multiplication
    call matvec(mQ,2,2,u,3,ut,2,2,2)            
    errQ = ut(2) ! should be zero            
    ae = -ut(1)/fuel_time_cell
    ce = -u(3)/fuel_time_cell      
    cet=ce!keep ce
    call matmatp(xylist,8,2,mQ,2,2,xytlist,8,2,npoint+1,2,2)            
    call sortxt( xytlist, 8,2, xt,8,npoint )            
    out=0.0
    aupp=0.0
    cupp=0.0
    alow=0.0
    clow=0.0
    do k=1,npoint-1
        xt1=xt(k)
        xt2=xt(k+1)
        upper=0
        lower=0
        ah=0
        ch=0
        if ( xt2-xt1 > eps*100 ) then
                
            do ss=1,npoint
                xts=xytlist(ss,1)
                yts=xytlist(ss,2)
                xte=xytlist(ss+1,1)
                yte=xytlist(ss+1,2)
                  
                if ( (xts>xt1 .and. xte>xt1) .or. &
                     (xts<xt2 .and. xte<xt2) ) then
                    aa = 0 ! do nothing
                    cc = 0
                else
                    aa = (yts-yte)/(xts-xte)
                    cc = (xts*yte-xte*yts)/(xts-xte)                    
                    if (xte<xts) then
                        aupp = aa
                        cupp = cc
                        ah=ah+aa
                        ch=ch+cc
                        upper=upper+1
                    else
                        alow = aa
                        clow = cc
                        lower=lower+1
                    end if
                end if!(xts>xt1 .and. xte>xt1)              
            end do ! ss
            ce=cet !use stored ce
            if (ae*xt1+ce > 0 ) then
              ce=ce-(ae*xt1+ce)!shift small amounts exp(-**)
            end if
            if (ae*xt2+ce > 0) then
            ce=ce-(ae*xt2+ce)
            end if

            ah = aupp-alow
            ch = cupp-clow  
            ! integrate (ah*x+ch)*(1-exp(ae*x+ce) from xt1 to xt2
            ! numerically sound for ae->0, ae -> infty
            ! this can be important for different model scales
            ! esp. if someone runs the model in single precision!!
            ! s1=int((ah*x+ch),x,xt1,xt2)
            s1 = (xt2-xt1)*((1./2.)*ah*(xt2+xt1)+ch)            
            ! s2=int((ch)*(-exp(ae*x+ce)),x,xt1,xt2)
            ceae=ce/ae;
            s2 = -ch*exp(ae*(xt1+ceae))*(xt2-xt1)*intexp(ae*(xt2-xt1))                
            ! s3=int((ah*x)*(-exp(ae*x+ce)),x,xt1,xt2)
            ! s3=int((ah*x)*(-exp(ae*(x+ceae))),x,xt1,xt2)
            ! expand in Taylor series around ae=0
            ! collect(expand(taylor(int(x*(-exp(ae*(x+ceae))),x,xt1,xt2)*ae^2,ae,4)/ae^2),ae)
            ! =(1/8*xt1^4+1/3*xt1^3*ceae+1/4*xt1^2*ceae^2-1/8*xt2^4-1/3*xt2^3*ceae-1/4*xt2^2*ceae^2)*ae^2
            !     + (-1/3*xt2^3-1/2*xt2^2*ceae+1/3*xt1^3+1/2*xt1^2*ceae)*ae 
            !     + 1/2*xt1^2-1/2*xt2^2
            !
            ! coefficient at ae^2 in the expansion, after some algebra            
            a2=(xt1-xt2)*((1./4.)*(xt1+xt2)*ceae**2+(1./3.)* &
               (xt1**2+xt1*xt2+xt2**2)*ceae+(1./8.)* &
               (xt1**3+xt1*(xt2**2)+xt1**2*xt2+xt2**3))               
            d=(ae**4)*a2
            
            if (abs(d)>eps) then
            ! since ae*xt1+ce<=0 ae*xt2+ce<=0 all fine for large ae
            ! for ae, ce -> 0 rounding error approx eps/ae^2
                s3=( exp(ae*(xt1+ceae))*(ae*xt1-1)-&
                     exp(ae*(xt2+ceae))*(ae*xt2-1) )/(ae**2)
                
            !we do not worry about rounding as xt1 -> xt2, then s3 -> 0
            else
                ! coefficient at ae^1 in the expansion
                a1=(xt1-xt2)*((1./2.)*ceae*(xt1+xt2)+(1./3.)*&
                   (xt1**2+xt1*xt2+xt2**2))
                ! coefficient at ae^0 in the expansion for ae->0
                a0=(1./2.)*(xt1-xt2)*(xt1+xt2)
                s3=a0+a1*ae+a2*ae**2; ! approximate the integral
                            end if

            s3=ah*s3                                                
            out=out+s1+s2+s3
            out=1-out !fuel_left
            if(out<0 .or. out>1) then                                  
                print *,':fuel_fraction should be between 0 and 1'
                !print *, 'eps= ', eps
                !print *, 'xt1= ', xt1, 'xt2= ', xt2
                !print *,'ae= ',ae,'ce= ',ce,'ah= ',ah,'ch= ',ch
                !print *,'a0= ', a0,'a1= ', a1,'a2= ', a2
                print *,'s1= ', s1,'s2= ', s2,'s3= ', s3
                print *,':fuel_fraction =',out
            end if!print
                
        end if
    end do ! k     
    
end if ! if case0, elseif case4 ,else case123

900 continue 
if(out>1. .or. out<0.)call crash('fuel_left_cell_2: fuel fraction out of bounds [0,1]')
fuel_left_cell_2 = out
end function fuel_left_cell_2

!
!****************************************
!
real function intexp(ab)
implicit none
real::ab
!calls
intrinsic epsilon

real, parameter:: zero=0.,one=1.,eps=epsilon(zero)

!eps = 2.2204*(10.0**(-8))!from matlab
if ( eps < abs(ab)**3/6. ) then
    intexp=(exp(ab)-1)/ab
  else
    intexp=1+ab/2.
end if
end function
!
!****************************************
!
subroutine sortxt(xytlist,nrow,ncolumn,xt,nxt,nvec)
implicit none
integer::nrow,ncolumn,nxt,nvec
real,dimension(nrow,ncolumn)::xytlist
real,dimension(nxt)::xt

integer::i,j
real::temp

do i=1,nvec
  xt(i)=xytlist(i,1)
end do

do i=1,nvec-1
  do j=i+1,nvec
    if ( xt(i) > xt(j) ) then
         temp = xt(i)
         xt(i)=xt(j)
         xt(j)=temp
    end if
  end do
end do

end subroutine !sortxt
!
!****************************************
!
subroutine matvec(A,m,n,V,nv,out,nout,nrow,ncolumn)
implicit none
integer::m,n,nv,nout,nrow,ncolumn
real,dimension(m,n)::A   ! allocated m by n 
real,dimension(nv)::V    ! allocated nv
real,dimension(nout)::out! allocated nout 

integer::i,j

do i=1,nrow
  out(i)=0.0
  do j=1,ncolumn
    out(i)=out(i)+A(i,j)*V(j)
  end do
end do
end subroutine
!
!****************************************
!
subroutine matmatp(A,mA,nA,B,mB,nB,C,mC,nC,nrow,ncolumn,nP)
implicit none
integer::mA,nA,mB,nB,mC,nC,nrow,ncolumn,nP
real,dimension(mA,nA)::A   ! allocated m by n 
real,dimension(mB,nB)::B   ! allocated m by n 
real,dimension(mC,nC)::C   ! allocated m by n 
integer::i,j,k
do i=1,nrow  
  do j=1,ncolumn
    C(i,j)=0.0
  do k=1,nP
    C(i,j)=C(i,j)+A(i,k)*B(j,k) ! B'
  end do
end do
end do
end subroutine

!
!****************************************
!
#endif

subroutine prop_ls_rk3(id,               &             
                ifds,ifde,jfds,jfde,     &                 
                ifms,ifme,jfms,jfme,     &                    
                ifps,ifpe,jfps,jfpe,     &            
                ifts,ifte,jfts,jfte,     &                    
                ts,dt,dx,dy,             &                     
                tbound,                  &                  
                lfn_in,                  &
                lfn_0,lfn_1,lfn_2,       & 
                lfn_out,tign,ros,        &           
                fp,                      &
                grid,                    &       
                ids,ide,jds,jde,kds,kde, & 
                ims,ime,jms,jme,kms,kme, & 
                ips,ipe,jps,jpe,kps,kpe  &
                   )


#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_lfn_1_sub, halo_fire_lfn_2_sub, halo_fire_lfn_0_sub
#endif
USE module_domain , only: domain

implicit none

!*** purpose: advance level function in time

!***************************************************************************************!
!*** Level-set method recoded for true mpi communications and added                  ***!
!*** 3rd-order Runge-Kutta time integration and 3rd/5th-order WENO advection schemes ***!
!*** Implemented by: Domingo Munoz-Esparza (NCAR/RAL, April 2016)                    ***!
!*** Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate      ***!
!*** fire-spread algorithm in the Weather Research and Forecasting model using the   ***!
!*** level-set method", Journal of Advances in Modeling Earth Systems, 2018          ***!
!*** https://doi.org/10.1002/2017MS001108                                            ***!
!***************************************************************************************!

!*** description
!
! Propagation of closed curve by a level function method. The level function
! lfn is defined by its values at the nodes of a rectangular grid. 
! The area where lfn < 0 is inside the curve. The curve is 
! described implicitly by lfn=0. Points where the curve intersects gridlines
! can be found by linear interpolation from nodes.
!
! The level function is advanced from time ts to time ts + dt. 
!
! The level function should be initialized to (an approximation of) the signed
! distance from the curve. If the initial curve is a circle, the initial level
! function is simply the distance from the center minus the radius.
! 
! The curve moves outside with speed given by function speed_func.
!   
! Method: Godunov/ENO method for the normal motion. The timestep is checked for
! CFL condition. For a straight segment in a constant field and locally linear
! level function, the method reduces to the exact normal motion. The advantage of 
! the level set method is that it treats automatically special cases such as
! the curve approaching itself and merging components of the area inside the curve.
!
! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by 
! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
! Dept. Computer Science, University of British Columbia, 2007
! http://www.cs.ubc.ca/\~mitchell/Toolbo\LS
! 
  
!*** arguments 

! id                in    unique identification for prints and dumps
! ids,ide,jds,jde   in    domain dimensions
! ims,ime,jms,jme   in    memory dimensions
! its,ite,jts,jte   in    tile dimensions
! ts                in    start time
! dt                in    time step
! dx,dy             in    grid spacing
! tbound            out   bound on stable time step from CFL condition, if tbound>=dt then OK
! lfn_in,lfn_out    inout,out the level set function at nodes
! tign              inout the ignition time at nodes

!*** calls
!
! DME added for halo update
type(domain) , target :: grid                     
integer, intent(in):: ids,ide,jds,jde,kds,kde, &
    ims,ime,jms,jme,kms,kme, &
    ips,ipe,jps,jpe,kps,kpe
!

integer,intent(in)::id,ifms,ifme,jfms,jfme,ifds,ifde,jfds,jfde,ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe 
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_in,tign
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_1,lfn_2,lfn_0 
real,dimension(ifms:ifme,jfms:jfme),intent(out)::lfn_out,ros
real,intent(in)::dx,dy,ts,dt
real,intent(out)::tbound
type(fire_params),intent(in)::fp

real,dimension(ifms:ifme,jfms:jfme):: tend
! scalars
real::grad2,rr,tbound2,tbound3

real::gradx,grady,aspeed,err,aerr,time_now
integer::i,j,its1,ite1,jts1,jte1,k,kk,id1
character(len=128)::msg
integer::nfirenodes,nfireline
real::sum_err,min_err,max_err,sum_aerr,min_aerr,max_aerr   

! constants
integer,parameter :: mstep=1000, printl=1
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon,tiny,huge

!*** executable

!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a8,i5,a6,i5,3(a1,i5))')'prop_ls:',id,' tile ',ifts,':',ifte,',',jfts,':',jfte
!$OMP END CRITICAL(FIRE_CORE_CRIT)
call message(msg)

!!!!!!!!!!!!!!!!!!!!!!
! Runge-Kutta step 1 !
!!!!!!!!!!!!!!!!!!!!!!

    do j=jfts,jfte
        do i=ifts,ifte
            lfn_0(i,j) = lfn_in(i,j)
        enddo
    enddo

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_0.inc"
#endif

    id1 = id  ! for debug prints
    if(id1.ne.0)id1=id1+1000
    call tend_ls(id1,    &
    ifds,ifde,jfds,jfde, &                       ! domain dims 
    ifps,ifpe,jfps,jfpe, &                       ! patch dims
    ifts,ifte,jfts,jfte, &                       ! tile dims
    ifms,ifme,jfms,jfme, &                       ! memory dims
    ts,dt,dx,dy,         &                       ! scalars in
    lfn_0,               &                       ! arrays in
    tbound,              &                       ! scalars out 
    tend, ros,           &                       ! arrays out        
    fp                   &                       ! params
)

    do j=jfts,jfte 
        do i=ifts,ifte 
            lfn_1(i,j) = lfn_0(i,j) + (dt/3.0)*tend(i,j) 
        enddo
    enddo

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_1.inc"
#endif

!!!!!!!!!!!!!!!!!!!!!!
! Runge-Kutta step 2 !
!!!!!!!!!!!!!!!!!!!!!!

    if(id1.ne.0)id1=id1+1000
    call tend_ls(id1,    &
    ifds,ifde,jfds,jfde, &                     
    ifps,ifpe,jfps,jfpe, &                    
    ifts,ifte,jfts,jfte, &                    
    ifms,ifme,jfms,jfme, &               
    ts+dt,dt,dx,dy,      &                  
    lfn_1,               &                             
    tbound2,             &                              
    tend,ros,            &                           
    fp                   &
)

    do j=jfts,jfte
        do i=ifts,ifte
            lfn_2(i,j) = lfn_0(i,j) + (dt/2.0)*tend(i,j)
        enddo
    enddo     

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_2.inc"
#endif

!!!!!!!!!!!!!!!!!!!!!!
! Runge-Kutta step 3 !
!!!!!!!!!!!!!!!!!!!!!!

    if(id1.ne.0)id1=id1+1000
    call tend_ls(id1,    &
    ifds,ifde,jfds,jfde, &                      
    ifps,ifpe,jfps,jfpe, &                     
    ifts,ifte,jfts,jfte, &                    
    ifms,ifme,jfms,jfme, &                 
    ts+dt,dt,dx,dy,      &                  
    lfn_2,               &                            
    tbound3,             &                            
    tend,ros,            &                           
    fp                   &
)

    do j=jfts,jfte
        do i=ifts,ifte
            lfn_out(i,j) = lfn_0(i,j) + dt*tend(i,j)
            lfn_2(i,j) = lfn_out(i,j) ! lfn_2=lfn_out (needed for reinitialization purposes)
        enddo
    enddo     

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_2.inc"
#endif

! CFL check

    tbound=min(tbound,tbound2,tbound3)

!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,'(a,f10.2,4(a,f7.2))')'prop_ls: time',ts,' dt=',dt,' bound',min(tbound,999.99), &
        ' dx=',dx,' dy=',dy
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)
    if(dt>tbound)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
        write(msg,'(2(a,f10.2))')'prop_ls: WARNING: time step ',dt, &
        ' > bound =',tbound
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        call message(msg)
    endif
    
end subroutine prop_ls_rk3

!
!*****************************
!

subroutine reinit_ls_rk3(id,                       &
                ifts,ifte,jfts,jfte,               &                     
                ifms,ifme,jfms,jfme,               &                     
                ifds,ifde,jfds,jfde,               &                     
                ifps,ifpe,jfps,jfpe,               &                      
                ts,dt,dx,dy,                       &                    
                lfn_in,                            & 
                lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3, & 
                lfn_out,tign,                      &             
                grid,                              &          
                ids,ide,jds,jde,kds,kde,           &
                ims,ime,jms,jme,kms,kme,           & 
                ips,ipe,jps,jpe,kps,kpe            &
                ) 


#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_lfn_s1_sub,halo_fire_lfn_s2_sub,halo_fire_lfn_s3_sub
#endif
USE module_domain , only: domain
USE module_configure, only: grid_config_rec_type

implicit none

!*************************************************************************************!
!*** Level-set function reinitialization following:                                ***!
!*** Sussman, Smereka, Osher. Journal of Computational Physics 114, 146-159 (1994) ***!
!*** implemented by Domingo Munoz-Esparza (NCAR/RAL, April 2016)                   ***!
!*** Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate    ***!
!*** fire-spread algorithm in the Weather Research and Forecasting model using the ***!
!*** level-set method", Journal of Advances in Modeling Earth Systems, 2018        ***!
!*** https://doi.org/10.1002/2017MS001108                                          ***!
!*************************************************************************************!

!*** purpose: reinitialize the level-set function

type(domain) , target :: grid
integer, intent(in):: ids,ide,jds,jde,kds,kde, & 
                      ims,ime,jms,jme,kms,kme, & 
                      ips,ipe,jps,jpe,kps,kpe    

integer,intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,id 
integer,intent(in)::ifds,ifde,jfds,jfde,ifps,ifpe,jfps,jfpe  
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_in,tign
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_out
real,intent(in)::dx,dy,ts,dt
real::dt_s
real,dimension(ifts:ifte,jfts:jfte):: tend_1,tend_2,tend_3
real::diffLx,diffLy,diffRx,diffRy,diff2x,diff2y,grad,time_now
integer::nts,i,j,k,kk
intrinsic epsilon
character(len=128)::msg
integer::itso,iteo,jtso,jteo
real::threshold_HLl,threshold_HLu
integer,parameter::bdy_eno1=10
!*** executable

threshold_HLl=-fire_lsm_band_ngp*dx 
threshold_HLu=fire_lsm_band_ngp*dx

! Define S0 based on current lfn values

    do j=jfts,jfte 
        do i=ifts,ifte 
            lfn_s0(i,j) = lfn_2(i,j)/sqrt(lfn_2(i,j)**2.0+dx**2.0) 
            lfn_s3(i,j) = lfn_2(i,j)
        enddo
    enddo
#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_S3.inc"
#endif

    call continue_at_boundary(1,1,fire_lfn_ext_up, &   !extend by extrapolation but never down 
    ifms,ifme,jfms,jfme, &                             ! memory dims
    ifds,ifde,jfds,jfde, &                             ! domain
    ifps,ifpe,jfps,jfpe, &                             ! patch
    ifts,ifte,jfts,jfte, &                             ! tile
    itso,iteo,jtso,jteo, &                             ! where set now
    lfn_s3) 

    
dt_s=0.01*dx
do nts=1,fire_lsm_reinit_iter ! iterate to solve to steady state (1 iter each time step is enoguh)
    ! Runge-Kutta step 1 !
    
    call advance_ls_reinit( &
    ifms,ifme,jfms,jfme,    &           
    ifds,ifde,jfds,jfde,    &           
    ifts,ifte,jfts,jfte,    &               
    dx,dy,dt_s,bdy_eno1,threshold_HLu, &               
    lfn_s0,lfn_s3,lfn_s3,lfn_s1,1.0/3.0)   ! sign funcition, initial ls, current stage ls, next stage advanced ls, RK coefficient

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_S1.inc"
#endif

    call continue_at_boundary(1,1,fire_lfn_ext_up, & 
    ifms,ifme,jfms,jfme, &                          
    ifds,ifde,jfds,jfde, &                           
    ifps,ifpe,jfps,jfpe, &                           
    ifts,ifte,jfts,jfte, &                           
    itso,iteo,jtso,jteo, &               
    lfn_s1) 

    ! Runge-Kutta step 2 !

    call advance_ls_reinit( &
    ifms,ifme,jfms,jfme,    &
    ifds,ifde,jfds,jfde,    &
    ifts,ifte,jfts,jfte,    &
    dx,dy,dt_s,bdy_eno1,threshold_HLu, &
    lfn_s0,lfn_s3,lfn_s1,lfn_s2,1.0/2.0)

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_S2.inc"
#endif

    call continue_at_boundary(1,1,fire_lfn_ext_up, &  
    ifms,ifme,jfms,jfme, &              
    ifds,ifde,jfds,jfde, &                 
    ifps,ifpe,jfps,jfpe, &                 
    ifts,ifte,jfts,jfte, &               
    itso,iteo,jtso,jteo, & 
    lfn_s2) 

    ! Runge-Kutta step 3 !

    call advance_ls_reinit( &
    ifms,ifme,jfms,jfme,    &
    ifds,ifde,jfds,jfde,    &
    ifts,ifte,jfts,jfte,    &
    dx,dy,dt_s,bdy_eno1,threshold_HLu, &
    lfn_s0,lfn_s3,lfn_s2,lfn_s3,1.0)  

#ifdef DM_PARALLEL
#      include "HALO_FIRE_LFN_S3.inc"
#endif

    call continue_at_boundary(1,1,fire_lfn_ext_up, &  
    ifms,ifme,jfms,jfme, &              
    ifds,ifde,jfds,jfde, &                  
    ifps,ifpe,jfps,jfpe, &                
    ifts,ifte,jfts,jfte, &              
    itso,iteo,jtso,jteo, &           
    lfn_s3) 

enddo ! end iterations for steady state solution of reinitialization PDE

    do j=jfts,jfte 
        do i=ifts,ifte 
          lfn_out(i,j)=lfn_s3(i,j) ! assing to lfn_out the reinitialized level-set function
          lfn_out(i,j)=min(lfn_out(i,j),lfn_in(i,j)) ! fire area can only increase
        enddo
    enddo

end subroutine reinit_ls_rk3

!
!*****************************
!

subroutine advance_ls_reinit(ifms,ifme,jfms,jfme, &
                          ifds,ifde,jfds,jfde,    &
                          ifts,ifte,jfts,jfte,    &
                          dx,dy,dt_s,bdy_eno1,threshold_HLu, &
                          lfn_s0,lfn_ini,lfn_curr,lfn_fin,rk_coeff)

! Calculates right-hand-side forcing and advances a RK-stage the level-set reinitialization PDE
! Domingo Munoz-Esparza, NCAR/RAL
! Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate   
! fire-spread algorithm in the Weather Research and Forecasting model using the 
! level-set method", Journal of Advances in Modeling Earth Systems, 2018      
! https://doi.org/10.1002/2017MS001108


implicit none

integer,intent(in)::ifms,ifme,jfms,jfme,ifts,ifte,jfts,jfte,ifds,ifde,jfds,jfde,bdy_eno1
real,dimension(ifms:ifme,jfms:jfme),intent(in)::lfn_s0,lfn_ini,lfn_curr
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_fin
real,intent(in)::dx,dy,dt_s,threshold_HLu,rk_coeff
!
integer::i,j
real::diffLx,diffLy,diffRx,diffRy,diff2x,diff2y,grad,tend_r
!

    do j=jfts,jfte 
        do i=ifts,ifte 
          if (i.lt.ifds+bdy_eno1 .OR. i.gt.ifde-bdy_eno1 .OR. j.lt.jfds+bdy_eno1 .OR. j.gt.jfde-bdy_eno1) then ! 
            diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
            diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
            diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
            diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
            diff2x=select_eno(diffLx,diffRx)
            diff2y=select_eno(diffLy,diffRy)
          else
            select case(fire_upwinding_reinit)
            case(1)
             diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
             diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
             diff2x=select_weno3(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j),lfn_s0(i,j)*diff2x)
             diff2y=select_weno3(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2),lfn_s0(i,j)*diff2y)
            case(2)
             diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
             diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
             diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j),lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
             diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1),lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
            case(3)
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno3(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j),lfn_s0(i,j)*diff2x)
               diff2y=select_weno3(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2),lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            case(4)
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j),lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
               diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1),lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            case default
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j),lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
               diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1),lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            end select
          endif
            grad=sqrt(diff2x*diff2x+diff2y*diff2y)
            tend_r=lfn_s0(i,j)*(1.0-grad)
            lfn_fin(i,j)=lfn_ini(i,j)+(dt_s*rk_coeff)*tend_r
        enddo
    enddo

end subroutine advance_ls_reinit

!
!*****************************
!

subroutine tign_update(ifts,ifte,jfts,jfte, &              ! tile dims
                       ifms,ifme,jfms,jfme, &              ! memory dims
                       ifds,jfds,ifde,jfde, &              ! domain dims         
                       ts,dt,               &              ! scalars in
                       lfn_in,lfn_out,tign  &              ! arrays inout          
                      )

implicit none

integer,intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,ifds,jfds,ifde,jfde
real,dimension(ifms:ifme,jfms:jfme),intent(inout)::tign
real,dimension(ifms:ifme,jfms:jfme),intent(in)::lfn_in,lfn_out
real,intent(in)::ts,dt
real::time_now
integer::i,j,k,kk
intrinsic epsilon
character(len=128)::msg
!*** executable

! compute ignition time by interpolation
! the node was not burning at start but it is burning at end
! interpolate from the level functions at start and at end
! lfn_in==lfn   is the level set function value at time ts
! lfn_out  is the level set function value at time ts+dt (after reinitialization)
! 0        is the level set function value at time tign(i,j)
! thus assuming the level function is approximately linear =>
! tign(i,j)= ts + ((ts + td) - ts) * lfn_in / (lfn_in - lfn_out)
!        = ts + dt * lfn_in / (lfn_in - lfn_out)
    
time_now=ts+dt
time_now = time_now + abs(time_now)*epsilon(time_now)*2.
do j=jfts,jfte
    do i=ifts,ifte
        ! interpolate the cross-over time
        if (.not. lfn_out(i,j)>0 .and. lfn_in(i,j)>0)then
            tign(i,j) = ts + dt * lfn_in(i,j) / (lfn_in(i,j) - lfn_out(i,j))
    endif
        ! set the ignition time outside of burning region
        if(lfn_out(i,j)>0.)tign(i,j)=time_now
    enddo
enddo
! stop simulation if fire is within boundary_guard grid points from the domain boundaries 
do j=jfts,jfte
  if (j.le.boundary_guard .or. j.gt.(jfde-boundary_guard)) then
    do i=ifts,ifte
      if (lfn_out(i,j).lt.0.) then 
        WRITE(msg,*)'j-boundary reached'
        CALL wrf_debug(0,msg)
        WRITE(msg,*)'i,j,lfn_out=',i,j,lfn_out(i,j)
        CALL wrf_debug(0,msg)
        call crash('wrf: SUCCESS COMPLETE WRF. Fire has reached domain boundary.')
      endif
    enddo
  endif
enddo
do i=ifts,ifte
  if (i.le.boundary_guard .or. i.gt.(ifde-boundary_guard)) then
    do j=jfts,jfte
      if (lfn_out(i,j).lt.0.) then 
        WRITE(msg,*)'i-boundary reached'
        CALL wrf_debug(0,msg)
        WRITE(msg,*)'i,j,lfn_out=',i,j,lfn_out(i,j)
        CALL wrf_debug(0,msg)
        call crash('wrf: SUCCESS COMPLETE WRF. Fire has reached domain boundary.')
      endif
    enddo
  endif
enddo
    
call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, &
               lfn_out,'prop_ls: lfn out')

end subroutine tign_update

!
!*****************************
!

subroutine calc_flame_length(ifts,ifte,jfts,jfte, &
                             ifms,ifme,jfms,jfme, &
                             ros,iboros,flame_length,ros_fl,fire_area)

! Diagnoses flame length
! Domingo Munoz-Esparza, NCAR/RAL

implicit none

integer,intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme
real,dimension(ifms:ifme,jfms:jfme),intent(in)::ros,iboros,fire_area
real,dimension(ifms:ifme,jfms:jfme),intent(out)::flame_length,ros_fl
! local
integer::i,j

do j=jfts,jfte
  do i=ifts,ifte
    if (fire_area(i,j).gt.0.0 .AND. fire_area(i,j).lt.1.0) then
      flame_length(i,j)=0.0775*(iboros(i,j)*ros(i,j))**0.46 ! flame length according to Byram (1959)
      ros_fl(i,j)=ros(i,j)
    else
      flame_length(i,j)=0.0
      ros_fl(i,j)=0.0
    endif
  enddo
enddo

end subroutine calc_flame_length

!
!*****************************
!

subroutine tend_ls( id, &
    ids,ide,jds,jde,    &              ! domain - nodes where lfn defined
    ips,ipe,jps,jpe,    &              ! patch - nodes owned by this process 
    its,ite,jts,jte,    &              ! region - nodes where tend computed
    ims,ime,jms,jme,    &              ! memory dims for ros 
    t,dt,dx,dy,         &              ! scalars in
    lfn,                &              ! arrays in
    tbound,             &              ! scalars out 
    tend, ros,          &              ! arrays out
    fp                  &
)

implicit none
! purpose
! compute the right hand side of the level set equation

!*** arguments
integer,intent(in)::id 
integer,intent(in)::ims,ime,jms,jme,its,ite,jts,jte
integer, intent(in)::ids,ide,jds,jde,ips,ipe,jps,jpe 
real,intent(in)::t                                 ! time
real,intent(in)::dt,dx,dy                          ! mesh step
real,dimension(ims:ime,jms:jme),intent(inout)::lfn ! level set function 
real,dimension(ims:ime,jms:jme),intent(out)::tend  ! tendency (rhs of the level set pde)
real,dimension(ims:ime,jms:jme),intent(out)::ros   ! rate of spread 
real,intent(out)::tbound                           ! max allowed time step
type(fire_params),intent(in)::fp

!*** local 
real:: te,diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy,diff2x,diff2y,grad,rr, &
   ros_base,ros_wind,ros_slope,ros_back,advx,advy,scale,nvx,nvy, &
   speed,tanphi
integer::i,j,itso,iteo,jtso,jteo
character(len=128)msg

! constants
real, parameter:: eps=epsilon(0.0)
!intrinsic epsilon
real, parameter:: zero=0.,one=1.,tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe
!
real :: diff2xn, diff2yn
real :: a_valor, signo_x, signo_y
real :: threshold_HLl,threshold_HLu
real :: threshold_AV,fire_viscosity_var
integer,parameter :: bdy_eno1=10

! f90 intrinsic function

intrinsic max,min,sqrt,nint,tiny,huge


!*** executable

threshold_HLl=-fire_lsm_band_ngp*dx 
threshold_HLu=fire_lsm_band_ngp*dx
threshold_AV=fire_viscosity_ngp*dx
    
    call continue_at_boundary(1,1,fire_lfn_ext_up, &   !extend by extrapolation but never down 
    ims,ime,jms,jme, &                                 ! memory dims
    ids,ide,jds,jde, &                                 ! domain
    ips,ipe,jps,jpe, &                                 ! patch
    its,ite,jts,jte, &                                 ! tile
    itso,iteo,jtso,jteo, &                             ! where set now
    lfn)                                               ! field 

    tbound=0    
    do j=jts,jte
        do i=its,ite
            ! one sided differences
            diffRx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx = (lfn(i,j)-lfn(i-1,j))/dx
            diffRy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy = (lfn(i,j)-lfn(i,j-1))/dy
            diffCx = diffLx+diffRx   ! 	TWICE CENTRAL DIFFERENCE
            diffCy = diffLy+diffRy

            if (i.lt.ids+bdy_eno1 .OR. i.gt.ide-bdy_eno1 .OR. j.lt.jds+bdy_eno1 .OR. j.gt.jde-bdy_eno1) then ! DME: use eno1 near domain boundaries
              diff2x=select_eno(diffLx,diffRx)
              diff2y=select_eno(diffLy,diffRy)
              grad=sqrt(diff2x*diff2x + diff2y*diff2y)
            else
              select case(fire_upwinding)
              case(0)  ! none
                  grad=sqrt(diffCx**2 + diffCy**2)
              case(1) ! standard
                  diff2x=select_upwind(diffLx,diffRx)
                  diff2y=select_upwind(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(2) ! godunov per osher/fedkiw
                  diff2x=select_godunov(diffLx,diffRx)
                  diff2y=select_godunov(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(3) ! eno
                  diff2x=select_eno(diffLx,diffRx)
                  diff2y=select_eno(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(4) ! Sethian - twice stronger pushdown of bumps
                  grad=sqrt(max(diffLx,0.)**2+min(diffRx,0.)**2   &
                          + max(diffLy,0.)**2+min(diffRy,0.)**2)
              case(5) ! 2nd order (DME)
                  diff2x=select_2nd(rr,dx,lfn(i,j),lfn(i-1,j),lfn(i+1,j))
                  diff2y=select_2nd(rr,dy,lfn(i,j),lfn(i,j-1),lfn(i,j+1))
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(6) ! 3rd-order WENO (DME)
                  a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*fp%vx(i,j)+ &
                          select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*fp%vy(i,j)
                  signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                  signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                  diff2x=select_weno3(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j),signo_x)
                  diff2y=select_weno3(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2),signo_y)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(7) ! 5th-order WENO (DME)
                  a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*fp%vx(i,j)+ &
                          select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*fp%vy(i,j)
                  signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                  signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                  diff2x=select_weno5(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i-3,j),lfn(i+1,j),lfn(i+2,j),lfn(i+3,j),signo_x)
                  diff2y=select_weno5(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j-3),lfn(i,j+1),lfn(i,j+2),lfn(i,j+3),signo_y)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(8) ! high-order local band method WENO3/ENO1 (DME)
                  if (abs(lfn(i,j)).lt.threshold_HLu) then
                    a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*fp%vx(i,j)+ &
                            select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*fp%vy(i,j)
                    signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                    signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                    diff2x=select_weno3(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j),signo_x)
                    diff2y=select_weno3(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2),signo_y)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  else
                    diff2x=select_eno(diffLx,diffRx)
                    diff2y=select_eno(diffLy,diffRy)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  endif
              case(9) ! high-order local band method WENO5/ENO1 (DME)
                  if (abs(lfn(i,j)).lt.threshold_HLu) then
                    a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*fp%vx(i,j)+ &
                            select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*fp%vy(i,j)
                    signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                    signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                    diff2x=select_weno5(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i-3,j),lfn(i+1,j),lfn(i+2,j),lfn(i+3,j),signo_x)
                    diff2y=select_weno5(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j-3),lfn(i,j+1),lfn(i,j+2),lfn(i,j+3),signo_y)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  else
                    diff2x=select_eno(diffLx,diffRx)
                    diff2y=select_eno(diffLy,diffRy)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  endif
              case default
                   grad=0.
              end select
            endif
! DME use the same discretization scheme from fire_upwinding option to calculate the normals
            scale=sqrt(grad**2.0+eps) 
            nvx=diff2x/scale 
            nvy=diff2y/scale 

            ! wind speed in direction of spread
            speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
            ! get rate of spread from wind speed and slope
            call fire_ros(ros_base,ros_wind,ros_slope, &
            nvx,nvy,i,j,fp)
            rr=ros_base + ros_wind + fire_slope_factor*ros_slope

            if(fire_grows_only.gt.0)rr=max(rr,0.)
            ! set ros for output
            if(i.ge.its.and.i.le.ite.and.j.ge.jts.and.j.le.jte)ros(i,j)=rr

            if(fire_upwind_split.eq.0)then

                ! get rate of spread
                te = -rr*grad   ! normal term 

            else

                ! normal direction backing rate only
                te = - ros_base*grad

		! advection in wind direction 
                if (abs(speed)> eps) then
                    advx=fp%vx(i,j)*ros_wind/speed
                    advy=fp%vy(i,j)*ros_wind/speed
                else 
                    advx=0
                    advy=0
                endif

                tanphi =  fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
		! advection from slope direction 
                if(abs(tanphi)>eps) then
                    advx=advx+fp%dzdxf(i,j)*ros_slope/tanphi
                    advy=advy+fp%dzdyf(i,j)*ros_slope/tanphi
                endif

                if(fire_upwind_split.eq.1)then   

                    ! one-sided upwinding
                    te = te - max(advx,0.)*diffLx - min(advx,0.)*diffRy &
                            - max(advy,0.)*diffLy - min(advy,0.)*diffRy


                elseif(fire_upwind_split.eq.2)then   
 
                    ! Lax-Friedrichs
                    call crash('prop_ls: bad fire_upwind_split, Lax-Friedrichs not done yet')

                else

                    call crash('prop_ls: bad fire_upwind_split')

                endif
            endif

            ! cfl condition
            if (grad > 0.) then
                 tbound = max(tbound,rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad)
            endif

            ! DME: adds spatially variable artificial viscosity
            if (abs(lfn(i,j)).lt.threshold_AV .AND. (i.gt.ids+bdy_eno1 .and. i.lt.ide-bdy_eno1) .AND. (j.gt.jds+bdy_eno1 .and. j.lt.jde-bdy_eno1)) then 
              fire_viscosity_var=fire_viscosity_bg
            elseif (abs(lfn(i,j)).ge.threshold_AV .AND. abs(lfn(i,j)).lt.threshold_AV*(1.0+fire_viscosity_band) .AND. (i.gt.ids+10 .and. i.lt.ide-10) .AND. (j.gt.jds+10 .and. j.lt.jde-10)) then
              fire_viscosity_var=min(fire_viscosity_bg+(fire_viscosity-fire_viscosity_bg)*(abs(lfn(i,j))-threshold_AV)/(fire_viscosity_band*threshold_AV),fire_viscosity)
            else
              fire_viscosity_var=fire_viscosity
            endif
            te=te + fire_viscosity_var*abs(rr)*((diffRx-diffLx)+(diffRy-diffLy))
            tend(i,j)=te
        enddo
    enddo        

    call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme, & 
                   tend,'tend_ls: tend out')

    ! the final CFL bound
    tbound = 1/(tbound+tol)

end subroutine tend_ls

!
!**************************
!
!

real function select_upwind(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x

! upwind differences, L or R if bith same sign, otherwise zero    

diff2x=0
if (diffLx>0.and.diffRx>0.)diff2x=diffLx
if (diffLx<0.and.diffRx<0.)diff2x=diffRx

select_upwind=diff2x
end function select_upwind


!
!**************************
!

real function select_godunov(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x,diffCx

! Godunov scheme: upwind differences, L or R or none    
! always test on > or < never = , much faster because of IEEE
! central diff >= 0 => take left diff if >0, ortherwise 0
! central diff <= 0 => take right diff if <0, ortherwise 0

diff2x=0
diffCx=diffRx+diffLx
if (diffLx>0.and..not.diffCx<0)diff2x=diffLx
if (diffRx<0.and.     diffCx<0)diff2x=diffRx

select_godunov=diff2x
end function select_godunov

!
!**************************
!

real function select_eno(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x

! 1st order ENO scheme

if    (.not.diffLx>0 .and. .not.diffRx>0)then
    diff2x=diffRx
elseif(.not.diffLx<0 .and. .not.diffRx<0)then
    diff2x=diffLx
elseif(.not.diffLx<0 .and. .not.diffRx>0)then
    if(.not. abs(diffRx) < abs(diffLx))then
        diff2x=diffRx
    else
        diff2x=diffLx
    endif
else
    diff2x=0.
endif

select_eno=diff2x
end function select_eno
      
!
!**************************
!


real function select_2nd(ros,dx,lfn_i,lfn_im1,lfn_ip1)
implicit none
real, intent(in):: lfn_i, lfn_im1, lfn_ip1
real, intent(in):: ros, dx
real:: diff2x_p, diff2x_m 

! 2nd-order advection scheme in the x,y-direction (DME)

diff2x_p=0.
diff2x_m=0.
diff2x_p=(lfn_ip1+lfn_i)/(2.*dx)
diff2x_m=(lfn_i+lfn_im1)/(2.*dx)
select_2nd=diff2x_p-diff2x_m
end function select_2nd

!
!**************************
!


real function select_4th(dx,lfn_i,lfn_im1,lfn_im2,lfn_ip1,lfn_ip2)
implicit none
real, intent(in):: lfn_i,lfn_im1,lfn_im2,lfn_ip1,lfn_ip2
real, intent(in):: dx
real:: diff2x_p,diff2x_m 

! 4th-order advection scheme in the x,y-direction (DME)

diff2x_p=0.
diff2x_m=0.
diff2x_p=(7.0*lfn_ip1+7.0*lfn_i-lfn_ip2-lfn_im1)/(12.*dx)
diff2x_m=(7.0*lfn_i+7.0*lfn_im1-lfn_ip1-lfn_im2)/(12.*dx)
select_4th=diff2x_p-diff2x_m
end function select_4th

!
!**************************
!


real function select_weno3(dx,lfn_it,lfn_im1t,lfn_im2t,lfn_ip1t,lfn_ip2t,uf)
implicit none
real, intent(in)::lfn_it,lfn_im1t,lfn_im2t,lfn_ip1t,lfn_ip2t,uf
real:: lfn_i,lfn_im1,lfn_im2,lfn_ip1,lfn_ip2
real, intent(in):: dx
real:: flux_p,flux_m 
real, parameter:: gamma1=1./3.,gamma2=2./3.,tol=1e-6
real:: w1,w2,w1t,w2t,beta1,beta2 
real:: fh_1,fh_2

! 3rd-order advection WENO scheme in the x,y-direction (DME)

flux_p=0.
flux_m=0.

if (uf .ge. 0.0 ) then
  lfn_i=lfn_it
  lfn_im1=lfn_im1t
  lfn_im2=lfn_im2t
  lfn_ip1=lfn_ip1t
else
  lfn_i=lfn_it
  lfn_im1=lfn_ip1t
  lfn_im2=lfn_ip2t
  lfn_ip1=lfn_im1t
endif

! numerical flux at i,j+1/2 face
fh_1=-0.5*lfn_im1+1.5*lfn_i
fh_2=0.5*lfn_i+0.5*lfn_ip1
beta1=(lfn_i-lfn_im1)**2
beta2=(lfn_ip1-lfn_i)**2
w1t=gamma1/(beta1+tol)**2
w2t=gamma2/(beta2+tol)**2
w1=w1t/(w1t+w2t)
w2=w2t/(w1t+w2t)
flux_p=w1*fh_1+w2*fh_2
! numerical flux at i,j-1/2 face
fh_1=-0.5*lfn_im2+1.5*lfn_im1
fh_2=0.5*lfn_im1+0.5*lfn_i
beta1=(lfn_im1-lfn_im2)**2
beta2=(lfn_i-lfn_im1)**2
w1t=gamma1/(beta1+tol)**2
w2t=gamma2/(beta2+tol)**2
w1=w1t/(w1t+w2t)
w2=w2t/(w1t+w2t)
flux_m=w1*fh_1+w2*fh_2

if (uf .ge. 0.0 ) then
  select_weno3=(flux_p-flux_m)/dx
else
  select_weno3=(flux_m-flux_p)/dx
endif
end function select_weno3

!
!**************************
!


real function select_weno5(dx,lfn_it,lfn_im1t,lfn_im2t,lfn_im3t,lfn_ip1t,lfn_ip2t,lfn_ip3t,uf)
implicit none
real, intent(in):: lfn_it,lfn_im1t,lfn_im2t,lfn_im3t,lfn_ip1t,lfn_ip2t,lfn_ip3t,uf
real:: lfn_i,lfn_im1,lfn_im2,lfn_im3,lfn_ip1,lfn_ip2,lfn_ip3
real, intent(in):: dx
real:: flux_p,flux_m 
real, parameter:: gamma1=1./10.,gamma2=3./5.,gamma3=3./10.,tol=1e-6
real:: w1,w2,w3,w1t,w2t,w3t,beta1,beta2,beta3 
real:: fh_1,fh_2,fh_3

! 5th-order advection WENO scheme in the x,y-direction (DME)

flux_p=0.
flux_m=0.

if (uf .ge. 0.0 ) then
  lfn_i=lfn_it
  lfn_im1=lfn_im1t
  lfn_im2=lfn_im2t
  lfn_im3=lfn_im3t
  lfn_ip1=lfn_ip1t
  lfn_ip2=lfn_ip2t
else
  lfn_i=lfn_it
  lfn_im1=lfn_ip1t
  lfn_im2=lfn_ip2t
  lfn_im3=lfn_ip3t
  lfn_ip1=lfn_im1t
  lfn_ip2=lfn_im2t
endif

! numerical flux at i,j+1/2 face
fh_1=(2.0*lfn_im2-7.0*lfn_im1+11.0*lfn_i)/6.0
fh_2=(-1.0*lfn_im1+5.0*lfn_i+2.0*lfn_ip1)/6.0
fh_3=(2.0*lfn_i+5.0*lfn_ip1-1.0*lfn_ip2)/6.0
beta1=(13.0/12.0)*(lfn_im2-2.0*lfn_im1+lfn_i)**2+0.25*(lfn_im2-4.0*lfn_im1+3.0*lfn_i)**2
beta2=(13.0/12.0)*(lfn_im1-2.0*lfn_i+lfn_ip1)**2+0.25*(lfn_im1-lfn_ip1)**2
beta3=(13.0/12.0)*(lfn_i-2.0*lfn_ip1+lfn_ip2)**2+0.25*(3.0*lfn_i-4.0*lfn_ip1+lfn_ip2)**2
w1t=gamma1/(beta1+tol)**2
w2t=gamma2/(beta2+tol)**2
w3t=gamma3/(beta3+tol)**2
w1=w1t/(w1t+w2t+w3t)
w2=w2t/(w1t+w2t+w3t)
w3=w3t/(w1t+w2t+w3t)
flux_p=w1*fh_1+w2*fh_2+w3*fh_3
! numerical flux at i,j-1/2 face
fh_1=(2.0*lfn_im3-7.0*lfn_im2+11.0*lfn_im1)/6.0
fh_2=(-1.0*lfn_im2+5.0*lfn_im1+2.0*lfn_i)/6.0
fh_3=(2.0*lfn_im1+5.0*lfn_i-1.0*lfn_ip1)/6.0
beta1=(13.0/12.0)*(lfn_im3-2.0*lfn_im2+lfn_im1)**2+0.25*(lfn_im3-4.0*lfn_im2+3.0*lfn_im1)**2
beta2=(13.0/12.0)*(lfn_im2-2.0*lfn_im1+lfn_i)**2+0.25*(lfn_im2-lfn_i)**2
beta3=(13.0/12.0)*(lfn_im1-2.0*lfn_i+lfn_ip1)**2+0.25*(3.0*lfn_im1-4.0*lfn_i+lfn_ip1)**2
w1t=gamma1/(beta1+tol)**2
w2t=gamma2/(beta2+tol)**2
w3t=gamma3/(beta3+tol)**2
w1=w1t/(w1t+w2t+w3t)
w2=w2t/(w1t+w2t+w3t)
w3=w3t/(w1t+w2t+w3t)
flux_m=w1*fh_1+w2*fh_2+w3*fh_3

if (uf .ge. 0.0 ) then
  select_weno5=(flux_p-flux_m)/dx
else
  select_weno5=(flux_m-flux_p)/dx
endif
end function select_weno5

!
!**************************
!

real function speed_func(diffCx,diffCy,dx,dy,i,j,fp)
!*** purpose
!    the level set method speed function
implicit none
!*** arguments
real, intent(in)::diffCx,diffCy  ! x and y coordinates of the direction of propagation
real, intent(in)::dx,dy  ! x and y coordinates of the direction of propagation
integer, intent(in)::i,j         ! indices of the node to compute the speed at
type(fire_params),intent(in)::fp
!*** local
real::scale,nvx,nvy,r
real::ros_base , ros_wind , ros_slope
real, parameter:: eps=epsilon(0.0)
!*** executable
            ! normal direction, from central differences
            scale=sqrt(diffCx*diffCx+diffCy*diffCy+eps) 
            nvx=diffCx/scale
            nvy=diffCy/scale
                      
            ! get rate of spread from wind speed and slope

            call fire_ros(ros_base,ros_wind,ros_slope, &
            nvx,nvy,i,j,fp)

            r=ros_base + ros_wind + ros_slope
            if(fire_grows_only.gt.0)r=max(r,0.)
            speed_func=r

end function speed_func

end module module_fr_fire_core
