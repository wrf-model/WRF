module gsi_thinning
!$$$ subprogram documenation block
!
! abstract:  This module contains code which may be used to selectively
!            thin satellite data.
!
! program history log:
!   2006-10-28 Jianjun Xu, developed from GSI 
!   2007-03-30 Zhiquan Liu, modify and add comments
!
!
! Subroutines Included:
!   sub makegrids      - set up thinning grids
!   sub map2tgrids     - map observation to location on thinning grid
!   sub destroygrids   - deallocate thinning grid arrays
!   sub destroy_sfc    - deallocate full horizontal surface arrays
!
! Variable Definitions:
!   def mlat           - number of latitudes in thinning grid
!   def mlon           - number of longitudes in thinning grid
!   def maxthin        - maximum number of obs to retain in thinning grid box
!   def itxmax         - total number of points in thinning grid
!   def istart_val     - starting location on thinning grid for superobs (not used)
!   def icount         - observation counter
!   def ibest_obs      - index of "best" quality obs in thinning grid box
!   def glat           - latitudes on thinning grid
!   def glon           - longitudes on thinning grid
!   def hll            - (i,j) index of point on thinning grid
!   def score_crit     - "best" quality obs score in thinning grid box
!
!$$$ end documentation block
  use da_control, only: kms, kme
  use da_reporting, only : message, da_error
  use gsi_kinds, only: r_kind,i_kind
  use gsi_constants, only: deg2rad,rearth_equator,zero,two,pi,half,one,quarter,&
       rad2deg
  implicit none

  real(r_kind),parameter:: r90   = 90.0_r_kind
  real(r_kind),parameter:: r360  = 360.0_r_kind
  real(r_kind),parameter:: r999  = 999.0_r_kind
  real(r_kind),parameter:: r1000 = 1000.0_r_kind 
 
  ! lat/lon range inside tile
  real(r_kind) rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid

  type thinning_type
! mlat: lat #, mlonx: max lon #, itxmax: grid box #
    integer(i_kind) mlat,maxthin,itxmax,dthin,mlonx,mlony
    integer(i_kind),dimension(0:51):: istart_val
    
! mlon(mlat): lon # in each lat   
    integer(i_kind),allocatable,dimension(:):: mlon,icount,ibest_obs
    integer(i_kind),allocatable,dimension(:,:):: isli

! glat(mlat): lat #, glon(mlat,mlonx), hll(mlat,mlonx)
    integer(i_kind),allocatable,dimension(:,:) :: hll
    integer(i_kind),allocatable,dimension(:,:,:) :: hll_3d
    real(r_kind),allocatable,dimension(:)   :: glat
    real(r_kind),allocatable,dimension(:,:) :: glon,sli,sno
    real(r_kind),allocatable,dimension(:)   :: score_crit
  end type thinning_type

  type(thinning_type), allocatable  :: thinning_grid(:,:)   
  type(thinning_type), allocatable  :: thinning_grid_conv(:)

contains

  subroutine makegrids (n,rmesh,ifgat)
! compute dimention of thinning box
! output (mlat,mlonx,istart_val)
    implicit none

    integer(i_kind), intent(in) :: n  ! sensor index
    real(r_kind), intent(in) :: rmesh ! thinning box size
    integer(i_kind), intent(in) :: ifgat

    logical odd
    integer(i_kind) i,ii,j,k,nlat,nlon
    integer(i_kind) icnt,mlonj
    real(r_kind) delonx,delat,dgv,dx,dy
    real(r_kind) twopi,halfpi,dlon_g,dlat_g,dlon_e,dlat_e
    real(r_kind) factor,factors,delon
    real(r_kind) rkm2dg,glatm,glatx

!   Initialize variables, set constants
      thinning_grid(n,ifgat)%dthin = 1
      thinning_grid(n,ifgat)%maxthin=thinning_grid(n,ifgat)%dthin

      thinning_grid(n,ifgat)%istart_val=0
      twopi  = two*pi
      halfpi = pi*half
      rkm2dg = r360/(twopi*rearth_equator)*r1000

       dx    = rmesh*rkm2dg
       dy    = dx
       thinning_grid(n,ifgat)%mlat  = dlat_grid/dy + half
       thinning_grid(n,ifgat)%mlonx = dlon_grid/dx + half
       delat = dlat_grid/thinning_grid(n,ifgat)%mlat
       delonx= dlon_grid/thinning_grid(n,ifgat)%mlonx
       dgv   = delat*half

       thinning_grid(n,ifgat)%mlat=max(2,thinning_grid(n,ifgat)%mlat)
       thinning_grid(n,ifgat)%mlonx=max(2,thinning_grid(n,ifgat)%mlonx)
    
      do ii=1,thinning_grid(n,ifgat)%maxthin
       thinning_grid(n,ifgat)%istart_val(ii+1)=thinning_grid(n,ifgat)%istart_val(ii)
          icnt=0
          do j = 1,thinning_grid(n,ifgat)%mlat
             glatx = rlat_min + (j-1)*delat
             glatx = glatx*deg2rad
             glatm = glatx + dgv*deg2rad
             factor = abs(cos(abs(glatm)))
             mlonj = nint(thinning_grid(n,ifgat)%mlonx*factor)
             mlonj = max(2,mlonj)
             do i = 1,mlonj
                icnt=icnt+1
                thinning_grid(n,ifgat)%istart_val(ii+1)=thinning_grid(n,ifgat)%istart_val(ii+1)+1
             enddo
          enddo
      end do

! making thinning box
! output: mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat)

    allocate(thinning_grid(n,ifgat)%mlon(thinning_grid(n,ifgat)%mlat), &
             thinning_grid(n,ifgat)%glat(thinning_grid(n,ifgat)%mlat), &
             thinning_grid(n,ifgat)%glon(thinning_grid(n,ifgat)%mlonx,thinning_grid(n,ifgat)%mlat), &
             thinning_grid(n,ifgat)%hll(thinning_grid(n,ifgat)%mlonx,thinning_grid(n,ifgat)%mlat))

!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.

       thinning_grid(n,ifgat)%itxmax=0
      do j = 1,thinning_grid(n,ifgat)%mlat
       thinning_grid(n,ifgat)%glat(j) = rlat_min + (j-1)*delat
       thinning_grid(n,ifgat)%glat(j) = thinning_grid(n,ifgat)%glat(j)*deg2rad
       glatm = thinning_grid(n,ifgat)%glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       mlonj  = nint(thinning_grid(n,ifgat)%mlonx*factor)     
       thinning_grid(n,ifgat)%mlon(j) = max(2,mlonj)
       delon = dlon_grid/thinning_grid(n,ifgat)%mlon(j)

       thinning_grid(n,ifgat)%glat(j) = min(max(-halfpi,thinning_grid(n,ifgat)%glat(j)),halfpi)
       do i = 1,thinning_grid(n,ifgat)%mlon(j)
          thinning_grid(n,ifgat)%itxmax=thinning_grid(n,ifgat)%itxmax+1
          thinning_grid(n,ifgat)%hll(i,j)=thinning_grid(n,ifgat)%itxmax
          thinning_grid(n,ifgat)%glon(i,j) = rlon_min + (i-1)*delon
          thinning_grid(n,ifgat)%glon(i,j) = thinning_grid(n,ifgat)%glon(i,j)*deg2rad
          thinning_grid(n,ifgat)%glon(i,j) = min(max(zero,thinning_grid(n,ifgat)%glon(i,j)),twopi)
       enddo
       !write(6,'(f10.5,i8,2i10)') glat(j)*rad2deg, mlon(j),hll(1,j),hll(mlon(j),j)
       !write(6,'(10f8.3)')   (glon(i,j)*rad2deg,i=1,mlon(j))

    end do

!   Allocate  and initialize arrays
    allocate(thinning_grid(n,ifgat)%icount(thinning_grid(n,ifgat)%itxmax))
    allocate(thinning_grid(n,ifgat)%ibest_obs(thinning_grid(n,ifgat)%itxmax))
    allocate(thinning_grid(n,ifgat)%score_crit(thinning_grid(n,ifgat)%itxmax))

    do j=1,thinning_grid(n,ifgat)%itxmax
       thinning_grid(n,ifgat)%icount(j)     = 0
       thinning_grid(n,ifgat)%ibest_obs(j)  = 0
       thinning_grid(n,ifgat)%score_crit(j) = 9.99e6_r_kind
    end do

    return
  end subroutine makegrids

  subroutine make3grids (n,rmesh, thin_3d)
! compute dimention of thinning box
! output (mlat,mlonx,istart_val)
    implicit none

    integer(i_kind), intent(in) :: n  ! sensor index
    real(r_kind), intent(in) :: rmesh ! thinning box size
    logical, optional, intent(in) :: thin_3d

    integer(i_kind) i,ii,j,k,nlat,nlon
    integer(i_kind) icnt,mlonj
    real(r_kind) delonx,delat,dgv,dx,dy
    real(r_kind) twopi,halfpi,dlon_g,dlat_g,dlon_e,dlat_e
    real(r_kind) factor,factors,delon
    real(r_kind) rkm2dg,glatm,glatx

!   Initialize variables, set constants
      thinning_grid_conv(n)%dthin = 1
      thinning_grid_conv(n)%maxthin=thinning_grid_conv(n)%dthin

      thinning_grid_conv(n)%istart_val=0
      twopi  = two*pi
      halfpi = pi*half
      rkm2dg = r360/(twopi*rearth_equator)*r1000

       dx    = rmesh*rkm2dg
       dy    = dx
       thinning_grid_conv(n)%mlat  = dlat_grid/dy + half
       thinning_grid_conv(n)%mlonx = dlon_grid/dx + half
       delat = dlat_grid/thinning_grid_conv(n)%mlat
       delonx= dlon_grid/thinning_grid_conv(n)%mlonx
       dgv   = delat*half

       thinning_grid_conv(n)%mlat=max(2,thinning_grid_conv(n)%mlat)
       thinning_grid_conv(n)%mlonx=max(2,thinning_grid_conv(n)%mlonx)
    
      do ii=1,thinning_grid_conv(n)%maxthin
       thinning_grid_conv(n)%istart_val(ii+1)=thinning_grid_conv(n)%istart_val(ii)
          icnt=0
          do j = 1,thinning_grid_conv(n)%mlat
             glatx = rlat_min + (j-1)*delat
             glatx = glatx*deg2rad
             glatm = glatx + dgv*deg2rad
             factor = abs(cos(abs(glatm)))
             mlonj = nint(thinning_grid_conv(n)%mlonx*factor)
             mlonj = max(2,mlonj)
             do i = 1,mlonj
                icnt=icnt+1
                thinning_grid_conv(n)%istart_val(ii+1)=thinning_grid_conv(n)%istart_val(ii+1)+1
             enddo
          enddo
      end do

! making thinning box
! output: mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat)

    allocate(thinning_grid_conv(n)%mlon(thinning_grid_conv(n)%mlat), &
             thinning_grid_conv(n)%glat(thinning_grid_conv(n)%mlat), &
             thinning_grid_conv(n)%glon(thinning_grid_conv(n)%mlonx,thinning_grid_conv(n)%mlat))   
     if( .not. present(thin_3d)) then
          allocate(thinning_grid_conv(n)%hll(thinning_grid_conv(n)%mlonx,thinning_grid_conv(n)%mlat))
     else
       allocate(thinning_grid_conv(n)%hll_3d(thinning_grid_conv(n)%mlonx,thinning_grid_conv(n)%mlat, kms:kme))
     end if

!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.

       thinning_grid_conv(n)%itxmax=0

      do j = 1,thinning_grid_conv(n)%mlat
       thinning_grid_conv(n)%glat(j) = rlat_min + (j-1)*delat
       thinning_grid_conv(n)%glat(j) = thinning_grid_conv(n)%glat(j)*deg2rad
       glatm = thinning_grid_conv(n)%glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       mlonj  = nint(thinning_grid_conv(n)%mlonx*factor)     
       thinning_grid_conv(n)%mlon(j) = max(2,mlonj)
       delon = dlon_grid/thinning_grid_conv(n)%mlon(j)

       thinning_grid_conv(n)%glat(j) = min(max(-halfpi,thinning_grid_conv(n)%glat(j)),halfpi)
       do i = 1,thinning_grid_conv(n)%mlon(j)
          thinning_grid_conv(n)%glon(i,j) = rlon_min + (i-1)*delon
          thinning_grid_conv(n)%glon(i,j) = thinning_grid_conv(n)%glon(i,j)*deg2rad
          thinning_grid_conv(n)%glon(i,j) = min(max(zero,thinning_grid_conv(n)%glon(i,j)),twopi)
          if( .not. present(thin_3d)) then
             thinning_grid_conv(n)%itxmax=thinning_grid_conv(n)%itxmax+1
             thinning_grid_conv(n)%hll(i,j)=thinning_grid_conv(n)%itxmax
          else
             do k=kms, kme
                thinning_grid_conv(n)%itxmax=thinning_grid_conv(n)%itxmax+1
                thinning_grid_conv(n)%hll_3d(i,j,k)=thinning_grid_conv(n)%itxmax
             end do 
          end if
       enddo
       !write(6,'(f10.5,i8,2i10)') glat(j)*rad2deg, mlon(j),hll(1,j),hll(mlon(j),j)
       !write(6,'(10f8.3)')   (glon(i,j)*rad2deg,i=1,mlon(j))

    end do

!   Allocate  and initialize arrays
    allocate(thinning_grid_conv(n)%icount(thinning_grid_conv(n)%itxmax))
    allocate(thinning_grid_conv(n)%ibest_obs(thinning_grid_conv(n)%itxmax))
    allocate(thinning_grid_conv(n)%score_crit(thinning_grid_conv(n)%itxmax))

    do j=1,thinning_grid_conv(n)%itxmax
       thinning_grid_conv(n)%icount(j)     = 0
       thinning_grid_conv(n)%ibest_obs(j)  = 0
       thinning_grid_conv(n)%score_crit(j) = 9.99e6_r_kind
    end do

    return
  end subroutine make3grids

  subroutine map2grids(n,ifgat,dlat_earth,dlon_earth,crit1,iobs,itx,ithin,itt,iobsout,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map2grids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps observations to the thinning grid.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-03-25  kistler - define iobsout for the case use_all=.true.
!
!   input argument list:
!         n      - sensor index
!     ifgat      - number of time slots for FGAT and 4DVAR 
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     iuse  - .true. if observation should be used
!
    implicit none
    logical, intent(out) :: iuse
    integer(i_kind), intent(out) :: itt,itx
    integer(i_kind), intent(in)  :: ithin,n,ifgat
    integer(i_kind), intent(inout) :: iobs, iobsout
    real(r_kind),intent(in):: dlat_earth,dlon_earth,crit1

    integer(i_kind) :: ix,iy
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy
    real(r_kind) dist1,crit

!   Compute (i,j) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth

    call grdcrd(dlat1,1,thinning_grid(n,ifgat)%glat,thinning_grid(n,ifgat)%mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,thinning_grid(n,ifgat)%mlat))

    call grdcrd(dlon1,1,thinning_grid(n,ifgat)%glon(1,iy),thinning_grid(n,ifgat)%mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,thinning_grid(n,ifgat)%mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=thinning_grid(n,ifgat)%hll(ix,iy)
    itt=thinning_grid(n,ifgat)%istart_val(ithin)+itx
    if(ithin == 0) itt=0

!   Increment obs counter on coarse mesh grid.  Also accumulate observation
!   score and distance functions

    thinning_grid(n,ifgat)%icount(itx)=thinning_grid(n,ifgat)%icount(itx)+1
!   dist1=one - quarter*(dista + distb)  !dist1 is min at grid box center and 
                                    !ranges from 1 (at corners)to 
                                    !.5 (at center of box)
    crit=crit1*dist1
    iuse=.false.
    
    if(thinning_grid(n,ifgat)%icount(itx) == 1)then

!   Increment obs counter

      iuse=.true.
      iobs=iobs+1
      thinning_grid(n,ifgat)%score_crit(itx)= crit
      thinning_grid(n,ifgat)%ibest_obs(itx) = iobs
      iobsout=iobs

    end if
    if(crit < thinning_grid(n,ifgat)%score_crit(itx) .and. thinning_grid(n,ifgat)%icount(itx) > 1)then
      iuse=.true.
      iobs=iobs+1
      thinning_grid(n,ifgat)%score_crit(itx)= crit
      thinning_grid(n,ifgat)%ibest_obs(itx)=iobs
      iobsout=iobs
    end if

    return
  end subroutine map2grids

  subroutine map2grids_conv(n,dlat_earth,dlon_earth,crit1,iobs,itx,ithin,itt,iobsout,iuse, zk, thin_3d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map2grids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine maps observations to the thinning grid.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-07-23  derber - modify code to thin obs as read in
!   2004-12-08  li, xu - fix bug --> set iuse=.true. when use_all=.true.
!   2005-10-14  treadon - variable name change (dlat0,dlon0) --> d*_earth
!   2006-03-25  kistler - define iobsout for the case use_all=.true.
!
!   input argument list:
!         n      - sensor index
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     iuse  - .true. if observation should be used
!
    implicit none
    logical, intent(out) :: iuse
    integer(i_kind), intent(out) :: itt,itx
    integer(i_kind), intent(in)  :: ithin,n
    integer(i_kind), intent(inout) :: iobs, iobsout
    real(r_kind),intent(in):: dlat_earth,dlon_earth,crit1
    real(r_kind), optional, intent(in) :: zk    
    logical, optional, intent(in) :: thin_3d

    integer(i_kind) :: ix,iy,iz
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy,dz,dzz
    real(r_kind) dist1,crit,dist2
    integer(i_kind) :: model_lev

    model_lev=kme-kms+1

!   Compute (i,j) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth


    call grdcrd(dlat1,1,thinning_grid_conv(n)%glat,thinning_grid_conv(n)%mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,thinning_grid_conv(n)%mlat))

    call grdcrd(dlon1,1,thinning_grid_conv(n)%glon(1,iy),thinning_grid_conv(n)%mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,thinning_grid_conv(n)%mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    dist2=1.
    if( present (thin_3d)) then
       if( present(zk)) then
          iz=int(zk)
          dz=zk-iz
          iz=max(1,min(iz,model_lev))
          dzz=half-min(dz,one-dz)
          dist2=dzz*dzz+half 
          itx=thinning_grid_conv(n)%hll_3d(ix,iy,iz)
       else
          write(unit=message(1),fmt='(A)')' For 3D thinning zk is required'
          call da_error(__FILE__,__LINE__,message(1:1))
       end if
    else  
       itx=thinning_grid_conv(n)%hll(ix,iy)
    end if
    itt=thinning_grid_conv(n)%istart_val(ithin)+itx
    if(ithin == 0) itt=0

!   Increment obs counter on coarse mesh grid.  Also accumulate observation
!   score and distance functions

    thinning_grid_conv(n)%icount(itx)=thinning_grid_conv(n)%icount(itx)+1
!   dist1=one - quarter*(dista + distb)  !dist1 is min at grid box center and 
                                    !ranges from 1 (at corners)to 
                                    !.5 (at center of box)
    crit=crit1*dist1*dist2
    iuse=.false.
    
    if(thinning_grid_conv(n)%icount(itx) == 1)then

!   Increment obs counter

      iuse=.true.
      iobs=iobs+1
      thinning_grid_conv(n)%score_crit(itx)= crit
      thinning_grid_conv(n)%ibest_obs(itx) = iobs
      iobsout=iobs

    end if
    if(crit < thinning_grid_conv(n)%score_crit(itx) .and. thinning_grid_conv(n)%icount(itx) > 1)then
      iuse=.true.
      thinning_grid_conv(n)%score_crit(itx)= crit
      iobsout = thinning_grid_conv(n)%ibest_obs(itx)
    end if

    return
  end subroutine map2grids_conv

  subroutine map2tgrid(n,ifgat,dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
!   input argument list:
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     ifgat      - number of time slots for FGAT and 4DVAR 
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     iuse  - .true. if observation should be used
!     
    implicit none
    logical,intent(out):: iuse
    integer(i_kind),intent(in):: ithin,n,ifgat
    integer(i_kind),intent(out):: itt,itx
    real(r_kind),intent(in):: dlat_earth,dlon_earth,crit1
    real(r_kind),intent(out):: dist1

    integer(i_kind) ix,iy
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy


!   Compute (i,j) indices of coarse mesh grid (grid number 1) which
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth
    
    call grdcrd(dlat1,1,thinning_grid(n,ifgat)%glat,thinning_grid(n,ifgat)%mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,thinning_grid(n,ifgat)%mlat))

    call grdcrd(dlon1,1,thinning_grid(n,ifgat)%glon(1,iy),thinning_grid(n,ifgat)%mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,thinning_grid(n,ifgat)%mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=thinning_grid(n,ifgat)%hll(ix,iy)
    itt=thinning_grid(n,ifgat)%istart_val(ithin)+itx
    if(ithin == 0) itt=0
    iuse=.true.
    if(dist1*crit1 > thinning_grid(n,ifgat)%score_crit(itx) .or. thinning_grid(n,ifgat)%icount(itx) == 0)iuse=.false.

    !write(6,'(a,3f10.3)') 'dlat_earth dlon_earth crit1 ',dlat_earth*rad2deg,dlon_earth*rad2deg,crit1
    !write(6,'(a,2i5,3f10.3,i10,e12.5,2x,L)') 'ix iy',ix,iy,dx,dy,dist1,itx,score_crit(itx),iuse
    return
  end subroutine map2tgrid

  subroutine grdcrd(d,nd,x,nx,flg)
  implicit none
  integer(i_kind),intent(in) :: nd,nx,flg
  real(r_kind),intent(inout) :: d
  real(r_kind),dimension(nx), intent(in) :: x

  integer(i_kind) :: id,ix
! Treat "normal" case in which nx>1
  if(nx>1) then
        if (flg.eq.1) then

!          Case in which x is in increasing order
           if(d<=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d,flg)-1
           end if
           if(ix==nx) ix=ix-1

        else if (flg.eq.(-1)) then

!          Case in which x is in decreasing order
           if(d>=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d,flg)-1
           end if
        end if
        d=float(ix)+(d-x(ix))/(x(ix+1)-x(ix))

! Treat special case of nx=1
  elseif (nx==1) then
        d = one
  endif

  return
end subroutine grdcrd 

  subroutine checkob(n,ifgat,dist1,crit1,itx,iuse)
!
!   input argument list:
!     ifgat  - number of time slots for FGAT and 4DVAR
!     dist1  - quality indicator for distance (smaller = better)
!     crit1      - quality indicator for observation (smaller = better)
!     itx   - combined (i,j) index of observation on thinning grid
!     iuse  - .true. if observation should be used
!
!   output argument list:
!     iuse  - .true. if observation should be used
!
    implicit none
    logical,intent(inout):: iuse
    integer(i_kind),intent(in):: n,itx,ifgat
    real(r_kind),intent(in):: dist1,crit1

!   If data (no thinning), simply return to calling routine
    if(.not. iuse .or. thinning_grid(n,ifgat)%icount(itx)==0)return
    if(crit1*dist1 > thinning_grid(n,ifgat)%score_crit(itx))iuse=.false.

    return
  end subroutine checkob

  subroutine finalcheck(n,ifgat,dist1,crit1,iobs,itx,iobsout,iuse,sis)
!
!   input argument list:
!     ifgat  - number of time slots for FGAT and 4DVAR 
!     dist1  - quality indicator for distance (smaller = better)
!     crit1  - quality indicator for observation (smaller = better)
!     itx    - combined (i,j) index of observation on thinning grid
!     iobs   - observation counter
!     iuse   - .true. if observation should be used
!     sis    - sensor/instrument/satellite 
!
!   output argument list:
!     iobs   - observation counter
!     iobsout- location for observation to be put
!     iuse   - .true. if observation should be used
!
    implicit none
    logical,intent(inout):: iuse
    integer(i_kind),intent(inout):: iobs,iobsout
    integer(i_kind),intent(in):: n,itx,ifgat
    real(r_kind),intent(in):: dist1,crit1
    character(20),intent(in):: sis

    real(r_kind) crit

    if(.not. iuse)return

!   If using all data (no thinning), simply return to calling routine


    crit=crit1*dist1

    if(thinning_grid(n,ifgat)%icount(itx) == 0)then

!   Increment obs counter

      if(iobs < thinning_grid(n,ifgat)%itxmax)then
       iobs=iobs+1
       iobsout=iobs
       thinning_grid(n,ifgat)%score_crit(itx)= crit
       thinning_grid(n,ifgat)%ibest_obs(itx) = iobs
       thinning_grid(n,ifgat)%icount(itx)=thinning_grid(n,ifgat)%icount(itx)+1
      else
       iuse = .false.
       write(6,*)' ndata > maxobs when reading data for ',sis,thinning_grid(n,ifgat)%itxmax
      end if

    else if(crit < thinning_grid(n,ifgat)%score_crit(itx))then
      thinning_grid(n,ifgat)%score_crit(itx)= crit
      iobsout=thinning_grid(n,ifgat)%ibest_obs(itx)
      thinning_grid(n,ifgat)%icount(itx)=thinning_grid(n,ifgat)%icount(itx)+1
    else
      iuse = .false.
    end if


    return
  end subroutine finalcheck

  subroutine destroygrids(n,ifgat)
    implicit none
    integer(i_kind), intent(in) :: n,ifgat
    deallocate(thinning_grid(n,ifgat)%mlon,thinning_grid(n,ifgat)%glat, &
               thinning_grid(n,ifgat)%glon,thinning_grid(n,ifgat)%hll)
    deallocate(thinning_grid(n,ifgat)%icount)
    deallocate(thinning_grid(n,ifgat)%ibest_obs)
    deallocate(thinning_grid(n,ifgat)%score_crit)
    return
  end subroutine destroygrids

  subroutine destroygrids_conv(n, thin_3d)
    implicit none
    integer(i_kind), intent(in) :: n
    logical, optional, intent(in) :: thin_3d
    deallocate(thinning_grid_conv(n)%mlon,thinning_grid_conv(n)%glat,thinning_grid_conv(n)%glon)
    deallocate(thinning_grid_conv(n)%icount)
    deallocate(thinning_grid_conv(n)%ibest_obs)
    deallocate(thinning_grid_conv(n)%score_crit)
    if( present (thin_3d) ) then
       deallocate(thinning_grid_conv(n)%hll_3d)
    else
       deallocate(thinning_grid_conv(n)%hll)
    end if
    return
  end subroutine destroygrids_conv

  subroutine cleangrids_conv(n)
    implicit none
    integer(i_kind), intent(in) :: n
    thinning_grid_conv(n)%icount(:)     = 0
    thinning_grid_conv(n)%ibest_obs(:)  = 0
    thinning_grid_conv(n)%score_crit(:) = 9.99e6_r_kind
    return
  end subroutine cleangrids_conv

  subroutine destroy_sfc(n,ifgat)
    implicit none
    integer(i_kind), intent(in) :: n,ifgat
    deallocate(thinning_grid(n,ifgat)%sli,thinning_grid(n,ifgat)%sno,thinning_grid(n,ifgat)%isli)
    return
  end subroutine destroy_sfc

function isrchf(nx1,x,y,flg)
!                .      .    .                                       .
! subprogram:    isrchf
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   2005-03-07  treadon - add doc block
!
!   input argument list:
!     nx1    - number of input points
!     x      - grid values
!     y      - target value
!     flg    - marks order of values in x
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     isrchf  - array index of input grid value near target value
!
  implicit none
  integer(i_kind):: isrchf
  integer(i_kind),intent(in):: nx1
  integer(i_kind),intent(in):: flg
  real(r_kind),intent(in):: y
  real(r_kind),dimension(nx1),intent(in):: x

  integer(i_kind) k

  if(flg.eq.1) then
    do k=1,nx1
      if(y<=x(k)) then
        isrchf=k

        go to 100
      end if
    end do
  else
    do k=1,nx1
      if(y>=x(k)) then
         isrchf=k
        go to 100
      end if
    end do
  end if

  isrchf=nx1+1
  if(nx1<=0) isrchf=0

100 continue
  return
end  function isrchf

end module gsi_thinning 
