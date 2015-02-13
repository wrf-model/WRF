module module_GW_baseflow

#ifdef MPP_LAND
   use module_mpp_land
#endif
   implicit none

#include "gw_field_include.inc"
#include "rt_include.inc"
!yw #include "namelist.inc"
contains

!------------------------------------------------------------------------------
!DJG   Simple GW Bucket Model
!------------------------------------------------------------------------------

   subroutine simp_gw_buck(ix,jx,ixrt,jxrt,numbasns,basns_area,&
                            gwsubbasmsk, runoff1x, runoff2x, z_gwsubbas_tmp, qin_gwsubbas,&
                            qout_gwsubbas,qinflowbase,gw_strm_msk,gwbas_pix_ct,dist,DT,&
                            C,ex,z_mx,GWBASESWCRT,OVRTSWCRT)
   implicit none
   
!!!Declarations...
   integer, intent(in)                               :: ix,jx,ixrt,jxrt
   integer, intent(in)                               :: numbasns
   integer, intent(in), dimension(ix,jx)             :: gwsubbasmsk
   real, intent(in), dimension(ix,jx)                :: runoff2x 
   real, intent(in), dimension(ix,jx)                :: runoff1x 
   real, intent(in)                                  :: basns_area(numbasns),dist(ixrt,jxrt,9),DT
   real, intent(in),dimension(numbasns)              :: C,ex,z_mx
   real, intent(out),dimension(numbasns)             :: qout_gwsubbas
   real, intent(out),dimension(numbasns)             :: qin_gwsubbas
   real*8                                            :: z_gwsubbas(numbasns)
   real                                              :: qout_max, qout_spill, z_gw_spill
   real, intent(inout),dimension(numbasns)           :: z_gwsubbas_tmp
   real, intent(out),dimension(ixrt,jxrt)            :: qinflowbase
   integer, intent(in),dimension(ixrt,jxrt)          :: gw_strm_msk
   integer, intent(in)                               :: GWBASESWCRT
   integer, intent(in)                               :: OVRTSWCRT
   

   real*8, dimension(numbasns)                      :: sum_perc8,ct_bas8
   real, dimension(numbasns)                        :: sum_perc
   real, dimension(numbasns)                        :: net_perc

   real, dimension(numbasns)                        :: ct_bas
   real, dimension(numbasns)                        :: gwbas_pix_ct
   integer                                          :: i,j,bas
   character(len=19)				    :: header
   character(len=1)				    :: jnk


!!!Initialize variables...
   ct_bas8 = 0
   sum_perc8 = 0.
   net_perc = 0.
   qout_gwsubbas = 0.
   qin_gwsubbas = 0.
   z_gwsubbas = z_gwsubbas_tmp



!!!Calculate aggregated percolation from deep runoff into GW basins...
   do i=1,ix
     do j=1,jx
       do bas=1,numbasns
         if(gwsubbasmsk(i,j).eq.bas) then
           if(OVRTSWCRT.ne.0) then
             sum_perc8(bas) = sum_perc8(bas)+runoff2x(i,j)  !Add only drainage to bucket...runoff2x in (mm)
           else
             sum_perc8(bas) = sum_perc8(bas)+runoff1x(i,j)+runoff2x(i,j)  !Add sfc water & drainage to bucket...runoff1x and runoff2x in (mm)
           end if
           ct_bas8(bas) = ct_bas8(bas) + 1
         end if
       end do
     end do
   end do

#ifdef MPP_LAND
   call sum_real8(sum_perc8,numbasns)
   call sum_real8(ct_bas8,numbasns)
#endif
   sum_perc = sum_perc8
   ct_bas = ct_bas8
   



!!!Loop through GW basins to adjust for inflow/outflow

   DO bas=1,numbasns     ! Loop for GW bucket calcs...
! #ifdef MPP_LAND
!      if(ct_bas(bas) .gt. 0) then
! #endif

     net_perc(bas) = sum_perc(bas) / ct_bas(bas)   !units (mm)
!DJG...old change to cms     qin_gwsubbas(bas) = net_perc(bas)/1000. * ct_bas(bas) * basns_area(bas) !units (m^3)
     qin_gwsubbas(bas) = net_perc(bas)/1000.* &
                         ct_bas(bas)*basns_area(bas)/DT    !units (m^3/s)


!Adjust level of GW depth...(conceptual GW bucket units (mm))
!DJG...old change to cms inflow...     z_gwsubbas(bas) = z_gwsubbas(bas) + net_perc(bas) / 1000.0   ! (m)

!DJG...debug    write (6,*) "DJG...before",C(bas),ex(bas),z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas)

     z_gwsubbas(bas) = z_gwsubbas(bas) + qin_gwsubbas(bas)*DT/( &
                       ct_bas(bas)*basns_area(bas))*1000.   !  units (mm)





!Calculate baseflow as a function of GW bucket depth...

     if(GWBASESWCRT.eq.1) then  !active exponential bucket... if/then for bucket model discharge type...

!DJG...Estimation of bucket 'overflow' (qout_spill) if/when bucket gets filled...
     qout_spill = 0.
     z_gw_spill = 0.
     if (z_gwsubbas(bas).gt.z_mx(bas)) then  !If/then for bucket overflow case...
       z_gw_spill = z_gwsubbas(bas) - z_mx(bas)
       z_gwsubbas(bas) = z_mx(bas)
       write (6,*) "Bucket spilling...", bas, z_gwsubbas(bas), z_mx(bas), z_gw_spill
     else
       z_gw_spill = 0.
     end if   ! End if for bucket overflow case...

     qout_spill = z_gw_spill/1000.*(ct_bas(bas)*basns_area(bas))/DT  !amount spilled from bucket overflow...units (cms)


!DJG...Maximum estimation of bucket outlfow that is limited by total quantity in bucket...
     qout_max = z_gwsubbas(bas)/1000.*(ct_bas(bas)*basns_area(bas))/DT   ! Estimate max bucket disharge limit to total volume in bucket...(m^3/s)


! Assume exponential relation between z/zmax and Q...
!DJG...old...creates non-asymptotic flow...   qout_gwsubbas(bas) = C(bas)*EXP(ex(bas)*z_gwsubbas(bas)/z_mx(bas)) !Exp.model. q_out (m^3/s)
!DJG force asymptote to zero to prevent 'overdraft'... 
!DJG debug hardwire test...       qout_gwsubbas(bas) = 1*(EXP(7.0*10./100.)-1) !Exp.model. q_out (m^3/s)
     qout_gwsubbas(bas) = C(bas)*(EXP(ex(bas)*z_gwsubbas(bas)/z_mx(bas))-1) !Exp.model. q_out (m^3/s)
       
!DJG...Calculation of max bucket outlfow that is limited by total quantity in bucket...
     qout_gwsubbas(bas) = MIN(qout_max,qout_gwsubbas(bas))   ! Limit bucket discharge to max. bucket limit

     write (6,*) "DJG-exp bucket...during",C(bas),ex(bas),z_gwsubbas(bas),qin_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_max, qout_spill



     elseif (GWBASESWCRT.eq.2) then  !Pass through/steady-state bucket

! Assuming a steady-state (inflow=outflow) model...
!DJG convert input and output units to cms...       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3)
       qout_gwsubbas(bas) = qin_gwsubbas(bas)  !steady-state model...(m^3/s)

!DJG...debug       write (6,*) "DJG-pass through...during",C(bas),ex(bas),qin_gwsubbas(bas), z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_max

     end if    ! End if for bucket model discharge type....




!Adjust level of GW depth...
!DJG bug adjust output to be mm and correct area bug...       z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT &
!DJG bug adjust output to be mm and correct area bug...                       / (ct_bas(bas)*basns_area(bas))   !units(m)

     z_gwsubbas(bas) = z_gwsubbas(bas) - qout_gwsubbas(bas)*DT/( &
                       ct_bas(bas)*basns_area(bas))*1000.   ! units (mm)	

!DJG...Combine calculated bucket discharge and amount spilled from bucket...
     qout_gwsubbas(bas) = qout_gwsubbas(bas) + qout_spill   ! units (cms)


     write (6,*) "DJG...after",C(bas),ex(bas),z_gwsubbas(bas),z_mx(bas),z_gwsubbas(bas)/z_mx(bas), qout_gwsubbas(bas), qout_spill
     write (6,*) "DJG...after...calc",bas,ct_bas(bas),ct_bas(bas)*basns_area(bas),basns_area(bas),DT




! #ifdef MPP_LAND
!      endif
! #endif
   END DO                 ! End loop for GW bucket calcs...

   z_gwsubbas_tmp = z_gwsubbas


!!!Distribute basin integrated baseflow to stream pixels as stream 'inflow'...

      qinflowbase = 0.


      do i=1,ixrt
        do j=1,jxrt
!!!    -simple uniform disaggregation (8.31.06)
           if (gw_strm_msk(i,j).gt.0) then

             qinflowbase(i,j) = qout_gwsubbas(gw_strm_msk(i,j))*1000.*DT/ &
                gwbas_pix_ct(gw_strm_msk(i,j))/dist(i,j,9)     ! units (mm) that gets passed into chan routing as stream inflow

           end if
        end do
      end do


!!!    - weighted redistribution...(need to pass accum weights (slope) in...)
!        NOT FINISHED just BASIC framework...
!         do bas=1,numbasns
!           do k=1,gwbas_pix_ct(bas)
!             qinflowbase(i,j) = k*slope
!           end do
!         end do

        z_gwsubbas = z_gwsubbas_tmp

   return

!------------------------------------------------------------------------------
   End subroutine simp_gw_buck
!------------------------------------------------------------------------------




#ifdef MPP_LAND
   subroutine pix_ct_1(in_gw_strm_msk,ixrt,jxrt,gwbas_pix_ct,numbasns)
      USE module_mpp_land
      implicit none
      integer ::    i,j,ixrt,jxrt,numbasns, bas
      integer,dimension(ixrt,jxrt) :: in_gw_strm_msk
      integer,dimension(global_rt_nx,global_rt_ny) :: gw_strm_msk
      real,dimension(numbasns) :: gwbas_pix_ct 

      gw_strm_msk = 0
      call write_IO_rt_int(in_gw_strm_msk, gw_strm_msk)    

      if(my_id .eq. IO_id) then
         gwbas_pix_ct = 0.
         do bas = 1,numbasns  
         do i=1,global_rt_nx
           do j=1,global_rt_ny
             if(gw_strm_msk(i,j) .eq. bas) then
                gwbas_pix_ct(gw_strm_msk(i,j)) = gwbas_pix_ct(gw_strm_msk(i,j)) &
                     + 1.0
             endif
           end do
         end do
         end do
      end if
      call mpp_land_bcast_real(numbasns,gwbas_pix_ct)

      return
   end subroutine pix_ct_1
#endif


!------------------------------------------------------------------------------
! Benjamin Fersch  2d groundwater model
!------------------------------------------------------------------------------
   subroutine gw2d_ini(did,dt,dx)
     use module_GW_baseflow_data, only: gw2d
     implicit none
     integer did
     real dt,dx

	   gw2d(did)%dx=dx
           gw2d(did)%dt=dt
           ! bftodo: develop proper landtype mask
           
           gw2d(did)%compres=0. ! currently not implemented

   return
   end subroutine gw2d_ini

   subroutine gw2d_allocate(did, ix, jx, nsoil)
      use module_GW_baseflow_data, only: gw2d
      implicit none
      integer ix, jx, nsoil
      integer istatus, did
      
      if(gw2d(did)%allo_status .eq. 1) return
      gw2d(did)%allo_status = 1
      
      gw2d(did)%ix = ix
      gw2d(did)%jx = jx


      allocate(gw2d(did)%ltype  (ix,jx))
      allocate(gw2d(did)%elev   (ix,jx))
      allocate(gw2d(did)%bot    (ix,jx))
      allocate(gw2d(did)%hycond (ix,jx))
      allocate(gw2d(did)%poros  (ix,jx))
      allocate(gw2d(did)%compres(ix,jx))
      allocate(gw2d(did)%ho     (ix,jx))
      allocate(gw2d(did)%h      (ix,jx))
      allocate(gw2d(did)%convgw (ix,jx))
!       allocate(gw2d(did)% (ix,jx))

    end subroutine gw2d_allocate


    subroutine gwstep(ix, jx, dx,              &
		      ltype, elev, bot,        &
		      hycond, poros, compres,  &
                      ho, h, convgw,           &
                      ebot, eocn,              &
		      dt, istep)
! #else
!           dx, istep, dt,                          &        !supplied
!           ims,ime,jms,jme,its,ite,jts,jte,           &        !supplied
!           ids,ide,jds,jde,ifs,ife,jfs,jfe)                    !supplied
! #endif

! New (volug): calling routines use change in head, convgw = d(h-ho)/dt.

! Steps ground-water hydrology (head) through one timestep.
! Modified from Prickett and Lonnquist (1971), basic one-layer aquifer 
! simulation program, with mods by Zhongbo Yu(1997).
! Solves S.dh/dt = d/dx(T.dh/dx) + d/dy(T.dh/dy) + "external sources"
! for a single layer, where h is head, S is storage coeff and T is 
! transmissivity. 3-D arrays in main program (hycond,poros,h,bot)
! are 2-D here, since only a single (uppermost) layer is solved.
! Uses an iterative time-implicit ADI method.

! use module_hms_constants



      integer, intent(in) :: ix, jx

      integer, intent(in), dimension(ix,jx) ::  ltype     ! land-sfc type  (supp)
      real,    intent(in), dimension(ix,jx) ::  &
        elev,           &  ! elev/bathymetry of sfc rel to sl (m) (supp)
        bot,            &  ! elev. aquifer bottom rel to sl (m)   (supp)
        hycond,         &  ! hydraulic conductivity (m/s per m/m) (supp)
        poros,          &  ! porosity (m3/m3)                     (supp)
        compres,        &  ! compressibility (1/Pa)               (supp)
        ho                 ! head at start of timestep (m)        (supp)

      real,    intent(inout), dimension(ix,jx) ::  &
        h,              &  ! head, after ghmcompute (m)           (ret)
        convgw             ! convergence due to gw flow (m/s)     (ret)

      real, intent(inout) :: ebot, eocn
     


      integer ::  istep !, dt
      real, intent(in) :: dt, dx

! #endif      
!       eocn  = mean spurious sink for h_ocn = sealev fix (m/s)(ret)
!               This equals the total ground-water flow across 
!               land->ocean boundaries.
!       ebot  = mean spurious source for "bot" fix (m/s) (returned)
!       time  = elapsed time from start of run (sec)
!       dt = timestep length (sec)
!       istep = timestep counter

! Local arrays:

      real, dimension(ix,jx)   :: sf2    ! storage coefficient (m3 of h2o / bulk m3)
      real, dimension(ix,jx,2) ::   t    ! transmissivity (m2/s)..1 for N-S,..2 for E-W
      real, dimension(0:ix+jx) :: b,g    ! work arrays


      real, parameter    :: botinc = 0.01  ! re-wetting increment to fix h < bot
!     parameter (botinc = 0.  )  ! re-wetting increment to fix h < bot
                                 ! (m); else no flow into dry cells
      real, parameter    :: delskip = 0.005 ! av.|dhead| value for iter.skip out(m)
      integer, parameter :: itermax = 10    ! maximum number of iterations
      integer, parameter :: itermin = 3     ! minimum number of iterations
      real, parameter    :: sealev = -1.     ! sea-level elevation (m)


! die müssen noch sortiert, geprüft und aufgeräumt werden
      integer ::                &
        iter,                   &
        j,                      &
        i,                      &
        jp,                     &
        ip,                     &
        ii,                     &
        n,                      &
        jj,                     &
        ierr,                   &
        ier
        
!       real :: su, sc, shp, bb, aa, cc, w, zz, tareal, dtoa, dtot
      real ::                   &
        dy,                     &
        e,                      &
        su,                     &
        sc,                     &
        shp,                    &
        bb,                     &
        dd,                     &
        aa,                     &
        cc,                     &
        w,                      &
        ha,                     &
        delcur,                 &
        dtot,                   &
        dtoa,                   &
        darea,                  &
        tareal,                 &
        zz

#ifdef MPP_LAND
      real mpiDelcur
      integer mpiSize
#endif

      dy = dx
      darea = dx*dy
      
      
      call scopy (ix*jx, ho, 1, h, 1)

!       Top of iterative loop for ADI solution

      iter = 0
!~~~~~~~~~~~~~
   80 continue
!~~~~~~~~~~~~~
      iter = iter+1
      
#ifdef MPP_LAND
       call MPP_LAND_COM_REAL(h, ix, jx, 99)
#endif

      e    = 0.       ! absolute changes in head (for iteration control)
!      eocn = 0.       ! accumulated fixes for h = 0 over ocean (diag)
!      ebot = 0.       ! accumulated fixes for h < bot (diagnostic)

!       Set storage coefficient (sf2)
      
! #ifdef HMSWRF
! 
       tareal = 0.
! 
!       do j=jfs,jfe
!         do i=ifs,ife
! 
! 
! #else
      do j=1,jx
        do i=1,ix
         if(ltype(i,j) .ge. 1) tareal = tareal + darea

! #endif
!         unconfined water table (h < e): V = poros*(h-b)
!                                         dV/dh = poros
!         saturated to surface (h >= e) : V = poros*(e-b) + (h-e)
!                                         dV/dh = 1
!         (compressibility is ignored)
!
!         su = poros(i,j)*(1.-theta(i,j))    ! old (pre-volug)
          su = poros(i,j)                    ! new (volug)
          sc = 1.
 
          if      (ho(i,j).le.elev(i,j) .and. h(i,j).le.elev(i,j)) then
            sf2(i,j) = su
          else if (ho(i,j).ge.elev(i,j) .and. h(i,j).ge.elev(i,j)) then
            sf2(i,j) = sc
          else if (ho(i,j).le.elev(i,j) .and. h(i,j).ge.elev(i,j)) then
            shp = sf2(i,j) * (h(i,j) - ho(i,j))
            sf2(i,j) = shp * sc / (shp - (su-sc)*(elev(i,j)-ho(i,j)))
          else if (ho(i,j).ge.elev(i,j) .and. h(i,j).le.elev(i,j)) then
            shp = sf2(i,j) * (ho(i,j) - h(i,j))
            sf2(i,j) = shp * su / (shp + (su-sc)*(ho(i,j)-elev(i,j)))
          endif

        enddo
      enddo
      
#ifdef MPP_LAND
       ! communicate storage coefficient
       call MPP_LAND_COM_REAL(sf2, ix, jx, 99)

#endif


!==========================
!       Column calculations
!==========================

!       Set transmissivities. Use min(h,elev)-bot instead of h-bot,
!       since if h > elev, thickness of groundwater flow is just
!       elev-bot.

! #ifdef HMSWRF
! 
!       do j=jfs,jfe
!         jp = min (j+1,jfe)
!         do i=ifs,ife
!           ip = min (i+1,ife)
! 
! #else

      do j=1,jx
        jp = min (j+1,jx)
        do i=1,ix
          ip = min (i+1,ix)

! #endif
          t(i,j,2) = sqrt( abs(                                           &
                        hycond(i, j)*(min(h(i ,j),elev(i ,j))-bot(i ,j))  &
                       *hycond(ip,j)*(min(h(ip,j),elev(ip,j))-bot(ip,j))  &
                         )    )                                           &
! #ifdef HMSWRF
                   * (0.5*(dy+dy)) & ! in WRF the dx and dy are usually equal
                   / (0.5*(dx+dx))
! #else
!                    * (0.5*(dy(i,j)+dy(ip,j)))  &
!                    / (0.5*(dx(i,j)+dx(ip,j)))
! #endif

          t(i,j,1) = sqrt( abs(                                           &
                        hycond(i,j )*(min(h(i,j ),elev(i,j ))-bot(i,j ))  &
                       *hycond(i,jp)*(min(h(i,jp),elev(i,jp))-bot(i,jp))  &
                         )    )                                           &
! #ifdef HMSWRF
                   * (0.5*(dx+dx))  &
                   / (0.5*(dy+dy))
! #else
!                    * (0.5*(dx(i,j)+dx(i,jp))) &
!                    / (0.5*(dy(i,j)+dy(i,jp)))
! #endif
        enddo
      enddo

#ifdef MPP_LAND
      ! communicate transmissivities in x and y direction
       call MPP_LAND_COM_REAL(t(:,:,1), ix, jx, 99)
       call MPP_LAND_COM_REAL(t(:,:,2), ix, jx, 99)
#endif
      b = 0.
      g = 0.

!-------------------
      do 190 ii=1,ix
!-------------------
        i=ii
        if (mod(istep+iter,2).eq.1) i=ix-i+1

!          calculate b and g arrays

!>>>>>>>>>>>>>>>>>>>>
        do 170 j=1,jx
!>>>>>>>>>>>>>>>>>>>>
!           bb = (sf2(i,j)/dt) * darea(i,j)
!           dd = ( ho(i,j)*sf2(i,j)/dt ) * darea(i,j)
          bb = (sf2(i,j)/dt) * darea
          dd = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa = 0.0
          cc = 0.0

          if (j-1) 90,100,90 
   90     aa = -t(i,j-1,1)
          bb = bb + t(i,j-1,1)

  100     if (j-jx) 110,120,110
  110     cc = -t(i,j,1)
          bb = bb + t(i,j,1)

  120     if (i-1) 130,140,130
  130     bb = bb + t(i-1,j,2)
          dd = dd + h(i-1,j)*t(i-1,j,2)

  140     if (i-ix) 150,160,150
  150     bb = bb + t(i,j,2)
          dd = dd + h(i+1,j)*t(i,j,2)

  160     w = bb - aa*b(j-1)
          b(j) = cc/w
          g(j) = (dd-aa*g(j-1))/w
!>>>>>>>>>>>>>>>
  170   continue
!>>>>>>>>>>>>>>>

!          re-estimate heads

        e = e + abs(h(i,jx)-g(jx))
        h(i,jx) = g(jx)
        n = jx-1
  180   if (n.eq.0) goto 185
        ha = g(n) - b(n)*h(i,n+1)
        e = e + abs(ha-h(i,n))
        h(i,n) = ha
        n = n-1
        goto 180
  185   continue

!-------------
  190 continue
!-------------

#ifdef MPP_LAND
       call MPP_LAND_COM_REAL(h, ix, jx, 99)
#endif


!=======================
!       Row calculations
!=======================

!       set transmissivities (same as above)

      do j=1,jx
        jp = min (j+1,jx)
        do i=1,ix
          ip = min (i+1,ix)
          t(i,j,2) = sqrt( abs(                                             &
                        hycond(i, j)*(min(h(i ,j),elev(i ,j))-bot(i ,j))    &
                       *hycond(ip,j)*(min(h(ip,j),elev(ip,j))-bot(ip,j))    &
                         )    )                                             &
!                    * (0.5*(dy(i,j)+dy(ip,j)))                               &
!                    / (0.5*(dx(i,j)+dx(ip,j)))
                   * (0.5*(dy+dy))                               &
                   / (0.5*(dx+dx))

          t(i,j,1) = sqrt( abs(                                             &
                        hycond(i,j )*(min(h(i,j ),elev(i,j ))-bot(i,j ))    &
                       *hycond(i,jp)*(min(h(i,jp),elev(i,jp))-bot(i,jp))    &
                         )    )                                             &
                   * (0.5*(dx+dx))                               &
                   / (0.5*(dy+dy))
        enddo
      enddo
      
#ifdef MPP_LAND
      ! communicate transmissivities in x and y direction
       call MPP_LAND_COM_REAL(t(:,:,1), ix, jx, 99)
       call MPP_LAND_COM_REAL(t(:,:,2), ix, jx, 99)
#endif
      b = 0.
      g = 0.

!-------------------
      do 300 jj=1,jx
!-------------------
        j=jj
        if (mod(istep+iter,2).eq.1) j = jx-j+1

!         calculate b and g arrays

!>>>>>>>>>>>>>>>>>>>>
        do 280 i=1,ix
!>>>>>>>>>>>>>>>>>>>>
!           bb = (sf2(i,j)/dt) * darea(i,j)
!           dd = ( ho(i,j)*sf2(i,j)/dt ) * darea(i,j)
          bb = (sf2(i,j)/dt) * darea
          dd = ( ho(i,j)*sf2(i,j)/dt ) * darea
          aa = 0.0
          cc = 0.0

          if (j-1) 200,210,200
  200     bb = bb + t(i,j-1,1)
          dd = dd + h(i,j-1)*t(i,j-1,1)

  210     if (j-jx) 220,230,220
  220     dd = dd + h(i,j+1)*t(i,j,1)
          bb = bb + t(i,j,1)

  230     if (i-1) 240,250,240
  240     bb = bb + t(i-1,j,2)
          aa = -t(i-1,j,2)

  250     if (i-ix) 260,270,260
  260     bb = bb + t(i,j,2)
          cc = -t(i,j,2)

  270     w = bb - aa*b(i-1)
          b(i) = cc/w
          g(i) = (dd-aa*g(i-1))/w
!>>>>>>>>>>>>>>>
  280   continue
!>>>>>>>>>>>>>>>

!          re-estimate heads

        e = e + abs(h(ix,j)-g(ix))
        h(ix,j) = g(ix)
        n = ix-1
  290   if (n.eq.0) goto 295
        ha = g(n)-b(n)*h(n+1,j)
        e = e + abs(h(n,j)-ha)
        h(n,j) = ha
        n = n-1
        goto 290
  295   continue

!-------------
  300 continue
!-------------

!         fix head < bottom of aquifer
! #endif
! 
! #ifdef HMSWRF
! 
!       do j=jfs,jfe
!         do i=ifs,ife
! 
! #else
      do j=1,jx
        do i=1,ix
! #endif
          if (ltype(i,j).eq.1 .and. h(i,j).le.bot(i,j)+botinc) then

! #ifndef HMSWRF
            e = e +  bot(i,j) + botinc - h(i,j)
!             ebot = ebot + (bot(i,j)+botinc-h(i,j))*sf2(i,j)*darea(i,j)
            ebot = ebot + (bot(i,j)+botinc-h(i,j))*sf2(i,j)*darea
! #endif

            h(i,j) = bot(i,j) + botinc
          endif
        enddo
      enddo
!        maintain head = sea level for ocean (only for adjacent ocean,
!        rest has hycond=0)

! #ifdef HMSWRF
! 
!       do j=jfs,jfe
!         do i=its,ife
! 
! #else
      do j=1,jx
        do i=1,ix
! #endif
          if (ltype(i,j).eq.2) then
! #ifndef HMSWRF
            eocn = eocn + (h(i,j)-sealev)*sf2(i,j)*darea
!             eocn = eocn + (h(i,j)-sealev)*sf2(i,j)*darea(i,j)
! #endif
            h(i,j) = sealev
          endif
        enddo
      enddo

!        Loop back for next ADI iteration

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! #ifdef HMSWRF
!       delcur = e/(xdim*ydim)
! #else
      delcur = e/(ix*jx)
! #endif

#ifdef MPP_LAND

call mpi_reduce(delcur, mpiDelcur, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
call MPI_COMM_SIZE( MPI_COMM_WORLD, mpiSize, ierr ) 

mpiDelcur = mpiDelcur/mpiSize

call mpi_bcast(delcur, 1, mpi_real, 0, MPI_COMM_WORLD, ierr)

#endif

      if ( (delcur.gt.delskip*dt/86400. .and. iter.lt.itermax)      &
           .or. iter.lt.itermin ) then
        goto 80
      else
      endif

      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!        Compute convergence rate due to ground water flow (returned)

! #ifdef HMSWRF
! 
!       do j=jfs,jfe
!         do i=ifs,ife
! 
! #else
      do j=1,jx
        do i=1,ix
! #endif
          if (ltype(i,j).eq.1) then
            convgw(i,j) = sf2(i,j) * (h(i,j)-ho(i,j)) / dt
          else
            convgw(i,j) = 0.
          endif
        enddo
      enddo

!        Diagnostic water conservation check for this timestep

      dtot = 0.     ! total change in water storage (m3)
      dtoa = 0.

! #ifdef HMSWRF
! 
!       do j=jts,jte
!         do i=its,ite
! 
! #else
      do j=1,jx
        do i=1,ix
! #endif
          if (ltype(i,j).eq.1) then
! #ifdef HMSWRF
            dtot = dtot + sf2(i,j) *(h(i,j)-ho(i,j)) * darea
            dtoa = dtoa + sf2(i,j) * abs(h(i,j)-ho(i,j)) * darea
! #else
!             dtot = dtot + sf2(i,j) *(h(i,j)-ho(i,j)) * darea(i,j)
!             dtoa = dtoa + sf2(i,j) * abs(h(i,j)-ho(i,j)) * darea(i,j)
! #endif
          endif
        enddo
      enddo

      dtot = (dtot/tareal)/dt   ! convert to m/s, rel to land area
      dtoa = (dtoa/tareal)/dt
      eocn = (eocn/tareal)/dt
      ebot = (ebot/tareal)/dt

      zz = 1.e3 * 86400.                    ! convert printout to mm/day
#ifdef HYDRO_D
        write (*,900)                         &
          dtot*zz, dtoa*zz, -eocn*zz, ebot*zz,     &
          (dtot-(-eocn+ebot))*zz
#endif
  900 format                                       &
        (3x,'    dh/dt       |dh/dt|        ocnflx        botfix',&
            '                  ','      ghmerror'  &
!         /3x,4f9.4,2(9x),e14.4)
        /3x,5(e14.4))
      
      return
      end subroutine gwstep
      
      
      SUBROUTINE SCOPY (NT, ARR, INCA, BRR, INCB)
!
!        Copies array ARR to BRR, incrementing by INCA and INCB
!        respectively, up to a total length of NT words of ARR.
!        (Same as Cray SCOPY.)
!
      real, DIMENSION(*) :: ARR, BRR
      integer :: ia, nt, inca, incb, ib
!
      IB = 1
      DO 10 IA=1,NT,INCA
         BRR(IB) = ARR(IA)
         IB = IB + INCB
   10 CONTINUE
!
      RETURN
      END SUBROUTINE SCOPY

end module module_GW_baseflow   
