module module_membrane_mslp
  implicit none
  private

#if ( HWRF == 1 )

  public :: make_membrane_mslp

  integer, parameter :: npres = 33
  real, parameter :: badheight=-9e9

  ! NCEP Unified Post standard pressure levels (SLPDEF) used for this
  ! membrane MSLP calculation.  These are ALL of the post pressure
  ! levels up to 200mbar:
  real, parameter :: post_stdpres(npres) = (/ 20000.,          &
       22500., 25000., 27500., 30000., 32500., 35000., 37500., 40000., &
       42500., 45000., 47500., 50000., 52500., 55000., 57500., 60000., &
       62500., 65000., 67500., 70000., 72500., 75000., 77500., 80000., &
       82500., 85000., 87500., 90000., 92500., 95000., 97500.,100000./)

  ! index within post_stdpres of the 850mbar, 700mbar and 500mbar
  ! levels, respectively:
  integer, parameter :: k850 = 27, k700=21, k500=13

  ! Pressure "interface" levels, used only for interpolation.  These
  ! are half-way between pressure levels (post_stdpres) in pressure
  ! space (instead of z, Z or density), to match assumptions made in
  ! the post's Memberane MSLP calculation:
  real, parameter :: post_istdpres(npres+1) = (/ 18750., &
       21250., 23750., 26250., 28750., 31250., 33750., 36250., 38750., &
       41250., 43750., 46250., 48750., 51250., 53750., 56250., 58750., &
       61250., 63750., 66250., 68750., 71250., 73750., 76250., 78750., &
       81250., 83750., 86250., 88750., 91250., 93750., 96250., 98750., &
       101250./)

  ! Constants from the NCEP Unified Post used for interpolation and
  ! extrapolation:
  real, parameter :: post_H1=1.0
  real, parameter :: post_PQ0=379.90516
  real, parameter :: post_A2=17.2693882
  real, parameter :: post_A3=273.16
  real, parameter :: post_A4=35.86
  real, parameter :: post_D608=0.608
  real, parameter :: post_RD=287.04
  real, parameter :: post_G=9.81
  real, parameter :: post_GAMMA=6.5E-3
  real, parameter :: post_RGAMOG=post_RD*post_GAMMA/post_G
  real, parameter :: post_RHmin=1.0E-6     ! minimal RH bound
  real, parameter :: post_smallQ=1.E-12

  real, parameter :: post_slope=-6.6e-4 ! K/km

  REAL, PARAMETER :: old_COEF3=post_RD*post_SLOPE
  REAL, PARAMETER :: old_COEF2=-1./old_COEF3

contains

  subroutine make_membrane_mslp(grid)
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 :: message

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE

    ! Make sure the two constant pressure level values are right:
100 format('In module_membrane_mslp, post_stdpres(',A,')=',F0.3,' but should be ',F0.3)
    if(abs(post_stdpres(k850)-85000.)>1) then
       write(message,100) 'k850',post_stdpres(k850),85000.
       call wrf_error_fatal(message)
    endif
    if(abs(post_stdpres(k700)-70000.)>1) then
       write(message,100) 'k850',post_stdpres(k700),70000.
       call wrf_error_fatal(message)
    endif

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    call membrane_mslp_impl(grid,         &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

  end subroutine make_membrane_mslp

  ! ------------------------------------------------------------
  ! BEGIN IMPLEMENTATION
  ! ------------------------------------------------------------


  ! ------------------------------------------------------------
  ! membrane_mslp_impl - top-level implementation function
  subroutine membrane_mslp_impl(grid, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain
    USE MODULE_RELAX
#ifdef DM_PARALLEL
    USE MODULE_COMM_DM, ONLY : HALO_NMM_MEMBRANE_INTERP_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_integer
#endif

    implicit none

    type(domain), intent(inout) :: grid

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real :: presTv(ips:ipe,jps:jpe,npres), Pmsl(ips:ipe,jps:jpe)
    real :: presZ(ips:ipe,jps:jpe,npres)

    real :: interP(ips:ipe,jps:jpe,npres+1), interZ(ips:ipe,jps:jpe,npres+1)

    logical :: ground_mask(ips:ipe,jps:jpe,npres)
    integer :: ground_level(ips:ipe,jps:jpe)
    integer :: ipres,i,j,mpres,imin,jmin,k,need_to_relax,imax,jmax
    real :: pmin
    character*255 :: message

    if(size(grid%p700rv)>1) then
       ! Need a halo for winds in order to get vorticity and H point wind magnetudes:
#ifdef DM_PARALLEL
#      include "HALO_NMM_MEMBRANE_INTERP.inc"
#endif
    endif

    ! UPPER BOUND: MPRES

    ! Find mpres: the lowest pressure that we need to handle.  This is
    ! mostly for efficiency: we don't need to interpolate or relax
    ! anything higher in the atmosphere than the next pressure level
    ! above the domain-wide lowest surface pressure:
    pmin=9e9
    imin=-99
    jmin=-99
    do j=max(jps,jds),min(jpe,jde-1)
       do i=max(ips,ids),min(ipe,ide-1)
          pmin=min(pmin,grid%pint(i,j,1))
          imin=i
          jmin=j
       enddo
    enddo
#ifdef DM_PARALLEL
    call wrf_dm_minval_real(pmin,imin,jmin)
#endif

    ! FIXME: DON'T HANDLE ANYTHING ABOVE PMIN
    ! NOTE: MUST HANDLE TWO LEVELS ABOVE

    ! Step 1: calculate Tv, Q and Z on pressure levels using the same
    ! method as the NCEP Unified Post:
    call calculate_3D(grid,presTv,presZ,ground_mask,ground_level, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    ! Step 2: smooth Tv through an overrelaxation method:

    ! Modify the relax mask so that the outermost three rows and
    ! columns are always relaxed.  This is needed to overcome bad
    ! values fed in from the parent every timestep.  Setting the mask
    ! to true on the boundaries of the domain prevent them from being
    ! used as boundaries of the overrelaxation.  

    ! (Some of the reasons for boundary issues: The parent and nest
    ! terrain will never match because the nest terrain is smoothed on
    ! the boundary, and the parent is not.  Also, the user may have
    ! set a different terrain for different domains in their
    ! namelist.wps, in which case you'll get an even worse mismatch.
    ! Every time the nest moves, ips terrain changes on the leading
    ! and trailing edges of the nest.  That causes huge shocks when
    ! there are high mountains near the boundaries.  If you do a plot
    ! of 500mbar geopotential height, it looks like a piece of jello
    ! shaking every time the nest moves.  Also, there is some
    ! weirdness on the lateral boundaries of the MOAD due to the
    ! mismatch between GFS terrain (which has its higher spectral
    ! components discarded) and the smoothed HWRF terrain.)

    grid%relaxmask=.true.

    ! Now loop over all vertical levels and relax them:
    do ipres=npres,1,-1
       ! In the inner regions (all but outermost row & col) set the
       ! relaxmask to the ground_mask:
       need_to_relax=0
       do j=max(jps,jds+1),min(jde-2,jpe)
          do i=max(ips,ids+1),min(ide-2,ipe)
             grid%relaxmask(i,j)=ground_mask(i,j,ipres)
             if(grid%relaxmask(i,j)) need_to_relax=1
          enddo
       enddo

       ! If we do not need to relax any points, we are done.
#ifdef DM_PARALLEL
       call wrf_dm_maxval_integer(need_to_relax,imax,jmax)
#endif
       if(need_to_relax==0) then
 38       format('end mslp relax loop at ',I0)
          write(message,38) ipres
          call wrf_debug(2,message)
          exit
       endif

       ! Store Tv in relaxwork:
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             grid%relaxwork(i,j)=presTv(i,j,ipres)
          enddo
       enddo

       ! Overrelax:
       call relax4e(grid,0.7,100,2, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)

       ! Copy back the modified relaxation mask
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             ground_mask(i,j,ipres)=grid%relaxmask(i,j)
          enddo
       enddo

       ! Copy the relaxed values back to Tv:
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             presTv(i,j,ipres)=grid%relaxwork(i,j)
          enddo
       enddo
    end do

    ! Step 3: Solve for Z on interface levels (pressure space
    ! interface levels) using the hydrostatic equation.  Once Z=0 is
    ! reached, solve for Pmsl.
    call calculate_interP(presTv,presZ,grid%Z,Pmsl,grid%PINT, &
         grid%T(:,:,1), grid%Q(:,:,1), &
         ground_level, ground_mask,grid%fis, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    ! Copy the MSLP values back to the grid:
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%membrane_MSLP(i,j)=Pmsl(i,j)
       enddo
    enddo

    ! Smooth the membrane_mslp values:
    call smoothMSLP(grid,1, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    if(size(grid%p850z)>1) then
       ! Copy 700 and 850 mbar heights to their arrays:
       do j=max(jds,jps),min(jde-1,jpe)
          do i=max(ids,ips),min(ide-1,ipe)
             grid%p850z(i,j)=presZ(i,j,k850)
             grid%p700z(i,j)=presZ(i,j,k700)
          enddo
       enddo
    endif

  end subroutine membrane_mslp_impl

  subroutine calculate_3D(grid,presTv,presZ,ground_mask,ground_level, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain
#ifdef DM_PARALLEL
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
    USE MODULE_COMM_DM, ONLY : HALO_NMM_MEMBRANE_INTERP_sub
    use module_dm, only: wrf_dm_maxval_integer
#endif
    implicit none

    type(domain), intent(inout) :: grid

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real, intent(inout) :: presTv(ips:ipe,jps:jpe,npres)
    real, intent(inout) :: presZ(ips:ipe,jps:jpe,npres)

    logical, intent(inout) :: ground_mask(ips:ipe,jps:jpe,npres)
    integer, intent(inout) :: ground_level(ips:ipe,jps:jpe)

    integer :: Tkdest(ips:ipe,jps:jpe), Zkdest(ips:ipe,jps:jpe), Zbottom(ips:ipe,jps:jpe)
    integer :: i,j,ks,a,kd,k
    real :: weight, TL,QL,PL, tempT, RHL, TVRL, TVRBLO, TBLO,QBLO

    integer,target, dimension(ips:ipe,jps:jpe) :: ks850,ks700,ks500
    real, target,dimension(ips:ipe,jps:jpe) :: dummy1,dummy2
    integer, pointer, dimension(:,:) :: ksX
    integer :: nanfound
    real, pointer, dimension(:,:) :: preswind,presrv,presu,presv

    real :: Pmass(ips:ipe,jps:jpe,kds:kde)
    real :: numsum,densum,modelP1,modelP2,pdiff,presQ,presT,ZL,QSAT, U1, V1, U2, V2, dudy1,dvdx1, dudy2,dvdx2
    character*255 :: message
    logical :: wantuv
    
#ifdef DM_PARALLEL
#      include "HALO_NMM_MEMBRANE_INTERP.inc"
#endif

    ! ks: k in source (model level) array
    ! kd: k in destination (pressure level) array
    ground_level=0
    ground_mask=.false.
    Zkdest=1
    Tkdest=1
    Zbottom=0

    ks850=0
    ks700=0
    ks500=0

    ! Interpolate geopotential height to post_stdpres pressure levels
    ! and create a temporary array with non-hydrostatic pressure
    ! (PINT) on model mass points:
    do ks=kde-1,kds,-1
       do j=jps,min(jde-1,jpe)
          iZ: do i=ips,min(ide-1,ipe)
             Pmass(i,j,ks)=sqrt(grid%PINT(i,j,ks)*grid%PINT(i,j,ks+1))
          enddo iZ
       enddo
    enddo

    ! Interpolate temperature and specific humidity to post_stdpres
    ! pressure levels:
    do ks=kde-1,kds+1,-1
       do j=jps,min(jde-1,jpe)
          iTQ: do i=ips,min(ide-1,ipe)
             kd=Tkdest(i,j)
             if(kd<=npres) then
                innerTQ: do while(kd<=npres)
                   if(.not.(post_stdpres(kd)<=Pmass(i,j,ks-1) &
                        .and. post_stdpres(kd)>=Pmass(i,j,ks))) then
                      cycle iTQ
                   endif
                   weight=log(post_stdpres(kd)/Pmass(i,j,ks))/log(Pmass(i,j,ks-1)/Pmass(i,j,ks))

                   presZ(i,j,kd)=weight*grid%Z(i,j,ks-1) + (1.-weight)*grid%Z(i,j,ks)

                   presT=weight*grid%T(i,j,ks-1) + (1.-weight)*grid%T(i,j,ks)
                   presQ=weight*grid%Q(i,j,ks-1) + (1.-weight)*grid%Q(i,j,ks)
                   presTv(i,j,kd)=presT*(1.+post_D608*presQ)

                   if(kd==k850) then
                      ks850(i,j)=ks
                   elseif(kd==k700) then
                      ks700(i,j)=ks
                   elseif(kd==k500) then
                      ks500(i,j)=ks
                   endif

103                format('interp ks=',I0,' kd=',I0,' presT(i=',I0,',j=',I0,',kd)=',F0.3, &
                        ' between T(i,j,ks-1)=',F0.3,' and T(i,j,ks)=', &
                        F0.3,' using weight=',F0.3)
                   !write(0,103) ks,kd,i,j,presT,grid%T(i,j,ks-1),grid%T(i,j,ks),weight
104                format(' Pmass(i,j,ks)=',F0.3,' Pmass(i,j,ks-1)=',F0.3,' post_stdpres(kd)=',F0.3)
                   !write(0,104) Pmass(i,j,ks),Pmass(i,j,ks-1),post_stdpres(kd)
                   if(weight<0 .or. weight>1) then
                      write(0,*) 'Bad weight: ',weight
                      call wrf_error_fatal('bad weight')
                   endif
                   kd=kd+1
                   Tkdest(i,j)=kd
                   Zkdest(i,j)=kd
                   Zbottom(i,j)=ks
                end do innerTQ
             end if
          end do iTQ
       end do
    end do

   ! Interpolate to regions between the middle of the lowest mass
   ! level and the bottom of the atmosphere:
   do j=jps,min(jde-1,jpe)
      iTQ2: do i=ips,min(ide-1,ipe)
         kd=Zkdest(i,j)
         if(kd<=npres) then
            do while(kd<=npres)
               if(.not.(post_stdpres(kd)<=grid%PINT(i,j,kds) &
                    .and. post_stdpres(kd)>=Pmass(i,j,kds))) then
                  cycle iTQ2
               endif

               presT=grid%T(i,j,1)
               presQ=grid%Q(i,j,1)
               presTv(i,j,kd)=presT*(1.+post_D608*presQ)

               weight=log(post_stdpres(kd)/Pmass(i,j,kds))/log(grid%PINT(i,j,kds)/Pmass(i,j,kds))
               presZ(i,j,kd)=(1.-weight)*grid%Z(i,j,1)+weight*grid%fis(i,j)/post_g

               kd=kd+1
               Tkdest(i,j)=kd
               Zkdest(i,j)=kd
               Zbottom(i,j)=ks
            end do
         end if
      end do iTQ2
   end do

1234 format('grid ',I0,': size(',A,') = ',I0)
   write(message,1234) grid%id,'grid%p700rv',size(grid%p700rv)
   call wrf_debug(2,trim(message))
   write(message,1234) grid%id,'grid%p700u',size(grid%p700u)
   call wrf_debug(2,trim(message))

   wantuv=(grid%vortex_tracker == 7) ! do I need to calc. presu & presv?

   ifwind: if(size(grid%p700rv)>1 .or. size(grid%p700u)>1) then
    ! Interpolate wind to H points on pressure levels, calculating
    ! horizontal wind vector magnitude and vertical component of
    ! vorticity.  Interpolate only to 700 and 850 mbar, except for U &
    ! V which are also interpolated to 500mbar.
    nullify(presu)
    nullify(presv)
    windloop: do k=0,2
       if(k==0) then
          ! Only need wind components at 500 mbar
          kd=k500
          ksX=>ks500
          preswind=>dummy1
          presrv=>dummy2
          if(wantuv) then
             presu=>grid%p500u
             presv=>grid%p500v
          endif
       elseif(k==1) then
          ksX=>ks700
          preswind=>grid%p700wind
          presrv=>grid%p700rv
          kd=k700
          if(wantuv) then
             presu=>grid%p700u
             presv=>grid%p700v
          endif
       elseif(k==2) then
          ksX=>ks850
          kd=k850
          preswind=>grid%p850wind
          presrv=>grid%p850rv 
          if(wantuv) then
             presu=>grid%p850u
             presv=>grid%p850v
          endif
       endif

      ! No wind on boundaries:
      if(jps<=jds) then
         do i=ips,min(ide-1,ipe)
            preswind(i,jds)=0
            presrv(i,jds)=0
         enddo
         if(wantuv) then
            do i=ips,min(ide-1,ipe)
               presu(i,jds)=0
               presv(i,jds)=0
            enddo
         endif
      endif
      if(jpe>=jde-1) then
         do i=ips,min(ide-1,ipe)
            preswind(i,jde-1)=0
            presrv(i,jde-1)=0
         enddo
         if(wantuv) then
            do i=ips,min(ide-1,ipe)
               presu(i,jde-1)=0
               presv(i,jde-1)=0
            enddo
         endif
      endif
      if(ips<=ids) then
         do j=jps,min(jde-1,jpe)
            preswind(ids,j)=0
            presrv(ids,j)=0
         enddo
         if(wantuv) then
            do j=jps,min(jde-1,jpe)
               presu(ids,j)=0
               presv(ids,j)=0
            enddo
         endif
      endif
      if(ipe>=ide-1) then
         do j=jps,min(jde-1,jpe)
            preswind(ide-1,j)=0
            presrv(ide-1,j)=0
         enddo
         if(wantuv) then
            do j=jps,min(jde-1,jpe)
               presu(ide-1,j)=0
               presv(ide-1,j)=0
            enddo
         endif
      endif

      ! Interpolate winds:
      do j=max(jps,jds+2),min(jde-2,jpe)
         a=mod(j,2)
         do i=max(ips,ids+2),min(ide-2,ipe)
            ks=ksX(i,j)
            if(ks>1) then
               ! Interpolate between mass levels:
               weight=log(post_stdpres(kd)/Pmass(i,j,ks))/log(Pmass(i,j,ks-1)/Pmass(i,j,ks))

               U1=0.25*(grid%u(i,j-1,ks) + grid%u(i,j+1,ks) + grid%u(i-a,j,ks) + grid%u(i+1-a,j,ks))
               V1=0.25*(grid%v(i,j-1,ks) + grid%v(i,j+1,ks) + grid%v(i-a,j,ks) + grid%v(i+1-a,j,ks))
               U2=0.25*(grid%u(i,j-1,ks-1) + grid%u(i,j+1,ks-1) + grid%u(i-a,j,ks-1) + grid%u(i+1-a,j,ks-1))
               V2=0.25*(grid%v(i,j-1,ks-1) + grid%v(i,j+1,ks-1) + grid%v(i-a,j,ks-1) + grid%v(i+1-a,j,ks-1))
               
               dvdx1 = (grid%v(i+1-a,j,ks)-grid%v(i-a,j,ks))/(2.*grid%dx_nmm(i,j))
               dudy1 = (grid%u(i,j+1,ks)-grid%u(i,j-1,ks))/(2.*grid%dy_nmm)
               dvdx2 = (grid%v(i+1-a,j,ks-1)-grid%v(i-a,j,ks-1))/(2.*grid%dx_nmm(i,j))
               dudy2 = (grid%u(i,j+1,ks-1)-grid%u(i,j-1,ks-1))/(2.*grid%dy_nmm)

               if(wantuv) then
                  presu(i,j)=weight*u2+(1.-weight)*u1
                  presv(i,j)=weight*v2+(1.-weight)*v1
               endif
               preswind(i,j)=weight*sqrt(u2*u2+v2*v2) + (1.-weight)*sqrt(u1*u1+v1*v1)
               presrv(i,j)=(dvdx2-dudy2)*weight + (dvdx1-dudy1)*(1.-weight)
            elseif(post_stdpres(kd)>=Pmass(i,j,kds)) then
               ! At and below lowest mass level, use lowest model level winds
               ks=1
               U1=0.25*(grid%u(i,j-1,ks) + grid%u(i,j+1,ks) + grid%u(i-a,j,ks) + grid%u(i+1-a,j,ks))
               V1=0.25*(grid%v(i,j-1,ks) + grid%v(i,j+1,ks) + grid%v(i-a,j,ks) + grid%v(i+1-a,j,ks))
               
               dvdx1 = (grid%v(i+1-a,j,ks)-grid%v(i-a,j,ks))/(2.*grid%dx_nmm(i,j))
               dudy1 = (grid%u(i,j+1,ks)-grid%u(i,j-1,ks))/(2.*grid%dy_nmm)

               preswind(i,j)=sqrt(u1*u1 + v1*v1)
               presrv(i,j)=dvdx1-dudy1
               if(wantuv) then
                  presu(i,j)=u1
                  presv(i,j)=v1
               endif
            endif
         end do
      end do
   enddo windloop

   ! Calculate 10m wind magnitude and vorticity
   ! NOTE: u10 and v10 are already on H points
   nanfound=0
   do j=max(jps,jds+1),min(jpe,jde-2)
      a=mod(j,2)
      do i=max(ips,ids+1),min(ipe,ide-2)
         grid%m10wind(i,j)=sqrt(grid%u10(i,j)*grid%u10(i,j) + grid%v10(i,j)*grid%v10(i,j))
         dvdx1 = 0.5*(grid%v10(i-a+1,j+1)-grid%v10(i-a,j+1) + &
                     grid%v10(i-a+1,j-1)-grid%v10(i-a,j-1)) / (2*grid%dx_nmm(i,j))
         dudy1 = 0.5*(grid%u10(i-a,j+1)-grid%u10(i-a,j-1) + &
                     grid%u10(i-a+1,j+1)-grid%u10(i-a+1,j-1)) / (2*grid%dy_nmm)
         grid%m10rv(i,j) = dvdx1 - dudy1
         if(grid%m10rv(i,j) == grid%m10rv(i,j)) then
            call wrf_debug(1000,'FIXME: REMOVE THIS CHECK')
         else
3088        format('NaN m10rv at i=',I0,' j=',I0,': a=',I0,' dx=',F0.3,' dy=',F0.3)
            write(message,3088) i,j,a,grid%dx_nmm(i,j),grid%dy_nmm
            call wrf_message2(trim(message))
3089        format('NaN m10rv at i=',I0,' j=',I0,': dvdx1=',F0.5,' dudy=',F0.5)
            write(message,3089) i,j,dvdx1,dudy1
            call wrf_message2(trim(message))
            nanfound=1
         endif
      enddo
   enddo
#ifdef DM_PARALLEL
   call wrf_dm_maxval_integer(nanfound,i,j)
#endif
   if(nanfound/=0) then
      call wrf_error_fatal('ERROR: NaN m10rv seen; aborting.')
   endif
  elseif(grid%id==3) then
     call wrf_error_fatal('ERROR: NOT INTERPOLATING WIND')
  endif ifwind

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          ground_level(i,j)=min(Zkdest(i,j),Tkdest(i,j))
       enddo
    enddo

    do kd=1,npres
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             ground_mask(i,j,kd) = (kd>=ground_level(i,j))
          enddo
       enddo
    enddo

    ! Extrapolate below-ground temperature but not height.  Fill in
    ! badheight for height below ground.  
    jloop2: do j=jps,min(jde-1,jpe)
       iloop2: do i=ips,min(ide-1,ipe)
          if(ground_level(i,j)>npres) then
301          format('Extrap: i=',I0,' j=',I0,' NO EXTRAP: ground at ',I0)
             !write(0,301) i,j,ground_level(i,j)
             cycle iloop2
          else
302          format('Extrap: i=',I0,' j=',I0,' extrap from ',F0.3,' ground at ',I0)
             !write(0,302) i,j,post_stdpres(ground_level(i,j)),ground_level(i,j)
          endif
          kloop2: do kd=ground_level(i,j),npres
             ! Extrapolate first guess below-ground values using the
             ! exact same method used in the post.  Even the constants
             ! are copied from the post:
             PL=grid%PINT(I,J,2)
             ZL=0.5*(grid%Z(I,J,2)+grid%Z(I,J,1))
             TL=0.5*(grid%T(I,J,2)+grid%T(I,J,1))
             QL=0.5*(grid%Q(I,J,2)+grid%Q(I,J,1))
             QSAT=post_PQ0/PL*EXP(post_A2*(TL-post_A3)/(TL-post_A4))
             !
             RHL=QL/QSAT
             !
             IF(RHL.GT.1.)THEN
                RHL=1.
                QL =RHL*QSAT
             ENDIF
             !
             IF(RHL.LT.post_RHmin)THEN
                RHL=post_RHmin
                QL =RHL*QSAT
             ENDIF
             !
             TVRL  =TL*(1.+post_D608*QL)
             TVRBLO=TVRL*(post_stdpres(kd)/PL)**post_RGAMOG
             TBLO  =TVRBLO/(1.+post_D608*QL)

             !QSAT=post_PQ0/post_stdpres(kd)*EXP(post_A2*(TBLO-post_A3)/(TBLO-post_A4))

             !QBLO =RHL*QSAT
             !presQ(i,j,kd)=AMAX1(post_smallQ,QBLO)

             presTv(i,j,kd)=TBLO

             ! Extrapolated virtual temperature:
             !presTv(i,j,kd)=TBLO*(1.+post_D608*QBLO)

             ! extrapolated temperature, with virtual part removed using extrapolated specific humidity:
             !presTv(i,j,kd)=TVRBLO/(1.+post_D608*QBLO)

             ! Below-ground Z is recalcluated after smoothing Tv.  We
             ! only fill in badval here:
             presZ(i,j,kd)=badheight

303          format('Extrap i=',I0,' j=',I0,' kd=',I0,' presTv=',F0.3,' presZ=',F0.3)
304          format('   TL=',F0.3,' QL=',F0.3,' ZL=',F0.3,' QSAT=',F0.3)
305          format('   TVRL=',F0.3,' TVRBLO=',F0.3,' TBLO=',F0.3,' RHL=',F0.3)
             !write(0,303) i,j,kd,presTv(i,j,kd),presZ(i,j,kd)
             !write(0,304) TL,QL,ZL,QSAT
             !write(0,305) TVRL,TVRBLO,TBLO,RHL
          enddo kloop2
       enddo iloop2
    enddo jloop2
  end subroutine calculate_3D

  subroutine calculate_interP( &
       presTv,presZ,modelZ,Pmsl,PINT,T1,Q1, &
       ground_level,ground_mask,fis, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain

    implicit none

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real, intent(in) :: PINT(ims:ime,jms:jme,kms:kme), modelZ(ims:ime,jms:jme,kms:kme)
    real, intent(in) :: T1(ims:ime,jms:jme,1)
    real, intent(in) :: Q1(ims:ime,jms:jme,1)

    real, intent(in) :: fis(ims:ime,jms:jme)
    real, intent(out) :: Pmsl(ips:ipe,jps:jpe)
    real, intent(inout) :: presTv(ips:ipe,jps:jpe,npres)
    real, intent(inout) :: presZ(ips:ipe,jps:jpe,npres)

    logical, intent(inout) :: ground_mask(ips:ipe,jps:jpe,npres)
    integer, intent(inout) :: ground_level(ips:ipe,jps:jpe)

    real :: Z,midTv,dZ,newZ,P,newP,TVRT,TLYR,DIS,oa,slope
    integer :: kp,ip,i,j

    ! What this code does:

    ! For every point where the surface is above Z=0, we start from
    ! the lowest above-ground pressure and integrate the hydrostatic
    ! equation downward to find P at Z=0.

    ! For points where the surface Z<=0 (surface is at or below sea
    ! level), we interpolate to get P at Z=0.


    ! STEP 1: extrapolate below-ground values
    do j=jps,min(jde-1,jpe)
       iloop: do i=ips,min(ide-1,ipe)
          !          nearground: if(modelZ(i,j,1)<50.0) then
          !             Pmsl(i,j)=pint1(i,j,1)
          !                method(i,j)=-30
          !          else
          if(ground_level(i,j)<npres+1) then
             kp=ground_level(i,j)-1
101          format('i=',I0,' j=',I0,' kp=',I0,' ground level =',I0)
             !write(0,101) i,j,kp,ground_level(i,j)
             if(kp<1) then
                call wrf_error_fatal("Lowest model surface pressure is lower than second lowest standard pressure level." )
             endif
             ! Ground is below lowest model level
             !newZ=presZ(i,j,kp)
             !newP=post_stdpres(kp)
             newZ=fis(i,j)/post_G
             newP=pint(i,j,1)
             do ip=kp,npres-1
                P=newP
                Z=newZ
                !                midTv=0.5*(presTv(i,j,ip)+presTv(i,j,ip+1))
                midTv=presTv(i,j,ip+1)
                newP=post_stdpres(ip+1)
                dZ=post_Rd*midTv*alog(P/newP)/post_g
102             format('  make some Z at ip=',I0,': P=',F0.3,' newP=',F0.3)
1021            format('  Z=',F0.3,' midTv=',F0.3,' dZ=',F0.3)
                !write(0,102) ip,P,newP
                !write(0,1021) Z,midTv,dZ
                if(dZ>=0.) then
                   call wrf_error_fatal("dZ>=0.")
                endif
                newZ=Z+dZ
                presZ(i,j,ip+1)=newZ
                if(newZ<=0) then
                   ! interpolate between Z and newZ
1022               format('  extrap using ',F0.3,'/exp(-',F0.3,'*',F0.3,'/(',F0.3,'*',F0.3,'))')
                   !write(0,1022) P,Z,post_G,post_RD,presTV(i,j,ip)


                   !Pmsl(i,j)=P/exp(-Z*post_G/(post_RD*presTv(i,j,ip)))
                   Pmsl(i,j)=(Z*newP-newZ*P)/(-dZ)
10221              format('  result: ',F0.3)
                   !write(0,10221) Pmsl(i,j)
!                   method(i,j)=ip
                   cycle iloop
                endif
             enddo
          endif
          ! If we get here, then Z=0 is below the lowest standard
          ! pressure level and we must extrapolate.

          !               if(pint1(i,j,1)>post_stdpres(npres) .and. modelZ(i,j,1)>0.)then
          !                  ! Model surface pressure is a higher pressure than the
          !                  ! highest standard pressure level.  Use the model
          !                  ! fields to extrapolate.
          !                  TVRT=T1(I,J,1)*(post_H1+post_D608*Q1(I,J,1))
          !                  !DIS=modelZ(I,J,2)-modelZ(I,J,1)+0.5*modelZ(I,J,2) ???
          !                  DIS=0.5*(modelZ(I,J,2)+modelZ(I,J,1))
          !                  TLYR=TVRT-DIS*post_SLOPE*post_G*0.5
          !                  Pmsl(I,J)=PINT(I,J,1)*EXP((modelZ(I,J,1))*post_G/(post_RD*TLYR))
          ! ! 1023            format('  use model: TVRT=',F0.3,' DIS=',F0.3,' TLYR=',F0.3,' Pmsl=',F0.3)
          ! ! 1024            format('     result: ',F0.3,'*EXP(',F0.3,'/(',F0.3,'*',F0.3'))')
          ! !                 write(0,1023) TVRT,DIS,TLYR,Pmsl(i,j)
          ! !                 write(0,1024) PINT(I,J,1),modelZ(I,J,2),post_RD,TLYR
          !                method(i,j)=-20
          !               ELSE
          ! Highest pressure level (post_stdpres(1)) has a
          ! higher pressure than the model surface pressure, so
          ! extrapolate using the pressure level values.
1025      format('  use npres: TLYR=',F0.3,' Pmsl=',F0.3)
1026      format('     result: ',F0.3,'/EXP(-',F0.3,'*',F0.3,'/(',F0.3,'*',F0.3,'))')
          TLYR=presTv(I,J,npres)-presZ(I,J,npres)*post_SLOPE*post_G*0.5
          Pmsl(I,J)=post_stdpres(npres)/EXP(-presZ(I,J,npres)*post_G/(post_RD*TLYR))
          !oa=0.5*post_SLOPE*post_g*presZ(i,j,npres)/TLYR
          !Pmsl(i,j)=post_stdpres(npres)*(1.-oa)**old_coef2
          !write(0,1025) TLYR,Pmsl(I,J)
          !write(0,1026) post_stdpres(npres),presZ(I,J,npres),post_G,post_RD,TLYR
!          method(i,j)=-10
          !             END IF
          !          endif nearground
       enddo iloop
    enddo
  end subroutine calculate_interP

  subroutine smoothMSLP(grid,iterations,  &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    use module_relax
    USE MODULE_DOMAIN, ONLY : domain
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: iterations

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%relaxmask(i,j)=.true.
          grid%relaxwork(i,j)=grid%membrane_mslp(i,j)
       enddo
    enddo

    call relax4e(grid,0.5,iterations,0, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%membrane_mslp(i,j)=grid%relaxwork(i,j)
       enddo
    enddo

  end subroutine smoothMSLP

#endif
end module module_membrane_mslp
