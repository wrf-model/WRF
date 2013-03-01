#define MODAL_AERO
#define WRF_PORT
! module_cam_mam_wateruptake.F
! adapted from cam3 modal_aero_wateruptake.F90 by r.c.easter, june 2010
!Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
! 2010-06-24 rce notes
!--------------------------------------------------------------
! modal_aero_wateruptake.F90


!----------------------------------------------------------------------
!BOP
!
! !MODULE: modal_aero_wateruptake --- modal aerosol mode merging (renaming)
!
! !INTERFACE:
   module modal_aero_wateruptake

! !USES:
   use shr_kind_mod, only : r8 => shr_kind_r8
#ifndef WRF_PORT
   use cam_logfile,  only: iulog
#else
   use module_cam_support, only: iulog
#endif   

   implicit none
   private
   save
                                                                                                                             
!  !PUBLIC MEMBER FUNCTIONS:
   public modal_aero_wateruptake_sub
                                                                                                                             
! !PUBLIC DATA MEMBERS:
!  currently none

                                                                                                                             
! !DESCRIPTION: This module implements ...
!
! !REVISION HISTORY:
!
!   RCE 07.04.13:  Adapted from MIRAGE2 code
!
!EOP
!----------------------------------------------------------------------
!BOC
! list private module data here
!EOC
!----------------------------------------------------------------------
                                                                                                                             
                                                                                                                             
   contains
                                                                                                                             
                                                                                                                             
!----------------------------------------------------------------------
      subroutine modal_aero_wateruptake_sub(                &
                 lchnk, ncol, nstep,                        &
                 iwaterup_flag, loffset,                    &
                 aero_mmr_flag, h2o_mmr_flag,               &
                 deltat, h2ommr, t, pmid, pdel, cldn,       &
                 raer, raertend, dotend, qaerwat,           &
                 dgncur_a, dgncur_awet, wetdens             &
#ifdef OBSRH
                 , obsrh                                    &
#endif
                                                            )
! following are local for now -- wait and see
!                maer, naer, wetrad, density                )

!-----------------------------------------------------------------------
!
! Purpose: Compute aerosol wet radius
!
! Method:  Kohler theory
!
! Author:  S. Ghan
!
!-----------------------------------------------------------------------
      use modal_aero_data
#ifndef WRF_PORT
      use mo_constants,  only: pi
      use pmgrid,        only: plat, plon
      use ppgrid,        only: begchunk, endchunk, pcols, pver
#endif
      use physconst,     only: cpair, epsilo, gravit, mwdry, mwh2o,   &
                               rair, rga, rhoh2o, rh2o, latvap
#ifndef WRF_PORT
      use constituents,  only: pcnst, qmin
#endif
      use wv_saturation, only: aqsat, qsat_water
#ifndef WRF_PORT
      use phys_grid,     only: get_lat_all_p, get_lon_all_p

      use abortutils,    only : endrun
      use cam_history,   only : outfld
      use spmd_utils,    only : masterproc
#else
      use module_cam_support, only: pcnst => pcnst_runtime, &
                                    pcols, pver, &
                                    endrun, masterproc, outfld
      use physconst,     only: pi
#endif

      implicit none

      integer,  intent(in)  :: lchnk              ! chunk index
      integer,  intent(in)  :: ncol               ! number of columns
      integer,  intent(in)  :: nstep              ! time step
      integer,  intent(in)  :: iwaterup_flag       
                               ! identifies call location from typhysbc (1,2)
      integer,  intent(in)  :: loffset            ! offset applied to modal aero "pointers"

      logical,  intent(in)  :: aero_mmr_flag      ! if .true.,  aerosol q are kg/kg-air
                                                  ! if .false., aerosol q are mol/mol-air
      logical,  intent(in)  :: h2o_mmr_flag       ! if .true.,  h2ommr is kg/kg-air
      logical,  intent(inout)::dotend(pcnst)
                               ! identifies species for which tendencies are computed

      real(r8), intent(in)  :: deltat             ! time step (s)
      real(r8), intent(in)  :: h2ommr(pcols,pver) ! layer specific humidity
      real(r8), intent(in)  :: t(pcols,pver)      ! layer temperatures (K)
      real(r8), intent(in)  :: pmid(pcols,pver)   ! layer pressure (Pa)
      real(r8), intent(in)  :: pdel(pcols,pver)   ! layer pressure thickness (Pa)
      real(r8), intent(in)  :: cldn(pcols,pver)   ! layer cloud fraction (0-1)
      real(r8), intent(in)  :: raer(pcols,pver,pcnst)
                               ! aerosol species MRs (kg/kg and #/kg)
      real(r8), intent(inout)::raertend(pcols,pver,pcnst)
                               ! aerosol MR tendencies (kg/kg/s)
                               ! only defined for aerosol water
      real(r8), intent(out)   :: qaerwat(pcols,pver,ntot_amode)
      real(r8), intent(in)    :: dgncur_a(pcols,pver,ntot_amode)
      real(r8), intent(out)   :: dgncur_awet(pcols,pver,ntot_amode)
      real(r8), intent(out)   :: wetdens(pcols,pver,ntot_amode)

! following are local for now -- wait and see
!     real(r8), intent(out) :: maer(pcols,pver,ntot_amode)
!                              ! aerosol wet mass MR (including water) (kg/kg-air)
!     real(r8), intent(out) :: naer(pcols,pver,ntot_amode)
!                              ! aerosol number MR (bounded!) (#/kg-air)
!     real(r8), intent(out) :: wetrad(pcols,pver,ntot_amode)  
!                              ! wet radius of aerosol (m)
!     real(r8), intent(out) :: density(pcols,pver,ntot_amode)
!                              ! wet mean density of aerosol (kg/m3)

!     local variables

      integer i,k,m
      integer icol_diag
      integer l ! species index
      integer lmass ! pointer for aerosol mass
      integer ltype ! pointer for aerosol type
!     integer lwater ! pointer for water on aerosol
      integer  lat(pcols), lon(pcols)      ! lat,lon indices

      real(r8) density_water                   ! density of water (kg/m3)
      real(r8) drydens(ntot_amode)   ! dry particle density  (kg/m^3)
      real(r8) drymass(ntot_amode)   ! single-particle-mean dry mass  (kg)
      real(r8) dryrad(pcols,pver,ntot_amode) ! dry volume mean radius of aerosol (m)
      real(r8) dryvol(ntot_amode)    ! single-particle-mean dry volume (m3)
      real(r8) dryvolmr(ntot_amode)  ! volume MR for aerosol mode (m3/kg)
      real(r8) duma, dumb
      real(r8) es(pcols,pver)        ! saturation vapor pressure (Pa)
      real(r8) hygro(ntot_amode)     ! volume-weighted mean hygroscopicity (--)
      real(r8) hystfac(ntot_amode)   ! working variable for hysteresis
      real(r8) pi43
      real(r8) qs(pcols,pver)        ! saturation specific humidity
      real(r8) qwater                ! aerosol water MR
      real(r8) rh(pcols,pver)        ! relative humidity (0-1)
      real(r8) third
      real(r8) v2ncur_a(pcols,pver,ntot_amode)
      real(r8) wtrvol(ntot_amode)    ! single-particle-mean water volume in wet aerosol (m3)
      real(r8) wetvol(ntot_amode)    ! single-particle-mean wet volume (m3)

      real(r8) :: maer(pcols,pver,ntot_amode)
                              ! aerosol wet mass MR (including water) (kg/kg-air)
      real(r8) :: naer(pcols,pver,ntot_amode)
                              ! aerosol number MR (bounded!) (#/kg-air)
      real(r8) :: wetrad(pcols,pver,ntot_amode)  
                              ! wet radius of aerosol (m)

      character(len=3) :: trnum       ! used to hold mode number (as characters)

!-----------------------------------------------------------------------



! set the dotend's
! currently, this is one of several routines called from aerosol_wet_intr
! so DO NOT re-initialize dotend=.false. and raertend=0.0
!     dotend(:) = .false.
      do m=1,ntot_amode
!        lwater = lwaterptr_amode(m) - loffset
!        dotend(lwater) = .true.
!        raertend(1:ncol,:,lwater) = 0.0
      end do


      third=1./3.
      pi43 = pi*4.0/3.0
      density_water = rhoh2o   ! is (kg/m3)

! compute size-related factors
!    when numptr_amode(m)=0, earlier code set dryrad = prescribed volume.
!    this is no longer needed because
!           dryrad(i,k,m) = (1.0/v2ncur_a(i,k,m)/pi43)**third
!    works in all cases
      do m=1,ntot_amode
           hystfac(m) = 1.0 / max( 1.0e-5_r8,   &
                              (rhdeliques_amode(m)-rhcrystal_amode(m)) )
      enddo

! main loops over i, k

!     call aqsat( t, pmid, es, qs, pcols, ncol, pver, 1, pver )
      do k=1,pver
      do i=1,ncol

         qs(i,k)=qsat_water(t(i,k),pmid(i,k))

         if ( h2o_mmr_flag ) then
            rh(i,k) = h2ommr(i,k)/qs(i,k)
         else
            rh(i,k) = h2ommr(i,k)*mwh2o/(mwdry*qs(i,k))
         end if
         rh(i,k) = max(rh(i,k),0.0_r8)
         rh(i,k) = min(rh(i,k),0.98_r8)
         if (cldn(i,k) .lt. 1.0_r8) then
           rh(i,k) = (rh(i,k) - cldn(i,k)) / (1.0_r8 - cldn(i,k))  ! clear portion
         end if
         rh(i,k) = max(rh(i,k),0.0_r8)
           

!     compute dryvolmr, maer, naer for each mode
         do m=1,ntot_amode

            maer(i,k,m)=0.
            dryvolmr(m)=0.
            hygro(m)=0.
            do l = 1, nspec_amode(m)
               lmass = lmassptr_amode(l,m) - loffset
               ltype = lspectype_amode(l,m)
               if ( aero_mmr_flag ) then
                  duma = raer(i,k,lmass)
               else
                  duma = raer(i,k,lmass)*(specmw_amode(ltype)/mwdry)
               end if
               maer(i,k,m) = maer(i,k,m) + duma
               dumb = duma/specdens_amode(ltype)
               dryvolmr(m) = dryvolmr(m) + dumb
               hygro(m) = hygro(m) + dumb*spechygro(ltype)
            enddo
            if (dryvolmr(m) > 1.0e-30_r8) then
               hygro(m) = hygro(m)/dryvolmr(m)
            else
               hygro(m) = spechygro( lspectype_amode(1,m) )
            end if

!     naer = aerosol number (#/kg)
!     the new v2ncur_a replaces old coding here
            v2ncur_a(i,k,m) = 1. / ( (pi/6.)*                            &
                (dgncur_a(i,k,m)**3.)*exp(4.5*alnsg_amode(m)**2.) )
            naer(i,k,m) = dryvolmr(m)*v2ncur_a(i,k,m)
         enddo   !m=1,ntot_amode

!     compute mean (1 particle) dry volume and mass for each mode
!     old coding is replaced because the new (1/v2ncur_a) is equal to
!        the mean particle volume
!     also moletomass forces maer >= 1.0e-30, so (maer/dryvolmr)
!        should never cause problems (but check for maer < 1.0e-31 anyway)
         do m=1,ntot_amode
            if (maer(i,k,m) .gt. 1.0e-31) then
               drydens(m) = maer(i,k,m)/dryvolmr(m)
            else
               drydens(m) = 1.0
            end if
            dryvol(m) = 1.0/v2ncur_a(i,k,m)
            drymass(m) = drydens(m)*dryvol(m)
            dryrad(i,k,m) = (dryvol(m)/pi43)**third
         enddo

!     compute wet radius for each mode
         do m=1,ntot_amode
            call modal_aero_kohler(                          &
                    dryrad(i,k,m), hygro(m), rh(i,k),        &
                    wetrad(i,k,m), 1, 1                      )

            wetrad(i,k,m)=max(wetrad(i,k,m),dryrad(i,k,m))
            dgncur_awet(i,k,m) = dgncur_a(i,k,m)*   &
                                     (wetrad(i,k,m)/dryrad(i,k,m))
            wetvol(m) = pi43*wetrad(i,k,m)*wetrad(i,k,m)*wetrad(i,k,m)
            wetvol(m) = max(wetvol(m),dryvol(m))
            wtrvol(m) = wetvol(m) - dryvol(m)
            wtrvol(m) = max( wtrvol(m), 0.0_r8 )

!     apply simple treatment of deliquesence/crystallization hysteresis
!     for rhcrystal < rh < rhdeliques, aerosol water is a fraction of
!     the "upper curve" value, and the fraction is a linear function of rh
            if (rh(i,k) < rhcrystal_amode(m)) then
               wetrad(i,k,m) = dryrad(i,k,m)
               wetvol(m) = dryvol(m)
               wtrvol(m) = 0.0_r8
            else if (rh(i,k) < rhdeliques_amode(m)) then
               wtrvol(m) = wtrvol(m)*hystfac(m)   &
                                    *(rh(i,k) - rhcrystal_amode(m))
               wtrvol(m) = max( wtrvol(m), 0.0_r8 )
               wetvol(m) = dryvol(m) + wtrvol(m)
               wetrad(i,k,m) = (wetvol(m)/pi43)**third
            end if

!     compute aer. water tendency = (new_water - old_water)/deltat
!     [ either (kg-h2o/kg-air/s) or (mol-h2o/mol-air/s) ]
!           lwater = lwaterptr_amode(m) - loffset
            if ( aero_mmr_flag ) then
               duma = 1.0_r8
            else
#if (defined MIMIC_CAM3)
               duma = mwdry/18.0_r8
#else
               duma = mwdry/mwh2o
#endif
            end if
            qwater = density_water*naer(i,k,m)*wtrvol(m)*duma

!     old_water (after modal_aero_calcsize) is 
!           qwater_old = raer(i,k,lwater) + raertend(i,k,lwater)*deltat
!     and water tendency is
!           raertend(i,k,lwater) = raertend(i,k,lwater)   &
!                                + (qwater - qwater_old)/deltat
!     which is equivalent to
!           raertend(i,k,lwater) = (qwater - raer(i,k,lwater))/deltat
            qaerwat(i,k,m) = qwater

!     compute aerosol wet density (kg/m3)
            if (wetvol(m) > 1.0e-30_r8) then
               wetdens(i,k,m) = (drymass(m) + density_water*wtrvol(m))/wetvol(m)
            else
               wetdens(i,k,m) = specdens_amode( lspectype_amode(1,m) )
            end if

         enddo


      end do   ! "i=1,ncol"
      end do   ! "k=1,pver"


! output to history
      do m = 1, ntot_amode
         write( trnum, '(i3.3)' ) m
         call outfld( 'wat_a'//trnum(3:3),  qaerwat(:,:,m),     pcols, lchnk)
! note - eventually we should change these from "dgnd_a0N" to "dgnd_aN"
         call outfld( 'dgnd_a'//trnum(2:3), dgncur_a(:,:,m),    pcols, lchnk)
         call outfld( 'dgnw_a'//trnum(2:3), dgncur_awet(:,:,m), pcols, lchnk)
      end do


      return
      end subroutine modal_aero_wateruptake_sub


!-----------------------------------------------------------------------
      subroutine modal_aero_kohler(   &
          rdry_in, hygro, s, rwet_out, im, imx )

! calculates equlibrium radius r of haze droplets as function of
! dry particle mass and relative humidity s using kohler solution
! given in pruppacher and klett (eqn 6-35)

! for multiple aerosol types, assumes an internal mixture of aerosols

      implicit none

! arguments
      integer :: im         ! number of grid points to be processed
      integer :: imx        ! dimensioned number of grid points
      real(r8) :: rdry_in(imx)    ! aerosol dry radius (m)
      real(r8) :: hygro(imx)      ! aerosol volume-mean hygroscopicity (--)
      real(r8) :: s(imx)          ! relative humidity (1 = saturated)
      real(r8) :: rwet_out(imx)   ! aerosol wet radius (m)

! local variables
      integer, parameter :: imax=200
      integer :: i, n, nsol

      real(r8) :: a, b
      real(r8) :: p40(imax),p41(imax),p42(imax),p43(imax) ! coefficients of polynomial
      real(r8) :: p30(imax),p31(imax),p32(imax) ! coefficients of polynomial
      real(r8) :: p
      real(r8) :: r3, r4
      real(r8) :: r(imx)        ! wet radius (microns)
      real(r8) :: rdry(imax)    ! radius of dry particle (microns)
      real(r8) :: ss            ! relative humidity (1 = saturated)
      real(r8) :: slog(imax)    ! log relative humidity
      real(r8) :: vol(imax)     ! total volume of particle (microns**3)
      real(r8) :: xi, xr

      complex(r8) :: cx4(4,imax),cx3(3,imax)

      real(r8), parameter :: eps = 1.e-4
      real(r8), parameter :: mw = 18.
      real(r8), parameter :: pi = 3.14159
      real(r8), parameter :: rhow = 1.
      real(r8), parameter :: surften = 76.
      real(r8), parameter :: tair = 273.
      real(r8), parameter :: third = 1./3.
      real(r8), parameter :: ugascon = 8.3e7


!     effect of organics on surface tension is neglected
      a=2.e4*mw*surften/(ugascon*tair*rhow)

      do i=1,im
           rdry(i) = rdry_in(i)*1.0e6   ! convert (m) to (microns)
           vol(i) = rdry(i)**3          ! vol is r**3, not volume
           b = vol(i)*hygro(i)

!          quartic
           ss=min(s(i),1.-eps)
           ss=max(ss,1.e-10_r8)
           slog(i)=log(ss)
           p43(i)=-a/slog(i)
           p42(i)=0.
           p41(i)=b/slog(i)-vol(i)
           p40(i)=a*vol(i)/slog(i)
!          cubic for rh=1
           p32(i)=0.
           p31(i)=-b/a
           p30(i)=-vol(i)
      end do


       do 100 i=1,im

!       if(vol(i).le.1.e-20)then
        if(vol(i).le.1.e-12)then
           r(i)=rdry(i)
           go to 100
        endif

        p=abs(p31(i))/(rdry(i)*rdry(i))
        if(p.lt.eps)then
!          approximate solution for small particles
           r(i)=rdry(i)*(1.+p*third/(1.-slog(i)*rdry(i)/a))
        else
           call makoh_quartic(cx4(1,i),p43(i),p42(i),p41(i),p40(i),1)
!          find smallest real(r8) solution
           r(i)=1000.*rdry(i)
           nsol=0
           do n=1,4
              xr=real(cx4(n,i))
              xi=imag(cx4(n,i))
              if(abs(xi).gt.abs(xr)*eps) cycle  
              if(xr.gt.r(i)) cycle  
              if(xr.lt.rdry(i)*(1.-eps)) cycle  
              if(xr.ne.xr) cycle  
              r(i)=xr
              nsol=n
           end do  
           if(nsol.eq.0)then
              write(iulog,*)   &
               'ccm kohlerc - no real(r8) solution found (quartic)'
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              write(iulog,*)'roots =', (cx4(n,i),n=1,4)
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              write(iulog,*)'p0-p3 =', p40(i), p41(i), p42(i), p43(i)
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              write(iulog,*)'rh=',s(i)
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              write(iulog,*)'setting radius to dry radius=',rdry(i)
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              r(i)=rdry(i)
!             stop
           endif
        endif

        if(s(i).gt.1.-eps)then
!          save quartic solution at s=1-eps
           r4=r(i)
!          cubic for rh=1
           p=abs(p31(i))/(rdry(i)*rdry(i))
           if(p.lt.eps)then
              r(i)=rdry(i)*(1.+p*third)
           else
              call makoh_cubic(cx3,p32,p31,p30,im)
!             find smallest real(r8) solution
              r(i)=1000.*rdry(i)
              nsol=0
              do n=1,3
                 xr=real(cx3(n,i))
                 xi=imag(cx3(n,i))
                 if(abs(xi).gt.abs(xr)*eps) cycle  
                 if(xr.gt.r(i)) cycle  
                 if(xr.lt.rdry(i)*(1.-eps)) cycle  
                 if(xr.ne.xr) cycle  
                 r(i)=xr
                 nsol=n
              end do  
              if(nsol.eq.0)then
                 write(iulog,*)   &
                  'ccm kohlerc - no real(r8) solution found (cubic)'
#ifdef WRF_PORT
                 call wrf_message(iulog)
#endif 
                 write(iulog,*)'roots =', (cx3(n,i),n=1,3)
#ifdef WRF_PORT
                 call wrf_message(iulog)
#endif 
                 write(iulog,*)'p0-p2 =', p30(i), p31(i), p32(i)
#ifdef WRF_PORT
                 call wrf_message(iulog)
#endif 
                 write(iulog,*)'rh=',s(i)
#ifdef WRF_PORT
                 call wrf_message(iulog)
#endif 
                 write(iulog,*)'setting radius to dry radius=',rdry(i)
#ifdef WRF_PORT
                 call wrf_message(iulog)
#endif 
                 r(i)=rdry(i)
!                stop
              endif
           endif
           r3=r(i)
!          now interpolate between quartic, cubic solutions
           r(i)=(r4*(1.-s(i))+r3*(s(i)-1.+eps))/eps
        endif

  100 continue

! bound and convert from microns to m
      do i=1,im
         r(i) = min(r(i),30._r8) ! upper bound based on 1 day lifetime
         rwet_out(i) = r(i)*1.e-6
      end do

      return
      end subroutine modal_aero_kohler


!-----------------------------------------------------------------------
      subroutine makoh_cubic( cx, p2, p1, p0, im )
!
!     solves  x**3 + p2 x**2 + p1 x + p0 = 0
!     where p0, p1, p2 are real
!
      integer, parameter :: imx=200
      integer :: im
      real(r8) :: p0(imx), p1(imx), p2(imx)
      complex(r8) :: cx(3,imx)

      integer :: i
      real(r8) :: eps, q(imx), r(imx), sqrt3, third
      complex(r8) :: ci, cq, crad(imx), cw, cwsq, cy(imx), cz(imx)

      save eps
      data eps/1.e-20/

      third=1./3.
      ci=dcmplx(0.,1.)
      sqrt3=sqrt(3.)
      cw=0.5*(-1+ci*sqrt3)
      cwsq=0.5*(-1-ci*sqrt3)

      do i=1,im
      if(p1(i).eq.0.)then
!        completely insoluble particle
         cx(1,i)=(-p0(i))**third
         cx(2,i)=cx(1,i)
         cx(3,i)=cx(1,i)
      else
         q(i)=p1(i)/3.
         r(i)=p0(i)/2.
         crad(i)=r(i)*r(i)+q(i)*q(i)*q(i)
         crad(i)=sqrt(crad(i))

         cy(i)=r(i)-crad(i)
         if (abs(cy(i)).gt.eps) cy(i)=cy(i)**third
         cq=q(i)
         cz(i)=-cq/cy(i)

         cx(1,i)=-cy(i)-cz(i)
         cx(2,i)=-cw*cy(i)-cwsq*cz(i)
         cx(3,i)=-cwsq*cy(i)-cw*cz(i)
      endif
      enddo

      return
      end subroutine makoh_cubic


!-----------------------------------------------------------------------
      subroutine makoh_quartic( cx, p3, p2, p1, p0, im )

!     solves x**4 + p3 x**3 + p2 x**2 + p1 x + p0 = 0
!     where p0, p1, p2, p3 are real
!
      integer, parameter :: imx=200
      integer :: im
      real(r8) :: p0(imx), p1(imx), p2(imx), p3(imx)
      complex(r8) :: cx(4,imx)

      integer :: i
      real(r8) :: third, q(imx), r(imx)
      complex(r8) :: cb(imx), cb0(imx), cb1(imx),   &
                     crad(imx), cy(imx), czero


      czero=cmplx(0.0,0.0)
      third=1./3.

      do 10 i=1,im

      q(i)=-p2(i)*p2(i)/36.+(p3(i)*p1(i)-4*p0(i))/12.
      r(i)=-(p2(i)/6)**3+p2(i)*(p3(i)*p1(i)-4*p0(i))/48.   &
       +(4*p0(i)*p2(i)-p0(i)*p3(i)*p3(i)-p1(i)*p1(i))/16

      crad(i)=r(i)*r(i)+q(i)*q(i)*q(i)
      crad(i)=sqrt(crad(i))

      cb(i)=r(i)-crad(i)
      if(cb(i).eq.czero)then
!        insoluble particle
         cx(1,i)=(-p1(i))**third
         cx(2,i)=cx(1,i)
         cx(3,i)=cx(1,i)
         cx(4,i)=cx(1,i)
      else
         cb(i)=cb(i)**third

         cy(i)=-cb(i)+q(i)/cb(i)+p2(i)/6

         cb0(i)=sqrt(cy(i)*cy(i)-p0(i))
         cb1(i)=(p3(i)*cy(i)-p1(i))/(2*cb0(i))

         cb(i)=p3(i)/2+cb1(i)
         crad(i)=cb(i)*cb(i)-4*(cy(i)+cb0(i))
         crad(i)=sqrt(crad(i))
         cx(1,i)=(-cb(i)+crad(i))/2.
         cx(2,i)=(-cb(i)-crad(i))/2.

         cb(i)=p3(i)/2-cb1(i)
         crad(i)=cb(i)*cb(i)-4*(cy(i)-cb0(i))
         crad(i)=sqrt(crad(i))
         cx(3,i)=(-cb(i)+crad(i))/2.
         cx(4,i)=(-cb(i)-crad(i))/2.
      endif
   10 continue

      return
      end subroutine makoh_quartic

!----------------------------------------------------------------------

   end module modal_aero_wateruptake


