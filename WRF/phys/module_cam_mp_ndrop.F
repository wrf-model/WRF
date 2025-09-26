#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
      module ndrop
#ifdef MODAL_AERO
      use shr_kind_mod,  only: r8 => shr_kind_r8
#ifndef WRF_PORT
      use abortutils,    only: endrun
      use modal_aero_data, only: ntot_amode, qqcw_get_field
      use cam_logfile,     only: iulog
#else
      use module_cam_support, only: endrun, iulog
      use modal_aero_data, only: ntot_amode

#endif

      implicit none

      private
      save

      public activate_init, dropmixnuc

      real(r8) :: npv(ntot_amode) ! number per volume concentration
      real(r8) :: alogsig(ntot_amode) ! natl log of geometric standard dev of aerosol
      real(r8) :: exp45logsig(ntot_amode)
      real(r8) :: argfactor(ntot_amode)
      real(r8) :: f1(ntot_amode),f2(ntot_amode)  ! abdul-razzak functions of width
      real(r8) :: t0 ! reference temperature
      real(r8) :: aten
      real(r8) :: surften       ! surface tension of water w/respect to air (N/m)
      real(r8) :: alogten,alog2,alog3,alogaten
      real(r8) :: third, twothird, sixth, zero
      real(r8) :: sq2, sqpi, pi

      type qqcw_type
         real(r8), pointer :: fldcw(:,:)
      end type qqcw_type

contains

      subroutine dropmixnuc(lchnk, ncol, ncldwtr,tendnd, temp,omega,  &
                    pmid,pint,pdel,rpdel,zm,kvh,wsub,cldn,cldo,     &
                    raer, cflx, raertend, dtmicro                   &
#ifdef WRF_PORT
                    , qqcw_inout                                    &
#endif
                                                                    ) 

!     vertical diffusion and nucleation of cloud droplets
!     assume cloud presence controlled by cloud fraction
!     doesn't distinguish between warm, cold clouds
#ifndef WRF_PORT
      use ppgrid,        only: pcols, pver, pverp
#else
      use module_cam_support, only: pcols, pver, pverp
#endif
      use physconst,     only: gravit, rhoh2o, latvap, cpair, epsilo, rair, mwh2o, r_universal
#ifndef WRF_PORT
      use time_manager,  only: get_nstep
      use constituents,  only: pcnst, qmin, cnst_get_ind, cnst_name
#else
      use module_cam_support, only: pcnst => pcnst_mp
      use constituents,  only: cnst_get_ind, cnst_name 
#endif
      use error_function, only: erf

!      use aerosol_radiation_interface, only: collect_sw_aer_masses, aer_mass

#ifndef WRF_PORT
      use modal_aero_data
      use cam_history, only: fieldname_len
      use phys_grid,   only: get_lat_all_p, get_lon_all_p
      use physics_types, only: physics_state
      use cam_history,   only: outfld
#else
      use modal_aero_data,    only: numptrcw_amode => numptrcw_amode_mp, &
           nspec_amode  => nspec_amode_mp, lmassptrcw_amode => lmassptrcw_amode_mp, &
           numptr_amode => numptr_amode_mp  , lmassptr_amode   => lmassptr_amode_mp, &
           cnst_name_cw => cnst_name_cw_mp
      use module_cam_support, only: fieldname_len, outfld
#endif

      implicit none

!     input

      integer, intent(in) :: lchnk                ! chunk identifier
      integer, intent(in) :: ncol                 ! number of columns
!      type(physics_state), intent(in) :: state      ! Physics state variables
      real(r8), intent(in) :: dtmicro             ! time step for microphysics (s)
      real(r8), intent(in) :: temp(pcols,pver)    ! temperature (K)
      real(r8), intent(inout) :: ncldwtr(pcols,pver) ! droplet number concentration (#/kg)
      real(r8), intent(inout) :: tendnd(pcols,pver) ! change in droplet number concentration (#/kg/s)
      real(r8), intent(in) :: omega(pcols,pver)   ! vertical velocity (Pa/s)
      real(r8), intent(in) :: pmid(pcols,pver)    ! mid-level pressure (Pa)
      real(r8), intent(in) :: pint(pcols,pverp)   ! pressure at layer interfaces (Pa)
      real(r8), intent(in) :: pdel(pcols,pver)    ! pressure thickess of layer (Pa)
      real(r8), intent(in) :: rpdel(pcols,pver)   ! inverse of pressure thickess of layer (/Pa)
      real(r8), intent(in) :: zm(pcols,pver)      ! geopotential height of level (m)
      real(r8), intent(in) :: kvh(pcols,pverp)    ! vertical diffusivity (m2/s)
      real(r8), intent(in) :: wsub(pcols,pver)    ! subgrid vertical velocity
      real(r8), intent(in) :: cldo(pcols,pver)    ! cloud fraction on previous time step
      real(r8), intent(in) :: cldn(pcols,pver)    ! cloud fraction


!     output

      real(r8), intent(in) :: raer(pcols,pver,pcnst) ! aerosol mass, number mixing ratios
      real(r8), intent(in) :: cflx(pcols,pcnst) ! constituent flux from surface
      real(r8), intent(out) :: raertend(pcols,pver,pcnst) ! tendency of aerosol mass, number mixing ratios
#ifdef WRF_PORT
      real(r8), target, intent(inout) :: qqcw_inout(pcols,pver,pcnst) ! cloud-borne aerosol
#endif

!--------------------Local storage-------------------------------------
!
      type(qqcw_type) :: QQCW(pcnst)

      real(r8) depvel(pcols,pcnst)! deposition velocity for droplets (m/s)
      real(r8) qcld(pver) ! cloud droplet number mixing ratio (#/kg)
      real(r8) wtke(pcols,pver)     ! turbulent vertical velocity at base of layer k (m/s)
      real(r8) wtke_cen(pcols,pver) ! turbulent vertical velocity at center of layer k (m/s)
      real(r8) zn(pver) ! g/pdel (m2/g) for layer
      real(r8) zs(pver) ! inverse of distance between levels (m)
      real(r8), parameter :: zkmin=0.01_r8,zkmax=100._r8
      real(r8) w(pcols,pver)       ! vertical velocity (m/s)
      real(r8) cs(pcols,pver)      ! air density (kg/m3)
      real(r8) dz(pcols,pver)      ! geometric thickness of layers (m)
      real(r8) zero,flxconv ! convergence of flux into lowest layer

      real(r8) wdiab           ! diabatic vertical velocity
      real(r8), parameter :: wmixmin = 0.1 ! minimum turbulence vertical velocity (m/s)
!       real(r8), parameter :: wmixmin = 0.2 ! minimum turbulence vertical velocity (m/s)
!      real(r8), parameter :: wmixmin = 1.0 ! minimum turbulence vertical velocity (m/s)
      real(r8) qncld(pver)     ! droplet number nucleated on cloud boundaries
      real(r8) ekd(pver)       ! diffusivity for droplets (m2/s)
      real(r8) ekk(0:pver)       ! density*diffusivity for droplets (kg/m3 m2/s)
      real(r8) srcn(pver) ! droplet source rate (/s)
      real(r8), parameter :: sq2pi=2.5066283_r8
      real(r8) dtinv

      logical top        ! true if cloud top, false if cloud base or new cloud
      integer km1,kp1
      real(r8) wbar,wmix,wmin,wmax
      real(r8) dum,dumc
      real(r8) dact
      real(r8) fluxntot         ! (#/cm2/s)
      real(r8) fac_srflx
      real(r8) surfrate(pcnst) ! surface exchange rate (/s)
      real(r8) surfratemax      ! max surfrate for all species treated here
      real(r8) dtmin,tinv,dtt
      integer nsubmix,nsubmix_bnd
      integer i,k,m,n
      real(r8) tempr4 ! temperature
      real(r8) dtmix
      real(r8) alogarg
      integer nstep
      real(r8) pi
      integer nnew,nsav,ntemp
      real(r8) overlapp(pver),overlapm(pver) ! cloud overlap
      real(r8) ekkp(pver),ekkm(pver) ! zn*zs*density*diffusivity
      integer count_submix(100)
      save count_submix
      real(r8) kvhmax
      save kvhmax
      real(r8) nsource(pcols,pver)            ! droplet number source (#/kg/s)
      real(r8) ndropmix(pcols,pver)           ! droplet number mixing (#/kg/s)
      real(r8) ndropcol(pcols)               ! column droplet number (#/m2)

      integer lnum, lnumcw, lmass, lmasscw, lphase, &
              lsfc, lsfccw, lsig, lspec, ltype, lwater
      integer ntype(ntot_amode)

      real(r8) na(pcols),va(pcols),hy(pcols)
      real(r8) naermod(ntot_amode) ! (/m3)
      real(r8) sigmag(ntot_amode)  ! geometric standard deviation of aerosol size dist
      real(r8) hygro(ntot_amode)  ! hygroscopicity of aerosol mode
      real(r8) vaerosol(ntot_amode) ! interstit+activated aerosol volume conc (cm3/cm3)
      real(r8) raercol(pver,pcnst,2) ! aerosol mass, number mixing ratios
      real(r8) raercol_cw(pver,pcnst,2) ! cloud-borne aerosol mass, number mixing ratios
      real(r8) surfrate_cw(pcnst) ! surface exchange rate for cloud-borne aerosol (/s)
      real(r8) source(pver) !

      real(r8) fn(ntot_amode)         ! activation fraction for aerosol number
      real(r8) fm(ntot_amode)         ! activation fraction for aerosol mass

      real(r8) fluxn(ntot_amode)      ! number  activation fraction flux (cm/s)
      real(r8) fluxm(ntot_amode)      ! mass    activation fraction flux (cm/s)
      real(r8) sum,sumcw,flux,fluxcw
!     note:  activation fraction fluxes are defined as 
!     fluxn = [flux of activated aero. number into cloud (#/cm2/s)]
!           / [aero. number conc. in updraft, just below cloudbase (#/cm3)]

      real(r8) nact(pver,ntot_amode)  ! fractional aero. number  activation rate (/s)
      real(r8) mact(pver,ntot_amode)  ! fractional aero. mass    activation rate (/s)
      real(r8) sigmag_amode_cur(ntot_amode) ! sigmag for current grid cell
      save sigmag_amode_cur
      real(r8) :: qqcwtend(pcols,pver,pcnst) ! tendency of cloudborne aerosol mass, number mixing ratios
      real(r8) :: coltend(pcols)    ! column tendency
      real(r8) :: tmpa
      integer :: lc
      character(len=fieldname_len)   :: tmpname
      character(len=fieldname_len+3) :: fieldname
      real(r8) :: csbot(pver)       ! air density at bottom (interface) of layer (kg/m3)
      real(r8) :: csbot_cscen(pver) ! csbot(i)/cs(i,k)
      real(r8) :: flux_fullact(pver)     ! 100%    activation fraction flux (cm/s)
      real(r8) :: taumix_internal_pver_inv ! 1/(internal mixing time scale for k=pver) (1/s)
      real(r8) dactn,taunuc,damp
      real(r8) :: cldo_tmp, cldn_tmp
      real(r8) :: tau_cld_regenerate

      logical cldinterior

      integer ixndrop, l
      real(r8) naer_tot,naer(pcols)
      integer, parameter :: psat=6 ! number of supersaturations to calc ccn concentration
      real(r8)  :: supersat(psat)= & ! supersaturation (%) to determine ccn concentration
               (/0.02,0.05,0.1,0.2,0.5,1.0/)
      real(r8) ccn(pcols,pver,psat)        ! number conc of aerosols activated at supersat
      character(len=8), dimension(psat) :: ccn_name(psat)= &
               (/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)
      real(r8) arg
      integer phase ! phase of aerosol



    arg = 1.0_r8
    if (abs(0.8427_r8-erf(arg))/0.8427_r8>0.001_r8) then
       write (iulog,*) 'erf(1.0) = ',ERF(arg)
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif 
       call endrun('dropmixnuc: Error function error')
    endif
    arg = 0.0
    if (erf(arg) /= 0.0) then
       write (iulog,*) 'erf(0.0) = ',erf(arg)
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif 
       write (iulog,*) 'dropmixnuc: Error function error'
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif 
       call endrun('dropmixnuc: Error function error')
    endif
     zero=0._r8


       pi = 4._r8*atan(1.0_r8)
       dtinv=1./dtmicro
#ifndef WRF_PORT
       nstep = get_nstep()
#endif

!       call t_startf('load_aerosol')


       call cnst_get_ind('NUMLIQ', ixndrop)
       depvel(:,:) = 0.0_r8        ! droplet number is done in pkg_cld_sediment, aerosols in mz_aerosols_intr
!      depvel(:,ixndrop) =  0.1_r8 ! prescribed here rather than getting it from MIRAGE depvel_part

!    call t_stopf('load_aerosol')

       do m=1,ntot_amode
#ifndef WRF_PORT
          QQCW(numptrcw_amode(m))%fldcw => qqcw_get_field(numptrcw_amode(m),lchnk)
#else
          QQCW(numptrcw_amode(m))%fldcw => qqcw_inout(:,:,numptrcw_amode(m))
#endif
          do l=1,nspec_amode(m)
#ifndef WRF_PORT             
             QQCW(lmassptrcw_amode(l,m))%fldcw => qqcw_get_field(lmassptrcw_amode(l,m),lchnk)
#else
             QQCW(lmassptrcw_amode(l,m))%fldcw => qqcw_inout(:,:,lmassptrcw_amode(l,m))
#endif
          end do
       end do


!     start loop over columns
overall_main_i_loop: &
      do i=1,ncol

!        load number nucleated into qcld on cloud boundaries

! initialization for current i .........................................

!    call t_startf('calc_wtke')
         do k=1,pver-1
	    zs(k)=1._r8/(zm(i,k)-zm(i,k+1))
	 enddo
	 zs(pver)=zs(pver-1)

	 do k=1,pver
            qcld(k)=ncldwtr(i,k)
            qncld(k)=0._r8
            srcn(k)=0._r8
	    cs(i,k)=pmid(i,k)/(rair*temp(i,k)) ! air density (kg/m3)
	    dz(i,k)=1._r8/(cs(i,k)*gravit*rpdel(i,k)) ! layer thickness in m
	    w(i,k)=-1._r8*omega(i,k)/(cs(i,k)*gravit) ! large-scale vertical velocity m/s
            do m=1,ntot_amode
	       nact(k,m)=0._r8
	       mact(k,m)=0._r8
            enddo
            zn(k)=gravit*rpdel(i,k)
	    kvhmax=max(kvh(i,k),kvhmax)
	    if(k<pver)then
               ekd(k)=kvh(i,k+1)
               ekd(k)=max(ekd(k),zkmin)
               ekd(k)=min(ekd(k),zkmax)
               csbot(k)=2.0_r8*pint(i,k+1)/(rair*(temp(i,k)+temp(i,k+1)))
               csbot_cscen(k) = csbot(k)/cs(i,k)
	    else
               ekd(k)=0._r8
               csbot(k)=cs(i,k)
               csbot_cscen(k) = 1.0_r8
	    endif
!           diagnose subgrid vertical velocity from diffusivity
            if(k.eq.pver)then
               wtke(i,k)=sq2pi*depvel(i,ixndrop)
!               wtke(i,k)=sq2pi*kvh(i,k+1)
               wtke(i,k)=max(wtke(i,k),wmixmin)
            else
               wtke(i,k)=sq2pi*ekd(k)/dz(i,k)
            endif
! get sub-grid vertical velocity from diff coef.
! following morrison et al. 2005, JAS
! assume mixing length of 30 m
! rce-comment - define wtke at layer centers for new-cloud activation
!    and at layer boundaries for old-cloud activation

!++ag
!            wtke_cen(i,k)=0.5_r8*(kvh(i,k)+kvh(i,k+1))/30._r8
!            wtke(i,k)=kvh(i,k+1)/30._r8
            wtke_cen(i,k)=wsub(i,k)
            wtke(i,k)=wsub(i,k)
!--ag
            wtke_cen(i,k)=max(wtke_cen(i,k),wmixmin)
            wtke(i,k)=max(wtke(i,k),wmixmin)
            nsource(i,k)=0._r8
         enddo

            surfratemax = 0.0_r8
	    nsav=1
	    nnew=2
            surfrate(ixndrop)=depvel(i,ixndrop)/dz(i,pver)
            surfratemax = max( surfratemax, surfrate(ixndrop) )
            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)
	       if(lnum>0)then
                  surfrate(lnum)=depvel(i,lnum)/dz(i,pver)
!                 surfrate(lnumcw)=surfrate(ixndrop)
                  surfrate_cw(lnumcw)=surfrate(ixndrop)
                  surfratemax = max( surfratemax, surfrate(lnum) )
!                 raercol(:,lnumcw,nsav)=raer(i,:,lnumcw)
                  raercol_cw(:,lnumcw,nsav)=qqcw(lnumcw)%fldcw(i,:)   ! use cloud-borne aerosol array
		  raercol(:,lnum,nsav)=raer(i,:,lnum)
	       endif
               do l=1,nspec_amode(m)
                  lmass=lmassptr_amode(l,m)
                  lmasscw=lmassptrcw_amode(l,m)
                  surfrate(lmass)=depvel(i,lmass)/dz(i,pver)
!                 surfrate(lmasscw)=surfrate(ixndrop)
                  surfrate_cw(lmasscw)=surfrate(ixndrop)
                  surfratemax = max( surfratemax, surfrate(lmass) )
!                 raercol(:,lmasscw,nsav)=raer(i,:,lmasscw)
                  raercol_cw(:,lmasscw,nsav)=qqcw(lmasscw)%fldcw(i,:)  ! use cloud-borne aerosol array
		  raercol(:,lmass,nsav)=raer(i,:,lmass)
               enddo
!              lwater=lwaterptr_amode(m)
!              surfrate(lwater)=depvel(i,lwater)/dz(i,pver)
!              surfratemax = max( surfratemax, surfrate(lwater) )
!              raercol(:,lwater,nsav)=raer(i,:,lwater)
            enddo
!    call t_stopf('calc_wtke')

!        droplet nucleation/aerosol activation

! tau_cld_regenerate = time scale for regeneration of cloudy air 
!    by (horizontal) exchange with clear air
            tau_cld_regenerate = 3600.0_r8 * 3.0_r8 

! k-loop for growing/shrinking cloud calcs .............................
grow_shrink_main_k_loop: &
         do k=1,pver
            km1=max0(k-1,1)
            kp1=min0(k+1,pver)

! shrinking cloud ......................................................
!    treat the reduction of cloud fraction from when cldn(i,k) < cldo(i,k)
!    and also dissipate the portion of the cloud that will be regenerated
              cldo_tmp = cldo(i,k)
              cldn_tmp = cldn(i,k) * exp( -dtmicro/tau_cld_regenerate )
!    alternate formulation
!             cldn_tmp = cldn(i,k) * max( 0.0_r8, (1.0_r8-dtmicro/tau_cld_regenerate) )

              if(cldn_tmp.lt.cldo_tmp)then
!                droplet loss in decaying cloud
!++ sungsup
		 nsource(i,k)=nsource(i,k)+qcld(k)*(cldn_tmp-cldo_tmp)/cldo_tmp*dtinv
                 qcld(k)=qcld(k)*(1.+(cldn_tmp-cldo_tmp)/cldo_tmp)
!-- sungsup

!                 convert activated aerosol to interstitial in decaying cloud

                  dumc=(cldn_tmp-cldo_tmp)/cldo_tmp
                  do m=1,ntot_amode
		     lnum=numptr_amode(m)
		     lnumcw=numptrcw_amode(m)
		     if(lnum.gt.0)then
		        dact=raercol_cw(k,lnumcw,nsav)*dumc
!                       raercol(k,lnumcw,nsav)=raercol(k,lnumcw,nsav)+dact
                        raercol_cw(k,lnumcw,nsav)=raercol_cw(k,lnumcw,nsav)+dact   ! cloud-borne aerosol
		        raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
		     endif
		     do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)
			dact=raercol_cw(k,lmasscw,nsav)*dumc
!                       raercol(k,lmasscw,nsav)=raercol(k,lmasscw,nsav)+dact
                        raercol_cw(k,lmasscw,nsav)=raercol_cw(k,lmasscw,nsav)+dact  ! cloud-borne aerosol
			raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                     enddo
                  enddo
               endif

! growing cloud ......................................................
!    treat the increase of cloud fraction from when cldn(i,k) > cldo(i,k)
!    and also regenerate part of the cloud 
              cldo_tmp = cldn_tmp
              cldn_tmp = cldn(i,k)

              if(cldn_tmp-cldo_tmp.gt.0.01)then
!                wmix=wtke(i,k)
!                wbar=wtke(i,k)
! rce-comment - use wtke at layer centers for new-cloud activation
                 wbar=wtke_cen(i,k)
                 wmix=0._r8
                 wmin=0._r8
                 wmax=10._r8
                 wdiab=0
!                load aerosol properties, assuming external mixtures

                phase=1 ! interstitial
                 do m=1,ntot_amode
                    call loadaer(raer,qqcw,i,i,k,m,cs,npv(m),phase, &
                         na, va,  hy )
			 naermod(m)=na(i)
			 vaerosol(m)=va(i)
			 hygro(m)=hy(i)
                 end do
!		 m=1
!		 naermod(m)=1000.e6
!		 vaerosol(m)=naermod(m)/(density*num_to_mass_aer(m))

!                call t_startf ('activate')

                 call activate_modal(wbar,wmix,wdiab,wmin,wmax,temp(i,k),cs(i,k), &
                      naermod, ntot_amode,ntot_amode, vaerosol, sigmag,hygro,       &
                      fn,fm,fluxn,fluxm,flux_fullact(k))

!                call t_stopf ('activate')
!		 write(iulog,'(a,15f8.2)')'wtke,naer,fn=',wtke(i,k),naer_tot*1.e-6,(fn(m),m=1,ntot_amode)


                 dumc=(cldn_tmp-cldo_tmp)
                 do m=1,ntot_amode
                    lnum=numptr_amode(m)
                    lnumcw=numptrcw_amode(m)
		    dact=dumc*fn(m)*raer(i,k,lnum) ! interstitial only
                    qcld(k)=qcld(k)+dact
		    nsource(i,k)=nsource(i,k)+dact*dtinv
		    if(lnum.gt.0)then
                       raercol_cw(k,lnumcw,nsav)=raercol_cw(k,lnumcw,nsav)+dact  ! cloud-borne aerosol
		       raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
		    endif
                    dum=dumc*fm(m)
                    do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)
			dact=dum*(raer(i,k,lmass)) ! interstitial only
                        raercol_cw(k,lmasscw,nsav)=raercol_cw(k,lmasscw,nsav)+dact  ! cloud-borne aerosol
			raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                    enddo
                 enddo
               endif

         enddo grow_shrink_main_k_loop
! end of k-loop for growing/shrinking cloud calcs ......................


! ......................................................................
! start of k-loop for calc of old cloud activation tendencies ..........
!
! rce-comment
!    changed this part of code to use current cloud fraction (cldn) exclusively
!    consider case of cldo(:)=0, cldn(k)=1, cldn(k+1)=0
!    previous code (which used cldo below here) would have no cloud-base activation
!       into layer k.  however, activated particles in k mix out to k+1,
!       so they are incorrectly depleted with no replacement
!
old_cloud_main_k_loop: &
         do k=1,pver
            km1=max0(k-1,1)
            kp1=min0(k+1,pver)
            taumix_internal_pver_inv = 0.0

            if(cldn(i,k).gt.0.01)then
! rce-comment
!             if(cldo(i,k).gt.0.01)then   ! with cldo changed to cldn this is redundant
               wdiab=0
!               wmix=wtke(i,k) ! spectrum of updrafts
!	       wbar=w(i,k) ! spectrum of updrafts
               wmix=0._r8 ! single updraft
	       wbar=wtke(i,k) ! single updraft
! rce-comment - different treatment for k=pver
	       if (k == pver) wbar=wtke_cen(i,k) ! single updraft
               wmax=10._r8
	       wmin=0._r8
!              old cloud
                  if(cldn(i,k)-cldn(i,kp1).gt.0.01.or.k.eq.pver)then

!                   cloud base
                    
                    ekd(k)=wtke(i,k)*dz(i,k)/sq2pi
! rce-comments
!   first, should probably have 1/zs(k) here rather than dz(i,k) because
!      the turbulent flux is proportional to ekd(k)*zs(k),
!      while the dz(i,k) is used to get flux divergences
!      and mixing ratio tendency/change
!   second and more importantly, using a single updraft velocity here
!      means having monodisperse turbulent updraft and downdrafts.
!      The sq2pi factor assumes a normal draft spectrum.
!      The fluxn/fluxm from activate must be consistent with the
!      fluxes calculated in explmix.
                    ekd(k)=wbar/zs(k)

                    alogarg=max(1.e-20_r8,1/cldn(i,k)-1._r8)
                    wmin=wbar+wmix*0.25*sq2pi*log(alogarg)
                    phase=1 ! interstitial
                    do m=1,ntot_amode
! rce-comment - use kp1 here as old-cloud activation involves 
!   aerosol from layer below
!                      call loadaer(raer,qqcw,i,i,k,m,cs, npv(m),phase, &
                       call loadaer(raer,qqcw,i,i,kp1,m,cs, npv(m),phase, &
                         na, va,  hy )
			 naermod(m)=na(i)
			 vaerosol(m)=va(i)
			 hygro(m)=hy(i)
                    end do
!		    m=1
!		    naermod(m)=1000.e6
!		    vaerosol(1,m)=naermod(m)/(density*num_to_mass_aer(m))
!                   call t_startf ('activate')

                    call activate_modal(wbar,wmix,wdiab,wmin,wmax,temp(i,k),cs(i,k), &
                      naermod, ntot_amode,ntot_amode, vaerosol, sigmag, hygro,    &
                      fn,fm,fluxn,fluxm,flux_fullact(k))

!                   call t_stopf ('activate')
!		    write(iulog,'(a,15f8.2)')'wtke,naer,fn=',wtke(i,k),naer_tot*1.e-6,(fn(m),m=1,ntot_amode)

                      if(k.lt.pver)then
                          dumc = cldn(i,k)-cldn(i,kp1)
                      else
                          dumc=cldn(i,k)
                      endif
                    fluxntot=0
! rce-comment 1
!    flux of activated mass into layer k (in kg/m2/s)
!       = "actmassflux" = dumc*fluxm*raercol(kp1,lmass)*csbot(k)
!    source of activated mass (in kg/kg/s) = flux divergence
!       = actmassflux/(cs(i,k)*dz(i,k))
!    so need factor of csbot_cscen = csbot(k)/cs(i,k)
!                   dum=1./(dz(i,k))
                    dum=csbot_cscen(k)/(dz(i,k))
! rce-comment 2
!    code for k=pver was changed to use the following conceptual model
!    in k=pver, there can be no cloud-base activation unless one considers
!       a scenario such as the layer being partially cloudy, 
!       with clear air at bottom and cloudy air at top
!    assume this scenario, and that the clear/cloudy portions mix with 
!       a timescale taumix_internal = dz(i,pver)/wtke_cen(i,pver)
!    in the absence of other sources/sinks, qact (the activated particle 
!       mixratio) attains a steady state value given by
!          qact_ss = fcloud*fact*qtot
!       where fcloud is cloud fraction, fact is activation fraction, 
!       qtot=qact+qint, qint is interstitial particle mixratio
!    the activation rate (from mixing within the layer) can now be
!       written as
!          d(qact)/dt = (qact_ss - qact)/taumix_internal
!                     = qtot*(fcloud*fact*wtke/dz) - qact*(wtke/dz)
!    note that (fcloud*fact*wtke/dz) is equal to the nact/mact
!    also, d(qact)/dt can be negative.  in the code below
!       it is forced to be >= 0
!
! steve -- 
!    you will likely want to change this.  i did not really understand 
!       what was previously being done in k=pver
!    in the cam3_5_3 code, wtke(i,pver) appears to be equal to the
!       droplet deposition velocity which is quite small
!    in the cam3_5_37 version, wtke is done differently and is much
!       larger in k=pver, so the activation is stronger there
!
                    if (k == pver) then
                       taumix_internal_pver_inv = flux_fullact(k)/dz(i,k)
                    end if
                    do m=1,ntot_amode
                       fluxn(m)=fluxn(m)*dumc
                       fluxm(m)=fluxm(m)*dumc
                       lnum=numptr_amode(m)
                       nact(k,m)=nact(k,m)+fluxn(m)*dum
                       mact(k,m)=mact(k,m)+fluxm(m)*dum
                       if (k > pver) then
! note that kp1 is used here
                          fluxntot = fluxntot &
                                   +fluxn(m)*raercol(kp1,lnum,nsav)*cs(i,k)
                       else
                          lnumcw=numptrcw_amode(m)
                          tmpa = raercol(kp1,lnum,nsav)*fluxn(m) &
                               + raercol(kp1,lnumcw,nsav)*(fluxn(m) &
                                    -taumix_internal_pver_inv)
                          fluxntot = fluxntot + max(0.0_r8,tmpa)*cs(i,k)
                       end if
                    enddo
                      srcn(k)=srcn(k)+fluxntot/(cs(i,k)*dz(i,k))
		      nsource(i,k)=nsource(i,k)+fluxntot/(cs(i,k)*dz(i,k))

                 endif  ! (cldn(i,k)-cldn(i,kp1).gt.0.01 .or. k.eq.pver)

! rce-comment
!             endif ! (cldo(i,k).gt.0.01)   ! with cldo changed to cldn this is redundant

            else
!              no cloud
               nsource(i,k)=nsource(i,k)-qcld(k)*dtinv
               qcld(k)=0
!              convert activated aerosol to interstitial in decaying cloud
               do m=1,ntot_amode
	          lnum=numptr_amode(m)
		  lnumcw=numptrcw_amode(m)
		  if(lnum.gt.0)then
                     raercol(k,lnum,nsav)=raercol(k,lnum,nsav)+raercol_cw(k,lnumcw,nsav)  ! cloud-borne aerosol
                     raercol_cw(k,lnumcw,nsav)=0.
		  endif
		  do l=1,nspec_amode(m)
		     lmass=lmassptr_amode(l,m)
		     lmasscw=lmassptrcw_amode(l,m)
                     raercol(k,lmass,nsav)=raercol(k,lmass,nsav)+raercol_cw(k,lmasscw,nsav) ! cloud-borne aerosol
                     raercol_cw(k,lmasscw,nsav)=0.
                  enddo
                enddo
            endif
         enddo old_cloud_main_k_loop
!           switch nsav, nnew so that nnew is the updated aerosol
 	    ntemp=nsav
 	    nsav=nnew
 	    nnew=ntemp
!          call t_startf ('nsubmix')

!        load new droplets in layers above, below clouds

         dtmin=dtmicro
         ekk(0)=0.0
         ekk(pver)=0.0
         do k=1,pver-1
! rce-comment -- ekd(k) is eddy-diffusivity at k/k+1 interface
!   want ekk(k) = ekd(k) * (density at k/k+1 interface)
!   so use pint(i,k+1) as pint is 1:pverp 
!           ekk(k)=ekd(k)*2.*pint(i,k)/(rair*(temp(i,k)+temp(i,k+1)))
!           ekk(k)=ekd(k)*2.*pint(i,k+1)/(rair*(temp(i,k)+temp(i,k+1)))
            ekk(k)=ekd(k)*csbot(k)
         enddo
         do k=1,pver
            km1=max0(k-1,1)
            ekkp(k)=zn(k)*ekk(k)*zs(k)
            ekkm(k)=zn(k)*ekk(k-1)*zs(km1)
            tinv=ekkp(k)+ekkm(k)

            if(k.eq.pver)tinv=tinv+surfratemax
! rce-comment -- tinv is the sum of all first-order-loss-rates
!    for the layer.  for most layers, the activation loss rate
!    (for interstitial particles) is accounted for by the loss by
!    turb-transfer to the layer above.
!    k=pver is special, and the loss rate for activation within 
!    the layer must be added to tinv.  if not, the time step
!    can be too big, and explmix can produce negative values.
!    the negative values are reset to zero, resulting in an 
!    artificial source.
            if(k.eq.pver)tinv=tinv+taumix_internal_pver_inv

            if(tinv.gt.1.e-6)then
               dtt=1./tinv
               dtmin=min(dtmin,dtt)
            endif
         enddo
         dtmix=0.9*dtmin
         nsubmix=dtmicro/dtmix+1
	 if(nsubmix>100)then
	    nsubmix_bnd=100
	 else
	    nsubmix_bnd=nsubmix
	 endif
	 count_submix(nsubmix_bnd)=count_submix(nsubmix_bnd)+1
         dtmix=dtmicro/nsubmix
         fac_srflx = -1.0/(zn(pver)*nsubmix)

	 do k=1,pver
            kp1=min(k+1,pver)
            km1=max(k-1,1)
	    ! maximum overlap assumption
            if(cldn(i,kp1).gt.1.e-10_r8)then
               overlapp(k)=min(cldn(i,k)/cldn(i,kp1),1._r8)
            else
               overlapp(k)=1.
            endif
            if(cldn(i,km1).gt.1.e-10_r8)then
               overlapm(k)=min(cldn(i,k)/cldn(i,km1),1._r8)
            else
               overlapm(k)=1.
            endif
	 enddo


! rce-comment
!    the activation source(k) = mact(k,m)*raercol(kp1,lmass)
!       should not exceed the rate of transfer of unactivated particles
!       from kp1 to k which = ekkp(k)*raercol(kp1,lmass)
!    however it might if things are not "just right" in subr activate
!    the following is a safety measure to avoid negatives in explmix
         do k = 1, pver-1
         do m = 1, ntot_amode
            nact(k,m) = min( nact(k,m), ekkp(k) )
            mact(k,m) = min( mact(k,m), ekkp(k) )
         end do
         end do


old_cloud_nsubmix_loop: &
         do n=1,nsubmix
            qncld(:)=qcld(:)
!           switch nsav, nnew so that nsav is the updated aerosol
	    ntemp=nsav
	    nsav=nnew
	    nnew=ntemp
            srcn(:)=0.0
            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)
!              update droplet source
! rce-comment- activation source in layer k involves particles from k+1
!	       srcn(:)=srcn(:)+nact(:,m)*(raercol(:,lnum,nsav))
               srcn(1:pver-1)=srcn(1:pver-1)+nact(1:pver-1,m)*(raercol(2:pver,lnum,nsav))
! rce-comment- new formulation for k=pver
!              srcn(  pver  )=srcn(  pver  )+nact(  pver  ,m)*(raercol(  pver,lnum,nsav))
               tmpa = raercol(pver,lnum,nsav)*nact(pver,m) &
                    + raercol(pver,lnumcw,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
               srcn(pver)=srcn(pver) + max(0.0_r8,tmpa)
            enddo
            call explmix(qcld,srcn,ekkp,ekkm,overlapp,overlapm,   &
                         qncld,surfrate(ixndrop),zero,pver,dtmix,.false.)

! rce-comment
!    the interstitial particle mixratio is different in clear/cloudy portions
!    of a layer, and generally higher in the clear portion.  (we have/had
!    a method for diagnosing the the clear/cloudy mixratios.)  the activation
!    source terms involve clear air (from below) moving into cloudy air (above).
!    in theory, the clear-portion mixratio should be used when calculating 
!    source terms
            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)
               if(lnum>0)then
! rce-comment -   activation source in layer k involves particles from k+1
!	          source(:)= nact(:,m)*(raercol(:,lnum,nsav))
                  source(1:pver-1)= nact(1:pver-1,m)*(raercol(2:pver,lnum,nsav))
! rce-comment- new formulation for k=pver
!                 source(  pver  )= nact(  pver,  m)*(raercol(  pver,lnum,nsav))
                  tmpa = raercol(pver,lnum,nsav)*nact(pver,m) &
                       + raercol(pver,lnumcw,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
                  source(pver) = max(0.0_r8,tmpa)
!                  flxconv=cflx(i,lnum)*zn(pver)
                  flxconv=0.
                  call explmix(raercol_cw(1,lnumcw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                               raercol_cw(1,lnumcw,nsav),surfrate_cw(lnumcw),zero,pver,dtmix,&
                               .false.)
                  call explmix(raercol(1,lnum,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                               raercol(1,lnum,nsav),surfrate(lnum),flxconv,pver,dtmix, &
                               .true.,raercol_cw(1,lnumcw,nsav))
               endif

               do l=1,nspec_amode(m)
                  lmass=lmassptr_amode(l,m)
                  lmasscw=lmassptrcw_amode(l,m)
! rce-comment -   activation source in layer k involves particles from k+1
!	          source(:)= mact(:,m)*(raercol(:,lmass,nsav))
                  source(1:pver-1)= mact(1:pver-1,m)*(raercol(2:pver,lmass,nsav))
! rce-comment- new formulation for k=pver
!                 source(  pver  )= mact(  pver  ,m)*(raercol(  pver,lmass,nsav))
                  tmpa = raercol(pver,lmass,nsav)*mact(pver,m) &
                       + raercol(pver,lmasscw,nsav)*(mact(pver,m) - taumix_internal_pver_inv)
                  source(pver) = max(0.0_r8,tmpa)
!                  flxconv=cflx(i,lmass)*zn(pver)
		  flxconv=0.

                  call explmix(raercol_cw(1,lmasscw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                               raercol_cw(1,lmasscw,nsav),surfrate_cw(lmasscw),zero,pver,dtmix,&
                               .false.)
                  call explmix(raercol(1,lmass,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                               raercol(1,lmass,nsav),surfrate(lmass),flxconv, pver,dtmix,&
                               .true.,raercol_cw(1,lmasscw,nsav))
	       enddo   ! l
!              lwater=lwaterptr_amode(m)  ! aerosol water
!              source(:)=0.
!              call explmix(   raercol(1,lwater,nnew),source,ekkp,ekkm,overlapp,overlapm,   &
!                              raercol(1,lwater,nsav),surfrate(lwater),zero,pver,dtmix,&
!                              .true.,source)
            enddo   ! m

         enddo old_cloud_nsubmix_loop
!          call t_stopf ('nsubmix')

!        evaporate particles again if no cloud

         do k=1,pver
	    if(cldn(i,k).eq.0.)then
!              no cloud
               qcld(k)=0.
!              convert activated aerosol to interstitial in decaying cloud
               do m=1,ntot_amode
	          lnum=numptr_amode(m)
		  lnumcw=numptrcw_amode(m)
		  if(lnum.gt.0)then
                        raercol(k,lnum,nnew)=raercol(k,lnum,nnew)+raercol_cw(k,lnumcw,nnew)
                        raercol_cw(k,lnumcw,nnew)=0.
		  endif
!
		  do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)
!                       raercol(k,lmass,nnew)=raercol(k,lmass,nnew)+raercol(k,lmasscw,nnew)
!                       raercol(k,lmasscw,nnew)=0.
                        raercol(k,lmass,nnew)=raercol(k,lmass,nnew)+raercol_cw(k,lmasscw,nnew)
                        raercol_cw(k,lmasscw,nnew)=0.
                  enddo
                enddo
            endif
         enddo

!        droplet number

         ndropcol(i)=0.
         do k=1,pver
	    ndropmix(i,k)=(qcld(k)-ncldwtr(i,k))*dtinv - nsource(i,k)
	    tendnd(i,k) = (max(qcld(k),1.e-6_r8)-ncldwtr(i,k))*dtinv
	    ndropcol(i) = ndropcol(i) + ncldwtr(i,k)*pdel(i,k)
	 enddo
	 ndropcol(i) = ndropcol(i)/gravit


!        aerosol tendency
         do m=1,ntot_amode
            lnum=numptr_amode(m)
            lnumcw=numptrcw_amode(m)
	    if(lnum.gt.0)then
               qqcwtend(i,:,lnumcw)=(raercol_cw(:,lnumcw,nnew)-qqcw(lnumcw)%fldcw(i,:))*dtinv
               qqcw(lnumcw)%fldcw(i,:)=raercol_cw(:,lnumcw,nnew)    ! update cloud-borne aerosol
	       raertend(i,:,lnum)= (raercol(:,lnum,nnew)-raer(i,:,lnum))*dtinv
	    endif
	    do l=1,nspec_amode(m)
	       lmass=lmassptr_amode(l,m)
	       lmasscw=lmassptrcw_amode(l,m)
               qqcwtend(i,:,lmasscw)=(raercol_cw(:,lmasscw,nnew)-qqcw(lmasscw)%fldcw(i,:))*dtinv
               qqcw(lmasscw)%fldcw(i,:)=raercol_cw(:,lmasscw,nnew)   ! update cloud-borne aerosol
	       raertend(i,:,lmass)=(raercol(:,lmass,nnew)-raer(i,:,lmass))*dtinv
            enddo
!           lwater=lwaterptr_amode(m)
!           raertend(i,:,lwater)=(raercol(:,lwater,nnew)-raer(i,:,lwater))*dtinv
         enddo

      enddo overall_main_i_loop
! end of main loop over i/longitude ....................................

      call outfld('NDROPCOL', ndropcol  , pcols, lchnk   )
      call outfld('NDROPSRC', nsource    , pcols, lchnk   )
      call outfld('NDROPMIX', ndropmix    , pcols, lchnk   )
      call outfld('LCLOUD  ', cldn    , pcols, lchnk   )
      call outfld('WTKE    ', wtke    , pcols, lchnk   )

      call ccncalc(lchnk,ncol,temp,cs,raer,qqcw,ccn,psat,supersat,alogsig,npv)
      do l=1,psat
         call outfld(ccn_name(l), ccn(1,1,l)    , pcols, lchnk   )
      enddo

! do column tendencies
      do m = 1, ntot_amode
      do lphase = 1, 2
      do lspec = 0, nspec_amode(m)+1   ! loop over number + chem constituents + water
         if (lspec == 0) then   ! number
            if (lphase == 1) then
               l = numptr_amode(m)
            else
               l = numptrcw_amode(m)
            endif
         else if (lspec <= nspec_amode(m)) then   ! non-water mass
            if (lphase == 1) then
               l = lmassptr_amode(lspec,m)
            else
               l = lmassptrcw_amode(lspec,m)
            endif
         else   ! water mass
!           if (lphase == 1) then
!              l = lwaterptr_amode(m)
!           else
               cycle
!           end if
         end if
         if (l <= 0) cycle

! mixnuc1 tendency for interstitial = (total tendency) - cflx
! mixnuc1 tendency for activated    = (total tendency)
         coltend(:) = 0.0
         if (lphase == 1) then
            tmpname = cnst_name(l)
            do i = 1, ncol
               coltend(i) = sum( pdel(i,:)*raertend(i,:,l) )/gravit
!               coltend(i) = coltend(i) - cflx(i,l)
            end do
         else
            tmpname = cnst_name_cw(l)
            do i = 1, ncol
               coltend(i) = sum( pdel(i,:)*qqcwtend(i,:,l) )/gravit
            end do
         end if
         fieldname = trim(tmpname) // '_mixnuc1'
         call outfld( fieldname, coltend, pcols, lchnk)

      end do   ! lspec
      end do   ! lphase
      end do   ! m

      return
      end subroutine dropmixnuc

   subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
                       qold, surfrate, flxconv, pver, dt, is_unact, qactold )

!  explicit integration of droplet/aerosol mixing
!     with source due to activation/nucleation
      

   implicit none
   integer, intent(in) :: pver ! number of levels
   real(r8), intent(out) :: q(pver) ! mixing ratio to be updated
   real(r8), intent(in) :: qold(pver) ! mixing ratio from previous time step
   real(r8), intent(in) :: src(pver) ! source due to activation/nucleation (/s)
   real(r8), intent(in) :: ekkp(pver) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! below layer k  (k,k+1 interface)
   real(r8), intent(in) :: ekkm(pver) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! above layer k  (k,k+1 interface)
   real(r8), intent(in) :: overlapp(pver) ! cloud overlap below
   real(r8), intent(in) :: overlapm(pver) ! cloud overlap above
   real(r8), intent(in) :: surfrate ! surface exchange rate (/s)
   real(r8), intent(in) :: flxconv ! convergence of flux from surface
   real(r8), intent(in) :: dt ! time step (s)
   logical, intent(in) :: is_unact ! true if this is an unactivated species
   real(r8), intent(in),optional :: qactold(pver)
          ! mixing ratio of ACTIVATED species from previous step
          ! *** this should only be present
          !     if the current species is unactivated number/sfc/mass

   integer k,kp1,km1

   if ( is_unact ) then
!     the qactold*(1-overlap) terms are resuspension of activated material
      do k=1,pver
         kp1=min(k+1,pver)
         km1=max(k-1,1)
         q(k) = qold(k) + dt*( - src(k) + ekkp(k)*(qold(kp1) - qold(k) +       &
                           qactold(kp1)*(1.0-overlapp(k)))               &
                                  + ekkm(k)*(qold(km1) - qold(k) +     &
                           qactold(km1)*(1.0-overlapm(k))) )
!        force to non-negative
!        if(q(k)<-1.e-30)then
!           write(iulog,*)'q=',q(k),' in explmix'
            q(k)=max(q(k),0._r8)
!        endif
      end do
!     diffusion loss at base of lowest layer
      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt
!        force to non-negative
!        if(q(pver)<-1.e-30)then
!           write(iulog,*)'q=',q(pver),' in explmix'
            q(pver)=max(q(pver),0._r8)
!        endif
   else
      do k=1,pver
         kp1=min(k+1,pver)
         km1=max(k-1,1)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +      &
                                    ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )
!        force to non-negative
!        if(q(k)<-1.e-30)then
!           write(iulog,*)'q=',q(k),' in explmix'
            q(k)=max(q(k),0._r8)
!        endif
      end do
!     diffusion loss at base of lowest layer
      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt
!        force to non-negative
!        if(q(pver)<-1.e-30)then
!           write(iulog,*)'q=',q(pver),' in explmix'
            q(pver)=max(q(pver),0._r8)
!        endif
   end if

   return
   end subroutine explmix

   subroutine activate_init
#ifndef WRF_PORT
      use ppgrid,  only: pver
#else
      use module_cam_support, only: pver
#endif
#ifndef WRF_PORT
      use modal_aero_data
      use cam_history,   only: addfld, add_default, phys_decomp
#else
      use modal_aero_data, only:sigmag_amode => sigmag_amode_mp, &
           numptr_amode   => numptr_amode_mp, nspec_amode => nspec_amode_mp, &
           lmassptr_amode => lmassptr_amode_mp, dgnum_amode => dgnum_amode_mp
      use module_cam_support, only: addfld, add_default, phys_decomp
#endif
      use physconst, only: rhoh2o, mwh2o, r_universal
      use error_function, only: erf

      implicit none

      integer l,m
      real(r8) arg
      integer lnum, lmass

      character*16 ccn_longname

!      mathematical constants

      zero=0._r8
      third=1./3._r8
      twothird=2.*third
      sixth=1./6._r8
      sq2=sqrt(2._r8)
      pi=4._r8*atan(1.0_r8)
      sqpi=sqrt(pi)

      t0=273.
      surften=0.076_r8
      aten=2.*mwh2o*surften/(r_universal*t0*rhoh2o)
      alogaten=log(aten)
      alog2=log(2._r8)
      alog3=log(3._r8)

      do m=1,ntot_amode
!         use only if width of size distribution is prescribed
          alogsig(m)=log(sigmag_amode(m))
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
	  argfactor(m)=2./(3.*sqrt(2.)*alogsig(m))
          f1(m)=0.5*exp(2.5*alogsig(m)*alogsig(m))
	  f2(m)=1.+0.25*alogsig(m)
          lnum=numptr_amode(m)
          do l=1,nspec_amode(m)
               lmass=lmassptr_amode(l,m)
               if(lmass.le.0)then
                  write(iulog,*)'lmassptr_amode(',l,m,')=',lmass
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif 
                  call endrun
               endif
          enddo
          npv(m)=6./(pi*dgnum_amode(m)**3*exp45logsig(m))
      enddo
      
      call addfld ('WTKE     ', 'm/s     ', pver, 'A', 'Standard deviation of updraft velocity'                  ,phys_decomp)
      call addfld ('LCLOUD   ', '        ', pver, 'A', 'Liquid cloud fraction'                                   ,phys_decomp)
      call addfld ('NDROPMIX ', '#/kg/s  ', pver, 'A', 'Droplet number mixing'                     ,phys_decomp)
      call addfld ('NDROPSRC ', '#/kg/s  ', pver, 'A', 'Droplet number source'                     ,phys_decomp)
      call addfld ('NDROPSNK ', '#/kg/s  ', pver, 'A', 'Droplet number loss by microphysics'       ,phys_decomp)
      call addfld ('NDROPCOL ', '#/m2    ', 1,    'A', 'Column droplet number'                     ,phys_decomp)
      call add_default ('WTKE    ', 1, ' ')
      call add_default ('LCLOUD  ', 1, ' ')

      return
      end subroutine activate_init

      subroutine activate_modal(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
                          na, pmode, nmode, volume, sigman, hygro, &
                          fn, fm, fluxn, fluxm, flux_fullact )

!      calculates number, surface, and mass fraction of aerosols activated as CCN
!      calculates flux of cloud droplets, surface area, and aerosol mass into cloud
!      assumes an internal mixture within each of up to pmode multiple aerosol modes
!      a gaussiam spectrum of updrafts can be treated.

!      mks units

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      use physconst, only: rair, epsilo, cpair, rh2o, latvap, gravit,   &
                                 rhoh2o, mwh2o, r_universal
      use wv_saturation, only: estblf, epsqs
      use error_function, only: erf

      implicit none


!      input

      integer pmode ! dimension of modes
      real(r8) wbar          ! grid cell mean vertical velocity (m/s)
      real(r8) sigw          ! subgrid standard deviation of vertical vel (m/s)
      real(r8) wdiab         ! diabatic vertical velocity (0 if adiabatic)
      real(r8) wminf         ! minimum updraft velocity for integration (m/s)
      real(r8) wmaxf         ! maximum updraft velocity for integration (m/s)
      real(r8) tair          ! air temperature (K)
      real(r8) rhoair        ! air density (kg/m3)
      real(r8) na(pmode)           ! aerosol number concentration (/m3)
      integer nmode      ! number of aerosol modes
      real(r8) volume(pmode)     ! aerosol volume concentration (m3/m3)
      real(r8) sigman(pmode)  ! geometric standard deviation of aerosol size distribution
      real(r8) hygro(pmode)  ! hygroscopicity of aerosol mode

!      output

      real(r8) fn(pmode)      ! number fraction of aerosols activated
      real(r8) fm(pmode)      ! mass fraction of aerosols activated
      real(r8) fluxn(pmode)   ! flux of activated aerosol number fraction into cloud (cm/s)
      real(r8) fluxm(pmode)   ! flux of activated aerosol mass fraction into cloud (cm/s)
      real(r8) flux_fullact   ! flux of activated aerosol fraction assuming 100% activation (cm/s)
                              !    rce-comment
                              !    used for consistency check -- this should match (ekd(k)*zs(k))
                              !    also, fluxm/flux_fullact gives fraction of aerosol mass flux
                              !       that is activated

!      local

      integer, parameter:: nx=200
      integer iquasisect_option, isectional
      real(r8) integ,integf
      real(r8), parameter :: p0 = 1013.25e2_r8    ! reference pressure (Pa)
      real(r8) xmin(ntot_amode),xmax(ntot_amode) ! ln(r) at section interfaces
      real(r8) volmin(ntot_amode),volmax(ntot_amode) ! volume at interfaces
      real(r8) tmass ! total aerosol mass concentration (g/cm3)
      real(r8) sign(ntot_amode)    ! geometric standard deviation of size distribution
      real(r8) rm ! number mode radius of aerosol at max supersat (cm)
      real(r8) pres ! pressure (Pa)
      real(r8) path ! mean free path (m)
      real(r8) diff ! diffusivity (m2/s)
      real(r8) conduct ! thermal conductivity (Joule/m/sec/deg)
      real(r8) diff0,conduct0
      real(r8) es ! saturation vapor pressure
      real(r8) qs ! water vapor saturation mixing ratio
      real(r8) dqsdt ! change in qs with temperature
      real(r8) dqsdp ! change in qs with pressure
      real(r8) g ! thermodynamic function (m2/s)
      real(r8) zeta(ntot_amode), eta(ntot_amode)
      real(r8) lnsmax ! ln(smax)
      real(r8) alpha
      real(r8) gamma
      real(r8) beta
      real(r8) sqrtg(ntot_amode)
      real(r8) :: amcube(ntot_amode) ! cube of dry mode radius (m)
      real(r8) :: smcrit(ntot_amode) ! critical supersatuation for activation
      real(r8) :: lnsm(ntot_amode) ! ln(smcrit)
      real(r8) smc(ntot_amode) ! critical supersaturation for number mode radius
      real(r8) sumflx_fullact
      real(r8) sumflxn(ntot_amode)
      real(r8) sumflxm(ntot_amode)
      real(r8) sumfn(ntot_amode)
      real(r8) sumfm(ntot_amode)
      real(r8) fnold(ntot_amode)   ! number fraction activated
      real(r8) fmold(ntot_amode)   ! mass fraction activated
      real(r8) wold,gold
      real(r8) alogam
      real(r8) rlo,rhi,xint1,xint2,xint3,xint4
      real(r8) wmin,wmax,w,dw,dwmax,dwmin,wnuc,dwnew,wb
      real(r8) dfmin,dfmax,fnew,fold,fnmin,fnbar,fsbar,fmbar
      real(r8) alw,sqrtalw
      real(r8) smax
      real(r8) x,arg
      real(r8) xmincoeff,xcut,volcut,surfcut
      real(r8) z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real(r8) etafactor1,etafactor2(ntot_amode),etafactor2max
      integer m,n
!      numerical integration parameters
      real(r8), parameter :: eps=0.3_r8,fmax=0.99_r8,sds=3._r8
 
      real(r8), parameter :: namin=1.e6_r8   ! minimum aerosol number concentration (/m3)

      integer ndist(nx)  ! accumulates frequency distribution of integration bins required
      data ndist/nx*0/
      save ndist

      if(ntot_amode<pmode)then
         write(iulog,*)'ntot_amode,pmode in activate =',ntot_amode,pmode
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
	 call endrun('activate')
      endif

      fn(:)=0._r8
      fm(:)=0._r8
      fluxn(:)=0._r8
      fluxm(:)=0._r8
      flux_fullact=0._r8

      if(nmode.eq.1.and.na(1).lt.1.e-20_r8)return

      if(sigw.le.1.e-5_r8.and.wbar.le.0.)return

      pres=rair*rhoair*tair
      diff0=0.211e-4_r8*(p0/pres)*(tair/t0)**1.94
      conduct0=(5.69_r8+0.017_r8*(tair-t0))*4.186e2_r8*1.e-5_r8 ! convert to J/m/s/deg
      es = estblf(tair)
      qs = epsilo*es/(pres-(1.0_r8 - epsqs)*es)
      dqsdt=latvap/(rh2o*tair*tair)*qs
      alpha=gravit*(latvap/(cpair*rh2o*tair*tair)-1./(rair*tair))
      gamma=(1+latvap/cpair*dqsdt)/(rhoair*qs)
      etafactor2max=1.e10/(alpha*wmaxf)**1.5 ! this should make eta big if na is very small.

      do m=1,nmode
         if(volume(m).gt.1.e-39_r8.and.na(m).gt.1.e-39_r8)then
!            number mode radius (m)
!           write(iulog,*)'alogsig,volc,na=',alogsig(m),volc(m),na(m)
            amcube(m)=(3.*volume(m)/(4.*pi*exp45logsig(m)*na(m)))  ! only if variable size dist
!           growth coefficent Abdul-Razzak & Ghan 1998 eqn 16
!           should depend on mean radius of mode to account for gas kinetic effects
!           see Fountoukis and Nenes, JGR2005 and Meskhidze et al., JGR2006
!           for approriate size to use for effective diffusivity.
            g=1._r8/(rhoh2o/(diff0*rhoair*qs)                                    &
              +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1._r8))
            sqrtg(m)=sqrt(g)
            beta=2._r8*pi*rhoh2o*g*gamma
            etafactor2(m)=1._r8/(na(m)*beta*sqrtg(m))
            if(hygro(m).gt.1.e-10)then
               smc(m)=2.*aten*sqrt(aten/(27.*hygro(m)*amcube(m))) ! only if variable size dist
            else
               smc(m)=100.
            endif
!	    write(iulog,*)'sm,hygro,amcube=',smcrit(m),hygro(m),amcube(m)
         else
            g=1._r8/(rhoh2o/(diff0*rhoair*qs)                                    &
              +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1._r8))
            sqrtg(m)=sqrt(g)
            smc(m)=1._r8
	    etafactor2(m)=etafactor2max ! this should make eta big if na is very small.
         endif
         lnsm(m)=log(smc(m)) ! only if variable size dist
!	 write(iulog,'(a,i4,4g12.2)')'m,na,amcube,hygro,sm,lnsm=', &
!                   m,na(m),amcube(m),hygro(m),sm(m),lnsm(m)
      enddo

      if(sigw.gt.1.e-5_r8)then ! spectrum of updrafts

         wmax=min(wmaxf,wbar+sds*sigw)
         wmin=max(wminf,-wdiab)
         wmin=max(wmin,wbar-sds*sigw)
         w=wmin
         dwmax=eps*sigw
         dw=dwmax
         dfmax=0.2_r8
         dfmin=0.1_r8
         if(wmax.le.w)then
            do m=1,nmode
               fluxn(m)=0.
               fn(m)=0.
               fluxm(m)=0.
               fm(m)=0.
            enddo
            flux_fullact=0._r8
            return
         endif
         do m=1,nmode
            sumflxn(m)=0._r8
            sumfn(m)=0._r8
	    fnold(m)=0._r8
            sumflxm(m)=0._r8
            sumfm(m)=0._r8
	    fmold(m)=0._r8
         enddo
         sumflx_fullact=0._r8

         fold=0._r8
         wold=0._r8
         gold=0._r8

         dwmin = min( dwmax, 0.01_r8 )

         do n=1,200
 100        wnuc=w+wdiab
!           write(iulog,*)'wnuc=',wnuc
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
	    etafactor1=alw*sqrtalw

              do m=1,nmode
	         eta(m)=etafactor1*etafactor2(m)
                 zeta(m)=twothird*sqrtalw*aten/sqrtg(m)
              enddo

              call maxsat(zeta,eta,nmode,smc,smax)
!	      write(iulog,*)'w,smax=',w,smax

              lnsmax=log(smax)

              x=twothird*(lnsm(nmode)-lnsmax)/(sq2*alogsig(nmode))
              fnew=0.5_r8*(1._r8-erf(x))


            dwnew = dw
            if(fnew-fold.gt.dfmax.and.n.gt.1)then
!              reduce updraft increment for greater accuracy in integration
	       if (dw .gt. 1.01*dwmin) then
                  dw=0.7_r8*dw
                  dw=max(dw,dwmin)
                  w=wold+dw
                  go to 100
               else
                  dwnew = dwmin
               endif
            endif

            if(fnew-fold.lt.dfmin)then
!              increase updraft increment to accelerate integration
	       dwnew=min(1.5_r8*dw,dwmax)
            endif
            fold=fnew

            z=(w-wbar)/(sigw*sq2)
            g=exp(-z*z)
            fnmin=1._r8
            xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3

            do m=1,nmode
!              modal
               x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
               fn(m)=0.5_r8*(1.-erf(x))
               fnmin=min(fn(m),fnmin)
!               integration is second order accurate
!               assumes linear variation of f*g with w
               fnbar=(fn(m)*g+fnold(m)*gold)
	       arg=x-1.5_r8*sq2*alogsig(m)
               fm(m)=0.5_r8*(1._r8-erf(arg))
               fmbar=(fm(m)*g+fmold(m)*gold)
               wb=(w+wold)
               if(w.gt.0.)then
                  sumflxn(m)=sumflxn(m)+sixth*(wb*fnbar           &
                      +(fn(m)*g*w+fnold(m)*gold*wold))*dw
                  sumflxm(m)=sumflxm(m)+sixth*(wb*fmbar           &
                      +(fm(m)*g*w+fmold(m)*gold*wold))*dw
               endif
               sumfn(m)=sumfn(m)+0.5_r8*fnbar*dw
!	       write(iulog,'(a,9g10.2)')'lnsmax,lnsm(m),x,fn(m),fnold(m),g,gold,fnbar,dw=',lnsmax,lnsm(m),x,fn(m),fnold(m),g,gold,fnbar,dw
               fnold(m)=fn(m)
               sumfm(m)=sumfm(m)+0.5_r8*fmbar*dw
               fmold(m)=fm(m)
            enddo
!           same form as sumflxm but replace the fm with 1.0
            sumflx_fullact = sumflx_fullact &
                           + sixth*(wb*(g+gold) + (g*w+gold*wold))*dw
!            sumg=sumg+0.5_r8*(g+gold)*dw
            gold=g
            wold=w
            dw=dwnew
            if(n.gt.1.and.(w.gt.wmax.or.fnmin.gt.fmax))go to 20
            w=w+dw
         enddo
         write(iulog,*)'do loop is too short in activate'
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
         write(iulog,*)'wmin=',wmin,' w=',w,' wmax=',wmax,' dw=',dw
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
         write(iulog,*)'wbar=',wbar,' sigw=',sigw,' wdiab=',wdiab
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif          
         write(iulog,*)'wnuc=',wnuc
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
         write(iulog,*)'na=',(na(m),m=1,nmode)
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
         write(iulog,*)'fn=',(fn(m),m=1,nmode)
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
!   dump all subr parameters to allow testing with standalone code
!   (build a driver that will read input and call activate)
         write(iulog,*)'wbar,sigw,wdiab,tair,rhoair,nmode='
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
         write(iulog,*) wbar,sigw,wdiab,tair,rhoair,nmode
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
         write(iulog,*)'na=',na
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
         write(iulog,*)'volume=', (volume(m),m=1,nmode)
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
         write(iulog,*)'sigman=',sigman
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
         write(iulog,*)'hydro='
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
         write(iulog,*) hygro
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif

         call endrun
   20    continue
         ndist(n)=ndist(n)+1
         if(w.lt.wmaxf)then

!            contribution from all updrafts stronger than wmax
!            assuming constant f (close to fmax)
            wnuc=w+wdiab

            z1=(w-wbar)/(sigw*sq2)
            z2=(wmaxf-wbar)/(sigw*sq2)
            g=exp(-z1*z1)
            integ=sigw*0.5*sq2*sqpi*(erf(z2)-erf(z1))
!            consider only upward flow into cloud base when estimating flux
            wf1=max(w,zero)
            zf1=(wf1-wbar)/(sigw*sq2)
            gf1=exp(-zf1*zf1)
            wf2=max(wmaxf,zero)
            zf2=(wf2-wbar)/(sigw*sq2)
            gf2=exp(-zf2*zf2)
            gf=(gf1-gf2)
            integf=wbar*sigw*0.5*sq2*sqpi*(erf(zf2)-erf(zf1))+sigw*sigw*gf

            do m=1,nmode
               sumflxn(m)=sumflxn(m)+integf*fn(m)
               sumfn(m)=sumfn(m)+fn(m)*integ
              sumflxm(m)=sumflxm(m)+integf*fm(m)
               sumfm(m)=sumfm(m)+fm(m)*integ
            enddo
!           same form as sumflxm but replace the fm with 1.0
            sumflx_fullact = sumflx_fullact + integf
!            sumg=sumg+integ
         endif


         do m=1,nmode
            fn(m)=sumfn(m)/(sq2*sqpi*sigw)
!            fn(m)=sumfn(m)/(sumg)
            if(fn(m).gt.1.01)then
               write(iulog,*)'fn=',fn(m),' > 1 in activate'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
	       write(iulog,*)'w,m,na,amcube=',w,m,na(m),amcube(m)
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
	       write(iulog,*)'integ,sumfn,sigw=',integ,sumfn(m),sigw
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
	       call endrun('activate')
            endif
            fluxn(m)=sumflxn(m)/(sq2*sqpi*sigw)
           fm(m)=sumfm(m)/(sq2*sqpi*sigw)
!            fm(m)=sumfm(m)/(sumg)
            if(fm(m).gt.1.01)then
               write(iulog,*)'fm=',fm(m),' > 1 in activate'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
            endif
            fluxm(m)=sumflxm(m)/(sq2*sqpi*sigw)
         enddo
!        same form as fluxm
         flux_fullact = sumflx_fullact/(sq2*sqpi*sigw)

      else

!        single updraft
         wnuc=wbar+wdiab

         if(wnuc.gt.0._r8)then

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
	    etafactor1=alw*sqrtalw

            do m=1,nmode
	       eta(m)=etafactor1*etafactor2(m)
               zeta(m)=twothird*sqrtalw*aten/sqrtg(m)
            enddo

            call maxsat(zeta,eta,nmode,smc,smax)

            lnsmax=log(smax)
            xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3


            do m=1,nmode
!                 modal
		  x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
                  fn(m)=0.5_r8*(1._r8-erf(x))
		  arg=x-1.5_r8*sq2*alogsig(m)
                  fm(m)=0.5_r8*(1._r8-erf(arg))
                if(wbar.gt.0._r8)then
                   fluxn(m)=fn(m)*w
                   fluxm(m)=fm(m)*w
               endif
            enddo
            flux_fullact = w
         endif

      endif

      return
      end subroutine activate_modal

      subroutine maxsat(zeta,eta,nmode,smc,smax)

!      calculates maximum supersaturation for multiple
!      competing aerosol modes.

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      implicit none

      integer nmode ! number of modes
      real(r8) smc(ntot_amode) ! critical supersaturation for number mode radius
      real(r8) zeta(ntot_amode), eta(ntot_amode)
      real(r8) smax ! maximum supersaturation
      integer m  ! mode index
      real(r8) sum, g1, g2, g1sqrt, g2sqrt

      do m=1,nmode
         if(zeta(m).gt.1.e5_r8*eta(m).or.smc(m)*smc(m).gt.1.e5_r8*eta(m))then
!            weak forcing. essentially none activated
            smax=1.e-20_r8
         else
!            significant activation of this mode. calc activation all modes.
            go to 1
         endif
      enddo

      return

  1   continue

      sum=0
      do m=1,nmode
         if(eta(m).gt.1.e-20_r8)then
            g1=zeta(m)/eta(m)
            g1sqrt=sqrt(g1)
            g1=g1sqrt*g1
            g2=smc(m)/sqrt(eta(m)+3._r8*zeta(m))
            g2sqrt=sqrt(g2)
            g2=g2sqrt*g2
            sum=sum+(f1(m)*g1+f2(m)*g2)/(smc(m)*smc(m))
         else
            sum=1.e20_r8
         endif
      enddo

      smax=1._r8/sqrt(sum)

      return

      end subroutine maxsat

      subroutine ccncalc(lchnk,ncol,tair,cs,raer,qqcw,ccn,psat,supersat,alogsig,npv)

!     calculates number concentration of aerosols activated as CCN at
!     supersaturation supersat.
!     assumes an internal mixture of a multiple externally-mixed aerosol modes
!     cgs units

!     Ghan et al., Atmos. Res., 1993, 198-221.

      use shr_kind_mod,  only: r8 => shr_kind_r8
#ifndef WRF_PORT
      use ppgrid,        only: pcols, pver, pverp
      use constituents,  only: pcnst
      use modal_aero_data
#else
      use module_cam_support, only: pcols, pver, pverp, pcnst =>pcnst_mp
#endif
      use physconst,     only: rhoh2o, mwh2o, r_universal
      use modal_aero_data, only:
      use error_function, only: erf

      implicit none

!     input

      integer lchnk ! chunk index
      integer, intent(in) :: ncol ! number of columns
      integer, intent(in) :: psat ! number of supesaturations
      real(r8), intent(in) :: raer(pcols,pver,pcnst) ! aerosol mass, number mixing ratios
      type(qqcw_type), intent(in) :: QQCW(:)


      real(r8), intent(in) :: tair(pcols,pver)          ! air temperature (K)
      real(r8), intent(in) :: cs(pcols,pver)    ! air density (kg/m3)
      real(r8), intent(in) :: supersat(psat)
      real(r8), intent(in) :: npv(ntot_amode) ! number per volume concentration
      real(r8), intent(in) :: alogsig(ntot_amode) ! natl log of geometric standard dev of aerosol
      real(r8), intent(out) :: ccn(pcols,pver,psat) ! number conc of aerosols activated at supersat (#/m3)

!     local

      real(r8) naerosol(pcols) ! interstit+activated aerosol number conc (/m3)
      real(r8) vaerosol(pcols) ! interstit+activated aerosol volume conc (m3/m3)
      real(r8) exp45logsig(ntot_amode)     ! exp(4.5*alogsig**2)
      real(r8) amcube(pcols)
      real(r8) super(psat) ! supersaturation
      real(r8) amcubecoef(ntot_amode)
      real(r8) :: surften       ! surface tension of water w/respect to air (N/m)
      real(r8) surften_coef
      real(r8) a(pcols) ! surface tension parameter
      real(r8) hygro(pcols)  ! aerosol hygroscopicity
      real(r8) sm(pcols)  ! critical supersaturation at mode radius
      real(r8) argfactor(ntot_amode),arg(pcols)
!     mathematical constants
      real(r8) pi
      real(r8) twothird,sq2
      integer l,m,n,i,k
      real(r8) log,cc
      real(r8) smcoefcoef,smcoef(pcols)
      integer phase ! phase of aerosol

      super(:)=supersat(:)*0.01
      pi = 4.*atan(1.0)
      sq2=sqrt(2._r8)
      twothird=2._r8/3._r8
      surften=0.076_r8
      surften_coef=2._r8*mwh2o*surften/(r_universal*rhoh2o)
      smcoefcoef=2._r8/sqrt(27._r8)

      do m=1,ntot_amode
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
	  amcubecoef(m)=3./(4.*pi*exp45logsig(m))
	  argfactor(m)=twothird/(sq2*alogsig(m))
      end do

      do k=1,pver

         do i=1,ncol
            ccn(i,k,:)=0.
            a(i)=surften_coef/tair(i,k)
	    smcoef(i)=smcoefcoef*a(i)*sqrt(a(i))
	 end do

         do m=1,ntot_amode
            phase=3 ! interstitial+cloudborne
            call loadaer(raer,qqcw,1,ncol,k,m,cs,npv(m), phase, &
                         naerosol, vaerosol,  hygro )
            where(naerosol(:ncol)>1.e-3)
               amcube(:ncol)=amcubecoef(m)*vaerosol(:ncol)/naerosol(:ncol)
	       sm(:ncol)=smcoef(:ncol)/sqrt(hygro(:ncol)*amcube(:ncol)) ! critical supersaturation
	    elsewhere
	       sm(:ncol)=1. ! value shouldn't matter much since naerosol is small
	    endwhere
            do l=1,psat
               do i=1,ncol
	          arg(i)=argfactor(m)*log(sm(i)/super(l))
                  ccn(i,k,l)=ccn(i,k,l)+naerosol(i)*0.5*(1._r8-erf(arg(i)))
               enddo
             enddo
         enddo
      enddo
      ccn(:ncol,:,:)=ccn(:ncol,:,:)*1.e-6 ! convert from #/m3 to #/cm3

      return
      end subroutine ccncalc

      subroutine loadaer(raer,qqcw,istart,istop,k,m,cs,npv1, phase, &
                         naerosol, vaerosol,  hygro )

      use shr_kind_mod,  only: r8 => shr_kind_r8
#ifndef WRF_PORT
      use abortutils,    only: endrun
      use ppgrid,        only: pcols, pver
      use modal_aero_data
#else
      use module_cam_support, only: pcols, pver, endrun, pcnst =>pcnst_mp
      use modal_aero_data,   only: nspec_amode => nspec_amode_mp, &
           lmassptr_amode    => lmassptr_amode_mp , lmassptrcw_amode  => lmassptrcw_amode_mp, &
           lspectype_amode   => lspectype_amode_mp, specdens_amode    => specdens_amode_mp,   &
           spechygro         => spechygro_mp      , numptr_amode      => numptr_amode_mp,     &
           numptrcw_amode    => numptrcw_amode_mp , voltonumbhi_amode => voltonumbhi_amode_mp,&
           voltonumblo_amode => voltonumblo_amode_mp 
#endif
     

      implicit none

      !      load aerosol number, volume concentrations, and bulk hygroscopicity

      type(qqcw_type), intent(in) :: QQCW(:)

      real(r8), intent(in) :: raer(pcols,pver,pcnst) ! aerosol mass, number mixing ratios
       integer, intent(in) :: istart, istop ! start, stop column index
       integer, intent(in) ::  m          ! m=mode index
       integer, intent(in) ::  k          ! level index
       real(r8), intent(in) :: cs(pcols,pver)  ! air density (kg/m3)
       integer, intent(in) :: phase ! phase of aerosol: 1 for interstitial, 2 for cloud-borne, 3 for sum
       real(r8), intent(in) :: npv1 ! number per volume
       real(r8), intent(out) :: naerosol(pcols)                ! interstitial number conc (/m3)
       real(r8), intent(out) :: vaerosol(pcols)       ! interstitial+activated volume conc (m3/m3)
       real(r8), intent(out) :: hygro(pcols) ! bulk hygroscopicity of mode

!      internal

       real(r8) vol(pcols) ! aerosol volume mixing ratio
       integer i,lnum,lnumcw,l,ltype,lmass,lmasscw

          do i=istart,istop
             vaerosol(i)=0._r8
	     hygro(i)=0._r8
	  end do

          do l=1,nspec_amode(m)
             lmass=lmassptr_amode(l,m) ! interstitial
             lmasscw=lmassptrcw_amode(l,m) ! cloud-borne
             ltype=lspectype_amode(l,m)
	     if(phase.eq.3)then
	        do i=istart,istop
!                  vol(i)=max(raer(i,k,lmass)+raer(i,k,lmasscw),0._r8)/specdens_amode(ltype)
                   vol(i)=max(raer(i,k,lmass)+qqcw(lmasscw)%fldcw(i,k),0._r8)/specdens_amode(ltype)
		end do
             elseif(phase.eq.2)then
                do i=istart,istop
                   vol(i)=max(qqcw(lmasscw)%fldcw(i,k),0._r8)/specdens_amode(ltype)
                end do
             elseif(phase.eq.1)then
                do i=istart,istop
                   vol(i)=max(raer(i,k,lmass),0._r8)/specdens_amode(ltype)
		end do
             else
	        write(iulog,*)'phase=',phase,' in loadaer'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
	        call endrun('phase error in loadaer')
	     endif
	     do i=istart,istop
                vaerosol(i)=vaerosol(i)+vol(i)
                hygro(i)=hygro(i)+vol(i)*spechygro(ltype)
	     end do
          enddo
	  do i=istart,istop
             if (vaerosol(i) > 1.0e-30_r8) then   ! +++xl add 8/2/2007
               hygro(i)=hygro(i)/(vaerosol(i))
               vaerosol(i)=vaerosol(i)*cs(i,k)
             else
               hygro(i)=0.0_r8
               vaerosol(i)=0.0_r8
             endif
	  end do

          lnum=numptr_amode(m)
          lnumcw=numptrcw_amode(m)
          if(lnum.gt.0)then
!            aerosol number predicted
             if(phase.eq.3)then
	        do i=istart,istop
!                  naerosol(i)=(raer(i,k,lnum)+raer(i,k,lnumcw))*cs(i,k)
                   naerosol(i)=(raer(i,k,lnum)+qqcw(lnumcw)%fldcw(i,k))*cs(i,k)
		end do
             elseif(phase.eq.2)then
                do i=istart,istop
                   naerosol(i)=qqcw(lnumcw)%fldcw(i,k)*cs(i,k)
                end do
	     else
	        do i=istart,istop
                   naerosol(i)=raer(i,k,lnum)*cs(i,k)
		end do
	     endif
!            adjust number so that dgnumlo < dgnum < dgnumhi
	     do i=istart,istop
                naerosol(i) = max( naerosol(i), vaerosol(i)*voltonumbhi_amode(m) )
                naerosol(i) = min( naerosol(i), vaerosol(i)*voltonumblo_amode(m) )
	     end do
          else
!            aerosol number diagnosed from mass and prescribed size
	     do i=istart,istop
                naerosol(i)=vaerosol(i)*npv1
                naerosol(i)=max(naerosol(i),0._r8)
	     end do
          endif

       return
       end subroutine loadaer
#endif
      end module ndrop



