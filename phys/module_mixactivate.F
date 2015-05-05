!************************************************************************
! This computer software was prepared by Battelle Memorial Institute, 
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see chem/module_mosaic_driver.F for references and terms
! of use
!************************************************************************

MODULE module_mixactivate
PRIVATE
PUBLIC prescribe_aerosol_mixactivate, mixactivate, activate !BSINGH - added 'activate' for WRFCuP scheme
CONTAINS


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! 06-nov-2005 rce - grid_id & ktau added to arg list
! 25-apr-2006 rce - dens_aer is (g/cm3), NOT (kg/m3)
      subroutine prescribe_aerosol_mixactivate (                      &
		grid_id, ktau, dtstep, naer,                          &
		ccn_conc, chem_opt,                                   & ! RAS
		rho_phy, th_phy, pi_phy, w, cldfra, cldfra_old,       &
		z, dz8w, p_at_w, t_at_w, exch_h,                      &
        qv, qc, qi, qndrop3d,                                &
        nsource,                                              &
		ids,ide, jds,jde, kds,kde,                            &
		ims,ime, jms,jme, kms,kme,                            &
		its,ite, jts,jte, kts,kte,                            &
		f_qc, f_qi, cn                                        )

!        USE module_configure

! wrapper to call mixactivate for mosaic description of aerosol

	implicit none

!   subr arguments
	integer, intent(in) ::                  &
		grid_id, ktau,                  &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(in) :: dtstep
	real, intent(inout) :: naer ! aerosol number (/kg)
        real, intent(in) :: ccn_conc ! CCN conc set within namelist
        integer, optional, intent(in) :: chem_opt

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		rho_phy, th_phy, pi_phy, w,  &
		z, dz8w, p_at_w, t_at_w, exch_h

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: cldfra, cldfra_old

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		qv, qc, qi

	real, intent(inout), optional,  &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		cn ! single-size CCN concentration

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		qndrop3d

	real, intent(out),   &
		dimension( ims:ime, kms:kme, jms:jme) :: nsource

    LOGICAL, OPTIONAL :: f_qc, f_qi

! local vars
	integer maxd_aphase, maxd_atype, maxd_asize, maxd_acomp, max_chem
	parameter (maxd_aphase=2,maxd_atype=1,maxd_asize=1,maxd_acomp=1, max_chem=10)
	real ddvel(its:ite, jts:jte, max_chem) ! dry deposition velosity
	real qsrflx(ims:ime, jms:jme, max_chem) ! dry deposition flux of aerosol
	real chem(ims:ime, kms:kme, jms:jme, max_chem) ! chem array
	integer i,j,k,l,m,n,p
	real hygro( its:ite, kts:kte, jts:jte, maxd_asize, maxd_atype ) ! bulk
	integer ntype_aer, nsize_aer(maxd_atype),ncomp_aer(maxd_atype), nphase_aer
      	integer massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ),   &
      	  waterptr_aer( maxd_asize, maxd_atype ),   &
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
	  ai_phase, cw_phase
        real dlo_sect( maxd_asize, maxd_atype ),   & ! minimum size of section (cm)
             dhi_sect( maxd_asize, maxd_atype ),   & ! maximum size of section (cm)
	     sigmag_aer(maxd_asize, maxd_atype),   & ! geometric standard deviation of aerosol size dist
	     dgnum_aer(maxd_asize, maxd_atype),    & ! median diameter (cm) of number distrib of mode
	     dens_aer( maxd_acomp, maxd_atype),    & ! density (g/cm3) of material
	     mw_aer( maxd_acomp, maxd_atype),      & ! molecular weight (g/mole)
	     dpvolmean_aer(maxd_asize, maxd_atype)   ! mean-volume diameter (cm) of mode
! terminology:  (pi/6) * (mean-volume diameter)**3 ==
!       (volume mixing ratio of section/mode)/(number mixing ratio)
	real, dimension(ims:ime,kms:kme,jms:jme) :: &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat
	integer idrydep_onoff
	real, dimension(ims:ime,kms:kme,jms:jme) :: t_phy
	integer msectional


	  integer ptr
	  real maer

!      if(naer.lt.1.)then
!	     naer=1000.e6 ! #/kg default value
!      endif
      IF ( (naer.lt.1.) .OR. ( PRESENT(chem_opt) .AND. (chem_opt.eq.401))) THEN
             naer = ccn_conc !CCN value set in namelist
      ENDIF
	  ai_phase=1
	  cw_phase=2
	  idrydep_onoff = 0
	  msectional = 0

	  t_phy(its:ite,kts:kte,jts:jte)=th_phy(its:ite,kts:kte,jts:jte)*pi_phy(its:ite,kts:kte,jts:jte)

      ntype_aer=maxd_atype
      do n=1,ntype_aer
         nsize_aer(n)=maxd_asize
	 ncomp_aer(n)=maxd_acomp
      end do
      nphase_aer=maxd_aphase

! set properties for each type and size
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          dlo_sect( m,n )=0.01e-4    ! minimum size of section (cm)
          dhi_sect( m,n )=0.5e-4    ! maximum size of section (cm)
	  sigmag_aer(m,n)=2.      ! geometric standard deviation of aerosol size dist
	  dgnum_aer(m,n)=0.1e-4       ! median diameter (cm) of number distrib of mode
	  dpvolmean_aer(m,n) = dgnum_aer(m,n) * exp( 1.5 * (log(sigmag_aer(m,n)))**2 )
	  end do
	  do l=1,ncomp_aer(n)
	     dens_aer( l, n)=1.0   ! density (g/cm3) of material
	     mw_aer( l, n)=132. ! molecular weight (g/mole)
	  end do
      end do
       ptr=0
       do p=1,nphase_aer
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          ptr=ptr+1
          numptr_aer( m, n, p )=ptr
        
        if(p.eq.ai_phase)then
         IF ( present( cn ) ) THEN
            chem(its:ite,kts:kte,jts:jte,ptr) = cn(its:ite,kts:kte,jts:jte)
          ELSE  ! ERM: use qndrop3d as a proxy for number of activated CCN
            chem(its:ite,kts:kte,jts:jte,ptr) = Max(0.0, naer-qndrop3d(its:ite,kts:kte,jts:jte) )
          ENDIF
        else
           chem(its:ite,kts:kte,jts:jte,ptr)=0.
        endif
      end do ! size
      end do ! type
      end do ! phase
       do p=1,maxd_aphase
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
	  do l=1,ncomp_aer(n)
          ptr=ptr+1
	     if(ptr.gt.max_chem)then
	        write(6,*)'ptr,max_chem=',ptr,max_chem,' in prescribe_aerosol_mixactivate'
	        call wrf_error_fatal("1")
	     endif
	     massptr_aer(l, m, n, p)=ptr
! maer is ug/kg-air;  naer is #/kg-air;  dgnum is cm;  dens_aer is g/cm3
! 1.e6 factor converts g to ug
	     maer= 1.0e6 * naer * dens_aer(l,n) * ( (3.1416/6.) *   &
                 (dgnum_aer(m,n)**3) * exp( 4.5*((log(sigmag_aer(m,n)))**2) ) )
           if(p.eq.ai_phase)then
            IF ( present( cn ) ) THEN
              chem(its:ite,kts:kte,jts:jte,ptr) = 1.0e6 * cn(its:ite,kts:kte,jts:jte)* dens_aer(l,n) * ( (3.1416/6.) *   &
                 (dgnum_aer(m,n)**3) * exp( 4.5*((log(sigmag_aer(m,n)))**2) ) )
            ELSE ! ERM: use qndrop3d as a proxy for number of activated CCN
              chem(its:ite,kts:kte,jts:jte,ptr) = 1.0e6 * Max(0.0, naer -qndrop3d(its:ite,kts:kte,jts:jte))* &
                  dens_aer(l,n) * ( (3.1416/6.) *(dgnum_aer(m,n)**3) * exp( 4.5*((log(sigmag_aer(m,n)))**2) ) )
            ENDIF
           else
              chem(its:ite,kts:kte,jts:jte,ptr)=0.
           endif
        end do
	end do ! size
	end do ! type
	end do ! phase
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          ptr=ptr+1
	  if(ptr.gt.max_chem)then
	     write(6,*)'ptr,max_chem=',ptr,max_chem,' in prescribe_aerosol_mixactivate'
	     call wrf_error_fatal("1")
	  endif
!wig	  waterptr_aer(m, n)=ptr
	  waterptr_aer(m, n)=-1
	end do ! size
	end do ! type
	ddvel(its:ite,jts:jte,:)=0.
    hygro(its:ite,kts:kte,jts:jte,:,:) = 0.5

! 06-nov-2005 rce - grid_id & ktau added to arg list
      call mixactivate(  msectional,     &
            chem,max_chem,qv,qc,qi,qndrop3d,        &
            t_phy, w, ddvel, idrydep_onoff,  &
            maxd_acomp, maxd_asize, maxd_atype, maxd_aphase,   &
            ncomp_aer, nsize_aer, ntype_aer, nphase_aer,  &
            numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dpvolmean_aer,  &
            dens_aer, mw_aer,           &
            waterptr_aer, hygro,  ai_phase, cw_phase,                &
            ids,ide, jds,jde, kds,kde,                            &
            ims,ime, jms,jme, kms,kme,                            &
            its,ite, jts,jte, kts,kte,                            &
            rho_phy, z, dz8w, p_at_w, t_at_w, exch_h,      &
            cldfra, cldfra_old, qsrflx,         &
            ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
            grid_id, ktau, dtstep, &
            F_QC=f_qc, F_QI=f_qi                              )


       ! ERM : If CCN field was passed in, then copy back the new field, which is the first
       IF ( present( cn ) ) THEN
         cn(its:ite,kts:kte,jts:jte) = Max(0.0, chem(its:ite,kts:kte,jts:jte,1))
       ENDIF

      end subroutine prescribe_aerosol_mixactivate

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   nov-04 sg ! replaced amode with aer and expanded aerosol dimension to include type and phase

! 06-nov-2005 rce - grid_id & ktau added to arg list
! 25-apr-2006 rce - dens_aer is (g/cm3), NOT (kg/m3)
subroutine mixactivate(  msectional,            &
           chem, num_chem, qv, qc, qi, qndrop3d,         &
           temp, w, ddvel, idrydep_onoff,  &
           maxd_acomp, maxd_asize, maxd_atype, maxd_aphase,   &
           ncomp_aer, nsize_aer, ntype_aer, nphase_aer,  &
           numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dpvolmean_aer,  &
           dens_aer, mw_aer,               &
           waterptr_aer, hygro, ai_phase, cw_phase,              &
           ids,ide, jds,jde, kds,kde,                            &
           ims,ime, jms,jme, kms,kme,                            &
           its,ite, jts,jte, kts,kte,                            &
           rho, zm, dz8w, p_at_w, t_at_w, kvh,      &
           cldfra, cldfra_old, qsrflx,          &
           ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
           grid_id, ktau, dtstep, &
           f_qc, f_qi                       )


!     vertical diffusion and nucleation of cloud droplets
!     assume cloud presence controlled by cloud fraction
!     doesn't distinguish between warm, cold clouds

  USE module_model_constants, only: g, rhowater, xlv, cp, rvovrd, r_d, r_v, mwdry, ep_2
  USE module_radiation_driver, only: cal_cldfra2

  implicit none

!     input

  INTEGER, intent(in) ::         grid_id, ktau
  INTEGER, intent(in) ::         num_chem
  integer, intent(in) ::         ids,ide, jds,jde, kds,kde,    &
                                 ims,ime, jms,jme, kms,kme,    &
                                 its,ite, jts,jte, kts,kte

  integer maxd_aphase, nphase_aer, maxd_atype, ntype_aer
  integer maxd_asize, maxd_acomp, nsize_aer(maxd_atype)
  integer, intent(in) ::   &
       ncomp_aer( maxd_atype  ),   &
       massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ),   &
       waterptr_aer( maxd_asize, maxd_atype ),   &
       numptr_aer( maxd_asize, maxd_atype, maxd_aphase), &
       ai_phase, cw_phase
  integer, intent(in) :: msectional ! 1 for sectional, 0 for modal
  integer, intent(in) :: idrydep_onoff
  real, intent(in)  ::                       &
       dlo_sect( maxd_asize, maxd_atype ),   & ! minimum size of section (cm)
       dhi_sect( maxd_asize, maxd_atype ),   & ! maximum size of section (cm)
       sigmag_aer(maxd_asize, maxd_atype),   & ! geometric standard deviation of aerosol size dist
       dens_aer( maxd_acomp, maxd_atype),    & ! density (g/cm3) of material
       mw_aer( maxd_acomp, maxd_atype),      & ! molecular weight (g/mole)
       dpvolmean_aer(maxd_asize, maxd_atype)   ! mean-volume diameter (cm) of mode
! terminology:  (pi/6) * (mean-volume diameter)**3 ==
!       (volume mixing ratio of section/mode)/(number mixing ratio)


  REAL, intent(inout), DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ) :: &
       chem ! aerosol molar mixing ratio (ug/kg or #/kg)

  REAL, intent(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: &
       qv, qc, qi ! water species (vapor, cloud drops, cloud ice) mixing ratio (g/g)

  LOGICAL, OPTIONAL :: f_qc, f_qi

  REAL, intent(inout), DIMENSION( ims:ime, kms:kme, jms:jme ) :: &
       qndrop3d    ! water species mixing ratio (g/g)

  real, intent(in) :: dtstep             ! time step for microphysics (s)
  real, intent(in) :: temp(ims:ime, kms:kme, jms:jme)    ! temperature (K)
  real, intent(in) :: w(ims:ime, kms:kme, jms:jme)   ! vertical velocity (m/s)
  real, intent(in) :: rho(ims:ime, kms:kme, jms:jme)    ! density at mid-level  (kg/m3)
  REAL, intent(in) :: ddvel( its:ite, jts:jte, num_chem ) ! deposition velocity  (m/s)
  real, intent(in) :: zm(ims:ime, kms:kme, jms:jme)     ! geopotential height of level (m)
  real, intent(in) :: dz8w(ims:ime, kms:kme, jms:jme) ! layer thickness (m)
  real, intent(in) :: p_at_w(ims:ime, kms:kme, jms:jme) ! pressure at layer interface (Pa)
  real, intent(in) :: t_at_w(ims:ime, kms:kme, jms:jme) ! temperature at layer interface (K)
  real, intent(in) :: kvh(ims:ime, kms:kme, jms:jme)    ! vertical diffusivity (m2/s)
  real, intent(inout) :: cldfra_old(ims:ime, kms:kme, jms:jme)! cloud fraction on previous time step
  real, intent(inout) :: cldfra(ims:ime, kms:kme, jms:jme)    ! cloud fraction
  real, intent(in) :: hygro( its:ite, kts:kte, jts:jte, maxd_asize, maxd_atype ) ! bulk hygroscopicity   &

  REAL, intent(out), DIMENSION( ims:ime, jms:jme, num_chem ) ::   qsrflx ! dry deposition rate for aerosol
  real, intent(out), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &  ! droplet number source (#/kg/s)
       ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat


!--------------------Local storage-------------------------------------
!
  real :: dgnum_aer(maxd_asize, maxd_atype) ! median diameter (cm) of number distrib of mode
  real :: qndrop(kms:kme)      ! cloud droplet number mixing ratio (#/kg)
  real :: lcldfra(kms:kme)     ! liquid cloud fraction
  real :: lcldfra_old(kms:kme) ! liquid cloud fraction for previous timestep
  real :: wtke(kms:kme)        ! turbulent vertical velocity at base of layer k (m2/s)
  real zn(kms:kme)             ! g/pdel (m2/g) for layer
  real zs(kms:kme)             ! inverse of distance between levels (m)
  real, parameter :: zkmin = 0.01
  real, parameter :: zkmax = 100.
  real cs(kms:kme)             ! air density (kg/m3) at layer center
  real csbot(kms:kme)          ! air density (kg/m3) at layer bottom
  real csbot_cscen(kms:kme)    ! csbot(k)/cs(k)
  real dz(kms:kme)             ! geometric thickness of layers (m)

  real wdiab                   ! diabatic vertical velocity
!      real, parameter :: wmixmin = 0.1 ! minimum turbulence vertical velocity (m/s)
  real, parameter :: wmixmin = 0.2 ! minimum turbulence vertical velocity (m/s)
!      real, parameter :: wmixmin = 1.0 ! minimum turbulence vertical velocity (m/s)
  real :: qndrop_new(kms:kme)  ! droplet number nucleated on cloud boundaries
  real :: ekd(kms:kme)         ! diffusivity for droplets (m2/s)
  real :: ekk(kms:kme)         ! density*diffusivity for droplets (kg/m3 m2/s)
  real :: srcn(kms:kme)        ! droplet source rate (/s)
  real, parameter :: sq2pi = 2.5066282746
  real dtinv

  integer km1,kp1
  real wbar,wmix,wmin,wmax
  real dum
  real tmpa, tmpb, tmpc, tmpc1, tmpc2, tmpd, tmpe, tmpf
  real tmpcourno
  real dact
  real fluxntot         ! (#/cm2/s)
  real fac_srflx
  real depvel_drop, depvel_tmp
  real, parameter :: depvel_uplimit = 1.0 ! upper limit for dep vels (m/s)
  real :: surfrate(num_chem) ! surface exchange rate (/s)
  real surfratemax      ! max surfrate for all species treated here
  real surfrate_drop    ! surfade exchange rate for droplelts
  real dtmin,tinv,dtt
  integer nsubmix,nsubmix_bnd
  integer i,j,k,m,n,nsub
  real dtmix
  real alogarg
  real qcld
  real pi
  integer nnew,nsav,ntemp
  real :: overlapp(kms:kme),overlapm(kms:kme) ! cloud overlap
  real ::  ekkp(kms:kme),ekkm(kms:kme) ! zn*zs*density*diffusivity
!  integer, save :: count_submix(100)=0 ! wig: Note that this is a no-no for tile threads with OMP

  integer lnum,lnumcw,l,lmass,lmasscw,lsfc,lsfccw,ltype,lsig,lwater
  integer :: ntype(maxd_asize)

  real ::  naerosol(maxd_asize, maxd_atype)    ! interstitial aerosol number conc (/m3)
  real ::  naerosolcw(maxd_asize, maxd_atype)  ! activated number conc (/m3)
  real ::   maerosol(maxd_acomp,maxd_asize, maxd_atype)   ! interstit mass conc (kg/m3)
  real ::   maerosolcw(maxd_acomp,maxd_asize, maxd_atype) ! activated mass conc (kg/m3)
  real ::   maerosol_tot(maxd_asize, maxd_atype)     ! species-total interstit mass conc (kg/m3)
  real ::   maerosol_totcw(maxd_asize, maxd_atype)   ! species-total activated mass conc (kg/m3)
  real ::   vaerosol(maxd_asize, maxd_atype) ! interstit+activated aerosol volume conc (m3/m3)
  real ::   vaerosolcw(maxd_asize, maxd_atype) ! activated aerosol volume conc (m3/m3)
  real ::   raercol(kms:kme,num_chem,2) ! aerosol mass, number mixing ratios
  real ::   source(kms:kme) !

  real ::   fn(maxd_asize, maxd_atype)         ! activation fraction for aerosol number
  real ::   fs(maxd_asize, maxd_atype)         ! activation fraction for aerosol sfcarea
  real ::   fm(maxd_asize, maxd_atype)         ! activation fraction for aerosol mass
  integer ::   ncomp(maxd_atype)

  real ::   fluxn(maxd_asize, maxd_atype)      ! number  activation fraction flux (m/s)
  real ::   fluxs(maxd_asize, maxd_atype)      ! sfcarea activation fraction flux (m/s)
  real ::   fluxm(maxd_asize, maxd_atype)      ! mass    activation fraction flux (m/s)
  real ::   flux_fullact(kms:kme)              ! 100%    activation fraction flux (m/s)
!     note:  activation fraction fluxes are defined as
!     fluxn = [flux of activated aero. number into cloud (#/m2/s)]
!           / [aero. number conc. in updraft, just below cloudbase (#/m3)]

  real :: nact(kms:kme,maxd_asize, maxd_atype)  ! fractional aero. number  activation rate (/s)
  real :: mact(kms:kme,maxd_asize, maxd_atype)  ! fractional aero. mass    activation rate (/s)
  real :: npv(maxd_asize, maxd_atype) ! number per volume concentration (/m3)
  real scale

  real :: hygro_aer(maxd_asize, maxd_atype)  ! hygroscopicity of aerosol mode
  real :: exp45logsig     ! exp(4.5*alogsig**2)
  real :: alogsig(maxd_asize, maxd_atype) ! natl log of geometric standard dev of aerosol
  integer, parameter :: psat=6  ! number of supersaturations to calc ccn concentration
  real ccn(kts:kte,psat)        ! number conc of aerosols activated at supersat
  real, parameter :: supersat(psat)= &! supersaturation (%) to determine ccn concentration
       (/0.02,0.05,0.1,0.2,0.5,1.0/)
  real super(psat) ! supersaturation
  real, parameter :: surften = 0.076 ! surface tension of water w/respect to air (N/m)
  real :: ccnfact(psat,maxd_asize, maxd_atype)
  real :: amcube(maxd_asize, maxd_atype) ! cube of dry mode radius (m)
  real :: argfactor(maxd_asize, maxd_atype)
  real aten ! surface tension parameter
  real t0 ! reference temperature
  real sm ! critical supersaturation
  real arg

  integer,parameter :: icheck_colmass = 0
           ! icheck_colmass > 0 turns on mass/number conservation checking
           ! values of 1, 10, 100 produce less to more diagnostics
  integer :: colmass_worst_ij( 2, 0:maxd_acomp, maxd_asize, maxd_atype )
  integer :: colmass_maxworst_i(3)
  real :: colmass_bgn( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_end( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_sfc( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_worst( 0:maxd_acomp, maxd_asize, maxd_atype )
  real :: colmass_maxworst_r
  real :: rhodz( kts:kte ), rhodzsum

!!$#if (defined AIX)
!!$#define ERF erf
!!$#define ERFC erfc
!!$#else
!!$#define ERF erf
!!$    real erf
!!$#define ERFC erfc
!!$    real erfc
!!$#endif

  character*8, parameter :: ccn_name(psat)=(/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)


  colmass_worst(:,:,:) = 0.0
  colmass_worst_ij(:,:,:,:) = -1


  arg = 1.0
  if (abs(0.8427-ERF_ALT(arg))/0.8427>0.001) then
     write (6,*) 'erf_alt(1.0) = ',ERF_ALT(arg)
     call wrf_error_fatal('dropmixnuc: Error function error')
  endif
  arg = 0.0
  if (ERF_ALT(arg) /= 0.0) then
     write (6,*) 'erf_alt(0.0) = ',ERF_ALT(arg)
     call wrf_error_fatal('dropmixnuc: Error function error')
  endif

  pi = 4.*atan(1.0)
  dtinv=1./dtstep

  depvel_drop =  0.1 ! prescribed here rather than getting it from dry_dep_driver
  if (idrydep_onoff .le. 0) depvel_drop =  0.0
  depvel_drop =  min(depvel_drop,depvel_uplimit)

  do n=1,ntype_aer
     do m=1,nsize_aer(n)
        ncomp(n)=ncomp_aer(n)
        alogsig(m,n)=alog(sigmag_aer(m,n))
        dgnum_aer(m,n) = dpvolmean_aer(m,n) * exp( -1.5*alogsig(m,n)*alogsig(m,n) )
!    print *,'sigmag_aer,dgnum_aer=',sigmag_aer(m,n),dgnum_aer(m,n)
        ! npv is used only if number is diagnosed from volume
        npv(m,n)=6./(pi*(0.01*dgnum_aer(m,n))**3*exp(4.5*alogsig(m,n)*alogsig(m,n)))
     end do
  end do
  t0=273.15   !wig, 1-Mar-2009: Added .15
  aten=2.*surften/(r_v*t0*rhowater)
  super(:)=0.01*supersat(:)
  do n=1,ntype_aer
     do m=1,nsize_aer(n)
        exp45logsig=exp(4.5*alogsig(m,n)*alogsig(m,n))
        argfactor(m,n)=2./(3.*sqrt(2.)*alogsig(m,n))
        amcube(m,n)=3./(4.*pi*exp45logsig*npv(m,n))
     enddo
  enddo

  IF( PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
     CALL cal_cldfra2(CLDFRA,qc,qi,f_qc,f_qi,     &
          ids,ide, jds,jde, kds,kde,              &
          ims,ime, jms,jme, kms,kme,              &
          its,ite, jts,jte, kts,kte               )
  END IF

  qsrflx(its:ite,jts:jte,:) = 0.

!     start loop over columns

OVERALL_MAIN_J_LOOP: do j=jts,jte
OVERALL_MAIN_I_LOOP: do i=its,ite

!        load number nucleated into qndrop on cloud boundaries

! initialization for current i .........................................

     do k=kts+1,kte
	    zs(k)=1./(zm(i,k,j)-zm(i,k-1,j))
	 enddo
	 zs(kts)=zs(kts+1)
     zs(kte+1)=0.

     do k=kts,kte
!!$	    if(qndrop3d(i,k,j).lt.-10.e6.or.qndrop3d(i,k,j).gt.1.E20)then
!!$!	       call wrf_error_fatal("1")
!!$	    endif
        if(f_qi)then
           qcld=qc(i,k,j)+qi(i,k,j)
        else
           qcld=qc(i,k,j)
        endif
        if(qcld.lt.-1..or.qcld.gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qcld=',qcld,' for i,k,j=',i,k,j
           call wrf_error_fatal("1")
        endif
        if(qcld.gt.1.e-20)then
           lcldfra(k)=cldfra(i,k,j)*qc(i,k,j)/qcld
           lcldfra_old(k)=cldfra_old(i,k,j)*qc(i,k,j)/qcld
        else
           lcldfra(k)=0.
           lcldfra_old(k)=0.
        endif
        qndrop(k)=qndrop3d(i,k,j)
!	    qndrop(k)=1.e5
        cs(k)=rho(i,k,j) ! air density (kg/m3)
        dz(k)=dz8w(i,k,j)
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              nact(k,m,n)=0.
              mact(k,m,n)=0.
           enddo
        enddo
        zn(k)=1./(cs(k)*dz(k))
        if(k>kts)then
           ekd(k)=kvh(i,k,j)
           ekd(k)=max(ekd(k),zkmin)
           ekd(k)=min(ekd(k),zkmax)
        else
           ekd(k)=0
        endif
!           diagnose subgrid vertical velocity from diffusivity
        if(k.eq.kts)then
           wtke(k)=sq2pi*depvel_drop
!               wtke(k)=sq2pi*kvh(i,k,j)
!               wtke(k)=max(wtke(k),wmixmin)
        else
           wtke(k)=sq2pi*ekd(k)/dz(k)
        endif
        wtke(k)=max(wtke(k),wmixmin)
        nsource(i,k,j)=0.
     enddo
     nsource(i,kte+1,j) = 0.
     qndrop(kte+1)      = 0.
     zn(kte+1)          = 0.

     do k = kts+1, kte
        tmpa = dz(k-1) ; tmpb = dz(k)
        tmpc = tmpa/(tmpa + tmpb)
        csbot(k) = cs(k-1)*(1.0-tmpc) + cs(k)*tmpc
        csbot_cscen(k) = csbot(k)/cs(k)
     end do
     csbot(kts) = cs(kts)
     csbot_cscen(kts) = 1.0
     csbot(kte+1) = cs(kte)
     csbot_cscen(kte+1) = 1.0

    !  calculate surface rate and mass mixing ratio for aerosol

     surfratemax = 0.0
     nsav=1
     nnew=2
     surfrate_drop=depvel_drop/dz(kts)
     surfratemax = max( surfratemax, surfrate_drop )
     do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum>0)then
              depvel_tmp = max( 0.0, min( ddvel(i,j,lnum), depvel_uplimit ) )
              surfrate(lnum)=depvel_tmp/dz(kts)
              surfrate(lnumcw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lnum) )
!             scale = 1000./mwdry ! moles/kg
              scale = 1.
              raercol(kts:kte,lnumcw,nsav)=chem(i,kts:kte,j,lnumcw)*scale ! #/kg
              raercol(kts:kte,lnum,nsav)=chem(i,kts:kte,j,lnum)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
!             scale = mw_aer(l,n)/mwdry
              scale = 1.e-9 ! kg/ug
              depvel_tmp = max( 0.0, min( ddvel(i,j,lmass), depvel_uplimit ) )
              surfrate(lmass)=depvel_tmp/dz(kts)
              surfrate(lmasscw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lmass) )
              raercol(kts:kte,lmasscw,nsav)=chem(i,kts:kte,j,lmasscw)*scale ! kg/kg
              raercol(kts:kte,lmass,nsav)=chem(i,kts:kte,j,lmass)*scale ! kg/kg
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)then
              depvel_tmp = max( 0.0, min( ddvel(i,j,lwater), depvel_uplimit ) )
              surfrate(lwater)=depvel_tmp/dz(kts)
              surfratemax = max( surfratemax, surfrate(lwater) )
              raercol(kts:kte,lwater,nsav)=chem(i,kts:kte,j,lwater) ! don't bother to convert units,
             ! because it doesn't contribute to aerosol mass
           endif
        enddo ! size
     enddo ! type


! mass conservation checking
     if (icheck_colmass > 0) then

! calc initial column burdens
        colmass_bgn(:,:,:,:) = 0.0
        colmass_end(:,:,:,:) = 0.0
        colmass_sfc(:,:,:,:) = 0.0
        rhodz(kts:kte) = 1.0/zn(kts:kte)
        rhodzsum = sum( rhodz(kts:kte) )
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
              lnumcw=numptr_aer(m,n,cw_phase)
              if(lnum>0)then
                 colmass_bgn(0,m,n,1) = sum( chem(i,kts:kte,j,lnum  )*rhodz(kts:kte) )
                 colmass_bgn(0,m,n,2) = sum( chem(i,kts:kte,j,lnumcw)*rhodz(kts:kte) )
              endif
              do l=1,ncomp(n)
                 lmass=massptr_aer(l,m,n,ai_phase)
                 lmasscw=massptr_aer(l,m,n,cw_phase)
                 colmass_bgn(l,m,n,1) = sum( chem(i,kts:kte,j,lmass  )*rhodz(kts:kte) )
                 colmass_bgn(l,m,n,2) = sum( chem(i,kts:kte,j,lmasscw)*rhodz(kts:kte) )
              enddo
           enddo ! size
        enddo ! type
     endif ! (icheck_colmass > 0)


!        droplet nucleation/aerosol activation

! k-loop for growing/shrinking cloud calcs .............................
GROW_SHRINK_MAIN_K_LOOP: do k=kts,kte
        km1=max0(k-1,1)
        kp1=min0(k+1,kde-1)


!       if(lcldfra(k)-lcldfra_old(k).gt.0.01)then   ! this line is the "old" criterion
!       go to 10

!       growing cloud PLUS
!       upwards vertical advection when lcldfra(k-1) < lcldfra(k)
!
! tmpc1 = cloud fraction increase from previous time step
        tmpc1 = max( (lcldfra(k)-lcldfra_old(k)), 0.0 )
        if (k > kts) then
! tmpc2 = fraction of layer for which vertical advection from below
!             (over dtstep) displaces cloudy air with clear air
!       = (courant number using upwards w at layer bottom)*(difference in cloud fraction)
           tmpcourno = dtstep*max(w(i,k,j),0.0)/dz(k)
           tmpc2 = max( (lcldfra(k)-lcldfra(km1)), 0.0 ) * tmpcourno
           tmpc2 = min( tmpc2, 1.0 )
!          tmpc2 = 0.0   ! this turns off the vertical advect part
        else
           tmpc2 = 0.0
        endif

        if ((tmpc1 > 0.001) .or. (tmpc2 > 0.001)) then

!                wmix=wtke(k)
           wbar=w(i,k,j)+wtke(k)
           wmix=0.
           wmin=0.
! 06-nov-2005 rce - increase wmax from 10 to 50 (deep convective clouds)
           wmax=50.
           wdiab=0

!                load aerosol properties, assuming external mixtures
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 call loadaer(raercol(1,1,nsav),k,kms,kme,num_chem,    &
                      cs(k), npv(m,n), dlo_sect(m,n),dhi_sect(m,n),             &
                      maxd_acomp, ncomp(n), &
                      grid_id, ktau, i, j, m, n,   &
                      numptr_aer(m,n,ai_phase),numptr_aer(m,n,cw_phase),  &
                      dens_aer(1,n),    &
                      massptr_aer(1,m,n,ai_phase), massptr_aer(1,m,n,cw_phase),  &
                      maerosol(1,m,n), maerosolcw(1,m,n),          &
                      maerosol_tot(m,n), maerosol_totcw(m,n),      &
                      naerosol(m,n), naerosolcw(m,n),                  &
                      vaerosol(m,n), vaerosolcw(m,n) )

                 hygro_aer(m,n)=hygro(i,k,j,m,n)
              enddo
           enddo

! 06-nov-2005 rce - grid_id & ktau added to arg list
           call activate(wbar,wmix,wdiab,wmin,wmax,temp(i,k,j),cs(k), &
                msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                naerosol, vaerosol,  &
                dlo_sect,dhi_sect,sigmag_aer,hygro_aer,              &
                fn,fs,fm,fluxn,fluxs,fluxm,flux_fullact(k), grid_id, ktau, i, j, k )

           do n = 1,ntype_aer
              do m = 1,nsize_aer(n)
                 lnum   = numptr_aer(m,n,ai_phase)
                 lnumcw = numptr_aer(m,n,cw_phase)
                 if (tmpc1 > 0.0) then
                    dact = tmpc1*fn(m,n)*raercol(k,lnum,nsav) ! interstitial only
                 else
                    dact = 0.0
                 endif
                 if (tmpc2 > 0.0) then
                    dact = dact + tmpc2*fn(m,n)*raercol(km1,lnum,nsav) ! interstitial only
                 endif
                 dact = min( dact, 0.99*raercol(k,lnum,nsav) )
                 raercol(k,lnumcw,nsav) = raercol(k,lnumcw,nsav)+dact
                 raercol(k,lnum,  nsav) = raercol(k,lnum,  nsav)-dact
                 qndrop(k) = qndrop(k)+dact
                 nsource(i,k,j) = nsource(i,k,j)+dact*dtinv
                 do l = 1,ncomp(n)
                    lmass   = massptr_aer(l,m,n,ai_phase)
                    lmasscw = massptr_aer(l,m,n,cw_phase)
                    if (tmpc1 > 0.0) then
                       dact = tmpc1*fm(m,n)*raercol(k,lmass,nsav) ! interstitial only
                    else
                       dact = 0.0
                    endif
                    if (tmpc2 > 0.0) then
                       dact = dact + tmpc2*fm(m,n)*raercol(km1,lmass,nsav) ! interstitial only
                    endif
                    dact = min( dact, 0.99*raercol(k,lmass,nsav) )
                    raercol(k,lmasscw,nsav)  =  raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,  nsav)  =  raercol(k,lmass,  nsav)-dact
                 enddo
              enddo
           enddo
!   10 continue
        endif   ! ((tmpc1 > 0.001) .or. (tmpc2 > 0.001))


        if(lcldfra(k) < lcldfra_old(k) .and. lcldfra_old(k) > 1.e-20)then   ! this line is the "old" criterion
!         go to 20

!       shrinking cloud ......................................................

!                droplet loss in decaying cloud
           nsource(i,k,j)=nsource(i,k,j)+qndrop(k)*(lcldfra(k)-lcldfra_old(k))*dtinv
           qndrop(k)=qndrop(k)*(1.+lcldfra(k)-lcldfra_old(k))
!                 convert activated aerosol to interstitial in decaying cloud

           tmpc = (lcldfra(k)-lcldfra_old(k))/lcldfra_old(k)
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    dact=raercol(k,lnumcw,nsav)*tmpc
                    raercol(k,lnumcw,nsav)=raercol(k,lnumcw,nsav)+dact
                    raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    dact=raercol(k,lmasscw,nsav)*tmpc
                    raercol(k,lmasscw,nsav)=raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                 enddo
              enddo
           enddo
!             20 continue
        endif

     enddo GROW_SHRINK_MAIN_K_LOOP
! end of k-loop for growing/shrinking cloud calcs ......................



! ......................................................................
! start of main k-loop for calc of old cloud activation tendencies ..........
! this loop does "set up" for the nsubmix loop
!
! rce-comment
!    changed this part of code to use current cloud fraction (lcldfra) exclusively

OLD_CLOUD_MAIN_K_LOOP: do k=kts,kte
        km1=max0(k-1,kts)
        kp1=min0(k+1,kde-1)
        flux_fullact(k) = 0.0
        if(lcldfra(k).gt.0.01)then
!          go to 30

!               old cloud
              if(lcldfra(k)-lcldfra(km1).gt.0.01.or.k.eq.kts)then

!                   interior cloud
!                   cloud base

                 wdiab=0
                 wmix=wtke(k) ! spectrum of updrafts
                 wbar=w(i,k,j) ! spectrum of updrafts
!                    wmix=0. ! single updraft
!               wbar=wtke(k) ! single updraft
! 06-nov-2005 rce - increase wmax from 10 to 50 (deep convective clouds)
                 wmax=50.
                 ekd(k)=wtke(k)*dz(k)/sq2pi
                 alogarg=max(1.e-20,1/lcldfra(k)-1.)
                 wmin=wbar+wmix*0.25*sq2pi*alog(alogarg)

                 do n=1,ntype_aer
                    do m=1,nsize_aer(n)
                       call loadaer(raercol(1,1,nsav),km1,kms,kme,num_chem,    &
                            cs(k), npv(m,n),dlo_sect(m,n),dhi_sect(m,n),               &
                            maxd_acomp, ncomp(n), &
                            grid_id, ktau, i, j, m, n,   &
                            numptr_aer(m,n,ai_phase),numptr_aer(m,n,cw_phase),  &
                            dens_aer(1,n),   &
                            massptr_aer(1,m,n,ai_phase), massptr_aer(1,m,n,cw_phase),  &
                            maerosol(1,m,n), maerosolcw(1,m,n),          &
                            maerosol_tot(m,n), maerosol_totcw(m,n),      &
                            naerosol(m,n), naerosolcw(m,n),                  &
                            vaerosol(m,n), vaerosolcw(m,n) )

                       hygro_aer(m,n)=hygro(i,k,j,m,n)

                    enddo
                 enddo
!          print *,'old cloud wbar,wmix=',wbar,wmix

                 call activate(wbar,wmix,wdiab,wmin,wmax,temp(i,k,j),cs(k), &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      naerosol, vaerosol,  &
                      dlo_sect,dhi_sect, sigmag_aer,hygro_aer,                    &
                      fn,fs,fm,fluxn,fluxs,fluxm,flux_fullact(k), grid_id, ktau, i, j, k )
                 
! rce-comment
!    the activation-fraction fluxes (fluxn, fluxm) from subr activate assume that
!       wbar << wmix, which is valid for global-model scale but not mesoscale
!    for wrf-chem application, divide these by flux_fullact to get a 
!       "flux-weighted-average" activation fraction, then multiply by (ekd(k)*zs(k)) 
!       which is the local "turbulent vertical-mixing velocity"
                 if (k > kts) then
                    if (flux_fullact(k) > 1.0e-20) then
                       tmpa = ekd(k)*zs(k)
                       tmpf = flux_fullact(k)
                       do n=1,ntype_aer
                       do m=1,nsize_aer(n)
                          tmpb = max( fluxn(m,n), 0.0 ) / max( fluxn(m,n), tmpf )
                          fluxn(m,n) = tmpa*tmpb
                          tmpb = max( fluxm(m,n), 0.0 ) / max( fluxm(m,n), tmpf )
                          fluxm(m,n) = tmpa*tmpb
                       enddo
                       enddo
                    else
                       fluxn(:,:) = 0.0
                       fluxm(:,:) = 0.0
                    endif
                 endif

                 if(k.gt.kts)then
                    tmpc = lcldfra(k)-lcldfra(km1)
                 else
                    tmpc=lcldfra(k)
                 endif
! rce-comment
!    flux of activated mass into layer k (in kg/m2/s)
!       = "actmassflux" = dumc*fluxm*raercol(kp1,lmass)*csbot(k)
!    source of activated mass (in kg/kg/s) = flux divergence
!       = actmassflux/(cs(i,k)*dz(i,k))
!    so need factor of csbot_cscen = csbot(k)/cs(i,k)
!                tmpe=1./(dz(k))
                 tmpe = csbot_cscen(k)/(dz(k))
                 fluxntot=0.
                 do n=1,ntype_aer
                 do m=1,nsize_aer(n)
                    fluxn(m,n)=fluxn(m,n)*tmpc
!                   fluxs(m,n)=fluxs(m,n)*tmpc
                    fluxm(m,n)=fluxm(m,n)*tmpc
                    lnum=numptr_aer(m,n,ai_phase)
                    fluxntot=fluxntot+fluxn(m,n)*raercol(km1,lnum,nsav)
!             print *,'fn=',fn(m,n),' for m,n=',m,n
!             print *,'old cloud tmpc=',tmpc,' fn=',fn(m,n),' for m,n=',m,n
                    nact(k,m,n)=nact(k,m,n)+fluxn(m,n)*tmpe
                    mact(k,m,n)=mact(k,m,n)+fluxm(m,n)*tmpe
                 enddo
                 enddo
                 flux_fullact(k) = flux_fullact(k)*tmpe
                 nsource(i,k,j)=nsource(i,k,j)+fluxntot*zs(k)
                 fluxntot=fluxntot*cs(k)
              endif
!       30 continue

        else
!       go to 40
!              no cloud
           if(qndrop(k).gt.10000.e6)then
              print *,'i,k,j,lcldfra,qndrop=',i,k,j,lcldfra(k),qndrop(k)
              print *,'cldfra,ql,qi',cldfra(i,k,j),qc(i,k,j),qi(i,k,j)
           endif
           nsource(i,k,j)=nsource(i,k,j)-qndrop(k)*dtinv
           qndrop(k)=0.
!              convert activated aerosol to interstitial in decaying cloud
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    raercol(k,lnum,nsav)=raercol(k,lnum,nsav)+raercol(k,lnumcw,nsav)
                    raercol(k,lnumcw,nsav)=0.
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    raercol(k,lmass,nsav)=raercol(k,lmass,nsav)+raercol(k,lmasscw,nsav)
                    raercol(k,lmasscw,nsav)=0.
                 enddo
              enddo
           enddo
!      40 continue
        endif

     enddo OLD_CLOUD_MAIN_K_LOOP

!    cycle OVERALL_MAIN_I_LOOP


!    switch nsav, nnew so that nnew is the updated aerosol

     ntemp=nsav
     nsav=nnew
     nnew=ntemp

!    load new droplets in layers above, below clouds

     dtmin=dtstep
     ekk(kts)=0.0
! rce-comment -- ekd(k) is eddy-diffusivity at k/k-1 interface
!   want ekk(k) = ekd(k) * (density at k/k-1 interface)
     do k=kts+1,kte
        ekk(k)=ekd(k)*csbot(k)
     enddo
     ekk(kte+1)=0.0
     do k=kts,kte
        ekkp(k)=zn(k)*ekk(k+1)*zs(k+1)
        ekkm(k)=zn(k)*ekk(k)*zs(k)
        tinv=ekkp(k)+ekkm(k)
        if(k.eq.kts)tinv=tinv+surfratemax
        if(tinv.gt.1.e-6)then
           dtt=1./tinv
           dtmin=min(dtmin,dtt)
        endif
     enddo
     dtmix=0.9*dtmin
     nsubmix=dtstep/dtmix+1
     if(nsubmix>100)then
        nsubmix_bnd=100
     else
        nsubmix_bnd=nsubmix
     endif
!     count_submix(nsubmix_bnd)=count_submix(nsubmix_bnd)+1
     dtmix=dtstep/nsubmix
     fac_srflx = -1.0/(zn(1)*nsubmix)
     
     do k=kts,kte
        kp1=min(k+1,kde-1)
        km1=max(k-1,1)
        if(lcldfra(kp1).gt.0)then
           overlapp(k)=min(lcldfra(k)/lcldfra(kp1),1.)
        else
           overlapp(k)=1.
        endif
        if(lcldfra(km1).gt.0)then
           overlapm(k)=min(lcldfra(k)/lcldfra(km1),1.)
        else
           overlapm(k)=1.
        endif
     enddo



! ......................................................................
! start of nsubmix-loop for calc of old cloud activation tendencies ....
OLD_CLOUD_NSUBMIX_LOOP: do nsub=1,nsubmix
        qndrop_new(kts:kte)=qndrop(kts:kte)
!           switch nsav, nnew so that nsav is the updated aerosol
        ntemp=nsav
        nsav=nnew
        nnew=ntemp
        srcn(:)=0.0
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
!              update droplet source
! rce-comment - activation source in layer k involves particles from k-1
!             srcn(kts  :kte)=srcn(kts  :kte)+nact(kts  :kte,m,n)*(raercol(kts:kte  ,lnum,nsav))
              srcn(kts+1:kte)=srcn(kts+1:kte)+nact(kts+1:kte,m,n)*(raercol(kts:kte-1,lnum,nsav))
! rce-comment - new formulation for k=kts should be implemented
              srcn(kts      )=srcn(kts      )+nact(kts      ,m,n)*(raercol(kts      ,lnum,nsav))
           enddo
        enddo
        call explmix(qndrop,srcn,ekkp,ekkm,overlapp,overlapm,   &
             qndrop_new,surfrate_drop,kms,kme,kts,kte,dtmix,.false.)
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
              lnumcw=numptr_aer(m,n,cw_phase)
              if(lnum>0)then
! rce-comment - activation source in layer k involves particles from k-1
!                source(kts  :kte)= nact(kts  :kte,m,n)*(raercol(kts:kte  ,lnum,nsav))
                 source(kts+1:kte)= nact(kts+1:kte,m,n)*(raercol(kts:kte-1,lnum,nsav))
! rce-comment - new formulation for k=kts should be implemented
                 source(kts      )= nact(kts      ,m,n)*(raercol(kts      ,lnum,nsav))
                 call explmix(raercol(1,lnumcw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lnumcw,nsav),surfrate(lnumcw),kms,kme,kts,kte,dtmix,&
                      .false.)
                 call explmix(raercol(1,lnum,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lnum,nsav),surfrate(lnum),kms,kme,kts,kte,dtmix, &
                      .true.,raercol(1,lnumcw,nsav))
                 qsrflx(i,j,lnum) = qsrflx(i,j,lnum) + fac_srflx*            &
                      raercol(kts,lnum,nsav)*surfrate(lnum)
                 qsrflx(i,j,lnumcw) = qsrflx(i,j,lnumcw) + fac_srflx*        &
                      raercol(kts,lnumcw,nsav)*surfrate(lnumcw)
                 if (icheck_colmass > 0) then
                    tmpf = dtmix*rhodz(kts)
                    colmass_sfc(0,m,n,1) = colmass_sfc(0,m,n,1) &
                          + raercol(kts,lnum  ,nsav)*surfrate(lnum  )*tmpf
                    colmass_sfc(0,m,n,2) = colmass_sfc(0,m,n,2) &
                          + raercol(kts,lnumcw,nsav)*surfrate(lnumcw)*tmpf
                 endif
              endif
              do l=1,ncomp(n)
                 lmass=massptr_aer(l,m,n,ai_phase)
                 lmasscw=massptr_aer(l,m,n,cw_phase)
! rce-comment - activation source in layer k involves particles from k-1
!                source(kts  :kte)= mact(kts  :kte,m,n)*(raercol(kts:kte  ,lmass,nsav))
                 source(kts+1:kte)= mact(kts+1:kte,m,n)*(raercol(kts:kte-1,lmass,nsav))
! rce-comment - new formulation for k=kts should be implemented
                 source(kts      )= mact(kts      ,m,n)*(raercol(kts      ,lmass,nsav))
                 call explmix(raercol(1,lmasscw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lmasscw,nsav),surfrate(lmasscw),kms,kme,kts,kte,dtmix,  &
                      .false.)
                 call explmix(raercol(1,lmass,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lmass,nsav),surfrate(lmass),kms,kme,kts,kte,dtmix,  &
                      .true.,raercol(1,lmasscw,nsav))
                 qsrflx(i,j,lmass) = qsrflx(i,j,lmass) + fac_srflx*          &
                      raercol(kts,lmass,nsav)*surfrate(lmass)
                 qsrflx(i,j,lmasscw) = qsrflx(i,j,lmasscw) + fac_srflx*      &
                      raercol(kts,lmasscw,nsav)*surfrate(lmasscw)
                 if (icheck_colmass > 0) then
                    ! colmass_sfc calculation
                    !    colmass_bgn/end = bgn/end column burden = sum.over.k.of{ rho(k)*dz(k)*chem(k,l) }
                    !    colmass_sfc = surface loss over dtstep
                    !       = sum.over.nsubmix.substeps{ depvel(l)*rho(kts)*chem(kts,l)*dtmix }
                    !    surfrate(l) = depvel(l)/dz(kts) so need to multiply by dz(kts)
                    !    for mass, raercol(k,l) = chem(k,l)*1.0e-9, so need to multiply by 1.0e9
                    tmpf = dtmix*rhodz(kts)*1.0e9
                    colmass_sfc(l,m,n,1) = colmass_sfc(l,m,n,1) &
                          + raercol(kts,lmass  ,nsav)*surfrate(lmass  )*tmpf
                    colmass_sfc(l,m,n,2) = colmass_sfc(l,m,n,2) &
                          + raercol(kts,lmasscw,nsav)*surfrate(lmasscw)*tmpf
                 endif
              enddo
              lwater=waterptr_aer(m,n)  ! aerosol water
              if(lwater>0)then
                 source(:)=0.
                 call explmix(   raercol(1,lwater,nnew),source,ekkp,ekkm,overlapp,overlapm,   &
                      raercol(1,lwater,nsav),surfrate(lwater),kms,kme,kts,kte,dtmix,  &
                      .true.,source)
              endif
           enddo ! size
        enddo ! type

     enddo OLD_CLOUD_NSUBMIX_LOOP

!    cycle OVERALL_MAIN_I_LOOP

!        evaporate particles again if no cloud

     do k=kts,kte
        if(lcldfra(k).eq.0.)then

!              no cloud

           qndrop(k)=0.
!              convert activated aerosol to interstitial in decaying cloud
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    raercol(k,lnum,nnew)=raercol(k,lnum,nnew)+raercol(k,lnumcw,nnew)
                    raercol(k,lnumcw,nnew)=0.
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    raercol(k,lmass,nnew)=raercol(k,lmass,nnew)+raercol(k,lmasscw,nnew)
                    raercol(k,lmasscw,nnew)=0.
                 enddo
              enddo
           enddo
        endif
     enddo

!         cycle OVERALL_MAIN_I_LOOP

!        droplet number

     do k=kts,kte
!       if(lcldfra(k).gt.0.1)then
!           write(6,'(a,3i5,f12.1)')'i,j,k,qndrop=',i,j,k,qndrop(k)
!       endif
        if(qndrop(k).lt.-10.e6.or.qndrop(k).gt.1.e12)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop=',qndrop(k),' for i,k,j=',i,k,j
        endif

        qndrop3d(i,k,j) = max(qndrop(k),1.e-6)

        if(qndrop3d(i,k,j).lt.-10.e6.or.qndrop3d(i,k,j).gt.1.E20)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop3d=',qndrop3d(i,k,j),' for i,k,j=',i,k,j
        endif
        if(qc(i,k,j).lt.-1..or.qc(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qc=',qc(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal("1")
        endif
        if(qi(i,k,j).lt.-1..or.qi(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qi=',qi(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal("1")
        endif
        if(qv(i,k,j).lt.-1..or.qv(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qv=',qv(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal("1")
        endif
        cldfra_old(i,k,j) = cldfra(i,k,j)
!       if(k.gt.6.and.k.lt.11)cldfra_old(i,k,j)=1.
     enddo

!    cycle OVERALL_MAIN_I_LOOP

!        update chem and convert back to mole/mole

     ccn(:,:) = 0.
     do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum.gt.0)then
              !          scale=mwdry*0.001
              scale = 1.
              chem(i,kts:kte,j,lnumcw)= raercol(kts:kte,lnumcw,nnew)*scale
              chem(i,kts:kte,j,lnum)= raercol(kts:kte,lnum,nnew)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
!          scale = mwdry/mw_aer(l,n)
              scale = 1.e9
              chem(i,kts:kte,j,lmasscw)=raercol(kts:kte,lmasscw,nnew)*scale ! ug/kg
              chem(i,kts:kte,j,lmass)=raercol(kts:kte,lmass,nnew)*scale ! ug/kg
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)chem(i,kts:kte,j,lwater)=raercol(kts:kte,lwater,nnew) ! don't convert units
           do k=kts,kte
              sm=2.*aten*sqrt(aten/(27.*hygro(i,k,j,m,n)*amcube(m,n)))
              do l=1,psat
                 arg=argfactor(m,n)*log(sm/super(l))
                 if(arg<2)then
                    if(arg<-2)then
                       ccnfact(l,m,n)=1.e-6 ! convert from #/m3 to #/cm3
                    else
                       ccnfact(l,m,n)=1.e-6*0.5*ERFC_NUM_RECIPES(arg)
                    endif
                 else
                    ccnfact(l,m,n) = 0.
                 endif
!                 ccn concentration as diagnostic
!                 assume same hygroscopicity and ccnfact for cloud-phase and aerosol phase particles
                 ccn(k,l)=ccn(k,l)+(raercol(k,lnum,nnew)+raercol(k,lnumcw,nnew))*cs(k)*ccnfact(l,m,n)
              enddo
           enddo
        enddo
     enddo
     do l=1,psat
        !wig, 22-Nov-2006: added vertical bounds to prevent out-of-bounds at top
        if(l.eq.1)ccn1(i,kts:kte,j)=ccn(:,l)
        if(l.eq.2)ccn2(i,kts:kte,j)=ccn(:,l)
        if(l.eq.3)ccn3(i,kts:kte,j)=ccn(:,l)
        if(l.eq.4)ccn4(i,kts:kte,j)=ccn(:,l)
        if(l.eq.5)ccn5(i,kts:kte,j)=ccn(:,l)
        if(l.eq.6)ccn6(i,kts:kte,j)=ccn(:,l)
     end do

! mass conservation checking
     if (icheck_colmass > 0) then
! calc final column burdens
        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum>0)then
              colmass_end(0,m,n,1) = sum( chem(i,kts:kte,j,lnum  )*rhodz(kts:kte) )
              colmass_end(0,m,n,2) = sum( chem(i,kts:kte,j,lnumcw)*rhodz(kts:kte) )
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
              colmass_end(l,m,n,1) = sum( chem(i,kts:kte,j,lmass  )*rhodz(kts:kte) )
              colmass_end(l,m,n,2) = sum( chem(i,kts:kte,j,lmasscw)*rhodz(kts:kte) )
           enddo
        enddo ! size
        enddo ! type
! calc burden change errors for each interstitial/activated pair
        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           do l=0,ncomp(n)
              ! tmpa & tmpb = beginning & ending column burden divided by rhodzsum,
              !             = beginning & ending column-mean mixing ratios
              ! tmpc = loss to surface divided by rhodzsum,
              tmpa = ( colmass_bgn(l,m,n,1) + colmass_bgn(l,m,n,2) )/rhodzsum
              tmpb = ( colmass_end(l,m,n,1) + colmass_end(l,m,n,2) )/rhodzsum
              tmpc = ( colmass_sfc(l,m,n,1) + colmass_sfc(l,m,n,2) )/rhodzsum

              ! tmpd = ((final burden) + (sfc loss)) - (initial burden)
              !      = burden change error
              tmpd = (tmpb + tmpc) - tmpa
              tmpe = max( tmpa, 1.0e-20 )

              ! tmpf = (burden change error) / (initial burden)
              if (abs(tmpd) < 1.0e5*tmpe) then
                 tmpf = tmpd/tmpe
              else if (tmpf < 0.0) then
                 tmpf = -1.0e5
              else
                 tmpf = 1.0e5
              end if
              if (abs(tmpf) > abs(colmass_worst(l,m,n))) then
                 colmass_worst(l,m,n) = tmpf
                 colmass_worst_ij(1,l,m,n) = i
                 colmass_worst_ij(2,l,m,n) = j
              endif
           enddo
        enddo ! size
        enddo ! type
     endif ! (icheck_colmass > 0)


     enddo OVERALL_MAIN_I_LOOP ! end of main loop over i
     enddo OVERALL_MAIN_J_LOOP ! end of main loop over j


! mass conservation checking
     if (icheck_colmass > 0) then
        if (icheck_colmass >= 100) write(*,'(a)') &
             'mixactivate colmass worst errors bgn - type, size, comp, err, i, j'
        colmass_maxworst_r = 0.0
        colmass_maxworst_i(:) = -1
        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           do l=0,ncomp(n)
              if (icheck_colmass >= 100) &
                 write(*,'(3i3,1p,e10.2,2i4)') n, m, l, &
                 colmass_worst(l,m,n), colmass_worst_ij(1:2,l,m,n) 
              if (abs(colmass_worst(l,m,n)) > abs(colmass_maxworst_r)) then
                 colmass_maxworst_r = colmass_worst(l,m,n) 
                 colmass_maxworst_i(1) = n
                 colmass_maxworst_i(2) = m
                 colmass_maxworst_i(3) = l
              end if
           enddo
        enddo ! size
        enddo ! type
        if ((icheck_colmass >= 10) .or. (abs(colmass_maxworst_r) >= 1.0e-6)) &
             write(*,'(a,3i3,1p,e10.2)') 'mixactivate colmass maxworst', &
             colmass_maxworst_i(1:3), colmass_maxworst_r
     endif ! (icheck_colmass > 0)


     return
   end subroutine mixactivate


!----------------------------------------------------------------------
!----------------------------------------------------------------------
   subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
                       qold, surfrate, kms, kme, kts, kte, dt, &
                       is_unact, qactold )

!  explicit integration of droplet/aerosol mixing
!     with source due to activation/nucleation


   implicit none
   integer, intent(in) :: kms,kme ! number of levels for array definition
   integer, intent(in) :: kts,kte ! number of levels for looping
   real, intent(inout) :: q(kms:kme) ! mixing ratio to be updated
   real, intent(in) :: qold(kms:kme) ! mixing ratio from previous time step
   real, intent(in) :: src(kms:kme) ! source due to activation/nucleation (/s)
   real, intent(in) :: ekkp(kms:kme) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! below layer k  (k,k+1 interface)
   real, intent(in) :: ekkm(kms:kme) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! above layer k  (k,k+1 interface)
   real, intent(in) :: overlapp(kms:kme) ! cloud overlap below
   real, intent(in) :: overlapm(kms:kme) ! cloud overlap above
   real, intent(in) :: surfrate ! surface exchange rate (/s)
   real, intent(in) :: dt ! time step (s)
   logical, intent(in) :: is_unact ! true if this is an unactivated species
   real, intent(in),optional :: qactold(kms:kme)
          ! mixing ratio of ACTIVATED species from previous step
          ! *** this should only be present
          !     if the current species is unactivated number/sfc/mass

   integer k,kp1,km1

   if ( is_unact ) then
!     the qactold*(1-overlap) terms are resuspension of activated material
      do k=kts,kte
         kp1=min(k+1,kte)
         km1=max(k-1,kts)
         q(k) = qold(k) + dt*( - src(k) + ekkp(k)*(qold(kp1) - qold(k) +  &
                           qactold(kp1)*(1.0-overlapp(k)))               &
                                  + ekkm(k)*(qold(km1) - qold(k) +     &
                           qactold(km1)*(1.0-overlapm(k))) )
!         if(q(k)<-1.e-30)then ! force to non-negative
!            print *,'q=',q(k),' in explmix'
             q(k)=max(q(k),0.)
!         endif
      end do

   else
      do k=kts,kte
         kp1=min(k+1,kte)
         km1=max(k-1,kts)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +  &
                                    ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )
!        if(q(k)<-1.e-30)then ! force to non-negative
!           print *,'q=',q(k),' in explmix'
            q(k)=max(q(k),0.)
!        endif
      end do
   end if

!  dry deposition loss at base of lowest layer
   q(kts)=q(kts)-surfrate*qold(kts)*dt
!  if(q(kts)<-1.e-30)then ! force to non-negative
!     print *,'q=',q(kts),' in explmix'
      q(kts)=max(q(kts),0.)
!  endif

   return
   end subroutine explmix

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! 06-nov-2005 rce - grid_id & ktau added to arg list
      subroutine activate(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      na, volc, dlo_sect,dhi_sect,sigman, hygro, &
                      fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
                      grid_id, ktau, ii, jj, kk,smax_prescribed )!BSINGH - Added smax_prescribed for WRFCuP

!      calculates number, surface, and mass fraction of aerosols activated as CCN
!      calculates flux of cloud droplets, surface area, and aerosol mass into cloud
!      assumes an internal mixture within each of aerosol mode.
!      A sectional treatment within each type is assumed if ntype_aer >7.
!      A gaussiam spectrum of updrafts can be treated.

!      mks units

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      USE module_model_constants, only: g,rhowater, xlv, cp, rvovrd, r_d, r_v, &
              mwdry,svp1,svp2,svp3,ep_2

      implicit none


!      input

      integer,intent(in) :: maxd_atype      ! dimension of types
      integer,intent(in) :: maxd_asize      ! dimension of sizes
      integer,intent(in) :: ntype_aer       ! number of types
      integer,intent(in) :: nsize_aer(maxd_atype) ! number of sizes for type
      integer,intent(in) :: msectional      ! 1 for sectional, 0 for modal
      integer,intent(in) :: grid_id         ! WRF grid%id
      integer,intent(in) :: ktau            ! WRF time step count
      integer,intent(in) :: ii, jj, kk      ! i,j,k of current grid cell
      real,intent(in) :: wbar          ! grid cell mean vertical velocity (m/s)
      real,intent(in) :: sigw          ! subgrid standard deviation of vertical vel (m/s)
      real,intent(in) :: wdiab         ! diabatic vertical velocity (0 if adiabatic)
      real,intent(in) :: wminf         ! minimum updraft velocity for integration (m/s)
      real,intent(in) :: wmaxf         ! maximum updraft velocity for integration (m/s)
      real,intent(in) :: tair          ! air temperature (K)
      real,intent(in) :: rhoair        ! air density (kg/m3)
      real,intent(in) :: na(maxd_asize,maxd_atype)     ! aerosol number concentration (/m3)
      real,intent(in) :: sigman(maxd_asize,maxd_atype) ! geometric standard deviation of aerosol size distribution
      real,intent(in) :: hygro(maxd_asize,maxd_atype)  ! bulk hygroscopicity of aerosol mode
      real,intent(in) :: volc(maxd_asize,maxd_atype)   ! total aerosol volume  concentration (m3/m3)
      real,intent(in) :: dlo_sect( maxd_asize, maxd_atype ), &  ! minimum size of section (cm)
           dhi_sect( maxd_asize, maxd_atype )     ! maximum size of section (cm)
      real,intent(in),optional :: smax_prescribed  ! prescribed max. supersaturation for secondary activation !BSINGH - Added for WRFCuP

!      output

      real,intent(inout) :: fn(maxd_asize,maxd_atype)    ! number fraction of aerosols activated
      real,intent(inout) :: fs(maxd_asize,maxd_atype)    ! surface fraction of aerosols activated
      real,intent(inout) :: fm(maxd_asize,maxd_atype)    ! mass fraction of aerosols activated
      real,intent(inout) :: fluxn(maxd_asize,maxd_atype) ! flux of activated aerosol number fraction into cloud (m/s)
      real,intent(inout) :: fluxs(maxd_asize,maxd_atype) ! flux of activated aerosol surface fraction (m/s)
      real,intent(inout) :: fluxm(maxd_asize,maxd_atype) ! flux of activated aerosol mass fraction into cloud (m/s)
      real,intent(inout) :: flux_fullact                 ! flux when activation fraction = 100% (m/s)

!      local

!!$      external erf,erfc
!!$      real erf,erfc
!      external qsat_water
      integer, parameter:: nx=200
      integer iquasisect_option, isectional
      real integ,integf
      real, parameter :: surften = 0.076 ! surface tension of water w/respect to air (N/m)
      real, parameter :: p0 = 1013.25e2  ! reference pressure (Pa)
      real, parameter :: t0 = 273.15     ! reference temperature (K)
      real ylo(maxd_asize,maxd_atype),yhi(maxd_asize,maxd_atype) ! 1-particle volume at section interfaces
      real ymean(maxd_asize,maxd_atype) ! 1-particle volume at r=rmean
      real ycut, lnycut, betayy, betayy2, gammayy, phiyy
      real surfc(maxd_asize,maxd_atype) ! surface concentration (m2/m3)
      real sign(maxd_asize,maxd_atype)    ! geometric standard deviation of size distribution
      real alnsign(maxd_asize,maxd_atype) ! natl log of geometric standard dev of aerosol
      real am(maxd_asize,maxd_atype) ! number mode radius of dry aerosol (m)
      real lnhygro(maxd_asize,maxd_atype) ! ln(b)
      real f1(maxd_asize,maxd_atype) ! array to hold parameter for maxsat
      real pres ! pressure (Pa)
      real path ! mean free path (m)
      real diff ! diffusivity (m2/s)
      real conduct ! thermal conductivity (Joule/m/sec/deg)
      real diff0,conduct0
      real es ! saturation vapor pressure
      real qs ! water vapor saturation mixing ratio
      real dqsdt ! change in qs with temperature
      real dqsdp ! change in qs with pressure
      real gg ! thermodynamic function (m2/s)
      real sqrtg ! sqrt(gg)
      real sm(maxd_asize,maxd_atype) ! critical supersaturation for number mode radius
      real lnsm(maxd_asize,maxd_atype) ! ln( sm )
      real zeta, eta(maxd_asize,maxd_atype)
      real lnsmax ! ln(smax)
      real alpha
      real gamma
      real beta
      real gaus
      logical :: top        ! true if cloud top, false if cloud base or new cloud
      real asub(maxd_asize,maxd_atype),bsub(maxd_asize,maxd_atype) ! coefficients of submode size distribution N=a+bx
      real totn(maxd_atype) ! total aerosol number concentration
      real aten ! surface tension parameter
      real gmrad(maxd_atype) ! geometric mean radius
      real gmradsq(maxd_atype) ! geometric mean of radius squared
      real gmlnsig(maxd_atype) ! geometric standard deviation
      real gmsm(maxd_atype) ! critical supersaturation at radius gmrad
      real sumflxn(maxd_asize,maxd_atype)
      real sumflxs(maxd_asize,maxd_atype)
      real sumflxm(maxd_asize,maxd_atype)
      real sumflx_fullact
      real sumfn(maxd_asize,maxd_atype)
      real sumfs(maxd_asize,maxd_atype)
      real sumfm(maxd_asize,maxd_atype)
      real sumns(maxd_atype)
      real fnold(maxd_asize,maxd_atype)   ! number fraction activated
      real fsold(maxd_asize,maxd_atype)   ! surface fraction activated
      real fmold(maxd_asize,maxd_atype)   ! mass fraction activated
      real wold,gold
      real alogten,alog2,alog3,alogaten
      real alogam
      real rlo(maxd_asize,maxd_atype), rhi(maxd_asize,maxd_atype)
      real rmean(maxd_asize,maxd_atype)
                  ! mean radius (m) for the section (not used with modal)
                  ! calculated from current volume & number
      real ccc
      real dumaa,dumbb
      real wmin,wmax,w,dw,dwmax,dwmin,wnuc,dwnew,wb
      real dfmin,dfmax,fnew,fold,fnmin,fnbar,fsbar,fmbar
      real alw,sqrtalw
      real smax
      real x,arg
      real xmincoeff,xcut
      real z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real etafactor1,etafactor2(maxd_asize,maxd_atype),etafactor2max
      integer m,n,nw,nwmax

!      numerical integration parameters
      real, parameter :: eps = 0.3
      real, parameter :: fmax = 0.99
      real, parameter :: sds = 3.

!      mathematical constants
      real third, twothird, sixth, zero, one, two, three

      real, parameter :: sq2  = 1.4142135624
      real, parameter :: sqpi = 1.7724538509
      real, parameter :: pi   = 3.1415926536

!      integer, save :: ndist(nx)  ! accumulates frequency distribution of integration bins required
!      data ndist/nx*0/

!     for nsize_aer>7, a sectional approach is used and isectional = iquasisect_option
!     activation fractions (fn,fs,fm) are computed as follows
!     iquasisect_option = 1,3 - each section treated as a narrow lognormal
!     iquasisect_option = 2,4 - within-section dn/dx = a + b*x,  x = ln(r)
!     smax is computed as follows (when explicit activation is OFF)
!     iquasisect_option = 1,2 - razzak-ghan modal parameterization with
!     single mode having same ntot, dgnum, sigmag as the combined sections
!     iquasisect_option = 3,4 - razzak-ghan sectional parameterization
!     for nsize_aer=<9, a modal approach is used and isectional = 0

! rce 08-jul-2005
! if either (na(n,m) < nsmall) or (volc(n,m) < vsmall)
! then treat bin/mode (n,m) as being empty, and set its fn/fs/fm=0.0
!     (for single precision, gradual underflow starts around 1.0e-38,
!      and strange things can happen when in that region)
      real, parameter :: nsmall = 1.0e-20    ! aer number conc in #/m3
      real, parameter :: vsmall = 1.0e-37    ! aer volume conc in m3/m3
      logical bin_is_empty(maxd_asize,maxd_atype), all_bins_empty
      logical bin_is_narrow(maxd_asize,maxd_atype)

      integer idiagaa, ipass_nwloop
      integer idiag_dndy_neg, idiag_fnsm_prob

! The flag for cloud top is no longer used so set it to false. This is an
! antiquated feature related to radiation enhancing mass fluxes at cloud
! top. It is currently, as of Feb. 2009, set to false in the CAM version
! as well.
      top = .false.

!.......................................................................
!
!   start calc. of modal or sectional activation properties (start of section 1)
!
!.......................................................................
      idiag_dndy_neg = 1      ! set this to 0 to turn off 
                              !     warnings about dn/dy < 0
      idiag_fnsm_prob = 1     ! set this to 0 to turn off 
                              !     warnings about fn/fs/fm misbehavior

      iquasisect_option = 2
      if(msectional.gt.0)then
         isectional = iquasisect_option
      else
         isectional = 0
      endif
      !BSINGH - For WRFCuP
      if ( present( smax_prescribed ) ) then
         if (smax_prescribed <= 0.0) then
            do n=1,ntype_aer
               do m=1,nsize_aer(n)
                  fluxn(m,n)=0.
                  fn(m,n)=0.
                  fluxs(m,n)=0.
                  fs(m,n)=0.
                  fluxm(m,n)=0.
                  fm(m,n)=0.
               end do
            end do
            flux_fullact=0.
            return
         end if
      end if
      
      !BSINGH - ENDS


      do n=1,ntype_aer
!         print *,'ntype_aer,n,nsize_aer(n)=',ntype_aer,n,nsize_aer(n)

        if(ntype_aer.eq.1.and.nsize_aer(n).eq.1.and.na(1,1).lt.1.e-20)then
         fn(1,1)=0.
         fs(1,1)=0.
         fm(1,1)=0.
         fluxn(1,1)=0.
         fluxs(1,1)=0.
         fluxm(1,1)=0.
         flux_fullact=0.
         return
        endif
      enddo

      zero = 0.0
      one = 1.0
      two = 2.0
      three = 3.0
      third = 1.0/3.0
      twothird = 2.0/3.0 !wig, 1-Mar-2009: Corrected value from 2/6
      sixth = 1.0/6.0

      pres=r_d*rhoair*tair
      diff0=0.211e-4*(p0/pres)*(tair/t0)**1.94
      conduct0=(5.69+0.017*(tair-t0))*4.186e2*1.e-5 ! convert to J/m/s/deg
      es=1000.*svp1*exp( svp2*(tair-t0)/(tair-svp3) )
      qs=ep_2*es/(pres-es)
      dqsdt=xlv/(r_v*tair*tair)*qs
      alpha=g*(xlv/(cp*r_v*tair*tair)-1./(r_d*tair))
      gamma=(1+xlv/cp*dqsdt)/(rhoair*qs)
      gg=1./(rhowater/(diff0*rhoair*qs)+xlv*rhowater/(conduct0*tair)*(xlv/(r_v*tair)-1.))
      sqrtg=sqrt(gg)
      beta=4.*pi*rhowater*gg*gamma
      aten=2.*surften/(r_v*tair*rhowater)
      alogaten=log(aten)
      alog2=log(two)
      alog3=log(three)
      ccc=4.*pi*third
      etafactor2max=1.e10/(alpha*wmaxf)**1.5 ! this should make eta big if na is very small.

      all_bins_empty = .true.
      do n=1,ntype_aer
      totn(n)=0.
      gmrad(n)=0.
      gmradsq(n)=0.
      sumns(n)=0.
      do m=1,nsize_aer(n)
         alnsign(m,n)=log(sigman(m,n))
!         internal mixture of aerosols

         bin_is_empty(m,n) = .true.
         if (volc(m,n).gt.vsmall .and. na(m,n).gt.nsmall) then
            bin_is_empty(m,n) = .false.
            all_bins_empty = .false.
            lnhygro(m,n)=log(hygro(m,n))
!            number mode radius (m,n)
!           write(6,*)'alnsign,volc,na=',alnsign(m,n),volc(m,n),na(m,n)
            am(m,n)=exp(-1.5*alnsign(m,n)*alnsign(m,n))*              &
              (3.*volc(m,n)/(4.*pi*na(m,n)))**third

            if (isectional .gt. 0) then
!               sectional model.
!               need to use bulk properties because parameterization doesn't
!               work well for narrow bins.
               totn(n)=totn(n)+na(m,n)
               alogam=log(am(m,n))
               gmrad(n)=gmrad(n)+na(m,n)*alogam
               gmradsq(n)=gmradsq(n)+na(m,n)*alogam*alogam
            endif
            etafactor2(m,n)=1./(na(m,n)*beta*sqrtg)

            if(hygro(m,n).gt.1.e-10)then
               sm(m,n)=2.*aten/(3.*am(m,n))*sqrt(aten/(3.*hygro(m,n)*am(m,n)))
            else
               sm(m,n)=100.
            endif
!           write(6,*)'sm,hygro,am=',sm(m,n),hygro(m,n),am(m,n)
         else
            sm(m,n)=1.
            etafactor2(m,n)=etafactor2max ! this should make eta big if na is very small.

         endif
         lnsm(m,n)=log(sm(m,n))
         if ((isectional .eq. 3) .or. (isectional .eq. 4)) then
            sumns(n)=sumns(n)+na(m,n)/sm(m,n)**twothird
         endif
!        write(6,'(a,i4,6g12.2)')'m,na,am,hygro,lnhygro,sm,lnsm=',m,na(m,n),am(m,n),hygro(m,n),lnhygro(m,n),sm(m,n),lnsm(m,n)
      end do ! size
      end do ! type

!  if all bins are empty, set all activation fractions to zero and exit
         if ( all_bins_empty ) then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fluxn(m,n)=0.
               fn(m,n)=0.
               fluxs(m,n)=0.
               fs(m,n)=0.
               fluxm(m,n)=0.
               fm(m,n)=0.
            end do
            end do
            flux_fullact=0.
            return
         endif



         if (isectional .le. 0) then
            ! Initialize maxsat at this cell and timestep for the
            ! modal setup (the sectional case is handled below).
            call maxsat_init(maxd_atype, ntype_aer, &
                 maxd_asize, nsize_aer, alnsign, f1)

            goto 30000
         end if

         do n=1,ntype_aer
            !wig 19-Oct-2006: Add zero trap based May 2006 e-mail from
            !Ghan. Transport can clear out a cell leading to
            !inconsistencies with the mass.
            gmrad(n)=gmrad(n)/max(totn(n),1e-20)
            gmlnsig=gmradsq(n)/totn(n)-gmrad(n)*gmrad(n)    ! [ln(sigmag)]**2
            gmlnsig(n)=sqrt( max( 1.e-4, gmlnsig(n) ) )
            gmrad(n)=exp(gmrad(n))
            if ((isectional .eq. 3) .or. (isectional .eq. 4)) then
               gmsm(n)=totn(n)/sumns(n)
               gmsm(n)=gmsm(n)*gmsm(n)*gmsm(n)
               gmsm(n)=sqrt(gmsm(n))
            else
!              gmsm(n)=2.*aten/(3.*gmrad(n))*sqrt(aten/(3.*hygro(1,n)*gmrad(n)))
               gmsm(n)=2.*aten/(3.*gmrad(n))*sqrt(aten/(3.*hygro(nsize_aer(n),n)*gmrad(n)))
            endif
         enddo
         
         ! Initialize maxsat at this cell and timestep for the
         ! sectional setup (the modal case is handled above)...
         call maxsat_init(maxd_atype, ntype_aer, &
              maxd_asize, (/1/), gmlnsig, f1)

!.......................................................................
!   calculate sectional "sub-bin" size distribution
!
!   dn/dy = nt*( a + b*y )   for  ylo < y < yhi
!
!   nt = na(m,n) = number mixing ratio of the bin
!   y = v/vhi
!       v = (4pi/3)*r**3 = particle volume
!       vhi = v at r=rhi (upper bin boundary)
!   ylo = y at lower bin boundary = vlo/vhi = (rlo/rhi)**3
!   yhi = y at upper bin boundary = 1.0
!
!   dv/dy = v * dn/dy = nt*vhi*( a*y + b*y*y )
!
!.......................................................................
! 02-may-2006 - this dn/dy replaces the previous
!       dn/dx = a + b*x   where l = ln(r)
!    the old dn/dx was overly complicated for cases of rmean near rlo or rhi
!    the new dn/dy is consistent with that used in the movesect routine,
!       which does continuous growth by condensation and aqueous chemistry
!.......................................................................
             do 25002 n = 1,ntype_aer
             do 25000 m = 1,nsize_aer(n)

! convert from diameter in cm to radius in m
                rlo(m,n) = 0.5*0.01*dlo_sect(m,n)
                rhi(m,n) = 0.5*0.01*dhi_sect(m,n)
                ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                yhi(m,n) = 1.0

! 04-nov-2005 - extremely narrow bins will be treated using 0/1 activation
!    this is to avoid potential numerical problems
                bin_is_narrow(m,n) = .false.
                if ((rhi(m,n)/rlo(m,n)) .le. 1.01) bin_is_narrow(m,n) = .true.

! rmean is mass mean radius for the bin; xmean = log(rmean)
! just use section midpoint if bin is empty
                if ( bin_is_empty(m,n) ) then
                   rmean(m,n) = sqrt(rlo(m,n)*rhi(m,n)) 
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                   goto 25000
                end if

                rmean(m,n) = (volc(m,n)/(ccc*na(m,n)))**third
                rmean(m,n) = max( rlo(m,n), min( rhi(m,n), rmean(m,n) ) )
                ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                if ( bin_is_narrow(m,n) ) goto 25000

! if rmean is extremely close to either rlo or rhi, 
! treat the bin as extremely narrow
                if ((rhi(m,n)/rmean(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   rlo(m,n) = min( rmean(m,n), (rhi(m,n)/1.01) )
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   goto 25000
                else if ((rmean(m,n)/rlo(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   rhi(m,n) = max( rmean(m,n), (rlo(m,n)*1.01) )
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                   goto 25000
                endif

! if rmean is somewhat close to either rlo or rhi, then dn/dy will be 
!    negative near the upper or lower bin boundary
! in these cases, assume that all the particles are in a subset of the full bin,
!    and adjust rlo or rhi so that rmean will be near the center of this subset
! note that the bin is made narrower LOCALLY/TEMPORARILY, 
!    just for the purposes of the activation calculation
                gammayy = (ymean(m,n)-ylo(m,n)) / (yhi(m,n)-ylo(m,n))
                if (gammayy .lt. 0.34) then
                   dumaa = ylo(m,n) + (yhi(m,n)-ylo(m,n))*(gammayy/0.34)
                   rhi(m,n) = rhi(m,n)*(dumaa**third)
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                else if (gammayy .ge. 0.66) then
                   dumaa = ylo(m,n) + (yhi(m,n)-ylo(m,n))*((gammayy-0.66)/0.34)
                   ylo(m,n) = dumaa
                   rlo(m,n) = rhi(m,n)*(dumaa**third)
                end if
                if ((rhi(m,n)/rlo(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   goto 25000
                end if

                betayy = ylo(m,n)/yhi(m,n)
                betayy2 = betayy*betayy
                bsub(m,n) = (12.0*ymean(m,n) - 6.0*(1.0+betayy)) /   &
                   (4.0*(1.0-betayy2*betayy) - 3.0*(1.0-betayy2)*(1.0+betayy))
                asub(m,n) = (1.0 - bsub(m,n)*(1.0-betayy2)*0.5) / (1.0-betayy)

                if ( asub(m,n)+bsub(m,n)*ylo(m,n) .lt. 0. ) then
                  if (idiag_dndy_neg .gt. 0) then
                    print *,'dndy<0 at lower boundary'
                    print *,'n,m=',n,m
                    print *,'na=',na(m,n),' volc=',volc(m,n)
                    print *,'volc/(na*pi*4/3)=', (volc(m,n)/(na(m,n)*ccc))
                    print *,'rlo(m,n),rhi(m,n)=',rlo(m,n),rhi(m,n)
                    print *,'dlo_sect/2,dhi_sect/2=',   &
                             (0.005*dlo_sect(m,n)),(0.005*dhi_sect(m,n))
                    print *,'asub,bsub,ylo,yhi=',asub(m,n),bsub(m,n),ylo(m,n),yhi(m,n)
                    print *,'asub+bsub*ylo=',   &
                             (asub(m,n)+bsub(m,n)*ylo(m,n))
                    print *,'subr activate error 11 - i,j,k =', ii, jj, kk
                  endif
                endif
                if ( asub(m,n)+bsub(m,n)*yhi(m,n) .lt. 0. ) then
                  if (idiag_dndy_neg .gt. 0) then
                    print *,'dndy<0 at upper boundary'
                    print *,'n,m=',n,m
                    print *,'na=',na(m,n),' volc=',volc(m,n)
                    print *,'volc/(na*pi*4/3)=', (volc(m,n)/(na(m,n)*ccc))
                    print *,'rlo(m,n),rhi(m,n)=',rlo(m,n),rhi(m,n)
                    print *,'dlo_sect/2,dhi_sect/2=',   &
                             (0.005*dlo_sect(m,n)),(0.005*dhi_sect(m,n))
                    print *,'asub,bsub,ylo,yhi=',asub(m,n),bsub(m,n),ylo(m,n),yhi(m,n)
                    print *,'asub+bsub*yhi=',   &
                             (asub(m,n)+bsub(m,n)*yhi(m,n))
                    print *,'subr activate error 12 - i,j,k =', ii, jj, kk
                  endif
                endif

25000        continue      ! m=1,nsize_aer(n)
25002        continue      ! n=1,ntype_aer


30000    continue
!.......................................................................
!
!   end calc. of modal or sectional activation properties (end of section 1)
!
!.......................................................................



!      sjg 7-16-98  upward
!      print *,'wbar,sigw=',wbar,sigw

      if(sigw.le.1.e-5) goto 50000

!.......................................................................
!
!   start calc. of activation fractions/fluxes
!   for spectrum of updrafts (start of section 2)
!
!.......................................................................
         ipass_nwloop = 1
         idiagaa = 0
! 06-nov-2005 rce - set idiagaa=1 for testing/debugging
!        if ((grid_id.eq.1) .and. (ktau.eq.167) .and.   &
!            (ii.eq.24) .and. (jj.eq. 1) .and. (kk.eq.14)) idiagaa = 1

40000    continue
         if(top)then
           wmax=0.
           wmin=min(zero,-wdiab)
         else
           wmax=min(wmaxf,wbar+sds*sigw)
           wmin=max(wminf,-wdiab)
         endif
         wmin=max(wmin,wbar-sds*sigw)
         w=wmin
         dwmax=eps*sigw
         dw=dwmax
         dfmax=0.2
         dfmin=0.1
         if(wmax.le.w)then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fluxn(m,n)=0.
               fn(m,n)=0.
               fluxs(m,n)=0.
               fs(m,n)=0.
               fluxm(m,n)=0.
               fm(m,n)=0.
            end do
            end do
            flux_fullact=0.
            return
         endif
         do n=1,ntype_aer
         do m=1,nsize_aer(n)
            sumflxn(m,n)=0.
            sumfn(m,n)=0.
            fnold(m,n)=0.
            sumflxs(m,n)=0.
            sumfs(m,n)=0.
            fsold(m,n)=0.
            sumflxm(m,n)=0.
            sumfm(m,n)=0.
            fmold(m,n)=0.
         enddo
         enddo
         sumflx_fullact=0.

         fold=0
         gold=0
! 06-nov-2005 rce - set wold=w here
!        wold=0
         wold=w


! 06-nov-2005 rce - define nwmax; calc dwmin from nwmax
         nwmax = 200
!        dwmin = min( dwmax, 0.01 )
         dwmin = (wmax - wmin)/(nwmax-1)
         dwmin = min( dwmax, dwmin )
         dwmin = max( 0.01,  dwmin )

!
! loop over updrafts, incrementing sums as you go
! the "200" is (arbitrary) upper limit for number of updrafts
! if integration finishes before this, OK; otherwise, ERROR
!
         if (idiagaa.gt.0) then
             write(*,94700) ktau, grid_id, ii, jj, kk, nwmax
             write(*,94710) 'wbar,sigw,wdiab=', wbar, sigw, wdiab
             write(*,94710) 'wmin,wmax,dwmin,dwmax=', wmin, wmax, dwmin, dwmax
             write(*,94720) -1, w, wold, dw
         end if
94700    format( / 'activate 47000 - ktau,id,ii,jj,kk,nwmax=', 6i5 )
94710    format( 'activate 47000 - ', a, 6(1x,f11.5) )
94720    format( 'activate 47000 - nw,w,wold,dw=', i5, 3(1x,f11.5) )

         do 47000 nw = 1, nwmax
41000       wnuc=w+wdiab

            if (idiagaa.gt.0) write(*,94720) nw, w, wold, dw

!           write(6,*)'wnuc=',wnuc
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)
            etafactor1=2.*alw*sqrtalw
            if (isectional .gt. 0) then
!              sectional model.
!              use bulk properties

              do n=1,ntype_aer
                 if(totn(n).gt.1.e-10)then
                    eta(1,n)=etafactor1/(totn(n)*beta*sqrtg)
                 else
                    eta(1,n)=1.e10
                 endif
              enddo
              !BSINGH - For WRFCuP scheme
              ! use smax_prescribed if it is present; otherwise get smax from subr maxsat
              if ( present( smax_prescribed ) ) then
                 smax = smax_prescribed
              else
                 !BSINGH -ENDS
                 
                 call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                      maxd_asize,(/1/),gmsm,gmlnsig,f1,smax)
              endif
              lnsmax=log(smax)
              x=2*(log(gmsm(1))-lnsmax)/(3*sq2*gmlnsig(1))
              fnew=0.5*(1.-ERF_ALT(x))

            else

              do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 eta(m,n)=etafactor1*etafactor2(m,n)
              enddo
              enddo
              !BSINGH - For WRFCuP scheme
              if ( present( smax_prescribed ) ) then
                 smax = smax_prescribed
              else
                 !BSINGH -ENDS
                 call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                      maxd_asize,nsize_aer,sm,alnsign,f1,smax)
              endif
!             write(6,*)'w,smax=',w,smax

              lnsmax=log(smax)

              x=2*(lnsm(nsize_aer(1),1)-lnsmax)/(3*sq2*alnsign(nsize_aer(1),1))
              fnew=0.5*(1.-ERF_ALT(x))

            endif

            dwnew = dw
! 06-nov-2005 rce - "n" here should be "nw" (?) 
!           if(fnew-fold.gt.dfmax.and.n.gt.1)then
            if(fnew-fold.gt.dfmax.and.nw.gt.1)then
!              reduce updraft increment for greater accuracy in integration
               if (dw .gt. 1.01*dwmin) then
                  dw=0.7*dw
                  dw=max(dw,dwmin)
                  w=wold+dw
                  go to 41000
               else
                  dwnew = dwmin
               endif
            endif

            if(fnew-fold.lt.dfmin)then
!              increase updraft increment to accelerate integration
               dwnew=min(1.5*dw,dwmax)
            endif
            fold=fnew

            z=(w-wbar)/(sigw*sq2)
            gaus=exp(-z*z)
            fnmin=1.
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3
!           write(6,*)'xmincoeff=',xmincoeff


            do 44002 n=1,ntype_aer
            do 44000 m=1,nsize_aer(n)
               if ( bin_is_empty(m,n) ) then
                   fn(m,n)=0.
                   fs(m,n)=0.
                   fm(m,n)=0.
               else if ((isectional .eq. 2) .or. (isectional .eq. 4)) then
!                 sectional
!                  within-section dn/dx = a + b*x
                  xcut=xmincoeff-third*lnhygro(m,n)
!                 ycut=(exp(xcut)/rhi(m,n))**3
! 07-jul-2006 rce - the above line gave a (rare) overflow when smax=1.0e-20
! if (ycut > yhi), then actual value of ycut is unimportant, 
! so do the following to avoid overflow
                  lnycut = 3.0 * ( xcut - log(rhi(m,n)) )
                  lnycut = min( lnycut, log(yhi(m,n)*1.0e5) )
                  ycut=exp(lnycut)
!                 write(6,*)'m,n,rcut,rlo,rhi=',m,n,exp(xcut),rlo(m,n),rhi(m,n)
!                   if(lnsmax.lt.lnsmn(m,n))then
                  if(ycut.gt.yhi(m,n))then
                     fn(m,n)=0.
                     fs(m,n)=0.
                     fm(m,n)=0.
                  elseif(ycut.lt.ylo(m,n))then
                     fn(m,n)=1.
                     fs(m,n)=1.
                     fm(m,n)=1.
                  elseif ( bin_is_narrow(m,n) ) then
! 04-nov-2005 rce - for extremely narrow bins, 
! do zero activation if xcut>xmean, 100% activation otherwise
                     if (ycut.gt.ymean(m,n)) then
                        fn(m,n)=0.
                        fs(m,n)=0.
                        fm(m,n)=0.
                     else
                        fn(m,n)=1.
                        fs(m,n)=1.
                        fm(m,n)=1.
                     endif
                  else
                     phiyy=ycut/yhi(m,n)
                     fn(m,n) = asub(m,n)*(1.0-phiyy) + 0.5*bsub(m,n)*(1.0-phiyy*phiyy)
                     if (fn(m,n).lt.zero .or. fn(m,n).gt.one) then
                      if (idiag_fnsm_prob .gt. 0) then
                        print *,'fn(',m,n,')=',fn(m,n),' outside 0,1 - activate err21'
                        print *,'na,volc       =', na(m,n), volc(m,n)
                        print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                        print *,'yhi,ycut      =', yhi(m,n), ycut
                      endif
                     endif

                     if (fn(m,n) .le. zero) then
! 10-nov-2005 rce - if fn=0, then fs & fm must be 0
                        fn(m,n)=zero
                        fs(m,n)=zero
                        fm(m,n)=zero
                     else if (fn(m,n) .ge. one) then
! 10-nov-2005 rce - if fn=1, then fs & fm must be 1
                        fn(m,n)=one
                        fs(m,n)=one
                        fm(m,n)=one
                     else
! 10-nov-2005 rce - otherwise, calc fm and check it
                        fm(m,n) = (yhi(m,n)/ymean(m,n)) * (0.5*asub(m,n)*(1.0-phiyy*phiyy) +   &
                                  third*bsub(m,n)*(1.0-phiyy*phiyy*phiyy))
                        if (fm(m,n).lt.fn(m,n) .or. fm(m,n).gt.one) then
                         if (idiag_fnsm_prob .gt. 0) then
                           print *,'fm(',m,n,')=',fm(m,n),' outside fn,1 - activate err22'
                           print *,'na,volc,fn    =', na(m,n), volc(m,n), fn(m,n)
                           print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                           print *,'yhi,ycut     =', yhi(m,n), ycut
                         endif
                        endif
                        if (fm(m,n) .le. fn(m,n)) then
! 10-nov-2005 rce - if fm=fn, then fs must =fn
                           fm(m,n)=fn(m,n)
                           fs(m,n)=fn(m,n)
                        else if (fm(m,n) .ge. one) then
! 10-nov-2005 rce - if fm=1, then fs & fn must be 1
                           fm(m,n)=one
                           fs(m,n)=one
                           fn(m,n)=one
                        else
! 10-nov-2005 rce - these two checks assure that the mean size
! of the activated & interstitial particles will be between rlo & rhi
                           dumaa = fn(m,n)*(yhi(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )
                           dumaa = 1.0 + (fn(m,n)-1.0)*(ylo(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )
! 10-nov-2005 rce - now calculate fs and bound it by fn, fm
                           betayy = ylo(m,n)/yhi(m,n)
                           dumaa = phiyy**twothird
                           dumbb = betayy**twothird
                           fs(m,n) =   &
                              (asub(m,n)*(1.0-phiyy*dumaa) +   &
                                  0.625*bsub(m,n)*(1.0-phiyy*phiyy*dumaa)) /   &
                              (asub(m,n)*(1.0-betayy*dumbb) +   &
                                  0.625*bsub(m,n)*(1.0-betayy*betayy*dumbb))
                           fs(m,n)=max(fs(m,n),fn(m,n))
                           fs(m,n)=min(fs(m,n),fm(m,n))
                        endif
                     endif
                  endif

               else
!                 modal
                  x=2*(lnsm(m,n)-lnsmax)/(3*sq2*alnsign(m,n))
                  fn(m,n)=0.5*(1.-ERF_ALT(x))
                  arg=x-sq2*alnsign(m,n)
                  fs(m,n)=0.5*(1.-ERF_ALT(arg))
                  arg=x-1.5*sq2*alnsign(m,n)
                  fm(m,n)=0.5*(1.-ERF_ALT(arg))
!                 print *,'w,x,fn,fs,fm=',w,x,fn(m,n),fs(m,n),fm(m,n)
               endif

!                     fn(m,n)=1.  !test
!                     fs(m,n)=1.
!                     fm(m,n)=1.
               fnmin=min(fn(m,n),fnmin)
!               integration is second order accurate
!               assumes linear variation of f*gaus with w
               wb=(w+wold)
               fnbar=(fn(m,n)*gaus+fnold(m,n)*gold)
               fsbar=(fs(m,n)*gaus+fsold(m,n)*gold)
               fmbar=(fm(m,n)*gaus+fmold(m,n)*gold)
               if((top.and.w.lt.0.).or.(.not.top.and.w.gt.0.))then
                  sumflxn(m,n)=sumflxn(m,n)+sixth*(wb*fnbar           &
                      +(fn(m,n)*gaus*w+fnold(m,n)*gold*wold))*dw
                  sumflxs(m,n)=sumflxs(m,n)+sixth*(wb*fsbar           &
                      +(fs(m,n)*gaus*w+fsold(m,n)*gold*wold))*dw
                  sumflxm(m,n)=sumflxm(m,n)+sixth*(wb*fmbar           &
                      +(fm(m,n)*gaus*w+fmold(m,n)*gold*wold))*dw
               endif
               sumfn(m,n)=sumfn(m,n)+0.5*fnbar*dw
!              write(6,'(a,9g10.2)')'lnsmax,lnsm(m,n),x,fn(m,n),fnold(m,n),g,gold,fnbar,dw=', &
!                lnsmax,lnsm(m,n),x,fn(m,n),fnold(m,n),g,gold,fnbar,dw
               fnold(m,n)=fn(m,n)
               sumfs(m,n)=sumfs(m,n)+0.5*fsbar*dw
               fsold(m,n)=fs(m,n)
               sumfm(m,n)=sumfm(m,n)+0.5*fmbar*dw
               fmold(m,n)=fm(m,n)

44000       continue      ! m=1,nsize_aer(n)
44002       continue      ! n=1,ntype_aer

!           same form as sumflxm(m,n) but replace the fm/fmold(m,n) with 1.0
            sumflx_fullact = sumflx_fullact &
                           + sixth*(wb*(gaus+gold) + (gaus*w + gold*wold))*dw
!            sumg=sumg+0.5*(gaus+gold)*dw
            gold=gaus
            wold=w
            dw=dwnew

            if(nw.gt.1.and.(w.gt.wmax.or.fnmin.gt.fmax))go to 48000
            w=w+dw

47000    continue      ! nw = 1, nwmax


         print *,'do loop is too short in activate'
         print *,'wmin=',wmin,' w=',w,' wmax=',wmax,' dw=',dw
         print *,'wbar=',wbar,' sigw=',sigw,' wdiab=',wdiab
         print *,'wnuc=',wnuc
         do n=1,ntype_aer
            print *,'ntype=',n
            print *,'na=',(na(m,n),m=1,nsize_aer(n))
            print *,'fn=',(fn(m,n),m=1,nsize_aer(n))
         end do
!   dump all subr parameters to allow testing with standalone code
!   (build a driver that will read input and call activate)
         print *,'top,wbar,sigw,wdiab,tair,rhoair,ntype_aer='
         print *, top,wbar,sigw,wdiab,tair,rhoair,ntype_aer
         print *,'na='
         print *, na
         print *,'volc='
         print *, volc
         print *,'sigman='
         print *, sigman
         print *,'hygro='
         print *, hygro

         print *,'subr activate error 31 - i,j,k =', ii, jj, kk
! 06-nov-2005 rce - if integration fails, repeat it once with additional diagnostics
         if (ipass_nwloop .eq. 1) then
             ipass_nwloop = 2
             idiagaa = 2
             goto 40000
         end if
         call wrf_error_fatal("STOP: activate before 48000")

48000    continue


!         ndist(n)=ndist(n)+1
         if(.not.top.and.w.lt.wmaxf)then

!            contribution from all updrafts stronger than wmax
!            assuming constant f (close to fmax)
            wnuc=w+wdiab

            z1=(w-wbar)/(sigw*sq2)
            z2=(wmaxf-wbar)/(sigw*sq2)
            integ=sigw*0.5*sq2*sqpi*(ERFC_NUM_RECIPES(z1)-ERFC_NUM_RECIPES(z2))
!            consider only upward flow into cloud base when estimating flux
            wf1=max(w,zero)
            zf1=(wf1-wbar)/(sigw*sq2)
            gf1=exp(-zf1*zf1)
            wf2=max(wmaxf,zero)
            zf2=(wf2-wbar)/(sigw*sq2)
            gf2=exp(-zf2*zf2)
            gf=(gf1-gf2)
            integf=wbar*sigw*0.5*sq2*sqpi*(ERFC_NUM_RECIPES(zf1)-ERFC_NUM_RECIPES(zf2))+sigw*sigw*gf

            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               sumflxn(m,n)=sumflxn(m,n)+integf*fn(m,n)
               sumfn(m,n)=sumfn(m,n)+fn(m,n)*integ
               sumflxs(m,n)=sumflxs(m,n)+integf*fs(m,n)
               sumfs(m,n)=sumfs(m,n)+fs(m,n)*integ
               sumflxm(m,n)=sumflxm(m,n)+integf*fm(m,n)
               sumfm(m,n)=sumfm(m,n)+fm(m,n)*integ
            end do
            end do
!           same form as sumflxm(m,n) but replace the fm(m,n) with 1.0
            sumflx_fullact = sumflx_fullact + integf
!            sumg=sumg+integ
         endif


         do n=1,ntype_aer
         do m=1,nsize_aer(n)

!           fn(m,n)=sumfn(m,n)/(sumg)
            fn(m,n)=sumfn(m,n)/(sq2*sqpi*sigw)
            fluxn(m,n)=sumflxn(m,n)/(sq2*sqpi*sigw)
            if(fn(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fn=',fn(m,n),' > 1 - activate err41'
               print *,'w,m,n,na,am=',w,m,n,na(m,n),am(m,n)
               print *,'integ,sumfn,sigw=',integ,sumfn(m,n),sigw
               print *,'subr activate error - i,j,k =', ii, jj, kk
             endif
             fluxn(m,n) = fluxn(m,n)/fn(m,n)
            endif

            fs(m,n)=sumfs(m,n)/(sq2*sqpi*sigw)
            fluxs(m,n)=sumflxs(m,n)/(sq2*sqpi*sigw)
            if(fs(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fs=',fs(m,n),' > 1 - activate err42'
               print *,'m,n,isectional=',m,n,isectional
               print *,'alnsign(m,n)=',alnsign(m,n)
               print *,'rcut,rlo(m,n),rhi(m,n)',exp(xcut),rlo(m,n),rhi(m,n)
               print *,'w,m,na,am=',w,m,na(m,n),am(m,n)
               print *,'integ,sumfs,sigw=',integ,sumfs(m,n),sigw
             endif
             fluxs(m,n) = fluxs(m,n)/fs(m,n)
            endif

!           fm(m,n)=sumfm(m,n)/(sumg)
            fm(m,n)=sumfm(m,n)/(sq2*sqpi*sigw)
            fluxm(m,n)=sumflxm(m,n)/(sq2*sqpi*sigw)
            if(fm(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fm(',m,n,')=',fm(m,n),' > 1 - activate err43'
             endif
             fluxm(m,n) = fluxm(m,n)/fm(m,n)
            endif

         end do
         end do
!        same form as fluxm(m,n)
         flux_fullact = sumflx_fullact/(sq2*sqpi*sigw)

      goto 60000
!.......................................................................
!
!   end calc. of activation fractions/fluxes
!   for spectrum of updrafts (end of section 2)
!
!.......................................................................

!.......................................................................
!
!   start calc. of activation fractions/fluxes
!   for (single) uniform updraft (start of section 3)
!
!.......................................................................
50000 continue

         wnuc=wbar+wdiab
!         write(6,*)'uniform updraft =',wnuc

! 04-nov-2005 rce - moved the code for "wnuc.le.0" code to here
         if(wnuc.le.0.)then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fn(m,n)=0
               fluxn(m,n)=0
               fs(m,n)=0
               fluxs(m,n)=0
               fm(m,n)=0
               fluxm(m,n)=0
            end do
            end do
            flux_fullact=0.
            return
         endif

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)

            if (isectional .gt. 0) then
!              sectional model.
!              use bulk properties
              do n=1,ntype_aer
              if(totn(n).gt.1.e-10)then
                 eta(1,n)=2*alw*sqrtalw/(totn(n)*beta*sqrtg)
              else
                 eta(1,n)=1.e10
              endif
              end do
              !BSINGH - For WRFCuP
              ! use smax_prescribed if it is present; otherwise get smax from subr maxsat
              if ( present( smax_prescribed ) ) then
                 smax = smax_prescribed
              else
                 !BSINGH -ENDS
                 call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                      maxd_asize,(/1/),gmsm,gmlnsig,f1,smax)
              endif

            else

              do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 if(na(m,n).gt.1.e-10)then
                    eta(m,n)=2*alw*sqrtalw/(na(m,n)*beta*sqrtg)
                 else
                    eta(m,n)=1.e10
                 endif
              end do
              end do
              !BSINGH - For WRFCuP
              ! use smax_prescribed if it is present; otherwise get smax from subr maxsat
              if ( present( smax_prescribed ) ) then
                 smax = smax_prescribed
              else
                 !BSINGH -ENDS
                 
                 call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                      maxd_asize,nsize_aer,sm,alnsign,f1,smax)
              endif

            endif

            lnsmax=log(smax)
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

            do 55002 n=1,ntype_aer
            do 55000 m=1,nsize_aer(n)

! 04-nov-2005 rce - check for bin_is_empty here too, just like earlier
               if ( bin_is_empty(m,n) ) then
                   fn(m,n)=0.
                   fs(m,n)=0.
                   fm(m,n)=0.

               else if ((isectional .eq. 2) .or. (isectional .eq. 4)) then
!                 sectional
!                  within-section dn/dx = a + b*x
                  xcut=xmincoeff-third*lnhygro(m,n)
!                 ycut=(exp(xcut)/rhi(m,n))**3
! 07-jul-2006 rce - the above line gave a (rare) overflow when smax=1.0e-20
! if (ycut > yhi), then actual value of ycut is unimportant, 
! so do the following to avoid overflow
                  lnycut = 3.0 * ( xcut - log(rhi(m,n)) )
                  lnycut = min( lnycut, log(yhi(m,n)*1.0e5) )
                  ycut=exp(lnycut)
!                 write(6,*)'m,n,rcut,rlo,rhi=',m,n,exp(xcut),rlo(m,n),rhi(m,n)
!                   if(lnsmax.lt.lnsmn(m,n))then
                  if(ycut.gt.yhi(m,n))then
                     fn(m,n)=0.
                     fs(m,n)=0.
                     fm(m,n)=0.
!                   elseif(lnsmax.gt.lnsmx(m,n))then
                  elseif(ycut.lt.ylo(m,n))then
                     fn(m,n)=1.
                     fs(m,n)=1.
                     fm(m,n)=1.
                  elseif ( bin_is_narrow(m,n) ) then
! 04-nov-2005 rce - for extremely narrow bins, 
! do zero activation if xcut>xmean, 100% activation otherwise
                     if (ycut.gt.ymean(m,n)) then
                        fn(m,n)=0.
                        fs(m,n)=0.
                        fm(m,n)=0.
                     else
                        fn(m,n)=1.
                        fs(m,n)=1.
                        fm(m,n)=1.
                     endif
                  else
                     phiyy=ycut/yhi(m,n)
                     fn(m,n) = asub(m,n)*(1.0-phiyy) + 0.5*bsub(m,n)*(1.0-phiyy*phiyy)
                     if (fn(m,n).lt.zero .or. fn(m,n).gt.one) then
                      if (idiag_fnsm_prob .gt. 0) then
                        print *,'fn(',m,n,')=',fn(m,n),' outside 0,1 - activate err21'
                        print *,'na,volc       =', na(m,n), volc(m,n)
                        print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                        print *,'yhi,ycut      =', yhi(m,n), ycut
                      endif
                     endif

                     if (fn(m,n) .le. zero) then
! 10-nov-2005 rce - if fn=0, then fs & fm must be 0
                        fn(m,n)=zero
                        fs(m,n)=zero
                        fm(m,n)=zero
                     else if (fn(m,n) .ge. one) then
! 10-nov-2005 rce - if fn=1, then fs & fm must be 1
                        fn(m,n)=one
                        fs(m,n)=one
                        fm(m,n)=one
                     else
! 10-nov-2005 rce - otherwise, calc fm and check it
                        fm(m,n) = (yhi(m,n)/ymean(m,n)) * (0.5*asub(m,n)*(1.0-phiyy*phiyy) +   &
                                  third*bsub(m,n)*(1.0-phiyy*phiyy*phiyy))
                        if (fm(m,n).lt.fn(m,n) .or. fm(m,n).gt.one) then
                         if (idiag_fnsm_prob .gt. 0) then
                           print *,'fm(',m,n,')=',fm(m,n),' outside fn,1 - activate err22'
                           print *,'na,volc,fn    =', na(m,n), volc(m,n), fn(m,n)
                           print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                           print *,'yhi,ycut      =', yhi(m,n), ycut
                         endif
                        endif
                        if (fm(m,n) .le. fn(m,n)) then
! 10-nov-2005 rce - if fm=fn, then fs must =fn
                           fm(m,n)=fn(m,n)
                           fs(m,n)=fn(m,n)
                        else if (fm(m,n) .ge. one) then
! 10-nov-2005 rce - if fm=1, then fs & fn must be 1
                           fm(m,n)=one
                           fs(m,n)=one
                           fn(m,n)=one
                        else
! 10-nov-2005 rce - these two checks assure that the mean size
! of the activated & interstitial particles will be between rlo & rhi
                           dumaa = fn(m,n)*(yhi(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )
                           dumaa = 1.0 + (fn(m,n)-1.0)*(ylo(m,n)/ymean(m,n))
                           fm(m,n) = min( fm(m,n), dumaa )
! 10-nov-2005 rce - now calculate fs and bound it by fn, fm
                           betayy = ylo(m,n)/yhi(m,n)
                           dumaa = phiyy**twothird
                           dumbb = betayy**twothird
                           fs(m,n) =   &
                              (asub(m,n)*(1.0-phiyy*dumaa) +   &
                                  0.625*bsub(m,n)*(1.0-phiyy*phiyy*dumaa)) /   &
                              (asub(m,n)*(1.0-betayy*dumbb) +   &
                                  0.625*bsub(m,n)*(1.0-betayy*betayy*dumbb))
                           fs(m,n)=max(fs(m,n),fn(m,n))
                           fs(m,n)=min(fs(m,n),fm(m,n))
                        endif
                     endif

                  endif

               else
!                 modal
                  x=2*(lnsm(m,n)-lnsmax)/(3*sq2*alnsign(m,n))
                  fn(m,n)=0.5*(1.-ERF_ALT(x))
                  arg=x-sq2*alnsign(m,n)
                  fs(m,n)=0.5*(1.-ERF_ALT(arg))
                  arg=x-1.5*sq2*alnsign(m,n)
                  fm(m,n)=0.5*(1.-ERF_ALT(arg))
               endif

!                     fn(m,n)=1. ! test
!                     fs(m,n)=1.
!                     fm(m,n)=1.
                if((top.and.wbar.lt.0.).or.(.not.top.and.wbar.gt.0.))then
                   fluxn(m,n)=fn(m,n)*w
                   fluxs(m,n)=fs(m,n)*w
                   fluxm(m,n)=fm(m,n)*w
                else
                   fluxn(m,n)=0
                   fluxs(m,n)=0
                   fluxm(m,n)=0
               endif

55000       continue      ! m=1,nsize_aer(n)
55002       continue      ! n=1,ntype_aer

            if((top.and.wbar.lt.0.).or.(.not.top.and.wbar.gt.0.))then
               flux_fullact = w
            else
               flux_fullact = 0.0
            endif

! 04-nov-2005 rce - moved the code for "wnuc.le.0" from here 
! to near the start the uniform undraft section

!.......................................................................
!
!   end calc. of activation fractions/fluxes 
!   for (single) uniform updraft (end of section 3)
!
!.......................................................................



60000 continue


!            do n=1,ntype_aer
!            do m=1,nsize_aer(n)
!                write(6,'(a,2i3,5e10.1)')'n,m,na,wbar,sigw,fn,fm=',n,m,na(m,n),wbar,sigw,fn(m,n),fm(m,n)
!            end do
!            end do


      return
      end subroutine activate



!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine maxsat(zeta,eta, &
                        maxd_atype,ntype_aer,maxd_asize,nsize_aer, &
                        sm,alnsign,f1,smax)

!      Calculates maximum supersaturation for multiple competing aerosol
!      modes. Note that maxsat_init must be called before calling this
!      subroutine.

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      implicit none

      integer, intent(in) :: maxd_atype
      integer, intent(in) :: ntype_aer
      integer, intent(in) :: maxd_asize
      integer, intent(in) :: nsize_aer(maxd_atype) ! number of size bins
      real, intent(in) :: sm(maxd_asize,maxd_atype) ! critical supersaturation for number mode radius
      real, intent(in) :: zeta, eta(maxd_asize,maxd_atype)
      real, intent(in) :: alnsign(maxd_asize,maxd_atype) ! ln(sigma)
      real, intent(in) :: f1(maxd_asize,maxd_atype)
      real, intent(out) :: smax ! maximum supersaturation

      real :: g1, g2
      real thesum
      integer m ! size index
      integer n ! type index

      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(zeta.gt.1.e5*eta(m,n) .or. &
              sm(m,n)*sm(m,n).gt.1.e5*eta(m,n))then
!           weak forcing. essentially none activated
            smax=1.e-20
         else
!           significant activation of this mode. calc activation all modes.
            go to 1
         endif
      end do
      end do

      return

  1   continue

      thesum=0
      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(eta(m,n).gt.1.e-20)then
            g1=sqrt(zeta/eta(m,n))
            g1=g1*g1*g1
            g2=sm(m,n)/sqrt(eta(m,n)+3*zeta)
            g2=sqrt(g2)
            g2=g2*g2*g2
            thesum=thesum + &
                 (f1(m,n)*g1+(1.+0.25*alnsign(m,n))*g2)/(sm(m,n)*sm(m,n))
         else
            thesum=1.e20
         endif
      end do
      end do

      smax=1./sqrt(thesum)

      return
      end subroutine maxsat



!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine maxsat_init(maxd_atype, ntype_aer, &
           maxd_asize, nsize_aer, alnsign, f1)

!     Calculates the f1 paramter needed by maxsat.

!     Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!     2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      implicit none

      integer, intent(in)  :: maxd_atype
      integer, intent(in)  :: ntype_aer ! number of aerosol types
      integer, intent(in)  :: maxd_asize
      integer, intent(in)  :: nsize_aer(maxd_atype) ! number of size bins
      real,    intent(in)  :: alnsign(maxd_asize,maxd_atype) ! ln(sigma)
      real,    intent(out) :: f1(maxd_asize,maxd_atype)

      integer m ! size index
      integer n ! type index

!  calculate and save f1(sigma), assumes sigma is invariant
!  between calls to this init routine

      do n=1,ntype_aer
         do m=1,nsize_aer(n)
            f1(m,n)=0.5*exp(2.5*alnsign(m,n)*alnsign(m,n))
         end do
      end do

      end subroutine maxsat_init



!----------------------------------------------------------------------
!----------------------------------------------------------------------
! 25-apr-2006 rce - dens_aer is (g/cm3), NOT (kg/m3);
!     grid_id, ktau, i, j, isize, itype added to arg list to assist debugging
       subroutine loadaer(chem,k,kmn,kmx,num_chem,cs,npv, &
                          dlo_sect,dhi_sect,maxd_acomp, ncomp,                &
                          grid_id, ktau, i, j, isize, itype,   &
                          numptr_aer, numptrcw_aer, dens_aer,   &
                          massptr_aer, massptrcw_aer,   &
                          maerosol, maerosolcw,                 &
                          maerosol_tot, maerosol_totcw,         &
                          naerosol, naerosolcw,                 &
                          vaerosol, vaerosolcw)

      implicit none

!      load aerosol number, surface, mass concentrations

!      input

       integer, intent(in) ::  num_chem ! maximum number of consituents
       integer, intent(in) ::  k,kmn,kmx
       real,    intent(in) ::  chem(kmn:kmx,num_chem) ! aerosol mass, number mixing ratios
       real,    intent(in) ::  cs  ! air density (kg/m3)
       real,    intent(in) ::  npv ! number per volume concentration (/m3)
       integer, intent(in) ::  maxd_acomp,ncomp
       integer, intent(in) ::  numptr_aer,numptrcw_aer
       integer, intent(in) ::  massptr_aer(maxd_acomp), massptrcw_aer(maxd_acomp)
       real,    intent(in) ::  dens_aer(maxd_acomp) ! aerosol material density (g/cm3)
       real,    intent(in) ::  dlo_sect,dhi_sect ! minimum, maximum diameter of section (cm)
       integer, intent(in) ::  grid_id, ktau, i, j, isize, itype

!      output

       real, intent(out) ::  naerosol                ! interstitial number conc (/m3)
       real, intent(out) ::  naerosolcw              ! activated    number conc (/m3)
       real, intent(out) ::  maerosol(maxd_acomp)   ! interstitial mass conc (kg/m3)
       real, intent(out) ::  maerosolcw(maxd_acomp) ! activated    mass conc (kg/m3)
       real, intent(out) ::  maerosol_tot   ! total-over-species interstitial mass conc (kg/m3)
       real, intent(out) ::  maerosol_totcw ! total-over-species activated    mass conc (kg/m3)
       real, intent(out) ::  vaerosol       ! interstitial volume conc (m3/m3)
       real, intent(out) ::  vaerosolcw     ! activated volume conc (m3/m3)

!      internal

       integer lnum,lnumcw,l,ltype,lmass,lmasscw,lsfc,lsfccw
       real num_at_dhi, num_at_dlo
       real npv_at_dhi, npv_at_dlo
       real, parameter :: pi = 3.1415926526
       real specvol ! inverse aerosol material density (m3/kg)

          lnum=numptr_aer
          lnumcw=numptrcw_aer
          maerosol_tot=0.
          maerosol_totcw=0.
          vaerosol=0.
          vaerosolcw=0.
          do l=1,ncomp
             lmass=massptr_aer(l)
             lmasscw=massptrcw_aer(l)
             maerosol(l)=chem(k,lmass)*cs
             maerosol(l)=max(maerosol(l),0.)
             maerosolcw(l)=chem(k,lmasscw)*cs
             maerosolcw(l)=max(maerosolcw(l),0.)
             maerosol_tot=maerosol_tot+maerosol(l)
             maerosol_totcw=maerosol_totcw+maerosolcw(l)
! [ 1.e-3 factor because dens_aer is (g/cm3), specvol is (m3/kg) ]
             specvol=1.0e-3/dens_aer(l)
             vaerosol=vaerosol+maerosol(l)*specvol
             vaerosolcw=vaerosolcw+maerosolcw(l)*specvol
!            write(6,'(a,3e12.2)')'maerosol,dens_aer,vaerosol=',maerosol(l),dens_aer(l),vaerosol
          enddo

          if(lnum.gt.0)then
!            aerosol number predicted
! [ 1.0e6 factor because because dhi_ & dlo_sect are (cm), vaerosol is (m3) ]
             npv_at_dhi = 6.0e6/(pi*dhi_sect*dhi_sect*dhi_sect)
             npv_at_dlo = 6.0e6/(pi*dlo_sect*dlo_sect*dlo_sect)

             naerosol=chem(k,lnum)*cs
             naerosolcw=chem(k,lnumcw)*cs
             num_at_dhi = vaerosol*npv_at_dhi
             num_at_dlo = vaerosol*npv_at_dlo
             naerosol = max( num_at_dhi, min( num_at_dlo, naerosol ) )

!            write(6,'(a,5e10.1)')'naerosol,num_at_dhi,num_at_dlo,dhi_sect,dlo_sect', &
!                          naerosol,num_at_dhi,num_at_dlo,dhi_sect,dlo_sect
             num_at_dhi = vaerosolcw*npv_at_dhi
             num_at_dlo = vaerosolcw*npv_at_dlo
             naerosolcw = max( num_at_dhi, min( num_at_dlo, naerosolcw ) )
          else
!            aerosol number diagnosed from mass and prescribed size
             naerosol=vaerosol*npv
             naerosol=max(naerosol,0.)
             naerosolcw=vaerosolcw*npv
             naerosolcw=max(naerosolcw,0.)
          endif


       return
       end subroutine loadaer



!-----------------------------------------------------------------------
        real function erfc_num_recipes( x )
!
!   from press et al, numerical recipes, 1990, page 164
!
        implicit none
        real x
        double precision erfc_dbl, dum, t, zz

        zz = abs(x)
        t = 1.0/(1.0 + 0.5*zz)

!       erfc_num_recipes =
!     &   t*exp( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
!     &   t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
!     &                                    t*(-1.13520398 +
!     &   t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

        dum =  ( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
          t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
                                           t*(-1.13520398 +   &
          t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

        erfc_dbl = t * exp(dum)
        if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl

        erfc_num_recipes = erfc_dbl

        return
        end function erfc_num_recipes     

!-----------------------------------------------------------------------
    real function erf_alt( x )

    implicit none

    real,intent(in) :: x

    erf_alt = 1. - erfc_num_recipes(x)

    end function erf_alt

END MODULE module_mixactivate
