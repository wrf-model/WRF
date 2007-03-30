!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for information and terms of use
!**********************************************************************************  

MODULE module_mixactivate

CONTAINS


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! 06-nov-2005 rce - grid_id & ktau added to arg list
! 25-apr-2006 rce - dens_aer is (g/cm3), NOT (kg/m3)
      subroutine prescribe_aerosol_mixactivate (                      &
		grid_id, ktau, dtstep, naer,                          &
		rho_phy, th_phy, pi_phy, w, cldfra, cldfra_old,       &
		z, dz8w, p_at_w, t_at_w, exch_h,                      &
        qv, qc, qi, qndrop3d,                                 &
        nsource,                                              &
		ims,ime, jms,jme, kms,kme,                            &
		its,ite, jts,jte, kts,kte,                            &
		f_qc, f_qi                                            )

!        USE module_configure

! wrapper to call mixactivate for mosaic description of aerosol

	implicit none

!   subr arguments
	integer, intent(in) ::                  &
		grid_id, ktau,                  &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(in) :: dtstep
	real, intent(inout) :: naer ! aerosol number (/kg)

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		rho_phy, th_phy, pi_phy, w,  &
		z, dz8w, p_at_w, t_at_w, exch_h

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: cldfra, cldfra_old

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		qv, qc, qi

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
	     dgnum_aer(maxd_asize, maxd_atype),    & ! mean diameter (cm) of mode
	     dens_aer( maxd_acomp, maxd_atype),    & ! density (g/cm3) of material
	     mw_aer( maxd_acomp, maxd_atype)         ! molecular weight (g/mole)
      real, dimension(ims:ime,kms:kme,jms:jme) :: &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat
	     integer idrydep_onoff
      real, dimension(ims:ime,kms:kme,jms:jme) :: t_phy
      integer msectional


	  integer ptr
	  real maer

      if(naer.lt.1.)then
	     naer=1000.e6 ! #/kg default value
      endif
	  ai_phase=1
	  cw_phase=2
	  idrydep_onoff = 0
	  msectional = 0

	  t_phy(:,:,:)=th_phy(:,:,:)*pi_phy(:,:,:)

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
	  dgnum_aer(m,n)=0.1e-4       ! mean diameter (cm) of mode
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
	     chem(:,:,:,ptr)=naer
	  else
	     chem(:,:,:,ptr)=0
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
	        call exit(1)
	     endif
	     massptr_aer(l, m, n, p)=ptr
! maer is ug/kg-air;  naer is #/kg-air;  dgnum is cm;  dens_aer is g/cm3
! 1.e6 factor converts g to ug
	     maer= 1.0e6 * naer * dens_aer(l,n) * ( (3.1416/6.) *   &
                 (dgnum_aer(m,n)**3) * exp( 4.5*((log(sigmag_aer(m,n)))**2) ) )
	     if(p.eq.ai_phase)then
	        chem(:,:,:,ptr)=maer
	     else
	        chem(:,:,:,ptr)=0
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
	     call exit(1)
	  endif
!wig	  waterptr_aer(m, n)=ptr
	  waterptr_aer(m, n)=-1
	end do ! size
	end do ! type
	ddvel(:,:,:)=0.
    hygro( :,:,:,:,:) = 0.5

! 06-nov-2005 rce - grid_id & ktau added to arg list
      call mixactivate(  msectional,     &
            chem,max_chem,qv,qc,qi,qndrop3d,        &
            t_phy, w, ddvel, idrydep_onoff,  &
            maxd_acomp, maxd_asize, maxd_atype, maxd_aphase,   &
            ncomp_aer, nsize_aer, ntype_aer, nphase_aer,  &
            numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dgnum_aer,  &
            dens_aer, mw_aer,           &
            waterptr_aer, hygro,  ai_phase, cw_phase,                &
            ims,ime, jms,jme,  &
            kms,kme,                            &
            its,ite, jts,jte, kts,kte,                            &
            rho_phy, z, dz8w, p_at_w, t_at_w, exch_h,      &
            cldfra, cldfra_old, qsrflx,         &
            ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
            grid_id, ktau, dtstep, &
            F_QC=f_qc, F_QI=f_qi                              )


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
           numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dgnum_aer,  &
           dens_aer, mw_aer,               &
           waterptr_aer, hygro, ai_phase, cw_phase,              &
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
  USE module_radiation_driver, only: cal_cldfra

  implicit none

!     input

  INTEGER, intent(in) ::         grid_id, ktau
  INTEGER, intent(in) ::         num_chem
  integer, intent(in) ::         ims,ime, jms,jme, kms,kme,    &
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
  real, intent(in)  ::             &
       dlo_sect( maxd_asize, maxd_atype ),   & ! minimum size of section (cm)
       dhi_sect( maxd_asize, maxd_atype ),   & ! maximum size of section (cm)
       sigmag_aer(maxd_asize, maxd_atype),   & ! geometric standard deviation of aerosol size dist
       dgnum_aer(maxd_asize, maxd_atype),    & ! mean diameter (cm) of mode
       dens_aer( maxd_acomp, maxd_atype),    & ! density (g/cm3) of material
       mw_aer( maxd_acomp, maxd_atype)         ! molecular weight (g/mole)


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
  real :: qndrop(kms:kme) ! cloud droplet number mixing ratio (#/kg)
  real :: lcldfra(kms:kme) ! liquid cloud fraction
  real :: lcldfra_old(kms:kme) ! liquid cloud fraction for previous timestep
  real :: wtke(kms:kme) ! turbulent vertical velocity at base of layer k (m2/s)
  real zn(kms:kme) ! g/pdel (m2/g) for layer
  real zs(kms:kme) ! inverse of distance between levels (m)
  real zkmin,zkmax
  data zkmin/0.01/,zkmax/100./
  save zkmin,zkmax
  real cs(kms:kme) ! air density (kg/m3)
  real dz(kms:kme) ! geometric thickness of layers (m)

  real wdiab           ! diabatic vertical velocity
!      real, parameter :: wmixmin = 0.1 ! minimum turbulence vertical velocity (m/s)
  real, parameter :: wmixmin = 0.2 ! minimum turbulence vertical velocity (m/s)
!      real, parameter :: wmixmin = 1.0 ! minimum turbulence vertical velocity (m/s)
  real :: qndrop_new(kms:kme)     ! droplet number nucleated on cloud boundaries
  real :: ekd(kms:kme)       ! diffusivity for droplets (m2/s)
  real :: ekk(kms:kme)       ! density*diffusivity for droplets (kg/m3 m2/s)
  real :: srcn(kms:kme) ! droplet source rate (/s)
  real sq2pi
  data sq2pi/2.5066282746/
  save sq2pi
  real dtinv

  logical top        ! true if cloud top, false if cloud base or new cloud
  logical first
  save first
  data first/.true./
  integer km1,kp1
  real wbar,wmix,wmin,wmax
  real cmincld
  data cmincld/1.e-12/
  save cmincld
  real dum,dumc
  real dact
  real fluxntot         ! (#/cm2/s)
  real fac_srflx
  real depvel_drop
  real :: surfrate(num_chem) ! surface exchange rate (/s)
  real surfratemax      ! max surfrate for all species treated here
  real surfrate_drop ! surfade exchange rate for droplelts
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
  integer count_submix(100)
  save count_submix

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
!     note:  activation fraction fluxes are defined as
!     fluxn = [flux of activated aero. number into cloud (#/cm2/s)]
!           / [aero. number conc. in updraft, just below cloudbase (#/cm3)]

  real :: nact(kms:kme,maxd_asize, maxd_atype)  ! fractional aero. number  activation rate (/s)
  real :: mact(kms:kme,maxd_asize, maxd_atype)  ! fractional aero. mass    activation rate (/s)
  real :: npv(maxd_asize, maxd_atype) ! number per volume concentration (/m3)
  real scale

  real :: hygro_aer(maxd_asize, maxd_atype)  ! hygroscopicity of aerosol mode
  real :: exp45logsig     ! exp(4.5*alogsig**2)
  real :: alogsig(maxd_asize, maxd_atype) ! natl log of geometric standard dev of aerosol
  integer psat
  parameter (psat=6) ! number of supersaturations to calc ccn concentration
  real ccn(kts:kte,psat)        ! number conc of aerosols activated at supersat
  real, parameter :: supersat(psat)= &! supersaturation (%) to determine ccn concentration
       (/0.02,0.05,0.1,0.2,0.5,1.0/)
  real super(psat) ! supersaturation
  real surften       ! surface tension of water w/respect to air (N/m)
  data surften/0.076/
  save surften
  real :: ccnfact(psat,maxd_asize, maxd_atype)
  real :: amcube(maxd_asize, maxd_atype) ! cube of dry mode radius (m)
  real :: argfactor(maxd_asize, maxd_atype)
  real aten ! surface tension parameter
  real t0 ! reference temperature
  real sm ! critical supersaturation
  real arg

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
  integer ids,ide, jds,jde, kds,kde

  arg = 1.0
  if (abs(0.8427-ERF_ALT(arg))/0.8427>0.001) then
     write (6,*) 'erf_alt(1.0) = ',ERF_ALT(arg)
     write (6,*) 'dropmixnuc: Error function error'
     call exit
  endif
  arg = 0.0
  if (ERF_ALT(arg) /= 0.0) then
     write (6,*) 'erf_alt(0.0) = ',ERF_ALT(arg)
     write (6,*) 'dropmixnuc: Error function error'
     call exit
  endif

  pi = 4.*atan(1.0)
  dtinv=1./dtstep

  depvel_drop =  0.1 ! prescribed here rather than getting it from dry_dep_driver
  if (idrydep_onoff .le. 0) depvel_drop =  0.0

  do n=1,ntype_aer
     do m=1,nsize_aer(n)
        ncomp(n)=ncomp_aer(n)
!	    print *,'sigmag_aer,dgnum_aer=',sigmag_aer(m,n),dgnum_aer(m,n)
        alogsig(m,n)=alog(sigmag_aer(m,n))
        ! used only if number is diagnosed from volume
        npv(m,n)=6./(pi*(0.01*dgnum_aer(m,n))**3*exp(4.5*alogsig(m,n)*alogsig(m,n)))
     end do
  end do

  t0=273.
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
     CALL cal_cldfra(CLDFRA,qc,qi,f_qc,f_qi,      &
          ids,ide, jds,jde, kds,kde,              &
          ims,ime, jms,jme, kms,kme,              &
          its,ite, jts,jte, kts,kte               )
  END IF

  qsrflx(:,:,:) = 0.

!     start loop over columns

  do 120 j=jts,jte
  do 100 i=its,ite

     raercol(:,:,:) = 0. !~ wig: added, but should not be necessary
     fluxn(:,:) = 0.     !~
     fluxs(:,:) = 0.     !~
     fluxm(:,:) = 0.     !~
     fn(:,:) = 0.        !~
     fs(:,:) = 0.        !~
     fm(:,:) = 0.        !~

!        load number nucleated into qndrop on cloud boundaries

! initialization for current i .........................................

     do k=kts+1,kte-1
	    zs(k)=1./(zm(i,k,j)-zm(i,k-1,j))
	 enddo
	 zs(kts)=zs(kts+1)
     zs(kte)=0.

     do k=kts,kte-1
!!$	    if(qndrop3d(i,k,j).lt.-10.e6.or.qndrop3d(i,k,j).gt.1.E20)then
!!$!	       call exit(1)
!!$	    endif
        if(f_qi)then
           qcld=qc(i,k,j)+qi(i,k,j)
        else
           qcld=qc(i,k,j)
        endif
        if(qcld.lt.-1..or.qcld.gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qcld=',qcld,' for i,k,j=',i,k,j
           call exit(1)
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
              surfrate(lnum)=ddvel(i,j,lnum)/dz(kts)
              surfrate(lnumcw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lnum) )
!             scale = 1000./mwdry ! moles/kg
              scale = 1.
              raercol(kts:kte-1,lnumcw,nsav)=chem(i,kts:kte-1,j,lnumcw)*scale ! #/kg
              raercol(kts:kte-1,lnum,nsav)=chem(i,kts:kte-1,j,lnum)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
!             scale = mw_aer(l,n)/mwdry
              scale = 1.e-9 ! kg/ug
              surfrate(lmass)=ddvel(i,j,lmass)/dz(kts)
              surfrate(lmasscw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lmass) )
              raercol(kts:kte-1,lmasscw,nsav)=chem(i,kts:kte-1,j,lmasscw)*scale ! kg/kg
              raercol(kts:kte-1,lmass,nsav)=chem(i,kts:kte-1,j,lmass)*scale ! kg/kg
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)then
              surfrate(lwater)=ddvel(i,j,lwater)/dz(kts)
              surfratemax = max( surfratemax, surfrate(lwater) )
              raercol(kts:kte-1,lwater,nsav)=chem(i,kts:kte-1,j,lwater) ! don't bother to convert units,
             ! because it doesn't contribute to aerosol mass
           endif
        enddo ! size
     enddo ! type


!        droplet nucleation/aerosol activation

! k-loop for growing/shrinking cloud calcs .............................

     do k=kts,kte-1
        km1=max0(k-1,1)
        kp1=min0(k+1,kte-1)

        if(lcldfra(k)-lcldfra_old(k).gt.0.01)then
!       go to 10

!                growing cloud

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
                fn,fs,fm,fluxn,fluxs,fluxm, grid_id, ktau, i, j, k )

           dumc=(lcldfra(k)-lcldfra_old(k))
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 dact=dumc*fn(m,n)*(raercol(k,lnum,nsav)) ! interstitial only
!          print *,'fn=',fn(m,n),' for m,n=',m,n
!                   print *,'growing cloud dumc=',dumc,' fn=',fn(m,n)
                 qndrop(k)=qndrop(k)+dact
                 nsource(i,k,j)=nsource(i,k,j)+dact*dtinv
                 if(lnum.gt.0)then
                    raercol(k,lnumcw,nsav) = raercol(k,lnumcw,nsav)+dact
                    raercol(k,lnum,nsav) = raercol(k,lnum,nsav)-dact
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
! rce 07-jul-2005 - changed dact for mass to mimic that used for number
!         dact=dum*(raercol(k,lmass,nsav)) ! interstitial only
                    dact=dumc*fm(m,n)*(raercol(k,lmass,nsav)) ! interstitial only
                    raercol(k,lmasscw,nsav) = raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,nsav) = raercol(k,lmass,nsav)-dact
                 enddo
              enddo
           enddo
!   10 continue
        endif

        if(lcldfra(k) < lcldfra_old(k) .and. lcldfra_old(k) > 1.e-20)then
!         go to 20

!                shrinking cloud ......................................................

!                droplet loss in decaying cloud
           nsource(i,k,j)=nsource(i,k,j)+qndrop(k)*(lcldfra(k)-lcldfra_old(k))*dtinv
           qndrop(k)=qndrop(k)*(1.+lcldfra(k)-lcldfra_old(k))
!                 convert activated aerosol to interstitial in decaying cloud

           dumc=(lcldfra(k)-lcldfra_old(k))/lcldfra_old(k)
!       print *,'shrinking cloud dumc=',dumc
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    dact=raercol(k,lnumcw,nsav)*dumc
                    raercol(k,lnumcw,nsav)=raercol(k,lnumcw,nsav)+dact
                    raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    dact=raercol(k,lmasscw,nsav)*dumc
                    raercol(k,lmasscw,nsav)=raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                 enddo
              enddo
           enddo
!             20 continue
        endif

     enddo !k loop

! end of k-loop for growing/shrinking cloud calcs ......................


! ......................................................................
! start of k-loop for calc of old cloud activation tendencies ..........

     do k=kts,kte-1
        km1=max0(k-1,kts)
        kp1=min0(k+1,kte-1)
        if(lcldfra(k).gt.0.01)then
           if(lcldfra_old(k).gt.0.01)then
!          go to 30

!               old cloud

              if(lcldfra_old(k)-lcldfra_old(km1).gt.0.01.or.k.eq.kts)then

!                   interior cloud

!                   cloud base

                 wdiab=0
                 wmix=wtke(k) ! spectrum of updrafts
                 wbar=w(i,k,j) ! spectrum of updrafts
!                    wmix=0. ! single updraft
!               wbar=wtke(k) ! single updraft
! 06-nov-2005 rce - increase wmax from 10 to 50 (deep convective clouds)
                 wmax=50.
                 top=.false.
                 ekd(k)=wtke(k)*dz(k)/sq2pi
                 alogarg=max(1.e-20,1/lcldfra_old(k)-1.)
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
                      fn,fs,fm,fluxn,fluxs,fluxm, grid_id, ktau, i, j, k )
                 
                 if(k.gt.kts)then
                    dumc = lcldfra_old(k)-lcldfra_old(km1)
                 else
                    dumc=lcldfra_old(k)
                 endif
                 dum=1./(dz(k))
                 fluxntot=0.
                 do n=1,ntype_aer
                    do m=1,nsize_aer(n)
                       fluxn(m,n)=fluxn(m,n)*dumc
!                       fluxs(m,n)=fluxs(m,n)*dumc
                       fluxm(m,n)=fluxm(m,n)*dumc
                       lnum=numptr_aer(m,n,ai_phase)
                       fluxntot=fluxntot+fluxn(m,n)*raercol(km1,lnum,nsav)
!             print *,'fn=',fn(m,n),' for m,n=',m,n
!             print *,'old cloud dumc=',dumc,' fn=',fn(m,n),' for m,n=',m,n
                       nact(k,m,n)=nact(k,m,n)+fluxn(m,n)*dum
                       mact(k,m,n)=mact(k,m,n)+fluxm(m,n)*dum
                    enddo
                 enddo
                 nsource(i,k,j)=nsource(i,k,j)+fluxntot*zs(k)
                 fluxntot=fluxntot*cs(k)
              endif
!       30 continue
           endif
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
     enddo
!    50 continue

!    go to 100

!        switch nsav, nnew so that nnew is the updated aerosol

     ntemp=nsav
     nsav=nnew
     nnew=ntemp

!        load new droplets in layers above, below clouds

     dtmin=dtstep
     ekk(kts)=0.0
     do k=kts+1,kte-1
        ekk(k)=ekd(k)*p_at_w(i,k,j)/(r_d*t_at_w(i,k,j))
     enddo
     ekk(kte)=0.0
     do k=kts,kte-1
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
     count_submix(nsubmix_bnd)=count_submix(nsubmix_bnd)+1
     dtmix=dtstep/nsubmix
     fac_srflx = -1.0/(zn(1)*nsubmix)
     
     do k=kts,kte-1
        kp1=min(k+1,kte-1)
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

     do nsub=1,nsubmix
        qndrop_new(kts:kte-1)=qndrop(kts:kte-1)
!           switch nsav, nnew so that nsav is the updated aerosol
        ntemp=nsav
        nsav=nnew
        nnew=ntemp
        srcn(:)=0.0
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
!              update droplet source
              srcn(kts:kte-1)=srcn(kts:kte-1)+nact(kts:kte-1,m,n)*(raercol(kts:kte-1,lnum,nsav))
           enddo
        enddo

        call explmix(qndrop,srcn,ekkp,ekkm,overlapp,overlapm,   &
             qndrop_new,surfrate_drop,kts,kte-1,dtmix,.false.)
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
              lnumcw=numptr_aer(m,n,cw_phase)
              if(lnum>0)then
                 source(kts:kte-1)= nact(kts:kte-1,m,n)*(raercol(kts:kte-1,lnum,nsav))
                 call explmix(raercol(1,lnumcw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lnumcw,nsav),surfrate(lnumcw),kts,kte-1,dtmix,&
                      .false.)
                 call explmix(raercol(1,lnum,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lnum,nsav),surfrate(lnum),kts,kte-1,dtmix, &
                      .true.,raercol(1,lnumcw,nsav))
                 qsrflx(i,j,lnum) = qsrflx(i,j,lnum) + fac_srflx*            &
                      raercol(kts,lnum,nsav)*surfrate(lnum)
                 qsrflx(i,j,lnumcw) = qsrflx(i,j,lnumcw) + fac_srflx*        &
                      raercol(kts,lnumcw,nsav)*surfrate(lnumcw)
              endif
              do l=1,ncomp(n)
                 lmass=massptr_aer(l,m,n,ai_phase)
                 lmasscw=massptr_aer(l,m,n,cw_phase)
                 source(kts:kte-1)= mact(kts:kte-1,m,n)*(raercol(kts:kte-1,lmass,nsav))
                 call explmix(raercol(1,lmasscw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lmasscw,nsav),surfrate(lmasscw),kts,kte-1,dtmix,  &
                      .false.)
                 call explmix(raercol(1,lmass,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lmass,nsav),surfrate(lmass),kts,kte-1,dtmix,  &
                      .true.,raercol(1,lmasscw,nsav))
                 qsrflx(i,j,lmass) = qsrflx(i,j,lmass) + fac_srflx*          &
                      raercol(kts,lmass,nsav)*surfrate(lmass)
                 qsrflx(i,j,lmasscw) = qsrflx(i,j,lmasscw) + fac_srflx*      &
                      raercol(kts,lmasscw,nsav)*surfrate(lmasscw)
              enddo
              lwater=waterptr_aer(m,n)  ! aerosol water
              if(lwater>0)then
                 source(:)=0.
                 call explmix(   raercol(1,lwater,nnew),source,ekkp,ekkm,overlapp,overlapm,   &
                      raercol(1,lwater,nsav),surfrate(lwater),kts,kte-1,dtmix,  &
                      .true.,source)
              endif
           enddo ! size
        enddo ! type

     enddo !nsub

!    go to 100

!        evaporate particles again if no cloud

     do k=kts,kte-1
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

!         go to 100
!        droplet number

     do k=kts,kte-1
!       if(lcldfra(k).gt.0.1)then
!           write(6,'(a,3i5,f12.1)')'i,j,k,qndrop=',i,j,k,qndrop(k)
!       endif
        if(qndrop(k).lt.-10.e6.or.qndrop(k).gt.1.e12)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop=',qndrop(k),' for i,k,j=',i,k,j
!          call exit(1)
        endif

        qndrop3d(i,k,j) = max(qndrop(k),1.e-6)

        if(qndrop3d(i,k,j).lt.-10.e6.or.qndrop3d(i,k,j).gt.1.E20)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop=',qndrop3d(i,k,j),' for i,k,j=',i,k,j
!          call exit(1)
        endif
        if(qc(i,k,j).lt.-1..or.qc(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qc=',qc(i,k,j),' for i,k,j=',i,k,j
           call exit(1)
        endif
        if(qi(i,k,j).lt.-1..or.qi(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qi=',qi(i,k,j),' for i,k,j=',i,k,j
           call exit(1)
        endif
        if(qv(i,k,j).lt.-1..or.qv(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qv=',qv(i,k,j),' for i,k,j=',i,k,j
           call exit(1)
        endif
        cldfra_old(i,k,j) = cldfra(i,k,j)
!       if(k.gt.6.and.k.lt.11)cldfra_old(i,k,j)=1.
     enddo



!    go to 100
!        update chem and convert back to mole/mole

     ccn(:,:) = 0.
     do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum.gt.0)then
              !          scale=mwdry*0.001
              scale = 1.
              chem(i,kts:kte-1,j,lnumcw)= raercol(kts:kte-1,lnumcw,nnew)*scale
              chem(i,kts:kte-1,j,lnum)= raercol(kts:kte-1,lnum,nnew)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
!          scale = mwdry/mw_aer(l,n)
              scale = 1.e9
              chem(i,kts:kte-1,j,lmasscw)=raercol(kts:kte-1,lmasscw,nnew)*scale ! ug/kg
              chem(i,kts:kte-1,j,lmass)=raercol(kts:kte-1,lmass,nnew)*scale ! ug/kg
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)chem(i,kts:kte-1,j,lwater)=raercol(kts:kte-1,lwater,nnew) ! don't convert units
           do k=kts,kte-1
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

100  continue ! end of main loop over i
120  continue ! end of main loop over j


     return
   end subroutine mixactivate


!----------------------------------------------------------------------
!----------------------------------------------------------------------
   subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
                       qold, surfrate, kts, kte, dt, is_unact,   &
             qactold )

!  explicit integration of droplet/aerosol mixing
!     with source due to activation/nucleation


   implicit none
   integer, intent(in) :: kts,kte ! number of levels
   real, intent(inout) :: q(kts:kte) ! mixing ratio to be updated
   real, intent(in) :: qold(kts:kte) ! mixing ratio from previous time step
   real, intent(in) :: src(kts:kte) ! source due to activation/nucleation (/s)
   real, intent(in) :: ekkp(kts:kte) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! below layer k  (k,k+1 interface)
   real, intent(in) :: ekkm(kts:kte) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
                      ! above layer k  (k,k+1 interface)
   real, intent(in) :: overlapp(kts:kte) ! cloud overlap below
   real, intent(in) :: overlapm(kts:kte) ! cloud overlap above
   real, intent(in) :: surfrate ! surface exchange rate (/s)
   real, intent(in) :: dt ! time step (s)
   logical, intent(in) :: is_unact ! true if this is an unactivated species
   real, intent(in),optional :: qactold(kts:kte)
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
!          if(q(k)<-1.e-30)then ! force to non-negative
!             print *,'q=',q(k),' in explmix'
             q(k)=max(q(k),0.)
!          endif
      end do
   else
      do k=kts,kte
         kp1=min(k+1,kte)
         km1=max(k-1,kts)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +  &
                                    ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )
!         if(q(k)<-1.e-30)then ! force to non-negative
!            print *,'q=',q(k),' in explmix'
            q(k)=max(q(k),0.)
!         endif
      end do
   end if
!     diffusion loss at base of lowest layer
      q(kts)=q(kts)-surfrate*qold(kts)*dt

!          if(q(kts)<-1.e-30)then ! force to non-negative
!             print *,'q=',q(kts),' in explmix'
             q(kts)=max(q(kts),0.)
!          endif

   return
   end subroutine explmix

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! 06-nov-2005 rce - grid_id & ktau added to arg list
      subroutine activate(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      na, volc, dlo_sect,dhi_sect,sigman, hygro, &
                      fn, fs, fm, fluxn, fluxs, fluxm, &
                      grid_id, ktau, ii, jj, kk )

!      calculates number, surface, and mass fraction of aerosols activated as CCN
!      calculates flux of cloud droplets, surface area, and aerosol mass into cloud
!      assumes an internal mixture within each of up to pmaxd_atype X pmaxd_asize 
!      multiple aerosol modes. 
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

!      output

      real,intent(inout) :: fn(maxd_asize,maxd_atype)    ! number fraction of aerosols activated
      real,intent(inout) :: fs(maxd_asize,maxd_atype)    ! surface fraction of aerosols activated
      real,intent(inout) :: fm(maxd_asize,maxd_atype)    ! mass fraction of aerosols activated
      real,intent(inout) :: fluxn(maxd_asize,maxd_atype) ! flux of activated aerosol number fraction into cloud (m/s)
      real,intent(inout) :: fluxs(maxd_asize,maxd_atype) ! flux of activated aerosol surface fraction (m/s)
      real,intent(inout) :: fluxm(maxd_asize,maxd_atype) ! flux of activated aerosol mass fraction into cloud (m/s)

!      local

!!$      external erf,erfc
!!$      real erf,erfc
!      external qsat_water
      integer, parameter:: nx=200
      integer iquasisect_option, isectional
      real integ,integf
      real surften       ! surface tension of water w/respect to air (N/m)
      data surften/0.076/
      save surften
      real p0     ! reference pressure (Pa)
      real t0     ! reference temperature (K)
      data p0/1013.25e2/,t0/273.15/
      save p0,t0
      real ylo(maxd_asize,maxd_atype),yhi(maxd_asize,maxd_atype) ! 1-particle volume at section interfaces
      real ymean(maxd_asize,maxd_atype) ! 1-particle volume at r=rmean
      real ycut, lnycut, betayy, betayy2, gammayy, phiyy
      real surfc(maxd_asize,maxd_atype) ! surface concentration (m2/m3)
      real sign(maxd_asize,maxd_atype)    ! geometric standard deviation of size distribution
      real alnsign(maxd_asize,maxd_atype) ! natl log of geometric standard dev of aerosol
      real am(maxd_asize,maxd_atype) ! number mode radius of dry aerosol (m)
      real lnhygro(maxd_asize,maxd_atype) ! ln(b)
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
      logical top        ! true if cloud top, false if cloud base or new cloud
      data top/.false./
      save top
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
      real eps,fmax,sds
      data eps/0.3/,fmax/0.99/,sds/3./

!      mathematical constants
      real third, twothird, sixth, zero, one, two, three
! 04-nov-2005 rce - make this more precise
!     data third/0.333333/, twothird/0.66666667/, sixth/0.166666667/,zero/0./,one/1./,two/2./,three/3./
!     data third/0.33333333333/, twothird/0.66666666667/, sixth/0.16666666667/
!     data zero/0./,one/1./,two/2./,three/3./
!     save third, sixth,twothird,zero,one,two,three

      real sq2, sqpi, pi
! 04-nov-2005 rce - make this more precise
!     data sq2/1.4142136/, sqpi/1.7724539/,pi/3.14159/
      data sq2/1.4142135624/, sqpi/1.7724538509/,pi/3.1415926536/
      save sq2,sqpi,pi

      integer ndist(nx)  ! accumulates frequency distribution of integration bins required
      data ndist/nx*0/
      save eps,fmax,sds,ndist

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

      do n=1,ntype_aer
!         print *,'ntype_aer,n,nsize_aer(n)=',ntype_aer,n,nsize_aer(n)

        if(ntype_aer.eq.1.and.nsize_aer(n).eq.1.and.na(1,1).lt.1.e-20)then
         fn(1,1)=0.
         fs(1,1)=0.
         fm(1,1)=0.
         fluxn(1,1)=0.
         fluxs(1,1)=0.
         fluxm(1,1)=0.
         return
        endif
      enddo

      zero = 0.0
      one = 1.0
      two = 2.0
      three = 3.0
      third = 1.0/3.0
      twothird = 2.0/6.0
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
            return
         endif



         if (isectional .le. 0) goto 30000

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
!                gmsm(n)=2.*aten/(3.*gmrad(n))*sqrt(aten/(3.*hygro(1,n)*gmrad(n)))
                 gmsm(n)=2.*aten/(3.*gmrad(n))*sqrt(aten/(3.*hygro(nsize_aer(n),n)*gmrad(n)))
             endif
             enddo

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
!    this is to avoid potential numberical problems
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
! 07-nov-2005 rce - don't stop for this, it's not fatal
!                   stop
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
!                   stop
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
               call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,(/1/),gmsm,gmlnsig,smax)
!              call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,nsize_aer,gmsm,gmlnsig,smax)
              lnsmax=log(smax)
              x=2*(log(gmsm(1))-lnsmax)/(3*sq2*gmlnsig(1))
              fnew=0.5*(1.-ERF_ALT(x))

            else

              do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 eta(m,n)=etafactor1*etafactor2(m,n)
              enddo
              enddo

              call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,nsize_aer,sm,alnsign,smax)
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
         stop

48000    continue


         ndist(n)=ndist(n)+1
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
!              call exit
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
               call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,(/1/),gmsm,gmlnsig,smax)
!              call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,nsize_aer,gmsm,gmlnsig,smax)

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

              call maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,nsize_aer,sm,alnsign,smax)

            endif

            lnsmax=log(smax)
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

!           print *,'smax=',smax


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
      subroutine maxsat(zeta,eta,maxd_atype,ntype_aer,maxd_asize,nsize_aer, &
                        sm,alnsign,smax)

!      calculates maximum supersaturation for multiple
!      competing aerosol modes.

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      integer maxd_atype
      integer ntype_aer
      integer maxd_asize
      integer nsize_aer(maxd_atype) ! number of size bins
      real sm(maxd_asize,maxd_atype) ! critical supersaturation for number mode radius
      real zeta, eta(maxd_asize,maxd_atype)
      real alnsign(maxd_asize,maxd_atype) ! ln(sigma)
      integer pmax
      parameter (pmax=100)
      real f1(pmax,pmax)
      real smax ! maximum supersaturation
      save f1
      logical first
      data first/.true./
      save first
      real twothird,sum
! 04-nov-2005 rce - make this more precise
!     data twothird/0.666666666/
      data twothird/0.66666666667/
      save twothird
      integer m ! size index
      integer n ! type index

      if(first)then
!         calculate and save f1(sigma). assumes sigma is invariant.
         do n=1,ntype_aer
         do m=1,nsize_aer(n)
            if(ntype_aer>pmax)then
                print *,'pmax < ',ntype_aer,' in maxsat'
                call exit
            endif
            if(nsize_aer(n)>pmax)then
                print *,'pmax < ',nsize_aer(n),' in maxsat'
                call exit
            endif
            f1(m,n)=0.5*exp(2.5*alnsign(m,n)*alnsign(m,n))
         end do
         end do
         first=.false.
      endif

      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(zeta.gt.1.e5*eta(m,n).or.sm(m,n)*sm(m,n).gt.1.e5*eta(m,n))then
!            weak forcing. essentially none activated
            smax=1.e-20
         else
!            significant activation of this mode. calc activation all modes.
            go to 1
         endif
      end do
      end do

      return

  1   continue

      sum=0
      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(eta(m,n).gt.1.e-20)then
            g1=sqrt(zeta/eta(m,n))
            g1=g1*g1*g1
            g2=sm(m,n)/sqrt(eta(m,n)+3*zeta)
            g2=sqrt(g2)
            g2=g2*g2*g2
            sum=sum+(f1(m,n)*g1+(1.+0.25*alnsign(m,n))*g2)/(sm(m,n)*sm(m,n))
         else
            sum=1.e20
         endif
      end do
      end do

      smax=1./sqrt(sum)

      return
      end subroutine maxsat




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

       integer num_chem ! maximum number of consituents
       integer k,kmn,kmx
       real chem(kmn:kmx,num_chem) ! aerosol mass, number mixing ratios
       real cs  ! air density (kg/m3)
       real npv ! number per volume concentration (/m3)
       integer maxd_acomp,ncomp
       integer numptr_aer,numptrcw_aer
       integer massptr_aer(maxd_acomp), massptrcw_aer(maxd_acomp)
       real dens_aer(maxd_acomp) ! aerosol material density (g/cm3)
       real dlo_sect,dhi_sect ! minimum, maximum diameter of section (cm)
       integer grid_id, ktau, i, j, isize, itype

!      output

       real naerosol                ! interstitial number conc (/m3)
       real naerosolcw              ! activated    number conc (/m3)
       real maerosol(maxd_acomp)   ! interstitial mass conc (kg/m3)
       real maerosolcw(maxd_acomp) ! activated    mass conc (kg/m3)
       real maerosol_tot   ! total-over-species interstitial mass conc (kg/m3)
       real maerosol_totcw ! total-over-species activated    mass conc (kg/m3)
       real vaerosol       ! interstitial volume conc (m3/m3)
       real vaerosolcw     ! activated volume conc (m3/m3)

!      internal

       integer lnum,lnumcw,l,ltype,lmass,lmasscw,lsfc,lsfccw
       real num_at_dhi, num_at_dlo
       real npv_at_dhi, npv_at_dlo
       real pi
       real specvol ! inverse aerosol material density (m3/kg)
! 04-nov-2005 rce - make this more precise
!      data pi/3.14159/
       data pi/3.1415926526/
       save pi


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
