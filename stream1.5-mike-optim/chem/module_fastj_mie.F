!************************************************************************
! This computer software was prepared by Battelle Memorial Institute,
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Module to Compute Aerosol Optical Properties
! * Primary investigator: James C. Barnard
! * Co-investigators: Rahul A. Zaveri, Richard C. Easter, William I.
!   Gustafson Jr.
! Last update: February 2009
!
! Contact:
! Jerome D. Fast, PhD
! Staff Scientist
! Pacific Northwest National Laboratory
! P.O. Box 999, MSIN K9-30
! Richland, WA, 99352
! Phone: (509) 372-6116
! Email: Jerome.Fast@pnl.gov
!
! Please report any bugs or problems to Jerome Fast, the WRF-chem
! implmentation team leader for PNNL.
!
! Terms of Use:
!  1) Users are requested to consult the primary author prior to
!     modifying this module or incorporating it or its submodules in
!     another code. This is meant to ensure that the any linkages and/or
!     assumptions will not adversely affect the operation of this module.
!  2) The source code in this module is intended for research and
!     educational purposes. Users are requested to contact the primary
!     author regarding the use of the code for any commercial application.
!  3) Users preparing publications resulting from the usage of this code
!     are requested to cite one or more of the references below
!     (depending on the application) for proper acknowledgement.
!
! Note that the aerosol optical properites are currently tied to the use
! of Fast-J and MOSAIC.  Future modifications will make the calculations
! more generic so the code is not tied to the photolysis scheme and the
! code can be used  for both modal and sectional treatments.
!
! References: 
! * Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!   Barnard, E.G. Chapman, G.A. Grell, and S.E. Peckham (2005), Evolution
!   of ozone, particulates, and aerosol direct radiative forcing in the
!   vicinity of Houston using a fully-coupled meteorology-chemistry-
!   aerosol model, J. Geophys. Res., 111, D21305,
!   doi:10.1029/2005JD006721.
! * Barnard, J.C., J.D. Fast, G. Paredes-Miranda, W.P. Arnott, and
!   A. Laskin (2010), Technical note: evaluation of the WRF-Chem
!   "aerosol chemical to aerosol optical properties" module using data
!   from the MILAGRO campaign, Atmos. Chem. Phys., 10, 7325-7340,
!   doi:10.5194/acp-10-7325-2010.
!
! Contact Jerome Fast for updates on the status of manuscripts under
! review.
!
! Additional information:
! *  www.pnl.gov/atmospheric/research/wrf-chem
!
! Support:
! Funding for adapting Fast-J was provided by the U.S. Department of
! Energy under the auspices of Atmospheric Science Program of the Office
! of Biological and Environmental Research the PNNL Laboratory Research
! and Directed Research and Development program.
!************************************************************************
	module module_fastj_mie
	
! jcb 2007-feb-01  added capability to calculate aerosol backscatter
! units (km)^-1
! need to divide by 4*pie to get units of (km*ster)-1
! jcb 2007-feb-01  now calculate true extinctionm units km^-1


! rce 2005-apr-22 - want lunerr = -1 for wrf-chem 3d; 
! also, define lunerr here, and it's available everywhere
	integer, parameter :: lunerr = -1

	
	contains

!***********************************************************************
! <1.> subr mieaer
! Purpose:  calculate aerosol optical depth, single scattering albedo,
!   asymmetry factor, extinction, Legendre coefficients, and average aerosol
!   size. parameterizes aerosol coefficients using chebychev polynomials
!   requires double precision on 32-bit machines
!   uses Wiscombe's (1979) mie scattering code
! INPUT
!       id -- grid id number
!   	iclm, jclm -- i,j of grid column being processed
!       nbin_a -- number of bins
!	number_bin(nbin_a,kmaxd) --   number density in layer, #/cm^3
!	radius_wet(nbin_a,kmaxd) -- wet radius, cm
!    	refindx(nbin_a,kmaxd) --volume averaged complex index of refraction
!	dz -- depth of individual cells in column, m
!	isecfrm0 - time from start of run, sec
!	lpar -- number of grid cells in vertical (via module_fastj_cmnh)
!   kmaxd -- predefined maximum allowed levels from module_data_mosaic_other
!            passed here via module_fastj_cmnh
! OUTPUT: saved in module_fastj_cmnmie
!   real tauaer  ! aerosol optical depth
!        waer    ! aerosol single scattering albedo
!        gaer    ! aerosol asymmetery factor
!        extaer  ! aerosol extinction
!        l2,l3,l4,l5,l6,l7 ! Legendre coefficients, numbered 0,1,2,......
!        sizeaer ! average wet radius
!	 bscoef ! aerosol backscatter coefficient with units km-1 * steradian -1  JCB 2007/02/01
!----------------------------------------------------------------------
        subroutine mieaer( &
	          id, iclm, jclm, nbin_a,   &
              number_bin, radius_wet, refindx,   &
              dz, isecfrm0, lpar, &
              sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7,bscoef)  ! added bscoef JCB 2007/02/01

        USE module_data_mosaic_other, only : kmaxd
        USE module_data_mosaic_therm, ONLY: nbin_a_maxd
        USE module_peg_util, only:  peg_error_fatal, peg_message
        
        IMPLICIT NONE
!   subr arguments        
!jdf
        integer,parameter :: nspint = 4 ! Num of spectral intervals across
                                        ! solar spectrum for FAST-J
        integer, intent(in) :: lpar
        real, dimension (nspint, kmaxd+1),intent(out) :: sizeaer,extaer,waer,gaer,tauaer
        real, dimension (nspint, kmaxd+1),intent(out) :: l2,l3,l4,l5,l6,l7
        real, dimension (nspint, kmaxd+1),intent(out) :: bscoef  !JCB 2007/02/01
        real, dimension (nspint),save :: wavmid !cm
        data wavmid     &
            / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
    integer, intent(in) :: id, iclm, jclm, nbin_a, isecfrm0
    real, intent(in), dimension(nbin_a_maxd, kmaxd) :: number_bin
    real, intent(inout), dimension(nbin_a_maxd, kmaxd) :: radius_wet
    complex, intent(in) :: refindx(nbin_a_maxd, kmaxd)
    real, intent(in)    :: dz(lpar)		

!local variables
	real weighte, weights
! various bookeeping variables
	integer ltype ! total number of indicies of refraction
	parameter (ltype = 1)  ! bracket refractive indices based on information from Rahul, 2002/11/07
	real x
	real thesum ! for normalizing things
	real sizem ! size in microns
	integer kcallmieaer
!
	integer m, j, nc, klevel
        real pext	    ! parameterized specific extinction (cm2/g)
        real pasm       ! parameterized asymmetry factor
        real ppmom2     ! 2 Lengendre expansion coefficient (numbered 0,1,2,...)
        real ppmom3     ! 3     ...
        real ppmom4     ! 4     ...
        real ppmom5     ! 5     ...
        real ppmom6     ! 6     ...
        real ppmom7     ! 7     ...
        real sback2     ! JCB 2007/02/01 sback*conjg(sback)

        integer ns 	    ! Spectral loop index
        integer i  	    ! Longitude loop index
        integer k       ! Level loop index
        real pscat      !scattering cross section

        integer prefr,prefi,nrefr,nrefi,nr,ni
        save nrefr,nrefi
        parameter (prefr=7,prefi=7)

        complex*16 sforw,sback,tforw(2),tback(2)
        integer numang,nmom,ipolzn,momdim
        real*8 pmom(0:7,1)
        logical perfct,anyang,prnt(2)
        logical first
        integer, parameter ::  nsiz=200,nlog=30,ncoef=50
!
        real p2(nsiz),p3(nsiz),p4(nsiz),p5(nsiz)
        real p6(nsiz),p7(nsiz)
!
        real*8 xmu(1)
        data xmu/1./,anyang/.false./
        data numang/0/
        complex*16 s1(1),s2(1)
        data first/.true./
        save first
        real*8 mimcut
        data perfct/.false./,mimcut/0.0/
        data nmom/7/,ipolzn/0/,momdim/7/
        data prnt/.false.,.false./
!     coefficients for parameterizing aerosol radiative properties
!     in terms of refractive index and wet radius
      real extp(ncoef,prefr,prefi,nspint)	! specific extinction
      real albp(ncoef,prefr,prefi,nspint)	! single scat alb
      real asmp(ncoef,prefr,prefi,nspint)	! asymmetry factor
      real ascat(ncoef,prefr,prefi,nspint)	! scattering efficiency, JCB 2004/02/09
      real pmom2(ncoef,prefr,prefi,nspint)	! phase function expansion, #2
      real pmom3(ncoef,prefr,prefi,nspint)	! phase function expansion, #3
      real pmom4(ncoef,prefr,prefi,nspint)	! phase function expansion, #4
      real pmom5(ncoef,prefr,prefi,nspint)	! phase function expansion, #5
      real pmom6(ncoef,prefr,prefi,nspint)	! phase function expansion, #6
      real pmom7(ncoef,prefr,prefi,nspint)  	! phase function expansion, #7
      real sback2p(ncoef,prefr,prefi,nspint)    ! JCB 2007/02/01 - backscatter

      save :: extp,albp,asmp,ascat,pmom2,pmom3,pmom4,pmom5,pmom6,pmom7,sback2p ! JCB 2007/02/01 - added sback2p
!--------------
      real cext(ncoef),casm(ncoef),cpmom2(ncoef)
      real cscat(ncoef)  ! JCB 2004/02/09
      real cpmom3(ncoef),cpmom4(ncoef),cpmom5(ncoef)
      real cpmom6(ncoef),cpmom7(ncoef)
      real cpsback2p(ncoef) ! JCB 2007/02/09  - backscatter
      integer itab,jtab
      real ttab,utab

!     nsiz = number of wet particle sizes
!     crefin = complex refractive index
      integer n
      real*8 thesize 	! 2 pi radpart / waveleng = size parameter
      real*8 qext(nsiz) ! array of extinction efficiencies
      real*8 qsca(nsiz) ! array of scattering efficiencies
      real*8 gqsc(nsiz) ! array of asymmetry factor * scattering efficiency
      real asymm(nsiz)  ! array of asymmetry factor
      real scat(nsiz)   ! JCB 2004/02/09
      real sb2(nsiz)     ! JCB 2007/02/01 - 4*abs(sback)^2/(size parameter)^2 backscattering efficiency
!     specabs = absorption coeff / unit dry mass
!     specscat = scattering coeff / unit dry mass
      complex*16 crefin,crefd,crefw
      save crefw
      real, save :: rmin,rmax   ! min, max aerosol size bin
      data rmin /0.005e-4/ 	! rmin in cm. 5e-3 microns min allowable size
      data rmax /50.e-4 /	! rmax in cm. 50 microns, big particle, max allowable size

      real bma,bpa

      real, save :: xrmin,xrmax,xr
      real rs(nsiz) ! surface mode radius (cm)
      real xrad ! normalized aerosol radius
      real ch(ncoef) ! chebychev polynomial

      real, save :: rhoh2o ! density of liquid water (g/cm3)
      data rhoh2o/1./

      real refr     ! real part of refractive index
      real refi     ! imaginary part of refractive index
      real qextr4(nsiz)          !  extinction, real*4

      real refrmin ! minimum of real part of refractive index
      real refrmax ! maximum of real part of refractive index
      real refimin ! minimum of imag part of refractive index
      real refimax ! maximum of imag part of refractive index
      real drefr ! increment in real part of refractive index
      real drefi ! increment in imag part of refractive index
      real, save :: refrtab(prefr) ! table of real refractive indices for aerosols
      real, save :: refitab(prefi) ! table of imag refractive indices for aerosols
	complex specrefndx(ltype) ! refractivr indices
      real pie,third

      integer irams, jrams
! diagnostic declarations
      integer kcallmieaer2
      integer ibin
      character*150 msg

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec run_out.25 has aerosol physical parameter info for bins 1-8
!ec and vertical cells 1 to kmaxd.
!        ilaporte = 33
!        jlaporte = 34
	if (iclm .eq. CHEM_DBG_I) then
	  if (jclm .eq. CHEM_DBG_J) then
!   initial entry
	   if (kcallmieaer2 .eq. 0) then
	      write(*,9099)iclm, jclm
 9099	format('for cell i = ', i3, 2x, 'j = ', i3)	
              write(*,9100)
 9100     format(   &
               'isecfrm0', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
      	       'ibin', 3x,   &
               'refindx(ibin,k)', 3x,   &
               'radius_wet(ibin,k)', 3x,   &
               'number_bin(ibin,k)'   &
               )
	   end if
!ec output for run_out.25
            do k = 1, lpar
            do ibin = 1, nbin_a
              write(*, 9120)   &
                 isecfrm0,iclm, jclm, k, ibin,   &
                 refindx(ibin,k),   &
                 radius_wet(ibin,k),   &
                 number_bin(ibin,k)
9120    format( i7,3(2x,i4),2x,i4, 4x, 4(e14.6,2x))
            end do
            end do
	kcallmieaer2 = kcallmieaer2 + 1
        end if
        end if
!ec end print of aerosol physical parameter diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif
!
! assign fast-J wavelength, these values are in cm
!      wavmid(1)=0.30e-4
!      wavmid(2)=0.40e-4
!      wavmid(3)=0.60e-4
!      wavmid(4)=0.999e-04
!
      pie=4.*atan(1.)
      third=1./3.
      if(first)then
        first=.false.

!       parameterize aerosol radiative properties in terms of
!       relative humidity, surface mode wet radius, aerosol species,
!       and wavelength

!       first find min,max of real and imaginary parts of refractive index

!       real and imaginary parts of water refractive index

        crefw=cmplx(1.33,0.0)
        refrmin=real(crefw)
        refrmax=real(crefw)
! change Rahul's imaginary part of the refractive index from positive to negative
        refimin=-imag(crefw)
        refimax=-imag(crefw)

!       aerosol mode loop
        specrefndx(1)=cmplx(1.82,-0.74) ! max values from Rahul, 7 Nov 2002
!
	
	do i=1,ltype ! loop over all possible refractive indices

!             real and imaginary parts of aerosol refractive index

              refrmin=amin1(refrmin,real(specrefndx(ltype)))
              refrmax=amax1(refrmax,real(specrefndx(ltype)))
              refimin=amin1(refimin,aimag(specrefndx(ltype)))
              refimax=amax1(refimax,aimag(specrefndx(ltype)))

           enddo

         drefr=(refrmax-refrmin)
         if(drefr.gt.1.e-4)then
            nrefr=prefr
            drefr=drefr/(nrefr-1)
         else
            nrefr=1
         endif

         drefi=(refimax-refimin)
         if(drefi.gt.1.e-4)then
            nrefi=prefi
            drefi=drefi/(nrefi-1)
         else
            nrefi=1
         endif


!
               bma=0.5*alog(rmax/rmin) ! JCB
               bpa=0.5*alog(rmax*rmin) ! JCB
	xrmin=alog(rmin)
	xrmax=alog(rmax)

!        wavelength loop

         do 200 ns=1,nspint

!           calibrate parameterization with range of refractive indices

            do 120 nr=1,nrefr
            do 120 ni=1,nrefi

               refrtab(nr)=refrmin+(nr-1)*drefr
               refitab(ni)=refimin+(ni-1)*drefi
               crefd=cmplx(refrtab(nr),refitab(ni))

!              mie calculations of optical efficiencies

               do n=1,nsiz
                  xr=cos(pie*(float(n)-0.5)/float(nsiz))
                  rs(n)=exp(xr*bma+bpa)


!                 size parameter and weighted refractive index

                  thesize=2.*pie*rs(n)/wavmid(ns)
                  thesize=min(thesize,10000.d0)

                  call miev0(thesize,crefd,perfct,mimcut,anyang,   &
                     numang,xmu,nmom,ipolzn,momdim,prnt,   &
                     qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,   &
                     s2,tforw,tback )
		  qextr4(n)=qext(n)
!                 qabs(n)=qext(n)-qsca(n) ! not necessary anymore JCB 2004/02/09
                  scat(n)=qsca(n) ! JCB 2004/02/09
                  asymm(n)=gqsc(n)/qsca(n) ! assume always greater than zero
! coefficients of phase function expansion; note modification by JCB of miev0 coefficients
		  p2(n)=pmom(2,1)/pmom(0,1)*5.0
		  p3(n)=pmom(3,1)/pmom(0,1)*7.0
		  p4(n)=pmom(4,1)/pmom(0,1)*9.0
		  p5(n)=pmom(5,1)/pmom(0,1)*11.0
		  p6(n)=pmom(6,1)/pmom(0,1)*13.0
		  p7(n)=pmom(7,1)/pmom(0,1)*15.0
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
		  sb2(n)=4.0*sback*dconjg(sback)/(thesize*thesize) ! JCB 2007/02/01  
		enddo
  100          continue
!
               call fitcurv(rs,qextr4,extp(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,scat,ascat(1,nr,ni,ns),ncoef,nsiz) ! JCB 2004/02/07 - scattering efficiency
               call fitcurv(rs,asymm,asmp(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p2,pmom2(1,nr,ni,ns),ncoef,nsiz)
	       call fitcurv_nolog(rs,p3,pmom3(1,nr,ni,ns),ncoef,nsiz)
	       call fitcurv_nolog(rs,p4,pmom4(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p5,pmom5(1,nr,ni,ns),ncoef,nsiz)
	       call fitcurv_nolog(rs,p6,pmom6(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p7,pmom7(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,sb2,sback2p(1,nr,ni,ns),ncoef,nsiz) ! JCB 2007/02/01 - backscattering efficiency             

  120       continue
  200    continue


      endif
! begin level loop
	do 2000 klevel=1,lpar
! sum densities for normalization
	thesum=0.0
	do m=1,nbin_a
	thesum=thesum+number_bin(m,klevel)
	enddo
! Begin spectral loop
      do 1000 ns=1,nspint

!        aerosol optical properties

               tauaer(ns,klevel)=0.
               waer(ns,klevel)=0.
               gaer(ns,klevel)=0.
	       sizeaer(ns,klevel)=0.0
		extaer(ns,klevel)=0.0
		l2(ns,klevel)=0.0
		l3(ns,klevel)=0.0
		l4(ns,klevel)=0.0
		l5(ns,klevel)=0.0
		l6(ns,klevel)=0.0
		l7(ns,klevel)=0.0
		bscoef(ns,klevel)=0.0  ! JCB 2007/02/01 - backscattering coefficient
		if(thesum.le.1e-21)goto 1000 ! set everything = 0 if no aerosol !wig changed 0.0 to 1e-21, 31-Oct-2005

! loop over the bins
               do m=1,nbin_a ! nbin_a is number of bins
! check to see if there's any aerosol
		if(number_bin(m,klevel).le.1e-21)goto 70  ! no aerosol !wig changed 0.0 to 1e-21, 31-Oct-2005
! here's the size
		sizem=radius_wet(m,klevel) ! radius in cm
! check limits of particle size
! rce 2004-dec-07 - use klevel in write statements
		if(radius_wet(m,klevel).le.rmin)then
		  radius_wet(m,klevel)=rmin
		  write( msg, '(a, 5i4,1x, e11.4)' )	&
		  'FASTJ mie: radius_wet set to rmin,'  //	&
		  'id,i,j,k,m,rm(m,k)', id, iclm, jclm, klevel, m, radius_wet(m,klevel)
		  call peg_message( lunerr, msg )
!		   write(6,'('' particle size too small '')')
		endif
!
		if(radius_wet(m,klevel).gt.rmax)then
		   write( msg, '(a, 5i4,1x, e11.4)' )	&
                'FASTJ mie: radius_wet set to rmax,'  //	&
                'id,i,j,k,m,rm(m,k)', &
                id, iclm, jclm, klevel, m, radius_wet(m,klevel)
           call peg_message( lunerr, msg )
           radius_wet(m,klevel)=rmax
!           write(6,'('' particle size too large '')')
		endif
!
		x=alog(radius_wet(m,klevel)) ! radius in cm
!
	          crefin=refindx(m,klevel)
                  refr=real(crefin)
! change Rahul's imaginary part of the index of refraction from positive to negative
                  refi=-imag(crefin)
!
		xrad=x

		thesize=2.0*pie*exp(x)/wavmid(ns)
! normalize size parameter
                   xrad=(2*xrad-xrmax-xrmin)/(xrmax-xrmin)
! retain this diagnostic code
		  if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'FASTJ mie /refr/ outside range 1e-3 - 10 ' //  &
           'refr= ', refr
!                      print *,'refr=',refr
         	     call peg_error_fatal( lunerr, msg )
                  endif
                  if(abs(refi).gt.10.)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'FASTJ mie /refi/ >10 '  //  &
            'refi', refi
!                      print *,'refi=',refi
         	     call peg_error_fatal( lunerr, msg )                  
                  endif

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  call binterp(extp(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cext)

! JCB 2004/02/09  -- new code for scattering cross section
                  call binterp(ascat(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cscat)
                  call binterp(asmp(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,casm)
                  call binterp(pmom2(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom2)
                  call binterp(pmom3(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom3)
                  call binterp(pmom4(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom4)
                  call binterp(pmom5(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom5)
                  call binterp(pmom6(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom6)
                  call binterp(pmom7(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpmom7)
                  call binterp(sback2p(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtab,refitab,itab,jtab,   &
                               ttab,utab,cpsback2p)

!                 chebyshev polynomials

                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo
!                 parameterized optical properties

                     pext=0.5*cext(1)
                     do nc=2,ncoef
                        pext=pext+ch(nc)*cext(nc)
                     enddo
                    pext=exp(pext)
	
! JCB 2004/02/09 -- for scattering efficiency
                  pscat=0.5*cscat(1)
                  do nc=2,ncoef
                     pscat=pscat+ch(nc)*cscat(nc)
                  enddo
                  pscat=exp(pscat)
!
                  pasm=0.5*casm(1)
                  do nc=2,ncoef
                     pasm=pasm+ch(nc)*casm(nc)
                  enddo
                  pasm=exp(pasm)
!
                  ppmom2=0.5*cpmom2(1)
                  do nc=2,ncoef
                     ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                  enddo
		  if(ppmom2.le.0.0)ppmom2=0.0
!
                  ppmom3=0.5*cpmom3(1)
                  do nc=2,ncoef
                     ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                  enddo
!		  ppmom3=exp(ppmom3)  ! no exponentiation required
		  if(ppmom3.le.0.0)ppmom3=0.0
!
                  ppmom4=0.5*cpmom4(1)
                  do nc=2,ncoef
                     ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                  enddo
		  if(ppmom4.le.0.0.or.sizem.le.0.03e-04)ppmom4=0.0
!
                  ppmom5=0.5*cpmom5(1)
                  do nc=2,ncoef
                     ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                  enddo
		  if(ppmom5.le.0.0.or.sizem.le.0.03e-04)ppmom5=0.0
!
                  ppmom6=0.5*cpmom6(1)
                  do nc=2,ncoef
                     ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                  enddo
		  if(ppmom6.le.0.0.or.sizem.le.0.03e-04)ppmom6=0.0
!
                  ppmom7=0.5*cpmom7(1)
                  do nc=2,ncoef
                     ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                  enddo
		  if(ppmom7.le.0.0.or.sizem.le.0.03e-04)ppmom7=0.0
!
                  sback2=0.5*cpsback2p(1) ! JCB 2007/02/01 - backscattering efficiency
                  do nc=2,ncoef
                     sback2=sback2+ch(nc)*cpsback2p(nc)
                  enddo
                     sback2=exp(sback2)
		  if(sback2.le.0.0)sback2=0.0
!
!
! weights:
	weighte=pext*pie*exp(x)**2 ! JCB, extinction cross section
	weights=pscat*pie*exp(x)**2 ! JCB, scattering cross section
       	tauaer(ns,klevel)=tauaer(ns,klevel)+weighte*number_bin(m,klevel)  ! must be multiplied by deltaZ
!	extaer(ns,klevel)=extaer(ns,klevel)+pext*number_bin(m,klevel) ! not the true extinction, JCB 2007/02/01
	sizeaer(ns,klevel)=sizeaer(ns,klevel)+exp(x)*10000.0*   &
      	number_bin(m,klevel)
        waer(ns,klevel)=waer(ns,klevel)+weights*number_bin(m,klevel) !JCB
        gaer(ns,klevel)=gaer(ns,klevel)+pasm*weights*   &
        number_bin(m,klevel) !JCB
! need weighting by scattering cross section ?  JCB 2004/02/09
	l2(ns,klevel)=l2(ns,klevel)+weights*ppmom2*number_bin(m,klevel)
	l3(ns,klevel)=l3(ns,klevel)+weights*ppmom3*number_bin(m,klevel)
	l4(ns,klevel)=l4(ns,klevel)+weights*ppmom4*number_bin(m,klevel)
	l5(ns,klevel)=l5(ns,klevel)+weights*ppmom5*number_bin(m,klevel)
	l6(ns,klevel)=l6(ns,klevel)+weights*ppmom6*number_bin(m,klevel)
	l7(ns,klevel)=l7(ns,klevel)+weights*ppmom7*number_bin(m,klevel)	
! convert backscattering efficiency to backscattering coefficient, units (cm)^-1
	bscoef(ns,klevel)=bscoef(ns,klevel)+pie*exp(x)**2*sback2*number_bin(m,klevel) ! JCB 2007/02/01 - backscatter coefficient,

        end do ! end of nbin_a loop
! take averages - weighted by cross section - new code JCB 2004/02/09
	sizeaer(ns,klevel)=sizeaer(ns,klevel)/thesum
	gaer(ns,klevel)=gaer(ns,klevel)/waer(ns,klevel) ! JCB removed *3 factor 2/9/2004
! because factor is applied in subroutine opmie, file zz01fastj_mod.f
	l2(ns,klevel)=l2(ns,klevel)/waer(ns,klevel)
	l3(ns,klevel)=l3(ns,klevel)/waer(ns,klevel)
	l4(ns,klevel)=l4(ns,klevel)/waer(ns,klevel)
	l5(ns,klevel)=l5(ns,klevel)/waer(ns,klevel)
	l6(ns,klevel)=l6(ns,klevel)/waer(ns,klevel)
	l7(ns,klevel)=l7(ns,klevel)/waer(ns,klevel)
! backscatter coef, divide by 4*Pie to get units of (km*ster)^-1 JCB 2007/02/01
	bscoef(ns,klevel)=bscoef(ns,klevel)*1.0e5  ! JCB 2007/02/01 - units are now (km)^-1
	extaer(ns,klevel)=tauaer(ns,klevel)*1.0e5  ! JCB 2007/02/01 - now true extincion, units (km)^-1
! this must be last!! JCB 2007/02/01
	waer(ns,klevel)=waer(ns,klevel)/tauaer(ns,klevel) ! JCB

 70   continue ! bail out if no aerosol;go on to next wavelength bin

 1000 continue  ! end of wavelength loop


2000   continue  ! end of klevel loop
!
! before returning, multiply tauaer by depth of individual cells.
! tauaer is in cm-1, dz in m; multiply dz by 100 to convert from m to cm.
  	do ns = 1, nspint
  	do klevel = 1, lpar
  	   tauaer(ns,klevel) = tauaer(ns,klevel) * dz(klevel)* 100.   
        end do
        end do 	

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  fastj diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec run_out.30 has aerosol optical info for cells 1 to kmaxd.
!        ilaporte = 33
!         jlaporte = 34
 	if (iclm .eq. CHEM_DBG_I) then
 	  if (jclm .eq. CHEM_DBG_J) then
!   initial entry
 	   if (kcallmieaer .eq. 0) then
 	       write(*,909) CHEM_DBG_I, CHEM_DBG_J
 909	format(	' for cell i = ', i3, ' j = ', i3)          	
               write(*,910)
 910     format(   &
               'isecfrm0', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
      	        'dzmfastj', 8x,   &
               'tauaer(1,k)',1x, 'tauaer(2,k)',1x,'tauaer(3,k)',3x,   &
               'tauaer(4,k)',5x,   &
               'waer(1,k)', 7x, 'waer(2,k)', 7x,'waer(3,k)', 7x,   &
               'waer(4,k)', 7x,   &
               'gaer(1,k)', 7x, 'gaer(2,k)', 7x,'gaer(3,k)', 7x,   &
               'gaer(4,k)', 7x,   &
               'extaer(1,k)',5x, 'extaer(2,k)',5x,'extaer(3,k)',5x,   &
               'extaer(4,k)',5x,   &
               'sizeaer(1,k)',4x, 'sizeaer(2,k)',4x,'sizeaer(3,k)',4x,   &
               'sizeaer(4,k)'  )
 	   end if
!ec output for run_out.30
         do k = 1, lpar
         write(*, 912)   &
           isecfrm0,iclm, jclm, k,   &
           dz(k) ,   &
           (tauaer(n,k),   n=1,4),   &
           (waer(n,k),     n=1,4),   &
           (gaer(n,k),     n=1,4),   &
           (extaer(n,k),   n=1,4),   &
           (sizeaer(n,k),  n=1,4)
 912    format( i7,3(2x,i4),2x,21(e14.6,2x))
         end do
 	kcallmieaer = kcallmieaer + 1
        end if
         end if
!ec end print of fastj diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif

      return
      end subroutine mieaer                    
!****************************************************************
      subroutine fitcurv(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

      USE module_peg_util, only:  peg_message

      IMPLICIT NONE
!      integer nmodes, nrows, maxm, ncoef
!      parameter (nmodes=500,nrows=8)
      integer, intent(in) :: maxm, ncoef

!      real rs(nmodes),yin(nmodes),coef(ncoef)
!      real x(nmodes),y(nmodes)
      real, dimension(ncoef) :: coef
      real, dimension(:) :: rs, yin
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg

!!$      if(maxm.gt.nmodes)then
!!$        write ( msg, '(a, 1x,i6)' )  &
!!$           'FASTJ mie nmodes too small in fitcurv, '  //  &
!!$           'maxm ', maxm
!!$!        write(*,*)'nmodes too small in fitcurv',maxm
!!$          call peg_error_fatal( lunerr, msg )
!!$      endif

      do 100 m=1,maxm
      x(m)=alog(rs(m))
      y(m)=alog(yin(m))
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv                        
!**************************************************************
      subroutine fitcurv_nolog(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

      USE module_peg_util, only:  peg_message
      IMPLICIT NONE

!      integer nmodes, nrows, maxm, ncoef
!      parameter (nmodes=500,nrows=8)
      integer, intent(in) :: maxm, ncoef

!      real rs(nmodes),yin(nmodes),coef(ncoef)
      real, dimension(:) :: rs, yin
      real, dimension(ncoef) :: coef(ncoef)
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg
           
!!$      if(maxm.gt.nmodes)then
!!$        write ( msg, '(a,1x, i6)' )  &
!!$           'FASTJ mie nmodes too small in fitcurv '  //  &
!!$           'maxm ', maxm
!!$!        write(*,*)'nmodes too small in fitcurv',maxm
!!$          call peg_error_fatal( lunerr, msg )
!!$      endif

      do 100 m=1,maxm
      x(m)=alog(rs(m))
      y(m)=yin(m) ! note, no "alog" here
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv_nolog                        
!************************************************************************
      subroutine chebft(c,ncoef,n,f)
!     given a function f with values at zeroes x_k of Chebychef polynomial
!     T_n(x), calculate coefficients c_j such that
!     f(x)=sum(k=1,n) c_k t_(k-1)(y) - 0.5*c_1
!     where y=(x-0.5*(xmax+xmin))/(0.5*(xmax-xmin))
!     See Numerical Recipes, pp. 148-150.

      IMPLICIT NONE
      real pi
      integer ncoef, n
      parameter (pi=3.14159265)
      real c(ncoef),f(n)

! local variables      
      real fac, thesum
      integer j, k
      
      fac=2./n
      do j=1,ncoef
         thesum=0
         do k=1,n
            thesum=thesum+f(k)*cos((pi*(j-1))*((k-0.5)/n))
         enddo
         c(j)=fac*thesum
      enddo
      return
      end subroutine chebft             
!*************************************************************************
      subroutine binterp(table,km,im,jm,x,y,xtab,ytab,ix,jy,t,u,out)

!     bilinear interpolation of table
!
      implicit none
      integer im,jm,km
      real table(km,im,jm),xtab(im),ytab(jm),out(km)
      integer i,ix,ip1,j,jy,jp1,k
      real x,dx,t,y,dy,u,tu,  tuc,tcu,tcuc

      if(ix.gt.0)go to 30
      if(im.gt.1)then
        do i=1,im
          if(x.lt.xtab(i))go to 10
        enddo
   10   ix=max0(i-1,1)
        ip1=min0(ix+1,im)
        dx=(xtab(ip1)-xtab(ix))
        if(abs(dx).gt.1.e-20)then
           t=(x-xtab(ix))/(xtab(ix+1)-xtab(ix))
        else
           t=0
        endif
      else
        ix=1
        ip1=1
        t=0
      endif
      if(jm.gt.1)then
        do j=1,jm
          if(y.lt.ytab(j))go to 20
        enddo
   20   jy=max0(j-1,1)
        jp1=min0(jy+1,jm)
        dy=(ytab(jp1)-ytab(jy))
        if(abs(dy).gt.1.e-20)then
           u=(y-ytab(jy))/dy
        else
           u=0
        endif
      else
        jy=1
        jp1=1
        u=0
      endif
   30 continue
      jp1=min(jy+1,jm)
      ip1=min(ix+1,im)
      tu=t*u
      tuc=t-tu
      tcuc=1-tuc-u
      tcu=u-tu
      do k=1,km
         out(k)=tcuc*table(k,ix,jy)+tuc*table(k,ip1,jy)   &
               +tu*table(k,ip1,jp1)+tcu*table(k,ix,jp1)
      enddo
      return
      end subroutine binterp                                            
!***************************************************************
      subroutine  miev0 ( xx, crefin, perfct, mimcut, anyang,   &
                          numang, xmu, nmom, ipolzn, momdim, prnt,   &
                          qext, qsca, gqsc, pmom, sforw, sback, s1,   &
                          s2, tforw, tback )
!
!    computes mie scattering and extinction efficiencies; asymmetry
!    factor;  forward- and backscatter amplitude;  scattering
!    amplitudes for incident polarization parallel and perpendicular
!    to the plane of scattering, as functions of scattering angle;
!    coefficients in the legendre polynomial expansions of either the
!    unpolarized phase function or the polarized phase matrix;
!    and some quantities needed in polarized radiative transfer.
!
!      calls :  biga, ckinmi, small1, small2, testmi, miprnt,
!               lpcoef, errmsg
!
!      i n t e r n a l   v a r i a b l e s
!      -----------------------------------
!
!  an,bn           mie coefficients  little-a-sub-n, little-b-sub-n
!                     ( ref. 1, eq. 16 )
!  anm1,bnm1       mie coefficients  little-a-sub-(n-1),
!                     little-b-sub-(n-1);  used in -gqsc- sum
!  anp             coeffs. in s+ expansion ( ref. 2, p. 1507 )
!  bnp             coeffs. in s- expansion ( ref. 2, p. 1507 )
!  anpm            coeffs. in s+ expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  bnpm            coeffs. in s- expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  calcmo(k)       true, calculate moments for k-th phase quantity
!                     (derived from -ipolzn-; used only in 'lpcoef')
!  cbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( complex version )
!  cior            complex index of refraction with negative
!                     imaginary part (van de hulst convention)
!  cioriv          1 / cior
!  coeff           ( 2n + 1 ) / ( n ( n + 1 ) )
!  fn              floating point version of index in loop performing
!                     mie series summation
!  lita,litb(n)    mie coefficients -an-, -bn-, saved in arrays for
!                     use in calculating legendre moments *pmom*
!  maxtrm          max. possible no. of terms in mie series
!  mm              + 1 and  - 1,  alternately.
!  mim             magnitude of imaginary refractive index
!  mre             real part of refractive index
!  maxang          max. possible value of input variable -numang-
!  nangd2          (numang+1)/2 ( no. of angles in 0-90 deg; anyang=f )
!  noabs           true, sphere non-absorbing (determined by -mimcut-)
!  np1dn           ( n + 1 ) / n
!  npquan          highest-numbered phase quantity for which moments are
!                     to be calculated (the largest digit in -ipolzn-
!                     if  ipolzn .ne. 0)
!  ntrm            no. of terms in mie series
!  pass1           true on first entry, false thereafter; for self-test
!  pin(j)          angular function little-pi-sub-n ( ref. 2, eq. 3 )
!                     at j-th angle
!  pinm1(j)        little-pi-sub-(n-1) ( see -pin- ) at j-th angle
!  psinm1          ricatti-bessel function psi-sub-(n-1), argument -xx-
!  psin            ricatti-bessel function psi-sub-n of argument -xx-
!                     ( ref. 1, p. 11 ff. )
!  rbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( real version, for when imag refrac index = 0 )
!  rioriv          1 / mre
!  rn              1 / n
!  rtmp            (real) temporary variable
!  sp(j)           s+  for j-th angle  ( ref. 2, p. 1507 )
!  sm(j)           s-  for j-th angle  ( ref. 2, p. 1507 )
!  sps(j)          s+  for (numang+1-j)-th angle ( anyang=false )
!  sms(j)          s-  for (numang+1-j)-th angle ( anyang=false )
!  taun            angular function little-tau-sub-n ( ref. 2, eq. 4 )
!                     at j-th angle
!  tcoef           n ( n+1 ) ( 2n+1 ) (for summing tforw,tback series)
!  twonp1          2n + 1
!  yesang          true if scattering amplitudes are to be calculated
!  zetnm1          ricatti-bessel function  zeta-sub-(n-1) of argument
!                     -xx-  ( ref. 2, eq. 17 )
!  zetn            ricatti-bessel function  zeta-sub-n of argument -xx-
!
! ----------------------------------------------------------------------
! --------  i / o specifications for subroutine miev0  -----------------
! ----------------------------------------------------------------------
      implicit none
      logical  anyang, perfct, prnt(*)
      integer  ipolzn, momdim, numang, nmom
      real*8     gqsc, mimcut, pmom( 0:momdim, * ), qext, qsca,   &
               xmu(*), xx
      complex*16  crefin, sforw, sback, s1(*), s2(*), tforw(*),   &
               tback(*)
      integer maxang,mxang2,maxtrm
      real*8 onethr
! ----------------------------------------------------------------------
!
      parameter ( maxang = 501, mxang2 = maxang/2 + 1 )
!
!                                  ** note --  maxtrm = 10100  is neces-
!                                  ** sary to do some of the test probs,
!                                  ** but 1100 is sufficient for most
!                                  ** conceivable applications
      parameter ( maxtrm = 1100 )
      parameter ( onethr = 1./3. )
!
      logical   anysav, calcmo(4), noabs, ok, pass1, persav, yesang
      integer   npquan
      integer i,j,n,nmosav,iposav,numsav,ntrm,nangd2
      real*8      mim, mimsav, mre, mm, np1dn
      real*8 rioriv,xmusav,xxsav,sq,fn,rn,twonp1,tcoef, coeff
      real*8 xinv,psinm1,chinm1,psin,chin,rtmp,taun
      real*8      rbiga( maxtrm ), pin( maxang ), pinm1( maxang )
      complex*16   an, bn, anm1, bnm1, anp, bnp, anpm, bnpm, cresav,   &
                cior, cioriv, ctmp, zet, zetnm1, zetn
      complex*16   cbiga( maxtrm ), lita( maxtrm ), litb( maxtrm ),   &
                sp( maxang ), sm( maxang ), sps( mxang2 ), sms( mxang2 )
      equivalence  ( cbiga, rbiga )
      save  pass1
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!                                   ** save certain user input values
         xxsav  = xx
         cresav = crefin
         mimsav = mimcut
         persav = perfct
         anysav = anyang
         nmosav = nmom
         iposav = ipolzn
         numsav = numang
         xmusav = xmu( 1 )
!                              ** reset input values for test case
         xx      = 10.0
         crefin  = ( 1.5, - 0.1 )
         perfct  = .false.
         mimcut  = 0.0
         anyang  = .true.
         numang  = 1
         xmu( 1 )= - 0.7660444
         nmom    = 1
         ipolzn  = - 1
!
      end if
!                                        ** check input and calculate
!                                        ** certain variables from input
!
   10 call  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                    nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
      if ( perfct .and. xx .le. 0.1 )  then
!                                            ** use totally-reflecting
!                                            ** small-particle limit
!
         call  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                        sback, s1, s2, tforw, tback, lita, litb )
         ntrm = 2
         go to 200
      end if
!
      if ( .not.perfct )  then
!
         cior = crefin
         if ( dimag( cior ) .gt. 0.0 )  cior = dconjg( cior )
         mre =     dble( cior )
         mim =  - dimag( cior )
         noabs = mim .le. mimcut
         cioriv = 1.0 / cior
         rioriv = 1.0 / mre
!
         if ( xx * dmax1( 1.d0, cdabs(cior) ) .le. 0.d1 ) then
!
!                                    ** use general-refractive-index
!                                    ** small-particle limit
!                                    ** ( ref. 2, p. 1508 )
!
            call  small2 ( xx, cior, .not.noabs, numang, xmu, qext,   &
                           qsca, gqsc, sforw, sback, s1, s2, tforw,   &
                           tback, lita, litb )
            ntrm = 2
            go to 200
         end if
!
      end if
!
      nangd2 = ( numang + 1 ) / 2
      yesang = numang .gt. 0
!                              ** estimate number of terms in mie series
!                              ** ( ref. 2, p. 1508 )
      if ( xx.le.8.0 )  then
         ntrm = xx + 4. * xx**onethr + 1.
      else if ( xx.lt.4200. )  then
         ntrm = xx + 4.05 * xx**onethr + 2.
      else
         ntrm = xx + 4. * xx**onethr + 2.
      end if
      if ( ntrm+1 .gt. maxtrm )   &
           call errmsg( 'miev0--parameter maxtrm too small', .true. )
!
!                            ** calculate logarithmic derivatives of
!                            ** j-bessel-fcn., big-a-sub-(1 to ntrm)
      if ( .not.perfct )   &
           call  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
!
!                            ** initialize ricatti-bessel functions
!                            ** (psi,chi,zeta)-sub-(0,1) for upward
!                            ** recurrence ( ref. 1, eq. 19 )
      xinv = 1.0 / xx
      psinm1   = dsin( xx )
      chinm1   = dcos( xx )
      psin = psinm1 * xinv - chinm1
      chin = chinm1 * xinv + psinm1
      zetnm1 = dcmplx( psinm1, chinm1 )
      zetn   = dcmplx( psin, chin )
!                                     ** initialize previous coeffi-
!                                     ** cients for -gqsc- series
      anm1 = ( 0.0, 0.0 )
      bnm1 = ( 0.0, 0.0 )
!                             ** initialize angular function little-pi
!                             ** and sums for s+, s- ( ref. 2, p. 1507 )
      if ( anyang )  then
         do  60  j = 1, numang
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
   60    continue
      else
         do  70  j = 1, nangd2
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
            sps( j ) = ( 0.0, 0.0 )
            sms( j ) = ( 0.0, 0.0 )
   70    continue
      end if
!                         ** initialize mie sums for efficiencies, etc.
      qsca = 0.0
      gqsc = 0.0
      sforw      = ( 0., 0. )
      sback      = ( 0., 0. )
      tforw( 1 ) = ( 0., 0. )
      tback( 1 ) = ( 0., 0. )
!
!
! ---------  loop to sum mie series  -----------------------------------
!
      mm = + 1.0
      do  100  n = 1, ntrm
!                           ** compute various numerical coefficients
         fn     = n
         rn     = 1.0 / fn
         np1dn  = 1.0 + rn
         twonp1 = 2 * n + 1
         coeff  = twonp1 / ( fn * ( n + 1 ) )
         tcoef  = twonp1 * ( fn * ( n + 1 ) )
!
!                              ** calculate mie series coefficients
         if ( perfct )  then
!                                   ** totally-reflecting case
!
            an = ( ( fn*xinv ) * psin - psinm1 ) /   &
                 ( ( fn*xinv ) * zetn - zetnm1 )
            bn = psin / zetn
!
         else if ( noabs )  then
!                                      ** no-absorption case
!
            an =  ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn =  ( (  mre * rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( (  mre * rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
         else
!                                       ** absorptive case
!
            an = ( ( cioriv * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( ( cioriv * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn = ( (   cior * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( (   cior * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            qsca = qsca + twonp1 * ( sq( an ) + sq( bn ) )
!
         end if
!                       ** save mie coefficients for *pmom* calculation
         lita( n ) = an
         litb( n ) = bn
!                            ** increment mie sums for non-angle-
!                            ** dependent quantities
!
         sforw      = sforw      + twonp1 * ( an + bn )
         tforw( 1 ) = tforw( 1 ) + tcoef  * ( an - bn )
         sback      = sback      + ( mm * twonp1 ) * ( an - bn )
         tback( 1 ) = tback( 1 ) + ( mm * tcoef )  * ( an + bn )
         gqsc = gqsc + ( fn - rn ) * dble( anm1 * dconjg( an )   &
                                         + bnm1 * dconjg( bn ) )   &
                + coeff * dble( an * dconjg( bn ) )
!
         if ( yesang )  then
!                                      ** put mie coefficients in form
!                                      ** needed for computing s+, s-
!                                      ** ( ref. 2, p. 1507 )
            anp = coeff * ( an + bn )
            bnp = coeff * ( an - bn )
!                                      ** increment mie sums for s+, s-
!                                      ** while upward recursing
!                                      ** angular functions little pi
!                                      ** and little tau
            if ( anyang )  then
!                                         ** arbitrary angles
!
!                                              ** vectorizable loop
               do  80  j = 1, numang
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp( j )  = sp( j ) + anp * ( pin( j ) + taun )
                  sm( j )  = sm( j ) + bnp * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   80          continue
!
            else
!                                  ** angles symmetric about 90 degrees
               anpm = mm * anp
               bnpm = mm * bnp
!                                          ** vectorizable loop
               do  90  j = 1, nangd2
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp ( j ) = sp ( j ) +  anp * ( pin( j ) + taun )
                  sms( j ) = sms( j ) + bnpm * ( pin( j ) + taun )
                  sm ( j ) = sm ( j ) +  bnp * ( pin( j ) - taun )
                  sps( j ) = sps( j ) + anpm * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   90          continue
!
            end if
         end if
!                          ** update relevant quantities for next
!                          ** pass through loop
         mm   =  - mm
         anm1 = an
         bnm1 = bn
!                           ** upward recurrence for ricatti-bessel
!                           ** functions ( ref. 1, eq. 17 )
!
         zet    = ( twonp1 * xinv ) * zetn - zetnm1
         zetnm1 = zetn
         zetn   = zet
         psinm1 = psin
         psin   = dble( zetn )
  100 continue
!
! ---------- end loop to sum mie series --------------------------------
!
!
      qext = 2. / xx**2 * dble( sforw )
      if ( perfct .or. noabs )  then
         qsca = qext
      else
         qsca = 2. / xx**2 * qsca
      end if
!
      gqsc = 4. / xx**2 * gqsc
      sforw = 0.5 * sforw
      sback = 0.5 * sback
      tforw( 2 ) = 0.5 * (   sforw + 0.25 * tforw( 1 ) )
      tforw( 1 ) = 0.5 * (   sforw - 0.25 * tforw( 1 ) )
      tback( 2 ) = 0.5 * (   sback + 0.25 * tback( 1 ) )
      tback( 1 ) = 0.5 * ( - sback + 0.25 * tback( 1 ) )
!
      if ( yesang )  then
!                                ** recover scattering amplitudes
!                                ** from s+, s- ( ref. 1, eq. 11 )
         if ( anyang )  then
!                                         ** vectorizable loop
            do  110  j = 1, numang
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  110       continue
!
         else
!                                         ** vectorizable loop
            do  120  j = 1, nangd2
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  120       continue
!                                         ** vectorizable loop
            do  130  j = 1, nangd2
               s1( numang+1 - j ) = 0.5 * ( sps( j ) + sms( j ) )
               s2( numang+1 - j ) = 0.5 * ( sps( j ) - sms( j ) )
  130       continue
         end if
!
      end if
!                                         ** calculate legendre moments
  200 if ( nmom.gt.0 )   &
           call lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                         lita, litb, pmom )
!
      if ( dimag(crefin) .gt. 0.0 )  then
!                                         ** take complex conjugates
!                                         ** of scattering amplitudes
         sforw = dconjg( sforw )
         sback = dconjg( sback )
         do  210  i = 1, 2
            tforw( i ) = dconjg( tforw(i) )
            tback( i ) = dconjg( tback(i) )
  210    continue
!
         do  220  j = 1, numang
            s1( j ) = dconjg( s1(j) )
            s2( j ) = dconjg( s2(j) )
  220    continue
!
      end if
!
      if ( pass1 )  then
!                             ** compare test case results with
!                             ** correct answers and abort if bad
!
         call  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                        tforw, tback, pmom, momdim, ok )
         if ( .not. ok )  then
            prnt(1) = .false.
            prnt(2) = .false.
            call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                          qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                          pmom, sforw, sback, tforw, tback, s1, s2 )
            call errmsg( 'miev0 -- self-test failed', .true. )
         end if
!                                       ** restore user input values
         xx     = xxsav
         crefin = cresav
         mimcut = mimsav
         perfct = persav
         anyang = anysav
         nmom   = nmosav
         ipolzn = iposav
         numang = numsav
         xmu(1) = xmusav
         pass1 = .false.
         go to 10
!
      end if
!
      if ( prnt(1) .or. prnt(2) )   &
         call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                       qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                       pmom, sforw, sback, tforw, tback, s1, s2 )
!
      return
!
      end subroutine  miev0 
!****************************************************************************                                           
      subroutine  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                          nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
!        check for bad input to 'miev0' and calculate -calcmo,npquan-
!
      implicit none
      logical  perfct, anyang, calcmo(*)
      integer  numang, maxang, momdim, nmom, ipolzn, npquan
      real*8    xx, xmu(*)
      integer i,l,j,ip
      complex*16  crefin
!
      character*4  string
      logical  inperr
!
      inperr = .false.
!
      if ( numang.gt.maxang )  then
         call errmsg( 'miev0--parameter maxang too small', .true. )
         inperr = .true.
      end if
      if ( numang.lt.0 )  call  wrtbad( 'numang', inperr )
      if ( xx.lt.0. )     call  wrtbad( 'xx', inperr )
      if ( .not.perfct .and. dble(crefin).le.0. )   &
           call wrtbad( 'crefin', inperr )
      if ( momdim.lt.1 )  call wrtbad( 'momdim', inperr )
!
      if ( nmom.ne.0 )  then
         if ( nmom.lt.0 .or. nmom.gt.momdim ) call wrtbad('nmom',inperr)
         if ( iabs(ipolzn).gt.4444 )  call  wrtbad( 'ipolzn', inperr )
         npquan = 0
         do 5  l = 1, 4
            calcmo( l ) = .false.
    5    continue
         if ( ipolzn.ne.0 )  then
!                                 ** parse out -ipolzn- into its digits
!                                 ** to find which phase quantities are
!                                 ** to have their moments calculated
!
            write( string, '(i4)' )  iabs(ipolzn)
            do 10  j = 1, 4
               ip = ichar( string(j:j) ) - ichar( '0' )
               if ( ip.ge.1 .and. ip.le.4 )  calcmo( ip ) = .true.
               if ( ip.eq.0 .or. (ip.ge.5 .and. ip.le.9) )   &
                    call  wrtbad( 'ipolzn', inperr )
               npquan = max0( npquan, ip )
   10       continue
         end if
      end if
!
      if ( anyang )  then
!                                ** allow for slight imperfections in
!                                ** computation of cosine
          do  20  i = 1, numang
             if ( xmu(i).lt.-1.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   20     continue
      else
          do  22  i = 1, ( numang + 1 ) / 2
             if ( xmu(i).lt.-0.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   22     continue
      end if
!
      if ( inperr )   &
           call errmsg( 'miev0--input error(s).  aborting...', .true. )
!
      if ( xx.gt.20000.0 .or. dble(crefin).gt.10.0 .or.   &
           dabs( dimag(crefin) ).gt.10.0 )  call  errmsg(   &
           'miev0--xx or crefin outside tested range', .false. )
!
      return
      end subroutine  ckinmi
!***********************************************************************                                                   
      subroutine  lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                           a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities ( ref. 5 formulation )
!
!     input:  ntrm                    number terms in mie series
!             nmom, ipolzn, momdim    'miev0' arguments
!             calcmo                  flags calculated from -ipolzn-
!             npquan                  defined in 'miev0'
!             a, b                    mie series coefficients
!
!     output: pmom                   legendre moments ('miev0' argument)
!
!     *** notes ***
!
!         (1)  eqs. 2-5 are in error in dave, appl. opt. 9,
!         1888 (1970).  eq. 2 refers to m1, not m2;  eq. 3 refers to
!         m2, not m1.  in eqs. 4 and 5, the subscripts on the second
!         term in square brackets should be interchanged.
!
!         (2)  the general-case logic in this subroutine works correctly
!         in the two-term mie series case, but subroutine  'lpco2t'
!         is called instead, for speed.
!
!         (3)  subroutine  'lpco1t', to do the one-term case, is never
!         called within the context of 'miev0', but is included for
!         complete generality.
!
!         (4)  some improvement in speed is obtainable by combining the
!         310- and 410-loops, if moments for both the third and fourth
!         phase quantities are desired, because the third phase quantity
!         is the real part of a complex series, while the fourth phase
!         quantity is the imaginary part of that very same series.  but
!         most users are not interested in the fourth phase quantity,
!         which is related to circular polarization, so the present
!         scheme is usually more efficient.
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom, ntrm, npquan
      real*8    pmom( 0:momdim, * )
      complex*16  a(*), b(*)
!
!           ** specification of local variables
!
!      am(m)       numerical coefficients  a-sub-m-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bi(i)       numerical coefficients  b-sub-i-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bidel(i)    1/2 bi(i) times factor capital-del in dave
!
!      cm,dm()     arrays c and d in dave, eqs. 16-17 (mueller form),
!                     calculated using recurrence derived in ref. 5
!
!      cs,ds()     arrays c and d in ref. 4, eqs. a5-a6 (sekera form),
!                     calculated using recurrence derived in ref. 5
!
!      c,d()       either -cm,dm- or -cs,ds-, depending on -ipolzn-
!
!      evenl       true for even-numbered moments;  false otherwise
!
!      idel        1 + little-del  in dave
!
!      maxtrm      max. no. of terms in mie series
!
!      maxmom      max. no. of non-zero moments
!
!      nummom      number of non-zero moments
!
!      recip(k)    1 / k
!
      integer maxtrm,maxmom,mxmom2,maxrcp
      parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2,   &
                   maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 ),   &
                 recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm ),   &
                 c( maxtrm ), d( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 thesum
      equivalence  ( c, cm ),  ( d, dm )
      logical    pass1, evenl
      save  pass1, recip
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!
         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.
!
      end if
!
      do  5  j = 1, max0( 1, npquan )
         do  5  l = 0, nmom
            pmom( l, j ) = 0.0
    5 continue
!
      if ( ntrm.eq.1 )  then
         call  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      else if ( ntrm.eq.2 )  then
         call  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      end if
!
      if ( ntrm+2 .gt. maxtrm )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
!
!                                     ** calculate mueller c, d arrays
      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm )   &
                   + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm )   &
                   + ( 1. - recip(ntrm) ) * a( ntrm-1 )
!
      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * a( k )   &
                             + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * b( k )   &
                             + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )
!
      if ( ipolzn.ge.0 )  then
!
         do  20  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cm( k )
            d( k ) = ( 2*k - 1 ) * dm( k )
   20    continue
!
      else
!                                    ** compute sekera c and d arrays
         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )
!
         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue
!
         do  40  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cs( k )
            d( k ) = ( 2*k - 1 ) * ds( k )
   40    continue
!
      end if
!
!
      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
!
!                               ** loop over moments
      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0
!                                    ** calculate numerical coefficients
!                                    ** a-sub-m and b-sub-i in dave
!                                    ** double-sums for moments
         if( l.eq.0 )  then
!
            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0
!
         else if( evenl )  then
!
            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )
!
         else
!
            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue
!
         end if
!                                     ** establish upper limits for sums
!                                     ** and incorporate factor capital-
!                                     ** del into b-sub-i
         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )
!
!                                    ** perform double sums just for
!                                    ** phase quantities desired by user
         if( ipolzn.eq.0 )  then
!
            do  110  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  100  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( c(m+i+idel) ) )   &
                            + dble( d(m-i+1) * dconjg( d(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500
!
         end if
!
         if ( calcmo(1) )  then
            do  160  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  150  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( c(m-i+1) * dconjg( c(m+i+idel) ) )
  150          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  160       continue
         end if
!
!
         if ( calcmo(2) )  then
            do  210  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  200  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( d(m-i+1) * dconjg( d(m+i+idel) ) )
  200          continue
               pmom( l,2 ) = pmom( l,2 ) + bidel( i ) * thesum
  210       continue
         end if
!
!
         if ( calcmo(3) )  then
            do  310  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  300  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dble( c(m+i+idel) * dconjg( d(m-i+1) ) ) )
  300          continue
               pmom( l,3 ) = pmom( l,3 ) + bidel( i ) * thesum
  310       continue
            pmom( l,3 ) = 0.5 * pmom( l,3 )
         end if
!
!
         if ( calcmo(4) )  then
            do  410  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  400  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dimag( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dimag( c(m+i+idel) * dconjg( d(m-i+1) ) ))
  400          continue
               pmom( l,4 ) = pmom( l,4 ) + bidel( i ) * thesum
  410       continue
            pmom( l,4 ) = - 0.5 * pmom( l,4 )
         end if
!
  500 continue
!
!
  600 return
      end subroutine  lpcoef
!*********************************************************************                                                    
      subroutine  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 1
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1), b(1)               mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,nummom,l
      real*8    pmom( 0:momdim, * ),sq,a1sq,b1sq
      complex*16  a(*), b(*), ctmp, a1b1c
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a1sq = sq( a(1) )
      b1sq = sq( b(1) )
      a1b1c = a(1) * dconjg( b(1) )
!
      if( ipolzn.lt.0 )  then
!
         if( calcmo(1) )  pmom( 0,1 ) = 2.25 * b1sq
         if( calcmo(2) )  pmom( 0,2 ) = 2.25 * a1sq
         if( calcmo(3) )  pmom( 0,3 ) = 2.25 * dble( a1b1c )
         if( calcmo(4) )  pmom( 0,4 ) = 2.25 *dimag( a1b1c )
!
      else
!
         nummom = min0( nmom, 2 )
!                                   ** loop over moments
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 )  then
               if( l.eq.0 )  pmom( l,1 ) = 1.5 * ( a1sq + b1sq )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.15 * ( a1sq + b1sq )
               go to 100
            end if
!
            if( calcmo(1) )  then
               if( l.eq.0 )  pmom( l,1 ) = 2.25 * ( a1sq + b1sq / 3. )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.3 * b1sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 )  pmom( l,2 ) = 2.25 * ( b1sq + a1sq / 3. )
               if( l.eq.1 )  pmom( l,2 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,2 ) = 0.3 * a1sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 )  pmom( l,3 ) = 3.0 * dble( a1b1c )
               if( l.eq.1 )  pmom( l,3 ) = 0.75 * ( a1sq + b1sq )
               if( l.eq.2 )  pmom( l,3 ) = 0.3 * dble( a1b1c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 )  pmom( l,4 ) = - 1.5 * dimag( a1b1c )
               if( l.eq.1 )  pmom( l,4 ) = 0.0
               if( l.eq.2 )  pmom( l,4 ) = 0.3 * dimag( a1b1c )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco1t 
!********************************************************************                                                  
      subroutine  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 2
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1-2), b(1-2)           mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,l,nummom
      real*8    pmom( 0:momdim, * ),sq,pm1,pm2,a2sq,b2sq
      complex*16  a(*), b(*)
      complex*16  a2c, b2c, ctmp, ca, cac, cat, cb, cbc, cbt, cg, ch
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ca = 3. * a(1) - 5. * b(2)
      cat= 3. * b(1) - 5. * a(2)
      cac = dconjg( ca )
      a2sq = sq( a(2) )
      b2sq = sq( b(2) )
      a2c = dconjg( a(2) )
      b2c = dconjg( b(2) )
!
      if( ipolzn.lt.0 )  then
!                                   ** loop over sekera moments
         nummom = min0( nmom, 2 )
         do  50  l = 0, nummom
!
            if( calcmo(1) )  then
               if( l.eq.0 ) pmom( l,1 ) = 0.25 * ( sq(cat) +   &
                                                   (100./3.) * b2sq )
               if( l.eq.1 ) pmom( l,1 ) = (5./3.) * dble( cat * b2c )
               if( l.eq.2 ) pmom( l,1 ) = (10./3.) * b2sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 ) pmom( l,2 ) = 0.25 * ( sq(ca) +   &
                                                   (100./3.) * a2sq )
               if( l.eq.1 ) pmom( l,2 ) = (5./3.) * dble( ca * a2c )
               if( l.eq.2 ) pmom( l,2 ) = (10./3.) * a2sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,3 ) = 5./6. * dble( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,3 ) = 10./3. * dble( b(2) * a2c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = -0.25 * dimag( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,4 ) = -5./6. * dimag( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,4 ) = -10./3. * dimag( b(2) * a2c )
            end if
!
   50    continue
!
      else
!
         cb = 3. * b(1) + 5. * a(2)
         cbt= 3. * a(1) + 5. * b(2)
         cbc = dconjg( cb )
         cg = ( cbc*cbt + 10.*( cac*a(2) + b2c*cat) ) / 3.
         ch = 2.*( cbc*a(2) + b2c*cbt )
!
!                                   ** loop over mueller moments
         nummom = min0( nmom, 4 )
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 .or. calcmo(1) )  then
               if( l.eq.0 ) pm1 = 0.25 * sq(ca) + sq(cb) / 12.   &
                                  + (5./3.) * dble(ca*b2c) + 5.*b2sq
               if( l.eq.1 ) pm1 = dble( cb * ( cac/6. + b2c ) )
               if( l.eq.2 ) pm1 = sq(cb)/30. + (20./7.) * b2sq   &
                                  + (2./3.) * dble( ca * b2c )
               if( l.eq.3 ) pm1 = (2./7.) * dble( cb * b2c )
               if( l.eq.4 ) pm1 = (40./63.) * b2sq
               if ( calcmo(1) )  pmom( l,1 ) = pm1
            end if
!
            if( ipolzn.eq.0 .or. calcmo(2) )  then
               if( l.eq.0 ) pm2 = 0.25*sq(cat) + sq(cbt) / 12.   &
                                  + (5./3.) * dble(cat*a2c) + 5.*a2sq
               if( l.eq.1 ) pm2 = dble( cbt * ( dconjg(cat)/6. + a2c) )
               if( l.eq.2 ) pm2 = sq(cbt)/30. + (20./7.) * a2sq   &
                                  + (2./3.) * dble( cat * a2c )
               if( l.eq.3 ) pm2 = (2./7.) * dble( cbt * a2c )
               if( l.eq.4 ) pm2 = (40./63.) * a2sq
               if ( calcmo(2) )  pmom( l,2 ) = pm2
            end if
!
            if( ipolzn.eq.0 )  then
               pmom( l,1 ) = 0.5 * ( pm1 + pm2 )
               go to 100
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cac*cat + cg +   &
                                                       20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,3 ) = dble( cac*cbt + cbc*cat +   &
                                                3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,3 ) = 0.1 * dble( cg + (200./7.) *   &
                                                      b2c * a(2) )
               if( l.eq.3 ) pmom( l,3 ) = dble( ch ) / 14.
               if( l.eq.4 ) pmom( l,3 ) = 40./63. * dble( b2c * a(2) )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = 0.25 * dimag( cac*cat + cg +   &
                                                        20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,4 ) = dimag( cac*cbt + cbc*cat +   &
                                                 3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,4 ) = 0.1 * dimag( cg + (200./7.) *   &
                                                       b2c * a(2) )
               if( l.eq.3 ) pmom( l,4 ) = dimag( ch ) / 14.
               if( l.eq.4 ) pmom( l,4 ) = 40./63. * dimag( b2c * a(2) )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco2t 
!*********************************************************************                                                  
      subroutine  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
!
!        calculate logarithmic derivatives of j-bessel-function
!
!     input :  cior, xx, ntrm, noabs, yesang  (defined in 'miev0')
!
!    output :  rbiga or cbiga  (defined in 'miev0')
!
!    internal variables :
!
!       confra     value of lentz continued fraction for -cbiga(ntrm)-,
!                     used to initialize downward recurrence.
!       down       = true, use down-recurrence.  false, do not.
!       f1,f2,f3   arithmetic statement functions used in determining
!                     whether to use up-  or down-recurrence
!                     ( ref. 2, eqs. 6-8 )
!       mre        real refractive index
!       mim        imaginary refractive index
!       rezinv     1 / ( mre * xx ); temporary variable for recurrence
!       zinv       1 / ( cior * xx ); temporary variable for recurrence
!
      implicit none
      logical  down, noabs, yesang
      integer  ntrm,n
      real*8    mre, mim, rbiga(*), xx, rezinv, rtmp, f1,f2,f3
!      complex*16  cior, ctmp, confra, cbiga(*), zinv
      complex*16  cior, ctmp,  cbiga(*), zinv
      f1( mre ) =  - 8.0 + mre**2 * ( 26.22 + mre * ( - 0.4474   &
                   + mre**3 * ( 0.00204 - 0.000175 * mre ) ) )
      f2( mre ) = 3.9 + mre * ( - 10.8 + 13.78 * mre )
      f3( mre ) =  - 15.04 + mre * ( 8.42 + 16.35 * mre )
!
!                                  ** decide whether 'biga' can be
!                                  ** calculated by up-recurrence
      mre =  dble( cior )
      mim =  dabs( dimag( cior ) )
      if ( mre.lt.1.0 .or. mre.gt.10.0 .or. mim.gt.10.0 )  then
         down = .true.
      else if ( yesang )  then
         down = .true.
         if ( mim*xx .lt. f2( mre ) )  down = .false.
      else
         down = .true.
         if ( mim*xx .lt. f1( mre ) )  down = .false.
      end if
!
      zinv  = 1.0 / ( cior * xx )
      rezinv = 1.0 / ( mre * xx )
!
      if ( down )  then
!                          ** compute initial high-order 'biga' using
!                          ** lentz method ( ref. 1, pp. 17-20 )
!
         ctmp = confra( ntrm, zinv, xx )
!
!                                   *** downward recurrence for 'biga'
!                                   *** ( ref. 1, eq. 22 )
         if ( noabs )  then
!                                            ** no-absorption case
            rbiga( ntrm ) = dble( ctmp )
            do  25  n = ntrm, 2, - 1
               rbiga( n-1 ) = (n*rezinv)   &
                               - 1.0 / ( (n*rezinv) + rbiga( n ) )
   25       continue
!
         else
!                                            ** absorptive case
            cbiga( ntrm ) = ctmp
            do  30  n = ntrm, 2, - 1
               cbiga( n-1 ) = (n*zinv) - 1.0 / ( (n*zinv) + cbiga( n ) )
   30       continue
!
         end if
!
      else
!                              *** upward recurrence for 'biga'
!                              *** ( ref. 1, eqs. 20-21 )
         if ( noabs )  then
!                                            ** no-absorption case
            rtmp = dsin( mre*xx )
            rbiga( 1 ) =  - rezinv   &
                           + rtmp / ( rtmp*rezinv - dcos( mre*xx ) )
            do  40  n = 2, ntrm
               rbiga( n ) = - ( n*rezinv )   &
                             + 1.0 / ( ( n*rezinv ) - rbiga( n-1 ) )
   40       continue
!
         else
!                                                ** absorptive case
            ctmp = cdexp( - dcmplx(0.d0,2.d0) * cior * xx )
            cbiga( 1 ) = - zinv + (1.-ctmp) / ( zinv * (1.-ctmp) -   &
                           dcmplx(0.d0,1.d0)*(1.+ctmp) )
            do  50  n = 2, ntrm
               cbiga( n ) = - (n*zinv) + 1.0 / ((n*zinv) - cbiga( n-1 ))
   50       continue
         end if
!
      end if
!
      return
      end subroutine  biga  
!**********************************************************************                                                   
      complex*16 function  confra( n, zinv, xx )
!
!         compute bessel function ratio capital-a-sub-n from its
!         continued fraction using lentz method ( ref. 1, pp. 17-20 )
!
!         zinv = reciprocal of argument of capital-a
!
!    i n t e r n a l    v a r i a b l e s
!    ------------------------------------
!
!    cak      term in continued fraction expansion of capital-a
!                ( ref. 1, eq. 25 )
!    capt     factor used in lentz iteration for capital-a
!                ( ref. 1, eq. 27 )
!    cdenom   denominator in -capt-  ( ref. 1, eq. 28b )
!    cnumer   numerator   in -capt-  ( ref. 1, eq. 28a )
!    cdtd     product of two successive denominators of -capt-
!                factors  ( ref. 1, eq. 34c )
!    cntn     product of two successive numerators of -capt-
!                factors  ( ref. 1, eq. 34b )
!    eps1     ill-conditioning criterion
!    eps2     convergence criterion
!    kk       subscript k of -cak-  ( ref. 1, eq. 25b )
!    kount    iteration counter ( used only to prevent runaway )
!    maxit    max. allowed no. of iterations
!    mm       + 1  and - 1, alternately
!
      implicit none
      integer   n,maxit,mm,kk,kount
      real*8     xx,eps1,eps2
      complex*16   zinv
      complex*16   cak, capt, cdenom, cdtd, cnumer, cntn
!      data  eps1 / 1.e - 2 /, eps2 / 1.e - 8 /
      data  eps1 / 1.d-2 /, eps2 / 1.d-8 /
      data  maxit / 10000 /
!
!                                      *** ref. 1, eqs. 25a, 27
      confra = ( n + 1 ) * zinv
      mm     =  - 1
      kk     = 2 * n + 3
      cak    = ( mm * kk ) * zinv
      cdenom = cak
      cnumer = cdenom + 1.0 / confra
      kount  = 1
!
   20 kount = kount + 1
      if ( kount.gt.maxit )   &
           call errmsg( 'confra--iteration failed to converge$', .true.)
!
!                                         *** ref. 2, eq. 25b
      mm  =  - mm
      kk  = kk + 2
      cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eq. 32
      if (      cdabs( cnumer/cak ).le.eps1   &
           .or. cdabs( cdenom/cak ).le.eps1 )  then
!
!                                  ** ill-conditioned case -- stride
!                                  ** two terms instead of one
!
!                                         *** ref. 2, eqs. 34
         cntn   = cak * cnumer + 1.0
         cdtd   = cak * cdenom + 1.0
         confra = ( cntn / cdtd ) * confra
!                                             *** ref. 2, eq. 25b
         mm  =  - mm
         kk  = kk + 2
         cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eqs. 35
         cnumer = cak + cnumer / cntn
         cdenom = cak + cdenom / cdtd
         kount  = kount + 1
         go to 20
!
      else
!                                ** well-conditioned case
!
!                                        *** ref. 2, eqs. 26, 27
         capt   = cnumer / cdenom
         confra = capt * confra
!                                    ** check for convergence
!                                    ** ( ref. 2, eq. 31 )
!
         if (      dabs( dble(capt) - 1.0 ).ge.eps2   &
              .or. dabs( dimag(capt) )      .ge.eps2 )  then
!
!                                        *** ref. 2, eqs. 30a-b
            cnumer = cak + 1.0 / cnumer
            cdenom = cak + 1.0 / cdenom
            go to 20
         end if
      end if
!
      return
!
      end function confra
!********************************************************************      
      subroutine  miprnt( prnt, xx, perfct, crefin, numang, xmu,   &
                          qext, qsca, gqsc, nmom, ipolzn, momdim,   &
                          calcmo, pmom, sforw, sback, tforw, tback,   &
                          s1, s2 )
!
!         print scattering quantities of a single particle
!
      implicit none
      logical  perfct, prnt(*), calcmo(*)
      integer  ipolzn, momdim, nmom, numang,i,m,j
      real*8    gqsc, pmom( 0:momdim, * ), qext, qsca, xx, xmu(*)
      real*8 fi1,fi2,fnorm
      complex*16  crefin, sforw, sback, tforw(*), tback(*), s1(*), s2(*)
      character*22  fmt
!
!
      if ( perfct )  write ( *, '(''1'',10x,a,1p,e11.4)' )   &
                      'perfectly conducting case, size parameter =', xx
      if ( .not.perfct )  write ( *, '(''1'',10x,3(a,1p,e11.4))' )   &
                        'refractive index:  real ', dble(crefin),   &
                   '  imag ', dimag(crefin), ',   size parameter =', xx
!
      if ( prnt(1) .and. numang.gt.0 )  then
!
         write ( *, '(/,a)' )   &
          '    cos(angle)  ------- s1 ---------  ------- s2 ---------'//   &
          '  --- s1*conjg(s2) ---   i1=s1**2   i2=s2**2  (i1+i2)/2'//   &
          '  deg polzn'
         do  10  i = 1, numang
            fi1 = dble( s1(i) ) **2 + dimag( s1(i) )**2
            fi2 = dble( s2(i) ) **2 + dimag( s2(i) )**2
            write( *, '( i4, f10.6, 1p,10e11.3 )'   )   &
                    i, xmu(i), s1(i), s2(i), s1(i)*dconjg(s2(i)),   &
                    fi1, fi2, 0.5*(fi1+fi2), (fi2-fi1)/(fi2+fi1)
   10    continue
!
      end if
!
!
      if ( prnt(2) )  then
!
         write ( *, '(/,a,9x,a,17x,a,17x,a,/,(0p,f7.2, 1p,6e12.3) )' )   &
                 '  angle', 's-sub-1', 't-sub-1', 't-sub-2',   &
                     0.0,     sforw,    tforw(1),  tforw(2),   &
                    180.,     sback,    tback(1),  tback(2)
         write ( *, '(/,4(a,1p,e11.4))' )   &
                 ' efficiency factors,  extinction:', qext,   &
                                    '   scattering:', qsca,   &
                                    '   absorption:', qext-qsca,   &
                                 '   rad. pressure:', qext-gqsc
!
         if ( nmom.gt.0 )  then
!
            write( *, '(/,a)' )  ' normalized moments of :'
            if ( ipolzn.eq.0 ) write ( *, '(''+'',27x,a)' ) 'phase fcn'
            if ( ipolzn.gt.0 )  write ( *, '(''+'',33x,a)' )   &
               'm1           m2          s21          d21'
            if ( ipolzn.lt.0 )  write ( *, '(''+'',33x,a)' )   &
               'r1           r2           r3           r4'
!
            fnorm = 4. / ( xx**2 * qsca )
            do  20  m = 0, nmom
               write ( *, '(a,i4)' )  '      moment no.', m
               do 20  j = 1, 4
                  if( calcmo(j) )  then
                     write( fmt, 98 )  24 + (j-1)*13
                     write ( *,fmt )  fnorm * pmom(m,j)
                  end if
   20       continue
         end if
!
      end if
!
      return
!
   98 format( '( ''+'', t', i2, ', 1p,e13.4 )' )
      end subroutine  miprnt  
!**************************************************************************                                            
      subroutine  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                           sback, s1, s2, tforw, tback, a, b )
!
!       small-particle limit of mie quantities in totally reflecting
!       limit ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
      implicit none
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,fivnin,sq,rtmp
      complex*16  a( 2 ), b( 2 ), sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3., fivnin = 5./9. )
      complex*16    ctmp
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a( 1 ) = dcmplx ( 0.d0, twothr * ( 1. - 0.2 * xx**2 ) )   &
             / dcmplx ( 1.d0 - 0.5 * xx**2, twothr * xx**3 )
!
      b( 1 ) = dcmplx ( 0.d0, - ( 1. - 0.1 * xx**2 ) / 3. )   &
             / dcmplx ( 1.d0 + 0.5 * xx**2, - xx**3 / 3. )
!
      a( 2 ) = dcmplx ( 0.d0,   xx**2 / 30. )
      b( 2 ) = dcmplx ( 0.d0, - xx**2 / 45. )
!
      qsca = 6. * xx**4 * ( sq( a(1) ) + sq( b(1) )   &
                            + fivthr * ( sq( a(2) ) + sq( b(2) ) ) )
      qext = qsca
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) )   &
                          + ( b(1) + fivnin * a(2) ) * dconjg( b(2) ) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * ( a(2) + b(2) ) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * ( a(2) - b(2) ) )
      tforw( 1 ) = rtmp * ( b(1) + fivthr * ( 2.*b(2) - a(2) ) )
      tforw( 2 ) = rtmp * ( a(1) + fivthr * ( 2.*a(2) - b(2) ) )
      tback( 1 ) = rtmp * ( b(1) - fivthr * ( 2.*b(2) + a(2) ) )
      tback( 2 ) = rtmp * ( a(1) - fivthr * ( 2.*a(2) + b(2) ) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + b(1) * xmu(j) + fivthr *   &
                    ( a(2) * xmu(j) + b(2) * ( 2.*xmu(j)**2 - 1. )) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr *   &
                    ( b(2) * xmu(j) + a(2) * ( 2.*xmu(j)**2 - 1. )) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = xx**3 * b( 2 )
!
      return
      end subroutine  small1 
!*************************************************************************                                                 
      subroutine  small2 ( xx, cior, calcqe, numang, xmu, qext, qsca,   &
                           gqsc, sforw, sback, s1, s2, tforw, tback,   &
                           a, b )
!
!       small-particle limit of mie quantities for general refractive
!       index ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
!        ciorsq    square of refractive index
!
      implicit none
      logical  calcqe
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,sq,rtmp
      complex*16  a( 2 ), b( 2 ), cior, sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3. )
      complex*16  ctmp, ciorsq
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ciorsq = cior**2
      ctmp = dcmplx( 0.d0, twothr ) * ( ciorsq - 1.0 )
      a(1) = ctmp * ( 1.0 - 0.1 * xx**2 + (ciorsq/350. + 1./280.)*xx**4)   &
             / ( ciorsq + 2.0 + ( 1.0 - 0.7 * ciorsq ) * xx**2   &
                 - ( ciorsq**2/175. - 0.275 * ciorsq + 0.25 ) * xx**4   &
                 + xx**3 * ctmp * ( 1.0 - 0.1 * xx**2 ) )
!
      b(1) = (xx**2/30.) * ctmp * ( 1.0 + (ciorsq/35. - 1./14.) *xx**2 )   &
             / ( 1.0 - ( ciorsq/15. - 1./6. ) * xx**2 )
!
      a(2) = ( 0.1 * xx**2 ) * ctmp * ( 1.0 - xx**2 / 14. )   &
             / ( 2. * ciorsq + 3. - ( ciorsq/7. - 0.5 ) * xx**2 )
!
      qsca = 6. * xx**4 * ( sq(a(1)) + sq(b(1)) + fivthr * sq(a(2)) )
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) ) )
      qext = qsca
      if ( calcqe ) qext = 6. * xx * dble( a(1) + b(1) + fivthr * a(2) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * a(2) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * a(2) )
      tforw( 1 ) = rtmp * ( b(1) - fivthr * a(2) )
      tforw( 2 ) = rtmp * ( a(1) + 2. * fivthr * a(2) )
      tback( 1 ) = tforw( 1 )
      tback( 2 ) = rtmp * ( a(1) - 2. * fivthr * a(2) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + ( b(1) + fivthr * a(2) ) * xmu(j) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr * a(2)   &
                            * ( 2. * xmu(j)**2 - 1. ) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = ( 0., 0. )
!
      return
      end subroutine  small2  
!***********************************************************************                                                 
      subroutine  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                           tforw, tback, pmom, momdim, ok )
!
!         compare mie code test case results with correct answers
!         and return  ok=false  if even one result is inaccurate.
!
!         the test case is :  mie size parameter = 10
!                             refractive index   = 1.5 - 0.1 i
!                             scattering angle = 140 degrees
!                             1 sekera moment
!
!         results for this case may be found among the test cases
!         at the end of reference (1).
!
!         *** note *** when running on some computers, esp. in single
!         precision, the 'accur' criterion below may have to be relaxed.
!         however, if 'accur' must be set larger than 10**-3 for some
!         size parameters, your computer is probably not accurate
!         enough to do mie computations for those size parameters.
!
      implicit none
      integer momdim,m,n
      real*8    qext, qsca, gqsc, pmom( 0:momdim, * )
      complex*16  sforw, sback, s1(*), s2(*), tforw(*), tback(*)
      logical  ok, wrong
!
      real*8    accur, testqe, testqs, testgq, testpm( 0:1 )
      complex*16 testsf, testsb,tests1,tests2,testtf(2), testtb(2)
      data   testqe / 2.459791 /,  testqs / 1.235144 /,   &
             testgq / 1.139235 /,  testsf / ( 61.49476, -3.177994 ) /,   &
             testsb / ( 1.493434, 0.2963657 ) /,   &
             tests1 / ( -0.1548380, -1.128972) /,   &
             tests2 / ( 0.05669755, 0.5425681) /,   &
             testtf / ( 12.95238, -136.6436 ), ( 48.54238, 133.4656 ) /,   &
             testtb / ( 41.88414, -15.57833 ), ( 43.37758, -15.28196 )/,   &
             testpm / 227.1975, 183.6898 /
      real*8 calc,exact
!      data   accur / 1.e-5 /
      data   accur / 1.e-4 /
      wrong( calc, exact ) = dabs( (calc - exact) / exact ) .gt. accur
!
!
      ok = .true.
      if ( wrong( qext,testqe ) )   &
           call  tstbad( 'qext', abs((qext - testqe) / testqe), ok )
      if ( wrong( qsca,testqs ) )   &
           call  tstbad( 'qsca', abs((qsca - testqs) / testqs), ok )
      if ( wrong( gqsc,testgq ) )   &
           call  tstbad( 'gqsc', abs((gqsc - testgq) / testgq), ok )
!
      if ( wrong(  dble(sforw),  dble(testsf) ) .or.   &
           wrong( dimag(sforw), dimag(testsf) ) )   &
           call  tstbad( 'sforw', cdabs((sforw - testsf) / testsf), ok )
!
      if ( wrong(  dble(sback),  dble(testsb) ) .or.   &
           wrong( dimag(sback), dimag(testsb) ) )   &
           call  tstbad( 'sback', cdabs((sback - testsb) / testsb), ok )
!
      if ( wrong(  dble(s1(1)),  dble(tests1) ) .or.   &
           wrong( dimag(s1(1)), dimag(tests1) ) )   &
           call  tstbad( 's1', cdabs((s1(1) - tests1) / tests1), ok )
!
      if ( wrong(  dble(s2(1)),  dble(tests2) ) .or.   &
           wrong( dimag(s2(1)), dimag(tests2) ) )   &
           call  tstbad( 's2', cdabs((s2(1) - tests2) / tests2), ok )
!
      do  20  n = 1, 2
         if ( wrong(  dble(tforw(n)),  dble(testtf(n)) ) .or.   &
              wrong( dimag(tforw(n)), dimag(testtf(n)) ) )   &
              call  tstbad( 'tforw', cdabs( (tforw(n) - testtf(n)) /   &
                                           testtf(n) ), ok )
         if ( wrong(  dble(tback(n)),  dble(testtb(n)) ) .or.   &
              wrong( dimag(tback(n)), dimag(testtb(n)) ) )   &
              call  tstbad( 'tback', cdabs( (tback(n) - testtb(n)) /   &
                                            testtb(n) ), ok )
   20 continue
!
      do  30  m = 0, 1
         if ( wrong( pmom(m,1), testpm(m) ) )   &
              call  tstbad( 'pmom', dabs( (pmom(m,1)-testpm(m)) /   &
                                         testpm(m) ), ok )
   30 continue
!
      return
!
      end subroutine  testmi  
!**************************************************************************                                              
      subroutine  errmsg( messag, fatal )
!
!        print out a warning or error message;  abort if error
!
      USE module_peg_util, only:  peg_message, peg_error_fatal

      implicit none
      logical       fatal, once
      character*80 msg
      character*(*) messag
      integer       maxmsg, nummsg
      save          maxmsg, nummsg, once
      data nummsg / 0 /,  maxmsg / 100 /,  once / .false. /
!
!
      if ( fatal )  then
!         write ( *, '(2a)' )  ' ******* error >>>>>>  ', messag
!         stop
   	  write( msg, '(a)' )   &
                  'FASTJ mie fatal error ' //   &
                  messag                  
                  call peg_message( lunerr, msg )             
                  call peg_error_fatal( lunerr, msg )
      end if
!
      nummsg = nummsg + 1
      if ( nummsg.gt.maxmsg )  then
!         if ( .not.once )  write ( *,99 )
	 if ( .not.once )then
	    write( msg, '(a)' )   &
             'FASTJ mie: too many warning messages -- no longer printing '
            call peg_message( lunerr, msg )
         end if    
         once = .true.
      else
         msg =   'FASTJ mie warning '  // messag
         call peg_message( lunerr, msg )  
!         write ( *, '(2a)' )  ' ******* warning >>>>>>  ', messag
      endif
!
      return
!
!   99 format( ///,' >>>>>>  too many warning messages --  ',   &
!         'they will no longer be printed  <<<<<<<', /// )
      end subroutine  errmsg  
!********************************************************************                     
      subroutine  wrtbad ( varnam, erflag )
!
!          write names of erroneous variables
!
!      input :   varnam = name of erroneous variable to be written
!                         ( character, any length )
!
!      output :  erflag = logical flag, set true by this routine
! ----------------------------------------------------------------------
      USE module_peg_util, only:  peg_message
      
      implicit none
      character*(*)  varnam
      logical        erflag
      integer        maxmsg, nummsg
      character*80 msg
      save  nummsg, maxmsg
      data  nummsg / 0 /,  maxmsg / 50 /     
!
!
      nummsg = nummsg + 1
!      write ( *, '(3a)' )  ' ****  input variable  ', varnam,   &
!                           '  in error  ****'
        msg = 'FASTJ mie input variable in error ' // varnam                   
      call peg_message( lunerr, msg )
      erflag = .true.
      if ( nummsg.eq.maxmsg )   &     
         call  errmsg ( 'too many input variable errors.  aborting...$', .true. )
      return
!
      end subroutine  wrtbad 
!******************************************************************                        
      subroutine  tstbad( varnam, relerr, ok )
!
!       write name (-varnam-) of variable failing self-test and its
!       percent error from the correct value.  return  ok = false.
!
      implicit none
      character*(*)  varnam
      logical        ok
      real*8          relerr
!
!
      ok = .false.
      write( *, '(/,3a,1p,e11.2,a)' )   &
             ' output variable  ', varnam,'  differed by', 100.*relerr,   &
             '  per cent from correct value.  self-test failed.'
      return
!
      end subroutine  tstbad                      
!-----------------------------------------------------------------------
	end module module_fastj_mie
