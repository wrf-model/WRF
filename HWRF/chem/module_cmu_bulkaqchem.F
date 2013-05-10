!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

      module module_cmu_bulkaqchem


      use module_peg_util, only:  peg_error_fatal, peg_message


      implicit none


      contains



!**********************************************************************
!
! revised aqueous-phase chemistry module for the 3-d acid deposition model
!
!**********************************************************************
!
! last modification : june 2, 1998 by s. pandis
!  the algorithm was modified to speed up execution. two major changes
!    (1) a bulk aqueous-phase chemistry approach is used and the
!        results are distributed over the particle spectrum according
!        to water content
!    (2) the water content is assumed to be constant (an input to the code)
!
!    ******   for bulk   ******
!
!-----------------------------------------------------------------------
!
! This code was obtained from S. Pandis in July 2003.
! It was converted to Fortran-90 and adapted for use in WRF-chem with
!     the MOSAIC aerosol modules by R. Easter (PNNL) in July 2005.
!
! 27-oct-2005 rce
!	switched from svode (single prec) to dvode (double prec) solver
!	changes in fullequil & aqratesa to make the ph calc more robust, 
!	    and do a graceful exit (?) when it fails
! 01-nov-2005 rce
!	this version is completely double precision, including arguments
! 09-dec-2005 rce
!	when iradical_in=100/101/102, the aqueous radical species are 
!	    calculated directly by dvode, and hybrd is not used
!	when idecomp_hmsa_hso5 > 0 & iradical_in = 100/1/2
!	    aqueous so5- is transferred to gas so2
!	    aqueous so4- is transferred to aerosol so4=
!	    (this is in addition to the previous actions 
!	     when idecomp_hmsa_hso5 > 0:
!		aqueous hso5 is transferred to gas so2
!		aqueous hmsa is transferred to gas so2 & hcho)
! 15-may-2006 rce
!	deleted the call to hybrd, so iradical_in>0 results in iradical=100
!-----------------------------------------------------------------------




!----------------------------------------------------------------------
      subroutine aqoperator1(   &
        istat_fatal, istat_warn,   &
        tbeg_sec, tend_sec,   &
        gas, aerosol, gas_aqfrac,   &
        temp, p, lwc, rh,   &
        co2_mixrat_in, photo_in, xprescribe_ph,   &
        iradical_in, idecomp_hmsa_hso5, iaq,   &
        yaq_beg, yaq_end, ph_cmuaq_cur )

      use module_data_cmu_bulkaqchem, only:   &
              kaqx_hmsa, kaqx_hso5m, kaqx_siv,   &
              mdiag_fullequil, mdiag_hybrd,   &
              mdiag_negconc, mdiag_rsrate, mdiag_svode,   &
              meqn1max, naers, ngas,   &
              na4, naa, nac, nahmsa, nahso5, nan, nar, nas,   &
              ng4, nga, ngc, ngh2o2, nghcho, nghcooh, ngn, ngso2,   &
              rideal,   &
              wh2o2, wh2so4, whcho, whcl, whcooh, whno3,   &
              wnh3, wso2, wmol

!
!.....inputs
!
!     tbeg_sec : start of integration (in sec)
!     tend_sec : end   of integration time (in sec)
!
!     gas(ngas) : gas-phase concentration vector (in ppm)
!     aerosol(naers) : aqueous (aerosol) species concentration (in ug/m3)
!
!     temp : temperature (in k)
!     p : pressure (in atm)
!     lwc : liquid water content (in g/m3)
!     rh : relative humidity  (in 0-1 scale)
!     iradical_in : flag: 1/0 means aqueous free radical chemistry is on/off
!     photo_in : factor applied to aqueous photochemical rates
!
!     idecomp_hmsa_hso5 : flag -- if > 0, then hso5 is decomposed to so2,
!             and hmsa is decomposed to so2 & hcho, at end of integration.
!             If 0, hso5 and hmsa are left as is.
!     iaq : flag: 1 at first call, 0 there after
!
!.....outputs
!     gas(ngas) : updated gas-phase concentrations (in ppm)
!     aerosol(naers) : updated aqueous species concentration (in ug/m3)
!     iaq : flag set to zero
!
!     yaq_beg(meqn1max) : initial yaq values computed from initial gas & aerosol
!     yaq_end(meqn1max) : final   yaq values
!     ph_cmuaq_cur : final ph values
!
!.....variables
!     gcon(ngas) : gas-phase concentrations (in ppm) (through rpar to rates)
!
!
!.....differential equations are solved for the following species
!
!       total                        gas             aqueous
!-----------------------------------------------------------------------
!     1. formaldehyde total        1. so2       1.  s(iv)
!     2. formic acid total         2. h2o2      2.  h2o2(aq)
!                                  3. nh3       3.  nitrate
!                                               4.  chloride
!                                               5.  ammonium
!                                               6.  sulfate
!                                               7.  hso5-
!                                               8.  hmsa
!
!
!.....y matrix structure  everything is (ug/m3)
!     only 11 odes are solved for the whole distribution
!
!  yaq(1) = total formaldehyde
!  yaq(2) = total formic acid
!  yaq(3) = so2 (g)
!  yaq(4) = h2o2 (g)
!  yaq(5) = nh3 (g)
!  yaq(6) = so2 (aq)
!  yaq(7) = h2o2 (aq)
!  yaq(8) = nitrate (aq)
!  yaq(9) = chloride (aq)
!  yaq(10) = ammonium (aq)
!  yaq(11) = sulfate (aq)
!  yaq(12) = hso5- (aq)
!  yaq(13) = hmsa (aq)
!

!   arguments
      integer istat_fatal, istat_warn
      integer iradical_in, idecomp_hmsa_hso5, iaq
      double precision tbeg_sec, tend_sec
      double precision temp, p, lwc, rh
      double precision co2_mixrat_in, photo_in, xprescribe_ph
      double precision gas(ngas), aerosol(naers)
      double precision gas_aqfrac(ngas)
      double precision yaq_beg(meqn1max), yaq_end(meqn1max), ph_cmuaq_cur

!   local variables
      integer i, icount, iradical, iprint, isp
      integer meqn1, negconc_count

      double precision   &
        ammonold, chlorold, co2_mixrat, crustal,   &
        dammon, dchlor, dhmsa, dhso5, dnit, dsulf,   &
        fammon, fh2o2, fh2so4, fhcho, fhcl, fhcooh, fhno3, fso2,   &
        hmsaold, hso5old,   &
        photo, pres,   &
        salt, sodium, stime, stout, sulfateold,   &
        tnitold, watcont, watvap,   &
        whchowhmsa, wso2whmsa, wso2whso5, wso2wsiv,   &
        yaq_h2so4_g, yaq_hcl_g, yaq_hno3_g
      double precision   &
        duma, dumb, dumhion, vlwc
      double precision gascon(ngas)
      double precision yaq(meqn1max)
      double precision akeq(17), akhen(21), akre(120)

      integer ipar(10)
      double precision rpar(178+ngas+1)

!
!     initialization (only on first call)
!     (calculation of sectional diameters (which isn't really needed),
!      and loading of molec. weights (which is needed))
!
      if (iaq .eq. 1) then
          call dropinit
          iaq = 0
      endif

      mdiag_fullequil = 1
      mdiag_hybrd = 1
      mdiag_negconc = 1
      mdiag_rsrate = 1
      mdiag_svode = 1

!
!     calc temperature dependent parameters
!
      pres = p                                   ! pressure in atm
      call constants( temp, akeq, akhen, akre )

!
!     zero the yaq matrix
!
      do i=1, meqn1max
      yaq(i)=0.0
      enddo

      watcont = lwc

!     for iradical_in<0, set iradical=0
!     for iradical_in>0, set iradical=100 except when iradical_in=101,102
      iradical = iradical_in
      if (iradical .gt. 0) then
          if ((iradical .ne. 101) .and.   &
              (iradical .ne. 102)) iradical = 100
      else
          iradical = 0
      end if

      photo = photo_in
      co2_mixrat = co2_mixrat_in

      meqn1 = 13
!     if (iradical .eq. 100) meqn1 = 20
      if (iradical .gt.   0) meqn1 = 20

      icount=0
! Undefined.  Provide a value for now.
      iprint=20
      watvap = 0.d0

!
!     transfer of all gas-phase concentrations to gascon and then
!     through rpar to rates
!
      do i=1, ngas
          gascon(i) = gas(i)
          gas_aqfrac(i) = 0.0
      enddo
!
!     unit conversion factors
!
      fso2=1000.*pres*wso2/(rideal*temp)     !so2   conver. factor from ppm to ug/m3
      fh2o2=1000.*pres*wh2o2/(rideal*temp)   !h2o2  conver. factor from ppm to ug/m3
      fhcho=1000.*pres*whcho/(rideal*temp)   !hcho  conver. factor from ppm to ug/m3
      fhcooh=1000.*pres*whcooh/(rideal*temp) !hcooh conver. factor from ppm to ug/m3
      fammon=1000.*pres*wnh3/(rideal*temp)   !nh3   conver. factor from ppm to ug/m3
!
!
!   *** beginning of aqueous-phase calculation
!   loading of the concentrations in the yaq vector
!
!   the total formaldehyde/formic acid are transferred as gas-phase species
!    (they are not included in the aqueous or aerosol matrices)
      yaq(1) = gas(nghcho)*fhcho           ! total hcho (ug/m3)
      yaq(2) = gas(nghcooh)*fhcooh         ! total hcooh (ug/m3)
!
!   gas-phase species
!
      yaq(3) = gas(ngso2)*fso2             ! total so2 (ug/m3)
      yaq(4) = gas(ngh2o2)*fh2o2           ! total h2o2 (ug/m3)
      yaq(5) = gas(nga) *fammon            ! nh3(g) in ug/m3

!
!   aqueous-phase species
!
!   "aerosol" array holds the bulk aqueous chemical concentrations
!   (in ug/m3-of-air), so there is no "activation" or summing over sections
!
!     ** the droplets are assumed to by without so2 or
!        h2o2 in the beginning of a timestep **
!      (this is done to avoid carrying the s(iv)/h2o2 concentrations
!      in the 3d simulation **
      yaq(6) = 1.e-10     ! so2 (aq) in ug/m3
      yaq(7) = 1.e-10     ! h2o2 (aq) in ug/m3

      yaq(8)  = aerosol(nan)       ! nitrate (aq) in ug/m3
      yaq(9)  = aerosol(nac)       ! chloride (aq) in ug/m3
      yaq(10) = aerosol(naa)       ! ammonium (aq) in ug/m3
      yaq(11) = aerosol(na4)       ! sulfate (aq) in ug/m3
      yaq(12) = aerosol(nahso5)    ! hso5- in ug/m3
      yaq(13) = aerosol(nahmsa)    ! hmsa in ug/m3

!
!     save the old aqueous-phase concentrations before the integration
!
      tnitold = yaq(8)
      chlorold = yaq(9)
      ammonold = yaq(10)
      sulfateold = yaq(11)
      hso5old = yaq(12)
      hmsaold = yaq(13)
!
!     calculation of the dissolution of the available h2so4, hno3 and hcl
!     to the droplets/particles. we assume that the timescale of dissolution
!     is small and that all hno3 and hcl are dissolved (zero vapor pressure)
!
      fh2so4=1000.*wh2so4*pres/(rideal*temp) !h2so4 conver. factor from ppm to ug/m3
      fhno3 =1000.*whno3*pres/(rideal*temp)  !hno3  conver. factor from ppm to ug/m3
      fhcl  =1000.*whcl*pres/(rideal*temp)   !hcl   conver. factor from ppm to ug/m3
!
      yaq_h2so4_g = gas(ng4)*fh2so4 ! h2so4(g) (in ug/m3)
      yaq_hno3_g  = gas(ngn)*fhno3  ! hno3(g) (in ug/m3)
      yaq_hcl_g   = gas(ngc)*fhcl   ! hcl(g) (in ug/m3)
!
!     ** transfer the gas-phase h2so4, hno3 and hcl to aqueous phase **
!     (and account for molec-weight differences)
      gas(ng4) = 0.0          ! all h2so4 is dissolved
      gas(ngn) = 0.0          ! all hno3  is dissolved
      gas(ngc) = 0.0          ! all hcl   is dissolved
      yaq(11) = yaq(11) + (yaq_h2so4_g/wh2so4)*wmol(2)   ! so4-- increase
      yaq(8)  = yaq(8)  + (yaq_hno3_g/whno3)*wmol(4)     ! no3- increase
      yaq(9)  = yaq(9)  + (yaq_hcl_g/whcl)*wmol(15)      ! cl- increase

!
!.....the yaq matrix has been initialized
!

!     test to be removed
!     write(27,*)' initial values of the yaqs'
!     write(27,*)lwc,rh,temp
!     do i = 1, 11
!         write(27,*)i, yaq(i)
!     enddo

!
!     variables for integration
!
      stime = tbeg_sec                        ! in seconds
      stout = tend_sec                        ! in seconds
!
!     calculation of sodium (needed for the integration routine)
!
      sodium = aerosol(nas)
!
!     calculate crustal species concentration inside droplets
!     (it is used in the aqueous-phase chemistry calculation to
!      estimate fe and mn concentrations
!
      crustal = aerosol(nar)
      salt = aerosol(nas)

!
!     save yaq to yaq_beg
!
      do i = 1, 13
         yaq_beg(i) = yaq(i)
      end do

!
!     load ipar, rpar
!
      ipar(1) = icount
      ipar(2) = iprint
      ipar(3) = 0
      ipar(4) = 0
      ipar(5) = 0
      ipar(6) = 0
      ipar(7) = 0
      ipar(8) = 0
      ipar(9) = iradical

      rpar( 1) = temp
      rpar( 2) = pres
      rpar( 3) = watcont
      rpar( 4) = watvap
      rpar( 5) = crustal
      rpar( 6) = salt
      rpar( 7) = sodium
      rpar( 8) = photo
      rpar( 9) = co2_mixrat
      rpar(10) = xprescribe_ph

      rpar(11) = ph_cmuaq_cur
      rpar(21: 37) = akeq( 1: 17)
      rpar(38: 58) = akhen(1: 21)
      rpar(59:178) = akre( 1:120)
      do i = 1, ngas
          rpar(178+i) = gascon(i)
      end do

!
!     integrate
!
      call aqintegr1( meqn1, yaq, stime, stout, rpar, ipar )

!
!     unload ipar, rpar
!
      ph_cmuaq_cur = rpar(11)

!
!     calculate the differences
!
      dnit = yaq(8) - tnitold
      dchlor = yaq(9) - chlorold
      dammon = yaq(10) - ammonold
      dsulf = yaq(11) - sulfateold
      dhso5 = yaq(12) - hso5old
      dhmsa = yaq(13) - hmsaold

!
!     transfer new aqueous concentrations back to aerosol array
!
      aerosol(nan) = yaq(8)		! nitrate (aq) in ug/m3
      aerosol(nac) = yaq(9)		! chloride (aq) in ug/m3
      aerosol(naa) = yaq(10)		! ammonium (aq) in ug/m3
      aerosol(na4) = yaq(11)		! sulfate (aq) in ug/m3
      aerosol(nahso5) = yaq(12)		! hso5 (aq) in ug/m3
      aerosol(nahmsa) = yaq(13)		! hmsa (aq) in ug/m3

!
!     check if the algorithm resulted in negative concentrations
!     report the corrections at the warning file
!
      negconc_count = 0
      do isp=1, naers
         if (aerosol(isp) .lt. 0.0) then
            negconc_count = negconc_count + 1
            if (mdiag_negconc .gt. 0) then
!              write(2,*)'negative concentration at', stime
!              write(2,*)'species=',isp, aerosol(isp)
               if (negconc_count .eq. 1) write(6,*)   &
                  '*** module_cmuaq_bulk aqoperator1 neg conc at t', stime
               write(6,*) '   isp, conc', isp, aerosol(isp)
            end if
            aerosol(isp)=1.e-12
         endif
      enddo

!
!     return gas yaq results to their original matrices
!
!     ** gas-phase species **
!     important : the dissolved s(iv) and h2o2 are transferred
!                 back to the gas phase
!                 (this change is applied to "gas" but not to "yaq")
!
      gas(nghcho) = yaq(1)/fhcho              ! total hcho (ppm)
      gas(nghcooh) = yaq(2)/fhcooh             ! total hcooh (ppm)

      wso2wsiv = wso2/wmol(kaqx_siv)
!     gas(ngso2) = (yaq(3)+yaq(6)*0.7901)/fso2       ! so2(g) (ppm)
      gas(ngso2) = (yaq(3)+yaq(6)*wso2wsiv)/fso2     ! so2(g) (ppm)
      gas_aqfrac(ngso2) = (yaq(6)*wso2wsiv) / (yaq(3)+yaq(6)*wso2wsiv)

      gas(ngh2o2) = (yaq(4)+yaq(7))/fh2o2             ! h2o2(g) (ppm)
      gas_aqfrac(ngh2o2) = yaq(7) / (yaq(4)+yaq(7))

      gas(nga) = yaq(5)/fammon               ! nh3(g) (ppm)

!
!     gas fractions [gas/(gas+aqueous)] for hcho and hcooh
!     hcho  expression is from subr aqratesa (duma here = hc1 there)
!     hcooh expression is from subr electro  (duma here = dform there)
!
      vlwc = lwc*1.0e-6		! (liter-h2o)/(liter-air)

      duma = rideal*temp*vlwc*akhen(7)
      gas_aqfrac(nghcho) = duma/(duma + 1.0)

      dumb = max( 0.0d0, min( 14.0d0, ph_cmuaq_cur ) )
      dumhion = 10.0**(-dumb)
      duma = rideal*temp*vlwc*akhen(8)*(1.+akeq(13)/dumhion)
      gas_aqfrac(nghcooh) = duma/(duma + 1.0)


!
!     if idecomp_hmsa_hso5 > 0, then
!         aqueous hso5 is transferred to gas so2
!         aqueous hmsa is transferred to gas so2 & hcho
!         (this change is applied to 'gas' & 'aerosol' but not to 'yaq')
!     also, if idecomp_hmsa_hso5 > 0 & iradical = 100/1/2
!         aqueous so5- is transferred to gas so2
!         aqueous so4- is transferred to aerosol so4=
!
      if (idecomp_hmsa_hso5 .gt. 0) then
          wso2whso5 = wso2/wmol(kaqx_hso5m)
          wso2whmsa = wso2/wmol(kaqx_hmsa)
          whchowhmsa = whcho/wmol(kaqx_hmsa)
          gas(ngso2) = gas(ngso2) +   &
              (yaq(12)*wso2whso5 + yaq(13)*wso2whmsa)/fso2
          gas(nghcho) = gas(nghcho) + yaq(13)*whchowhmsa/fhcho
          aerosol(nahso5) = 0.0
          aerosol(nahmsa) = 0.0
      end if
      if ( (idecomp_hmsa_hso5 .gt. 0) .and.   &
           (iabs(iradical-101) .le. 1) ) then
          gas(ngso2) = gas(ngso2) + (yaq(19)*wso2/wmol(25))/fso2
          aerosol(na4) = aerosol(na4) + (yaq(18)*wmol(2)/wmol(24))
      end if

!
!     save yaq to yaq_end
!
      do i = 1, 13
         yaq_end(i) = yaq(i)
      end do

!
! istat_fatal = fatal error status
!    (ipar(3) .ne. 0) --> svode     had problems -->  10s digit = -1
!    (ipar(7) .ne. 0) --> fullequil had problems --> 100s digit = -2
! istat_warn = warning status
!    (ipar(5) .ne. 0) --> hybrd     had problems -->  10s digit = +1
!    (negconc_count .ne. 0)                      --> 100s digit = +2
!
      istat_fatal = 0
      if (ipar(3) .ne. 0) istat_fatal = istat_fatal -  10
      if (ipar(7) .ne. 0) istat_fatal = istat_fatal - 200

      istat_warn = 0
      if (ipar(5)       .ne. 0) istat_warn = istat_warn +  10
      if (negconc_count .ne. 0) istat_warn = istat_warn + 200

!
! the code neglects the change in the size distribution shape
! of the nonvolatile aerosol components because of the sulfate
! production. the change in the sulfate distribution is calculated
! using the distribution functions while the change in the
! distribution of the volatile inorganic aerosol components will
! calculated by the aerosol module after cloud evaporation
!
!
!.....end of code
      return
      end subroutine aqoperator1 



!----------------------------------------------------------------------
!
! this routine prepares the necessary variables for the call
! to the stiff ode integrator (svode)
! its interface to the main code is the same as with the
! assymptotic solver so one can interchange solvers easily
!
! last modification: june 13, 1998 by s.p.
!
!----------------------------------------------------------------------
      subroutine aqintegr1( meqn1, y, stime, stout, rpar, ipar )

      use module_data_cmu_bulkaqchem, only:   &
              iopt, itask, itol, liw1, lrw1,   &
              mdiag_svode, meqn1max, mf,   &
              tola, tolr, worki, workr

      use module_cmu_dvode_solver, only:  dvode

!   arguments
      integer meqn1, ipar(*)
      double precision y(meqn1max), stime, stout, rpar(*)

!   local variables
      integer i, istate, liw, lrw
      integer iwork(30+meqn1max)
      double precision rwork(22+9*meqn1max+2*meqn1max**2)
      double precision atol(meqn1max),rtol(meqn1max)

!
      do i=1, meqn1
          atol(i) = tola              ! absolute tolerance in ug/m3
          rtol(i) = tolr              ! relative tolerance
          if (i .gt. 13) atol(i) = 1.0e-20
      enddo
      lrw = lrw1                      ! dimension of double precision work vector
      liw = liw1                      ! dimension of integer work vector
      iwork(6) = worki                ! steps allowed
      rwork(6) = workr                ! maximum step in seconds

!
!     ready for the call to svode
!
      istate = 1

      call dvode( fex1, meqn1, y, stime, stout, itol, rtol, atol, itask,   &
                  istate, iopt, rwork, lrw, iwork, liw, jex, mf,   &
                  rpar, ipar )

      if (istate .ne. 2) then
!         write(*,*) '*** aqintegr1 -- svode failed'
!         write(*,*) '    stime, istate =', stime, istate
!         stop

          if (mdiag_svode .gt. 0) then
              write(6,*)   &
              '*** module_cmuaq_bulk, aubr aqintegr1 -- ' //   &
              'svode failed, istate =', istate
          end if
          ipar(3) = ipar(3) + 1
          ipar(4) = istate
      endif
!
      do i=1, meqn1
          if (y(i) .lt. 0.0) y(i) = 1.e-20
      enddo
!
      return
      end subroutine aqintegr1                                     



!----------------------------------------------------------------------
      subroutine fex1( meqn1, t, y, f, rpar, ipar )

      use module_data_cmu_bulkaqchem, only:  meqn1max

!   arguments
      integer meqn1, ipar(*)
      double precision t, y(meqn1max), f(meqn1max), rpar(*)

!   local variables
      double precision a(meqn1max),b(meqn1max)


      call aqratesa( meqn1, t, y, a, b, f, rpar, ipar )

      return
      end subroutine fex1                          



!----------------------------------------------------------------------
      subroutine jex( meqn1, t, y, ml, mu, pd, nrowpd, rpar, ipar )
      integer meqn1, ml, mu, nrowpd, ipar(*)
      double precision t, y(meqn1), pd(nrowpd,meqn1), rpar(*)
      call peg_error_fatal( -1,   &
          '*** module_cmuaq_bulk, subr jex -- should not be here ***' )
      end subroutine jex                             



!----------------------------------------------------------------------
!
! last modification : feb. 15, 1995 by s. pandis
!*************************************************************************
!                                aqrates.for
!*************************************************************************
! this routine calculates the rates of change of the y's at time tmin
! for the aqueous-phase species
! it does bulk-phase chemistry
!
!
      subroutine aqratesa( meqn1, t, yaq, aqprod, aqdest, yaqprime,   &
      		rpar, ipar )

      use module_data_cmu_bulkaqchem, only:   &
              avdiam,   &
              caratio,   &
              epsfcn, factor, firon, fman, gmol,   &
              ldfjac, lr,   &
              maxfev, meqn1max, ml, mode, mu,   &
              mdiag_hybrd, mdiag_rsrate,   &
              ngas, ngch3o2, nghno2, ngho2,   &
              ngno, ngno2, ngno3, ngo3, ngoh, ngpan,   &
              nprint, numfunc,   &
              rideal,   &
              wh2o2, whcho, whcooh, wnh3, wso2, wmol,   &
              xtol

#if defined ( ccboxtest_box_testing_active )
      use module_data_ccboxwrf, only:  con_ccboxtest
#endif

!   arguments
      integer meqn1, ipar(*)
      double precision t, yaq(meqn1max), yaqprime(meqn1max)
      double precision aqprod(meqn1max), aqdest(meqn1max)
      double precision rpar(*)

!   local variables
      integer i, icount, info, iprint, iradical
      integer j
      integer n, nfev

      double precision akeq(17), akhen(21), akre(120)
      double precision c(46), cmet(4), con(28)
      double precision dp(49), dl(49), diag(numfunc)
      double precision fgl(21), flg(21), fvec(numfunc), fjac(ldfjac,numfunc)
      double precision gfgl(21), gflg(21), gascon(ngas), gcon(22)
      double precision spres(22)
      double precision x(numfunc)
      double precision qtf(numfunc)
      double precision rp(28), rl(28), r(lr), rr(120)
      double precision wa1(numfunc),wa2(numfunc),wa3(numfunc),wa4(numfunc)

      double precision, parameter :: one=1.
!
      double precision af, ah, arytm
      double precision chen, chyd, co2_mixrat, crustal
      double precision dum
      double precision form
      double precision hc1, hc2, hyd, hno2
      double precision ph, photo, pres
      double precision qsat
      double precision rad, rsrate
      double precision salt, sodium, sulfrate, sulfrateb
      double precision temp, tlwc, tmin
      double precision vlwc
      double precision watvap, watcont, wl, wlm, wvol
                ! lwc,wat. vapor in g/m3
                ! crustal species concentration inside droplets (ug/m3)
                ! na concentration inside droplets (ug/m3)
      double precision xprescribe_ph


!
!   unload ipar, rpar values
!
      icount  = ipar(1)
      iprint  = ipar(2)
      iradical= ipar(9)

      temp    = rpar(1)
      pres    = rpar(2)
      watcont = rpar(3)
      watvap  = rpar(4)
      crustal = rpar(5)
      salt    = rpar(6)
      sodium  = rpar(7)
      photo         = rpar( 8)
      co2_mixrat    = rpar( 9)
      xprescribe_ph = rpar(10)
      ph            = rpar(11)
      akeq( 1: 17)  = rpar(21: 37) 
      akhen(1: 21)  = rpar(38: 58) 
      akre( 1:120)  = rpar(59:178) 
      do i = 1, ngas
          gascon(i) = rpar(178+i)
      end do

!
!     calculation of the temperature for this time (temp)
!      (assuming linear temperature change for each operator stepp)
!
      tmin = t                                   !in seconds
      n = numfunc

      call qsaturation (temp, qsat)              ! qsat is in ug/m3
!
!     zero the rate of change vectors
!
      do i=1, meqn1
      yaqprime(i)=0.0
      aqprod(i)=0.0
      aqdest(i)=0.0
      enddo
!
!     set dummy ph to zero for printing only
!
      ph=0.0
!
!     find total lwc
!
      tlwc = watcont*1.e6                            ! in ug/m3
      vlwc=tlwc*1.e-12                               ! vol/vol
!
!     check for negative values
!
!sp      do i=1, meqn1
!sp      if (yaq(i) .le. 0.) yaq(i)=1.e-20
!sp      enddo
!
!     reconstruct the matrices using the available yaq values
!
!     ** gas phase concentrations (in ppm) **
!      (some are assumed to remain constant during the aqueous-phase
!        chemical processes, the rest are updated)
!     note : all hcho and hcooh are still in the gas-phase
!
      spres(1)  = yaq(3)*rideal*temp/(1000.*wso2*pres)     ! so2 (g)
      spres(2)  = 1.e-20                                   ! h2so4 (g)
      spres(3)  = gascon(nghno2)                           ! hno2 (g)
      spres(4)  = 1.e-20                                   ! hno3 (g) [it has already dissolved]
!     spres(5)  = 350.                                     ! co2 (g)
      spres(5)  = co2_mixrat                               ! 2004-nov-15 rce
      spres(6)  = yaq(4)*rideal*temp/(1000.*wh2o2*pres)    ! h2o2 (g)
      hc1=rideal*temp*vlwc*akhen(7)
      hc2=rideal*temp/(1000.*whcho*pres)
      spres(7)  = yaq(1)*hc2/(hc1+1.)                      ! hcho (g)
      spres(8)  = yaq(2)*rideal*temp/(1000.*whcooh*pres)   ! hcooh (g)
      spres(9)  = gascon(ngno)                             ! no (g)
      spres(10) = gascon(ngno2)                            ! no2 (g)
      spres(11) = gascon(ngo3)                             ! o3 (g)
      spres(12) = gascon(ngpan)                            ! pan (g)
      spres(13) = 1.e-20                                   ! ch3coooh (g)
      spres(14) = 1.e-20                                   ! ch3ooh (g)
      spres(15) = 1.e-20                                   ! hcl (g)  [it has already dissolved]
      spres(16) = gascon(ngoh)                             ! oh (g)
      spres(17) = gascon(ngho2)                            ! ho2 (g)
      spres(18) = gascon(ngno3)                            ! no3 (g)
      spres(19) = yaq(5)*rideal*temp/(1000.*wnh3*pres)     ! nh3 (g)
      spres(20) = gascon(ngch3o2)                          ! ch3o2 (g)
      spres(21) = 1.e-20                                   ! ch3oh (g)
      spres(22) = watvap*rideal*temp/(1000.*18.*pres)      ! h2o (g)
!
! calculation of gcon vector (gas-phase concentrations in mole/l)
!
      do 5 i=1,22
!     gcon(i) = spres(i)*1.e-6/(0.08206*temp)
      gcon(i) = spres(i)*1.e-6/(rideal*temp)
5     continue
!
!     ** radius and lwc for the section
!
      rad = 0.5*avdiam                                ! in m
      wl =  watcont                                   ! in g/m3
      wvol= wl*1.e-6                                  ! in vol/vol
      wlm = wl*1.e6                                   ! in ug/m3
!
!     loading of all metal species
!     note : na+ is an input. the rest of the metal species are
!            calculated as a fraction of the crustal aerosol mass.
!
      cmet(1) = firon*crustal*1000./(55.85*wlm)  ! fe(3+) mol/l
      cmet(2) = fman*crustal*1000./(54.9*wlm)    ! mn(2+) mol/l
      cmet(3) = salt*1000./(23.*wlm)          ! na(+)  mol/l
      cmet(4) = caratio*crustal*1000./(40.08*wlm)   ! ca(2+) mol/l
!
!     do not let the fe(3+) and mn(2+) concentrations exceed a
!     certain limit because they cause extreme stiffness due to
!     the reaction s(iv)->s(vi)
!
!      if (cmet(1) .gt. 1.0e-5) cmet(1)=1.0e-5
!      if (cmet(2) .gt. 1.0e-5) cmet(2)=1.0e-5
!
!     loading of the main aqueous concentrations  (in m)
!
      con(1) = yaq(6)*1000./(wmol(1)*wlm)   ! s(iv)
      if (con(1) .lt. 1.e-20) con(1)=1.e-20
      con(2) = yaq(11)*1000./(wmol(2)*wlm)   ! s(vi)
      con(3) = 0.                                ! n(iii) (determined later)
      con(4) = yaq(8)*1000./(wmol(4)*wlm)   ! n(v)
      con(5) = 0.                                ! co2 (determined later)
      con(6) = yaq(7)*1000./(wmol(6)*wlm)      ! h2o2
      if (con(6) .lt. 1.e-20) con(6)=1.e-20
      con(7) = akhen(7)*spres(7)*1.e-6           ! hcho
      con(8) = 0.                                ! hcooh (determined later)
      con(9) = 1.0e-6*akhen(9)*spres(9)          ! no
      con(10) = 1.0e-6*akhen(10)*spres(10)       ! no2
      con(11) = 1.0e-6*akhen(11)*spres(11)       ! o3
      con(12) = 1.0e-6*akhen(12)*spres(12)       ! pan
      con(13) = 1.0e-6*akhen(13)*spres(13)       ! ch3coooh
      con(14) = 1.0e-6*akhen(14)*spres(14)       ! ch3ooh
      con(15) = yaq(9)*1000./(wmol(15)*wlm) ! hcl
      con(16) = 0.                               ! oh (determined later)
      con(17) = 0.                               ! ho2 (determined later)
      con(18) = 0.                               ! no3 (determined later)
      con(19) = yaq(10)*1000./(wmol(19)*wlm)     ! nh3
      con(20) = 1.0e-6*akhen(20)*spres(20)       ! ch3o2
      con(21) = 1.0e-6*akhen(21)*spres(21)       ! ch3oh
      con(22) = 0.                               ! cl (determined later)
      con(23) = 0.                               ! cloh- (determined later)
      con(24) = 0.                               ! so4- (determined later)
      con(25) = 0.                               ! so5- (determined later)
      con(26) = yaq(12)*1000./(wmol(26)*wlm)      ! hso5-
      con(27) = yaq(13)*1000./(wmol(27)*wlm)       ! hoch2so3-
      con(28) = 0.                               ! co3- (determined later)
!
!     set a minimum concentration (to avoid divisions by zero)
!
      do i=1, 28
          if (con(i) .lt. 1.0e-20) con(i)=1.0e-20
      enddo

!
! 27-oct-2005 rce - previously there was a bug here and the code 
!   would continue on even if fullequil failed
!
!     calculation of ph and volatile concentrations (co2, n(iii), hcooh)
!         (solve the system of equations)
!     if ipar(7)>0, this means that fullequil has failed 
!         and it's time to shut down the integration
!     do this by setting the yaqprime=0, which hopefully will allow
!         the integrator to complete
!
      if (ipar(7) .le. 0)   &
          call fullequil( con, spres, cmet, akeq, akhen, vlwc,   &
               temp, hyd, xprescribe_ph, ipar(7) )
      if (ipar(7) .gt. 0) then
          do i = 1, meqn1
              yaqprime(i) = 0.0
          end do
          ph=30.0
          ipar(1) = icount
          rpar(11) = ph
#if defined ( ccboxtest_box_testing_active )
          con_ccboxtest(:) = con(:)
#endif
          return
      end if
      ph=-log10(hyd)

!     when iradical = 100/101/102, the radical species are computed
!         by directly by dvode rather than by hybrd
!     the con's for radicals are loaded here (after call to fullequil)
!         as that more closely follows the approach with hybrd 
      if (iabs(iradical-101) .le. 1) then
          con(16) = yaq(14)*1000./(wmol(16)*wlm)   ! oh    (mw = 17)
          con(17) = yaq(15)*1000./(wmol(17)*wlm)   ! ho2   (mw = 33)
          con(18) = yaq(16)*1000./(wmol(18)*wlm)   ! no3   (mw = 62)
          con(23) = yaq(17)*1000./(wmol(23)*wlm)   ! cloh- (mw = 52.5)
          con(24) = yaq(18)*1000./(wmol(24)*wlm)   ! so4-  (mw = 96)
          con(25) = yaq(19)*1000./(wmol(25)*wlm)   ! so5-  (mw = 122)
          con(28) = yaq(20)*1000./(wmol(28)*wlm)   ! co3-  (mw = 60)
          dum = 1.0d-35
          con(16) = max( con(16), dum )
          con(17) = max( con(17), dum )
          con(18) = max( con(18), dum )
          con(23) = max( con(23), dum )
          con(24) = max( con(24), dum )
          con(25) = max( con(25), dum )
          con(28) = max( con(28), dum )
      end if

!
      ah = rideal*temp*vlwc*akhen(3)*(1.+akeq(7)/hyd)/pres
      hno2=spres(3)/(1.+ah)
      con(3)=akhen(3)*(1.+akeq(7)/hyd)*1.e-6*hno2
!
      chen=akhen(5)*(1.+akeq(8)/hyd+akeq(8)*akeq(9)/hyd**2)
      con(5)=chen*spres(5)*1.e-6          ! [co2 t]aq m
!
      af=rideal*temp*vlwc*akhen(8)*(1.+akeq(13)/hyd)/pres
      form=spres(8)/(1.+af)               ! new hcooh(g) ppm
      con(8)=akhen(8)*(1.+akeq(13)/hyd)*1.e-6*form
!
!     we calculate the ionic species concentrations
!
      call values(hyd, con, cmet, akeq, c)
!
!     bypass the radical calculation by hybrd if necessary
!
      if (iradical .eq. 0) go to 270
      if (iabs(iradical-101) .le. 1) go to 280
!
!     this is where the call to hybrd was made when
!        the aqueous radical species were treated as steady state
!     now we treate iradical.gt.0 same as iradical=100
!
      go to 280
!
!     set the radical concentrations to zero
!
270   continue
!     if (info .eq. 2 .or. info .eq. 3   &
!        .or. info .eq. 4 .or. info .eq. 5 .or. iradical .eq. 0) then
      con(16)=1.e-25
      con(17)=1.e-25
      con(18)=1.e-25
      con(23)=1.e-25
      con(24)=1.e-25
      con(25)=1.e-25
      con(28)=1.e-25
!     endif
!
!     pseudo-steady-state approximation for cl radical
!     not used because it is of secondary importance
!sp      call values(hyd, con, cmet, akeq,c)
!sp      call react(c,cmet,con,photo,akre,rr,arytm)
!sp      pro=rr(23)+rr(49)+rr(96)
!sp      if (con(22) .le. 0.0)then
!sp      con(22) = 1.e-20
!sp      go to 20
!sp      endif
!sp      destr=(rr(24)+rr(25)+rr(26)+rr(27)+rr(28)+rr(29)+rr(30)+rr(42)+
!sp     &  rr(56)+rr(61)+rr(69)+rr(109))/con(22)
!sp      if (destr .eq. 0.0)then
!sp      con(22)= 1.e-10
!sp      go to 20
!sp      endif
!sp      con(22)=pro/destr
!sp 20   continue
!

280   continue
      call values(hyd, con, cmet, akeq,c)
!
!     final calculation of reaction rates
!
      call react(c,cmet,con,photo,akre,rr,arytm)
!
!     calculation of net production and consumption rates
!
      call addit(rr, arytm, rp, rl)
!
!     calculation of mass transfer rates
!
      call mass(wvol,rad,temp,gcon,con,c,akeq,akhen,fgl,flg,gfgl,gflg)
!
!     calculation of net rates of change
!
      call differ(rp,rl,fgl,flg,gfgl,gflg,dp,dl)
!
!     calculation of right hand sides of the derivatives
!
!     ** gas-phase species **
!          (rates in ug/m3 air s)
      yaqprime(1) = 1.e9*wvol*wmol(7)*(rp(7)-rl(7))   ! hcho t
      aqprod(1) = 1.e9*wvol*wmol(7)*rp(7)
      aqdest(1) = 1.e9*wvol*wmol(7)*rl(7)
!
      yaqprime(2) = 1.e9*wvol*wmol(8)*(rp(8)-rl(8)) ! hcooh t
      aqprod(2) = 1.e9*wvol*wmol(8)*rp(8)
      aqdest(2) = 1.e9*wvol*wmol(8)*rl(8)
!
      yaqprime(3) = 1.e9*gmol(1)*(dp(29)-dl(29))    ! so2(g)
      aqprod(3)   = 1.e9*gmol(1)*dp(29)
      aqdest(3)   = 1.e9*gmol(1)*dl(29)
!
      yaqprime(4) = 1.e9*gmol(6)*(dp(34)-dl(34)) ! h2o2(g)
      aqprod(4)   = 1.e9*gmol(6)*dp(34)
      aqdest(4)   = 1.e9*gmol(6)*dl(34)
!
      yaqprime(5) = 1.e9*gmol(19)*(dp(47)-dl(47)) ! nh3(g)
      aqprod(5)   = 1.e9*gmol(19)*dp(47)
      aqdest(5)   = 1.e9*gmol(19)*dl(47)
!
!     ** aqueous-phase species **
!
      yaqprime(6)= 1.e9*wvol*wmol(1)*(dp(1)-dl(1))   ! s(iv)
      aqprod(6)  = 1.e9*wvol*wmol(1)*dp(1)
      aqdest(6)  = 1.e9*wvol*wmol(1)*dl(1)
!
      yaqprime(7)= 1.e9*wvol*wmol(6)*(dp(6)-dl(6))   ! h2o2
      aqprod(7)  = 1.e9*wvol*wmol(6)*dp(6)
      aqdest(7)  = 1.e9*wvol*wmol(6)*dl(6)
!
      yaqprime(8)= 1.e9*wvol*wmol(4)*(dp(4)-dl(4))   ! n(v)
      aqprod(8)  = 1.e9*wvol*wmol(4)*dp(4)
      aqdest(8)  = 1.e9*wvol*wmol(4)*dl(4)
!
      yaqprime(9)= 1.e9*wvol*wmol(15)*(dp(15)-dl(15)) ! cl-
      aqprod(9)  = 1.e9*wvol*wmol(15)*dp(15)
      aqdest(9)  = 1.e9*wvol*wmol(15)*dl(15)
!
      yaqprime(10)= 1.e9*wvol*wmol(19)*(dp(19)-dl(19))  ! nh4+
      aqprod(10)  = 1.e9*wvol*wmol(19)*dp(19)
      aqdest(10)  = 1.e9*wvol*wmol(19)*dl(19)
!
      yaqprime(11)= 1.e9*wvol*wmol(2)*(dp(2)-dl(2))   ! s(vi)
      aqprod(11)  = 1.e9*wvol*wmol(2)*dp(2)
      aqdest(11)  = 1.e9*wvol*wmol(2)*dl(2)
!
      yaqprime(12)= 1.e9*wvol*wmol(26)*(dp(26)-dl(26)) ! hso5-
      aqprod(12)  = 1.e9*wvol*wmol(26)*dp(26)
      aqdest(12)  = 1.e9*wvol*wmol(26)*dl(26)
!
      yaqprime(13)= 1.e9*wvol*wmol(27)*(dp(27)-dl(27)) ! hmsa
      aqprod(13)  = 1.e9*wvol*wmol(27)*dp(27)
      aqdest(13)  = 1.e9*wvol*wmol(27)*dl(27)
!
      if (iabs(iradical-101) .le. 1) then
!
          yaqprime(14)= 1.e9*wvol*wmol(16)*(dp(16)-dl(16)) ! oh(aq)
          aqprod(14)  = 1.e9*wvol*wmol(16)*dp(16)
          aqdest(14)  = 1.e9*wvol*wmol(16)*dl(16)
!
          yaqprime(15)= 1.e9*wvol*wmol(17)*(dp(17)-dl(17)) ! ho2(aq)
          aqprod(15)  = 1.e9*wvol*wmol(17)*dp(17)
          aqdest(15)  = 1.e9*wvol*wmol(17)*dl(17)
!
          yaqprime(16)= 1.e9*wvol*wmol(18)*(dp(18)-dl(18)) ! no3(aq)
          aqprod(16)  = 1.e9*wvol*wmol(18)*dp(18)
          aqdest(16)  = 1.e9*wvol*wmol(18)*dl(18)
!
          yaqprime(17)= 1.e9*wvol*wmol(23)*(dp(23)-dl(23)) ! cloh-(aq)
          aqprod(17)  = 1.e9*wvol*wmol(23)*dp(23)
          aqdest(17)  = 1.e9*wvol*wmol(23)*dl(23)
!
          yaqprime(18)= 1.e9*wvol*wmol(24)*(dp(24)-dl(24)) ! so4-(aq)
          aqprod(18)  = 1.e9*wvol*wmol(24)*dp(24)
          aqdest(18)  = 1.e9*wvol*wmol(24)*dl(24)
!
          yaqprime(19)= 1.e9*wvol*wmol(25)*(dp(25)-dl(25)) ! so5-(aq)
          aqprod(19)  = 1.e9*wvol*wmol(25)*dp(25)
          aqdest(19)  = 1.e9*wvol*wmol(25)*dl(25)
!
          yaqprime(20)= 1.e9*wvol*wmol(28)*(dp(28)-dl(28)) ! co3-(aq)
          aqprod(20)  = 1.e9*wvol*wmol(28)*dp(28)
          aqdest(20)  = 1.e9*wvol*wmol(28)*dl(28)
      end if
!
!     calculation of appropriate destruction rate
!
      do 50 i=1, meqn1
         if (yaq(i) .le. 1.e-20) then
         aqdest(i) = 0.0
         go to 50
         endif
         aqdest(i) = aqdest(i)/yaq(i)
 50   continue
!
!     change to avoid divisions by zero in integration
!
      do i=1,meqn1
      if (aqdest(i) .le. 1.e-18) aqdest(i)=1.e-18
      enddo

!
!     calculation of characteristic times (used for debugging)
!
!db      tsm=100.
!db      do 110 i=1, meqn1
!
!db      if (aqdest(i) .le. 1.e-10) go to 110
!db      tchar=1./aqdest(i)
!
!db      if (tchar .lt. 0.01)then
!db      write(80,*)tmin,i,yaq(i),yaqprime(i),tchar
!db      write(6,*) i,yaq(i),yprime(i)
!db      endif
!
!db      if (tchar .lt. tsm) then
!db      tsm=tchar
!db      endif
!
!db110   continue

!
!     mass balance for sulfur
!     original sulfrate calc includes so2(g), so2(aq), so4=, hso5-, hmsa
!         and does not always balance
!     sulfrateb calc also includes so4-, so5- and gives a closer balance
!
!     sulfrate=yaqprime(3)/gmol(1)+yaqprime(6)/wmol(1)+   &
!      yaqprime(11)/wmol(2)
!     rsrate=sulfrate/(abs(yaqprime(3))+abs(yaqprime(11)) +   &
!       abs(yaqprime(6)))
!     if (abs(rsrate) .ge. 0.01) then
!     write(80, *)'problem at ',tmin/60.
!     write(80, *) yaqprime(3),yaqprime(6),yaqprime(11)
!     write(80, *) rsrate
!     write(80, *)'************************'
!     endif
      sulfrate = (yaqprime( 3)/gmol( 1)) + (yaqprime( 6)/wmol( 1)) +   &
                 (yaqprime(11)/wmol( 2)) + (yaqprime(12)/wmol(26)) +   &
                 (yaqprime(13)/wmol(27))
      sulfrateb = sulfrate +   &
                 1.0e9*wvol*(rp(24) - rl(24) + rp(25) - rl(25))
      rsrate =  abs(yaqprime( 3)/gmol( 1)) + abs(yaqprime( 6)/wmol( 1)) +   &
                abs(yaqprime(11)/wmol( 2)) + abs(yaqprime(12)/wmol(26)) +   &
                abs(yaqprime(13)/wmol(27))
      rsrate = max(rsrate, 1.0d-30)
      if (mdiag_rsrate .gt. 0) then
        if (abs(sulfrateb/rsrate) .ge. 1.0e-5) then
          write(6,*)
          write(6,'(a,1p,3e11.2)')   &
              'aqratesa sulfbal prob - rerr, rerrb, t =',   &
              (sulfrate/rsrate), (sulfrateb/rsrate), tmin
          write(6,'(a,1p,e15.6/4e15.6)')   &
              'yaqprime/wmol so2,siv,svi,hso5-,hmsa   =',    &
              (yaqprime(3)/gmol(1)), (yaqprime(6)/wmol(1)),   &
              (yaqprime(11)/wmol(2)), (yaqprime(12)/wmol(26)),   &
              (yaqprime(13)/wmol(27))
          write(6,*)
        end if
      end if

!
!     diagnostic output
!
      icount=icount+1
      if (icount .ge. iprint)then
!rce      write(6,120)tmin/60.,yaq(11)       !,ph,rsrate,x(1)*1.e12,yaq(13)
!rce      write(79,*)tmin/60.,ph
!     printing of all reaction rates for debugging
!sp       write(3,*)tmin/60.,'****(um/hr)*******'
!sp       do i=1,109
!sp           write(3,*)i,rr(i)*1.e6*3600.
!sp       enddo
          icount=0
      endif
!120  format(1x,2(1x,f8.4))
120   format( 'aqratesa - tmin, yaq(11)=so4', 2(1x,f8.4) )

!
!   load ipar, rpar values
!
      ipar(1) = icount
      rpar(11) = ph
#if defined ( ccboxtest_box_testing_active )
      con_ccboxtest(:) = con(:)
#endif

!     write(*,'(a,1p,8e10.2 )') 'xxx t,yaq1-7 ', t, (yaq(i), i=1,7)
!     write(*,'(a,1p,8e10.2 )') 'xxx t,yaq8-13', t, (yaq(i), i=8,13)
!     write(*,'(a,1p,8e10.2 )') 'xxx t,rad-con', t,   &
!         (con(i), i=16,18), (con(i), i=23,25), (con(i), i=28,28) 
!     dum = 1.e9*wvol
!     write(*,'(a,1p,8e10.2/)') 'xxx t,rad-yaq', t,   &
!         (con(i)*wmol(i)*dum, i=16,18), (con(i)*wmol(i)*dum, i=23,25),   &
!         (con(i)*wmol(i)*dum, i=28,28) 

      return
      end subroutine aqratesa                                       




!************************************************************************
! this routine calculates the the steady state species concentrations
!************************************************************************
      subroutine steady(radius,temp,c,con,gcon,akeq,akhen,akre)
!
!..inputs:
!     radius : droplet radius in m
!     temp : temperature (in k)
!     c(46) : the concentrations of the rest of the aqueous-phase species
!     gcon(22) : gas-phase concentrations
!     akeq,akhen,akre : reaction constants
!..outputs:
!     x(8) the values of the steady state species concentrations
!

!   arguments
      double precision radius, temp
      double precision c(46),gcon(22),akeq(17),akhen(21),akre(120)
      double precision con(28)

!   local variables
      integer icount
      double precision a1, a2, a3, a4, acc, airl
      double precision dg, ho2, o2
      double precision kn,n,ikn,kmt
      double precision rideal
      double precision x(8)
!
!     airl is the mean free path of air. later we have to substitute
!     the numerical value given here by a function of temperature
!     airl=65x10-9  m
      airl=65.e-9
!     kn is the knudsen number
      kn=airl/radius
      ikn=1.0/kn
!     acc is the accomodation coefficient assumed the same here for
!     all the species
      acc=0.01
!     n is the coefficient entering the flux expression
      n=1.0/(1.+((1.33+0.71*ikn)/(1.+ikn)+4.*(1.-acc)   &
      /(3.*acc))*kn)
!     dg is the gas phase diffusivity assumed here the same for all
!     the gases. dg=1.x10-5 m**2/sec
      dg=1.0e-5
!     rideal is the gas constant in [atm/K/(mol/liter)]
      rideal=0.082058
      kmt=(3.0*n*dg)/(radius*radius)
!
!     iteration loop
!
      do icount=1,2

!
!     no3 calculation
!
      x(3)=(kmt*gcon(18))/(akre(43)*c(8)+akre(45)+akre(46)*c(29)+   &
      akre(47)*c(30) +akre(48)*c(14)+   &
      akre(49)*c(27)+akre(54)*c(18)+akre(59)*c(19)+akre(71)*c(35)+   &
      akre(109)*c(2)+kmt/(akhen(18)*rideal*temp))
      con(18)=x(3)
!
!     so4-  calculation
!
      x(5)=(akre(109)*c(2)*x(3)+2.*akre(86)*c(40)*c(40))   &
       /(akre(89)*c(41)+akre(92)*c(2)+   &
      akre(93)*c(3)+akre(94)*c(29)+akre(95)*c(30)+   &
      akre(96)*c(45)+akre(97)*c(14)+akre(98)*c(8)+   &
      akre(99)*c(12)+akre(100)*c(19)+akre(101)*c(27)+   &
      akre(102)*c(18)+akre(108)*c(35))
      c(39)=x(5)
      con(24)=c(39)
!
      a1=c(46)/(akeq(15)+c(46))
      a2=akeq(15)/(akeq(15)+c(46))
!
!     ho2 calculation
!
      x(2)=((akre(48)*c(14)+akre(54)*c(18)+akre(59)*c(19)+   &
            akre(71)*c(35)) * x(3)+   &
        (akre(97)*c(14)+akre(100)*c(19)+akre(102)*c(18)+   &
        akre(108)*c(35))*x(5)+   &
       2.0*akre(14)*c(45)*c(22)+   &
      akre(28)*c(14)*c(36)+akre(29)*c(14)*c(37)+akre(55)*c(18)*c(22)+   &
      akre(56)*c(18)*c(36)+akre(61)*c(19)*c(36)+akre(69)*c(35)*c(36)+   &
      akre(65)*c(25)+akre(15)*c(22)*c(15)+akre(58)*c(19)*c(22)+   &
      kmt*gcon(17) +akre(5)*c(14)*c(28)+akre(11)*c(22)*c(28)+   &
      akre(20)*c(14)*c(44)+akre(50)*c(17)*c(28)+akre(52)*c(18)*c(28)+   &
      akre(57)*c(19)*c(28)+akre(60)*c(19)*c(44)+akre(67)*c(35)*c(28)+   &
      akre(68)*c(35)*c(44)+akre(84)*c(18)*c(40)+   &
      akre(85)*c(19)*c(40))/   &
      (a1*(akre(3)*c(28)+2.*akre(6)*c(29)+2.*akre(7)*c(30)+   &
      akre(9)*c(14)+akre(12)*c(22)+akre(25)*c(36)+   &
      akre(27)*c(37)+akre(46)*x(3)+akre(63)*c(34)+   &
      akre(94)*c(39)+akre(107)*c(3))+   &
      a2*(akre(4)*c(28)+2.*akre(8)*c(30)+akre(10)*c(14)+   &
      akre(13)*c(22)+akre(18)*c(12)+akre(19)*c(44)+akre(26)*c(36)+   &
      akre(47)*x(3)+akre(64)*c(34)+akre(83)*c(40)+akre(95)*c(39))   &
      +(kmt*a1)/(akhen(17)*rideal*temp))
!
      ho2=(x(2)*c(46))/(akeq(15)+c(46))
      o2=(x(2)*akeq(15))/(akeq(15)+c(46))
      c(29)=ho2
      c(30)=o2
      con(17)=c(29)+c(30)
!
      a3=(akre(21)*akre(22)*c(27))/(akre(22)+akre(23)*c(46))
      a4=(akre(22)*akre(24)*c(37))/(akre(22)+akre(23)*c(46))
!
!     oh calculation
!
      x(1)=(2.*akre(1)*c(14)+akre(15)*c(15)*c(22)+akre(30)*c(45)*   &
      c(36)+   &
      akre(35)*c(7)+akre(36)*c(8)+akre(44)*c(10)+akre(55)*c(18)*c(22)+   &
      akre(58)*c(19)*c(22)+akre(65)*c(25)+kmt*gcon(16)+a4+   &
      (akre(9)*c(14)+akre(12)*c(22)+akre(107)*c(3))*ho2+   &
      (akre(10)*c(14)+akre(13)*c(22))*o2+akre(96)*c(45)*x(5))/   &
      (akre(3)*ho2+akre(4)*o2+akre(5)*c(14)+akre(11)*c(22)+   &
      akre(17)*c(12)+akre(21)*c(27)+akre(33)*c(20)+akre(34)*c(21)+   &
      akre(37)*c(7)+akre(38)*c(8)+akre(50)*c(17)+akre(52)*c(18)+   &
      akre(57)*c(19)+akre(66)*c(25)+akre(67)*c(35)+akre(80)*c(3)+   &
      akre(81)*c(2)+akre(88)*c(41)+akre(115)*c(42)+   &
      kmt/(akhen(16)*rideal*temp)-a3)
      c(28)=x(1)
      con(16)=c(28)
!
!     cloh- calculation
!
      x(4)=(akre(21)*c(27)*x(1)+akre(24)*c(37))/(akre(22)+   &
      akre(23)*c(46))
      c(38)=x(4)
      con(23)=c(38)
!
!     co3- calculation
!
      x(7)=(akre(17)*c(12)*x(1)+akre(99)*c(12)*x(5)+akre(18)*c(12)*o2)/   &
      (akre(19)*o2+akre(20)*c(14)+akre(41)*c(8)+akre(60)*c(19)+   &
      akre(68)*c(35))
      c(44)=x(7)
      con(28)=c(44)
!
!     so5- calculation
!
      x(6)=(akre(116)*c(2)*c(36)+(akre(80)*c(3)+akre(81)*c(2)+   &
      akre(88)*c(41)+akre(115)*c(42))*x(1)+(akre(89)*c(41)+   &
      akre(92)*c(2)+akre(93)*c(3))*x(5))/   &
      (akre(83)*o2+akre(84)*c(18)+akre(85)*c(19)+2.*akre(86)*c(40))
      c(40)=x(6)
      con(25)=c(40)

      enddo
!


      return
      end subroutine steady                                        




! ***********************************************************************
! the  routine  fullequil  solves the electroneutrality equation
! using  bisection  method when the concentrations of [c], n(iii)
! and hcooh are unknown.
! inputs in the sub are con(28),spres(21),cmet(4),akeq(17),akhen(21).
! output is the value of [h+]
! the routine electro gives values of the electroneutrality equation.
! inputs in the sub are x=[h+],con(28),cmet(4),akeq(17).
! output is the value of f ( f(x)=0.0 at the solution)
! ***********************************************************************

       subroutine fullequil(acon,aspres,acmet,aakeq,aakhen,awv,   &
        atemp,axsol,xprescribe_ph,nerr_fullequil)

       use module_data_cmu_bulkaqchem, only:  mdiag_fullequil

!   arguments
       integer nerr_fullequil
       double precision acon(28), aspres(21), acmet(4), aakeq(17),   &
           aakhen(21)
       double precision awv, atemp, axsol, xprescribe_ph

!   local variables
       integer i, k, ntry
       integer ipass_01, idum_01
       double precision aa, bb, error, f, fa, fm, rtol, x, xm
       double precision wv, temp, xsol
       double precision con(28), spres(21), cmet(4), akeq(17), akhen(21)
!
!      change variables to double precision to avoid low ph errors
!
       ipass_01 = 1
       idum_01 = 0
300    continue

       do k=1,28
       con(k)=acon(k)
       enddo
!
       do k=1,21
       spres(k)=aspres(k)
       akhen(k)=aakhen(k)
       enddo
!
       do k=1,4
       cmet(k)=acmet(k)
       enddo
!
       do k=1,17
       akeq(k)=aakeq(k)
       enddo
!
       wv=awv
       temp=atemp
!
!      we find the initial interval [aa,bb] for the bisection method
!      new version (31/10/87)
       x=10.0d0**(-14)
       call electro(x,fa,con,spres,cmet, &
            akeq,akhen,wv,temp,xprescribe_ph)
       aa=x

!      do 1035 i=-14,1
       do 1035 i = -(14+idum_01), (1+idum_01)
           x=10.0d0**i
           call electro(x,f,con,spres,cmet, &
                akeq,akhen,wv,temp,xprescribe_ph)
           if (f*fa .ge. 0.0d0) then
               aa=x
               fa=f
           else
               bb=x
!               fb=f
               go to 1040
           end if
1035   continue

! 27-oct-2005 rce - previously there was a bug here and the code 
!   continued on to label 1040 after reporting the "mistake in fullequil"
!   Now the code tries a greater range of initial ph values,
!      then gives up if it fails.
!
!      unable to find 2 initial hion values that bracket the "solution"
!      if ipass_01 = 1, try again
       if (ipass_01 .eq. 1) then
           write(6,*)    &
           '*** module_cmuaq_bulk - mistake in fullequil with ipass_01 = 1'
           ipass_01 = 2
           idum_01 = 5
           goto 300
       else if (ipass_01 .eq. 2) then
           write(6,*)    &
           '*** module_cmuaq_bulk - mistake in fullequil with ipass_01 = 2'
           ipass_01 = 3
           idum_01 = 10
           goto 300
       end if

!      otherwise, report the error and exit
       nerr_fullequil = nerr_fullequil + 1
       if (mdiag_fullequil .gt. 0) then
           write(6,*)    &
           '*** module_cmuaq_bulk - mistake in fullequil - con, cmet ='
           write(6,*) con, cmet
           return
       end if

1040   continue
!
!      bisection method for the solution of the equation
!      rtol : relative tolerance
       ntry=0
! 02-nov-2005 rce - smaller rtol for h+ makes dvode run faster
!      rtol=0.00001d0
       rtol=1.0d-8
1050   error= dabs(bb-aa)/aa
       ntry=ntry+1
       if (error .le. rtol) then
           xsol=(aa+bb)/2.0d0
           axsol=xsol                        ! single precision
           return
       end if
       xm=(aa+bb)/2.0d0
       call electro(xm,fm,con,spres,cmet, &
            akeq,akhen,wv,temp,xprescribe_ph)
       if (fa*fm .gt.  0.0d0) then
           aa=xm
           fa=fm
       else
           bb=xm
!           fb=fm
       end if
       go to 1050
       end subroutine fullequil                                    




! ***********************************************************************
! routine that gives values of the electroneutrality equation
! called by fullequil
! ***********************************************************************

       subroutine electro(x,f,con,spres,cmet, &
                  zkeq,zkhen,wv,temp,xprescribe_ph)

       use module_data_cmu_bulkaqchem, only:  rideal

!   arguments
!
!   original subr arguments were akeq & akhen
!   renamed them to zkeq & zkhen 
!       to avoid conflict with module_data_cmu_bulkaqchem
!
       double precision x, f, wv, temp, xprescribe_ph
       double precision con(28),spres(21),cmet(4),zkeq(17),zkhen(21)

!   local variables
       double precision bparam, cparam, cl, dfac, dform, diak
       double precision f1, f2, f3, f4, f5, form, hcl, hno2
       double precision cc(46)
!
       cc(2)=(zkeq(1)*con(1)*x)/(x*x+zkeq(1)*x+zkeq(1)*zkeq(2))       ! hso3-
       cc(3)=(zkeq(1)*zkeq(2)*con(1))/(x*x+zkeq(1)*x+zkeq(1)*zkeq(2)) ! so3--
       cc(5)=(zkeq(3)*con(2)*x)/(x*x+zkeq(3)*x+zkeq(3)*zkeq(4))       ! hso4-
       cc(6)=(zkeq(3)*zkeq(4)*con(2))/(x*x+zkeq(3)*x+zkeq(3)*zkeq(4)) ! so4--
!
!      ** no2- calculation from equilibrium **
       dfac=rideal*temp*wv*zkhen(3)*(1.+zkeq(7)/x)
       hno2 = spres(3)/(1.+dfac)                        ! new hno2(g) in ppm
       cc(8)=zkhen(3)*1.e-6*(zkeq(7)/x)*hno2
!
       cc(10)=(zkeq(6)*con(4))/(x+zkeq(6))                            ! no3-
!
!      ** co2 equilibrium (constant gas co2 concentration) **
       cc(12)= zkeq(8)*zkhen(5)*spres(5)*1.e-6/x
       cc(13)= zkeq(9)*cc(12)/x
!
       cc(15)=(zkeq(5)*con(6))/(x+zkeq(5))                            ! ho2-
!
!      ** hcoo- equilibrium (partitioning with gas-phase) **
       dform=rideal*temp*wv*zkhen(8)*(1.+zkeq(13)/x)
       form=spres(8)/(1.+dform)                          ! new hcooh
       cc(19)=zkhen(8)*1.e-6*(zkeq(13)/x)*form
!
       cc(30)=(zkeq(15)*con(17))/(x+zkeq(15))                         ! o2-
       cc(38)=con(23)                                                 ! cloh-
       cc(39)=con(24)                                                 ! so4-
       cc(40)=con(25)                                                 ! so5-
       cc(41)=con(26)                                                 ! hso5-
       cc(42)=(x*con(27))/(x+zkeq(17))                                ! hoch2so3-
       cc(43)=(zkeq(17)*con(27))/(x+zkeq(17))                         ! -och2so3-
       cc(44)=con(28)                                                 ! co3-
       cc(45)=zkeq(11)/x                                              ! oh-

         bparam=zkeq(16)+con(15)-con(22)
         cparam=zkeq(16)*con(22)
         diak=bparam*bparam+4.0*cparam
         if (diak .le. 0.) diak=1.0e-20
         cl=(-bparam+(diak)**0.5)/2.0
         hcl=(x*(con(15)-con(22)+cl))/(x+zkeq(14))
         cc(27)=(zkeq(14)*hcl)/x                                      ! cl-
         cc(36)=(zkeq(14)*cl*hcl)/(zkeq(16)*x)                        ! cl2-

        cc(33)=(zkeq(10)*x*con(19))/(zkeq(11)+zkeq(10)*x)             ! nh4+
        cc(46)=x                                                      ! h+

        f1=cc(2)+2.0*cc(3)+cc(5)+2.0*cc(6)+cc(8)+cc(10)
        f2=cc(12)+2.0*cc(13)+cc(15)+cc(19)+cc(27)+cc(30)
        f3=cc(36)+cc(38)+cc(39)+cc(40)+cc(41)+cc(42)
        f4=2.0*cc(43)+cc(44)+cc(45)-cc(33)-cc(46)
        f5=-3.0*cmet(1)-2.0*cmet(2)-cmet(3)-2.0*cmet(4)

        f=f1+f2+f3+f4+f5

        if (xprescribe_ph .ge. 0.0) then
            f = 10.0**(-xprescribe_ph) - x
        end if

        return
        end subroutine electro                                       



!----------------------------------------------------------------------
!
! routines used by the aqeous-phase module
!
! 1. dropinit : initialization

      subroutine dropinit

      use module_data_cmu_bulkaqchem, only:   &
              amol, gmol,   &
              wmol, wh2o2, wh2so4, whcho, whcl, whcooh, whno3, wnh3, wso2


!   local variables


!
!     loading of molecular weights
!
      wso2 = 64.
      wh2o2 = 34.
      whcho = 30.
      whcooh = 46.
      wnh3 = 17.
      whno3 = 63.
      whcl = 36.5
      wh2so4 = 98.
!
!      molecular weights
!
       wmol(1)= 81.0e0
       wmol(2)= 96.0e0
       wmol(3)= 47.0e0
       wmol(4)= 62.0e0
       wmol(5)= 62.0e0
       wmol(6)= 34.0e0
       wmol(7)= 48.0e0  ! was previously 60.0e0
       wmol(8)= 46.0e0
       wmol(9)= 30.0e0
       wmol(10)=46.0e0
       wmol(11)=48.0e0
       wmol(12)=121.0e0
       wmol(13)=76.0e0
       wmol(14)=48.0e0
       wmol(15)=35.5e0
       wmol(16)=17.0e0
       wmol(17)=33.0e0
       wmol(18)=62.0e0
       wmol(19)=18.0e0
       wmol(20)=47.0e0
       wmol(21)=32.0e0
       wmol(22)=35.5e0
       wmol(23)=52.50e0
       wmol(24)=96.0e0
       wmol(25)=112.0e0
       wmol(26)=113.0e0
       wmol(27)=111.0e0
       wmol(28)=60.00e0
       wmol(29)=18.0e0
!
       amol(1)= 55.85e0
       amol(2)= 55.0e0
       amol(3)= 23.0e0
!
       gmol(1)=64.0
       gmol(2)=98.08
       gmol(3)=47.02
       gmol(4)=63.02
       gmol(5)=44.01
       gmol(6)=34.02
! 09-nov-2005 rce - set gmol(6) == wh2o2 to conserve h2o2
       gmol(6)=34.0
       gmol(7)=30.03
       gmol(8)=46.00
       gmol(9)=30.01
       gmol(10)=46.01
       gmol(11)=48.00
       gmol(12)=121.05
       gmol(13)=76.00
       gmol(14)=48.00
       gmol(15)=36.50
       gmol(16)=17.00
       gmol(17)=33.01
       gmol(18)=62.01
       gmol(19)=17.00
       gmol(20)=47.00
       gmol(21)=32.00
       gmol(22)=18.00

      return
      end subroutine dropinit



!----------------------------------------------------------------------
       subroutine qsaturation(temp,qsat)
!
!      this routine calculates the saturation mass concentration (in ug/m3)
!      over liquid water for a temperature temp (k)
!

!   arguments
       double precision temp,qsat

!   local variables
       double precision psat, t  ! these should be double precision ?
       double precision rideal,a0,a1,a2,a3,a4,a5,a6,esat,csat
!
       t = temp-273.15                             ! in c
       rideal = 0.082058d0 		! gas constant in [atm/K/(mol/liter)]
       a0 = 6.107799961d-0
       a1 = 4.436518521d-1
       a2 = 1.428945805d-2
       a3 = 2.650648471d-4
       a4 = 3.031240396d-6
       a5 = 2.034080948d-8
       a6 = 6.136820929d-11
!
       esat=a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+a6*t))))) ! in mb
       psat = esat/(1000.0*1.01325)                    ! in atm
       csat = psat/(rideal*temp)                       ! in mole/l
       qsat = 18000.0d0*csat*1.e6                      ! in ug/m3
!       write(6,*)t,esat/1000.,qsat
       return
       end subroutine qsaturation           




!      s u b r o u t i n e s     f o r    d r o p l e t     p r o g r a m
!************************************************************************
! this routine calculates the 20 equilibrium costants {akeq}, the 20
! henry's law costants {akhen} and the 120 reaction rate costants {akre}
!             with only input the temperature (temp).
!    included in the routine are the corresponding enthalpies
!   {dheq,dhhen,dhre} and the costants at 298 k {bkeq,bkhen,bkre}.
!************************************************************************

       subroutine constants( temp, akeq, akhen, akre )

       use module_data_cmu_bulkaqchem, only:   &
               maqurxn_all, maqurxn_sulf1, mopt_eqrt_cons

!   arguments
       double precision temp, akeq(17), akhen(21), akre(120)

!   local variable
       integer i, iusei
       double precision, save :: dheq(17),dhhen(21),dhre(120)
       double precision, save :: bkeq(17),bkhen(21),bkre(120)
       double precision :: bkre_75, dhre_75


       data dheq/1960.,1500.,0.,2720.,-3730.,8700.,-1260.,   &
       -1000.,-1760.,   &
       -450.,-6710.,4020.,-20.,6900.,0.e0,0.,0./
       data bkeq/1.23e-2,6.61e-8,1.e3,1.02e-2,2.2e-12,15.4,5.1e-4,   &
       4.46e-7,4.68e-11,1.75e-5,1.0e-14,1.82e3,1.78e-4,1.74e6,3.5e-5,   &
       5.26e-6,2.0e-12/
       data dhhen/3120.,0.0,4780.,8700.,2420.,6620.,   &
       6460.e0,5740.e0,1480.e0,   &
       2500.e0,2300.e0,5910.e0,6170.e0,5610.e0,2020.e0,5280.e0,   &
       6640.e0,8700.e0,3400.e0,   &
       5600.e0,4900.e0/
       data bkhen/1.23e0,0.0e0,49.e0,2.1e5,3.4e-2,7.45e4,6.3e3,   &
       3.5e3,1.9e-3,   &
       0.01e0,1.13e-2,2.9e0,473.e0,227.e0,727.e0,25.e0,2000.e0,2.1e5,   &
       75.e0,6.e0,220.e0/


       data dhre/0.0e0,0.0e0,-1500.e0,-1500.e0,-1700.e0,-2365.e0,   &
       -1500.e0,0.0e0,0.0e0,   &
       0.0e0,0.0e0,0.0e0,-1500.e0,0.0e0,-2520.e0,0.0e0,-1910.e0,   &
       0.0e0,-1500.e0,-2820.e0,   &
       -1500.e0,0.0e0,0.0e0,0.0e0,-1500.e0,-1500.e0,-1500.e0,   &
       -3370.e0,0.0e0,-2160.e0,   &
       -1500.e0,-1500.e0,-1500.e0,-1500.e0,0.0e0,0.0e0,-1500.e0,   &
       -1500.e0,-6693.e0,-6950.e0,   &
       0.0e0,-1500.e0,-1500.e0,0.0e0,0.0e0,-1500.e0,-1500.e0,   &
       -2800.e0,-1500.e0,-1500.e0,   &
       0.0e0,-1500.e0,-5180.e0,-3200.e0,0.0e0,-4300.e0,-1500.e0,   &
       0.0e0,-1500.e0,-3400.e0,   &
       -2600.e0,0.0e0,-3000.e0,-1600.e0,0.0e0,-1700.e0,-1500.e0,   &
       -4500.e0,-4400.e0,-1800.e0,   &
       -2800.e0,0.0e0,-5530.e0,-5280.e0,-4430.e0,-13700.e0,   &
       -11000.e0,-13700e0,-11000.e0,   &
       -1500.e0,-1500.e0,-3100.e0,-1500.e0,-5300.e0,-4000.e0,   &
       -1500.e0,-4755.e0,-1900.e0,   &
        0.0e0,-6650.e0,-7050.e0,-1500.e0,-1500.e0,-1500.e0,   &
       -1500.e0,-1500.e0,-2000.e0,   &
       -1500.e0,-2100.e0,-1500.e0,-1500.e0,-2700.e0,0.0e0,   &
       -3800.e0,-4000.e0,0.0e0,0.0e0,   &
       -1800.e0,0.0e0,0.0e0,0.0e0,-6100.e0,-4900.e0,   &
       -4500.e0,-1500.e0,-1500.e0,   &
       -2000.e0,0.e0,-1800.e0,120.e0/


       data bkre/2.5e-6,2.0e-5,7.0e9,1.0e10,2.7e7,   &
       8.6e5,1.0e8,0.3e0,0.5e0,   &
       0.13e0,2.0e9,1.0e4,1.5e9,70.e0,2.8e6,7.8e-3,1.5e7,   &
       1.5e6,4.0e8,8.0e5,   &
       4.3e9,6.1e9,2.1e10,1.3e3,4.5e9,1.0e9,3.1e9,   &
       1.4e5,4.5e7,7.3e6,   &
       2.0e8,1.0e8,2.0e10,1.3e9,3.7e-5,6.3e-6,1.0e9,   &
       1.0e10,6.3e3,5.0e5,   &
       4.0e5,2.5e8,1.2e9,1.0e-7,1.0e-5,4.5e9,1.0e9,   &
       1.0e6,1.0e8,2.0e9,   &
       0.1e0,1.6e8,4.6e-6,2.1e5,5.0e0,6.7e3,2.5e9,100.0e0,   &
       6.0e7,1.1e5,1.9e6,   &
       4.0e-4,7.6e5,5.0e7,5.4e-7,2.7e7,4.5e8,2.6e3,   &
       3.5e3,1.9e7,1.0e6,   &
       2.4e4,3.7e5,1.5e9,1.3e6,4.7e0,0.82e0,5.0e3,1.0e7,   &
       4.6e9,4.2e9,3.0e5,   &
       1.0e8,200.e0,1.4e4,2.e8,7.5e7,1.7e7,1.e5,0.31e0,   &
       1.8e-3,1.3e9,5.3e8,   &
       5.0e9,5.0e9,8.0e7,1.2e7,8.8e8,9.1e6,1.7e8,   &
       2.0e8,1.4e6,6.7e-3,   &
       1.9e7,5.0e7,6.0e2,1.0e6,2.5e7,1.0e8,2.0e6,   &
       1.42e2,4.77e3,2.94e2,   &
       3.6e3,1.4e9,3.4e8,   &
       2.5e4,1.0e5,2.5e7,120.e0/


       do 1020 i=1,17
       akeq(i)=bkeq(i)*exp(dheq(i)*(1.0/temp-1.0/298.0))
 1020  continue

       do 1025 i=1,21
       akhen(i)=bkhen(i)*exp(dhhen(i)*(1.0/temp-1.0/298.0))
 1025  continue

       do 1030 i=1,120
       akre(i)=bkre(i)*exp(dhre(i)*(1.0/temp-1.0/298.0))
 1030  continue

!  when mopt_eqrt_cons=20, set s(iv)+h2o2 rxn rate constant
!  to that used in mtcrm and testaqu22
       if (mopt_eqrt_cons .eq. 20) then
           bkre_75  = 4.19e7
           dhre_75  = -1950.0
           akre(75) = bkre_75*exp(dhre_75*(1.0/temp-1.0/298.0))
       end if

!  turn reactions on/off selectively
       do i = 1, 120
           iusei = 0
           if (maqurxn_all .gt. 0) iusei = 1
           if (maqurxn_sulf1 .gt. 0) then
               if ((i .ge. 72) .and. (i .le. 75)) iusei = 1
           end if
           if (iusei .le. 0) akre(i) = 0.0
           if (iusei .le. 0) bkre(i) = 0.0
       end do

       return
       end subroutine constants      




!*************************************************************************
!  this   routine   calculates   the   values   of  the  concentrations
! of all the 46 species  that  appear  in  our  aqueous  phase  mechanism.
! it  has  as  inputs  the  values of [h+],con(28),cmet(3),akeq(17) and as
! outputs the values of cc(46)
!*************************************************************************

        subroutine values(x,con,cmet,akeq,cc)

!   arguments
        double precision x
        double precision con(28),cmet(4),akeq(17),cc(46)

!   local variables
        double precision bparam,cparam,diak,cl,hcl
!
!    	species in the aqueous phase mechanism
!    	cc (1 - 46)
!	1.)     so2*h2o		24.)	ch3c(o)ooh
!	2.)	hso3(-)		25.)	ch3ooh
!    	3.)	so3(2-)		26.)	hcl
!    	4.)	h2so4		27.)	cl(-)
!    	5.)	hso4(-)		28.)	oh
!    	6.)	so4(2-)		29.)	ho2
!    	7.)	hno2		30.)	o2(-)
!	8.)	no2(-)		31.)	no3
!	9.)	hno3		32.)	nh4oh
!	10.)	no3(-)		33.)	nh4(+)
!	11.)	co2*h2o		34.)	ch3o2
!	12.)	hco3(-)		35.)	ch3oh
!	13.)	co3(2-)		36.)	cl2(-)
!	14.)	h2o2		37.)	cl
!	15.)	ho2(-)		38.)	cloh(-)
!	16.)	hcho		39.)	so4(-)
!	17.)	h2c(oh)2	40.)	so5(-)
!	18.)	hcooh		41.)	hso5(-)
!	19.)	hcoo(-)		42.)	hoch2so3(-)
!	20.)	no		43.)	och2so3(2-)
!	21.)	no2 		44.)	co3(-)
!	22.)	o3		45.)	oh(-)
!	23.)	pan		46.)	h(+)
!	
!	con(1-28)
!
!	1.)	so2(g)		15.)	hcl(g)
!	2.)	h2so4(g)	16.)	oh(g)
!	3.)	hno2(g)		17.)	ho2(g)
!	4.)	hno3(g)		18.)	no3(g)
!	5.)	co2(g)		19.)	nh3(g)
!	6.)	h2o2(g)		20.)	ch3o2(g)
!	7.)	hcho(g)		21.)	ch3oh(g)
!	8.)	hcooh(g)	22.)	cl2(-), cl
!	9.)	no(g)		23.)	cloh(-)
!	10.)	no2(g)		24.)	so4(-)
!	11.)	o3(g)		25.)	so5(-)
!	12.)	pan(g)		26.)	hso5(-)
!	13.)	ch3c(o)ooh(g)	27.)	hoch2so3(-),och2so3(2-)
!	14.)	ch3ooh(g)	28.)	co3(-)
!	
         bparam=akeq(16)+con(15)-con(22)
         cparam=akeq(16)*con(22)
         diak=bparam*bparam+4.0d0*cparam
         if (diak .le. 0.0d0) diak=1.0d-30
         cl=(-bparam+(diak)**0.5d0)/2.0d0
         hcl=(x*(con(15)-con(22)+cl))/(x+akeq(14))

       cc(1)=(con(1)*x*x)/(x*x+akeq(1)*x+akeq(1)*akeq(2))
       cc(2)=(akeq(1)*con(1)*x)/(x*x+akeq(1)*x+akeq(1)*akeq(2))
       cc(3)=(akeq(1)*akeq(2)*con(1))/(x*x+akeq(1)*x+akeq(1)*akeq(2))

       cc(4)=(con(2)*x*x)/(x*x+akeq(3)*x+akeq(3)*akeq(4))
       cc(5)=(akeq(3)*con(2)*x)/(x*x+akeq(3)*x+akeq(3)*akeq(4))
       cc(6)=(akeq(3)*akeq(4)*con(2))/(x*x+akeq(3)*x+akeq(3)*akeq(4))

       cc(7)=(x*con(3))/(x+akeq(7))
       cc(8)=(akeq(7)*con(3))/(x+akeq(7))

       cc(9)=(x*con(4))/(x+akeq(6))
       cc(10)=(akeq(6)*con(4))/(x+akeq(6))

       cc(11)=(x*x*con(5))/(x*x+akeq(8)*x+akeq(8)*akeq(9))
       cc(12)=(akeq(8)*con(5)*x)/(x*x+akeq(8)*x+akeq(8)*akeq(9))
       cc(13)=(akeq(8)*akeq(9)*con(5))/(x*x+akeq(8)*x+akeq(8)*akeq(9))

       cc(14)=(x*con(6))/(x+akeq(5))
       cc(15)=(akeq(5)*con(6))/(x+akeq(5))

       cc(16)=con(7)/(1.0d0+akeq(12))
       cc(17)=(akeq(12)*con(7))/(1.0d0+akeq(12))

       cc(18)=(x*con(8))/(x+akeq(13))
       cc(19)=(akeq(13)*con(8))/(x+akeq(13))

       cc(20)=con(9)

       cc(21)=con(10)

       cc(22)=con(11)

       cc(23)=con(12)

       cc(24)=con(13)

       cc(25)=con(14)

       cc(26)=hcl
       cc(27)=(akeq(14)*hcl)/x

       cc(28)=con(16)

       cc(29)=(x*con(17))/(x+akeq(15))
       cc(30)=(akeq(15)*con(17))/(x+akeq(15))

       cc(31)=con(18)

       cc(32)=(akeq(11)*con(19))/(akeq(11)+akeq(10)*x)
       cc(33)=(akeq(10)*x*con(19))/(akeq(11)+akeq(10)*x)

       cc(34)=con(20)

       cc(35)=con(21)

       cc(36)=(akeq(14)*cl*hcl)/(akeq(16)*x)
       cc(37)=cl

       cc(38)=con(23)
       cc(39)=con(24)
       cc(40)=con(25)
       cc(41)=con(26)
       cc(42)=(x*con(27))/(x+akeq(17))
       cc(43)=(akeq(17)*con(27))/(x+akeq(17))
       cc(44)=con(28)
       cc(45)=akeq(11)/x
       cc(46)=x

       return
       end subroutine values                    




!************************************************************************
! this program contains the routine react which calculates
! the rates of all the reactions taking place in the aqueous phase.
! inputs in the sub are the 46 concentrations ,the 3 metal concentrations
! the 28 main species concentrations and the 120 reaction constants.
! output is the matrix of the 120 reaction rates.
!************************************************************************

      subroutine react(c,cmet,con,photo,zkre,rr,arytm)

      use module_data_cmu_bulkaqchem, only:  chlorine, kiron,   &
              mopt_eqrt_cons

!   arguments
!
!   original argument was akre
!   renamed it to zkre 
!       to avoid conflict with module_data_cmu_bulkaqchem
!
!     dimension c(46),cmet(4),con(28),zkre(120),rr(120)
      double precision c(46),cmet(4),con(28),zkre(120),rr(120)
      double precision photo,arytm

!   local variables
      double precision ph, r1, r2, r3, r4, r5, sn


      rr(1)=zkre(1)*c(14)*photo
      rr(2)=zkre(2)*c(22)*photo
      rr(3)=zkre(3)*c(28)*c(29)
      rr(4)=zkre(4)*c(28)*c(30)
      rr(5)=zkre(5)*c(28)*c(14)
      rr(6)=zkre(6)*c(29)*c(29)
      rr(7)=zkre(7)*c(29)*c(30)
      rr(8)=zkre(8)*c(30)*c(30)
      rr(9)=zkre(9)*c(29)*c(14)
      rr(10)=zkre(10)*c(30)*c(14)
      rr(11)=zkre(11)*c(28)*c(22)
      rr(12)=zkre(12)*c(29)*c(22)
      rr(13)=zkre(13)*c(30)*c(22)
      rr(14)=zkre(14)*c(45)*c(22)
      rr(15)=zkre(15)*c(15)*c(22)
      if (c(22) .le. 0.0d0) c(22)=1.0d-30
      rr(16)=zkre(16)*c(14)*(c(22)**0.5)

      rr(17)=zkre(17)*c(12)*c(28)
      rr(18)=zkre(18)*c(12)*c(30)
      rr(19)=zkre(19)*c(44)*c(30)
      rr(20)=zkre(20)*c(44)*c(14)
      rr(21)=zkre(21)*c(27)*c(28)*chlorine
      rr(22)=zkre(22)*c(38)*chlorine
      rr(23)=zkre(23)*c(46)*c(38)*chlorine
      rr(24)=zkre(24)*c(37)*chlorine
      rr(25)=zkre(25)*c(29)*c(36)*chlorine
      rr(26)=zkre(26)*c(30)*c(36)*chlorine
      rr(27)=zkre(27)*c(29)*c(37)*chlorine
      rr(28)=zkre(28)*c(14)*c(36)*chlorine
      rr(29)=zkre(29)*c(37)*c(14)*chlorine
      rr(30)=zkre(30)*c(45)*c(36)*chlorine

      rr(31)=zkre(31)*c(20)*c(21)
      rr(32)=zkre(32)*c(21)*c(21)
      rr(33)=zkre(33)*c(20)*c(28)
      rr(34)=zkre(34)*c(21)*c(28)
      rr(35)=zkre(35)*c(7)*photo
      rr(36)=zkre(36)*c(8)*photo
      rr(37)=zkre(37)*c(7)*c(28)
      rr(38)=zkre(38)*c(8)*c(28)
      rr(39)=zkre(39)*c(46)*c(14)*c(7)
      rr(40)=zkre(40)*c(8)*c(22)
      rr(41)=zkre(41)*c(8)*c(44)
      rr(42)=zkre(42)*c(8)*c(36)*chlorine
      rr(43)=zkre(43)*c(8)*c(31)
      rr(44)=zkre(44)*c(10)*photo
      rr(45)=zkre(45)*c(31)*photo
      rr(46)=zkre(46)*c(31)*c(29)
      rr(47)=zkre(47)*c(31)*c(30)
      rr(48)=zkre(48)*c(31)*c(14)
      rr(49)=zkre(49)*c(31)*c(27)*chlorine
      rr(50)=zkre(50)*c(17)*c(28)
      rr(51)=zkre(51)*c(17)*c(22)
      rr(52)=zkre(52)*c(18)*c(28)
      rr(53)=zkre(53)*c(18)*c(14)
      rr(54)=zkre(54)*c(18)*c(31)
      rr(55)=zkre(55)*c(18)*c(22)
      rr(56)=zkre(56)*c(18)*c(36)*chlorine
      rr(57)=zkre(57)*c(19)*c(28)
      rr(58)=zkre(58)*c(19)*c(22)
      rr(59)=zkre(59)*c(19)*c(31)
      rr(60)=zkre(60)*c(19)*c(44)
      rr(61)=zkre(61)*c(19)*c(36)*chlorine
      rr(62)=zkre(62)*c(23)
      rr(63)=zkre(63)*c(34)*c(29)
      rr(64)=zkre(64)*c(34)*c(30)
      rr(65)=zkre(65)*c(25)*photo
      rr(66)=zkre(66)*c(25)*c(28)
      rr(67)=zkre(67)*c(35)*c(28)
      rr(68)=zkre(68)*c(35)*c(44)
      rr(69)=zkre(69)*c(35)*c(36)*chlorine
      rr(70)=zkre(70)*c(25)*c(28)
      rr(71)=zkre(71)*c(35)*c(31)

      rr(72)=(zkre(72)*c(1)+zkre(73)*c(2)+zkre(74)*c(3))*c(22)
      rr(73)=(zkre(75)*c(14)*c(1))/(1.0d0+16.0d0*c(46))
!  when mopt_eqrt_cons=20, calc s(iv)+h2o2 rxn rate
!  same as in mtcrm and testaqu22
      if (mopt_eqrt_cons .eq. 20) then
          rr(73)=(zkre(75)*c(14)*c(2)*c(46))/(1.0d0+16.0d0*c(46))
      end if
!
!     rate expressions for the metal catalysed oxidation of s(iv)
!
!     ** phenomenological expression by martin et al. (1991) **
      ph=-log10(c(46))
      if (kiron .eq. 1) then
!
      if (ph .le. 3.0) rr(74)=6.*cmet(1)*con(1)/c(46)
      if (ph .gt. 3.0 .and. ph .le. 4.5)   &
       rr(74) = 1.e9*con(1)*cmet(1)*cmet(1)
      if (ph .gt. 4.5 .and. ph .le. 6.5) rr(74) = 1.0e-3*con(1)
      if (ph .gt. 6.5) rr(74)=1.0e-4*con(1)
      endif

!     ** expression by martin (1984) **
      if (kiron .eq. 2) then
      if ((c(46) .ge. 1.0d-4).and.(con(1) .ge. 1.0d-5)) then
      r1=(zkre(76)*cmet(2)*cmet(2))/c(46)
      r2=(zkre(77)*cmet(1)*con(1)/c(46))
      if (cmet(2) .le. 0.0d0) cmet(2)=1.0d-30
      r3=r2*(1.0d0+(1.7d3*cmet(2)**1.5)/(6.3d-6+cmet(1)))
      rr(74)=r1+r3
      go to 1300
      end if

      if (cmet(1)*cmet(2) .lt. 1.0d-15) then
      sn=1.0d0
      else
      sn=3.0d0
      end if

      if ((c(46) .ge. 1.0d-4).and.(con(1) .lt. 1.0d-5)) then
      rr(74)=sn*(zkre(78)*cmet(2)*c(2)+zkre(77)*cmet(1)*con(1)/c(46))
      go to 1300
      end if

      if ((c(46) .lt. 1.0d-4).and.(con(1) .ge. 1.0d-5)) then
      r4=zkre(76)*cmet(2)*cmet(2)/c(46)
      r5=zkre(79)*cmet(1)*con(1)*con(1)
      rr(74)=r4+r5
      go to 1300
      end if

      rr(74)=zkre(78)*cmet(2)*c(2)

1300  continue
      endif

! 09-nov-2005 rce - if rate constants 76,77,78,79 are all zero, set rr(74)=0.
! This allows ANY/ALL rxns to be turned on/off in subr constants.
      if (abs(zkre(76)+zkre(77)+zkre(78)+zkre(79)) .le. 1.0e-37) rr(74)=0.0

!     ** end of martin's expression **

      rr(75)=zkre(80)*c(3)*c(28)
      rr(76)=zkre(81)*c(2)*c(28)
      rr(77)=zkre(82)*c(40)*c(2)+zkre(117)*c(40)*c(3)
      rr(78)=zkre(83)*c(40)*c(30)
      rr(79)=zkre(84)*c(40)*c(18)
      rr(80)=zkre(85)*c(40)*c(19)
      rr(81)=zkre(86)*c(40)*c(40)
      rr(82)=zkre(87)*c(41)*c(2)*c(46)
      rr(83)=zkre(88)*c(41)*c(28)
      rr(84)=zkre(89)*c(41)*c(39)
      rr(85)=zkre(90)*c(41)*c(8)
      rr(86)=zkre(91)*c(41)*c(27)
      rr(87)=zkre(92)*c(39)*c(2)
      rr(88)=zkre(93)*c(39)*c(3)
      rr(89)=zkre(94)*c(39)*c(29)
      rr(90)=zkre(95)*c(39)*c(30)
      rr(91)=zkre(96)*c(39)*c(45)
      rr(92)=zkre(97)*c(39)*c(14)
      rr(93)=zkre(98)*c(39)*c(8)
      rr(94)=zkre(99)*c(39)*c(12)
      rr(95)=zkre(100)*c(39)*c(19)
      rr(96)=zkre(101)*c(39)*c(27)
      rr(97)=zkre(102)*c(39)*c(18)
      rr(98)=zkre(103)*c(23)*c(2)/c(46)
      rr(99)=zkre(104)*c(2)*c(25)*c(46)
      rr(100)=(zkre(105)*c(46)+zkre(106))*c(2)*c(24)
      rr(101)=zkre(107)*c(29)*c(3)+zkre(118)*c(3)*c(30)
      rr(102)=zkre(108)*c(39)*c(35)
      rr(103)=zkre(109)*c(2)*c(31)
      rr(104)=zkre(110)*con(1)*c(21)

      if (c(46) .ge. 1.0d-3) then
      rr(105)=zkre(111)*con(3)*con(1)*c(46)**0.5d0
      arytm=1.0d0
      else
      rr(105)=zkre(112)*c(8)*c(2)*c(46)
      arytm=0.0d0
      end if

      rr(106)=zkre(113)*c(16)*c(2)+zkre(119)*c(16)*c(3)
      rr(107)=zkre(114)*c(42)*c(45)
      rr(108)=zkre(115)*c(42)*c(28)
      rr(109)=zkre(116)*c(2)*c(36)*chlorine+   &
       zkre(116)*c(3)*c(36)*chlorine

      return
      end subroutine react                          




!************************************************************************
! this program contains the routine mass which calculates the mass
! fluxes for the mass balances.
! inputs : wl, radius, temp, gcon(21), con(28), akeq(17),akhen(21)
! outputs : fgl(21),flg(21)
!************************************************************************

      subroutine mass(wl,radius,temp,gcon,con,c,akeq,akhen,fgl,flg,   &
        gfgl,gflg)

!   arguments
      double precision wl, radius, temp
      double precision gcon(22), con(28), c(46), akeq(17), akhen(21)
      double precision fgl(21), flg(21), gfgl(21), gflg(21)

!   local variables
      integer i
      double precision acc, airl, dg, rideal
!     ekhen(i) is the effective henry's law constant
!     dimension ekhen(21)
      double precision ekhen(21)
      double precision kn,n,ikn,kmt


      ekhen(1)=akhen(1)*(1.d0+akeq(1)/c(46)+akeq(1)*akeq(2)/c(46)**2)
      ekhen(2)=1.0d30
      ekhen(3)=akhen(3)*(1.d0+akeq(7)/c(46))
      ekhen(4)=akhen(4)*(1.d0+akeq(6)/c(46))
      ekhen(5)=akhen(5)*(1.d0+akeq(8)/c(46)+akeq(8)*akeq(9)/c(46)**2)
      ekhen(6)=akhen(6)*(1.d0+akeq(5)/c(46))
      ekhen(7)=akhen(7)*((1.d0+akeq(12))/akeq(12))
      ekhen(8)=akhen(8)*(1.d0+akeq(13)/c(46))
      ekhen(9)=akhen(9)
      ekhen(10)=akhen(10)
      ekhen(11)=akhen(11)
      ekhen(12)=akhen(12)
      ekhen(13)=akhen(13)
      ekhen(14)=akhen(14)
      ekhen(15)=akhen(15)*(1.d0+akeq(14)/c(46)+(akeq(14)*c(37))/   &
      (akeq(16)*c(46)))
      ekhen(16)=akhen(16)
      ekhen(17)=akhen(17)*(1.d0+akeq(15)/c(46))
      ekhen(18)=akhen(18)
      ekhen(19)=akhen(19)*(1.d0+akeq(10)/c(45))
      ekhen(20)=akhen(20)
      ekhen(21)=akhen(21)

!     airl is the mean free path of air. later we have to substitute
!     the numerical value given here by a function of temperature
!     airl=65x10-9  m
      airl=65.d-9
!     kn is the knudsen number
      kn=airl/radius
      ikn=1.d0/kn
!     acc is the accomodation coefficient assumed the same here for
!     all the species
      acc=0.1d0
!     n is the coefficient entering the flux expression
      n=1.d0/(1.d0+((1.33d0+0.71d0*ikn)/(1.d0+ikn)+4.d0*(1.d0-acc)   &
      /(3.d0*acc))*kn)
!     dg is the gas phase diffusivity assumed here the same for all
!     the gases.we shall probably have to change it later.
!     dg=1.x10-5 m**2/sec
      dg=1.0d-5
!     rideal is the gas constant in [atm/K/(mol/liter)]
      rideal=0.082058d0
      kmt=(3.0d0*n*dg)/(radius*radius)

      do 1500 i=1,21
      fgl(i)=kmt*gcon(i)
      flg(i)=(kmt*con(i))/(ekhen(i)*rideal*temp)
      gfgl(i)=fgl(i)*wl
      gflg(i)=flg(i)*wl
1500  continue



      return
      end subroutine mass                                              




!***********************************************************************
! this routine calculates the differentials dc(i)/dt for the 28
! main species in the aqueous phase and the 21 species in the gas phase.
! units for all rates are (mol/lt.s)
! note that there are no reaction terms for the gas phase.
!  revised 23 nov 1988
!***********************************************************************
!
       subroutine differ(rp,rl,fgl,flg,gfgl,gflg,dp,dl)

!   arguments
       double precision rp(28),rl(28),fgl(21),flg(21),gfgl(21),gflg(21)
       double precision dp(49),dl(49)

!   local variables
       integer i


       do 1510 i=1,21
       dp(i)=rp(i)+fgl(i)
       dl(i)=rl(i)+flg(i)
1510   continue

       do 1520 i=22,28
       dp(i)=rp(i)
       dl(i)=rl(i)
1520   continue

       do 1530 i=29,49
       dp(i)=gflg(i-28)
       dl(i)=gfgl(i-28)
1530   continue


       return
       end subroutine differ                               




! ***********************************************************************
! the routine addit sums up the rates of
! the 120 reactions to give the rates for the 28 main species.
! input : the 120 reaction rates from the sub react
! output : the 28 formation and destruction rates
!  revised 23 nov 1988
! ***********************************************************************

       subroutine addit(rr,arytm,rp,rl)

!   arguments
       double precision arytm
       double precision rr(120),rp(28),rl(28)


!
!      ** s(iv) **
!
       rp(1)=rr(107)
       rl(1)=+rr(72)+rr(73)+rr(74)+rr(98)+rr(101)+rr(105)*arytm   &
        +rr(76)+rr(77)+rr(82)+rr(87)+rr(99)+rr(100)+2.0d0*rr(103)   &
        +rr(104)+2.0d0*rr(105)*(1.0d0-arytm)+rr(106)+rr(109)   &
        +rr(75)+rr(88)
!
!      ** s(vi) **
!
       rp(2)= rr(72)+rr(73)+rr(74)+rr(98)+rr(101)+rr(105)*arytm   &
       +rr(85)   &
       +2.d0*rr(82)+rr(84)+rr(86)+rr(87)+rr(88)+rr(89)+rr(90)   &
       +rr(91)+rr(92)+rr(93)+rr(94)+rr(95)+rr(96)+rr(97)+rr(99)   &
       +rr(100)+rr(102)+rr(103)+rr(104)
       rl(2)=0.0d0
!
!      ** n(iii) **
!
       rp(3)=2.0d0*rr(31)+rr(32)+rr(33)+2.0d0*rr(104)
       rl(3)=rr(35)+rr(37)+rr(39)+rr(105)*arytm+rr(36)+rr(38)+rr(40)   &
       +rr(41)+rr(42)+rr(43)+rr(85)+rr(93)+rr(105)*(1.0d0-arytm)
!
!      ** n(v) **
!
       rp(4)=rr(32)+rr(34)+rr(39)+rr(40)+rr(43)+rr(46)+rr(47)+rr(48)+   &
       rr(49)+rr(54)+rr(59)+rr(62)+rr(71)+rr(85)+rr(103)
       rl(4)=rr(44)
!
!      ** co2 **
!
       rp(5)=rr(52)+rr(54)+rr(55)+rr(56)+rr(57)+rr(58)+rr(59)+   &
       rr(60)+rr(61)+rr(79)+rr(80)+rr(95)+rr(97)+rr(19)+rr(20)+rr(60)+   &
       rr(68)+rr(41)
       rl(5)=rr(17)+rr(18)+rr(94)
!
!      ** h2o2 **
!
       rp(6)=rr(2)+rr(6)+rr(7)+rr(8)+rr(18)
       rl(6)=rr(1)+rr(5)+rr(9)+rr(10)+rr(16)+rr(20)+rr(29)+rr(39)+   &
        rr(48)+rr(53)+rr(73)+rr(92)+rr(15)+rr(28)
!
!       ** hcho **
!
       rp(7)= rr(65)+rr(67)+rr(68)+rr(69)+rr(70)+rr(71)+rr(102)+rr(107)   &
        +rr(108)
       rl(7)= rr(106)+rr(50)+rr(51)
!
!       ** hcooh **
!
       rp(8)=rr(50)
       rl(8)=rr(52)+rr(53)+rr(54)+rr(55)+rr(56)+rr(79)+rr(97)+rr(57)+   &
        rr(58)+rr(59)+rr(60)+rr(61)+rr(80)+rr(95)
!
!      ** no **
!
       rp(9)=rr(35)+rr(36)+rr(45)
       rl(9)=rr(31)+rr(33)
!
!      ** no2 **
!
       rp(10)=rr(37)+rr(38)+rr(41)+rr(42)+rr(43)+rr(44)+rr(93)
       rl(10)=rr(31)+2.0d0*rr(32)+rr(34)+2.0d0*rr(104)
!
!      ** o3 **
!
       rp(11)=0.0d0
       rl(11)=rr(2)+rr(11)+rr(12)+rr(13)+rr(14)+rr(15)+rr(16)+rr(40)+   &
       rr(51)+rr(55)+rr(58)+rr(72)
!
!      ** pan **
!
       rp(12)=0.0d0
       rl(12)=rr(62)+rr(98)
!
!      ** ch3coooh **
!
       rp(13)=0.0d0
       rl(13)=rr(100)
!
!      ** ch3ooh **
!
       rp(14)=rr(63)+rr(64)
       rl(14)=rr(65)+rr(66)+rr(70)+rr(99)
!
!      ** hcl **
!
       rp(15)=rr(22)+2.d0*rr(25)+2.d0*rr(26)+rr(27)+rr(29)+2.d0*rr(30)   &
       +2.d0*rr(42)+2.d0*rr(56)+2.d0*rr(61)+   &
       2.d0*rr(69)+2.d0*rr(109)+2.d0*rr(28)
       rl(15)=rr(21)+rr(49)+rr(86)+rr(96)
!
!      ** oh **
!
       rp(16)=2.0d0*rr(1)+rr(9)+rr(10)+rr(12)+   &
        rr(13)+rr(15)+rr(22)+rr(30)+rr(35)+rr(36)+rr(44)+rr(55)+rr(58)+   &
        rr(65)+rr(91)+rr(101)
       rl(16)=rr(3)+rr(4)+rr(5)+rr(11)+rr(17)+rr(21)+rr(33)+rr(34)+   &
        rr(37)+rr(38)+rr(50)+rr(52)+rr(57)+rr(66)+rr(67)+rr(75)+rr(76)+   &
        rr(83)+rr(108)
!
!      ** ho2 **
!
       rp(17)=rr(5)+rr(11)+rr(20)+rr(28)+rr(29)+rr(48)+rr(50)+rr(52)+   &
       rr(54)+rr(55)+rr(56)+rr(57)+rr(59)+rr(60)+rr(61)+rr(65)+   &
       rr(67)+rr(68)+rr(69)+rr(71)+rr(79)+rr(92)+rr(95)+rr(97)+   &
       rr(102)+rr(14)+rr(14)+rr(15)+rr(58)+rr(80)
       rl(17)=rr(3)+2.0d0*rr(6)+rr(7)+rr(9)+rr(12)+rr(25)+rr(27)+rr(46)   &
        +rr(63)+rr(89)+rr(101)+rr(4)+rr(7)+2.0d0*rr(8)+rr(10)+rr(13)   &
        +rr(18)+rr(19)+rr(26)+rr(47)+rr(64)+rr(78)+rr(90)
!
!      ** no3 **
!
       rp(18)=0.0d0
       rl(18)= rr(43)+rr(45)+rr(46)+rr(47)+rr(48)+rr(49)+rr(54)+   &
       rr(59)+rr(71)+rr(103)
!
!      ** nh3 **
!
       rp(19)=0.0d0
       rl(19)=0.0d0
!
!      ** ch3o2 **
!
       rp(20)=rr(66)
       rl(20)=rr(63)+rr(64)
!
!      ** ch3oh **
!
       rp(21)=0.0d0
       rl(21)=rr(67)+rr(68)+rr(69)+rr(71)+rr(102)
!
!      ** cl2-, cl **
!
       rp(22)=rr(49)+rr(96)+rr(23)
       rl(22)=rr(25)+rr(26)+rr(28)+rr(30)+rr(42)+rr(56)+rr(61)+   &
       rr(69)+rr(109)+rr(24)+rr(27)+rr(29)
!
!      ** cloh- **
!
       rp(23)=rr(21)+rr(24)
       rl(23)=rr(22)+rr(23)
!
!      ** so4- **
!
       rp(24)=2.d0*rr(81)+rr(103)
       rl(24)= rr(84)+rr(87)+rr(88)+rr(89)+rr(90)+rr(91)+   &
        rr(92)+rr(93)+rr(94)+rr(95)+rr(96)+rr(97)+rr(102)
!
!      ** so5- **
!
       rp(25)=rr(75)+rr(76)+rr(83)+rr(84)+rr(87)+rr(88)+rr(108)+rr(109)
       rl(25)=rr(78)+rr(79)+rr(80)+2.0d0*rr(81)
!
!      ** hso5- **
!
       rp(26)=rr(77)+rr(78)+rr(79)+rr(80)
       rl(26)=+rr(82)+rr(83)+rr(84)+rr(85)+rr(86)
!
!      ** hoch2so3- **
!
       rp(27)=rr(106)
       rl(27)=rr(107)+rr(108)
!
!      ** co3- **
!
       rp(28)=rr(17)+rr(18)+rr(94)
       rl(28)=rr(19)+rr(20)+rr(41)+rr(60)+rr(68)

       return
       end subroutine addit                
!----------------------------------------------------------------------



      end module module_cmu_bulkaqchem
