!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for information and terms of use
!**********************************************************************************  

      module module_data_cmu_bulkaqchem


      implicit none



!-----------------------------------------------------------------------
!   aerpar.inc
!-----------------------------------------------------------------------
!******************************************************************
!                        aerosol parameters
!******************************************************************
!
! useful constants
!
      double precision pi, pi6
      parameter (pi  = 3.14159)
      parameter (pi6 = pi/6.0)

      double precision rho
      parameter (rho = 1.4e12)       ! particle density [ug/m^3]
!
! aerosol components in the aerosol concentration vector
!
      integer nas, nah, naa, nan, nac, na4, naw, nae, nao, nar
      integer nahso5, nahmsa, naspec
      parameter (nas =  1)           ! sodium
      parameter (nah =  2)           ! hydrogen
      parameter (naa =  3)           ! ammonium
      parameter (nan =  4)           ! nitrate
      parameter (nac =  5)           ! chloride
      parameter (na4 =  6)           ! sulfate
      parameter (naw =  7)           ! water
      parameter (nae =  8)           ! elemental carbon
      parameter (nao =  9)           ! organics
      parameter (nar = 10)           ! crustal
      parameter (nahso5 = 11)        ! hso5-
      parameter (nahmsa = 12)        ! hmsa
      parameter (naspec = 12)        ! number of aerosol species
!
! condensible gas-phase components in local arrays
!
      integer ngca, ngcn, ngcc, ngc4, ngco, ngcspec
      parameter (ngca =  1)          ! ammonia
      parameter (ngcn =  2)          ! nitric acid
      parameter (ngcc =  3)          ! hydrochloric acid
      parameter (ngc4 =  4)          ! gas-phase sulfate
      parameter (ngco =  5)          ! gas-phase organics
      parameter (ngcspec = 5)        ! number of condensible gas-phase species
!
! condensible gas-phase components in global gas-phase array
!
! this must be customized to have the correct addresses
!
      integer nga, ngn, ngc, ng4, ngo, ngspec
      parameter (nga =  1)           ! ammonia
      parameter (ngn =  2)           ! nitric acid
      parameter (ngc =  3)           ! hydrochloric acid
      parameter (ng4 =  4)           ! gas-phase sulfate
      parameter (ngo =  5)           ! gas-phase organics
      parameter (ngspec = 5)         ! number of condensible gas-phase species
!
! total number of gas phase species so we know where the aerosol starts
!
      integer ngtotal, ngas, naers
!     parameter (ngtotal = 50)
      parameter (ngtotal = 26) 		! 2004-nov-15 rce
      parameter (ngas=ngtotal)
      parameter (naers=naspec)



!-----------------------------------------------------------------------
!   droppar.inc
!-----------------------------------------------------------------------
! updated droppar.inc for the bulk model
! last update : 10 june 1998
!*************************************************************************
!                                droppar.inc
!*************************************************************************
!
!                aqueous-phase parameters and variables
!
! aqueous-phase components
!
!   important : all components have the same positions in
!               both aerosol and aqueous matrices
!               never change this convention because aqmain
!               depends on it
!
      integer ksod, khyd, kamm, knit, kchl, ksvi, kwat, kec, koc, kcru
      parameter (ksod = nas)               ! na(+)
      parameter (khyd = nah)               ! h(+)
      parameter (kamm = naa)               ! nh4(+)
      parameter (knit = nan)               ! no3(-)
      parameter (kchl = nac)               ! cl(-)
      parameter (ksvi = na4)               ! s(vi)
      parameter (kwat = naw)               ! h2o
      parameter (kec  = nae)               ! ec
      parameter (koc  = nao)               ! oc
      parameter (kcru = nar)               ! crustal
!      parameter (khso5 = 1)                ! hso5-
!      parameter (khmsa = 2)                ! hmsa
!      parameter (kform = 3)                ! formic acid
!
! gases in local array
!
!     incorrect  ******* to be fixed *************
      integer ngso2, ngh2o2, nghcho, nghcooh
      integer nghno2, ngno, ngno2, ngo3, ngpan, ngoh, ngho2, ngno3
      integer ngch3o2, ngch3o2h, ngch3oh, ngch3co3h
      parameter (ngso2 = 11)
      parameter (ngh2o2 = 12)
      parameter (nghcho = 13)
      parameter (nghcooh = 14)
      parameter (nghno2 = 15)
      parameter (ngno = 16)
      parameter (ngno2 = 17)
      parameter (ngo3 = 18)
      parameter (ngpan = 19)
      parameter (ngoh = 20)
      parameter (ngho2 = 21)
      parameter (ngno3 = 22)
      parameter (ngch3o2 = 23)
      parameter (ngch3o2h = 24)
      parameter (ngch3oh = 25)
      parameter (ngch3co3h = 26)
!
!     number of equations for aqueous-phase chemistry solution
!
      integer meqn1max
      parameter (meqn1max = 20)
      integer, save :: meqn1 = meqn1max
!
!     activation diameter (dry)
!
      double precision dactiv
      parameter (dactiv = 0.7e-6)       ! in m
!
!
!
!     wet diameter
!
      double precision avdiam
      parameter (avdiam = 20.e-6)
!
!     choice of expression for iron chemistry
!               = 0 (no iron/manganese chemistry)
!          kiron = 1 (phenomenological, martin et al., 1991)
!                = 2 (martin, 1984)
!
      integer kiron
!     parameter (kiron = 1)            ! was 1
!     parameter (kiron = 0)            ! rce 2004-mar-24 - turn off metal chem
      parameter (kiron = 1)            ! rce 2005-jan-17 - turn it back on
!
!     choice of turning on or off radical chemisty
!     (it is better to turn it off during the night)
!
      integer, save :: iradical
!     parameter (iradical = 0)		! rce 2004-nov-15 - now a common var

!
!     choice of turning off chlorine chemistry
!
      double precision chlorine
      parameter (chlorine = 0.0)
!
!     parameter for scaling of photolysis rates
!
      double precision, save :: photo
!     parameter (photo = 1.0)
!     parameter (photo = 0.0) 		! rce 2004-mar-24 - turn off photo chem
					! rce 2004-nov-15 - now a common var
!
!     fraction of crustal material that is alkaline
!
      double precision caratio
!     parameter (caratio = 0.05)        ! was 0.1
! rce 2005-jul-14 - reduce caratio to .001 to get lower ph
!     with 0.05 value, ca=.05*oin, and the initial aerosol is alkaline
      parameter (caratio = 0.001)       
!
!
!
!     fraction of liquid water content that goes to each s.r. section
!
      double precision frac1, frac2
      parameter (frac1 = 0.8)               ! fraction of lwc in sect. 1
      parameter (frac2 = 0.2)               ! fraction of lwc in sect. 2
!
!
!     assumption : fe(3+) and mn(2+) = 0.003%, 0.001% of crustal mass
!
      double precision firon, fman
!     parameter (firon = 0.00003)
!     parameter (fman = 0.00001)
!     parameter (firon = 0.0)          ! rce 2004-mar-24 - turn off metal chem
!     parameter (fman  = 0.0)          ! rce 2004-mar-24 - turn off metal chem
      parameter (firon = 0.00003)      ! rce 2005-jan-17 - turn it back on
      parameter (fman = 0.00001)       ! rce 2005-jan-17 - turn it back on

!     co2 mixing ratio (ppm)
      double precision, save :: co2_mixrat

!     common / aqcmu_cmn11 / iradical, photo, co2_mixrat


!-----------------------------------------------------------------------
!   dropcom.inc
!-----------------------------------------------------------------------
!
! common groups and corresponding matrices for aqueous-phase module
!
	double precision, save :: akeq(17), akhen(21), akre(120)
	double precision, save :: wso2, wh2o2, whcho, whcooh, wnh3, whno3, whcl, wh2so4
	double precision, save :: wmol(29), amol(3), gmol(22)
!	common / drop / diameter, dd, daer
!	common / mw / wso2, wh2o2, whcho, whcooh, wnh3, whno3, whcl, wh2so4
!	common /aqrates2/akeq,akhen,akre
!	common /aqrates3/wmol,amol,gmol

	double precision, save :: gcon(22), con(28), cmet(4), rad, wvol, chyd,   &
	        temp_cmuaq_cur, pres_cmuaq_cur
!	common / sstate / gcon, con, cmet, rad, wvol, chyd,   &
!	        temp_cmuaq_cur, pres_cmuaq_cur



!-----------------------------------------------------------------------
!   math.inc
!-----------------------------------------------------------------------
!     include file for svode parameters and non-changing values
!     input to hybrid.f

!     integer itol,itask,istate,iopt,mf,worki,lrw1,liw1
      integer itol,itask,iopt,mf,worki,lrw1,liw1
      double precision tola,tolr,workr
      integer numfunc, mode, nprint, maxfev, ml, mu, lr,ldfjac
      double precision factor, epsfcn, xtol

!      for svode
      parameter (itol = 4)
!     parameter (tola = 1.e-4)             ! was 1.e-3
      parameter (tola = 1.e-6)             ! 17-may-2006 rce - need smaller tola
      parameter (tolr = 1.e-5)             ! was 1.e-3
      parameter (itask = 1)
!     parameter (istate = 1)       ! rce 2004-mar-18 - istate is a variable
      parameter (iopt = 1)
      parameter (mf = 22)
      parameter (worki = 100000)             ! maximum steps allowed
      parameter (workr = 300.0)
!  for bulk
      parameter (lrw1 = 22+9*meqn1max+2*meqn1max**2)
      parameter (liw1 = 30+meqn1max)
!
!   where
!      itol: 4=use arrays for tolerances
!      tola: absolute tolerance in ug/m3
!      tolr: relative tolerance
!      itask: 1 for normal computation of output values of y at t = tout.
!      istate: integer flag (input and output).  set istate = 1.
!      iopt: 0 to indicate no optional input used.
!      rwork: double precision work array of length at least..
!             20 + 16*neq                      for mf = 10,
!             22 +  9*neq + 2*neq**2           for mf = 21 or 22,
!             22 + 11*neq + (3*ml + 2*mu)*neq  for mf = 24 or 25.
!      lrw: declared length of rwork (in user's dimension statement).
!      iwork: integer work array of length at least..
!             30        for mf = 10,
!             30 + neq  for mf = 21, 22, 24, or 25.
!          if mf = 24 or 25, input in iwork(1),iwork(2) the lower
!          and upper half-bandwidths ml,mu.
!      liw: declared length of iwork (in user's dimension statement).
!      mf: method flag.  standard values are..
!          10 for nonstiff (adams) method, no jacobian used.
!          21 for stiff (bdf) method, user-supplied full jacobian.
!          22 for stiff method, internally generated full jacobian.
!          24 for stiff method, user-supplied banded jacobian.
!          25 for stiff method, internally generated banded jacobian.
!      iopt: 1 = some optional parameters used
!           here:  workr: rwork(6) (max absolute step size allowed -
!                                    default value is infinite.)
!                  worki: iwork(6) (maximum number of (internally defined)
!                                    steps allowed during one call to the
!				    solver. the default value is 500.)

!      for hybrid.f

       parameter (numfunc = 7)
!      parameter (xtol = 0.1e0**3)
       parameter (xtol = 1.0e-3)
       parameter (maxfev = 300*(numfunc+1) )
       parameter (ml = numfunc - 1, mu = numfunc -1)
       parameter (epsfcn = 0.0e0, factor = 100., mode = 2)
       parameter (nprint = 0)
       parameter (lr = numfunc*(numfunc+1)/2, ldfjac = numfunc)
!
!      numfunc : number of functions and variables
!      xtol : termination occurs when the rel error  between two consecutive
!             iterates is at most xtol
!      maxfev : termination occurs when the number of calls to fcn is at least maxfev
!      ml     : specifies the number of subdiagonals within the band of the
!               jacobian matrix.  if the jacobian is not banded, set ml to at
!               least n -1.
!      mu     : specifies the number of superdiagonals within the band of the
!               jacobian matrix.  if the jacobian is not banded, set mu to at
!               least n -1.
!      epsfcn : used in determining a suitable step length for the
!               forward-difference approximation
!      factor : used in determining the initial step bound
!      mode   : if 1, the variables will be scaled internally; if 2, the
!               scaling is specified by the input diag.
!      nprint : input variable that enables controlled
!               printing of iterates if it is positive. in this case,
!               fcn is called with iflag = 0 at the beginning of the first
!               iteration and every nprint iterations thereafter and
!               immediately prior to return, with x and fvec available
!               for printing. if nprint is not positive, no special calls
!               of fcn with iflag = 0 are made.



!-----------------------------------------------------------------------
!   etest_cmn71.inc
!-----------------------------------------------------------------------
!
!   maqurxn_all - if positive, all reactions are enabled.  
!           If zero/negative, all reactions rates are zeroed.
!   maqurxn_sulf1 - if positive, 4 primary sulfur reactions are enabled.
!           This has no effect when maqurxn_all=1. 
!           When maqurxn_all=0 & maqurxn_sulf1=1, only the 4 primary
!           sulfur reactions (rxns 72-75) are enabled.
!
!   mopt_eqrt_cons - if =20, certain equilib. constants and reaction rates 
!           are modified to allow closer comparison with 
!           other cloud chemistry codes
!   mequlib_h2o2_ho2m - currently not used
!   mgasrxn - currently not used
!
!   mdiag_fullequil - if positive, warning messages from subr. fullequil 
!           are enabled
!   mdiag_hybrd - if positive, warning messages from subr. hybrd are enabled
!   mdiag_negconc - if positive, warning messages from subr. aqoperator1
!           about negative concentrations are enabled
!   mdiag_rsrate - if positive, warning messages from subr. aqratesa
!           about sulfur mass balance are enabled.  This diagnostic is somewhat
!           misleading as some reactions do not conserve sulfur.
!   mdiag_svode - if positive, warning messages from subr. svode are enabled
!
!   mprescribe_ph - if positive, cloudwater ph is set to xprescribe_ph
!
	integer, save :: maqurxn_all = 1
	integer, save :: maqurxn_sulf1 = 0
	integer, save :: mopt_eqrt_cons = 0
	integer, save :: mequlib_h2o2_ho2m = 0
	integer, save :: mgasrxn = 0
	integer, save :: mdiag_fullequil = 1
	integer, save :: mdiag_hybrd = 1
	integer, save :: mdiag_negconc = 1
	integer, save :: mdiag_rsrate = 1
	integer, save :: mdiag_svode = 1
	integer, save :: mprescribe_ph = 0
	double precision,    save :: xprescribe_ph = 4.5

!	common / etest_cmn71 /   &
!      	    maqurxn_all, maqurxn_sulf1, mequlib_h2o2_ho2m,   &
!      	    mgasrxn, mopt_eqrt_cons, mprescribe_ph, mdiagaa,   &
!      	    xprescribe_ph


!   gas constant in [atm/K/(mol/liter)]
      double precision rideal
      parameter (rideal = 0.082058e0)

!   indices to wmol array, for molecular weights of aqueous species
      integer kaqx_siv, kaqx_svi, kaqx_no3m, kaqx_h2o2,   &
              kaqx_clm, kaqx_nh4p, kaqx_hso5m, kaqx_hmsa
      parameter(kaqx_siv = 1)
      parameter(kaqx_svi = 2)
      parameter(kaqx_no3m = 4)
      parameter(kaqx_h2o2 = 6)
      parameter(kaqx_clm = 15)
      parameter(kaqx_nh4p = 19)
      parameter (kaqx_hso5m = 26)
      parameter (kaqx_hmsa = 27)



      end module module_data_cmu_bulkaqchem
