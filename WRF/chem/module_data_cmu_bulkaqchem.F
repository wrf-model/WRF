!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

      module module_data_cmu_bulkaqchem


      implicit none


!
!   summary of module **variables**
!
!   the following variables are flags that could be converted to parameters
!	integer, save :: maqurxn_all = 1
!	integer, save :: maqurxn_sulf1 = 0
!	integer, save :: mopt_eqrt_cons = 0
!	integer, save :: mequlib_h2o2_ho2m = 0
!	integer, save :: mgasrxn = 0
!	integer, save :: mdiag_fullequil = 1
!	integer, save :: mdiag_hybrd = 1
!	integer, save :: mdiag_negconc = 1
!	integer, save :: mdiag_rsrate = 1
!	integer, save :: mdiag_svode = 1
!
!   the following variables are constants (they are set in subr dropinit, then never changed)
!	double precision, save :: wso2, wh2o2, whcho, whcooh, wnh3, whno3, whcl, wh2so4
!	double precision, save :: wmol(29), amol(3), gmol(22)
!


!-----------------------------------------------------------------------
!   aerpar.inc
!-----------------------------------------------------------------------
!******************************************************************
!                        aerosol parameters
!******************************************************************
!
! useful constants
!
      double precision, parameter :: pi  = 3.14159
      double precision, parameter :: pi6 = pi/6.0

      double precision, parameter :: rho = 1.4e12       ! particle density [ug/m^3]
!
! aerosol components in the aerosol concentration vector
!
      integer, parameter :: nas =  1           ! sodium
      integer, parameter :: nah =  2           ! hydrogen
      integer, parameter :: naa =  3           ! ammonium
      integer, parameter :: nan =  4           ! nitrate
      integer, parameter :: nac =  5           ! chloride
      integer, parameter :: na4 =  6           ! sulfate
      integer, parameter :: naw =  7           ! water
      integer, parameter :: nae =  8           ! elemental carbon
      integer, parameter :: nao =  9           ! organics
      integer, parameter :: nar = 10           ! crustal
      integer, parameter :: nahso5 = 11        ! hso5-
      integer, parameter :: nahmsa = 12        ! hmsa
      integer, parameter :: naspec = 12        ! number of aerosol species
!
! condensible gas-phase components in local arrays
!
      integer, parameter :: ngca =  1          ! ammonia
      integer, parameter :: ngcn =  2          ! nitric acid
      integer, parameter :: ngcc =  3          ! hydrochloric acid
      integer, parameter :: ngc4 =  4          ! gas-phase sulfate
      integer, parameter :: ngco =  5          ! gas-phase organics
      integer, parameter :: ngcspec = 5        ! number of condensible gas-phase species
!
! condensible gas-phase components in global gas-phase array
!
! this must be customized to have the correct addresses
!
      integer, parameter :: nga =  1           ! ammonia
      integer, parameter :: ngn =  2           ! nitric acid
      integer, parameter :: ngc =  3           ! hydrochloric acid
      integer, parameter :: ng4 =  4           ! gas-phase sulfate
      integer, parameter :: ngo =  5           ! gas-phase organics
      integer, parameter :: ngspec = 5         ! number of condensible gas-phase species
!
! total number of gas phase species so we know where the aerosol starts
!
!     integer, parameter :: ngtotal = 50
      integer, parameter :: ngtotal = 26 		! 2004-nov-15 rce
      integer, parameter :: ngas=ngtotal
      integer, parameter :: naers=naspec



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
      integer, parameter :: ksod = nas               ! na(+)
      integer, parameter :: khyd = nah               ! h(+)
      integer, parameter :: kamm = naa               ! nh4(+)
      integer, parameter :: knit = nan               ! no3(-)
      integer, parameter :: kchl = nac               ! cl(-)
      integer, parameter :: ksvi = na4               ! s(vi)
      integer, parameter :: kwat = naw               ! h2o
      integer, parameter :: kec  = nae               ! ec
      integer, parameter :: koc  = nao               ! oc
      integer, parameter :: kcru = nar               ! crustal
!      integer, parameter :: khso5 = 1                ! hso5-
!      integer, parameter :: khmsa = 2                ! hmsa
!      integer, parameter :: kform = 3                ! formic acid
!
! gases in local array
!
      integer, parameter :: ngso2     = 11
      integer, parameter :: ngh2o2    = 12
      integer, parameter :: nghcho    = 13
      integer, parameter :: nghcooh   = 14
      integer, parameter :: nghno2    = 15
      integer, parameter :: ngno      = 16
      integer, parameter :: ngno2     = 17
      integer, parameter :: ngo3      = 18
      integer, parameter :: ngpan     = 19
      integer, parameter :: ngoh      = 20
      integer, parameter :: ngho2     = 21
      integer, parameter :: ngno3     = 22
      integer, parameter :: ngch3o2   = 23
      integer, parameter :: ngch3o2h  = 24
      integer, parameter :: ngch3oh   = 25
      integer, parameter :: ngch3co3h = 26
!
!     number of equations for aqueous-phase chemistry solution
!
      integer, parameter :: meqn1max = 20
!     integer, save :: meqn1 = meqn1max     ! rce 2008-mar-12 ELIMINATED
!
!     activation diameter (dry)
!
      double precision, parameter :: dactiv = 0.7e-6       ! in m
!
!
!
!     wet diameter
!
      double precision, parameter :: avdiam = 20.e-6
!
!     choice of expression for iron chemistry
!               = 0 (no iron/manganese chemistry)
!          kiron = 1 (phenomenological, martin et al., 1991)
!                = 2 (martin, 1984)
!
!     integer, parameter :: kiron = 1            ! was 1
!     integer, parameter :: kiron = 0            ! rce 2004-mar-24 - turn off metal chem
      integer, parameter :: kiron = 1            ! rce 2005-jan-17 - turn it back on
!
!     choice of turning on or off radical chemisty
!     (it is better to turn it off during the night)
!
!     integer, save :: iradical                    ! rce 2008-mar-12 ELIMINATED
!     integer, parameter :: iradical = 0           ! rce 2004-nov-15 - now a variable

!
!     choice of turning off chlorine chemistry
!
      double precision, parameter :: chlorine = 0.0
!
!     parameter for scaling of photolysis rates
!
!     double precision, save :: photo               ! rce 2008-mar-12 ELIMINATED
!     double precision, parameter :: photo = 1.0
!     double precision, parameter :: photo = 0.0    ! rce 2004-mar-24 - turn off photo chem
                                                    ! rce 2004-nov-15 - now a variable
!
!     fraction of crustal material that is alkaline
!
!     double precision, parameter :: caratio = 0.05        ! was 0.1
! rce 2005-jul-14 - reduce caratio to .001 to get lower ph
!     with 0.05 value, ca=.05*oin, and the initial aerosol is alkaline
      double precision, parameter :: caratio = 0.001
!
!
!
!     fraction of liquid water content that goes to each s.r. section
!
      double precision, parameter :: frac1 = 0.8               ! fraction of lwc in sect. 1
      double precision, parameter :: frac2 = 0.2               ! fraction of lwc in sect. 2
!
!
!     assumption : fe(3+) and mn(2+) = 0.003%, 0.001% of crustal mass
!
!     double precision, parameter :: firon = 0.00003 
!     double precision, parameter :: fman  = 0.00001 
!     double precision, parameter :: firon = 0.0           ! rce 2004-mar-24 - turn off metal chem
!     double precision, parameter :: fman  = 0.0           ! rce 2004-mar-24 - turn off metal chem
      double precision, parameter :: firon = 0.00003       ! rce 2005-jan-17 - turn it back on
      double precision, parameter :: fman  = 0.00001       ! rce 2005-jan-17 - turn it back on

!     co2 mixing ratio (ppm)
!     double precision, save :: co2_mixrat     ! rce 2008-mar-12 ELIMINATED


!-----------------------------------------------------------------------
!   dropcom.inc
!-----------------------------------------------------------------------
!
! cmn groups and corresponding matrices for aqueous-phase module
!
!	double precision, save :: akeq(17), akhen(21), akre(120)     ! rce 2008-mar-12 ELIMINATED
	double precision, save :: wso2, wh2o2, whcho, whcooh, wnh3, whno3, whcl, wh2so4
	double precision, save :: wmol(29), amol(3), gmol(22)

!	double precision, save :: gcon(22), con(28), cmet(4), rad, wvol, chyd     ! rce 2008-mar-12 ELIMINATED



!-----------------------------------------------------------------------
!   math.inc
!-----------------------------------------------------------------------
!     include file for svode parameters and non-changing values
!     input to hybrid.f

!      for svode
      integer, parameter :: itol = 4
      integer, parameter :: itask = 1
!     integer, parameter :: istate = 1       ! rce 2004-mar-18 - istate is a variable
      integer, parameter :: iopt = 1
      integer, parameter :: mf = 22
      integer, parameter :: worki = 100000             ! maximum steps allowed
!  for bulk
      integer, parameter :: lrw1 = 22+9*meqn1max+2*meqn1max**2
      integer, parameter :: liw1 = 30+meqn1max

!     double precision, parameter :: tola = 1.e-4             ! was 1.e-3
!     double precision, parameter :: tola = 1.e-6             ! 17-may-2006 rce - need smaller tola
      double precision, parameter :: tola = 1.e-8             ! 24-mar-2008 rce - need smaller tola

      double precision, parameter :: tolr = 1.e-5             ! was 1.e-3

      double precision, parameter :: workr = 300.0

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

       integer, parameter :: numfunc = 7
       integer, parameter :: maxfev = 300*(numfunc+1)
       integer, parameter :: ml = numfunc - 1, mu = numfunc -1
       integer, parameter :: nprint = 0
       integer, parameter :: lr = numfunc*(numfunc+1)/2, ldfjac = numfunc
       integer, parameter :: mode = 2

!      double precision, parameter :: xtol = 0.1e0**3
       double precision, parameter :: xtol = 1.0e-3
       double precision, parameter :: epsfcn = 0.0e0, factor = 100.
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
!	integer, save :: mprescribe_ph = 0     ! rce 2008-mar-12 ELIMINATED
!	double precision,    save :: xprescribe_ph = 4.5     ! rce 2008-mar-12 ELIMINATED

!   gas constant in [atm/K/(mol/liter)]
      double precision, parameter :: rideal = 0.082058e0

!   indices to wmol array, for molecular weights of aqueous species
      integer, parameter :: kaqx_siv   = 1
      integer, parameter :: kaqx_svi   = 2
      integer, parameter :: kaqx_no3m  = 4
      integer, parameter :: kaqx_h2o2  = 6
      integer, parameter :: kaqx_clm   = 15
      integer, parameter :: kaqx_nh4p  = 19
      integer, parameter :: kaqx_hso5m = 26
      integer, parameter :: kaqx_hmsa  = 27



      end module module_data_cmu_bulkaqchem

