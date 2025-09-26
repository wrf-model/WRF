
MODULE module_data_sorgam
   USE module_data_radm2
!
!   param.inc start
      IMPLICIT NONE
      INTEGER NP                !bs maximum expected value of N
      PARAMETER (NP = 8)
!      integer numaer
!      parameter (numaer=50)

      INTEGER MAXITS            !bs maximum number of iterations
      PARAMETER (MAXITS = 100)

      REAL TOLF                 !bs convergence criterion on function values
      PARAMETER (TOLF = 1.E-09)

      REAL TOLMIN                 !bs criterion whether superios convergence to
      PARAMETER (TOLMIN = 1.E-12) !bs a minimum of fmin has occurred

      REAL TOLX                 !bs convergence criterion on delta_x
      PARAMETER (TOLX = 1.E-10)

      REAL STPMX                !bs scaled maximum step length allowed
      PARAMETER (STPMX = 100.)


      REAL c303, c302
      PARAMETER (c303=19.83,c302=5417.4)

      INTEGER lcva, lcvb, lspcv, ldesn
      PARAMETER (lcva=4,lcvb=4,lspcv=lcva+lcvb)
      PARAMETER (ldesn=13)
!mh    ldesn is number of deposition species
!mh    true number of deposited species may be larger since there
!mh    are species which are deposited with the same rate

      INTEGER laerdvc, lnonaerdvc, l1ae, laero, imodes, aspec
!     PARAMETER (laerdvc=34,lnonaerdvc=8+lspcv)
      PARAMETER (laerdvc=39,lnonaerdvc=8+lspcv)
      PARAMETER (l1ae=laerdvc+lnonaerdvc)
      PARAMETER (laero=4,imodes=4,aspec=1)
!     LAERDVC  number of advected aerosol dynamic parameters for a given
!     component species
!ia     L1AE        advected parameters+non-advected parameters
!ia     LAERO       number of aerosol component species
!ia     imodes      number of aerosol modes
!ia     ASPEC       number of gas phase comp. that are added dynamically
!ia		    currently only sulfate (=1)
!bs
!bs * BS ** BS ** BS ** BS ** BS ** BS ** BS ** BS ** BS ** BS ** BS **
!bs
      INTEGER aemiss
      PARAMETER (aemiss=4)
!bs *  AEMISS      # of aerosol species with emissions link to gas phase
!bs                currently ECI, ECJ, BCI, BCJ
      INTEGER ldroga
      PARAMETER (ldroga=11)
      INTEGER ldrogb
      PARAMETER (ldrogb=6)
      INTEGER ldrog
      PARAMETER (ldrog=ldroga+ldrogb)
!bs * LDROGA      # of anthropogenic organic aerosol precursor gases (DR
!bs * LDROGB      # of biogenic organic aerosol precursor gases (DROG)
!bs * LSPCV       # of condensable organic vapor interacting between gas
!bs               aerosol phase with SORGAM
!bs
!     param.inc stop

! //////////////////////////////////////////////////////////////////////
! FSB include file

! *** declare and set flag for organic aerosol production method
! *** Two method are available:

! *** The method of Pandis,Harley, Cass, and Seinfeld, 1992,
!     Secondary aerosol formation and transport, Atmos. Environ., 26A,
!     pp 2453-2466
!     Bowman et al. Atmospheric Environment
!     Vol 29, pp 579-589, 1995.
! *** and
! *** The method of Odum, Hoffmann, Bowman, Collins, Flagen and
!     Seinfeld, 1996, Gas/particle partitioning and secondary organic ae
!     yields, Environ. Sci, Technol, 30, pp 2580-2585.


                            ! 1 = Pandis et al.  1992 method is used
      INTEGER orgaer
                            ! 2 = Pankow 1994/Odum et al. 1996 method is
! ***
! switch for organic aerosol method         
      PARAMETER (orgaer=2)

! *** information about visibility variables
! number of visibility variables    
      INTEGER n_ae_vis_spc
      PARAMETER (n_ae_vis_spc=2)

! index for visual range in deciview             
      INTEGER idcvw
      PARAMETER (idcvw=1)
! index for extinction [ 1/km ]                  
      INTEGER ibext
      PARAMETER (ibext=2)


! *** set up indices for array  CBLK

! index for Accumulation mode sulfate aerosol
      INTEGER vso4aj
      PARAMETER (vso4aj=1)

! index for Aitken mode sulfate concentration
      INTEGER vso4ai
      PARAMETER (vso4ai=2)

! index for Accumulation mode aerosol ammonium
      INTEGER vnh4aj
      PARAMETER (vnh4aj=3)

! index for Aitken mode ammonium concentration
      INTEGER vnh4ai
      PARAMETER (vnh4ai=4)

! index for Accumulation mode aerosol nitrate
      INTEGER vno3aj
      PARAMETER (vno3aj=5)

! index for Aitken mode nitrate concentration
      INTEGER vno3ai
      PARAMETER (vno3ai=6)

! index for Accumulation mode aerosol sodium
      INTEGER vnaaj
      PARAMETER (vnaaj=7)

! index for Aitken mode sodium concentration
      INTEGER vnaai
      PARAMETER (vnaai=8)

! index for Accumulation mode aerosol chloride
      INTEGER vclaj
      PARAMETER (vclaj=9)

! index for Aitken mode chloride concentration
      INTEGER vclai
      PARAMETER (vclai=10)

! index for Accumulation mode anthropogenic
      INTEGER vorgaro1j
      PARAMETER (vorgaro1j=11)

! index for Aitken mode anthropogenic org
      INTEGER vorgaro1i
      PARAMETER (vorgaro1i=12)

! index for Accumulation mode anthropogenic
      INTEGER vorgaro2j
      PARAMETER (vorgaro2j=13)

! index for Aitken mode anthropogenic org
      INTEGER vorgaro2i
      PARAMETER (vorgaro2i=14)

! index for Accumulation mode anthropogenic
      INTEGER vorgalk1j
      PARAMETER (vorgalk1j=15)

! index for Aitken mode anthropogenic org
      INTEGER vorgalk1i
      PARAMETER (vorgalk1i=16)

! index for Accumulation mode anthropogenic
      INTEGER vorgole1j
      PARAMETER (vorgole1j=17)

! index for Aitken mode anthropogenic org
      INTEGER vorgole1i
      PARAMETER (vorgole1i=18)

! index for Accumulation mode biogenic aerosol
      INTEGER vorgba1j
      PARAMETER (vorgba1j=19)

! index for Aitken mode biogenic aerosol concentration
      INTEGER vorgba1i
      PARAMETER (vorgba1i=20)

! index for Accumulation mode biogenic aerosol
      INTEGER vorgba2j
      PARAMETER (vorgba2j=21)

! index for Aitken mode biogenic aerosol concentration
      INTEGER vorgba2i
      PARAMETER (vorgba2i=22)

! index for Accumulation mode biogenic aerosol
      INTEGER vorgba3j
      PARAMETER (vorgba3j=23)

! index for Aitken mode biogenic aerosol concentration
      INTEGER vorgba3i
      PARAMETER (vorgba3i=24)

! index for Accumulation mode biogenic aerosol
      INTEGER vorgba4j
      PARAMETER (vorgba4j=25)

! index for Aitken mode biogenic aerosol concentration
      INTEGER vorgba4i
      PARAMETER (vorgba4i=26)

! index for Accumulation mode primary anthropogenic
      INTEGER vorgpaj
      PARAMETER (vorgpaj=27)

! index for Aitken mode primary anthropogenic
      INTEGER vorgpai
      PARAMETER (vorgpai=28)

! index for Accumulation mode aerosol elemen
      INTEGER vecj
      PARAMETER (vecj=29)

! index for Aitken mode elemental carbon    
      INTEGER veci
      PARAMETER (veci=30)

! index for Accumulation mode primary PM2.5 
      INTEGER vp25aj
      PARAMETER (vp25aj=31)

! index for Aitken mode primary PM2.5 concentration
      INTEGER vp25ai
      PARAMETER (vp25ai=32)

! index for coarse mode anthropogenic aerososol
      INTEGER vantha
      PARAMETER (vantha=33)

! index for coarse mode marine aerosol concentration
      INTEGER vseas
      PARAMETER (vseas=34)

! index for coarse mode soil-derived aerosol
      INTEGER vsoila
      PARAMETER (vsoila=35)

! index for Aitken mode number              
      INTEGER vnu0
      PARAMETER (vnu0=36)

! index for accum  mode number              
      INTEGER vac0
      PARAMETER (vac0=37)

! index for coarse mode number              
      INTEGER vcorn
      PARAMETER (vcorn=38)

! index for Accumulation mode aerosol water 
      INTEGER vh2oaj
      PARAMETER (vh2oaj=39)

! index for Aitken mode aerosol water concentration
      INTEGER vh2oai
      PARAMETER (vh2oai=40)

! index for Aitken mode 3'rd moment         
      INTEGER vnu3
      PARAMETER (vnu3=41)

! index for Accumulation mode 3'rd moment   
      INTEGER vac3
      PARAMETER (vac3=42)

! index for coarse mode 3rd moment          
      INTEGER vcor3
      PARAMETER (vcor3=43)

! index for sulfuric acid vapor concentration
      INTEGER vsulf
      PARAMETER (vsulf=44)

! index for nitric acid vapor concentration
      INTEGER vhno3
      PARAMETER (vhno3=45)

! index for ammonia gas concentration
      INTEGER vnh3
      PARAMETER (vnh3=46)

! index for HCL gas concentration
      INTEGER vhcl
      PARAMETER (vhcl=47)

! index for cond. vapor from aromatics   
      INTEGER vcvaro1
      PARAMETER (vcvaro1=48)

! index for cond. vapor from aromatics   
      INTEGER vcvaro2
      PARAMETER (vcvaro2=49)

! index for cond. vapor from anth. alkane
      INTEGER vcvalk1
      PARAMETER (vcvalk1=50)

! index for cond. vapor from anth. olefin
      INTEGER vcvole1
      PARAMETER (vcvole1=51)

! index for cond. vapor from biogenics   
      INTEGER vcvapi1
      PARAMETER (vcvapi1=52)

! index for cond. vapor from biogenics   
      INTEGER vcvapi2
      PARAMETER (vcvapi2=53)

! index for cond. vapor from biogenics   
      INTEGER vcvlim1
      PARAMETER (vcvlim1=54)

! index for cond. vapor from biogenics   
      INTEGER vcvlim2
      PARAMETER (vcvlim2=55)

!           COMMON /CBLKINDCS/
!    &         VSO4AJ,VSO4AI,VNH4AJ,VNH4AI,VNO3AJ,VNO3AI,
!    &         VORGAJ,VORGAI, VORGPAJ,VORGPAI,
!    &         VORGBAJ,VORGBAI,VECJ,VECI,
!    &         VP25AJ,VP25AI,VANTHA,VSEAS,VSOILA,
!    &         VNU0,VAC0,VCORN,
!    &         VH2OAJ,VH2OAI,
!    &         VNU3,VAC3,VCOR3,
!    &         VSULF,VHNO3,VNH3

! *** set up species dimension and indices for sedimentation
!     velocity array VSED

! number of sedimentation velocities         
      INTEGER naspcssed
      PARAMETER (naspcssed=6)

! index for Aitken mode number                  
      INTEGER vsnnuc
      PARAMETER (vsnnuc=1)

! index for Accumulation mode number            
      INTEGER vsnacc
      PARAMETER (vsnacc=2)

! index for coarse mode number                  
      INTEGER vsncor
      PARAMETER (vsncor=3)

! index for Aitken mode mass                     
      INTEGER vsmnuc
      PARAMETER (vsmnuc=4)

! index for accumulation mode mass               
      INTEGER vsmacc
      PARAMETER (vsmacc=5)

! index for coarse mass                         
      INTEGER vsmcor
      PARAMETER (vsmcor=6)

! *** set up species dimension and indices for deposition
!     velocity array VDEP

! number of deposition velocities            
      INTEGER naspcsdep
      PARAMETER (naspcsdep=7)

! index for Aitken mode number                  
      INTEGER vdnnuc
      PARAMETER (vdnnuc=1)

! index for accumulation mode number            
      INTEGER vdnacc
      PARAMETER (vdnacc=2)

! index for coarse mode number                  
      INTEGER vdncor
      PARAMETER (vdncor=3)

! index for Aitken mode mass                    
      INTEGER vdmnuc
      PARAMETER (vdmnuc=4)

! index for accumulation mode                   
      INTEGER vdmacc
      PARAMETER (vdmacc=5)

! index for fine mode mass (Aitken + accumulatio
      INTEGER vdmfine
      PARAMETER (vdmfine=6)

! index for coarse mode mass                    
      INTEGER vdmcor
      PARAMETER (vdmcor=7)

! *** END AEROSTUFF.EXT
!bs
!BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * !
!BS *                                                                * !
!BS *            include file used in SORGAM routines                * !
!BS *                                                                * !
!BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * BS * !
!bs
!bs
!bs * species pointer for condensable vapor production
!bs
!bs XYL + OH                            
      INTEGER pxyl
      PARAMETER (pxyl=1)
!bs TOL + OH                            
      INTEGER ptol
      PARAMETER (ptol=2)
!bs CSL + OH                            
      INTEGER pcsl1
      PARAMETER (pcsl1=3)
!bs CSL + NO                            
      INTEGER pcsl2
      PARAMETER (pcsl2=4)
!bs HC8 + OH                            
      INTEGER phc8
      PARAMETER (phc8=5)
!bs OLI + OH                            
      INTEGER poli1
      PARAMETER (poli1=6)
!bs OLI + NO                            
      INTEGER poli2
      PARAMETER (poli2=7)
!bs OLI + O3                            
      INTEGER poli3
      PARAMETER (poli3=8)
!bs OLT + OH                            
      INTEGER polt1
      PARAMETER (polt1=9)
!bs OLT + NO                            
      INTEGER polt2
      PARAMETER (polt2=10)
!bs OLT + O3                            
      INTEGER polt3
      PARAMETER (polt3=11)
!bs API + OH                            
      INTEGER papi1
      PARAMETER (papi1=12)
!bs API + NO                            
      INTEGER papi2
      PARAMETER (papi2=13)
!bs API + O3                            
      INTEGER papi3
      PARAMETER (papi3=14)
!bs LIM + OH                            
      INTEGER plim1
      PARAMETER (plim1=15)
!bs LIM + NO                            
      INTEGER plim2
      PARAMETER (plim2=16)
!bs LIM + O3                            
      INTEGER plim3
      PARAMETER (plim3=17)
!bs
!bs * Number of lumped condensable vapors in SORGAM
!bs
!bs      INTEGER NACV              !bs # of anth. cond. vapors
!bs      PARAMETER (NACV = 2)
!bs      INTEGER NBCV              !bs # of bio. cond. vapors
!bs      PARAMETER (NBCV = 1)
!bs      INTEGER NCV               !bs total # of cond. vapor
!bs      PARAMETER (NCV = NACV + NBCV)
!bs
!bs * species pointer for SOA species
!bs
      INTEGER psoaaro1
      PARAMETER (psoaaro1=1)
      INTEGER psoaaro2
      PARAMETER (psoaaro2=2)
      INTEGER psoaalk1
      PARAMETER (psoaalk1=3)
      INTEGER psoaole1
      PARAMETER (psoaole1=4)
      INTEGER psoaapi1
      PARAMETER (psoaapi1=5)
      INTEGER psoaapi2
      PARAMETER (psoaapi2=6)
      INTEGER psoalim1
      PARAMETER (psoalim1=7)
      INTEGER psoalim2
      PARAMETER (psoalim2=8)
!bs
!bs * end of AERO_SOA.EXT *
!bs

! *** include file for aerosol routines


!....................................................................

!  CONTAINS: Fundamental constants for air quality modeling

!  DEPENDENT UPON:  none

!  REVISION HISTORY:

!    Adapted 6/92 by CJC from ROM's PI.EXT.

!    Revised 3/1/93 John McHenry to include constants needed by
!    LCM aqueous chemistry
!    Revised 9/93 by John McHenry to include additional constants
!    needed for FMEM clouds and aqueous chemistry

!    Revised 3/4/96 by Dr. Francis S. Binkowski to reflect current
!    Models3 view that MKS units should be used wherever possible,
!    and that sources be documentated. Some variables have been added
!    names changed, and values revised.

!    Revised 3/7/96 to have universal gas constant input and compute
!    gas constant is chemical form. TWOPI is now calculated rather than

!    Revised 3/13/96 to group declarations and parameter statements.

!    Revised 9/13/96 to include more physical constants.
!    Revised 12/24/96 eliminate silly EPSILON, AMISS

!    Revised 1/06/97 to eliminate most derived constants

! FSB REFERENCES:

!      CRC76,        CRC Handbook of Chemistry and Physics (76th Ed),
!                     CRC Press, 1995
!      Hobbs, P.V.   Basic Physical Chemistry for the Atmospheric Scien
!                     Cambridge Univ. Press, 206 pp, 1995.
!      Snyder, J.P., Map Projections-A Working Manual, U.S. Geological
!                     Paper 1395 U.S.GPO, Washington, DC, 1987.
!      Stull, R. B., An Introduction to Bounday Layer Meteorology, Klu
!                     Dordrecht, 1988

! Geometric Constants:

      REAL*8 & ! PI (single precision 3.141593)
        pirs
      PARAMETER (pirs=3.14159265358979324)
!      REAL     PIRS ! PI (single precision 3.141593)
!      PARAMETER ( PIRS = 3.141593 )
! Fundamental Constants: ( Source: CRC76, pp 1-1 to 1-6)

! Avogadro's Constant [ 1/mol ]
      REAL avo
      PARAMETER (avo=6.0221367E23)

! universal gas constant [ J/mol-K ]
      REAL rgasuniv
      PARAMETER (rgasuniv=8.314510)

! standard atmosphere  [ Pa ]
      REAL stdatmpa
      PARAMETER (stdatmpa=101325.0)

! Standard Temperature [ K ]
      REAL stdtemp
      PARAMETER (stdtemp=273.15)

! Stefan-Boltzmann [ W/(m**2 K**4) ]
      REAL stfblz
      PARAMETER (stfblz=5.67051E-8)


! mean gravitational acceleration [ m/sec**2 ]
      REAL grav
      PARAMETER (grav=9.80622)
! FSB Non MKS qualtities:

! Molar volume at STP [ L/mol ] Non MKS units
      REAL molvol
      PARAMETER (molvol=22.41410)


! Atmospheric Constants:

! FSB                     78.06%  N2, 21% O2 and 0.943% A on a mole
      REAL mwair
                        ! fraction basis. ( Source : Hobbs, 1995) pp 69-
! mean molecular weight for dry air [ g/mol ]
      PARAMETER (mwair=28.9628)

! dry-air gas constant [ J / kg-K ]
      REAL rdgas
      PARAMETER (rdgas=1.0E3*rgasuniv/mwair)

!  3*PI
      REAL threepi
      PARAMETER (threepi=3.0*pirs)

!  6/PI
      REAL f6dpi
      PARAMETER (f6dpi=6.0/pirs)

!  1.0e9 * 6/PIRS
      REAL f6dpi9
      PARAMETER (f6dpi9=1.0E9*f6dpi)

! 1.0e-9 * 6/PIRS
      REAL f6dpim9
      PARAMETER (f6dpim9=1.0E-9*f6dpi)

!  SQRT( PI )
      REAL sqrtpi
      PARAMETER (sqrtpi=1.7724539)

!  SQRT( 2 )
      REAL sqrt2
      PARAMETER (sqrt2=1.4142135623731)

!  ln( sqrt( 2 ) )
      REAL lgsqt2
      PARAMETER (lgsqt2=0.34657359027997)

!  1/ln( sqrt( 2 ) )
      REAL dlgsqt2
      PARAMETER (dlgsqt2=1.0/lgsqt2)

!  1/3
      REAL one3
      PARAMETER (one3=1.0/3.0)

!  2/3
      REAL two3
      PARAMETER (two3=2.0/3.0)


! *** physical constants:

! Boltzmann's Constant [ J / K ]
      REAL boltz
      PARAMETER (boltz=rgasuniv/avo)


! *** component densities [ kg/m**3 ] :


!  bulk density of aerosol sulfate
      REAL rhoso4
      PARAMETER (rhoso4=1.8E3)

!  bulk density of aerosol ammonium
      REAL rhonh4
      PARAMETER (rhonh4=1.8E3)

! bulk density of aerosol nitrate
      REAL rhono3
      PARAMETER (rhono3=1.8E3)

!  bulk density of aerosol water
      REAL rhoh2o
      PARAMETER (rhoh2o=1.0E3)

! bulk density for aerosol organics
      REAL rhoorg
      PARAMETER (rhoorg=1.0E3)

! bulk density for aerosol soil dust
      REAL rhosoil
      PARAMETER (rhosoil=2.6E3)

! bulk density for marine aerosol
      REAL rhoseas
      PARAMETER (rhoseas=2.2E3)

! bulk density for anthropogenic aerosol
      REAL rhoanth
      PARAMETER (rhoanth=2.2E3)

! bulk density of aerosol sodium
      REAL rhona
      PARAMETER (rhona=2.2E3)

! bulk density of aerosol chloride
      REAL rhocl
      PARAMETER (rhocl=2.2E3)

! *** Factors for converting aerosol mass concentration [ ug m**-3] to
!      	  to 3rd moment concentration [ m**3 m^-3]

      REAL so4fac
      PARAMETER (so4fac=f6dpim9/rhoso4)

      REAL nh4fac
      PARAMETER (nh4fac=f6dpim9/rhonh4)

      REAL h2ofac
      PARAMETER (h2ofac=f6dpim9/rhoh2o)

      REAL no3fac
      PARAMETER (no3fac=f6dpim9/rhono3)

      REAL orgfac
      PARAMETER (orgfac=f6dpim9/rhoorg)

      REAL soilfac
      PARAMETER (soilfac=f6dpim9/rhosoil)

      REAL seasfac
      PARAMETER (seasfac=f6dpim9/rhoseas)

      REAL anthfac
      PARAMETER (anthfac=f6dpim9/rhoanth)

      REAL nafac
      PARAMETER (nafac=f6dpim9/rhona)

      REAL clfac
      PARAMETER (clfac=f6dpim9/rhocl)

!  starting standard surface pressure [ Pa ]  
      REAL pss0
      PARAMETER (pss0=101325.0)

!  starting standard surface temperature [ K ]
      REAL tss0
      PARAMETER (tss0=288.15)

!  initial sigma-G for nucleimode                 
      REAL sginin
      PARAMETER (sginin=1.70)

!  initial sigma-G for accumulation mode          
      REAL sginia
      PARAMETER (sginia=2.00)

! initial sigma-G for coarse mode               
      REAL sginic
      PARAMETER (sginic=2.5)

!  initial mean diameter for nuclei mode [ m ]    
      REAL dginin
      PARAMETER (dginin=0.01E-6)

!  initial mean diameter for accumulation mode [ m ]
      REAL dginia
      PARAMETER (dginia=0.07E-6)

! initial mean diameter for coarse mode [ m ]  
      REAL dginic
      PARAMETER (dginic=1.0E-6)



!................   end   AERO3box.EXT   ...............................
!///////////////////////////////////////////////////////////////////////





!     LOGICAL diagnostics
! *** Scalar variables for fixed standard deviations.

! Flag for writing diagnostics to file       
! nuclei mode exp( log^2( sigmag )/8 )  
      REAL en1
! accumulation mode exp( log^2( sigmag )
      REAL ea1

      REAL ec1
! coarse mode exp( log^2( sigmag )/8 )  
! nuclei        **4                    
      REAL esn04
! accumulation                         
      REAL esa04

      REAL esc04
! coarse                               
! nuclei        **5                    
      REAL esn05

      REAL esa05
! accumulation                         
! nuclei        **8                    
      REAL esn08
! accumulation                         
      REAL esa08

      REAL esc08
! coarse                               
! nuclei        **9                    
      REAL esn09

      REAL esa09
! accumulation                         
! nuclei        **12                   
      REAL esn12
! accumulation                         
      REAL esa12

      REAL esc12
! coarse mode                          
! nuclei        **16                   
      REAL esn16
! accumulation                         
      REAL esa16

      REAL esc16
! coarse                               
! nuclei        **20                   
      REAL esn20
! accumulation                         
      REAL esa20

      REAL esc20
! coarse                               
! nuclei        **25                   
      REAL esn25

      REAL esa25
! accumulation                         
! nuclei        **24                   
      REAL esn24
! accumulation                         
      REAL esa24

      REAL esc24
! coarse                               
! nuclei        **28                   
      REAL esn28
! accumulation                         
      REAL esa28

      REAL esc28
! coarse                               
! nuclei        **32                   
      REAL esn32
! accumulation                         
      REAL esa32

      REAL esc32
! coarese                              
! nuclei        **36                   
      REAL esn36
! accumulation                         
      REAL esa36

      REAL esc36
! coarse                               
! nuclei        **49                   
      REAL esn49

      REAL esa49
! accumulation                         
! nuclei        **52                   
      REAL esn52

      REAL esa52
! accumulation                         
! nuclei        **64                   
      REAL esn64
! accumulation                         
      REAL esa64

      REAL esc64
! coarse                               

      REAL esn100
! nuclei        **100                  
! nuclei        **(-20)                
      REAL esnm20
! accumulation                         
      REAL esam20

      REAL escm20
! coarse                               
! nuclei        **(-32)                
      REAL esnm32
! accumulation                         
      REAL esam32

      REAL escm32
! coarse                               
! log(sginin)                           
      REAL xxlsgn
! log(sginia)                           
      REAL xxlsga

      REAL xxlsgc
! log(sginic )                          
! log(sginin ) ** 2                           
      REAL l2sginin
! log(sginia ) ** 2                           
      REAL l2sginia

      REAL l2sginic


! *** set up COMMON blocks for esg's:



! log(sginic ) ** 2                           

! *** SET NUCLEATION FLAG:

                            ! INUCL = 0, Kerminen & Wexler Mechanism
      INTEGER inucl
                            ! INUCL = 1, Youngblood and Kreidenweis mech
                            ! INUCL = 2, Kulmala et al. mechanism
! Flag for Choice of nucleation Mechanism   
      PARAMETER (inucl=2)

! *** Set flag for sedimentation velocities:

      LOGICAL icoarse
      PARAMETER (icoarse=.FALSE.) ! *** END AERO_INTERNAL.EXT
! *** Diameters and standard deviations for emissions
!     the diameters are the volume (mass) geometric mean diameters

! *** Aitken mode:
! special factor to compute mass transfer           
      REAL dgvem_i
      PARAMETER (dgvem_i=0.03E-6) ! [ m ]                            
      REAL sgem_i
      PARAMETER (sgem_i=1.7)

! *** Accumulation mode:
      REAL dgvem_j
      PARAMETER (dgvem_j=0.3E-6) ! [ m ]                             
      REAL sgem_j
      PARAMETER (sgem_j=2.0)

! *** Coarse mode
      REAL dgvem_c
      PARAMETER (dgvem_c=6.0E-6) ! [ m ] <<< Corrected 11/19/97      
      REAL sgem_c
      PARAMETER (sgem_c=2.2)

! *** factors for getting number emissions rate from mass emissions rate
! Aitken mode                                       
      REAL factnumn
! accumulation mode                                 
      REAL factnuma

      REAL factnumc
! coarse mode                                       
      REAL facatkn_min, facacc_min
      PARAMETER (facatkn_min=0.04,facacc_min=1.0-facatkn_min)
      REAL conmin,xxm3
      PARAMETER (conmin=epsilc)
! [ ug/m**3 ] ! changed 1/6/98 
      REAL*8 & ! factor to set minimum for Aitken mode number  
        nummin_i
      REAL*8 & ! factor to set minimum for accumulation mode nu
        nummin_j
      REAL*8 & 
        nummin_c
! factor to set minimum for coarse mode number  
!bs
!bs      REAL ALPHSULF ! Accommodation coefficient for sulfuric acid
!bs      PARAMETER ( ALPHSULF = 0.05 ) ! my be set to one in future
!bs
!bs      REAL DIFFSULF ! molecular diffusivity for sulfuric acid [ m**2
!bs      PARAMETER( DIFFSULF = 0.08E-4 ) ! may be changed in future
!bs
!bs * 23/03/99 updates of ALPHSULF and DIFFSULF adopted fro new code fro
!bs * DIFFSULF is calculated from Reid, Prausnitz, and Poling, The prope
!bs * of gases and liquids, 4th edition, McGraw-Hill, 1987, pp 587-588.
!bs * Equation (11-4.4) was used.
!bs * The value is at T = 273.16 K and P = 1.01325E05 Pa
!bs * Temperature dependence is included for DIFFSULF via DIFFCORR (see
!bs
! Accommodation coefficient for sulfuric
      REAL alphsulf
      PARAMETER (alphsulf=1.0) 
!bs updated from code of FSB         
! molecular weight for sulfuric acid [ kg/mole ] MKS 
      REAL mwh2so4
      PARAMETER (mwh2so4=98.07354E-3) 
!cia corrected error 24/11/97
! molecular diffusivity for sulfuric acid [ m**2 /se
      REAL diffsulf
      PARAMETER (diffsulf=9.362223E-06) 
!bs updated from code of FSB 
!bs Accomodation coefficient for organic
      REAL alphaorg
      PARAMETER (alphaorg=1.0)                                    !bs Kleeman et al. '99 propose alpha
!bs Bowman et al. '97 uses alpha = 1.
!bs mean molecular weight of organics [k
      REAL mworg
      PARAMETER (mworg=175.0E-03)
!bs
!bs * DIFFORG is calculated from the same formula as DIFFSULF.
!bs * An average elemental composition of C=8, O=3, N=1, H=17 is asuumed
!bs * to calculate DIFFORG at T = 273.16K and  P = 1.01325E05 Pa.
!bs * Temepratur dependence is included below.
!bs molecular diffusivity for organics [
      REAL difforg
      PARAMETER (difforg=5.151174E-06)
! *** CCONC is the factor for near-continuum condensation.
! ccofm * sqrt( ta )                    
      REAL cconc
      PARAMETER (cconc=2.0*pirs*diffsulf) 
!bs * factor for NC condensation for organics
! [ m**2 / sec ]       
      REAL cconc_org
      PARAMETER (cconc_org=2.0*pirs*difforg) 
! [ m**2 / sec ]    
!bs analogue to CCOFM but for organics  
      REAL ccofm_org
! FSB  CCOFM is  the accommodation coefficient
!      times the mean molecular velocity for h2so4 without the temperatu
!      after some algebra

!bs CCOFM_ORG * sqrt(TA)                
! set to a value below                  
      REAL ccofm
! minimum aerosol sulfate concentration          
      REAL aeroconcmin
      PARAMETER (aeroconcmin=0.0001) 



!*******************************************************************
!*                                                                 *
!*  start parameters and variables for aerosol-cloud interactions  *
!*                                                                 *
!*******************************************************************
!
!   maxd_atype = maximum allowable number of aerosol types
!   maxd_asize = maximum allowable number of aerosol size bins
!   maxd_acomp = maximum allowable number of chemical components
!	in each aerosol size bin
!   maxd_aphase = maximum allowable number of aerosol phases (gas, cloud, ice, rain, ...)
!
!   ntype_aer = number of aerosol types
!   nsize_aer(t) = number of aerosol size bins for aerosol type t. each bin w/ same set of components
!   nphase_aer = number of aerosol phases
!
!   msectional - if positive, moving-center sectional code is utilized,
!	and each mode is actually a section.
!   maerosolincw - if positive, both unactivated/interstitial and activated
!	aerosol species are simulated.  if zero/negative, only the
!	unactivated are simulated.
!
!   ncomp_aer(t) = number of chemical components for aerosol type t
!   ncomp_aer_nontracer(t) = number of "non-tracer" chemical components while in gchm code
!   mastercompptr_aer(c,t) = mastercomp type/i.d. for chemical component c
!	(1=sulfate, others to be defined) and aerosol type t.
!   massptr_aer(c,s,t,p) = gchm r-array index for the mixing ratio
!	(moles-x/mole-air) for chemical component c in size bin s for type t and phase p
!
!   waterptr_aer(s,t) = mixing ratio (moles-water/mole-air) for water
!       associated with aerosol size bin s and type t
!   hygroptr_aer(s,t) = gchm r-array index for the bulk hygroscopicity of the size bin and type
!   numptr_aer(s,t,p) = gchm r-array index for the number mixing ratio
!	(particles/mole-air) for aerosol size bin s, type t, and phase p
!       If zero or negative, then number is not being simulated.
!
!   mprognum_aer(s,t,p) - if positive, number mixing-ratio for size s, type t,
!       and phase p will be prognosed.  Otherwise, no.
!
!   ntot_mastercomp_aer = number of aerosol chemical components defined
!   dens_mastercomp_aer(mc) = dry density (g/cm^3) of aerosol master chemical component type c
!   mw_mastercomp_aer(mc) = molecular weight of aerosol master chemical component type mc
!   name_mastercomp_aer(mc) = name of aerosol master chemical component type mc
!   mc=mastercompptr_aer(c,t)
!   dens_aer(c,t) = dry density (g/cm^3) of aerosol chemical component type c and type t
!   mw_aer(c,t) = molecular weight of aerosol chemical component type c and type t
!   name_aer(c,t) = name of aerosol chemical component type c and type t
!
!   lptr_so4_aer(s,t,p) = gchm r-array index for the
!	mixing ratio for sulfate associated with aerosol size bin s, type t, and phase p
!   (similar for msa, oc, bc, nacl, dust)
!
!-----------------------------------------------------------------------
!
!   volumcen_sect(s,t)= volume (cm^3) at center of section m
!   volumlo_sect(s,t) = volume (cm^3) at lower boundary of section m
!   volumhi_sect(s,t) = volume (cm^3) at upper boundary of section m
!
!   dlo_sect(s,t) = diameter (cm) at lower boundary of section m
!   dhi_sect(s,t) = diameter (cm) at upper boundary of section m
!   dcen_sect(s,t) = volume arithmetic-mean diameter (cm) of section m
!	(corresponds to volumcen_sect == 0.5*(volumlo_sect + volumhi_sect)
!
!-----------------------------------------------------------------------
!   nov-04 sg ! replaced amode with aer and expanded aerosol dimension to include type and phase

	integer, parameter :: maxd_atype = 2
	integer, parameter :: maxd_asize = 2
	integer, parameter :: maxd_acomp = 19
	integer, parameter :: maxd_aphase = 2
	integer, save :: ai_phase ! interstitial phase of aerosol
	integer, save :: cw_phase ! cloud water phase of aerosol
	integer, save :: ci_phase ! cloud ice  phase of aerosol
	integer, save :: cr_phase ! rain  phase of aerosol
	integer, save :: cs_phase ! snow  phase of aerosol
	integer, save :: cg_phase ! graupel phase of aerosol

	integer, save :: ntype_aer = 0 ! number of types
	integer, save :: ntot_mastercomp_aer = 0 ! number of master components
	integer, save :: nphase_aer = 0 ! number of phases

	integer, save ::   &
      	  msectional, maerosolincw,   &
      	  nsize_aer( maxd_atype ),   & ! number of size bins
      	  ncomp_aer( maxd_atype ),   & ! number of chemical components
      	  ncomp_aer_nontracer( maxd_atype ),   &
          mastercompptr_aer(maxd_acomp, maxd_atype), &   !  mastercomp index
      	  massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), & ! index for mixing ratio
      	  waterptr_aer( maxd_asize, maxd_atype ), & ! index for aerosol water
      	  hygroptr_aer( maxd_asize, maxd_atype ), & ! index for aerosol hygroscopicity
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), & ! index for the number mixing ratio
          mprognum_aer(maxd_asize,maxd_atype,maxd_aphase)

	real, save ::   &
          dens_aer( maxd_acomp, maxd_atype ),   &
          dens_mastercomp_aer( maxd_acomp ),   &
      	  mw_mastercomp_aer( maxd_acomp ), &
      	  mw_aer( maxd_acomp, maxd_atype ),  &
      	  hygro_mastercomp_aer( maxd_acomp ), &
      	  hygro_aer( maxd_acomp, maxd_atype )
	character*10, save ::   &
      	  name_mastercomp_aer( maxd_acomp ), &
      	  name_aer( maxd_acomp, maxd_atype )

	real, save ::   &
          volumcen_sect( maxd_asize, maxd_atype ),   &
          volumlo_sect( maxd_asize, maxd_atype ),   &
          volumhi_sect( maxd_asize, maxd_atype ),   &
          dcen_sect( maxd_asize, maxd_atype ),   &
          dlo_sect( maxd_asize, maxd_atype ),   &
          dhi_sect( maxd_asize, maxd_atype ),   &
	  sigmag_aer(maxd_asize, maxd_atype)

	integer, save ::                     &
      	  lptr_so4_aer(maxd_asize,maxd_atype,maxd_aphase),        &
      	  lptr_nh4_aer(maxd_asize,maxd_atype,maxd_aphase),        &
      	  lptr_no3_aer(maxd_asize,maxd_atype,maxd_aphase),        &
      	  lptr_orgaro1_aer(maxd_asize,maxd_atype,maxd_aphase),    &
      	  lptr_orgaro2_aer(maxd_asize,maxd_atype,maxd_aphase),    &
      	  lptr_orgalk_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgole_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgba1_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgba2_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgba3_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgba4_aer(maxd_asize,maxd_atype,maxd_aphase),     &
      	  lptr_orgpa_aer(maxd_asize,maxd_atype,maxd_aphase),      &
      	  lptr_ec_aer(maxd_asize,maxd_atype,maxd_aphase),         &
      	  lptr_p25_aer(maxd_asize,maxd_atype,maxd_aphase),        &
          lptr_anth_aer(maxd_asize,maxd_atype,maxd_aphase),       &
      	  lptr_cl_aer(maxd_asize,maxd_atype,maxd_aphase),         &
      	  lptr_na_aer(maxd_asize,maxd_atype,maxd_aphase),         &
      	  lptr_seas_aer(maxd_asize,maxd_atype,maxd_aphase),       &
      	  lptr_soil_aer(maxd_asize,maxd_atype,maxd_aphase)

	logical, save ::                     &
      	  do_cloudchem_aer(maxd_asize,maxd_atype)


!   molecular weights (g/mol)
	real, parameter :: mw_so4_aer   = 96.066
	real, parameter :: mw_no3_aer   = 62.007
	real, parameter :: mw_nh4_aer   = 18.042
	real, parameter :: mw_oc_aer    = 250.0
	real, parameter :: mw_ec_aer    = 1.0
	real, parameter :: mw_oin_aer   = 1.0
	real, parameter :: mw_dust_aer  = 100.087
	real, parameter :: mw_seas_aer  = 58.440
	real, parameter :: mw_cl_aer    = 35.450
	real, parameter :: mw_na_aer    = 22.990
	real, parameter :: mw_water_aer = 18.016

!   dry densities (g/cm3)
	real, parameter :: dens_so4_aer  = 1.80   ! = rhoso4
	real, parameter :: dens_no3_aer  = 1.80   ! = rhono3
	real, parameter :: dens_nh4_aer  = 1.80   ! = rhonh4
	real, parameter :: dens_oc_aer   = 1.00   ! = rhoorg
	real, parameter :: dens_ec_aer   = 1.70
	real, parameter :: dens_dust_aer = 2.60  ! = rhosoil
	real, parameter :: dens_oin_aer  = 2.20  ! = rhoanth
	real, parameter :: dens_seas_aer = 2.20  ! = rhoseas
	real, parameter :: dens_cl_aer   = 2.20
	real, parameter :: dens_na_aer   = 2.20

!   water density (g/cm3)
	real, parameter :: dens_water_aer  = 1.0

!   hygroscopicity (dimensionless)
	real, parameter :: hygro_so4_aer  = 0.5
	real, parameter :: hygro_no3_aer  = 0.5
	real, parameter :: hygro_nh4_aer  = 0.5
	real, parameter :: hygro_oc_aer   = 0.14
	real, parameter :: hygro_ec_aer   = 1.e-6
	real, parameter :: hygro_oin_aer  = 0.14
	real, parameter :: hygro_dust_aer = 0.1
	real, parameter :: hygro_seas_aer = 1.16
	real, parameter :: hygro_cl_aer   = 1.16
	real, parameter :: hygro_na_aer   = 1.16

! table lookup of aerosol impaction/interception scavenging rates
	real dlndg_nimptblgrow
	integer nimptblgrow_mind, nimptblgrow_maxd
	parameter (nimptblgrow_mind=-14, nimptblgrow_maxd=24)
     	real scavimptblnum(4, nimptblgrow_mind:nimptblgrow_maxd, maxd_asize, maxd_atype), &
     	     scavimptblvol(4, nimptblgrow_mind:nimptblgrow_maxd, maxd_asize, maxd_atype)


!SAM 10/08 Gaussian quadrature constants for SORGAM deposition numerical integration
      INTEGER NGAUSdv
      PARAMETER( NGAUSdv = 7 )  ! Number of Gaussian Quadrature Points - constants defined in aerosols_sorgam_init
      REAL Y_GQ(NGAUSdv), WGAUS(NGAUSdv)

!*****************************************************************
!*                                                               *
!*  end parameters and variables for aerosol-cloud interactions  *
!*                                                               *
!*****************************************************************


END Module module_data_sorgam
