
MODULE module_data_sorgam
   USE module_data_radm2
!
!   param.inc start
      IMPLICIT NONE
      INTEGER NP                !bs maximum expected value of N
      PARAMETER (NP = 8)
      integer numaer
      parameter (numaer=50)

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
      PARAMETER (laerdvc=34,lnonaerdvc=8+lspcv)
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

! index for Accumulation mode sulfate aeroso
      INTEGER vso4aj
      PARAMETER (vso4aj=1)

! index for Aitken mode sulfate concentraton
      INTEGER vso4ai
      PARAMETER (vso4ai=2)

! index for Accumulation mode aerosol ammoni
      INTEGER vnh4aj
      PARAMETER (vnh4aj=3)

! index for Aitken mode ammonium concentrati
      INTEGER vnh4ai
      PARAMETER (vnh4ai=4)

! index for Accumulation mode aerosol nitrat
      INTEGER vno3aj
      PARAMETER (vno3aj=5)

! index for Aitken mode nitrate concentratio
      INTEGER vno3ai
      PARAMETER (vno3ai=6)

! index for Accumulation mode anthropogen
      INTEGER vorgaro1j
      PARAMETER (vorgaro1j=7)

! index for Aitken mode anthropogenic org
      INTEGER vorgaro1i
      PARAMETER (vorgaro1i=8)

! index for Accumulation mode anthropogen
      INTEGER vorgaro2j
      PARAMETER (vorgaro2j=9)

! index for Aitken mode anthropogenic org
      INTEGER vorgaro2i
      PARAMETER (vorgaro2i=10)

! index for Accumulation mode anthropogen
      INTEGER vorgalk1j
      PARAMETER (vorgalk1j=11)

! index for Aitken mode anthropogenic org
      INTEGER vorgalk1i
      PARAMETER (vorgalk1i=12)

! index for Accumulation mode anthropogen
      INTEGER vorgole1j
      PARAMETER (vorgole1j=13)

! index for Aitken mode anthropogenic org
      INTEGER vorgole1i
      PARAMETER (vorgole1i=14)

! index for Accumulation mode biogenic aero
      INTEGER vorgba1j
      PARAMETER (vorgba1j=15)

! index for Aitken mode biogenic aerosol co
      INTEGER vorgba1i
      PARAMETER (vorgba1i=16)

! index for Accumulation mode biogenic aero
      INTEGER vorgba2j
      PARAMETER (vorgba2j=17)

! index for Aitken mode biogenic aerosol co
      INTEGER vorgba2i
      PARAMETER (vorgba2i=18)

! index for Accumulation mode biogenic aero
      INTEGER vorgba3j
      PARAMETER (vorgba3j=19)

! index for Aitken mode biogenic aerosol co
      INTEGER vorgba3i
      PARAMETER (vorgba3i=20)

! index for Accumulation mode biogenic aero
      INTEGER vorgba4j
      PARAMETER (vorgba4j=21)

! index for Aitken mode biogenic aerosol co
      INTEGER vorgba4i
      PARAMETER (vorgba4i=22)

! index for Accumulation mode primary anthro
      INTEGER vorgpaj
      PARAMETER (vorgpaj=23)

! index for Aitken mode primary anthropogeni
      INTEGER vorgpai
      PARAMETER (vorgpai=24)

! index for Accumulation mode aerosol elemen
      INTEGER vecj
      PARAMETER (vecj=25)

! index for Aitken mode elemental carbon    
      INTEGER veci
      PARAMETER (veci=26)

! index for Accumulation mode primary PM2.5 
      INTEGER vp25aj
      PARAMETER (vp25aj=27)

! index for Aitken mode primary PM2.5 concen
      INTEGER vp25ai
      PARAMETER (vp25ai=28)

! index for coarse mode anthropogenic aeroso
      INTEGER vantha
      PARAMETER (vantha=29)

! index for coarse mode marine aerosol conce
      INTEGER vseas
      PARAMETER (vseas=30)

! index for coarse mode soil-derived aerosol
      INTEGER vsoila
      PARAMETER (vsoila=31)

! index for Aitken mode number              
      INTEGER vnu0
      PARAMETER (vnu0=32)

! index for accum  mode number              
      INTEGER vac0
      PARAMETER (vac0=33)

! index for coarse mode number              
      INTEGER vcorn
      PARAMETER (vcorn=34)

! index for Accumulation mode aerosol water 
      INTEGER vh2oaj
      PARAMETER (vh2oaj=35)

! index for Aitken mode aerosol water concen
      INTEGER vh2oai
      PARAMETER (vh2oai=36)

! index for Aitken mode 3'rd moment         
      INTEGER vnu3
      PARAMETER (vnu3=37)

! index for Accumulation mode 3'rd moment   
      INTEGER vac3
      PARAMETER (vac3=38)

! index for coarse mode 3rd moment          
      INTEGER vcor3
      PARAMETER (vcor3=39)

! index for sulfuric acid vapor concentratio
      INTEGER vsulf
      PARAMETER (vsulf=40)

! index for nitric acid vapor concentration 
      INTEGER vhno3
      PARAMETER (vhno3=41)

! index for ammonia gas concentration       
      INTEGER vnh3
      PARAMETER (vnh3=42)

! index for cond. vapor from aromatics   
      INTEGER vcvaro1
      PARAMETER (vcvaro1=43)

! index for cond. vapor from aromatics   
      INTEGER vcvaro2
      PARAMETER (vcvaro2=44)

! index for cond. vapor from anth. alkane
      INTEGER vcvalk1
      PARAMETER (vcvalk1=45)

! index for cond. vapor from anth. olefin
      INTEGER vcvole1
      PARAMETER (vcvole1=46)

! index for cond. vapor from biogenics   
      INTEGER vcvapi1
      PARAMETER (vcvapi1=47)

! index for cond. vapor from biogenics   
      INTEGER vcvapi2
      PARAMETER (vcvapi2=48)

! index for cond. vapor from biogenics   
      INTEGER vcvlim1
      PARAMETER (vcvlim1=49)

! index for cond. vapor from biogenics   
      INTEGER vcvlim2
      PARAMETER (vcvlim2=50)

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

! Boltzmann's Constant [ J / K]             
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

!  initial mean diameter for accumulation mode [ m
      REAL dginia
      PARAMETER (dginia=0.07E-6)

! initial mean diameter for coarse mode [ m ]  
      REAL dginic
      PARAMETER (dginic=1.0E-6)



!................   end   AERO3box.EXT   ...............................
!///////////////////////////////////////////////////////////////////////





      LOGICAL diagnostics
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



END Module module_data_sorgam
