!WRF:MODEL_LAYER:CONSTANTS
!
!----------------------------------------------------------------------
      MODULE module_nmm_constants
!----------------------------------------------------------------------
!
!***  A really small number.
!
      REAL,PARAMETER :: EPSILON=1.E-15
!----------------------------------------------------------------------
!
!***  FOLLOWING IS INFORMATION RELATED TO THE PHYSICAL CONSTANTS.
!
! JM NOTE -- can we name this grav instead?
!
      REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

      REAL    , PARAMETER :: r_d          = 287.04
      REAL    , PARAMETER :: r_v          = 461.6
      REAL    , PARAMETER :: cp           = 1004.6
      REAL    , PARAMETER :: cv           = cp-r_d
      REAL    , PARAMETER :: cliq         = 4190.
      REAL    , PARAMETER :: cice         = 2106.
      REAL    , PARAMETER :: psat         = 610.78
      REAL    , PARAMETER :: rcv          = r_d/cv
      REAL    , PARAMETER :: rcp          = r_d/cp
      REAL    , PARAMETER :: c2           = cp * rcv
!
      REAL    , PARAMETER :: p1000mb      = 100000.
      REAL    , PARAMETER :: cpovcv       = cp/(cp-r_d)
      REAL    , PARAMETER :: cvovcp       = 1./cpovcv
      REAL    , PARAMETER :: rvovrd       = r_v/r_d
!
      REAL    , PARAMETER :: reradius     = 1./6370.e03 
!
      REAL    , PARAMETER :: asselin      = .025
      REAL    , PARAMETER :: cb           = 25.
!
      REAL    , PARAMETER :: XLV0         = 3.15E6
      REAL    , PARAMETER :: XLV1         = 2370.
      REAL    , PARAMETER :: XLS0         = 2.905E6
      REAL    , PARAMETER :: XLS1         = 259.532
!
      REAL    , PARAMETER :: XLS          = 2.85E6
      REAL    , PARAMETER :: XLV          = 2.5E6
      REAL    , PARAMETER :: XLF          = 3.50E5
!
      REAL    , PARAMETER :: rhowater     = 1000.
      REAL    , PARAMETER :: rhosnow      = 100.
      REAL    , PARAMETER :: rhoair0      = 1.28
!
      REAL    , PARAMETER :: DEGRAD       = 3.1415926/180.
      REAL    , PARAMETER :: DPD          = 360./365.
!
      REAL    , PARAMETER ::  SVP1=0.6112
      REAL    , PARAMETER ::  SVP2=17.67
      REAL    , PARAMETER ::  SVP3=29.65
      REAL    , PARAMETER ::  SVPT0=273.15
      REAL    , PARAMETER ::  EP_1=R_v/R_d-1.
      REAL    , PARAMETER ::  EP_2=R_d/R_v
      REAL    , PARAMETER ::  KARMAN=0.4
      REAL    , PARAMETER ::  EOMEG=7.2921E-5
      REAL    , PARAMETER ::  STBOLT=5.67051E-8
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***
!***  NCEP_MESO
!***
!----------------------------------------------------------------------
      REAL,PARAMETER :: A2=17.2693882
      REAL,PARAMETER :: A3=273.16
      REAL,PARAMETER :: A4=35.86
      REAL,PARAMETER :: CLIMIT=1.E-20
      REAL,PARAMETER :: CM1=2937.4
      REAL,PARAMETER :: CM2=4.9283
      REAL,PARAMETER :: CM3=23.5518
      REAL,PARAMETER :: DEFC=8.0
      REAL,PARAMETER :: DEFM=32.0
      REAL,PARAMETER :: EPSFC=1./1.05
      REAL,PARAMETER :: EPSQ=1.E-12
      REAL,PARAMETER :: EPSQ2=0.2
      REAL,PARAMETER :: EPSWET=0.0
      REAL,PARAMETER :: FCM=0.0003
      REAL,PARAMETER :: FF1=0.52500
      REAL,PARAMETER :: FF2=-0.64813
      REAL,PARAMETER :: FF3=0.24520
      REAL,PARAMETER :: FF4=-0.12189
      REAL,PARAMETER :: FCDIF=1.0
      REAL,PARAMETER :: GMA=-R_D*(1.-RCP)*0.5
      REAL,PARAMETER :: P400=40000.0
      REAL,PARAMETER :: P608=RVOVRD-1.
      REAL,PARAMETER :: PI2=2.*3.1415926
      REAL,PARAMETER :: PHITP=15000.0
      REAL,PARAMETER :: PLBTM=105000.0
      REAL,PARAMETER :: PLOMD=64200.0
      REAL,PARAMETER :: PMDHI=35000.0
      REAL,PARAMETER :: PQ0=379.90516
      REAL,PARAMETER :: Q2INI=0.50
      REAL,PARAMETER :: RFCP=0.25/CP
      REAL,PARAMETER :: RHCRIT_LAND=0.75
      REAL,PARAMETER :: RHCRIT_SEA=0.80
      REAL,PARAMETER :: RLAG=14.8125
      REAL,PARAMETER :: RLX=0.90
      REAL,PARAMETER :: SCQ2=50.0
      REAL,PARAMETER :: TLC=2.*0.703972477
      REAL,PARAMETER :: WA=0.15
      REAL,PARAMETER :: WGHT=0.35
      REAL,PARAMETER :: WPC=0.075
      REAL,PARAMETER :: Z0LAND=0.10
      REAL,PARAMETER :: Z0SEA=0.001
      REAL,PARAMETER :: Z0MAX=10.0
!
      REAL,DIMENSION(3) :: CTHK,TAUCV
!
!----------------------------------------------------------------------
      CONTAINS
!
!----------------------------------------------------------------------
      SUBROUTINE INIT_MODULE_NMM_CONSTANTS
!
      CTHK=(/20000.0,20000.0,20000.0/)
      TAUCV=(/0.16,0.14,0.12/)
!
      END SUBROUTINE INIT_MODULE_NMM_CONSTANTS
!----------------------------------------------------------------------
      END MODULE MODULE_NMM_CONSTANTS
