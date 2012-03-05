!WRF:MODEL_LAYER:CHEMICS
!
    MODULE module_data_radm2
      IMPLICIT NONE
      REAL, PARAMETER ::  epsilc = 1.E-16

!--- for radm solver
! .. Parameters ..
      INTEGER, PARAMETER :: ldiag = 18, lpred = 39, lss = 2, &
        lump = 4, naqre = 70,  nreacj = 21, nreack = 140, &
        ntroe = 7, numchem_radm = 41
      INTEGER, PARAMETER :: lspec = lpred + lss
      INTEGER, DIMENSION(1:NTROE) :: itroe = (/11, 22, 10, 15, 21, 24, 28/)
!
!
!
      INTEGER, PARAMETER :: lso2=1
      INTEGER, PARAMETER :: lsulf=2
      INTEGER, PARAMETER :: lno2=3
      INTEGER, PARAMETER :: lno=4
      INTEGER, PARAMETER :: lo3=5
      INTEGER, PARAMETER :: lhno3=6
      INTEGER, PARAMETER :: lh2o2=7
      INTEGER, PARAMETER :: lald=8
      INTEGER, PARAMETER :: lhcho=9
      INTEGER, PARAMETER :: lop1=10
      INTEGER, PARAMETER :: lop2=11
      INTEGER, PARAMETER :: lpaa=12
      INTEGER, PARAMETER :: lora1=13
      
      INTEGER, PARAMETER :: lora2=14
      INTEGER, PARAMETER :: lnh3=15
      INTEGER, PARAMETER :: ln2o5=16
      INTEGER, PARAMETER :: lno3=17
      INTEGER, PARAMETER :: lpan=18
      INTEGER, PARAMETER :: lhc3=19
      INTEGER, PARAMETER :: lhc5=20
      INTEGER, PARAMETER :: lhc8=21
      
      INTEGER, PARAMETER :: leth=22
      INTEGER, PARAMETER :: lco=23
      INTEGER, PARAMETER :: lol2=24
      INTEGER, PARAMETER :: lolt=25
      INTEGER, PARAMETER :: loli=26
      INTEGER, PARAMETER :: ltol=27
      INTEGER, PARAMETER :: lxyl=28
      INTEGER, PARAMETER :: laco3=29
      
      INTEGER, PARAMETER :: ltpan=30
      INTEGER, PARAMETER :: lhono=31
      INTEGER, PARAMETER :: lhno4=32
      INTEGER, PARAMETER :: lket=33
      INTEGER, PARAMETER :: lgly=34
      INTEGER, PARAMETER :: lmgly=35
      INTEGER, PARAMETER :: ldcb=36
      INTEGER, PARAMETER :: lonit=37
      
      INTEGER, PARAMETER :: lcsl=38
      INTEGER, PARAMETER :: liso=39
      INTEGER, PARAMETER :: lho=40
      INTEGER, PARAMETER :: lho2=41
! parameters for timestep, integration
      INTEGER, DIMENSION(1:lpred) :: intgrt = (/1, 1, 1, 0, 1, &
                                 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, &
                                 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, &
                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                                                   1, 1, 1, 1 /)
!     INTEGER, DIMENSION(1:lspec) ::  qdtc  = (/0, 0, 1, 0, 1, &
!                                0, 1, 0, 1, 0, 0, 0, 1, 0, 0, &
!                                1, 1, 1, 0, 0, 0, 0, 0, 0, 0, &
!                                0, 0, 0, 1, 1, 1, 1, 0, 0, 0, &
!                                            0, 0, 0, 0, 0, 0 /)
      INTEGER, DIMENSION(1:lspec) ::  qdtc  = (/1, 1, 1, 0, 1, &
                                 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, &
                                 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, &
                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                                             1, 1, 1, 1, 0, 0 /)
! max, min values,
      INTEGER :: itrdu
!
      REAL, DIMENSION(1:lspec) :: cmin =(/(1.E-16,itrdu=1,lspec)/)
!
      REAL, DIMENSION(1:lspec) :: cmax=(/1.,  1.,  1.,   1., .2, &
                3., .05, .01, .01, .01, .05, .01, .05,  .05,.05, &
                1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,   1., 1., &
                1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,.0001, .1, &
                                    1., .001, .01, .01, .01, .01/) 

!
!
!
      INTEGER, PARAMETER :: lo3p=1
      INTEGER, PARAMETER :: lo1d=2
      INTEGER, PARAMETER :: ltco3=3
      INTEGER, PARAMETER :: lhc3p=4
      INTEGER, PARAMETER :: lhc5p=5
      INTEGER, PARAMETER :: lhc8p=6
       
      INTEGER, PARAMETER :: lol2p=7
      INTEGER, PARAMETER :: loltp=8
      INTEGER, PARAMETER :: lolip=9
      INTEGER, PARAMETER :: ltolp=10
      INTEGER, PARAMETER :: lxylp=11
      INTEGER, PARAMETER :: lethp=12
      INTEGER, PARAMETER :: lketp=13
      INTEGER, PARAMETER :: loln=14
       
      INTEGER, PARAMETER :: lxo2=15
      INTEGER, PARAMETER :: lxno2=16
      INTEGER, PARAMETER :: lxho=17
      INTEGER, PARAMETER :: lmo2=18
!
!
      INTEGER, PARAMETER ::  lnox=1
      INTEGER, PARAMETER ::  lhox=2
      INTEGER, PARAMETER ::  lpao3=3
      INTEGER, PARAMETER ::  ln2n3=4
! ..
      REAL, PARAMETER ::  ch4=1.7
      REAL, PARAMETER ::  co2=350.
      REAL, PARAMETER ::  n2=7.81E5
      REAL, PARAMETER ::  o2=2.09E5
      REAL, PARAMETER ::  pi=3.141592654

! ..
      REAL :: afac(2), &
        bfac(2),  const(3), eor(nreack), &
        thafac(nreack), &
        xk0300(ntroe), &
        xkf300(ntroe), xmtroe(ntroe), xntroe(ntroe)

! ..
! .. Data Statements ..
      DATA thafac/0.00, 6.50E-12, 1.80E-11, 3.20E-11, 2.20E-10, 2.00E-12, &
        1.60E-12, 1.10E-14, 3.70E-12, 4*0.00, 3.30E-12, 0.00, 3.30E-19, &
        1.40E-13, 1.70E-11, 2.50E-14, 2.50E-12, 2*0.00, 2.00E-21, 2*0.00, &
        1.30E-12, 4.60E-11, 2*0.00, 6.95E-18, 1.37E-17, 1.59E-11, 1.73E-11, &
        3.64E-11, 2.15E-12, 5.32E-12, 1.07E-11, 2.10E-12, 1.89E-11, 4.00E-11, &
        9.00E-12, 6.87E-12, 1.20E-11, 1.15E-11, 1.70E-11, 2.80E-11, 1.00E-11, &
        1.00E-11, 1.00E-11, 6.85E-18, 1.55E-11, 2.55E-11, 2.80E-12, 1.95E+16, &
        4.70E-12, 1.95E+16, 4.20E-12, 4.20E-12, 0.00, 4.20E-12, 0.00, &
        4.20E-12, 0.00, 10*4.20E-12, 6.00E-13, 1.40E-12, 6.00E-13, 1.40E-12, &
        1.40E-12, 2.20E-11, 2.00E-12, 1.00E-11, 3.23E-11, 5.81E-13, 1.20E-14, &
        1.32E-14, 7.29E-15, 1.23E-14, 14*7.70E-14, 1.90E-13, 1.40E-13, &
        4.20E-14, 3.40E-14, 2.90E-14, 1.40E-13, 1.40E-13, 1.70E-14, 1.70E-14, &
        9.60E-13, 1.70E-14, 1.70E-14, 9.60E-13, 3.40E-13, 1.00E-13, 8.40E-14, &
        7.20E-14, 3.40E-13, 3.40E-13, 4.20E-14, 4.20E-14, 1.19E-12, 4.20E-14, &
        4.20E-14, 1.19E-12, 7.70E-14, 1.70E-14, 4.20E-14, 3.60E-16, 4.20E-12, &
        4.20E-12, 7.70E-14, 1.70E-14, 4.20E-14, 3.60E-16, 0.00, 1.70E-14, &
        4.20E-14, 3.60E-16/
! ..
! constants for RADM2 rate coefficients
      DATA eor/0., -120., -110., -70., 0., 1400., 940., 500., -240., 0., 0., &
        0., 0., 200., 0., -530., 2500., -150., 1230., 0., 0., 0., 0., 0., 0., &
        -380., -230., 0., 0., 1280., 444., 540., 380., 380., -411., -504., &
        -549., -322., -116., 0., 0., -256., 745., 0., 0., 0., 0., 0., 0., &
        444., 540., -409., -181., 13543., 0., 13543., -180., -180., 0., -180., &
        0., -180., 0., -180., -180., -180., -180., -180., -180., -180., -180., &
        -180., -180., 2058., 1900., 2058., 1900., 1900., 0., 2923., 1895., &
        975., 0., 2633., 2105., 1136., 2013., -1300., -1300., -1300., -1300., &
        -1300., -1300., -1300., -1300., -1300., -1300., -1300., -1300., &
        -1300., -1300., 25* -220., -1300., -220., -220., -220., -180., -180., &
        -1300., -220., -220., 0., 0., -220., -220., -220./

      DATA xk0300/1.8E-31, 2.2E-30, 1.8E-31, 7.E-31, 2.2E-30, 2.6E-30, 3.E-31/
      DATA xntroe/3.2, 4.3, 3.2, 2.6, 4.3, 3.2, 3.3/
      DATA xkf300/4.7E-12, 1.5E-12, 4.7E-12, 1.5E-11, 1.5E-12, 2.4E-11, &
        1.5E-12/
      DATA xmtroe/1.4, 0.5, 1.4, 2*.5, 1.3, 0./
      DATA afac/2.1E-27, 1.1E-27/
      DATA bfac/10900., 11200./
      DATA const/7.34E21, 4.4E17, 3.23E33/

    END MODULE module_data_radm2
