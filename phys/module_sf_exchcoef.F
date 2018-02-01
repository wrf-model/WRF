!  This MODULE holds the routines that calculate air-sea exchange coefficients 

MODULE module_sf_exchcoef
CONTAINS

  SUBROUTINE znot_m_v1(uref,znotm)
  IMPLICIT NONE

! uref(m/s)   :   Reference level wind
! znotm(meter):   Roughness scale for momentum
! Author      :  Biju Thomas on 02/07/2014
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znotm
    REAL             :: bs0, bs1, bs2, bs3, bs4, bs5, bs6
    REAL             :: cf0, cf1, cf2, cf3, cf4, cf5, cf6


    bs0 = -8.367276172397277e-12
    bs1 = 1.7398510865876079e-09
    bs2 = -1.331896578363359e-07
    bs3 = 4.507055294438727e-06
    bs4 = -6.508676881906914e-05
    bs5 = 0.00044745137674732834
    bs6 = -0.0010745704660847233

    cf0 = 2.1151080765239772e-13
    cf1 = -3.2260663894433345e-11
    cf2 = -3.329705958751961e-10
    cf3 = 1.7648562021709124e-07
    cf4 = 7.107636825694182e-06
    cf5 = -0.0013914681964973246
    cf6 = 0.0406766967657759


    IF ( uref .LE. 5.0 ) THEN
      znotm = (0.0185 / 9.8*(7.59e-4*uref**2+2.46e-2*uref)**2)
    ELSEIF (uref .GT. 5.0 .AND. uref .LT. 10.0) THEN
      znotm =.00000235*(uref**2 - 25 ) + 3.805129199617346e-05
    ELSEIF ( uref .GE. 10.0  .AND. uref .LT. 60.0) THEN
      znotm = bs6 + bs5*uref + bs4*uref**2 + bs3*uref**3 + bs2*uref**4 +  &
              bs1*uref**5 + bs0*uref**6
    ELSE
      znotm = cf6 + cf5*uref + cf4*uref**2 + cf3*uref**3 + cf2*uref**4 +  &
              cf1*uref**5 + cf0*uref**6

    END IF

  END SUBROUTINE znot_m_v1
        
  SUBROUTINE znot_m_v0(uref,znotm)
  IMPLICIT NONE

! uref(m/s)   :   Reference level wind
! znotm(meter):   Roughness scale for momentum
! Author      :  Biju Thomas on 02/07/2014

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znotm 
    REAL             :: yz, y1, y2, y3, y4

    yz =  0.0001344
    y1 =  3.015e-05
    y2 =  1.517e-06
    y3 = -3.567e-08
    y4 =  2.046e-10

    IF ( uref .LT. 12.5 ) THEN
       znotm  = (0.0185 / 9.8*(7.59e-4*uref**2+2.46e-2*uref)**2)
    ELSE IF ( uref .GE. 12.5 .AND. uref .LT. 30.0 ) THEN
       znotm = (0.0739793 * uref -0.58)/1000.0
    ELSE
       znotm = yz + uref*y1 + uref**2*y2 + uref**3*y3 + uref**4*y4
    END IF

  END SUBROUTINE znot_m_v0


  SUBROUTINE znot_t_v1(uref,znott)
  IMPLICIT NONE

! uref(m/s)   :   Reference level wind
! znott(meter):   Roughness scale for temperature/moisture
! Author      :  Biju Thomas on 02/07/2014

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znott
    REAL             :: to0, to1, to2, to3
    REAL             :: tr0, tr1, tr2, tr3
    REAL             :: tn0, tn1, tn2, tn3, tn4, tn5
    REAL             :: ta0, ta1, ta2, ta3, ta4, ta5, ta6
    REAL             :: tt0, tt1, tt2, tt3, tt4, tt5, tt6, tt7


    tr0 = 6.451939325286488e-08
    tr1 = -7.306388137342143e-07
    tr2 = -1.3709065148333262e-05
    tr3 = 0.00019109962089098182

    to0 = 1.4379320027061375e-08
    to1 = -2.0674525898850674e-07
    to2 = -6.8950970846611e-06
    to3 = 0.00012199648268521026

    tn0 = 1.4023940955902878e-10
    tn1 = -1.4752557214976321e-08
    tn2 = 5.90998487691812e-07
    tn3 = -1.0920804077770066e-05
    tn4 = 8.898205876940546e-05
    tn5 = -0.00021123340439418298

    tt0 = 1.92409564131838e-12
    tt1 = -5.765467086754962e-10
    tt2 = 7.276979099726975e-08
    tt3 = -5.002261599293387e-06
    tt4 = 0.00020220445539973736
    tt5 = -0.0048088230565883
    tt6 = 0.0623468551971189
    tt7 = -0.34019193746967424

    ta0 = -1.7787470700719361e-10
    ta1 = 4.4691736529848764e-08
    ta2 = -3.0261975348463414e-06
    ta3 = -0.00011680322286017206
    ta4 = 0.024449377821884846
    ta5 = -1.1228628619105638
    ta6 = 17.358026773905973

    IF ( uref .LE. 7.0 ) THEN
      znott = (0.0185 / 9.8*(7.59e-4*uref**2+2.46e-2*uref)**2)
    ELSEIF ( uref  .GE. 7.0 .AND. uref .LT. 12.5 ) THEN
      znott =  tr3 + tr2*uref + tr1*uref**2 + tr0*uref**3
    ELSEIF ( uref  .GE. 12.5 .AND. uref .LT. 15.0 ) THEN
      znott =  to3 + to2*uref + to1*uref**2 + to0*uref**3
    ELSEIF ( uref .GE. 15.0  .AND. uref .LT. 30.0) THEN
      znott =  tn5 + tn4*uref + tn3*uref**2 + tn2*uref**3 + tn1*uref**4 +   &
                                                       tn0*uref**5
    ELSEIF ( uref .GE. 30.0  .AND. uref .LT. 60.0) THEN
      znott = tt7 + tt6*uref + tt5*uref**2  + tt4*uref**3 + tt3*uref**4 +   &
             tt2*uref**5 + tt1*uref**6 + tt0*uref**7
    ELSE
      znott =  ta6 + ta5*uref + ta4*uref**2  + ta3*uref**3 + ta2*uref**4 +  &
              ta1*uref**5 + ta0*uref**6
    END IF

  END SUBROUTINE znot_t_v1
        
  SUBROUTINE znot_t_v0(uref,znott)
  IMPLICIT NONE

! uref(m/s)   :   Reference level wind
! znott(meter):   Roughness scale for temperature/moisture
! Author      :  Biju Thomas on 02/07/2014

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znott 

    IF ( uref .LT. 7.0 ) THEN
       znott = (0.0185 / 9.8*(7.59e-4*uref**2+2.46e-2*uref)**2)
    ELSE
       znott = (0.2375*exp(-0.5250*uref) + 0.0025*exp(-0.0211*uref))*0.01
    END IF

  END SUBROUTINE znot_t_v0


  SUBROUTINE znot_t_v2(uu,znott)
  IMPLICIT NONE

! uu in MKS
! znott in m
! Biju Thomas on 02/12/2015
!

    REAL, INTENT(IN) :: uu
    REAL, INTENT(OUT):: znott
    REAL             :: ta0, ta1, ta2, ta3, ta4, ta5, ta6
    REAL             :: tb0, tb1, tb2, tb3, tb4, tb5, tb6
    REAL             :: tt0, tt1, tt2, tt3, tt4, tt5, tt6

    ta0 = 2.51715926619e-09
    ta1 = -1.66917514012e-07
    ta2 = 4.57345863551e-06
    ta3 = -6.64883696932e-05
    ta4 = 0.00054390175125
    ta5 = -0.00239645231325
    ta6 = 0.00453024927761


    tb0 = -1.72935914649e-14
    tb1 = 2.50587455802e-12
    tb2 = -7.90109676541e-11
    tb3 = -4.40976353607e-09
    tb4 = 3.68968179733e-07
    tb5 = -9.43728336756e-06
    tb6 = 8.90731312383e-05

    tt0 = 4.68042680888e-14
    tt1 = -1.98125754931e-11
    tt2 = 3.41357133496e-09
    tt3 = -3.05130605309e-07
    tt4 = 1.48243563819e-05
    tt5 = -0.000367207751936
    tt6 = 0.00357204479347

    IF ( uu .LE. 7.0 ) THEN
       znott = (0.0185 / 9.8*(7.59e-4*uu**2+2.46e-2*uu)**2)
    ELSEIF ( uu  .GE. 7.0 .AND. uu .LT. 15. ) THEN
       znott = ta6 + ta5*uu + ta4*uu**2  + ta3*uu**3 + ta2*uu**4 +     &
               ta1*uu**5 + ta0*uu**6
    ELSEIF ( uu .GE. 15.0  .AND. uu .LT. 60.0) THEN
       znott = tb6 + tb5*uu + tb4*uu**2 + tb3*uu**3 + tb2*uu**4 +      & 
               tb1*uu**5 + tb0*uu**6
    ELSE
       znott = tt6 + tt5*uu + tt4*uu**2  + tt3*uu**3 + tt2*uu**4 +    &
               tt1*uu**5 + tt0*uu**6
    END IF

  END SUBROUTINE znot_t_v2

  SUBROUTINE znot_m_v6(uref,znotm)
  IMPLICIT NONE
! Calculate areodynamical roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Cd-U10 relationship from COARE V3.5 (Edson et al. 2013)
! For high winds, try to fit available observational data
!
! Bin Liu, NOAA/NCEP/EMC 2017
! 
! uref(m/s)   :   wind speed at 10-m height
! znotm(meter):   areodynamical roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znotm
    REAL             :: p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p40

    p13 = -1.296521881682694e-02
    p12 =  2.855780863283819e-01
    p11 = -1.597898515251717e+00
    p10 = -8.396975715683501e+00

    p25 =  3.790846746036765e-10
    p24 =  3.281964357650687e-09
    p23 =  1.962282433562894e-07
    p22 = -1.240239171056262e-06
    p21 =  1.739759082358234e-07
    p20 =  2.147264020369413e-05

    p35 =  1.840430200185075e-07
    p34 = -2.793849676757154e-05
    p33 =  1.735308193700643e-03
    p32 = -6.139315534216305e-02
    p31 =  1.255457892775006e+00
    p30 = -1.663993561652530e+01

    p40 =  4.579369142033410e-04

    if (uref >= 0.0 .and.  uref <= 6.5 ) then
      znotm = exp( p10 + p11*uref + p12*uref**2 + p13*uref**3)
    elseif (uref > 6.5 .and. uref <= 15.7) then
      znotm = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 15.7 .and. uref <= 53.0) then
      znotm = exp( p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30 )
    elseif ( uref > 53.0) then
      znotm = p40
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_m_v6

  SUBROUTINE znot_t_v6(uref,znott)
  IMPLICIT NONE
! Calculate scalar roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Ck-U10 relationship from COARE algorithm
! For high winds, try to retain the Ck-U10 relationship of FY2015 HWRF
!
! Bin Liu, NOAA/NCEP/EMC 2017
!
! uref(m/s)   :   wind speed at 10-m height
! znott(meter):   scalar roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znott

    REAL             :: p00
    REAL             :: p15, p14, p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p45, p44, p43, p42, p41, p40
    REAL             :: p56, p55, p54, p53, p52, p51, p50
    REAL             :: p60

    p00 =  1.100000000000000e-04

    p15 = -9.144581627678278e-10
    p14 =  7.020346616456421e-08
    p13 = -2.155602086883837e-06
    p12 =  3.333848806567684e-05
    p11 = -2.628501274963990e-04
    p10 =  8.634221567969181e-04

    p25 = -8.654513012535990e-12
    p24 =  1.232380050058077e-09
    p23 = -6.837922749505057e-08
    p22 =  1.871407733439947e-06
    p21 = -2.552246987137160e-05
    p20 =  1.428968311457630e-04

    p35 =  3.207515102100162e-12
    p34 = -2.945761895342535e-10
    p33 =  8.788972147364181e-09
    p32 = -3.814457439412957e-08
    p31 = -2.448983648874671e-06
    p30 =  3.436721779020359e-05

    p45 = -3.530687797132211e-11
    p44 =  3.939867958963747e-09
    p43 = -1.227668406985956e-08
    p42 = -1.367469811838390e-05
    p41 =  5.988240863928883e-04
    p40 = -7.746288511324971e-03

    p56 = -1.187982453329086e-13
    p55 =  4.801984186231693e-11
    p54 = -8.049200462388188e-09
    p53 =  7.169872601310186e-07
    p52 = -3.581694433758150e-05
    p51 =  9.503919224192534e-04
    p50 = -1.036679430885215e-02

    p60 =  4.751256171799112e-05

    if (uref >= 0.0 .and. uref < 5.9 ) then
      znott = p00
    elseif (uref >= 5.9 .and. uref <= 15.4) then
      znott = p15*uref**5 + p14*uref**4 + p13*uref**3 + p12*uref**2 + p11*uref + p10
    elseif (uref > 15.4 .and. uref <= 21.6) then
      znott = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 21.6 .and. uref <= 42.2) then
      znott = p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30
    elseif ( uref > 42.2 .and. uref <= 53.3) then
      znott = p45*uref**5 + p44*uref**4 + p43*uref**3 + p42*uref**2 + p41*uref + p40
    elseif ( uref > 53.3 .and. uref <= 80.0) then
      znott = p56*uref**6 + p55*uref**5 + p54*uref**4 + p53*uref**3 + p52*uref**2 + p51*uref + p50
    elseif ( uref > 80.0) then
      znott = p60
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_t_v6

  SUBROUTINE znot_m_v7(uref,znotm)
  IMPLICIT NONE
! Calculate areodynamical roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Cd-U10 relationship from COARE V3.5 (Edson et al. 2013)
! For high winds, try to fit available observational data
! Comparing to znot_t_v6, slightly decrease Cd for higher wind speed 
!
! Bin Liu, NOAA/NCEP/EMC 2018
! 
! uref(m/s)   :   wind speed at 10-m height
! znotm(meter):   areodynamical roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znotm
    REAL             :: p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p40

    p13 = -1.296521881682694e-02
    p12 =  2.855780863283819e-01
    p11 = -1.597898515251717e+00
    p10 = -8.396975715683501e+00

    p25 =  3.790846746036765e-10
    p24 =  3.281964357650687e-09
    p23 =  1.962282433562894e-07
    p22 = -1.240239171056262e-06
    p21 =  1.739759082358234e-07
    p20 =  2.147264020369413e-05

    p35 =  1.897534489606422e-07
    p34 = -3.019495980684978e-05
    p33 =  1.931392924987349e-03
    p32 = -6.797293095862357e-02
    p31 =  1.346757797103756e+00
    p30 = -1.707846930193362e+01

    p40 =  3.371427455376717e-04

    if (uref >= 0.0 .and.  uref <= 6.5 ) then
      znotm = exp( p10 + p11*uref + p12*uref**2 + p13*uref**3)
    elseif (uref > 6.5 .and. uref <= 15.7) then
      znotm = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 15.7 .and. uref <= 53.0) then
      znotm = exp( p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30 )
    elseif ( uref > 53.0) then
      znotm = p40
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_m_v7

  SUBROUTINE znot_t_v7(uref,znott)
  IMPLICIT NONE
! Calculate scalar roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Ck-U10 relationship from COARE algorithm
! For high winds, try to retain the Ck-U10 relationship of FY2015 HWRF
! To be compatible with the slightly decreased Cd for higher wind speed
!
! Bin Liu, NOAA/NCEP/EMC 2018
!
! uref(m/s)   :   wind speed at 10-m height
! znott(meter):   scalar roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znott

    REAL             :: p00
    REAL             :: p15, p14, p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p45, p44, p43, p42, p41, p40
    REAL             :: p56, p55, p54, p53, p52, p51, p50
    REAL             :: p60

    p00 =  1.100000000000000e-04

    p15 = -9.193764479895316e-10
    p14 =  7.052217518653943e-08
    p13 = -2.163419217747114e-06
    p12 =  3.342963077911962e-05
    p11 = -2.633566691328004e-04
    p10 =  8.644979973037803e-04
    
    p25 = -9.402722450219142e-12
    p24 =  1.325396583616614e-09
    p23 = -7.299148051141852e-08
    p22 =  1.982901461144764e-06
    p21 = -2.680293455916390e-05
    p20 =  1.484341646128200e-04
    
    p35 =  7.921446674311864e-12
    p34 = -1.019028029546602e-09
    p33 =  5.251986927351103e-08
    p32 = -1.337841892062716e-06
    p31 =  1.659454106237737e-05
    p30 = -7.558911792344770e-05
    
    p45 = -2.694370426850801e-10
    p44 =  5.817362913967911e-08
    p43 = -5.000813324746342e-06
    p42 =  2.143803523428029e-04
    p41 = -4.588070983722060e-03
    p40 =  3.924356617245624e-02
    
    p56 = -1.663918773476178e-13
    p55 =  6.724854483077447e-11
    p54 = -1.127030176632823e-08
    p53 =  1.003683177025925e-06
    p52 = -5.012618091180904e-05
    p51 =  1.329762020689302e-03
    p50 = -1.450062148367566e-02
    
    p60 =  6.840803042788488e-05

    if (uref >= 0.0 .and. uref < 5.9 ) then
      znott = p00
    elseif (uref >= 5.9 .and. uref <= 15.4) then
      znott = p15*uref**5 + p14*uref**4 + p13*uref**3 + p12*uref**2 + p11*uref + p10
    elseif (uref > 15.4 .and. uref <= 21.6) then
      znott = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 21.6 .and. uref <= 42.6) then
      znott = p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30
    elseif ( uref > 42.6 .and. uref <= 53.0) then
      znott = p45*uref**5 + p44*uref**4 + p43*uref**3 + p42*uref**2 + p41*uref + p40
    elseif ( uref > 53.0 .and. uref <= 80.0) then
      znott = p56*uref**6 + p55*uref**5 + p54*uref**4 + p53*uref**3 + p52*uref**2 + p51*uref + p50
    elseif ( uref > 80.0) then
      znott = p60
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_t_v7

  SUBROUTINE znot_m_v8(uref,znotm)
  IMPLICIT NONE
! Calculate areodynamical roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Cd-U10 relationship from COARE V3.5 (Edson et al. 2013)
! For high winds, try to fit available observational data
! Comparing to znot_t_v6, slightly decrease Cd for higher wind speed 
! And this is another variation similar to v7
!
! Bin Liu, NOAA/NCEP/EMC 2018
! 
! uref(m/s)   :   wind speed at 10-m height
! znotm(meter):   areodynamical roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znotm
    REAL             :: p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p40

    p13 = -1.296521881682694e-02
    p12 =  2.855780863283819e-01
    p11 = -1.597898515251717e+00
    p10 = -8.396975715683501e+00

    p25 =  3.790846746036765e-10
    p24 =  3.281964357650687e-09
    p23 =  1.962282433562894e-07
    p22 = -1.240239171056262e-06
    p21 =  1.739759082358234e-07
    p20 =  2.147264020369413e-05

    p35 =  1.897534489606422e-07
    p34 = -3.019495980684978e-05
    p33 =  1.931392924987349e-03
    p32 = -6.797293095862357e-02
    p31 =  1.346757797103756e+00
    p30 = -1.707846930193362e+01

    p40 =  3.886804744928044e-04

    if (uref >= 0.0 .and.  uref <= 6.5 ) then
      znotm = exp( p10 + p11*uref + p12*uref**2 + p13*uref**3)
    elseif (uref > 6.5 .and. uref <= 15.7) then
      znotm = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 15.7 .and. uref <= 51.5) then
      znotm = exp( p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30 )
    elseif ( uref > 51.5) then
      znotm = p40
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_m_v8

  SUBROUTINE znot_t_v8(uref,znott)
  IMPLICIT NONE
! Calculate scalar roughness over water with input 10-m wind
! For low-to-moderate winds, try to match the Ck-U10 relationship from COARE algorithm
! For high winds, try to retain the Ck-U10 relationship of FY2015 HWRF
! To be compatible with the slightly decreased Cd for higher wind speed
! And this is another variation similar to v7
!
! Bin Liu, NOAA/NCEP/EMC 2018
!
! uref(m/s)   :   wind speed at 10-m height
! znott(meter):   scalar roughness scale over water
!

    REAL, INTENT(IN) :: uref
    REAL, INTENT(OUT):: znott

    REAL             :: p00
    REAL             :: p15, p14, p13, p12, p11, p10
    REAL             :: p25, p24, p23, p22, p21, p20
    REAL             :: p35, p34, p33, p32, p31, p30
    REAL             :: p45, p44, p43, p42, p41, p40
    REAL             :: p56, p55, p54, p53, p52, p51, p50
    REAL             :: p60

    p00 =  1.100000000000000e-04

    p15 = -9.193764479895316e-10
    p14 =  7.052217518653943e-08
    p13 = -2.163419217747114e-06
    p12 =  3.342963077911962e-05
    p11 = -2.633566691328004e-04
    p10 =  8.644979973037803e-04
    
    p25 = -9.402722450219142e-12
    p24 =  1.325396583616614e-09
    p23 = -7.299148051141852e-08
    p22 =  1.982901461144764e-06
    p21 = -2.680293455916390e-05
    p20 =  1.484341646128200e-04
    
    p35 =  7.921446674311864e-12
    p34 = -1.019028029546602e-09
    p33 =  5.251986927351103e-08
    p32 = -1.337841892062716e-06
    p31 =  1.659454106237737e-05
    p30 = -7.558911792344770e-05

    p45 = -2.706461188613193e-10
    p44 =  5.845859022891930e-08
    p43 = -5.027577045502003e-06
    p42 =  2.156326523752734e-04
    p41 = -4.617267288861201e-03
    p40 =  3.951492707214883e-02
    
    p56 = -1.112896580069263e-13
    p55 =  4.450334755105140e-11
    p54 = -7.375373918500171e-09
    p53 =  6.493685149526543e-07
    p52 = -3.206421106713471e-05
    p51 =  8.407596231678149e-04
    p50 = -9.027924333673693e-03
    
    p60 =  5.791179079892191e-05

    if (uref >= 0.0 .and. uref < 5.9 ) then
      znott = p00
    elseif (uref >= 5.9 .and. uref <= 15.4) then
      znott = p15*uref**5 + p14*uref**4 + p13*uref**3 + p12*uref**2 + p11*uref + p10
    elseif (uref > 15.4 .and. uref <= 21.6) then
      znott = p25*uref**5 + p24*uref**4 + p23*uref**3 + p22*uref**2 + p21*uref + p20
    elseif (uref > 21.6 .and. uref <= 42.6) then
      znott = p35*uref**5 + p34*uref**4 + p33*uref**3 + p32*uref**2 + p31*uref + p30
    elseif ( uref > 42.6 .and. uref <= 51.5) then
      znott = p45*uref**5 + p44*uref**4 + p43*uref**3 + p42*uref**2 + p41*uref + p40
    elseif ( uref > 51.5 .and. uref <= 80.0) then
      znott = p56*uref**6 + p55*uref**5 + p54*uref**4 + p53*uref**3 + p52*uref**2 + p51*uref + p50
    elseif ( uref > 80.0) then
      znott = p60
    else
      print*, 'Wrong input uref value:',uref
    endif

  END SUBROUTINE znot_t_v8

   SUBROUTINE znot_wind10m(w10m,znott,znotm,icoef_sf)
   IMPLICIT NONE

! w10m(m/s)   :   10-m wind speed
! znott(meter):   Roughness scale for temperature/moisture, zt
! znotm(meter):   Roughness scale for momentum, z0
! Author      :  Weiguo Wang on 02/24/2016
!            convert from icoef=0,1,2 to have 10m level cd, ch match obs
     REAL, INTENT(IN) :: w10m
     INTEGER, INTENT(IN) :: icoef_sf
     REAL, INTENT(OUT):: znott, znotm

     real :: zm,zt,windmks, zlev,z10, tmp, zlevt, aaa, zm1,zt1
        zlev=20.0
        zlevt=10.0
        z10=10.0
            windmks=w10m
            if (windmks > 85.0) windmks=85.0
            if (windmks < 1.0) windmks=1.0
            if ( icoef_sf .EQ. 1) then
              call  znot_m_v1(windmks,zm1)
              call  znot_t_v1(windmks,zt1)

            else if ( icoef_sf .EQ. 0 ) then
              call  znot_m_v0(windmks,zm1)
              call  znot_t_v0(windmks,zt1)

            else  if( icoef_sf .EQ. 2 ) then
              call  znot_m_v1(windmks,zm1)
              call  znot_t_v2(windmks,zt1)

            else  if( icoef_sf .EQ. 3 ) then
              call  znot_m_v1(windmks,zm)
              call  znot_t_v2(windmks,zt)
!! adjust a little to match obs at 10m, cd is reduced
            tmp=0.4*0.4/(alog(zlev/zm))**2   ! cd at zlev
            zm1=z10/exp( sqrt(0.4*0.4/(tmp*0.95-0.0002)) ) 
!ch
            tmp=0.4*0.4/(alog(zlevt/zm)*alog(zlevt/zt))  ! ch at zlev using old formula
            zt1=z10/exp( 0.4*0.4/( 0.95*tmp*alog(z10/zm1) )  )

            else if( icoef_sf .EQ. 4 ) then

              call  znot_m_v1(windmks,zm)
              call  znot_t_v2(windmks,zt)
!!  for wind<20, cd similar to icoef=2 at 10m, then reduced 
             tmp=0.4*0.4/(alog(10.0/zm))**2   ! cd at zlev
             aaa=0.75
            if (windmks < 20) then
              aaa=0.99
            elseif(windmks < 45.0) then
              aaa=0.99+(windmks-20)*(0.75-0.99)/(45.0-20.0)
            endif
            zm1=z10/exp( sqrt(0.4*0.4/(tmp*aaa)) )  
!ch
          tmp=0.4*0.4/(alog(zlevt/zm)*alog(zlevt/zt))  ! ch at zlev using old formula
            zt1=z10/exp( 0.4*0.4/( 0.95*tmp*alog(z10/zm1) )  )

            else if( icoef_sf .EQ. 5 ) then

              call  znot_m_v1(windmks,zm)
              call  znot_t_v2(windmks,zt)
!!  for wind<20, cd similar to icoef=2 at 10m, then reduced
             tmp=0.4*0.4/(alog(10.0/zm))**2   ! cd at zlev
             aaa=0.80
            if (windmks < 20) then
              aaa=1.0
            elseif(windmks < 45.0) then
              aaa=1.0+(windmks-20)*(0.80-1.0)/(45.0-20.0)
            endif
            zm1=z10/exp( sqrt(0.4*0.4/(tmp*aaa)) )
!ch
          tmp=0.4*0.4/(alog(zlevt/zm)*alog(zlevt/zt))  ! ch at zlev using old formula
            zt1=z10/exp( 0.4*0.4/( 1.0*tmp*alog(z10/zm1) )  )

            else  if( icoef_sf .EQ. 6 ) then
              call  znot_m_v6(windmks,zm1)
              call  znot_t_v6(windmks,zt1)
            else  if( icoef_sf .EQ. 7 ) then
              call  znot_m_v7(windmks,zm1)
              call  znot_t_v7(windmks,zt1)
            else  if( icoef_sf .EQ. 8 ) then
              call  znot_m_v8(windmks,zm1)
              call  znot_t_v8(windmks,zt1)
           else
             write(0,*)'stop, icoef_sf must be one of 0,1,2,3,4,5,6,7,8'
             stop
          endif
          znott=zt1
          znotm=zm1

  end subroutine znot_wind10m

END MODULE module_sf_exchcoef

