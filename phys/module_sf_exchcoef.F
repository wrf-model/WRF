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


END MODULE module_sf_exchcoef

