MODULE module_hetn2o5


!temporary:
! from dentener and crutzen for 5S, 160E on 100mb levels startin at 1000mb
    REAL, PARAMETER, DIMENSION(10) :: &
        kheti = (/4.2e-5, 3.8e-5, 1.e-5,3.e-6,4.e-6, 2.e-6,3.e-6,5.e-6,1.3e-5,1.8e-5/)

    !kheta:  on aerosol
   REAL, SAVE, DIMENSION(120) :: kheta

   LOGICAL :: do_aerosol=.TRUE.


! sticking coefficients for cloud water and cloud ice

   REAL    , PARAMETER, PRIVATE :: gammacldw = 0.05,   &
                                   gammacldi = 0.03  !cms check !
                           
   REAL     , PARAMETER, PRIVATE ::     rhograul = 400. ,&
                                        rhoice   = 917.                       

   REAL, PARAMETER, PRIVATE :: pi = 3.1415926


    ! D_g: diffusivity of species in gas phase in m^2/s      
       REAL, PARAMETER, PRIVATE ::  D_g = 0.1E-6 
                             !in Match this is 1.e-5 (Schwartz ?!)
     



    ! abar_c:  mass mean radius for cloud drops in m
       REAL, PARAMETER, PRIVATE :: abar_c = 10.E-6 



    ! RSTAR2 : universal gas constant in J/(kmol K)
       REAL, PARAMETER, PRIVATE  :: RSTAR2 = 8314.


    ! parameters from Lin scheme
       REAL    , PARAMETER, PRIVATE ::     xnor = 8.0e6
       REAL    , PARAMETER, PRIVATE ::     xnos = 3.0e6
       REAL    , PARAMETER, PRIVATE ::     xnog = 4.0e6



 CONTAINS
  SUBROUTINE hetn2o5calc(hetn2o5, rho, T, QC, QR, QI, QS, QG, &
                       rhowater, rhosnow, M_n2o5,             &
                       P_QI,P_QS,P_QG,                        &
                       ids,ide, jds,jde, kds,kde,             &
                       ims,ime, jms,jme, kms,kme,             & 
                       its,ite, jts,jte, kts,kte              )



      IMPLICIT NONE



  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , & 
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte , &
                                      P_QI,P_QS,P_QG


   REAL, DIMENSION( its:ite , kts:kte , jts:jte),        &
         INTENT(INOUT )                               :: hetn2o5



  REAL, INTENT(IN   ) ::                                rhosnow,  &
                                                        rhowater, &
                                                        M_n2o5


  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &
                                                              QC, &
                                                              QR, &
                                                              QI, &
                                                              QS, &
                                                              QG, &
                                                             rho, &
                                                               T


!local vars

    REAL :: orho, tmp1


  ! mass mean radii of the different hydrometeors
    REAL ::                &
                                 abar_r, abar_i, abar_s, abar_g


  ! nu_th: thermal velocity of species in m/s 
    REAL :: nu_th

    ! tau_Dg: gas diffusion timescale 
    ! tau_i : timescale for mass transfer across interface of hydrometeor
       REAL :: tau_i, tau_Dg


    !  L : water contents in cloud drops, rain drops, hail stones,...
    !  in (cm^3 H_2O / cm^3 air)
       REAL :: L


    ! loss rates:
       REAL :: kc, kr, ki, ks, kg
 

      INTEGER :: i,j,k


!----

   j_loop:  DO j = jts, jte
   i_loop:  DO i = its, ite

     DO k = kts, kte
           orho = 1./rho(i,k,j)



         kc = 0.
         kr = 0.
         ki = 0.
         ks = 0.
         kg = 0.



            !! abar_c


          IF(QR(i,k,j) .GT. 1.e-12) THEN
            tmp1=sqrt(pi*rhowater*xnor*orho/QR(i,k,j)) 
            !!xlambdar1Dr(k)=sqrt(tmp1)
            abar_r =  2./MAX(sqrt(tmp1), 1.E-8 ) !if abar is large, kt becomes small
          ENDIF


!!$           IF(QI(i,k,j) .GT. 1.e-8) THEN
!!$            tmp1=sqrt(pi*rhoice*xnoi*orho/QI(i,k,j))
!!$            !!xlambdar1Di(k)=sqrt(tmp1)
!!$            abar_i = 2./MAX(sqrt(tmp1), 1.E-8 )
!!$           ENDIF


             abar_i =  abar_c

           IF(QS(i,k,j) .GT. 1.e-12) THEN
            tmp1=sqrt(pi*rhosnow*xnos*orho/QS(i,k,j))
            !!xlambdar1Ds(k)=sqrt(tmp1)
             abar_s =  2./MAX(sqrt(tmp1), 1.E-8 )
           ENDIF



           IF(QG(i,k,j) .GT. 1.e-12) THEN
            tmp1=sqrt(pi*rhograul*xnog*orho/QG(i,k,j))
            !!xlambdar1Dg(k)=sqrt(tmp1)
            abar_g = 2./MAX(sqrt(tmp1), 1.E-8 )  
           ENDIF




! calculate thermal velocity of species
        nu_th = SQRT(8*RSTAR2*T(i,k,j)/(pi*M_n2o5))
       


! calculate timescales
  !cloud droplets
    IF(QC(i,k,j) .GT. 1.e-12) THEN
      tau_i = (4.*abar_c) / (3.* nu_th * gammacldw)
      tau_Dg = (abar_c**2) / (3.* D_g)

      L  = (rho(i,k,j) / rhowater) * QC(i,k,j)
      kc = L/(tau_i +  tau_Dg)
    ENDIF

  !rain
    IF(QR(i,k,j) .GT. 1.e-12) THEN
      tau_i = (4.*abar_r) / (3.* nu_th * gammacldw)
      tau_Dg = (abar_r**2) / (3.* D_g)

      L  = (rho(i,k,j) / rhowater) * QR(i,k,j)
      kr = L/(tau_i +  tau_Dg)
    ENDIF

  !cloud ice
    IF(QI(i,k,j) .GT. 1.e-12) THEN
      tau_i = (4.*abar_i) / (3.* nu_th * gammacldi)
      tau_Dg = (abar_i**2) / (3.* D_g)

      L  = (rho(i,k,j) / rhowater) * QI(i,k,j)
      ki = L/(tau_i +  tau_Dg)
    ENDIF

  !snow
     IF(QS(i,k,j).GT. 1.e-12) THEN
      tau_i = (4.*abar_s) / (3.* nu_th * gammacldi)
      tau_Dg = (abar_s**2) / (3.* D_g)

      L  = (rho(i,k,j) / rhowater) * QS(i,k,j)
      ks = L/(tau_i +  tau_Dg)
     ENDIF

  !graupel
     IF(QG(i,k,j).GT. 1.e-12) THEN
      tau_i = (4.*abar_g) / (3.* nu_th * gammacldi)
      tau_Dg = (abar_g**2) / (3.* D_g)

      L  = (rho(i,k,j) / rhowater) * QG(i,k,j)
      kg = L/(tau_i +  tau_Dg)
     ENDIF



!!HERE THE VENTILATION COEFF SHOULD BE INCLUDED AND IT PROBABLY DOES NOT HAPPEN ON ICE!!

!!    hetn2o5(i,k,j) = kc + kr + ki + ks + kg
     




     hetn2o5(i,k,j) =  kc + kr + kheta(k)  !!+ kg
     !hetn2o5(i,k,j) = 0. !n2o5_test


     ENDDO

   ENDDO i_loop
   ENDDO j_loop

  END SUBROUTINE hetn2o5calc





!-----------------------------------------------------------------

 SUBROUTINE hetn2o5_ini(      pb, pp, z,                     &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte)

#ifdef DM_PARALLEL
   USE module_dm
#endif

    IMPLICIT NONE


    INTEGER, INTENT(IN    )  :: ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte


    REAL, DIMENSION( ims:ime, kms:kme, jms:jme),  &
              INTENT(IN    )  ::                      z, pb, pp

     REAL, DIMENSION(10) :: pin !moguntia


  LOGICAL , EXTERNAL      :: wrf_dm_on_monitor


!local

     REAL, DIMENSION(kms:kme) :: zloc, ploc

      INTEGER :: k, kk, l_low, l_up
      REAL :: m, b, dp


#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
  IF (.NOT. do_aerosol) RETURN

!currently not really necessary 
dm_on_monitor: IF ( wrf_dm_on_monitor() ) THEN

  kheta(:) = 0.

 

   pin(kms)=1000.
 DO k=2,10
   pin(k)=pin(k-1)-100.
 ENDDO


 DO k=kms,kme

  zloc(k) = z(its,k,jts)
  ploc(k) = pb(its,k,jts) + pp(its,k,jts) 

   IF ( zloc(k) .GT. 1.e6 .OR. zloc(k) .LT. 0. ) THEN
      CALL wrf_error_fatal ("STOP: hetn2o5_ini")
   ENDIF

 ENDDO

 DO k=kms,kme
   IF ( ploc(k) .GT. 1.e5 ) THEN
      kheta(k) = kheti(1)
   ELSE

    l_low = 11.-CEILING(ploc(k)/1.e4)
    l_up=l_low+1


     dp=ploc(k)/100. - pin(l_up)
     m=(kheti(l_low)-kheti(l_up))/100.
     kheta(k)=kheti(l_up) +  m*dp

    

   !print *,l_low
   !print *, ploc(k), pin(l_low), pin(l_up)
   !print *,  "  ", kheti(l_low),kheti(l_up), kheta(k)



   END IF


 ENDDO




ENDIF dm_on_monitor

  DM_BCAST_MACRO( kheta )

   

 END SUBROUTINE hetn2o5_ini


END MODULE module_hetn2o5






