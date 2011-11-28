!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_ra_hs

CONTAINS

!------------------------------------------------------------------
   SUBROUTINE HSRAD(RTHRATEN,p8w,p_phy,pi_phy,dz8w,t_phy,          &
                     t8w, rho_phy, R_d,G,CP,dt,xlat,degrad,        &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   INTEGER,    INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte  

   REAL, INTENT(IN    )      ::        DEGRAD

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN

   REAL, INTENT(IN   )   ::                   R_d,CP,G,dt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                                     dz8w, &
                                                             p8w, &
                                                           p_phy, &
                                                          pi_phy, &
                                                           t_phy, &
                                                             t8w, &
                                                         rho_phy  
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN ) ::                                     xlat

   INTEGER :: i,j,K,NK
   real :: delty,delthez,p0,sec_p_d,sigb,kka,kks,kkf,rcp
   real :: ttmp,teq,sig,sigterm,kkt,t_tend

!------------------------------------------------------------------
! Newtonian relaxation scheme from Held and Suarez, Bull. Amer. Met.
! Soc., Vol. 75, No. 10., p1825-1830, 1994.  (box on page 1826)
! CEN and MIR  31-JUL-04

   delty   = 60.0
   delthez = 10.0
   p0      = 100000.0
   sec_p_d = 86400.
   sigb    = 0.7
   kka     = 1.0/40.0   ! units of per day
   kks     = 0.25
   kkf     = 1.0
   rcp     = R_d/CP

   j_loop: DO J=jts,MIN(jte,jde-1)
   k_loop: DO K=kts,MIN(kte,kde-1)
   i_loop: DO I=its,MIN(ite,ide-1)

      ttmp = 315.0 - delty*(sin(xlat(i,j)*degrad))**2.0- &
               delthez*alog(p_phy(i,k,j)/p0)*(cos(xlat(i,j)*degrad))**2.0
               
      teq=max(200.0,ttmp*(p_phy(i,k,j)/p0)**rcp)

      sig=p_phy(i,k,j)/p8w(i,1,j)
      sigterm=max(0.0,(sig-sigb)/(1.0-sigb))

      kkt=kka+(kks-kka)*sigterm*(cos(xlat(i,j)*degrad))**4.0

      t_tend=-kkt*(t_phy(i,k,j)-teq)/sec_p_d  ! t_tend in kinetic K/s

      RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+t_tend/pi_phy(i,k,j)

   ENDDO i_loop
   ENDDO k_loop
   ENDDO j_loop                                          

   END SUBROUTINE HSRAD

!====================================================================
   SUBROUTINE hsinit(RTHRATEN,restart,                              &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           :: restart
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::        &
                                                          RTHRATEN
   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHRATEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE hsinit

!====================================================================

END MODULE module_ra_hs
