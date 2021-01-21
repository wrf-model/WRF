

MODULE module_sf_oml

CONTAINS


   SUBROUTINE OML1D(I,J,TML,T0ML,H,H0,HUML,                              &
                    HVML,TSK,HFX,                                        &
                    LH,GSW,GLW,TMOML,                                    &
                    UAIR,VAIR,UST,F,EMISS,STBOLT,G,DT,OML_GAMMA,         &
                    OML_RELAXATION_TIME,                                 &
                    ids,ide, jds,jde, kds,kde,                           &
                    ims,ime, jms,jme, kms,kme,                           &
                    its,ite, jts,jte, kts,kte                            )


   IMPLICIT NONE































   INTEGER, INTENT(IN   )    ::      I, J
   INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte

   REAL,    INTENT(INOUT)    :: TML, H, HUML, HVML, TSK

   REAL,    INTENT(IN   )    :: T0ML, H0, HFX, LH, GSW, GLW,        &
                                UAIR, VAIR, UST, F, EMISS, TMOML

   REAL,    INTENT(IN) :: STBOLT, G, DT, OML_GAMMA, OML_RELAXATION_TIME


   REAL :: rhoair, rhowater, Gam, alp, BV2, A1, A2, B2, u, v, wspd, &
           hu1, hv1, hu2, hv2, taux, tauy, tauxair, tauyair, q, hold, &
           hsqrd, thp, cwater, ust2
   CHARACTER(LEN=120) :: time_series

      hu1=huml
      hv1=hvml
      rhoair=1.
      rhowater=1000.
      cwater=4200.

      Gam=oml_gamma







      alp=max((tml-273.15)*1.e-5, 1.e-6)
      BV2=alp*g*Gam
      thp=t0ml-Gam*(h-h0)
      A1=(tml-thp)*h - 0.5*Gam*h*h
      if(h.ne.0.)then
        u=hu1/h
        v=hv1/h
      else
        u=0.
        v=0.
      endif



        q=(-hfx-lh+gsw+glw*emiss-stbolt*emiss*tml*tml*tml*tml)/(rhowater*cwater)

        wspd=sqrt(uair*uair+vair*vair)
        if (wspd .lt. 1.e-10 ) then

           wspd = 1.e-10
        endif



        ust2=0.5*ust
        tauxair=ust2*ust2*uair/wspd
        taux=rhoair/rhowater*tauxair
        tauyair=ust2*ust2*vair/wspd
        tauy=rhoair/rhowater*tauyair

        hu2=hu1+dt*( f*hv1 + taux)
        hv2=hv1+dt*(-f*hu2 + tauy)

        A2=A1+q*dt

        huml=hu2
        hvml=hv2

        hold=h
        B2=hu2*hu2+hv2*hv2
        hsqrd=-A2/Gam + sqrt(A2*A2/(Gam*Gam) + 2.*B2/BV2)
        h=sqrt(max(hsqrd,0.0))

        if(h.lt.hold)h=hold


        if(tml.ge.tmoml .and. h.ne.0.)then


          if(tml.ge.tmoml)then
            tml=max(t0ml - Gam*(h-h0) + 0.5*Gam*h + A2/h, tmoml)
          else 
            tml=tmoml
          endif
          u=hu2/h
          v=hv2/h
        else
          tml=t0ml
          u=0.
          v=0.
        endif



        if (oml_relaxation_time .gt. 0.) then
          tml = tml - (tml-t0ml)*dt/oml_relaxation_time
          h = h - (h-h0)*dt/oml_relaxation_time
          huml = huml - huml*dt/oml_relaxation_time
          hvml = hvml - hvml*dt/oml_relaxation_time
        end if

        tsk=tml











   END SUBROUTINE OML1D


   SUBROUTINE omlinit(oml_hml0, tsk,                           &
                      tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                      allowed_to_read, start_of_simulation,    &
                      ids,ide, jds,jde, kds,kde,               &
                      ims,ime, jms,jme, kms,kme,               &
                      its,ite, jts,jte, kts,kte                )

   IMPLICIT NONE

   LOGICAL , INTENT(IN)      ::      allowed_to_read
   LOGICAL , INTENT(IN)      ::      start_of_simulation
   INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN)    ::                               TSK

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::     TML, T0ML, HML, H0ML, HUML, HVML, TMOML
   REAL   , INTENT(IN   )    ::     oml_hml0



   INTEGER                   ::      L,J,I,itf,jtf
   CHARACTER*1024 message


 
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(start_of_simulation) THEN
     DO J=jts,jtf
     DO I=its,itf
       TML(I,J)=TSK(I,J)
       T0ML(I,J)=TSK(I,J)
     ENDDO
     ENDDO
     IF (oml_hml0 .gt. 0.) THEN
        WRITE(message,*)'Initializing OML with HML0 = ', oml_hml0
        CALL wrf_debug (0, TRIM(message))
        DO J=jts,jtf
        DO I=its,itf
          HML(I,J)=oml_hml0
          H0ML(I,J)=HML(I,J)
          HUML(I,J)=0.
          HVML(I,J)=0.
          TMOML(I,J)=TSK(I,J)-5.
        ENDDO
        ENDDO
     ELSE IF (oml_hml0 .eq. 0.) THEN
        WRITE(message,*)'Initializing OML with climatological mixed layer depth'
        CALL wrf_debug (0, TRIM(message))
        DO J=jts,jtf
        DO I=its,itf
          HML(I,J)=H0ML(I,J)
          HUML(I,J)=0.
          HVML(I,J)=0.
          TMOML(I,J)=TSK(I,J)-5.
        ENDDO
        ENDDO
     ELSE
        WRITE(message,*)'Initializing OML with 2d HML0'
        CALL wrf_debug (0, TRIM(message))
        DO J=jts,jtf
        DO I=its,itf
          HML(I,J)=H0ML(I,J)

          IF(TMOML(I,J).GT.200. .and. TMOML(I,J).LE.201.) TMOML(I,J)=TSK(I,J)
        ENDDO
        ENDDO
     ENDIF
   ENDIF

   END SUBROUTINE omlinit

END MODULE module_sf_oml
