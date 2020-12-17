!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_oml

CONTAINS

!----------------------------------------------------------------
   SUBROUTINE OML1D(I,J,TML,T0ML,H,H0,HUML,                              &
                    HVML,TSK,HFX,                                        &
                    LH,GSW,GLW,TMOML,                                    &
                    UAIR,VAIR,UST,F,EMISS,STBOLT,G,DT,OML_GAMMA,         &
                    OML_RELAXATION_TIME,                                 &
                    ids,ide, jds,jde, kds,kde,                           &
                    ims,ime, jms,jme, kms,kme,                           &
                    its,ite, jts,jte, kts,kte                            )

!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
!
!  SUBROUTINE OCEANML CALCULATES THE SEA SURFACE TEMPERATURE (TSK) 
!  FROM A SIMPLE OCEAN MIXED LAYER MODEL BASED ON 
!  (Pollard, Rhines and Thompson (1973).
!
!-- TML         ocean mixed layer temperature (K)
!-- T0ML        ocean mixed layer temperature (K) at initial time
!-- TMOML       top 200 m ocean mean temperature (K) at initial time
!-- H           ocean mixed layer depth (m)
!-- H0          ocean mixed layer depth (m) at initial time
!-- HUML        ocean mixed layer u component of wind
!-- HVML        ocean mixed layer v component of wind
!-- OML_GAMMA   deep water lapse rate (K m-1)
!-- SF_OCEAN_PHYSICS     whether to call oml model
!-- UAIR,VAIR   lowest model level wind component
!-- UST         frictional velocity
!-- HFX         upward heat flux at the surface (W/m^2)
!-- LH          latent heat flux at the surface (W/m^2)
!-- TSK         surface temperature (K)
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- EMISS       emissivity of the surface
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- F           Coriolis parameter
!-- DT          time step (second)
!-- G           acceleration due to gravity
!-- OML_RELAXATION_TIME  time scale (s) to relax TML to T0ML, H to H0,
!                        HUML and HVML to 0; value <=0 means no relaxation
!
!----------------------------------------------------------------
   INTEGER, INTENT(IN   )    ::      I, J
   INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte

   REAL,    INTENT(INOUT)    :: TML, H, HUML, HVML, TSK

   REAL,    INTENT(IN   )    :: T0ML, H0, HFX, LH, GSW, GLW,        &
                                UAIR, VAIR, UST, F, EMISS, TMOML

   REAL,    INTENT(IN) :: STBOLT, G, DT, OML_GAMMA, OML_RELAXATION_TIME

! Local
   REAL :: rhoair, rhowater, Gam, alp, BV2, A1, A2, B2, u, v, wspd, &
           hu1, hv1, hu2, hv2, taux, tauy, tauxair, tauyair, q, hold, &
           hsqrd, thp, cwater, ust2
   CHARACTER(LEN=120) :: time_series

      hu1=huml
      hv1=hvml
      rhoair=1.
      rhowater=1000.
      cwater=4200.
! Deep ocean lapse rate (K/m) - from Rich
      Gam=oml_gamma
!     if(i.eq.1 .and. j.eq.1 .or. i.eq.105.and.j.eq.105) print *, 'gamma = ', gam
!     Gam=0.14
!     Gam=5.6/40.
!     Gam=5./100.
! Thermal expansion coeff (/K)
!     alp=.0002
!     temp dependence (/K)
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

!  time step

        q=(-hfx-lh+gsw+glw*emiss-stbolt*emiss*tml*tml*tml*tml)/(rhowater*cwater)
!       wspd=max(sqrt(uair*uair+vair*vair),0.1)
        wspd=sqrt(uair*uair+vair*vair)
        if (wspd .lt. 1.e-10 ) then
!          print *, 'i,j,wspd are ', i,j,wspd
           wspd = 1.e-10
        endif
! limit ust to 1.6 to give a value of ust for water of 0.05
!       ust2=min(ust, 1.6)
! new limit for ust: reduce atmospheric ust by half for ocean
        ust2=0.5*ust
        tauxair=ust2*ust2*uair/wspd
        taux=rhoair/rhowater*tauxair
        tauyair=ust2*ust2*vair/wspd
        tauy=rhoair/rhowater*tauyair
! note: forward-backward coriolis force for effective time-centering
        hu2=hu1+dt*( f*hv1 + taux)
        hv2=hv1+dt*(-f*hu2 + tauy)
! consider the flux effect
        A2=A1+q*dt

        huml=hu2
        hvml=hv2

        hold=h
        B2=hu2*hu2+hv2*hv2
        hsqrd=-A2/Gam + sqrt(A2*A2/(Gam*Gam) + 2.*B2/BV2)
        h=sqrt(max(hsqrd,0.0))
! limit to positive h change
        if(h.lt.hold)h=hold

! no change unless tml is warmer than layer mean temp tmol or tsk-5 (see omlinit)
        if(tml.ge.tmoml .and. h.ne.0.)then

! no change unless tml is warmer than layer mean temp tmoml or tsk-5 (see omlinit)
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

! relax TML T0ML and H to H0, HUML and HVML to 0

        if (oml_relaxation_time .gt. 0.) then
          tml = tml - (tml-t0ml)*dt/oml_relaxation_time
          h = h - (h-h0)*dt/oml_relaxation_time
          huml = huml - huml*dt/oml_relaxation_time
          hvml = hvml - hvml*dt/oml_relaxation_time
        end if

        tsk=tml
!        if(h.gt.100.)print *,i,j,h,tml,' h,tml'

! ww: output point data
!     if( (i.eq.190 .and. j.eq.115) .or. (i.eq.170 .and. j.eq.125) ) then
!        write(jtime,fmt='("TS ",f10.0)') float(itimestep)
!        CALL wrf_message ( TRIM(jtime) )
!        write(time_series,fmt='("OML",2I4,2F9.5,2F8.2,2E15.5,F8.3)') &
!              i,j,u,v,tml,h,taux,tauy,a2
!        CALL wrf_message ( TRIM(time_series) )
!     end if

   END SUBROUTINE OML1D

!================================================================
   SUBROUTINE omlinit(oml_hml0, tsk,                           &
                      tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                      allowed_to_read, start_of_simulation,    &
                      ids,ide, jds,jde, kds,kde,               &
                      ims,ime, jms,jme, kms,kme,               &
                      its,ite, jts,jte, kts,kte                )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
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

!  LOCAR VAR

   INTEGER                   ::      L,J,I,itf,jtf
   CHARACTER*1024 message

!----------------------------------------------------------------
 
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
! fill in near coast area with SST: 200 K was set as missing value in ocean pre-processing code
          IF(TMOML(I,J).GT.200. .and. TMOML(I,J).LE.201.) TMOML(I,J)=TSK(I,J)
        ENDDO
        ENDDO
     ENDIF
   ENDIF

   END SUBROUTINE omlinit

END MODULE module_sf_oml
