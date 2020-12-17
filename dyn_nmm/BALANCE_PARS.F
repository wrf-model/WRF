!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  parameters used in the triply-nested movable mesh tropical
!  cyclone model with sigma=p/ps as the vertical coordinate.
!=============================================================
!
!-------------------------------------------------------------
! nn---- the total hourly TC positions, Pmin, Vmax;
! kmx---- model levels in vertical
!--------------------------------------------------------------
      integer nn,kmx,kmx1
      parameter(nn=1241,kmx=26,kmx1=kmx+1)
!---------------------------------------------------------------
! pse --- the base surface pressure (Pa)
! fi0 --- standard latitude of the beta or f-plane.
!---------------------------------------------------------------
      real pse,fi0
      parameter(pse=101000.,fi0=12.5)
!---------------------------------------------------------------
! grv --- acceleration of gravity
! cp ---- specific heat of dry air at constant pressure
! cv ---- specific heat of dry air at constant volume
! rgas -- gas constant for dry air
! rcp --- rgas/cp
!--------------------------------------------------------------
      real grv,cp,rgas,rcp
      parameter(grv=9.8066,cp=1004.6,rgas=287.05,rcp=rgas/cp)
      real gocp,rogrv,gor
      parameter(gocp=grv/cp,rogrv=rgas/grv,gor=grv/rgas)
      real cv,cpocv,gocv,rocv
      parameter(cv=717.0,cpocv=cp/cv,gocv=grv/cv,rocv=rgas/cv)
!--------------------------------------------------------------
! al ---- Latent heat of condensation
! rgv --- gas constant for water vapor
!---------------------------------------------------------------
      real al,rgv
      parameter(al=2.5e6,rgv=461.5)
!---------------------------------------------------------------
! three constants in calculating the saturated WV mixing ratio
! respect to water
!---------------------------------------------------------------
      real cs2,cs3,cs4
      parameter(cs2=17.67,cs3=273.15,cs4=29.65)
!---------------------------------------------------------------
!  Ends the parameters set
!++++++++++++++++++++++++++++
