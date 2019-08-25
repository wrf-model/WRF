MODULE module_ltng_strokes
!https://journals.ametsoc.org/doi/10.1175/WAF-D-11-00144.1

! j_pos,j_neg,j_neu:Energy per flash; threshold values set in Registry.EM_COMMON
! Values can be modified through namelist.input
! Sample values used:  parameter (j_pos=5.E9,j_neg=1.E9,j_neu=1.E9)
! References for this and accompanying program module_calc_lpi.F
! Note, the calculation of lpi is different than in the published paper by Yair
! et al., JGR:
! https://scholar.google.co.il/citations?user=vzBSZmEAAAAJ&hl=en#d=gs_md_cita-d&u=%2Fcitations%3Fview_op%3Dview_citation%26hl%3Den%26user%3DvzBSZmEAAAAJ%26citation_for_view%3DvzBSZmEAAAAJ%3AULOm3_A8WrAC%26tzom%3D-120
! Lynn, B. H., Y. Yair, C. Price, G. Kelman, A. Clark,
! 2012: Predicting Cloud-to-Ground and Intracloud Lightning in Weather
!Forecast Models.  Wea. Forecast.  27, 1470-1486.
! Lynn, B. H., G. Kelman, and G. Ellrod, 2015: An Evaluation of the
! Efficacy of Using Observed Lightning to Improve Convective Lightning Forecasts.
! Wea. Forecasting, 30, 405–423. 
! Lynn, B.H, 2016: The Usefulness and Economic Value of Total Lightning
! Forecasts made with a "Dynamic" Lightning Scheme coupled with Lightning Data
! Assimilation. Wea. Forecasting. 32, 645 – 663.
! Contact: barry.lynn@weather-it-is.com or barry.h.lynn@gmail.com

CONTAINS
!===================================================================
!
  SUBROUTINE calcstroke(dx,dy &
                 ,w,dz8w,pi_phy,th_phy,p_phy,rho_phy                    &
                 ,light_pe &
                 ,light_ne &
                 ,light_neu &
                 ,lpos,lneg,lneu&
                 ,j_pos,j_neg,j_neu &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
  REAL, INTENT(IN):: j_pos,j_neg,j_neu
  real j_sa ! scale aware
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
 
       REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                      light_pe,&
                                                      light_ne, &
                                                      light_neu
      REAL,INTENT(IN):: dx,dy
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::  w
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN) ::                                         th_phy
!

!
       REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                             &
                                                                rho_phy, &
                                                                dz8w, &
                                                              pi_phy, &
                                                                   p_phy

      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)::      &
     &                      LPOS,LNEG,LNEU
!     REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)::      &
!    &                      LPI,LPOS,LNEG,LNEU,plpi,nlpi,neulpi        



      REAL, DIMENSION(kms:kme)::    tempk,rh,qtot,qitot
      REAL, DIMENSION(kms:kme):: qv1d,p1d,rho1d,qti1d
      REAL, DIMENSION(kms:kme):: temp,qc1d,ql1d,qi1d,qs1d,qg1d,lpi1d
      REAL, DIMENSION(0:kme):: w1d,height
      REAL, DIMENSION(kms:kme):: e1d,height_t,w1d_t
      REAL z_full
      INTEGER level
      INTEGER I_COLLAPSE
      LOGICAL LOOK_T
      real dxkm

  INTEGER ::               i,j,k
!-------------------------------------------------------------------

! NOTE: dxkm is a "scale-aware" term.
! The volume change from grid to grid depends on  the square of the area.
! The vertical velocity, though, will increase, for example, by deltax
! the ratio of  w to volume is deltax/deltax-sq or 1./delta_x
! 4 km is the reference value for deltax (based on paper published in 2012).
      dxkm = dx*1.E-3
      DO j = jts,jte
      DO i = its,ite
       z_full=0.
       height(0)=z_full
       do k=kts,kte
        z_full=z_full+dz8w(i,k,j)
        height(k)=z_full
        temp(k) = th_phy(i,k,j)*pi_phy(i,k,j)-273.16
        rho1d(k)=rho_phy(i,k,j)
       end do
! note: light_neu, etc, are multiplied by rho in calc_lpi.F, for converting from
! /kg to /m^3
       e1d(kme) = 0
       do k=kts,kte
        e1d(k)=light_neu(i,k,j)
       end do
!      j_neu=1.E9
       j_sa=j_neu*dxkm/4.
       call flash(height,temp,e1d,j_sa,lneu(i,j),kme,kte)
       do k=kts,kte
        if (rho1d(k).le.1.E-10)rho1d(k) = 100000.
        light_neu(i,k,j) = e1d(k)/rho1d(k)
       end do
       light_neu(i,kme,j) = 0
       e1d(kme) = 0
       do k = kts,kte
        e1d(k)=light_ne(i,k,j)
       end do
       j_sa=j_neg*dxkm/4.
       call flash(height,temp,e1d,j_sa,lneg(i,j),kme,kte)
       do k = kts,kte
        if (rho1d(k).le.1.E-10)rho1d(k) = 100000.
        light_ne(i,k,j) = e1d(k)/rho1d(k)
       end do
       light_ne(i,kme,j) = 0
!      j_pos=5.E9
       j_sa=j_pos*dxkm/4.
       e1d(kme) = 0
       do k = kts,kte
       e1d(k)=light_pe(i,k,j)
       end do 
       call flash(height,temp,e1d,j_sa,lpos(i,j),kme,kte)
       do k = kts,kte
        if (rho1d(k).le.1.E-10)rho1d(k) = 100000.
          light_pe(i,k,j)=e1d(k)/rho1d(k)
       end do
       light_pe(i,kme,j) = 0
      END DO
      END DO


      return
      end subroutine calcstroke
      subroutine &
     & flash(height,t3d,pe1d,j_pos,lpos,nk,nke)
      implicit none
      integer nk,nke
      real lpos,j_pos
      real pe1d(nk)
      integer i,j,k
      integer num_ran
      parameter (num_ran=2)
      real ran_num(num_ran)
      real    i_seed
      real t3d(nk)
      real height(0:nk)
      real del_z(nk)
      real top, bot
      real n,ave_z,del_z_tot
      real residual,e_p,e_n,e_neu,loss,e_min
      del_z_tot=0
      do k=1,nke-1
        top=0.5*(height(k+1)+height(k))
        bot=0.5*(height(k)+height(k-1))
        del_z(k)=top-bot
      end do
!     plpi=0
      e_p=0
      do k=1,nke
       e_p=pe1d(k)+e_p
      end do
! FLASHES AND LOSSES
! POSITIVE
      pe1d(1)=0
      pe1d(nk)=0
      pe1d(nke)=0
      do k=2,nke-1
        del_z_tot=del_z_tot+del_z(k)
      end do
       if (e_p.ge.j_pos)then
        lpos=lpos+int(e_p/j_pos)
        loss = int(e_p/j_pos)
        residual=e_p-j_pos*loss
        do k=2,nke-1
          e_min=min(pe1d(k),e_p)
          pe1d(k)=pe1d(k)-e_min+residual*del_z(k)/del_z_tot
        end do
       end if
      return
      end subroutine flash
END MODULE  module_ltng_strokes
