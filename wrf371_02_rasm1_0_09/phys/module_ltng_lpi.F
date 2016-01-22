MODULE module_ltng_lpi
!Yair, Y., B. Lynn, C. Price, V. Kotroni, K. Lagouvardos, E. Morin,
!A. Magnai, and M. del Carmen Llasat (2010), Predicting the potential for
!lightning activity in Mediterranean storms based on the Weather
!Research and Forecasting (WRF) model dynamic and microphysical
!fields, J. Geophys. Res., 115, D04205, doi:10.1029/2008JD010868.
! However, we don't check for collapsing cell (so as not to require use of halo).
! This means that lpi is also calculated in cells that are no longer (on average) growing
! For a "complete" lightning forecast scheme, please see:
!http://journals.ametsoc.org/doi/abs/10.1175/WAF-D-11-00144.1
!(Predicting Cloud-to-Ground and Intracloud Lightning in Weather Forecast Models)

CONTAINS
!===================================================================
!
  SUBROUTINE calclpi(qv,qc, qr, qi, qs, qg, qh                            &
                 ,w,z,dz8w,pi_phy,th_phy,p_phy,rho_phy                    &
                 ,lpi&
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN) ::                                          &
                                                              qv, &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg,qh

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::  w, z
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,pi_phy,p_phy,rho_phy
      REAL, INTENT(IN),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)::      &
     &                      LPI




      REAL, DIMENSION(kms:kme)::    tempk,rh
      REAL, DIMENSION(kms:kme):: qv1d,p1d,rho1d,qti1d
      REAL, DIMENSION(kms:kme):: temp,qc1d,ql1d,qi1d,qs1d,qg1d,lpi1d
      REAL, DIMENSION(0:kme):: w1d,height
      REAL, DIMENSION(kms:kme):: e1d,height_t,w1d_t
      REAL z_full,qrs,teten,RELHUM,LOC,Td_850,Td_700,PC_DWPT
      INTEGER level
      REAL :: dt_lpi,t_base,t_top
      INTEGER I_COLLAPSE
      LOGICAL LOOK_T
      INTEGER I_START,I_END,J_START,J_END


  INTEGER ::               i,j,k
!-------------------------------------------------------------------
      DO j = jts,jte
      DO i = its,ite
        z_full=0.
        height(0)=z_full
        w1d(0)=w(i,1,j)
      DO k = kts,kte-1
          if (k.lt.kte-1)then
           w1d(k)=w(i,k+1,j)
          else
           w1d(k)=0.
          end if
          temp(k) = th_phy(i,k,j)*pi_phy(i,k,j)-273.16
          tempk(k) = th_phy(i,k,j)*pi_phy(i,k,j)
          qv1d(k)=qv(i,k,j)
          p1d(k)=p_phy(i,k,j)
          rho1d(k)=rho_phy(i,k,j)
          z_full=z_full+dz8w(i,k,j)
          height(k)=z_full
          qc1d(k)=qc(i,k,j)
          ql1d(k)=qc(i,k,j)+qr(i,k,j)
          qi1d(k)=qi(i,k,j)
          qti1d(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)+qh(i,k,j)
          qs1d(k)=qs(i,k,j)
!         qg1d(k)=qg(i,k,j)+qh(i,k,j)
! Hail doesn't usually charge
          qg1d(k)=qg(i,k,j)
! For conservative advection multiply by rho1d and divide by it below
      ENDDO
      do k = kts,kte-1
       height_t(k)=0.5*(height(k-1)+height(k))
       w1d_t(k)=0.5*(w1d(k-1)+w1d(k))
      end do
      t_base=-0
      t_top=-20
      call calc_lpi(ql1d,qi1d,qs1d,qg1d,w1d,temp,height,lpi(i,j),t_base,t_top,kme,kte)
      END DO
      END DO
      return
      end subroutine calclpi
      subroutine &
     &  calc_lpi(ql3d,qi3d,qs3d,qg3d,w3d,t3d,height,lpi,t_base,t_top,nk,nke)
      implicit none
      integer nk,nke
      real t_base,t_top
      real ql3d(nk)
      real qg3d(nk)
      real qi3d(nk)
      real qs3d(nk)
      real w3d(0:nk)
      real t3d(nk)
      real height(0:nk)
      real lpi
      real del_z(nk)
      real w_ave(nk)
      integer ic,jc,icnt,i,j,k,i_collapse
      real i_dist,j_dist,del_z_tot
      real top, bot
      real num,den,ave_z
      real num_s,den_s
      real num_i,den_i
      real q_isg
      icnt=0
      do k=1,nke
        top=height(k)
        bot=height(k-1)
        del_z(k)=top-bot
        w_ave(k)=0.5*(w3d(k)+w3d(k-1))
      end do
!
!     Check for collapsing cell
! Here, we don't check, since it requires a halo.
      ave_z=0
      del_z_tot=0
      lpi=0
      do k=1,nke-1
       if (t3d(k).le.t_base.and.t3d(k).gt.t_top)then ! set temp range
        
        den_i = qi3d(k)+qg3d(k)     
        den_s = qs3d(k)+qg3d(k)
        if (qs3d(k).eq.0.or.qg3d(k).eq.0.)then !checks for zeroes
         den_s=10000.
         num_s = 0.
        else
         num_s = sqrt(qs3d(k)*qg3d(k))   
        end if
        if (qi3d(k).eq.0.or.qg3d(k).eq.0.)then  ! checks for zeroes
         den_i=10000.
         num_i = 0.
        else
         num_i = sqrt(qi3d(k)*qg3d(k))
        end if
        q_isg = qg3d(k)*(num_i/den_i+num_s/den_s)  ! ice "fract"-content

        if (ql3d(k).eq.0.or.q_isg.eq.0)then
          num=0
          den=10000.
        else
         num = sqrt(ql3d(k)*q_isg)
         den = ql3d(k)+q_isg
        end if
        del_z_tot=del_z_tot+del_z(k)
        if (num.gt.0)then
         ave_z=ave_z+del_z(k)*(2.*num/den)*w_ave(k)**2 ! lightning potential index J/unit-mass
        end if
       end if
      end do
!
      if (del_z_tot.eq.0)del_z_tot=100000
      lpi=ave_z/del_z_tot
       
!
      return
      end subroutine calc_lpi
  END MODULE module_ltng_lpi
