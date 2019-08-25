MODULE module_ltng_pe
CONTAINS
!https://journals.ametsoc.org/doi/10.1175/WAF-D-11-00144.1
! coul_pos, etc, are charging constants, in units of Coulombs..
! coul_pos, etc, now set in Registry.EM_COMMON; can be added to namelist.input
! The values set work well for the NSSL scheme, and probably for Thompson
! Aerosol Aware.  They are suggested values and  can be modified by the user  based on a set of  case study comparisons.
! For the WSM6, it is suggested to double the values. 
  SUBROUTINE calc_dyn_pe(dx,dy,qc, qr, qi, qs, qg                                &
                 ,w,dz8w,pi_phy,th_phy,p_phy,rho_phy                    &
                 ,light_pe &
                 ,light_ne &
                 ,light_neu &
                 ,dt &
                 ,coul_pos,coul_neg,coul_neu &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
  REAL,   INTENT(IN) ::  coul_pos,coul_neg,coul_neu
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN) ::                                          &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg
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

!     REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)::      &
!    &                      prec_ice,wqg_m15
      REAL, DIMENSION(ims:ime,jms:jme)::      &
     &                      LPI,LPOS,LNEG,LNEU,plpi,nlpi,neulpi
!     REAL, DIMENSION(ims:ime,jms:jme)::      &
!    &                      LPI,LPOS,LNEG,LNEU,plpi,nlpi,neulpi,prec_ice,wqg_m15




      REAL, DIMENSION(kms:kme)::    tempk,rh,qtot,qitot
      REAL, DIMENSION(kms:kme):: p1d,rho1d,qti1d
      REAL, DIMENSION(kms:kme):: temp,qc1d,ql1d,qi1d,qs1d,qg1d,lpi1d
      REAL, DIMENSION(0:kme):: w1d,height
      REAL, DIMENSION(kms:kme):: e1d,height_t,w1d_t
      REAL z_full,qrs,teten,RELHUM,LOC,Td_850,Td_700,PC_DWPT
      INTEGER level
      REAL :: dt,dxdy,t_base,t_top
      INTEGER I_COLLAPSE
      LOGICAL LOOK_T
      real t_pos,t_neg,t_neu
      INTEGER I_START,I_END,J_START,J_END


  INTEGER ::               i,j,k
!-------------------------------------------------------------------
! !   print*,'ims,ime,jms,jme,kms,kme = ',ims,ime,jms,jme,kms,kme 
!     print*,'its,ite,jts,jte,kts,kte = ',its,ite,jts,jte,kts,kte 
!     parameter(t_pos=0.12,t_neg=0.06,t_neu=0.03) 
!     note: t_pos is modified based on the height of the top of the cloud
!     I believe that the value of 60000 m/s might be a typical speed
!     See: https\://en.wikipedia.org/wiki/Lightning
      t_pos=0.12
      t_neg=0.06
      t_neu=0.03
      dxdy=dx*dy
!     print*,'kte = ',kte
!     return (1)
      DO j = jts,jte
      DO i = its,ite
        z_full=0.
        height(0)=z_full
        w1d(0)=w(i,1,j)
      DO k = kts,kte
          if (k.lt.kte)then
           w1d(k)=w(i,k+1,j)
          else
           w1d(k)=0.
          end if
          temp(k) = th_phy(i,k,j)*pi_phy(i,k,j)-273.16
          tempk(k) = th_phy(i,k,j)*pi_phy(i,k,j)
          p1d(k)=p_phy(i,k,j)
          rho1d(k)=rho_phy(i,k,j)
          z_full=z_full+dz8w(i,k,j)
          height(k)=z_full
          qc1d(k)=qc(i,k,j)
          ql1d(k)=qc(i,k,j)+qr(i,k,j)
          qi1d(k)=qi(i,k,j)
!         qti1d(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)+qh(i,k,j)
          qti1d(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)
          qs1d(k)=qs(i,k,j)
!         qg1d(k)=qg(i,k,j)+qh(i,k,j)
          qg1d(k)=qg(i,k,j)
!         qtot(k)=qc(i,k,j)+qr(i,k,j)+qi(i,k,j)+qs(i,k,j)+qg(i,k,j)+qh(i,k,j)
          qtot(k)=qc(i,k,j)+qr(i,k,j)+qi(i,k,j)+qs(i,k,j)+qg(i,k,j)
          qitot(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)
!         qitot(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)+qh(i,k,j)
! For conservative advection multiply by rho1d and divide by it below
          qitot(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)
      ENDDO
      do k = kts,kte
       height_t(k)=0.5*(height(k-1)+height(k))
       w1d_t(k)=0.5*(w1d(k-1)+w1d(k))
      end do
      t_base=-10
      t_top=-30
      do k = kts,kte-1
        e1d(k)=light_pe(i,k,j)*rho1d(k)
      end do
      call calc_lpi(ql1d,qi1d,qs1d,qg1d,w1d,temp,height,plpi(i,j),lpi1d,t_base,t_top,kme,kte)
      look_t=.true.
       do k=kte-1,kts,-1
       if (look_t)then
       if (temp(k).ge.-27.and.qtot(k).gt.0.00001.and.height(k).ge.4000)then
        t_pos=height_t(k)/60000.
!       print*,'height_t(k) = ',k,height_t(k)
!       print*,'t_pos = ',t_pos
        look_t=.false.
       end if
       end if
       end do
      call power_index(temp,qitot,rho1d,plpi(i,j),lpi1d,e1d,dt,dxdy,t_pos,t_base,t_top,coul_pos,kme,kte)
      do k = kts,kte-1
          light_pe(i,k,j)=e1d(k)
      end do
      do k = kts,kte-1
        e1d(k)=light_ne(i,k,j)*rho1d(k)
!     IF (ISNAN(e1d(k))) STOP 'BARRY WANTS TO STOP HERE 18'
      end do
      t_base=-0
      t_top=-20
      call calc_lpi(ql1d,qi1d,qs1d,qg1d,w1d,temp,height,nlpi(i,j),lpi1d,t_base,t_top,kme,kte)
      lpi(i,j)=nlpi(i,j)
! OLD 2
       look_t=.true.
       do k=kte-1,kts,-1
       if (look_t)then
       if (temp(k).ge.-2.and.qtot(k).gt.0.0001.and.height(k).ge.1000)then
        t_neg=height_t(k)/60000.
        look_t=.false.
       end if
       end if
       end do
      call power_index(temp,qitot,rho1d,nlpi(i,j),lpi1d,e1d,dt,dxdy,t_neg,t_base,t_top,coul_neg,kme,kte)
      do k = kts,kte-1
          light_ne(i,k,j)=e1d(k)
      end do
! OLD 3
      do k = kts,kte-1
        e1d(k)=light_neu(i,k,j)*rho1d(k)
      end do
      t_base=0.
      t_top=-30.
      call calc_lpi(ql1d,qi1d,qs1d,qg1d,w1d,temp,height,neulpi(i,j),lpi1d,t_base,t_top,kme,kte)
       t_neu=0.5*(t_pos-t_neg)
       if (abs(t_pos-t_neg).le.01)t_neu=0.01
      call power_index(temp,qitot,rho1d,neulpi(i,j),lpi1d,e1d,dt,dxdy,t_neu,t_base,t_top,coul_neu,kme,kte)
      do k = kts,kte-1
          light_neu(i,k,j)=e1d(k)
      end do


      END DO
      END DO
      return
      end subroutine calc_dyn_pe
      subroutine &
     &  calc_lpi(ql3d,qi3d,qs3d,qg3d,w3d,t3d,height,lpi,lpi1d,t_base,t_top,nk,nke)
      implicit none
      integer nk,nke
      real t_base,t_top
      real lpi1d(nk)
      real ql3d(nk)
      real qg3d(nk)
      real qi3d(nk)
      real qs3d(nk)
      real w3d(0:nk)
      real t3d(nk)
      real height(0:nk)
      real lpi,plpi
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
!     print*,'nke = ',nke
      do k=1,nke
        lpi1d(k)=0
        top=height(k)
        bot=height(k-1)
        del_z(k)=top-bot
        w_ave(k)=0.5*(w3d(k)+w3d(k-1))
      end do
      lpi1d(nk)=0
!
!     Check for collapsing cell
      ave_z=0
      del_z_tot=0
      lpi=0
      do k=1,nke-1
!      print*,'k = ',k
!      print*,'t3d(k) = ',t3d(k)
!      print*,'t_base = ',t_base
!      print*,'t_top = ',t_top

       if (t3d(k).le.t_base.and.t3d(k).gt.t_top)then ! set temp range
        
        den_i = qi3d(k)+qg3d(k)     
        den_s = qs3d(k)+qg3d(k)
        if (qs3d(k).le.1.E-10.or.qg3d(k).le.1.E-10)then !checks for zeroes
         den_s=10000.
         num_s = 0.
        else
         num_s = sqrt(qs3d(k)*qg3d(k))   
        end if
!       print*,'qi3d(k) = ',qi3d(k)
!       print*,'qg3d(k) = ',qg3d(k)
        if (qi3d(k).le.1.E-10.or.qg3d(k).le.1.E-10)then
         num_i = 0.
         den_i = 10000
        else
         num_i = sqrt(qi3d(k)*qg3d(k))
        end if
!       print*,'num_i = ',num_i
!       print*,'den_i = ',den_i
!       print*,'num_s = ',num_s
!       print*,'den_s = ',den_s

        q_isg = qg3d(k)*(num_i/den_i+num_s/den_s)  ! ice "fract"-content

        if (ql3d(k).le.1.E-10.or.q_isg.le.1.E-10)then
          num=0
          den=10000.
        else
         num = sqrt(ql3d(k)*q_isg)
         den = ql3d(k)+q_isg
        end if
        del_z_tot=del_z_tot+del_z(k)
        if (num.gt.0)then
         ave_z=ave_z+del_z(k)*(2.*num/den)*w_ave(k)**2 ! power index J/unit-mass
         lpi1d(k)=del_z(k)*(2.*num/den)*w_ave(k)**2 ! power index J/unit-mass
        end if
       end if
      end do
!
      if (del_z_tot.eq.0)del_z_tot=100000
      lpi=ave_z/del_z_tot
       
!
      return
      end subroutine calc_lpi
      subroutine &
     &  power_index(t3d,qitot,rho1d,lpi,lpi1d,le1d,dt,dxdy,t_pos,t_base,t_top,coul_value,nk,nke)
      implicit none
      integer nk,nke
      real coul_value
      real t_pos,t_top,t_base
      real lpi1d(nk),lpi,dxdy
      real le1d(nk)
      real rho1d(nk)
      real ql3d(nk)
      real qitot(nk)
      real qg3d(nk)
      real qi3d(nk)
      real qs3d(nk)
      real w3d(nk)
      real t3d(nk)
      real del_z(nk)
      real w_ave(nk)
      real power
      integer ic,jc,icnt,i,j,k,i_collapse
      real i_dist,j_dist,del_z_tot
      real top, bot
      real num,den,ave_z
      real num_s,den_s
      real num_i,den_i
      real q_isg
      real dt
      lpi=0
      le1d(nk)=0
!     print*,'t_pos = ',t_pos
      do k=1,nke-1
       if (t3d(k).le.t_base.and.t3d(k).gt.t_top)then ! set temp range
        power=coul_value/t_pos*qitot(k)*rho1d(k)*lpi1d(k)
        le1d(k)=le1d(k)+power*dt*dxdy
        lpi = lpi+le1d(k)
       end if
      end do
      return
      end subroutine power_index
  END MODULE module_ltng_pe
