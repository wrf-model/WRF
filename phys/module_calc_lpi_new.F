MODULE module_calc_lpi_new
CONTAINS
  SUBROUTINE calc_lpi_new(qc, qr, qi, qs, qg    &
                 ,w,dz8w,pi_phy,th_phy,p_phy,rho_phy                    &
                 ,lpi2d,lpi3d &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )

  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
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
        INTENT(INOUT),OPTIONAL ::                                          &
                                                      lpi3d 
!                                                     ,&
!                                                     light_ne, &

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
     &                      lpi2d
!     REAL, DIMENSION(ims:ime,jms:jme)::      &
!    &                      LPI,LPOS,LNEG,LNEU,plpi,nlpi,neulpi,prec_ice,wqg_m15




      REAL, DIMENSION(kms:kme)::    tempk,rh,qtot,qitot
      REAL, DIMENSION(kms:kme):: p1d,rho1d,qti1d
      REAL, DIMENSION(kms:kme):: temp,qc1d,ql1d,qi1d,qs1d,qg1d,lpi1d,del_z
      REAL, DIMENSION(0:kme):: w1d,height
      REAL, DIMENSION(kms:kme):: e1d,height_t,w1d_t
      REAL z_full,qrs,teten,RELHUM,LOC,Td_850,Td_700,PC_DWPT
      INTEGER level
      REAL :: t_base,t_top
      real top, bot
      real num,den,ave_z
      real num_s,den_s
      real num_i,den_i
      real q_isg,del_z_tot 

      INTEGER I_COLLAPSE
      LOGICAL LOOK_T
      INTEGER I_START,I_END,J_START,J_END


  INTEGER ::               i,j,k
!-------------------------------------------------------------------
! !   print*,'ims,ime,jms,jme,kms,kme = ',ims,ime,jms,jme,kms,kme 
!     print*,'its,ite,jts,jte,kts,kte = ',its,ite,jts,jte,kts,kte 
!     parameter(t_pos=0.12,t_neg=0.06,t_neu=0.03) 
!     note: t_pos is modified based on the height of the top of the cloud
!     I believe that the value of 60000 m/s might be a typical speed
!     See: https\://en.wikipedia.org/wiki/Lightning
!     print*,'dt = ',dt
!     print*,'dx = ',dx
!     print*,'coul_pos = ',coul_pos
!     print*,'coul_neg = ',coul_neg
!     print*,'coul_neu = ',coul_neu
!     return (1)
!     if (MAXVAL(light_pe).ne.0)print*,'maxval light_pe'
!     if (MAXVAL(light_pe).ne.0)write(MAXVAL(light_pe))
!     if (MAXVAL(light_ne).ne.0)print*,'maxval light_ne'
!     if (MAXVAL(light_ne).ne.0)write(MAXVAL(light_ne))
!-------------------------------------------------------------------
      t_base =  0
      t_top  =  -20.

      DO j = jts,jte
      DO i = its,ite
        z_full=0.
        height(0)=z_full
        w1d(0)=w(i,1,j)
      do k = kts,kte-1
          if (k.lt.kte-1)then
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
! Hail doesn't usually charge
          qg1d(k)=qg(i,k,j)
! For conservative advection multiply by rho1d and divide by it below
      enddo
      do k = kts,kte-1
       w1d_t(k)=0.5*(w1d(k-1)+w1d(k))
      end do
      do k=kts,kte
        lpi3d(i,k,j) = 0
        top=height(k+1)
        bot=height(k)
        del_z(k)=top-bot
!       if (max_w(i,j).gt.0.5)print*,"max_w > 0.5"
      end do

      ave_z = 0
      del_z_tot = 0
      lpi2d(i,j) = 0
 
      do k=kts,kte
       if (temp(k).le.t_base.and.temp(k).gt.t_top)then ! set t1d range
        
        den_i = qi1d(k)+qg1d(k)     
        den_s = qs1d(k)+qg1d(k)
         if (qs1d(k).eq.0.or.qg1d(k).eq.0.)then !checks for zeroes
         den_s=10000.
         num_s = 0.
        else
         num_s = sqrt(qs1d(k)*qg1d(k))   
        end if
        if (qi1d(k).eq.0.or.qg1d(k).eq.0.)then  ! checks for zeroes
         den_i=10000.
         num_i = 0.
        else
         num_i = sqrt(qi1d(k)*qg1d(k))
        end if
        q_isg = qg1d(k)*(num_i/den_i+num_s/den_s)  ! ice "fract"-content

        if (ql1d(k).eq.0.or.q_isg.eq.0)then
          num=0
          den=10000.
        else
         num = sqrt(ql1d(k)*q_isg)
         den = ql1d(k)+q_isg
        end if
        del_z_tot=del_z_tot+del_z(k)
        if (num.gt.0)then
         ave_z=ave_z+del_z(k)*(2.*num/den)*w1d_t(k)**2 ! power index J/unit-mass
        end if
      end if
      if (lpi2d(i,j).lt.0)lpi2d(i,j)=0
      end do
!
      if (del_z_tot.eq.0)del_z_tot=100000
        lpi2d(i,j)=ave_z/del_z_tot
      ave_z = 0.
      do k=kts,kte
        
        den_i = qi1d(k)+qg1d(k)     
        den_s = qs1d(k)+qg1d(k)
        if (qs1d(k).eq.0.or.qg1d(k).eq.0.)then !checks for zeroes
         den_s=10000.
         num_s = 0.
        else
         num_s = sqrt(qs1d(k)*qg1d(k))   
        end if
        if (qi1d(k).eq.0.or.qg1d(k).eq.0.)then  ! checks for zeroes
         den_i=10000.
         num_i = 0.
        else
         num_i = sqrt(qi1d(k)*qg1d(k))
        end if
        q_isg = qg1d(k)*(num_i/den_i+num_s/den_s)  ! ice "fract"-content

        if (ql1d(k).eq.0.or.q_isg.eq.0)then
          num=0
          den=10000.
        else
         num = sqrt(ql1d(k)*q_isg)
         den = ql1d(k)+q_isg
        end if
        del_z_tot=del_z_tot+del_z(k)
        if (num.gt.0)then
         ave_z=(2.*num/den)*w1d_t(k)**2 ! power index J/unit-mass
         lpi3d(i,k,j) = ave_z
        end if
      end do
      end do
      end do
!     check cell for active cell
!     go to 10! 
! Check within five grid elements that a majority of max_w > 0.5 m/s
      return
      end subroutine calc_lpi_new
  END MODULE module_calc_lpi_new
