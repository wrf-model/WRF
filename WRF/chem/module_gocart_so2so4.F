MODULE module_gocart_so2so4

CONTAINS

  subroutine so2so4(ils,chem,p_so2,p_sulf,p_h2o2,p_QC,T_PHY,MOIST,gd,          &
                    gd_cldfr,gd_on, NUM_CHEM,NUM_MOIST,                        &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte                                  )
   INTEGER,      INTENT(IN   ) :: ils,num_chem,num_moist,                      &
                                  p_so2,p_sulf,p_h2o2,p_QC,gd_on,          &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::                     t_phy,gd
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          OPTIONAL,                                                        &
          INTENT(IN   ) ::                     gd_cldfr

   integer :: i,k,j
   real :: tc2,tc3,h2o2,cldf
          do j=jts,jte
          do k=kts,kte
          do i=its,ite
             cldf=0.
             if(ils.eq.0 .and. gd_on==1 ) then
                cldf=gd_cldfr(i,k,j)
             elseif(p_qc.gt.1 )then
               if(moist(i,k,j,p_qc).gt.0 )cldf=1.
             endif
             tc2=chem(i,k,j,p_so2)
             IF (cldf > 0.0 .AND. tc2 > 0.0 .AND. t_phy(i,k,j) > 258.0) THEN
                 tc3=chem(i,k,j,p_sulf)
                 h2o2=chem(i,k,j,p_h2o2)

! ****************************************************************************
! *  Update SO2 concentration after cloud chemistry.                         *
! *  SO2 chemical loss rate  = SO4 production rate (MixingRatio/timestep).   *
! ****************************************************************************

           ! Cloud chemistry (above 258K): 

              IF (tc2 > h2o2) THEN
                 cldf = cldf * (h2o2/tc2)
                 h2o2 = h2o2 * (1.0 - cldf)
              ELSE
                 h2o2 = h2o2 * (1.0 - cldf*tc2/h2o2)
              END IF
              chem(i,k,j,p_so2) = max(1.e-16,tc2 * (1.0 - cldf) )
              chem(i,k,j,p_sulf) = max(1.e-16,(tc3 + tc2*cldf))
              chem(i,k,j,p_h2o2)=max(1.e-16,h2o2)
!             write(0,*)'2,so2,sulf,h2o2 = ',chem(i,k,j,p_so2),chem(i,k,j,p_sulf),chem(i,k,j,p_h2o2)
           END IF
           enddo
           enddo
           enddo

! ****************************************************************************

END  subroutine so2so4
END MODULE module_gocart_so2so4
