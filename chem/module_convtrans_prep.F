Module module_convtrans_prep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
    subroutine convtrans_prep(gd_cloud,gd_cloud2,gd_cloud_a,gd_cloud_b,raincv,raincv_a,raincv_b,   &
               gd_cloud2_a,gd_cloud2_b,stepave,ktau,dt,cu_rad_feedback,cu_phys,                    &
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte,kts,kte                                               )
   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
 
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::                                             &
           gd_cloud,gd_cloud2
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           gd_cloud_a,gd_cloud_b,gd_cloud2_a,gd_cloud2_b
   REAL, DIMENSION( ims:ime, jms:jme ),                       &
         INTENT(IN ) :: raincv
   REAL, DIMENSION( ims:ime, jms:jme ),                       &
         INTENT(INOUT ) :: raincv_a,raincv_b
   INTEGER, INTENT(IN) :: ktau,cu_phys
   INTEGER, INTENT(INOUT) :: stepave
   REAL, INTENT(IN) :: dt
   LOGICAL, INTENT(IN) :: cu_rad_feedback


  stepave=1800./dt
  if(ktau.le.1)then
    gd_cloud2_a=0.
    raincv_a=0.
    raincv_b=0.
  endif
! 
! no time average available in first half hour
!   
         if(ktau.le.stepave)then
           do j=jts,jte
                do i=its,ite
                     raincv_b(i,j)=raincv(i,j)
                enddo
           enddo
          endif
! 
! build time average, and stored in raincv_b to be used by convective transport routine
!   
          if(mod(ktau,stepave).ne.0)then
           do j=jts,jte
                do i=its,ite
                   raincv_a(i,j)=raincv_a(i,j)+raincv(i,j)
                enddo
           enddo
          else if(mod(ktau,stepave).eq.0)then
           do j=jts,jte
                do i=its,ite
                   raincv_b(i,j)=raincv_a(i,j)/float(stepave)
                   raincv_a(i,j)=0.
                enddo
           enddo
         endif
!
! do the same for convective parameterization cloud water mix ratio, 
! currently only for cu_physics=3,5, used by both photolysis and atmospheric radiation
!
       if(cu_rad_feedback.and.(cu_phys.eq.3.or.cu_phys.eq.5))then
         if(ktau.le.stepave)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                          gd_cloud_b(i,k,j)=gd_cloud(i,k,j)
                          gd_cloud2_b(i,k,j)=gd_cloud2(i,k,j)
                     enddo
                enddo
           enddo
         endif   ! stepave
!
! 
!
          if(mod(ktau,stepave).ne.0)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                       gd_cloud_a(i,k,j)=gd_cloud_a(i,k,j)+gd_cloud(i,k,j)
                       gd_cloud2_a(i,k,j)=gd_cloud2_a(i,k,j)+gd_cloud2(i,k,j)
                     enddo
                enddo
           enddo
          else if(mod(ktau,stepave).eq.0)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                       gd_cloud_b(i,k,j)=gd_cloud_a(i,k,j)/float(stepave)
                       gd_cloud_a(i,k,j)=0.
                       gd_cloud2_b(i,k,j)=gd_cloud2_a(i,k,j)/float(stepave)
                       gd_cloud2_a(i,k,j)=0.
                     enddo
                enddo
           enddo
          endif !stepave
       endif ! cu_rad_feedback
    END subroutine convtrans_prep
END MODULE MODULE_CONVTRANS_prep
