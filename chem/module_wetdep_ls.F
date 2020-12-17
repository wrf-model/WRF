MODULE module_wetdep_ls
USE module_state_description, only:p_qv,p_qc,p_so2,p_sulf, &
                              p_bc1,p_bc2,p_oc1,p_oc2,p_seas_1,p_seas_2,     &
                              p_seas_3,p_seas_4,p_dms

CONTAINS
subroutine wetdep_ls(dt,var,rain,moist,rho,num_moist, &
         num_chem,numgas,dz8w,vvel,chem_opt,             &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: num_chem,numgas,num_moist,               &
                                  chem_opt,                                &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   real, INTENT(IN ) :: dt
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) :: rho,dz8w,vvel        
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ,1:num_chem),                        &
          INTENT(INOUT) :: var        
   REAL,  DIMENSION( ims:ime , jms:jme ),                                  &
          INTENT(IN   ) :: rain
!  REAL,  DIMENSION( ims:ime ,  jms:jme ),                                  &
!         INTENT(INOUT   ) :: var_rmv
   REAL,  DIMENSION( its:ite ,  jts:jte ) :: var_sum,var_rmv
   REAL,  DIMENSION( its:ite ,  kts:kte, jts:jte ) :: var_rmvl
   REAL,  DIMENSION( its:ite ,  jts:jte ) :: frc,var_sum_clw,rain_clw     
    real :: dvar,factor,clsum,alpha,rho_water
   integer :: nv,i,j,k,km,kb,kbeg
    rho_water = 1000.
!   write(0,*) 'in wetdepls, numgas,num_chem = ',numgas,num_chem,chem_opt
!   nv=p_bc1
    do nv=1,num_chem
       if(nv.le. numgas .and. nv.ne.p_sulf)cycle
       alpha = .5    ! scavenging factor
       if(chem_opt >= 300)then
          if(nv.eq.p_bc1 .or. nv.eq.p_oc1 .or. nv.eq.p_dms)cycle
          if(nv.eq.p_bc2 .or. nv.eq.p_oc2)alpha=0.8
       endif
       if(nv.eq.p_sulf .or. nv.eq.p_seas_1 .or. nv.eq.p_seas_2    &
                       .or. nv.eq.p_seas_3 .or. nv.eq.p_seas_4)alpha=1.
       do i=its,ite
       do j=jts,jte
        var_sum_clw(i,j)=0.
        var_sum(i,j)=0.
        var_rmvl(i,:,j)=0.
        frc(i,j)=0.
        rain_clw(i,j)=0.
        if(rain(i,j).gt.1.e-10)then
!          write(0,*)i,j,rain(i,j)
! convert rain back to rate
!
           rain_clw(i,j)=rain(i,j)/dt
! total cloud water
!
           do k=1,kte-1
              dvar=max(0.,moist(i,k,j,p_qc)*rho(i,k,j)*vvel(i,k,j)*dz8w(i,k,j))
              var_sum_clw(i,j)=var_sum_clw(i,j)+dvar
              var_sum(i,j)=var_sum(i,j)+var(i,k,j,nv)*rho(i,k,j)
           enddo
           if(var_sum(i,j).gt.1.e-10 .and. var_sum_clw(i,j).gt.1.e-10 ) then
   !        assuming that frc is onstant, it is my conversion factor 
!       (just like in convec. parameterization
              frc(i,j)=rain_clw(i,j)/var_sum_clw(i,j)
!    write(0,*)'frc ', frc(i,j),var_sum_clw(i,j),var_sum(i,j)
              frc(i,j)=max(1.e-6,min(frc(i,j),.005))
           endif
        endif
      enddo
    enddo
!
! get rid of it
!
    do i=its,ite
    do j=jts,jte
     if(rain(i,j).gt.1.e-10 .and. var_sum(i,j).gt.1.e-10 .and. var_sum_clw(i,j).gt.1.e-10)then
       do k=kts,kte-2
        if(var(i,k,j,nv).gt.1.e-16 .and. moist(i,k,j,p_qc).gt.0.)then
        factor = max(0.,frc(i,j)*rho(i,k,j)*dz8w(i,k,j)*vvel(i,k,j))
!       write(0,*)'var before ',k,km,var(i,k,j,nv),factor
        dvar=alpha*factor/(1+factor)*var(i,k,j,nv)
        var(i,k,j,nv)=max(1.e-16,var(i,k,j,nv)-dvar)
!       write(0,*)'var after ',km,var(i,k,j,nv),dvar
        endif
       enddo
!      var_rmv(i,j)=var_rmv(i,j)+var_rmvl(i,j)
    endif
    enddo
    enddo
    enddo
END SUBROUTINE WETDEP_LS
END MODULE module_wetdep_ls


