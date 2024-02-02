!=================================================================================================================
 module module_bl_gwdo
 use ccpp_kind_types,only: kind_phys

 use bl_gwdo,only: bl_gwdo_run


 implicit none
 private
 public:: gwdo


 contains


!=================================================================================================================
 subroutine gwdo(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,                            &
                 rublten,rvblten,                                             &
                 dtaux3d,dtauy3d,dusfcg,dvsfcg,                               &
                 var2d,oc12d,oa2d1,oa2d2,oa2d3,oa2d4,ol2d1,ol2d2,ol2d3,ol2d4, &
                 sina,cosa,znu,znw,p_top,                                     &
                 cp,g,rd,rv,ep1,pi,                                           &
                 dt,dx,kpbl2d,itimestep,                                      &
                 ids,ide, jds,jde, kds,kde,                                   &
                 ims,ime, jms,jme, kms,kme,                                   &
                 its,ite, jts,jte, kts,kte,                                   &
                 errmsg,errflg                                                &
                )
!=================================================================================================================
!                                                                       
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rublten     u tendency due to pbl parameterization (m/s/s) 
!-- rvblten     v tendency due to pbl parameterization (m/s/s)
!-- sina        sine rotation angle
!-- cosa        cosine rotation angle
!-- znu         eta values (sigma values)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rd          gas constant for dry air (j/kg/k)
!-- z           height above sea level (m)
!-- rv          gas constant for water vapor (j/kg/k)
!-- dt          time step (s)
!-- dx          model grid interval (m)
!-- ep1         constant for virtual temperature (r_v/r_d - 1) (dimensionless)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte
 integer,intent(in):: itimestep

 integer,intent(in),dimension(ims:ime,jms:jme):: kpbl2d

 real(kind=kind_phys),intent(in):: dt,cp,g,rd,rv,ep1,pi
 real(kind=kind_phys),intent(in),optional:: p_top

 real(kind=kind_phys),intent(in),dimension(kms:kme),optional:: &
                                                                 znu, &
                                                                 znw

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: &
                                                                  dx, &
                                                               var2d, &
                                                               oc12d, &
                                             oa2d1,oa2d2,oa2d3,oa2d4, &
                                             ol2d1,ol2d2,ol2d3,ol2d4, &
                                                           sina,cosa

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
                                                                qv3d, &
                                                                 p3d, &
                                                                pi3d, &
                                                                 t3d, &
                                                                 u3d, &
                                                                 v3d, &
                                                                   z

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
                                                                 p3di

!--- output arguments:
 character(len=*),intent(out)::  errmsg

 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(ims:ime,jms:jme):: &
                                                              dusfcg, &
                                                              dvsfcg

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme ):: &
                                                             dtaux3d, &
                                                             dtauy3d

!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
                                                             rublten, &
                                                             rvblten

!--- local variables and arrays:
 integer:: i,j,k

 real(kind=kind_phys),dimension(its:ite):: &
    var2d_hv,oc12d_hv,dx_hv,sina_hv,cosa_hv
 real(kind=kind_phys),dimension(its:ite):: &
    oa2d1_hv,oa2d2_hv,oa2d3_hv,oa2d4_hv,ol2d1_hv,ol2d2_hv,ol2d3_hv,ol2d4_hv
 real(kind=kind_phys),dimension(its:ite):: &
    dusfcg_hv,dvsfcg_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
     u3d_hv,v3d_hv,t3d_hv,qv3d_hv,pi3d_hv,p3d_hv,z_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
     rublten_hv,rvblten_hv,dtaux3d_hv,dtauy3d_hv

 real(kind=kind_phys),dimension(its:ite,kms:kme):: &
     p3di_hv

!-----------------------------------------------------------------------------------------------------------------

!  Outer j-loop. Allows consistency between WRF and MPAS in the driver.

 do j = jts,jte

    !  All variables for gwdo2d are tile-sized and have only a single
    !  horizontal dimension. The _hv suffix refers to "horizontal vertical",
    !  a reminder that there is a single horizontal index. Yes, we know that 
    !  variables that have only a horizontal index are not *really* _hv.

    !  All of the following 3d and 2d variables are declared intent(in) in the
    !  gwdo2d subroutine, so there is no need to put the updated values back
    !  from the temporary arrays back into the original arrays.

    !  Variables that are INTENT(IN) or INTENT(INOUT)

    !  3d, interface levels:
    do k = kts,kte+1
       do i = its,ite
          p3di_hv(i,k) = p3di(i,k,j)
       enddo
    enddo

    !  3d, layers:
    do k = kts,kte
       do i = its,ite
          rublten_hv(i,k) = rublten(i,k,j)
          rvblten_hv(i,k) = rvblten(i,k,j)
              u3d_hv(i,k) =     u3d(i,k,j)
              v3d_hv(i,k) =     v3d(i,k,j)
              t3d_hv(i,k) =     t3d(i,k,j)
             qv3d_hv(i,k) =    qv3d(i,k,j)
              p3d_hv(i,k) =     p3d(i,k,j)
             pi3d_hv(i,k) =    pi3d(i,k,j)
                z_hv(i,k) =       z(i,k,j)
       enddo
    enddo

    !  2d:
    do i = its,ite
          dx_hv(i) =    dx(i,j)
       var2d_hv(i) = var2d(i,j)
       oc12d_hv(i) = oc12d(i,j)
        sina_hv(i) =  sina(i,j)
        cosa_hv(i) =  cosa(i,j)
       oa2d1_hv(i) = oa2d1(i,j)
       oa2d2_hv(i) = oa2d2(i,j)
       oa2d3_hv(i) = oa2d3(i,j)
       oa2d4_hv(i) = oa2d4(i,j)
       ol2d1_hv(i) = ol2d1(i,j)
       ol2d2_hv(i) = ol2d2(i,j)
       ol2d3_hv(i) = ol2d3(i,j)
       ol2d4_hv(i) = ol2d4(i,j)
    enddo

    call bl_gwdo_run(sina=sina_hv,cosa=cosa_hv                &
                    ,rublten=rublten_hv,rvblten=rvblten_hv    &
                    ,dtaux3d=dtaux3d_hv,dtauy3d=dtauy3d_hv    &
                    ,dusfcg=dusfcg_hv,dvsfcg=dvsfcg_hv        &
                    ,uproj=u3d_hv,vproj=v3d_hv                &
                    ,t1=t3d_hv,q1=qv3d_hv                     &
                    ,prsi=p3di_hv                             &
                    ,prsl=p3d_hv,prslk=pi3d_hv                &
                    ,zl=z_hv                                  &
                    ,var=var2d_hv,oc1=oc12d_hv                &
                    ,oa2d1=oa2d1_hv, oa2d2=oa2d2_hv           &
                    ,oa2d3=oa2d3_hv, oa2d4=oa2d4_hv           &
                    ,ol2d1=ol2d1_hv, ol2d2=ol2d2_hv           &
                    ,ol2d3=ol2d3_hv, ol2d4=ol2d4_hv           &
                    ,g_=g,cp_=cp,rd_=rd,rv_=rv,fv_=ep1,pi_=pi &
                    ,dxmeter=dx_hv,deltim=dt                  &
                    ,its=its,ite=ite,kte=kte,kme=kte+1        &
                    ,errmsg=errmsg,errflg=errflg)

    !  Variables that are INTENT(OUT) or INTENT(INOUT):

    !  3d, layers:
    do k = kts,kte
       do i = its,ite
          rublten(i,k,j) = rublten_hv(i,k)
          rvblten(i,k,j) = rvblten_hv(i,k)
          dtaux3d(i,k,j) = dtaux3d_hv(i,k)
          dtauy3d(i,k,j) = dtauy3d_hv(i,k)
       enddo
    enddo

    !  2d:
    do i = its,ite
       dusfcg(i,j) = dusfcg_hv(i)
       dvsfcg(i,j) = dvsfcg_hv(i)
    enddo

 enddo ! Outer J-loop

 end subroutine gwdo

!=================================================================================================================
end module module_bl_gwdo
!=================================================================================================================
