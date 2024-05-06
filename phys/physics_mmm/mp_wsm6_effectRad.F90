!=================================================================================================================
 module mp_wsm6_effectrad
 use ccpp_kind_types,only: kind_phys


 use mp_wsm6,only: alpha,n0s,n0smax,pidn0s,pidnc


 implicit none
 private
 public:: mp_wsm6_effectRad_run,     &
          mp_wsm6_effectrad_init,    &
          mp_wsm6_effectRad_finalize


 contains


!=================================================================================================================
!>\section arg_table_mp_wsm6_effectRad_init
!!\html\include mp_wsm6_effectRad_init.html
!!
 subroutine mp_wsm6_effectRad_init(errmsg,errflg)
!=================================================================================================================

!output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'mp_wsm6_effectRad_init OK'
 errflg = 0

 end subroutine mp_wsm6_effectRad_init

!=================================================================================================================
!>\section arg_table_mp_wsm6_effectRad_finalize
!!\html\include mp_wsm6_effectRad_finalize.html
!!
 subroutine mp_wsm6_effectRad_finalize(errmsg,errflg)
!=================================================================================================================

!output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'mp_wsm6_effectRad_final OK'
 errflg = 0

 end subroutine mp_wsm6_effectRad_finalize

!=================================================================================================================
!>\section arg_table_mp_wsm6_effectRad_run
!!\html\include mp_wsm6_effectRad_run.html
!!
 subroutine mp_wsm6_effectRad_run(do_microp_re,t,qc,qi,qs,rho,qmin,t0c,re_qc_bg,re_qi_bg,re_qs_bg, &
                                  re_qc_max,re_qi_max,re_qs_max,re_qc,re_qi,re_qs,its,ite,kts,kte, &
                                  errmsg,errflg)
!=================================================================================================================
!  Compute radiation effective radii of cloud water, ice, and snow for
!  single-moment microphysics.
!  These are entirely consistent with microphysics assumptions, not
!  constant or otherwise ad hoc as is internal to most radiation
!  schemes.
!  Coded and implemented by Soo ya Bae, KIAPS, January 2015.
!-----------------------------------------------------------------------------------------------------------------


!..Sub arguments
 logical,intent(in):: do_microp_re
 integer,intent(in):: its,ite,kts,kte
 real(kind=kind_phys),intent(in):: qmin
 real(kind=kind_phys),intent(in):: t0c
 real(kind=kind_phys),intent(in):: re_qc_bg,re_qi_bg,re_qs_bg
 real(kind=kind_phys),intent(in):: re_qc_max,re_qi_max,re_qs_max
 real(kind=kind_phys),dimension(its:,:),intent(in)::  t
 real(kind=kind_phys),dimension(its:,:),intent(in)::  qc
 real(kind=kind_phys),dimension(its:,:),intent(in)::  qi
 real(kind=kind_phys),dimension(its:,:),intent(in)::  qs
 real(kind=kind_phys),dimension(its:,:),intent(in)::  rho
 real(kind=kind_phys),dimension(its:,:),intent(inout):: re_qc
 real(kind=kind_phys),dimension(its:,:),intent(inout):: re_qi
 real(kind=kind_phys),dimension(its:,:),intent(inout):: re_qs

!...Output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!..Local variables
 integer:: i,k
 integer:: inu_c
 real(kind=kind_phys),dimension(its:ite,kts:kte):: ni
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rqc
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rqi
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rni
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rqs
 real(kind=kind_phys):: temp
 real(kind=kind_phys):: lamdac
 real(kind=kind_phys):: supcol,n0sfac,lamdas
 real(kind=kind_phys):: diai      ! diameter of ice in m
 logical:: has_qc, has_qi, has_qs
!..Minimum microphys values
 real(kind=kind_phys),parameter:: R1 = 1.E-12
 real(kind=kind_phys),parameter:: R2 = 1.E-6
!..Mass power law relations:  mass = am*D**bm
 real(kind=kind_phys),parameter:: bm_r = 3.0
 real(kind=kind_phys),parameter:: obmr = 1.0/bm_r
 real(kind=kind_phys),parameter:: nc0  = 3.E8

!-----------------------------------------------------------------------------------------------------------------

 if(.not. do_microp_re) return

!--- initialization of effective radii of cloud water, cloud ice, and snow to background values:
 do k = kts,kte
   do i = its,ite
      re_qc(i,k) = re_qc_bg
      re_qi(i,k) = re_qi_bg
      re_qs(i,k) = re_qs_bg
   enddo
 enddo

!--- computation of effective radii:
 has_qc = .false.
 has_qi = .false.
 has_qs = .false.

 do k = kts,kte
   do i = its,ite
     ! for cloud
     rqc(i,k) = max(R1,qc(i,k)*rho(i,k))
     if (rqc(i,k).gt.R1) has_qc = .true.
     ! for ice
     rqi(i,k) = max(R1,qi(i,k)*rho(i,k))
     temp = (rho(i,k)*max(qi(i,k),qmin))
     temp = sqrt(sqrt(temp*temp*temp))
     ni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
     rni(i,k)= max(R2,ni(i,k)*rho(i,k))
     if (rqi(i,k).gt.R1 .and. rni(i,k).gt.R2) has_qi = .true.
     ! for snow
     rqs(i,k) = max(R1,qs(i,k)*rho(i,k))
     if (rqs(i,k).gt.R1) has_qs = .true.
   enddo
 enddo

 if (has_qc) then
   do k = kts,kte
     do i = its,ite
       if (rqc(i,k).le.R1) CYCLE
       lamdac = (pidnc*nc0/rqc(i,k))**obmr
       re_qc(i,k) =  max(2.51E-6,min(1.5*(1.0/lamdac),re_qc_max))
     enddo
   enddo
 endif

 if (has_qi) then
   do k = kts,kte
     do i = its,ite
       if (rqi(i,k).le.R1 .or. rni(i,k).le.R2) CYCLE
       diai = 11.9*sqrt(rqi(i,k)/ni(i,k))
       re_qi(i,k) = max(10.01E-6,min(0.75*0.163*diai,re_qi_max))
     enddo
   enddo
 endif

 if (has_qs) then
   do i = its,ite
     do k = kts,kte
       if (rqs(i,k).le.R1) CYCLE
       supcol = t0c-t(i,k)
       n0sfac = max(min(exp(alpha*supcol),n0smax/n0s),1.)
       lamdas = sqrt(sqrt(pidn0s*n0sfac/rqs(i,k)))
       re_qs(i,k) = max(25.E-6,min(0.5*(1./lamdas),re_qs_max))
     enddo
   enddo
 endif

!--- limit effective radii of cloud water, cloud ice, and snow to maximum values:
 do k = kts,kte
   do i = its,ite
      re_qc(i,k) = max(re_qc_bg,min(re_qc(i,k),re_qc_max))
      re_qi(i,k) = max(re_qi_bg,min(re_qi(i,k),re_qi_max))
      re_qs(i,k) = max(re_qs_bg,min(re_qs(i,k),re_qs_max))
   enddo
 enddo

 errmsg = 'mp_wsm6_effectRad_run OK'
 errflg = 0

 end subroutine mp_wsm6_effectRad_run

!=================================================================================================================
 end module mp_wsm6_effectrad
!=================================================================================================================
