!
!
!
!
module module_sf_scmflux
contains
!
!-------------------------------------------------------------------
!
   subroutine scmflux(u3d, v3d, t3d, qv3d, p3d, dz8w,                           &
                     cp, rovcp, xlv, psfc, cpm, xland,                          &
                     psim, psih, hfx, qfx, lh, tsk, flhc, flqc,                 &
                     znt, gz1oz0, wspd,                                         &
                     julian_in, karman, p1000mb,                                &
                     itimestep,chklowq,                                          &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, itimestep        
!   
   real, intent(in)      ::         cp, rovcp, xlv, julian_in, karman, p1000mb
!
   real, dimension( ims:ime, kms:kme, jms:jme )                               , &
            intent(in)   ::                                                u3d, &
                                                                           v3d, &
                                                                           t3d, &
                                                                          qv3d, &
                                                                           p3d, &
                                                                          dz8w
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(in)   ::                                               psfc, &
                                                                         xland, &
                                                                          flhc, &
                                                                          flqc 
!
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                cpm, &
                                                                           znt, &
                                                                        gz1oz0, &
                                                                          wspd, &
                                                                          psim, &
                                                                          psih, &
                                                                           hfx, &
                                                                           qfx, &
                                                                            lh, &
                                                                           tsk,&
                                                                        chklowq           
! local vars

   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   real, parameter       ::                                           den = 1.
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real                       ::                     qfx_interp,hfx_interp ! JP
   real, dimension( its:ite, jts:jte) ::                                   u2d, &
                                                                           v2d, &
                                                                           t2d, &
                                                                          qv2d, &
                                                                           p2d, &
                                                                        dz8w1d, &
                                                                            za, &
                                                                           thx, &
                                                                          thgb
   logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!
   open(unit=11, file='scmflx_bdy', form='formatted', status='old')
   print*,'scmflx_bdy' 
   read(11,*) julian_s, julian_e, fc_int
!
     end_of_file = .false.
     n=1
     do while (.not. end_of_file)
       read(11,*,end=100) fc_hfx(n), fc_qfx(n)
       fc_julian(n)=julian_s+(n-1)*fc_int/86400.
       n=n+1
       go to 110
 100   end_of_file = .true.  
 110   continue
     enddo
     nt=n-1
   close(11)
!
!-----linear interpolation of the fluxes for each time step
!
   do n=1,nt 
     if (julian_in.ge.fc_julian(n) .and. julian_in.lt.fc_julian(n+1)) then
       qfx_interp= fc_qfx(n)                                                      &
                +(fc_qfx(n+1)-fc_qfx(n))*(julian_in-fc_julian(n))/(fc_int/86400.)
       hfx_interp= fc_hfx(n)                                                      &
                +(fc_hfx(n+1)-fc_hfx(n))*(julian_in-fc_julian(n))/(fc_int/86400.)
     endif
   enddo
!
!-----compute surface moisture and heat fluxes, in the unit of [W m-2]
!

!-----compute skin temperature
!
   do j=jts,jte
     do i=its,ite
       u2d(i,j)=u3d(i,1,j)
       v2d(i,j)=v3d(i,1,j)
       t2d(i,j)=t3d(i,1,j)
       qv2d(i,j)=qv3d(i,1,j)
       p2d(i,j)=p3d(i,1,j)
       dz8w1d(i,j)=dz8w(i,1,j)
       za(i,j)=0.5*dz8w1d(i,j)
     enddo
   enddo 

   do j=jts, jte
     do i=its, ite
!
!-----compute surface moisture flux
!
       qfx(i,j)=qfx_interp/1000.
       qfx(i,j)=amax1(qfx(i,j),0.)
       lh(i,j)=xlv*qfx(i,j)
!


!-----compute surface heat flux
!
       cpm(i,j)=cp*(1.+0.8*qv2d(i,j))
!       print*,'i j cpm xland qv2d',i,j,cpm(i,j),xland(i,j), qv2d(i,j)
!       print*,hfx_interp
       if(xland(i,j)-1.5 .gt. 0.)then
         hfx(i,j)=hfx_interp*cpm(i,j)
       elseif(xland(i,j)-1.5 .lt. 0.)then
         hfx(i,j)=hfx_interp*cpm(i,j)
         hfx(i,j)=amax1(hfx(i,j),-250.)
       endif
     enddo
   enddo
!
   
   if (itimestep .eq. 1) then
     psih=0.0
     psim=0.0

   endif
     chklowq=1.0 !JP

   
   do j=jts,jte
     do i=its,ite
       gz1oz0(i,j)=alog(za(i,j)/znt(i,j))
       fh=gz1oz0(i,j)-psih(i,j)
       fm=gz1oz0(i,j)-psim(i,j)
       ch=karman**2/fh/fm
       wspd(i,j)=sqrt(u2d(i,j)**2+v2d(i,j)**2)
       dtdiff=-hfx(i,j)/den/cpm(i,j)/ch/wspd(i,j)
       tsk(i,j)=t2d(i,j)-dtdiff
     enddo
   enddo
   
   end subroutine scmflux
!-------------------------------------------------------------------
end module module_sf_scmflux
