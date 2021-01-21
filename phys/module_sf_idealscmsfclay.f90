

MODULE module_sf_idealscmsfclay

CONTAINS


   SUBROUTINE idealscmsfclay(u3d,v3d,th3d,qv3d,p3d,pi3d,rho,z,ht,         &
                     cp,g,rovcp,r,xlv,psfc,chs,chs2,cqs2,cpm,      &
                     znt,ust,mavail,xland,                         &
                     hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,            &
                     u10,v10,th2,t2,q2,                            &
                     svp1,svp2,svp3,svpt0,ep1,ep2,                 &
                     karman,fCor,exch_temf,                          &
                     hfx_force, lh_force, tsk_force,               &
                     hfx_force_tend, lh_force_tend, tsk_force_tend, &
                     dt,itimestep,                                 &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                    &
                     )

      IMPLICIT NONE





























































      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   ) :: u3d, v3d, th3d, qv3d, p3d, pi3d, rho, z
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   ) :: mavail, xland, fCor, ht, psfc, znt
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT) :: hfx, qfx, lh, flhc, flqc, tsk
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT) :: ust, chs2, cqs2, chs, cpm, qgh, qsfc
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  ) :: u10, v10, th2, t2, q2
      REAL,     DIMENSION( ims:ime, jms:jme )           , &
                INTENT(  OUT) :: exch_temf
                                        
      REAL,     INTENT(INOUT) :: hfx_force, lh_force, tsk_force
      REAL,     INTENT(IN   ) :: hfx_force_tend, lh_force_tend, tsk_force_tend
      REAL,     INTENT(IN   ) :: dt
      INTEGER,  INTENT(IN   ) :: itimestep

      REAL,     INTENT(IN   ) :: cp,g,rovcp,r,xlv
      REAL,     INTENT(IN   ) :: svp1,svp2,svp3,svpt0
      REAL,     INTENT(IN   ) :: ep1,ep2,karman



      INTEGER ::  J






   
   hfx_force = hfx_force + dt*hfx_force_tend
   lh_force  = lh_force  + dt*lh_force_tend
   tsk_force = tsk_force + dt*tsk_force_tend

      DO J=jts,jte

        CALL idealscmsfclay1d(j,u1d=u3d(ims,kms,j),v1d=v3d(ims,kms,j),     &
                th1d=th3d(ims,kms,j),qv1d=qv3d(ims,kms,j),p1d=p3d(ims,kms,j), &
                pi1d=pi3d(ims,kms,j),rho=rho(ims,kms,j),z=z(ims,kms,j),&
                zsrf=ht(ims,j),      &
                cp=cp,g=g,rovcp=rovcp,r=r,xlv=xlv,psfc=psfc(ims,j),    &
                chs=chs(ims,j),chs2=chs2(ims,j),cqs2=cqs2(ims,j),      &
                cpm=cpm(ims,j),znt=znt(ims,j),ust=ust(ims,j),          &
                mavail=mavail(ims,j),xland=xland(ims,j),    &
                hfx=hfx(ims,j),qfx=qfx(ims,j),lh=lh(ims,j),tsk=tsk(ims,j), &
                flhc=flhc(ims,j),flqc=flqc(ims,j),qgh=qgh(ims,j),      &
                qsfc=qsfc(ims,j),u10=u10(ims,j),v10=v10(ims,j),        &
                th2=th2(ims,j),t2=t2(ims,j),q2=q2(ims,j),        &
                svp1=svp1,svp2=svp2,svp3=svp3,svpt0=svpt0,             &
                ep1=ep1,ep2=ep2,karman=karman,fCor=fCor(ims,j),  &
                exch_temfx=exch_temf(ims,j),                     &
                hfx_force=hfx_force,lh_force=lh_force,tsk_force=tsk_force, &
                hfx_force_tend=hfx_force_tend,                         &
                lh_force_tend=lh_force_tend,                           &
                tsk_force_tend=tsk_force_tend,                         &
                dt=dt,itimestep=itimestep,                             &
                ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,     &
                ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,     &
                its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte      &
                                                                   )
      ENDDO

   END SUBROUTINE idealscmsfclay



   SUBROUTINE idealscmsfclay1d(j,u1d,v1d,th1d,qv1d,p1d, &
                pi1d,rho,z,zsrf,cp,g,rovcp,r,xlv,psfc,    &
                chs,chs2,cqs2,cpm,znt,ust,          &
                mavail,xland,hfx,qfx,lh,tsk, &
                flhc,flqc,qgh,qsfc,u10,v10,        &
                th2,t2,q2,svp1,svp2,svp3,svpt0,             &
                ep1,ep2,karman,fCor,  &
                exch_temfx,           &
                hfx_force,lh_force,tsk_force, &
                hfx_force_tend,lh_force_tend,tsk_force_tend, &
                dt,itimestep,                                 &
                ids,ide, jds,jde, kds,kde,                    &
                ims,ime, jms,jme, kms,kme,                    &
                its,ite, jts,jte, kts,kte                    &
                     )

      IMPLICIT NONE

      INTEGER,  INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        j
                                                               
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) ::             &
                                        u1d,v1d,qv1d,p1d,th1d,pi1d,rho,z,zsrf
      REAL,     INTENT(IN   ) ::        cp,g,rovcp,r,xlv
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: psfc,znt
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        chs,chs2,cqs2,cpm,ust
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: mavail,xland 
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        hfx,qfx,lh
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) :: tsk
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) ::             &
                                        flhc,flqc
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        qgh,qsfc
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) ::             &
                                        u10,v10,th2,t2,q2
      REAL,     INTENT(IN   ) ::        svp1,svp2,svp3,svpt0
      REAL,     INTENT(IN   ) ::        ep1,ep2,karman
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: fCor
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) :: exch_temfx
      REAL,     INTENT(INOUT) ::        hfx_force,lh_force,tsk_force
      REAL,     INTENT(IN   ) ::   hfx_force_tend,lh_force_tend,tsk_force_tend
      REAL,     INTENT(IN   ) :: dt
      INTEGER,  INTENT(IN   ) :: itimestep



   logical, parameter :: MFopt = .true.  
   real, parameter :: TEmin = 1e-3
   real, parameter :: ftau0 = 0.17
   real, parameter :: fth0 = 0.145
   real, parameter :: Cf = 0.185
   real, parameter :: CN = 2.0

   real, parameter :: Ceps = 0.070
   real, parameter :: Cgamma = Ceps
   real, parameter :: Cphi = Ceps

   real, parameter :: PrT0 = Cphi/Ceps * ftau0**2 / 2. / fth0**2

   integer :: i
   real :: e1
   real, dimension( its:ite)    ::  wstr, wm
   real, dimension( its:ite)    ::  z0t
   real, dimension( its:ite) :: dthdz, dqtdz, dudz, dvdz
   real, dimension( its:ite) :: lepsmin
   real, dimension( its:ite) :: thetav
   real, dimension( its:ite) :: N2, S, Ri, beta, fth, ratio
   real, dimension( its:ite) :: TKE, TE2
   real, dimension( its:ite) :: ustrtilde, linv
   real, dimension( its:ite) :: km, kh
   real, dimension( its:ite) :: qsfc_air





   do i = its,ite      

      
      

      
      hfx(i) = hfx_force
      lh(i)  = lh_force
      qfx(i) = lh(i) / xlv
      tsk(i) = tsk_force

      
      flhc(i) = hfx(i) / (tsk(i) - th1d(i)*pi1d(i))
      exch_temfx(i)  = flhc(i) / (rho(i) * cp)
      
      flqc(i) = exch_temfx(i) * mavail(i)

   end do  

   END SUBROUTINE idealscmsfclay1d


   SUBROUTINE idealscmsfclayinit( allowed_to_read )         

   LOGICAL , INTENT(IN)      ::      allowed_to_read

   END SUBROUTINE idealscmsfclayinit



END MODULE module_sf_idealscmsfclay
