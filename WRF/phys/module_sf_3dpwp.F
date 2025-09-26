!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_3dpwp

CONTAINS

!-------------------------------------------------------------------          
! cyl: Implemented a three-dimensional ocean model (DPWP)    Chiaying Lee RSMAS/UM        
!---------------------------------------------------------------------------          
       SUBROUTINE DPWP   (ims,ime, jms,jme, kms,kme,its,ite, jts,jte, kts,kte, &
                          ids,ide, jds,jde, kds,kde,okms, okme,                &
                          om_tmp,om_s,om_u, om_v, om_density, om_depth, om_ml,   &
                          om_lat, om_lon,                                      &
                          HFX, QFX, GSW, GLW, UST, U_PHY, V_PHY,               &
                          STBOLT, DELTSM, TSK, LH, XLAND,                      &
                          rdx, rdy, msfu, msfv, msft,xtime,om_tini,om_sini,id,omdt)
!----------------------------------------------------------------------------
        implicit none
!----------------------------------------------------------------------------
! Subroutine DPWP is a three-dimensional full physics ocean circulation model based 
! on Price et al. (1994, JPO). This ocean model is coupled with WRF    
! at every ocean time step. It is developed as a part of the University of Miami Coupled Model 
! (UMCM, Chen et al. 2012, JAS). A detailed description of the coupling process can
! be found in Lee and Chen (2013, MWR). 
!
! This ocean model can be used with multi-nested grids the same manner as in WRF. It is recommended 
! to use grid spacing > 3 km for the ocean model while WRF can be configured at smaller    
! grid spacing then the ocean model.
!
! The ocean model can be initialized with observed satellite SST or global model analyzed SST. 
! The and upper ocean temperature and sanility profiles can be initialized from climatology, 
! in-situ observations, and global ocean model analysis fields.
!
!-- om_tmp	3-dimensional ocean temperature (k)
!-- om_s	3-dimensional ocean salinity (psu) 
!-- om_u	3-dimensional ocean current in x-direction (ms-1)
!-- om_v	3-dimensional ocean current in y-direction (ms-1)
!-- om_depth	3-dimensional ocean depth (m)
!-- om_ml	ocean mixed layer depth (m)
!-- om_tini	temperature reference profile based on ocean initial condition (k)
!-- om_sini	salinity reference profile based on ocean initial condition (psu)
!-- om_dt 	ocean timestep (s)
!-------------------------------------------------------------------------------   

        !logical number to control the dynamics of ocean model
        integer :: noasym, nonlcl, nodeep, nopg, idia
        ! spatial variables
        INTEGER , INTENT(IN)        :: id
        INTEGER,  INTENT(IN ) ::    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte
        REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy,xtime
        integer :: i_start, i_end, j_start, j_end
        !physical variables from WRF
        REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft
        real    :: mrdx, mrdy
        real    :: pscale, hascale, vascale
        REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) ::HFX, QFX, LH, GSW, GLW, UST,&
                                                               XLAND
        REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TSK
        REAL, INTENT(IN   ) :: STBOLT, DELTSM
        REAL , DIMENSION( ims:ime ,kms:kme, jms:jme ),INTENT(IN   ) :: U_PHY, V_PHY
        !constant variables
        real    :: cp, crbc, cadv, ddx, g
        real    :: d2r, ro, rhoair, cpw,  rb, rg, udrag, ucon
        real , dimension (ims:ime, jms:jme) :: f, dml, tr, sr
        real :: beta1,beta2
        real    , dimension (ims:ime, jms:jme) :: dr, alpha, beta
        integer :: i, j, k, im, ip,jm,jp,km,kp,kk
        integer :: ii, ntss
        !ocean variables ::
        integer, intent(in) :: okms, okme
        real, dimension(ims:ime, okms:okme, jms:jme), INTENT(INOUT):: OM_TMP,OM_S,OM_U,OM_V,OM_DEPTH
        real, dimension(ims:ime, okms:okme, jms:jme), INTENT(IN):: OM_TINI,OM_SINI
        real, dimension(ims:ime, okms:okme, jms:jme), INTENT(INOUT):: OM_DENSITY
        real, dimension(ims:ime, okms:okme, jms:jme):: OM_W
        real, dimension(ims:ime, jms:jme),INTENT(INOUT):: OM_ML, OM_LAT, OM_LON
        real, dimension(ims:ime, okms:okme, jms:jme):: tt, st, ut, vt, t9, s9, u9,v9
        real, dimension(ims:ime, okms:okme, jms:jme):: pp, tentm, t0, s0, u0,v0
        real, dimension(ims:ime, okms:okme, jms:jme):: dz, tham, tvam
        real :: divu, divv, tha, sha, uha, vha, tva, vva, sva, uva, pgx, pgy
        real, dimension(okms:okme) :: tb5, sb5, ub5, vb5, tb4, sb4, ub4, vb4
        real, dimension(ims:ime,okms:okme, jms:jme) :: trr, srr
        real, dimension(ims:ime, okms:okme, jms:jme)::bbeta, aalpha,sg
        ! other variables
        real :: dt,  dti,dt7,vs, us,omdt
        real :: dtdy, dtdx, dsdy, dsdx, dudy, dudx, dvdy, dvdx, wavg, &
                dtdz, dsdz, dudz, dvdz
        integer :: iupw, kvs, kus, ia, ja

        i_start = max(its,ids+1)
        i_end   = min(ite,ide-2)
        j_start = max(jts,jds+1)
        j_end   = min(jte,jde-2)


       print*,'3DPWP domain',id,' run ocean'

       dtdy = 0.0
       dtdx = 0.0
       dsdy = 0.0
       dudy = 0.0
       dudx = 0.0
       dsdx = 0.0
       dvdy = 0.0
       dvdx = 0.0
       wavg = 0.0
       dtdz = 0.0
       dsdz = 0.0
       dudz = 0.0
       dvdz = 0.0
       divu = 0.0
       divv = 0.0
       tha = 0.0
       sha = 0.0
       uha = 0.0
       vha = 0.0
       tva = 0.0
       vva = 0.0
       sva = 0.0
       uva = 0.0
       pgx = 0.0
       pgy = 0.0 
       !----------------------------------------------------------------
       !  these flags control the dynamics of the model:
       !
       !  noasym = 1 shuts off the asymmetry of the storm winds
       !         = 2 also shuts off radial wind component of storm winds
       !  nonlcl = 1 shuts off horizontal advection,
       !         = 2 shuts off horizontal and vertical advection,
       !         = 3 above plus applies spatially constant wind stress
       !                (useful for testing 1-d dynamics)
       !  nodeep = 1 shuts off ml deepening by entrainment
       !  nopg = 1 shuts off pressure gradients
       !  idia = 1 turns on diagnostics
       ! to calculate 1D pwp just choose nonlcl = 2, and nopg = 1
       !
       !  set flags to default values
       noasym = 0
       nonlcl = 0
       nodeep = 0
       nopg = 0
       idia = 0
       !  set some scales that are used for diagnostics -  be careful with these !!
       pscale = 1.0
       if(nopg.eq.1) pscale = 0.
       hascale = 1.0
       if(nonlcl.ge.1) hascale = 0.
       vascale = 1.0
       if(nonlcl.ge.2) then
         vascale = 0.
         hascale = 0.
       end if
       !------------------------------------------------------------------

       ! calculate the expansion coefficient
       d2r = 2*3.14159/360.
       g = 9.8
       ro = 1.024e3   ! kg/m3
       rhoair = 1
       cpw = 4183.3
       beta1 = .6
       beta2 = 20.0
       rb = 0.65
       rg = 0.25
       cp = 1.5
  
       do i = i_start-1, i_end+1
         do j = j_start-1, j_end+1
          f (i,j)= 2.0*7.29e-5*sin(om_lat(i,j)*d2r)
          if (om_tmp(i,1,j) .gt. 0.0) then
            do k = 1,okme
               trr(i,k,j) = om_tini(i,k,j)
               srr(i,k,j) = om_sini(i,k,j)
               aalpha(i,k,j) = sgt(trr(i,k,j)+0.5, srr(i,k,j), sg(i,k,j))- &
               sgt(trr(i,k,j)-0.5,srr(i,k,j), sg(i,k,j))
               bbeta(i,k,j) = sgt(trr(i,k,j), srr(i,k,j)+0.5, sg(i,k,j) )-sgt(trr(i,k,j),srr(i,k,j)-0.5, sg(i,k,j))
            enddo
          endif
         enddo
       enddo

       udrag = 9999.
       ucon = 0.
       !set time step
       dt =omdt*60.0
       dti = 2.*dt
       ntss =2
       do  1111 ii = 1, ntss
         if (ii .eq. 2) go to 88
         do i= i_start-1, i_end+1
          do j = j_start-1, j_end+1
           do k = 1, okme
              tt(i,k,j) = 0.0
              ut(i,k,j) = 0.0
              vt(i,k,j) = 0.0
              st(i,k,j) = 0.0
              t9(i,k,j) = om_tmp(i,k,j)
              u9(i,k,j) = om_u(i,k,j)
              v9(i,k,j) = om_v(i,k,j)
              s9(i,k,j) = om_s(i,k,j)
              t0(i,k,j) = om_tmp(i,k,j)
              u0(i,k,j) = om_u(i,k,j)
              v0(i,k,j) = om_v(i,k,j)
              s0(i,k,j) = om_s(i,k,j)
              om_w(i,k,j) = 0.0
              pp(i,k,j) = 0.0
              tham(i,k,j) = 0.0
              tentm(i,k,j) = 0.0
            enddo
          enddo
         enddo

        go to 89
!second iteration
     88 continue
        dti = dt/2.
         do i = i_start-1, i_end+1
          do j = j_start-1, j_end+1
           do k = 1, okme
              t0(i,k,j) = t9(i,k,j)
              u0(i,k,j) = u9(i,k,j)
              v0(i,k,j) = v9(i,k,j)
              s0(i,k,j) = s9(i,k,j)
              om_w(i,k,j) = 0.0
              pp(i,k,j) = 0.0
              tham(i,k,j) = 0.0
              tentm(i,k,j) = 0.0
           enddo
          enddo
         enddo

    89 continue
      do 635 j = j_start, j_end
        do 635 i = i_start, i_end
                                                                                                      
         jp = j + 1
         jm = j - 1
         ip = i + 1
         im = i - 1

       if (om_tmp(ip,1,j) .le. 0 .or. om_tmp(im,1,j) .le. 0 .or. om_tmp(i,1,jp) .le. 0 .or. om_tmp(i,1,jm) .le.0 ) then

         do k = 1, okme
          om_w(i,k,j) = 0.0
         enddo
       else
         
         do 6381 k = 1, okme
           mrdx = rdx* msfu(i,j)
           mrdy = rdy* msfv(i,j)
           if (k .eq. 1) then
            dz(i,k,j) = om_depth(i,k,j)
           else
            dz(i,k,j) = om_depth(i,k,j)-om_depth(i,k-1,j)
           endif
           divu  = dz(i,k,j)*(om_u(ip,k,j)-om_u(im,k,j))*mrdx/2.0
           divv  = dz(i,k,j)*(om_v(i,k,jp)-om_v(i,k,jm))*mrdy/2.0
           om_w(i,k,j) = divu+divv
    6381 continue
          do k = 2, okme
           om_w(i,k,j) = om_w(i,k,j)+om_w(i,k-1,j)
          enddo
                                                                                                      
       endif
  635 continue
      do 6328 k = 1, okme
        if (jts .eq. jds) then
          do  i = its, ite
             om_w(i,k,jts) = om_w(i,k,jts+1)
          enddo
        endif
        if (jte .eq. jde-1) then
          do  i = its, ite
            om_w(i,k,jte) =  om_w(i,k,jte-1)
          enddo
        endif
        if (its .eq. ids) then
          do  j = jts, jte
             om_w(its,k,j) = om_w(its+1,k,j)
          enddo
        endif
        if (ite.eq. ide-1) then
          do  j = jts, jte
            om_w(ite,k,j) = om_w(ite-1,k,j)
          enddo
        endif
        if(its.eq. ids.and.jts .eq. jds) then
          om_w(its,k,jts) = om_w(its+1,k,jts+1)
        endif
        if(its.eq. ids.and.jte .eq. jde-1) then
          om_w(its,k,jte) = om_w(its+1,k,jte-1)
        endif
        if(ite.eq. ide-1.and.jte .eq. jde-1) then
          om_w(ite,k,jte) = om_w(ite-1,k,jte-1)
        endif
        if(ite.eq. ide-1.and.jts .eq. jds) then
          om_w(ite,k,jts) = om_w(ite-1,k,jts+1)
        endif


 6328 continue
! compute the pressure perturbation by integrating
      do 6327 i = i_start-1, i_end+1
      do 6327 j = j_start-1, j_end+1
        do k = 1, okme
           if (k .eq. 1) then
             dz(i,k,j) = om_depth(i,k,j)
           else
             dz(i,k,j) = om_depth(i,k,j)-om_depth(i,k-1,j)
           endif
        enddo

        if (om_tmp(i,1,j) .gt. 0) then 
           pp (i,okme,j) = -g*dz(i,okme,j)*(aalpha(i,okme,j)*(om_tmp(i,okme,j)-trr(i,okme,j))+ &
                           bbeta(i,okme,j)*(om_s(i,okme,j)-srr(i,okme,j)))

           do 632 k = 1,okme-1
             kk = okme - k
             pp(i,kk,j) = -g*dz(i,kk,j)*(aalpha(i,kk,j)*(om_tmp(i,kk,j)-trr(i,kk,j))+ &
                          bbeta(i,kk,j)*(om_s(i,kk,j)-srr(i,kk,j)))+pp(i,kk+1,j)

   632     continue
        endif 


 6327 continue
        do 410 j = j_start, j_end
           jp = j+1
           jm = j-1
           do 410 i = i_start, i_end
              ip = i + 1
              im = i - 1
              do 420 k = 1 ,okme
                 mrdx = rdx* msfu(i,j)
                 mrdy = rdy* msfv(i,j)
                 tha = 0.
                 sha = 0.
                 uha = 0.
                 vha = 0.

                 tva = 0.
                 sva = 0.
                 vva = 0.
                 uva = 0.

                 pgx = 0.
                 pgy = 0.
! estimate the tendencies due to horizontal advection
                 IUPW = 1
                 if (IUPW .eq. 1) then
                    vs = sign(1.,om_v(i,k,j))
                    kvs = -1* nint(vs)
                    us = sign(1.,om_u(i,k,j))
                    kus = -1* nint(us)
                    dtdy = vs*(om_tmp(i,k,j)-om_tmp(i,k,j+kvs))*mrdy
                    dtdx = us*(om_tmp(i,k,j)-om_tmp(i+kus,k,j))*mrdx


                    dsdy = vs*(om_s(i,k,j)-om_s(i,k,j+kvs))*mrdy
                    dsdx = us*(om_s(i,k,j)-om_s(i+kus,k,j))*mrdx


                    dudy = vs*(om_u(i,k,j)-om_u(i,k,j+kvs))*mrdy
                    dudx = us*(om_u(i,k,j)-om_u(i+kus,k,j))*mrdx


                    dvdy = vs*(om_v(i,k,j)-om_v(i,k,j+kvs))*mrdy
                    dvdx = us*(om_v(i,k,j)-om_v(i+kus,k,j))*mrdx

                    tha =-(om_v(i,k,j)*dtdy + om_u(i,k,j)*dtdx)
                    sha =-(om_v(i,k,j)*dsdy + om_u(i,k,j)*dsdx)
                    uha =-(om_v(i,k,j)*dudy + om_u(i,k,j)*dudx)
                    vha =-(om_v(i,k,j)*dvdy + om_u(i,k,j)*dvdx)
                 endif
! estimate the tendencies due to vertical advection
! skip if k = 1, assume that there is no vertical advection at surface

                 if ( k .eq. 1) go to 3323
                    wavg = 0.5*(om_w(i,k-1,j) + om_w(i,k,j))
                                                                                                      
                    km = k - 1
                    kp = k + 1
                    if (k .eq. okme) kp = k
                       dtdz = (om_tmp(i,km,j)-om_tmp(i,kp,j))/(om_depth(i,km,j)-om_depth(i,kp,j))
                       dsdz = (om_s(i,km,j)-om_s(i,kp,j))/(om_depth(i,km,j)-om_depth(i,kp,j))
                       dudz = (om_u(i,km,j)-om_u(i,kp,j))/(om_depth(i,km,j)-om_depth(i,kp,j))
                       dvdz = (om_v(i,km,j)-om_v(i,kp,j))/(om_depth(i,km,j)-om_depth(i,kp,j))


                       tva =wavg*dtdz
                       sva =wavg*dsdz
                       uva =wavg*dudz
                       vva =wavg*dvdz
  3323           continue


!calculate pressure gradient
                 pgx = (pp(ip,k,j) - pp(im,k,j))*mrdx/2.0
                 pgy = (pp(i,k,jp) - pp(i,k,jm))*mrdy/2.0

                 if (( i .eq. its .and. its .eq. ids) .or. (i .eq. ite .and. ite .eq. ide-1) .or.    &
                    ( j .eq. jts .and. jts .eq. jds) .or. (j .eq. jte .and. jte .eq. jde-1))  then


                     tt(i,k,j) = tt(i,k,j)
                     st(i,k,j) = st(i,k,j)
                     ut(i,k,j) = ut(i,k,j)
                     vt(i,k,j) = vt(i,k,j)
                 else


                     tt(i,k,j) = tt(i,k,j) + tha*hascale + tva*vascale
                     st(i,k,j) = st(i,k,j) + sha*hascale + sva*vascale
                     ut(i,k,j) = ut(i,k,j) + uha*hascale + uva*vascale  - pgx*pscale/1023.0
                     vt(i,k,j) = vt(i,k,j) + vha*hascale + vva*vascale  - pgy*pscale/1023.0
                 endif                                                                                           
    420       continue

    410 continue
                                                                                                      


        do 700 k = 1, okme
           if (jts .eq. jds)then
              ja = 1
              do 794 i = its, ite
                crbc = -cp*rdy*msfv(i,j)
                j = jts
                tt(i,k,j) = tt(i,k,j) + crbc*(om_tmp(i,k,j) - om_tmp(i, k, j+ja))
                st(i,k,j) = st(i,k,j) + crbc*(om_s(i,k,j) - om_s(i, k, j+ja))
                ut(i,k,j) = ut(i,k,j) + crbc*(om_u(i,k,j) - om_u(i, k, j+ja))
                vt(i,k,j) = vt(i,k,j) + crbc*(om_v(i,k,j) - om_v(i, k, j+ja))
    794       continue
           endif
           ja = -1
           if (jte .eq. jde-1)then
              do 795 i = its, ite
                 crbc = -cp*rdy*msfv(i,j)
                 j = jte
                 tt(i,k,j) = tt(i,k,j) + crbc*(om_tmp(i,k,j) - om_tmp(i, k, j+ja))
                 st(i,k,j) = st(i,k,j) + crbc*(om_s(i,k,j) - om_s(i, k, j+ja))
                 ut(i,k,j) = ut(i,k,j) + crbc*(om_u(i,k,j) - om_u(i, k, j+ja))
                 vt(i,k,j) = vt(i,k,j) + crbc*(om_v(i,k,j) - om_v(i, k, j+ja))
    795       continue
           endif
           if (its .eq. ids)then
              do 796 j = jts , jte
                 ia = 1
                 crbc = -cp*(rdx*msfu(i,j))
                 i = its
                 tt(i,k,j) = tt(i,k,j) + crbc*(om_tmp(i,k,j) - om_tmp(i+ia, k, j))
                 st(i,k,j) = st(i,k,j) + crbc*(om_s(i,k,j) - om_s(i+ia, k, j))
                 ut(i,k,j) = ut(i,k,j) + crbc*(om_u(i,k,j) - om_u(i+ia, k, j))
                 vt(i,k,j) = vt(i,k,j) + crbc*(om_v(i,k,j) - om_v(i+ia, k, j))
    796       continue
           endif
           if (ite .eq. ide-1)then
              do 797 j = jts , jte
                 ia = -1
                 crbc = -cp*(rdx*msfu(i,j))
                 i = ite
                 tt(i,k,j) = tt(i,k,j) + crbc*(om_tmp(i,k,j) - om_tmp(i+ia, k, j))
                 st(i,k,j) = st(i,k,j) + crbc*(om_s(i,k,j) - om_s(i+ia, k, j))
                 ut(i,k,j) = ut(i,k,j) + crbc*(om_u(i,k,j) - om_u(i+ia, k, j))
                 vt(i,k,j) = vt(i,k,j) + crbc*(om_v(i,k,j) - om_v(i+ia, k, j))
    797       continue
           endif

           if (its .eq. ids .and. jts .eq. jds) then
              tt(its,k,jts) = tt(its+1,k,jts+1)
           endif
           if (its .eq. ids .and. jte .eq. jde-1) then
              tt(its,k,jte) = tt(its+1,k,jte-1)
           endif
           if (ite .eq. ide-1 .and. jte .eq. jde-1) then
              tt(ite,k,jte) = tt(ite-1,k,jte-1)
           endif
           if (ite .eq. ide-1 .and. jts .eq. jds) then
              tt(ite,k,jts) = tt(ite-1,k,jts+1)
           endif

     700  enddo


                                                                                                      
!!end radiation bc
! apply the wind stress and air-sea heat hluxes, and deepen the mixed layer by 1Dpwp
                                                                                                      
!  do 510 i = i_start, i_end
!        do 510 j = j_start, j_end
  do 510 i = its, ite
        do 510 j = jts, jte
         if (ii .eq. 1) then
           do k = 1, okme
           tb5(k) = om_tmp(i,k,j)
            sb5(k) = om_s(i,k,j)
            ub5(k) = om_u(i,k,j)
            vb5(k) = om_v(i,k,j)
            tb4(k) = tb5(k)
            sb4(k) = sb5(k)
            ub4(k) = ub5(k)
            vb4(k) = vb5(k)
           enddo
         endif
         if (ii .eq. 2) then
          do k = 1, okme
           tb5(k) =t0(i,k,j)
           sb5(k) =s0(i,k,j)
           ub5(k) =u0(i,k,j)
           vb5(k) =v0(i,k,j)
           tb4(k) = tb5(k)
           sb4(k) = sb5(k)
           ub4(k) = ub5(k)
           vb4(k) = vb5(k)
          enddo
         endif

! vertical mixing
          call PWP(i,j,k,ims, ime, jms, jme, okms,okme,tb4,&
            sb4,om_density(i,:,j),om_depth(i,:,j),ub4,&
            vb4,om_ml(i,j),om_lat(i,j),&
            om_lon(i,j),HFX(i,j), QFX(i,j),LH(i,j), GSW(i,j),&
            GLW(i,j), UST(i,j),U_PHY(i,kts,j),V_PHY(i,kts,j),&
            STBOLT,DELTSM,aalpha(i,:,j),bbeta(i,:,j),om_tini(i,:,j),&
            om_sini(i,:,j),trr(i,:,j),srr(i,:,j),omdt)
! add mixing tendencies
       do  k = 1 , okme
         tt(i,k,j) = tt(i,k,j) + (tb4(k)-tb5(k))/dt
         st(i,k,j) = st(i,k,j) + (sb4(k)-sb5(k))/dt
         ut(i,k,j) = ut(i,k,j) + (ub4(k)-ub5(k))/dt
         vt(i,k,j) = vt(i,k,j) + (vb4(k)-vb5(k))/dt


       enddo

   510 continue
       do 24 i = its, ite
       do 24 j = jts, jte
       do 24 k = 1, okme
        om_tmp(i,k,j) = t0(i,k,j) + dti*tt(i,k,j)
        om_s(i,k,j) = s0(i,k,j) + dti*st(i,k,j)
        om_u(i,k,j) = u0(i,k,j) + dti*ut(i,k,j)
        om_v(i,k,j) = v0(i,k,j) + dti*vt(i,k,j)
    24 continue

      do  i = its, ite
       do  j = jts, jte


        if (XLAND(i,j).GE.1.5)then


         TSK(i,j) = om_tmp(i, 1, j)
        endif


       enddo
       enddo
 1111 enddo
      return

    END subroutine DPWP
! cyl: end 3D PWP 
!------------------------------------------------------------------
! cyl: add 1D PWP 
        SUBROUTINE PWP(i,j,k,ims, ime, jms, jme, okms,okme,om_tmp,om_s, &
        om_density, om_depth,om_u, om_v,om_ml,om_lat, om_lon,    &
         HFX, QFX,LH, GSW, GLW, UST,UAIR,VAIR,STBOLT,DELTSM,aalpha,bbeta,&
         om_tini, om_sini,trr,srr,omdt)
        implicit none
!----------------------------------------------------------------------------
! Subroutine PWP is a one-dimensional vertical mixing scheme used in DPWP 
! based on on Price et al. (1994, JPO).   
!----------------------------------------------------------------------------


        integer i, j, k                                  
        integer ,intent(in) ::ims, ime, jms, jme, okms,okme            !dimensions
        integer ::ml,kml,mml                               
        real, dimension(okms:okme), intent(inout):: om_tmp, om_s,                &
              om_density,om_depth
        real, dimension(okms:okme),intent(inout):: om_u, om_v
                                                      
        real,intent(inout) :: om_ml         
        real,intent(inout) :: om_lat, om_lon
        real TSK
        real,intent(in) :: HFX, QFX,LH, GSW, GLW, ust,STBOLT,DELTSM
                                                      
        real, dimension(okms:okme)::absrb,u,v
        real dt, dz                                      
        real  :: tauxair, tauyair, taux, tauy,                     &
              wspd
                                                      
        real d2r, g, hcon, f, tr, sr, dr, alpha, beta, energy, sg, &
            ang,dml
                                                     
        real ro, rhoair, cpw                             
        real beta1, beta2, ql, qi
        real sipe
        real rb,rv, rvc, delr, duv,rg,du,dv
        real sa,ca,uair,vair,rc
        real udrag, ucon
        real, intent(in) :: omdt
        real,  dimension(okms:okme),intent(in)::aalpha,bbeta, om_tini,om_sini,trr,srr


        dt = omdt*60.0
        d2r = 2*3.14159/360.
        g = 9.8
        ro = 1.024e3   ! kg/m3
        rhoair = 1
        cpw = 4183.3
        f = 2*7.29e-5*sin(om_lat*d2r)
        beta1 = .6
        beta2 = 20.0
        rb = 0.65
        rg = 0.25
        dml = om_ml
        tr = trr(1)
        sr = srr(1)
        dr = sgt(tr,sr,sg)  ! unit of density is sigma
        alpha = sgt(tr+0.5, sr, sg )-sgt(tr-0.5,sr, sg)
        beta = sgt(tr, sr+0.5, sg )-sgt(tr,sr-0.5, sg)
        udrag = 9999.
        ucon = 0.
        if (udrag .lt. 100) ucon = 1./(86400*udrag)
           call absorb(i,j, ims, ime,jms,jme,okms,okme, beta1,beta2, &
                       absrb, om_depth,GSW,GLW)
           ql = -1.*(LH+HFX+STBOLT*om_tmp(1)**4)         ! corrected V3.8
           qi = GLW+GSW
           ql = ql+absrb(1)*qi
           om_tmp(1) = om_tmp(1) + dt*ql/(om_depth(1)*ro*cpw)
           om_s(1)=om_s(1)+om_s(1)*QFX*dt/ro/om_depth(1) ! corrected V3.8
           do k =  2, okme
              dz = om_depth(k)-om_depth(k-1)
              om_tmp(k) = om_tmp(k) + dt*qi*absrb(k)/(dz*ro*cpw)
           enddo
           do k = 1,  okme
              om_density(k) = (dr+1000.0) +alpha*(om_tmp(k)-tr)+      &
                              beta*(om_s(k)-sr)
           enddo
           ml = 0
           call mldep(i,j,k,ims,ime,jms,jme,okms,okme, om_density, &
                     om_depth, ml)
           if (dml .gt. 100) print*,'mldep1',dml
           call si (i, j, ims,ime,jms,jme,okms,okme,om_density,    &
                   om_depth,sipe, ml)

          if (ml .gt. 1) then
             call mix1(om_tmp, ml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
             call mix1(om_s, ml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
             call mix1(om_u, ml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
             call mix1(om_v, ml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
             call mix1(om_density, ml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
          endif
           ! above this, should be statically stable 

          ang = f*dt/2.
          sa = sin(ang)
          ca = cos(ang)
          do k = 1, okme
             call rot(i, j,ims,ime,jms,jme,okms,okme, om_u(k), om_v(k), sa, ca)
          enddo
          call mldep(i,j,k, ims,ime,jms,jme,okms,okme, om_density, &
                    om_depth, ml)
          if (ml .eq. 1 ) then
             om_ml = om_depth(ml)
          else
             om_ml = om_depth(ml)+(om_depth(ml) - om_depth(ml-1))/2.
          endif
          dml = om_ml
          wspd = max(sqrt(uair*uair+vair*vair), 0.1)
          tauxair = ust*ust*uair/wspd
          taux = rhoair/ro*tauxair
          tauyair = ust*ust*vair/wspd
          tauy = rhoair/ro*tauyair
          du = taux*dt/dml
          dv = tauy*dt/dml
          do k = 1,ml
             om_u(k) = om_u(k) + du
             om_v(k) = om_v(k) + dv
          enddo
          if(ucon .gt. 1.e-10) then
            do k = 1,okme
               om_u(k) = om_u(k)*(1. - dt*ucon)
               om_v(k) = om_v(k)*(1. - dt*ucon)
            enddo
          endif
! In pwp origional code, Dr. Price turns off the diffusion
! To use it, check the diffusion parameter in subroutine first
!         call diffusion(okms,okme,dt,om_tmp,om_depth)
!         call diffusion(okms,okme,dt,om_s,om_depth)
!         call diffusion(okms,okme,dt,om_u,om_depth)
!         call diffusion(okms,okme,dt,om_v,om_depth)
!         call diffusion(okms,okme,dt,om_density,om_depth)
!         print*,'ca,sa',ca,sa
         if(rb .le. 0.00001) go to 50
          rvc = rb
         do 40 k = ml, okme-1
           if (k .eq. 1) then
           dz = om_depth(k)
           else
           dz =  om_depth(k) + (om_depth(k) - om_depth(k-1))/2.
           endif
           delr = (om_density(k+1)-om_density(k))/ro
           duv =(om_u(k+1)-om_u(k))**2+(om_v(k+1)             &
               -om_v(k))**2 + 1.e-8
           rv = g*delr*dz/duv
           if (rv .gt. rvc) go to 41
           mml=k+1
           call mix1(om_tmp, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
           call mix1(om_s, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
           call mix1(om_u, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
           call mix1(om_v, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
           call mix1(om_density, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
  40  continue
  41  continue
  50  continue
      if (rg .gt. 1.e-4) call rimix(i,j,k,ims,ime,jms,jme,okms,    &
       okme,rc,                                                &
         rg,om_density,om_depth, om_u, om_v, om_s, om_tmp)
         do k = 1, okme
          call rot(i,j,ims,ime,jms,jme,okms,okme, om_u(k), om_v(k), &
           sa, ca)
         enddo
      call mldep(i,j,k, ims,ime,jms,jme,okms,okme, om_density, &
       om_depth, ml)
      if (ml .eq. 1) then
       om_ml = om_depth(ml)
      else
      om_ml = om_depth(ml)+ (om_depth(ml) - om_depth(ml-1))/2.
      endif
!      TSK = om_tmp(1)
      return
      END SUBROUTINE PWP
! cyl : end PWP
! cyl : PWP's subrouting  
! cyl--------------------
SUBROUTINE absorb(i,j, ims, ime, jms, jme, okms,okme,beta1, &
       beta2, absrb, om_depth,GSW,GLW)
      implicit none
      integer i,j,k
      integer ims, ime, jms, jme, okms, okme
      real, dimension(okms:okme),intent(inout)::absrb, om_depth
      real abss, rs1, rs2, beta1, beta2, z1b1, z2b1, z2b2, z1b2,&
          z1,z2,GSW,GLW
      abss = 0.
      rs1 = GSW/(GSW+GLW)
      rs2 = 1.0 - rs1
      do k = 1, okme
       absrb(k) = 0.
      enddo
      do k =1, okme
       if (k .eq. 1) then
        z1 = 0
       else
       z1 = om_depth(k-1)
       endif
      z2 = om_depth(k)
      z1b1 = z1/beta1
      z2b1 = z2/beta1
      z1b2 = z1/beta2
      z2b2 = z2/beta2
      if (z2b1 .lt. 70) absrb(k) = absrb(k) +          &
          rs1*(exp(-z1b1)-exp(-z2b1))
      if (z2b2 .lt. 70) absrb(k) = absrb(k) +          &
          rs2*(exp(-z1b2)-exp(-z2b2))
      abss = abss + absrb(k)
      enddo
      return
      END SUBROUTINE absorb
!cyl--------------------
      SUBROUTINE mldep (i,j,k, ims,ime,jms,jme,okms,okme, om_density, &
                    om_depth, ml)
      integer i,j,k,kk, ims,ime,jms,jme,okms,okme
      real deps, dml, dd
      integer ml
      real, dimension(okms:okme),intent(inout)::om_density,om_depth
      deps = 1.e-5
      do 70 k = 1, okme-1
       dd = abs(om_density(k+1) - om_density(k))
        if (dd .gt. deps) go to 71
   70 continue
   71 continue
        ml = k
      return
      END SUBROUTINE mldep
!cyl--------------------
      SUBROUTINE si(i, j, ims,ime,jms,jme,okms,okme,om_density, &
             om_depth,                                          &
             sipe, ml)
      integer i, j, k, ims,ime,jms,jme,okms,okme, mml,ml
      real, dimension(okms:okme),intent(inout)::om_density, om_depth
      real,  dimension(okms:okme) :: temp_density
      real pesum, sipe
      do k = 1, okme
       temp_density(k) = om_density(k)
      enddo
      mml = 1
      do 80 k = 2, okme
      if (om_density(k) .gt. om_density(k-1)) go to 81
      if (om_density(k) .lt. om_density(k-1))then
      mml = k
      call mix1(om_density, mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
      endif
  80  continue
  81  continue
      ml = mml

      return
      END SUBROUTINE si
!cyl--------------------
      SUBROUTINE mix1(b,mml, i, j,ims,ime,jms,jme,okms,okme,om_depth)
      integer  i, j, k, ims,ime,jms,jme,okms,okme, mml
      real,dimension(okms:okme),intent(inout) :: b
      real,dimension(okms:okme),intent(in) :: om_depth
      real,dimension(okms:okme) :: dz
      real bs,zs
      do k = 1, okme
       if (k .eq. 1) then
            dz(k) = om_depth(k)
           else
            dz(k) = om_depth(k)-om_depth(k-1)
       endif
      enddo
      bs = 0.
      zs = 0.
      do k = 1, mml
       bs = bs + b(k)*dz(k)
       zs = zs + dz(k)
      enddo
      bs = bs/zs
      do k = 1, mml
       b(k) = bs
      enddo
      return
      END SUBROUTINE mix1
!cyl--------------------
      SUBROUTINE rot(i, j,ims,ime,jms,jme,okms,okme, om_u, om_v, sa, ca)
      integer i, j, k, ims,ime,jms,jme,okms,okme
      real,intent(inout)::om_u, om_v
      real:: u0, v0
      real sa, ca
       u0= om_u
       v0= om_v
       om_u = u0*ca+v0*sa
       om_v = v0*ca-u0*sa
      return
      END SUBROUTINE rot
!cyl--------------------
      SUBROUTINE rimix(i,j,k,ims,ime,jms,jme,okms,okme,rc, rg,  &
          om_density,om_depth, om_u, om_v, om_s, om_tmp)
      implicit none
      integer i,j,k,ims,ime,jms,jme,okms,okme,kcd,nmix,k1,k2,ks
      real rc, rg,dd,g,dz,dv,ro,rs
      real,dimension(okms:okme) :: r
      real, dimension(okms:okme),intent(inout)::om_density, om_u,             &
             om_depth,om_v, om_s, om_tmp
      ro = 1000.
      rc = rg
      g = 9.8
      kcd = 0
      nmix = 0
      k1 = 1
      k2 = okme - 1


   10 continue
      do 1 k = k1, k2
       dd = om_density(k+1)-om_density(k)
       if (dd .lt. 1.e-3) dd = 1.e-3
        dv = (om_u(k+1)-om_u(k))**2 + (om_v(k+1)-om_v(k))**2
       if(dv .lt. 1.e-6) dv = 1.e-6
       if (k .eq. 1) then
        dz = (om_depth(1) + om_depth(2) - om_depth(1))/2.
       else
        dz = (om_depth(k)-om_depth(k-1) + om_depth(k+1) - om_depth(k))/2.
       endif
       r(k) = g*dz*dd/(dv*ro)
   1  continue
      rs = r(1)
      ks = 1
      do 2 k = 2, okme-1
      if (r(k) .lt. rs) go to 3
      go to 2
   3  continue
      rs =r(k)
      ks = k
   2  continue
      if (rs .ge. rc) go to 99
      if (ks .ge. kcd ) kcd = ks + 1
      call stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs, om_tmp, ks,om_depth)
      call stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs, om_s, ks,om_depth)
      call stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs, om_density, ks,om_depth)
      call stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs, om_u, ks,om_depth)
      call stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs, om_v, ks,om_depth)
      nmix = nmix +1
      k1 = ks - 2
      if (k1 .lt. 1) k1 = 1
      k2 = ks +2
      if (k2 .gt. okme-1) k2 = okme -1
      go to 10
   99 continue


      return
      END SUBROUTINE rimix
!cyl--------------------
      SUBROUTINE diffusion(okms,okme,dt,a,om_depth)
      implicit none
      integer okms,okme,k
      real dt,rkz,dz
      real, dimension(okms:okme):: a, work,dconst,om_depth
       rkz =0.3
       do k = okms, okme
       if (k .eq. 1) then
        dz = om_depth(1)
       else
        dz = om_depth(k)-om_depth(k-1)
       endif
       dconst(k) = dt*rkz/dz**2
       enddo
      do k = 2, okme-1
        work(k) = dconst(k)*(a(k-1)+a(k+1)-2.*a(k))
      enddo
      do k = 2, okme-1
       a(k) = a(k) + work(k)
      enddo
      return
      END SUBROUTINE diffusion
!cyl--------------------
      SUBROUTINE stir(i,j,ims,ime,jms,jme,okms,okme,rc,rs,a,ks,depth)
      implicit none
      integer i,j,k,ims,ime,jms,jme,okms,okme,ks,b
      real, dimension(okms:okme),intent(inout)::a
      real, dimension(okms:okme),intent(in)::depth
      real, dimension(okms:okme)::dz
      real rcon,rc,rs,ff,rnew,da
       rnew = 0.3
       ff = 1. - rs/rnew
       do k = 1,okme
         if (k.eq. 1) then
          dz(k) = depth(1)
         else
          dz(k) = depth(k) -depth(k-1)
         endif
       enddo
       da = (a(ks+1)- a(ks))*ff/2.
       b=2./(1.+dz(ks)/dz(ks+1))
       a(ks+1)= a(ks+1)-da*b*dz(ks)/dz(ks+1)
       a(ks) = a(ks)+da*b
!       print*,'da',da
!       print*,'a',a(ks+1),a(ks)
       return
      END SUBROUTINE stir
!cyl--------------------
      FUNCTION sg0(s)
      implicit none
      real s,sg0
      sg0 = ((6.76786136e-6*s-4.8249614e-4)*s+0.814876577)*s         &
            -0.0934458632
      return
      END FUNCTION sg0
      FUNCTION sgt(tt,s,sg)
      implicit none
      real tt,t, s, sg,sgt
      t=tt-273.0
      sg = sg0(s)
   20 sgt = ((((-1.43803061e-7*t-1.98248399e-3)*t-0.545939111)*t      &
             +4.53168426)*t)/(t+67.26)+((((1.667e-8*t-8.164e-7)*t     &
            +1.803e-5)*t)*sg+((-1.0843e-6*t+9.8185e-5)*t-4.7867e-3)*t &
            +1.0)*sg
      return
      END FUNCTION sgt

!--------------------
END MODULE module_sf_3dpwp
