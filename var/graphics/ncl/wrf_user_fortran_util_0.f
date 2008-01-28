C NCLFORTSTART
      subroutine compute_pi( pi, pressure, nx, ny, nz )
      implicit none
      integer nx,ny,nz
      real    pi(nx,ny,nz)
      real    pressure(nx,ny,nz)
C NCLEND

      integer i,j,k
      real p1000mb, r_d, cp
      parameter ( p1000mb = 100000., r_d = 287., cp = 7.*r_d/2. )

         do k=1,nz
         do j=1,ny         
         do i=1,nx
          pi(i,j,k)=(pressure(i,j,k)/p1000mb)**(r_d/cp)
         enddo
         enddo
         enddo

      end

C NCLFORTSTART
      subroutine compute_tk( tk, pressure, theta, nx, ny, nz )
      implicit none
      integer nx,ny,nz
      real    pi
      real    pressure(nx,ny,nz)
      real    theta(nx,ny,nz)
      real    tk(nx,ny,nz)
C NCLEND

      integer i,j,k
      real p1000mb, r_d, cp
      parameter ( p1000mb = 100000., r_d = 287., cp = 7.*r_d/2. )

         do k=1,nz
         do j=1,ny         
         do i=1,nx
          pi=(pressure(i,j,k)/p1000mb)**(r_d/cp)
          tk(i,j,k) = pi*theta(i,j,k)
         enddo
         enddo
         enddo

      end

C NCLFORTSTART
      subroutine compute_tk_2d( tk, pressure, theta, nx, ny )
      implicit none
      integer nx,ny
      real    pi
      real    pressure(nx,ny)
      real    theta(nx,ny)
      real    tk(nx,ny)
C NCLEND

      integer i,j
      real p1000mb, r_d, cp
      parameter ( p1000mb = 100000., r_d = 287., cp = 7.*r_d/2. )

         do j=1,ny
         do i=1,nx
          pi=(pressure(i,j)/p1000mb)**(r_d/cp)
          tk(i,j) = pi*theta(i,j)
         enddo
         enddo

      end

C NCLFORTSTART
      subroutine interp_3dz( v3d, v2d, z, loc, 
     &                       nx,  ny,  nz      )
      implicit none
      integer nx,ny,nz
      real    v3d(nx,ny,nz), v2d(nx,ny)
      real    z(nx,ny,nz)
      real    loc
C NCLEND

      integer i,j,kp, ip, im
      logical interp
      real    height, w1, w2

      height = loc

c does vertical coordinate increase or decrease with increasing k?
c set offset appropriately

      ip = 0
      im = 1
      if (z(1,1,1) .gt. z(1,1,nz)) then
        ip = 1
        im = 0
      endif

      DO i=1,nx
      DO j=1,ny

        interp = .false.
        kp = nz

        DO WHILE ( (.not. interp) .and. (kp .ge. 2) )

          IF(   ((z(i,j,kp-im) .le. height) .and. 
     &           (z(i,j,kp-ip) .gt. height))             )   THEN
            w2 = (height-z(i,j,kp-im))/(z(i,j,kp-ip)-z(i,j,kp-im))
            w1 = 1.-w2
            v2d(i,j) = w1*v3d(i,j,kp-im) + w2*v3d(i,j,kp-ip)
            interp = .true.
          END IF
        kp = kp-1

        ENDDO

      ENDDO
      ENDDO

      RETURN
      END

C NCLFORTSTART
      subroutine z_stag( znew, nx, ny, nz,
     &                   z,   nxz,  nyz,  nzz,
     &                   terrain                )
      implicit none
      integer nx,ny,nz,nxz,nyz,nzz
      real    znew(nx,ny,nz), z(nxz,nyz,nzz)
      real    terrain(nxz,nyz)
C NCLEND

      integer i,j,k, ii, im1, jj, jm1

c check for u, v, or w (x,y,or z) staggering

      if(nx .gt. nxz) then  ! for x and y stag, avg z to x, y, point

       do k=1,nz
       do j=1,ny
       do i=1,nx
         ii = min0(i,nxz)
         im1 = max0(i-1,1)
         znew(i,j,k) = 0.5*(z(ii,j,k)+z(im1,j,k))
       enddo
       enddo
       enddo

      else if(ny .gt. nyz) then

       do k=1,nz
       do j=1,ny
         jj = min0(j,nyz)
         jm1 = max0(j-1,1)
       do i=1,nx
         znew(i,j,k) = 0.5*(z(i,jj,k)+z(i,jm1,k))
       enddo
       enddo
       enddo

      else if(nz .gt. nzz) then  ! w (z) staggering

       do j=1,ny
       do i=1,nx
        znew(i,j,1) = terrain(i,j)
       enddo
       enddo

       do k=2,nz
       do j=1,ny
       do i=1,nx
        znew(i,j,k) = znew(i,j,k-1) + 2.*(z(i,j,k-1)-znew(i,j,k-1))
       enddo
       enddo
       enddo

      endif

      return
      end

C NCLFORTSTART
      subroutine interp_2d_xy( v3d, v2d, xy, 
     &                         nx,  ny,  nz, nxy  )
      implicit none
      integer nx,ny,nz,nxy
      real    v3d(nx,ny,nz), v2d(nxy,nz)
      real    xy(2,nxy)
C NCLEND

      integer i,j,k,ij
      real    w11, w12, w21, w22, wx, wy

      do ij=1,nxy

        i = max0(1,min0(nx-1,int(xy(1,ij)+1)))
        j = max0(1,min0(ny-1,int(xy(2,ij)+1)))
        wx = float(i+1) - (xy(1,ij)+1)
        wy = float(j+1) - (xy(2,ij)+1)
        w11 = wx*wy
        w21 = (1.-wx)*wy
        w12 = wx*(1.-wy)
        w22 = (1.-wx)*(1.-wy)
        do k=1,nz
          v2d(ij,k) = w11*v3d(i,j  ,k)+w21*v3d(i+1,j  ,k)
     &               +w12*v3d(i,j+1,k)+w22*v3d(i+1,j+1,k)
        enddo
      enddo

      return
      end

C NCLFORTSTART
      subroutine interp_1d( v_in, v_out,
     &                      z_in, z_out, nz_in, nz_out )
      implicit none
      integer nz_in, nz_out
      real    v_in(nz_in), z_in(nz_in)
      real    v_out(nz_out), z_out(nz_out)
C NCLEND

      integer kp, k, im, ip
      logical interp, increasing_z 
      real    height, w1, w2

c does vertical coordinate increase of decrease with increasing k?
c set offset appropriately

      ip = 0
      im = 1
      if (z_in(1) .gt. z_in(nz_in)) then
        ip = 1
        im = 0
      endif

      do k=1, nz_out

         interp = .false.
         kp = nz_in
         height = z_out(k)

         DO WHILE ( (.not. interp) .and. (kp .ge. 2) )

         IF(   ((z_in(kp-im) .le. height) .and. 
     &          (z_in(kp-ip) .gt. height))             )   THEN
           w2 = (height-z_in(kp-im))/(z_in(kp-ip)-z_in(kp-im))
           w1 = 1.-w2
           v_out(k) = w1*v_in(kp-im) + w2*v_in(kp-ip)
           interp = .true.
         END IF
         kp = kp-1

         ENDDO

      ENDDO

      RETURN
      END

c---------------------------------------------

c Bill,
c This routine assumes
c    index order is (i,j,k)
c    wrf staggering
c    units: pressure (Pa), temperature(K), height (m), mixing ratio (kg kg{-1})
c    availability of 3d p, t, and qv; 2d terrain; 1d half-level zeta string
c    output units of SLP are Pa, but you should divide that by 100 for the
c          weather weenies.
c    virtual effects are included
c   
c I have done no testing on this routine.  Please feel free to give me a call
c on Monday  208 362-9747
c Dave

C NCLFORTSTART
      SUBROUTINE compute_seaprs ( nx , ny , nz  ,
     &                            z, 
     &                            t , p , q ,
     &                            sea_level_pressure,
     &                            t_sea_level, t_surf, level ) 
      IMPLICIT NONE
c     Estimate sea level pressure.
      INTEGER nx , ny , nz
      REAL    z(nx,ny,nz)
      REAL    t(nx,ny,nz) , p(nx,ny,nz) , q(nx,ny,nz)
c     The output is the 2d sea level pressure.
      REAL    sea_level_pressure(nx,ny)
      INTEGER level(nx,ny)
      REAL t_surf(nx,ny) , t_sea_level(nx,ny)
C NCLEND
c     Some required physical constants:

      REAL R, G, GAMMA
      PARAMETER (R=287.04, G=9.81, GAMMA=0.0065)

c     Specific constants for assumptions made in this routine:

      REAL    TC, PCONST
      PARAMETER (TC=273.16+17.5, PCONST = 10000)
      LOGICAL ridiculous_mm5_test
      PARAMETER (ridiculous_mm5_test = .TRUE.)
c      PARAMETER (ridiculous_mm5_test = .false.)

c     Local variables:

      INTEGER i , j , k 
      INTEGER klo , khi 


      REAL plo , phi , tlo, thi , zlo , zhi
      REAL p_at_pconst , t_at_pconst , z_at_pconst
      REAL z_half_lowest

      LOGICAL  l1 , l2 , l3, found

c     Find least zeta level that is PCONST Pa above the surface.  We later use this
c     level to extrapolate a surface pressure and temperature, which is supposed
c     to reduce the effect of the diurnal heating cycle in the pressure field.

      DO j = 1 , ny
         DO i = 1 , nx
            level(i,j) = -1

            k = 1
            found = .false.
            do while( (.not. found) .and. (k.le.nz))
               IF ( p(i,j,k) .LT. p(i,j,1)-PCONST ) THEN
                  level(i,j) = k
                  found = .true.
               END IF
               k = k+1
            END DO 

            IF ( level(i,j) .EQ. -1 ) THEN
            PRINT '(A,I4,A)','Troubles finding level ',
     &                  NINT(PCONST)/100,' above ground.'
            PRINT '(A,I4,A,I4,A)',
     &            'Problems first occur at (',i,',',j,')'
            PRINT '(A,F6.1,A)',
     &            'Surface pressure = ',p(i,j,1)/100,' hPa.'
            STOP 'Error_in_finding_100_hPa_up'
         END IF


         END DO
      END DO

c     Get temperature PCONST Pa above surface.  Use this to extrapolate 
c     the temperature at the surface and down to sea level.

      DO j = 1 , ny
         DO i = 1 , nx

            klo = MAX ( level(i,j) - 1 , 1      )
            khi = MIN ( klo + 1        , nz - 1 )
     
            IF ( klo .EQ. khi ) THEN
               PRINT '(A)','Trapping levels are weird.'
               PRINT '(A,I3,A,I3,A)','klo = ',klo,', khi = ',khi,
     &                      ': and they should not be equal.'
               STOP 'Error_trapping_levels'
            END IF

         plo = p(i,j,klo)
         phi = p(i,j,khi)
         tlo = t(i,j,klo) * (1. + 0.608 * q(i,j,klo) )
         thi = t(i,j,khi) * (1. + 0.608 * q(i,j,khi) )
c         zlo = zetahalf(klo)/ztop*(ztop-terrain(i,j))+terrain(i,j)
c         zhi = zetahalf(khi)/ztop*(ztop-terrain(i,j))+terrain(i,j)
         zlo = z(i,j,klo)         
         zhi = z(i,j,khi)

         p_at_pconst = p(i,j,1) - pconst
         t_at_pconst = thi-(thi-tlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         z_at_pconst = zhi-(zhi-zlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)

         t_surf(i,j) = t_at_pconst*(p(i,j,1)/p_at_pconst)**(gamma*R/g)
         t_sea_level(i,j) = t_at_pconst+gamma*z_at_pconst

         END DO
      END DO

c     If we follow a traditional computation, there is a correction to the sea level 
c     temperature if both the surface and sea level temnperatures are *too* hot.

      IF ( ridiculous_mm5_test ) THEN
         DO j = 1 , ny
            DO i = 1 , nx
               l1 = t_sea_level(i,j) .LT. TC 
               l2 = t_surf     (i,j) .LE. TC
               l3 = .NOT. l1
               IF ( l2 .AND. l3 ) THEN
                  t_sea_level(i,j) = TC
               ELSE
                  t_sea_level(i,j) = TC - 0.005*(t_surf(i,j)-TC)**2
               END IF
            END DO
         END DO
      END IF

c     The grand finale: ta da!

      DO j = 1 , ny
      DO i = 1 , nx
c         z_half_lowest=zetahalf(1)/ztop*(ztop-terrain(i,j))+terrain(i,j)
         z_half_lowest=z(i,j,1)
         sea_level_pressure(i,j) = p(i,j,1) *
     &                         EXP((2.*g*z_half_lowest)/
     &                             (R*(t_sea_level(i,j)+t_surf(i,j))))
      END DO
      END DO

c     print *,'sea pres input at weird location i=20,j=1,k=1'
c     print *,'t=',t(20,1,1),t(20,2,1),t(20,3,1)
c     print *,'z=',z(20,1,1),z(20,2,1),z(20,3,1)
c     print *,'p=',p(20,1,1),p(20,2,1),p(20,3,1)
c     print *,'slp=',sea_level_pressure(20,1),
c    *         sea_level_pressure(20,2),sea_level_pressure(20,3)
   
      END


c---------------------------------------------------

C NCLFORTSTART
      SUBROUTINE filter2d( a, b, nx , ny , it)
      IMPLICIT NONE
c     Estimate sea level pressure.
      INTEGER nx , ny, it
      REAL    a(nx,ny),b(nx,ny)
C NCLEND

      REAL coef
      parameter( coef = 0.25)
      INTEGER i,j,iter

      do iter=1, it
        do j=1,ny
        do i=1,nx
          b(i,j) = a(i,j)
        enddo
        enddo
        do j=2,ny-1
        do i=1,nx
          a(i,j) = a(i,j) + coef*(b(i,j-1)-2*b(i,j)+b(i,j+1))
        enddo
        enddo
        do j=1,ny
        do i=2,nx-1
          a(i,j) = a(i,j) + coef*(b(i-1,j)-2*b(i,j)+b(i+1,j))
        enddo
        enddo
c        do j=1,ny
c        do i=1,nx
c          b(i,j) = a(i,j)
c        enddo
c        enddo
c        do j=2,ny-1
c        do i=1,nx
c          a(i,j) = a(i,j) - .99*coef*(b(i,j-1)-2*b(i,j)+b(i,j+1))
c        enddo
c        enddo
c        do j=1,ny
c        do i=2,nx-1
c          a(i,j) = a(i,j) - .99*coef*(b(i-1,j)-2*b(i,j)+b(i+1,j))
c        enddo
c        enddo
      enddo
      return
      end
c---------------------------------------------------------

C NCLFORTSTART
      SUBROUTINE compute_rh ( qv, p, t, rh, nx , ny , nz )

      IMPLICIT NONE
      INTEGER nx , ny , nz
      REAL    qv(nx,ny,nz), p(nx,ny,nz),t(nx,ny,nz),rh(nx,ny,nz)
C NCLEND
      REAL    svp1, svp2, svp3, svpt0
      parameter (SVP1=0.6112,SVP2=17.67,SVP3=29.65,SVPT0=273.15)
      integer i,j,k
      real qvs, es, pressure, temperature
      REAL    ep_2, r_d, r_v
      PARAMETER (r_d=287.,r_v=461.6, EP_2=R_d/R_v)
      REAL    ep_3
      PARAMETER (ep_3=0.622)

      do k=1,nz
      do j=1,ny
      do i=1,nx
        pressure = p(i,j,k)
        temperature = t(i,j,k)
c       es  = 1000.*svp1*
        es  = 10.*svp1*
     &      exp(svp2*(temperature-svpt0)/(temperature-svp3))
c       qvs = ep_2*es/(pressure-es)
        qvs = ep_3*es/(0.01 * pressure-(1.-ep_3)*es) 
c        rh = 100*amax1(1., qv(i,j,k)/qvs)
c       rh(i,j,k) = 100.*qv(i,j,k)/qvs
        rh(i,j,k) = 100.*AMAX1(AMIN1(qv(i,j,k)/qvs,1.0),0.0)
      enddo
      enddo
      enddo

      return
      end

c----------------------------------------------

C NCLFORTSTART
      SUBROUTINE get_ij_lat_long( lat_array,long_array, 
     &                      lat,longitude,ir,jr,nx,ny )
      IMPLICIT NONE
      INTEGER nx , ny
      REAL    lat_array(nx,ny), long_array(nx,ny)
      REAL    ir, jr
      REAL    lat, longitude
      REAL    longd, latd
C NCLEND
      integer i,j
      real dist_min, dist
      real w00, w01, w02, w10, w11, w12, w20, w21, w22

      dist_min = 1.e+20
      do j=1,ny
      do i=1,nx
        latd = (lat_array(i,j)-lat)**2 
        longd = amin1((long_array(i,j)-longitude)**2, 
     &                (long_array(i,j)+longitude)**2 )
        dist = sqrt(latd + longd)
        if(dist_min .gt. dist) then
          dist_min = dist
          ir = float(i)
          jr = float(j)
        end if
      enddo
      enddo
      i = nint(ir)
      j = nint(jr)

c we will just return the nearest point at present

      return
      end

C NCLFORTSTART
      subroutine compute_uvmet( u, v, uvmet, longca, longcb, 
     &                          flong, flat, cen_long, cone,
     &                          rpd, nx, ny, nz, nxp1, nyp1 )
      implicit none
      integer nx,ny,nz,nxp1,nyp1,nl
      real    u(nxp1,ny,nz),v(nx,nyp1,nz)
      real    uvmet(nx,ny,nz,2)
      real    flong(nx,ny), flat(nx,ny)
      real    longcb(nx,ny), longca(nx,ny)
      real    cen_long, cone, rpd
C NCLEND

      integer i,j,k
      real    uk, vk


      write(6,*) ' in compute_uvmet ',nx,ny,nz,nxp1,nyp1

       do j = 1, ny
       do i = 1, nx

         longca(i,j) = flong(i,j) - cen_long
         if(longca(i,j) .gt. 180.) then
           longca(i,j) = longca(i,j) - 360.
         end if
         if(longca(i,j) .lt. -180.) then
           longca(i,j) = longca(i,j) + 360.
         end if
         if(flat(i,j) .lt. 0.) then
           longcb(i,j) = - longca(i,j) * cone * rpd
         else
           longcb(i,j) = longca(i,j) * cone * rpd
         end if

         longca(i,j) = cos(longcb(i,j))
         longcb(i,j) = sin(longcb(i,j))

       enddo
       enddo

      write(6,*) ' computing velocities '

       do k=1,nz
       do j=1,ny
       do i=1,nx
           uk = 0.5*(u(i,j,k)+u(i+1,j,k))
           vk = 0.5*(v(i,j,k)+v(i,j+1,k))
           uvmet(i,j,k,1) = vk*longcb(i,j) + uk*longca(i,j)
           uvmet(i,j,k,2) = vk*longca(i,j) - uk*longcb(i,j)
       enddo
       end do
       end do

       return
       end

C NCLFORTSTART
      subroutine compute_td( td, pressure, qv_in, nx, ny, nz )
      implicit none
      integer nx,ny,nz
      real    pressure(nx,ny,nz)
      real    qv_in(nx,ny,nz)
      real    td(nx,ny,nz)
      real    qv, tdc
C NCLEND

      integer i,j,k
      real p1000mb, r_d, cp

         do k=1,nz
         do j=1,ny         
         do i=1,nx
           qv = amax1(qv_in(i,j,k),0.)
           tdc = qv*pressure(i,j,k)/(.622+qv)  ! vapor pressure 
           tdc = amax1(tdc, 0.001)            ! avoid problems near zero
           td(i,j,k) = (243.5*log(tdc)-440.8)/(19.48-log(tdc))
         enddo
         enddo
         enddo

      return
      end

C NCLFORTSTART
      subroutine compute_td_2d( td, pressure, qv_in, nx, ny )
      implicit none
      integer nx,ny
      real    pressure(nx,ny)
      real    qv_in(nx,ny)
      real    td(nx,ny)
      real    qv, tdc
C NCLEND

      integer i,j,k
      real p1000mb, r_d, cp

         do j=1,ny
         do i=1,nx
           qv = amax1(qv_in(i,j),0.)
           tdc = qv*pressure(i,j)/(.622+qv)   ! vapor pressure
           tdc = amax1(tdc, 0.001)            ! avoid problems near zero
           td(i,j) = (243.5*log(tdc)-440.8)/(19.48-log(tdc))
         enddo
         enddo

      return
      end

C NCLFORTSTART
      subroutine compute_iclw( iclw, pressure, qc_in, nx, ny, nz )
      implicit none
      integer nx,ny,nz
      real    pressure(nx,ny,nz)
      real    qc_in(nx,ny,nz)
      real    iclw(nx,ny)
      real    qclw, dp, gg
C NCLEND

      integer i,j,k

      gg = 1000./9.8

         do j=1,ny
         do i=1,nx
            iclw(i,j) = 0.
         end do
         end do

         do j=3,ny-2
         do i=3,nx-2
         do k=1,nz
           qclw = amax1(qc_in(i,j,k),0.)
           if (k.eq.1) then
             dp = (pressure(i,j,k-1)-pressure(i,j,k))
           else if (k.eq.nz) then
             dp = (pressure(i,j,k)-pressure(i,j,k+1))
           else
             dp = (pressure(i,j,k-1)-pressure(i,j,k+1))/2.
           end if
           iclw(i,j) = iclw(i,j) + qclw*dp*gg
         enddo
         enddo
         enddo

      return
      end
