!WRF:MEDIATION_LAYER:UTIL
!

MODULE module_solvedebug_em
CONTAINS
      SUBROUTINE var_min_max( u,v,w,t,r,                  &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL,  DIMENSION( kms: , ims: , jms: ), &
                   INTENT(IN) :: u,v,w,t,r

      INTEGER  :: i, j, k, istag, jstag, imax, imin, jmax, jmin, &
                  kmax, kmin

      REAL :: vmax, vmin, vavg

      vmin = u(1,1,1)
      vmax = u(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe-1
      do i=ips,ipe
      do k=kps,kpe-1
        if(u(k,i,j) .gt. vmax) then
          vmax = u(k,i,j)
          imax = i
          jmax = j
          kmax = k
         endif

        if(u(k,i,j) .lt. vmin) then
          vmin = u(k,i,j)
          imin = i
          jmin = j
          kmin = k
         endif
        vavg = vavg + abs(u(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips)*(jpe-jps-1)*(kpe-kps-1))
      write(6,*) ' ru min,max,avg ',vmin,vmax,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin


      vmin = v(1,1,1)
      vmax = v(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe
      do i=ips,ipe-1
      do k=kps,kpe-1
        if(v(k,i,j) .gt. vmax) then
          vmax = v(k,i,j)
          imax = i
          jmax = j
          kmax = k
        endif
        if(v(k,i,j) .lt. vmin) then
          vmin = v(k,i,j)
          imin = i
          jmin = j
          kmin = k
        endif
        vavg = vavg + abs(v(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips-1)*(jpe-jps)*(kpe-kps-1))
      write(6,*) ' rv min,max,avg ',vmin,vmax,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin



      vmin = w(1,1,1)
      vmax = w(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe-1
      do i=ips,ipe-1
      do k=kps,kpe
        if(w(k,i,j) .gt. vmax) then
          vmax = w(k,i,j)
          imax = i
          jmax = j
          kmax = k
        endif
        if(w(k,i,j) .lt. vmin) then
          vmin = w(k,i,j)
          imin = i
          jmin = j
          kmin = k
        endif
        vavg = vavg + abs(w(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips-1)*(jpe-jps-1)*(kpe-kps))
      write(6,*) ' rom min,max,avg ',vmin,vmax,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin



      vmin = t(1,1,1)
      vmax = t(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe-1
      do i=ips,ipe-1
      do k=kps,kpe-1
        if(t(k,i,j) .gt. vmax) then
          vmax = t(k,i,j)
          imax = i
          jmax = j
          kmax = k
        endif
        if(t(k,i,j) .lt. vmin) then
          vmin = t(k,i,j)
          imin = i
          jmin = j
          kmin = k
        endif
        vavg = vavg + abs(t(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips-1)*(jpe-jps-1)*(kpe-kps-1))
      write(6,*) ' rtp min,max,avg ',vmin,vmax,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin



      vmin = r(1,1,1)
      vmax = r(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe-1
      do i=ips,ipe-1
      do k=kps,kpe-1
        if(r(k,i,j) .gt. vmax) then
          vmax = r(k,i,j)
          imax = i
          jmax = j
          kmax = k
        endif
        if(r(k,i,j) .lt. vmin) then
          vmin = r(k,i,j)
          imin = i
          jmin = j
          kmin = k
        endif
        vavg = vavg + abs(r(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips-1)*(jpe-jps-1)*(kpe-kps-1))
      write(6,*) ' rhop min,max,avg ',vmin,vmax,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin

      return
      end subroutine var_min_max

      SUBROUTINE var1_min_max( u, &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL,  DIMENSION(kms: , ims: , jms: ), &
                   INTENT(IN) :: u

      INTEGER  :: i, j, k, istag, jstag, imax, imin, jmax, jmin, &
                  kmax, kmin

      REAL :: vmax, vmin, vavg

      write(6,*) ' min, max, and avg stats '

      vmin = u(1,1,1)
      vmax = u(1,1,1)
      vavg = 0.
      imax = 1
      imin = 1
      jmax = 1
      jmin = 1
      kmax = 1
      kmin = 1

      do j=jps,jpe-1
      do i=ips,ipe
      do k=kps,kpe-1
        if(u(k,i,j) .gt. vmax) then
          vmax = u(k,i,j)
          imax = i
          jmax = j
          kmax = k
         endif

        if(u(k,i,j) .lt. vmin) then
          vmin = u(k,i,j)
          imin = i
          jmin = j
          kmin = k
         endif
        vavg = vavg + abs(u(k,i,j))
      enddo
      enddo
      enddo
      vavg = vavg/float((ipe-ips)*(jpe-jps-1)*(kpe-kps-1))
      write(6,*) ' ru max,min,avg ',vmax,vmin,vavg
      write(6,*) kmax, imax, jmax, kmin, imin, jmin

      return
      end subroutine var1_min_max




      SUBROUTINE var_print ( u, &
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              level                )  

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: level

      REAL,  DIMENSION(kms:kme, ims:ime, jms:jme), &
                   INTENT(IN) :: u

      INTEGER  :: i, j, k, istag, jstag, it, imax, imin, jmax, jmin, &
                  kmax, kmin, ii,jj

      REAL :: vmax, vmin, vavg

      write(6,*) ' level for print ',level
      write(6,*) (u(level, ii, 1),ii=1,ipe)
      write(6,*) (u(level, 1, jj),jj=1,jpe)

      return
      end subroutine var_print

      SUBROUTINE symm_check ( f, &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              level                )  

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: level

      REAL,  DIMENSION(kms:kme, ims:ime, jms:jme), &
                   INTENT(IN) :: f

      INTEGER  :: i, j, k, istag, jstag, it, imax, imin, jmax, jmin, &
                  kmax, kmin, ii,jj

      REAL :: vmax, vmin, vavg

      write(6,*) ide,' = ide'

      do k=kps,kpe
       do i=ips,ipe
        do j=jps,jpe
          if(f(k,i,j).ne.f(k,ide-i,j))print *,' x asymmetry at kij ',k,i,j
          if(f(k,i,j).ne.f(k,i,jde-j))print *,' y asymmetry at kij ',k,i,j
        enddo
       enddo
      enddo
      return
      end subroutine symm_check
END MODULE module_solvedebug_em
