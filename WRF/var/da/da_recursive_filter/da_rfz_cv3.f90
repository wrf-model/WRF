module da_rfz_cv3

   !---------------------------------------------------------------------------
   ! Purpose: da_rfz subroutines from NCEP, used by CV3
   !---------------------------------------------------------------------------

contains


 SUBROUTINE da_rfz(p1,n1,n2,n,m,vz,be,table,nta,swidth,&
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
  DIMENSION P1(ims:ime, jms:jme, kms:kme)
  dimension P2(n),p3(n)                 
  dimension BE(M),vz(kts:kte,its:ite,jts:jte)               
  dimension AL(n,m),GA(n2,M),DE(n2,M),table(nta,m) 
      nm=n-1
      itsm=its-1
      jtsm=jts-1
      ta=float(nta)/swidth

    !$OMP PARALLEL DO &
    !$OMP PRIVATE (j, jj, i, ii, k, kk, im, al, p2, p3, GA, DE)
    do j=1,n2
     jj=j+jtsm
      do i=1,n1
       ii=i+itsm
        do k=1,n
!         kk=nint(ta*vz(k,ii,jj)) 
          kk=nint(ta*vz(n+1-k,ii,jj)) 
          kk=max(1,min(kk,nta))
          do im=1,m
          al(k,im)=table(kk,im)
          enddo
        enddo
       do k=1,n
!     p2(k)=p1(ii,jj,k)
      p2(k)=p1(ii,jj,n+1-k)
       enddo
      CALL RF0V(GA,DE,M)
      CALL RFHV(P2,p3,1,Nm,M,AL,BE,GA,DE)
       do k=1,n
!     p1(ii,jj,k)=p3(k)
      p1(ii,jj,k)=p3(n+1-k)
        enddo
      enddo
    enddo
    !$OMP END PARALLEL DO

      RETURN
      END SUBROUTINE da_rfz
 SUBROUTINE da_rfz0(p1,n,n1,m,vz,be,table,nta,swidth)
  DIMENSION P1(n1,N),P2(N),p3(n),BE(M),vz(n)
  dimension AL(n,m),GA(M),DE(M),table(nta,m)
      nm=n-1
      ta=float(nta)/swidth

        do k=1,n
          ii=nint(ta*vz(k))
          ii=max(1,min(ii,nta))
          do im=1,m
          al(k,im)=table(ii,im)
          enddo
        enddo
      do i=1,n1
      p2(:)=p1(i,:)
      CALL RF0V(GA,DE,M)
      CALL RFHV(P2,p3,1,Nm,M,AL,BE,GA,DE)
      p1(i,:)=p3(:)
      enddo

      RETURN
      END SUBROUTINE da_rfz0


   SUBROUTINE RF0V(GAP,DEP,M)
      DIMENSION GAP(M),DEP(M)
      DO I=1,M
       GAP(I)=0.
       DEP(I)=0.
      ENDDO
      RETURN
      END SUBROUTINE RF0V
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!                   SUBROUTINE RFHV
!   Apply pseudo-Gaussian "right-plus-left" smoother to a single "vector"
!   of data.
!
! --> P1:   Input data.
! <-- P2:   Output data.
! --> N:    Number of grid intervals (one less than number of points).
! --> M:    Degree of approximation of smoother to a Gaussian.
! --> AL:   Matrix of "Alpha" coefficients (computed in RFDPARV).
! --> BE:   Vector of "Beta" coefficients (computed in RFDPAR1).
! <-> GAP:  Gamma vector of exponential amplitudes of P decaying to the right.
! <-> DEP:  Delta vector of exponential amplitudes of P decaying to the left.
!------------------------------------------------------------------------------
 SUBROUTINE RFHV(P1,p2,nc,N,M,AL,BE,GA,DE)
  DIMENSION P1(nc,0:N),P2(nc,0:N),AL(0:N,M),BE(M),GA(nc,M),DE(nc,M)
      KMOD2=MOD(M,2)
       do j=0,n
       do i=1,nc
       p2(i,j)=0.
       enddo
       enddo
! Advancing filter:
      DO I=0,N
!      P1I=P1(I)
!      P2(I)=0.
       IF(KMOD2.EQ.1)THEN  ! Treat the real root:
        do j=1,nc
        GA(j,1)=AL(I,1)*GA(j,1)+BE(1)*P1(j,i)
        enddo
        do j=1,nc
        P2(j,I)=P2(j,I)+GA(j,1)
        enddo
       ENDIF
                           ! Treat remaining complex roots:
       DO KR=KMOD2+1,M,2   ! <-- Index of "real" components
        KI=KR+1            ! <-- Index of "imag" components
        do j=1,nc
        GAKR=GA(j,KR)
        GAKI=GA(j,KI)
        GA(j,KR)=AL(I,KR)*GAKR-AL(I,KI)*GAKI+BE(KR)*P1(j,i)
        GA(j,KI)=AL(I,KI)*GAKR+AL(I,KR)*GAKI+BE(KI)*P1(j,i)
        enddo
        do j=1,nc
        P2(j,I)=P2(j,I)+GA(j,KR)
        enddo
       ENDDO
      ENDDO

! Backing filter:
      DO I=N,0,-1
!      P1I=P1(I)
       IF(KMOD2.EQ.1)THEN  ! Treat the real root:
        do j=1,nc
        P2(j,I)=P2(j,I)+DE(j,1)
        enddo
        do j=1,nc
        DE(j,1)=AL(I,1)*(DE(j,1)+BE(1)*P1(j,i))
        enddo
       ENDIF
                           ! Treat remaining complex roots:
       DO KR=KMOD2+1,M,2   ! <-- Index of "real" components
        KI=KR+1            ! <-- Index of "imag" components
        do j=1,nc
        P2(j,I)=P2(j,I)+DE(j,KR)
        enddo
        do j=1,nc
        DEKR=DE(j,KR)+BE(KR)*P1(j,i)
        DEKI=DE(j,KI)+BE(KI)*P1(j,i)
        DE(j,KR)=AL(I,KR)*DEKR-AL(I,KI)*DEKI
        DE(j,KI)=AL(I,KI)*DEKR+AL(I,KR)*DEKI
        enddo
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFHV

end module da_rfz_cv3
