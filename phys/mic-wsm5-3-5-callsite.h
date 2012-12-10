!write(0,*)'qmin ',qmin

!ntds = omp_get_max_threads()
!CALL omp_set_num_threads( max(1,ntds/3) )
!write(0,*)__FILE__,__LINE__,omp_get_max_threads()

!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ii,k,ic,ip,i,j,t_,q_,p_,delz_,qci_,qrs_,den_,rain_,rainncv_,sr_,snow_,snowncv_ ),schedule(dynamic)
      DO ip = 1,((1+(ite-its+1)/CHUNK)*CHUNK)*(jte-jts+1),CHUNK
       j  = jts+(ip-1)/((1+(ite-its+1)/CHUNK)*CHUNK)
       IF ( j .ge. jts .and. j .le. jte ) THEN
        ii = its+mod((ip-1),((1+(ite-its+1)/CHUNK)*CHUNK))
         DO k=kts,kte

!DIR$ ASSUME_ALIGNED th(ii,k,j):64
!DIR$ ASSUME_ALIGNED pii(ii,k,j):64
!DIR$ ASSUME_ALIGNED q(ii,k,j):64
!DIR$ ASSUME_ALIGNED p(ii,k,j):64
!DIR$ ASSUME_ALIGNED delz(ii,k,j):64
!DIR$ ASSUME_ALIGNED qc(ii,k,j):64
!DIR$ ASSUME_ALIGNED qi(ii,k,j):64
!DIR$ ASSUME_ALIGNED qr(ii,k,j):64
!DIR$ ASSUME_ALIGNED qs(ii,k,j):64
!DIR$ ASSUME_ALIGNED den(ii,k,j):64
!DIR$ ASSUME_ALIGNED rain(ii,j):64
!DIR$ ASSUME_ALIGNED rainncv(ii,j):64
!DIR$ ASSUME_ALIGNED sr(ii,j):64
!DIR$ ASSUME_ALIGNED snow(ii,j):64
!DIR$ ASSUME_ALIGNED snowncv(ii,j):64

          DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            t_(ic,k)=th(i,k,j)*pii(i,k,j)
            q_(ic,k)=q(i,k,j)
            p_(ic,k)=p(i,k,j)
            delz_(ic,k)=delz(i,k,j)
            qci_(ic,k,1) = qc(i,k,j)
            qci_(ic,k,2) = qi(i,k,j)
            qrs_(ic,k,1) = qr(i,k,j)
            qrs_(ic,k,2) = qs(i,k,j)
            den_(ic,k) = den(i,k,j)
          ENDDO
         ENDDO
         DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            rain_(ic) = rain(i,j)
            rainncv_(ic) = rainncv(i,j)
            sr_(ic) = sr(i,j)
            snow_(ic) = snow(i,j)
            snowncv_(ic) = snowncv(i,j)
         ENDDO

#if 0
DO ic=1,min(CHUNK,ite-ii+1)
i = ii+ic -1
if ( i .eq. 59 .and. j .eq. 207 ) then
  DO k=kts,kte
    write(0,'(a20,3i4,e25.15)')"th(i,k,j)*pii(i,k,j) ",i,j,k,th(i,k,j)*pii(i,k,j)
    write(0,'(a20,3i4,e25.15)')"q(i,k,j) ",i,j,k,q(i,k,j)
    write(0,'(a20,3i4,e25.15)')"p(i,k,j) ",i,j,k,p(i,k,j)
    write(0,'(a20,3i4,e25.15)')"delz(i,k,j) ",i,j,k,delz(i,k,j)
    write(0,'(a20,3i4,e25.15)')"qc(i,k,j) ",i,j,k,qc(i,k,j)
    write(0,'(a20,3i3,e25.15)')"qi(i,k,j) ",i,j,k,qi(i,k,j)
    write(0,'(a20,3i4,e25.15)')"qr(i,k,j) ",i,j,k,qr(i,k,j)
    write(0,'(a20,3i4,e25.15)')"qs(i,k,j) ",i,j,k,qs(i,k,j)
  ENDDO
  write(0,'(a20,2i4,e25.15)')"rain(i,j) ",i,j,rain(i,j)
  write(0,'(a20,2i4,e25.15)')"rainncv(i,j) ",i,j,rainncv(i,j)
  write(0,'(a20,2i4,e25.15)')"sr(i,j) ",i,j,sr(i,j)
  write(0,'(a20,2i4,e25.15)')"snow(i,j) ",i,j,snow(i,j)
  write(0,'(a20,2i4,e25.15)')"snowncv(i,j) ",i,j,snowncv(i,j)
endif
ENDDO
#endif
         IF ( min(CHUNK,ite-ii+1) .gt. 0 ) THEN
           CALL wsm52D(t_, q_, qci_, qrs_                          &
                    ,den_                                          &
                    ,p_, delz_                                     &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,ii,j                                          &
                    ,rain_      ,rainncv_                          &
                    ,sr_                                           &
                    ,snow_      ,snowncv_                          &
                    ,CHUNK, kte-kts+1, min(CHUNK,ite-ii+1)         &
                    ,.true., kts,kte ) !, tid,ip                            )
         ENDIF
         DO K=kts,kte
          DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            th(i,k,j)=t_(ic,k)/pii(i,k,j)
            q(i,k,j) = q_(ic,k)
            qc(i,k,j) = qci_(ic,k,1)
!if ( k .eq. 1 ) write(0,*)'qc: ',i,j,qc(i,1,j)
            qi(i,k,j) = qci_(ic,k,2)
            qr(i,k,j) = qrs_(ic,k,1)
            qs(i,k,j) = qrs_(ic,k,2)
          ENDDO
         ENDDO
         DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            rain(i,j) = rain_(ic)
            rainncv(i,j) = rainncv_(ic)
            sr(i,j) = sr_(ic)
            snow(i,j) = snow_(ic)
            snowncv(i,j) = snowncv_(ic)
         ENDDO
       ENDIF
      ENDDO
       !$OMP END PARALLEL DO

!CALL omp_set_num_threads( max(1,ntds) )
!write(0,*)__FILE__,__LINE__,omp_get_max_threads()
