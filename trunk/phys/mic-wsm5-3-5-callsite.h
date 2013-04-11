!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ii,k,ic,ip,i,j,t_,q_,p_,delz_,qci_,qrs_,den_,rain_,rainncv_,sr_,snow_,snowncv_ ) &
!$OMP SCHEDULE(dynamic)
      DO ip = 1,((1+(ite-its+1)/CHUNK)*CHUNK)*(jte-jts+1),CHUNK
       ! tid = omp_get_thread_num() + 1   ! not currently used but available and useful for debugging
       j  = jts+(ip-1)/((1+(ite-its+1)/CHUNK)*CHUNK)
       IF ( j .ge. jts .and. j .le. jte ) THEN
        ii = its+mod((ip-1),((1+(ite-its+1)/CHUNK)*CHUNK))
         DO k=kts,kte
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
         IF ( min(CHUNK,ite-ii+1) .gt. 0 ) THEN
           CALL wsm52D(T=t_, Q=q_, QCI=qci_, QRS=qrs_                &
                      ,DEN=den_                                      &
                      ,P=p_, DELZ=delz_                              &
                      ,DELT=delt,G=g, CPD=cpd, CPV=cpv, RD=rd        &
                      ,RV=rv, T0C=t0c                                &
                      ,EP1=ep1, EP2=ep2, QMIN=qmin                   &
                      ,XLS=XLS, XLV0=XLV0, XLF0=XLF0                 &
                      ,DEN0=den0, DENR=denr                          &
                      ,CLIQ=cliq,CICE=cice,PSAT=psat                 &
                      ,LON=ii,LAT=j                                  &
                      ,RAIN=rain_  ,RAINNCV=rainncv_                 &
                      ,SR=sr_                                        &
                      ,SNOW=snow_      ,SNOWNCV=snowncv_             &
                      ,NX0=CHUNK, NK0=kte-kts+1                      &
                      ,IRESTRICT=min(CHUNK,ite-ii+1)                 &
                      ,DOIT=.TRUE., KTS=kts,KTE=kte                  )
         ENDIF
         DO K=kts,kte
          DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            th(i,k,j)=t_(ic,k)/pii(i,k,j)
            q(i,k,j) = q_(ic,k)
            qc(i,k,j) = qci_(ic,k,1)
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

