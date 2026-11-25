
      SUBROUTINE SINT(XF,                        &
                   ims, ime, jms, jme, icmask ,  &
                   its, ite, jts, jte, nf, xstag, ystag )
      IMPLICIT NONE
      INTEGER ims, ime, jms, jme, &
              its, ite, jts, jte

      LOGICAL icmask( ims:ime, jms:jme )
      LOGICAL xstag, ystag

      INTEGER nf, ior
      REAL    one12, one24, ep
      PARAMETER(one12=1./12.,one24=1./24.)                              
      PARAMETER(ior=2)                        
!                                                                       
      REAL XF(ims:ime,jms:jme,NF)
!                                                                       
      ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
      REAL Y(-IOR:IOR),                &
           ZX(its:ite,jts-IOR:jte+IOR),&
           F(0:1)
      LOGICAL xmask(its:ite,jts-IOR:jte+IOR)
      ! END LICENSE
!
      INTEGER I,J,II,JJ,IIM
      INTEGER N2STAR, N2END, N1STAR, N1END
!                                                                       
      DATA  EP/ 1.E-10/                                                 

      ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
      REAL W,OV,UN,MXM,MN,FL(0:1)
      ! END LICENSE
      REAL XIG(NF*NF), XJG(NF*NF)  ! NF is parent to child grid refinement ratio
      integer rr

      REAL rioff, rjoff
!                                                                       
      REAL donor, y1, y2, a
      DONOR(Y1,Y2,A)=(Y1*AMAX1(0.,SIGN(1.,A))-Y2*AMIN1(0.,SIGN(1.,A)))*A
      REAL tr4, ym1, y0, yp1, yp2
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))               &
       -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)          & 
       -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))                
      REAL pp, pn, x
      PP(X)=AMAX1(0.,X)                                                 
      PN(X)=AMIN1(0.,X)                                                 
      ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
      ! Guard empty 1-cell boundary tiles before sweeping J = N2STAR-IOR..N2END+IOR
      IF (its .GT. ite .OR. jts .GT. jte) RETURN
      ! END LICENSE

      rr = nint(sqrt(float(nf)))
!!      write(6,*) ' nf, rr are ',nf,rr

      rioff = 0
      rjoff = 0
      if(xstag .and. (mod(rr,2) .eq. 0)) rioff = 1.
      if(ystag .and. (mod(rr,2) .eq. 0)) rjoff = 1.

      DO I=1,rr
        DO J=1,rr
          XIG(J+(I-1)*rr)=(float(rr)-1.-rioff)/float(2*rr)-FLOAT(J-1)*1./float(rr)
          XJG(J+(I-1)*rr)=(float(rr)-1.-rjoff)/float(2*rr)-FLOAT(I-1)*1./float(rr)   
        ENDDO
      ENDDO

      N2STAR = jts
      N2END  = jte
      N1STAR = its
      N1END  = ite

      ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
      ! Precompute 5-row stencil mask so each unique x-pass row is computed once into ZX
      xmask = .FALSE.
      DO JJ = N2STAR, N2END
        DO II = N1STAR, N1END
          IF ( icmask(II,JJ) ) xmask(II, JJ-IOR:JJ+IOR) = .TRUE.
        ENDDO
      ENDDO
      ! END LICENSE
      DO 2000 IIM=1,NF                                                  
!                                                                       
!  HERE STARTS RESIDUAL ADVECTION                                       
!                                                                       
          ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
          DO 50 J=N2STAR-IOR,N2END+IOR
            DO 811 II=N1STAR,N1END
              IF ( xmask(II,J) ) THEN
                Y(-2)=XF(II-2,J,IIM)
                Y(-1)=XF(II-1,J,IIM)
                Y(0)=XF(II,J,IIM)
                Y(1)=XF(II+1,J,IIM)
                Y(2)=XF(II+2,J,IIM)
                FL(0)=DONOR(Y(-1),Y(0),XIG(IIM))
                FL(1)=DONOR(Y(0),Y(1),XIG(IIM))
                W=Y(0)-(FL(1)-FL(0))
                MXM=AMAX1(Y(-1),Y(0),Y(1),W)
                MN=AMIN1(Y(-1),Y(0),Y(1),W)
                F(0)=TR4(Y(-2),Y(-1),Y(0),Y(1),XIG(IIM))
                F(1)=TR4(Y(-1),Y(0),Y(1),Y(2),XIG(IIM))
                F(0)=F(0)-FL(0)
                F(1)=F(1)-FL(1)
                OV=(MXM-W)/(-PN(F(1))+PP(F(0))+EP)
                UN=(W-MN)/(PP(F(1))-PN(F(0))+EP)
                F(0)=PP(F(0))*AMIN1(1.,OV)+PN(F(0))*AMIN1(1.,UN)
                F(1)=PP(F(1))*AMIN1(1.,UN)+PN(F(1))*AMIN1(1.,OV)
                ZX(II,J)=W-(F(1)-F(0))
          ! END LICENSE
              ENDIF
  811         CONTINUE
!                                                                       
!  END IF FIRST J LOOP                                                  
!                                                                       
   50     CONTINUE                                                      
        ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
        DO 9000 JJ=N2STAR,N2END
        ! END LICENSE
          DO 911 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
              FL(0)=DONOR(ZX(II,JJ-1),ZX(II,JJ),XJG(IIM))
              FL(1)=DONOR(ZX(II,JJ),ZX(II,JJ+1),XJG(IIM))
              W=ZX(II,JJ)-(FL(1)-FL(0))
              MXM=AMAX1(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),W)
              MN=AMIN1(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),W)
              F(0)=TR4(ZX(II,JJ-2),ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),XJG(IIM))
              F(1)=TR4(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),ZX(II,JJ+2),XJG(IIM))
              F(0)=F(0)-FL(0)
              F(1)=F(1)-FL(1)
              OV=(MXM-W)/(-PN(F(1))+PP(F(0))+EP)
              UN=(W-MN)/(PP(F(1))-PN(F(0))+EP)
              F(0)=PP(F(0))*AMIN1(1.,OV)+PN(F(0))*AMIN1(1.,UN)
              F(1)=PP(F(1))*AMIN1(1.,UN)+PN(F(1))*AMIN1(1.,OV)
              XF(II,JJ,IIM)=W-(F(1)-F(0))
              ! END LICENSE
            ENDIF
  911       CONTINUE
 9000   CONTINUE                                                        
                                                                        
!                                                                       
 2000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        
! Version of sint that replaces mask with detailed ranges for avoiding boundaries
! may help performance by getting the conditionals out of innner loops

      SUBROUTINE SINTB(XF1, XF ,                  &
                   ims, ime, jms, jme, icmask ,  &
                   its, ite, jts, jte, nf, xstag, ystag )
      IMPLICIT NONE
      INTEGER ims, ime, jms, jme, &
              its, ite, jts, jte

      LOGICAL icmask( ims:ime, jms:jme )
      LOGICAL xstag, ystag

      INTEGER nf, ior
      REAL    one12, one24, ep
      PARAMETER(one12=1./12.,one24=1./24.)                              
      PARAMETER(ior=2)                        
!                                                                       
      REAL XF(ims:ime,jms:jme,NF)
      REAL XF1(ims:ime,jms:jme,NF)
!                                                                       
      REAL Y(-IOR:IOR),    &
           ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
           ZX(its:ite,jts-IOR:jte+IOR),&
           ! END LICENSE
           F(0:1)                                       
!
      INTEGER I,J,II,JJ,IIM
      INTEGER N2STAR, N2END, N1STAR, N1END
!                                                                       
      DATA  EP/ 1.E-10/                                                 
!                                                                       
!      PARAMETER(NONOS=1)                                                
!      PARAMETER(N1OS=N1*NONOS+1-NONOS,N2OS=N2*NONOS+1-NONOS)            
!                                                                       
      REAL W,OV,UN                     
      REAL MXM,MN                               
      REAL FL(0:1)                                            
      REAL XIG(NF*NF), XJG(NF*NF)  ! NF is the parent to child grid refinement ratio
      integer rr

      REAL rioff, rjoff
!                                                                       
      REAL donor, y1, y2, a
      DONOR(Y1,Y2,A)=(Y1*AMAX1(0.,SIGN(1.,A))-Y2*AMIN1(0.,SIGN(1.,A)))*A
      REAL tr4, ym1, y0, yp1, yp2
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))               &
       -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)          & 
       -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))                
      REAL pp, pn, x
      PP(X)=AMAX1(0.,X)                                                 
      PN(X)=AMIN1(0.,X)                                                 
      ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
      ! Guard empty 1-cell boundary tiles before sweeping J = N2STAR-IOR..N2END+IOR
      IF (its .GT. ite .OR. jts .GT. jte) RETURN
      ! END LICENSE

      rr = nint(sqrt(float(nf)))

      rioff = 0
      rjoff = 0
      if(xstag .and. (mod(rr,2) .eq. 0)) rioff = 1.
      if(ystag .and. (mod(rr,2) .eq. 0)) rjoff = 1.

      DO I=1,rr
        DO J=1,rr
          XIG(J+(I-1)*rr)=(float(rr)-1.-rioff)/float(2*rr)-FLOAT(J-1)*1./float(rr)
          XJG(J+(I-1)*rr)=(float(rr)-1.-rjoff)/float(2*rr)-FLOAT(I-1)*1./float(rr)   
        ENDDO
      ENDDO

      N2STAR = jts
      N2END  = jte
      N1STAR = its
      N1END  = ite

      DO 2000 IIM=1,NF                                                  
!                                                                       
!  HERE STARTS RESIDUAL ADVECTION                                       
!                                                                       
!cdir unroll=5
          ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
          ! Compute each unique x-direction row once into ZX and reuse across adjacent JJ stencils
          DO 50 J=N2STAR-IOR,N2END+IOR
          ! END LICENSE

!cdir unroll=5
              DO 511 II=N1STAR,N1END                                    
                ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
                Y(-2)=XF1(II-2,J,IIM)
                Y(-1)=XF1(II-1,J,IIM)
                Y(0)=XF1(II,J,IIM)
                Y(1)=XF1(II+1,J,IIM)
                Y(2)=XF1(II+2,J,IIM)
                ! END LICENSE

              FL(0)=DONOR(Y(-1),Y(0),XIG(IIM))        
              FL(1)=DONOR(Y(0),Y(1),XIG(IIM))           
              W=Y(0)-(FL(1)-FL(0))               
              MXM=                                             &
                       AMAX1(Y(-1),Y(0),Y(1),       &
                       W)                                      
              MN=AMIN1(Y(-1),Y(0),Y(1),W) 
              F(0)=                                               &
                   TR4(Y(-2),Y(-1),Y(0),        &
                   Y(1),XIG(IIM))                           
              F(1)=                                                 &
                       TR4(Y(-1),Y(0),Y(1),Y(2),&
                       XIG(IIM))                                        
              F(0)=F(0)-FL(0)                         
              F(1)=F(1)-FL(1)                           
              OV=(MXM-W)/(-PN(F(1))+         &
                      PP(F(0))+EP)                              
              UN=(W-MN)/(PP(F(1))-             &
                    PN(F(0))+EP)                                
              F(0)=PP(F(0))*AMIN1(1.,OV)+            &
                   PN(F(0))*AMIN1(1.,UN)             
              F(1)=PP(F(1))*AMIN1(1.,UN)+            &
                   PN(F(1))*AMIN1(1.,OV)             
              Y(0)=W-(F(1)-F(0))                 
              ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
              ZX(II,J)=Y(0)
              ! END LICENSE
  511         CONTINUE
!                                                                       
!  END IF FIRST J LOOP                                                  
!                                                                       
   50     CONTINUE                                                      

        ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
        DO 9000 JJ=N2STAR,N2END
        ! END LICENSE
          DO 911 II=N1STAR,N1END                                        
            ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
            FL(0)=DONOR(ZX(II,JJ-1),ZX(II,JJ),XJG(IIM))
            FL(1)=DONOR(ZX(II,JJ),ZX(II,JJ+1),XJG(IIM))
            W=ZX(II,JJ)-(FL(1)-FL(0))
            MXM=AMAX1(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),W)
             MN=AMIN1(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),W)
            ! END LICENSE
            F(0)=                                                 &
                 ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
                 TR4(ZX(II,JJ-2),ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1)&
                 ! END LICENSE
                 ,XJG(IIM))                                       
            F(1)=                                                   &
                 ! THE FOLLOWING LINES OF CODE ARE LICENSED UNDER doc/licenses/google_license.txt
                 TR4(ZX(II,JJ-1),ZX(II,JJ),ZX(II,JJ+1),ZX(II,JJ+2),  &
                 ! END LICENSE
                 XJG(IIM))                                          
            F(0)=F(0)-FL(0)                           
            F(1)=F(1)-FL(1)                             
            OV=(MXM-W)/(-PN(F(1))+           &
               PP(F(0))+EP)                                
            UN=(W-MN)/(PP(F(1))-PN(F(0))+ &
                    EP)                                                 
            F(0)=PP(F(0))*AMIN1(1.,OV)+PN(F(0))  &
                 *AMIN1(1.,UN)                             
            F(1)=PP(F(1))*AMIN1(1.,UN)+PN(F(1))  &
                       *AMIN1(1.,OV)                             
            XF(II,JJ,IIM)=W-(F(1)-F(0))               
  911     CONTINUE                                                      
 9000   CONTINUE                                                        
                                                                        
!                                                                       
 2000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        

