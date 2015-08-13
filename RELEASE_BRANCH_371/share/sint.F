
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
      REAL Y(ims:ime,jms:jme,-IOR:IOR),    &
           Z(ims:ime,jms:jme,-IOR:IOR),    &
           F(ims:ime,jms:jme,0:1)                                       
!
      INTEGER I,J,II,JJ,IIM
      INTEGER N2STAR, N2END, N1STAR, N1END
!                                                                       
      DATA  EP/ 1.E-10/                                                 

      REAL W(ims:ime,jms:jme),OV(ims:ime,jms:jme),UN(ims:ime,jms:jme)                     
      REAL MXM(ims:ime,jms:jme),MN(ims:ime,jms:jme)                                 
      REAL FL(ims:ime,jms:jme,0:1)                                            
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

      DO 2000 IIM=1,NF                                                  
!                                                                       
!  HERE STARTS RESIDUAL ADVECTION                                       
!                                                                       
        DO 9000 JJ=N2STAR,N2END                                         
          DO 50 J=-IOR,IOR                                              

            DO 51 I=-IOR,IOR                                            
              DO 511 II=N1STAR,N1END                                    
                IF ( icmask(II,JJ) ) Y(II,JJ,I)=XF(II+I,JJ+J,IIM)              
  511         CONTINUE
   51       CONTINUE                                                    

            DO 811 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                FL(II,JJ,0)=DONOR(Y(II,JJ,-1),Y(II,JJ,0),XIG(IIM))        
                FL(II,JJ,1)=DONOR(Y(II,JJ,0),Y(II,JJ,1),XIG(IIM))           
              ENDIF
  811         CONTINUE
            DO 812 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) W(II,JJ)=Y(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))               
  812         CONTINUE
            DO 813 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                MXM(II,JJ)=                                             &
                         AMAX1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),       &
                         W(II,JJ))                                      
                MN(II,JJ)=AMIN1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),W(II,JJ)) 
              ENDIF
  813         CONTINUE
            DO 312 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                F(II,JJ,0)=                                               &
                           TR4(Y(II,JJ,-2),Y(II,JJ,-1),Y(II,JJ,0),        &
                           Y(II,JJ,1),XIG(IIM))                           
                F(II,JJ,1)=                                                 &
                         TR4(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),Y(II,JJ,2),&
                         XIG(IIM))                                        
                ENDIF
  312         CONTINUE
            DO 822 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                         
                F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                           
              ENDIF
  822         CONTINUE
            DO 823 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+         &
                        PP(F(II,JJ,0))+EP)                              
                UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-             &
                      PN(F(II,JJ,0))+EP)                                
              ENDIF
  823         CONTINUE
            DO 824 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+            &
                           PN(F(II,JJ,0))*AMIN1(1.,UN(II,JJ))             
                F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+            &
                           PN(F(II,JJ,1))*AMIN1(1.,OV(II,JJ))             
              ENDIF                                                    
  824         CONTINUE                                                    
            DO 825 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) THEN
                Y(II,JJ,0)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                 
              ENDIF
  825         CONTINUE
            DO 361 II=N1STAR,N1END                                      
              IF ( icmask(II,JJ) ) Z(II,JJ,J)=Y(II,JJ,0)                                       
  361         CONTINUE
!                                                                       
!  END IF FIRST J LOOP                                                  
!                                                                       
 8000       CONTINUE                                                    
   50     CONTINUE                                                      

          DO 911 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              FL(II,JJ,0)=DONOR(Z(II,JJ,-1),Z(II,JJ,0),XJG(IIM))          
              FL(II,JJ,1)=DONOR(Z(II,JJ,0),Z(II,JJ,1),XJG(IIM))             
            ENDIF
  911       CONTINUE
          DO 912 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) W(II,JJ)=Z(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                 
  912       CONTINUE
          DO 913 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              MXM(II,JJ)=AMAX1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))
              MN(II,JJ)=AMIN1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))   
            ENDIF
  913       CONTINUE
          DO 412 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              F(II,JJ,0)=                                                 &
                         TR4(Z(II,JJ,-2),Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1)&
                         ,XJG(IIM))                                       
              F(II,JJ,1)=                                                   &
                         TR4(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),Z(II,JJ,2),  &
                         XJG(IIM))                                          
            ENDIF
  412       CONTINUE
          DO 922 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                           
              F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                             
            ENDIF
  922       CONTINUE
          DO 923 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+           &
                        PP(F(II,JJ,0))+EP)                                
              UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-PN(F(II,JJ,0))+ &
                      EP)                                                 
            ENDIF
  923       CONTINUE
          DO 924 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) THEN
              F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+PN(F(II,JJ,0))  &
                         *AMIN1(1.,UN(II,JJ))                             
              F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+PN(F(II,JJ,1))  &
                         *AMIN1(1.,OV(II,JJ))                             
            ENDIF
  924     CONTINUE                                                      
 9000   CONTINUE                                                        
        DO 925 JJ=N2STAR,N2END                                          
          DO 925 II=N1STAR,N1END                                        
            IF ( icmask(II,JJ) ) XF(II,JJ,IIM)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                
  925     CONTINUE
                                                                        
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
      REAL Y(ims:ime,jms:jme,-IOR:IOR),    &
           Z(ims:ime,jms:jme,-IOR:IOR),    &
           F(ims:ime,jms:jme,0:1)                                       
!
      INTEGER I,J,II,JJ,IIM
      INTEGER N2STAR, N2END, N1STAR, N1END
!                                                                       
      DATA  EP/ 1.E-10/                                                 
!                                                                       
!      PARAMETER(NONOS=1)                                                
!      PARAMETER(N1OS=N1*NONOS+1-NONOS,N2OS=N2*NONOS+1-NONOS)            
!                                                                       
      REAL W(ims:ime,jms:jme),OV(ims:ime,jms:jme),UN(ims:ime,jms:jme)                     
      REAL MXM(ims:ime,jms:jme),MN(ims:ime,jms:jme)                                 
      REAL FL(ims:ime,jms:jme,0:1)                                            
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
        DO 9000 JJ=N2STAR,N2END                                         
!cdir unroll=5
          DO 50 J=-IOR,IOR                                              

!cdir unroll=5
            DO 51 I=-IOR,IOR                                            
              DO 511 II=N1STAR,N1END                                    
                Y(II,JJ,I)=XF1(II+I,JJ+J,IIM)              
  511         CONTINUE
   51       CONTINUE                                                    

            DO 811 II=N1STAR,N1END                                      
              FL(II,JJ,0)=DONOR(Y(II,JJ,-1),Y(II,JJ,0),XIG(IIM))        
              FL(II,JJ,1)=DONOR(Y(II,JJ,0),Y(II,JJ,1),XIG(IIM))           
  811         CONTINUE
            DO 812 II=N1STAR,N1END                                      
              W(II,JJ)=Y(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))               
  812         CONTINUE
            DO 813 II=N1STAR,N1END                                      
              MXM(II,JJ)=                                             &
                       AMAX1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),       &
                       W(II,JJ))                                      
              MN(II,JJ)=AMIN1(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),W(II,JJ)) 
  813         CONTINUE
            DO 312 II=N1STAR,N1END                                      
              F(II,JJ,0)=                                               &
                         TR4(Y(II,JJ,-2),Y(II,JJ,-1),Y(II,JJ,0),        &
                         Y(II,JJ,1),XIG(IIM))                           
              F(II,JJ,1)=                                                 &
                       TR4(Y(II,JJ,-1),Y(II,JJ,0),Y(II,JJ,1),Y(II,JJ,2),&
                       XIG(IIM))                                        
  312         CONTINUE
            DO 822 II=N1STAR,N1END                                      
              F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                         
              F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                           
  822         CONTINUE
            DO 823 II=N1STAR,N1END                                      
              OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+         &
                      PP(F(II,JJ,0))+EP)                              
              UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-             &
                    PN(F(II,JJ,0))+EP)                                
  823         CONTINUE
            DO 824 II=N1STAR,N1END                                      
              F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+            &
                         PN(F(II,JJ,0))*AMIN1(1.,UN(II,JJ))             
              F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+            &
                         PN(F(II,JJ,1))*AMIN1(1.,OV(II,JJ))             
  824         CONTINUE                                                    
            DO 825 II=N1STAR,N1END                                      
              Y(II,JJ,0)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                 
  825         CONTINUE
            DO 361 II=N1STAR,N1END                                      
              Z(II,JJ,J)=Y(II,JJ,0)                                       
  361         CONTINUE
!                                                                       
!  END IF FIRST J LOOP                                                  
!                                                                       
 8000       CONTINUE                                                    
   50     CONTINUE                                                      

          DO 911 II=N1STAR,N1END                                        
            FL(II,JJ,0)=DONOR(Z(II,JJ,-1),Z(II,JJ,0),XJG(IIM))          
            FL(II,JJ,1)=DONOR(Z(II,JJ,0),Z(II,JJ,1),XJG(IIM))             
  911       CONTINUE
          DO 912 II=N1STAR,N1END                                        
            W(II,JJ)=Z(II,JJ,0)-(FL(II,JJ,1)-FL(II,JJ,0))                 
  912       CONTINUE
          DO 913 II=N1STAR,N1END                                        
            MXM(II,JJ)=AMAX1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))
            MN(II,JJ)=AMIN1(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),W(II,JJ))   
  913       CONTINUE
          DO 412 II=N1STAR,N1END                                        
            F(II,JJ,0)=                                                 &
                       TR4(Z(II,JJ,-2),Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1)&
                       ,XJG(IIM))                                       
            F(II,JJ,1)=                                                   &
                       TR4(Z(II,JJ,-1),Z(II,JJ,0),Z(II,JJ,1),Z(II,JJ,2),  &
                       XJG(IIM))                                          
  412       CONTINUE
          DO 922 II=N1STAR,N1END                                        
            F(II,JJ,0)=F(II,JJ,0)-FL(II,JJ,0)                           
            F(II,JJ,1)=F(II,JJ,1)-FL(II,JJ,1)                             
  922       CONTINUE
          DO 923 II=N1STAR,N1END                                        
            OV(II,JJ)=(MXM(II,JJ)-W(II,JJ))/(-PN(F(II,JJ,1))+           &
                      PP(F(II,JJ,0))+EP)                                
            UN(II,JJ)=(W(II,JJ)-MN(II,JJ))/(PP(F(II,JJ,1))-PN(F(II,JJ,0))+ &
                    EP)                                                 
  923       CONTINUE
          DO 924 II=N1STAR,N1END                                        
            F(II,JJ,0)=PP(F(II,JJ,0))*AMIN1(1.,OV(II,JJ))+PN(F(II,JJ,0))  &
                       *AMIN1(1.,UN(II,JJ))                             
            F(II,JJ,1)=PP(F(II,JJ,1))*AMIN1(1.,UN(II,JJ))+PN(F(II,JJ,1))  &
                       *AMIN1(1.,OV(II,JJ))                             
  924     CONTINUE                                                      
 9000   CONTINUE                                                        
        DO 925 JJ=N2STAR,N2END                                          
          DO 925 II=N1STAR,N1END                                        
            XF(II,JJ,IIM)=W(II,JJ)-(F(II,JJ,1)-F(II,JJ,0))                
  925     CONTINUE
                                                                        
!                                                                       
 2000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                        

