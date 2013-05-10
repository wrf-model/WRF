!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: tables.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE TABLES (IDO,IP,WA) 
      REAL  WA(IDO,IP-1,2) 
!                                                                       
      TPI = 8.*ATAN(1.) 
      ARGZ = TPI/REAL(IP) 
      ARG1 = TPI/REAL(IDO*IP) 
      DO 110 J=2,IP 
         ARG2 = REAL(J-1)*ARG1 
         DO 100 I=1,IDO 
            ARG3 = REAL(I-1)*ARG2 
            WA(I,J-1,1) = COS(ARG3) 
            WA(I,J-1,2) = SIN(ARG3) 
  100    CONTINUE 
         IF (IP .LE. 5) GO TO 110 
         ARG4 = REAL(J-1)*ARGZ 
         WA(1,J-1,1) = COS(ARG4) 
         WA(1,J-1,2) = SIN(ARG4) 
  110 END DO 
      RETURN 
      END                                           
