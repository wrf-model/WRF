      SUBROUTINE GH2TV(GZ, TV,    PS,    TOPO, 
     1					A, B, IMAX, JMAX, LMAX,
     W					PK,PBETA,DPHI,ALPHA)
C----------------------------------------------------------------------         
C     INPUT   GZ  (I,J,L) : GEOPOTENTIAL HEIGHT(M)                              
C             PS  (I,J)   : SURFACE PRESSURE(HPA)                               
C             TOPO(I,J)   : SURFACE GEOPOTENTIAL(GPM)
C             A(L), B(L)  : A+B*PS DEFINITION OF ETA-HALF LEVEL                 
C     OUTPUT  TV  (I,J,L) : VIRTUAL TEMPERATURE(K)                              
C----------------------------------------------------------------------         
      DIMENSION Z(IMAX*JMAX,LMAX), TV(IMAX*JMAX,LMAX), PS(IMAX*JMAX),           
     1     A(LMAX), B(LMAX),TOPO(IMAX*JMAX),GZ(IMAX*JMAX,LMAX)
      DIMENSION    PK(IMAX*JMAX,LMAX),PBETA(IMAX*JMAX,LMAX),                     
     1          ALPHA(IMAX*JMAX,LMAX), DPHI(IMAX*JMAX,LMAX)                      
	  DIMENSION VMAX(LMAX),VMIN(LMAX)
           
		Z=GZ*gravity
C......... PK is pressure at half level
        DO L = 1, LMAX
        DO I = 1, IMAX*JMAX
          PK(I,L) = A(L)+B(L)*PS(I)
		END DO
		END DO
C...... DPHI is DEL PHI
		WRITE(*,*)'MAX PHI(1)=',MAXVAL(Z(:,1)),' MAX TOPO=',MAXVAL(TOPO)
		DO I=1, IMAX*JMAX
			DPHI(I,1)=Z(I,1)-TOPO(I)
		END DO
		DO L=2,LMAX
		DO I=1, IMAX*JMAX
			DPHI(I,L)=Z(I,L)-Z(I,L-1)
		END DO
		END DO
C
C		VMAX=MAXVAL(DPHI,DIM=1)
C		VMIN=MINVAL(DPHI,DIM=1)
C		WRITE(*,*)' LEVEL    MAX         MIN   OF DPHI'
C		DO L=1,LMAX
C			WRITE(*,'(I3,2F13.4)')L,VMAX(L),VMIN(L)
C		END DO
C		WRITE(*,*)'MAX(1)=',VMAX(1)
C....... PBETA is ln(Pk-1/2)-ln(Pk+1/2)
        DO L = 1, LMAX-1                                                    
        DO I = 1, IMAX*JMAX                                                 
			PBETA(I,L)=LOG(PK(I,L)/PK(I,L+1))
		END DO
		END DO
C....... ALPHA is ALPHA(K)
        DO L = 1, LMAX-1                                                    
        DO I = 1, IMAX*JMAX                                                 
			ALPHA(I,L)=1-PK(I,L+1)*PBETA(I,L)/
     1				  (PK(I,L)-PK(I,L+1))
		END DO
		END DO
        DO I = 1, IMAX*JMAX                                                 
			ALPHA(I,LMAX)=LOG(2.0)
		END DO
C
C		VMAX=MAXVAL(ALPHA,DIM=1)
C		VMIN=MINVAL(ALPHA,DIM=1)
C		WRITE(*,*)' LEVEL    MAX         MIN   OF ALPHA'
C		DO L=1,LMAX
C			WRITE(*,'(I3,2F13.8)')L,VMAX(L),VMIN(L)
C		END DO
C........ CACULATE Tv
		DO I = 1, IMAX*JMAX
			TV(I,1)=DPHI(I,1)/(ALPHA(I,1)*gas_constant)
		END DO
		DO L = 2, LMAX
		DO I = 1, IMAX*JMAX
		  TV(I,L)=(DPHI(I,L)+gas_constant*TV(I,L-1)*
     1                    (ALPHA(I,L-1)-PBETA(I,L-1)))/
     1			  (ALPHA(I,L)*gas_constant)
		END DO
		END DO
C
      RETURN                                                                    
      END SUBROUTINE GH2TV
