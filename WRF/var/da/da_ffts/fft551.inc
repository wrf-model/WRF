!DECK FFT551
!     SUBROUTINE 'FFT551' - MULTIPLE FAST COSINE TRANSFORM
!
!     AUTHOR: CLIVE TEMPERTON, MAY 1988
!     [ALL-FORTRAN VERSION: C.T., OCTOBER 1995]
!
!     COSINE TRANSFORM OF LENGTH N IS CONVERTED TO
!     REAL PERIODIC TRANSFORM OF LENGTH N BY PRE- AND POST-
!     PROCESSING. REAL PERIODIC TRANSFORM IS PERFORMED BY
!     PRUNING REDUNDANT OPERATIONS FROM COMPLEX TRANSFORM.
!
!     SEE FOR EXAMPLE PAUL SWARZTRAUBER, "SYMMETRIC FFT'S",
!     MATH. COMP. 47 (1986), 323-346.
!
!     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*MIN(LOT,64)
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N+1 IS THE LENGTH OF THE DATA VECTORS
!        (WHICH INCLUDE NONZERO VALUES AT BOTH ENDPOINTS)
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:   Z(0) , Z(1) , Z(2) , ... , Z(N)
!
!     ORDERING OF DATA:           X(0) , X(1) , X(2) , ... , X(N)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS
!     IN PARALLEL
!
!     N MUST BE COMPOSED OF FACTORS 2,3 & 5 AND MUST BE EVEN
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(I)=SUM(J=0,...,N)(E(J)*Z(J)*COS(I*J*PI/N))
!                    WHERE E(J)=0.5 FOR J=0,N --- ELSE E(J)=1
!
!     ISIGN=-1: Z(J)=(2/N)*SUM(I=0,...,N)(E(I)*X(I)*COS(I*J*PI/N))
!
! N.B.  FFT551 has an unusual definition of the FFTs,
!       such that the the coeff of wave0 is NOT the mean.
!
!---------------------------------------------------------------------
Subroutine FFT551 & ! in
 ( ISIGN,                & ! in
   INC,                  & ! in
   JUMP,                 & ! in
   LOT,                  & ! in
   N,                    & ! in
   IFAX,                 & ! in
   TRIGS,                & ! in
   A,                    & ! inout
   IDIM )                   ! in

! Code Description:  ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for 
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data 

 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array                                       !  vectors  (which include zeros 
                                           ! at the endpoints)
 Integer , intent (in)    :: IDIM           ! dimension workspace 

 Real :: WORK(IDIM)                      ! size (n+1)*min(lot,VectorLength)
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K, IC, J, LA, IGO, JA,JB,IA,IB
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: CO,S, t1,t2,si,scale, vectorlength

CHARACTER (LEN=*), PARAMETER :: RoutineName = "Var_FFT551"

      VectorLength = LOT
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      DO 200 NB=1,NBLOX
!
!     PREPROCESSING
!     -------------
      IA=ISTART
      IB=IA+NH*INC
      IC=IA+N*INC
      JA=1
      JB=NH+1
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 105 J=1,NVEX
         T1=0.5*(A(IA)+A(IC))
         T2=0.5*(A(IA)-A(IC))
         A(IA)=T1
         A(IC)=T2
         IA=IA+JUMP
         IC=IC+JUMP
  105    CONTINUE
      ELSE  
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA)=0.5*(A(IA)+A(IC))
         WORK(JB)=A(IB)
         A(IC)=0.5*(A(IA)-A(IC))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ENDIF
!
      DO 130 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      IC=ISTART+N*INC
      SI=TRIGS(2*N+K)
      CO=TRIGS(2*N+NH-K)
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         T2 = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
  115    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 120 J=1,NVEX
         WORK(JA) = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         WORK(JB) = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  120    CONTINUE
      ENDIF
  130 CONTINUE
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
      IGO=1-2*MOD(NFAX,2)
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL qpassm(WORK,WORK(IFAC*LA+1),A(IA),A(IA+LA*INC), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL qpassm(A(IA),A(IA+IFAC*LA*INC),WORK,WORK(LA+1), &
                    TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      ENDIF
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
  140 CONTINUE
!
!     POSTPROCESSING
!     --------------
      SCALE=2.0
      IF (ISIGN.EQ.+1) SCALE = FLOAT(N)
      S=1.0
      IF (ISIGN.EQ.-1) S = 2.0/FLOAT(N)
      JA=ISTART
      JB=JA+N*INC
      IA=1
      IB=N
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=S*A(JB)
      A(JB)=SCALE*WORK(IB)
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE
      GO TO 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(UNIT=0,FMT='(A,I4,A)') &
        'VECTOR LENGTH =',NVEX,', GREATER THAN VectorLength'
      GO TO 570
  530 CONTINUE
      WRITE(UNIT=0,FMT='(A,I3,A)') &
        'FACTOR =',IFAC,', NOT CATERED FOR'
      GO TO 570
  550 CONTINUE
      WRITE(UNIT=0,FMT='(A,I3,A)') &
        'FACTOR =',IFAC,', ONLY CATERED FOR IF LA*IFAC=N'
  570 CONTINUE

      RETURN
      END SUBROUTINE FFT551


