Subroutine FFT661  & ! in
 ( ISIGN,              & ! in
   INC,                & ! in
   JUMP,               & ! in
   LOT,                & ! in
   N,                  & ! in
   IFAX,               & ! in
   TRIGS,              & ! in
   A,                  & ! inout
   DIM )                 ! in
!
!
! Description:
!     MULTIPLE FAST SINE TRANSFORM
!     (Originally called FFT661, then Var_SinTrans)
!      author: clive temperton, may 1988 
!       (slightly modified for all-fortran version)
!
!     Sine transform of length n is converted to
!     Real periodic transform of length n by pre- and post-
!     processing. Real periodic transform is performed by
!     pruning redundant operations from complex transform.
!
!     see for example paul swarztrauber, "symmetric fft's",
!     math. comp. 47 (1986), 323-346.
!
! Method:
!
!     ordering of coefficients:   z(0) , z(1) , z(2) , ... , z(n)
!     ordering of data:           x(0) , x(1) , x(2) , ... , x(n)
!
!    vectorization is achieved on cray by doing the transforms
!    in parallel
!
!    N must be composed of factors 2,3 & 5 and must be even
!
!    definition of transforms:
!     -------------------------
!
!     isign=+1: x(i)=sum(j=1,...,n-1)(z(j)*sin(i*j*pi/n))
!
!     isign=-1: z(j)=(2/n)*sum(i=1,...,n-1)(x(i)*sin(i*j*pi/n))
!
! Current Code Owner: Andrew Lorenc
!
!   History:
! Version   Date     Comment
! -------   ----     -------
! 0.1       14/12/93 Original code. Phil Andrews
! 0.2       16/09/94 Small Modifications for the
!                    incorporation in the VAR project. HB
! 1.1       21/04/95 placed under control. JB
! 1.2       01/06/95 Tracing added. JB
!
! Code Description:
!    NB   BECAUSE OF THE TRICKY NESTED LOOPS 
!         ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

Implicit none

! Subroutine arguments
 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for 
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data 
                                           !  vectors  (which include zeros 
                                           ! at the endpoints)
 Integer , intent (in)    :: DIM           ! dimension workspace 
 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array

                                                    ! No descriptions given
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K,JA,JB,IA,IB,IGO,LA,J
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: SI,T1,T2,SCALE, vectorlength
 Real                     :: WORK(DIM)     ! size (n+1)*min(lot,VectorLength)
     
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
      DO 120 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      SI=TRIGS(2*N+K)
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA) = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         WORK(JB) = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         T2 = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
  115    CONTINUE
      ENDIF
  120 CONTINUE

      JA=1
      JB=NH+1
      IA=ISTART
      IB=ISTART+NH*INC
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 130 J=1,NVEX
         WORK(JA)=0.0
         WORK(JB)=2.0*A(IB)
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  130    CONTINUE
         IGO = +1
      ELSE
!DIR$ IVDEP
         DO 135 J=1,NVEX
         A(IA)=0.0
         A(IB)=2.0*A(IB)
         IA=IA+JUMP
         IB=IB+JUMP
  135    CONTINUE
         IGO = -1
      ENDIF
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL qpassm(WORK,WORK(IFAC*LA+1),A(IA),A(LA*INC+IA), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL qpassm(A(IA),A(IFAC*LA*INC+IA),WORK,WORK(LA+1), &
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
      JA=ISTART
      JB=JA+N*INC
      IA=1
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=0.0
      A(JA+INC)=0.5*SCALE*WORK(IA)
      A(JB)=0.0
      IA=IA+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=-SCALE*WORK(IA+1)
      A(JA+INC)=SCALE*WORK(IA)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE

      Go To 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'NVEX=', NVEX ,' GREATER THAN VectorLength'
      GO TO 570
  530 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'IFAC=', IFAC, 'NOT CATERED FOR'
      GO TO 570
  550 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'IFAC=', IFAC, ' ONLY CATERED FOR IF LA*IFAC=N'
  570 CONTINUE


      RETURN


End subroutine FFT661



