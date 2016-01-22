
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE mexFunction( nlhs, plhs, nrhs, prhs )
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C             Matlab Gateway for the Derivative Function Fun
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      INTEGER nlhs, nrhs
      INTEGER plhs(*), prhs(*)
      INTEGER mxGetPr, mxCreateFull, mxGetM, mxgetN
      INTEGER VPtr, FPtr, RPtr, VdotPtr
      KPP_REAL V(KPP_NVAR), F(KPP_NFIX), RCT(KPP_NREACT)
      KPP_REAL Vdot(KPP_NVAR)

C Check for the right number of input arguments
      IF ( nrhs .ne. 3 ) THEN
         CALL mexErrMsgTxt('Fun requires 3 input vectors: 
     &V(KPP_NVAR), F(KPP_NFIX), RCT(KPP_NREACT)')
      END IF 
C Check for the right number of output arguments
      IF ( nlhs .ne. 1 ) THEN
         CALL mexErrMsgTxt('Fun requires 1 output vector: 
     &Vdot(KPP_NVAR)')
      END IF 

      plhs(1) = mxCreateDoubleMatrix(KPP_NVAR,1,0)

      VPtr = mxGetPr(prhs(1))
      CALL mxCopyPtrToReal8(VPtr,V,KPP_NVAR)
      
      FPtr = mxGetPr(prhs(2))
      CALL mxCopyPtrToReal8(FPtr,F,KPP_NFIX)
      
      RPtr = mxGetPr(prhs(3))
      CALL mxCopyPtrToReal8(RPtr,RCT,KPP_NREACT)

      VdotPtr = mxGetPr(plhs(1))

      CALL Fun( V, F, RCT, Vdot )

      CALL mxCopyReal8ToPtr(Vdot, VdotPtr, KPP_NVAR)

      RETURN 
      END
