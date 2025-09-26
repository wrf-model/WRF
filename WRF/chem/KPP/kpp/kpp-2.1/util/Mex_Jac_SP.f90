!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE mexFunction( nlhs, plhs, nrhs, prhs )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!          Matlab Gateway for the Sparse Jacobian Function Jac_SP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 USE KPP_ROOT_Model

      INTEGER nlhs, nrhs
      INTEGER plhs(*), prhs(*)
      INTEGER mxGetPr, mxCreateFull, mxGetM, mxgetN
      INTEGER VPtr, FPtr, RPtr, JVSPtr
      KPP_REAL V(KPP_NVAR), F(KPP_NFIX), RCT(KPP_NREACT)
      KPP_REAL JVS(KPP_LU_NONZERO)

! Check for the right number of input arguments
      IF ( nrhs .ne. 3 ) THEN
         CALL mexErrMsgTxt('Jac_SP requires 3 input vectors: &
     &V(KPP_NVAR), F(KPP_NFIX), RCT(KPP_NREACT)')
      END IF 
! Check for the right number of output arguments
      IF ( nlhs .ne. 1 ) THEN
         CALL mexErrMsgTxt('Jac_SP requires 1 output vector: &
     &JVS(KPP_LU_NONZERO)')
      END IF 

      plhs(1) = mxCreateDoubleMatrix(KPP_LU_NONZERO,1,0)

      VPtr = mxGetPr(prhs(1))
      CALL mxCopyPtrToReal8(VPtr,V,KPP_NVAR)
      
      FPtr = mxGetPr(prhs(2))
      CALL mxCopyPtrToReal8(FPtr,F,KPP_NFIX)
      
      RPtr = mxGetPr(prhs(3))
      CALL mxCopyPtrToReal8(RPtr,RCT,KPP_NREACT)

      JVSPtr = mxGetPr(plhs(1))

      CALL Jac_SP( V, F, RCT, JVS )

      CALL mxCopyReal8ToPtr(JVS, JVSPtr, KPP_LU_NONZERO)

 END SUBROUTINE mexFunction
