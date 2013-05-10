/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Matlab Gateway for the Derivative Function Fun 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "mex.h"
#define min( x, y ) (x) < (y) ? (x) : (y)
#define max( x, y ) (x) > (y) ? (x) : (y)

void mexFunction( int nlhs, mxArray *plhs[], 
                     int nrhs, const mxArray *prhs[] )
{
 int mrows, mcols;
 KPP_REAL *V, *F, *RCT, *Vdot;

/* Check for the right number and size of input arguments */
 if ( nrhs != 3 ) {
   mexErrMsgTxt("KPP_ROOT_Fun requires 3 input vectors: V(KPP_NVAR), F(KPP_NFIX), RCT(KPP_NREACT)");
 }
 mrows =  mxGetM(prhs[0]); mcols = mxGetN(prhs[0]);
 if ( ( mrows != KPP_NVAR )||( mcols != 1 ) ) {
   mexPrintf("First KPP_ROOT_Fun input argument is of size V(%d,%d).",  
               mrows, mcols);
   mexErrMsgTxt("First KPP_ROOT_Fun input argument should be a column vector V(KPP_NVAR,1)");
 }
 mrows =  mxGetM(prhs[1]); mcols = mxGetN(prhs[1]);
 if ( ( mrows != KPP_NFIX )||( mcols != 1 ) ) {
   mexPrintf("Second KPP_ROOT_Fun input argument is of size F(%d,%d).",  
               mrows, mcols);
   mexErrMsgTxt("Second KPP_ROOT_Fun input argument should be a column vector F(KPP_NFIX,1)");
 }
 mrows =  mxGetM(prhs[2]); mcols = mxGetN(prhs[2]);
 if ( (  mrows != KPP_NREACT )||( mcols != 1 ) ) {
   mexPrintf("Third KPP_ROOT_Fun input argument is of size RCT(%d,%d).",  
               mrows, mcols);
   mexErrMsgTxt("Third KPP_ROOT_Fun input argument should be a column vector RCT(KPP_NREACT,1)");
 }
 
/* Check for the right number of output arguments */
 if ( nlhs != 1 ) {
   mexErrMsgTxt("KPP_ROOT_Fun requires 1 output column vector: Vdot(KPP_NVAR)");
 }

 V   = mxGetPr(prhs[0]);
 F   = mxGetPr(prhs[1]);
 RCT = mxGetPr(prhs[2]);

 plhs[0] = mxCreateDoubleMatrix(KPP_NVAR,1,mxREAL);
 Vdot = mxGetPr(plhs[0]);

 Fun( V, F, RCT, Vdot );

}
