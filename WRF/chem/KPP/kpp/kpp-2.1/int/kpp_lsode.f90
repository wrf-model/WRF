!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  LSODE - Stiff method based on backward differentiation formulas (BDF)  !
!  By default the code employs the KPP sparse linear algebra routines     !
!  Compile with -DFULL_ALGEBRA to use full linear algebra (LAPACK)        !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! A. Sandu - version of July 2005

MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Precision
  USE KPP_ROOT_Global, ONLY: FIX, RCONST, TIME, ATOL, RTOL
  USE KPP_ROOT_Parameters, ONLY: NVAR, NSPEC, NFIX, LU_NONZERO
  USE KPP_ROOT_JacobianSP, ONLY: LU_DIAG
  USE KPP_ROOT_LinearAlgebra, ONLY: KppDecomp, KppSolve, &
               Set2zero, WLAMCH
  
  IMPLICIT NONE
  PUBLIC
  SAVE
  
  !~~~>  Statistics on the work performed by the LSODE method
  INTEGER :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
  INTEGER, PARAMETER :: ifun=1, ijac=2, istp=3, iacc=4,  &
    irej=5, idec=6, isol=7, isng=8, itexit=1, ihexit=2
  !  SDIRK method coefficients
  KPP_REAL :: rkAlpha(5,4), rkBeta(5,4), rkD(4,5),  &
                   rkGamma, rkA(5,5), rkB(5), rkC(5)

  ! mz_rs_20050717: TODO: use strings of IERR_NAMES for error messages
  ! description of the error numbers IERR
  CHARACTER(LEN=50), PARAMETER, DIMENSION(-8:1) :: IERR_NAMES = (/ &
    'Matrix is repeatedly singular                     ', & ! -8
    'Step size too small                               ', & ! -7
    'No of steps exceeds maximum bound                 ', & ! -6
    'Improper tolerance values                         ', & ! -5
    'FacMin/FacMax/FacRej must be positive             ', & ! -4
    'Hmin/Hmax/Hstart must be positive                 ', & ! -3
    'Improper value for maximal no of Newton iterations', & ! -2
    'Improper value for maximal no of steps            ', & ! -1
    '                                                  ', & !  0 (not used)
    'Success                                           ' /) !  1

CONTAINS

SUBROUTINE INTEGRATE( TIN, TOUT, &
  ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )

   USE KPP_ROOT_Parameters
   USE KPP_ROOT_Global
   IMPLICIT NONE

   KPP_REAL, INTENT(IN) :: TIN  ! Start Time
   KPP_REAL, INTENT(IN) :: TOUT ! End Time
   ! Optional input parameters and statistics
   INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
   KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
   KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

   KPP_REAL :: RCNTRL(20), RSTATUS(20)
   INTEGER       :: ICNTRL(20), ISTATUS(20), IERR
!!$   INTEGER, SAVE :: Ntotal = 0

   ICNTRL(:)  = 0
   RCNTRL(:)  = 0.0_dp
   ISTATUS(:) = 0
   RSTATUS(:) = 0.0_dp

   ICNTRL(5) = 2 ! maximal order

   ! If optional parameters are given, and if they are >0, 
   ! then they overwrite default settings. 
   IF (PRESENT(ICNTRL_U)) THEN
     WHERE(ICNTRL_U(:) > 0) ICNTRL(:) = ICNTRL_U(:)
   END IF
   IF (PRESENT(RCNTRL_U)) THEN
     WHERE(RCNTRL_U(:) > 0) RCNTRL(:) = RCNTRL_U(:)
   END IF

   CALL KppLsode( TIN,TOUT,VAR,RTOL,ATOL,                &
                  RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR )

! mz_rs_20050716: IERR and ISTATUS are returned to the user who then
! decides what to do about it, i.e. either stop the run or ignore it.
!!$   IF (IERR < 0) THEN
!!$        PRINT*,'LSODE: Unsuccessful exit at T=',TIN,' (IERR=',IERR,')'
!!$        STOP
!!$   ENDIF
!!$   Ntotal = Ntotal + ISTATUS(3)
!!$   PRINT*,'Nsteps = ', ISTATUS(3),'  (',Ntotal,')'

   STEPMIN = RSTATUS(ihexit) ! Save last step
   
   ! if optional parameters are given for output they to return information
   IF (PRESENT(ISTATUS_U)) ISTATUS_U(:) = ISTATUS(1:20)
   IF (PRESENT(RSTATUS_U)) RSTATUS_U(:) = RSTATUS(1:20)
   IF (PRESENT(IERR_U))    IERR_U       = IERR

   END SUBROUTINE INTEGRATE

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE KppLsode( TIN,TOUT,Y,RelTol,AbsTol,      &
               RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!       corresponding variables are used.
!
!    Note: For input parameters equal to zero the default values of the
!          corresponding variables are used.
!~~~>  
!    ICNTRL(1) = not used
!
!    ICNTRL(2) = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1: AbsTol, RelTol are scalars
!
!    ICNTRL(3) = not used
!
!    ICNTRL(4)  -> maximum number of integration steps
!        For ICNTRL(4)=0 the default value of 100000 is used
!
!    ICNTRL(5)  -> maximum order of the integration formula allowed
!
!~~~>  Real parameters
!
!    RCNTRL(1)  -> Hmin, lower bound for the integration step size
!                  It is strongly recommended to keep Hmin = ZERO
!    RCNTRL(2)  -> Hmax, upper bound for the integration step size
!    RCNTRL(3)  -> Hstart, starting value for the integration step size
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     OUTPUT PARAMETERS:
!
!    Note: each call to Rosenbrock adds the current no. of fcn calls
!      to previous value of ISTATUS(1), and similar for the other params.
!      Set ISTATUS(1:10) = 0 before call to avoid this accumulation.
!
!    ISTATUS(1) = No. of function calls
!    ISTATUS(2) = No. of jacobian calls
!    ISTATUS(3) = No. of steps
!
!    RSTATUS(1)  -> Texit, the time corresponding to the
!                   computed Y upon return
!    RSTATUS(2)  -> Hexit, last predicted step before exit
!    For multiple restarts, use Hexit as Hstart in the following run
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
      KPP_REAL :: Y(NVAR), AbsTol(NVAR), RelTol(NVAR), TIN, TOUT
      KPP_REAL :: RCNTRL(20), RSTATUS(20)
      INTEGER       :: ICNTRL(20), ISTATUS(20)
      INTEGER, PARAMETER :: LRW = 25 + 9*NVAR+2*NVAR*NVAR, &
                            LIW = 32 + NVAR
      KPP_REAL :: RWORK(LRW), RPAR(1)
      INTEGER :: IWORK(LIW), IPAR(1), ITOL, ITASK,         &
                 IERR, IOPT, MF

      !~~~> NORMAL COMPUTATION
      ITASK  = 1
      IERR   = 1
      IOPT   = 1     ! 0=no/1=use optional input
      
      RWORK(1:30) = 0.0d0
      IWORK(1:30) = 0
      
      IF (ICNTRL(2)==0) THEN
         ITOL = 4           ! Abs/RelTol are both vectors
      ELSE              
         ITOL = 1           ! Abs/RelTol are both scalars
      END IF   
      IWORK(6) = ICNTRL(4)  ! max number of internal steps 
      IWORK(5) = ICNTRL(5)  ! maximal order

      MF = 21  !~~~> stiff case, analytic full Jacobian

      RWORK(5) = RCNTRL(3)  ! Hstart
      RWORK(6) = RCNTRL(2)  ! Hmax                    
      RWORK(7) = RCNTRL(1)  ! Hmin                    

      CALL DLSODE ( FUN_CHEM, NVAR, Y, TIN, TOUT, ITOL, RelTol, AbsTol, ITASK,&
                    IERR, IOPT, RWORK, LRW, IWORK, LIW, JAC_CHEM, MF)

      ISTATUS(1) = IWORK(12) ! Number of function evaluations 
      ISTATUS(2) = IWORK(13) ! Number of Jacobian evaluations   
      ISTATUS(3) = IWORK(11) ! Number of steps

      RSTATUS(1) = TOUT      ! mz_rs_20050717
      RSTATUS(2) = RWORK(11) ! mz_rs_20050717

      END SUBROUTINE KppLsode
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!DECK DLSODE                                                            
      SUBROUTINE DLSODE (F, NEQ, Y, T, TOUT, ITOL, RelTol, AbsTol, ITASK,   &
                       ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF)  
      EXTERNAL F, JAC 
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, LIW, IWORK(LIW), MF 
      KPP_REAL Y(*), T, TOUT, RelTol(*), AbsTol(*), RWORK(LRW)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!***BEGIN PROLOGUE  DLSODE                                              
!***PURPOSE  Livermore Solver for Ordinary Differential Equations.      
!            DLSODE solves the initial-value problem for stiff or       
!            nonstiff systems of first-order ODE's,                     
!               dy/dt = f(t,y),   or, in component form,                
!               dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(N)),  i=1,...,N.
!***CATEGORY  I1A                                                       
!***TYPE      KPP_REAL (SLSODE-S, DLSODE-D)                     
!***KEYWORDS  ORDINARY DIFFERENTIAL EQUATIONS, INITIAL VALUE PROBLEM,   
!             STIFF, NONSTIFF                                           
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!             Center for Applied Scientific Computing, L-561            
!             Lawrence Livermore National Laboratory                    
!             Livermore, CA 94551.                                      
!***DESCRIPTION                                                         
!                                                                       
!     NOTE: The "Usage" and "Arguments" sections treat only a subset of 
!           available options, in condensed fashion.  The options       
!           covered and the information supplied will support most      
!           standard uses of DLSODE.                                    
!                                                                       
!           For more sophisticated uses, full details on all options are
!           given in the concluding section, headed "Long Description." 
!           A synopsis of the DLSODE Long Description is provided at the
!           beginning of that section; general topics covered are:      
!           - Elements of the call sequence; optional input and output  
!           - Optional supplemental routines in the DLSODE package      
!           - internal COMMON block                                     
!                                                                       
! *Usage:                                                               
!     Communication between the user and the DLSODE package, for normal 
!     situations, is summarized here.  This summary describes a subset  
!     of the available options.  See "Long Description" for complete    
!     details, including optional communication, nonstandard options,   
!     and instructions for special situations.                          
!                                                                       
!     A sample program is given in the "Examples" section.              
!                                                                       
!     Refer to the argument descriptions for the definitions of the     
!     quantities that appear in the following sample declarations.      
!                                                                       
!     For MF = 10,                                                      
!        PARAMETER  (LRW = 20 + 16*NEQ,           LIW = 20)             
!     For MF = 21 or 22,                                                
!        PARAMETER  (LRW = 22 +  9*NEQ + NEQ**2,  LIW = 20 + NEQ)       
!     For MF = 24 or 25,                                                
!        PARAMETER  (LRW = 22 + 10*NEQ + (2*ML+MU)*NEQ,                 
!       *                                         LIW = 20 + NEQ)       
!                                                                       
!        EXTERNAL F, JAC                                                
!        INTEGER  NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK(LIW),      
!       *         LIW, MF                                               
!        KPP_REAL Y(NEQ), T, TOUT, RelTol, AbsTol(ntol), RWORK(LRW) 
!                                                                       
!        CALL DLSODE (F, NEQ, Y, T, TOUT, ITOL, RelTol, AbsTol, ITASK,      
!       *            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF)     
!                                                                       
! *Arguments:                                                           
!     F     :EXT    Name of subroutine for right-hand-side vector f.    
!                   This name must be declared EXTERNAL in calling      
!                   program.  The form of F must be:                    
!                                                                       
!                   SUBROUTINE  F (NEQ, T, Y, YDOT)                     
!                   INTEGER  NEQ                                        
!                   KPP_REAL  T, Y(*), YDOT(*)                  
!                                                                       
!                   The inputs are NEQ, T, Y.  F is to set              
!                                                                       
!                   YDOT(i) = f(i,T,Y(1),Y(2),...,Y(NEQ)),              
!                                                     i = 1, ..., NEQ . 
!                                                                       
!     NEQ   :IN     Number of first-order ODE's.                        
!                                                                       
!     Y     :INOUT  Array of values of the y(t) vector, of length NEQ.  
!                   Input:  For the first call, Y should contain the    
!                           values of y(t) at t = T. (Y is an input     
!                           variable only if ISTATE = 1.)               
!                   Output: On return, Y will contain the values at the 
!                           new t-value.                                
!                                                                       
!     T     :INOUT  Value of the independent variable.  On return it    
!                   will be the current value of t (normally TOUT).     
!                                                                       
!     TOUT  :IN     Next point where output is desired (.NE. T).        
!                                                                       
!     ITOL  :IN     1 or 2 according as AbsTol (below) is a scalar or     
!                   an array.                                           
!                                                                       
!     RelTol  :IN     Relative tolerance parameter (scalar).              
!                                                                       
!     AbsTol  :IN     Absolute tolerance parameter (scalar or array).     
!                   If ITOL = 1, AbsTol need not be dimensioned.          
!                   If ITOL = 2, AbsTol must be dimensioned at least NEQ. 
!                                                                       
!                   The estimated local error in Y(i) will be controlled
!                   so as to be roughly less (in magnitude) than        
!                                                                       
!                   EWT(i) = RelTol*ABS(Y(i)) + AbsTol     if ITOL = 1, or  
!                   EWT(i) = RelTol*ABS(Y(i)) + AbsTol(i)  if ITOL = 2.     
!                                                                       
!                   Thus the local error test passes if, in each        
!                   component, either the absolute error is less than   
!                   AbsTol (or AbsTol(i)), or the relative error is less    
!                   than RelTol.                                          
!                                                                       
!                   Use RelTol = 0.0 for pure absolute error control, and 
!                   use AbsTol = 0.0 (or AbsTol(i) = 0.0) for pure relative 
!                   error control.  Caution:  Actual (global) errors may
!                   exceed these local tolerances, so choose them       
!                   conservatively.                                     
!                                                                       
!     ITASK :IN     Flag indicating the task DLSODE is to perform.      
!                   Use ITASK = 1 for normal computation of output      
!                   values of y at t = TOUT.                            
!                                                                       
!     ISTATE:INOUT  Index used for input and output to specify the state
!                   of the calculation.                                 
!                   Input:                                              
!                    1   This is the first call for a problem.          
!                    2   This is a subsequent call.                     
!                   Output:                                             
!                    1   Nothing was done, because TOUT was equal to T. 
!                    2   DLSODE was successful (otherwise, negative).   
!                        Note that ISTATE need not be modified after a  
!                        successful return.                             
!                   -1   Excess work done on this call (perhaps wrong   
!                        MF).                                           
!                   -2   Excess accuracy requested (tolerances too      
!                        small).                                        
!                   -3   Illegal input detected (see printed message).  
!                   -4   Repeated error test failures (check all        
!                        inputs).                                       
!                   -5   Repeated convergence failures (perhaps bad     
!                        Jacobian supplied or wrong choice of MF or     
!                        tolerances).                                   
!                   -6   Error weight became zero during problem        
!                        (solution component i vanished, and AbsTol or    
!                        AbsTol(i) = 0.).                                 
!                                                                       
!     IOPT  :IN     Flag indicating whether optional inputs are used:   
!                   0   No.                                             
!                   1   Yes.  (See "Optional inputs" under "Long        
!                       Description," Part 1.)                          
!                                                                       
!     RWORK :WORK   Real work array of length at least:                 
!                   20 + 16*NEQ                    for MF = 10,         
!                   22 +  9*NEQ + NEQ**2           for MF = 21 or 22,   
!                   22 + 10*NEQ + (2*ML + MU)*NEQ  for MF = 24 or 25.   
!                                                                       
!     LRW   :IN     Declared length of RWORK (in user's DIMENSION       
!                   statement).                                         
!                                                                       
!     IWORK :WORK   Integer work array of length at least:              
!                   20        for MF = 10,                              
!                   20 + NEQ  for MF = 21, 22, 24, or 25.               
!                                                                       
!                   If MF = 24 or 25, input in IWORK(1),IWORK(2) the    
!                   lower and upper Jacobian half-bandwidths ML,MU.     
!                                                                       
!                   On return, IWORK contains information that may be   
!                   of interest to the user:                            
!                                                                       
!            Name   Location   Meaning                                  
!            -----  ---------  -----------------------------------------
!            NST    IWORK(11)  Number of steps taken for the problem so 
!                              far.                                     
!            NFE    IWORK(12)  Number of f evaluations for the problem  
!                              so far.                                  
!            NJE    IWORK(13)  Number of Jacobian evaluations (and of   
!                              matrix LU decompositions) for the problem
!                              so far.                                  
!            NQU    IWORK(14)  Method order last used (successfully).   
!            LENRW  IWORK(17)  Length of RWORK actually required.  This 
!                              is defined on normal returns and on an   
!                              illegal input return for insufficient    
!                              storage.                                 
!            LENIW  IWORK(18)  Length of IWORK actually required.  This 
!                              is defined on normal returns and on an   
!                              illegal input return for insufficient    
!                              storage.                                 
!                                                                       
!     LIW   :IN     Declared length of IWORK (in user's DIMENSION       
!                   statement).                                         
!                                                                       
!     JAC   :EXT    Name of subroutine for Jacobian matrix (MF =        
!                   21 or 24).  If used, this name must be declared     
!                   EXTERNAL in calling program.  If not used, pass a   
!                   dummy name.  The form of JAC must be:               
!                                                                       
!                   SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD)      
!                   INTEGER  NEQ, ML, MU, NROWPD                        
!                   KPP_REAL  T, Y(*), PD(NROWPD,*)             
!                                                                       
!                   See item c, under "Description" below for more      
!                   information about JAC.                              
!                                                                       
!     MF    :IN     Method flag.  Standard values are:                  
!                   10  Nonstiff (Adams) method, no Jacobian used.      
!                   21  Stiff (BDF) method, user-supplied full Jacobian.
!                   22  Stiff method, internally generated full         
!                       Jacobian.                                       
!                   24  Stiff method, user-supplied banded Jacobian.    
!                   25  Stiff method, internally generated banded       
!                       Jacobian.                                       
!                                                                       
! *Description:                                                         
!     DLSODE solves the initial value problem for stiff or nonstiff     
!     systems of first-order ODE's,                                     
!                                                                       
!        dy/dt = f(t,y) ,                                               
!                                                                       
!     or, in component form,                                            
!                                                                       
!        dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ))                  
!                                                  (i = 1, ..., NEQ) .  
!                                                                       
!     DLSODE is a package based on the GEAR and GEARB packages, and on  
!     the October 23, 1978, version of the tentative ODEPACK user       
!     interface standard, with minor modifications.                     
!                                                                       
!     The steps in solving such a problem are as follows.               
!                                                                       
!     a. First write a subroutine of the form                           
!                                                                       
!           SUBROUTINE  F (NEQ, T, Y, YDOT)                             
!           INTEGER  NEQ                                                
!           KPP_REAL  T, Y(*), YDOT(*)                          
!                                                                       
!        which supplies the vector function f by loading YDOT(i) with   
!        f(i).                                                          
!                                                                       
!     b. Next determine (or guess) whether or not the problem is stiff. 
!        Stiffness occurs when the Jacobian matrix df/dy has an         
!        eigenvalue whose real part is negative and large in magnitude  
!        compared to the reciprocal of the t span of interest.  If the  
!        problem is nonstiff, use method flag MF = 10.  If it is stiff, 
!        there are four standard choices for MF, and DLSODE requires the
!        Jacobian matrix in some form.  This matrix is regarded either  
!        as full (MF = 21 or 22), or banded (MF = 24 or 25).  In the    
!        banded case, DLSODE requires two half-bandwidth parameters ML  
!        and MU. These are, respectively, the widths of the lower and   
!        upper parts of the band, excluding the main diagonal.  Thus the
!        band consists of the locations (i,j) with                      
!                                                                       
!           i - ML <= j <= i + MU ,                                     
!                                                                       
!        and the full bandwidth is ML + MU + 1 .                        
!                                                                       
!     c. If the problem is stiff, you are encouraged to supply the      
!        Jacobian directly (MF = 21 or 24), but if this is not feasible,
!        DLSODE will compute it internally by difference quotients (MF =
!        22 or 25).  If you are supplying the Jacobian, write a         
!        subroutine of the form                                         
!                                                                       
!           SUBROUTINE  JAC (NEQ, T, Y, ML, MU, PD, NROWPD)             
!           INTEGER  NEQ, ML, MU, NRWOPD                                
!           KPP_REAL  T, Y(*), PD(NROWPD,*)                     
!                                                                       
!        which provides df/dy by loading PD as follows:                 
!        - For a full Jacobian (MF = 21), load PD(i,j) with df(i)/dy(j),
!          the partial derivative of f(i) with respect to y(j).  (Ignore
!          the ML and MU arguments in this case.)                       
!        - For a banded Jacobian (MF = 24), load PD(i-j+MU+1,j) with    
!          df(i)/dy(j); i.e., load the diagonal lines of df/dy into the 
!          rows of PD from the top down.                                
!        - In either case, only nonzero elements need be loaded.        
!                                                                       
!     d. Write a main program that calls subroutine DLSODE once for each
!        point at which answers are desired.  This should also provide  
!        for possible use of logical unit 6 for output of error messages
!        by DLSODE.                                                     
!                                                                       
!        Before the first call to DLSODE, set ISTATE = 1, set Y and T to
!        the initial values, and set TOUT to the first output point.  To
!        continue the integration after a successful return, simply     
!        reset TOUT and call DLSODE again.  No other parameters need be 
!        reset.                                                         
!                                                                       
! *Examples:                                                            
!     The following is a simple example problem, with the coding needed 
!     for its solution by DLSODE. The problem is from chemical kinetics,
!     and consists of the following three rate equations:               
!                                                                       
!        dy1/dt = -.04*y1 + 1.E4*y2*y3                                  
!        dy2/dt = .04*y1 - 1.E4*y2*y3 - 3.E7*y2**2                      
!        dy3/dt = 3.E7*y2**2                                            
!                                                                       
!     on the interval from t = 0.0 to t = 4.E10, with initial conditions
!     y1 = 1.0, y2 = y3 = 0. The problem is stiff.                      
!                                                                       
!     The following coding solves this problem with DLSODE, using       
!     MF = 21 and printing results at t = .4, 4., ..., 4.E10.  It uses  
!     ITOL = 2 and AbsTol much smaller for y2 than for y1 or y3 because y2
!     has much smaller values.  At the end of the run, statistical      
!     quantities of interest are printed.                               
!                                                                       
!        EXTERNAL  FEX, JEX                                             
!        INTEGER  IOPT, IOUT, ISTATE, ITASK, ITOL, IWORK(23), LIW, LRW, 
!       *         MF, NEQ                                               
!        KPP_REAL  AbsTol(3), RelTol, RWORK(58), T, TOUT, Y(3)      
!        NEQ = 3                                                        
!        Y(1) = 1.D0                                                    
!        Y(2) = 0.D0                                                    
!        Y(3) = 0.D0                                                    
!        T = 0.D0                                                       
!        TOUT = .4D0                                                    
!        ITOL = 2                                                       
!        RelTol = 1.D-4                                                   
!        AbsTol(1) = 1.D-6                                                
!        AbsTol(2) = 1.D-10                                               
!        AbsTol(3) = 1.D-6                                                
!        ITASK = 1                                                      
!        ISTATE = 1                                                     
!        IOPT = 0                                                       
!        LRW = 58                                                       
!        LIW = 23                                                       
!        MF = 21                                                        
!        DO 40 IOUT = 1,12                                              
!          CALL DLSODE (FEX, NEQ, Y, T, TOUT, ITOL, RelTol, AbsTol, ITASK,  
!       *               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)  
!          WRITE(6,20)  T, Y(1), Y(2), Y(3)                             
!    20    FORMAT(' At t =',D12.4,'   y =',3D14.6)                      
!          IF (ISTATE .LT. 0)  GO TO 80                                 
!    40    TOUT = TOUT*10.D0                                            
!        WRITE(6,60)  IWORK(11), IWORK(12), IWORK(13)                   
!    60  FORMAT(/' No. steps =',i4,',  No. f-s =',i4,',  No. J-s =',i4) 
!        STOP                                                           
!    80  WRITE(6,90)  ISTATE                                            
!    90  FORMAT(///' Error halt.. ISTATE =',I3)                         
!        STOP                                                           
!        END                                                            
!                                                                       
!        SUBROUTINE  FEX (NEQ, T, Y, YDOT)                              
!        INTEGER  NEQ                                                   
!        KPP_REAL  T, Y(3), YDOT(3)                             
!        YDOT(1) = -.04D0*Y(1) + 1.D4*Y(2)*Y(3)                         
!        YDOT(3) = 3.D7*Y(2)*Y(2)                                       
!        YDOT(2) = -YDOT(1) - YDOT(3)                                   
!        RETURN                                                         
!        END                                                            
!                                                                       
!        SUBROUTINE  JEX (NEQ, T, Y, ML, MU, PD, NRPD)                  
!        INTEGER  NEQ, ML, MU, NRPD                                     
!        KPP_REAL  T, Y(3), PD(NRPD,3)                          
!        PD(1,1) = -.04D0                                               
!        PD(1,2) = 1.D4*Y(3)                                            
!        PD(1,3) = 1.D4*Y(2)                                            
!        PD(2,1) = .04D0                                                
!        PD(2,3) = -PD(1,3)                                             
!        PD(3,2) = 6.D7*Y(2)                                            
!        PD(2,2) = -PD(1,2) - PD(3,2)                                   
!        RETURN                                                         
!        END                                                            
!                                                                       
!     The output from this program (on a Cray-1 in single precision)    
!     is as follows.                                                    
!                                                                       
!     At t =  4.0000e-01   y =  9.851726e-01  3.386406e-05  1.479357e-02
!     At t =  4.0000e+00   y =  9.055142e-01  2.240418e-05  9.446344e-02
!     At t =  4.0000e+01   y =  7.158050e-01  9.184616e-06  2.841858e-01
!     At t =  4.0000e+02   y =  4.504846e-01  3.222434e-06  5.495122e-01
!     At t =  4.0000e+03   y =  1.831701e-01  8.940379e-07  8.168290e-01
!     At t =  4.0000e+04   y =  3.897016e-02  1.621193e-07  9.610297e-01
!     At t =  4.0000e+05   y =  4.935213e-03  1.983756e-08  9.950648e-01
!     At t =  4.0000e+06   y =  5.159269e-04  2.064759e-09  9.994841e-01
!     At t =  4.0000e+07   y =  5.306413e-05  2.122677e-10  9.999469e-01
!     At t =  4.0000e+08   y =  5.494530e-06  2.197825e-11  9.999945e-01
!     At t =  4.0000e+09   y =  5.129458e-07  2.051784e-12  9.999995e-01
!     At t =  4.0000e+10   y = -7.170603e-08 -2.868241e-13  1.000000e+00
!                                                                       
!     No. steps = 330,  No. f-s = 405,  No. J-s = 69                    
!                                                                       
! *Accuracy:                                                            
!     The accuracy of the solution depends on the choice of tolerances  
!     RelTol and AbsTol.  Actual (global) errors may exceed these local     
!     tolerances, so choose them conservatively.                        
!                                                                       
! *Cautions:                                                            
!     The work arrays should not be altered between calls to DLSODE for 
!     the same problem, except possibly for the conditional and optional
!     inputs.                                                           
!                                                                       
! *Portability:                                                         
!     Since NEQ is dimensioned inside DLSODE, some compilers may object 
!     to a call to DLSODE with NEQ a scalar variable.  In this event,   
!     use DIMENSION NEQ.  Similar remarks apply to RelTol and AbsTol.    
!                                                                       
!     Note to Cray users:                                               
!     For maximum efficiency, use the CFT77 compiler.  Appropriate      
!     compiler optimization directives have been inserted for CFT77.    
!                                                                       
! *Reference:                                                           
!     Alan C. Hindmarsh, "ODEPACK, A Systematized Collection of ODE     
!     Solvers," in Scientific Computing, R. S. Stepleman, et al., Eds.  
!     (North-Holland, Amsterdam, 1983), pp. 55-64.                      
!                                                                       
! *Long Description:                                                    
!     The following complete description of the user interface to       
!     DLSODE consists of four parts:                                    
!                                                                       
!     1.  The call sequence to subroutine DLSODE, which is a driver     
!         routine for the solver.  This includes descriptions of both   
!         the call sequence arguments and user-supplied routines.       
!         Following these descriptions is a description of optional     
!         inputs available through the call sequence, and then a        
!         description of optional outputs in the work arrays.           
!                                                                       
!     2.  Descriptions of other routines in the DLSODE package that may 
!         be (optionally) called by the user.  These provide the ability
!         to alter error message handling, save and restore the internal
!         COMMON, and obtain specified derivatives of the solution y(t).
!                                                                       
!     3.  Descriptions of COMMON block to be declared in overlay or     
!         similar environments, or to be saved when doing an interrupt  
!         of the problem and continued solution later.                  
!                                                                       
!     4.  Description of two routines in the DLSODE package, either of  
!         which the user may replace with his own version, if desired.  
!         These relate to the measurement of errors.                    
!                                                                       
!                                                                       
!                         Part 1.  Call Sequence                        
!                         ----------------------                        
!                                                                       
!     Arguments                                                         
!     ---------                                                         
!     The call sequence parameters used for input only are              
!                                                                       
!        F, NEQ, TOUT, ITOL, RelTol, AbsTol, ITASK, IOPT, LRW, LIW, JAC, MF,
!                                                                       
!     and those used for both input and output are                      
!                                                                       
!        Y, T, ISTATE.                                                  
!                                                                       
!     The work arrays RWORK and IWORK are also used for conditional and 
!     optional inputs and optional outputs.  (The term output here      
!     refers to the return from subroutine DLSODE to the user's calling 
!     program.)                                                         
!                                                                       
!     The legality of input parameters will be thoroughly checked on the
!     initial call for the problem, but not checked thereafter unless a 
!     change in input parameters is flagged by ISTATE = 3 on input.     
!                                                                       
!     The descriptions of the call arguments are as follows.            
!                                                                       
!     F        The name of the user-supplied subroutine defining the ODE
!              system.  The system must be put in the first-order form  
!              dy/dt = f(t,y), where f is a vector-valued function of   
!              the scalar t and the vector y. Subroutine F is to compute
!              the function f. It is to have the form                   
!                                                                       
!                 SUBROUTINE F (NEQ, T, Y, YDOT)                        
!                 KPP_REAL  T, Y(*), YDOT(*)                    
!                                                                       
!              where NEQ, T, and Y are input, and the array YDOT =      
!              f(T,Y) is output.  Y and YDOT are arrays of length NEQ.  
!              Subroutine F should not alter Y(1),...,Y(NEQ).  F must be
!              declared EXTERNAL in the calling program.                
!                                                                       
!              Subroutine F may access user-defined quantities in       
!              NEQ(2),... and/or in Y(NEQ+1),..., if NEQ is an array 
!              (dimensioned in F) and/or Y has length exceeding NEQ. 
!              See the descriptions of NEQ and Y below.                 
!                                                                       
!              If quantities computed in the F routine are needed       
!              externally to DLSODE, an extra call to F should be made  
!              for this purpose, for consistent and accurate results.   
!              If only the derivative dy/dt is needed, use DINTDY       
!              instead.                                                 
!                                                                       
!     NEQ      The size of the ODE system (number of first-order        
!              ordinary differential equations).  Used only for input.  
!              NEQ may be decreased, but not increased, during the      
!              problem.  If NEQ is decreased (with ISTATE = 3 on input),
!              the remaining components of Y should be left undisturbed,
!              if these are to be accessed in F and/or JAC.             
!                                                                       
!              Normally, NEQ is a scalar, and it is generally referred  
!              to as a scalar in this user interface description.       
!              However, NEQ may be an array, with NEQ set to the     
!              system size.  (The DLSODE package accesses only NEQ.) 
!              In either case, this parameter is passed as the NEQ      
!              argument in all calls to F and JAC.  Hence, if it is an  
!              array, locations NEQ(2),... may be used to store other   
!              integer data and pass it to F and/or JAC.  Subroutines   
!              F and/or JAC must include NEQ in a DIMENSION statement   
!              in that case.                                            
!                                                                       
!     Y        A real array for the vector of dependent variables, of   
!              length NEQ or more.  Used for both input and output on   
!              the first call (ISTATE = 1), and only for output on      
!              other calls.  On the first call, Y must contain the      
!              vector of initial values.  On output, Y contains the     
!              computed solution vector, evaluated at T. If desired,    
!              the Y array may be used for other purposes between       
!              calls to the solver.                                     
!                                                                       
!              This array is passed as the Y argument in all calls to F 
!              and JAC.  Hence its length may exceed NEQ, and locations 
!              Y(NEQ+1),... may be used to store other real data and    
!              pass it to F and/or JAC.  (The DLSODE package accesses   
!              only Y(1),...,Y(NEQ).)                                   
!                                                                       
!     T        The independent variable.  On input, T is used only on   
!              the first call, as the initial point of the integration. 
!              On output, after each call, T is the value at which a    
!              computed solution Y is evaluated (usually the same as    
!              TOUT).  On an error return, T is the farthest point      
!              reached.                                                 
!                                                                       
!     TOUT     The next value of T at which a computed solution is      
!              desired.  Used only for input.                           
!                                                                       
!              When starting the problem (ISTATE = 1), TOUT may be equal
!              to T for one call, then should not equal T for the next  
!              call.  For the initial T, an input value of TOUT .NE. T  
!              is used in order to determine the direction of the       
!              integration (i.e., the algebraic sign of the step sizes) 
!              and the rough scale of the problem.  Integration in      
!              either direction (forward or backward in T) is permitted.
!                                                                       
!              If ITASK = 2 or 5 (one-step modes), TOUT is ignored      
!              after the first call (i.e., the first call with          
!              TOUT .NE. T).  Otherwise, TOUT is required on every call.
!                                                                       
!              If ITASK = 1, 3, or 4, the values of TOUT need not be    
!              monotone, but a value of TOUT which backs up is limited  
!              to the current internal T interval, whose endpoints are  
!              TCUR - HU and TCUR.  (See "Optional Outputs" below for   
!              TCUR and HU.)                                            
!                                                                       
!                                                                       
!     ITOL     An indicator for the type of error control.  See         
!              description below under AbsTol.  Used only for input.      
!                                                                       
!     RelTol     A relative error tolerance parameter, either a scalar or 
!              an array of length NEQ.  See description below under     
!              AbsTol.  Input only.                                       
!                                                                       
!     AbsTol     An absolute error tolerance parameter, either a scalar or
!              an array of length NEQ.  Input only.                     
!                                                                       
!              The input parameters ITOL, RelTol, and AbsTol determine the  
!              error control performed by the solver.  The solver will  
!              control the vector e = (e(i)) of estimated local errors  
!              in Y, according to an inequality of the form             
!                                                                       
!                 rms-norm of ( e(i)/EWT(i) ) <= 1,                     
!                                                                       
!              where                                                    
!                                                                       
!                 EWT(i) = RelTol(i)*ABS(Y(i)) + AbsTol(i),                 
!                                                                       
!              and the rms-norm (root-mean-square norm) here is         
!                                                                       
!                 rms-norm(v) = SQRT(sum v(i)**2 / NEQ).                
!                                                                       
!              Here EWT = (EWT(i)) is a vector of weights which must    
!              always be positive, and the values of RelTol and AbsTol      
!              should all be nonnegative.  The following table gives the
!              types (scalar/array) of RelTol and AbsTol, and the           
!              corresponding form of EWT(i).                            
!                                                                       
!              ITOL    RelTol      AbsTol      EWT(i)                       
!              ----    ------    ------    -----------------------------
!              1       scalar    scalar    RelTol*ABS(Y(i)) + AbsTol        
!              2       scalar    array     RelTol*ABS(Y(i)) + AbsTol(i)     
!              3       array     scalar    RelTol(i)*ABS(Y(i)) + AbsTol     
!              4       array     array     RelTol(i)*ABS(Y(i)) + AbsTol(i)  
!                                                                       
!              When either of these parameters is a scalar, it need not 
!              be dimensioned in the user's calling program.            
!                                                                       
!              If none of the above choices (with ITOL, RelTol, and AbsTol  
!              fixed throughout the problem) is suitable, more general  
!              error controls can be obtained by substituting           
!              user-supplied routines for the setting of EWT and/or for 
!              the norm calculation.  See Part 4 below.                 
!                                                                       
!              If global errors are to be estimated by making a repeated
!              run on the same problem with smaller tolerances, then all
!              components of RelTol and AbsTol (i.e., of EWT) should be     
!              scaled down uniformly.                                   
!                                                                       
!     ITASK    An index specifying the task to be performed.  Input     
!              only.  ITASK has the following values and meanings:      
!              1   Normal computation of output values of y(t) at       
!                  t = TOUT (by overshooting and interpolating).        
!              2   Take one step only and return.                       
!              3   Stop at the first internal mesh point at or beyond   
!                  t = TOUT and return.                                 
!              4   Normal computation of output values of y(t) at       
!                  t = TOUT but without overshooting t = TCRIT.  TCRIT  
!                  must be input as RWORK(1).  TCRIT may be equal to or 
!                  beyond TOUT, but not behind it in the direction of   
!                  integration.  This option is useful if the problem   
!                  has a singularity at or beyond t = TCRIT.            
!              5   Take one step, without passing TCRIT, and return.    
!                  TCRIT must be input as RWORK(1).                     
!                                                                       
!              Note:  If ITASK = 4 or 5 and the solver reaches TCRIT    
!              (within roundoff), it will return T = TCRIT (exactly) to 
!              indicate this (unless ITASK = 4 and TOUT comes before    
!              TCRIT, in which case answers at T = TOUT are returned    
!              first).                                                  
!                                                                       
!     ISTATE   An index used for input and output to specify the state  
!              of the calculation.                                      
!                                                                       
!              On input, the values of ISTATE are as follows:           
!              1   This is the first call for the problem               
!                  (initializations will be done).  See "Note" below.   
!              2   This is not the first call, and the calculation is to
!                  continue normally, with no change in any input       
!                  parameters except possibly TOUT and ITASK.  (If ITOL,
!                  RelTol, and/or AbsTol are changed between calls with     
!                  ISTATE = 2, the new values will be used but not      
!                  tested for legality.)                                
!              3   This is not the first call, and the calculation is to
!                  continue normally, but with a change in input        
!                  parameters other than TOUT and ITASK.  Changes are   
!                  allowed in NEQ, ITOL, RelTol, AbsTol, IOPT, LRW, LIW, MF,
!                  ML, MU, and any of the optional inputs except H0.    
!                  (See IWORK description for ML and MU.)               
!                                                                       
!              Note:  A preliminary call with TOUT = T is not counted as
!              a first call here, as no initialization or checking of   
!              input is done.  (Such a call is sometimes useful for the 
!              purpose of outputting the initial conditions.)  Thus the 
!              first call for which TOUT .NE. T requires ISTATE = 1 on  
!              input.                                                   
!                                                                       
!              On output, ISTATE has the following values and meanings: 
!               1  Nothing was done, as TOUT was equal to T with        
!                  ISTATE = 1 on input.                                 
!               2  The integration was performed successfully.          
!              -1  An excessive amount of work (more than MXSTEP steps) 
!                  was done on this call, before completing the         
!                  requested task, but the integration was otherwise    
!                  successful as far as T. (MXSTEP is an optional input 
!                  and is normally 500.)  To continue, the user may     
!                  simply reset ISTATE to a value >1 and call again (the
!                  excess work step counter will be reset to 0).  In    
!                  addition, the user may increase MXSTEP to avoid this 
!                  error return; see "Optional Inputs" below.           
!              -2  Too much accuracy was requested for the precision of 
!                  the machine being used.  This was detected before    
!                  completing the requested task, but the integration   
!                  was successful as far as T. To continue, the         
!                  tolerance parameters must be reset, and ISTATE must  
!                  be set to 3. The optional output TOLSF may be used   
!                  for this purpose.  (Note:  If this condition is      
!                  detected before taking any steps, then an illegal    
!                  input return (ISTATE = -3) occurs instead.)          
!              -3  Illegal input was detected, before taking any        
!                  integration steps.  See written message for details. 
!                  (Note:  If the solver detects an infinite loop of    
!                  calls to the solver with illegal input, it will cause
!                  the run to stop.)                                    
!              -4  There were repeated error-test failures on one       
!                  attempted step, before completing the requested task,
!                  but the integration was successful as far as T.  The 
!                  problem may have a singularity, or the input may be  
!                  inappropriate.                                       
!              -5  There were repeated convergence-test failures on one 
!                  attempted step, before completing the requested task,
!                  but the integration was successful as far as T. This 
!                  may be caused by an inaccurate Jacobian matrix, if   
!                  one is being used.                                   
!              -6  EWT(i) became zero for some i during the integration.
!                  Pure relative error control (AbsTol(i)=0.0) was        
!                  requested on a variable which has now vanished.  The 
!                  integration was successful as far as T.              
!                                                                       
!              Note:  Since the normal output value of ISTATE is 2, it  
!              does not need to be reset for normal continuation.  Also,
!              since a negative input value of ISTATE will be regarded  
!              as illegal, a negative output value requires the user to 
!              change it, and possibly other inputs, before calling the 
!              solver again.                                            
!                                                                       
!     IOPT     An integer flag to specify whether any optional inputs   
!              are being used on this call.  Input only.  The optional  
!              inputs are listed under a separate heading below.        
!              0   No optional inputs are being used.  Default values   
!                  will be used in all cases.                           
!              1   One or more optional inputs are being used.          
!                                                                       
!     RWORK    A real working array (double precision).  The length of  
!              RWORK must be at least                                   
!                                                                       
!                 20 + NYH*(MAXORD + 1) + 3*NEQ + LWM                   
!                                                                       
!              where                                                    
!                 NYH = the initial value of NEQ,                       
!              MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a   
!                       smaller value is given as an optional input),   
!                 LWM = 0           if MITER = 0,                       
!                 LWM = NEQ**2 + 2  if MITER = 1 or 2,                  
!                 LWM = NEQ + 2     if MITER = 3, and                   
!                 LWM = (2*ML + MU + 1)*NEQ + 2                         
!                                   if MITER = 4 or 5.                  
!              (See the MF description below for METH and MITER.)       
!                                                                       
!              Thus if MAXORD has its default value and NEQ is constant,
!              this length is:                                          
!              20 + 16*NEQ                    for MF = 10,              
!              22 + 16*NEQ + NEQ**2           for MF = 11 or 12,        
!              22 + 17*NEQ                    for MF = 13,              
!              22 + 17*NEQ + (2*ML + MU)*NEQ  for MF = 14 or 15,        
!              20 +  9*NEQ                    for MF = 20,              
!              22 +  9*NEQ + NEQ**2           for MF = 21 or 22,        
!              22 + 10*NEQ                    for MF = 23,              
!              22 + 10*NEQ + (2*ML + MU)*NEQ  for MF = 24 or 25.        
!                                                                       
!              The first 20 words of RWORK are reserved for conditional 
!              and optional inputs and optional outputs.                
!                                                                       
!              The following word in RWORK is a conditional input:      
!              RWORK(1) = TCRIT, the critical value of t which the      
!                         solver is not to overshoot.  Required if ITASK
!                         is 4 or 5, and ignored otherwise.  See ITASK. 
!                                                                       
!     LRW      The length of the array RWORK, as declared by the user.  
!              (This will be checked by the solver.)                    
!                                                                       
!     IWORK    An integer work array.  Its length must be at least      
!              20       if MITER = 0 or 3 (MF = 10, 13, 20, 23), or     
!              20 + NEQ otherwise (MF = 11, 12, 14, 15, 21, 22, 24, 25).
!              (See the MF description below for MITER.)  The first few 
!              words of IWORK are used for conditional and optional     
!              inputs and optional outputs.                             
!                                                                       
!              The following two words in IWORK are conditional inputs: 
!              IWORK(1) = ML   These are the lower and upper half-      
!              IWORK(2) = MU   bandwidths, respectively, of the banded  
!                              Jacobian, excluding the main diagonal.   
!                         The band is defined by the matrix locations   
!                         (i,j) with i - ML <= j <= i + MU. ML and MU   
!                         must satisfy 0 <= ML,MU <= NEQ - 1. These are 
!                         required if MITER is 4 or 5, and ignored      
!                         otherwise.  ML and MU may in fact be the band 
!                         parameters for a matrix to which df/dy is only
!                         approximately equal.                          
!                                                                       
!     LIW      The length of the array IWORK, as declared by the user.  
!              (This will be checked by the solver.)                    
!                                                                       
!     Note:  The work arrays must not be altered between calls to DLSODE
!     for the same problem, except possibly for the conditional and     
!     optional inputs, and except for the last 3*NEQ words of RWORK.    
!     The latter space is used for internal scratch space, and so is    
!     available for use by the user outside DLSODE between calls, if    
!     desired (but not for use by F or JAC).                            
!                                                                       
!     JAC      The name of the user-supplied routine (MITER = 1 or 4) to
!              compute the Jacobian matrix, df/dy, as a function of the 
!              scalar t and the vector y.  (See the MF description below
!              for MITER.)  It is to have the form                      
!                                                                       
!                 SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD)        
!                 KPP_REAL T, Y(*), PD(NROWPD,*)                
!                                                                       
!              where NEQ, T, Y, ML, MU, and NROWPD are input and the    
!              array PD is to be loaded with partial derivatives        
!              (elements of the Jacobian matrix) on output.  PD must be 
!              given a first dimension of NROWPD.  T and Y have the same
!              meaning as in subroutine F.                              
!                                                                       
!              In the full matrix case (MITER = 1), ML and MU are       
!              ignored, and the Jacobian is to be loaded into PD in     
!              columnwise manner, with df(i)/dy(j) loaded into PD(i,j). 
!                                                                       
!              In the band matrix case (MITER = 4), the elements within 
!              the band are to be loaded into PD in columnwise manner,  
!              with diagonal lines of df/dy loaded into the rows of PD. 
!              Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).  ML
!              and MU are the half-bandwidth parameters (see IWORK).    
!              The locations in PD in the two triangular areas which    
!              correspond to nonexistent matrix elements can be ignored 
!              or loaded arbitrarily, as they are overwritten by DLSODE.
!                                                                       
!              JAC need not provide df/dy exactly. A crude approximation
!              (possibly with a smaller bandwidth) will do.             
!                                                                       
!              In either case, PD is preset to zero by the solver, so   
!              that only the nonzero elements need be loaded by JAC.    
!              Each call to JAC is preceded by a call to F with the same
!              arguments NEQ, T, and Y. Thus to gain some efficiency,   
!              intermediate quantities shared by both calculations may  
!              be saved in a user COMMON block by F and not recomputed  
!              by JAC, if desired.  Also, JAC may alter the Y array, if 
!              desired.  JAC must be declared EXTERNAL in the calling   
!              program.                                                 
!                                                                       
!              Subroutine JAC may access user-defined quantities in     
!              NEQ(2),... and/or in Y(NEQ+1),... if NEQ is an array  
!              (dimensioned in JAC) and/or Y has length exceeding       
!              NEQ.  See the descriptions of NEQ and Y above.        
!                                                                       
!     MF       The method flag.  Used only for input.  The legal values 
!              of MF are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24,    
!              and 25.  MF has decimal digits METH and MITER:           
!                 MF = 10*METH + MITER .                                
!                                                                       
!              METH indicates the basic linear multistep method:        
!              1   Implicit Adams method.                               
!              2   Method based on backward differentiation formulas    
!                  (BDF's).                                             
!                                                                       
!              MITER indicates the corrector iteration method:          
!              0   Functional iteration (no Jacobian matrix is          
!                  involved).                                           
!              1   Chord iteration with a user-supplied full (NEQ by    
!                  NEQ) Jacobian.                                       
!              2   Chord iteration with an internally generated         
!                  (difference quotient) full Jacobian (using NEQ       
!                  extra calls to F per df/dy value).                   
!              3   Chord iteration with an internally generated         
!                  diagonal Jacobian approximation (using one extra call
!                  to F per df/dy evaluation).                          
!              4   Chord iteration with a user-supplied banded Jacobian.
!              5   Chord iteration with an internally generated banded  
!                  Jacobian (using ML + MU + 1 extra calls to F per     
!                  df/dy evaluation).                                   
!                                                                       
!              If MITER = 1 or 4, the user must supply a subroutine JAC 
!              (the name is arbitrary) as described above under JAC.    
!              For other values of MITER, a dummy argument can be used. 
!                                                                       
!     Optional Inputs                                                   
!     ---------------                                                   
!     The following is a list of the optional inputs provided for in the
!     call sequence.  (See also Part 2.)  For each such input variable, 
!     this table lists its name as used in this documentation, its      
!     location in the call sequence, its meaning, and the default value.
!     The use of any of these inputs requires IOPT = 1, and in that case
!     all of these inputs are examined.  A value of zero for any of     
!     these optional inputs will cause the default value to be used.    
!     Thus to use a subset of the optional inputs, simply preload       
!     locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively,   
!     and then set those of interest to nonzero values.                 
!                                                                       
!     Name    Location   Meaning and default value                      
!     ------  ---------  -----------------------------------------------
!     H0      RWORK(5)   Step size to be attempted on the first step.   
!                        The default value is determined by the solver. 
!     HMAX    RWORK(6)   Maximum absolute step size allowed.  The       
!                        default value is infinite.                     
!     HMIN    RWORK(7)   Minimum absolute step size allowed.  The       
!                        default value is 0.  (This lower bound is not  
!                        enforced on the final step before reaching     
!                        TCRIT when ITASK = 4 or 5.)                    
!     MAXORD  IWORK(5)   Maximum order to be allowed.  The default value
!                        is 12 if METH = 1, and 5 if METH = 2. (See the 
!                        MF description above for METH.)  If MAXORD     
!                        exceeds the default value, it will be reduced  
!                        to the default value.  If MAXORD is changed    
!                        during the problem, it may cause the current   
!                        order to be reduced.                           
!     MXSTEP  IWORK(6)   Maximum number of (internally defined) steps   
!                        allowed during one call to the solver.  The    
!                        default value is 500.                          
!     MXHNIL  IWORK(7)   Maximum number of messages printed (per        
!                        problem) warning that T + H = T on a step      
!                        (H = step size).  This must be positive to     
!                        result in a nondefault value.  The default     
!                        value is 10.                                   
!                                                                       
!     Optional Outputs                                                  
!     ----------------                                                  
!     As optional additional output from DLSODE, the variables listed   
!     below are quantities related to the performance of DLSODE which   
!     are available to the user.  These are communicated by way of the  
!     work arrays, but also have internal mnemonic names as shown.      
!     Except where stated otherwise, all of these outputs are defined on
!     any successful return from DLSODE, and on any return with ISTATE =
!     -1, -2, -4, -5, or -6.  On an illegal input return (ISTATE = -3), 
!     they will be unchanged from their existing values (if any), except
!     possibly for TOLSF, LENRW, and LENIW.  On any error return,       
!     outputs relevant to the error will be defined, as noted below.    
!                                                                       
!     Name   Location   Meaning                                         
!     -----  ---------  ------------------------------------------------
!     HU     RWORK(11)  Step size in t last used (successfully).        
!     HCUR   RWORK(12)  Step size to be attempted on the next step.     
!     TCUR   RWORK(13)  Current value of the independent variable which 
!                       the solver has actually reached, i.e., the      
!                       current internal mesh point in t. On output,    
!                       TCUR will always be at least as far as the      
!                       argument T, but may be farther (if interpolation
!                       was done).                                      
!     TOLSF  RWORK(14)  Tolerance scale factor, greater than 1.0,       
!                       computed when a request for too much accuracy   
!                       was detected (ISTATE = -3 if detected at the    
!                       start of the problem, ISTATE = -2 otherwise).   
!                       If ITOL is left unaltered but RelTol and AbsTol are 
!                       uniformly scaled up by a factor of TOLSF for the
!                       next call, then the solver is deemed likely to  
!                       succeed.  (The user may also ignore TOLSF and   
!                       alter the tolerance parameters in any other way 
!                       appropriate.)                                   
!     NST    IWORK(11)  Number of steps taken for the problem so far.   
!     NFE    IWORK(12)  Number of F evaluations for the problem so far. 
!     NJE    IWORK(13)  Number of Jacobian evaluations (and of matrix LU
!                       decompositions) for the problem so far.         
!     NQU    IWORK(14)  Method order last used (successfully).          
!     NQCUR  IWORK(15)  Order to be attempted on the next step.         
!     IMXER  IWORK(16)  Index of the component of largest magnitude in  
!                       the weighted local error vector ( e(i)/EWT(i) ),
!                       on an error return with ISTATE = -4 or -5.      
!     LENRW  IWORK(17)  Length of RWORK actually required.  This is     
!                       defined on normal returns and on an illegal     
!                       input return for insufficient storage.          
!     LENIW  IWORK(18)  Length of IWORK actually required.  This is     
!                       defined on normal returns and on an illegal     
!                       input return for insufficient storage.          
!                                                                       
!     The following two arrays are segments of the RWORK array which may
!     also be of interest to the user as optional outputs.  For each    
!     array, the table below gives its internal name, its base address  
!     in RWORK, and its description.                                    
!                                                                       
!     Name  Base address  Description                                   
!     ----  ------------  ----------------------------------------------
!     YH    21            The Nordsieck history array, of size NYH by   
!                         (NQCUR + 1), where NYH is the initial value of
!                         NEQ.  For j = 0,1,...,NQCUR, column j + 1 of  
!                         YH contains HCUR**j/factorial(j) times the jth
!                         derivative of the interpolating polynomial    
!                         currently representing the solution, evaluated
!                         at t = TCUR.                                  
!     ACOR  LENRW-NEQ+1   Array of size NEQ used for the accumulated    
!                         corrections on each step, scaled on output to 
!                         represent the estimated local error in Y on   
!                         the last step.  This is the vector e in the   
!                         description of the error control.  It is      
!                         defined only on successful return from DLSODE.
!                                                                       
!                                                                       
!                    Part 2.  Other Callable Routines                   
!                    --------------------------------                   
!                                                                       
!     The following are optional calls which the user may make to gain  
!     additional capabilities in conjunction with DLSODE.               
!                                                                       
!     Form of call              Function                                
!     ------------------------  ----------------------------------------
!     CALL XSETUN(LUN)          Set the logical unit number, LUN, for   
!                               output of messages from DLSODE, if the  
!                               default is not desired.  The default    
!                               value of LUN is 6. This call may be made
!                               at any time and will take effect        
!                               immediately.                            
!     CALL XSETF(MFLAG)         Set a flag to control the printing of   
!                               messages by DLSODE.  MFLAG = 0 means do 
!                               not print.  (Danger:  this risks losing 
!                               valuable information.)  MFLAG = 1 means 
!                               print (the default).  This call may be  
!                               made at any time and will take effect   
!                               immediately.                            
!     CALL DSRCOM(RSAV,ISAV,JOB)  Saves and restores the contents of the
!                               internal COMMON blocks used by DLSODE   
!                               (see Part 3 below).  RSAV must be a     
!                               real array of length 218 or more, and   
!                               ISAV must be an integer array of length 
!                               37 or more.  JOB = 1 means save COMMON  
!                               into RSAV/ISAV.  JOB = 2 means restore  
!                               COMMON from same.  DSRCOM is useful if  
!                               one is interrupting a run and restarting
!                               later, or alternating between two or    
!                               more problems solved with DLSODE.       
!     CALL DINTDY(,,,,,)        Provide derivatives of y, of various    
!     (see below)               orders, at a specified point t, if      
!                               desired.  It may be called only after a 
!                               successful return from DLSODE.  Detailed
!                               instructions follow.                    
!                                                                       
!     Detailed instructions for using DINTDY                            
!     --------------------------------------                            
!     The form of the CALL is:                                          
!                                                                       
!           CALL DINTDY (T, K, RWORK(21), NYH, DKY, IFLAG)              
!                                                                       
!     The input parameters are:                                         
!                                                                       
!     T          Value of independent variable where answers are        
!                desired (normally the same as the T last returned by   
!                DLSODE).  For valid results, T must lie between        
!                TCUR - HU and TCUR.  (See "Optional Outputs" above     
!                for TCUR and HU.)                                      
!     K          Integer order of the derivative desired.  K must       
!                satisfy 0 <= K <= NQCUR, where NQCUR is the current    
!                order (see "Optional Outputs").  The capability        
!                corresponding to K = 0, i.e., computing y(t), is       
!                already provided by DLSODE directly.  Since            
!                NQCUR >= 1, the first derivative dy/dt is always       
!                available with DINTDY.                                 
!     RWORK(21)  The base address of the history array YH.              
!     NYH        Column length of YH, equal to the initial value of NEQ.
!                                                                       
!     The output parameters are:                                        
!                                                                       
!     DKY        Real array of length NEQ containing the computed value 
!                of the Kth derivative of y(t).                         
!     IFLAG      Integer flag, returned as 0 if K and T were legal,     
!                -1 if K was illegal, and -2 if T was illegal.          
!                On an error return, a message is also written.         
!                                                                       
!                                                                       
!                          Part 3.  Common Blocks                       
!                          ----------------------                       
!                                                                       
!     If DLSODE is to be used in an overlay situation, the user must    
!     declare, in the primary overlay, the variables in:                
!     (1) the call sequence to DLSODE,                                  
!     (2) the internal COMMON block /DLS001/, of length 255             
!         (218 double precision words followed by 37 integer words).    
!                                                                       
!     If DLSODE is used on a system in which the contents of internal   
!     COMMON blocks are not preserved between calls, the user should    
!     declare the above COMMON block in his main program to insure that 
!     its contents are preserved.                                       
!                                                                       
!     If the solution of a given problem by DLSODE is to be interrupted 
!     and then later continued, as when restarting an interrupted run or
!     alternating between two or more problems, the user should save,   
!     following the return from the last DLSODE call prior to the       
!     interruption, the contents of the call sequence variables and the 
!     internal COMMON block, and later restore these values before the  
!     next DLSODE call for that problem.   In addition, if XSETUN and/or
!     XSETF was called for non-default handling of error messages, then 
!     these calls must be repeated.  To save and restore the COMMON     
!     block, use subroutine DSRCOM (see Part 2 above).                  
!                                                                       
!                                                                       
!              Part 4.  Optionally Replaceable Solver Routines          
!              -----------------------------------------------          
!                                                                       
!     Below are descriptions of two routines in the DLSODE package which
!     relate to the measurement of errors.  Either routine can be       
!     replaced by a user-supplied version, if desired.  However, since  
!     such a replacement may have a major impact on performance, it     
!     should be done only when absolutely necessary, and only with great
!     caution.  (Note:  The means by which the package version of a     
!     routine is superseded by the user's version may be system-        
!     dependent.)                                                       
!                                                                       
!     DEWSET                                                            
!     ------                                                            
!     The following subroutine is called just before each internal      
!     integration step, and sets the array of error weights, EWT, as    
!     described under ITOL/RelTol/AbsTol above:                             
!                                                                       
!           SUBROUTINE DEWSET (NEQ, ITOL, RelTol, AbsTol, YCUR, EWT)        
!                                                                       
!     where NEQ, ITOL, RelTol, and AbsTol are as in the DLSODE call         
!     sequence, YCUR contains the current dependent variable vector,    
!     and EWT is the array of weights set by DEWSET.                    
!                                                                       
!     If the user supplies this subroutine, it must return in EWT(i)    
!     (i = 1,...,NEQ) a positive quantity suitable for comparing errors 
!     in Y(i) to.  The EWT array returned by DEWSET is passed to the    
!     DVNORM routine (see below), and also used by DLSODE in the        
!     computation of the optional output IMXER, the diagonal Jacobian   
!     approximation, and the increments for difference quotient         
!     Jacobians.                                                        
!                                                                       
!     In the user-supplied version of DEWSET, it may be desirable to use
!     the current values of derivatives of y. Derivatives up to order NQ
!     are available from the history array YH, described above under    
!     optional outputs.  In DEWSET, YH is identical to the YCUR array,  
!     extended to NQ + 1 columns with a column length of NYH and scale  
!     factors of H**j/factorial(j).  On the first call for the problem, 
!     given by NST = 0, NQ is 1 and H is temporarily set to 1.0.        
!     NYH is the initial value of NEQ.  The quantities NQ, H, and NST   
!     can be obtained by including in SEWSET the statements:            
!           KPP_REAL RLS                                        
!           COMMON /DLS001/ RLS(218),ILS(37)                            
!           NQ = ILS(33)                                                
!           NST = ILS(34)                                               
!           H = RLS(212)                                                
!     Thus, for example, the current value of dy/dt can be obtained as  
!     YCUR(NYH+i)/H (i=1,...,NEQ) (and the division by H is unnecessary 
!     when NST = 0).                                                    
!                                                                       
!     DVNORM                                                            
!     ------                                                            
!     DVNORM is a real function routine which computes the weighted     
!     root-mean-square norm of a vector v:                              
!                                                                       
!        d = DVNORM (n, v, w)                                           
!                                                                       
!     where:                                                            
!     n = the length of the vector,                                     
!     v = real array of length n containing the vector,                 
!     w = real array of length n containing weights,                    
!     d = SQRT( (1/n) * sum(v(i)*w(i))**2 ).                            
!                                                                       
!     DVNORM is called with n = NEQ and with w(i) = 1.0/EWT(i), where   
!     EWT is as set by subroutine DEWSET.                               
!                                                                       
!     If the user supplies this function, it should return a nonnegative
!     value of DVNORM suitable for use in the error control in DLSODE.  
!     None of the arguments should be altered by DVNORM.  For example, a
!     user-supplied DVNORM routine might:                               
!     - Substitute a max-norm of (v(i)*w(i)) for the rms-norm, or       
!     - Ignore some components of v in the norm, with the effect of     
!       suppressing the error control on those components of Y.         
!  ---------------------------------------------------------------------
!***ROUTINES CALLED  DEWSET, DINTDY, DUMACH, DSTODE, DVNORM, XERRWD     
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYYYMMDD)                                        
! 19791129  DATE WRITTEN                                                
! 19791213  Minor changes to declarations; DELP init. in STODE.         
! 19800118  Treat NEQ as array; integer declarations added throughout;  
!           minor changes to prologue.                                  
! 19800306  Corrected TESCO(1,NQP1) setting in CFODE.                   
! 19800519  Corrected access of YH on forced order reduction;           
!           numerous corrections to prologues and other comments.       
! 19800617  In main driver, added loading of SQRT(UROUND) in RWORK;     
!           minor corrections to main prologue.                         
! 19800923  Added zero initialization of HU and NQU.                    
! 19801218  Revised XERRWD routine; minor corrections to main prologue. 
! 19810401  Minor changes to comments and an error message.             
! 19810814  Numerous revisions: replaced EWT by 1/EWT; used flags       
!           JCUR, ICF, IERPJ, IERSL between STODE and subordinates;     
!           added tuning parameters CCMAX, MAXCOR, MSBP, MXNCF;         
!           reorganized returns from STODE; reorganized type decls.;    
!           fixed message length in XERRWD; changed default LUNIT to 6; 
!           changed Common lengths; changed comments throughout.        
! 19870330  Major update by ACH: corrected comments throughout;         
!           removed TRET from Common; rewrote EWSET with 4 loops;       
!           fixed t test in INTDY; added Cray directives in STODE;      
!           in STODE, fixed DELP init. and logic around PJAC call;      
!           combined routines to save/restore Common;                   
!           passed LEVEL = 0 in error message calls (except run abort). 
! 19890426  Modified prologue to SLATEC/LDOC format.  (FNF)             
! 19890501  Many improvements to prologue.  (FNF)                       
! 19890503  A few final corrections to prologue.  (FNF)                 
! 19890504  Minor cosmetic changes.  (FNF)                              
! 19890510  Corrected description of Y in Arguments section.  (FNF)     
! 19890517  Minor corrections to prologue.  (FNF)                       
! 19920514  Updated with prologue edited 891025 by G. Shaw for manual.  
! 19920515  Converted source lines to upper case.  (FNF)                
! 19920603  Revised XERRWD calls using mixed upper-lower case.  (ACH)   
! 19920616  Revised prologue comment regarding CFT.  (ACH)              
! 19921116  Revised prologue comments regarding Common.  (ACH).         
! 19930326  Added comment about non-reentrancy.  (FNF)                  
! 19930723  Changed D1MACH to DUMACH. (FNF)                             
! 19930801  Removed ILLIN and NTREP from Common (affects driver logic); 
!           minor changes to prologue and internal comments;            
!           changed Hollerith strings to quoted strings;                
!           changed internal comments to mixed case;                    
!           replaced XERRWD with new version using character type;      
!           changed dummy dimensions from 1 to *. (ACH)                 
! 19930809  Changed to generic intrinsic names; changed names of        
!           subprograms and Common blocks to DLSODE etc. (ACH)          
! 19930929  Eliminated use of REAL intrinsic; other minor changes. (ACH)
! 20010412  Removed all 'own' variables from Common block /DLS001/      
!           (affects declarations in 6 routines). (ACH)                 
! 20010509  Minor corrections to prologue. (ACH)                        
! 20031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
! 20031112  Added SAVE statements for data-loaded constants.            
!                                                                       
!***END PROLOGUE  DLSODE                                                
!                                                                       
!*Internal Notes:                                                       
!                                                                       
! Other Routines in the DLSODE Package.                                 
!                                                                       
! In addition to Subroutine DLSODE, the DLSODE package includes the     
! following subroutines and function routines:                          
!  DINTDY   computes an interpolated value of the y vector at t = TOUT. 
!  DSTODE   is the core integrator, which does one step of the          
!           integration and the associated error control.               
!  DCFODE   sets all method coefficients and test constants.            
!  DPREPJ   computes and preprocesses the Jacobian matrix J = df/dy     
!           and the Newton iteration matrix P = I - h*l0*J.             
!  DSOLSY   manages solution of linear system in chord iteration.       
!  DEWSET   sets the error weight vector EWT before each step.          
!  DVNORM   computes the weighted R.M.S. norm of a vector.              
!  DSRCOM   is a user-callable routine to save and restore              
!           the contents of the internal Common block.                  
!  DGEFA and DGESL   are routines from LINPACK for solving full         
!           systems of linear algebraic equations.                      
!  DGBFA and DGBSL   are routines from LINPACK for solving banded       
!           linear systems.                                             
!  DUMACH   computes the unit roundoff in a machine-independent manner. 
!  XERRWD, XSETUN, XSETF, IXSAV, IUMACH   handle the printing of all    
!           error messages and warnings.  XERRWD is machine-dependent.  
! Note: DVNORM, DUMACH, IXSAV, and IUMACH are function routines.        
! All the others are subroutines.                                       
!                                                                       
!**End                                                                  
!                                                                       
!  Declare externals.
!  Note: they are now internal                                                   
      !EXTERNAL DPREPJ, DSOLSY 
      !KPP_REAL DUMACH, DVNORM 
!                                                                       
!  Declare all other variables.                                         
      INTEGER INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS,          &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                      &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,                &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      INTEGER I, I1, I2, IFLAG, IMXER, KGO, LF0,                        &
        LENIW, LENRW, LENWM, ML, MORD(2), MU, MXHNL0, MXSTP0              
      KPP_REAL ROWNS,                                           &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND                  
      KPP_REAL AbsTolI, AYI, BIG, EWTI, H0, HMAX, HMX, RH, RelTolI, &
        TCRIT, TDIST, TNEXT, TOL, TOLSF, TP, SIZE, SUM, W0             

      LOGICAL IHIT 
      CHARACTER*80 MSG 
      SAVE MORD, MXSTP0, MXHNL0 
!-----------------------------------------------------------------------
! The following internal Common block contains                          
! (a) variables which are local to any subroutine but whose values must 
!     be preserved between calls to the routine ("own" variables), and  
! (b) variables which are communicated between subroutines.             
! The block DLS001 is declared in subroutines DLSODE, DINTDY, DSTODE,   
! DPREPJ, and DSOLSY.                                                   
! Groups of variables are replaced by dummy arrays in the Common        
! declarations in routines where those variables are not used.          
!-----------------------------------------------------------------------
      COMMON /DLS001/ ROWNS(209),                                      &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,                 &
        INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
!                                                                       
      DATA  MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/ 
!-----------------------------------------------------------------------
! Block A.                                                              
! This code block is executed on every call.                            
! It tests ISTATE and ITASK for legality and branches appropriately.    
! If ISTATE .GT. 1 but the flag INIT shows that initialization has      
! not yet been done, an error return occurs.                            
! If ISTATE = 1 and TOUT = T, return immediately.                       
!-----------------------------------------------------------------------
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DLSODE                                  
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601 
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602 
      IF (ISTATE .EQ. 1) GO TO 10 
      IF (INIT .EQ. 0) GO TO 603 
      IF (ISTATE .EQ. 2) GO TO 200 
      GO TO 20 
   10 INIT = 0 
      IF (TOUT .EQ. T) RETURN 
!-----------------------------------------------------------------------
! Block B.                                                              
! The next code block is executed for the initial call (ISTATE = 1),    
! or for a continuation call with parameter changes (ISTATE = 3).       
! It contains checking of all inputs and various initializations.       
!                                                                       
! First check legality of the non-optional inputs NEQ, ITOL, IOPT,      
! MF, ML, and MU.                                                       
!-----------------------------------------------------------------------
   20 IF (NEQ .LE. 0) GO TO 604 
      IF (ISTATE .EQ. 1) GO TO 25 
      IF (NEQ .GT. N) GO TO 605 
   25 N = NEQ 
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606 
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607 
      METH = MF/10 
      MITER = MF - 10*METH 
      IF (METH .LT. 1 .OR. METH .GT. 2) GO TO 608 
      IF (MITER .LT. 0 .OR. MITER .GT. 5) GO TO 608 
      IF (MITER .LE. 3) GO TO 30 
      ML = IWORK(1) 
      MU = IWORK(2) 
      IF (ML .LT. 0 .OR. ML .GE. N) GO TO 609 
      IF (MU .LT. 0 .OR. MU .GE. N) GO TO 610 
   30 CONTINUE 
! Next process and check the optional inputs. --------------------------
      IF (IOPT .EQ. 1) GO TO 40 
      MAXORD = MORD(METH) 
      MXSTEP = MXSTP0 
      MXHNIL = MXHNL0 
      IF (ISTATE .EQ. 1) H0 = 0.0D0 
      HMXI = 0.0D0 
      HMIN = 0.0D0 
      GO TO 60 
   40 MAXORD = IWORK(5) 
      IF (MAXORD .LT. 0) GO TO 611 
      IF (MAXORD .EQ. 0) MAXORD = 100 
      MAXORD = MIN(MAXORD,MORD(METH)) 
      MXSTEP = IWORK(6) 
      IF (MXSTEP .LT. 0) GO TO 612 
      IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0 
      MXHNIL = IWORK(7) 
      IF (MXHNIL .LT. 0) GO TO 613 
      IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0 
      IF (ISTATE .NE. 1) GO TO 50 
      H0 = RWORK(5) 
      IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 614 
   50 HMAX = RWORK(6) 
      IF (HMAX .LT. 0.0D0) GO TO 615 
      HMXI = 0.0D0 
      IF (HMAX .GT. 0.0D0) HMXI = 1.0D0/HMAX 
      HMIN = RWORK(7) 
      IF (HMIN .LT. 0.0D0) GO TO 616 
!-----------------------------------------------------------------------
! Set work array pointers and check lengths LRW and LIW.                
! Pointers to segments of RWORK and IWORK are named by prefixing L to   
! the name of the segment.  E.g., the segment YH starts at RWORK(LYH).  
! Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR.    
!-----------------------------------------------------------------------
   60 LYH = 21 
      IF (ISTATE .EQ. 1) NYH = N 
      LWM = LYH + (MAXORD + 1)*NYH 
      IF (MITER .EQ. 0) LENWM = 0 
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) LENWM = N*N + 2 
      IF (MITER .EQ. 3) LENWM = N + 2 
      IF (MITER .GE. 4) LENWM = (2*ML + MU + 1)*N + 2 
      LEWT = LWM + LENWM 
      LSAVF = LEWT + N 
      LACOR = LSAVF + N 
      LENRW = LACOR + N - 1 
      IWORK(17) = LENRW 
      LIWM = 1 
      LENIW = 20 + N 
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) LENIW = 20 
      IWORK(18) = LENIW 
      IF (LENRW .GT. LRW) GO TO 617 
      IF (LENIW .GT. LIW) GO TO 618 
! Check RelTol and AbsTol for legality. ------------------------------------
      RelTolI = RelTol(1) 
      AbsTolI = AbsTol(1) 
      DO 70 I = 1,N 
        IF (ITOL .GE. 3) RelTolI = RelTol(I) 
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) AbsTolI = AbsTol(I) 
        IF (RelTolI .LT. 0.0D0) GO TO 619 
        IF (AbsTolI .LT. 0.0D0) GO TO 620 
   70   CONTINUE 
      IF (ISTATE .EQ. 1) GO TO 100 
! If ISTATE = 3, set flag to signal parameter changes to DSTODE. -------
      JSTART = -1 
      IF (NQ .LE. MAXORD) GO TO 90 
! MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. ---------
      DO 80 I = 1,N 
   80   RWORK(I+LSAVF-1) = RWORK(I+LWM-1) 
! Reload WM(1) = RWORK(LWM), since LWM may have changed. ---------------
   90 IF (MITER .GT. 0) RWORK(LWM) = SQRT(UROUND) 
      IF (N .EQ. NYH) GO TO 200 
! NEQ was reduced.  Zero part of YH to avoid undefined references. -----
      I1 = LYH + L*NYH 
      I2 = LYH + (MAXORD + 1)*NYH - 1 
      IF (I1 .GT. I2) GO TO 200 
      DO 95 I = I1,I2 
   95   RWORK(I) = 0.0D0 
      GO TO 200 
!-----------------------------------------------------------------------
! Block C.                                                              
! The next block is for the initial call only (ISTATE = 1).             
! It contains all remaining initializations, the initial call to F,     
! and the calculation of the initial step size.                         
! The error weights in EWT are inverted after being loaded.             
!-----------------------------------------------------------------------
  100 UROUND = DUMACH() 
      TN = T 
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110 
      TCRIT = RWORK(1) 
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. 0.0D0) GO TO 625 
      IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)           &
        H0 = TCRIT - T                                                 
  110 JSTART = 0 
      IF (MITER .GT. 0) RWORK(LWM) = SQRT(UROUND) 
      NHNIL = 0 
      NST = 0 
      NJE = 0 
      NSLAST = 0 
      HU = 0.0D0 
      NQU = 0 
      CCMAX = 0.3D0 
      MAXCOR = 3 
      MSBP = 20 
      MXNCF = 10 
! Initial call to F.  (LF0 points to YH(*,2).) -------------------------
      LF0 = LYH + NYH 
      CALL F (NEQ, T, Y, RWORK(LF0)) 
      NFE = 1 
! Load the initial value vector in YH. ---------------------------------
      DO 115 I = 1,N 
  115   RWORK(I+LYH-1) = Y(I) 
! Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
      NQ = 1 
      H = 1.0D0 
      CALL DEWSET (N, ITOL, RelTol, AbsTol, RWORK(LYH), RWORK(LEWT)) 
      DO 120 I = 1,N 
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 621 
  120   RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1) 
!-----------------------------------------------------------------------
! The coding below computes the step size, H0, to be attempted on the   
! first step, unless the user has supplied a value for this.            
! First check that TOUT - T differs significantly from zero.            
! A scalar tolerance quantity TOL is computed, as MAX(RelTol(I))          
! if this is positive, or MAX(AbsTol(I)/ABS(Y(I))) otherwise, adjusted    
! so as to be between 100*UROUND and 1.0E-3.                            
! Then the computed value H0 is given by..                              
!                                      NEQ                              
!   H0**2 = TOL / ( w0**-2 + (1/NEQ) * SUM ( f(i)/ywt(i) )**2  )        
!                                       1                               
! where   w0     = MAX ( ABS(T), ABS(TOUT) ),                           
!         f(i)   = i-th component of initial value of f,                
!         ywt(i) = EWT(i)/TOL  (a weight for y(i)).                     
! The sign of H0 is inferred from the initial values of TOUT and T.     
!-----------------------------------------------------------------------
      IF (H0 .NE. 0.0D0) GO TO 180 
      TDIST = ABS(TOUT - T) 
      W0 = MAX(ABS(T),ABS(TOUT)) 
      IF (TDIST .LT. 2.0D0*UROUND*W0) GO TO 622 
      TOL = RelTol(1) 
      IF (ITOL .LE. 2) GO TO 140 
      DO 130 I = 1,N 
  130   TOL = MAX(TOL,RelTol(I)) 
  140 IF (TOL .GT. 0.0D0) GO TO 160 
      AbsTolI = AbsTol(1) 
      DO 150 I = 1,N 
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) AbsTolI = AbsTol(I) 
        AYI = ABS(Y(I)) 
        IF (AYI .NE. 0.0D0) TOL = MAX(TOL,AbsTolI/AYI) 
  150   CONTINUE 
  160 TOL = MAX(TOL,100.0D0*UROUND) 
      TOL = MIN(TOL,0.001D0) 
      SUM = DVNORM (N, RWORK(LF0), RWORK(LEWT)) 
      SUM = 1.0D0/(TOL*W0*W0) + TOL*SUM**2 
      H0 = 1.0D0/SQRT(SUM) 
      H0 = MIN(H0,TDIST) 
      H0 = SIGN(H0,TOUT-T) 
! Adjust H0 if necessary to meet HMAX bound. ---------------------------
  180 RH = ABS(H0)*HMXI 
      IF (RH .GT. 1.0D0) H0 = H0/RH 
! Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0 
      DO 190 I = 1,N 
  190   RWORK(I+LF0-1) = H0*RWORK(I+LF0-1) 
      GO TO 270 
!-----------------------------------------------------------------------
! Block D.                                                              
! The next code block is for continuation calls only (ISTATE = 2 or 3)  
! and is to check stop conditions before taking a step.                 
!-----------------------------------------------------------------------
  200 NSLAST = NST 
      GO TO (210, 250, 220, 230, 240), ITASK 
  210 IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250 
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG) 
      IF (IFLAG .NE. 0) GO TO 627 
      T = TOUT 
      GO TO 420 
  220 TP = TN - HU*(1.0D0 + 100.0D0*UROUND) 
      IF ((TP - TOUT)*H .GT. 0.0D0) GO TO 623 
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250 
      GO TO 400 
  230 TCRIT = RWORK(1) 
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624 
      IF ((TCRIT - TOUT)*H .LT. 0.0D0) GO TO 625 
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 245 
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG) 
      IF (IFLAG .NE. 0) GO TO 627 
      T = TOUT 
      GO TO 420 
  240 TCRIT = RWORK(1) 
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624 
  245 HMX = ABS(TN) + ABS(H) 
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX 
      IF (IHIT) GO TO 400 
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND) 
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250 
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND) 
      IF (ISTATE .EQ. 2) JSTART = -2 
!-----------------------------------------------------------------------
! Block E.                                                              
! The next block is normally executed for all calls and contains        
! the call to the one-step core integrator DSTODE.                      
!                                                                       
! This is a looping point for the integration steps.                    
!                                                                       
! First check for too many steps being taken, update EWT (if not at     
! start of problem), check for too much accuracy being requested, and   
! check for H below the roundoff level in T.                            
!-----------------------------------------------------------------------
  250 CONTINUE 
      IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500 
      CALL DEWSET (N, ITOL, RelTol, AbsTol, RWORK(LYH), RWORK(LEWT)) 
      DO 260 I = 1,N 
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 510 
  260   RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1) 
  270 TOLSF = UROUND*DVNORM (N, RWORK(LYH), RWORK(LEWT)) 
      IF (TOLSF .LE. 1.0D0) GO TO 280 
      TOLSF = TOLSF*2.0D0 
      IF (NST .EQ. 0) GO TO 626 
      GO TO 520 
  280 IF ((TN + H) .NE. TN) GO TO 290 
      NHNIL = NHNIL + 1 
      IF (NHNIL .GT. MXHNIL) GO TO 290 
      MSG = 'DLSODE-  Warning..internal T (=R1) and H (=R2) are' 
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG='      such that in the machine, T + H = T on the next step  ' 
      CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      (H = step size). Solver will continue anyway' 
      CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 2, TN, H) 
      IF (NHNIL .LT. MXHNIL) GO TO 290 
      MSG = 'DLSODE-  Above warning has been issued I1 times.  ' 
      CALL XERRWD (MSG, 50, 102, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      It will not be issued again for this problem' 
      CALL XERRWD (MSG, 50, 102, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0) 
  290 CONTINUE 
!-----------------------------------------------------------------------
!  CALL DSTODE(NEQ,Y,YH,NYH,YH,EWT,SAVF,ACOR,WM,IWM,F,JAC,DPREPJ,DSOLSY)
!-----------------------------------------------------------------------
      CALL DSTODE (NEQ, Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),   &
        RWORK(LSAVF), RWORK(LACOR), RWORK(LWM), IWORK(LIWM),           &
        F, JAC)                                        
        !F, JAC, DPREPJ, DSOLSY)                                        
      KGO = 1 - KFLAG 
      GO TO (300, 530, 540), KGO 
!-----------------------------------------------------------------------
! Block F.                                                              
! The following block handles the case of a successful return from the  
! core integrator (KFLAG = 0).  Test for stop conditions.               
!-----------------------------------------------------------------------
  300 INIT = 1 
      GO TO (310, 400, 330, 340, 350), ITASK 
! ITASK = 1.  If TOUT has been reached, interpolate. -------------------
  310 IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250 
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG) 
      T = TOUT 
      GO TO 420 
! ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
  330 IF ((TN - TOUT)*H .GE. 0.0D0) GO TO 400 
      GO TO 250 
! ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary. 
  340 IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 345 
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG) 
      T = TOUT 
      GO TO 420 
  345 HMX = ABS(TN) + ABS(H) 
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX 
      IF (IHIT) GO TO 400 
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND) 
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250 
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND) 
      JSTART = -2 
      GO TO 250 
! ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
  350 HMX = ABS(TN) + ABS(H) 
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX 
!-----------------------------------------------------------------------
! Block G.                                                              
! The following block handles all successful returns from DLSODE.       
! If ITASK .NE. 1, Y is loaded from YH and T is set accordingly.        
! ISTATE is set to 2, and the optional outputs are loaded into the      
! work arrays before returning.                                         
!-----------------------------------------------------------------------
  400 DO 410 I = 1,N 
  410   Y(I) = RWORK(I+LYH-1) 
      T = TN 
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420 
      IF (IHIT) T = TCRIT 
  420 ISTATE = 2 
      RWORK(11) = HU 
      RWORK(12) = H 
      RWORK(13) = TN 
      IWORK(11) = NST 
      IWORK(12) = NFE 
      IWORK(13) = NJE 
      IWORK(14) = NQU 
      IWORK(15) = NQ 
      RETURN 
!-----------------------------------------------------------------------
! Block H.                                                              
! The following block handles all unsuccessful returns other than       
! those for illegal input.  First the error message routine is called.  
! If there was an error test or convergence test failure, IMXER is set. 
! Then Y is loaded from YH and T is set to TN.  The optional outputs    
! are loaded into the work arrays before returning.                     
!-----------------------------------------------------------------------
! The maximum number of steps was taken before reaching TOUT. ----------
  500 MSG = 'DLSODE-  At current T (=R1), MXSTEP (=I1) steps   ' 
      CALL XERRWD (MSG, 50, 201, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      taken on this call before reaching TOUT     ' 
      CALL XERRWD (MSG, 50, 201, 0, 1, MXSTEP, 0, 1, TN, 0.0D0) 
      ISTATE = -1 
      GO TO 580 
! EWT(I) .LE. 0.0 for some I (not at start of problem). ----------------
  510 EWTI = RWORK(LEWT+I-1) 
      MSG = 'DLSODE-  At T (=R1), EWT(I1) has become R2 .LE. 0.' 
      CALL XERRWD (MSG, 50, 202, 0, 1, I, 0, 2, TN, EWTI) 
      ISTATE = -6 
      GO TO 580 
! Too much accuracy requested for machine precision. -------------------
  520 MSG = 'DLSODE-  At T (=R1), too much accuracy requested  ' 
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      for precision of machine..  see TOLSF (=R2) ' 
      CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 2, TN, TOLSF) 
      RWORK(14) = TOLSF 
      ISTATE = -2 
      GO TO 580 
! KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
  530 MSG = 'DLSODE-  At T(=R1) and step size H(=R2), the error' 
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      test failed repeatedly or with ABS(H) = HMIN' 
      CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 2, TN, H) 
      ISTATE = -4 
      GO TO 560 
! KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
  540 MSG = 'DLSODE-  At T (=R1) and step size H (=R2), the    ' 
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      corrector convergence failed repeatedly     ' 
      CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG = '      or with ABS(H) = HMIN   ' 
      CALL XERRWD (MSG, 30, 205, 0, 0, 0, 0, 2, TN, H) 
      ISTATE = -5 
! Compute IMXER if relevant. -------------------------------------------
  560 BIG = 0.0D0 
      IMXER = 1 
      DO 570 I = 1,N 
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1)) 
        IF (BIG .GE. SIZE) GO TO 570 
        BIG = SIZE 
        IMXER = I 
  570   CONTINUE 
      IWORK(16) = IMXER 
! Set Y vector, T, and optional outputs. -------------------------------
  580 DO 590 I = 1,N 
  590   Y(I) = RWORK(I+LYH-1) 
      T = TN 
      RWORK(11) = HU 
      RWORK(12) = H 
      RWORK(13) = TN 
      IWORK(11) = NST 
      IWORK(12) = NFE 
      IWORK(13) = NJE 
      IWORK(14) = NQU 
      IWORK(15) = NQ 
      RETURN 
!-----------------------------------------------------------------------
! Block I.                                                              
! The following block handles all error returns due to illegal input    
! (ISTATE = -3), as detected before calling the core integrator.        
! First the error message routine is called.  If the illegal input      
! is a negative ISTATE, the run is aborted (apparent infinite loop).    
!-----------------------------------------------------------------------
  601 MSG = 'DLSODE-  ISTATE (=I1) illegal ' 
      CALL XERRWD (MSG, 30, 1, 0, 1, ISTATE, 0, 0, 0.0D0, 0.0D0) 
      IF (ISTATE .LT. 0) GO TO 800 
      GO TO 700 
  602 MSG = 'DLSODE-  ITASK (=I1) illegal  ' 
      CALL XERRWD (MSG, 30, 2, 0, 1, ITASK, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  603 MSG = 'DLSODE-  ISTATE .GT. 1 but DLSODE not initialized ' 
      CALL XERRWD (MSG, 50, 3, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  604 MSG = 'DLSODE-  NEQ (=I1) .LT. 1     ' 
      CALL XERRWD (MSG, 30, 4, 0, 1, NEQ, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  605 MSG = 'DLSODE-  ISTATE = 3 and NEQ increased (I1 to I2)  ' 
      CALL XERRWD (MSG, 50, 5, 0, 2, N, NEQ, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  606 MSG = 'DLSODE-  ITOL (=I1) illegal   ' 
      CALL XERRWD (MSG, 30, 6, 0, 1, ITOL, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  607 MSG = 'DLSODE-  IOPT (=I1) illegal   ' 
      CALL XERRWD (MSG, 30, 7, 0, 1, IOPT, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  608 MSG = 'DLSODE-  MF (=I1) illegal     ' 
      CALL XERRWD (MSG, 30, 8, 0, 1, MF, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  609 MSG = 'DLSODE-  ML (=I1) illegal.. .LT.0 or .GE.NEQ (=I2)' 
      CALL XERRWD (MSG, 50, 9, 0, 2, ML, NEQ, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  610 MSG = 'DLSODE-  MU (=I1) illegal.. .LT.0 or .GE.NEQ (=I2)' 
      CALL XERRWD (MSG, 50, 10, 0, 2, MU, NEQ, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  611 MSG = 'DLSODE-  MAXORD (=I1) .LT. 0  ' 
      CALL XERRWD (MSG, 30, 11, 0, 1, MAXORD, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  612 MSG = 'DLSODE-  MXSTEP (=I1) .LT. 0  ' 
      CALL XERRWD (MSG, 30, 12, 0, 1, MXSTEP, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  613 MSG = 'DLSODE-  MXHNIL (=I1) .LT. 0  ' 
      CALL XERRWD (MSG, 30, 13, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  614 MSG = 'DLSODE-  TOUT (=R1) behind T (=R2)      ' 
      CALL XERRWD (MSG, 40, 14, 0, 0, 0, 0, 2, TOUT, T) 
      MSG = '      Integration direction is given by H0 (=R1)  ' 
      CALL XERRWD (MSG, 50, 14, 0, 0, 0, 0, 1, H0, 0.0D0) 
      GO TO 700 
  615 MSG = 'DLSODE-  HMAX (=R1) .LT. 0.0  ' 
      CALL XERRWD (MSG, 30, 15, 0, 0, 0, 0, 1, HMAX, 0.0D0) 
      GO TO 700 
  616 MSG = 'DLSODE-  HMIN (=R1) .LT. 0.0  ' 
      CALL XERRWD (MSG, 30, 16, 0, 0, 0, 0, 1, HMIN, 0.0D0) 
      GO TO 700 
  617 CONTINUE 
      MSG='DLSODE-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)' 
      CALL XERRWD (MSG, 60, 17, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  618 CONTINUE 
      MSG='DLSODE-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)' 
      CALL XERRWD (MSG, 60, 18, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0) 
      GO TO 700 
  619 MSG = 'DLSODE-  RelTol(I1) is R1 .LT. 0.0        ' 
      CALL XERRWD (MSG, 40, 19, 0, 1, I, 0, 1, RelTolI, 0.0D0) 
      GO TO 700 
  620 MSG = 'DLSODE-  AbsTol(I1) is R1 .LT. 0.0        ' 
      CALL XERRWD (MSG, 40, 20, 0, 1, I, 0, 1, AbsTolI, 0.0D0) 
      GO TO 700 
  621 EWTI = RWORK(LEWT+I-1) 
      MSG = 'DLSODE-  EWT(I1) is R1 .LE. 0.0         ' 
      CALL XERRWD (MSG, 40, 21, 0, 1, I, 0, 1, EWTI, 0.0D0) 
      GO TO 700 
  622 CONTINUE 
      MSG='DLSODE-  TOUT (=R1) too close to T(=R2) to start integration' 
      CALL XERRWD (MSG, 60, 22, 0, 0, 0, 0, 2, TOUT, T) 
      GO TO 700 
  623 CONTINUE 
      MSG='DLSODE-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  ' 
      CALL XERRWD (MSG, 60, 23, 0, 1, ITASK, 0, 2, TOUT, TP) 
      GO TO 700 
  624 CONTINUE 
      MSG='DLSODE-  ITASK = 4 OR 5 and TCRIT (=R1) behind TCUR (=R2)   ' 
      CALL XERRWD (MSG, 60, 24, 0, 0, 0, 0, 2, TCRIT, TN) 
      GO TO 700 
  625 CONTINUE 
      MSG='DLSODE-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   ' 
      CALL XERRWD (MSG, 60, 25, 0, 0, 0, 0, 2, TCRIT, TOUT) 
      GO TO 700 
  626 MSG = 'DLSODE-  At start of problem, too much accuracy   ' 
      CALL XERRWD (MSG, 50, 26, 0, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      MSG='      requested for precision of machine..  See TOLSF (=R1) ' 
      CALL XERRWD (MSG, 60, 26, 0, 0, 0, 0, 1, TOLSF, 0.0D0) 
      RWORK(14) = TOLSF 
      GO TO 700 
  627 MSG = 'DLSODE-  Trouble in DINTDY.  ITASK = I1, TOUT = R1' 
      CALL XERRWD (MSG, 50, 27, 0, 1, ITASK, 0, 1, TOUT, 0.0D0) 
!                                                                       
  700 ISTATE = -3 
      RETURN 
!                                                                       
  800 MSG = 'DLSODE-  Run aborted.. apparent infinite loop     ' 
      CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, 0.0D0, 0.0D0) 
      RETURN 
!----------------------- END OF SUBROUTINE DLSODE ----------------------
      !END SUBROUTINE DLSODE                                          
      CONTAINS                                          


!DECK DUMACH   
      KPP_REAL FUNCTION DUMACH () 
!***BEGIN PROLOGUE  DUMACH                                              
!***PURPOSE  Compute the unit roundoff of the machine.                  
!***CATEGORY  R1                                                        
!***TYPE      KPP_REAL (RUMACH-S, DUMACH-D)                     
!***KEYWORDS  MACHINE CONSTANTS                                         
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
! *Usage:                                                               
!        KPP_REAL  A, DUMACH                                    
!        A = DUMACH()                                                   
!                                                                       
! *Function Return Values:                                              
!     A : the unit roundoff of the machine.                             
!                                                                       
! *Description:                                                         
!     The unit roundoff is defined as the smallest positive machine     
!     number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH 
!     in a machine-independent manner.                                  
!                                                                       
!***REFERENCES  (NONE)                                                  
!***ROUTINES CALLED  DUMSUM                                             
!***REVISION HISTORY  (YYYYMMDD)                                        
!   19930216  DATE WRITTEN                                              
!   19930818  Added SLATEC-format prologue.  (FNF)                      
!   20030707  Added DUMSUM to force normal storage of COMP.  (ACH)      
!***END PROLOGUE  DUMACH                                                
!                                                                       
      KPP_REAL U, COMP 
!***FIRST EXECUTABLE STATEMENT  DUMACH                                  
      U = 1.0D0 
   10 U = U*0.5D0 
      CALL DUMSUM(1.0D0, U, COMP) 
      IF (COMP .NE. 1.0D0) GO TO 10 
      DUMACH = U*2.0D0 
      RETURN 
!----------------------- End of Function DUMACH ------------------------
      END FUNCTION DUMACH  
                                              
      SUBROUTINE DUMSUM(A,B,C) 
!     Routine to force normal storing of A + B, for DUMACH.             
      KPP_REAL A, B, C 
      C = A + B 
      RETURN 
      END SUBROUTINE DUMSUM                                        
!DECK DCFODE                                                            
      SUBROUTINE DCFODE (METH, ELCO, TESCO) 
!***BEGIN PROLOGUE  DCFODE                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Set ODE integrator coefficients.                           
!***TYPE      KPP_REAL (SCFODE-S, DCFODE-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  DCFODE is called by the integrator routine to set coefficients       
!  needed there.  The coefficients for the current method, as           
!  given by the value of METH, are set for all orders and saved.        
!  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2.  
!  (A smaller value of the maximum order is also allowed.)              
!  DCFODE is called once at the beginning of the problem,               
!  and is not called again unless and until METH is changed.            
!                                                                       
!  The ELCO array contains the basic method coefficients.               
!  The coefficients el(i), 1 .le. i .le. nq+1, for the method of        
!  order nq are stored in ELCO(i,nq).  They are given by a genetrating  
!  polynomial, i.e.,                                                    
!      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq.                   
!  For the implicit Adams methods, l(x) is given by                     
!      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0.  
!  For the BDF methods, l(x) is given by                                
!      l(x) = (x+1)*(x+2)* ... *(x+nq)/K,                               
!  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq).              
!                                                                       
!  The TESCO array contains test constants used for the                 
!  local error test and the selection of step size and/or order.        
!  At order nq, TESCO(k,nq) is used for the selection of step           
!  size at order nq - 1 if k = 1, at order nq if k = 2, and at order    
!  nq + 1 if k = 3.                                                     
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  (NONE)                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!***END PROLOGUE  DCFODE                                                
!**End                                                                  
      INTEGER METH 
      INTEGER I, IB, NQ, NQM1, NQP1 
      KPP_REAL ELCO(13,12), TESCO(3,12), PC(12) 
      KPP_REAL AGAMQ, FNQ, FNQM1, PINT, RAGQ, RQFAC, RQ1FAC, TSIGN, XPIN
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DCFODE                                  
      GO TO (100, 200), METH 
!                                                                       
  100 ELCO(1,1) = 1.0D0 
      ELCO(2,1) = 1.0D0 
      TESCO(1,1) = 0.0D0 
      TESCO(2,1) = 2.0D0 
      TESCO(1,2) = 1.0D0 
      TESCO(3,12) = 0.0D0 
      PC(1) = 1.0D0 
      RQFAC = 1.0D0 
      DO 140 NQ = 2,12 
!-----------------------------------------------------------------------
! The PC array will contain the coefficients of the polynomial          
!     p(x) = (x+1)*(x+2)*...*(x+nq-1).                                  
! Initially, p(x) = 1.                                                  
!-----------------------------------------------------------------------
        RQ1FAC = RQFAC 
        RQFAC = RQFAC/NQ 
        NQM1 = NQ - 1 
        FNQM1 = NQM1 
        NQP1 = NQ + 1 
! Form coefficients of p(x)*(x+nq-1). ----------------------------------
        PC(NQ) = 0.0D0 
        DO 110 IB = 1,NQM1 
          I = NQP1 - IB 
  110     PC(I) = PC(I-1) + FNQM1*PC(I) 
        PC(1) = FNQM1*PC(1) 
! Compute integral, -1 to 0, of p(x) and x*p(x). -----------------------
        PINT = PC(1) 
        XPIN = PC(1)/2.0D0 
        TSIGN = 1.0D0 
        DO 120 I = 2,NQ 
          TSIGN = -TSIGN 
          PINT = PINT + TSIGN*PC(I)/I 
  120     XPIN = XPIN + TSIGN*PC(I)/(I+1) 
! Store coefficients in ELCO and TESCO. --------------------------------
        ELCO(1,NQ) = PINT*RQ1FAC 
        ELCO(2,NQ) = 1.0D0 
        DO 130 I = 2,NQ 
  130     ELCO(I+1,NQ) = RQ1FAC*PC(I)/I 
        AGAMQ = RQFAC*XPIN 
        RAGQ = 1.0D0/AGAMQ 
        TESCO(2,NQ) = RAGQ 
        IF (NQ .LT. 12) TESCO(1,NQP1) = RAGQ*RQFAC/NQP1 
        TESCO(3,NQM1) = RAGQ 
  140   CONTINUE 
      RETURN 
!                                                                       
  200 PC(1) = 1.0D0 
      RQ1FAC = 1.0D0 
      DO 230 NQ = 1,5 
!-----------------------------------------------------------------------
! The PC array will contain the coefficients of the polynomial          
!     p(x) = (x+1)*(x+2)*...*(x+nq).                                    
! Initially, p(x) = 1.                                                  
!-----------------------------------------------------------------------
        FNQ = NQ 
        NQP1 = NQ + 1 
! Form coefficients of p(x)*(x+nq). ------------------------------------
        PC(NQP1) = 0.0D0 
        DO 210 IB = 1,NQ 
          I = NQ + 2 - IB 
  210     PC(I) = PC(I-1) + FNQ*PC(I) 
        PC(1) = FNQ*PC(1) 
! Store coefficients in ELCO and TESCO. --------------------------------
        DO 220 I = 1,NQP1 
  220     ELCO(I,NQ) = PC(I)/PC(2) 
        ELCO(2,NQ) = 1.0D0 
        TESCO(1,NQ) = RQ1FAC 
        TESCO(2,NQ) = NQP1/ELCO(1,NQ) 
        TESCO(3,NQ) = (NQ+2)/ELCO(1,NQ) 
        RQ1FAC = RQ1FAC/FNQ 
  230   CONTINUE 
      RETURN 
!----------------------- END OF SUBROUTINE DCFODE ----------------------
      END SUBROUTINE DCFODE                                         
!DECK DINTDY                                                            
      SUBROUTINE DINTDY (T, K, YH, NYH, DKY, IFLAG) 
!***BEGIN PROLOGUE  DINTDY                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Interpolate solution derivatives.                          
!***TYPE      KPP_REAL (SINTDY-S, DINTDY-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  DINTDY computes interpolated values of the K-th derivative of the    
!  dependent variable vector y, and stores it in DKY.  This routine     
!  is called within the package with K = 0 and T = TOUT, but may        
!  also be called by the user for any K up to the current order.        
!  (See detailed instructions in the usage documentation.)              
!                                                                       
!  The computed values in DKY are gotten by interpolation using the     
!  Nordsieck history array YH.  This array corresponds uniquely to a    
!  vector-valued polynomial of degree NQCUR or less, and DKY is set     
!  to the K-th derivative of this polynomial at T.                      
!  The formula for DKY is:                                              
!               q                                                       
!   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1)      
!              j=K                                                      
!  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR. 
!  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are         
!  communicated by COMMON.  The above sum is done in reverse order.     
!  IFLAG is returned negative if either K or T is out of bounds.        
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  XERRWD                                             
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!   010418  Reduced size of Common block /DLS001/. (ACH)                
!   031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
!   050427  Corrected roundoff decrement in TP. (ACH)                   
!***END PROLOGUE  DINTDY                                                
!**End                                                                  
      INTEGER K, NYH, IFLAG 
      KPP_REAL T, YH(NYH,*), DKY(*) 
      INTEGER IOWND, IOWNS,                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      KPP_REAL ROWNS,                                          &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND                  
      COMMON /DLS001/ ROWNS(209),                                      &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,                 &
        IOWND(6), IOWNS(6),                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      INTEGER I, IC, J, JB, JB2, JJ, JJ1, JP1 
      KPP_REAL C, R, S, TP 
      CHARACTER*80 MSG 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DINTDY                                  
      IFLAG = 0 
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80 
      TP = TN - HU -  100.0D0*UROUND*SIGN(ABS(TN) + ABS(HU), HU) 
      IF ((T-TP)*(T-TN) .GT. 0.0D0) GO TO 90 
!                                                                       
      S = (T - TN)/H 
      IC = 1 
      IF (K .EQ. 0) GO TO 15 
      JJ1 = L - K 
      DO 10 JJ = JJ1,NQ 
   10   IC = IC*JJ 
   15 C = IC 
      DO 20 I = 1,N 
   20   DKY(I) = C*YH(I,L) 
      IF (K .EQ. NQ) GO TO 55 
      JB2 = NQ - K 
      DO 50 JB = 1,JB2 
        J = NQ - JB 
        JP1 = J + 1 
        IC = 1 
        IF (K .EQ. 0) GO TO 35 
        JJ1 = JP1 - K 
        DO 30 JJ = JJ1,J 
   30     IC = IC*JJ 
   35   C = IC 
        DO 40 I = 1,N 
   40     DKY(I) = C*YH(I,JP1) + S*DKY(I) 
   50   CONTINUE 
      IF (K .EQ. 0) RETURN 
   55 R = H**(-K) 
      DO 60 I = 1,N 
   60   DKY(I) = R*DKY(I) 
      RETURN 
!                                                                       
   80 MSG = 'DINTDY-  K (=I1) illegal      ' 
      CALL XERRWD (MSG, 30, 51, 0, 1, K, 0, 0, 0.0D0, 0.0D0) 
      IFLAG = -1 
      RETURN 
   90 MSG = 'DINTDY-  T (=R1) illegal      ' 
      CALL XERRWD (MSG, 30, 52, 0, 0, 0, 0, 1, T, 0.0D0) 
      MSG='      T not in interval TCUR - HU (= R1) to TCUR (=R2)      ' 
      CALL XERRWD (MSG, 60, 52, 0, 0, 0, 0, 2, TP, TN) 
      IFLAG = -2 
      RETURN 
!----------------------- END OF SUBROUTINE DINTDY ----------------------
      END SUBROUTINE DINTDY                                          
!DECK DPREPJ                                                            
      SUBROUTINE DPREPJ (NEQ, Y, YH, NYH, EWT, FTEM, SAVF, WM, IWM, F, JAC)                                                        
!***BEGIN PROLOGUE  DPREPJ                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Compute and process Newton iteration matrix.               
!***TYPE      KPP_REAL (SPREPJ-S, DPREPJ-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  DPREPJ is called by DSTODE to compute and process the matrix         
!  P = I - h*el(1)*J , where J is an approximation to the Jacobian.     
!  Here J is computed by the user-supplied routine JAC if               
!  MITER = 1 or 4, or by finite differencing if MITER = 2, 3, or 5.     
!  If MITER = 3, a diagonal approximation to J is used.                 
!  J is stored in WM and replaced by P.  If MITER .ne. 3, P is then     
!  subjected to LU decomposition in preparation for later solution      
!  of linear systems with P as coefficient matrix.  This is done        
!  by DGEFA if MITER = 1 or 2, and by DGBFA if MITER = 4 or 5.          
!                                                                       
!  In addition to variables described in DSTODE and DLSODE prologues,   
!  communication with DPREPJ uses the following:                        
!  Y     = array containing predicted values on entry.                  
!  FTEM  = work array of length N (ACOR in DSTODE).                     
!  SAVF  = array containing f evaluated at predicted y.                 
!  WM    = real work space for matrices.  On output it contains the     
!          inverse diagonal matrix if MITER = 3 and the LU decomposition
!          of P if MITER is 1, 2 , 4, or 5.                             
!          Storage of matrix elements starts at WM(3).                  
!          WM also contains the following matrix-related data:          
!          WM(1) = SQRT(UROUND), used in numerical Jacobian increments. 
!          WM(2) = H*EL0, saved for later use if MITER = 3.             
!  IWM   = integer work space containing pivot information, starting at 
!          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band  
!          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.   
!  EL0   = EL(1) (input).                                               
!  IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if             
!          P matrix found to be singular.                               
!  JCUR  = output flag = 1 to indicate that the Jacobian matrix         
!          (or approximation) is now current.                           
!  This routine also uses the COMMON variables EL0, H, TN, UROUND,      
!  MITER, N, NFE, and NJE.                                              
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  DGBFA, DGEFA, DVNORM                               
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890504  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!   010418  Reduced size of Common block /DLS001/. (ACH)                
!   031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
!***END PROLOGUE  DPREPJ                                                
!**End                                                                  
      EXTERNAL F, JAC 
      INTEGER NEQ, NYH, IWM(*)
      KPP_REAL Y(*), YH(NYH,*), EWT(*), FTEM(*), SAVF(*), WM(*) 
      INTEGER IOWND, IOWNS,                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      KPP_REAL ROWNS,                                          &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND                  
      COMMON /DLS001/ ROWNS(209),                                      &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,                 &
        IOWND(6), IOWNS(6),                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      INTEGER I, I1, I2, IER, II, J, J1, JJ, LENP,                     &
              MBA, MBAND, MEB1, MEBAND, ML, ML3, MU, NP1                     
      KPP_REAL CON, DI, FAC, HL0, R, R0, SRUR, YI, YJ, YJJ
      !KPP_REAL  DVNORM                                                         
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DPREPJ                                  
      NJE = NJE + 1 
      IERPJ = 0 
      JCUR = 1 
      HL0 = H*EL0 
      CON = -HL0 

#ifdef FULL_ALGEBRA
      LENP = N*N 
      DO i = 1,LENP 
         WM(i+2) = 0.0D0 
      END DO
      CALL JAC_CHEM (NEQ, TN, Y, WM(3)) 
      DO I = 1,LENP 
        WM(I+2) = WM(I+2)*CON
      END DO 
      ! Add identity matrix
      J = 3 
      NP1 = N + 1 
      DO I = 1,N 
        WM(J) = WM(J) + 1.0D0 
        J = J + NP1 
      END DO
      ! Do LU decomposition on P
      CALL DGETRF(N,N,WM(3),N,IWM(21),IER)
#else
      CALL JAC_CHEM (NEQ, TN, Y, WM(3)) 
      DO i = 1,LU_NONZERO 
         WM(i+2) = WM(i+2)*CON 
      END DO
      ! Add identity matrix
      DO i = 1,N 
        j = 2+LU_DIAG(i)
        WM(j) = WM(j) + 1.0D0 
      END DO 
      ! Do LU decomposition on P
      CALL KppDecomp(WM(3),IER)
#endif      
      IF (IER .NE. 0) IERPJ = 1 
      RETURN 
 !----------------------- END OF SUBROUTINE DPREPJ ----------------------
      END SUBROUTINE DPREPJ                                          
!DECK DSOLSY                                                            
      SUBROUTINE DSOLSY (WM, IWM, X, TEM) 
!***BEGIN PROLOGUE  DSOLSY                                              
!***SUBSIDIARY                                                          
!***PURPOSE  ODEPACK linear system solver.                              
!***TYPE      KPP_REAL (SSOLSY-S, DSOLSY-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  This routine manages the solution of the linear system arising from  
!  a chord iteration.  It is called if MITER .ne. 0.                    
!  If MITER is 1 or 2, it calls DGESL to accomplish this.               
!  If MITER = 3 it updates the coefficient h*EL0 in the diagonal        
!  matrix, and then computes the solution.                              
!  If MITER is 4 or 5, it calls DGBSL.                                  
!  Communication with DSOLSY uses the following variables:              
!  WM    = real work space containing the inverse diagonal matrix if    
!          MITER = 3 and the LU decomposition of the matrix otherwise.  
!          Storage of matrix elements starts at WM(3).                  
!          WM also contains the following matrix-related data:          
!          WM(1) = SQRT(UROUND) (not used here),                        
!          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3. 
!  IWM   = integer work space containing pivot information, starting at 
!          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band  
!          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.   
!  X     = the right-hand side vector on input, and the solution vector 
!          on output, of length N.                                      
!  TEM   = vector of work space of length N, not used in this version.  
!  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred.  
!          IERSL = 1 if a singular matrix arose with MITER = 3.         
!  This routine also uses the COMMON variables EL0, H, MITER, and N.    
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  DGBSL, DGESL                                       
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!   010418  Reduced size of Common block /DLS001/. (ACH)                
!   031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
!***END PROLOGUE  DSOLSY                                                
!**End                                                                  
      INTEGER IWM(*) 
      KPP_REAL WM(*), X(*), TEM(*) 
      INTEGER IOWND, IOWNS,                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      KPP_REAL ROWNS,                                          &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND                  
      COMMON /DLS001/ ROWNS(209),                                      &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,                 &
        IOWND(6), IOWNS(6),                                            &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      INTEGER I, MEBAND, ML, MU 
      KPP_REAL DI, HL0, PHL0, R 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DSOLSY                                  
      IERSL = 0 
#ifdef FULL_ALGEBRA      
      CALL DGETRS ('N',N,1,WM(3),N,IWM(21),X,N,0)
#else
      CALL KppSolve(WM(3),X)
#endif      
      RETURN 
!----------------------- END OF SUBROUTINE DSOLSY ----------------------
      END SUBROUTINE DSOLSY                                          
!DECK DSRCOM                                                            
      SUBROUTINE DSRCOM (RSAV, ISAV, JOB) 
!***BEGIN PROLOGUE  DSRCOM                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Save/restore ODEPACK COMMON blocks.                        
!***TYPE      KPP_REAL (SSRCOM-S, DSRCOM-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  This routine saves or restores (depending on JOB) the contents of    
!  the COMMON block DLS001, which is used internally                    
!  by one or more ODEPACK solvers.                                      
!                                                                       
!  RSAV = real array of length 218 or more.                             
!  ISAV = integer array of length 37 or more.                           
!  JOB  = flag indicating to save or restore the COMMON blocks:         
!         JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV)      
!         JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV)    
!         A call with JOB = 2 presumes a prior call with JOB = 1.       
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  (NONE)                                             
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   921116  Deleted treatment of block /EH0001/.  (ACH)                 
!   930801  Reduced Common block length by 2.  (ACH)                    
!   930809  Renamed to allow single/double precision versions. (ACH)    
!   010418  Reduced Common block length by 209+12. (ACH)                
!   031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
!   031112  Added SAVE statement for data-loaded constants.             
!***END PROLOGUE  DSRCOM                                                
!**End                                                                  
      INTEGER ISAV(*), JOB 
      INTEGER ILS 
      INTEGER I, LENILS, LENRLS 
      KPP_REAL RSAV(*),   RLS 
      SAVE LENRLS, LENILS 
      COMMON /DLS001/ RLS(218), ILS(37) 
      DATA LENRLS/218/, LENILS/37/ 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DSRCOM                                  
      IF (JOB .EQ. 2) GO TO 100 
!                                                                       
      DO 10 I = 1,LENRLS 
   10   RSAV(I) = RLS(I) 
      DO 20 I = 1,LENILS 
   20   ISAV(I) = ILS(I) 
      RETURN 
!                                                                       
  100 CONTINUE 
      DO 110 I = 1,LENRLS 
  110    RLS(I) = RSAV(I) 
      DO 120 I = 1,LENILS 
  120    ILS(I) = ISAV(I) 
      RETURN 
!----------------------- END OF SUBROUTINE DSRCOM ----------------------
      END SUBROUTINE DSRCOM                                          
!DECK DSTODE                                                            
      SUBROUTINE DSTODE (NEQ, Y, YH, NYH, YH1, EWT, SAVF, ACOR, &
                         WM, IWM, F, JAC)                                   
                        !WM, IWM, F, JAC, PJAC, SLVS)                                   
!***BEGIN PROLOGUE  DSTODE                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Performs one step of an ODEPACK integration.               
!***TYPE      KPP_REAL (SSTODE-S, DSTODE-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  DSTODE performs one step of the integration of an initial value      
!  problem for a system of ordinary differential equations.             
!  Note:  DSTODE is independent of the value of the iteration method    
!  indicator MITER, when this is .ne. 0, and hence is independent       
!  of the type of chord method used, or the Jacobian structure.         
!  Communication with DSTODE is done with the following variables:      
!                                                                       
!  NEQ    = integer array containing problem size in NEQ, and        
!           passed as the NEQ argument in all calls to F and JAC.       
!  Y      = an array of length .ge. N used as the Y argument in         
!           all calls to F and JAC.                                     
!  YH     = an NYH by LMAX array containing the dependent variables     
!           and their approximate scaled derivatives, where             
!           LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate      
!           j-th derivative of y(i), scaled by h**j/factorial(j)        
!           (j = 0,1,...,NQ).  on entry for the first step, the first   
!           two columns of YH must be set from the initial values.      
!  NYH    = a constant integer .ge. N, the first dimension of YH.       
!  YH1    = a one-dimensional array occupying the same space as YH.     
!  EWT    = an array of length N containing multiplicative weights      
!           for local error measurements.  Local errors in Y(i) are     
!           compared to 1.0/EWT(i) in various error tests.              
!  SAVF   = an array of working storage, of length N.                   
!           Also used for input of YH(*,MAXORD+2) when JSTART = -1      
!           and MAXORD .lt. the current order NQ.                       
!  ACOR   = a work array of length N, used for the accumulated          
!           corrections.  On a successful return, ACOR(i) contains      
!           the estimated one-step local error in Y(i).                 
!  WM,IWM = real and integer work arrays associated with matrix         
!           operations in chord iteration (MITER .ne. 0).               
!  PJAC   = name of routine to evaluate and preprocess Jacobian matrix  
!           and P = I - h*el0*JAC, if a chord method is being used.     
!  SLVS   = name of routine to solve linear system in chord iteration.  
!  CCMAX  = maximum relative change in h*el0 before PJAC is called.     
!  H      = the step size to be attempted on the next step.             
!           H is altered by the error control algorithm during the      
!           problem.  H can be either positive or negative, but its     
!           sign must remain constant throughout the problem.           
!  HMIN   = the minimum absolute value of the step size h to be used.   
!  HMXI   = inverse of the maximum absolute value of h to be used.      
!           HMXI = 0.0 is allowed and corresponds to an infinite hmax.  
!           HMIN and HMXI may be changed at any time, but will not      
!           take effect until the next change of h is considered.       
!  TN     = the independent variable. TN is updated on each step taken. 
!  JSTART = an integer used for input only, with the following          
!           values and meanings:                                        
!                0  perform the first step.                             
!            .gt.0  take a new step continuing from the last.           
!               -1  take the next step with a new value of H, MAXORD,   
!                     N, METH, MITER, and/or matrix parameters.         
!               -2  take the next step with a new value of H,           
!                     but with other inputs unchanged.                  
!           On return, JSTART is set to 1 to facilitate continuation.   
!  KFLAG  = a completion code with the following meanings:              
!                0  the step was succesful.                             
!               -1  the requested error could not be achieved.          
!               -2  corrector convergence could not be achieved.        
!               -3  fatal error in PJAC or SLVS.                        
!           A return with KFLAG = -1 or -2 means either                 
!           abs(H) = HMIN or 10 consecutive failures occurred.          
!           On a return with KFLAG negative, the values of TN and       
!           the YH array are as of the beginning of the last            
!           step, and H is the last step size attempted.                
!  MAXORD = the maximum order of integration method to be allowed.      
!  MAXCOR = the maximum number of corrector iterations allowed.         
!  MSBP   = maximum number of steps between PJAC calls (MITER .gt. 0).  
!  MXNCF  = maximum number of convergence failures allowed.             
!  METH/MITER = the method flags.  See description in driver.           
!  N      = the number of first-order differential equations.           
!  The values of CCMAX, H, HMIN, HMXI, TN, JSTART, KFLAG, MAXORD,       
!  MAXCOR, MSBP, MXNCF, METH, MITER, and N are communicated via COMMON. 
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  DCFODE, DVNORM                                     
!***COMMON BLOCKS    DLS001                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!   010418  Reduced size of Common block /DLS001/. (ACH)                
!   031105  Restored 'own' variables to Common block /DLS001/, to       
!           enable interrupt/restart feature. (ACH)                     
!***END PROLOGUE  DSTODE                                                
!**End                                                                  
      EXTERNAL F, JAC !, PJAC, SLVS 
      INTEGER NEQ, NYH, IWM(*) 
      KPP_REAL Y(*), YH(NYH,*), YH1(*), EWT(*), SAVF(*),       &
                       ACOR(*), WM(*) 
      INTEGER IOWND, IALTH, IPUP, LMAX, MEO, NQNYH, NSLP,              &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
      INTEGER I, I1, IREDO, IRET, J, JB, M, NCF, NEWQ 
      KPP_REAL CONIT, CRATE, EL, ELCO, HOLD, RMAX, TESCO,      &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND                  
      KPP_REAL DCON, DDN, DEL, DELP, DSM, DUP, EXDN, EXSM,     &
              EXUP,R, RH, RHDN, RHSM, RHUP, TOLD
      !KPP_REAL DVNORM                          
      COMMON /DLS001/ CONIT, CRATE, EL(13), ELCO(13,12),               &
        HOLD, RMAX, TESCO(3,12),                                       &
        CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,                 &
        IOWND(6), IALTH, IPUP, LMAX, MEO, NQNYH, NSLP,                 &
        ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,                     &
        LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,               &
        MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU         
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DSTODE                                  
      KFLAG = 0 
      TOLD = TN 
      NCF = 0 
      IERPJ = 0 
      IERSL = 0 
      JCUR = 0 
      ICF = 0 
      DELP = 0.0D0 
      IF (JSTART .GT. 0) GO TO 200 
      IF (JSTART .EQ. -1) GO TO 100 
      IF (JSTART .EQ. -2) GO TO 160 
!-----------------------------------------------------------------------
! On the first call, the order is set to 1, and other variables are     
! initialized.  RMAX is the maximum ratio by which H can be increased   
! in a single step.  It is initially 1.E4 to compensate for the small   
! initial H, but then is normally equal to 10.  If a failure            
! occurs (in corrector convergence or error test), RMAX is set to 2     
! for the next increase.                                                
!-----------------------------------------------------------------------
      LMAX = MAXORD + 1 
      NQ = 1 
      L = 2 
      IALTH = 2 
      RMAX = 10000.0D0 
      RC = 0.0D0 
      EL0 = 1.0D0 
      CRATE = 0.7D0 
      HOLD = H 
      MEO = METH 
      NSLP = 0 
      IPUP = MITER 
      IRET = 3 
      GO TO 140 
!-----------------------------------------------------------------------
! The following block handles preliminaries needed when JSTART = -1.    
! IPUP is set to MITER to force a matrix update.                        
! If an order increase is about to be considered (IALTH = 1),           
! IALTH is reset to 2 to postpone consideration one more step.          
! If the caller has changed METH, DCFODE is called to reset             
! the coefficients of the method.                                       
! If the caller has changed MAXORD to a value less than the current     
! order NQ, NQ is reduced to MAXORD, and a new H chosen accordingly.    
! If H is to be changed, YH must be rescaled.                           
! If H or METH is being changed, IALTH is reset to L = NQ + 1           
! to prevent further changes in H for that many steps.                  
!-----------------------------------------------------------------------
  100 IPUP = MITER 
      LMAX = MAXORD + 1 
      IF (IALTH .EQ. 1) IALTH = 2 
      IF (METH .EQ. MEO) GO TO 110 
      CALL DCFODE (METH, ELCO, TESCO) 
      MEO = METH 
      IF (NQ .GT. MAXORD) GO TO 120 
      IALTH = L 
      IRET = 1 
      GO TO 150 
  110 IF (NQ .LE. MAXORD) GO TO 160 
  120 NQ = MAXORD 
      L = LMAX 
      DO 125 I = 1,L 
  125   EL(I) = ELCO(I,NQ) 
      NQNYH = NQ*NYH 
      RC = RC*EL(1)/EL0 
      EL0 = EL(1) 
      CONIT = 0.5D0/(NQ+2) 
      DDN = DVNORM (N, SAVF, EWT)/TESCO(1,L) 
      EXDN = 1.0D0/L 
      RHDN = 1.0D0/(1.3D0*DDN**EXDN + 0.0000013D0) 
      RH = MIN(RHDN,1.0D0) 
      IREDO = 3 
      IF (H .EQ. HOLD) GO TO 170 
      RH = MIN(RH,ABS(H/HOLD)) 
      H = HOLD 
      GO TO 175 
!-----------------------------------------------------------------------
! DCFODE is called to get all the integration coefficients for the      
! current METH.  Then the EL vector and related constants are reset     
! whenever the order NQ is changed, or at the start of the problem.     
!-----------------------------------------------------------------------
  140 CALL DCFODE (METH, ELCO, TESCO) 
  150 DO 155 I = 1,L 
  155   EL(I) = ELCO(I,NQ) 
      NQNYH = NQ*NYH 
      RC = RC*EL(1)/EL0 
      EL0 = EL(1) 
      CONIT = 0.5D0/(NQ+2) 
      GO TO (160, 170, 200), IRET 
!-----------------------------------------------------------------------
! If H is being changed, the H ratio RH is checked against              
! RMAX, HMIN, and HMXI, and the YH array rescaled.  IALTH is set to     
! L = NQ + 1 to prevent a change of H for that many steps, unless       
! forced by a convergence or error test failure.                        
!-----------------------------------------------------------------------
  160 IF (H .EQ. HOLD) GO TO 200 
      RH = H/HOLD 
      H = HOLD 
      IREDO = 3 
      GO TO 175 
  170 RH = MAX(RH,HMIN/ABS(H)) 
  175 RH = MIN(RH,RMAX) 
      RH = RH/MAX(1.0D0,ABS(H)*HMXI*RH) 
      R = 1.0D0 
      DO 180 J = 2,L 
        R = R*RH 
        DO 180 I = 1,N 
  180     YH(I,J) = YH(I,J)*R 
      H = H*RH 
      RC = RC*RH 
      IALTH = L 
      IF (IREDO .EQ. 0) GO TO 690 
!-----------------------------------------------------------------------
! This section computes the predicted values by effectively             
! multiplying the YH array by the Pascal Triangle matrix.               
! RC is the ratio of new to old values of the coefficient  H*EL(1).     
! When RC differs from 1 by more than CCMAX, IPUP is set to MITER       
! to force PJAC to be called, if a Jacobian is involved.                
! In any case, PJAC is called at least every MSBP steps.                
!-----------------------------------------------------------------------
  200 IF (ABS(RC-1.0D0) .GT. CCMAX) IPUP = MITER 
      IF (NST .GE. NSLP+MSBP) IPUP = MITER 
      TN = TN + H 
      I1 = NQNYH + 1 
      DO 215 JB = 1,NQ 
        I1 = I1 - NYH 
!dir$ ivdep                                                             
        DO 210 I = I1,NQNYH 
  210     YH1(I) = YH1(I) + YH1(I+NYH) 
  215   CONTINUE 
!-----------------------------------------------------------------------
! Up to MAXCOR corrector iterations are taken.  A convergence test is   
! made on the R.M.S. norm of each correction, weighted by the error     
! weight vector EWT.  The sum of the corrections is accumulated in the  
! vector ACOR(i).  The YH array is not altered in the corrector loop.   
!-----------------------------------------------------------------------
  220 M = 0 
      DO 230 I = 1,N 
  230   Y(I) = YH(I,1) 
      CALL F (NEQ, TN, Y, SAVF) 
      NFE = NFE + 1 
      IF (IPUP .LE. 0) GO TO 250 
!-----------------------------------------------------------------------
! If indicated, the matrix P = I - h*el(1)*J is reevaluated and         
! preprocessed before starting the corrector iteration.  IPUP is set    
! to 0 as an indicator that this has been done.                         
!-----------------------------------------------------------------------
      !CALL PJAC (NEQ, Y, YH, NYH, EWT, ACOR, SAVF, WM, IWM, F, JAC) 
      CALL DPREPJ(NEQ, Y, YH, NYH, EWT, ACOR, SAVF, WM, IWM, F, JAC)
      IPUP = 0 
      RC = 1.0D0 
      NSLP = NST 
      CRATE = 0.7D0 
      IF (IERPJ .NE. 0) GO TO 430 
  250 DO 260 I = 1,N 
  260   ACOR(I) = 0.0D0 
  270 IF (MITER .NE. 0) GO TO 350 
!-----------------------------------------------------------------------
! In the case of functional iteration, update Y directly from           
! the result of the last function evaluation.                           
!-----------------------------------------------------------------------
      DO 290 I = 1,N 
        SAVF(I) = H*SAVF(I) - YH(I,2) 
  290   Y(I) = SAVF(I) - ACOR(I) 
      DEL = DVNORM (N, Y, EWT) 
      DO 300 I = 1,N 
        Y(I) = YH(I,1) + EL(1)*SAVF(I) 
  300   ACOR(I) = SAVF(I) 
      GO TO 400 
!-----------------------------------------------------------------------
! In the case of the chord method, compute the corrector error,         
! and solve the linear system with that as right-hand side and          
! P as coefficient matrix.                                              
!-----------------------------------------------------------------------
  350 DO 360 I = 1,N 
  360   Y(I) = H*SAVF(I) - (YH(I,2) + ACOR(I)) 
      !CALL SLVS (WM, IWM, Y, SAVF) 
      CALL DSOLSY(WM, IWM, Y, SAVF)
      IF (IERSL .LT. 0) GO TO 430 
      IF (IERSL .GT. 0) GO TO 410 
      DEL = DVNORM (N, Y, EWT) 
      DO 380 I = 1,N 
        ACOR(I) = ACOR(I) + Y(I) 
  380   Y(I) = YH(I,1) + EL(1)*ACOR(I) 
!-----------------------------------------------------------------------
! Test for convergence.  If M.gt.0, an estimate of the convergence      
! rate constant is stored in CRATE, and this is used in the test.       
!-----------------------------------------------------------------------
  400 IF (M .NE. 0) CRATE = MAX(0.2D0*CRATE,DEL/DELP) 
      DCON = DEL*MIN(1.0D0,1.5D0*CRATE)/(TESCO(2,NQ)*CONIT) 
      IF (DCON .LE. 1.0D0) GO TO 450 
      M = M + 1 
      IF (M .EQ. MAXCOR) GO TO 410 
      IF (M .GE. 2 .AND. DEL .GT. 2.0D0*DELP) GO TO 410 
      DELP = DEL 
      CALL F (NEQ, TN, Y, SAVF) 
      NFE = NFE + 1 
      GO TO 270 
!-----------------------------------------------------------------------
! The corrector iteration failed to converge.                           
! If MITER .ne. 0 and the Jacobian is out of date, PJAC is called for   
! the next try.  Otherwise the YH array is retracted to its values      
! before prediction, and H is reduced, if possible.  If H cannot be     
! reduced or MXNCF failures have occurred, exit with KFLAG = -2.        
!-----------------------------------------------------------------------
  410 IF (MITER .EQ. 0 .OR. JCUR .EQ. 1) GO TO 430 
      ICF = 1 
      IPUP = MITER 
      GO TO 220 
  430 ICF = 2 
      NCF = NCF + 1 
      RMAX = 2.0D0 
      TN = TOLD 
      I1 = NQNYH + 1 
      DO 445 JB = 1,NQ 
        I1 = I1 - NYH 
!dir$ ivdep                                                             
        DO 440 I = I1,NQNYH 
  440     YH1(I) = YH1(I) - YH1(I+NYH) 
  445   CONTINUE 
      IF (IERPJ .LT. 0 .OR. IERSL .LT. 0) GO TO 680 
      IF (ABS(H) .LE. HMIN*1.00001D0) GO TO 670 
      IF (NCF .EQ. MXNCF) GO TO 670 
      RH = 0.25D0 
      IPUP = MITER 
      IREDO = 1 
      GO TO 170 
!-----------------------------------------------------------------------
! The corrector has converged.  JCUR is set to 0                        
! to signal that the Jacobian involved may need updating later.         
! The local error test is made and control passes to statement 500      
! if it fails.                                                          
!-----------------------------------------------------------------------
  450 JCUR = 0 
      IF (M .EQ. 0) DSM = DEL/TESCO(2,NQ) 
      IF (M .GT. 0) DSM = DVNORM (N, ACOR, EWT)/TESCO(2,NQ) 
      IF (DSM .GT. 1.0D0) GO TO 500 
!-----------------------------------------------------------------------
! After a successful step, update the YH array.                         
! Consider changing H if IALTH = 1.  Otherwise decrease IALTH by 1.     
! If IALTH is then 1 and NQ .lt. MAXORD, then ACOR is saved for         
! use in a possible order increase on the next step.                    
! If a change in H is considered, an increase or decrease in order      
! by one is considered also.  A change in H is made only if it is by a  
! factor of at least 1.1.  If not, IALTH is set to 3 to prevent         
! testing for that many steps.                                          
!-----------------------------------------------------------------------
      KFLAG = 0 
      IREDO = 0 
      NST = NST + 1 
      HU = H 
      NQU = NQ 
      DO 470 J = 1,L 
        DO 470 I = 1,N 
  470     YH(I,J) = YH(I,J) + EL(J)*ACOR(I) 
      IALTH = IALTH - 1 
      IF (IALTH .EQ. 0) GO TO 520 
      IF (IALTH .GT. 1) GO TO 700 
      IF (L .EQ. LMAX) GO TO 700 
      DO 490 I = 1,N 
  490   YH(I,LMAX) = ACOR(I) 
      GO TO 700 
!-----------------------------------------------------------------------
! The error test failed.  KFLAG keeps track of multiple failures.       
! Restore TN and the YH array to their previous values, and prepare     
! to try the step again.  Compute the optimum step size for this or     
! one lower order.  After 2 or more failures, H is forced to decrease   
! by a factor of 0.2 or less.                                           
!-----------------------------------------------------------------------
  500 KFLAG = KFLAG - 1 
      TN = TOLD 
      I1 = NQNYH + 1 
      DO 515 JB = 1,NQ 
        I1 = I1 - NYH 
!dir$ ivdep                                                             
        DO 510 I = I1,NQNYH 
  510     YH1(I) = YH1(I) - YH1(I+NYH) 
  515   CONTINUE 
      RMAX = 2.0D0 
      IF (ABS(H) .LE. HMIN*1.00001D0) GO TO 660 
      IF (KFLAG .LE. -3) GO TO 640 
      IREDO = 2 
      RHUP = 0.0D0 
      GO TO 540 
!-----------------------------------------------------------------------
! Regardless of the success or failure of the step, factors             
! RHDN, RHSM, and RHUP are computed, by which H could be multiplied     
! at order NQ - 1, order NQ, or order NQ + 1, respectively.             
! In the case of failure, RHUP = 0.0 to avoid an order increase.        
! The largest of these is determined and the new order chosen           
! accordingly.  If the order is to be increased, we compute one         
! additional scaled derivative.                                         
!-----------------------------------------------------------------------
  520 RHUP = 0.0D0 
      IF (L .EQ. LMAX) GO TO 540 
      DO 530 I = 1,N 
  530   SAVF(I) = ACOR(I) - YH(I,LMAX) 
      DUP = DVNORM (N, SAVF, EWT)/TESCO(3,NQ) 
      EXUP = 1.0D0/(L+1) 
      RHUP = 1.0D0/(1.4D0*DUP**EXUP + 0.0000014D0) 
  540 EXSM = 1.0D0/L 
      RHSM = 1.0D0/(1.2D0*DSM**EXSM + 0.0000012D0) 
      RHDN = 0.0D0 
      IF (NQ .EQ. 1) GO TO 560 
      DDN = DVNORM (N, YH(1,L), EWT)/TESCO(1,NQ) 
      EXDN = 1.0D0/NQ 
      RHDN = 1.0D0/(1.3D0*DDN**EXDN + 0.0000013D0) 
  560 IF (RHSM .GE. RHUP) GO TO 570 
      IF (RHUP .GT. RHDN) GO TO 590 
      GO TO 580 
  570 IF (RHSM .LT. RHDN) GO TO 580 
      NEWQ = NQ 
      RH = RHSM 
      GO TO 620 
  580 NEWQ = NQ - 1 
      RH = RHDN 
      IF (KFLAG .LT. 0 .AND. RH .GT. 1.0D0) RH = 1.0D0 
      GO TO 620 
  590 NEWQ = L 
      RH = RHUP 
      IF (RH .LT. 1.1D0) GO TO 610 
      R = EL(L)/L 
      DO 600 I = 1,N 
  600   YH(I,NEWQ+1) = ACOR(I)*R 
      GO TO 630 
  610 IALTH = 3 
      GO TO 700 
  620 IF ((KFLAG .EQ. 0) .AND. (RH .LT. 1.1D0)) GO TO 610 
      IF (KFLAG .LE. -2) RH = MIN(RH,0.2D0) 
!-----------------------------------------------------------------------
! If there is a change of order, reset NQ, l, and the coefficients.     
! In any case H is reset according to RH and the YH array is rescaled.  
! Then exit from 690 if the step was OK, or redo the step otherwise.    
!-----------------------------------------------------------------------
      IF (NEWQ .EQ. NQ) GO TO 170 
  630 NQ = NEWQ 
      L = NQ + 1 
      IRET = 2 
      GO TO 150 
!-----------------------------------------------------------------------
! Control reaches this section if 3 or more failures have occured.      
! If 10 failures have occurred, exit with KFLAG = -1.                   
! It is assumed that the derivatives that have accumulated in the       
! YH array have errors of the wrong order.  Hence the first             
! derivative is recomputed, and the order is set to 1.  Then            
! H is reduced by a factor of 10, and the step is retried,              
! until it succeeds or H reaches HMIN.                                  
!-----------------------------------------------------------------------
  640 IF (KFLAG .EQ. -10) GO TO 660 
      RH = 0.1D0 
      RH = MAX(HMIN/ABS(H),RH) 
      H = H*RH 
      DO 645 I = 1,N 
  645   Y(I) = YH(I,1) 
      CALL F (NEQ, TN, Y, SAVF) 
      NFE = NFE + 1 
      DO 650 I = 1,N 
  650   YH(I,2) = H*SAVF(I) 
      IPUP = MITER 
      IALTH = 5 
      IF (NQ .EQ. 1) GO TO 200 
      NQ = 1 
      L = 2 
      IRET = 3 
      GO TO 150 
!-----------------------------------------------------------------------
! All returns are made through this section.  H is saved in HOLD        
! to allow the caller to change H on the next step.                     
!-----------------------------------------------------------------------
  660 KFLAG = -1 
      GO TO 720 
  670 KFLAG = -2 
      GO TO 720 
  680 KFLAG = -3 
      GO TO 720 
  690 RMAX = 10.0D0 
  700 R = 1.0D0/TESCO(2,NQU) 
      DO 710 I = 1,N 
  710   ACOR(I) = ACOR(I)*R 
  720 HOLD = H 
      JSTART = 1 
      RETURN 
!----------------------- END OF SUBROUTINE DSTODE ----------------------
      END SUBROUTINE DSTODE                                          
!DECK DEWSET                                                            
      SUBROUTINE DEWSET (N, ITOL, RelTol, AbsTol, YCUR, EWT) 
!***BEGIN PROLOGUE  DEWSET                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Set error weight vector.                                   
!***TYPE      KPP_REAL (SEWSET-S, DEWSET-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  This subroutine sets the error weight vector EWT according to        
!      EWT(i) = RelTol(i)*ABS(YCUR(i)) + AbsTol(i),  i = 1,...,N,           
!  with the subscript on RelTol and/or AbsTol possibly replaced by 1 above, 
!  depending on the value of ITOL.                                      
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  (NONE)                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!***END PROLOGUE  DEWSET                                                
!**End                                                                  
      INTEGER N, ITOL 
      INTEGER I 
      KPP_REAL RelTol(*), AbsTol(*), YCUR(N), EWT(N) 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DEWSET                                  
      GO TO (10, 20, 30, 40), ITOL 
   10 CONTINUE 
      DO 15 I = 1,N 
   15   EWT(I) = RelTol(1)*ABS(YCUR(I)) + AbsTol(1) 
      RETURN 
   20 CONTINUE 
      DO 25 I = 1,N 
   25   EWT(I) = RelTol(1)*ABS(YCUR(I)) + AbsTol(I) 
      RETURN 
   30 CONTINUE 
      DO 35 I = 1,N 
   35   EWT(I) = RelTol(I)*ABS(YCUR(I)) + AbsTol(1) 
      RETURN 
   40 CONTINUE 
      DO 45 I = 1,N 
   45   EWT(I) = RelTol(I)*ABS(YCUR(I)) + AbsTol(I) 
      RETURN 
!----------------------- END OF SUBROUTINE DEWSET ----------------------
      END SUBROUTINE DEWSET                                          
!DECK DVNORM                                                            
      KPP_REAL FUNCTION DVNORM (N, V, W) 
!***BEGIN PROLOGUE  DVNORM                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Weighted root-mean-square vector norm.                     
!***TYPE      KPP_REAL (SVNORM-S, DVNORM-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  This function routine computes the weighted root-mean-square norm    
!  of the vector of length N contained in the array V, with weights     
!  contained in the array W of length N:                                
!    DVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 )                       
!                                                                       
!***SEE ALSO  DLSODE                                                    
!***ROUTINES CALLED  (NONE)                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   791129  DATE WRITTEN                                                
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)             
!   890503  Minor cosmetic changes.  (FNF)                              
!   930809  Renamed to allow single/double precision versions. (ACH)    
!***END PROLOGUE  DVNORM                                                
!**End                                                                  
      INTEGER N,   I 
      KPP_REAL V(N), W(N), SUM 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  DVNORM                                  
      SUM = 0.0D0 
      DO 10 I = 1,N 
   10   SUM = SUM + (V(I)*W(I))**2 
      DVNORM = SQRT(SUM/N) 
      RETURN 
!----------------------- END OF FUNCTION DVNORM ------------------------
      END FUNCTION DVNORM                                          
!DECK XERRWD                                                            
      SUBROUTINE XERRWD (MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2) 
!***BEGIN PROLOGUE  XERRWD                                              
!***SUBSIDIARY                                                          
!***PURPOSE  Write error message with values.                           
!***CATEGORY  R3C                                                       
!***TYPE      KPP_REAL (XERRWV-S, XERRWD-D)                     
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,   
!  as given here, constitute a simplified version of the SLATEC error   
!  handling package.                                                    
!                                                                       
!  All arguments are input arguments.                                   
!                                                                       
!  MSG    = The message (character array).                              
!  NMES   = The length of MSG (number of characters).                   
!  NERR   = The error number (not used).                                
!  LEVEL  = The error level..                                           
!           0 or 1 means recoverable (control returns to caller).       
!           2 means fatal (run is aborted--see note below).             
!  NI     = Number of integers (0, 1, or 2) to be printed with message. 
!  I1,I2  = Integers to be printed, depending on NI.                    
!  NR     = Number of reals (0, 1, or 2) to be printed with message.    
!  R1,R2  = Reals to be printed, depending on NR.                       
!                                                                       
!  Note..  this routine is machine-dependent and specialized for use    
!  in limited context, in the following ways..                          
!  1. The argument MSG is assumed to be of type CHARACTER, and          
!     the message is printed with a format of (1X,A).                   
!  2. The message is assumed to take only one line.                     
!     Multi-line messages are generated by repeated calls.              
!  3. If LEVEL = 2, control passes to the statement   STOP              
!     to abort the run.  This statement may be machine-dependent.       
!  4. R1 and R2 are assumed to be in double precision and are printed   
!     in D21.13 format.                                                 
!                                                                       
!***ROUTINES CALLED  IXSAV                                              
!***REVISION HISTORY  (YYMMDD)                                          
!   920831  DATE WRITTEN                                                
!   921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH)                      
!   930329  Modified prologue to SLATEC format. (FNF)                   
!   930407  Changed MSG from CHARACTER*1 array to variable. (FNF)       
!   930922  Minor cosmetic change. (FNF)                                
!***END PROLOGUE  XERRWD                                                
!                                                                       
!*Internal Notes:                                                       
!                                                                       
! For a different default logical unit number, IXSAV (or a subsidiary   
! routine that it calls) will need to be modified.                      
! For a different run-abort command, change the statement following     
! statement 100 at the end.                                             
!-----------------------------------------------------------------------
! Subroutines called by XERRWD.. None                                   
! Function routine called by XERRWD.. IXSAV                             
!-----------------------------------------------------------------------
!**End                                                                  
!                                                                       
!  Declare arguments.                                                   
!                                                                       
      KPP_REAL R1, R2 
      INTEGER NMES, NERR, LEVEL, NI, I1, I2, NR 
      CHARACTER*(*) MSG 
!                                                                       
!  Declare local variables.                                             
!                                                                       
      INTEGER LUNIT, MESFLG !, IXSAV 
!                                                                       
!  Get logical unit number and message print flag.                      
!                                                                       
!***FIRST EXECUTABLE STATEMENT  XERRWD                                  
      LUNIT = IXSAV (1, 0, .FALSE.) 
      MESFLG = IXSAV (2, 0, .FALSE.) 
      IF (MESFLG .EQ. 0) GO TO 100 
!                                                                       
!  Write the message.                                                   
!                                                                       
      WRITE (LUNIT,10)  MSG 
   10 FORMAT(1X,A) 
      IF (NI .EQ. 1) WRITE (LUNIT, 20) I1 
   20 FORMAT(6X,'In above message,  I1 =',I10) 
      IF (NI .EQ. 2) WRITE (LUNIT, 30) I1,I2 
   30 FORMAT(6X,'In above message,  I1 =',I10,3X,'I2 =',I10) 
      IF (NR .EQ. 1) WRITE (LUNIT, 40) R1 
   40 FORMAT(6X,'In above message,  R1 =',D21.13) 
      IF (NR .EQ. 2) WRITE (LUNIT, 50) R1,R2 
   50 FORMAT(6X,'In above,  R1 =',D21.13,3X,'R2 =',D21.13) 
!                                                                       
!  Abort the run if LEVEL = 2.                                          
!                                                                       
  100 IF (LEVEL .NE. 2) RETURN 
      STOP 
!----------------------- End of Subroutine XERRWD ----------------------
      END SUBROUTINE XERRWD                                          
!DECK XSETF                                                             
      SUBROUTINE XSETF (MFLAG) 
!***BEGIN PROLOGUE  XSETF                                               
!***PURPOSE  Reset the error print control flag.                        
!***CATEGORY  R3A                                                       
!***TYPE      ALL (XSETF-A)                                             
!***KEYWORDS  ERROR CONTROL                                             
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!   XSETF sets the error print control flag to MFLAG:                   
!      MFLAG=1 means print all messages (the default).                  
!      MFLAG=0 means no printing.                                       
!                                                                       
!***SEE ALSO  XERRWD, XERRWV                                            
!***REFERENCES  (NONE)                                                  
!***ROUTINES CALLED  IXSAV                                              
!***REVISION HISTORY  (YYMMDD)                                          
!   921118  DATE WRITTEN                                                
!   930329  Added SLATEC format prologue. (FNF)                         
!   930407  Corrected SEE ALSO section. (FNF)                           
!   930922  Made user-callable, and other cosmetic changes. (FNF)       
!***END PROLOGUE  XSETF                                                 
!                                                                       
! Subroutines called by XSETF.. None                                    
! Function routine called by XSETF.. IXSAV                              
!-----------------------------------------------------------------------
!**End                                                                  
      INTEGER MFLAG, JUNK !, IXSAV 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  XSETF                                   
      IF (MFLAG .EQ. 0 .OR. MFLAG .EQ. 1) JUNK = IXSAV (2,MFLAG,.TRUE.) 
      RETURN 
!----------------------- End of Subroutine XSETF -----------------------
      END SUBROUTINE XSETF                                          
!DECK XSETUN                                                            
      SUBROUTINE XSETUN (LUN) 
!***BEGIN PROLOGUE  XSETUN                                              
!***PURPOSE  Reset the logical unit number for error messages.          
!***CATEGORY  R3B                                                       
!***TYPE      ALL (XSETUN-A)                                            
!***KEYWORDS  ERROR CONTROL                                             
!***DESCRIPTION                                                         
!                                                                       
!   XSETUN sets the logical unit number for error messages to LUN.      
!                                                                       
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***SEE ALSO  XERRWD, XERRWV                                            
!***REFERENCES  (NONE)                                                  
!***ROUTINES CALLED  IXSAV                                              
!***REVISION HISTORY  (YYMMDD)                                          
!   921118  DATE WRITTEN                                                
!   930329  Added SLATEC format prologue. (FNF)                         
!   930407  Corrected SEE ALSO section. (FNF)                           
!   930922  Made user-callable, and other cosmetic changes. (FNF)       
!***END PROLOGUE  XSETUN                                                
!                                                                       
! Subroutines called by XSETUN.. None                                   
! Function routine called by XSETUN.. IXSAV                             
!-----------------------------------------------------------------------
!**End                                                                  
      INTEGER LUN, JUNK !, IXSAV 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  XSETUN                                  
      IF (LUN .GT. 0) JUNK = IXSAV (1,LUN,.TRUE.) 
      RETURN 
!----------------------- End of Subroutine XSETUN ----------------------
      END SUBROUTINE XSETUN                                          
!DECK IXSAV                                                             
      INTEGER FUNCTION IXSAV (IPAR, IVALUE, ISET) 
!***BEGIN PROLOGUE  IXSAV                                               
!***SUBSIDIARY                                                          
!***PURPOSE  Save and recall error message control parameters.          
!***CATEGORY  R3C                                                       
!***TYPE      ALL (IXSAV-A)                                             
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
!                                                                       
!  IXSAV saves and recalls one of two error message parameters:         
!    LUNIT, the logical unit number to which messages are printed, and  
!    MESFLG, the message print flag.                                    
!  This is a modification of the SLATEC library routine J4SAVE.         
!                                                                       
!  Saved local variables..                                              
!   LUNIT  = Logical unit number for messages.  The default is obtained 
!            by a call to IUMACH (may be machine-dependent).            
!   MESFLG = Print control flag..                                       
!            1 means print all messages (the default).                  
!            0 means no printing.                                       
!                                                                       
!  On input..                                                           
!    IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).          
!    IVALUE = The value to be set for the parameter, if ISET = .TRUE.   
!    ISET   = Logical flag to indicate whether to read or write.        
!             If ISET = .TRUE., the parameter will be given             
!             the value IVALUE.  If ISET = .FALSE., the parameter       
!             will be unchanged, and IVALUE is a dummy argument.        
!                                                                       
!  On return..                                                          
!    IXSAV = The (old) value of the parameter.                          
!                                                                       
!***SEE ALSO  XERRWD, XERRWV                                            
!***ROUTINES CALLED  IUMACH                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   921118  DATE WRITTEN                                                
!   930329  Modified prologue to SLATEC format. (FNF)                   
!   930915  Added IUMACH call to get default output unit.  (ACH)        
!   930922  Minor cosmetic changes. (FNF)                               
!   010425  Type declaration for IUMACH added. (ACH)                    
!***END PROLOGUE  IXSAV                                                 
!                                                                       
! Subroutines called by IXSAV.. None                                    
! Function routine called by IXSAV.. IUMACH                             
!-----------------------------------------------------------------------
!**End                                                                  
      LOGICAL ISET 
      INTEGER IPAR, IVALUE 
!-----------------------------------------------------------------------
      INTEGER LUNIT, MESFLG!, IUMACH
!-----------------------------------------------------------------------
! The following Fortran-77 declaration is to cause the values of the    
! listed (local) variables to be saved between calls to this routine.   
!-----------------------------------------------------------------------
      SAVE LUNIT, MESFLG 
      DATA LUNIT/-1/, MESFLG/1/ 
!                                                                       
!***FIRST EXECUTABLE STATEMENT  IXSAV                                   
      IF (IPAR .EQ. 1) THEN 
        IF (LUNIT .EQ. -1) LUNIT = IUMACH() 
        IXSAV = LUNIT 
        IF (ISET) LUNIT = IVALUE 
        ENDIF 
!                                                                       
      IF (IPAR .EQ. 2) THEN                                             
        IXSAV = MESFLG                                                  
        IF (ISET) MESFLG = IVALUE                                       
        ENDIF                                                           
!                                                                       
      RETURN                                                            
!----------------------- End of Function IXSAV -------------------------
      END FUNCTION IXSAV                                         
!DECK IUMACH                                                            
      INTEGER FUNCTION IUMACH() 
!***BEGIN PROLOGUE  IUMACH                                              
!***PURPOSE  Provide standard output unit number.                       
!***CATEGORY  R1                                                        
!***TYPE      INTEGER (IUMACH-I)                                        
!***KEYWORDS  MACHINE CONSTANTS                                         
!***AUTHOR  Hindmarsh, Alan C., (LLNL)                                  
!***DESCRIPTION                                                         
! *Usage:                                                               
!        INTEGER  LOUT, IUMACH                                          
!        LOUT = IUMACH()                                                
!                                                                       
! *Function Return Values:                                              
!     LOUT : the standard logical unit for Fortran output.              
!                                                                       
!***REFERENCES  (NONE)                                                  
!***ROUTINES CALLED  (NONE)                                             
!***REVISION HISTORY  (YYMMDD)                                          
!   930915  DATE WRITTEN                                                
!   930922  Made user-callable, and other cosmetic changes. (FNF)       
!***END PROLOGUE  IUMACH                                                
!                                                                       
!*Internal Notes:                                                       
!  The built-in value of 6 is standard on a wide range of Fortran       
!  systems.  This may be machine-dependent.                             
!**End                                                                  
!***FIRST EXECUTABLE STATEMENT  IUMACH                                  
      IUMACH = 6 
!                                                                       
      RETURN 
!----------------------- End of Function IUMACH ------------------------
      END FUNCTION IUMACH                                         

!---- END OF SUBROUTINE DLSODE AND ITS INTERNAL PROCEDURES
      END SUBROUTINE DLSODE                                          
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE FUN_CHEM(N, T, V, FCT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE KPP_ROOT_Parameters
      USE KPP_ROOT_Global
      USE KPP_ROOT_Function, ONLY: Fun
      USE KPP_ROOT_Rates

      IMPLICIT NONE

      INTEGER :: N
      KPP_REAL :: V(NVAR), FCT(NVAR), T, TOLD
      
!      TOLD = TIME
!      TIME = T
!      CALL Update_SUN()
!      CALL Update_RCONST()
!      CALL Update_PHOTO()
!      TIME = TOLD

      CALL Fun(V, FIX, RCONST, FCT)
      
      !Nfun=Nfun+1

      END SUBROUTINE FUN_CHEM


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE JAC_CHEM (N, T, V, JF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE KPP_ROOT_Parameters
      USE KPP_ROOT_Global
      USE KPP_ROOT_JacobianSP
      USE KPP_ROOT_Jacobian, ONLY: Jac_SP
      USE KPP_ROOT_Rates

      IMPLICIT NONE

      KPP_REAL :: V(NVAR), T, TOLD
      INTEGER :: I, J, N, ML, MU, NROWPD
#ifdef FULL_ALGEBRA    
      KPP_REAL :: JV(LU_NONZERO), JF(NVAR,NVAR)
#else
      KPP_REAL :: JF(LU_NONZERO)
#endif   
  
!      TOLD = TIME
!      TIME = T
!      CALL Update_SUN()
!      CALL Update_RCONST()
!      CALL Update_PHOTO()
!      TIME = TOLD
    
#ifdef FULL_ALGEBRA    
      CALL Jac_SP(V, FIX, RCONST, JV)
      DO j=1,NVAR
      DO i=1,NVAR
         JF(i,j) = 0.0d0
      END DO
      END DO
      DO i=1,LU_NONZERO
         JF(LU_IROW(i),LU_ICOL(i)) = JV(i)
      END DO
#else
      CALL Jac_SP(V, FIX, RCONST, JF) 
#endif   
      !Njac=Njac+1
    
      END SUBROUTINE JAC_CHEM


END MODULE KPP_ROOT_Integrator
