      SUBROUTINE INTEGRATE( TIN, TOUT )

      IMPLICIT KPP_REAL (A-H,O-Z)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT

      PARAMETER ( LRW=20+3*1500+16*NVAR )
      PARAMETER ( LIW=60 )
      EXTERNAL FUNC_CHEM, JAC_CHEM

      KPP_REAL RWORK(LRW)
      INTEGER IWORK(LIW)

      STEPCUT = 0.
      MAXORD = 5
      IBEGIN = 1
      ITOL=4

C ---- NORMAL COMPUTATION ---
      ITASK=1
      ISTATE=1
C ---- USE OPTIONAL INPUT ---
      IOPT=1

      IWORK(5) = MAXORD    ! MAX ORD
      IWORK(6) = 20000
      IWORK(7) = 0
      RWORK(6) = STEPMAX   ! STEP MAX
      RWORK(7) = STEPMIN   ! STEP MIN
      RWORK(5) = STEPMIN   ! INITIAL STEP

C ----- SIGNAL FOR STIFF CASE, FULL JACOBIAN, INTERN (22) or SUPPLIED (21)
      MF = 121

      CALL atmlsodes (FUNC_CHEM, NVAR, VAR, TIN, TOUT, ITOL, RTOL, ATOL,
     !            ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW,
     !            JAC_CHEM, MF)

      IF (ISTATE.LT.0) THEN
        print *,'LSODES: Unsucessfull exit at T=',
     &          TIN,' (ISTATE=',ISTATE,')'
      ENDIF

      RETURN
      END

      subroutine atmlsodes (f, neq, y, t, tout, itol, RelTol, AbsTol,
     1       itask, istate, iopt, rwork, lrw, iwork, liw, jac, mf)
      external f, jac
      integer neq, itol, itask, istate, iopt, lrw, iwork, liw, mf
      KPP_REAL y, t, tout, RelTol, AbsTol, rwork
      dimension neq(1), y(1), RelTol(1), AbsTol(1),
     1          rwork(lrw), iwork(liw)
c-----------------------------------------------------------------------
c this is the march 30, 1987 version of
c lsodes.. livermore solver for ordinary differential equations
c          with general sparse jacobian matrices.
c this version is in KPP_REAL.
c
c lsodes solves the initial value problem for stiff or nonstiff
c systems of first order ode-s,
c     dy/dt = f(t,y) ,  or, in component form,
c     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq).
c lsodes is a variant of the lsode package, and is intended for
c problems in which the jacobian matrix df/dy has an arbitrary
c sparse structure (when the problem is stiff).
c
c authors..      alan c. hindmarsh,
c                computing and mathematics research division, l-316
c                lawrence livermore national laboratory
c                livermore, ca 94550.
c
c and            andrew h. sherman
c                j. s. nolen and associates
c                houston, tx 77084
c-----------------------------------------------------------------------
c references..
c 1.  alan c. hindmarsh,  odepack, a systematized collection of ode
c     solvers, in scientific computing, r. s. stepleman et al. (eds.),
c     north-holland, amsterdam, 1983, pp. 55-64.
c
c 2.  s. c. eisenstat, m. c. gursky, m. h. schultz, and a. h. sherman,
c     yale sparse matrix package.. i. the symmetric codes,
c     int. j. num. meth. eng., 18 (1982), pp. 1145-1151.
c
c 3.  s. c. eisenstat, m. c. gursky, m. h. schultz, and a. h. sherman,
c     yale sparse matrix package.. ii. the nonsymmetric codes,
c     research report no. 114, dept. of computer sciences, yale
c     university, 1977.
c-----------------------------------------------------------------------
c summary of usage.
c
c communication between the user and the lsodes package, for normal
c situations, is summarized here.  this summary describes only a subset
c of the full set of options available.  see the full description for
c details, including optional communication, nonstandard options,
c and instructions for special situations.  see also the example
c problem (with program and output) following this summary.
c
c a. first provide a subroutine of the form..
c               subroutine f (neq, t, y, ydot)
c               dimension y(neq), ydot(neq)
c which supplies the vector function f by loading ydot(i) with f(i).
c
c b. next determine (or guess) whether or not the problem is stiff.
c stiffness occurs when the jacobian matrix df/dy has an eigenvalue
c whose real part is negative and large in magnitude, compared to the
c reciprocal of the t span of interest.  if the problem is nonstiff,
c use a method flag mf = 10.  if it is stiff, there are two standard
c for the method flag, mf = 121 and mf = 222.  in both cases, lsodes
c requires the jacobian matrix in some form, and it treats this matrix
c in general sparse form, with sparsity structure determined internally.
c (for options where the user supplies the sparsity structure, see
c the full description of mf below.)
c
c c. if the problem is stiff, you are encouraged to supply the jacobian
c directly (mf = 121), but if this is not feasible, lsodes will
c compute it internally by difference quotients (mf = 222).
c if you are supplying the jacobian, provide a subroutine of the form..
c               subroutine jac (neq, t, y, j, ian, jan, pdj)
c               dimension y(1), ian(1), jan(1), pdj(1)
c here neq, t, y, and j are input arguments, and the jac routine is to
c load the array pdj (of length neq) with the j-th column of df/dy.
c i.e., load pdj(i) with df(i)/dy(j) for all relevant values of i.
c the arguments ian and jan should be ignored for normal situations.
c lsodes will CALL the jac routine with j = 1,2,...,neq.
c only nonzero elements need be loaded.  usually, a crude approximation
c to df/dy, possibly with fewer nonzero elements, will suffice.
c
c d. write a main program which calls subroutine lsodes once for
c each point at which answers are desired.  this should also provide
c for possible use of logical unit 6 for output of error messages
c by lsodes.  on the first CALL to lsodes, supply arguments as follows..
c f      = name of subroutine for right-hand side vector f.
c          this name must be declared external in calling program.
c neq    = number of first order ode-s.
c y      = array of initial values, of length neq.
c t      = the initial value of the independent variable.
c tout   = first point where output is desired (.ne. t).
c itol   = 1 or 2 according as AbsTol (below) is a scalar or array.
c RelTol   = relative tolerance parameter (scalar).
c AbsTol   = absolute tolerance parameter (scalar or array).
c          the estimated local error in y(i) will be controlled so as
c          to be roughly less (in magnitude) than
c             ewt(i) = RelTol*abs(y(i)) + AbsTol     if itol = 1, or
c             ewt(i) = RelTol*abs(y(i)) + AbsTol(i)  if itol = 2.
c          thus the local error test passes if, in each component,
c          either the absolute error is less than AbsTol (or AbsTol(i)),
c          or the relative error is less than RelTol.
c          use RelTol = 0.0 for pure absolute error control, and
c          use AbsTol = 0.0 (or AbsTol(i) = 0.0) for pure relative error
c          control.  caution.. actual (global) errors may exceed these
c          local tolerances, so choose them conservatively.
c itask  = 1 for normal computation of output values of y at t = tout.
c istate = integer flag (input and output).  set istate = 1.
c iopt   = 0 to indicate no optional inputs used.
c rwork  = real work array of length at least..
c             20 + 16*neq            for mf = 10,
c             20 + (2 + 1./lenrat)*nnz + (11 + 9./lenrat)*neq
c                                    for mf = 121 or 222,
c          where..
c          nnz    = the number of nonzero elements in the sparse
c                   jacobian (if this is unknown, use an estimate), and
c          lenrat = the real to integer wordlength ratio (usually 1 in
c                   single precision and 2 in KPP_REAL).
c          in any case, the required size of rwork cannot generally
c          be predicted in advance if mf = 121 or 222, and the value
c          above is a rough estimate of a crude lower bound.  some
c          experimentation with this size may be necessary.
c          (when known, the correct required length is an optional
c          output, available in iwork(17).)
c lrw    = declared length of rwork (in user-s dimension).
c iwork  = integer work array of length at least 30.
c liw    = declared length of iwork (in user-s dimension).
c jac    = name of subroutine for jacobian matrix (mf = 121).
c          if used, this name must be declared external in calling
c          program.  if not used, pass a dummy name.
c mf     = method flag.  standard values are..
c          10  for nonstiff (adams) method, no jacobian used.
c          121 for stiff (bdf) method, user-supplied sparse jacobian.
c          222 for stiff method, internally generated sparse jacobian.
c note that the main program must declare arrays y, rwork, iwork,
c and possibly AbsTol.
c
c e. the output from the first CALL (or any call) is..
c      y = array of computed values of y(t) vector.
c      t = corresponding value of independent variable (normally tout).
c istate = 2  if lsodes was successful, negative otherwise.
c          -1 means excess work done on this CALL (perhaps wrong mf).
c          -2 means excess accuracy requested (tolerances too small).
c          -3 means illegal input detected (see printed message).
c          -4 means repeated error test failures (check all inputs).
c          -5 means repeated convergence failures (perhaps bad jacobian
c             supplied or wrong choice of mf or tolerances).
c          -6 means error weight became zero during problem. (solution
c             component i vanished, and AbsTol or AbsTol(i) = 0.)
c          -7 means a fatal error return flag came from the sparse
c             solver cdrv by way of prjs or slss.  should never happen.
c          a return with istate = -1, -4, or -5 may result from using
c          an inappropriate sparsity structure, one that is quite
c          different from the initial structure.  consider calling
c          lsodes again with istate = 3 to force the structure to be
c          reevaluated.  see the full description of istate below.
c
c f. to continue the integration after a successful return, simply
c reset tout and CALL lsodes again.  no other parameters need be reset.
c
c-----------------------------------------------------------------------
c example problem.
c
c the following is a simple example problem, with the coding
c needed for its solution by lsodes.  the problem is from chemical
c kinetics, and consists of the following 12 rate equations..
c    dy1/dt  = -rk1*y1
c    dy2/dt  = rk1*y1 + rk11*rk14*y4 + rk19*rk14*y5
c                - rk3*y2*y3 - rk15*y2*y12 - rk2*y2
c    dy3/dt  = rk2*y2 - rk5*y3 - rk3*y2*y3 - rk7*y10*y3
c                + rk11*rk14*y4 + rk12*rk14*y6
c    dy4/dt  = rk3*y2*y3 - rk11*rk14*y4 - rk4*y4
c    dy5/dt  = rk15*y2*y12 - rk19*rk14*y5 - rk16*y5
c    dy6/dt  = rk7*y10*y3 - rk12*rk14*y6 - rk8*y6
c    dy7/dt  = rk17*y10*y12 - rk20*rk14*y7 - rk18*y7
c    dy8/dt  = rk9*y10 - rk13*rk14*y8 - rk10*y8
c    dy9/dt  = rk4*y4 + rk16*y5 + rk8*y6 + rk18*y7
c    dy10/dt = rk5*y3 + rk12*rk14*y6 + rk20*rk14*y7
c                + rk13*rk14*y8 - rk7*y10*y3 - rk17*y10*y12
c                - rk6*y10 - rk9*y10
c    dy11/dt = rk10*y8
c    dy12/dt = rk6*y10 + rk19*rk14*y5 + rk20*rk14*y7
c                - rk15*y2*y12 - rk17*y10*y12
c
c with rk1 = rk5 = 0.1,  rk4 = rk8 = rk16 = rk18 = 2.5,
c      rk10 = 5.0,  rk2 = rk6 = 10.0,  rk14 = 30.0,
c      rk3 = rk7 = rk9 = rk11 = rk12 = rk13 = rk19 = rk20 = 50.0,
c      rk15 = rk17 = 100.0.
c
c the t interval is from 0 to 1000, and the initial conditions
c are y1 = 1, y2 = y3 = ... = y12 = 0.  the problem is stiff.
c
c the following coding solves this problem with lsodes, using mf = 121
c and printing results at t = .1, 1., 10., 100., 1000.  it uses
c itol = 1 and mixed relative/absolute tolerance controls.
c during the run and at the end, statistical quantities of interest
c are printed (see optional outputs in the full description below).
c
c     external fex, jex
c     KPP_REAL AbsTol, RelTol, rwork, t, tout, y
c     dimension y(12), rwork(500), iwork(30)
c     data lrw/500/, liw/30/
c     neq = 12
c     do 10 i = 1,neq
c 10    y(i) = 0.0d0
c     y(1) = 1.0d0
c     t = 0.0d0
c     tout = 0.1d0
c     itol = 1
c     RelTol = 1.0d-4
c     AbsTol = 1.0d-6
c     itask = 1
c     istate = 1
c     iopt = 0
c     mf = 121
c     do 40 iout = 1,5
c       CALL lsodes (fex, neq, y, t, tout, itol, RelTol, AbsTol,
c    1     itask, istate, iopt, rwork, lrw, iwork, liw, jex, mf)
c       write(6,30)t,iwork(11),rwork(11),(y(i),i=1,neq)
c 30    format(//7h at t =,e11.3,4x,
c    1    12h no. steps =,i5,4x,12h last step =,e11.3/
c    2    13h  y array =  ,4e14.5/13x,4e14.5/13x,4e14.5)
c       if (istate .lt. 0) go to 80
c       tout = tout*10.0d0
c 40    continue
c     lenrw = iwork(17)
c     leniw = iwork(18)
c     nst = iwork(11)
c     nfe = iwork(12)
c     nje = iwork(13)
c     nlu = iwork(21)
c     nnz = iwork(19)
c     nnzlu = iwork(25) + iwork(26) + neq
c     write (6,70) lenrw,leniw,nst,nfe,nje,nlu,nnz,nnzlu
c 70  format(//22h required rwork size =,i4,15h   iwork size =,i4/
c    1   12h no. steps =,i4,12h   no. f-s =,i4,12h   no. j-s =,i4,
c    2   13h   no. lu-s =,i4/23h no. of nonzeros in j =,i5,
c    3   26h   no. of nonzeros in lu =,i5)
c     stop
c 80  write(6,90)istate
c 90  format(///22h error halt.. istate =,i3)
c     stop
c     end
c
c     subroutine fex (neq, t, y, ydot)
c     KPP_REAL t, y, ydot
c     KPP_REAL rk1, rk2, rk3, rk4, rk5, rk6, rk7, rk8, rk9,
c    1   rk10, rk11, rk12, rk13, rk14, rk15, rk16, rk17
c     dimension y(12), ydot(12)
c     data rk1/0.1d0/, rk2/10.0d0/, rk3/50.0d0/, rk4/2.5d0/, rk5/0.1d0/,
c    1   rk6/10.0d0/, rk7/50.0d0/, rk8/2.5d0/, rk9/50.0d0/, rk10/5.0d0/,
c    2   rk11/50.0d0/, rk12/50.0d0/, rk13/50.0d0/, rk14/30.0d0/,
c    3   rk15/100.0d0/, rk16/2.5d0/, rk17/100.0d0/, rk18/2.5d0/,
c    4   rk19/50.0d0/, rk20/50.0d0/
c     ydot(1)  = -rk1*y(1)
c     ydot(2)  = rk1*y(1) + rk11*rk14*y(4) + rk19*rk14*y(5)
c    1           - rk3*y(2)*y(3) - rk15*y(2)*y(12) - rk2*y(2)
c     ydot(3)  = rk2*y(2) - rk5*y(3) - rk3*y(2)*y(3) - rk7*y(10)*y(3)
c    1           + rk11*rk14*y(4) + rk12*rk14*y(6)
c     ydot(4)  = rk3*y(2)*y(3) - rk11*rk14*y(4) - rk4*y(4)
c     ydot(5)  = rk15*y(2)*y(12) - rk19*rk14*y(5) - rk16*y(5)
c     ydot(6)  = rk7*y(10)*y(3) - rk12*rk14*y(6) - rk8*y(6)
c     ydot(7)  = rk17*y(10)*y(12) - rk20*rk14*y(7) - rk18*y(7)
c     ydot(8)  = rk9*y(10) - rk13*rk14*y(8) - rk10*y(8)
c     ydot(9)  = rk4*y(4) + rk16*y(5) + rk8*y(6) + rk18*y(7)
c     ydot(10) = rk5*y(3) + rk12*rk14*y(6) + rk20*rk14*y(7)
c    1           + rk13*rk14*y(8) - rk7*y(10)*y(3) - rk17*y(10)*y(12)
c    2           - rk6*y(10) - rk9*y(10)
c     ydot(11) = rk10*y(8)
c     ydot(12) = rk6*y(10) + rk19*rk14*y(5) + rk20*rk14*y(7)
c    1           - rk15*y(2)*y(12) - rk17*y(10)*y(12)
c     return
c     end
c
c     subroutine jex (neq, t, y, j, ia, ja, pdj)
c     KPP_REAL t, y, pdj
c     KPP_REAL rk1, rk2, rk3, rk4, rk5, rk6, rk7, rk8, rk9,
c    1   rk10, rk11, rk12, rk13, rk14, rk15, rk16, rk17
c     dimension y(1), ia(1), ja(1), pdj(1)
c     data rk1/0.1d0/, rk2/10.0d0/, rk3/50.0d0/, rk4/2.5d0/, rk5/0.1d0/,
c    1   rk6/10.0d0/, rk7/50.0d0/, rk8/2.5d0/, rk9/50.0d0/, rk10/5.0d0/,
c    2   rk11/50.0d0/, rk12/50.0d0/, rk13/50.0d0/, rk14/30.0d0/,
c    3   rk15/100.0d0/, rk16/2.5d0/, rk17/100.0d0/, rk18/2.5d0/,
c    4   rk19/50.0d0/, rk20/50.0d0/
c     go to (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), j
c 1   pdj(1) = -rk1
c     pdj(2) = rk1
c     return
c 2   pdj(2) = -rk3*y(3) - rk15*y(12) - rk2
c     pdj(3) = rk2 - rk3*y(3)
c     pdj(4) = rk3*y(3)
c     pdj(5) = rk15*y(12)
c     pdj(12) = -rk15*y(12)
c     return
c 3   pdj(2) = -rk3*y(2)
c     pdj(3) = -rk5 - rk3*y(2) - rk7*y(10)
c     pdj(4) = rk3*y(2)
c     pdj(6) = rk7*y(10)
c     pdj(10) = rk5 - rk7*y(10)
c     return
c 4   pdj(2) = rk11*rk14
c     pdj(3) = rk11*rk14
c     pdj(4) = -rk11*rk14 - rk4
c     pdj(9) = rk4
c     return
c 5   pdj(2) = rk19*rk14
c     pdj(5) = -rk19*rk14 - rk16
c     pdj(9) = rk16
c     pdj(12) = rk19*rk14
c     return
c 6   pdj(3) = rk12*rk14
c     pdj(6) = -rk12*rk14 - rk8
c     pdj(9) = rk8
c     pdj(10) = rk12*rk14
c     return
c 7   pdj(7) = -rk20*rk14 - rk18
c     pdj(9) = rk18
c     pdj(10) = rk20*rk14
c     pdj(12) = rk20*rk14
c     return
c 8   pdj(8) = -rk13*rk14 - rk10
c     pdj(10) = rk13*rk14
c     pdj(11) = rk10
c 9   return
c 10  pdj(3) = -rk7*y(3)
c     pdj(6) = rk7*y(3)
c     pdj(7) = rk17*y(12)
c     pdj(8) = rk9
c     pdj(10) = -rk7*y(3) - rk17*y(12) - rk6 - rk9
c     pdj(12) = rk6 - rk17*y(12)
c 11  return
c 12  pdj(2) = -rk15*y(2)
c     pdj(5) = rk15*y(2)
c     pdj(7) = rk17*y(10)
c     pdj(10) = -rk17*y(10)
c     pdj(12) = -rk15*y(2) - rk17*y(10)
c     return
c     end
c
c the output of this program (on a cray-1 in single precision)
c is as follows..
c
c
c at t =  1.000e-01     no. steps =   12     last step =  1.515e-02
c  y array =     9.90050e-01   6.28228e-03   3.65313e-03   7.51934e-07
c                1.12167e-09   1.18458e-09   1.77291e-12   3.26476e-07
c                5.46720e-08   9.99500e-06   4.48483e-08   2.76398e-06
c
c
c at t =  1.000e+00     no. steps =   33     last step =  7.880e-02
c  y array =     9.04837e-01   9.13105e-03   8.20622e-02   2.49177e-05
c                1.85055e-06   1.96797e-06   1.46157e-07   2.39557e-05
c                3.26306e-05   7.21621e-04   5.06433e-05   3.05010e-03
c
c
c at t =  1.000e+01     no. steps =   48     last step =  1.239e+00
c  y array =     3.67876e-01   3.68958e-03   3.65133e-01   4.48325e-05
c                6.10798e-05   4.33148e-05   5.90211e-05   1.18449e-04
c                3.15235e-03   3.56531e-03   4.15520e-03   2.48741e-01
c
c
c at t =  1.000e+02     no. steps =   91     last step =  3.764e+00
c  y array =     4.44981e-05   4.42666e-07   4.47273e-04  -3.53257e-11
c                2.81577e-08  -9.67741e-11   2.77615e-07   1.45322e-07
c                1.56230e-02   4.37394e-06   1.60104e-02   9.52246e-01
c
c
c at t =  1.000e+03     no. steps =  111     last step =  4.156e+02
c  y array =    -2.65492e-13   2.60539e-14  -8.59563e-12   6.29355e-14
c               -1.78066e-13   5.71471e-13  -1.47561e-12   4.58078e-15
c                1.56314e-02   1.37878e-13   1.60184e-02   9.52719e-01
c
c
c required rwork size = 442   iwork size =  30
c no. steps = 111   no. f-s = 142   no. j-s =   2   no. lu-s =  20
c no. of nonzeros in j =   44   no. of nonzeros in lu =   50
c-----------------------------------------------------------------------
c full description of user interface to lsodes.
c
c the user interface to lsodes consists of the following parts.
c
c i.   the CALL sequence to subroutine lsodes, which is a driver
c      routine for the solver.  this includes descriptions of both
c      the CALL sequence arguments and of user-supplied routines.
c      following these descriptions is a description of
c      optional inputs available through the CALL sequence, and then
c      a description of optional outputs (in the work arrays).
c
c ii.  descriptions of other routines in the lsodes package that may be
c      (optionally) called by the user.  these provide the ability to
c      alter error message handling, save and restore the internal
c      common, and obtain specified derivatives of the solution y(t).
c
c iii. descriptions of common blocks to be declared in overlay
c      or similar environments, or to be saved when doing an interrupt
c      of the problem and continued solution later.
c
c iv.  description of two routines in the lsodes package, either of
c      which the user may replace with his own version, if desired.
c      these relate to the measurement of errors.
c
c-----------------------------------------------------------------------
c part i.  CALL sequence.
c
c the CALL sequence parameters used for input only are
c     f, neq, tout, itol, RelTol, AbsTol, itask, iopt, lrw, liw, jac, mf,
c and those used for both input and output are
c     y, t, istate.
c the work arrays rwork and iwork are also used for conditional and
c optional inputs and optional outputs.  (the term output here refers
c to the return from subroutine lsodes to the user-s calling program.)
c
c the legality of input parameters will be thoroughly checked on the
c initial CALL for the problem, but not checked thereafter unless a
c change in input parameters is flagged by istate = 3 on input.
c
c the descriptions of the CALL arguments are as follows.
c
c f      = the name of the user-supplied subroutine defining the
c          ode system.  the system must be put in the first-order
c          form dy/dt = f(t,y), where f is a vector-valued function
c          of the scalar t and the vector y.  subroutine f is to
c          compute the function f.  it is to have the form
c               subroutine f (neq, t, y, ydot)
c               dimension y(1), ydot(1)
c          where neq, t, and y are input, and the array ydot = f(t,y)
c          is output.  y and ydot are arrays of length neq.
c          (in the dimension statement above, 1 is a dummy
c          dimension.. it can be replaced by any value.)
c          subroutine f should not alter y(1),...,y(neq).
c          f must be declared external in the calling program.
c
c          subroutine f may access user-defined quantities in
c          neq(2),... and/or in y(neq(1)+1),... if neq is an array
c          (dimensioned in f) and/or y has length exceeding neq(1).
c          see the descriptions of neq and y below.
c
c          if quantities computed in the f routine are needed
c          externally to lsodes, an extra CALL to f should be made
c          for this purpose, for consistent and accurate results.
c          if only the derivative dy/dt is needed, use intdy instead.
c
c neq    = the size of the ode system (number of first order
c          ordinary differential equations).  used only for input.
c          neq may be decreased, but not increased, during the problem.
c          if neq is decreased (with istate = 3 on input), the
c          remaining components of y should be left undisturbed, if
c          these are to be accessed in f and/or jac.
c
c          normally, neq is a scalar, and it is generally referred to
c          as a scalar in this user interface description.  however,
c          neq may be an array, with neq(1) set to the system size.
c          (the lsodes package accesses only neq(1).)  in either case,
c          this parameter is passed as the neq argument in all calls
c          to f and jac.  hence, if it is an array, locations
c          neq(2),... may be used to store other integer data and pass
c          it to f and/or jac.  subroutines f and/or jac must include
c          neq in a dimension statement in that case.
c
c y      = a real array for the vector of dependent variables, of
c          length neq or more.  used for both input and output on the
c          first CALL (istate = 1), and only for output on other calls.
c          on the first call, y must contain the vector of initial
c          values.  on output, y contains the computed solution vector,
c          evaluated at t.  if desired, the y array may be used
c          for other purposes between calls to the solver.
c
c          this array is passed as the y argument in all calls to
c          f and jac.  hence its length may exceed neq, and locations
c          y(neq+1),... may be used to store other real data and
c          pass it to f and/or jac.  (the lsodes package accesses only
c          y(1),...,y(neq).)
c
c t      = the independent variable.  on input, t is used only on the
c          first call, as the initial point of the integration.
c          on output, after each call, t is the value at which a
c          computed solution y is evaluated (usually the same as tout).
c          on an error return, t is the farthest point reached.
c
c tout   = the next value of t at which a computed solution is desired.
c          used only for input.
c
c          when starting the problem (istate = 1), tout may be equal
c          to t for one call, then should .ne. t for the next call.
c          for the initial t, an input value of tout .ne. t is used
c          in order to determine the direction of the integration
c          (i.e. the algebraic sign of the step sizes) and the rough
c          scale of the problem.  integration in either direction
c          (forward or backward in t) is permitted.
c
c          if itask = 2 or 5 (one-step modes), tout is ignored after
c          the first CALL (i.e. the first CALL with tout .ne. t).
c          otherwise, tout is required on every call.
c
c          if itask = 1, 3, or 4, the values of tout need not be
c          monotone, but a value of tout which backs up is limited
c          to the current internal t interval, whose endpoints are
c          tcur - hu and tcur (see optional outputs, below, for
c          tcur and hu).
c
c itol   = an indicator for the type of error control.  see
c          description below under AbsTol.  used only for input.
c
c RelTol   = a relative error tolerance parameter, either a scalar or
c          an array of length neq.  see description below under AbsTol.
c          input only.
c
c AbsTol   = an absolute error tolerance parameter, either a scalar or
c          an array of length neq.  input only.
c
c             the input parameters itol, RelTol, and AbsTol determine
c          the error control performed by the solver.  the solver will
c          control the vector e = (e(i)) of estimated local errors
c          in y, according to an inequality of the form
c                      rms-norm of ( e(i)/ewt(i) )   .le.   1,
c          where       ewt(i) = RelTol(i)*abs(y(i)) + AbsTol(i),
c          and the rms-norm (root-mean-square norm) here is
c          rms-norm(v) = sqrt(sum v(i)**2 / neq).  here ewt = (ewt(i))
c          is a vector of weights which must always be positive, and
c          the values of RelTol and AbsTol should all be non-negative.
c          the following table gives the types (scalar/array) of
c          RelTol and AbsTol, and the corresponding form of ewt(i).
c
c             itol    RelTol       AbsTol          ewt(i)
c              1     scalar     scalar     RelTol*abs(y(i)) + AbsTol
c              2     scalar     array      RelTol*abs(y(i)) + AbsTol(i)
c              3     array      scalar     RelTol(i)*abs(y(i)) + AbsTol
c              4     array      array      RelTol(i)*abs(y(i)) + AbsTol(i)
c
c          when either of these parameters is a scalar, it need not
c          be dimensioned in the user-s calling program.
c
c          if none of the above choices (with itol, RelTol, and AbsTol
c          fixed throughout the problem) is suitable, more general
c          error controls can be obtained by substituting
c          user-supplied routines for the setting of ewt and/or for
c          the norm calculation.  see part iv below.
c
c          if global errors are to be estimated by making a repeated
c          run on the same problem with smaller tolerances, then all
c          components of RelTol and AbsTol (i.e. of ewt) should be scaled
c          down uniformly.
c
c itask  = an index specifying the task to be performed.
c          input only.  itask has the following values and meanings.
c          1  means normal computation of output values of y(t) at
c             t = tout (by overshooting and interpolating).
c          2  means take one step only and return.
c          3  means stop at the first internal mesh point at or
c             beyond t = tout and return.
c          4  means normal computation of output values of y(t) at
c             t = tout but without overshooting t = tcrit.
c             tcrit must be input as rwork(1).  tcrit may be equal to
c             or beyond tout, but not behind it in the direction of
c             integration.  this option is useful if the problem
c             has a singularity at or beyond t = tcrit.
c          5  means take one step, without passing tcrit, and return.
c             tcrit must be input as rwork(1).
c
c          note..  if itask = 4 or 5 and the solver reaches tcrit
c          (within roundoff), it will return t = tcrit (exactly) to
c          indicate this (unless itask = 4 and tout comes before tcrit,
c          in which case answers at t = tout are returned first).
c
c istate = an index used for input and output to specify the
c          the state of the calculation.
c
c          on input, the values of istate are as follows.
c          1  means this is the first CALL for the problem
c             (initializations will be done).  see note below.
c          2  means this is not the first call, and the calculation
c             is to continue normally, with no change in any input
c             parameters except possibly tout and itask.
c             (if itol, RelTol, and/or AbsTol are changed between calls
c             with istate = 2, the new values will be used but not
c             tested for legality.)
c          3  means this is not the first call, and the
c             calculation is to continue normally, but with
c             a change in input parameters other than
c             tout and itask.  changes are allowed in
c             neq, itol, RelTol, AbsTol, iopt, lrw, liw, mf,
c             the conditional inputs ia and ja,
c             and any of the optional inputs except h0.
c             in particular, if miter = 1 or 2, a CALL with istate = 3
c             will cause the sparsity structure of the problem to be
c             recomputed (or reread from ia and ja if moss = 0).
c          note..  a preliminary CALL with tout = t is not counted
c          as a first CALL here, as no initialization or checking of
c          input is done.  (such a CALL is sometimes useful for the
c          purpose of outputting the initial conditions.)
c          thus the first CALL for which tout .ne. t requires
c          istate = 1 on input.
c
c          on output, istate has the following values and meanings.
c           1  means nothing was done, as tout was equal to t with
c              istate = 1 on input.  (however, an internal counter was
c              set to detect and prevent repeated calls of this type.)
c           2  means the integration was performed successfully.
c          -1  means an excessive amount of work (more than mxstep
c              steps) was done on this call, before completing the
c              requested task, but the integration was otherwise
c              successful as far as t.  (mxstep is an optional input
c              and is normally 500.)  to continue, the user may
c              simply reset istate to a value .gt. 1 and CALL again
c              (the excess work step counter will be reset to 0).
c              in addition, the user may increase mxstep to avoid
c              this error return (see below on optional inputs).
c          -2  means too much accuracy was requested for the precision
c              of the machine being used.  this was detected before
c              completing the requested task, but the integration
c              was successful as far as t.  to continue, the tolerance
c              parameters must be reset, and istate must be set
c              to 3.  the optional output tolsf may be used for this
c              purpose.  (note.. if this condition is detected before
c              taking any steps, then an illegal input return
c              (istate = -3) occurs instead.)
c          -3  means illegal input was detected, before taking any
c              integration steps.  see written message for details.
c              note..  if the solver detects an infinite loop of calls
c              to the solver with illegal input, it will cause
c              the run to stop.
c          -4  means there were repeated error test failures on
c              one attempted step, before completing the requested
c              task, but the integration was successful as far as t.
c              the problem may have a singularity, or the input
c              may be inappropriate.
c          -5  means there were repeated convergence test failures on
c              one attempted step, before completing the requested
c              task, but the integration was successful as far as t.
c              this may be caused by an inaccurate jacobian matrix,
c              if one is being used.
c          -6  means ewt(i) became zero for some i during the
c              integration.  pure relative error control (AbsTol(i)=0.0)
c              was requested on a variable which has now vanished.
c              the integration was successful as far as t.
c          -7  means a fatal error return flag came from the sparse
c              solver cdrv by way of prjs or slss (numerical
c              factorization or backsolve).  this should never happen.
c              the integration was successful as far as t.
c
c          note.. an error return with istate = -1, -4, or -5 and with
c          miter = 1 or 2 may mean that the sparsity structure of the
c          problem has changed significantly since it was last
c          determined (or input).  in that case, one can attempt to
c          complete the integration by setting istate = 3 on the next
c          call, so that a new structure determination is done.
c
c          note..  since the normal output value of istate is 2,
c          it does not need to be reset for normal continuation.
c          also, since a negative input value of istate will be
c          regarded as illegal, a negative output value requires the
c          user to change it, and possibly other inputs, before
c          calling the solver again.
c
c iopt   = an integer flag to specify whether or not any optional
c          inputs are being used on this call.  input only.
c          the optional inputs are listed separately below.
c          iopt = 0 means no optional inputs are being used.
c                   default values will be used in all cases.
c          iopt = 1 means one or more optional inputs are being used.
c
c rwork  = a work array used for a mixture of real (KPP_REAL)
c          and integer work space.
c          the length of rwork (in real words) must be at least
c             20 + nyh*(maxord + 1) + 3*neq + lwm    where
c          nyh    = the initial value of neq,
c          maxord = 12 (if meth = 1) or 5 (if meth = 2) (unless a
c                   smaller value is given as an optional input),
c          lwm = 0                                    if miter = 0,
c          lwm = 2*nnz + 2*neq + (nnz+9*neq)/lenrat   if miter = 1,
c          lwm = 2*nnz + 2*neq + (nnz+10*neq)/lenrat  if miter = 2,
c          lwm = neq + 2                              if miter = 3.
c          in the above formulas,
c          nnz    = number of nonzero elements in the jacobian matrix.
c          lenrat = the real to integer wordlength ratio (usually 1 in
c                   single precision and 2 in KPP_REAL).
c          (see the mf description for meth and miter.)
c          thus if maxord has its default value and neq is constant,
c          the minimum length of rwork is..
c             20 + 16*neq        for mf = 10,
c             20 + 16*neq + lwm  for mf = 11, 111, 211, 12, 112, 212,
c             22 + 17*neq        for mf = 13,
c             20 +  9*neq        for mf = 20,
c             20 +  9*neq + lwm  for mf = 21, 121, 221, 22, 122, 222,
c             22 + 10*neq        for mf = 23.
c          if miter = 1 or 2, the above formula for lwm is only a
c          crude lower bound.  the required length of rwork cannot
c          be readily predicted in general, as it depends on the
c          sparsity structure of the problem.  some experimentation
c          may be necessary.
c
c          the first 20 words of rwork are reserved for conditional
c          and optional inputs and optional outputs.
c
c          the following word in rwork is a conditional input..
c            rwork(1) = tcrit = critical value of t which the solver
c                       is not to overshoot.  required if itask is
c                       4 or 5, and ignored otherwise.  (see itask.)
c
c lrw    = the length of the array rwork, as declared by the user.
c          (this will be checked by the solver.)
c
c iwork  = an integer work array.  the length of iwork must be at least
c             31 + neq + nnz   if moss = 0 and miter = 1 or 2, or
c             30               otherwise.
c          (nnz is the number of nonzero elements in df/dy.)
c
c          in lsodes, iwork is used only for conditional and
c          optional inputs and optional outputs.
c
c          the following two blocks of words in iwork are conditional
c          inputs, required if moss = 0 and miter = 1 or 2, but not
c          otherwise (see the description of mf for moss).
c            iwork(30+j) = ia(j)     (j=1,...,neq+1)
c            iwork(31+neq+k) = ja(k) (k=1,...,nnz)
c          the two arrays ia and ja describe the sparsity structure
c          to be assumed for the jacobian matrix.  ja contains the row
c          indices where nonzero elements occur, reading in columnwise
c          order, and ia contains the starting locations in ja of the
c          descriptions of columns 1,...,neq, in that order, with
c          ia(1) = 1.  thus, for each column index j = 1,...,neq, the
c          values of the row index i in column j where a nonzero
c          element may occur are given by
c            i = ja(k),  where   ia(j) .le. k .lt. ia(j+1).
c          if nnz is the total number of nonzero locations assumed,
c          then the length of the ja array is nnz, and ia(neq+1) must
c          be nnz + 1.  duplicate entries are not allowed.
c
c liw    = the length of the array iwork, as declared by the user.
c          (this will be checked by the solver.)
c
c note..  the work arrays must not be altered between calls to lsodes
c for the same problem, except possibly for the conditional and
c optional inputs, and except for the last 3*neq words of rwork.
c the latter space is used for internal scratch space, and so is
c available for use by the user outside lsodes between calls, if
c desired (but not for use by f or jac).
c
c jac    = name of user-supplied routine (miter = 1 or moss = 1) to
c          compute the jacobian matrix, df/dy, as a function of
c          the scalar t and the vector y.  it is to have the form
c               subroutine jac (neq, t, y, j, ian, jan, pdj)
c               dimension y(1), ian(1), jan(1), pdj(1)
c          where neq, t, y, j, ian, and jan are input, and the array
c          pdj, of length neq, is to be loaded with column j
c          of the jacobian on output.  thus df(i)/dy(j) is to be
c          loaded into pdj(i) for all relevant values of i.
c          here t and y have the same meaning as in subroutine f,
c          and j is a column index (1 to neq).  ian and jan are
c          undefined in calls to jac for structure determination
c          (moss = 1).  otherwise, ian and jan are structure
c          descriptors, as defined under optional outputs below, and
c          so can be used to determine the relevant row indices i, if
c          desired.  (in the dimension statement above, 1 is a
c          dummy dimension.. it can be replaced by any value.)
c               jac need not provide df/dy exactly.  a crude
c          approximation (possibly with greater sparsity) will do.
c               in any case, pdj is preset to zero by the solver,
c          so that only the nonzero elements need be loaded by jac.
c          calls to jac are made with j = 1,...,neq, in that order, and
c          each such set of calls is preceded by a CALL to f with the
c          same arguments neq, t, and y.  thus to gain some efficiency,
c          intermediate quantities shared by both calculations may be
c          saved in a user common block by f and not recomputed by jac,
c          if desired.  jac must not alter its input arguments.
c          jac must be declared external in the calling program.
c               subroutine jac may access user-defined quantities in
c          neq(2),... and y(neq(1)+1),... if neq is an array
c          (dimensioned in jac) and y has length exceeding neq(1).
c          see the descriptions of neq and y above.
c
c mf     = the method flag.  used only for input.
c          mf has three decimal digits-- moss, meth, miter--
c             mf = 100*moss + 10*meth + miter.
c          moss indicates the method to be used to obtain the sparsity
c          structure of the jacobian matrix if miter = 1 or 2..
c            moss = 0 means the user has supplied ia and ja
c                     (see descriptions under iwork above).
c            moss = 1 means the user has supplied jac (see below)
c                     and the structure will be obtained from neq
c                     initial calls to jac.
c            moss = 2 means the structure will be obtained from neq+1
c                     initial calls to f.
c          meth indicates the basic linear multistep method..
c            meth = 1 means the implicit adams method.
c            meth = 2 means the method based on backward
c                     differentiation formulas (bdf-s).
c          miter indicates the corrector iteration method..
c            miter = 0 means functional iteration (no jacobian matrix
c                      is involved).
c            miter = 1 means chord iteration with a user-supplied
c                      sparse jacobian, given by subroutine jac.
c            miter = 2 means chord iteration with an internally
c                      generated (difference quotient) sparse jacobian
c                      (using ngp extra calls to f per df/dy value,
c                      where ngp is an optional output described below.)
c            miter = 3 means chord iteration with an internally
c                      generated diagonal jacobian approximation.
c                      (using 1 extra CALL to f per df/dy evaluation).
c          if miter = 1 or moss = 1, the user must supply a subroutine
c          jac (the name is arbitrary) as described above under jac.
c          otherwise, a dummy argument can be used.
c
c          the standard choices for mf are..
c            mf = 10  for a nonstiff problem,
c            mf = 21 or 22 for a stiff problem with ia/ja supplied
c                     (21 if jac is supplied, 22 if not),
c            mf = 121 for a stiff problem with jac supplied,
c                     but not ia/ja,
c            mf = 222 for a stiff problem with neither ia/ja nor
c                     jac supplied.
c          the sparseness structure can be changed during the
c          problem by making a CALL to lsodes with istate = 3.
c-----------------------------------------------------------------------
c optional inputs.
c
c the following is a list of the optional inputs provided for in the
c CALL sequence.  (see also part ii.)  for each such input variable,
c this table lists its name as used in this documentation, its
c location in the CALL sequence, its meaning, and the default value.
c the use of any of these inputs requires iopt = 1, and in that
c case all of these inputs are examined.  a value of zero for any
c of these optional inputs will cause the default value to be used.
c thus to use a subset of the optional inputs, simply preload
c locations 5 to 10 in rwork and iwork to 0.0 and 0 respectively, and
c then set those of interest to nonzero values.
c
c name    location      meaning and default value
c
c h0      rwork(5)  the step size to be attempted on the first step.
c                   the default value is determined by the solver.
c
c hmax    rwork(6)  the maximum absolute step size allowed.
c                   the default value is infinite.
c
c hmin    rwork(7)  the minimum absolute step size allowed.
c                   the default value is 0.  (this lower bound is not
c                   enforced on the final step before reaching tcrit
c                   when itask = 4 or 5.)
c
c seth    rwork(8)  the element threshhold for sparsity determination
c                   when moss = 1 or 2.  if the absolute value of
c                   an estimated jacobian element is .le. seth, it
c                   will be assumed to be absent in the structure.
c                   the default value of seth is 0.
c
c maxord  iwork(5)  the maximum order to be allowed.  the default
c                   value is 12 if meth = 1, and 5 if meth = 2.
c                   if maxord exceeds the default value, it will
c                   be reduced to the default value.
c                   if maxord is changed during the problem, it may
c                   cause the current order to be reduced.
c
c mxstep  iwork(6)  maximum number of (internally defined) steps
c                   allowed during one CALL to the solver.
c                   the default value is 500.
c
c mxhnil  iwork(7)  maximum number of messages printed (per problem)
c                   warning that t + h = t on a step (h = step size).
c                   this must be positive to result in a non-default
c                   value.  the default value is 10.
c-----------------------------------------------------------------------
c optional outputs.
c
c as optional additional output from lsodes, the variables listed
c below are quantities related to the performance of lsodes
c which are available to the user.  these are communicated by way of
c the work arrays, but also have internal mnemonic names as shown.
c except where stated otherwise, all of these outputs are defined
c on any successful return from lsodes, and on any return with
c istate = -1, -2, -4, -5, or -6.  on an illegal input return
c (istate = -3), they will be unchanged from their existing values
c (if any), except possibly for tolsf, lenrw, and leniw.
c on any error return, outputs relevant to the error will be defined,
c as noted below.
c
c name    location      meaning
c
c hu      rwork(11) the step size in t last used (successfully).
c
c hcur    rwork(12) the step size to be attempted on the next step.
c
c tcur    rwork(13) the current value of the independent variable
c                   which the solver has actually reached, i.e. the
c                   current internal mesh point in t.  on output, tcur
c                   will always be at least as far as the argument
c                   t, but may be farther (if interpolation was done).
c
c tolsf   rwork(14) a tolerance scale factor, greater than 1.0,
c                   computed when a request for too much accuracy was
c                   detected (istate = -3 if detected at the start of
c                   the problem, istate = -2 otherwise).  if itol is
c                   left unaltered but RelTol and AbsTol are uniformly
c                   scaled up by a factor of tolsf for the next call,
c                   then the solver is deemed likely to succeed.
c                   (the user may also ignore tolsf and alter the
c                   tolerance parameters in any other way appropriate.)
c
c nst     iwork(11) the number of steps taken for the problem so far.
c
c nfe     iwork(12) the number of f evaluations for the problem so far,
c                   excluding those for structure determination
c                   (moss = 2).
c
c nje     iwork(13) the number of jacobian evaluations for the problem
c                   so far, excluding those for structure determination
c                   (moss = 1).
c
c nqu     iwork(14) the method order last used (successfully).
c
c nqcur   iwork(15) the order to be attempted on the next step.
c
c imxer   iwork(16) the index of the component of largest magnitude in
c                   the weighted local error vector ( e(i)/ewt(i) ),
c                   on an error return with istate = -4 or -5.
c
c lenrw   iwork(17) the length of rwork actually required.
c                   this is defined on normal returns and on an illegal
c                   input return for insufficient storage.
c
c leniw   iwork(18) the length of iwork actually required.
c                   this is defined on normal returns and on an illegal
c                   input return for insufficient storage.
c
c nnz     iwork(19) the number of nonzero elements in the jacobian
c                   matrix, including the diagonal (miter = 1 or 2).
c                   (this may differ from that given by ia(neq+1)-1
c                   if moss = 0, because of added diagonal entries.)
c
c ngp     iwork(20) the number of groups of column indices, used in
c                   difference quotient jacobian aproximations if
c                   miter = 2.  this is also the number of extra f
c                   evaluations needed for each jacobian evaluation.
c
c nlu     iwork(21) the number of sparse lu decompositions for the
c                   problem so far.
c
c lyh     iwork(22) the base address in rwork of the history array yh,
c                   described below in this list.
c
c ipian   iwork(23) the base address of the structure descriptor array
c                   ian, described below in this list.
c
c ipjan   iwork(24) the base address of the structure descriptor array
c                   jan, described below in this list.
c
c nzl     iwork(25) the number of nonzero elements in the strict lower
c                   triangle of the lu factorization used in the chord
c                   iteration (miter = 1 or 2).
c
c nzu     iwork(26) the number of nonzero elements in the strict upper
c                   triangle of the lu factorization used in the chord
c                   iteration (miter = 1 or 2).
c                   the total number of nonzeros in the factorization
c                   is therefore nzl + nzu + neq.
c
c the following four arrays are segments of the rwork array which
c may also be of interest to the user as optional outputs.
c for each array, the table below gives its internal name,
c its base address, and its description.
c for yh and acor, the base addresses are in rwork (a real array).
c the integer arrays ian and jan are to be obtained by declaring an
c integer array iwk and identifying iwk(1) with rwork(21), using either
c an equivalence statement or a subroutine call.  then the base
c addresses ipian (of ian) and ipjan (of jan) in iwk are to be obtained
c as optional outputs iwork(23) and iwork(24), respectively.
c thus ian(1) is iwk(ipian), etc.
c
c name    base address      description
c
c ian    ipian (in iwk)  structure descriptor array of size neq + 1.
c jan    ipjan (in iwk)  structure descriptor array of size nnz.
c         (see above)    ian and jan together describe the sparsity
c                        structure of the jacobian matrix, as used by
c                        lsodes when miter = 1 or 2.
c                        jan contains the row indices of the nonzero
c                        locations, reading in columnwise order, and
c                        ian contains the starting locations in jan of
c                        the descriptions of columns 1,...,neq, in
c                        that order, with ian(1) = 1.  thus for each
c                        j = 1,...,neq, the row indices i of the
c                        nonzero locations in column j are
c                        i = jan(k),  ian(j) .le. k .lt. ian(j+1).
c                        note that ian(neq+1) = nnz + 1.
c                        (if moss = 0, ian/jan may differ from the
c                        input ia/ja because of a different ordering
c                        in each column, and added diagonal entries.)
c
c yh      lyh            the nordsieck history array, of size nyh by
c          (optional     (nqcur + 1), where nyh is the initial value
c          output)       of neq.  for j = 0,1,...,nqcur, column j+1
c                        of yh contains hcur**j/factorial(j) times
c                        the j-th derivative of the interpolating
c                        polynomial currently representing the solution,
c                        evaluated at t = tcur.  the base address lyh
c                        is another optional output, listed above.
c
c acor     lenrw-neq+1   array of size neq used for the accumulated
c                        corrections on each step, scaled on output
c                        to represent the estimated local error in y
c                        on the last step.  this is the vector e in
c                        the description of the error control.  it is
c                        defined only on a successful return from
c                        lsodes.
c
c-----------------------------------------------------------------------
c part ii.  other routines callable.
c
c the following are optional calls which the user may make to
c gain additional capabilities in conjunction with lsodes.
c (the routines xsetun and xsetf are designed to conform to the
c slatec error handling package.)
c
c     form of CALL                  function
c   CALL xsetun(lun)          set the logical unit number, lun, for
c                             output of messages from lsodes, if
c                             the default is not desired.
c                             the default value of lun is 6.
c
c   CALL xsetf(mflag)         set a flag to control the printing of
c                             messages by lsodes.
c                             mflag = 0 means do not print. (danger..
c                             this risks losing valuable information.)
c                             mflag = 1 means print (the default).
c
c                             either of the above calls may be made at
c                             any time and will take effect immediately.
c
c   CALL srcms(rsav,isav,job) saves and restores the contents of
c                             the internal common blocks used by
c                             lsodes (see part iii below).
c                             rsav must be a real array of length 224
c                             or more, and isav must be an integer
c                             array of length 75 or more.
c                             job=1 means save common into rsav/isav.
c                             job=2 means restore common from rsav/isav.
c                                srcms is useful if one is
c                             interrupting a run and restarting
c                             later, or alternating between two or
c                             more problems solved with lsodes.
c
c   CALL intdy(,,,,,)         provide derivatives of y, of various
c        (see below)          orders, at a specified point t, if
c                             desired.  it may be called only after
c                             a successful return from lsodes.
c
c the detailed instructions for using intdy are as follows.
c the form of the CALL is..
c
c   lyh = iwork(22)
c   CALL intdy (t, k, rwork(lyh), nyh, dky, iflag)
c
c the input parameters are..
c
c t         = value of independent variable where answers are desired
c             (normally the same as the t last returned by lsodes).
c             for valid results, t must lie between tcur - hu and tcur.
c             (see optional outputs for tcur and hu.)
c k         = integer order of the derivative desired.  k must satisfy
c             0 .le. k .le. nqcur, where nqcur is the current order
c             (see optional outputs).  the capability corresponding
c             to k = 0, i.e. computing y(t), is already provided
c             by lsodes directly.  since nqcur .ge. 1, the first
c             derivative dy/dt is always available with intdy.
c lyh       = the base address of the history array yh, obtained
c             as an optional output as shown above.
c nyh       = column length of yh, equal to the initial value of neq.
c
c the output parameters are..
c
c dky       = a real array of length neq containing the computed value
c             of the k-th derivative of y(t).
c iflag     = integer flag, returned as 0 if k and t were legal,
c             -1 if k was illegal, and -2 if t was illegal.
c             on an error return, a message is also written.
c-----------------------------------------------------------------------
c part iii.  common blocks.
c
c if lsodes is to be used in an overlay situation, the user
c must declare, in the primary overlay, the variables in..
c   (1) the CALL sequence to lsodes,
c   (2) the three internal common blocks
c         /ls0001/  of length  257  (218 KPP_REAL words
c                         followed by 39 integer words),
c         /lss001/  of length  40    ( 6 KPP_REAL words
c                         followed by 34 integer words),
c         /eh0001/  of length  2 (integer words).
c
c if lsodes is used on a system in which the contents of internal
c common blocks are not preserved between calls, the user should
c declare the above three common blocks in his main program to insure
c that their contents are preserved.
c
c if the solution of a given problem by lsodes is to be interrupted
c and then later continued, such as when restarting an interrupted run
c or alternating between two or more problems, the user should save,
c following the return from the last lsodes CALL prior to the
c interruption, the contents of the CALL sequence variables and the
c internal common blocks, and later restore these values before the
c next lsodes CALL for that problem.  to save and restore the common
c blocks, use subroutine srcms (see part ii above).
c
c-----------------------------------------------------------------------
c part iv.  optionally replaceable solver routines.
c
c below are descriptions of two routines in the lsodes package which
c relate to the measurement of errors.  either routine can be
c replaced by a user-supplied version, if desired.  however, since such
c a replacement may have a major impact on performance, it should be
c done only when absolutely necessary, and only with great caution.
c (note.. the means by which the package version of a routine is
c superseded by the user-s version may be system-dependent.)
c
c (a) ewset.
c the following subroutine is called just before each internal
c integration step, and sets the array of error weights, ewt, as
c described under itol/RelTol/AbsTol above..
c     subroutine ewset (neq, itol, RelTol, AbsTol, ycur, ewt)
c where neq, itol, RelTol, and AbsTol are as in the lsodes CALL sequence,
c ycur contains the current dependent variable vector, and
c ewt is the array of weights set by ewset.
c
c if the user supplies this subroutine, it must return in ewt(i)
c (i = 1,...,neq) a positive quantity suitable for comparing errors
c in y(i) to.  the ewt array returned by ewset is passed to the
c vnorm routine (see below), and also used by lsodes in the computation
c of the optional output imxer, the diagonal jacobian approximation,
c and the increments for difference quotient jacobians.
c
c in the user-supplied version of ewset, it may be desirable to use
c the current values of derivatives of y.  derivatives up to order nq
c are available from the history array yh, described above under
c optional outputs.  in ewset, yh is identical to the ycur array,
c extended to nq + 1 columns with a column length of nyh and scale
c factors of h**j/factorial(j).  on the first CALL for the problem,
c given by nst = 0, nq is 1 and h is temporarily set to 1.0.
c the quantities nq, nyh, h, and nst can be obtained by including
c in ewset the statements..
c     KPP_REAL h, rls
c     common /ls0001/ rls(218),ils(39)
c     nq = ils(35)
c     nyh = ils(14)
c     nst = ils(36)
c     h = rls(212)
c thus, for example, the current value of dy/dt can be obtained as
c ycur(nyh+i)/h  (i=1,...,neq)  (and the division by h is
c unnecessary when nst = 0).
c
c (b) vnorm.
c the following is a real function routine which computes the weighted
c root-mean-square norm of a vector v..
c     d = vnorm (n, v, w)
c where..
c   n = the length of the vector,
c   v = real array of length n containing the vector,
c   w = real array of length n containing weights,
c   d = sqrt( (1/n) * sum(v(i)*w(i))**2 ).
c vnorm is called with n = neq and with w(i) = 1.0/ewt(i), where
c ewt is as set by subroutine ewset.
c
c if the user supplies this function, it should return a non-negative
c value of vnorm suitable for use in the error control in lsodes.
c none of the arguments should be altered by vnorm.
c for example, a user-supplied vnorm routine might..
c   -substitute a max-norm of (v(i)*w(i)) for the rms-norm, or
c   -ignore some components of v in the norm, with the effect of
c    suppressing the error control on those components of y.
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c other routines in the lsodes package.
c
c in addition to subroutine lsodes, the lsodes package includes the
c following subroutines and function routines..
c  iprep    acts as an iterface between lsodes and prep, and also does
c           adjusting of work space pointers and work arrays.
c  prep     is called by iprep to compute sparsity and do sparse matrix
c           preprocessing if miter = 1 or 2.
c  jgroup   is called by prep to compute groups of jacobian column
c           indices for use when miter = 2.
c  adjlr    adjusts the length of required sparse matrix work space.
c           it is called by prep.
c  cntnzu   is called by prep and counts the nonzero elements in the
c           strict upper triangle of j + j-transpose, where j = df/dy.
c  intdy    computes an interpolated value of the y vector at t = tout.
c  stode    is the core integrator, which does one step of the
c           integration and the associated error control.
c  cfode    sets all method coefficients and test constants.
c  prjs     computes and preprocesses the jacobian matrix j = df/dy
c           and the newton iteration matrix p = i - h*l0*j.
c  slss     manages solution of linear system in chord iteration.
c  ewset    sets the error weight vector ewt before each step.
c  vnorm    computes the weighted r.m.s. norm of a vector.
c  srcms    is a user-callable routine to save and restore
c           the contents of the internal common blocks.
c  odrv     constructs a reordering of the rows and columns of
c           a matrix by the minimum degree algorithm.  odrv is a
c           driver routine which calls subroutines md, mdi, mdm,
c           mdp, mdu, and sro.  see ref. 2 for details.  (the odrv
c           module has been modified since ref. 2, however.)
c  cdrv     performs reordering, symbolic factorization, numerical
c           factorization, or linear system solution operations,
c           depending on a path argument ipath.  cdrv is a
c           driver routine which calls subroutines nroc, nsfc,
c           nnfc, nnsc, and nntc.  see ref. 3 for details.
c           lsodes uses cdrv to solve linear systems in which the
c           coefficient matrix is  p = i - con*j, where i is the
c           identity, con is a scalar, and j is an approximation to
c           the jacobian df/dy.  because cdrv deals with rowwise
c           sparsity descriptions, cdrv works with p-transpose, not p.
c  d1mach   computes the unit roundoff in a machine-independent manner.
c  xerrwv, xsetun, and xsetf   handle the printing of all error
c           messages and warnings.  xerrwv is machine-dependent.
c note..  vnorm and d1mach are function routines.
c all the others are subroutines.
c
c the intrinsic and external routines used by lsodes are..
c dabs, DMAX1, dmin1, dfloat, max0, min0, mod, DSIGN, DSQRT, and write.
c
c a block data subprogram is also included with the package,
c for loading some of the variables in internal common.
c
c-----------------------------------------------------------------------
c the following card is for optimized compilation on lll compilers.
clll. optimize
c-----------------------------------------------------------------------
      external prjs, slss
      integer illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,
     1   mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns
      integer icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     1   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     1   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     2   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     3   nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, i1, i2, iflag, imax, imul, imxer, ipflag, ipgo, irem,
     1   j, kgo, lenrat, lenyht, leniw, lenrw, lf0, lia, lja,
     2   lrtem, lwtem, lyhd, lyhn, mf1, mord, mxhnl0, mxstp0, ncolm
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL con0, conmin, ccmxj, psmall, rbig, seth
      KPP_REAL AbsToli, ayi, big, ewti, h0, hmax, hmx, rh, RelToli,
     1   tcrit, tdist, tnext, tol, tolsf, tp, size, sum, w0,
     2   d1mach, vnorm
      dimension mord(2)
      logical ihit
c-----------------------------------------------------------------------
c the following two internal common blocks contain
c (a) variables which are local to any subroutine but whose values must
c     be preserved between calls to the routine (own variables), and
c (b) variables which are communicated between subroutines.
c the structure of each block is as follows..  all real variables are
c listed first, followed by all integers.  within each type, the
c variables are grouped with those local to subroutine lsodes first,
c then those local to subroutine stode or subroutine prjs
c (no other routines have own variables), and finally those used
c for communication.  the block ls0001 is declared in subroutines
c lsodes, iprep, prep, intdy, stode, prjs, and slss.  the block lss001
c is declared in subroutines lsodes, iprep, prep, prjs, and slss.
c groups of variables are replaced by dummy arrays in the common
c declarations in routines where those variables are not used.
c-----------------------------------------------------------------------
      common /ls0001/ rowns(209),
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     2   illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,
     3   mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
c
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,
     1   iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     2   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     3   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     4   nslj, ngp, nlu, nnz, nsp, nzl, nzu
c
      data mord(1),mord(2)/12,5/, mxstp0/500/, mxhnl0/10/
c-----------------------------------------------------------------------
c in the data statement below, set lenrat equal to the ratio of
c the wordlength for a real number to that for an integer.  usually,
c lenrat = 1 for single precision and 2 for KPP_REAL.  if the
c true ratio is not an integer, use the next smaller integer (.ge. 1).
c-----------------------------------------------------------------------
      data lenrat/2/
c-----------------------------------------------------------------------
c block a.
c this code block is executed on every call.
c it tests istate and itask for legality and branches appropriately.
c if istate .gt. 1 but the flag init shows that initialization has
c not yet been done, an error return occurs.
c if istate = 1 and tout = t, jump to block g and return immediately.
c-----------------------------------------------------------------------
      if (istate .lt. 1 .or. istate .gt. 3) go to 601
      if (itask .lt. 1 .or. itask .gt. 5) go to 602
      if (istate .eq. 1) go to 10
      if (init .eq. 0) go to 603
      if (istate .eq. 2) go to 200
      go to 20
 10   init = 0
      if (tout .eq. t) go to 430
 20   ntrep = 0
c-----------------------------------------------------------------------
c block b.
c the next code block is executed for the initial CALL (istate = 1),
c or for a continuation CALL with parameter changes (istate = 3).
c it contains checking of all inputs and various initializations.
c if istate = 1, the final setting of work space pointers, the matrix
c preprocessing, and other initializations are done in block c.
c
c first check legality of the non-optional inputs neq, itol, iopt,
c mf, ml, and mu.
c-----------------------------------------------------------------------
      if (neq(1) .le. 0) go to 604
      if (istate .eq. 1) go to 25
      if (neq(1) .gt. n) go to 605
 25   n = neq(1)
      if (itol .lt. 1 .or. itol .gt. 4) go to 606
      if (iopt .lt. 0 .or. iopt .gt. 1) go to 607
      moss = mf/100
      mf1 = mf - 100*moss
      meth = mf1/10
      miter = mf1 - 10*meth
      if (moss .lt. 0 .or. moss .gt. 2) go to 608
      if (meth .lt. 1 .or. meth .gt. 2) go to 608
      if (miter .lt. 0 .or. miter .gt. 3) go to 608
      if (miter .eq. 0 .or. miter .eq. 3) moss = 0
c next process and check the optional inputs. --------------------------
      if (iopt .eq. 1) go to 40
      maxord = mord(meth)
      mxstep = mxstp0
      mxhnil = mxhnl0
      if (istate .eq. 1) h0 = 0.0d0
      hmxi = 0.0d0
      hmin = 0.0d0
      seth = 0.0d0
      go to 60
 40   maxord = iwork(5)
      if (maxord .lt. 0) go to 611
      if (maxord .eq. 0) maxord = 100
      maxord = min0(maxord,mord(meth))
      mxstep = iwork(6)
      if (mxstep .lt. 0) go to 612
      if (mxstep .eq. 0) mxstep = mxstp0
      mxhnil = iwork(7)
      if (mxhnil .lt. 0) go to 613
      if (mxhnil .eq. 0) mxhnil = mxhnl0
      if (istate .ne. 1) go to 50
      h0 = rwork(5)
      if ((tout - t)*h0 .lt. 0.0d0) go to 614
 50   hmax = rwork(6)
      if (hmax .lt. 0.0d0) go to 615
      hmxi = 0.0d0
      if (hmax .gt. 0.0d0) hmxi = 1.0d0/hmax
      hmin = rwork(7)
      if (hmin .lt. 0.0d0) go to 616
      seth = rwork(8)
      if (seth .lt. 0.0d0) go to 609
c check RelTol and AbsTol for legality. ------------------------------------
 60   RelToli = RelTol(1)
      AbsToli = AbsTol(1)
      do 65 i = 1,n
        if (itol .ge. 3) RelToli = RelTol(i)
        if (itol .eq. 2 .or. itol .eq. 4) AbsToli = AbsTol(i)
        if (RelToli .lt. 0.0d0) go to 619
        if (AbsToli .lt. 0.0d0) go to 620
 65     continue
c-----------------------------------------------------------------------
c compute required work array lengths, as far as possible, and test
c these against lrw and liw.  then set tentative pointers for work
c arrays.  pointers to rwork/iwork segments are named by prefixing l to
c the name of the segment.  e.g., the segment yh starts at rwork(lyh).
c segments of rwork (in order) are denoted  wm, yh, savf, ewt, acor.
c if miter = 1 or 2, the required length of the matrix work space wm
c is not yet known, and so a crude minimum value is used for the
c initial tests of lrw and liw, and yh is temporarily stored as far
c to the right in rwork as possible, to leave the maximum amount
c of space for wm for matrix preprocessing.  thus if miter = 1 or 2
c and moss .ne. 2, some of the segments of rwork are temporarily
c omitted, as they are not needed in the preprocessing.  these
c omitted segments are.. acor if istate = 1, ewt and acor if istate = 3
c and moss = 1, and savf, ewt, and acor if istate = 3 and moss = 0.
c-----------------------------------------------------------------------
      lrat = lenrat
      if (istate .eq. 1) nyh = n
      lwmin = 0
      if (miter .eq. 1) lwmin = 4*n + 10*n/lrat
      if (miter .eq. 2) lwmin = 4*n + 11*n/lrat
      if (miter .eq. 3) lwmin = n + 2
      lenyh = (maxord+1)*nyh
      lrest = lenyh + 3*n
      lenrw = 20 + lwmin + lrest
      iwork(17) = lenrw
      leniw = 30
      if (moss .eq. 0 .and. miter .ne. 0 .and. miter .ne. 3)
     1   leniw = leniw + n + 1
      iwork(18) = leniw
      if (lenrw .gt. lrw) go to 617
      if (leniw .gt. liw) go to 618
      lia = 31
      if (moss .eq. 0 .and. miter .ne. 0 .and. miter .ne. 3)
     1   leniw = leniw + iwork(lia+n) - 1
      iwork(18) = leniw
      if (leniw .gt. liw) go to 618
      lja = lia + n + 1
      lia = min0(lia,liw)
      lja = min0(lja,liw)
      lwm = 21
      if (istate .eq. 1) nq = 1
      ncolm = min0(nq+1,maxord+2)
      lenyhm = ncolm*nyh
      lenyht = lenyh
      if (miter .eq. 1 .or. miter .eq. 2) lenyht = lenyhm
      imul = 2
      if (istate .eq. 3) imul = moss
      if (moss .eq. 2) imul = 3
      lrtem = lenyht + imul*n
      lwtem = lwmin
      if (miter .eq. 1 .or. miter .eq. 2) lwtem = lrw - 20 - lrtem
      lenwk = lwtem
      lyhn = lwm + lwtem
      lsavf = lyhn + lenyht
      lewt = lsavf + n
      lacor = lewt + n
      istatc = istate
      if (istate .eq. 1) go to 100
c-----------------------------------------------------------------------
c istate = 3.  move yh to its new location.
c note that only the part of yh needed for the next step, namely
c min(nq+1,maxord+2) columns, is actually moved.
c a temporary error weight array ewt is loaded if moss = 2.
c sparse matrix processing is done in iprep/prep if miter = 1 or 2.
c if maxord was reduced below nq, then the pointers are finally set
c so that savf is identical to yh(*,maxord+2).
c-----------------------------------------------------------------------
      lyhd = lyh - lyhn
      imax = lyhn - 1 + lenyhm
c move yh.  branch for move right, no move, or move left. --------------
      if (lyhd) 70,80,74
 70   do 72 i = lyhn,imax
        j = imax + lyhn - i
 72     rwork(j) = rwork(j+lyhd)
      go to 80
 74   do 76 i = lyhn,imax
 76     rwork(i) = rwork(i+lyhd)
 80   lyh = lyhn
      iwork(22) = lyh
      if (miter .eq. 0 .or. miter .eq. 3) go to 92
      if (moss .ne. 2) go to 85
c temporarily load ewt if miter = 1 or 2 and moss = 2. -----------------
      CALL ewset (n, itol, RelTol, AbsTol, rwork(lyh), rwork(lewt))
      do 82 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0d0) go to 621
 82     rwork(i+lewt-1) = 1.0d0/rwork(i+lewt-1)
 85   continue
c iprep and prep do sparse matrix preprocessing if miter = 1 or 2. -----
      lsavf = min0(lsavf,lrw)
      lewt = min0(lewt,lrw)
      lacor = min0(lacor,lrw)
      CALL iprep (neq, y, rwork, iwork(lia), iwork(lja), ipflag, f, jac)
      lenrw = lwm - 1 + lenwk + lrest
      iwork(17) = lenrw
      if (ipflag .ne. -1) iwork(23) = ipian
      if (ipflag .ne. -1) iwork(24) = ipjan
      ipgo = -ipflag + 1
      go to (90, 628, 629, 630, 631, 632, 633), ipgo
 90   iwork(22) = lyh
      if (lenrw .gt. lrw) go to 617
c set flag to signal parameter changes to stode. -----------------------
 92   jstart = -1
      if (n .eq. nyh) go to 200
c neq was reduced.  zero part of yh to avoid undefined references. -----
      i1 = lyh + l*nyh
      i2 = lyh + (maxord + 1)*nyh - 1
      if (i1 .gt. i2) go to 200
      do 95 i = i1,i2
 95     rwork(i) = 0.0d0
      go to 200
c-----------------------------------------------------------------------
c block c.
c the next block is for the initial CALL only (istate = 1).
c it contains all remaining initializations, the initial CALL to f,
c the sparse matrix preprocessing (miter = 1 or 2), and the
c calculation of the initial step size.
c the error weights in ewt are inverted after being loaded.
c-----------------------------------------------------------------------
 100  continue
      lyh = lyhn
      iwork(22) = lyh
      tn = t
      nst = 0
      h = 1.0d0
      nnz = 0
      ngp = 0
      nzl = 0
      nzu = 0
c load the initial value vector in yh. ---------------------------------
      do 105 i = 1,n
 105    rwork(i+lyh-1) = y(i)
c initial CALL to f.  (lf0 points to yh(*,2).) -------------------------
      lf0 = lyh + nyh
      CALL f (neq, t, y, rwork(lf0))
      nfe = 1
c load and invert the ewt array.  (h is temporarily set to 1.0.) -------
      CALL ewset (n, itol, RelTol, AbsTol, rwork(lyh), rwork(lewt))
      do 110 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0d0) go to 621
 110    rwork(i+lewt-1) = 1.0d0/rwork(i+lewt-1)
      if (miter .eq. 0 .or. miter .eq. 3) go to 120
c iprep and prep do sparse matrix preprocessing if miter = 1 or 2. -----
      lacor = min0(lacor,lrw)
      CALL iprep (neq, y, rwork, iwork(lia), iwork(lja), ipflag, f, jac)
      lenrw = lwm - 1 + lenwk + lrest
      iwork(17) = lenrw
      if (ipflag .ne. -1) iwork(23) = ipian
      if (ipflag .ne. -1) iwork(24) = ipjan
      ipgo = -ipflag + 1
      go to (115, 628, 629, 630, 631, 632, 633), ipgo
 115  iwork(22) = lyh
      if (lenrw .gt. lrw) go to 617
c check tcrit for legality (itask = 4 or 5). ---------------------------
 120  continue
      if (itask .ne. 4 .and. itask .ne. 5) go to 125
      tcrit = rwork(1)
      if ((tcrit - tout)*(tout - t) .lt. 0.0d0) go to 625
      if (h0 .ne. 0.0d0 .and. (t + h0 - tcrit)*h0 .gt. 0.0d0)
     1   h0 = tcrit - t
c initialize all remaining parameters. ---------------------------------
 125  uround = d1mach(4)
      jstart = 0
      if (miter .ne. 0) rwork(lwm) = DSQRT(uround)
      msbj = 50
      nslj = 0
      ccmxj = 0.2d0
      psmall = 1000.0d0*uround
      rbig = 0.01d0/psmall
      nhnil = 0
      nje = 0
      nlu = 0
      nslast = 0
      hu = 0.0d0
      nqu = 0
      ccmax = 0.3d0
      maxcor = 3
      msbp = 20
      mxncf = 10
c-----------------------------------------------------------------------
c the coding below computes the step size, h0, to be attempted on the
c first step, unless the user has supplied a value for this.
c first check that tout - t differs significantly from zero.
c a scalar tolerance quantity tol is computed, as max(RelTol(i))
c if this is positive, or max(AbsTol(i)/abs(y(i))) otherwise, adjusted
c so as to be between 100*uround and 1.0e-3.
c then the computed value h0 is given by..
c                                      neq
c   h0**2 = tol / ( w0**-2 + (1/neq) * sum ( f(i)/ywt(i) )**2  )
c                                       1
c where   w0     = max ( abs(t), abs(tout) ),
c         f(i)   = i-th component of initial value of f,
c         ywt(i) = ewt(i)/tol  (a weight for y(i)).
c the sign of h0 is inferred from the initial values of tout and t.
c-----------------------------------------------------------------------
      lf0 = lyh + nyh
      if (h0 .ne. 0.0d0) go to 180
      tdist = dabs(tout - t)
      w0 = DMAX1(dabs(t),dabs(tout))
      if (tdist .lt. 2.0d0*uround*w0) go to 622
      tol = RelTol(1)
      if (itol .le. 2) go to 140
      do 130 i = 1,n
 130    tol = DMAX1(tol,RelTol(i))
 140  if (tol .gt. 0.0d0) go to 160
      AbsToli = AbsTol(1)
      do 150 i = 1,n
        if (itol .eq. 2 .or. itol .eq. 4) AbsToli = AbsTol(i)
        ayi = dabs(y(i))
        if (ayi .ne. 0.0d0) tol = DMAX1(tol,AbsToli/ayi)
 150    continue
 160  tol = DMAX1(tol,100.0d0*uround)
      tol = dmin1(tol,0.001d0)
      sum = vnorm (n, rwork(lf0), rwork(lewt))
      sum = 1.0d0/(tol*w0*w0) + tol*sum**2
      h0 = 1.0d0/DSQRT(sum)
      h0 = dmin1(h0,tdist)
      h0 = DSIGN(h0,tout-t)
c adjust h0 if necessary to meet hmax bound. ---------------------------
 180  rh = dabs(h0)*hmxi
      if (rh .gt. 1.0d0) h0 = h0/rh
c load h with h0 and scale yh(*,2) by h0. ------------------------------
      h = h0
      do 190 i = 1,n
 190    rwork(i+lf0-1) = h0*rwork(i+lf0-1)
      go to 270
c-----------------------------------------------------------------------
c block d.
c the next code block is for continuation calls only (istate = 2 or 3)
c and is to check stop conditions before taking a step.
c-----------------------------------------------------------------------
 200  nslast = nst
      go to (210, 250, 220, 230, 240), itask
 210  if ((tn - tout)*h .lt. 0.0d0) go to 250
      CALL intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 220  tp = tn - hu*(1.0d0 + 100.0d0*uround)
      if ((tp - tout)*h .gt. 0.0d0) go to 623
      if ((tn - tout)*h .lt. 0.0d0) go to 250
      go to 400
 230  tcrit = rwork(1)
      if ((tn - tcrit)*h .gt. 0.0d0) go to 624
      if ((tcrit - tout)*h .lt. 0.0d0) go to 625
      if ((tn - tout)*h .lt. 0.0d0) go to 245
      CALL intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 240  tcrit = rwork(1)
      if ((tn - tcrit)*h .gt. 0.0d0) go to 624
 245  hmx = dabs(tn) + dabs(h)
      ihit = dabs(tn - tcrit) .le. 100.0d0*uround*hmx
      if (ihit) go to 400
      tnext = tn + h*(1.0d0 + 4.0d0*uround)
      if ((tnext - tcrit)*h .le. 0.0d0) go to 250
      h = (tcrit - tn)*(1.0d0 - 4.0d0*uround)
      if (istate .eq. 2) jstart = -2
c-----------------------------------------------------------------------
c block e.
c the next block is normally executed for all calls and contains
c the CALL to the one-step core integrator stode.
c
c this is a looping point for the integration steps.
c
c first check for too many steps being taken, update ewt (if not at
c start of problem), check for too much accuracy being requested, and
c check for h below the roundoff level in t.
c-----------------------------------------------------------------------
 250  continue
      if ((nst-nslast) .ge. mxstep) go to 500
      CALL ewset (n, itol, RelTol, AbsTol, rwork(lyh), rwork(lewt))
      do 260 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0d0) go to 510
 260    rwork(i+lewt-1) = 1.0d0/rwork(i+lewt-1)
 270  tolsf = uround*vnorm (n, rwork(lyh), rwork(lewt))
      if (tolsf .le. 1.0d0) go to 280
      tolsf = tolsf*2.0d0
      if (nst .eq. 0) go to 626
      go to 520
 280  if ((tn + h) .ne. tn) go to 290
      nhnil = nhnil + 1
      if (nhnil .gt. mxhnil) go to 290
      CALL xerrwv(50hlsodes-- warning..internal t (=r1) and h (=r2) are,
     1   50, 101, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h      such that in the machine, t + h = t on the next step  ,
     1   60, 101, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      (h = step size). solver will continue anyway,
     1   50, 101, 0, 0, 0, 0, 2, tn, h)
      if (nhnil .lt. mxhnil) go to 290
      CALL xerrwv(50hlsodes-- above warning has been issued i1 times.  ,
     1   50, 102, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      it will not be issued again for this problem,
     1   50, 102, 0, 1, mxhnil, 0, 0, 0.0d0, 0.0d0)
 290  continue
c-----------------------------------------------------------------------
c    CALL stode(neq,y,yh,nyh,yh,ewt,savf,acor,wm,wm,f,jac,prjs,slss)
c-----------------------------------------------------------------------
      CALL stode (neq, y, rwork(lyh), nyh, rwork(lyh), rwork(lewt),
     1   rwork(lsavf), rwork(lacor), rwork(lwm), rwork(lwm),
     2   f, jac, prjs, slss)
      kgo = 1 - kflag
      go to (300, 530, 540, 550), kgo
c-----------------------------------------------------------------------
c block f.
c the following block handles the case of a successful return from the
c core integrator (kflag = 0).  test for stop conditions.
c-----------------------------------------------------------------------
 300  init = 1
      go to (310, 400, 330, 340, 350), itask
c itask = 1.  if tout has been reached, interpolate. -------------------
 310  if ((tn - tout)*h .lt. 0.0d0) go to 250
      CALL intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      t = tout
      go to 420
c itask = 3.  jump to exit if tout was reached. ------------------------
 330  if ((tn - tout)*h .ge. 0.0d0) go to 400
      go to 250
c itask = 4.  see if tout or tcrit was reached.  adjust h if necessary.
 340  if ((tn - tout)*h .lt. 0.0d0) go to 345
      CALL intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      t = tout
      go to 420
 345  hmx = dabs(tn) + dabs(h)
      ihit = dabs(tn - tcrit) .le. 100.0d0*uround*hmx
      if (ihit) go to 400
      tnext = tn + h*(1.0d0 + 4.0d0*uround)
      if ((tnext - tcrit)*h .le. 0.0d0) go to 250
      h = (tcrit - tn)*(1.0d0 - 4.0d0*uround)
      jstart = -2
      go to 250
c itask = 5.  see if tcrit was reached and jump to exit. ---------------
 350  hmx = dabs(tn) + dabs(h)
      ihit = dabs(tn - tcrit) .le. 100.0d0*uround*hmx
c-----------------------------------------------------------------------
c block g.
c the following block handles all successful returns from lsodes.
c if itask .ne. 1, y is loaded from yh and t is set accordingly.
c istate is set to 2, the illegal input counter is zeroed, and the
c optional outputs are loaded into the work arrays before returning.
c if istate = 1 and tout = t, there is a return with no action taken,
c except that if this has happened repeatedly, the run is terminated.
c-----------------------------------------------------------------------
 400  do 410 i = 1,n
 410    y(i) = rwork(i+lyh-1)
      t = tn
      if (itask .ne. 4 .and. itask .ne. 5) go to 420
      if (ihit) t = tcrit
 420  istate = 2
      illin = 0
      rwork(11) = hu
      rwork(12) = h
      rwork(13) = tn
      iwork(11) = nst
      iwork(12) = nfe
      iwork(13) = nje
      iwork(14) = nqu
      iwork(15) = nq
      iwork(19) = nnz
      iwork(20) = ngp
      iwork(21) = nlu
      iwork(25) = nzl
      iwork(26) = nzu
      return
c
 430  ntrep = ntrep + 1
      if (ntrep .lt. 5) return
      CALL xerrwv(
     1  60hlsodes-- repeated calls with istate = 1 and tout = t (=r1)  ,
     1   60, 301, 0, 0, 0, 0, 1, t, 0.0d0)
      go to 800
c-----------------------------------------------------------------------
c block h.
c the following block handles all unsuccessful returns other than
c those for illegal input.  first the error message routine is called.
c if there was an error test or convergence test failure, imxer is set.
c then y is loaded from yh, t is set to tn, and the illegal input
c counter illin is set to 0.  the optional outputs are loaded into
c the work arrays before returning.
c-----------------------------------------------------------------------
c the maximum number of steps was taken before reaching tout. ----------
 500  CALL xerrwv(50hlsodes-- at current t (=r1), mxstep (=i1) steps   ,
     1   50, 201, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      taken on this CALL before reaching tout     ,
     1   50, 201, 0, 1, mxstep, 0, 1, tn, 0.0d0)
      istate = -1
      go to 580
c ewt(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  ewti = rwork(lewt+i-1)
      CALL xerrwv(50hlsodes-- at t (=r1), ewt(i1) has become r2 .le. 0.,
     1   50, 202, 0, 1, i, 0, 2, tn, ewti)
      istate = -6
      go to 580
c too much accuracy requested for machine precision. -------------------
 520  CALL xerrwv(50hlsodes-- at t (=r1), too much accuracy requested  ,
     1   50, 203, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      for precision of machine..  see tolsf (=r2) ,
     1   50, 203, 0, 0, 0, 0, 2, tn, tolsf)
      rwork(14) = tolsf
      istate = -2
      go to 580
c kflag = -1.  error test failed repeatedly or with abs(h) = hmin. -----
 530  CALL xerrwv(50hlsodes-- at t(=r1) and step size h(=r2), the error,
     1   50, 204, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      test failed repeatedly or with abs(h) = hmin,
     1   50, 204, 0, 0, 0, 0, 2, tn, h)
      istate = -4
      go to 560
c kflag = -2.  convergence failed repeatedly or with abs(h) = hmin. ----
 540  CALL xerrwv(50hlsodes-- at t (=r1) and step size h (=r2), the    ,
     1   50, 205, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      corrector convergence failed repeatedly     ,
     1   50, 205, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(30h      or with abs(h) = hmin   ,
     1   30, 205, 0, 0, 0, 0, 2, tn, h)
      istate = -5
      go to 560
c kflag = -3.  fatal error flag returned by prjs or slss (cdrv). -------
 550  CALL xerrwv(50hlsodes-- at t (=r1) and step size h (=r2), a fatal,
     1   50, 207, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(50h      error flag was returned by cdrv (by way of  ,
     1   50, 207, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(30h      subroutine prjs or slss),
     1   30, 207, 0, 0, 0, 0, 2, tn, h)
      istate = -7
      go to 580
c compute imxer if relevant. -------------------------------------------
 560  big = 0.0d0
      imxer = 1
      do 570 i = 1,n
        size = dabs(rwork(i+lacor-1)*rwork(i+lewt-1))
        if (big .ge. size) go to 570
        big = size
        imxer = i
 570    continue
      iwork(16) = imxer
c set y vector, t, illin, and optional outputs. ------------------------
 580  do 590 i = 1,n
 590    y(i) = rwork(i+lyh-1)
      t = tn
      illin = 0
      rwork(11) = hu
      rwork(12) = h
      rwork(13) = tn
      iwork(11) = nst
      iwork(12) = nfe
      iwork(13) = nje
      iwork(14) = nqu
      iwork(15) = nq
      iwork(19) = nnz
      iwork(20) = ngp
      iwork(21) = nlu
      iwork(25) = nzl
      iwork(26) = nzu
      return
c-----------------------------------------------------------------------
c block i.
c the following block handles all error returns due to illegal input
c (istate = -3), as detected before calling the core integrator.
c first the error message routine is called.  then if there have been
c 5 consecutive such returns just before this CALL to the solver,
c the run is halted.
c-----------------------------------------------------------------------
 601  CALL xerrwv(30hlsodes-- istate (=i1) illegal ,
     1   30, 1, 0, 1, istate, 0, 0, 0.0d0, 0.0d0)
      go to 700
 602  CALL xerrwv(30hlsodes-- itask (=i1) illegal  ,
     1   30, 2, 0, 1, itask, 0, 0, 0.0d0, 0.0d0)
      go to 700
 603  CALL xerrwv(50hlsodes-- istate .gt. 1 but lsodes not initialized ,
     1   50, 3, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      go to 700
 604  CALL xerrwv(30hlsodes-- neq (=i1) .lt. 1     ,
     1   30, 4, 0, 1, neq(1), 0, 0, 0.0d0, 0.0d0)
      go to 700
 605  CALL xerrwv(50hlsodes-- istate = 3 and neq increased (i1 to i2)  ,
     1   50, 5, 0, 2, n, neq(1), 0, 0.0d0, 0.0d0)
      go to 700
 606  CALL xerrwv(30hlsodes-- itol (=i1) illegal   ,
     1   30, 6, 0, 1, itol, 0, 0, 0.0d0, 0.0d0)
      go to 700
 607  CALL xerrwv(30hlsodes-- iopt (=i1) illegal   ,
     1   30, 7, 0, 1, iopt, 0, 0, 0.0d0, 0.0d0)
      go to 700
 608  CALL xerrwv(30hlsodes-- mf (=i1) illegal     ,
     1   30, 8, 0, 1, mf, 0, 0, 0.0d0, 0.0d0)
      go to 700
 609  CALL xerrwv(30hlsodes-- seth (=r1) .lt. 0.0  ,
     1   30, 9, 0, 0, 0, 0, 1, seth, 0.0d0)
      go to 700
 611  CALL xerrwv(30hlsodes-- maxord (=i1) .lt. 0  ,
     1   30, 11, 0, 1, maxord, 0, 0, 0.0d0, 0.0d0)
      go to 700
 612  CALL xerrwv(30hlsodes-- mxstep (=i1) .lt. 0  ,
     1   30, 12, 0, 1, mxstep, 0, 0, 0.0d0, 0.0d0)
      go to 700
 613  CALL xerrwv(30hlsodes-- mxhnil (=i1) .lt. 0  ,
     1   30, 13, 0, 1, mxhnil, 0, 0, 0.0d0, 0.0d0)
      go to 700
 614  CALL xerrwv(40hlsodes-- tout (=r1) behind t (=r2)      ,
     1   40, 14, 0, 0, 0, 0, 2, tout, t)
      CALL xerrwv(50h      integration direction is given by h0 (=r1)  ,
     1   50, 14, 0, 0, 0, 0, 1, h0, 0.0d0)
      go to 700
 615  CALL xerrwv(30hlsodes-- hmax (=r1) .lt. 0.0  ,
     1   30, 15, 0, 0, 0, 0, 1, hmax, 0.0d0)
      go to 700
 616  CALL xerrwv(30hlsodes-- hmin (=r1) .lt. 0.0  ,
     1   30, 16, 0, 0, 0, 0, 1, hmin, 0.0d0)
      go to 700
 617  CALL xerrwv(50hlsodes-- rwork length is insufficient to proceed. ,
     1   50, 17, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. lenrw (=i1), exceeds lrw (=i2),
     1   60, 17, 0, 2, lenrw, lrw, 0, 0.0d0, 0.0d0)
      go to 700
 618  CALL xerrwv(50hlsodes-- iwork length is insufficient to proceed. ,
     1   50, 18, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. leniw (=i1), exceeds liw (=i2),
     1   60, 18, 0, 2, leniw, liw, 0, 0.0d0, 0.0d0)
      go to 700
 619  CALL xerrwv(40hlsodes-- RelTol(i1) is r1 .lt. 0.0        ,
     1   40, 19, 0, 1, i, 0, 1, RelToli, 0.0d0)
      go to 700
 620  CALL xerrwv(40hlsodes-- AbsTol(i1) is r1 .lt. 0.0        ,
     1   40, 20, 0, 1, i, 0, 1, AbsToli, 0.0d0)
      go to 700
 621  ewti = rwork(lewt+i-1)
      CALL xerrwv(40hlsodes-- ewt(i1) is r1 .le. 0.0         ,
     1   40, 21, 0, 1, i, 0, 1, ewti, 0.0d0)
      go to 700
 622  CALL xerrwv(
     1  60hlsodes-- tout (=r1) too close to t(=r2) to start integration,
     1   60, 22, 0, 0, 0, 0, 2, tout, t)
      go to 700
 623  CALL xerrwv(
     1  60hlsodes-- itask = i1 and tout (=r1) behind tcur - hu (= r2)  ,
     1   60, 23, 0, 1, itask, 0, 2, tout, tp)
      go to 700
 624  CALL xerrwv(
     1  60hlsodes-- itask = 4 or 5 and tcrit (=r1) behind tcur (=r2)   ,
     1   60, 24, 0, 0, 0, 0, 2, tcrit, tn)
      go to 700
 625  CALL xerrwv(
     1  60hlsodes-- itask = 4 or 5 and tcrit (=r1) behind tout (=r2)   ,
     1   60, 25, 0, 0, 0, 0, 2, tcrit, tout)
      go to 700
 626  CALL xerrwv(50hlsodes-- at start of problem, too much accuracy   ,
     1   50, 26, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h      requested for precision of machine..  see tolsf (=r1) ,
     1   60, 26, 0, 0, 0, 0, 1, tolsf, 0.0d0)
      rwork(14) = tolsf
      go to 700
 627  CALL xerrwv(50hlsodes-- trouble from intdy. itask = i1, tout = r1,
     1   50, 27, 0, 1, itask, 0, 1, tout, 0.0d0)
      go to 700
 628  CALL xerrwv(
     1  60hlsodes-- rwork length insufficient (for subroutine prep).   ,
     1   60, 28, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. lenrw (=i1), exceeds lrw (=i2),
     1   60, 28, 0, 2, lenrw, lrw, 0, 0.0d0, 0.0d0)
      go to 700
 629  CALL xerrwv(
     1  60hlsodes-- rwork length insufficient (for subroutine jgroup). ,
     1   60, 29, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. lenrw (=i1), exceeds lrw (=i2),
     1   60, 29, 0, 2, lenrw, lrw, 0, 0.0d0, 0.0d0)
      go to 700
 630  CALL xerrwv(
     1  60hlsodes-- rwork length insufficient (for subroutine odrv).   ,
     1   60, 30, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. lenrw (=i1), exceeds lrw (=i2),
     1   60, 30, 0, 2, lenrw, lrw, 0, 0.0d0, 0.0d0)
      go to 700
 631  CALL xerrwv(
     1  60hlsodes-- error from odrv in yale sparse matrix package      ,
     1   60, 31, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      imul = (iys - 1)/n
      irem = iys - imul*n
      CALL xerrwv(
     1  60h      at t (=r1), odrv returned error flag = i1*neq + i2.   ,
     1   60, 31, 0, 2, imul, irem, 1, tn, 0.0d0)
      go to 700
 632  CALL xerrwv(
     1  60hlsodes-- rwork length insufficient (for subroutine cdrv).   ,
     1   60, 32, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      CALL xerrwv(
     1  60h        length needed is .ge. lenrw (=i1), exceeds lrw (=i2),
     1   60, 32, 0, 2, lenrw, lrw, 0, 0.0d0, 0.0d0)
      go to 700
 633  CALL xerrwv(
     1  60hlsodes-- error from cdrv in yale sparse matrix package      ,
     1   60, 33, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      imul = (iys - 1)/n
      irem = iys - imul*n
      CALL xerrwv(
     1  60h      at t (=r1), cdrv returned error flag = i1*neq + i2.   ,
     1   60, 33, 0, 2, imul, irem, 1, tn, 0.0d0)
      if (imul .eq. 2) CALL xerrwv(
     1  60h        duplicate entry in sparsity structure descriptors   ,
     1   60, 33, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
      if (imul .eq. 3 .or. imul .eq. 6) CALL xerrwv(
     1  60h        insufficient storage for nsfc (called by cdrv)      ,
     1   60, 33, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
c
 700  if (illin .eq. 5) go to 710
      illin = illin + 1
      istate = -3
      return
 710  CALL xerrwv(50hlsodes-- repeated occurrences of illegal input    ,
     1   50, 302, 0, 0, 0, 0, 0, 0.0d0, 0.0d0)
c
 800  CALL xerrwv(50hlsodes-- run aborted.. apparent infinite loop     ,
     1   50, 303, 2, 0, 0, 0, 0, 0.0d0, 0.0d0)
      return
c----------------------- end of subroutine lsodes ----------------------
      end
      subroutine iprep (neq, y, rwork, ia, ja, ipflag, f, jac)
clll. optimize
      external f, jac
      integer neq, ia, ja, ipflag
      integer illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,
     1   mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns
      integer icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     1   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     1   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     2   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     3   nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, imax, lewtn, lyhd, lyhn
      KPP_REAL y, rwork
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL rlss
      dimension neq(1), y(1), rwork(1), ia(1), ja(1)
      common /ls0001/ rowns(209),
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     2   illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,
     3   mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ rlss(6),
     1   iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     2   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     3   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     4   nslj, ngp, nlu, nnz, nsp, nzl, nzu
c-----------------------------------------------------------------------
c this routine serves as an interface between the driver and
c subroutine prep.  it is called only if miter is 1 or 2.
c tasks performed here are..
c  * CALL prep,
c  * reset the required wm segment length lenwk,
c  * move yh back to its final location (following wm in rwork),
c  * reset pointers for yh, savf, ewt, and acor, and
c  * move ewt to its new position if istate = 1.
c ipflag is an output error indication flag.  ipflag = 0 if there was
c no trouble, and ipflag is the value of the prep error flag ipper
c if there was trouble in subroutine prep.
c-----------------------------------------------------------------------
      ipflag = 0
c CALL prep to do matrix preprocessing operations. ---------------------
c      CALL prep (neq, y, rwork(lyh), rwork(lsavf), rwork(lewt),
c     1   rwork(lacor), ia, ja, rwork(lwm), rwork(lwm), ipflag, f, jac)
      CALL prep (neq, y, rwork(lyh), rwork(lsavf), rwork(lewt),
     1   rwork(lacor), ia, ja, rwork(lwm), rwork(lwm), ipflag, f, jac)
      lenwk = max0(lreq,lwmin)
      if (ipflag .lt. 0) return
c if prep was successful, move yh to end of required space for wm. -----
      lyhn = lwm + lenwk
      if (lyhn .gt. lyh) return
      lyhd = lyh - lyhn
      if (lyhd .eq. 0) go to 20
      imax = lyhn - 1 + lenyhm
      do 10 i = lyhn,imax
 10     rwork(i) = rwork(i+lyhd)
      lyh = lyhn
c reset pointers for savf, ewt, and acor. ------------------------------
 20   lsavf = lyh + lenyh
      lewtn = lsavf + n
      lacor = lewtn + n
      if (istatc .eq. 3) go to 40
c if istate = 1, move ewt (left) to its new position. ------------------
      if (lewtn .gt. lewt) return
      do 30 i = 1,n
 30     rwork(i+lewtn-1) = rwork(i+lewt-1)
 40   lewt = lewtn
      return
c----------------------- end of subroutine iprep -----------------------
      end
      subroutine slss (wk, iwk, x, tem)
clll. optimize
      integer iwk
      integer iownd, iowns,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     1   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     2   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     3   nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i
      KPP_REAL wk, x, tem
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL rlss
      KPP_REAL di, hl0, phl0, r
      dimension wk(*), iwk(*), x(*), tem(*)
      common /ls0001/ rowns(209),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     3   iownd(14), iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ rlss(6),
     1   iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     2   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     3   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     4   nslj, ngp, nlu, nnz, nsp, nzl, nzu
c-----------------------------------------------------------------------
c this routine manages the solution of the linear system arising from
c a chord iteration.  it is called if miter .ne. 0.
c if miter is 1 or 2, it calls cdrv to accomplish this.
c if miter = 3 it updates the coefficient h*el0 in the diagonal
c matrix, and then computes the solution.
c communication with slss uses the following variables..
c wk    = real work space containing the inverse diagonal matrix if
c         miter = 3 and the lu decomposition of the matrix otherwise.
c         storage of matrix elements starts at wk(3).
c         wk also contains the following matrix-related data..
c         wk(1) = sqrt(uround) (not used here),
c         wk(2) = hl0, the previous value of h*el0, used if miter = 3.
c iwk   = integer work space for matrix-related data, assumed to
c         be equivalenced to wk.  in addition, wk(iprsp) and iwk(ipisp)
c         are assumed to have identical locations.
c x     = the right-hand side vector on input, and the solution vector
c         on output, of length n.
c tem   = vector of work space of length n, not used in this version.
c iersl = output flag (in common).
c         iersl = 0  if no trouble occurred.
c         iersl = -1 if cdrv returned an error flag (miter = 1 or 2).
c                    this should never occur and is considered fatal.
c         iersl = 1  if a singular matrix arose with miter = 3.
c this routine also uses other variables in common.
c-----------------------------------------------------------------------
      iersl = 0
      go to (100, 100, 300), miter
 100  CALL cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),
     1   wk(ipa),x,x,nsp,iwk(ipisp),wk(iprsp),iesp,4,iersl)
      if (iersl .ne. 0) iersl = -1
      return
c
 300  phl0 = wk(2)
      hl0 = h*el0
      wk(2) = hl0
      if (hl0 .eq. phl0) go to 330
      r = hl0/phl0
      do 320 i = 1,n
        di = 1.0d0 - r*(1.0d0 - 1.0d0/wk(i+2))
        if (dabs(di) .eq. 0.0d0) go to 390
 320    wk(i+2) = 1.0d0/di
 330  do 340 i = 1,n
 340    x(i) = wk(i+2)*x(i)
      return
 390  iersl = 1
      return
c
c----------------------- end of subroutine slss ------------------------
      end
      subroutine intdy (t, k, yh, nyh, dky, iflag)
clll. optimize
      integer k, nyh, iflag
      integer iownd, iowns,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, ic, j, jb, jb2, jj, jj1, jp1
      KPP_REAL t, yh, dky
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL c, r, s, tp
      dimension yh(nyh,1), dky(1)
      common /ls0001/ rowns(209),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     3   iownd(14), iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
c-----------------------------------------------------------------------
c intdy computes interpolated values of the k-th derivative of the
c dependent variable vector y, and stores it in dky.  this routine
c is called within the package with k = 0 and t = tout, but may
c also be called by the user for any k up to the current order.
c (see detailed instructions in the usage documentation.)
c-----------------------------------------------------------------------
c the computed values in dky are gotten by interpolation using the
c nordsieck history array yh.  this array corresponds uniquely to a
c vector-valued polynomial of degree nqcur or less, and dky is set
c to the k-th derivative of this polynomial at t.
c the formula for dky is..
c              q
c  dky(i)  =  sum  c(j,k) * (t - tn)**(j-k) * h**(-j) * yh(i,j+1)
c             j=k
c where  c(j,k) = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur.
c the quantities  nq = nqcur, l = nq+1, n = neq, tn, and h are
c communicated by common.  the above sum is done in reverse order.
c iflag is returned negative if either k or t is out of bounds.
c-----------------------------------------------------------------------
      iflag = 0
      if (k .lt. 0 .or. k .gt. nq) go to 80
      tp = tn - hu -  100.0d0*uround*(tn + hu)
      if ((t-tp)*(t-tn) .gt. 0.0d0) go to 90
c
      s = (t - tn)/h
      ic = 1
      if (k .eq. 0) go to 15
      jj1 = l - k
      do 10 jj = jj1,nq
 10     ic = ic*jj
 15   c = dfloat(ic)
      do 20 i = 1,n
 20     dky(i) = c*yh(i,l)
      if (k .eq. nq) go to 55
      jb2 = nq - k
      do 50 jb = 1,jb2
        j = nq - jb
        jp1 = j + 1
        ic = 1
        if (k .eq. 0) go to 35
        jj1 = jp1 - k
        do 30 jj = jj1,j
 30       ic = ic*jj
 35     c = dfloat(ic)
        do 40 i = 1,n
 40       dky(i) = c*yh(i,jp1) + s*dky(i)
 50     continue
      if (k .eq. 0) return
 55   r = h**(-k)
      do 60 i = 1,n
 60     dky(i) = r*dky(i)
      return
c
 80   CALL xerrwv(30hintdy--  k (=i1) illegal      ,
     1   30, 51, 0, 1, k, 0, 0, 0.0d0, 0.0d0)
      iflag = -1
      return
 90   CALL xerrwv(30hintdy--  t (=r1) illegal      ,
     1   30, 52, 0, 0, 0, 0, 1, t, 0.0d0)
      CALL xerrwv(
     1  60h      t not in interval tcur - hu (= r1) to tcur (=r2)      ,
     1   60, 52, 0, 0, 0, 0, 2, tp, tn)
      iflag = -2
      return
c----------------------- end of subroutine intdy -----------------------
      end
      subroutine prjs (neq,y,yh,nyh,ewt,ftem,savf,wk,iwk,f,jac)
clll. optimize
      external f,jac
      integer neq, nyh, iwk
      integer iownd, iowns,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     1   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     2   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     3   nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, imul, j, jj, jok, jmax, jmin, k, kmax, kmin, ng
      KPP_REAL JJJ(n,n)
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL con0, conmin, ccmxj, psmall, rbig, seth
      KPP_REAL con, di, fac, hl0, pij, r, r0, rcon, rcont,
     1   srur, vnorm
      dimension neq(*), iwk(*)
      KPP_REAL y(*), yh(nyh,*), ewt(*), ftem(*), savf(*), wk(*)
      common /ls0001/ rowns(209),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     3   iownd(14), iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,
     1   iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     2   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     3   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     4   nslj, ngp, nlu, nnz, nsp, nzl, nzu
c-----------------------------------------------------------------------
c prjs is called to compute and process the matrix
c p = i - h*el(1)*j , where j is an approximation to the jacobian.
c j is computed by columns, either by the user-supplied routine jac
c if miter = 1, or by finite differencing if miter = 2.
c if miter = 3, a diagonal approximation to j is used.
c if miter = 1 or 2, and if the existing value of the jacobian
c (as contained in p) is considered acceptable, then a new value of
c p is reconstructed from the old value.  in any case, when miter
c is 1 or 2, the p matrix is subjected to lu decomposition in cdrv.
c p and its lu decomposition are stored (separately) in wk.
c
c in addition to variables described previously, communication
c with prjs uses the following..
c y     = array containing predicted values on entry.
c ftem  = work array of length n (acor in stode).
c savf  = array containing f evaluated at predicted y.
c wk    = real work space for matrices.  on output it contains the
c         inverse diagonal matrix if miter = 3, and p and its sparse
c         lu decomposition if miter is 1 or 2.
c         storage of matrix elements starts at wk(3).
c         wk also contains the following matrix-related data..
c         wk(1) = sqrt(uround), used in numerical jacobian increments.
c         wk(2) = h*el0, saved for later use if miter = 3.
c iwk   = integer work space for matrix-related data, assumed to
c         be equivalenced to wk.  in addition, wk(iprsp) and iwk(ipisp)
c         are assumed to have identical locations.
c el0   = el(1) (input).
c ierpj = output error flag (in common).
c       = 0 if no error.
c       = 1  if zero pivot found in cdrv.
c       = 2  if a singular matrix arose with miter = 3.
c       = -1 if insufficient storage for cdrv (should not occur here).
c       = -2 if other error found in cdrv (should not occur here).
c jcur  = output flag = 1 to indicate that the jacobian matrix
c         (or approximation) is now current.
c this routine also uses other variables in common.
c-----------------------------------------------------------------------
      hl0 = h*el0
      con = -hl0
      if (miter .eq. 3) go to 300
c see whether j should be reevaluated (jok = 0) or not (jok = 1). ------
      jok = 1
      if (nst .eq. 0 .or. nst .ge. nslj+msbj) jok = 0
      if (icf .eq. 1 .and. dabs(rc - 1.0d0) .lt. ccmxj) jok = 0
      if (icf .eq. 2) jok = 0
      if (jok .eq. 1) go to 250
c
c miter = 1 or 2, and the jacobian is to be reevaluated. ---------------
 20   jcur = 1
      nje = nje + 1
      nslj = nst
      iplost = 0
      conmin = dabs(con)
      go to (100, 200), miter
c
c if miter = 1, call jac_chem, multiply by scalar, and add identity. --------
 100  continue
      kmin = iwk(ipian)
      call jac_chem (neq, tn, y, JJJ, j, iwk(ipian), iwk(ipjan))
      do 130 j = 1, n
        kmax = iwk(ipian+j) - 1
        do 110 i = 1,n
 110      ftem(i) = 0.0d0
C        call jac_chem (neq, tn, y, ftem, j, iwk(ipian), iwk(ipjan))
        do k=1,n
          ftem(k) = JJJ(k,j)
        end do
        do 120 k = kmin, kmax
          i = iwk(ibjan+k)
          wk(iba+k) = ftem(i)*con
          if (i .eq. j) wk(iba+k) = wk(iba+k) + 1.0d0
 120      continue
        kmin = kmax + 1
 130    continue
      go to 290
c
c if miter = 2, make ngp calls to f to approximate j and p. ------------
 200  continue
      fac = vnorm(n, savf, ewt)
      r0 = 1000.0d0 * dabs(h) * uround * dfloat(n) * fac
      if (r0 .eq. 0.0d0) r0 = 1.0d0
      srur = wk(1)
      jmin = iwk(ipigp)
      do 240 ng = 1,ngp
        jmax = iwk(ipigp+ng) - 1
        do 210 j = jmin,jmax
          jj = iwk(ibjgp+j)
          r = DMAX1(srur*dabs(y(jj)),r0/ewt(jj))
 210      y(jj) = y(jj) + r
        CALL f (neq, tn, y, ftem)
        do 230 j = jmin,jmax
          jj = iwk(ibjgp+j)
          y(jj) = yh(jj,1)
          r = DMAX1(srur*dabs(y(jj)),r0/ewt(jj))
          fac = -hl0/r
          kmin =iwk(ibian+jj)
          kmax =iwk(ibian+jj+1) - 1
          do 220 k = kmin,kmax
            i = iwk(ibjan+k)
            wk(iba+k) = (ftem(i) - savf(i))*fac
            if (i .eq. jj) wk(iba+k) = wk(iba+k) + 1.0d0
 220        continue
 230      continue
        jmin = jmax + 1
 240    continue
      nfe = nfe + ngp
      go to 290
c
c if jok = 1, reconstruct new p from old p. ----------------------------
 250  jcur = 0
      rcon = con/con0
      rcont = dabs(con)/conmin
      if (rcont .gt. rbig .and. iplost .eq. 1) go to 20
      kmin = iwk(ipian)
      do 275 j = 1,n
        kmax = iwk(ipian+j) - 1
        do 270 k = kmin,kmax
          i = iwk(ibjan+k)
          pij = wk(iba+k)
          if (i .ne. j) go to 260
          pij = pij - 1.0d0
          if (dabs(pij) .ge. psmall) go to 260
            iplost = 1
            conmin = dmin1(dabs(con0),conmin)
 260      pij = pij*rcon
          if (i .eq. j) pij = pij + 1.0d0
          wk(iba+k) = pij
 270      continue
        kmin = kmax + 1
 275    continue
c
c do numerical factorization of p matrix. ------------------------------
 290  nlu = nlu + 1
      con0 = con
      ierpj = 0
      do 295 i = 1,n
 295    ftem(i) = 0.0d0
      CALL cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),
     1   wk(ipa),ftem,ftem,nsp,iwk(ipisp),wk(iprsp),iesp,2,iys)
      if (iys .eq. 0) return
      imul = (iys - 1)/n
      ierpj = -2
      if (imul .eq. 8) ierpj = 1
      if (imul .eq. 10) ierpj = -1
      return
c
c if miter = 3, construct a diagonal approximation to j and p. ---------
 300  continue
      jcur = 1
      nje = nje + 1
      wk(2) = hl0
      ierpj = 0
      r = el0*0.1d0
      do 310 i = 1,n
 310    y(i) = y(i) + r*(h*savf(i) - yh(i,2))
      CALL f (neq, tn, y, wk(3))
      nfe = nfe + 1
      do 320 i = 1,n
        r0 = h*savf(i) - yh(i,2)
        di = 0.1d0*r0 - h*(wk(i+2) - savf(i))
        wk(i+2) = 1.0d0
        if (dabs(r0) .lt. uround/ewt(i)) go to 320
        if (dabs(di) .eq. 0.0d0) go to 330
        wk(i+2) = 0.1d0*r0/di
 320    continue
      return
 330  ierpj = 2
      return
c----------------------- end of subroutine prjs ------------------------
      end
      subroutine stode (neq, y, yh, nyh, yh1, ewt, savf, acor,
     1   wm, iwm, f, jac, pjac, slvs)
clll. optimize
      external f, jac, pjac, slvs
      integer neq, nyh, iwm
      integer iownd, ialth, ipup, lmax, meo, nqnyh, nslp,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, i1, iredo, iret, j, jb, m, ncf, newq
      KPP_REAL y, yh, yh1, ewt, savf, acor, wm
      KPP_REAL conit, crate, el, elco, hold, rmax, tesco,
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL dcon, ddn, del, delp, dsm, dup, exdn, exsm, exup,
     1   r, rh, rhdn, rhsm, rhup, told, vnorm
      dimension neq(*), y(*), yh(nyh,*), yh1(*), ewt(*), savf(*),
     1   acor(*), wm(*), iwm(*)
      common /ls0001/ conit, crate, el(13), elco(13,12),
     1   hold, rmax, tesco(3,12),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround, iownd(14),
     3   ialth, ipup, lmax, meo, nqnyh, nslp,
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
c-----------------------------------------------------------------------
c stode performs one step of the integration of an initial value
c problem for a system of ordinary differential equations.
c note.. stode is independent of the value of the iteration method
c indicator miter, when this is .ne. 0, and hence is independent
c of the type of chord method used, or the jacobian structure.
c communication with stode is done with the following variables..
c
c neq    = integer array containing problem size in neq(1), and
c          passed as the neq argument in all calls to f and jac.
c y      = an array of length .ge. n used as the y argument in
c          all calls to f and jac.
c yh     = an nyh by lmax array containing the dependent variables
c          and their approximate scaled derivatives, where
c          lmax = maxord + 1.  yh(i,j+1) contains the approximate
c          j-th derivative of y(i), scaled by h**j/factorial(j)
c          (j = 0,1,...,nq).  on entry for the first step, the first
c          two columns of yh must be set from the initial values.
c nyh    = a constant integer .ge. n, the first dimension of yh.
c yh1    = a one-dimensional array occupying the same space as yh.
c ewt    = an array of length n containing multiplicative weights
c          for local error measurements.  local errors in y(i) are
c          compared to 1.0/ewt(i) in various error tests.
c savf   = an array of working storage, of length n.
c          also used for input of yh(*,maxord+2) when jstart = -1
c          and maxord .lt. the current order nq.
c acor   = a work array of length n, used for the accumulated
c          corrections.  on a successful return, acor(i) contains
c          the estimated one-step local error in y(i).
c wm,iwm = real and integer work arrays associated with matrix
c          operations in chord iteration (miter .ne. 0).
c pjac   = name of routine to evaluate and preprocess jacobian matrix
c          and p = i - h*el0*jac, if a chord method is being used.
c slvs   = name of routine to solve linear system in chord iteration.
c ccmax  = maximum relative change in h*el0 before pjac is called.
c h      = the step size to be attempted on the next step.
c          h is altered by the error control algorithm during the
c          problem.  h can be either positive or negative, but its
c          sign must remain constant throughout the problem.
c hmin   = the minimum absolute value of the step size h to be used.
c hmxi   = inverse of the maximum absolute value of h to be used.
c          hmxi = 0.0 is allowed and corresponds to an infinite hmax.
c          hmin and hmxi may be changed at any time, but will not
c          take effect until the next change of h is considered.
c tn     = the independent variable. tn is updated on each step taken.
c jstart = an integer used for input only, with the following
c          values and meanings..
c               0  perform the first step.
c           .gt.0  take a new step continuing from the last.
c              -1  take the next step with a new value of h, maxord,
c                    n, meth, miter, and/or matrix parameters.
c              -2  take the next step with a new value of h,
c                    but with other inputs unchanged.
c          on return, jstart is set to 1 to facilitate continuation.
c kflag  = a completion code with the following meanings..
c               0  the step was succesful.
c              -1  the requested error could not be achieved.
c              -2  corrector convergence could not be achieved.
c              -3  fatal error in pjac or slvs.
c          a return with kflag = -1 or -2 means either
c          abs(h) = hmin or 10 consecutive failures occurred.
c          on a return with kflag negative, the values of tn and
c          the yh array are as of the beginning of the last
c          step, and h is the last step size attempted.
c maxord = the maximum order of integration method to be allowed.
c maxcor = the maximum number of corrector iterations allowed.
c msbp   = maximum number of steps between pjac calls (miter .gt. 0).
c mxncf  = maximum number of convergence failures allowed.
c meth/miter = the method flags.  see description in driver.
c n      = the number of first-order differential equations.
c-----------------------------------------------------------------------
      kflag = 0
      told = tn
      ncf = 0
      ierpj = 0
      iersl = 0
      jcur = 0
      icf = 0
      delp = 0.0d0
      if (jstart .gt. 0) go to 200
      if (jstart .eq. -1) go to 100
      if (jstart .eq. -2) go to 160
c-----------------------------------------------------------------------
c on the first call, the order is set to 1, and other variables are
c initialized.  rmax is the maximum ratio by which h can be increased
c in a single step.  it is initially 1.e4 to compensate for the small
c initial h, but then is normally equal to 10.  if a failure
c occurs (in corrector convergence or error test), rmax is set at 2
c for the next increase.
c-----------------------------------------------------------------------
      lmax = maxord + 1
      nq = 1
      l = 2
      ialth = 2
      rmax = 10000.0d0
      rc = 0.0d0
      el0 = 1.0d0
      crate = 0.7d0
      hold = h
      meo = meth
      nslp = 0
      ipup = miter
      iret = 3
      go to 140
c-----------------------------------------------------------------------
c the following block handles preliminaries needed when jstart = -1.
c ipup is set to miter to force a matrix update.
c if an order increase is about to be considered (ialth = 1),
c ialth is reset to 2 to postpone consideration one more step.
c if the caller has changed meth, cfode is called to reset
c the coefficients of the method.
c if the caller has changed maxord to a value less than the current
c order nq, nq is reduced to maxord, and a new h chosen accordingly.
c if h is to be changed, yh must be rescaled.
c if h or meth is being changed, ialth is reset to l = nq + 1
c to prevent further changes in h for that many steps.
c-----------------------------------------------------------------------
 100  ipup = miter
      lmax = maxord + 1
      if (ialth .eq. 1) ialth = 2
      if (meth .eq. meo) go to 110
      CALL cfode (meth, elco, tesco)
      meo = meth
      if (nq .gt. maxord) go to 120
      ialth = l
      iret = 1
      go to 150
 110  if (nq .le. maxord) go to 160
 120  nq = maxord
      l = lmax
      do 125 i = 1,l
 125    el(i) = elco(i,nq)
      nqnyh = nq*nyh
      rc = rc*el(1)/el0
      el0 = el(1)
      conit = 0.5d0/dfloat(nq+2)
      ddn = vnorm (n, savf, ewt)/tesco(1,l)
      exdn = 1.0d0/dfloat(l)
      rhdn = 1.0d0/(1.3d0*ddn**exdn + 0.0000013d0)
      rh = dmin1(rhdn,1.0d0)
      iredo = 3
      if (h .eq. hold) go to 170
      rh = dmin1(rh,dabs(h/hold))
      h = hold
      go to 175
c-----------------------------------------------------------------------
c cfode is called to get all the integration coefficients for the
c current meth.  then the el vector and related constants are reset
c whenever the order nq is changed, or at the start of the problem.
c-----------------------------------------------------------------------
 140  CALL cfode (meth, elco, tesco)
 150  do 155 i = 1,l
 155    el(i) = elco(i,nq)
      nqnyh = nq*nyh
      rc = rc*el(1)/el0
      el0 = el(1)
      conit = 0.5d0/dfloat(nq+2)
      go to (160, 170, 200), iret
c-----------------------------------------------------------------------
c if h is being changed, the h ratio rh is checked against
c rmax, hmin, and hmxi, and the yh array rescaled.  ialth is set to
c l = nq + 1 to prevent a change of h for that many steps, unless
c forced by a convergence or error test failure.
c-----------------------------------------------------------------------
 160  if (h .eq. hold) go to 200
      rh = h/hold
      h = hold
      iredo = 3
      go to 175
 170  rh = DMAX1(rh,hmin/dabs(h))
 175  rh = dmin1(rh,rmax)
      rh = rh/DMAX1(1.0d0,dabs(h)*hmxi*rh)
      r = 1.0d0
      do 180 j = 2,l
        r = r*rh
        do 180 i = 1,n
 180      yh(i,j) = yh(i,j)*r
      h = h*rh
      rc = rc*rh
      ialth = l
      if (iredo .eq. 0) go to 690
c-----------------------------------------------------------------------
c this section computes the predicted values by effectively
c multiplying the yh array by the pascal triangle matrix.
c rc is the ratio of new to old values of the coefficient  h*el(1).
c when rc differs from 1 by more than ccmax, ipup is set to miter
c to force pjac to be called, if a jacobian is involved.
c in any case, pjac is called at least every msbp steps.
c-----------------------------------------------------------------------
 200  if (dabs(rc-1.0d0) .gt. ccmax) ipup = miter
      if (nst .ge. nslp+msbp) ipup = miter
      tn = tn + h
      i1 = nqnyh + 1
      do 215 jb = 1,nq
        i1 = i1 - nyh
cdir$ ivdep
        do 210 i = i1,nqnyh
 210      yh1(i) = yh1(i) + yh1(i+nyh)
 215    continue
c-----------------------------------------------------------------------
c up to maxcor corrector iterations are taken.  a convergence test is
c made on the r.m.s. norm of each correction, weighted by the error
c weight vector ewt.  the sum of the corrections is accumulated in the
c vector acor(i).  the yh array is not altered in the corrector loop.
c-----------------------------------------------------------------------
 220  m = 0
      do 230 i = 1,n
 230    y(i) = yh(i,1)
      CALL f (neq, tn, y, savf)
      nfe = nfe + 1
      if (ipup .le. 0) go to 250
c-----------------------------------------------------------------------
c if indicated, the matrix p = i - h*el(1)*j is reevaluated and
c preprocessed before starting the corrector iteration.  ipup is set
c to 0 as an indicator that this has been done.
c-----------------------------------------------------------------------
      CALL pjac (neq, y, yh, nyh, ewt, acor, savf, wm, iwm, f, jac)
      ipup = 0
      rc = 1.0d0
      nslp = nst
      crate = 0.7d0
      if (ierpj .ne. 0) go to 430
 250  do 260 i = 1,n
 260    acor(i) = 0.0d0
 270  if (miter .ne. 0) go to 350
c-----------------------------------------------------------------------
c in the case of functional iteration, update y directly from
c the result of the last function evaluation.
c-----------------------------------------------------------------------
      do 290 i = 1,n
        savf(i) = h*savf(i) - yh(i,2)
 290    y(i) = savf(i) - acor(i)
      del = vnorm (n, y, ewt)
      do 300 i = 1,n
        y(i) = yh(i,1) + el(1)*savf(i)
 300    acor(i) = savf(i)
      go to 400
c-----------------------------------------------------------------------
c in the case of the chord method, compute the corrector error,
c and solve the linear system with that as right-hand side and
c p as coefficient matrix.
c-----------------------------------------------------------------------
 350  do 360 i = 1,n
 360    y(i) = h*savf(i) - (yh(i,2) + acor(i))
      CALL slvs (wm, iwm, y, savf)
      if (iersl .lt. 0) go to 430
      if (iersl .gt. 0) go to 410
      del = vnorm (n, y, ewt)
      do 380 i = 1,n
        acor(i) = acor(i) + y(i)
 380    y(i) = yh(i,1) + el(1)*acor(i)
c-----------------------------------------------------------------------
c test for convergence.  if m.gt.0, an estimate of the convergence
c rate constant is stored in crate, and this is used in the test.
c-----------------------------------------------------------------------
 400  if (m .ne. 0) crate = DMAX1(0.2d0*crate,del/delp)
      dcon = del*dmin1(1.0d0,1.5d0*crate)/(tesco(2,nq)*conit)
      if (dcon .le. 1.0d0) go to 450
      m = m + 1
      if (m .eq. maxcor) go to 410
      if (m .ge. 2 .and. del .gt. 2.0d0*delp) go to 410
      delp = del
      CALL f (neq, tn, y, savf)
      nfe = nfe + 1
      go to 270
c-----------------------------------------------------------------------
c the corrector iteration failed to converge.
c if miter .ne. 0 and the jacobian is out of date, pjac is called for
c the next try.  otherwise the yh array is retracted to its values
c before prediction, and h is reduced, if possible.  if h cannot be
c reduced or mxncf failures have occurred, exit with kflag = -2.
c-----------------------------------------------------------------------
 410  if (miter .eq. 0 .or. jcur .eq. 1) go to 430
      icf = 1
      ipup = miter
      go to 220
 430  icf = 2
      ncf = ncf + 1
      rmax = 2.0d0
      tn = told
      i1 = nqnyh + 1
      do 445 jb = 1,nq
        i1 = i1 - nyh
cdir$ ivdep
        do 440 i = i1,nqnyh
 440      yh1(i) = yh1(i) - yh1(i+nyh)
 445    continue
      if (ierpj .lt. 0 .or. iersl .lt. 0) go to 680
      if (dabs(h) .le. hmin*1.00001d0) go to 670
      if (ncf .eq. mxncf) go to 670
      rh = 0.25d0
      ipup = miter
      iredo = 1
      go to 170
c-----------------------------------------------------------------------
c the corrector has converged.  jcur is set to 0
c to signal that the jacobian involved may need updating later.
c the local error test is made and control passes to statement 500
c if it fails.
c-----------------------------------------------------------------------
 450  jcur = 0
      if (m .eq. 0) dsm = del/tesco(2,nq)
      if (m .gt. 0) dsm = vnorm (n, acor, ewt)/tesco(2,nq)
      if (dsm .gt. 1.0d0) go to 500
c-----------------------------------------------------------------------
c after a successful step, update the yh array.
c consider changing h if ialth = 1.  otherwise decrease ialth by 1.
c if ialth is then 1 and nq .lt. maxord, then acor is saved for
c use in a possible order increase on the next step.
c if a change in h is considered, an increase or decrease in order
c by one is considered also.  a change in h is made only if it is by a
c factor of at least 1.1.  if not, ialth is set to 3 to prevent
c testing for that many steps.
c-----------------------------------------------------------------------
      kflag = 0
      iredo = 0
      nst = nst + 1
      hu = h
      nqu = nq
      do 470 j = 1,l
        do 470 i = 1,n
 470      yh(i,j) = yh(i,j) + el(j)*acor(i)
      ialth = ialth - 1
      if (ialth .eq. 0) go to 520
      if (ialth .gt. 1) go to 700
      if (l .eq. lmax) go to 700
      do 490 i = 1,n
 490    yh(i,lmax) = acor(i)
      go to 700
c-----------------------------------------------------------------------
c the error test failed.  kflag keeps track of multiple failures.
c restore tn and the yh array to their previous values, and prepare
c to try the step again.  compute the optimum step size for this or
c one lower order.  after 2 or more failures, h is forced to decrease
c by a factor of 0.2 or less.
c-----------------------------------------------------------------------
 500  kflag = kflag - 1
      tn = told
      i1 = nqnyh + 1
      do 515 jb = 1,nq
        i1 = i1 - nyh
cdir$ ivdep
        do 510 i = i1,nqnyh
 510      yh1(i) = yh1(i) - yh1(i+nyh)
 515    continue
      rmax = 2.0d0
      if (dabs(h) .le. hmin*1.00001d0) go to 660
      if (kflag .le. -3) go to 640
      iredo = 2
      rhup = 0.0d0
      go to 540
c-----------------------------------------------------------------------
c regardless of the success or failure of the step, factors
c rhdn, rhsm, and rhup are computed, by which h could be multiplied
c at order nq - 1, order nq, or order nq + 1, respectively.
c in the case of failure, rhup = 0.0 to avoid an order increase.
c the largest of these is determined and the new order chosen
c accordingly.  if the order is to be increased, we compute one
c additional scaled derivative.
c-----------------------------------------------------------------------
 520  rhup = 0.0d0
      if (l .eq. lmax) go to 540
      do 530 i = 1,n
 530    savf(i) = acor(i) - yh(i,lmax)
      dup = vnorm (n, savf, ewt)/tesco(3,nq)
      exup = 1.0d0/dfloat(l+1)
      rhup = 1.0d0/(1.4d0*dup**exup + 0.0000014d0)
 540  exsm = 1.0d0/dfloat(l)
      rhsm = 1.0d0/(1.2d0*dsm**exsm + 0.0000012d0)
      rhdn = 0.0d0
      if (nq .eq. 1) go to 560
      ddn = vnorm (n, yh(1,l), ewt)/tesco(1,nq)
      exdn = 1.0d0/dfloat(nq)
      rhdn = 1.0d0/(1.3d0*ddn**exdn + 0.0000013d0)
 560  if (rhsm .ge. rhup) go to 570
      if (rhup .gt. rhdn) go to 590
      go to 580
 570  if (rhsm .lt. rhdn) go to 580
      newq = nq
      rh = rhsm
      go to 620
 580  newq = nq - 1
      rh = rhdn
      if (kflag .lt. 0 .and. rh .gt. 1.0d0) rh = 1.0d0
      go to 620
 590  newq = l
      rh = rhup
      if (rh .lt. 1.1d0) go to 610
      r = el(l)/dfloat(l)
      do 600 i = 1,n
 600    yh(i,newq+1) = acor(i)*r
      go to 630
 610  ialth = 3
      go to 700
 620  if ((kflag .eq. 0) .and. (rh .lt. 1.1d0)) go to 610
      if (kflag .le. -2) rh = dmin1(rh,0.2d0)
c-----------------------------------------------------------------------
c if there is a change of order, reset nq, l, and the coefficients.
c in any case h is reset according to rh and the yh array is rescaled.
c then exit from 690 if the step was ok, or redo the step otherwise.
c-----------------------------------------------------------------------
      if (newq .eq. nq) go to 170
 630  nq = newq
      l = nq + 1
      iret = 2
      go to 150
c-----------------------------------------------------------------------
c control reaches this section if 3 or more failures have occured.
c if 10 failures have occurred, exit with kflag = -1.
c it is assumed that the derivatives that have accumulated in the
c yh array have errors of the wrong order.  hence the first
c derivative is recomputed, and the order is set to 1.  then
c h is reduced by a factor of 10, and the step is retried,
c until it succeeds or h reaches hmin.
c-----------------------------------------------------------------------
 640  if (kflag .eq. -10) go to 660
      rh = 0.1d0
      rh = DMAX1(hmin/dabs(h),rh)
      h = h*rh
      do 645 i = 1,n
 645    y(i) = yh(i,1)
      CALL f (neq, tn, y, savf)
      nfe = nfe + 1
      do 650 i = 1,n
 650    yh(i,2) = h*savf(i)
      ipup = miter
      ialth = 5
      if (nq .eq. 1) go to 200
      nq = 1
      l = 2
      iret = 3
      go to 150
c-----------------------------------------------------------------------
c all returns are made through this section.  h is saved in hold
c to allow the caller to change h on the next step.
c-----------------------------------------------------------------------
 660  kflag = -1
      go to 720
 670  kflag = -2
      go to 720
 680  kflag = -3
      go to 720
 690  rmax = 10.0d0
 700  r = 1.0d0/tesco(2,nqu)
      do 710 i = 1,n
 710    acor(i) = acor(i)*r
 720  hold = h
      jstart = 1
      return
c----------------------- end of subroutine stode -----------------------
      end
      subroutine xerrwv (msg, nmes, nerr, level, ni, i1, i2, nr, r1, r2)
      integer msg, nmes, nerr, level, ni, i1, i2, nr,
     1   i, lun, lunit, mesflg, ncpw, nch, nwds
      KPP_REAL r1, r2
      dimension msg(nmes)
c-----------------------------------------------------------------------
c subroutines xerrwv, xsetf, and xsetun, as given here, constitute
c a simplified version of the slatec error handling package.
c written by a. c. hindmarsh at llnl.  version of march 30, 1987.
c this version is in KPP_REAL.
c
c all arguments are input arguments.
c
c msg    = the message (hollerith literal or integer array).
c nmes   = the length of msg (number of characters).
c nerr   = the error number (not used).
c level  = the error level..
c          0 or 1 means recoverable (control returns to caller).
c          2 means fatal (run is aborted--see note below).
c ni     = number of integers (0, 1, or 2) to be printed with message.
c i1,i2  = integers to be printed, depending on ni.
c nr     = number of reals (0, 1, or 2) to be printed with message.
c r1,r2  = reals to be printed, depending on nr.
c
c note..  this routine is machine-dependent and specialized for use
c in limited context, in the following ways..
c 1. the number of hollerith characters stored per word, denoted
c    by ncpw below, is a data-loaded constant.
c 2. the value of nmes is assumed to be at most 60.
c    (multi-line messages are generated by repeated calls.)
c 3. if level = 2, control passes to the statement   stop
c    to abort the run.  this statement may be machine-dependent.
c 4. r1 and r2 are assumed to be in KPP_REAL and are printed
c    in d21.13 format.
c 5. the common block /eh0001/ below is data-loaded (a machine-
c    dependent feature) with default values.
c    this block is needed for proper retention of parameters used by
c    this routine which the user can reset by calling xsetf or xsetun.
c    the variables in this block are as follows..
c       mesflg = print control flag..
c                1 means print all messages (the default).
c                0 means no printing.
c       lunit  = logical unit number for messages.
c                the default is 6 (machine-dependent).
c-----------------------------------------------------------------------
c the following are instructions for installing this routine
c in different machine environments.
c
c to change the default output unit, change the data statement
c in the block data subprogram below.
c
c for a different number of characters per word, change the
c data statement setting ncpw below, and format 10.  alternatives for
c various computers are shown in comment cards.
c
c for a different run-abort command, change the statement following
c statement 100 at the end.
c-----------------------------------------------------------------------
      common /eh0001/ mesflg, lunit
c-----------------------------------------------------------------------
c the following data-loaded value of ncpw is valid for the cdc-6600
c and cdc-7600 computers.
c     data ncpw/10/
c the following is valid for the cray-1 computer.
c     data ncpw/8/
c the following is valid for the burroughs 6700 and 7800 computers.
c     data ncpw/6/
c the following is valid for the pdp-10 computer.
c     data ncpw/5/
c the following is valid for the vax computer with 4 bytes per integer,
c and for the ibm-360, ibm-370, ibm-303x, and ibm-43xx computers.
      data ncpw/4/
c the following is valid for the pdp-11, or vax with 2-byte integers.
c     data ncpw/2/
c-----------------------------------------------------------------------
      if (mesflg .eq. 0) go to 100
c get logical unit number. ---------------------------------------------
      lun = lunit
c get number of words in message. --------------------------------------
      nch = min0(nmes,60)
      nwds = nch/ncpw
      if (nch .ne. nwds*ncpw) nwds = nwds + 1
c write the message. ---------------------------------------------------
      write (lun, 10) (msg(i),i=1,nwds)
c-----------------------------------------------------------------------
c the following format statement is to have the form
c 10  format(1x,mmann)
c where nn = ncpw and mm is the smallest integer .ge. 60/ncpw.
c the following is valid for ncpw = 10.
c 10  format(1x,6a10)
c the following is valid for ncpw = 8.
c 10  format(1x,8a8)
c the following is valid for ncpw = 6.
c 10  format(1x,10a6)
c the following is valid for ncpw = 5.
c 10  format(1x,12a5)
c the following is valid for ncpw = 4.
  10  format(1x,15a4)
c the following is valid for ncpw = 2.
c 10  format(1x,30a2)
c-----------------------------------------------------------------------
      if (ni .eq. 1) write (lun, 20) i1
 20   format(6x,23hin above message,  i1 =,i10)
      if (ni .eq. 2) write (lun, 30) i1,i2
 30   format(6x,23hin above message,  i1 =,i10,3x,4hi2 =,i10)
      if (nr .eq. 1) write (lun, 40) r1
 40   format(6x,23hin above message,  r1 =,d21.13)
      if (nr .eq. 2) write (lun, 50) r1,r2
 50   format(6x,15hin above,  r1 =,d21.13,3x,4hr2 =,d21.13)
c abort the run if level = 2. ------------------------------------------
 100  if (level .ne. 2) return
      stop
c----------------------- end of subroutine xerrwv ----------------------
      end
      KPP_REAL function vnorm (n, v, w)
clll. optimize
c-----------------------------------------------------------------------
c this function routine computes the weighted root-mean-square norm
c of the vector of length n contained in the array v, with weights
c contained in the array w of length n..
c   vnorm = sqrt( (1/n) * sum( v(i)*w(i) )**2 )
c-----------------------------------------------------------------------
      integer n,   i
      KPP_REAL v, w,   sum
      dimension v(n), w(n)
      sum = 0.0d0
      do 10 i = 1,n
 10     sum = sum + (v(i)*w(i))**2
      vnorm = DSQRT(sum/dfloat(n))
      return
c----------------------- end of function vnorm -------------------------
      end
      subroutine ewset (n, itol, RelTol, AbsTol, ycur, ewt)
clll. optimize
c-----------------------------------------------------------------------
c this subroutine sets the error weight vector ewt according to
c     ewt(i) = RelTol(i)*abs(ycur(i)) + AbsTol(i),  i = 1,...,n,
c with the subscript on RelTol and/or AbsTol possibly replaced by 1 above,
c depending on the value of itol.
c-----------------------------------------------------------------------
      integer n, itol
      integer i
      KPP_REAL RelTol, AbsTol, ycur, ewt
      dimension RelTol(1), AbsTol(1), ycur(n), ewt(n)
c
      go to (10, 20, 30, 40), itol
 10   continue
      do 15 i = 1,n
 15     ewt(i) = RelTol(1)*dabs(ycur(i)) + AbsTol(1)
      return
 20   continue
      do 25 i = 1,n
 25     ewt(i) = RelTol(1)*dabs(ycur(i)) + AbsTol(i)
      return
 30   continue
      do 35 i = 1,n
 35     ewt(i) = RelTol(i)*dabs(ycur(i)) + AbsTol(1)
      return
 40   continue
      do 45 i = 1,n
 45     ewt(i) = RelTol(i)*dabs(ycur(i)) + AbsTol(i)
      return
c----------------------- end of subroutine ewset -----------------------
      end
      subroutine cfode (meth, elco, tesco)
clll. optimize
      integer meth
      integer i, ib, nq, nqm1, nqp1
      KPP_REAL elco, tesco
      KPP_REAL agamq, fnq, fnqm1, pc, pint, ragq,
     1   rqfac, rq1fac, tsign, xpin
      dimension elco(13,12), tesco(3,12)
c-----------------------------------------------------------------------
c cfode is called by the integrator routine to set coefficients
c needed there.  the coefficients for the current method, as
c given by the value of meth, are set for all orders and saved.
c the maximum order assumed here is 12 if meth = 1 and 5 if meth = 2.
c (a smaller value of the maximum order is also allowed.)
c cfode is called once at the beginning of the problem,
c and is not called again unless and until meth is changed.
c
c the elco array contains the basic method coefficients.
c the coefficients el(i), 1 .le. i .le. nq+1, for the method of
c order nq are stored in elco(i,nq).  they are given by a genetrating
c polynomial, i.e.,
c     l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq.
c for the implicit adams methods, l(x) is given by
c     dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0.
c for the bdf methods, l(x) is given by
c     l(x) = (x+1)*(x+2)* ... *(x+nq)/k,
c where         k = factorial(nq)*(1 + 1/2 + ... + 1/nq).
c
c the tesco array contains test constants used for the
c local error test and the selection of step size and/or order.
c at order nq, tesco(k,nq) is used for the selection of step
c size at order nq - 1 if k = 1, at order nq if k = 2, and at order
c nq + 1 if k = 3.
c-----------------------------------------------------------------------
      dimension pc(12)
c
      go to (100, 200), meth
c
 100  elco(1,1) = 1.0d0
      elco(2,1) = 1.0d0
      tesco(1,1) = 0.0d0
      tesco(2,1) = 2.0d0
      tesco(1,2) = 1.0d0
      tesco(3,12) = 0.0d0
      pc(1) = 1.0d0
      rqfac = 1.0d0
      do 140 nq = 2,12
c-----------------------------------------------------------------------
c the pc array will contain the coefficients of the polynomial
c     p(x) = (x+1)*(x+2)*...*(x+nq-1).
c initially, p(x) = 1.
c-----------------------------------------------------------------------
        rq1fac = rqfac
        rqfac = rqfac/dfloat(nq)
        nqm1 = nq - 1
        fnqm1 = dfloat(nqm1)
        nqp1 = nq + 1
c form coefficients of p(x)*(x+nq-1). ----------------------------------
        pc(nq) = 0.0d0
        do 110 ib = 1,nqm1
          i = nqp1 - ib
 110      pc(i) = pc(i-1) + fnqm1*pc(i)
        pc(1) = fnqm1*pc(1)
c compute integral, -1 to 0, of p(x) and x*p(x). -----------------------
        pint = pc(1)
        xpin = pc(1)/2.0d0
        tsign = 1.0d0
        do 120 i = 2,nq
          tsign = -tsign
          pint = pint + tsign*pc(i)/dfloat(i)
 120      xpin = xpin + tsign*pc(i)/dfloat(i+1)
c store coefficients in elco and tesco. --------------------------------
        elco(1,nq) = pint*rq1fac
        elco(2,nq) = 1.0d0
        do 130 i = 2,nq
 130      elco(i+1,nq) = rq1fac*pc(i)/dfloat(i)
        agamq = rqfac*xpin
        ragq = 1.0d0/agamq
        tesco(2,nq) = ragq
        if (nq .lt. 12) tesco(1,nqp1) = ragq*rqfac/dfloat(nqp1)
        tesco(3,nqm1) = ragq
 140    continue
      return
c
 200  pc(1) = 1.0d0
      rq1fac = 1.0d0
      do 230 nq = 1,5
c-----------------------------------------------------------------------
c the pc array will contain the coefficients of the polynomial
c     p(x) = (x+1)*(x+2)*...*(x+nq).
c initially, p(x) = 1.
c-----------------------------------------------------------------------
        fnq = dfloat(nq)
        nqp1 = nq + 1
c form coefficients of p(x)*(x+nq). ------------------------------------
        pc(nqp1) = 0.0d0
        do 210 ib = 1,nq
          i = nq + 2 - ib
 210      pc(i) = pc(i-1) + fnq*pc(i)
        pc(1) = fnq*pc(1)
c store coefficients in elco and tesco. --------------------------------
        do 220 i = 1,nqp1
 220      elco(i,nq) = pc(i)/pc(2)
        elco(2,nq) = 1.0d0
        tesco(1,nq) = rq1fac
        tesco(2,nq) = dfloat(nqp1)/elco(1,nq)
        tesco(3,nq) = dfloat(nq+2)/elco(1,nq)
        rq1fac = rq1fac/fnq
 230    continue
      return
c----------------------- end of subroutine cfode -----------------------
      end
      subroutine cdrv
     *     (n, r,c,ic, ia,ja,a, b, z, nsp,isp,rsp,esp, path, flag)
clll. optimize
c*** subroutine cdrv
c*** driver for subroutines for solving sparse nonsymmetric systems of
c       linear equations (compressed pointer storage)
c
c
c    parameters
c    class abbreviations are--
c       n - integer variable
c       f - real variable
c       v - supplies a value to the driver
c       r - returns a result from the driver
c       i - used internally by the driver
c       a - array
c
c class - parameter
c ------+----------
c       -
c         the nonzero entries of the coefficient matrix m are stored
c    row-by-row in the array a.  to identify the individual nonzero
c    entries in each row, we need to know in which column each entry
c    lies.  the column indices which correspond to the nonzero entries
c    of m are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
c    ja(k) = j.  in addition, we need to know where each row starts and
c    how long it is.  the index positions in ja and a where the rows of
c    m begin are stored in the array ia.  i.e., if m(i,j) is the first
c    nonzero entry (stored) in the i-th row and a(k) = m(i,j),  then
c    ia(i) = k.  moreover, the index in ja and a of the first location
c    following the last element in the last row is stored in ia(n+1).
c    thus, the number of entries in the i-th row is given by
c    ia(i+1) - ia(i),  the nonzero entries of the i-th row are stored
c    consecutively in
c            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
c    and the corresponding column indices are stored consecutively in
c            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
c    for example, the 5 by 5 matrix
c                ( 1. 0. 2. 0. 0.)
c                ( 0. 3. 0. 0. 0.)
c            m = ( 0. 4. 5. 6. 0.)
c                ( 0. 0. 0. 7. 0.)
c                ( 0. 0. 0. 8. 9.)
c    would be stored as
c               - 1  2  3  4  5  6  7  8  9
c            ---+--------------------------
c            ia - 1  3  4  7  8 10
c            ja - 1  3  2  2  3  4  4  4  5
c             a - 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
c
c nv    - n     - number of variables/equations.
c fva   - a     - nonzero entries of the coefficient matrix m, stored
c       -           by rows.
c       -           size = number of nonzero entries in m.
c nva   - ia    - pointers to delimit the rows in a.
c       -           size = n+1.
c nva   - ja    - column numbers corresponding to the elements of a.
c       -           size = size of a.
c fva   - b     - right-hand side b.  b and z can the same array.
c       -           size = n.
c fra   - z     - solution x.  b and z can be the same array.
c       -           size = n.
c
c         the rows and columns of the original matrix m can be
c    reordered (e.g., to reduce fillin or ensure numerical stability)
c    before calling the driver.  if no reordering is done, then set
c    r(i) = c(i) = ic(i) = i  for i=1,...,n.  the solution z is returned
c    in the original order.
c         if the columns have been reordered (i.e.,  c(i).ne.i  for some
c    i), then the driver will CALL a subroutine (nroc) which rearranges
c    each row of ja and a, leaving the rows in the original order, but
c    placing the elements of each row in increasing order with respect
c    to the new ordering.  if  path.ne.1,  then nroc is assumed to have
c    been called already.
c
c nva   - r     - ordering of the rows of m.
c       -           size = n.
c nva   - c     - ordering of the columns of m.
c       -           size = n.
c nva   - ic    - inverse of the ordering of the columns of m.  i.e.,
c       -           ic(c(i)) = i  for i=1,...,n.
c       -           size = n.
c
c         the solution of the system of linear equations is divided into
c    three stages --
c      nsfc -- the matrix m is processed symbolically to determine where
c               fillin will occur during the numeric factorization.
c      nnfc -- the matrix m is factored numerically into the product ldu
c               of a unit lower triangular matrix l, a diagonal matrix
c               d, and a unit upper triangular matrix u, and the system
c               mx = b  is solved.
c      nnsc -- the linear system  mx = b  is solved using the ldu
c  or           factorization from nnfc.
c      nntc -- the transposed linear system  mt x = b  is solved using
c               the ldu factorization from nnf.
c    for several systems whose coefficient matrices have the same
c    nonzero structure, nsfc need be done only once (for the first
c    system).  then nnfc is done once for each additional system.  for
c    several systems with the same coefficient matrix, nsfc and nnfc
c    need be done only once (for the first system).  then nnsc or nntc
c    is done once for each additional right-hand side.
c
c nv    - path  - path specification.  values and their meanings are --
c       -           1  perform nroc, nsfc, and nnfc.
c       -           2  perform nnfc only  (nsfc is assumed to have been
c       -               done in a manner compatible with the storage
c       -               allocation used in the driver).
c       -           3  perform nnsc only  (nsfc and nnfc are assumed to
c       -               have been done in a manner compatible with the
c       -               storage allocation used in the driver).
c       -           4  perform nntc only  (nsfc and nnfc are assumed to
c       -               have been done in a manner compatible with the
c       -               storage allocation used in the driver).
c       -           5  perform nroc and nsfc.
c
c         various errors are detected by the driver and the individual
c    subroutines.
c
c nr    - flag  - error flag.  values and their meanings are --
c       -             0     no errors detected
c       -             n+k   null row in a  --  row = k
c       -            2n+k   duplicate entry in a  --  row = k
c       -            3n+k   insufficient storage in nsfc  --  row = k
c       -            4n+1   insufficient storage in nnfc
c       -            5n+k   null pivot  --  row = k
c       -            6n+k   insufficient storage in nsfc  --  row = k
c       -            7n+1   insufficient storage in nnfc
c       -            8n+k   zero pivot  --  row = k
c       -           10n+1   insufficient storage in cdrv
c       -           11n+1   illegal path specification
c
c         working storage is needed for the factored form of the matrix
c    m plus various temporary vectors.  the arrays isp and rsp should be
c    equivalenced.  integer storage is allocated from the beginning of
c    isp and real storage from the end of rsp.
c
c nv    - nsp   - declared dimension of rsp.  nsp generally must
c       -           be larger than  8n+2 + 2k  (where  k = (number of
c       -           nonzero entries in m)).
c nvira - isp   - integer working storage divided up into various arrays
c       -           needed by the subroutines.  isp and rsp should be
c       -           equivalenced.
c       -           size = lratio*nsp.
c fvira - rsp   - real working storage divided up into various arrays
c       -           needed by the subroutines.  isp and rsp should be
c       -           equivalenced.
c       -           size = nsp.
c nr    - esp   - if sufficient storage was available to perform the
c       -           symbolic factorization (nsfc), then esp is set to
c       -           the amount of excess storage provided (negative if
c       -           insufficient storage was available to perform the
c       -           numeric factorization (nnfc)).
c
c
c  conversion to KPP_REAL
c
c    to convert these routines for KPP_REAL arrays..
c    (1) use the KPP_REAL declarations in place of the real
c    declarations in each subprogram, as given in comment cards.
c    (2) change the data-loaded value of the integer  lratio
c    in subroutine cdrv, as indicated below.
c    (3) change e0 to d0 in the constants in statement number 10
c    in subroutine nnfc and the line following that.
c
      integer  r(1), c(1), ic(1),  ia(1), ja(1),  isp(1), esp,  path,
     *   flag,  d, u, q, row, tmp, ar,  umax
      KPP_REAL  a(1), b(1), z(1), rsp(1)
c
c  set lratio equal to the ratio between the length of floating point
c  and integer array data.  e. g., lratio = 1 for (real, integer),
c  lratio = 2 for (KPP_REAL, integer)
c
      data lratio/2/
c
      if (path.lt.1 .or. 5.lt.path)  go to 111
c******initialize and divide up temporary storage  *******************
      il   = 1
      ijl  = il  + (n+1)
      iu   = ijl +   n
      iju  = iu  + (n+1)
      irl  = iju +   n
      jrl  = irl +   n
      jl   = jrl +   n
c
c  ******  reorder a if necessary, CALL nsfc if flag is set  ***********
      if ((path-1) * (path-5) .ne. 0)  go to 5
        max = (lratio*nsp + 1 - jl) - (n+1) - 5*n
        jlmax = max/2
        q     = jl   + jlmax
        ira   = q    + (n+1)
        jra   = ira  +   n
        irac  = jra  +   n
        iru   = irac +   n
        jru   = iru  +   n
        jutmp = jru  +   n
        jumax = lratio*nsp  + 1 - jutmp
        esp = max/lratio
        if (jlmax.le.0 .or. jumax.le.0)  go to 110
c
        do 1 i=1,n
          if (c(i).ne.i)  go to 2
   1      continue
        go to 3
   2    ar = nsp + 1 - n
        CALL  nroc
     *     (n, ic, ia,ja,a, isp(il), rsp(ar), isp(iu), flag)
        if (flag.ne.0)  go to 100
c
   3    CALL  nsfc
     *     (n, r, ic, ia,ja,
     *      jlmax, isp(il), isp(jl), isp(ijl),
     *      jumax, isp(iu), isp(jutmp), isp(iju),
     *      isp(q), isp(ira), isp(jra), isp(irac),
     *      isp(irl), isp(jrl), isp(iru), isp(jru),  flag)
        if(flag .ne. 0)  go to 100
c  ******  move ju next to jl  *****************************************
        jlmax = isp(ijl+n-1)
        ju    = jl + jlmax
        jumax = isp(iju+n-1)
        if (jumax.le.0)  go to 5
        do 4 j=1,jumax
   4      isp(ju+j-1) = isp(jutmp+j-1)
c
c  ******  CALL remaining subroutines  *********************************
   5  jlmax = isp(ijl+n-1)
      ju    = jl  + jlmax
      jumax = isp(iju+n-1)
      l     = (ju + jumax - 2 + lratio)  /  lratio    +    1
      lmax  = isp(il+n) - 1
      d     = l   + lmax
      u     = d   + n
      row   = nsp + 1 - n
      tmp   = row - n
      umax  = tmp - u
      esp   = umax - (isp(iu+n) - 1)
c
      if ((path-1) * (path-2) .ne. 0)  go to 6
        if (umax.lt.0)  go to 110
        CALL nnfc
     *     (n,  r, c, ic,  ia, ja, a, z, b,
     *      lmax, isp(il), isp(jl), isp(ijl), rsp(l),  rsp(d),
     *      umax, isp(iu), isp(ju), isp(iju), rsp(u),
     *      rsp(row), rsp(tmp),  isp(irl), isp(jrl),  flag)
        if(flag .ne. 0)  go to 100
c
   6  if ((path-3) .ne. 0)  go to 7
        CALL nnsc
     *     (n,  r, c,  isp(il), isp(jl), isp(ijl), rsp(l),
     *      rsp(d),    isp(iu), isp(ju), isp(iju), rsp(u),
     *      z, b,  rsp(tmp))
c
   7  if ((path-4) .ne. 0)  go to 8
        CALL nntc
     *     (n,  r, c,  isp(il), isp(jl), isp(ijl), rsp(l),
     *      rsp(d),    isp(iu), isp(ju), isp(iju), rsp(u),
     *      z, b,  rsp(tmp))
   8  return
c
c ** error.. error detected in nroc, nsfc, nnfc, or nnsc
 100  return
c ** error.. insufficient storage
 110  flag = 10*n + 1
      return
c ** error.. illegal path specification
 111  flag = 11*n + 1
      return
      end
      subroutine prep (neq, y, yh, savf, ewt, ftem, ia, ja,
     1                     wk, iwk, ipper, f, jac)
clll. optimize
      external f,jac
      integer neq(*), ia(*), ja(*), iwk(*), ipper
      integer iownd, iowns,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     1   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     2   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     3   nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, ibr, ier, ipil, ipiu, iptt1, iptt2, j, jfound, k,
     1   knew, kmax, kmin, ldif, lenigp, liwk, maxg, np1, nzsut
      KPP_REAL y(*), yh(*), savf(*), ewt(*), ftem(*), wk(*)
      KPP_REAL rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      KPP_REAL con0, conmin, ccmxj, psmall, rbig, seth
      KPP_REAL dq, dyj, erwt, fac, yj, JJJ(n,n)
      common /ls0001/ rowns(209),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     3   iownd(14), iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,
     1   iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,
     2   ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,
     3   lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,
     4   nslj, ngp, nlu, nnz, nsp, nzl, nzu
c-----------------------------------------------------------------------
c this routine performs preprocessing related to the sparse linear
c systems that must be solved if miter = 1 or 2.
c the operations that are performed here are..
c  * compute sparseness structure of jacobian according to moss,
c  * compute grouping of column indices (miter = 2),
c  * compute a new ordering of rows and columns of the matrix,
c  * reorder ja corresponding to the new ordering,
c  * perform a symbolic lu factorization of the matrix, and
c  * set pointers for segments of the iwk/wk array.
c in addition to variables described previously, prep uses the
c following for communication..
c yh     = the history array.  only the first column, containing the
c          current y vector, is used.  used only if moss .ne. 0.
c savf   = a work array of length neq, used only if moss .ne. 0.
c ewt    = array of length neq containing (inverted) error weights.
c          used only if moss = 2 or if istate = moss = 1.
c ftem   = a work array of length neq, identical to acor in the driver,
c          used only if moss = 2.
c wk     = a real work array of length lenwk, identical to wm in
c          the driver.
c iwk    = integer work array, assumed to occupy the same space as wk.
c lenwk  = the length of the work arrays wk and iwk.
c istatc = a copy of the driver input argument istate (= 1 on the
c          first call, = 3 on a continuation call).
c iys    = flag value from odrv or cdrv.
c ipper  = output error flag with the following values and meanings..
c          0  no error.
c         -1  insufficient storage for internal structure pointers.
c         -2  insufficient storage for jgroup.
c         -3  insufficient storage for odrv.
c         -4  other error flag from odrv (should never occur).
c         -5  insufficient storage for cdrv.
c         -6  other error flag from cdrv.
c-----------------------------------------------------------------------
      ibian = lrat*2
      ipian = ibian + 1
      np1 = n + 1
      ipjan = ipian + np1
      ibjan = ipjan - 1
      liwk = lenwk*lrat
      if (ipjan+n-1 .gt. liwk) go to 210
      if (moss .eq. 0) go to 30
c
      if (istatc .eq. 3) go to 20
c istate = 1 and moss .ne. 0.  perturb y for structure determination. --
      do 10 i = 1,n
        erwt = 1.0d0/ewt(i)
        fac = 1.0d0 + 1.0d0/(dfloat(i)+1.0d0)
        y(i) = y(i) + fac*DSIGN(erwt,y(i))
 10     continue
      go to (70, 100), moss
c
 20   continue
c istate = 3 and moss .ne. 0.  load y from yh(*,1). --------------------
      do 25 i = 1,n
 25     y(i) = yh(i)
      go to (70, 100), moss
c
c moss = 0.  process user-s ia,ja.  add diagonal entries if necessary. -
 30   knew = ipjan
      kmin = ia(1)
      iwk(ipian) = 1
      do 60 j = 1,n
        jfound = 0
        kmax = ia(j+1) - 1
        if (kmin .gt. kmax) go to 45
        do 40 k = kmin,kmax
          i = ja(k)
          if (i .eq. j) jfound = 1
          if (knew .gt. liwk) go to 210
          iwk(knew) = i
          knew = knew + 1
 40       continue
        if (jfound .eq. 1) go to 50
 45     if (knew .gt. liwk) go to 210
        iwk(knew) = j
        knew = knew + 1
 50     iwk(ipian+j) = knew + 1 - ipjan
        kmin = kmax + 1
 60     continue
      go to 140
c
c moss = 1.  compute structure from user-supplied jacobian routine jac.
 70   continue
c a dummy CALL to f allows user to create temporaries for use in jac. --
      CALL f (neq, tn, y, savf)
      k = ipjan
      iwk(ipian) = 1
      call jac_chem (neq, tn, y, JJJ, j, iwk(ipian), iwk(ipjan))
      do 90 j = 1,n
        if (k .gt. liwk) go to 210
        iwk(k) = j
        k = k + 1
        do 75 i = 1,n
 75       savf(i) = 0.0d0
C        call jac_chem (neq, tn, y, j, iwk(ipian), iwk(ipjan), savf)
        do i=1,n
           savf(i) = JJJ(i,j)
        end do
        do 80 i = 1,n
          if (dabs(savf(i)) .le. seth) go to 80
          if (i .eq. j) go to 80
          if (k .gt. liwk) go to 210
          iwk(k) = i
          k = k + 1
 80       continue
        iwk(ipian+j) = k + 1 - ipjan
 90     continue
      go to 140
c
c moss = 2.  compute structure from results of n + 1 calls to f. -------
 100  k = ipjan
      iwk(ipian) = 1
      CALL f (neq, tn, y, savf)
      do 120 j = 1,n
        if (k .gt. liwk) go to 210
        iwk(k) = j
        k = k + 1
        yj = y(j)
        erwt = 1.0d0/ewt(j)
        dyj = DSIGN(erwt,yj)
        y(j) = yj + dyj
        CALL f (neq, tn, y, ftem)
        y(j) = yj
        do 110 i = 1,n
          dq = (ftem(i) - savf(i))/dyj
          if (dabs(dq) .le. seth) go to 110
          if (i .eq. j) go to 110
          if (k .gt. liwk) go to 210
          iwk(k) = i
          k = k + 1
 110      continue
        iwk(ipian+j) = k + 1 - ipjan
 120    continue
c
 140  continue
      if (moss .eq. 0 .or. istatc .ne. 1) go to 150
c if istate = 1 and moss .ne. 0, restore y from yh. --------------------
      do 145 i = 1,n
 145    y(i) = yh(i)
 150  nnz = iwk(ipian+n) - 1
      lenigp = 0
      ipigp = ipjan + nnz
      if (miter .ne. 2) go to 160
c
c compute grouping of column indices (miter = 2). ----------------------
      maxg = np1
      ipjgp = ipjan + nnz
      ibjgp = ipjgp - 1
      ipigp = ipjgp + n
      iptt1 = ipigp + np1
      iptt2 = iptt1 + n
      lreq = iptt2 + n - 1
      if (lreq .gt. liwk) go to 220
      CALL jgroup (n, iwk(ipian), iwk(ipjan), maxg, ngp, iwk(ipigp),
     1   iwk(ipjgp), iwk(iptt1), iwk(iptt2), ier)
      if (ier .ne. 0) go to 220
      lenigp = ngp + 1
c
c compute new ordering of rows/columns of jacobian. --------------------
 160  ipr = ipigp + lenigp
      ipc = ipr
      ipic = ipc + n
      ipisp = ipic + n
      iprsp = (ipisp - 2)/lrat + 2
      iesp = lenwk + 1 - iprsp
      if (iesp .lt. 0) go to 230
      ibr = ipr - 1
      do 170 i = 1,n
 170    iwk(ibr+i) = i
      nsp = liwk + 1 - ipisp
      CALL odrv (n, iwk(ipian), iwk(ipjan), wk, iwk(ipr), iwk(ipic),
     1   nsp, iwk(ipisp), 1, iys)
      if (iys .eq. 11*n+1) go to 240
      if (iys .ne. 0) go to 230
c
c reorder jan and do symbolic lu factorization of matrix. --------------
      ipa = lenwk + 1 - nnz
      nsp = ipa - iprsp
      lreq = max0(12*n/lrat, 6*n/lrat+2*n+nnz) + 3
      lreq = lreq + iprsp - 1 + nnz
      if (lreq .gt. lenwk) go to 250
      iba = ipa - 1
      do 180 i = 1,nnz
 180    wk(iba+i) = 0.0d0
      ipisp = lrat*(iprsp - 1) + 1
      CALL cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),
     1   wk(ipa),wk(ipa),wk(ipa),nsp,iwk(ipisp),wk(iprsp),iesp,5,iys)
      lreq = lenwk - iesp
      if (iys .eq. 10*n+1) go to 250
      if (iys .ne. 0) go to 260
      ipil = ipisp
      ipiu = ipil + 2*n + 1
      nzu = iwk(ipil+n) - iwk(ipil)
      nzl = iwk(ipiu+n) - iwk(ipiu)
      if (lrat .gt. 1) go to 190
      CALL adjlr (n, iwk(ipisp), ldif)
      lreq = lreq + ldif
 190  continue
      if (lrat .eq. 2 .and. nnz .eq. n) lreq = lreq + 1
      nsp = nsp + lreq - lenwk
      ipa = lreq + 1 - nnz
      iba = ipa - 1
      ipper = 0
      return
c
 210  ipper = -1
      lreq = 2 + (2*n + 1)/lrat
      lreq = max0(lenwk+1,lreq)
      return
c
 220  ipper = -2
      lreq = (lreq - 1)/lrat + 1
      return
c
 230  ipper = -3
      CALL cntnzu (n, iwk(ipian), iwk(ipjan), nzsut)
      lreq = lenwk - iesp + (3*n + 4*nzsut - 1)/lrat + 1
      return
c
 240  ipper = -4
      return
c
 250  ipper = -5
      return
c
 260  ipper = -6
      lreq = lenwk
      return
c----------------------- end of subroutine prep ------------------------
      end
      subroutine cntnzu (n, ia, ja, nzsut)
      integer n, ia, ja, nzsut
      dimension ia(1), ja(1)
c-----------------------------------------------------------------------
c this routine counts the number of nonzero elements in the strict
c upper triangle of the matrix m + m(transpose), where the sparsity
c structure of m is given by pointer arrays ia and ja.
c this is needed to compute the storage requirements for the
c sparse matrix reordering operation in odrv.
c-----------------------------------------------------------------------
      integer ii, jj, j, jmin, jmax, k, kmin, kmax, num
c
      num = 0
      do 50 ii = 1,n
        jmin = ia(ii)
        jmax = ia(ii+1) - 1
        if (jmin .gt. jmax) go to 50
        do 40 j = jmin,jmax
          if (ja(j) - ii) 10, 40, 30
 10       jj =ja(j)
          kmin = ia(jj)
          kmax = ia(jj+1) - 1
          if (kmin .gt. kmax) go to 30
          do 20 k = kmin,kmax
            if (ja(k) .eq. ii) go to 40
 20         continue
 30       num = num + 1
 40       continue
 50     continue
      nzsut = num
      return
c----------------------- end of subroutine cntnzu ----------------------
      end
      subroutine nntc
     *     (n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, z, b, tmp)
clll. optimize
c*** subroutine nntc
c*** numeric solution of the transpose of a sparse nonsymmetric system
c      of linear equations given lu-factorization (compressed pointer
c      storage)
c
c
c       input variables..  n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, b
c       output variables.. z
c
c       parameters used internally..
c fia   - tmp   - temporary vector which gets result of solving ut y = b
c       -           size = n.
c
c  internal variables..
c    jmin, jmax - indices of the first and last positions in a row of
c      u or l  to be used.
c
      integer r(1), c(1), il(1), jl(1), ijl(1), iu(1), ju(1), iju(1)
      KPP_REAL l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk,sum
c
c  ******  set tmp to reordered b  *************************************
      do 1 k=1,n
   1    tmp(k) = b(c(k))
c  ******  solve  ut y = b  by forward substitution  *******************
      do 3 k=1,n
        jmin = iu(k)
        jmax = iu(k+1) - 1
        tmpk = -tmp(k)
        if (jmin .gt. jmax) go to 3
        mu = iju(k) - jmin
        do 2 j=jmin,jmax
   2      tmp(ju(mu+j)) = tmp(ju(mu+j)) + tmpk * u(j)
   3    continue
c  ******  solve  lt x = y  by back substitution  **********************
      k = n
      do 6 i=1,n
        sum = -tmp(k)
        jmin = il(k)
        jmax = il(k+1) - 1
        if (jmin .gt. jmax) go to 5
        ml = ijl(k) - jmin
        do 4 j=jmin,jmax
   4      sum = sum + l(j) * tmp(jl(ml+j))
   5    tmp(k) = -sum * d(k)
        z(r(k)) = tmp(k)
        k = k - 1
   6    continue
      return
      end
      subroutine nnsc
     *     (n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, z, b, tmp)
clll. optimize
c*** subroutine nnsc
c*** numerical solution of sparse nonsymmetric system of linear
c      equations given ldu-factorization (compressed pointer storage)
c
c
c       input variables..  n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, b
c       output variables.. z
c
c       parameters used internally..
c fia   - tmp   - temporary vector which gets result of solving  ly = b.
c       -           size = n.
c
c  internal variables..
c    jmin, jmax - indices of the first and last positions in a row of
c      u or l  to be used.
c
      integer r(1), c(1), il(1), jl(1), ijl(1), iu(1), ju(1), iju(1)
      KPP_REAL  l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk,sum
c
c  ******  set tmp to reordered b  *************************************
      do 1 k=1,n
   1    tmp(k) = b(r(k))
c  ******  solve  ly = b  by forward substitution  *********************
      do 3 k=1,n
        jmin = il(k)
        jmax = il(k+1) - 1
        tmpk = -d(k) * tmp(k)
        tmp(k) = -tmpk
        if (jmin .gt. jmax) go to 3
        ml = ijl(k) - jmin
        do 2 j=jmin,jmax
   2      tmp(jl(ml+j)) = tmp(jl(ml+j)) + tmpk * l(j)
   3    continue
c  ******  solve  ux = y  by back substitution  ************************
      k = n
      do 6 i=1,n
        sum = -tmp(k)
        jmin = iu(k)
        jmax = iu(k+1) - 1
        if (jmin .gt. jmax) go to 5
        mu = iju(k) - jmin
        do 4 j=jmin,jmax
   4      sum = sum + u(j) * tmp(ju(mu+j))
   5    tmp(k) = -sum
        z(c(k)) = -sum
        k = k - 1
   6    continue
      return
      end
      subroutine nroc (n, ic, ia, ja, a, jar, ar, p, flag)
clll. optimize
c
c       ----------------------------------------------------------------
c
c               yale sparse matrix package - nonsymmetric codes
c                    solving the system of equations mx = b
c
c    i.   calling sequences
c         the coefficient matrix can be processed by an ordering routine
c    (e.g., to reduce fillin or ensure numerical stability) before using
c    the remaining subroutines.  if no reordering is done, then set
c    r(i) = c(i) = ic(i) = i  for i=1,...,n.  if an ordering subroutine
c    is used, then nroc should be used to reorder the coefficient matrix
c    the calling sequence is --
c        (       (matrix ordering))
c        (nroc   (matrix reordering))
c         nsfc   (symbolic factorization to determine where fillin will
c                  occur during numeric factorization)
c         nnfc   (numeric factorization into product ldu of unit lower
c                  triangular matrix l, diagonal matrix d, and unit
c                  upper triangular matrix u, and solution of linear
c                  system)
c         nnsc   (solution of linear system for additional right-hand
c                  side using ldu factorization from nnfc)
c    (if only one system of equations is to be solved, then the
c    subroutine trk should be used.)
c
c    ii.  storage of sparse matrices
c         the nonzero entries of the coefficient matrix m are stored
c    row-by-row in the array a.  to identify the individual nonzero
c    entries in each row, we need to know in which column each entry
c    lies.  the column indices which correspond to the nonzero entries
c    of m are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
c    ja(k) = j.  in addition, we need to know where each row starts and
c    how long it is.  the index positions in ja and a where the rows of
c    m begin are stored in the array ia.  i.e., if m(i,j) is the first
c    (leftmost) entry in the i-th row and  a(k) = m(i,j),  then
c    ia(i) = k.  moreover, the index in ja and a of the first location
c    following the last element in the last row is stored in ia(n+1).
c    thus, the number of entries in the i-th row is given by
c    ia(i+1) - ia(i),  the nonzero entries of the i-th row are stored
c    consecutively in
c            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
c    and the corresponding column indices are stored consecutively in
c            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
c    for example, the 5 by 5 matrix
c                ( 1. 0. 2. 0. 0.)
c                ( 0. 3. 0. 0. 0.)
c            m = ( 0. 4. 5. 6. 0.)
c                ( 0. 0. 0. 7. 0.)
c                ( 0. 0. 0. 8. 9.)
c    would be stored as
c               - 1  2  3  4  5  6  7  8  9
c            ---+--------------------------
c            ia - 1  3  4  7  8 10
c            ja - 1  3  2  2  3  4  4  4  5
c             a - 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
c
c         the strict upper (lower) triangular portion of the matrix
c    u (l) is stored in a similar fashion using the arrays  iu, ju, u
c    (il, jl, l)  except that an additional array iju (ijl) is used to
c    compress storage of ju (jl) by allowing some sequences of column
c    (row) indices to used for more than one row (column)  (n.b., l is
c    stored by columns).  iju(k) (ijl(k)) points to the starting
c    location in ju (jl) of entries for the kth row (column).
c    compression in ju (jl) occurs in two ways.  first, if a row
c    (column) i was merged into the current row (column) k, and the
c    number of elements merged in from (the tail portion of) row
c    (column) i is the same as the final length of row (column) k, then
c    the kth row (column) and the tail of row (column) i are identical
c    and iju(k) (ijl(k)) points to the start of the tail.  second, if
c    some tail portion of the (k-1)st row (column) is identical to the
c    head of the kth row (column), then iju(k) (ijl(k)) points to the
c    start of that tail portion.  for example, the nonzero structure of
c    the strict upper triangular part of the matrix
c            d 0 x x x
c            0 d 0 x x
c            0 0 d x 0
c            0 0 0 d x
c            0 0 0 0 d
c    would be represented as
c                - 1 2 3 4 5 6
c            ----+------------
c             iu - 1 4 6 7 8 8
c             ju - 3 4 5 4
c            iju - 1 2 4 3           .
c    the diagonal entries of l and u are assumed to be equal to one and
c    are not stored.  the array d contains the reciprocals of the
c    diagonal entries of the matrix d.
c
c    iii. additional storage savings
c         in nsfc, r and ic can be the same array in the calling
c    sequence if no reordering of the coefficient matrix has been done.
c         in nnfc, r, c, and ic can all be the same array if no
c    reordering has been done.  if only the rows have been reordered,
c    then c and ic can be the same array.  if the row and column
c    orderings are the same, then r and c can be the same array.  z and
c    row can be the same array.
c         in nnsc or nntc, r and c can be the same array if no
c    reordering has been done or if the row and column orderings are the
c    same.  z and b can be the same array.  however, then b will be
c    destroyed.
c
c    iv.  parameters
c         following is a list of parameters to the programs.  names are
c    uniform among the various subroutines.  class abbreviations are --
c       n - integer variable
c       f - real variable
c       v - supplies a value to a subroutine
c       r - returns a result from a subroutine
c       i - used internally by a subroutine
c       a - array
c
c class - parameter
c ------+----------
c fva   - a     - nonzero entries of the coefficient matrix m, stored
c       -           by rows.
c       -           size = number of nonzero entries in m.
c fva   - b     - right-hand side b.
c       -           size = n.
c nva   - c     - ordering of the columns of m.
c       -           size = n.
c fvra  - d     - reciprocals of the diagonal entries of the matrix d.
c       -           size = n.
c nr    - flag  - error flag.  values and their meanings are --
c       -            0     no errors detected
c       -            n+k   null row in a  --  row = k
c       -           2n+k   duplicate entry in a  --  row = k
c       -           3n+k   insufficient storage for jl  --  row = k
c       -           4n+1   insufficient storage for l
c       -           5n+k   null pivot  --  row = k
c       -           6n+k   insufficient storage for ju  --  row = k
c       -           7n+1   insufficient storage for u
c       -           8n+k   zero pivot  --  row = k
c nva   - ia    - pointers to delimit the rows of a.
c       -           size = n+1.
c nvra  - ijl   - pointers to the first element in each column in jl,
c       -           used to compress storage in jl.
c       -           size = n.
c nvra  - iju   - pointers to the first element in each row in ju, used
c       -           to compress storage in ju.
c       -           size = n.
c nvra  - il    - pointers to delimit the columns of l.
c       -           size = n+1.
c nvra  - iu    - pointers to delimit the rows of u.
c       -           size = n+1.
c nva   - ja    - column numbers corresponding to the elements of a.
c       -           size = size of a.
c nvra  - jl    - row numbers corresponding to the elements of l.
c       -           size = jlmax.
c nv    - jlmax - declared dimension of jl.  jlmax must be larger than
c       -           the number of nonzeros in the strict lower triangle
c       -           of m plus fillin minus compression.
c nvra  - ju    - column numbers corresponding to the elements of u.
c       -           size = jumax.
c nv    - jumax - declared dimension of ju.  jumax must be larger than
c       -           the number of nonzeros in the strict upper triangle
c       -           of m plus fillin minus compression.
c fvra  - l     - nonzero entries in the strict lower triangular portion
c       -           of the matrix l, stored by columns.
c       -           size = lmax.
c nv    - lmax  - declared dimension of l.  lmax must be larger than
c       -           the number of nonzeros in the strict lower triangle
c       -           of m plus fillin  (il(n+1)-1 after nsfc).
c nv    - n     - number of variables/equations.
c nva   - r     - ordering of the rows of m.
c       -           size = n.
c fvra  - u     - nonzero entries in the strict upper triangular portion
c       -           of the matrix u, stored by rows.
c       -           size = umax.
c nv    - umax  - declared dimension of u.  umax must be larger than
c       -           the number of nonzeros in the strict upper triangle
c       -           of m plus fillin  (iu(n+1)-1 after nsfc).
c fra   - z     - solution x.
c       -           size = n.
c
c       ----------------------------------------------------------------
c
c*** subroutine nroc
c*** reorders rows of a, leaving row order unchanged
c
c
c       input parameters.. n, ic, ia, ja, a
c       output parameters.. ja, a, flag
c
c       parameters used internally..
c nia   - p     - at the kth step, p is a linked list of the reordered
c       -           column indices of the kth row of a.  p(n+1) points
c       -           to the first entry in the list.
c       -           size = n+1.
c nia   - jar   - at the kth step,jar contains the elements of the
c       -           reordered column indices of a.
c       -           size = n.
c fia   - ar    - at the kth step, ar contains the elements of the
c       -           reordered row of a.
c       -           size = n.
c
      integer  ic(1), ia(1), ja(1), jar(1), p(1), flag
      KPP_REAL  a(1), ar(1)
c
c  ******  for each nonempty row  *******************************
      do 5 k=1,n
        jmin = ia(k)
        jmax = ia(k+1) - 1
        if(jmin .gt. jmax) go to 5
        p(n+1) = n + 1
c  ******  insert each element in the list  *********************
        do 3 j=jmin,jmax
          newj = ic(ja(j))
          i = n + 1
   1      if(p(i) .ge. newj) go to 2
            i = p(i)
            go to 1
   2      if(p(i) .eq. newj) go to 102
          p(newj) = p(i)
          p(i) = newj
          jar(newj) = ja(j)
          ar(newj) = a(j)
   3      continue
c  ******  replace old row in ja and a  *************************
        i = n + 1
        do 4 j=jmin,jmax
          i = p(i)
          ja(j) = jar(i)
   4      a(j) = ar(i)
   5    continue
      flag = 0
      return
c
c ** error.. duplicate entry in a
 102  flag = n + k
      return
      end
      subroutine adjlr (n, isp, ldif)
      integer n, isp, ldif
      dimension isp(1)
c-----------------------------------------------------------------------
c this routine computes an adjustment, ldif, to the required
c integer storage space in iwk (sparse matrix work space).
c it is called only if the word length ratio is lrat = 1.
c this is to account for the possibility that the symbolic lu phase
c may require more storage than the numerical lu and solution phases.
c-----------------------------------------------------------------------
      integer ip, jlmax, jumax, lnfc, lsfc, nzlu
c
      ip = 2*n + 1
c get jlmax = ijl(n) and jumax = iju(n) (sizes of jl and ju). ----------
      jlmax = isp(ip)
      jumax = isp(ip+ip)
c nzlu = (size of l) + (size of u) = (il(n+1)-il(1)) + (iu(n+1)-iu(1)).
      nzlu = isp(n+1) - isp(1) + isp(ip+n+1) - isp(ip+1)
      lsfc = 12*n + 3 + 2*max0(jlmax,jumax)
      lnfc = 9*n + 2 + jlmax + jumax + nzlu
      ldif = max0(0, lsfc - lnfc)
      return
c----------------------- end of subroutine adjlr -----------------------
      end
      subroutine odrv
     *     (n, ia,ja,a, p,ip, nsp,isp, path, flag)
clll. optimize
c                                                                 5/2/83
c***********************************************************************
c  odrv -- driver for sparse matrix reordering routines
c***********************************************************************
c
c  description
c
c    odrv finds a minimum degree ordering of the rows and columns
c    of a matrix m stored in (ia,ja,a) format (see below).  for the
c    reordered matrix, the work and storage required to perform
c    gaussian elimination is (usually) significantly less.
c
c    note.. odrv and its subordinate routines have been modified to
c    compute orderings for general matrices, not necessarily having any
c    symmetry.  the miminum degree ordering is computed for the
c    structure of the symmetric matrix  m + m-transpose.
c    modifications to the original odrv module have been made in
c    the coding in subroutine mdi, and in the initial comments in
c    subroutines odrv and md.
c
c    if only the nonzero entries in the upper triangle of m are being
c    stored, then odrv symmetrically reorders (ia,ja,a), (optionally)
c    with the diagonal entries placed first in each row.  this is to
c    ensure that if m(i,j) will be in the upper triangle of m with
c    respect to the new ordering, then m(i,j) is stored in row i (and
c    thus m(j,i) is not stored),  whereas if m(i,j) will be in the
c    strict lower triangle of m, then m(j,i) is stored in row j (and
c    thus m(i,j) is not stored).
c
c
c  storage of sparse matrices
c
c    the nonzero entries of the matrix m are stored row-by-row in the
c    array a.  to identify the individual nonzero entries in each row,
c    we need to know in which column each entry lies.  these column
c    indices are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
c    ja(k) = j.  to identify the individual rows, we need to know where
c    each row starts.  these row pointers are stored in the array ia.
c    i.e., if m(i,j) is the first nonzero entry (stored) in the i-th row
c    and  a(k) = m(i,j),  then  ia(i) = k.  moreover, ia(n+1) points to
c    the first location following the last element in the last row.
c    thus, the number of entries in the i-th row is  ia(i+1) - ia(i),
c    the nonzero entries in the i-th row are stored consecutively in
c
c            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
c
c    and the corresponding column indices are stored consecutively in
c
c            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
c
c    since the coefficient matrix is symmetric, only the nonzero entries
c    in the upper triangle need be stored.  for example, the matrix
c
c             ( 1  0  2  3  0 )
c             ( 0  4  0  0  0 )
c         m = ( 2  0  5  6  0 )
c             ( 3  0  6  7  8 )
c             ( 0  0  0  8  9 )
c
c    could be stored as
c
c            - 1  2  3  4  5  6  7  8  9 10 11 12 13
c         ---+--------------------------------------
c         ia - 1  4  5  8 12 14
c         ja - 1  3  4  2  1  3  4  1  3  4  5  4  5
c          a - 1  2  3  4  2  5  6  3  6  7  8  8  9
c
c    or (symmetrically) as
c
c            - 1  2  3  4  5  6  7  8  9
c         ---+--------------------------
c         ia - 1  4  5  7  9 10
c         ja - 1  3  4  2  3  4  4  5  5
c          a - 1  2  3  4  5  6  7  8  9          .
c
c
c  parameters
c
c    n    - order of the matrix
c
c    ia   - integer one-dimensional array containing pointers to delimit
c           rows in ja and a.  dimension = n+1
c
c    ja   - integer one-dimensional array containing the column indices
c           corresponding to the elements of a.  dimension = number of
c           nonzero entries in (the upper triangle of) m
c
c    a    - real one-dimensional array containing the nonzero entries in
c           (the upper triangle of) m, stored by rows.  dimension =
c           number of nonzero entries in (the upper triangle of) m
c
c    p    - integer one-dimensional array used to return the permutation
c           of the rows and columns of m corresponding to the minimum
c           degree ordering.  dimension = n
c
c    ip   - integer one-dimensional array used to return the inverse of
c           the permutation returned in p.  dimension = n
c
c    nsp  - declared dimension of the one-dimensional array isp.  nsp
c           must be at least  3n+4k,  where k is the number of nonzeroes
c           in the strict upper triangle of m
c
c    isp  - integer one-dimensional array used for working storage.
c           dimension = nsp
c
c    path - integer path specification.  values and their meanings are -
c             1  find minimum degree ordering only
c             2  find minimum degree ordering and reorder symmetrically
c                  stored matrix (used when only the nonzero entries in
c                  the upper triangle of m are being stored)
c             3  reorder symmetrically stored matrix as specified by
c                  input permutation (used when an ordering has already
c                  been determined and only the nonzero entries in the
c                  upper triangle of m are being stored)
c             4  same as 2 but put diagonal entries at start of each row
c             5  same as 3 but put diagonal entries at start of each row
c
c    flag - integer error flag.  values and their meanings are -
c               0    no errors detected
c              9n+k  insufficient storage in md
c             10n+1  insufficient storage in odrv
c             11n+1  illegal path specification
c
c
c  conversion from real to KPP_REAL
c
c    change the real declarations in odrv and sro to KPP_REAL
c    declarations.
c
c-----------------------------------------------------------------------
c
      integer  ia(1), ja(1),  p(1), ip(1),  isp(1),  path,  flag,
     *   v, l, head,  tmp, q
      KPP_REAL  a(1)
      logical  dflag
c
c----initialize error flag and validate path specification
      flag = 0
      if (path.lt.1 .or. 5.lt.path)  go to 111
c
c----allocate storage and find minimum degree ordering
      if ((path-1) * (path-2) * (path-4) .ne. 0)  go to 1
        max = (nsp-n)/2
        v    = 1
        l    = v     +  max
        head = l     +  max
        next = head  +  n
        if (max.lt.n)  go to 110
c
        CALL  md
     *     (n, ia,ja, max,isp(v),isp(l), isp(head),p,ip, isp(v), flag)
        if (flag.ne.0)  go to 100
c
c----allocate storage and symmetrically reorder matrix
   1  if ((path-2) * (path-3) * (path-4) * (path-5) .ne. 0)  go to 2
        tmp = (nsp+1) -      n
        q   = tmp     - (ia(n+1)-1)
        if (q.lt.1)  go to 110
c
        dflag = path.eq.4 .or. path.eq.5
        CALL sro
     *     (n,  ip,  ia, ja, a,  isp(tmp),  isp(q),  dflag)
c
   2  return
c
c ** error -- error detected in md
 100  return
c ** error -- insufficient storage
 110  flag = 10*n + 1
      return
c ** error -- illegal path specified
 111  flag = 11*n + 1
      return
      end
      subroutine nnfc
     *     (n, r,c,ic, ia,ja,a, z, b,
     *      lmax,il,jl,ijl,l, d, umax,iu,ju,iju,u,
     *      row, tmp, irl,jrl, flag)
clll. optimize
c*** subroutine nnfc
c*** numerical ldu-factorization of sparse nonsymmetric matrix and
c      solution of system of linear equations (compressed pointer
c      storage)
c
c
c       input variables..  n, r, c, ic, ia, ja, a, b,
c                          il, jl, ijl, lmax, iu, ju, iju, umax
c       output variables.. z, l, d, u, flag
c
c       parameters used internally..
c nia   - irl,  - vectors used to find the rows of  l.  at the kth step
c nia   - jrl       of the factorization,  jrl(k)  points to the head
c       -           of a linked list in  jrl  of column indices j
c       -           such j .lt. k and  l(k,j)  is nonzero.  zero
c       -           indicates the end of the list.  irl(j)  (j.lt.k)
c       -           points to the smallest i such that i .ge. k and
c       -           l(i,j)  is nonzero.
c       -           size of each = n.
c fia   - row   - holds intermediate values in calculation of  u and l.
c       -           size = n.
c fia   - tmp   - holds new right-hand side  b*  for solution of the
c       -           equation ux = b*.
c       -           size = n.
c
c  internal variables..
c    jmin, jmax - indices of the first and last positions in a row to
c      be examined.
c    sum - used in calculating  tmp.
c
      integer rk,umax
      integer  r(1), c(1), ic(1), ia(1), ja(1), il(1), jl(1), ijl(1)
      integer  iu(1), ju(1), iju(1), irl(1), jrl(1), flag
      KPP_REAL  a(1), l(1), d(1), u(1), z(1), b(1), row(1)
      KPP_REAL  tmp(1), lki, sum, dk
c
c  ******  initialize pointers and test storage  ***********************
      if(il(n+1)-1 .gt. lmax) go to 104
      if(iu(n+1)-1 .gt. umax) go to 107
      do 1 k=1,n
        irl(k) = il(k)
        jrl(k) = 0
   1    continue
c
c  ******  for each row  ***********************************************
      do 19 k=1,n
c  ******  reverse jrl and zero row where kth row of l will fill in  ***
        row(k) = 0
        i1 = 0
        if (jrl(k) .eq. 0) go to 3
        i = jrl(k)
   2    i2 = jrl(i)
        jrl(i) = i1
        i1 = i
        row(i) = 0
        i = i2
        if (i .ne. 0) go to 2
c  ******  set row to zero where u will fill in  ***********************
   3    jmin = iju(k)
        jmax = jmin + iu(k+1) - iu(k) - 1
        if (jmin .gt. jmax) go to 5
        do 4 j=jmin,jmax
   4      row(ju(j)) = 0
c  ******  place kth row of a in row  **********************************
   5    rk = r(k)
        jmin = ia(rk)
        jmax = ia(rk+1) - 1
        do 6 j=jmin,jmax
          row(ic(ja(j))) = a(j)
   6      continue
c  ******  initialize sum, and link through jrl  ***********************
        sum = b(rk)
        i = i1
        if (i .eq. 0) go to 10
c  ******  assign the kth row of l and adjust row, sum  ****************
   7      lki = -row(i)
c  ******  if l is not required, then comment out the following line  **
          l(irl(i)) = -lki
          sum = sum + lki * tmp(i)
          jmin = iu(i)
          jmax = iu(i+1) - 1
          if (jmin .gt. jmax) go to 9
          mu = iju(i) - jmin
          do 8 j=jmin,jmax
   8        row(ju(mu+j)) = row(ju(mu+j)) + lki * u(j)
   9      i = jrl(i)
          if (i .ne. 0) go to 7
c
c  ******  assign kth row of u and diagonal d, set tmp(k)  *************
  10    if (row(k) .eq. 0.0d0) go to 108
        dk = 1.0d0 / row(k)
        d(k) = dk
        tmp(k) = sum * dk
        if (k .eq. n) go to 19
        jmin = iu(k)
        jmax = iu(k+1) - 1
        if (jmin .gt. jmax)  go to 12
        mu = iju(k) - jmin
        do 11 j=jmin,jmax
  11      u(j) = row(ju(mu+j)) * dk
  12    continue
c
c  ******  update irl and jrl, keeping jrl in decreasing order  ********
        i = i1
        if (i .eq. 0) go to 18
  14    irl(i) = irl(i) + 1
        i1 = jrl(i)
        if (irl(i) .ge. il(i+1)) go to 17
        ijlb = irl(i) - il(i) + ijl(i)
        j = jl(ijlb)
  15    if (i .gt. jrl(j)) go to 16
          j = jrl(j)
          go to 15
  16    jrl(i) = jrl(j)
        jrl(j) = i
  17    i = i1
        if (i .ne. 0) go to 14
  18    if (irl(k) .ge. il(k+1)) go to 19
        j = jl(ijl(k))
        jrl(k) = jrl(j)
        jrl(j) = k
  19    continue
c
c  ******  solve  ux = tmp  by back substitution  **********************
      k = n
      do 22 i=1,n
        sum =  tmp(k)
        jmin = iu(k)
        jmax = iu(k+1) - 1
        if (jmin .gt. jmax)  go to 21
        mu = iju(k) - jmin
        do 20 j=jmin,jmax
  20      sum = sum - u(j) * tmp(ju(mu+j))
  21    tmp(k) =  sum
        z(c(k)) =  sum
  22    k = k-1
      flag = 0
      return
c
c ** error.. insufficient storage for l
 104  flag = 4*n + 1
      return
c ** error.. insufficient storage for u
 107  flag = 7*n + 1
      return
c ** error.. zero pivot
 108  flag = 8*n + k
      return
      end
      subroutine jgroup (n,ia,ja,maxg,ngrp,igp,jgp,incl,jdone,ier)
clll. optimize
      integer n, ia, ja, maxg, ngrp, igp, jgp, incl, jdone, ier
      dimension ia(1), ja(1), igp(1), jgp(n), incl(n), jdone(n)
c-----------------------------------------------------------------------
c this subroutine constructs groupings of the column indices of
c the jacobian matrix, used in the numerical evaluation of the
c jacobian by finite differences.
c
c input..
c n      = the order of the matrix.
c ia,ja  = sparse structure descriptors of the matrix by rows.
c maxg   = length of available storate in the igp array.
c
c output..
c ngrp   = number of groups.
c jgp    = array of length n containing the column indices by groups.
c igp    = pointer array of length ngrp + 1 to the locations in jgp
c          of the beginning of each group.
c ier    = error indicator.  ier = 0 if no error occurred, or 1 if
c          maxg was insufficient.
c
c incl and jdone are working arrays of length n.
c-----------------------------------------------------------------------
      integer i, j, k, kmin, kmax, ncol, ng
c
      ier = 0
      do 10 j = 1,n
 10     jdone(j) = 0
      ncol = 1
      do 60 ng = 1,maxg
        igp(ng) = ncol
        do 20 i = 1,n
 20       incl(i) = 0
        do 50 j = 1,n
c reject column j if it is already in a group.--------------------------
          if (jdone(j) .eq. 1) go to 50
          kmin = ia(j)
          kmax = ia(j+1) - 1
          do 30 k = kmin,kmax
c reject column j if it overlaps any column already in this group.------
            i = ja(k)
            if (incl(i) .eq. 1) go to 50
 30         continue
c accept column j into group ng.----------------------------------------
          jgp(ncol) = j
          ncol = ncol + 1
          jdone(j) = 1
          do 40 k = kmin,kmax
            i = ja(k)
 40         incl(i) = 1
 50       continue
c stop if this group is empty (grouping is complete).-------------------
        if (ncol .eq. igp(ng)) go to 70
 60     continue
c error return if not all columns were chosen (maxg too small).---------
      if (ncol .le. n) go to 80
      ng = maxg
 70   ngrp = ng - 1
      return
 80   ier = 1
      return
c----------------------- end of subroutine jgroup ----------------------
      end
      subroutine nsfc
     *      (n, r, ic, ia,ja, jlmax,il,jl,ijl, jumax,iu,ju,iju,
     *       q, ira,jra, irac, irl,jrl, iru,jru, flag)
clll. optimize
c*** subroutine nsfc
c*** symbolic ldu-factorization of nonsymmetric sparse matrix
c      (compressed pointer storage)
c
c
c       input variables.. n, r, ic, ia, ja, jlmax, jumax.
c       output variables.. il, jl, ijl, iu, ju, iju, flag.
c
c       parameters used internally..
c nia   - q     - suppose  m*  is the result of reordering  m.  if
c       -           processing of the ith row of  m*  (hence the ith
c       -           row of  u) is being done,  q(j)  is initially
c       -           nonzero if  m*(i,j) is nonzero (j.ge.i).  since
c       -           values need not be stored, each entry points to the
c       -           next nonzero and  q(n+1)  points to the first.  n+1
c       -           indicates the end of the list.  for example, if n=9
c       -           and the 5th row of  m*  is
c       -              0 x x 0 x 0 0 x 0
c       -           then  q  will initially be
c       -              a a a a 8 a a 10 5           (a - arbitrary).
c       -           as the algorithm proceeds, other elements of  q
c       -           are inserted in the list because of fillin.
c       -           q  is used in an analogous manner to compute the
c       -           ith column of  l.
c       -           size = n+1.
c nia   - ira,  - vectors used to find the columns of  m.  at the kth
c nia   - jra,      step of the factorization,  irac(k)  points to the
c nia   - irac      head of a linked list in  jra  of row indices i
c       -           such that i .ge. k and  m(i,k)  is nonzero.  zero
c       -           indicates the end of the list.  ira(i)  (i.ge.k)
c       -           points to the smallest j such that j .ge. k and
c       -           m(i,j)  is nonzero.
c       -           size of each = n.
c nia   - irl,  - vectors used to find the rows of  l.  at the kth step
c nia   - jrl       of the factorization,  jrl(k)  points to the head
c       -           of a linked list in  jrl  of column indices j
c       -           such j .lt. k and  l(k,j)  is nonzero.  zero
c       -           indicates the end of the list.  irl(j)  (j.lt.k)
c       -           points to the smallest i such that i .ge. k and
c       -           l(i,j)  is nonzero.
c       -           size of each = n.
c nia   - iru,  - vectors used in a manner analogous to  irl and jrl
c nia   - jru       to find the columns of  u.
c       -           size of each = n.
c
c  internal variables..
c    jlptr - points to the last position used in  jl.
c    juptr - points to the last position used in  ju.
c    jmin,jmax - are the indices in  a or u  of the first and last
c                elements to be examined in a given row.
c                for example,  jmin=ia(k), jmax=ia(k+1)-1.
c
      integer cend, qm, rend, rk, vj
      integer ia(1), ja(1), ira(1), jra(1), il(1), jl(1), ijl(1)
      integer iu(1), ju(1), iju(1), irl(1), jrl(1), iru(1), jru(1)
      integer r(1), ic(1), q(1), irac(1), flag
c
c  ******  initialize pointers  ****************************************
      np1 = n + 1
      jlmin = 1
      jlptr = 0
      il(1) = 1
      jumin = 1
      juptr = 0
      iu(1) = 1
      do 1 k=1,n
        irac(k) = 0
        jra(k) = 0
        jrl(k) = 0
   1    jru(k) = 0
c  ******  initialize column pointers for a  ***************************
      do 2 k=1,n
        rk = r(k)
        iak = ia(rk)
        if (iak .ge. ia(rk+1))  go to 101
        jaiak = ic(ja(iak))
        if (jaiak .gt. k)  go to 105
        jra(k) = irac(jaiak)
        irac(jaiak) = k
   2    ira(k) = iak
c
c  ******  for each column of l and row of u  **************************
      do 41 k=1,n
c
c  ******  initialize q for computing kth column of l  *****************
        q(np1) = np1
        luk = -1
c  ******  by filling in kth column of a  ******************************
        vj = irac(k)
        if (vj .eq. 0)  go to 5
   3      qm = np1
   4      m = qm
          qm =  q(m)
          if (qm .lt. vj)  go to 4
          if (qm .eq. vj)  go to 102
            luk = luk + 1
            q(m) = vj
            q(vj) = qm
            vj = jra(vj)
            if (vj .ne. 0)  go to 3
c  ******  link through jru  *******************************************
   5    lastid = 0
        lasti = 0
        ijl(k) = jlptr
        i = k
   6      i = jru(i)
          if (i .eq. 0)  go to 10
          qm = np1
          jmin = irl(i)
          jmax = ijl(i) + il(i+1) - il(i) - 1
          long = jmax - jmin
          if (long .lt. 0)  go to 6
          jtmp = jl(jmin)
          if (jtmp .ne. k)  long = long + 1
          if (jtmp .eq. k)  r(i) = -r(i)
          if (lastid .ge. long)  go to 7
            lasti = i
            lastid = long
c  ******  and merge the corresponding columns into the kth column  ****
   7      do 9 j=jmin,jmax
            vj = jl(j)
   8        m = qm
            qm = q(m)
            if (qm .lt. vj)  go to 8
            if (qm .eq. vj)  go to 9
              luk = luk + 1
              q(m) = vj
              q(vj) = qm
              qm = vj
   9        continue
            go to 6
c  ******  lasti is the longest column merged into the kth  ************
c  ******  see if it equals the entire kth column  *********************
  10    qm = q(np1)
        if (qm .ne. k)  go to 105
        if (luk .eq. 0)  go to 17
        if (lastid .ne. luk)  go to 11
c  ******  if so, jl can be compressed  ********************************
        irll = irl(lasti)
        ijl(k) = irll + 1
        if (jl(irll) .ne. k)  ijl(k) = ijl(k) - 1
        go to 17
c  ******  if not, see if kth column can overlap the previous one  *****
  11    if (jlmin .gt. jlptr)  go to 15
        qm = q(qm)
        do 12 j=jlmin,jlptr
          if (jl(j) - qm)  12, 13, 15
  12      continue
        go to 15
  13    ijl(k) = j
        do 14 i=j,jlptr
          if (jl(i) .ne. qm)  go to 15
          qm = q(qm)
          if (qm .gt. n)  go to 17
  14      continue
        jlptr = j - 1
c  ******  move column indices from q to jl, update vectors  ***********
  15    jlmin = jlptr + 1
        ijl(k) = jlmin
        if (luk .eq. 0)  go to 17
        jlptr = jlptr + luk
        if (jlptr .gt. jlmax)  go to 103
          qm = q(np1)
          do 16 j=jlmin,jlptr
            qm = q(qm)
  16        jl(j) = qm
  17    irl(k) = ijl(k)
        il(k+1) = il(k) + luk
c
c  ******  initialize q for computing kth row of u  ********************
        q(np1) = np1
        luk = -1
c  ******  by filling in kth row of reordered a  ***********************
        rk = r(k)
        jmin = ira(k)
        jmax = ia(rk+1) - 1
        if (jmin .gt. jmax)  go to 20
        do 19 j=jmin,jmax
          vj = ic(ja(j))
          qm = np1
  18      m = qm
          qm = q(m)
          if (qm .lt. vj)  go to 18
          if (qm .eq. vj)  go to 102
            luk = luk + 1
            q(m) = vj
            q(vj) = qm
  19      continue
c  ******  link through jrl,  ******************************************
  20    lastid = 0
        lasti = 0
        iju(k) = juptr
        i = k
        i1 = jrl(k)
  21      i = i1
          if (i .eq. 0)  go to 26
          i1 = jrl(i)
          qm = np1
          jmin = iru(i)
          jmax = iju(i) + iu(i+1) - iu(i) - 1
          long = jmax - jmin
          if (long .lt. 0)  go to 21
          jtmp = ju(jmin)
          if (jtmp .eq. k)  go to 22
c  ******  update irl and jrl, *****************************************
            long = long + 1
            cend = ijl(i) + il(i+1) - il(i)
            irl(i) = irl(i) + 1
            if (irl(i) .ge. cend)  go to 22
              j = jl(irl(i))
              jrl(i) = jrl(j)
              jrl(j) = i
  22      if (lastid .ge. long)  go to 23
            lasti = i
            lastid = long
c  ******  and merge the corresponding rows into the kth row  **********
  23      do 25 j=jmin,jmax
            vj = ju(j)
  24        m = qm
            qm = q(m)
            if (qm .lt. vj)  go to 24
            if (qm .eq. vj)  go to 25
              luk = luk + 1
              q(m) = vj
              q(vj) = qm
              qm = vj
  25        continue
          go to 21
c  ******  update jrl(k) and irl(k)  ***********************************
  26    if (il(k+1) .le. il(k))  go to 27
          j = jl(irl(k))
          jrl(k) = jrl(j)
          jrl(j) = k
c  ******  lasti is the longest row merged into the kth  ***************
c  ******  see if it equals the entire kth row  ************************
  27    qm = q(np1)
        if (qm .ne. k)  go to 105
        if (luk .eq. 0)  go to 34
        if (lastid .ne. luk)  go to 28
c  ******  if so, ju can be compressed  ********************************
        irul = iru(lasti)
        iju(k) = irul + 1
        if (ju(irul) .ne. k)  iju(k) = iju(k) - 1
        go to 34
c  ******  if not, see if kth row can overlap the previous one  ********
  28    if (jumin .gt. juptr)  go to 32
        qm = q(qm)
        do 29 j=jumin,juptr
          if (ju(j) - qm)  29, 30, 32
  29      continue
        go to 32
  30    iju(k) = j
        do 31 i=j,juptr
          if (ju(i) .ne. qm)  go to 32
          qm = q(qm)
          if (qm .gt. n)  go to 34
  31      continue
        juptr = j - 1
c  ******  move row indices from q to ju, update vectors  **************
  32    jumin = juptr + 1
        iju(k) = jumin
        if (luk .eq. 0)  go to 34
        juptr = juptr + luk
        if (juptr .gt. jumax)  go to 106
          qm = q(np1)
          do 33 j=jumin,juptr
            qm = q(qm)
  33        ju(j) = qm
  34    iru(k) = iju(k)
        iu(k+1) = iu(k) + luk
c
c  ******  update iru, jru  ********************************************
        i = k
  35      i1 = jru(i)
          if (r(i) .lt. 0)  go to 36
          rend = iju(i) + iu(i+1) - iu(i)
          if (iru(i) .ge. rend)  go to 37
            j = ju(iru(i))
            jru(i) = jru(j)
            jru(j) = i
            go to 37
  36      r(i) = -r(i)
  37      i = i1
          if (i .eq. 0)  go to 38
          iru(i) = iru(i) + 1
          go to 35
c
c  ******  update ira, jra, irac  **************************************
  38    i = irac(k)
        if (i .eq. 0)  go to 41
  39      i1 = jra(i)
          ira(i) = ira(i) + 1
          if (ira(i) .ge. ia(r(i)+1))  go to 40
          irai = ira(i)
          jairai = ic(ja(irai))
          if (jairai .gt. i)  go to 40
          jra(i) = irac(jairai)
          irac(jairai) = i
  40      i = i1
          if (i .ne. 0)  go to 39
  41    continue
c
      ijl(n) = jlptr
      iju(n) = juptr
      flag = 0
      return
c
c ** error.. null row in a
 101  flag = n + rk
      return
c ** error.. duplicate entry in a
 102  flag = 2*n + rk
      return
c ** error.. insufficient storage for jl
 103  flag = 3*n + k
      return
c ** error.. null pivot
 105  flag = 5*n + k
      return
c ** error.. insufficient storage for ju
 106  flag = 6*n + k
      return
      end
      subroutine sro
     *     (n, ip, ia,ja,a, q, r, dflag)
clll. optimize
c***********************************************************************
c  sro -- symmetric reordering of sparse symmetric matrix
c***********************************************************************
c
c  description
c
c    the nonzero entries of the matrix m are assumed to be stored
c    symmetrically in (ia,ja,a) format (i.e., not both m(i,j) and m(j,i)
c    are stored if i ne j).
c
c    sro does not rearrange the order of the rows, but does move
c    nonzeroes from one row to another to ensure that if m(i,j) will be
c    in the upper triangle of m with respect to the new ordering, then
c    m(i,j) is stored in row i (and thus m(j,i) is not stored),  whereas
c    if m(i,j) will be in the strict lower triangle of m, then m(j,i) is
c    stored in row j (and thus m(i,j) is not stored).
c
c
c  additional parameters
c
c    q     - integer one-dimensional work array.  dimension = n
c
c    r     - integer one-dimensional work array.  dimension = number of
c            nonzero entries in the upper triangle of m
c
c    dflag - logical variable.  if dflag = .true., then store nonzero
c            diagonal elements at the beginning of the row
c
c-----------------------------------------------------------------------
c
      integer  ip(1),  ia(1), ja(1),  q(1), r(1)
      KPP_REAL  a(1),  ak
      logical  dflag
c
c
c--phase 1 -- find row in which to store each nonzero
c----initialize count of nonzeroes to be stored in each row
      do 1 i=1,n
  1     q(i) = 0
c
c----for each nonzero element a(j)
      do 3 i=1,n
        jmin = ia(i)
        jmax = ia(i+1) - 1
        if (jmin.gt.jmax)  go to 3
        do 2 j=jmin,jmax
c
c--------find row (=r(j)) and column (=ja(j)) in which to store a(j) ...
          k = ja(j)
          if (ip(k).lt.ip(i))  ja(j) = i
          if (ip(k).ge.ip(i))  k = i
          r(j) = k
c
c--------... and increment count of nonzeroes (=q(r(j)) in that row
  2       q(k) = q(k) + 1
  3     continue
c
c
c--phase 2 -- find new ia and permutation to apply to (ja,a)
c----determine pointers to delimit rows in permuted (ja,a)
      do 4 i=1,n
        ia(i+1) = ia(i) + q(i)
  4     q(i) = ia(i+1)
c
c----determine where each (ja(j),a(j)) is stored in permuted (ja,a)
c----for each nonzero element (in reverse order)
      ilast = 0
      jmin = ia(1)
      jmax = ia(n+1) - 1
      j = jmax
      do 6 jdummy=jmin,jmax
        i = r(j)
        if (.not.dflag .or. ja(j).ne.i .or. i.eq.ilast)  go to 5
c
c------if dflag, then put diagonal nonzero at beginning of row
          r(j) = ia(i)
          ilast = i
          go to 6
c
c------put (off-diagonal) nonzero in last unused location in row
  5       q(i) = q(i) - 1
          r(j) = q(i)
c
  6     j = j-1
c
c
c--phase 3 -- permute (ja,a) to upper triangular form (wrt new ordering)
      do 8 j=jmin,jmax
  7     if (r(j).eq.j)  go to 8
          k = r(j)
          r(j) = r(k)
          r(k) = k
          jak = ja(k)
          ja(k) = ja(j)
          ja(j) = jak
          ak = a(k)
          a(k) = a(j)
          a(j) = ak
          go to 7
  8     continue
c
      return
      end
      subroutine md
     *     (n, ia,ja, max, v,l, head,last,next, mark, flag)
clll. optimize
c***********************************************************************
c  md -- minimum degree algorithm (based on element model)
c***********************************************************************
c
c  description
c
c    md finds a minimum degree ordering of the rows and columns of a
c    general sparse matrix m stored in (ia,ja,a) format.
c    when the structure of m is nonsymmetric, the ordering is that
c    obtained for the symmetric matrix  m + m-transpose.
c
c
c  additional parameters
c
c    max  - declared dimension of the one-dimensional arrays v and l.
c           max must be at least  n+2k,  where k is the number of
c           nonzeroes in the strict upper triangle of m + m-transpose
c
c    v    - integer one-dimensional work array.  dimension = max
c
c    l    - integer one-dimensional work array.  dimension = max
c
c    head - integer one-dimensional work array.  dimension = n
c
c    last - integer one-dimensional array used to return the permutation
c           of the rows and columns of m corresponding to the minimum
c           degree ordering.  dimension = n
c
c    next - integer one-dimensional array used to return the inverse of
c           the permutation returned in last.  dimension = n
c
c    mark - integer one-dimensional work array (may be the same as v).
c           dimension = n
c
c    flag - integer error flag.  values and their meanings are -
c             0     no errors detected
c             9n+k  insufficient storage in md
c
c
c  definitions of internal parameters
c
c    ---------+---------------------------------------------------------
c    v(s)     - value field of list entry
c    ---------+---------------------------------------------------------
c    l(s)     - link field of list entry  (0 =) end of list)
c    ---------+---------------------------------------------------------
c    l(vi)    - pointer to element list of uneliminated vertex vi
c    ---------+---------------------------------------------------------
c    l(ej)    - pointer to boundary list of active element ej
c    ---------+---------------------------------------------------------
c    head(d)  - vj =) vj head of d-list d
c             -  0 =) no vertex in d-list d
c
c
c             -                  vi uneliminated vertex
c             -          vi in ek           -       vi not in ek
c    ---------+-----------------------------+---------------------------
c    next(vi) - undefined but nonnegative   - vj =) vj next in d-list
c             -                             -  0 =) vi tail of d-list
c    ---------+-----------------------------+---------------------------
c    last(vi) - (not set until mdp)         - -d =) vi head of d-list d
c             --vk =) compute degree        - vj =) vj last in d-list
c             - ej =) vi prototype of ej    -  0 =) vi not in any d-list
c             -  0 =) do not compute degree -
c    ---------+-----------------------------+---------------------------
c    mark(vi) - mark(vk)                    - nonneg. tag .lt. mark(vk)
c
c
c             -                   vi eliminated vertex
c             -      ei active element      -           otherwise
c    ---------+-----------------------------+---------------------------
c    next(vi) - -j =) vi was j-th vertex    - -j =) vi was j-th vertex
c             -       to be eliminated      -       to be eliminated
c    ---------+-----------------------------+---------------------------
c    last(vi) -  m =) size of ei = m        - undefined
c    ---------+-----------------------------+---------------------------
c    mark(vi) - -m =) overlap count of ei   - undefined
c             -       with ek = m           -
c             - otherwise nonnegative tag   -
c             -       .lt. mark(vk)         -
c
c-----------------------------------------------------------------------
c
      integer  ia(1), ja(1),  v(1), l(1),  head(1), last(1), next(1),
     *   mark(1),  flag,  tag, dmin, vk,ek, tail
      equivalence  (vk,ek)
c
c----initialization
      tag = 0
      CALL  mdi
     *   (n, ia,ja, max,v,l, head,last,next, mark,tag, flag)
      if (flag.ne.0)  return
c
      k = 0
      dmin = 1
c
c----while  k .lt. n  do
   1  if (k.ge.n)  go to 4
c
c------search for vertex of minimum degree
   2    if (head(dmin).gt.0)  go to 3
          dmin = dmin + 1
          go to 2
c
c------remove vertex vk of minimum degree from degree list
   3    vk = head(dmin)
        head(dmin) = next(vk)
        if (head(dmin).gt.0)  last(head(dmin)) = -dmin
c
c------number vertex vk, adjust tag, and tag vk
        k = k+1
        next(vk) = -k
        last(ek) = dmin - 1
        tag = tag + last(ek)
        mark(vk) = tag
c
c------form element ek from uneliminated neighbors of vk
        CALL  mdm
     *     (vk,tail, v,l, last,next, mark)
c
c------purge inactive elements and do mass elimination
        CALL  mdp
     *     (k,ek,tail, v,l, head,last,next, mark)
c
c------update degrees of uneliminated vertices in ek
        CALL  mdu
     *     (ek,dmin, v,l, head,last,next, mark)
c
        go to 1
c
c----generate inverse permutation from permutation
   4  do 5 k=1,n
        next(k) = -next(k)
   5    last(next(k)) = k
c
      return
      end
      subroutine mdi
     *     (n, ia,ja, max,v,l, head,last,next, mark,tag, flag)
clll. optimize
c***********************************************************************
c  mdi -- initialization
c***********************************************************************
      integer  ia(1), ja(1),  v(1), l(1),  head(1), last(1), next(1),
     *   mark(1), tag,  flag,  sfs, vi,dvi, vj
c
c----initialize degrees, element lists, and degree lists
      do 1 vi=1,n
        mark(vi) = 1
        l(vi) = 0
   1    head(vi) = 0
      sfs = n+1
c
c----create nonzero structure
c----for each nonzero entry a(vi,vj)
      do 6 vi=1,n
        jmin = ia(vi)
        jmax = ia(vi+1) - 1
        if (jmin.gt.jmax)  go to 6
        do 5 j=jmin,jmax
          vj = ja(j)
          if (vj-vi) 2, 5, 4
c
c------if a(vi,vj) is in strict lower triangle
c------check for previous occurrence of a(vj,vi)
   2      lvk = vi
          kmax = mark(vi) - 1
          if (kmax .eq. 0) go to 4
          do 3 k=1,kmax
            lvk = l(lvk)
            if (v(lvk).eq.vj) go to 5
   3        continue
c----for unentered entries a(vi,vj)
   4        if (sfs.ge.max)  go to 101
c
c------enter vj in element list for vi
            mark(vi) = mark(vi) + 1
            v(sfs) = vj
            l(sfs) = l(vi)
            l(vi) = sfs
            sfs = sfs+1
c
c------enter vi in element list for vj
            mark(vj) = mark(vj) + 1
            v(sfs) = vi
            l(sfs) = l(vj)
            l(vj) = sfs
            sfs = sfs+1
   5      continue
   6    continue
c
c----create degree lists and initialize mark vector
      do 7 vi=1,n
        dvi = mark(vi)
        next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        nextvi = next(vi)
        if (nextvi.gt.0)  last(nextvi) = vi
   7    mark(vi) = tag
c
      return
c
c ** error-  insufficient storage
 101  flag = 9*n + vi
      return
      end
      subroutine mdm
     *     (vk,tail, v,l, last,next, mark)
clll. optimize
c***********************************************************************
c  mdm -- form element from uneliminated neighbors of vk
c***********************************************************************
      integer  vk, tail,  v(1), l(1),   last(1), next(1),   mark(1),
     *   tag, s,ls,vs,es, b,lb,vb, blp,blpmax
      equivalence  (vs, es)
c
c----initialize tag and list of uneliminated neighbors
      tag = mark(vk)
      tail = vk
c
c----for each vertex/element vs/es in element list of vk
      ls = l(vk)
   1  s = ls
      if (s.eq.0)  go to 5
        ls = l(s)
        vs = v(s)
        if (next(vs).lt.0)  go to 2
c
c------if vs is uneliminated vertex, then tag and append to list of
c------uneliminated neighbors
          mark(vs) = tag
          l(tail) = s
          tail = s
          go to 4
c
c------if es is active element, then ...
c--------for each vertex vb in boundary list of element es
   2      lb = l(es)
          blpmax = last(es)
          do 3 blp=1,blpmax
            b = lb
            lb = l(b)
            vb = v(b)
c
c----------if vb is untagged vertex, then tag and append to list of
c----------uneliminated neighbors
            if (mark(vb).ge.tag)  go to 3
              mark(vb) = tag
              l(tail) = b
              tail = b
   3        continue
c
c--------mark es inactive
          mark(es) = tag
c
   4    go to 1
c
c----terminate list of uneliminated neighbors
   5  l(tail) = 0
c
      return
      end
      subroutine mdp
     *     (k,ek,tail, v,l, head,last,next, mark)
clll. optimize
c***********************************************************************
c  mdp -- purge inactive elements and do mass elimination
c***********************************************************************
      integer  ek, tail,  v(1), l(1),  head(1), last(1), next(1),
     *   mark(1),  tag, free, li,vi,lvi,evi, s,ls,es, ilp,ilpmax
c
c----initialize tag
      tag = mark(ek)
c
c----for each vertex vi in ek
      li = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 12
      do 11 ilp=1,ilpmax
        i = li
        li = l(i)
        vi = v(li)
c
c------remove vi from degree list
        if (last(vi).eq.0)  go to 3
          if (last(vi).gt.0)  go to 1
            head(-last(vi)) = next(vi)
            go to 2
   1        next(last(vi)) = next(vi)
   2      if (next(vi).gt.0)  last(next(vi)) = last(vi)
c
c------remove inactive items from element list of vi
   3    ls = vi
   4    s = ls
        ls = l(s)
        if (ls.eq.0)  go to 6
          es = v(ls)
          if (mark(es).lt.tag)  go to 5
            free = ls
            l(s) = l(ls)
            ls = s
   5      go to 4
c
c------if vi is interior vertex, then remove from list and eliminate
   6    lvi = l(vi)
        if (lvi.ne.0)  go to 7
          l(i) = l(li)
          li = i
c
          k = k+1
          next(vi) = -k
          last(ek) = last(ek) - 1
          go to 11
c
c------else ...
c--------classify vertex vi
   7      if (l(lvi).ne.0)  go to 9
            evi = v(lvi)
            if (next(evi).ge.0)  go to 9
              if (mark(evi).lt.0)  go to 8
c
c----------if vi is prototype vertex, then mark as such, initialize
c----------overlap count for corresponding element, and move vi to end
c----------of boundary list
                last(vi) = evi
                mark(evi) = -1
                l(tail) = li
                tail = li
                l(i) = l(li)
                li = i
                go to 10
c
c----------else if vi is duplicate vertex, then mark as such and adjust
c----------overlap count for corresponding element
   8            last(vi) = 0
                mark(evi) = mark(evi) - 1
                go to 10
c
c----------else mark vi to compute degree
   9            last(vi) = -ek
c
c--------insert ek in element list of vi
  10      v(free) = ek
          l(free) = l(vi)
          l(vi) = free
  11    continue
c
c----terminate boundary list
  12  l(tail) = 0
c
      return
      end
      subroutine mdu
     *     (ek,dmin, v,l, head,last,next, mark)
clll. optimize
c***********************************************************************
c  mdu -- update degrees of uneliminated vertices in ek
c***********************************************************************
      integer  ek, dmin,  v(1), l(1),  head(1), last(1), next(1),
     *   mark(1),  tag, vi,evi,dvi, s,vs,es, b,vb, ilp,ilpmax,
     *   blp,blpmax
      equivalence  (vs, es)
c
c----initialize tag
      tag = mark(ek) - last(ek)
c
c----for each vertex vi in ek
      i = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 11
      do 10 ilp=1,ilpmax
        i = l(i)
        vi = v(i)
        if (last(vi))  1, 10, 8
c
c------if vi neither prototype nor duplicate vertex, then merge elements
c------to compute degree
   1      tag = tag + 1
          dvi = last(ek)
c
c--------for each vertex/element vs/es in element list of vi
          s = l(vi)
   2      s = l(s)
          if (s.eq.0)  go to 9
            vs = v(s)
            if (next(vs).lt.0)  go to 3
c
c----------if vs is uneliminated vertex, then tag and adjust degree
              mark(vs) = tag
              dvi = dvi + 1
              go to 5
c
c----------if es is active element, then expand
c------------check for outmatched vertex
   3          if (mark(es).lt.0)  go to 6
c
c------------for each vertex vb in es
              b = es
              blpmax = last(es)
              do 4 blp=1,blpmax
                b = l(b)
                vb = v(b)
c
c--------------if vb is untagged, then tag and adjust degree
                if (mark(vb).ge.tag)  go to 4
                  mark(vb) = tag
                  dvi = dvi + 1
   4            continue
c
   5        go to 2
c
c------else if vi is outmatched vertex, then adjust overlaps but do not
c------compute degree
   6      last(vi) = 0
          mark(es) = mark(es) - 1
   7      s = l(s)
          if (s.eq.0)  go to 10
            es = v(s)
            if (mark(es).lt.0)  mark(es) = mark(es) - 1
            go to 7
c
c------else if vi is prototype vertex, then calculate degree by
c------inclusion/exclusion and reset overlap count
   8      evi = last(vi)
          dvi = last(ek) + last(evi) + mark(evi)
          mark(evi) = 0
c
c------insert vi in appropriate degree list
   9    next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        if (next(vi).gt.0)  last(next(vi)) = vi
        if (dvi.lt.dmin)  dmin = dvi
c
  10    continue
c
  11  return
      end
      KPP_REAL FUNCTION D1MACH (IDUM)
      INTEGER IDUM
C-----------------------------------------------------------------------
C This routine computes the unit roundoff of the machine.
C This is defined as the smallest positive machine number
C u such that  1.0 + u .ne. 1.0
C
C Subroutines/functions called by D1MACH.. None
C-----------------------------------------------------------------------
      KPP_REAL U, COMP
      U = 1.0D0
 10   U = U*0.5D0
      COMP = 1.0D0 + U
      IF (COMP .NE. 1.0D0) GO TO 10
      D1MACH = U*2.0D0
      RETURN
C----------------------- End of Function D1MACH ------------------------
      END

        SUBROUTINE FUNC_CHEM (N, T, V, FCT)
	IMPLICIT NONE
        INCLUDE 'KPP_ROOT_Parameters.h'
        INCLUDE 'KPP_ROOT_Global.h'
        KPP_REAL V(NVAR), FCT(NVAR)
	KPP_REAL T,TOLD
	INTEGER N
        TOLD = TIME
        TIME = T
        CALL Update_SUN()
        CALL Update_RCONST()
        TIME = TOLD
        CALL Fun(V, FIX, RCONST, FCT)
        RETURN
        END

        SUBROUTINE JAC_CHEM (N, T, V, JV, j, ian, jan)
	IMPLICIT NONE
        INCLUDE 'KPP_ROOT_Parameters.h'
        INCLUDE 'KPP_ROOT_Global.h'
        KPP_REAL V(NVAR), JV(NVAR,NVAR)
	KPP_REAL T,TOLD
        INTEGER N, j, ian(1), jan(1), ii, jj
        TOLD = TIME
        TIME = T
        CALL Update_SUN()
        CALL Update_RCONST()
        TIME = TOLD

        DO ii=1,NVAR
          DO jj=1,NVAR
              JV(ii,jj) = 0.D0
          END DO
        END DO
        call Jac(V, FIX, RCONST, JV)

        RETURN
        END

