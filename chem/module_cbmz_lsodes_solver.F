!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! CBMZ module: see module_cbmz.F for references and terms of use
!**********************************************************************************  

      module module_cbmz_lsodes_solver

!-----------------------------------------------------------------------
! 08-feb-2004 rce - this file contains a significantly modified
!	version of the 11-oct-1994 netlib lsodes code
!	and associated linpack routines
!   converted to lowercase and fortran90
!   converted to a module
!   integer variables used to store characters for error messages
!       changed to character variables
!   ruserpar, nruserpar, iuserpar, niuserpar argument added -
!	they are "user parameters" that are passed through to "subroutine f"
!-----------------------------------------------------------------------
! 18-mar-2006 rce - 
!   encountering a situation with overflow in function vnorm,
!	when called from lsodes_solver after label 160
!   first, tried to modify the vnorm code so that it would
!	scale the v(i)*w(i) when doing sum-of-squares.
!	Seemed like a good idea, but this just caused problems elsewhere
!   second, added iok_vnorm coding as a bandaid
!	in vnorm, if any v(i)*w(i) > 1.0e18, then vnorm
!	    is set to 1.0e18 and iok_vnorm to -1
!       in lsodes_solver, after vnorm call near label 160,
!	    iok_vnorm is tested, and "-1" causes a return
!	    with istate=-901
!       elsewhere in lsodes_solver, before each return,
!	    iok_vnorm is tested, and "-1" causes istate=-91x
!-----------------------------------------------------------------------
! 18-mar-2006 rce - 
!   subr r1mach - replaced the integer data statements used to
!	define rmach(1:5) with real*4 data statements
!	to avoid possible problems on mpp2
!     also added code to define rmach(1:5) using the
!	tiny, huge, spacing, epsilon, & log10 intrinsic functions,
!       BUT this code is currently commented out
!-----------------------------------------------------------------------


      contains


!ZZ
!
! Obtained Oct 11, 1994 from ODEPACK in NETLIB by RDS
      subroutine lsodes_solver (   &
                  f, neq, y, t, tout, itol, rtol, atol, itask,   &
                  istate, iopt, rwork, lrw, iwork, liw, jac, mf,   &
                  ruserpar, nruserpar, iuserpar, niuserpar )
      external f, jac
      integer neq, itol, itask, istate, iopt, lrw, iwork, liw, mf
      integer nruserpar, iuserpar, niuserpar
      real y, t, tout, rtol, atol, rwork
      real ruserpar
!jdf  dimension neq(1), y(1), rtol(1), atol(1), rwork(lrw), iwork(liw)
      dimension neq(*), y(*), rtol(*), atol(*), rwork(lrw), iwork(liw)
      dimension ruserpar(nruserpar), iuserpar(niuserpar)
!-----------------------------------------------------------------------
! this is the march 30, 1987 version of
! lsodes.. livermore solver for ordinary differential equations
!          with general sparse jacobian matrices.
! this version is in single precision.
!
! lsodes solves the initial value problem for stiff or nonstiff
! systems of first order ode-s,
!     dy/dt = f(t,y) ,  or, in component form,
!     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq).
! lsodes is a variant of the lsode package, and is intended for
! problems in which the jacobian matrix df/dy has an arbitrary
! sparse structure (when the problem is stiff).
!
! authors..      alan c. hindmarsh,
!                computing and mathematics research division, l-316
!                lawrence livermore national laboratory
!                livermore, ca 94550.
!
! and            andrew h. sherman
!                j. s. nolen and associates
!                houston, tx 77084
!-----------------------------------------------------------------------
! references..
! 1.  alan c. hindmarsh,  odepack, a systematized collection of ode
!     solvers, in scientific computing, r. s. stepleman et al. (eds.),
!     north-holland, amsterdam, 1983, pp. 55-64.
!
! 2.  s. c. eisenstat, m. c. gursky, m. h. schultz, and a. h. sherman,
!     yale sparse matrix package.. i. the symmetric codes,
!     int. j. num. meth. eng., 18 (1982), pp. 1145-1151.
!
! 3.  s. c. eisenstat, m. c. gursky, m. h. schultz, and a. h. sherman,
!     yale sparse matrix package.. ii. the nonsymmetric codes,
!     research report no. 114, dept. of computer sciences, yale
!     university, 1977.
!-----------------------------------------------------------------------
! summary of usage.
!
! communication between the user and the lsodes package, for normal
! situations, is summarized here.  this summary describes only a subset
! of the full set of options available.  see the full description for
! details, including optional communication, nonstandard options,
! and instructions for special situations.  see also the example
! problem (with program and output) following this summary.
!
! a. first provide a subroutine of the form..
!               subroutine f (neq, t, y, ydot)
!               dimension y(neq), ydot(neq)
! which supplies the vector function f by loading ydot(i) with f(i).
!
! b. next determine (or guess) whether or not the problem is stiff.
! stiffness occurs when the jacobian matrix df/dy has an eigenvalue
! whose real part is negative and large in magnitude, compared to the
! reciprocal of the t span of interest.  if the problem is nonstiff,
! use a method flag mf = 10.  if it is stiff, there are two standard
! for the method flag, mf = 121 and mf = 222.  in both cases, lsodes
! requires the jacobian matrix in some form, and it treats this matrix
! in general sparse form, with sparsity structure determined internally.
! (for options where the user supplies the sparsity structure, see
! the full description of mf below.)
!
! c. if the problem is stiff, you are encouraged to supply the jacobian
! directly (mf = 121), but if this is not feasible, lsodes will
! compute it internally by difference quotients (mf = 222).
! if you are supplying the jacobian, provide a subroutine of the form..
!               subroutine jac (neq, t, y, j, ian, jan, pdj)
!               dimension y(1), ian(1), jan(1), pdj(1)
! here neq, t, y, and j are input arguments, and the jac routine is to
! load the array pdj (of length neq) with the j-th column of df/dy.
! i.e., load pdj(i) with df(i)/dy(j) for all relevant values of i.
! the arguments ian and jan should be ignored for normal situations.
! lsodes will call the jac routine with j = 1,2,...,neq.
! only nonzero elements need be loaded.  usually, a crude approximation
! to df/dy, possibly with fewer nonzero elements, will suffice.
!
! d. write a main program which calls subroutine lsodes once for
! each point at which answers are desired.  this should also provide
! for possible use of logical unit 6 for output of error messages
! by lsodes.  on the first call to lsodes, supply arguments as follows..
! f      = name of subroutine for right-hand side vector f.
!          this name must be declared external in calling program.
! neq    = number of first order ode-s.
! y      = array of initial values, of length neq.
! t      = the initial value of the independent variable.
! tout   = first point where output is desired (.ne. t).
! itol   = 1 or 2 according as atol (below) is a scalar or array.
! rtol   = relative tolerance parameter (scalar).
! atol   = absolute tolerance parameter (scalar or array).
!          the estimated local error in y(i) will be controlled so as
!          to be roughly less (in magnitude) than
!             ewt(i) = rtol*abs(y(i)) + atol     if itol = 1, or
!             ewt(i) = rtol*abs(y(i)) + atol(i)  if itol = 2.
!          thus the local error test passes if, in each component,
!          either the absolute error is less than atol (or atol(i)),
!          or the relative error is less than rtol.
!          use rtol = 0.0 for pure absolute error control, and
!          use atol = 0.0 (or atol(i) = 0.0) for pure relative error
!          control.  caution.. actual (global) errors may exceed these
!          local tolerances, so choose them conservatively.
! itask  = 1 for normal computation of output values of y at t = tout.
! istate = integer flag (input and output).  set istate = 1.
! iopt   = 0 to indicate no optional inputs used.
! rwork  = real work array of length at least..
!             20 + 16*neq            for mf = 10,
!             20 + (2 + 1./lenrat)*nnz + (11 + 9./lenrat)*neq
!                                    for mf = 121 or 222,
!          where..
!          nnz    = the number of nonzero elements in the sparse
!                   jacobian (if this is unknown, use an estimate), and
!          lenrat = the real to integer wordlength ratio (usually 1 in
!                   single precision and 2 in double precision).
!          in any case, the required size of rwork cannot generally
!          be predicted in advance if mf = 121 or 222, and the value
!          above is a rough estimate of a crude lower bound.  some
!          experimentation with this size may be necessary.
!          (when known, the correct required length is an optional
!          output, available in iwork(17).)
! lrw    = declared length of rwork (in user-s dimension).
! iwork  = integer work array of length at least 30.
! liw    = declared length of iwork (in user-s dimension).
! jac    = name of subroutine for jacobian matrix (mf = 121).
!          if used, this name must be declared external in calling
!          program.  if not used, pass a dummy name.
! mf     = method flag.  standard values are..
!          10  for nonstiff (adams) method, no jacobian used.
!          121 for stiff (bdf) method, user-supplied sparse jacobian.
!          222 for stiff method, internally generated sparse jacobian.
! note that the main program must declare arrays y, rwork, iwork,
! and possibly atol.
!
! e. the output from the first call (or any call) is..
!      y = array of computed values of y(t) vector.
!      t = corresponding value of independent variable (normally tout).
! istate = 2  if lsodes was successful, negative otherwise.
!          -1 means excess work done on this call (perhaps wrong mf).
!          -2 means excess accuracy requested (tolerances too small).
!          -3 means illegal input detected (see printed message).
!          -4 means repeated error test failures (check all inputs).
!          -5 means repeated convergence failures (perhaps bad jacobian
!             supplied or wrong choice of mf or tolerances).
!          -6 means error weight became zero during problem. (solution
!             component i vanished, and atol or atol(i) = 0.)
!          -7 means a fatal error return flag came from the sparse
!             solver cdrv by way of prjs or slss.  should never happen.
!          a return with istate = -1, -4, or -5 may result from using
!          an inappropriate sparsity structure, one that is quite
!          different from the initial structure.  consider calling
!          lsodes again with istate = 3 to force the structure to be
!          reevaluated.  see the full description of istate below.
!
! f. to continue the integration after a successful return, simply
! reset tout and call lsodes again.  no other parameters need be reset.
!
!-----------------------------------------------------------------------
! example problem.
!
! the following is a simple example problem, with the coding
! needed for its solution by lsodes.  the problem is from chemical
! kinetics, and consists of the following 12 rate equations..
!    dy1/dt  = -rk1*y1
!    dy2/dt  = rk1*y1 + rk11*rk14*y4 + rk19*rk14*y5
!                - rk3*y2*y3 - rk15*y2*y12 - rk2*y2
!    dy3/dt  = rk2*y2 - rk5*y3 - rk3*y2*y3 - rk7*y10*y3
!                + rk11*rk14*y4 + rk12*rk14*y6
!    dy4/dt  = rk3*y2*y3 - rk11*rk14*y4 - rk4*y4
!    dy5/dt  = rk15*y2*y12 - rk19*rk14*y5 - rk16*y5
!    dy6/dt  = rk7*y10*y3 - rk12*rk14*y6 - rk8*y6
!    dy7/dt  = rk17*y10*y12 - rk20*rk14*y7 - rk18*y7
!    dy8/dt  = rk9*y10 - rk13*rk14*y8 - rk10*y8
!    dy9/dt  = rk4*y4 + rk16*y5 + rk8*y6 + rk18*y7
!    dy10/dt = rk5*y3 + rk12*rk14*y6 + rk20*rk14*y7
!                + rk13*rk14*y8 - rk7*y10*y3 - rk17*y10*y12
!                - rk6*y10 - rk9*y10
!    dy11/dt = rk10*y8
!    dy12/dt = rk6*y10 + rk19*rk14*y5 + rk20*rk14*y7
!                - rk15*y2*y12 - rk17*y10*y12
!
! with rk1 = rk5 = 0.1,  rk4 = rk8 = rk16 = rk18 = 2.5,
!      rk10 = 5.0,  rk2 = rk6 = 10.0,  rk14 = 30.0,
!      rk3 = rk7 = rk9 = rk11 = rk12 = rk13 = rk19 = rk20 = 50.0,
!      rk15 = rk17 = 100.0.
!
! the t interval is from 0 to 1000, and the initial conditions
! are y1 = 1, y2 = y3 = ... = y12 = 0.  the problem is stiff.
!
! the following coding solves this problem with lsodes, using mf = 121
! and printing results at t = .1, 1., 10., 100., 1000.  it uses
! itol = 1 and mixed relative/absolute tolerance controls.
! during the run and at the end, statistical quantities of interest
! are printed (see optional outputs in the full description below).
!
!     external fex, jex
!     dimension y(12), rwork(500), iwork(30)
!     data lrw/500/, liw/30/
!     neq = 12
!     do 10 i = 1,neq
! 10    y(i) = 0.0e0
!     y(1) = 1.0e0
!     t = 0.0e0
!     tout = 0.1e0
!     itol = 1
!     rtol = 1.0e-4
!     atol = 1.0e-6
!     itask = 1
!     istate = 1
!     iopt = 0
!     mf = 121
!     do 40 iout = 1,5
!       call lsodes (fex, neq, y, t, tout, itol, rtol, atol,
!    1     itask, istate, iopt, rwork, lrw, iwork, liw, jex, mf)
!       write(6,30)t,iwork(11),rwork(11),(y(i),i=1,neq)
! 30    format(//7h at t =,e11.3,4x,
!    1    12h no. steps =,i5,4x,12h last step =,e11.3/
!    2    13h  y array =  ,4e14.5/13x,4e14.5/13x,4e14.5)
!       if (istate .lt. 0) go to 80
!       tout = tout*10.0e0
! 40    continue
!     lenrw = iwork(17)
!     leniw = iwork(18)
!     nst = iwork(11)
!     nfe = iwork(12)
!     nje = iwork(13)
!     nlu = iwork(21)
!     nnz = iwork(19)
!     nnzlu = iwork(25) + iwork(26) + neq
!     write (6,70) lenrw,leniw,nst,nfe,nje,nlu,nnz,nnzlu
! 70  format(//22h required rwork size =,i4,15h   iwork size =,i4/
!    1   12h no. steps =,i4,12h   no. f-s =,i4,12h   no. j-s =,i4,
!    2   13h   no. lu-s =,i4/23h no. of nonzeros in j =,i5,
!    3   26h   no. of nonzeros in lu =,i5)
!     stop
! 80  write(6,90)istate
! 90  format(///22h error halt.. istate =,i3)
!     stop
!     end
!
!     subroutine fex (neq, t, y, ydot)
!     real t, y, ydot
!     real rk1, rk2, rk3, rk4, rk5, rk6, rk7, rk8, rk9,
!    1   rk10, rk11, rk12, rk13, rk14, rk15, rk16, rk17
!     dimension y(12), ydot(12)
!     data rk1/0.1e0/, rk2/10.0e0/, rk3/50.0e0/, rk4/2.5e0/, rk5/0.1e0/,
!    1   rk6/10.0e0/, rk7/50.0e0/, rk8/2.5e0/, rk9/50.0e0/, rk10/5.0e0/,
!    2   rk11/50.0e0/, rk12/50.0e0/, rk13/50.0e0/, rk14/30.0e0/,
!    3   rk15/100.0e0/, rk16/2.5e0/, rk17/100.0e0/, rk18/2.5e0/,
!    4   rk19/50.0e0/, rk20/50.0e0/
!     ydot(1)  = -rk1*y(1)
!     ydot(2)  = rk1*y(1) + rk11*rk14*y(4) + rk19*rk14*y(5)
!    1           - rk3*y(2)*y(3) - rk15*y(2)*y(12) - rk2*y(2)
!     ydot(3)  = rk2*y(2) - rk5*y(3) - rk3*y(2)*y(3) - rk7*y(10)*y(3)
!    1           + rk11*rk14*y(4) + rk12*rk14*y(6)
!     ydot(4)  = rk3*y(2)*y(3) - rk11*rk14*y(4) - rk4*y(4)
!     ydot(5)  = rk15*y(2)*y(12) - rk19*rk14*y(5) - rk16*y(5)
!     ydot(6)  = rk7*y(10)*y(3) - rk12*rk14*y(6) - rk8*y(6)
!     ydot(7)  = rk17*y(10)*y(12) - rk20*rk14*y(7) - rk18*y(7)
!     ydot(8)  = rk9*y(10) - rk13*rk14*y(8) - rk10*y(8)
!     ydot(9)  = rk4*y(4) + rk16*y(5) + rk8*y(6) + rk18*y(7)
!     ydot(10) = rk5*y(3) + rk12*rk14*y(6) + rk20*rk14*y(7)
!    1           + rk13*rk14*y(8) - rk7*y(10)*y(3) - rk17*y(10)*y(12)
!    2           - rk6*y(10) - rk9*y(10)
!     ydot(11) = rk10*y(8)
!     ydot(12) = rk6*y(10) + rk19*rk14*y(5) + rk20*rk14*y(7)
!    1           - rk15*y(2)*y(12) - rk17*y(10)*y(12)
!     return
!     end
!
!     subroutine jex (neq, t, y, j, ia, ja, pdj)
!     real t, y, pdj
!     real rk1, rk2, rk3, rk4, rk5, rk6, rk7, rk8, rk9,
!    1   rk10, rk11, rk12, rk13, rk14, rk15, rk16, rk17
!     dimension y(1), ia(1), ja(1), pdj(1)
!     data rk1/0.1e0/, rk2/10.0e0/, rk3/50.0e0/, rk4/2.5e0/, rk5/0.1e0/,
!    1   rk6/10.0e0/, rk7/50.0e0/, rk8/2.5e0/, rk9/50.0e0/, rk10/5.0e0/,
!    2   rk11/50.0e0/, rk12/50.0e0/, rk13/50.0e0/, rk14/30.0e0/,
!    3   rk15/100.0e0/, rk16/2.5e0/, rk17/100.0e0/, rk18/2.5e0/,
!    4   rk19/50.0e0/, rk20/50.0e0/
!     go to (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), j
! 1   pdj(1) = -rk1
!     pdj(2) = rk1
!     return
! 2   pdj(2) = -rk3*y(3) - rk15*y(12) - rk2
!     pdj(3) = rk2 - rk3*y(3)
!     pdj(4) = rk3*y(3)
!     pdj(5) = rk15*y(12)
!     pdj(12) = -rk15*y(12)
!     return
! 3   pdj(2) = -rk3*y(2)
!     pdj(3) = -rk5 - rk3*y(2) - rk7*y(10)
!     pdj(4) = rk3*y(2)
!     pdj(6) = rk7*y(10)
!     pdj(10) = rk5 - rk7*y(10)
!     return
! 4   pdj(2) = rk11*rk14
!     pdj(3) = rk11*rk14
!     pdj(4) = -rk11*rk14 - rk4
!     pdj(9) = rk4
!     return
! 5   pdj(2) = rk19*rk14
!     pdj(5) = -rk19*rk14 - rk16
!     pdj(9) = rk16
!     pdj(12) = rk19*rk14
!     return
! 6   pdj(3) = rk12*rk14
!     pdj(6) = -rk12*rk14 - rk8
!     pdj(9) = rk8
!     pdj(10) = rk12*rk14
!     return
! 7   pdj(7) = -rk20*rk14 - rk18
!     pdj(9) = rk18
!     pdj(10) = rk20*rk14
!     pdj(12) = rk20*rk14
!     return
! 8   pdj(8) = -rk13*rk14 - rk10
!     pdj(10) = rk13*rk14
!     pdj(11) = rk10
! 9   return
! 10  pdj(3) = -rk7*y(3)
!     pdj(6) = rk7*y(3)
!     pdj(7) = rk17*y(12)
!     pdj(8) = rk9
!     pdj(10) = -rk7*y(3) - rk17*y(12) - rk6 - rk9
!     pdj(12) = rk6 - rk17*y(12)
! 11  return
! 12  pdj(2) = -rk15*y(2)
!     pdj(5) = rk15*y(2)
!     pdj(7) = rk17*y(10)
!     pdj(10) = -rk17*y(10)
!     pdj(12) = -rk15*y(2) - rk17*y(10)
!     return
!     end
!
! the output of this program (on a cray-1 in single precision)
! is as follows..
!
!
! at t =  1.000e-01     no. steps =   12     last step =  1.515e-02
!  y array =     9.90050e-01   6.28228e-03   3.65313e-03   7.51934e-07
!                1.12167e-09   1.18458e-09   1.77291e-12   3.26476e-07
!                5.46720e-08   9.99500e-06   4.48483e-08   2.76398e-06
!
!
! at t =  1.000e+00     no. steps =   33     last step =  7.880e-02
!  y array =     9.04837e-01   9.13105e-03   8.20622e-02   2.49177e-05
!                1.85055e-06   1.96797e-06   1.46157e-07   2.39557e-05
!                3.26306e-05   7.21621e-04   5.06433e-05   3.05010e-03
!
!
! at t =  1.000e+01     no. steps =   48     last step =  1.239e+00
!  y array =     3.67876e-01   3.68958e-03   3.65133e-01   4.48325e-05
!                6.10798e-05   4.33148e-05   5.90211e-05   1.18449e-04
!                3.15235e-03   3.56531e-03   4.15520e-03   2.48741e-01
!
!
! at t =  1.000e+02     no. steps =   91     last step =  3.764e+00
!  y array =     4.44981e-05   4.42666e-07   4.47273e-04  -3.53257e-11
!                2.81577e-08  -9.67741e-11   2.77615e-07   1.45322e-07
!                1.56230e-02   4.37394e-06   1.60104e-02   9.52246e-01
!
!
! at t =  1.000e+03     no. steps =  111     last step =  4.156e+02
!  y array =    -2.65492e-13   2.60539e-14  -8.59563e-12   6.29355e-14
!               -1.78066e-13   5.71471e-13  -1.47561e-12   4.58078e-15
!                1.56314e-02   1.37878e-13   1.60184e-02   9.52719e-01
!
!
! required rwork size = 442   iwork size =  30
! no. steps = 111   no. f-s = 142   no. j-s =   2   no. lu-s =  20
! no. of nonzeros in j =   44   no. of nonzeros in lu =   50
!-----------------------------------------------------------------------
! full description of user interface to lsodes.
!
! the user interface to lsodes consists of the following parts.
!
! i.   the call sequence to subroutine lsodes, which is a driver
!      routine for the solver.  this includes descriptions of both
!      the call sequence arguments and of user-supplied routines.
!      following these descriptions is a description of
!      optional inputs available through the call sequence, and then
!      a description of optional outputs (in the work arrays).
!
! ii.  descriptions of other routines in the lsodes package that may be
!      (optionally) called by the user.  these provide the ability to
!      alter error message handling, save and restore the internal
!      common, and obtain specified derivatives of the solution y(t).
!
! iii. descriptions of common blocks to be declared in overlay
!      or similar environments, or to be saved when doing an interrupt
!      of the problem and continued solution later.
!
! iv.  description of two routines in the lsodes package, either of
!      which the user may replace with his own version, if desired.
!      these relate to the measurement of errors.
!
!-----------------------------------------------------------------------
! part i.  call sequence.
!
! the call sequence parameters used for input only are
!     f, neq, tout, itol, rtol, atol, itask, iopt, lrw, liw, jac, mf,
! and those used for both input and output are
!     y, t, istate.
! the work arrays rwork and iwork are also used for conditional and
! optional inputs and optional outputs.  (the term output here refers
! to the return from subroutine lsodes to the user-s calling program.)
!
! the legality of input parameters will be thoroughly checked on the
! initial call for the problem, but not checked thereafter unless a
! change in input parameters is flagged by istate = 3 on input.
!
! the descriptions of the call arguments are as follows.
!
! f      = the name of the user-supplied subroutine defining the
!          ode system.  the system must be put in the first-order
!          form dy/dt = f(t,y), where f is a vector-valued function
!          of the scalar t and the vector y.  subroutine f is to
!          compute the function f.  it is to have the form
!               subroutine f (neq, t, y, ydot)
!               dimension y(1), ydot(1)
!          where neq, t, and y are input, and the array ydot = f(t,y)
!          is output.  y and ydot are arrays of length neq.
!          (in the dimension statement above, 1 is a dummy
!          dimension.. it can be replaced by any value.)
!          subroutine f should not alter y(1),...,y(neq).
!          f must be declared external in the calling program.
!
!          subroutine f may access user-defined quantities in
!          neq(2),... and/or in y(neq(1)+1),... if neq is an array
!          (dimensioned in f) and/or y has length exceeding neq(1).
!          see the descriptions of neq and y below.
!
!          if quantities computed in the f routine are needed
!          externally to lsodes, an extra call to f should be made
!          for this purpose, for consistent and accurate results.
!          if only the derivative dy/dt is needed, use intdy instead.
!
! neq    = the size of the ode system (number of first order
!          ordinary differential equations).  used only for input.
!          neq may be decreased, but not increased, during the problem.
!          if neq is decreased (with istate = 3 on input), the
!          remaining components of y should be left undisturbed, if
!          these are to be accessed in f and/or jac.
!
!          normally, neq is a scalar, and it is generally referred to
!          as a scalar in this user interface description.  however,
!          neq may be an array, with neq(1) set to the system size.
!          (the lsodes package accesses only neq(1).)  in either case,
!          this parameter is passed as the neq argument in all calls
!          to f and jac.  hence, if it is an array, locations
!          neq(2),... may be used to store other integer data and pass
!          it to f and/or jac.  subroutines f and/or jac must include
!          neq in a dimension statement in that case.
!
! y      = a real array for the vector of dependent variables, of
!          length neq or more.  used for both input and output on the
!          first call (istate = 1), and only for output on other calls.
!          on the first call, y must contain the vector of initial
!          values.  on output, y contains the computed solution vector,
!          evaluated at t.  if desired, the y array may be used
!          for other purposes between calls to the solver.
!
!          this array is passed as the y argument in all calls to
!          f and jac.  hence its length may exceed neq, and locations
!          y(neq+1),... may be used to store other real data and
!          pass it to f and/or jac.  (the lsodes package accesses only
!          y(1),...,y(neq).)
!
! t      = the independent variable.  on input, t is used only on the
!          first call, as the initial point of the integration.
!          on output, after each call, t is the value at which a
!          computed solution y is evaluated (usually the same as tout).
!          on an error return, t is the farthest point reached.
!
! tout   = the next value of t at which a computed solution is desired.
!          used only for input.
!
!          when starting the problem (istate = 1), tout may be equal
!          to t for one call, then should .ne. t for the next call.
!          for the initial t, an input value of tout .ne. t is used
!          in order to determine the direction of the integration
!          (i.e. the algebraic sign of the step sizes) and the rough
!          scale of the problem.  integration in either direction
!          (forward or backward in t) is permitted.
!
!          if itask = 2 or 5 (one-step modes), tout is ignored after
!          the first call (i.e. the first call with tout .ne. t).
!          otherwise, tout is required on every call.
!
!          if itask = 1, 3, or 4, the values of tout need not be
!          monotone, but a value of tout which backs up is limited
!          to the current internal t interval, whose endpoints are
!          tcur - hu and tcur (see optional outputs, below, for
!          tcur and hu).
!
! itol   = an indicator for the type of error control.  see
!          description below under atol.  used only for input.
!
! rtol   = a relative error tolerance parameter, either a scalar or
!          an array of length neq.  see description below under atol.
!          input only.
!
! atol   = an absolute error tolerance parameter, either a scalar or
!          an array of length neq.  input only.
!
!             the input parameters itol, rtol, and atol determine
!          the error control performed by the solver.  the solver will
!          control the vector e = (e(i)) of estimated local errors
!          in y, according to an inequality of the form
!                      rms-norm of ( e(i)/ewt(i) )   .le.   1,
!          where       ewt(i) = rtol(i)*abs(y(i)) + atol(i),
!          and the rms-norm (root-mean-square norm) here is
!          rms-norm(v) = sqrt(sum v(i)**2 / neq).  here ewt = (ewt(i))
!          is a vector of weights which must always be positive, and
!          the values of rtol and atol should all be non-negative.
!          the following table gives the types (scalar/array) of
!          rtol and atol, and the corresponding form of ewt(i).
!
!             itol    rtol       atol          ewt(i)
!              1     scalar     scalar     rtol*abs(y(i)) + atol
!              2     scalar     array      rtol*abs(y(i)) + atol(i)
!              3     array      scalar     rtol(i)*abs(y(i)) + atol
!              4     array      array      rtol(i)*abs(y(i)) + atol(i)
!
!          when either of these parameters is a scalar, it need not
!          be dimensioned in the user-s calling program.
!
!          if none of the above choices (with itol, rtol, and atol
!          fixed throughout the problem) is suitable, more general
!          error controls can be obtained by substituting
!          user-supplied routines for the setting of ewt and/or for
!          the norm calculation.  see part iv below.
!
!          if global errors are to be estimated by making a repeated
!          run on the same problem with smaller tolerances, then all
!          components of rtol and atol (i.e. of ewt) should be scaled
!          down uniformly.
!
! itask  = an index specifying the task to be performed.
!          input only.  itask has the following values and meanings.
!          1  means normal computation of output values of y(t) at
!             t = tout (by overshooting and interpolating).
!          2  means take one step only and return.
!          3  means stop at the first internal mesh point at or
!             beyond t = tout and return.
!          4  means normal computation of output values of y(t) at
!             t = tout but without overshooting t = tcrit.
!             tcrit must be input as rwork(1).  tcrit may be equal to
!             or beyond tout, but not behind it in the direction of
!             integration.  this option is useful if the problem
!             has a singularity at or beyond t = tcrit.
!          5  means take one step, without passing tcrit, and return.
!             tcrit must be input as rwork(1).
!
!          note..  if itask = 4 or 5 and the solver reaches tcrit
!          (within roundoff), it will return t = tcrit (exactly) to
!          indicate this (unless itask = 4 and tout comes before tcrit,
!          in which case answers at t = tout are returned first).
!
! istate = an index used for input and output to specify the
!          the state of the calculation.
!
!          on input, the values of istate are as follows.
!          1  means this is the first call for the problem
!             (initializations will be done).  see note below.
!          2  means this is not the first call, and the calculation
!             is to continue normally, with no change in any input
!             parameters except possibly tout and itask.
!             (if itol, rtol, and/or atol are changed between calls
!             with istate = 2, the new values will be used but not
!             tested for legality.)
!          3  means this is not the first call, and the
!             calculation is to continue normally, but with
!             a change in input parameters other than
!             tout and itask.  changes are allowed in
!             neq, itol, rtol, atol, iopt, lrw, liw, mf,
!             the conditional inputs ia and ja,
!             and any of the optional inputs except h0.
!             in particular, if miter = 1 or 2, a call with istate = 3
!             will cause the sparsity structure of the problem to be
!             recomputed (or reread from ia and ja if moss = 0).
!          note..  a preliminary call with tout = t is not counted
!          as a first call here, as no initialization or checking of
!          input is done.  (such a call is sometimes useful for the
!          purpose of outputting the initial conditions.)
!          thus the first call for which tout .ne. t requires
!          istate = 1 on input.
!
!          on output, istate has the following values and meanings.
!           1  means nothing was done, as tout was equal to t with
!              istate = 1 on input.  (however, an internal counter was
!              set to detect and prevent repeated calls of this type.)
!           2  means the integration was performed successfully.
!          -1  means an excessive amount of work (more than mxstep
!              steps) was done on this call, before completing the
!              requested task, but the integration was otherwise
!              successful as far as t.  (mxstep is an optional input
!              and is normally 500.)  to continue, the user may
!              simply reset istate to a value .gt. 1 and call again
!              (the excess work step counter will be reset to 0).
!              in addition, the user may increase mxstep to avoid
!              this error return (see below on optional inputs).
!          -2  means too much accuracy was requested for the precision
!              of the machine being used.  this was detected before
!              completing the requested task, but the integration
!              was successful as far as t.  to continue, the tolerance
!              parameters must be reset, and istate must be set
!              to 3.  the optional output tolsf may be used for this
!              purpose.  (note.. if this condition is detected before
!              taking any steps, then an illegal input return
!              (istate = -3) occurs instead.)
!          -3  means illegal input was detected, before taking any
!              integration steps.  see written message for details.
!              note..  if the solver detects an infinite loop of calls
!              to the solver with illegal input, it will cause
!              the run to stop.
!          -4  means there were repeated error test failures on
!              one attempted step, before completing the requested
!              task, but the integration was successful as far as t.
!              the problem may have a singularity, or the input
!              may be inappropriate.
!          -5  means there were repeated convergence test failures on
!              one attempted step, before completing the requested
!              task, but the integration was successful as far as t.
!              this may be caused by an inaccurate jacobian matrix,
!              if one is being used.
!          -6  means ewt(i) became zero for some i during the
!              integration.  pure relative error control (atol(i)=0.0)
!              was requested on a variable which has now vanished.
!              the integration was successful as far as t.
!          -7  means a fatal error return flag came from the sparse
!              solver cdrv by way of prjs or slss (numerical
!              factorization or backsolve).  this should never happen.
!              the integration was successful as far as t.
!
!          note.. an error return with istate = -1, -4, or -5 and with
!          miter = 1 or 2 may mean that the sparsity structure of the
!          problem has changed significantly since it was last
!          determined (or input).  in that case, one can attempt to
!          complete the integration by setting istate = 3 on the next
!          call, so that a new structure determination is done.
!
!          note..  since the normal output value of istate is 2,
!          it does not need to be reset for normal continuation.
!          also, since a negative input value of istate will be
!          regarded as illegal, a negative output value requires the
!          user to change it, and possibly other inputs, before
!          calling the solver again.
!
! iopt   = an integer flag to specify whether or not any optional
!          inputs are being used on this call.  input only.
!          the optional inputs are listed separately below.
!          iopt = 0 means no optional inputs are being used.
!                   default values will be used in all cases.
!          iopt = 1 means one or more optional inputs are being used.
!
! rwork  = a work array used for a mixture of real (single precision)
!          and integer work space.
!          the length of rwork (in real words) must be at least
!             20 + nyh*(maxord + 1) + 3*neq + lwm    where
!          nyh    = the initial value of neq,
!          maxord = 12 (if meth = 1) or 5 (if meth = 2) (unless a
!                   smaller value is given as an optional input),
!          lwm = 0                                    if miter = 0,
!          lwm = 2*nnz + 2*neq + (nnz+9*neq)/lenrat   if miter = 1,
!          lwm = 2*nnz + 2*neq + (nnz+10*neq)/lenrat  if miter = 2,
!          lwm = neq + 2                              if miter = 3.
!          in the above formulas,
!          nnz    = number of nonzero elements in the jacobian matrix.
!          lenrat = the real to integer wordlength ratio (usually 1 in
!                   single precision and 2 in double precision).
!          (see the mf description for meth and miter.)
!          thus if maxord has its default value and neq is constant,
!          the minimum length of rwork is..
!             20 + 16*neq        for mf = 10,
!             20 + 16*neq + lwm  for mf = 11, 111, 211, 12, 112, 212,
!             22 + 17*neq        for mf = 13,
!             20 +  9*neq        for mf = 20,
!             20 +  9*neq + lwm  for mf = 21, 121, 221, 22, 122, 222,
!             22 + 10*neq        for mf = 23.
!          if miter = 1 or 2, the above formula for lwm is only a
!          crude lower bound.  the required length of rwork cannot
!          be readily predicted in general, as it depends on the
!          sparsity structure of the problem.  some experimentation
!          may be necessary.
!
!          the first 20 words of rwork are reserved for conditional
!          and optional inputs and optional outputs.
!
!          the following word in rwork is a conditional input..
!            rwork(1) = tcrit = critical value of t which the solver
!                       is not to overshoot.  required if itask is
!                       4 or 5, and ignored otherwise.  (see itask.)
!
! lrw    = the length of the array rwork, as declared by the user.
!          (this will be checked by the solver.)
!
! iwork  = an integer work array.  the length of iwork must be at least
!             31 + neq + nnz   if moss = 0 and miter = 1 or 2, or
!             30               otherwise.
!          (nnz is the number of nonzero elements in df/dy.)
!
!          in lsodes, iwork is used only for conditional and
!          optional inputs and optional outputs.
!
!          the following two blocks of words in iwork are conditional
!          inputs, required if moss = 0 and miter = 1 or 2, but not
!          otherwise (see the description of mf for moss).
!            iwork(30+j) = ia(j)     (j=1,...,neq+1)
!            iwork(31+neq+k) = ja(k) (k=1,...,nnz)
!          the two arrays ia and ja describe the sparsity structure
!          to be assumed for the jacobian matrix.  ja contains the row
!          indices where nonzero elements occur, reading in columnwise
!          order, and ia contains the starting locations in ja of the
!          descriptions of columns 1,...,neq, in that order, with
!          ia(1) = 1.  thus, for each column index j = 1,...,neq, the
!          values of the row index i in column j where a nonzero
!          element may occur are given by
!            i = ja(k),  where   ia(j) .le. k .lt. ia(j+1).
!          if nnz is the total number of nonzero locations assumed,
!          then the length of the ja array is nnz, and ia(neq+1) must
!          be nnz + 1.  duplicate entries are not allowed.
!
! liw    = the length of the array iwork, as declared by the user.
!          (this will be checked by the solver.)
!
! note..  the work arrays must not be altered between calls to lsodes
! for the same problem, except possibly for the conditional and
! optional inputs, and except for the last 3*neq words of rwork.
! the latter space is used for internal scratch space, and so is
! available for use by the user outside lsodes between calls, if
! desired (but not for use by f or jac).
!
! jac    = name of user-supplied routine (miter = 1 or moss = 1) to
!          compute the jacobian matrix, df/dy, as a function of
!          the scalar t and the vector y.  it is to have the form
!               subroutine jac (neq, t, y, j, ian, jan, pdj)
!               dimension y(1), ian(1), jan(1), pdj(1)
!          where neq, t, y, j, ian, and jan are input, and the array
!          pdj, of length neq, is to be loaded with column j
!          of the jacobian on output.  thus df(i)/dy(j) is to be
!          loaded into pdj(i) for all relevant values of i.
!          here t and y have the same meaning as in subroutine f,
!          and j is a column index (1 to neq).  ian and jan are
!          undefined in calls to jac for structure determination
!          (moss = 1).  otherwise, ian and jan are structure
!          descriptors, as defined under optional outputs below, and
!          so can be used to determine the relevant row indices i, if
!          desired.  (in the dimension statement above, 1 is a
!          dummy dimension.. it can be replaced by any value.)
!               jac need not provide df/dy exactly.  a crude
!          approximation (possibly with greater sparsity) will do.
!               in any case, pdj is preset to zero by the solver,
!          so that only the nonzero elements need be loaded by jac.
!          calls to jac are made with j = 1,...,neq, in that order, and
!          each such set of calls is preceded by a call to f with the
!          same arguments neq, t, and y.  thus to gain some efficiency,
!          intermediate quantities shared by both calculations may be
!          saved in a user common block by f and not recomputed by jac,
!          if desired.  jac must not alter its input arguments.
!          jac must be declared external in the calling program.
!               subroutine jac may access user-defined quantities in
!          neq(2),... and/or in y(neq(1)+1),... if neq is an array
!          (dimensioned in jac) and/or y has length exceeding neq(1).
!          see the descriptions of neq and y above.
!
! mf     = the method flag.  used only for input.
!          mf has three decimal digits-- moss, meth, miter--
!             mf = 100*moss + 10*meth + miter.
!          moss indicates the method to be used to obtain the sparsity
!          structure of the jacobian matrix if miter = 1 or 2..
!            moss = 0 means the user has supplied ia and ja
!                     (see descriptions under iwork above).
!            moss = 1 means the user has supplied jac (see below)
!                     and the structure will be obtained from neq
!                     initial calls to jac.
!            moss = 2 means the structure will be obtained from neq+1
!                     initial calls to f.
!          meth indicates the basic linear multistep method..
!            meth = 1 means the implicit adams method.
!            meth = 2 means the method based on backward
!                     differentiation formulas (bdf-s).
!          miter indicates the corrector iteration method..
!            miter = 0 means functional iteration (no jacobian matrix
!                      is involved).
!            miter = 1 means chord iteration with a user-supplied
!                      sparse jacobian, given by subroutine jac.
!            miter = 2 means chord iteration with an internally
!                      generated (difference quotient) sparse jacobian
!                      (using ngp extra calls to f per df/dy value,
!                      where ngp is an optional output described below.)
!            miter = 3 means chord iteration with an internally
!                      generated diagonal jacobian approximation.
!                      (using 1 extra call to f per df/dy evaluation).
!          if miter = 1 or moss = 1, the user must supply a subroutine
!          jac (the name is arbitrary) as described above under jac.
!          otherwise, a dummy argument can be used.
!
!          the standard choices for mf are..
!            mf = 10  for a nonstiff problem,
!            mf = 21 or 22 for a stiff problem with ia/ja supplied
!                     (21 if jac is supplied, 22 if not),
!            mf = 121 for a stiff problem with jac supplied,
!                     but not ia/ja,
!            mf = 222 for a stiff problem with neither ia/ja nor
!                     jac supplied.
!          the sparseness structure can be changed during the
!          problem by making a call to lsodes with istate = 3.
!-----------------------------------------------------------------------
! optional inputs.
!
! the following is a list of the optional inputs provided for in the
! call sequence.  (see also part ii.)  for each such input variable,
! this table lists its name as used in this documentation, its
! location in the call sequence, its meaning, and the default value.
! the use of any of these inputs requires iopt = 1, and in that
! case all of these inputs are examined.  a value of zero for any
! of these optional inputs will cause the default value to be used.
! thus to use a subset of the optional inputs, simply preload
! locations 5 to 10 in rwork and iwork to 0.0 and 0 respectively, and
! then set those of interest to nonzero values.
!
! name    location      meaning and default value
!
! h0      rwork(5)  the step size to be attempted on the first step.
!                   the default value is determined by the solver.
!
! hmax    rwork(6)  the maximum absolute step size allowed.
!                   the default value is infinite.
!
! hmin    rwork(7)  the minimum absolute step size allowed.
!                   the default value is 0.  (this lower bound is not
!                   enforced on the final step before reaching tcrit
!                   when itask = 4 or 5.)
!
! seth    rwork(8)  the element threshhold for sparsity determination
!                   when moss = 1 or 2.  if the absolute value of
!                   an estimated jacobian element is .le. seth, it
!                   will be assumed to be absent in the structure.
!                   the default value of seth is 0.
!
! maxord  iwork(5)  the maximum order to be allowed.  the default
!                   value is 12 if meth = 1, and 5 if meth = 2.
!                   if maxord exceeds the default value, it will
!                   be reduced to the default value.
!                   if maxord is changed during the problem, it may
!                   cause the current order to be reduced.
!
! mxstep  iwork(6)  maximum number of (internally defined) steps
!                   allowed during one call to the solver.
!                   the default value is 500.
!
! mxhnil  iwork(7)  maximum number of messages printed (per problem)
!                   warning that t + h = t on a step (h = step size).
!                   this must be positive to result in a non-default
!                   value.  the default value is 10.
!-----------------------------------------------------------------------
! optional outputs.
!
! as optional additional output from lsodes, the variables listed
! below are quantities related to the performance of lsodes
! which are available to the user.  these are communicated by way of
! the work arrays, but also have internal mnemonic names as shown.
! except where stated otherwise, all of these outputs are defined
! on any successful return from lsodes, and on any return with
! istate = -1, -2, -4, -5, or -6.  on an illegal input return
! (istate = -3), they will be unchanged from their existing values
! (if any), except possibly for tolsf, lenrw, and leniw.
! on any error return, outputs relevant to the error will be defined,
! as noted below.
!
! name    location      meaning
!
! hu      rwork(11) the step size in t last used (successfully).
!
! hcur    rwork(12) the step size to be attempted on the next step.
!
! tcur    rwork(13) the current value of the independent variable
!                   which the solver has actually reached, i.e. the
!                   current internal mesh point in t.  on output, tcur
!                   will always be at least as far as the argument
!                   t, but may be farther (if interpolation was done).
!
! tolsf   rwork(14) a tolerance scale factor, greater than 1.0,
!                   computed when a request for too much accuracy was
!                   detected (istate = -3 if detected at the start of
!                   the problem, istate = -2 otherwise).  if itol is
!                   left unaltered but rtol and atol are uniformly
!                   scaled up by a factor of tolsf for the next call,
!                   then the solver is deemed likely to succeed.
!                   (the user may also ignore tolsf and alter the
!                   tolerance parameters in any other way appropriate.)
!
! nst     iwork(11) the number of steps taken for the problem so far.
!
! nfe     iwork(12) the number of f evaluations for the problem so far,
!                   excluding those for structure determination
!                   (moss = 2).
!
! nje     iwork(13) the number of jacobian evaluations for the problem
!                   so far, excluding those for structure determination
!                   (moss = 1).
!
! nqu     iwork(14) the method order last used (successfully).
!
! nqcur   iwork(15) the order to be attempted on the next step.
!
! imxer   iwork(16) the index of the component of largest magnitude in
!                   the weighted local error vector ( e(i)/ewt(i) ),
!                   on an error return with istate = -4 or -5.
!
! lenrw   iwork(17) the length of rwork actually required.
!                   this is defined on normal returns and on an illegal
!                   input return for insufficient storage.
!
! leniw   iwork(18) the length of iwork actually required.
!                   this is defined on normal returns and on an illegal
!                   input return for insufficient storage.
!
! nnz     iwork(19) the number of nonzero elements in the jacobian
!                   matrix, including the diagonal (miter = 1 or 2).
!                   (this may differ from that given by ia(neq+1)-1
!                   if moss = 0, because of added diagonal entries.)
!
! ngp     iwork(20) the number of groups of column indices, used in
!                   difference quotient jacobian aproximations if
!                   miter = 2.  this is also the number of extra f
!                   evaluations needed for each jacobian evaluation.
!
! nlu     iwork(21) the number of sparse lu decompositions for the
!                   problem so far.
!
! lyh     iwork(22) the base address in rwork of the history array yh,
!                   described below in this list.
!
! ipian   iwork(23) the base address of the structure descriptor array
!                   ian, described below in this list.
!
! ipjan   iwork(24) the base address of the structure descriptor array
!                   jan, described below in this list.
!
! nzl     iwork(25) the number of nonzero elements in the strict lower
!                   triangle of the lu factorization used in the chord
!                   iteration (miter = 1 or 2).
!
! nzu     iwork(26) the number of nonzero elements in the strict upper
!                   triangle of the lu factorization used in the chord
!                   iteration (miter = 1 or 2).
!                   the total number of nonzeros in the factorization
!                   is therefore nzl + nzu + neq.
!
! the following four arrays are segments of the rwork array which
! may also be of interest to the user as optional outputs.
! for each array, the table below gives its internal name,
! its base address, and its description.
! for yh and acor, the base addresses are in rwork (a real array).
! the integer arrays ian and jan are to be obtained by declaring an
! integer array iwk and identifying iwk(1) with rwork(21), using either
! an equivalence statement or a subroutine call.  then the base
! addresses ipian (of ian) and ipjan (of jan) in iwk are to be obtained
! as optional outputs iwork(23) and iwork(24), respectively.
! thus ian(1) is iwk(ipian), etc.
!
! name    base address      description
!
! ian    ipian (in iwk)  structure descriptor array of size neq + 1.
! jan    ipjan (in iwk)  structure descriptor array of size nnz.
!         (see above)    ian and jan together describe the sparsity
!                        structure of the jacobian matrix, as used by
!                        lsodes when miter = 1 or 2.
!                        jan contains the row indices of the nonzero
!                        locations, reading in columnwise order, and
!                        ian contains the starting locations in jan of
!                        the descriptions of columns 1,...,neq, in
!                        that order, with ian(1) = 1.  thus for each
!                        j = 1,...,neq, the row indices i of the
!                        nonzero locations in column j are
!                        i = jan(k),  ian(j) .le. k .lt. ian(j+1).
!                        note that ian(neq+1) = nnz + 1.
!                        (if moss = 0, ian/jan may differ from the
!                        input ia/ja because of a different ordering
!                        in each column, and added diagonal entries.)
!
! yh      lyh            the nordsieck history array, of size nyh by
!          (optional     (nqcur + 1), where nyh is the initial value
!          output)       of neq.  for j = 0,1,...,nqcur, column j+1
!                        of yh contains hcur**j/factorial(j) times
!                        the j-th derivative of the interpolating
!                        polynomial currently representing the solution,
!                        evaluated at t = tcur.  the base address lyh
!                        is another optional output, listed above.
!
! acor     lenrw-neq+1   array of size neq used for the accumulated
!                        corrections on each step, scaled on output
!                        to represent the estimated local error in y
!                        on the last step.  this is the vector e in
!                        the description of the error control.  it is
!                        defined only on a successful return from
!                        lsodes.
!
!-----------------------------------------------------------------------
! part ii.  other routines callable.
!
! the following are optional calls which the user may make to
! gain additional capabilities in conjunction with lsodes.
! (the routines xsetun and xsetf are designed to conform to the
! slatec error handling package.)
!
!     form of call                  function
!   call xsetun(lun)          set the logical unit number, lun, for
!                             output of messages from lsodes, if
!                             the default is not desired.
!                             the default value of lun is 6.
!
!   call xsetf(mflag)         set a flag to control the printing of
!                             messages by lsodes.
!                             mflag = 0 means do not print. (danger..
!                             this risks losing valuable information.)
!                             mflag = 1 means print (the default).
!
!                             either of the above calls may be made at
!                             any time and will take effect immediately.
!
!   call srcms(rsav,isav,job) saves and restores the contents of
!                             the internal common blocks used by
!                             lsodes (see part iii below).
!                             rsav must be a real array of length 224
!                             or more, and isav must be an integer
!                             array of length 75 or more.
!                             job=1 means save common into rsav/isav.
!                             job=2 means restore common from rsav/isav.
!                                srcms is useful if one is
!                             interrupting a run and restarting
!                             later, or alternating between two or
!                             more problems solved with lsodes.
!
!   call intdy(,,,,,)         provide derivatives of y, of various
!        (see below)          orders, at a specified point t, if
!                             desired.  it may be called only after
!                             a successful return from lsodes.
!
! the detailed instructions for using intdy are as follows.
! the form of the call is..
!
!   lyh = iwork(22)
!   call intdy (t, k, rwork(lyh), nyh, dky, iflag)
!
! the input parameters are..
!
! t         = value of independent variable where answers are desired
!             (normally the same as the t last returned by lsodes).
!             for valid results, t must lie between tcur - hu and tcur.
!             (see optional outputs for tcur and hu.)
! k         = integer order of the derivative desired.  k must satisfy
!             0 .le. k .le. nqcur, where nqcur is the current order
!             (see optional outputs).  the capability corresponding
!             to k = 0, i.e. computing y(t), is already provided
!             by lsodes directly.  since nqcur .ge. 1, the first
!             derivative dy/dt is always available with intdy.
! lyh       = the base address of the history array yh, obtained
!             as an optional output as shown above.
! nyh       = column length of yh, equal to the initial value of neq.
!
! the output parameters are..
!
! dky       = a real array of length neq containing the computed value
!             of the k-th derivative of y(t).
! iflag     = integer flag, returned as 0 if k and t were legal,
!             -1 if k was illegal, and -2 if t was illegal.
!             on an error return, a message is also written.
!-----------------------------------------------------------------------
! part iii.  common blocks.
!
! if lsodes is to be used in an overlay situation, the user
! must declare, in the primary overlay, the variables in..
!   (1) the call sequence to lsodes,
!   (2) the three internal common blocks
!         /ls0001/  of length  257  (218 single precision words
!                         followed by 39 integer words),
!         /lss001/  of length  40    ( 6 single precision words
!                         followed by 34 integer words),
!         /eh0001/  of length  2 (integer words).
!
! if lsodes is used on a system in which the contents of internal
! common blocks are not preserved between calls, the user should
! declare the above three common blocks in his main program to insure
! that their contents are preserved.
!
! if the solution of a given problem by lsodes is to be interrupted
! and then later continued, such as when restarting an interrupted run
! or alternating between two or more problems, the user should save,
! following the return from the last lsodes call prior to the
! interruption, the contents of the call sequence variables and the
! internal common blocks, and later restore these values before the
! next lsodes call for that problem.  to save and restore the common
! blocks, use subroutine srcms (see part ii above).
!
! note.. in this version of lsodes, there are two data statements,
! in subroutines lsodes and xerrwv, which load variables into these
! labeled common blocks.  on some systems, it may be necessary to
! move these to a separate block data subprogram.
!
!-----------------------------------------------------------------------
! part iv.  optionally replaceable solver routines.
!
! below are descriptions of two routines in the lsodes package which
! relate to the measurement of errors.  either routine can be
! replaced by a user-supplied version, if desired.  however, since such
! a replacement may have a major impact on performance, it should be
! done only when absolutely necessary, and only with great caution.
! (note.. the means by which the package version of a routine is
! superseded by the user-s version may be system-dependent.)
!
! (a) ewset.
! the following subroutine is called just before each internal
! integration step, and sets the array of error weights, ewt, as
! described under itol/rtol/atol above..
!     subroutine ewset (neq, itol, rtol, atol, ycur, ewt)
! where neq, itol, rtol, and atol are as in the lsodes call sequence,
! ycur contains the current dependent variable vector, and
! ewt is the array of weights set by ewset.
!
! if the user supplies this subroutine, it must return in ewt(i)
! (i = 1,...,neq) a positive quantity suitable for comparing errors
! in y(i) to.  the ewt array returned by ewset is passed to the
! vnorm routine (see below), and also used by lsodes in the computation
! of the optional output imxer, the diagonal jacobian approximation,
! and the increments for difference quotient jacobians.
!
! in the user-supplied version of ewset, it may be desirable to use
! the current values of derivatives of y.  derivatives up to order nq
! are available from the history array yh, described above under
! optional outputs.  in ewset, yh is identical to the ycur array,
! extended to nq + 1 columns with a column length of nyh and scale
! factors of h**j/factorial(j).  on the first call for the problem,
! given by nst = 0, nq is 1 and h is temporarily set to 1.0.
! the quantities nq, nyh, h, and nst can be obtained by including
! in ewset the statements..
!     common /ls0001/ rls(218),ils(39)
!     nq = ils(35)
!     nyh = ils(14)
!     nst = ils(36)
!     h = rls(212)
! thus, for example, the current value of dy/dt can be obtained as
! ycur(nyh+i)/h  (i=1,...,neq)  (and the division by h is
! unnecessary when nst = 0).
!
! (b) vnorm.
! the following is a real function routine which computes the weighted
! root-mean-square norm of a vector v..
!     d = vnorm (n, v, w)
! where..
!   n = the length of the vector,
!   v = real array of length n containing the vector,
!   w = real array of length n containing weights,
!   d = sqrt( (1/n) * sum(v(i)*w(i))**2 ).
! vnorm is called with n = neq and with w(i) = 1.0/ewt(i), where
! ewt is as set by subroutine ewset.
!
! if the user supplies this function, it should return a non-negative
! value of vnorm suitable for use in the error control in lsodes.
! none of the arguments should be altered by vnorm.
! for example, a user-supplied vnorm routine might..
!   -substitute a max-norm of (v(i)*w(i)) for the rms-norm, or
!   -ignore some components of v in the norm, with the effect of
!    suppressing the error control on those components of y.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! other routines in the lsodes package.
!
! in addition to subroutine lsodes, the lsodes package includes the
! following subroutines and function routines..
!  iprep    acts as an iterface between lsodes and prep, and also does
!           adjusting of work space pointers and work arrays.
!  prep     is called by iprep to compute sparsity and do sparse matrix
!           preprocessing if miter = 1 or 2.
!  jgroup   is called by prep to compute groups of jacobian column
!           indices for use when miter = 2.
!  adjlr    adjusts the length of required sparse matrix work space.
!           it is called by prep.
!  cntnzu   is called by prep and counts the nonzero elements in the
!           strict upper triangle of j + j-transpose, where j = df/dy.
!  intdy    computes an interpolated value of the y vector at t = tout.
!  stode    is the core integrator, which does one step of the
!           integration and the associated error control.
!  cfode    sets all method coefficients and test constants.
!  prjs     computes and preprocesses the jacobian matrix j = df/dy
!           and the newton iteration matrix p = i - h*l0*j.
!  slss     manages solution of linear system in chord iteration.
!  ewset    sets the error weight vector ewt before each step.
!  vnorm    computes the weighted r.m.s. norm of a vector.
!  srcms    is a user-callable routine to save and restore
!           the contents of the internal common blocks.
!  odrv     constructs a reordering of the rows and columns of
!           a matrix by the minimum degree algorithm.  odrv is a
!           driver routine which calls subroutines md, mdi, mdm,
!           mdp, mdu, and sro.  see ref. 2 for details.  (the odrv
!           module has been modified since ref. 2, however.)
!  cdrv     performs reordering, symbolic factorization, numerical
!           factorization, or linear system solution operations,
!           depending on a path argument ipath.  cdrv is a
!           driver routine which calls subroutines nroc, nsfc,
!           nnfc, nnsc, and nntc.  see ref. 3 for details.
!           lsodes uses cdrv to solve linear systems in which the
!           coefficient matrix is  p = i - con*j, where i is the
!           identity, con is a scalar, and j is an approximation to
!           the jacobian df/dy.  because cdrv deals with rowwise
!           sparsity descriptions, cdrv works with p-transpose, not p.
!  r1mach   computes the unit roundoff in a machine-independent manner.
!  xerrwv, xsetun, and xsetf   handle the printing of all error
!           messages and warnings.  xerrwv is machine-dependent.
! note..  vnorm and r1mach are function routines.
! all the others are subroutines.
!
! the intrinsic and external routines used by lsodes are..
! abs, amax1, amin1, float, max0, min0, mod, sign, sqrt, and write.
!
!-----------------------------------------------------------------------
! the following card is for optimized compilation on lll compilers.
!lll. optimize
!-----------------------------------------------------------------------
!rce  external prjs, slss
      integer illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,   &
         mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns
      integer icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, i1, i2, iflag, imax, imul, imxer, ipflag, ipgo, irem,   &
         j, kgo, lenrat, lenyht, leniw, lenrw, lf0, lia, lja,   &
         lrtem, lwtem, lyhd, lyhn, mf1, mord, mxhnl0, mxstp0, ncolm
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real con0, conmin, ccmxj, psmall, rbig, seth
!rce  real atoli, ayi, big, ewti, h0, hmax, hmx, rh, rtoli,   &
!rce     tcrit, tdist, tnext, tol, tolsf, tp, size, sum, w0,   &
!rce     r1mach, vnorm
      real atoli, ayi, big, ewti, h0, hmax, hmx, rh, rtoli,   &
         tcrit, tdist, tnext, tol, tolsf, tp, size, sum, w0
      dimension mord(2)
      logical ihit
!-----------------------------------------------------------------------
! the following two internal common blocks contain
! (a) variables which are local to any subroutine but whose values must
!     be preserved between calls to the routine (own variables), and
! (b) variables which are communicated between subroutines.
! the structure of each block is as follows..  all real variables are
! listed first, followed by all integers.  within each type, the
! variables are grouped with those local to subroutine lsodes first,
! then those local to subroutine stode or subroutine prjs
! (no other routines have own variables), and finally those used
! for communication.  the block ls0001 is declared in subroutines
! lsodes, iprep, prep, intdy, stode, prjs, and slss.  the block lss001
! is declared in subroutines lsodes, iprep, prep, prjs, and slss.
! groups of variables are replaced by dummy arrays in the common
! declarations in routines where those variables are not used.
!-----------------------------------------------------------------------
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,   &
         mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,   &
         iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu

      integer iok_vnorm
      common / lsodes_cmn_iok_vnorm / iok_vnorm
!
      data mord(1),mord(2)/12,5/, mxstp0/500/, mxhnl0/10/
!raz      data illin/0/, ntrep/0/
!-----------------------------------------------------------------------
! in the data statement below, set lenrat equal to the ratio of
! the wordlength for a real number to that for an integer.  usually,
! lenrat = 1 for single precision and 2 for double precision.  if the
! true ratio is not an integer, use the next smaller integer (.ge. 1).
!-----------------------------------------------------------------------
      data lenrat/1/
!-----------------------------------------------------------------------
! block a.
! this code block is executed on every call.
! it tests istate and itask for legality and branches appropriately.
! if istate .gt. 1 but the flag init shows that initialization has
! not yet been done, an error return occurs.
! if istate = 1 and tout = t, jump to block g and return immediately.
!-----------------------------------------------------------------------
      iok_vnorm = 1

      if (istate .lt. 1 .or. istate .gt. 3) go to 601
      if (itask .lt. 1 .or. itask .gt. 5) go to 602
      if (istate .eq. 1) go to 10
      if (init .eq. 0) go to 603
      if (istate .eq. 2) go to 200
      go to 20
 10   init = 0
      if (tout .eq. t) go to 430
 20   ntrep = 0
!-----------------------------------------------------------------------
! block b.
! the next code block is executed for the initial call (istate = 1),
! or for a continuation call with parameter changes (istate = 3).
! it contains checking of all inputs and various initializations.
! if istate = 1, the final setting of work space pointers, the matrix
! preprocessing, and other initializations are done in block c.
!
! first check legality of the non-optional inputs neq, itol, iopt,
! mf, ml, and mu.
!-----------------------------------------------------------------------
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
! next process and check the optional inputs. --------------------------
      if (iopt .eq. 1) go to 40
      maxord = mord(meth)
      mxstep = mxstp0
      mxhnil = mxhnl0
      if (istate .eq. 1) h0 = 0.0e0
      hmxi = 0.0e0
      hmin = 0.0e0
      seth = 0.0e0
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
      if ((tout - t)*h0 .lt. 0.0e0) go to 614
 50   hmax = rwork(6)
      if (hmax .lt. 0.0e0) go to 615
      hmxi = 0.0e0
      if (hmax .gt. 0.0e0) hmxi = 1.0e0/hmax
      hmin = rwork(7)
      if (hmin .lt. 0.0e0) go to 616
      seth = rwork(8)
      if (seth .lt. 0.0e0) go to 609
! check rtol and atol for legality. ------------------------------------
 60   rtoli = rtol(1)
      atoli = atol(1)
      do 65 i = 1,n
        if (itol .ge. 3) rtoli = rtol(i)
        if (itol .eq. 2 .or. itol .eq. 4) atoli = atol(i)
        if (rtoli .lt. 0.0e0) go to 619
        if (atoli .lt. 0.0e0) go to 620
 65     continue
!-----------------------------------------------------------------------
! compute required work array lengths, as far as possible, and test
! these against lrw and liw.  then set tentative pointers for work
! arrays.  pointers to rwork/iwork segments are named by prefixing l to
! the name of the segment.  e.g., the segment yh starts at rwork(lyh).
! segments of rwork (in order) are denoted  wm, yh, savf, ewt, acor.
! if miter = 1 or 2, the required length of the matrix work space wm
! is not yet known, and so a crude minimum value is used for the
! initial tests of lrw and liw, and yh is temporarily stored as far
! to the right in rwork as possible, to leave the maximum amount
! of space for wm for matrix preprocessing.  thus if miter = 1 or 2
! and moss .ne. 2, some of the segments of rwork are temporarily
! omitted, as they are not needed in the preprocessing.  these
! omitted segments are.. acor if istate = 1, ewt and acor if istate = 3
! and moss = 1, and savf, ewt, and acor if istate = 3 and moss = 0.
!-----------------------------------------------------------------------
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
      if (moss .eq. 0 .and. miter .ne. 0 .and. miter .ne. 3)   &
         leniw = leniw + n + 1
      iwork(18) = leniw
      if (lenrw .gt. lrw) go to 617
      if (leniw .gt. liw) go to 618
      lia = 31
      if (moss .eq. 0 .and. miter .ne. 0 .and. miter .ne. 3)   &
         leniw = leniw + iwork(lia+n) - 1
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
!-----------------------------------------------------------------------
! istate = 3.  move yh to its new location.
! note that only the part of yh needed for the next step, namely
! min(nq+1,maxord+2) columns, is actually moved.
! a temporary error weight array ewt is loaded if moss = 2.
! sparse matrix processing is done in iprep/prep if miter = 1 or 2.
! if maxord was reduced below nq, then the pointers are finally set
! so that savf is identical to yh(*,maxord+2).
!-----------------------------------------------------------------------
      lyhd = lyh - lyhn
      imax = lyhn - 1 + lenyhm
! move yh.  branch for move right, no move, or move left. --------------
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
! temporarily load ewt if miter = 1 or 2 and moss = 2. -----------------
      call ewset (n, itol, rtol, atol, rwork(lyh), rwork(lewt))
      do 82 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0e0) go to 621
 82     rwork(i+lewt-1) = 1.0e0/rwork(i+lewt-1)
 85   continue
! iprep and prep do sparse matrix preprocessing if miter = 1 or 2. -----
      lsavf = min0(lsavf,lrw)
      lewt = min0(lewt,lrw)
      lacor = min0(lacor,lrw)
      call iprep (neq, y, rwork, iwork(lia), iwork(lja), ipflag, f, jac,   &
                  ruserpar, nruserpar, iuserpar, niuserpar)
      lenrw = lwm - 1 + lenwk + lrest
      iwork(17) = lenrw
      if (ipflag .ne. -1) iwork(23) = ipian
      if (ipflag .ne. -1) iwork(24) = ipjan
      ipgo = -ipflag + 1
      go to (90, 628, 629, 630, 631, 632, 633), ipgo
 90   iwork(22) = lyh
      if (lenrw .gt. lrw) go to 617
! set flag to signal parameter changes to stode. -----------------------
 92   jstart = -1
      if (n .eq. nyh) go to 200
! neq was reduced.  zero part of yh to avoid undefined references. -----
      i1 = lyh + l*nyh
      i2 = lyh + (maxord + 1)*nyh - 1
      if (i1 .gt. i2) go to 200
      do 95 i = i1,i2
 95     rwork(i) = 0.0e0
      go to 200
!-----------------------------------------------------------------------
! block c.
! the next block is for the initial call only (istate = 1).
! it contains all remaining initializations, the initial call to f,
! the sparse matrix preprocessing (miter = 1 or 2), and the
! calculation of the initial step size.
! the error weights in ewt are inverted after being loaded.
!-----------------------------------------------------------------------
 100  continue
      lyh = lyhn
      iwork(22) = lyh
      tn = t
      nst = 0
      h = 1.0e0
      nnz = 0
      ngp = 0
      nzl = 0
      nzu = 0
! load the initial value vector in yh. ---------------------------------
      do 105 i = 1,n
 105    rwork(i+lyh-1) = y(i)
! initial call to f.  (lf0 points to yh(*,2).) -------------------------
      lf0 = lyh + nyh
      call f (neq, t, y, rwork(lf0),   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      nfe = 1
! load and invert the ewt array.  (h is temporarily set to 1.0.) -------
      call ewset (n, itol, rtol, atol, rwork(lyh), rwork(lewt))
      do 110 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0e0) go to 621
 110    rwork(i+lewt-1) = 1.0e0/rwork(i+lewt-1)
      if (miter .eq. 0 .or. miter .eq. 3) go to 120
! iprep and prep do sparse matrix preprocessing if miter = 1 or 2. -----
      lacor = min0(lacor,lrw)
      call iprep (neq, y, rwork, iwork(lia), iwork(lja), ipflag, f, jac,   &
                  ruserpar, nruserpar, iuserpar, niuserpar)
      lenrw = lwm - 1 + lenwk + lrest
      iwork(17) = lenrw
      if (ipflag .ne. -1) iwork(23) = ipian
      if (ipflag .ne. -1) iwork(24) = ipjan
      ipgo = -ipflag + 1
      go to (115, 628, 629, 630, 631, 632, 633), ipgo
 115  iwork(22) = lyh
      if (lenrw .gt. lrw) go to 617
! check tcrit for legality (itask = 4 or 5). ---------------------------
 120  continue
      if (itask .ne. 4 .and. itask .ne. 5) go to 125
      tcrit = rwork(1)
      if ((tcrit - tout)*(tout - t) .lt. 0.0e0) go to 625
      if (h0 .ne. 0.0e0 .and. (t + h0 - tcrit)*h0 .gt. 0.0e0)   &
         h0 = tcrit - t
! initialize all remaining parameters. ---------------------------------
 125  uround = r1mach(4)
      jstart = 0
      if (miter .ne. 0) rwork(lwm) = sqrt(uround)
      msbj = 50
      nslj = 0
      ccmxj = 0.2e0
      psmall = 1000.0e0*uround
      rbig = 0.01e0/psmall
      nhnil = 0
      nje = 0
      nlu = 0
      nslast = 0
      hu = 0.0e0
      nqu = 0
      ccmax = 0.3e0
      maxcor = 3
      msbp = 20
      mxncf = 10
!-----------------------------------------------------------------------
! the coding below computes the step size, h0, to be attempted on the
! first step, unless the user has supplied a value for this.
! first check that tout - t differs significantly from zero.
! a scalar tolerance quantity tol is computed, as max(rtol(i))
! if this is positive, or max(atol(i)/abs(y(i))) otherwise, adjusted
! so as to be between 100*uround and 1.0e-3.
! then the computed value h0 is given by..
!                                      neq
!   h0**2 = tol / ( w0**-2 + (1/neq) * sum ( f(i)/ywt(i) )**2  )
!                                       1
! where   w0     = max ( abs(t), abs(tout) ),
!         f(i)   = i-th component of initial value of f,
!         ywt(i) = ewt(i)/tol  (a weight for y(i)).
! the sign of h0 is inferred from the initial values of tout and t.
!-----------------------------------------------------------------------
      lf0 = lyh + nyh
      if (h0 .ne. 0.0e0) go to 180
      tdist = abs(tout - t)
      w0 = amax1(abs(t),abs(tout))
      if (tdist .lt. 2.0e0*uround*w0) go to 622
      tol = rtol(1)
      if (itol .le. 2) go to 140
      do 130 i = 1,n
 130    tol = amax1(tol,rtol(i))
 140  if (tol .gt. 0.0e0) go to 160
      atoli = atol(1)
      do 150 i = 1,n
        if (itol .eq. 2 .or. itol .eq. 4) atoli = atol(i)
        ayi = abs(y(i))
        if (ayi .ne. 0.0e0) tol = amax1(tol,atoli/ayi)
 150    continue
 160  tol = amax1(tol,100.0e0*uround)
      tol = amin1(tol,0.001e0)
      sum = vnorm (n, rwork(lf0), rwork(lewt))
      if (iok_vnorm .lt. 0) then
          istate = -901
          return
      end if
      sum = 1.0e0/(tol*w0*w0) + tol*sum**2
      h0 = 1.0e0/sqrt(sum)
      h0 = amin1(h0,tdist)
      h0 = sign(h0,tout-t)
! adjust h0 if necessary to meet hmax bound. ---------------------------
 180  rh = abs(h0)*hmxi
      if (rh .gt. 1.0e0) h0 = h0/rh
! load h with h0 and scale yh(*,2) by h0. ------------------------------
      h = h0
      do 190 i = 1,n
 190    rwork(i+lf0-1) = h0*rwork(i+lf0-1)
      go to 270
!-----------------------------------------------------------------------
! block d.
! the next code block is for continuation calls only (istate = 2 or 3)
! and is to check stop conditions before taking a step.
!-----------------------------------------------------------------------
 200  nslast = nst
      go to (210, 250, 220, 230, 240), itask
 210  if ((tn - tout)*h .lt. 0.0e0) go to 250
      call intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 220  tp = tn - hu*(1.0e0 + 100.0e0*uround)
      if ((tp - tout)*h .gt. 0.0e0) go to 623
      if ((tn - tout)*h .lt. 0.0e0) go to 250
      go to 400
 230  tcrit = rwork(1)
      if ((tn - tcrit)*h .gt. 0.0e0) go to 624
      if ((tcrit - tout)*h .lt. 0.0e0) go to 625
      if ((tn - tout)*h .lt. 0.0e0) go to 245
      call intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      if (iflag .ne. 0) go to 627
      t = tout
      go to 420
 240  tcrit = rwork(1)
      if ((tn - tcrit)*h .gt. 0.0e0) go to 624
 245  hmx = abs(tn) + abs(h)
      ihit = abs(tn - tcrit) .le. 100.0e0*uround*hmx
      if (ihit) go to 400
      tnext = tn + h*(1.0e0 + 4.0e0*uround)
      if ((tnext - tcrit)*h .le. 0.0e0) go to 250
      h = (tcrit - tn)*(1.0e0 - 4.0e0*uround)
      if (istate .eq. 2) jstart = -2
!-----------------------------------------------------------------------
! block e.
! the next block is normally executed for all calls and contains
! the call to the one-step core integrator stode.
!
! this is a looping point for the integration steps.
!
! first check for too many steps being taken, update ewt (if not at
! start of problem), check for too much accuracy being requested, and
! check for h below the roundoff level in t.
!-----------------------------------------------------------------------
 250  continue
      if ((nst-nslast) .ge. mxstep) go to 500
      call ewset (n, itol, rtol, atol, rwork(lyh), rwork(lewt))
      do 260 i = 1,n
        if (rwork(i+lewt-1) .le. 0.0e0) go to 510
 260    rwork(i+lewt-1) = 1.0e0/rwork(i+lewt-1)
 270  tolsf = uround*vnorm (n, rwork(lyh), rwork(lewt))
      if (tolsf .le. 1.0e0) go to 280
! diagnostic dump
      tolsf = tolsf*2.0e0
      if (nst .eq. 0) go to 626
      go to 520
 280  if ((tn + h) .ne. tn) go to 290
      nhnil = nhnil + 1
      if (nhnil .gt. mxhnil) go to 290
      call xerrwv('lsodes-- warning..internal t (=r1) and h (=r2) are',   &
         50, 101, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '      such that in the machine, t + h = t on the next step  ',   &
         60, 101, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      (h = step size). solver will continue anyway',   &
         50, 101, 0, 0, 0, 0, 2, tn, h)
      if (nhnil .lt. mxhnil) go to 290
      call xerrwv('lsodes-- above warning has been issued i1 times.  ',   &
         50, 102, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      it will not be issued again for this problem',   &
         50, 102, 0, 1, mxhnil, 0, 0, 0.0e0, 0.0e0)
 290  continue
!-----------------------------------------------------------------------
!    call stode(neq,y,yh,nyh,yh,ewt,savf,acor,wm,wm,f,jac,prjs,slss)
!-----------------------------------------------------------------------
      call stode_lsodes (neq, y, rwork(lyh), nyh, rwork(lyh), rwork(lewt),   &
         rwork(lsavf), rwork(lacor), rwork(lwm), rwork(lwm),   &
         f, jac, prjs, slss,   &
         ruserpar, nruserpar, iuserpar, niuserpar )
      kgo = 1 - kflag
      go to (300, 530, 540, 550), kgo
!-----------------------------------------------------------------------
! block f.
! the following block handles the case of a successful return from the
! core integrator (kflag = 0).  test for stop conditions.
!-----------------------------------------------------------------------
 300  init = 1
      go to (310, 400, 330, 340, 350), itask
! itask = 1.  if tout has been reached, interpolate. -------------------
 310  if ((tn - tout)*h .lt. 0.0e0) go to 250
      call intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      t = tout
      go to 420
! itask = 3.  jump to exit if tout was reached. ------------------------
 330  if ((tn - tout)*h .ge. 0.0e0) go to 400
      go to 250
! itask = 4.  see if tout or tcrit was reached.  adjust h if necessary.
 340  if ((tn - tout)*h .lt. 0.0e0) go to 345
      call intdy (tout, 0, rwork(lyh), nyh, y, iflag)
      t = tout
      go to 420
 345  hmx = abs(tn) + abs(h)
      ihit = abs(tn - tcrit) .le. 100.0e0*uround*hmx
      if (ihit) go to 400
      tnext = tn + h*(1.0e0 + 4.0e0*uround)
      if ((tnext - tcrit)*h .le. 0.0e0) go to 250
      h = (tcrit - tn)*(1.0e0 - 4.0e0*uround)
      jstart = -2
      go to 250
! itask = 5.  see if tcrit was reached and jump to exit. ---------------
 350  hmx = abs(tn) + abs(h)
      ihit = abs(tn - tcrit) .le. 100.0e0*uround*hmx
!-----------------------------------------------------------------------
! block g.
! the following block handles all successful returns from lsodes.
! if itask .ne. 1, y is loaded from yh and t is set accordingly.
! istate is set to 2, the illegal input counter is zeroed, and the
! optional outputs are loaded into the work arrays before returning.
! if istate = 1 and tout = t, there is a return with no action taken,
! except that if this has happened repeatedly, the run is terminated.
!-----------------------------------------------------------------------
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
      if (iok_vnorm .lt. 0) istate = -912
      return
!
 430  ntrep = ntrep + 1
!     if (ntrep .lt. 5) return
      if (ntrep .lt. 5) then
          if (iok_vnorm .lt. 0) istate = -913
          return
      end if
      call xerrwv(   &
        'lsodes-- repeated calls with istate = 1 and tout = t (=r1)  ',   &
         60, 301, 0, 0, 0, 0, 1, t, 0.0e0)
      go to 800
!-----------------------------------------------------------------------
! block h.
! the following block handles all unsuccessful returns other than
! those for illegal input.  first the error message routine is called.
! if there was an error test or convergence test failure, imxer is set.
! then y is loaded from yh, t is set to tn, and the illegal input
! counter illin is set to 0.  the optional outputs are loaded into
! the work arrays before returning.
!-----------------------------------------------------------------------
! the maximum number of steps was taken before reaching tout. ----------
 500  call xerrwv('lsodes-- at current t (=r1), mxstep (=i1) steps   ',   &
         50, 201, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      taken on this call before reaching tout     ',   &
         50, 201, 0, 1, mxstep, 0, 1, tn, 0.0e0)
      istate = -1
      go to 580
! ewt(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  ewti = rwork(lewt+i-1)
      call xerrwv('lsodes-- at t (=r1), ewt(i1) has become r2 .le. 0.',   &
         50, 202, 0, 1, i, 0, 2, tn, ewti)
      istate = -6
      go to 580
! too much accuracy requested for machine precision. -------------------
 520  call xerrwv('lsodes-- at t (=r1), too much accuracy requested  ',   &
         50, 203, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      for precision of machine..  see tolsf (=r2) ',   &
         50, 203, 0, 0, 0, 0, 2, tn, tolsf)
      rwork(14) = tolsf
      istate = -2
      go to 580
! kflag = -1.  error test failed repeatedly or with abs(h) = hmin. -----
 530  call xerrwv('lsodes-- at t(=r1) and step size h(=r2), the error',   &
         50, 204, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      test failed repeatedly or with abs(h) = hmin',   &
         50, 204, 0, 0, 0, 0, 2, tn, h)
      istate = -4
      go to 560
! kflag = -2.  convergence failed repeatedly or with abs(h) = hmin. ----
 540  call xerrwv('lsodes-- at t (=r1) and step size h (=r2), the    ',   &
         50, 205, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      corrector convergence failed repeatedly     ',   &
         50, 205, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      or with abs(h) = hmin   ',   &
         30, 205, 0, 0, 0, 0, 2, tn, h)
      istate = -5
      go to 560
! kflag = -3.  fatal error flag returned by prjs or slss (cdrv). -------
 550  call xerrwv('lsodes-- at t (=r1) and step size h (=r2), a fatal',   &
         50, 207, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      error flag was returned by cdrv (by way of  ',   &
         50, 207, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv('      subroutine prjs or slss)',   &
         30, 207, 0, 0, 0, 0, 2, tn, h)
      istate = -7
      go to 580
! compute imxer if relevant. -------------------------------------------
 560  big = 0.0e0
      imxer = 1
      do 570 i = 1,n
        size = abs(rwork(i+lacor-1)*rwork(i+lewt-1))
        if (big .ge. size) go to 570
        big = size
        imxer = i
 570    continue
      iwork(16) = imxer
! set y vector, t, illin, and optional outputs. ------------------------
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
      if (iok_vnorm .lt. 0) istate = -914
      return
!-----------------------------------------------------------------------
! block i.
! the following block handles all error returns due to illegal input
! (istate = -3), as detected before calling the core integrator.
! first the error message routine is called.  then if there have been
! 5 consecutive such returns just before this call to the solver,
! the run is halted.
!-----------------------------------------------------------------------
 601  call xerrwv('lsodes-- istate (=i1) illegal ',   &
         30, 1, 0, 1, istate, 0, 0, 0.0e0, 0.0e0)
      go to 700
 602  call xerrwv('lsodes-- itask (=i1) illegal  ',   &
         30, 2, 0, 1, itask, 0, 0, 0.0e0, 0.0e0)
      go to 700
 603  call xerrwv('lsodes-- istate .gt. 1 but lsodes not initialized ',   &
         50, 3, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      go to 700
 604  call xerrwv('lsodes-- neq (=i1) .lt. 1     ',   &
         30, 4, 0, 1, neq(1), 0, 0, 0.0e0, 0.0e0)
      go to 700
 605  call xerrwv('lsodes-- istate = 3 and neq increased (i1 to i2)  ',   &
         50, 5, 0, 2, n, neq(1), 0, 0.0e0, 0.0e0)
      go to 700
 606  call xerrwv('lsodes-- itol (=i1) illegal   ',   &
         30, 6, 0, 1, itol, 0, 0, 0.0e0, 0.0e0)
      go to 700
 607  call xerrwv('lsodes-- iopt (=i1) illegal   ',   &
         30, 7, 0, 1, iopt, 0, 0, 0.0e0, 0.0e0)
      go to 700
 608  call xerrwv('lsodes-- mf (=i1) illegal     ',   &
         30, 8, 0, 1, mf, 0, 0, 0.0e0, 0.0e0)
      go to 700
 609  call xerrwv('lsodes-- seth (=r1) .lt. 0.0  ',   &
         30, 9, 0, 0, 0, 0, 1, seth, 0.0e0)
      go to 700
 611  call xerrwv('lsodes-- maxord (=i1) .lt. 0  ',   &
         30, 11, 0, 1, maxord, 0, 0, 0.0e0, 0.0e0)
      go to 700
 612  call xerrwv('lsodes-- mxstep (=i1) .lt. 0  ',   &
         30, 12, 0, 1, mxstep, 0, 0, 0.0e0, 0.0e0)
      go to 700
 613  call xerrwv('lsodes-- mxhnil (=i1) .lt. 0  ',   &
         30, 13, 0, 1, mxhnil, 0, 0, 0.0e0, 0.0e0)
      go to 700
 614  call xerrwv('lsodes-- tout (=r1) behind t (=r2)      ',   &
         40, 14, 0, 0, 0, 0, 2, tout, t)
      call xerrwv('      integration direction is given by h0 (=r1)  ',   &
         50, 14, 0, 0, 0, 0, 1, h0, 0.0e0)
      go to 700
 615  call xerrwv('lsodes-- hmax (=r1) .lt. 0.0  ',   &
         30, 15, 0, 0, 0, 0, 1, hmax, 0.0e0)
      go to 700
 616  call xerrwv('lsodes-- hmin (=r1) .lt. 0.0  ',   &
         30, 16, 0, 0, 0, 0, 1, hmin, 0.0e0)
      go to 700
 617  call xerrwv('lsodes-- rwork length is insufficient to proceed. ',   &
         50, 17, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. lenrw (=i1), exceeds lrw (=i2)',   &
         60, 17, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 618  call xerrwv('lsodes-- iwork length is insufficient to proceed. ',   &
         50, 18, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. leniw (=i1), exceeds liw (=i2)',   &
         60, 18, 0, 2, leniw, liw, 0, 0.0e0, 0.0e0)
      go to 700
 619  call xerrwv('lsodes-- rtol(i1) is r1 .lt. 0.0        ',   &
         40, 19, 0, 1, i, 0, 1, rtoli, 0.0e0)
      go to 700
 620  call xerrwv('lsodes-- atol(i1) is r1 .lt. 0.0        ',   &
         40, 20, 0, 1, i, 0, 1, atoli, 0.0e0)
      go to 700
 621  ewti = rwork(lewt+i-1)
      call xerrwv('lsodes-- ewt(i1) is r1 .le. 0.0         ',   &
         40, 21, 0, 1, i, 0, 1, ewti, 0.0e0)
      go to 700
 622  call xerrwv(   &
        'lsodes-- tout (=r1) too close to t(=r2) to start integration',   &
         60, 22, 0, 0, 0, 0, 2, tout, t)
      go to 700
 623  call xerrwv(   &
        'lsodes-- itask = i1 and tout (=r1) behind tcur - hu (= r2)  ',   &
         60, 23, 0, 1, itask, 0, 2, tout, tp)
      go to 700
 624  call xerrwv(   &
        'lsodes-- itask = 4 or 5 and tcrit (=r1) behind tcur (=r2)   ',   &
         60, 24, 0, 0, 0, 0, 2, tcrit, tn)
      go to 700
 625  call xerrwv(   &
        'lsodes-- itask = 4 or 5 and tcrit (=r1) behind tout (=r2)   ',   &
         60, 25, 0, 0, 0, 0, 2, tcrit, tout)
      go to 700
 626  call xerrwv('lsodes-- at start of problem, too much accuracy   ',   &
         50, 26, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '      requested for precision of machine..  see tolsf (=r1) ',   &
         60, 26, 0, 0, 0, 0, 1, tolsf, 0.0e0)
      rwork(14) = tolsf
      go to 700
 627  call xerrwv('lsodes-- trouble from intdy. itask = i1, tout = r1',   &
         50, 27, 0, 1, itask, 0, 1, tout, 0.0e0)
      go to 700
 628  call xerrwv(   &
        'lsodes-- rwork length insufficient (for subroutine prep).   ',   &
         60, 28, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. lenrw (=i1), exceeds lrw (=i2)',   &
         60, 28, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 629  call xerrwv(   &
        'lsodes-- rwork length insufficient (for subroutine jgroup). ',   &
         60, 29, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. lenrw (=i1), exceeds lrw (=i2)',   &
         60, 29, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 630  call xerrwv(   &
        'lsodes-- rwork length insufficient (for subroutine odrv).   ',   &
         60, 30, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. lenrw (=i1), exceeds lrw (=i2)',   &
         60, 30, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 631  call xerrwv(   &
        'lsodes-- error from odrv in yale sparse matrix package      ',   &
         60, 31, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      imul = (iys - 1)/n
      irem = iys - imul*n
      call xerrwv(   &
        '      at t (=r1), odrv returned error flag = i1*neq + i2.   ',   &
         60, 31, 0, 2, imul, irem, 1, tn, 0.0e0)
      go to 700
 632  call xerrwv(   &
        'lsodes-- rwork length insufficient (for subroutine cdrv).   ',   &
         60, 32, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      call xerrwv(   &
        '        length needed is .ge. lenrw (=i1), exceeds lrw (=i2)',   &
         60, 32, 0, 2, lenrw, lrw, 0, 0.0e0, 0.0e0)
      go to 700
 633  call xerrwv(   &
        'lsodes-- error from cdrv in yale sparse matrix package      ',   &
         60, 33, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      imul = (iys - 1)/n
      irem = iys - imul*n
      call xerrwv(   &
        '      at t (=r1), cdrv returned error flag = i1*neq + i2.   ',   &
         60, 33, 0, 2, imul, irem, 1, tn, 0.0e0)
      if (imul .eq. 2) call xerrwv(   &
        '        duplicate entry in sparsity structure descriptors   ',   &
         60, 33, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
      if (imul .eq. 3 .or. imul .eq. 6) call xerrwv(   &
        '        insufficient storage for nsfc (called by cdrv)      ',   &
         60, 33, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
!
 700  if (illin .eq. 5) go to 710
      illin = illin + 1
      istate = -3
      if (iok_vnorm .lt. 0) istate = -915
      return
 710  call xerrwv('lsodes-- repeated occurrences of illegal input    ',   &
         50, 302, 0, 0, 0, 0, 0, 0.0e0, 0.0e0)
!
 800  call xerrwv('lsodes-- run aborted.. apparent infinite loop     ',   &
         50, 303, 2, 0, 0, 0, 0, 0.0e0, 0.0e0)
      if (iok_vnorm .lt. 0) istate = -916
      return
!----------------------- end of subroutine lsodes ----------------------
      end subroutine lsodes_solver
      subroutine adjlr (n, isp, ldif)
      integer n, isp, ldif
!jdf  dimension isp(1)
      dimension isp(*)
!-----------------------------------------------------------------------
! this routine computes an adjustment, ldif, to the required
! integer storage space in iwk (sparse matrix work space).
! it is called only if the word length ratio is lrat = 1.
! this is to account for the possibility that the symbolic lu phase
! may require more storage than the numerical lu and solution phases.
!-----------------------------------------------------------------------
      integer ip, jlmax, jumax, lnfc, lsfc, nzlu
!
      ip = 2*n + 1
! get jlmax = ijl(n) and jumax = iju(n) (sizes of jl and ju). ----------
      jlmax = isp(ip)
      jumax = isp(ip+ip)
! nzlu = (size of l) + (size of u) = (il(n+1)-il(1)) + (iu(n+1)-iu(1)).
      nzlu = isp(n+1) - isp(1) + isp(ip+n+1) - isp(ip+1)
      lsfc = 12*n + 3 + 2*max0(jlmax,jumax)
      lnfc = 9*n + 2 + jlmax + jumax + nzlu
      ldif = max0(0, lsfc - lnfc)
      return
!----------------------- end of subroutine adjlr -----------------------
      end subroutine adjlr               
      subroutine cdrv   &
           (n, r,c,ic, ia,ja,a, b, z, nsp,isp,rsp,esp, path, flag)
!lll. optimize
!*** subroutine cdrv
!*** driver for subroutines for solving sparse nonsymmetric systems of
!       linear equations (compressed pointer storage)
!
!
!    parameters
!    class abbreviations are--
!       n - integer variable
!       f - real variable
!       v - supplies a value to the driver
!       r - returns a result from the driver
!       i - used internally by the driver
!       a - array
!
! class - parameter
! ------+----------
!       -
!         the nonzero entries of the coefficient matrix m are stored
!    row-by-row in the array a.  to identify the individual nonzero
!    entries in each row, we need to know in which column each entry
!    lies.  the column indices which correspond to the nonzero entries
!    of m are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
!    ja(k) = j.  in addition, we need to know where each row starts and
!    how long it is.  the index positions in ja and a where the rows of
!    m begin are stored in the array ia.  i.e., if m(i,j) is the first
!    nonzero entry (stored) in the i-th row and a(k) = m(i,j),  then
!    ia(i) = k.  moreover, the index in ja and a of the first location
!    following the last element in the last row is stored in ia(n+1).
!    thus, the number of entries in the i-th row is given by
!    ia(i+1) - ia(i),  the nonzero entries of the i-th row are stored
!    consecutively in
!            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
!    and the corresponding column indices are stored consecutively in
!            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
!    for example, the 5 by 5 matrix
!                ( 1. 0. 2. 0. 0.)
!                ( 0. 3. 0. 0. 0.)
!            m = ( 0. 4. 5. 6. 0.)
!                ( 0. 0. 0. 7. 0.)
!                ( 0. 0. 0. 8. 9.)
!    would be stored as
!               - 1  2  3  4  5  6  7  8  9
!            ---+--------------------------
!            ia - 1  3  4  7  8 10
!            ja - 1  3  2  2  3  4  4  4  5
!             a - 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
!
! nv    - n     - number of variables/equations.
! fva   - a     - nonzero entries of the coefficient matrix m, stored
!       -           by rows.
!       -           size = number of nonzero entries in m.
! nva   - ia    - pointers to delimit the rows in a.
!       -           size = n+1.
! nva   - ja    - column numbers corresponding to the elements of a.
!       -           size = size of a.
! fva   - b     - right-hand side b.  b and z can the same array.
!       -           size = n.
! fra   - z     - solution x.  b and z can be the same array.
!       -           size = n.
!
!         the rows and columns of the original matrix m can be
!    reordered (e.g., to reduce fillin or ensure numerical stability)
!    before calling the driver.  if no reordering is done, then set
!    r(i) = c(i) = ic(i) = i  for i=1,...,n.  the solution z is returned
!    in the original order.
!         if the columns have been reordered (i.e.,  c(i).ne.i  for some
!    i), then the driver will call a subroutine (nroc) which rearranges
!    each row of ja and a, leaving the rows in the original order, but
!    placing the elements of each row in increasing order with respect
!    to the new ordering.  if  path.ne.1,  then nroc is assumed to have
!    been called already.
!
! nva   - r     - ordering of the rows of m.
!       -           size = n.
! nva   - c     - ordering of the columns of m.
!       -           size = n.
! nva   - ic    - inverse of the ordering of the columns of m.  i.e.,
!       -           ic(c(i)) = i  for i=1,...,n.
!       -           size = n.
!
!         the solution of the system of linear equations is divided into
!    three stages --
!      nsfc -- the matrix m is processed symbolically to determine where
!               fillin will occur during the numeric factorization.
!      nnfc -- the matrix m is factored numerically into the product ldu
!               of a unit lower triangular matrix l, a diagonal matrix
!               d, and a unit upper triangular matrix u, and the system
!               mx = b  is solved.
!      nnsc -- the linear system  mx = b  is solved using the ldu
!  or           factorization from nnfc.
!      nntc -- the transposed linear system  mt x = b  is solved using
!               the ldu factorization from nnf.
!    for several systems whose coefficient matrices have the same
!    nonzero structure, nsfc need be done only once (for the first
!    system).  then nnfc is done once for each additional system.  for
!    several systems with the same coefficient matrix, nsfc and nnfc
!    need be done only once (for the first system).  then nnsc or nntc
!    is done once for each additional right-hand side.
!
! nv    - path  - path specification.  values and their meanings are --
!       -           1  perform nroc, nsfc, and nnfc.
!       -           2  perform nnfc only  (nsfc is assumed to have been
!       -               done in a manner compatible with the storage
!       -               allocation used in the driver).
!       -           3  perform nnsc only  (nsfc and nnfc are assumed to
!       -               have been done in a manner compatible with the
!       -               storage allocation used in the driver).
!       -           4  perform nntc only  (nsfc and nnfc are assumed to
!       -               have been done in a manner compatible with the
!       -               storage allocation used in the driver).
!       -           5  perform nroc and nsfc.
!
!         various errors are detected by the driver and the individual
!    subroutines.
!
! nr    - flag  - error flag.  values and their meanings are --
!       -             0     no errors detected
!       -             n+k   null row in a  --  row = k
!       -            2n+k   duplicate entry in a  --  row = k
!       -            3n+k   insufficient storage in nsfc  --  row = k
!       -            4n+1   insufficient storage in nnfc
!       -            5n+k   null pivot  --  row = k
!       -            6n+k   insufficient storage in nsfc  --  row = k
!       -            7n+1   insufficient storage in nnfc
!       -            8n+k   zero pivot  --  row = k
!       -           10n+1   insufficient storage in cdrv
!       -           11n+1   illegal path specification
!
!         working storage is needed for the factored form of the matrix
!    m plus various temporary vectors.  the arrays isp and rsp should be
!    equivalenced.  integer storage is allocated from the beginning of
!    isp and real storage from the end of rsp.
!
! nv    - nsp   - declared dimension of rsp.  nsp generally must
!       -           be larger than  8n+2 + 2k  (where  k = (number of
!       -           nonzero entries in m)).
! nvira - isp   - integer working storage divided up into various arrays
!       -           needed by the subroutines.  isp and rsp should be
!       -           equivalenced.
!       -           size = lratio*nsp.
! fvira - rsp   - real working storage divided up into various arrays
!       -           needed by the subroutines.  isp and rsp should be
!       -           equivalenced.
!       -           size = nsp.
! nr    - esp   - if sufficient storage was available to perform the
!       -           symbolic factorization (nsfc), then esp is set to
!       -           the amount of excess storage provided (negative if
!       -           insufficient storage was available to perform the
!       -           numeric factorization (nnfc)).
!
!
!  conversion to double precision
!
!    to convert these routines for double precision arrays..
!    (1) use the double precision declarations in place of the real
!    declarations in each subprogram, as given in comment cards.
!    (2) change the data-loaded value of the integer  lratio
!    in subroutine cdrv, as indicated below.
!    (3) change e0 to d0 in the constants in statement number 10
!    in subroutine nnfc and the line following that.
!
!jdf  integer  r(1), c(1), ic(1),  ia(1), ja(1),  isp(1), esp,  path,
!jdf *   flag,  d, u, q, row, tmp, ar,  umax
!jdf  real  a(1), b(1), z(1), rsp(1)
      integer  r(*), c(*), ic(*),  ia(*), ja(*),  isp(*), esp,  path,   &
         flag,  d, u, q, row, tmp, ar,  umax
      real  a(*), b(*), z(*), rsp(*)
!     double precision  a(1), b(1), z(1), rsp(1)
!
!  set lratio equal to the ratio between the length of floating point
!  and integer array data.  e. g., lratio = 1 for (real, integer),
!  lratio = 2 for (double precision, integer)
!
      data lratio/1/
!
      if (path.lt.1 .or. 5.lt.path)  go to 111
!******initialize and divide up temporary storage  *******************
      il   = 1
      ijl  = il  + (n+1)
      iu   = ijl +   n
      iju  = iu  + (n+1)
      irl  = iju +   n
      jrl  = irl +   n
      jl   = jrl +   n
!
!  ******  reorder a if necessary, call nsfc if flag is set  ***********
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
!
        do 1 i=1,n
          if (c(i).ne.i)  go to 2
   1      continue
        go to 3
   2    ar = nsp + 1 - n
        call  nroc   &
           (n, ic, ia,ja,a, isp(il), rsp(ar), isp(iu), flag)
        if (flag.ne.0)  go to 100
!
   3    call  nsfc   &
           (n, r, ic, ia,ja,   &
            jlmax, isp(il), isp(jl), isp(ijl),   &
            jumax, isp(iu), isp(jutmp), isp(iju),   &
            isp(q), isp(ira), isp(jra), isp(irac),   &
            isp(irl), isp(jrl), isp(iru), isp(jru),  flag)
        if(flag .ne. 0)  go to 100
!  ******  move ju next to jl  *****************************************
        jlmax = isp(ijl+n-1)
        ju    = jl + jlmax
        jumax = isp(iju+n-1)
        if (jumax.le.0)  go to 5
        do 4 j=1,jumax
   4      isp(ju+j-1) = isp(jutmp+j-1)
!
!  ******  call remaining subroutines  *********************************
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
!
      if ((path-1) * (path-2) .ne. 0)  go to 6
        if (umax.lt.0)  go to 110
        call nnfc   &
           (n,  r, c, ic,  ia, ja, a, z, b,   &
            lmax, isp(il), isp(jl), isp(ijl), rsp(l),  rsp(d),   &
            umax, isp(iu), isp(ju), isp(iju), rsp(u),   &
            rsp(row), rsp(tmp),  isp(irl), isp(jrl),  flag)
        if(flag .ne. 0)  go to 100
!
   6  if ((path-3) .ne. 0)  go to 7
        call nnsc   &
           (n,  r, c,  isp(il), isp(jl), isp(ijl), rsp(l),   &
            rsp(d),    isp(iu), isp(ju), isp(iju), rsp(u),   &
            z, b,  rsp(tmp))
!
   7  if ((path-4) .ne. 0)  go to 8
        call nntc   &
           (n,  r, c,  isp(il), isp(jl), isp(ijl), rsp(l),   &
            rsp(d),    isp(iu), isp(ju), isp(iju), rsp(u),   &
            z, b,  rsp(tmp))
   8  return
!
! ** error.. error detected in nroc, nsfc, nnfc, or nnsc
 100  return
! ** error.. insufficient storage
 110  flag = 10*n + 1
      return
! ** error.. illegal path specification
 111  flag = 11*n + 1
      return
      end subroutine cdrv
      subroutine cfode (meth, elco, tesco)
!lll. optimize
      integer meth
      integer i, ib, nq, nqm1, nqp1
      real elco, tesco
      real agamq, fnq, fnqm1, pc, pint, ragq,   &
         rqfac, rq1fac, tsign, xpin
      dimension elco(13,12), tesco(3,12)
!-----------------------------------------------------------------------
! cfode is called by the integrator routine to set coefficients
! needed there.  the coefficients for the current method, as
! given by the value of meth, are set for all orders and saved.
! the maximum order assumed here is 12 if meth = 1 and 5 if meth = 2.
! (a smaller value of the maximum order is also allowed.)
! cfode is called once at the beginning of the problem,
! and is not called again unless and until meth is changed.
!
! the elco array contains the basic method coefficients.
! the coefficients el(i), 1 .le. i .le. nq+1, for the method of
! order nq are stored in elco(i,nq).  they are given by a genetrating
! polynomial, i.e.,
!     l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq.
! for the implicit adams methods, l(x) is given by
!     dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0.
! for the bdf methods, l(x) is given by
!     l(x) = (x+1)*(x+2)* ... *(x+nq)/k,
! where         k = factorial(nq)*(1 + 1/2 + ... + 1/nq).
!
! the tesco array contains test constants used for the
! local error test and the selection of step size and/or order.
! at order nq, tesco(k,nq) is used for the selection of step
! size at order nq - 1 if k = 1, at order nq if k = 2, and at order
! nq + 1 if k = 3.
!-----------------------------------------------------------------------
      dimension pc(12)
!
      go to (100, 200), meth
!
 100  elco(1,1) = 1.0e0
      elco(2,1) = 1.0e0
      tesco(1,1) = 0.0e0
      tesco(2,1) = 2.0e0
      tesco(1,2) = 1.0e0
      tesco(3,12) = 0.0e0
      pc(1) = 1.0e0
      rqfac = 1.0e0
      do 140 nq = 2,12
!-----------------------------------------------------------------------
! the pc array will contain the coefficients of the polynomial
!     p(x) = (x+1)*(x+2)*...*(x+nq-1).
! initially, p(x) = 1.
!-----------------------------------------------------------------------
        rq1fac = rqfac
        rqfac = rqfac/float(nq)
        nqm1 = nq - 1
        fnqm1 = float(nqm1)
        nqp1 = nq + 1
! form coefficients of p(x)*(x+nq-1). ----------------------------------
        pc(nq) = 0.0e0
        do 110 ib = 1,nqm1
          i = nqp1 - ib
 110      pc(i) = pc(i-1) + fnqm1*pc(i)
        pc(1) = fnqm1*pc(1)
! compute integral, -1 to 0, of p(x) and x*p(x). -----------------------
        pint = pc(1)
        xpin = pc(1)/2.0e0
        tsign = 1.0e0
        do 120 i = 2,nq
          tsign = -tsign
          pint = pint + tsign*pc(i)/float(i)
 120      xpin = xpin + tsign*pc(i)/float(i+1)
! store coefficients in elco and tesco. --------------------------------
        elco(1,nq) = pint*rq1fac
        elco(2,nq) = 1.0e0
        do 130 i = 2,nq
 130      elco(i+1,nq) = rq1fac*pc(i)/float(i)
        agamq = rqfac*xpin
        ragq = 1.0e0/agamq
        tesco(2,nq) = ragq
        if (nq .lt. 12) tesco(1,nqp1) = ragq*rqfac/float(nqp1)
        tesco(3,nqm1) = ragq
 140    continue
      return
!
 200  pc(1) = 1.0e0
      rq1fac = 1.0e0
      do 230 nq = 1,5
!-----------------------------------------------------------------------
! the pc array will contain the coefficients of the polynomial
!     p(x) = (x+1)*(x+2)*...*(x+nq).
! initially, p(x) = 1.
!-----------------------------------------------------------------------
        fnq = float(nq)
        nqp1 = nq + 1
! form coefficients of p(x)*(x+nq). ------------------------------------
        pc(nqp1) = 0.0e0
        do 210 ib = 1,nq
          i = nq + 2 - ib
 210      pc(i) = pc(i-1) + fnq*pc(i)
        pc(1) = fnq*pc(1)
! store coefficients in elco and tesco. --------------------------------
        do 220 i = 1,nqp1
 220      elco(i,nq) = pc(i)/pc(2)
        elco(2,nq) = 1.0e0
        tesco(1,nq) = rq1fac
        tesco(2,nq) = float(nqp1)/elco(1,nq)
        tesco(3,nq) = float(nq+2)/elco(1,nq)
        rq1fac = rq1fac/fnq
 230    continue
      return
!----------------------- end of subroutine cfode -----------------------
      end subroutine cfode                    
      subroutine cntnzu (n, ia, ja, nzsut)
      integer n, ia, ja, nzsut
!jdf  dimension ia(1), ja(1)
      dimension ia(*), ja(*)
!-----------------------------------------------------------------------
! this routine counts the number of nonzero elements in the strict
! upper triangle of the matrix m + m(transpose), where the sparsity
! structure of m is given by pointer arrays ia and ja.
! this is needed to compute the storage requirements for the
! sparse matrix reordering operation in odrv.
!-----------------------------------------------------------------------
      integer ii, jj, j, jmin, jmax, k, kmin, kmax, num
!
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
!----------------------- end of subroutine cntnzu ----------------------
      end subroutine cntnzu                   
      subroutine ewset (n, itol, rtol, atol, ycur, ewt)
!lll. optimize
!-----------------------------------------------------------------------
! this subroutine sets the error weight vector ewt according to
!     ewt(i) = rtol(i)*abs(ycur(i)) + atol(i),  i = 1,...,n,
! with the subscript on rtol and/or atol possibly replaced by 1 above,
! depending on the value of itol.
!-----------------------------------------------------------------------
      integer n, itol
      integer i
      real rtol, atol, ycur, ewt
!jdf  dimension rtol(1), atol(1), ycur(n), ewt(n)
      dimension rtol(*), atol(*), ycur(n), ewt(n)
!
      go to (10, 20, 30, 40), itol
 10   continue
      do 15 i = 1,n
 15     ewt(i) = rtol(1)*abs(ycur(i)) + atol(1)
      return
 20   continue
      do 25 i = 1,n
 25     ewt(i) = rtol(1)*abs(ycur(i)) + atol(i)
      return
 30   continue
      do 35 i = 1,n
 35     ewt(i) = rtol(i)*abs(ycur(i)) + atol(1)
      return
 40   continue
      do 45 i = 1,n
 45     ewt(i) = rtol(i)*abs(ycur(i)) + atol(i)
      return
!----------------------- end of subroutine ewset -----------------------
      end subroutine ewset                                 
      subroutine intdy (t, k, yh, nyh, dky, iflag)
!lll. optimize
      integer k, nyh, iflag
      integer iownd, iowns,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, ic, j, jb, jb2, jj, jj1, jp1
      real t, yh, dky
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real c, r, s, tp
!jdf  dimension yh(nyh,1), dky(1)
      dimension yh(nyh,*), dky(*)
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         iownd(14), iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!-----------------------------------------------------------------------
! intdy computes interpolated values of the k-th derivative of the
! dependent variable vector y, and stores it in dky.  this routine
! is called within the package with k = 0 and t = tout, but may
! also be called by the user for any k up to the current order.
! (see detailed instructions in the usage documentation.)
!-----------------------------------------------------------------------
! the computed values in dky are gotten by interpolation using the
! nordsieck history array yh.  this array corresponds uniquely to a
! vector-valued polynomial of degree nqcur or less, and dky is set
! to the k-th derivative of this polynomial at t.
! the formula for dky is..
!              q
!  dky(i)  =  sum  c(j,k) * (t - tn)**(j-k) * h**(-j) * yh(i,j+1)
!             j=k
! where  c(j,k) = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur.
! the quantities  nq = nqcur, l = nq+1, n = neq, tn, and h are
! communicated by common.  the above sum is done in reverse order.
! iflag is returned negative if either k or t is out of bounds.
!-----------------------------------------------------------------------
      iflag = 0
      if (k .lt. 0 .or. k .gt. nq) go to 80
      tp = tn - hu -  100.0e0*uround*(tn + hu)
      if ((t-tp)*(t-tn) .gt. 0.0e0) go to 90
!
      s = (t - tn)/h
      ic = 1
      if (k .eq. 0) go to 15
      jj1 = l - k
      do 10 jj = jj1,nq
 10     ic = ic*jj
 15   c = float(ic)
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
 35     c = float(ic)
        do 40 i = 1,n
 40       dky(i) = c*yh(i,jp1) + s*dky(i)
 50     continue
      if (k .eq. 0) return
 55   r = h**(-k)
      do 60 i = 1,n
 60     dky(i) = r*dky(i)
      return
!
 80   call xerrwv('intdy--  k (=i1) illegal      ',   &
         30, 51, 0, 1, k, 0, 0, 0.0e0, 0.0e0)
      iflag = -1
      return
 90   call xerrwv('intdy--  t (=r1) illegal      ',   &
         30, 52, 0, 0, 0, 0, 1, t, 0.0e0)
      call xerrwv(   &
        '      t not in interval tcur - hu (= r1) to tcur (=r2)      ',   &
         60, 52, 0, 0, 0, 0, 2, tp, tn)
      iflag = -2
      return
!----------------------- end of subroutine intdy -----------------------
      end subroutine intdy                            
      subroutine iprep (neq, y, rwork, ia, ja, ipflag, f, jac,   &
                        ruserpar, nruserpar, iuserpar, niuserpar )
!lll. optimize
      external f, jac
      integer neq, ia, ja, ipflag
      integer illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,   &
         mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns
      integer icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, imax, lewtn, lyhd, lyhn
      integer nruserpar, iuserpar, niuserpar
      real y, rwork
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real rlss
      real ruserpar
!jdf  dimension neq(1), y(1), rwork(1), ia(1), ja(1)
      dimension neq(*), y(*), rwork(*), ia(*), ja(*)
      dimension ruserpar(nruserpar), iuserpar(niuserpar)
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,   &
         mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ rlss(6),   &
         iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
!-----------------------------------------------------------------------
! this routine serves as an interface between the driver and
! subroutine prep.  it is called only if miter is 1 or 2.
! tasks performed here are..
!  * call prep,
!  * reset the required wm segment length lenwk,
!  * move yh back to its final location (following wm in rwork),
!  * reset pointers for yh, savf, ewt, and acor, and
!  * move ewt to its new position if istate = 1.
! ipflag is an output error indication flag.  ipflag = 0 if there was
! no trouble, and ipflag is the value of the prep error flag ipper
! if there was trouble in subroutine prep.
!-----------------------------------------------------------------------
      ipflag = 0
! call prep to do matrix preprocessing operations. ---------------------
      call prep_lsodes (neq, y, rwork(lyh), rwork(lsavf), rwork(lewt),   &
         rwork(lacor), ia, ja, rwork(lwm), rwork(lwm), ipflag, f, jac,   &
         ruserpar, nruserpar, iuserpar, niuserpar )
      lenwk = max0(lreq,lwmin)
      if (ipflag .lt. 0) return
! if prep was successful, move yh to end of required space for wm. -----
      lyhn = lwm + lenwk
      if (lyhn .gt. lyh) return
      lyhd = lyh - lyhn
      if (lyhd .eq. 0) go to 20
      imax = lyhn - 1 + lenyhm
      do 10 i = lyhn,imax
 10     rwork(i) = rwork(i+lyhd)
      lyh = lyhn
! reset pointers for savf, ewt, and acor. ------------------------------
 20   lsavf = lyh + lenyh
      lewtn = lsavf + n
      lacor = lewtn + n
      if (istatc .eq. 3) go to 40
! if istate = 1, move ewt (left) to its new position. ------------------
      if (lewtn .gt. lewt) return
      do 30 i = 1,n
 30     rwork(i+lewtn-1) = rwork(i+lewt-1)
 40   lewt = lewtn
      return
!----------------------- end of subroutine iprep -----------------------
      end subroutine iprep                                        
      subroutine jgroup (n,ia,ja,maxg,ngrp,igp,jgp,incl,jdone,ier)
!lll. optimize
      integer n, ia, ja, maxg, ngrp, igp, jgp, incl, jdone, ier
!jdf  dimension ia(1), ja(1), igp(1), jgp(n), incl(n), jdone(n)
      dimension ia(*), ja(*), igp(*), jgp(n), incl(n), jdone(n)
!-----------------------------------------------------------------------
! this subroutine constructs groupings of the column indices of
! the jacobian matrix, used in the numerical evaluation of the
! jacobian by finite differences.
!
! input..
! n      = the order of the matrix.
! ia,ja  = sparse structure descriptors of the matrix by rows.
! maxg   = length of available storate in the igp array.
!
! output..
! ngrp   = number of groups.
! jgp    = array of length n containing the column indices by groups.
! igp    = pointer array of length ngrp + 1 to the locations in jgp
!          of the beginning of each group.
! ier    = error indicator.  ier = 0 if no error occurred, or 1 if
!          maxg was insufficient.
!
! incl and jdone are working arrays of length n.
!-----------------------------------------------------------------------
      integer i, j, k, kmin, kmax, ncol, ng
!
      ier = 0
      do 10 j = 1,n
 10     jdone(j) = 0
      ncol = 1
      do 60 ng = 1,maxg
        igp(ng) = ncol
        do 20 i = 1,n
 20       incl(i) = 0
        do 50 j = 1,n
! reject column j if it is already in a group.--------------------------
          if (jdone(j) .eq. 1) go to 50
          kmin = ia(j)
          kmax = ia(j+1) - 1
          do 30 k = kmin,kmax
! reject column j if it overlaps any column already in this group.------
            i = ja(k)
            if (incl(i) .eq. 1) go to 50
 30         continue
! accept column j into group ng.----------------------------------------
          jgp(ncol) = j
          ncol = ncol + 1
          jdone(j) = 1
          do 40 k = kmin,kmax
            i = ja(k)
 40         incl(i) = 1
 50       continue
! stop if this group is empty (grouping is complete).-------------------
        if (ncol .eq. igp(ng)) go to 70
 60     continue
! error return if not all columns were chosen (maxg too small).---------
      if (ncol .le. n) go to 80
      ng = maxg
 70   ngrp = ng - 1
      return
 80   ier = 1
      return
!----------------------- end of subroutine jgroup ----------------------
      end subroutine jgroup                                           
      subroutine md   &
           (n, ia,ja, max, v,l, head,last,next, mark, flag)
!lll. optimize
!***********************************************************************
!  md -- minimum degree algorithm (based on element model)
!***********************************************************************
!
!  description
!
!    md finds a minimum degree ordering of the rows and columns of a
!    general sparse matrix m stored in (ia,ja,a) format.
!    when the structure of m is nonsymmetric, the ordering is that
!    obtained for the symmetric matrix  m + m-transpose.
!
!
!  additional parameters
!
!    max  - declared dimension of the one-dimensional arrays v and l.
!           max must be at least  n+2k,  where k is the number of
!           nonzeroes in the strict upper triangle of m + m-transpose
!
!    v    - integer one-dimensional work array.  dimension = max
!
!    l    - integer one-dimensional work array.  dimension = max
!
!    head - integer one-dimensional work array.  dimension = n
!
!    last - integer one-dimensional array used to return the permutation
!           of the rows and columns of m corresponding to the minimum
!           degree ordering.  dimension = n
!
!    next - integer one-dimensional array used to return the inverse of
!           the permutation returned in last.  dimension = n
!
!    mark - integer one-dimensional work array (may be the same as v).
!           dimension = n
!
!    flag - integer error flag.  values and their meanings are -
!             0     no errors detected
!             9n+k  insufficient storage in md
!
!
!  definitions of internal parameters
!
!    ---------+---------------------------------------------------------
!    v(s)     - value field of list entry
!    ---------+---------------------------------------------------------
!    l(s)     - link field of list entry  (0 =) end of list)
!    ---------+---------------------------------------------------------
!    l(vi)    - pointer to element list of uneliminated vertex vi
!    ---------+---------------------------------------------------------
!    l(ej)    - pointer to boundary list of active element ej
!    ---------+---------------------------------------------------------
!    head(d)  - vj =) vj head of d-list d
!             -  0 =) no vertex in d-list d
!
!
!             -                  vi uneliminated vertex
!             -          vi in ek           -       vi not in ek
!    ---------+-----------------------------+---------------------------
!    next(vi) - undefined but nonnegative   - vj =) vj next in d-list
!             -                             -  0 =) vi tail of d-list
!    ---------+-----------------------------+---------------------------
!    last(vi) - (not set until mdp)         - -d =) vi head of d-list d
!             --vk =) compute degree        - vj =) vj last in d-list
!             - ej =) vi prototype of ej    -  0 =) vi not in any d-list
!             -  0 =) do not compute degree -
!    ---------+-----------------------------+---------------------------
!    mark(vi) - mark(vk)                    - nonneg. tag .lt. mark(vk)
!
!
!             -                   vi eliminated vertex
!             -      ei active element      -           otherwise
!    ---------+-----------------------------+---------------------------
!    next(vi) - -j =) vi was j-th vertex    - -j =) vi was j-th vertex
!             -       to be eliminated      -       to be eliminated
!    ---------+-----------------------------+---------------------------
!    last(vi) -  m =) size of ei = m        - undefined
!    ---------+-----------------------------+---------------------------
!    mark(vi) - -m =) overlap count of ei   - undefined
!             -       with ek = m           -
!             - otherwise nonnegative tag   -
!             -       .lt. mark(vk)         -
!
!-----------------------------------------------------------------------
!
!jdf  integer  ia(1), ja(1),  v(1), l(1),  head(1), last(1), next(1),
!jdf *   mark(1),  flag,  tag, dmin, vk,ek, tail
      integer  ia(*), ja(*),  v(*), l(*),  head(*), last(*), next(*),   &
         mark(*),  flag,  tag, dmin, vk,ek, tail
      equivalence  (vk,ek)
!
!----initialization
      tag = 0
      call  mdi   &
         (n, ia,ja, max,v,l, head,last,next, mark,tag, flag)
      if (flag.ne.0)  return
!
      k = 0
      dmin = 1
!
!----while  k .lt. n  do
   1  if (k.ge.n)  go to 4
!
!------search for vertex of minimum degree
   2    if (head(dmin).gt.0)  go to 3
          dmin = dmin + 1
          go to 2
!
!------remove vertex vk of minimum degree from degree list
   3    vk = head(dmin)
        head(dmin) = next(vk)
        if (head(dmin).gt.0)  last(head(dmin)) = -dmin
!
!------number vertex vk, adjust tag, and tag vk
        k = k+1
        next(vk) = -k
        last(ek) = dmin - 1
        tag = tag + last(ek)
        mark(vk) = tag
!
!------form element ek from uneliminated neighbors of vk
        call  mdm   &
           (vk,tail, v,l, last,next, mark)
!
!------purge inactive elements and do mass elimination
        call  mdp   &
           (k,ek,tail, v,l, head,last,next, mark)
!
!------update degrees of uneliminated vertices in ek
        call  mdu   &
           (ek,dmin, v,l, head,last,next, mark)
!
        go to 1
!
!----generate inverse permutation from permutation
   4  do 5 k=1,n
        next(k) = -next(k)
   5    last(next(k)) = k
!
      return
      end subroutine md
      subroutine mdi   &
           (n, ia,ja, max,v,l, head,last,next, mark,tag, flag)
!lll. optimize
!***********************************************************************
!  mdi -- initialization
!***********************************************************************
!jdf  integer  ia(1), ja(1),  v(1), l(1),  head(1), last(1), next(1),
!jdf *   mark(1), tag,  flag,  sfs, vi,dvi, vj
      integer  ia(*), ja(*),  v(*), l(*),  head(*), last(*), next(*),   &
         mark(*), tag,  flag,  sfs, vi,dvi, vj
!
!----initialize degrees, element lists, and degree lists
      do 1 vi=1,n
        mark(vi) = 1
        l(vi) = 0
   1    head(vi) = 0
      sfs = n+1
!
!----create nonzero structure
!----for each nonzero entry a(vi,vj)
      do 6 vi=1,n
        jmin = ia(vi)
        jmax = ia(vi+1) - 1
        if (jmin.gt.jmax)  go to 6
        do 5 j=jmin,jmax
          vj = ja(j)
          if (vj-vi) 2, 5, 4
!
!------if a(vi,vj) is in strict lower triangle
!------check for previous occurrence of a(vj,vi)
   2      lvk = vi
          kmax = mark(vi) - 1
          if (kmax .eq. 0) go to 4
          do 3 k=1,kmax
            lvk = l(lvk)
            if (v(lvk).eq.vj) go to 5
   3        continue
!----for unentered entries a(vi,vj)
   4        if (sfs.ge.max)  go to 101
!
!------enter vj in element list for vi
            mark(vi) = mark(vi) + 1
            v(sfs) = vj
            l(sfs) = l(vi)
            l(vi) = sfs
            sfs = sfs+1
!
!------enter vi in element list for vj
            mark(vj) = mark(vj) + 1
            v(sfs) = vi
            l(sfs) = l(vj)
            l(vj) = sfs
            sfs = sfs+1
   5      continue
   6    continue
!
!----create degree lists and initialize mark vector
      do 7 vi=1,n
        dvi = mark(vi)
        next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        nextvi = next(vi)
        if (nextvi.gt.0)  last(nextvi) = vi
   7    mark(vi) = tag
!
      return
!
! ** error-  insufficient storage
 101  flag = 9*n + vi
      return
      end subroutine mdi
      subroutine mdm   &
           (vk,tail, v,l, last,next, mark)
!lll. optimize
!***********************************************************************
!  mdm -- form element from uneliminated neighbors of vk
!***********************************************************************
!jdf  integer  vk, tail,  v(1), l(1),   last(1), next(1),   mark(1),
!jdf *   tag, s,ls,vs,es, b,lb,vb, blp,blpmax
      integer  vk, tail,  v(*), l(*),   last(*), next(*),   mark(*),   &
         tag, s,ls,vs,es, b,lb,vb, blp,blpmax
      equivalence  (vs, es)
!
!----initialize tag and list of uneliminated neighbors
      tag = mark(vk)
      tail = vk
!
!----for each vertex/element vs/es in element list of vk
      ls = l(vk)
   1  s = ls
      if (s.eq.0)  go to 5
        ls = l(s)
        vs = v(s)
        if (next(vs).lt.0)  go to 2
!
!------if vs is uneliminated vertex, then tag and append to list of
!------uneliminated neighbors
          mark(vs) = tag
          l(tail) = s
          tail = s
          go to 4
!
!------if es is active element, then ...
!--------for each vertex vb in boundary list of element es
   2      lb = l(es)
          blpmax = last(es)
          do 3 blp=1,blpmax
            b = lb
            lb = l(b)
            vb = v(b)
!
!----------if vb is untagged vertex, then tag and append to list of
!----------uneliminated neighbors
            if (mark(vb).ge.tag)  go to 3
              mark(vb) = tag
              l(tail) = b
              tail = b
   3        continue
!
!--------mark es inactive
          mark(es) = tag
!
   4    go to 1
!
!----terminate list of uneliminated neighbors
   5  l(tail) = 0
!
      return
      end subroutine mdm
      subroutine mdp   &
           (k,ek,tail, v,l, head,last,next, mark)
!lll. optimize
!***********************************************************************
!  mdp -- purge inactive elements and do mass elimination
!***********************************************************************
!jdf  integer  ek, tail,  v(1), l(1),  head(1), last(1), next(1),
!jdf *   mark(1),  tag, free, li,vi,lvi,evi, s,ls,es, ilp,ilpmax
      integer  ek, tail,  v(*), l(*),  head(*), last(*), next(*),   &
         mark(*),  tag, free, li,vi,lvi,evi, s,ls,es, ilp,ilpmax
!
!----initialize tag
      tag = mark(ek)
!
!----for each vertex vi in ek
      li = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 12
      do 11 ilp=1,ilpmax
        i = li
        li = l(i)
        vi = v(li)
!
!------remove vi from degree list
        if (last(vi).eq.0)  go to 3
          if (last(vi).gt.0)  go to 1
            head(-last(vi)) = next(vi)
            go to 2
   1        next(last(vi)) = next(vi)
   2      if (next(vi).gt.0)  last(next(vi)) = last(vi)
!
!------remove inactive items from element list of vi
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
!
!------if vi is interior vertex, then remove from list and eliminate
   6    lvi = l(vi)
        if (lvi.ne.0)  go to 7
          l(i) = l(li)
          li = i
!
          k = k+1
          next(vi) = -k
          last(ek) = last(ek) - 1
          go to 11
!
!------else ...
!--------classify vertex vi
   7      if (l(lvi).ne.0)  go to 9
            evi = v(lvi)
            if (next(evi).ge.0)  go to 9
              if (mark(evi).lt.0)  go to 8
!
!----------if vi is prototype vertex, then mark as such, initialize
!----------overlap count for corresponding element, and move vi to end
!----------of boundary list
                last(vi) = evi
                mark(evi) = -1
                l(tail) = li
                tail = li
                l(i) = l(li)
                li = i
                go to 10
!
!----------else if vi is duplicate vertex, then mark as such and adjust
!----------overlap count for corresponding element
   8            last(vi) = 0
                mark(evi) = mark(evi) - 1
                go to 10
!
!----------else mark vi to compute degree
   9            last(vi) = -ek
!
!--------insert ek in element list of vi
  10      v(free) = ek
          l(free) = l(vi)
          l(vi) = free
  11    continue
!
!----terminate boundary list
  12  l(tail) = 0
!
      return
      end subroutine mdp
      subroutine mdu   &
           (ek,dmin, v,l, head,last,next, mark)
!lll. optimize
!***********************************************************************
!  mdu -- update degrees of uneliminated vertices in ek
!***********************************************************************
!jdf  integer  ek, dmin,  v(1), l(1),  head(1), last(1), next(1),
!jdf *   mark(1),  tag, vi,evi,dvi, s,vs,es, b,vb, ilp,ilpmax,
!jdf *   blp,blpmax
      integer  ek, dmin,  v(*), l(*),  head(*), last(*), next(*),   &
         mark(*),  tag, vi,evi,dvi, s,vs,es, b,vb, ilp,ilpmax,   &
         blp,blpmax
      equivalence  (vs, es)
!
!----initialize tag
      tag = mark(ek) - last(ek)
!
!----for each vertex vi in ek
      i = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 11
      do 10 ilp=1,ilpmax
        i = l(i)
        vi = v(i)
        if (last(vi))  1, 10, 8
!
!------if vi neither prototype nor duplicate vertex, then merge elements
!------to compute degree
   1      tag = tag + 1
          dvi = last(ek)
!
!--------for each vertex/element vs/es in element list of vi
          s = l(vi)
   2      s = l(s)
          if (s.eq.0)  go to 9
            vs = v(s)
            if (next(vs).lt.0)  go to 3
!
!----------if vs is uneliminated vertex, then tag and adjust degree
              mark(vs) = tag
              dvi = dvi + 1
              go to 5
!
!----------if es is active element, then expand
!------------check for outmatched vertex
   3          if (mark(es).lt.0)  go to 6
!
!------------for each vertex vb in es
              b = es
              blpmax = last(es)
              do 4 blp=1,blpmax
                b = l(b)
                vb = v(b)
!
!--------------if vb is untagged, then tag and adjust degree
                if (mark(vb).ge.tag)  go to 4
                  mark(vb) = tag
                  dvi = dvi + 1
   4            continue
!
   5        go to 2
!
!------else if vi is outmatched vertex, then adjust overlaps but do not
!------compute degree
   6      last(vi) = 0
          mark(es) = mark(es) - 1
   7      s = l(s)
          if (s.eq.0)  go to 10
            es = v(s)
            if (mark(es).lt.0)  mark(es) = mark(es) - 1
            go to 7
!
!------else if vi is prototype vertex, then calculate degree by
!------inclusion/exclusion and reset overlap count
   8      evi = last(vi)
          dvi = last(ek) + last(evi) + mark(evi)
          mark(evi) = 0
!
!------insert vi in appropriate degree list
   9    next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        if (next(vi).gt.0)  last(next(vi)) = vi
        if (dvi.lt.dmin)  dmin = dvi
!
  10    continue
!
  11  return
      end subroutine mdu
      subroutine nnfc   &
           (n, r,c,ic, ia,ja,a, z, b,   &
            lmax,il,jl,ijl,l, d, umax,iu,ju,iju,u,   &
            row, tmp, irl,jrl, flag)
!lll. optimize
!*** subroutine nnfc
!*** numerical ldu-factorization of sparse nonsymmetric matrix and
!      solution of system of linear equations (compressed pointer
!      storage)
!
!
!       input variables..  n, r, c, ic, ia, ja, a, b,
!                          il, jl, ijl, lmax, iu, ju, iju, umax
!       output variables.. z, l, d, u, flag
!
!       parameters used internally..
! nia   - irl,  - vectors used to find the rows of  l.  at the kth step
! nia   - jrl       of the factorization,  jrl(k)  points to the head
!       -           of a linked list in  jrl  of column indices j
!       -           such j .lt. k and  l(k,j)  is nonzero.  zero
!       -           indicates the end of the list.  irl(j)  (j.lt.k)
!       -           points to the smallest i such that i .ge. k and
!       -           l(i,j)  is nonzero.
!       -           size of each = n.
! fia   - row   - holds intermediate values in calculation of  u and l.
!       -           size = n.
! fia   - tmp   - holds new right-hand side  b*  for solution of the
!       -           equation ux = b*.
!       -           size = n.
!
!  internal variables..
!    jmin, jmax - indices of the first and last positions in a row to
!      be examined.
!    sum - used in calculating  tmp.
!
      integer rk,umax
!jdf  integer  r(1), c(1), ic(1), ia(1), ja(1), il(1), jl(1), ijl(1)
!jdf  integer  iu(1), ju(1), iju(1), irl(1), jrl(1), flag
!jdf  real  a(1), l(1), d(1), u(1), z(1), b(1), row(1)
!jdf  real tmp(1), lki, sum, dk
      integer  r(*), c(*), ic(*), ia(*), ja(*), il(*), jl(*), ijl(*)
      integer  iu(*), ju(*), iju(*), irl(*), jrl(*), flag
      real  a(*), l(*), d(*), u(*), z(*), b(*), row(*)
      real tmp(*), lki, sum, dk
!     double precision  a(1), l(1), d(1), u(1), z(1), b(1), row(1)
!     double precision  tmp(1), lki, sum, dk
!
!  ******  initialize pointers and test storage  ***********************
      if(il(n+1)-1 .gt. lmax) go to 104
      if(iu(n+1)-1 .gt. umax) go to 107
      do 1 k=1,n
        irl(k) = il(k)
        jrl(k) = 0
   1    continue
!
!  ******  for each row  ***********************************************
      do 19 k=1,n
!  ******  reverse jrl and zero row where kth row of l will fill in  ***
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
!  ******  set row to zero where u will fill in  ***********************
   3    jmin = iju(k)
        jmax = jmin + iu(k+1) - iu(k) - 1
        if (jmin .gt. jmax) go to 5
        do 4 j=jmin,jmax
   4      row(ju(j)) = 0
!  ******  place kth row of a in row  **********************************
   5    rk = r(k)
        jmin = ia(rk)
        jmax = ia(rk+1) - 1
        do 6 j=jmin,jmax
          row(ic(ja(j))) = a(j)
   6      continue
!  ******  initialize sum, and link through jrl  ***********************
        sum = b(rk)
        i = i1
        if (i .eq. 0) go to 10
!  ******  assign the kth row of l and adjust row, sum  ****************
   7      lki = -row(i)
!  ******  if l is not required, then comment out the following line  **
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
!
!  ******  assign kth row of u and diagonal d, set tmp(k)  *************
  10    if (row(k) .eq. 0.0e0) go to 108
        dk = 1.0e0 / row(k)
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
!
!  ******  update irl and jrl, keeping jrl in decreasing order  ********
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
!
!  ******  solve  ux = tmp  by back substitution  **********************
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
!
! ** error.. insufficient storage for l
 104  flag = 4*n + 1
      return
! ** error.. insufficient storage for u
 107  flag = 7*n + 1
      return
! ** error.. zero pivot
 108  flag = 8*n + k
      return
      end subroutine nnfc
      subroutine nnsc   &
           (n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, z, b, tmp)
!lll. optimize
!*** subroutine nnsc
!*** numerical solution of sparse nonsymmetric system of linear
!      equations given ldu-factorization (compressed pointer storage)
!
!
!       input variables..  n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, b
!       output variables.. z
!
!       parameters used internally..
! fia   - tmp   - temporary vector which gets result of solving  ly = b.
!       -           size = n.
!
!  internal variables..
!    jmin, jmax - indices of the first and last positions in a row of
!      u or l  to be used.
!
!jdf  integer r(1), c(1), il(1), jl(1), ijl(1), iu(1), ju(1), iju(1)
!jdf  real l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk, sum
      integer r(*), c(*), il(*), jl(*), ijl(*), iu(*), ju(*), iju(*)
      real l(*), d(*), u(*), b(*), z(*), tmp(*), tmpk, sum
!     double precision  l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk,sum
!
!  ******  set tmp to reordered b  *************************************
      do 1 k=1,n
   1    tmp(k) = b(r(k))
!  ******  solve  ly = b  by forward substitution  *********************
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
!  ******  solve  ux = y  by back substitution  ************************
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
      end subroutine nnsc
      subroutine nntc   &
           (n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, z, b, tmp)
!lll. optimize
!*** subroutine nntc
!*** numeric solution of the transpose of a sparse nonsymmetric system
!      of linear equations given lu-factorization (compressed pointer
!      storage)
!
!
!       input variables..  n, r, c, il, jl, ijl, l, d, iu, ju, iju, u, b
!       output variables.. z
!
!       parameters used internally..
! fia   - tmp   - temporary vector which gets result of solving ut y = b
!       -           size = n.
!
!  internal variables..
!    jmin, jmax - indices of the first and last positions in a row of
!      u or l  to be used.
!
!jdf  integer r(1), c(1), il(1), jl(1), ijl(1), iu(1), ju(1), iju(1)
!jdf  real l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk,sum
      integer r(*), c(*), il(*), jl(*), ijl(*), iu(*), ju(*), iju(*)
      real l(*), d(*), u(*), b(*), z(*), tmp(*), tmpk,sum
!     double precision l(1), d(1), u(1), b(1), z(1), tmp(1), tmpk,sum
!
!  ******  set tmp to reordered b  *************************************
      do 1 k=1,n
   1    tmp(k) = b(c(k))
!  ******  solve  ut y = b  by forward substitution  *******************
      do 3 k=1,n
        jmin = iu(k)
        jmax = iu(k+1) - 1
        tmpk = -tmp(k)
        if (jmin .gt. jmax) go to 3
        mu = iju(k) - jmin
        do 2 j=jmin,jmax
   2      tmp(ju(mu+j)) = tmp(ju(mu+j)) + tmpk * u(j)
   3    continue
!  ******  solve  lt x = y  by back substitution  **********************
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
      end subroutine nntc
      subroutine nroc (n, ic, ia, ja, a, jar, ar, p, flag)
!lll. optimize
!
!       ----------------------------------------------------------------
!
!               yale sparse matrix package - nonsymmetric codes
!                    solving the system of equations mx = b
!
!    i.   calling sequences
!         the coefficient matrix can be processed by an ordering routine
!    (e.g., to reduce fillin or ensure numerical stability) before using
!    the remaining subroutines.  if no reordering is done, then set
!    r(i) = c(i) = ic(i) = i  for i=1,...,n.  if an ordering subroutine
!    is used, then nroc should be used to reorder the coefficient matrix
!    the calling sequence is --
!        (       (matrix ordering))
!        (nroc   (matrix reordering))
!         nsfc   (symbolic factorization to determine where fillin will
!                  occur during numeric factorization)
!         nnfc   (numeric factorization into product ldu of unit lower
!                  triangular matrix l, diagonal matrix d, and unit
!                  upper triangular matrix u, and solution of linear
!                  system)
!         nnsc   (solution of linear system for additional right-hand
!                  side using ldu factorization from nnfc)
!    (if only one system of equations is to be solved, then the
!    subroutine trk should be used.)
!
!    ii.  storage of sparse matrices
!         the nonzero entries of the coefficient matrix m are stored
!    row-by-row in the array a.  to identify the individual nonzero
!    entries in each row, we need to know in which column each entry
!    lies.  the column indices which correspond to the nonzero entries
!    of m are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
!    ja(k) = j.  in addition, we need to know where each row starts and
!    how long it is.  the index positions in ja and a where the rows of
!    m begin are stored in the array ia.  i.e., if m(i,j) is the first
!    (leftmost) entry in the i-th row and  a(k) = m(i,j),  then
!    ia(i) = k.  moreover, the index in ja and a of the first location
!    following the last element in the last row is stored in ia(n+1).
!    thus, the number of entries in the i-th row is given by
!    ia(i+1) - ia(i),  the nonzero entries of the i-th row are stored
!    consecutively in
!            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
!    and the corresponding column indices are stored consecutively in
!            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
!    for example, the 5 by 5 matrix
!                ( 1. 0. 2. 0. 0.)
!                ( 0. 3. 0. 0. 0.)
!            m = ( 0. 4. 5. 6. 0.)
!                ( 0. 0. 0. 7. 0.)
!                ( 0. 0. 0. 8. 9.)
!    would be stored as
!               - 1  2  3  4  5  6  7  8  9
!            ---+--------------------------
!            ia - 1  3  4  7  8 10
!            ja - 1  3  2  2  3  4  4  4  5
!             a - 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
!
!         the strict upper (lower) triangular portion of the matrix
!    u (l) is stored in a similar fashion using the arrays  iu, ju, u
!    (il, jl, l)  except that an additional array iju (ijl) is used to
!    compress storage of ju (jl) by allowing some sequences of column
!    (row) indices to used for more than one row (column)  (n.b., l is
!    stored by columns).  iju(k) (ijl(k)) points to the starting
!    location in ju (jl) of entries for the kth row (column).
!    compression in ju (jl) occurs in two ways.  first, if a row
!    (column) i was merged into the current row (column) k, and the
!    number of elements merged in from (the tail portion of) row
!    (column) i is the same as the final length of row (column) k, then
!    the kth row (column) and the tail of row (column) i are identical
!    and iju(k) (ijl(k)) points to the start of the tail.  second, if
!    some tail portion of the (k-1)st row (column) is identical to the
!    head of the kth row (column), then iju(k) (ijl(k)) points to the
!    start of that tail portion.  for example, the nonzero structure of
!    the strict upper triangular part of the matrix
!            d 0 x x x
!            0 d 0 x x
!            0 0 d x 0
!            0 0 0 d x
!            0 0 0 0 d
!    would be represented as
!                - 1 2 3 4 5 6
!            ----+------------
!             iu - 1 4 6 7 8 8
!             ju - 3 4 5 4
!            iju - 1 2 4 3           .
!    the diagonal entries of l and u are assumed to be equal to one and
!    are not stored.  the array d contains the reciprocals of the
!    diagonal entries of the matrix d.
!
!    iii. additional storage savings
!         in nsfc, r and ic can be the same array in the calling
!    sequence if no reordering of the coefficient matrix has been done.
!         in nnfc, r, c, and ic can all be the same array if no
!    reordering has been done.  if only the rows have been reordered,
!    then c and ic can be the same array.  if the row and column
!    orderings are the same, then r and c can be the same array.  z and
!    row can be the same array.
!         in nnsc or nntc, r and c can be the same array if no
!    reordering has been done or if the row and column orderings are the
!    same.  z and b can be the same array.  however, then b will be
!    destroyed.
!
!    iv.  parameters
!         following is a list of parameters to the programs.  names are
!    uniform among the various subroutines.  class abbreviations are --
!       n - integer variable
!       f - real variable
!       v - supplies a value to a subroutine
!       r - returns a result from a subroutine
!       i - used internally by a subroutine
!       a - array
!
! class - parameter
! ------+----------
! fva   - a     - nonzero entries of the coefficient matrix m, stored
!       -           by rows.
!       -           size = number of nonzero entries in m.
! fva   - b     - right-hand side b.
!       -           size = n.
! nva   - c     - ordering of the columns of m.
!       -           size = n.
! fvra  - d     - reciprocals of the diagonal entries of the matrix d.
!       -           size = n.
! nr    - flag  - error flag.  values and their meanings are --
!       -            0     no errors detected
!       -            n+k   null row in a  --  row = k
!       -           2n+k   duplicate entry in a  --  row = k
!       -           3n+k   insufficient storage for jl  --  row = k
!       -           4n+1   insufficient storage for l
!       -           5n+k   null pivot  --  row = k
!       -           6n+k   insufficient storage for ju  --  row = k
!       -           7n+1   insufficient storage for u
!       -           8n+k   zero pivot  --  row = k
! nva   - ia    - pointers to delimit the rows of a.
!       -           size = n+1.
! nvra  - ijl   - pointers to the first element in each column in jl,
!       -           used to compress storage in jl.
!       -           size = n.
! nvra  - iju   - pointers to the first element in each row in ju, used
!       -           to compress storage in ju.
!       -           size = n.
! nvra  - il    - pointers to delimit the columns of l.
!       -           size = n+1.
! nvra  - iu    - pointers to delimit the rows of u.
!       -           size = n+1.
! nva   - ja    - column numbers corresponding to the elements of a.
!       -           size = size of a.
! nvra  - jl    - row numbers corresponding to the elements of l.
!       -           size = jlmax.
! nv    - jlmax - declared dimension of jl.  jlmax must be larger than
!       -           the number of nonzeros in the strict lower triangle
!       -           of m plus fillin minus compression.
! nvra  - ju    - column numbers corresponding to the elements of u.
!       -           size = jumax.
! nv    - jumax - declared dimension of ju.  jumax must be larger than
!       -           the number of nonzeros in the strict upper triangle
!       -           of m plus fillin minus compression.
! fvra  - l     - nonzero entries in the strict lower triangular portion
!       -           of the matrix l, stored by columns.
!       -           size = lmax.
! nv    - lmax  - declared dimension of l.  lmax must be larger than
!       -           the number of nonzeros in the strict lower triangle
!       -           of m plus fillin  (il(n+1)-1 after nsfc).
! nv    - n     - number of variables/equations.
! nva   - r     - ordering of the rows of m.
!       -           size = n.
! fvra  - u     - nonzero entries in the strict upper triangular portion
!       -           of the matrix u, stored by rows.
!       -           size = umax.
! nv    - umax  - declared dimension of u.  umax must be larger than
!       -           the number of nonzeros in the strict upper triangle
!       -           of m plus fillin  (iu(n+1)-1 after nsfc).
! fra   - z     - solution x.
!       -           size = n.
!
!       ----------------------------------------------------------------
!
!*** subroutine nroc
!*** reorders rows of a, leaving row order unchanged
!
!
!       input parameters.. n, ic, ia, ja, a
!       output parameters.. ja, a, flag
!
!       parameters used internally..
! nia   - p     - at the kth step, p is a linked list of the reordered
!       -           column indices of the kth row of a.  p(n+1) points
!       -           to the first entry in the list.
!       -           size = n+1.
! nia   - jar   - at the kth step,jar contains the elements of the
!       -           reordered column indices of a.
!       -           size = n.
! fia   - ar    - at the kth step, ar contains the elements of the
!       -           reordered row of a.
!       -           size = n.
!
!jdf  integer  ic(1), ia(1), ja(1), jar(1), p(1), flag
!jdf  real  a(1), ar(1)
      integer  ic(*), ia(*), ja(*), jar(*), p(*), flag
      real  a(*), ar(*)
!     double precision  a(1), ar(1)
!
!  ******  for each nonempty row  *******************************
      do 5 k=1,n
        jmin = ia(k)
        jmax = ia(k+1) - 1
        if(jmin .gt. jmax) go to 5
        p(n+1) = n + 1
!  ******  insert each element in the list  *********************
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
!  ******  replace old row in ja and a  *************************
        i = n + 1
        do 4 j=jmin,jmax
          i = p(i)
          ja(j) = jar(i)
   4      a(j) = ar(i)
   5    continue
      flag = 0
      return
!
! ** error.. duplicate entry in a
 102  flag = n + k
      return
      end subroutine nroc                                     
      subroutine nsfc   &
            (n, r, ic, ia,ja, jlmax,il,jl,ijl, jumax,iu,ju,iju,   &
             q, ira,jra, irac, irl,jrl, iru,jru, flag)
!lll. optimize
!*** subroutine nsfc
!*** symbolic ldu-factorization of nonsymmetric sparse matrix
!      (compressed pointer storage)
!
!
!       input variables.. n, r, ic, ia, ja, jlmax, jumax.
!       output variables.. il, jl, ijl, iu, ju, iju, flag.
!
!       parameters used internally..
! nia   - q     - suppose  m*  is the result of reordering  m.  if
!       -           processing of the ith row of  m*  (hence the ith
!       -           row of  u) is being done,  q(j)  is initially
!       -           nonzero if  m*(i,j) is nonzero (j.ge.i).  since
!       -           values need not be stored, each entry points to the
!       -           next nonzero and  q(n+1)  points to the first.  n+1
!       -           indicates the end of the list.  for example, if n=9
!       -           and the 5th row of  m*  is
!       -              0 x x 0 x 0 0 x 0
!       -           then  q  will initially be
!       -              a a a a 8 a a 10 5           (a - arbitrary).
!       -           as the algorithm proceeds, other elements of  q
!       -           are inserted in the list because of fillin.
!       -           q  is used in an analogous manner to compute the
!       -           ith column of  l.
!       -           size = n+1.
! nia   - ira,  - vectors used to find the columns of  m.  at the kth
! nia   - jra,      step of the factorization,  irac(k)  points to the
! nia   - irac      head of a linked list in  jra  of row indices i
!       -           such that i .ge. k and  m(i,k)  is nonzero.  zero
!       -           indicates the end of the list.  ira(i)  (i.ge.k)
!       -           points to the smallest j such that j .ge. k and
!       -           m(i,j)  is nonzero.
!       -           size of each = n.
! nia   - irl,  - vectors used to find the rows of  l.  at the kth step
! nia   - jrl       of the factorization,  jrl(k)  points to the head
!       -           of a linked list in  jrl  of column indices j
!       -           such j .lt. k and  l(k,j)  is nonzero.  zero
!       -           indicates the end of the list.  irl(j)  (j.lt.k)
!       -           points to the smallest i such that i .ge. k and
!       -           l(i,j)  is nonzero.
!       -           size of each = n.
! nia   - iru,  - vectors used in a manner analogous to  irl and jrl
! nia   - jru       to find the columns of  u.
!       -           size of each = n.
!
!  internal variables..
!    jlptr - points to the last position used in  jl.
!    juptr - points to the last position used in  ju.
!    jmin,jmax - are the indices in  a or u  of the first and last
!                elements to be examined in a given row.
!                for example,  jmin=ia(k), jmax=ia(k+1)-1.
!
      integer cend, qm, rend, rk, vj
!jdf  integer ia(1), ja(1), ira(1), jra(1), il(1), jl(1), ijl(1)
!jdf  integer iu(1), ju(1), iju(1), irl(1), jrl(1), iru(1), jru(1)
!jdf  integer r(1), ic(1), q(1), irac(1), flag
      integer ia(*), ja(*), ira(*), jra(*), il(*), jl(*), ijl(*)
      integer iu(*), ju(*), iju(*), irl(*), jrl(*), iru(*), jru(*)
      integer r(*), ic(*), q(*), irac(*), flag
!
!  ******  initialize pointers  ****************************************
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
!  ******  initialize column pointers for a  ***************************
      do 2 k=1,n
        rk = r(k)
        iak = ia(rk)
        if (iak .ge. ia(rk+1))  go to 101
        jaiak = ic(ja(iak))
        if (jaiak .gt. k)  go to 105
        jra(k) = irac(jaiak)
        irac(jaiak) = k
   2    ira(k) = iak
!
!  ******  for each column of l and row of u  **************************
      do 41 k=1,n
!
!  ******  initialize q for computing kth column of l  *****************
        q(np1) = np1
        luk = -1
!  ******  by filling in kth column of a  ******************************
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
!  ******  link through jru  *******************************************
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
!  ******  and merge the corresponding columns into the kth column  ****
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
!  ******  lasti is the longest column merged into the kth  ************
!  ******  see if it equals the entire kth column  *********************
  10    qm = q(np1)
        if (qm .ne. k)  go to 105
        if (luk .eq. 0)  go to 17
        if (lastid .ne. luk)  go to 11
!  ******  if so, jl can be compressed  ********************************
        irll = irl(lasti)
        ijl(k) = irll + 1
        if (jl(irll) .ne. k)  ijl(k) = ijl(k) - 1
        go to 17
!  ******  if not, see if kth column can overlap the previous one  *****
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
!  ******  move column indices from q to jl, update vectors  ***********
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
!
!  ******  initialize q for computing kth row of u  ********************
        q(np1) = np1
        luk = -1
!  ******  by filling in kth row of reordered a  ***********************
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
!  ******  link through jrl,  ******************************************
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
!  ******  update irl and jrl, *****************************************
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
!  ******  and merge the corresponding rows into the kth row  **********
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
!  ******  update jrl(k) and irl(k)  ***********************************
  26    if (il(k+1) .le. il(k))  go to 27
          j = jl(irl(k))
          jrl(k) = jrl(j)
          jrl(j) = k
!  ******  lasti is the longest row merged into the kth  ***************
!  ******  see if it equals the entire kth row  ************************
  27    qm = q(np1)
        if (qm .ne. k)  go to 105
        if (luk .eq. 0)  go to 34
        if (lastid .ne. luk)  go to 28
!  ******  if so, ju can be compressed  ********************************
        irul = iru(lasti)
        iju(k) = irul + 1
        if (ju(irul) .ne. k)  iju(k) = iju(k) - 1
        go to 34
!  ******  if not, see if kth row can overlap the previous one  ********
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
!  ******  move row indices from q to ju, update vectors  **************
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
!
!  ******  update iru, jru  ********************************************
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
!
!  ******  update ira, jra, irac  **************************************
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
!
      ijl(n) = jlptr
      iju(n) = juptr
      flag = 0
      return
!
! ** error.. null row in a
 101  flag = n + rk
      return
! ** error.. duplicate entry in a
 102  flag = 2*n + rk
      return
! ** error.. insufficient storage for jl
 103  flag = 3*n + k
      return
! ** error.. null pivot
 105  flag = 5*n + k
      return
! ** error.. insufficient storage for ju
 106  flag = 6*n + k
      return
      end subroutine nsfc
      subroutine odrv   &
           (n, ia,ja,a, p,ip, nsp,isp, path, flag)
!lll. optimize
!                                                                 5/2/83
!***********************************************************************
!  odrv -- driver for sparse matrix reordering routines
!***********************************************************************
!
!  description
!
!    odrv finds a minimum degree ordering of the rows and columns
!    of a matrix m stored in (ia,ja,a) format (see below).  for the
!    reordered matrix, the work and storage required to perform
!    gaussian elimination is (usually) significantly less.
!
!    note.. odrv and its subordinate routines have been modified to
!    compute orderings for general matrices, not necessarily having any
!    symmetry.  the miminum degree ordering is computed for the
!    structure of the symmetric matrix  m + m-transpose.
!    modifications to the original odrv module have been made in
!    the coding in subroutine mdi, and in the initial comments in
!    subroutines odrv and md.
!
!    if only the nonzero entries in the upper triangle of m are being
!    stored, then odrv symmetrically reorders (ia,ja,a), (optionally)
!    with the diagonal entries placed first in each row.  this is to
!    ensure that if m(i,j) will be in the upper triangle of m with
!    respect to the new ordering, then m(i,j) is stored in row i (and
!    thus m(j,i) is not stored),  whereas if m(i,j) will be in the
!    strict lower triangle of m, then m(j,i) is stored in row j (and
!    thus m(i,j) is not stored).
!
!
!  storage of sparse matrices
!
!    the nonzero entries of the matrix m are stored row-by-row in the
!    array a.  to identify the individual nonzero entries in each row,
!    we need to know in which column each entry lies.  these column
!    indices are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
!    ja(k) = j.  to identify the individual rows, we need to know where
!    each row starts.  these row pointers are stored in the array ia.
!    i.e., if m(i,j) is the first nonzero entry (stored) in the i-th row
!    and  a(k) = m(i,j),  then  ia(i) = k.  moreover, ia(n+1) points to
!    the first location following the last element in the last row.
!    thus, the number of entries in the i-th row is  ia(i+1) - ia(i),
!    the nonzero entries in the i-th row are stored consecutively in
!
!            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
!
!    and the corresponding column indices are stored consecutively in
!
!            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
!
!    when the coefficient matrix is symmetric, only the nonzero entries
!    in the upper triangle need be stored.  for example, the matrix
!
!             ( 1  0  2  3  0 )
!             ( 0  4  0  0  0 )
!         m = ( 2  0  5  6  0 )
!             ( 3  0  6  7  8 )
!             ( 0  0  0  8  9 )
!
!    could be stored as
!
!            - 1  2  3  4  5  6  7  8  9 10 11 12 13
!         ---+--------------------------------------
!         ia - 1  4  5  8 12 14
!         ja - 1  3  4  2  1  3  4  1  3  4  5  4  5
!          a - 1  2  3  4  2  5  6  3  6  7  8  8  9
!
!    or (symmetrically) as
!
!            - 1  2  3  4  5  6  7  8  9
!         ---+--------------------------
!         ia - 1  4  5  7  9 10
!         ja - 1  3  4  2  3  4  4  5  5
!          a - 1  2  3  4  5  6  7  8  9          .
!
!
!  parameters
!
!    n    - order of the matrix
!
!    ia   - integer one-dimensional array containing pointers to delimit
!           rows in ja and a.  dimension = n+1
!
!    ja   - integer one-dimensional array containing the column indices
!           corresponding to the elements of a.  dimension = number of
!           nonzero entries in (the upper triangle of) m
!
!    a    - real one-dimensional array containing the nonzero entries in
!           (the upper triangle of) m, stored by rows.  dimension =
!           number of nonzero entries in (the upper triangle of) m
!
!    p    - integer one-dimensional array used to return the permutation
!           of the rows and columns of m corresponding to the minimum
!           degree ordering.  dimension = n
!
!    ip   - integer one-dimensional array used to return the inverse of
!           the permutation returned in p.  dimension = n
!
!    nsp  - declared dimension of the one-dimensional array isp.  nsp
!           must be at least  3n+4k,  where k is the number of nonzeroes
!           in the strict upper triangle of m
!
!    isp  - integer one-dimensional array used for working storage.
!           dimension = nsp
!
!    path - integer path specification.  values and their meanings are -
!             1  find minimum degree ordering only
!             2  find minimum degree ordering and reorder symmetrically
!                  stored matrix (used when only the nonzero entries in
!                  the upper triangle of m are being stored)
!             3  reorder symmetrically stored matrix as specified by
!                  input permutation (used when an ordering has already
!                  been determined and only the nonzero entries in the
!                  upper triangle of m are being stored)
!             4  same as 2 but put diagonal entries at start of each row
!             5  same as 3 but put diagonal entries at start of each row
!
!    flag - integer error flag.  values and their meanings are -
!               0    no errors detected
!              9n+k  insufficient storage in md
!             10n+1  insufficient storage in odrv
!             11n+1  illegal path specification
!
!
!  conversion from real to double precision
!
!    change the real declarations in odrv and sro to double precision
!    declarations.
!
!-----------------------------------------------------------------------
!
!jdf  integer  ia(1), ja(1),  p(1), ip(1),  isp(1),  path,  flag,
!jdf *   v, l, head,  tmp, q
!jdf  real  a(1)
      integer  ia(*), ja(*),  p(*), ip(*),  isp(*),  path,  flag,   &
         v, l, head,  tmp, q
      real  a(*)
!...  double precision  a(1)
      logical  dflag
!
!----initialize error flag and validate path specification
      flag = 0
      if (path.lt.1 .or. 5.lt.path)  go to 111
!
!----allocate storage and find minimum degree ordering
      if ((path-1) * (path-2) * (path-4) .ne. 0)  go to 1
        max = (nsp-n)/2
        v    = 1
        l    = v     +  max
        head = l     +  max
        next = head  +  n
        if (max.lt.n)  go to 110
!
        call  md   &
           (n, ia,ja, max,isp(v),isp(l), isp(head),p,ip, isp(v), flag)
        if (flag.ne.0)  go to 100
!
!----allocate storage and symmetrically reorder matrix
   1  if ((path-2) * (path-3) * (path-4) * (path-5) .ne. 0)  go to 2
        tmp = (nsp+1) -      n
        q   = tmp     - (ia(n+1)-1)
        if (q.lt.1)  go to 110
!
        dflag = path.eq.4 .or. path.eq.5
        call sro   &
           (n,  ip,  ia, ja, a,  isp(tmp),  isp(q),  dflag)
!
   2  return
!
! ** error -- error detected in md
 100  return
! ** error -- insufficient storage
 110  flag = 10*n + 1
      return
! ** error -- illegal path specified
 111  flag = 11*n + 1
      return
      end subroutine odrv



      subroutine prjs (neq,y,yh,nyh,ewt,ftem,savf,wk,iwk,f,jac,   &
                       ruserpar, nruserpar, iuserpar, niuserpar )
!lll. optimize
      external f,jac
      integer neq, nyh, iwk
      integer iownd, iowns,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, imul, j, jj, jok, jmax, jmin, k, kmax, kmin, ng
      integer nruserpar, iuserpar, niuserpar
      real y, yh, ewt, ftem, savf, wk
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real con0, conmin, ccmxj, psmall, rbig, seth
!rce  real con, di, fac, hl0, pij, r, r0, rcon, rcont,   &
!rce     srur, vnorm
      real con, di, fac, hl0, pij, r, r0, rcon, rcont,   &
         srur
      real ruserpar
!jdf  dimension neq(1), y(1), yh(nyh,1), ewt(1), ftem(1), savf(1),
!jdf 1   wk(1), iwk(1)
      dimension neq(*), y(*), yh(nyh,*), ewt(*), ftem(*), savf(*),   &
         wk(*), iwk(*)
      dimension ruserpar(nruserpar), iuserpar(niuserpar)
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         iownd(14), iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,   &
         iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
!-----------------------------------------------------------------------
! prjs is called to compute and process the matrix
! p = i - h*el(1)*j , where j is an approximation to the jacobian.
! j is computed by columns, either by the user-supplied routine jac
! if miter = 1, or by finite differencing if miter = 2.
! if miter = 3, a diagonal approximation to j is used.
! if miter = 1 or 2, and if the existing value of the jacobian
! (as contained in p) is considered acceptable, then a new value of
! p is reconstructed from the old value.  in any case, when miter
! is 1 or 2, the p matrix is subjected to lu decomposition in cdrv.
! p and its lu decomposition are stored (separately) in wk.
!
! in addition to variables described previously, communication
! with prjs uses the following..
! y     = array containing predicted values on entry.
! ftem  = work array of length n (acor in stode).
! savf  = array containing f evaluated at predicted y.
! wk    = real work space for matrices.  on output it contains the
!         inverse diagonal matrix if miter = 3, and p and its sparse
!         lu decomposition if miter is 1 or 2.
!         storage of matrix elements starts at wk(3).
!         wk also contains the following matrix-related data..
!         wk(1) = sqrt(uround), used in numerical jacobian increments.
!         wk(2) = h*el0, saved for later use if miter = 3.
! iwk   = integer work space for matrix-related data, assumed to
!         be equivalenced to wk.  in addition, wk(iprsp) and iwk(ipisp)
!         are assumed to have identical locations.
! el0   = el(1) (input).
! ierpj = output error flag (in common).
!       = 0 if no error.
!       = 1  if zero pivot found in cdrv.
!       = 2  if a singular matrix arose with miter = 3.
!       = -1 if insufficient storage for cdrv (should not occur here).
!       = -2 if other error found in cdrv (should not occur here).
! jcur  = output flag = 1 to indicate that the jacobian matrix
!         (or approximation) is now current.
! this routine also uses other variables in common.
!-----------------------------------------------------------------------
      hl0 = h*el0
      con = -hl0
      if (miter .eq. 3) go to 300
! see whether j should be reevaluated (jok = 0) or not (jok = 1). ------
      jok = 1
      if (nst .eq. 0 .or. nst .ge. nslj+msbj) jok = 0
      if (icf .eq. 1 .and. abs(rc - 1.0e0) .lt. ccmxj) jok = 0
      if (icf .eq. 2) jok = 0
      if (jok .eq. 1) go to 250
!
! miter = 1 or 2, and the jacobian is to be reevaluated. ---------------
 20   jcur = 1
      nje = nje + 1
      nslj = nst
      iplost = 0
      conmin = abs(con)
      go to (100, 200), miter
!
! if miter = 1, call jac, multiply by scalar, and add identity. --------
 100  continue
      kmin = iwk(ipian)
      do 130 j = 1, n
        kmax = iwk(ipian+j) - 1
        do 110 i = 1,n
 110      ftem(i) = 0.0e0
        call jac (neq, tn, y, j, iwk(ipian), iwk(ipjan), ftem,   &
            ruserpar, nruserpar, iuserpar, niuserpar)
        do 120 k = kmin, kmax
          i = iwk(ibjan+k)
          wk(iba+k) = ftem(i)*con
          if (i .eq. j) wk(iba+k) = wk(iba+k) + 1.0e0
 120      continue
        kmin = kmax + 1
 130    continue
      go to 290
!
! if miter = 2, make ngp calls to f to approximate j and p. ------------
 200  continue
      fac = vnorm(n, savf, ewt)
      r0 = 1000.0e0 * abs(h) * uround * float(n) * fac
      if (r0 .eq. 0.0e0) r0 = 1.0e0
      srur = wk(1)
      jmin = iwk(ipigp)
      do 240 ng = 1,ngp
        jmax = iwk(ipigp+ng) - 1
        do 210 j = jmin,jmax
          jj = iwk(ibjgp+j)
          r = amax1(srur*abs(y(jj)),r0/ewt(jj))
 210      y(jj) = y(jj) + r
        call f (neq, tn, y, ftem,   &
            ruserpar, nruserpar, iuserpar, niuserpar)
        do 230 j = jmin,jmax
          jj = iwk(ibjgp+j)
          y(jj) = yh(jj,1)
          r = amax1(srur*abs(y(jj)),r0/ewt(jj))
          fac = -hl0/r
          kmin =iwk(ibian+jj)
          kmax =iwk(ibian+jj+1) - 1
          do 220 k = kmin,kmax
            i = iwk(ibjan+k)
            wk(iba+k) = (ftem(i) - savf(i))*fac
            if (i .eq. jj) wk(iba+k) = wk(iba+k) + 1.0e0
 220        continue
 230      continue
        jmin = jmax + 1
 240    continue
      nfe = nfe + ngp
      go to 290
!
! if jok = 1, reconstruct new p from old p. ----------------------------
 250  jcur = 0
      rcon = con/con0
      rcont = abs(con)/conmin
      if (rcont .gt. rbig .and. iplost .eq. 1) go to 20
      kmin = iwk(ipian)
      do 275 j = 1,n
        kmax = iwk(ipian+j) - 1
        do 270 k = kmin,kmax
          i = iwk(ibjan+k)
          pij = wk(iba+k)
          if (i .ne. j) go to 260
          pij = pij - 1.0e0
          if (abs(pij) .ge. psmall) go to 260
            iplost = 1
            conmin = amin1(abs(con0),conmin)
 260      pij = pij*rcon
          if (i .eq. j) pij = pij + 1.0e0
          wk(iba+k) = pij
 270      continue
        kmin = kmax + 1
 275    continue
!
! do numerical factorization of p matrix. ------------------------------
 290  nlu = nlu + 1
      con0 = con
      ierpj = 0
      do 295 i = 1,n
 295    ftem(i) = 0.0e0
      call cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),   &
         wk(ipa),ftem,ftem,nsp,iwk(ipisp),wk(iprsp),iesp,2,iys)
      if (iys .eq. 0) return
      imul = (iys - 1)/n
      ierpj = -2
      if (imul .eq. 8) ierpj = 1
      if (imul .eq. 10) ierpj = -1
      return
!
! if miter = 3, construct a diagonal approximation to j and p. ---------
 300  continue
      jcur = 1
      nje = nje + 1
      wk(2) = hl0
      ierpj = 0
      r = el0*0.1e0
      do 310 i = 1,n
 310    y(i) = y(i) + r*(h*savf(i) - yh(i,2))
      call f (neq, tn, y, wk(3),   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      nfe = nfe + 1
      do 320 i = 1,n
        r0 = h*savf(i) - yh(i,2)
        di = 0.1e0*r0 - h*(wk(i+2) - savf(i))
        wk(i+2) = 1.0e0
        if (abs(r0) .lt. uround/ewt(i)) go to 320
        if (abs(di) .eq. 0.0e0) go to 330
        wk(i+2) = 0.1e0*r0/di
 320    continue
      return
 330  ierpj = 2
      return
!----------------------- end of subroutine prjs ------------------------
      end subroutine prjs                                          
      subroutine slss (wk, iwk, x, tem)
!lll. optimize
      integer iwk
      integer iownd, iowns,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i
      real wk, x, tem
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real rlss
      real di, hl0, phl0, r
!jdf  dimension wk(1), iwk(1), x(1), tem(1)
      dimension wk(*), iwk(*), x(*), tem(*)

      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         iownd(14), iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ rlss(6),   &
         iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
!-----------------------------------------------------------------------
! this routine manages the solution of the linear system arising from
! a chord iteration.  it is called if miter .ne. 0.
! if miter is 1 or 2, it calls cdrv to accomplish this.
! if miter = 3 it updates the coefficient h*el0 in the diagonal
! matrix, and then computes the solution.
! communication with slss uses the following variables..
! wk    = real work space containing the inverse diagonal matrix if
!         miter = 3 and the lu decomposition of the matrix otherwise.
!         storage of matrix elements starts at wk(3).
!         wk also contains the following matrix-related data..
!         wk(1) = sqrt(uround) (not used here),
!         wk(2) = hl0, the previous value of h*el0, used if miter = 3.
! iwk   = integer work space for matrix-related data, assumed to
!         be equivalenced to wk.  in addition, wk(iprsp) and iwk(ipisp)
!         are assumed to have identical locations.
! x     = the right-hand side vector on input, and the solution vector
!         on output, of length n.
! tem   = vector of work space of length n, not used in this version.
! iersl = output flag (in common).
!         iersl = 0  if no trouble occurred.
!         iersl = -1 if cdrv returned an error flag (miter = 1 or 2).
!                    this should never occur and is considered fatal.
!         iersl = 1  if a singular matrix arose with miter = 3.
! this routine also uses other variables in common.
!-----------------------------------------------------------------------
      iersl = 0
      go to (100, 100, 300), miter
 100  call cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),   &
         wk(ipa),x,x,nsp,iwk(ipisp),wk(iprsp),iesp,4,iersl)
      if (iersl .ne. 0) iersl = -1
      return
!
 300  phl0 = wk(2)
      hl0 = h*el0
      wk(2) = hl0
      if (hl0 .eq. phl0) go to 330
      r = hl0/phl0
      do 320 i = 1,n
        di = 1.0e0 - r*(1.0e0 - 1.0e0/wk(i+2))
        if (abs(di) .eq. 0.0e0) go to 390
 320    wk(i+2) = 1.0e0/di
 330  do 340 i = 1,n
 340    x(i) = wk(i+2)*x(i)
      return
 390  iersl = 1
      return
!
!----------------------- end of subroutine slss ------------------------
      end subroutine slss                  
      subroutine sro   &
           (n, ip, ia,ja,a, q, r, dflag)
!lll. optimize
!***********************************************************************
!  sro -- symmetric reordering of sparse symmetric matrix
!***********************************************************************
!
!  description
!
!    the nonzero entries of the matrix m are assumed to be stored
!    symmetrically in (ia,ja,a) format (i.e., not both m(i,j) and m(j,i)
!    are stored if i ne j).
!
!    sro does not rearrange the order of the rows, but does move
!    nonzeroes from one row to another to ensure that if m(i,j) will be
!    in the upper triangle of m with respect to the new ordering, then
!    m(i,j) is stored in row i (and thus m(j,i) is not stored),  whereas
!    if m(i,j) will be in the strict lower triangle of m, then m(j,i) is
!    stored in row j (and thus m(i,j) is not stored).
!
!
!  additional parameters
!
!    q     - integer one-dimensional work array.  dimension = n
!
!    r     - integer one-dimensional work array.  dimension = number of
!            nonzero entries in the upper triangle of m
!
!    dflag - logical variable.  if dflag = .true., then store nonzero
!            diagonal elements at the beginning of the row
!
!-----------------------------------------------------------------------
!
!jdf  integer  ip(1),  ia(1), ja(1),  q(1), r(1)
!jdf  real  a(1),  ak
      integer  ip(*),  ia(*), ja(*),  q(*), r(*)
      real  a(*),  ak
!...  double precision  a(1),  ak
      logical  dflag
!
!
!--phase 1 -- find row in which to store each nonzero
!----initialize count of nonzeroes to be stored in each row
      do 1 i=1,n
  1     q(i) = 0
!
!----for each nonzero element a(j)
      do 3 i=1,n
        jmin = ia(i)
        jmax = ia(i+1) - 1
        if (jmin.gt.jmax)  go to 3
        do 2 j=jmin,jmax
!
!--------find row (=r(j)) and column (=ja(j)) in which to store a(j) ...
          k = ja(j)
          if (ip(k).lt.ip(i))  ja(j) = i
          if (ip(k).ge.ip(i))  k = i
          r(j) = k
!
!--------... and increment count of nonzeroes (=q(r(j)) in that row
  2       q(k) = q(k) + 1
  3     continue
!
!
!--phase 2 -- find new ia and permutation to apply to (ja,a)
!----determine pointers to delimit rows in permuted (ja,a)
      do 4 i=1,n
        ia(i+1) = ia(i) + q(i)
  4     q(i) = ia(i+1)
!
!----determine where each (ja(j),a(j)) is stored in permuted (ja,a)
!----for each nonzero element (in reverse order)
      ilast = 0
      jmin = ia(1)
      jmax = ia(n+1) - 1
      j = jmax
      do 6 jdummy=jmin,jmax
        i = r(j)
        if (.not.dflag .or. ja(j).ne.i .or. i.eq.ilast)  go to 5
!
!------if dflag, then put diagonal nonzero at beginning of row
          r(j) = ia(i)
          ilast = i
          go to 6
!
!------put (off-diagonal) nonzero in last unused location in row
  5       q(i) = q(i) - 1
          r(j) = q(i)
!
  6     j = j-1
!
!
!--phase 3 -- permute (ja,a) to upper triangular form (wrt new ordering)
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
!
      return
      end subroutine sro



      real function vnorm (n, v, w)
!lll. optimize
!-----------------------------------------------------------------------
! this function routine computes the weighted root-mean-square norm
! of the vector of length n contained in the array v, with weights
! contained in the array w of length n..
!   vnorm = sqrt( (1/n) * sum( v(i)*w(i) )**2 )
!-----------------------------------------------------------------------
      integer n,   i
      real v, w,   sum
      dimension v(n), w(n)
      integer iok_vnorm
      common / lsodes_cmn_iok_vnorm / iok_vnorm
      sum = 0.0e0
      do 10 i = 1,n
        if (abs(v(i)*w(i)) .ge. 1.0e18) then
            vnorm = 1.0e18
            iok_vnorm = -1
            return
        end if
 10     sum = sum + (v(i)*w(i))**2
      vnorm = sqrt(sum/float(n))
      return
!----------------------- end of function vnorm -------------------------
      end function vnorm          
      subroutine xerrwv (msg, nmes, nerr, level, ni, i1, i2, nr, r1, r2)
      use module_peg_util, only:  peg_message, peg_error_fatal
!     integer msg, nmes, nerr, level, ni, i1, i2, nr,   &
      integer      nmes, nerr, level, ni, i1, i2, nr,   &
         i, lun, lunit, mesflg, ncpw, nch, nwds
      real r1, r2
      character(*) msg
      character*80 errmsg
!-----------------------------------------------------------------------
! subroutines xerrwv, xsetf, and xsetun, as given here, constitute
! a simplified version of the slatec error handling package.
! written by a. c. hindmarsh at llnl.  version of march 30, 1987.
!
! all arguments are input arguments.
!
! msg    = the message (hollerith literal or integer array).
! nmes   = the length of msg (number of characters).
! nerr   = the error number (not used).
! level  = the error level..
!          0 or 1 means recoverable (control returns to caller).
!          2 means fatal (run is aborted--see note below).
! ni     = number of integers (0, 1, or 2) to be printed with message.
! i1,i2  = integers to be printed, depending on ni.
! nr     = number of reals (0, 1, or 2) to be printed with message.
! r1,r2  = reals to be printed, depending on nr.
!
! note..  this routine is machine-dependent and specialized for use
! in limited context, in the following ways..
! 1. the number of hollerith characters stored per word, denoted
!    by ncpw below, is a data-loaded constant.
! 2. the value of nmes is assumed to be at most 60.
!    (multi-line messages are generated by repeated calls.)
! 3. if level = 2, control passes to the statement   stop
!    to abort the run.  this statement may be machine-dependent.
! 4. r1 and r2 are assumed to be in single precision and are printed
!    in e21.13 format.
! 5. the common block /eh0001/ below is data-loaded (a machine-
!    dependent feature) with default values.
!    this block is needed for proper retention of parameters used by
!    this routine which the user can reset by calling xsetf or xsetun.
!    the variables in this block are as follows..
!       mesflg = print control flag..
!                1 means print all messages (the default).
!                0 means no printing.
!       lunit  = logical unit number for messages.
!                the default is 6 (machine-dependent).
!-----------------------------------------------------------------------
! the following are instructions for installing this routine
! in different machine environments.
!
! to change the default output unit, change the data statement below.
!
! for some systems, the data statement below must be replaced
! by a separate block data subprogram.
!
! for a different number of characters per word, change the
! data statement setting ncpw below, and format 10.  alternatives for
! various computers are shown in comment cards.
!
! for a different run-abort command, change the statement following
! statement 100 at the end.
!-----------------------------------------------------------------------
      common /eh0001/ mesflg, lunit
!
!raz      data mesflg/1/, lunit/6/
	mesflg = 1
	lunit = 6
!-----------------------------------------------------------------------
! the following data-loaded value of ncpw is valid for the cdc-6600
! and cdc-7600 computers.
!     data ncpw/10/
! the following is valid for the cray-1 computer.
!     data ncpw/8/
! the following is valid for the burroughs 6700 and 7800 computers.
!     data ncpw/6/
! the following is valid for the pdp-10 computer.
!     data ncpw/5/
! the following is valid for the vax computer with 4 bytes per integer,
! and for the ibm-360, ibm-370, ibm-303x, and ibm-43xx computers.
      data ncpw/4/
! the following is valid for the pdp-11, or vax with 2-byte integers.
!     data ncpw/2/
!-----------------------------------------------------------------------
!
      if (mesflg .eq. 0) go to 100
! get logical unit number. ---------------------------------------------
      lun = lunit
! get number of words in message. --------------------------------------
      nch = min0(nmes,60)
      nwds = nch/ncpw
      if (nch .ne. nwds*ncpw) nwds = nwds + 1
! write the message. ---------------------------------------------------
!     write (lun, 10) (msg(i),i=1,nwds)
!     write (lun, 10)  msg
      call peg_message( lun, msg )
!-----------------------------------------------------------------------
! the following format statement is to have the form
! 10  format(1x,mmann)
! where nn = ncpw and mm is the smallest integer .ge. 60/ncpw.
! the following is valid for ncpw = 10.
! 10  format(1x,6a10)
! the following is valid for ncpw = 8.
! 10  format(1x,8a8)
! the following is valid for ncpw = 6.
! 10  format(1x,10a6)
! the following is valid for ncpw = 5.
! 10  format(1x,12a5)
! the following is valid for ncpw = 4.
! 10  format(1x,15a4)
  10  format(1x,a)
! the following is valid for ncpw = 2.
! 10  format(1x,30a2)
!-----------------------------------------------------------------------
      errmsg = ' '
!     if (ni .eq. 1) write (lun, 20) i1
      if (ni .eq. 1) write (errmsg, 20) i1
 20   format(6x,23hin above message,  i1 =,i10)

!     if (ni .eq. 2) write (lun, 30) i1,i2
      if (ni .eq. 2) write (errmsg, 30) i1,i2
 30   format(6x,23hin above message,  i1 =,i10,3x,4hi2 =,i10)

!     if (nr .eq. 1) write (lun, 40) r1
      if (nr .eq. 1) write (errmsg, 40) r1
 40   format(6x,23hin above message,  r1 =,e21.13)

!     if (nr .eq. 2) write (lun, 50) r1,r2
      if (nr .eq. 2) write (errmsg, 50) r1,r2
 50   format(6x,15hin above,  r1 =,e21.13,3x,4hr2 =,e21.13)

      if (errmsg .ne. ' ') call peg_message( lun, errmsg )

! abort the run if level = 2. ------------------------------------------
 100  if (level .ne. 2) return
      call peg_error_fatal( lun, '*** subr xerrwv fatal error' )

!----------------------- end of subroutine xerrwv ----------------------
      end subroutine xerrwv                                                 
!-----------------------------------------------------------------------
      real function r1mach(i)
      use module_peg_util, only:  peg_error_fatal
!
!  single-precision machine constants
!
!  r1mach(1) = b**(emin-1), the smallest positive magnitude.
!
!  r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
!
!  r1mach(3) = b**(-t), the smallest relative spacing.
!
!  r1mach(4) = b**(1-t), the largest relative spacing.
!
!  r1mach(5) = log10(b)
!
!  to alter this function for a particular environment,
!  the desired set of data statements should be activated by
!  removing the c from column 1.
!  on rare machines a static statement may need to be added.
!  (but probably more systems prohibit it than require it.)
!
!  for ieee-arithmetic machines (binary standard), the first
!  set of constants below should be appropriate.
!
!  where possible, decimal, octal or hexadecimal constants are used
!  to specify the constants exactly.  sometimes this requires using
!  equivalent integer arrays.  if your compiler uses half-word
!  integers by default (sometimes called integer*2), you may need to
!  change integer to integer*4 or otherwise instruct your compiler
!  to use full-word integers in the next 5 declarations.
!
      integer mach_small(2)
      integer mach_large(2)
      integer mach_right(2)
      integer mach_diver(2)
      integer mach_log10(2)
      integer sc
!
      character*80 errmsg
!
      real rmach(5)
!
      equivalence (rmach(1), mach_small(1))
      equivalence (rmach(2), mach_large(1))
      equivalence (rmach(3), mach_right(1))
      equivalence (rmach(4), mach_diver(1))
      equivalence (rmach(5), mach_log10(1))
!
!     machine constants for ieee arithmetic machines, such as the at&t
!     3b series, motorola 68000 based machines (e.g. sun 3 and at&t
!     pc 7300), and 8087 based micros (e.g. ibm pc and at&t 6300).
!
!      data small(1) /     8388608 /
!      data large(1) /  2139095039 /
!      data right(1) /   864026624 /
!      data diver(1) /   872415232 /
!      data log10(1) /  1050288283 /, sc/987/

! 18-may-2006 -- 
!   the following values are produced on our current linux 
!	workstations, when the data statments for 
!	'motorola 68000 based machines' are used
!   specifiying them using 'real' data statements should work fine
       data rmach(1) / 1.1754944000E-38 /
       data rmach(2) / 3.4028235000E+38 /
       data rmach(3) / 5.9604645000E-08 /
       data rmach(4) / 1.1920929000E-07 /
       data rmach(5) / 3.0103001000E-01 /
       data sc / 987 /
!
!     machine constants for amdahl machines.
!
!      data small(1) /    1048576 /
!      data large(1) / 2147483647 /
!      data right(1) /  990904320 /
!      data diver(1) / 1007681536 /
!      data log10(1) / 1091781651 /, sc/987/
!
!     machine constants for the burroughs 1700 system.
!
!      data rmach(1) / z400800000 /
!      data rmach(2) / z5ffffffff /
!      data rmach(3) / z4e9800000 /
!      data rmach(4) / z4ea800000 /
!      data rmach(5) / z500e730e8 /, sc/987/
!
!     machine constants for the burroughs 5700/6700/7700 systems.
!
!      data rmach(1) / o1771000000000000 /
!      data rmach(2) / o0777777777777777 /
!      data rmach(3) / o1311000000000000 /
!      data rmach(4) / o1301000000000000 /
!      data rmach(5) / o1157163034761675 /, sc/987/
!
!     machine constants for ftn4 on the cdc 6000/7000 series.
!
!      data rmach(1) / 00564000000000000000b /
!      data rmach(2) / 37767777777777777776b /
!      data rmach(3) / 16414000000000000000b /
!      data rmach(4) / 16424000000000000000b /
!      data rmach(5) / 17164642023241175720b /, sc/987/
!
!     machine constants for ftn5 on the cdc 6000/7000 series.
!
!      data rmach(1) / o"00564000000000000000" /
!      data rmach(2) / o"37767777777777777776" /
!      data rmach(3) / o"16414000000000000000" /
!      data rmach(4) / o"16424000000000000000" /
!      data rmach(5) / o"17164642023241175720" /, sc/987/
!
!     machine constants for convex c-1.
!
!      data rmach(1) / '00800000'x /
!      data rmach(2) / '7fffffff'x /
!      data rmach(3) / '34800000'x /
!      data rmach(4) / '35000000'x /
!      data rmach(5) / '3f9a209b'x /, sc/987/
!
!     machine constants for the cray 1, xmp, 2, and 3.
!
!      data rmach(1) / 200034000000000000000b /
!      data rmach(2) / 577767777777777777776b /
!      data rmach(3) / 377224000000000000000b /
!      data rmach(4) / 377234000000000000000b /
!      data rmach(5) / 377774642023241175720b /, sc/987/
!
!     machine constants for the data general eclipse s/200.
!
!     note - it may be appropriate to include the following line -
!     static rmach(5)
!
!      data small/20k,0/,large/77777k,177777k/
!      data right/35420k,0/,diver/36020k,0/
!      data log10/40423k,42023k/, sc/987/
!
!     machine constants for the harris slash 6 and slash 7.
!
!      data small(1),small(2) / '20000000, '00000201 /
!      data large(1),large(2) / '37777777, '00000177 /
!      data right(1),right(2) / '20000000, '00000352 /
!      data diver(1),diver(2) / '20000000, '00000353 /
!      data log10(1),log10(2) / '23210115, '00000377 /, sc/987/
!
!     machine constants for the honeywell dps 8/70 series.
!
!      data rmach(1) / o402400000000 /
!      data rmach(2) / o376777777777 /
!      data rmach(3) / o714400000000 /
!      data rmach(4) / o716400000000 /
!      data rmach(5) / o776464202324 /, sc/987/
!
!     machine constants for the ibm 360/370 series,
!     the xerox sigma 5/7/9 and the sel systems 85/86.
!
!      data rmach(1) / z00100000 /
!      data rmach(2) / z7fffffff /
!      data rmach(3) / z3b100000 /
!      data rmach(4) / z3c100000 /
!      data rmach(5) / z41134413 /, sc/987/
!
!     machine constants for the interdata 8/32
!     with the unix system fortran 77 compiler.
!
!     for the interdata fortran vii compiler replace
!     the z's specifying hex constants with y's.
!
!      data rmach(1) / z'00100000' /
!      data rmach(2) / z'7effffff' /
!      data rmach(3) / z'3b100000' /
!      data rmach(4) / z'3c100000' /
!      data rmach(5) / z'41134413' /, sc/987/
!
!     machine constants for the pdp-10 (ka or ki processor).
!----------------------------------------------------------------------
! rce 2004-01-07
! The following 5 lines for rmach(1-5) each contained one 
! quotation-mark character.
! The WRF preprocessor did not like this, so I changed the
! quotation-mark characters to QUOTE.
!
!      data rmach(1) / QUOTE000400000000 /
!      data rmach(2) / QUOTE377777777777 /
!      data rmach(3) / QUOTE146400000000 /
!      data rmach(4) / QUOTE147400000000 /
!      data rmach(5) / QUOTE177464202324 /, sc/987/
!----------------------------------------------------------------------
!
!     machine constants for pdp-11 fortrans supporting
!     32-bit integers (expressed in integer and octal).
!
!      data small(1) /    8388608 /
!      data large(1) / 2147483647 /
!      data right(1) /  880803840 /
!      data diver(1) /  889192448 /
!      data log10(1) / 1067065499 /, sc/987/
!
!      data rmach(1) / o00040000000 /
!      data rmach(2) / o17777777777 /
!      data rmach(3) / o06440000000 /
!      data rmach(4) / o06500000000 /
!      data rmach(5) / o07746420233 /, sc/987/
!
!     machine constants for pdp-11 fortrans supporting
!     16-bit integers  (expressed in integer and octal).
!
!      data small(1),small(2) /   128,     0 /
!      data large(1),large(2) / 32767,    -1 /
!      data right(1),right(2) / 13440,     0 /
!      data diver(1),diver(2) / 13568,     0 /
!      data log10(1),log10(2) / 16282,  8347 /, sc/987/
!
!      data small(1),small(2) / o000200, o000000 /
!      data large(1),large(2) / o077777, o177777 /
!      data right(1),right(2) / o032200, o000000 /
!      data diver(1),diver(2) / o032400, o000000 /
!      data log10(1),log10(2) / o037632, o020233 /, sc/987/
!
!     machine constants for the sequent balance 8000.
!
!      data small(1) / $00800000 /
!      data large(1) / $7f7fffff /
!      data right(1) / $33800000 /
!      data diver(1) / $34000000 /
!      data log10(1) / $3e9a209b /, sc/987/
!
!     machine constants for the univac 1100 series.
!
!      data rmach(1) / o000400000000 /
!      data rmach(2) / o377777777777 /
!      data rmach(3) / o146400000000 /
!      data rmach(4) / o147400000000 /
!      data rmach(5) / o177464202324 /, sc/987/
!
!     machine constants for the vax unix f77 compiler.
!
!      data small(1) /       128 /
!      data large(1) /    -32769 /
!      data right(1) /     13440 /
!      data diver(1) /     13568 /
!      data log10(1) / 547045274 /, sc/987/
!
!     machine constants for the vax-11 with
!     fortran iv-plus compiler.
!
!      data rmach(1) / z00000080 /
!      data rmach(2) / zffff7fff /
!      data rmach(3) / z00003480 /
!      data rmach(4) / z00003500 /
!      data rmach(5) / z209b3f9a /, sc/987/
!
!     machine constants for vax/vms version 2.2.
!
!      data rmach(1) /       '80'x /
!      data rmach(2) / 'ffff7fff'x /
!      data rmach(3) /     '3480'x /
!      data rmach(4) /     '3500'x /
!      data rmach(5) / '209b3f9a'x /, sc/987/
!
      real dum


!  ***  issue stop 778 if all data statements are commented...
!     if (sc .ne. 987) stop 778
      if (sc .ne. 987) then
          call peg_error_fatal( -1,   &
          '*** func r1mach fatal error -- all data statements inactive' )
      end if

      if (i .lt. 1  .or.  i .gt. 5) goto 999

      r1mach = rmach(i)

! 18-may-2006 -- 
!   the following compares results from data statements
!   and fortran90 functions
!     write(*,'(/a,i5      )')   &
!         'in module_cbmz_lsodes_solver r1mach - i =', i
!     dum = tiny( 1.0 )
!     write(*,'( a,1pe18.10)') '   rmach(1)    =', rmach(1)
!     write(*,'( a,1pe18.10)') '   tiny(1.0)   =', dum
!     dum = huge( 1.0 )
!     write(*,'( a,1pe18.10)') '   rmach(2)    =', rmach(2)
!     write(*,'( a,1pe18.10)') '   huge(1.0)   =', dum
!     dum = spacing( 0.5 )
!     write(*,'( a,1pe18.10)') '   rmach(3)    =', rmach(3)
!     write(*,'( a,1pe18.10)') '   spacing(0.5)=', dum
!     dum = epsilon( 1.0 )
!     write(*,'( a,1pe18.10)') '   rmach(4)    =', rmach(4)
!     write(*,'( a,1pe18.10)') '   epsilon(1.0)=', dum
!     dum = log10( 2.0 )
!     write(*,'( a,1pe18.10)') '   rmach(5)    =', rmach(5)
!     write(*,'( a,1pe18.10)') '   log10(2.0)  =', dum
!     write(*,*)

! 18-may-2006 -- 
!   the following fortran90 functions give the same results
!   as the 'real' data statements on our linux workstations
!   and could probably be used to replace the data statements
!     if (i .eq. 1) then
!         dum = 1.0
!         r1mach = tiny( dum )
!     else if (i .eq. 2) then
!         dum = 1.0
!         r1mach = huge( dum )
!     else if (i .eq. 3) then
!         dum = 0.5
!         r1mach = spacing( dum )
!     else if (i .eq. 4) then
!         dum = 1.0
!         r1mach = epsilon( dum )
!     else if (i .eq. 5) then
!         dum = 2.0
!         r1mach = log10( dum )
!     end if

      return

! 999 write(*,1999) i
!1999 format(' r1mach - i out of bounds',i10)
  999 write(errmsg,1999) i
 1999 format('*** func r1mach fatal error -- i out of bounds',i10)
      call peg_error_fatal( -1, errmsg )
      end function r1mach   
!
! subroutine xsetf

      subroutine xsetf (mflag)
!
! this routine resets the print control flag mflag.
!
      integer mflag, mesflg, lunit
      common /eh0001/ mesflg, lunit
!
      if (mflag .eq. 0 .or. mflag .eq. 1) mesflg = mflag
      return
!----------------------- end of subroutine xsetf -----------------------
      end subroutine xsetf        


!-----------------------------------------------------------------------
      subroutine set_lsodes_common_vars()
!
! place various constant or initial values into lsodes common blocks
!
      common /eh0001/ mesflg, lunit
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         illin, init, lyh, lewt, lacor, lsavf, lwm, liwm,   &
         mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu

! lsodes parameters
      illin = 0
      ntrep = 0
      mesflg = 1
      lunit = 6

      return
!--------------- end of subroutine set_lsodes_common_vars ---------------
      end subroutine set_lsodes_common_vars


      end module module_cbmz_lsodes_solver


!----------------------------------------------------------------------
! Subr stode and prep must be outside of the module definition.
! When lsodes calls stode, the rwork array (in lsodes) is passed to
!    both the wm and iwm arrays (in stode).  This is treated as a 
!    severe error if stode is within the module.
! The same problem arises when iprep calls prep.
! These two routines were renamed to stode_lsodes and prep_lsodes 
!    to reduce the chance of name conflicts.
!
      subroutine stode_lsodes (neq, y, yh, nyh, yh1, ewt, savf, acor,   &
         wm, iwm, f, jac, pjac, slvs,   &
         ruserpar, nruserpar, iuserpar, niuserpar )
      use module_cbmz_lsodes_solver, only:  cfode, prjs, slss, r1mach, vnorm
!lll. optimize
      external f, jac, pjac, slvs
      integer neq, nyh, iwm
      integer iownd, ialth, ipup, lmax, meo, nqnyh, nslp,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, i1, iredo, iret, j, jb, m, ncf, newq
      integer nruserpar, iuserpar, niuserpar
      real y, yh, yh1, ewt, savf, acor, wm
      real conit, crate, el, elco, hold, rmax, tesco,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
!rce  real dcon, ddn, del, delp, dsm, dup, exdn, exsm, exup,   &
!rce     r, rh, rhdn, rhsm, rhup, told, vnorm
      real dcon, ddn, del, delp, dsm, dup, exdn, exsm, exup,   &
         r, rh, rhdn, rhsm, rhup, told
      real ruserpar
!jdf  dimension neq(1), y(1), yh(nyh,1), yh1(1), ewt(1), savf(1),
!jdf 1   acor(1), wm(1), iwm(1)
      dimension neq(*), y(*), yh(nyh,*), yh1(*), ewt(*), savf(*),   &
         acor(*), wm(*), iwm(*)
      dimension ruserpar(nruserpar), iuserpar(niuserpar)
      common /ls0001/ conit, crate, el(13), elco(13,12),   &
         hold, rmax, tesco(3,12),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround, iownd(14),   &
         ialth, ipup, lmax, meo, nqnyh, nslp,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!-----------------------------------------------------------------------
! stode performs one step of the integration of an initial value
! problem for a system of ordinary differential equations.
! note.. stode is independent of the value of the iteration method
! indicator miter, when this is .ne. 0, and hence is independent
! of the type of chord method used, or the jacobian structure.
! communication with stode is done with the following variables..
!
! neq    = integer array containing problem size in neq(1), and
!          passed as the neq argument in all calls to f and jac.
! y      = an array of length .ge. n used as the y argument in
!          all calls to f and jac.
! yh     = an nyh by lmax array containing the dependent variables
!          and their approximate scaled derivatives, where
!          lmax = maxord + 1.  yh(i,j+1) contains the approximate
!          j-th derivative of y(i), scaled by h**j/factorial(j)
!          (j = 0,1,...,nq).  on entry for the first step, the first
!          two columns of yh must be set from the initial values.
! nyh    = a constant integer .ge. n, the first dimension of yh.
! yh1    = a one-dimensional array occupying the same space as yh.
! ewt    = an array of length n containing multiplicative weights
!          for local error measurements.  local errors in y(i) are
!          compared to 1.0/ewt(i) in various error tests.
! savf   = an array of working storage, of length n.
!          also used for input of yh(*,maxord+2) when jstart = -1
!          and maxord .lt. the current order nq.
! acor   = a work array of length n, used for the accumulated
!          corrections.  on a successful return, acor(i) contains
!          the estimated one-step local error in y(i).
! wm,iwm = real and integer work arrays associated with matrix
!          operations in chord iteration (miter .ne. 0).
! pjac   = name of routine to evaluate and preprocess jacobian matrix
!          and p = i - h*el0*jac, if a chord method is being used.
! slvs   = name of routine to solve linear system in chord iteration.
! ccmax  = maximum relative change in h*el0 before pjac is called.
! h      = the step size to be attempted on the next step.
!          h is altered by the error control algorithm during the
!          problem.  h can be either positive or negative, but its
!          sign must remain constant throughout the problem.
! hmin   = the minimum absolute value of the step size h to be used.
! hmxi   = inverse of the maximum absolute value of h to be used.
!          hmxi = 0.0 is allowed and corresponds to an infinite hmax.
!          hmin and hmxi may be changed at any time, but will not
!          take effect until the next change of h is considered.
! tn     = the independent variable. tn is updated on each step taken.
! jstart = an integer used for input only, with the following
!          values and meanings..
!               0  perform the first step.
!           .gt.0  take a new step continuing from the last.
!              -1  take the next step with a new value of h, maxord,
!                    n, meth, miter, and/or matrix parameters.
!              -2  take the next step with a new value of h,
!                    but with other inputs unchanged.
!          on return, jstart is set to 1 to facilitate continuation.
! kflag  = a completion code with the following meanings..
!               0  the step was succesful.
!              -1  the requested error could not be achieved.
!              -2  corrector convergence could not be achieved.
!              -3  fatal error in pjac or slvs.
!          a return with kflag = -1 or -2 means either
!          abs(h) = hmin or 10 consecutive failures occurred.
!          on a return with kflag negative, the values of tn and
!          the yh array are as of the beginning of the last
!          step, and h is the last step size attempted.
! maxord = the maximum order of integration method to be allowed.
! maxcor = the maximum number of corrector iterations allowed.
! msbp   = maximum number of steps between pjac calls (miter .gt. 0).
! mxncf  = maximum number of convergence failures allowed.
! meth/miter = the method flags.  see description in driver.
! n      = the number of first-order differential equations.
!-----------------------------------------------------------------------
      kflag = 0
      told = tn
      ncf = 0
      ierpj = 0
      iersl = 0
      jcur = 0
      icf = 0
      delp = 0.0e0
      if (jstart .gt. 0) go to 200
      if (jstart .eq. -1) go to 100
      if (jstart .eq. -2) go to 160
!-----------------------------------------------------------------------
! on the first call, the order is set to 1, and other variables are
! initialized.  rmax is the maximum ratio by which h can be increased
! in a single step.  it is initially 1.e4 to compensate for the small
! initial h, but then is normally equal to 10.  if a failure
! occurs (in corrector convergence or error test), rmax is set at 2
! for the next increase.
!-----------------------------------------------------------------------
      lmax = maxord + 1
      nq = 1
      l = 2
      ialth = 2
      rmax = 10000.0e0
      rc = 0.0e0
      el0 = 1.0e0
      crate = 0.7e0
      hold = h
      meo = meth
      nslp = 0
      ipup = miter
      iret = 3
      go to 140
!-----------------------------------------------------------------------
! the following block handles preliminaries needed when jstart = -1.
! ipup is set to miter to force a matrix update.
! if an order increase is about to be considered (ialth = 1),
! ialth is reset to 2 to postpone consideration one more step.
! if the caller has changed meth, cfode is called to reset
! the coefficients of the method.
! if the caller has changed maxord to a value less than the current
! order nq, nq is reduced to maxord, and a new h chosen accordingly.
! if h is to be changed, yh must be rescaled.
! if h or meth is being changed, ialth is reset to l = nq + 1
! to prevent further changes in h for that many steps.
!-----------------------------------------------------------------------
 100  ipup = miter
      lmax = maxord + 1
      if (ialth .eq. 1) ialth = 2
      if (meth .eq. meo) go to 110
      call cfode (meth, elco, tesco)
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
      conit = 0.5e0/float(nq+2)
      ddn = vnorm (n, savf, ewt)/tesco(1,l)
      exdn = 1.0e0/float(l)
      rhdn = 1.0e0/(1.3e0*ddn**exdn + 0.0000013e0)
      rh = amin1(rhdn,1.0e0)
      iredo = 3
      if (h .eq. hold) go to 170
      rh = amin1(rh,abs(h/hold))
      h = hold
      go to 175
!-----------------------------------------------------------------------
! cfode is called to get all the integration coefficients for the
! current meth.  then the el vector and related constants are reset
! whenever the order nq is changed, or at the start of the problem.
!-----------------------------------------------------------------------
 140  call cfode (meth, elco, tesco)
 150  do 155 i = 1,l
 155    el(i) = elco(i,nq)
      nqnyh = nq*nyh
      rc = rc*el(1)/el0
      el0 = el(1)
      conit = 0.5e0/float(nq+2)
      go to (160, 170, 200), iret
!-----------------------------------------------------------------------
! if h is being changed, the h ratio rh is checked against
! rmax, hmin, and hmxi, and the yh array rescaled.  ialth is set to
! l = nq + 1 to prevent a change of h for that many steps, unless
! forced by a convergence or error test failure.
!-----------------------------------------------------------------------
 160  if (h .eq. hold) go to 200
      rh = h/hold
      h = hold
      iredo = 3
      go to 175
 170  rh = amax1(rh,hmin/abs(h))
 175  rh = amin1(rh,rmax)
      rh = rh/amax1(1.0e0,abs(h)*hmxi*rh)
      r = 1.0e0
      do 180 j = 2,l
        r = r*rh
        do 180 i = 1,n
 180      yh(i,j) = yh(i,j)*r
      h = h*rh
      rc = rc*rh
      ialth = l
      if (iredo .eq. 0) go to 690
!-----------------------------------------------------------------------
! this section computes the predicted values by effectively
! multiplying the yh array by the pascal triangle matrix.
! rc is the ratio of new to old values of the coefficient  h*el(1).
! when rc differs from 1 by more than ccmax, ipup is set to miter
! to force pjac to be called, if a jacobian is involved.
! in any case, pjac is called at least every msbp steps.
!-----------------------------------------------------------------------
 200  if (abs(rc-1.0e0) .gt. ccmax) ipup = miter
      if (nst .ge. nslp+msbp) ipup = miter
      tn = tn + h
      i1 = nqnyh + 1
      do 215 jb = 1,nq
        i1 = i1 - nyh
!dir$ ivdep
        do 210 i = i1,nqnyh
 210      yh1(i) = yh1(i) + yh1(i+nyh)
 215    continue
!-----------------------------------------------------------------------
! up to maxcor corrector iterations are taken.  a convergence test is
! made on the r.m.s. norm of each correction, weighted by the error
! weight vector ewt.  the sum of the corrections is accumulated in the
! vector acor(i).  the yh array is not altered in the corrector loop.
!-----------------------------------------------------------------------
 220  m = 0
      do 230 i = 1,n
 230    y(i) = yh(i,1)
      call f (neq, tn, y, savf,   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      nfe = nfe + 1
      if (ipup .le. 0) go to 250
!-----------------------------------------------------------------------
! if indicated, the matrix p = i - h*el(1)*j is reevaluated and
! preprocessed before starting the corrector iteration.  ipup is set
! to 0 as an indicator that this has been done.
!-----------------------------------------------------------------------
      call pjac (neq, y, yh, nyh, ewt, acor, savf, wm, iwm, f, jac,   &
                 ruserpar, nruserpar, iuserpar, niuserpar )
      ipup = 0
      rc = 1.0e0
      nslp = nst
      crate = 0.7e0
      if (ierpj .ne. 0) go to 430
 250  do 260 i = 1,n
 260    acor(i) = 0.0e0
 270  if (miter .ne. 0) go to 350
!-----------------------------------------------------------------------
! in the case of functional iteration, update y directly from
! the result of the last function evaluation.
!-----------------------------------------------------------------------
      do 290 i = 1,n
        savf(i) = h*savf(i) - yh(i,2)
 290    y(i) = savf(i) - acor(i)
      del = vnorm (n, y, ewt)
      do 300 i = 1,n
        y(i) = yh(i,1) + el(1)*savf(i)
 300    acor(i) = savf(i)
      go to 400
!-----------------------------------------------------------------------
! in the case of the chord method, compute the corrector error,
! and solve the linear system with that as right-hand side and
! p as coefficient matrix.
!-----------------------------------------------------------------------
 350  do 360 i = 1,n
 360    y(i) = h*savf(i) - (yh(i,2) + acor(i))
      call slvs (wm, iwm, y, savf)
      if (iersl .lt. 0) go to 430
      if (iersl .gt. 0) go to 410
      del = vnorm (n, y, ewt)
      do 380 i = 1,n
        acor(i) = acor(i) + y(i)
 380    y(i) = yh(i,1) + el(1)*acor(i)
!-----------------------------------------------------------------------
! test for convergence.  if m.gt.0, an estimate of the convergence
! rate constant is stored in crate, and this is used in the test.
!-----------------------------------------------------------------------
 400  if (m .ne. 0) crate = amax1(0.2e0*crate,del/delp)
      dcon = del*amin1(1.0e0,1.5e0*crate)/(tesco(2,nq)*conit)
      if (dcon .le. 1.0e0) go to 450
      m = m + 1
      if (m .eq. maxcor) go to 410
      if (m .ge. 2 .and. del .gt. 2.0e0*delp) go to 410
      delp = del
      call f (neq, tn, y, savf,   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      nfe = nfe + 1
      go to 270
!-----------------------------------------------------------------------
! the corrector iteration failed to converge.
! if miter .ne. 0 and the jacobian is out of date, pjac is called for
! the next try.  otherwise the yh array is retracted to its values
! before prediction, and h is reduced, if possible.  if h cannot be
! reduced or mxncf failures have occurred, exit with kflag = -2.
!-----------------------------------------------------------------------
 410  if (miter .eq. 0 .or. jcur .eq. 1) go to 430
      icf = 1
      ipup = miter
      go to 220
 430  icf = 2
      ncf = ncf + 1
      rmax = 2.0e0
      tn = told
      i1 = nqnyh + 1
      do 445 jb = 1,nq
        i1 = i1 - nyh
!dir$ ivdep
        do 440 i = i1,nqnyh
 440      yh1(i) = yh1(i) - yh1(i+nyh)
 445    continue
      if (ierpj .lt. 0 .or. iersl .lt. 0) go to 680
      if (abs(h) .le. hmin*1.00001e0) go to 670
      if (ncf .eq. mxncf) go to 670
      rh = 0.25e0
      ipup = miter
      iredo = 1
      go to 170
!-----------------------------------------------------------------------
! the corrector has converged.  jcur is set to 0
! to signal that the jacobian involved may need updating later.
! the local error test is made and control passes to statement 500
! if it fails.
!-----------------------------------------------------------------------
 450  jcur = 0
      if (m .eq. 0) dsm = del/tesco(2,nq)
      if (m .gt. 0) dsm = vnorm (n, acor, ewt)/tesco(2,nq)
      if (dsm .gt. 1.0e0) go to 500
!-----------------------------------------------------------------------
! after a successful step, update the yh array.
! consider changing h if ialth = 1.  otherwise decrease ialth by 1.
! if ialth is then 1 and nq .lt. maxord, then acor is saved for
! use in a possible order increase on the next step.
! if a change in h is considered, an increase or decrease in order
! by one is considered also.  a change in h is made only if it is by a
! factor of at least 1.1.  if not, ialth is set to 3 to prevent
! testing for that many steps.
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! the error test failed.  kflag keeps track of multiple failures.
! restore tn and the yh array to their previous values, and prepare
! to try the step again.  compute the optimum step size for this or
! one lower order.  after 2 or more failures, h is forced to decrease
! by a factor of 0.2 or less.
!-----------------------------------------------------------------------
 500  kflag = kflag - 1
      tn = told
      i1 = nqnyh + 1
      do 515 jb = 1,nq
        i1 = i1 - nyh
!dir$ ivdep
        do 510 i = i1,nqnyh
 510      yh1(i) = yh1(i) - yh1(i+nyh)
 515    continue
      rmax = 2.0e0
      if (abs(h) .le. hmin*1.00001e0) go to 660
      if (kflag .le. -3) go to 640
      iredo = 2
      rhup = 0.0e0
      go to 540
!-----------------------------------------------------------------------
! regardless of the success or failure of the step, factors
! rhdn, rhsm, and rhup are computed, by which h could be multiplied
! at order nq - 1, order nq, or order nq + 1, respectively.
! in the case of failure, rhup = 0.0 to avoid an order increase.
! the largest of these is determined and the new order chosen
! accordingly.  if the order is to be increased, we compute one
! additional scaled derivative.
!-----------------------------------------------------------------------
 520  rhup = 0.0e0
      if (l .eq. lmax) go to 540
      do 530 i = 1,n
 530    savf(i) = acor(i) - yh(i,lmax)
      dup = vnorm (n, savf, ewt)/tesco(3,nq)
      exup = 1.0e0/float(l+1)
      rhup = 1.0e0/(1.4e0*dup**exup + 0.0000014e0)
 540  exsm = 1.0e0/float(l)
      rhsm = 1.0e0/(1.2e0*dsm**exsm + 0.0000012e0)
      rhdn = 0.0e0
      if (nq .eq. 1) go to 560
      ddn = vnorm (n, yh(1,l), ewt)/tesco(1,nq)
      exdn = 1.0e0/float(nq)
      rhdn = 1.0e0/(1.3e0*ddn**exdn + 0.0000013e0)
 560  if (rhsm .ge. rhup) go to 570
      if (rhup .gt. rhdn) go to 590
      go to 580
 570  if (rhsm .lt. rhdn) go to 580
      newq = nq
      rh = rhsm
      go to 620
 580  newq = nq - 1
      rh = rhdn
      if (kflag .lt. 0 .and. rh .gt. 1.0e0) rh = 1.0e0
      go to 620
 590  newq = l
      rh = rhup
      if (rh .lt. 1.1e0) go to 610
      r = el(l)/float(l)
      do 600 i = 1,n
 600    yh(i,newq+1) = acor(i)*r
      go to 630
 610  ialth = 3
      go to 700
 620  if ((kflag .eq. 0) .and. (rh .lt. 1.1e0)) go to 610
      if (kflag .le. -2) rh = amin1(rh,0.2e0)
!-----------------------------------------------------------------------
! if there is a change of order, reset nq, l, and the coefficients.
! in any case h is reset according to rh and the yh array is rescaled.
! then exit from 690 if the step was ok, or redo the step otherwise.
!-----------------------------------------------------------------------
      if (newq .eq. nq) go to 170
 630  nq = newq
      l = nq + 1
      iret = 2
      go to 150
!-----------------------------------------------------------------------
! control reaches this section if 3 or more failures have occured.
! if 10 failures have occurred, exit with kflag = -1.
! it is assumed that the derivatives that have accumulated in the
! yh array have errors of the wrong order.  hence the first
! derivative is recomputed, and the order is set to 1.  then
! h is reduced by a factor of 10, and the step is retried,
! until it succeeds or h reaches hmin.
!-----------------------------------------------------------------------
 640  if (kflag .eq. -10) go to 660
      rh = 0.1e0
      rh = amax1(hmin/abs(h),rh)
      h = h*rh
      do 645 i = 1,n
 645    y(i) = yh(i,1)
      call f (neq, tn, y, savf,   &
          ruserpar, nruserpar, iuserpar, niuserpar)
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
!-----------------------------------------------------------------------
! all returns are made through this section.  h is saved in hold
! to allow the caller to change h on the next step.
!-----------------------------------------------------------------------
 660  kflag = -1
      go to 720
 670  kflag = -2
      go to 720
 680  kflag = -3
      go to 720
 690  rmax = 10.0e0
 700  r = 1.0e0/tesco(2,nqu)
      do 710 i = 1,n
 710    acor(i) = acor(i)*r
 720  hold = h
      jstart = 1
      return
!----------------------- end of subroutine stode_lsodes -----------------------
      end subroutine stode_lsodes 



      subroutine prep_lsodes (neq, y, yh, savf, ewt, ftem, ia, ja,   &
                           wk, iwk, ipper, f, jac,   &
                           ruserpar, nruserpar, iuserpar, niuserpar )
      use module_cbmz_lsodes_solver, only:  adjlr, cdrv, cntnzu, jgroup,   &
                                       odrv
!lll. optimize
      external f,jac
      integer neq, ia, ja, iwk, ipper
      integer iownd, iowns,   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
      integer i, ibr, ier, ipil, ipiu, iptt1, iptt2, j, jfound, k,   &
         knew, kmax, kmin, ldif, lenigp, liwk, maxg, np1, nzsut
      integer nruserpar, iuserpar, niuserpar
      real y, yh, savf, ewt, ftem, wk
      real rowns,   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real con0, conmin, ccmxj, psmall, rbig, seth
      real dq, dyj, erwt, fac, yj
      real ruserpar
!jdf  dimension neq(1), y(1), yh(1), savf(1), ewt(1), ftem(1),
!jdf 1   ia(1), ja(1), wk(1), iwk(1)
      dimension neq(*), y(*), yh(*), savf(*), ewt(*), ftem(*),   &
         ia(*), ja(*), wk(*), iwk(*)
      dimension ruserpar(nruserpar), iuserpar(niuserpar)
      common /ls0001/ rowns(209),   &
         ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,   &
         iownd(14), iowns(6),   &
         icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,   &
         maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lss001/ con0, conmin, ccmxj, psmall, rbig, seth,   &
         iplost, iesp, istatc, iys, iba, ibian, ibjan, ibjgp,   &
         ipian, ipjan, ipjgp, ipigp, ipr, ipc, ipic, ipisp, iprsp, ipa,   &
         lenyh, lenyhm, lenwk, lreq, lrat, lrest, lwmin, moss, msbj,   &
         nslj, ngp, nlu, nnz, nsp, nzl, nzu
!-----------------------------------------------------------------------
! this routine performs preprocessing related to the sparse linear
! systems that must be solved if miter = 1 or 2.
! the operations that are performed here are..
!  * compute sparseness structure of jacobian according to moss,
!  * compute grouping of column indices (miter = 2),
!  * compute a new ordering of rows and columns of the matrix,
!  * reorder ja corresponding to the new ordering,
!  * perform a symbolic lu factorization of the matrix, and
!  * set pointers for segments of the iwk/wk array.
! in addition to variables described previously, prep uses the
! following for communication..
! yh     = the history array.  only the first column, containing the
!          current y vector, is used.  used only if moss .ne. 0.
! savf   = a work array of length neq, used only if moss .ne. 0.
! ewt    = array of length neq containing (inverted) error weights.
!          used only if moss = 2 or if istate = moss = 1.
! ftem   = a work array of length neq, identical to acor in the driver,
!          used only if moss = 2.
! wk     = a real work array of length lenwk, identical to wm in
!          the driver.
! iwk    = integer work array, assumed to occupy the same space as wk.
! lenwk  = the length of the work arrays wk and iwk.
! istatc = a copy of the driver input argument istate (= 1 on the
!          first call, = 3 on a continuation call).
! iys    = flag value from odrv or cdrv.
! ipper  = output error flag with the following values and meanings..
!          0  no error.
!         -1  insufficient storage for internal structure pointers.
!         -2  insufficient storage for jgroup.
!         -3  insufficient storage for odrv.
!         -4  other error flag from odrv (should never occur).
!         -5  insufficient storage for cdrv.
!         -6  other error flag from cdrv.
!-----------------------------------------------------------------------
      ibian = lrat*2
      ipian = ibian + 1
      np1 = n + 1
      ipjan = ipian + np1
      ibjan = ipjan - 1
      liwk = lenwk*lrat
      if (ipjan+n-1 .gt. liwk) go to 210
      if (moss .eq. 0) go to 30
!
      if (istatc .eq. 3) go to 20
! istate = 1 and moss .ne. 0.  perturb y for structure determination. --
      do 10 i = 1,n
        erwt = 1.0e0/ewt(i)
        fac = 1.0e0 + 1.0e0/(float(i)+1.0e0)
        y(i) = y(i) + fac*sign(erwt,y(i))
 10     continue
      go to (70, 100), moss
!
 20   continue
! istate = 3 and moss .ne. 0.  load y from yh(*,1). --------------------
      do 25 i = 1,n
 25     y(i) = yh(i)
      go to (70, 100), moss
!
! moss = 0.  process user-s ia,ja.  add diagonal entries if necessary. -
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
!
! moss = 1.  compute structure from user-supplied jacobian routine jac.
 70   continue
! a dummy call to f allows user to create temporaries for use in jac. --
      call f (neq, tn, y, savf,   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      k = ipjan
      iwk(ipian) = 1
      do 90 j = 1,n
        if (k .gt. liwk) go to 210
        iwk(k) = j
        k = k + 1
        do 75 i = 1,n
 75       savf(i) = 0.0e0
        call jac (neq, tn, y, j, iwk(ipian), iwk(ipjan), savf,   &
            ruserpar, nruserpar, iuserpar, niuserpar)
        do 80 i = 1,n
          if (abs(savf(i)) .le. seth) go to 80
          if (i .eq. j) go to 80
          if (k .gt. liwk) go to 210
          iwk(k) = i
          k = k + 1
 80       continue
        iwk(ipian+j) = k + 1 - ipjan
 90     continue
      go to 140
!
! moss = 2.  compute structure from results of n + 1 calls to f. -------
 100  k = ipjan
      iwk(ipian) = 1
      call f (neq, tn, y, savf,   &
          ruserpar, nruserpar, iuserpar, niuserpar)
      do 120 j = 1,n
        if (k .gt. liwk) go to 210
        iwk(k) = j
        k = k + 1
        yj = y(j)
        erwt = 1.0e0/ewt(j)
        dyj = sign(erwt,yj)
        y(j) = yj + dyj
        call f (neq, tn, y, ftem,   &
            ruserpar, nruserpar, iuserpar, niuserpar)
        y(j) = yj
        do 110 i = 1,n
          dq = (ftem(i) - savf(i))/dyj
          if (abs(dq) .le. seth) go to 110
          if (i .eq. j) go to 110
          if (k .gt. liwk) go to 210
          iwk(k) = i
          k = k + 1
 110      continue
        iwk(ipian+j) = k + 1 - ipjan
 120    continue
!
 140  continue
      if (moss .eq. 0 .or. istatc .ne. 1) go to 150
! if istate = 1 and moss .ne. 0, restore y from yh. --------------------
      do 145 i = 1,n
 145    y(i) = yh(i)
 150  nnz = iwk(ipian+n) - 1
      lenigp = 0
      ipigp = ipjan + nnz
      if (miter .ne. 2) go to 160
!
! compute grouping of column indices (miter = 2). ----------------------
      maxg = np1
      ipjgp = ipjan + nnz
      ibjgp = ipjgp - 1
      ipigp = ipjgp + n
      iptt1 = ipigp + np1
      iptt2 = iptt1 + n
      lreq = iptt2 + n - 1
      if (lreq .gt. liwk) go to 220
      call jgroup (n, iwk(ipian), iwk(ipjan), maxg, ngp, iwk(ipigp),   &
         iwk(ipjgp), iwk(iptt1), iwk(iptt2), ier)
      if (ier .ne. 0) go to 220
      lenigp = ngp + 1
!
! compute new ordering of rows/columns of jacobian. --------------------
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
      call odrv (n, iwk(ipian), iwk(ipjan), wk, iwk(ipr), iwk(ipic),   &
         nsp, iwk(ipisp), 1, iys)
      if (iys .eq. 11*n+1) go to 240
      if (iys .ne. 0) go to 230
!
! reorder jan and do symbolic lu factorization of matrix. --------------
      ipa = lenwk + 1 - nnz
      nsp = ipa - iprsp
      lreq = max0(12*n/lrat, 6*n/lrat+2*n+nnz) + 3
      lreq = lreq + iprsp - 1 + nnz
      if (lreq .gt. lenwk) go to 250
      iba = ipa - 1
      do 180 i = 1,nnz
 180    wk(iba+i) = 0.0e0
      ipisp = lrat*(iprsp - 1) + 1
      call cdrv (n,iwk(ipr),iwk(ipc),iwk(ipic),iwk(ipian),iwk(ipjan),   &
         wk(ipa),wk(ipa),wk(ipa),nsp,iwk(ipisp),wk(iprsp),iesp,5,iys)
      lreq = lenwk - iesp
      if (iys .eq. 10*n+1) go to 250
      if (iys .ne. 0) go to 260
      ipil = ipisp
      ipiu = ipil + 2*n + 1
      nzu = iwk(ipil+n) - iwk(ipil)
      nzl = iwk(ipiu+n) - iwk(ipiu)
      if (lrat .gt. 1) go to 190
      call adjlr (n, iwk(ipisp), ldif)
      lreq = lreq + ldif
 190  continue
      if (lrat .eq. 2 .and. nnz .eq. n) lreq = lreq + 1
      nsp = nsp + lreq - lenwk
      ipa = lreq + 1 - nnz
      iba = ipa - 1
      ipper = 0
      return
!
 210  ipper = -1
      lreq = 2 + (2*n + 1)/lrat
      lreq = max0(lenwk+1,lreq)
      return
!
 220  ipper = -2
      lreq = (lreq - 1)/lrat + 1
      return
!
 230  ipper = -3
      call cntnzu (n, iwk(ipian), iwk(ipjan), nzsut)
      lreq = lenwk - iesp + (3*n + 4*nzsut - 1)/lrat + 1
      return
!
 240  ipper = -4
      return
!
 250  ipper = -5
      return
!
 260  ipper = -6
      lreq = lenwk
      return
!----------------------- end of subroutine prep_lsodes ------------------------
      end subroutine prep_lsodes                                      
