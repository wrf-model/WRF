!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! CBMZ module: see module_cbmz.F for references and terms of use
!**********************************************************************************  
      module module_cbmz_rodas3_solver

      contains


!-----------------------------------------------------------------------

      subroutine rodas3_ff_x2( n, t, tnext, hmin, hmax, hstart,   &
            y, abstol, reltol, yposlimit, yneglimit,   &
            yfixed, rconst,   &
            lu_nonzero_v, lu_crow_v, lu_diag_v, lu_icol_v,   &
            info, iok, lunerr,   &
            dydtsubr, yjacsubr, decompsubr, solvesubr )
!     include 'sparse_s.h'

!       stiffly accurate rosenbrock 3(2), with
!     stiffly accurate embedded formula for error control.
!
!     all the arguments aggree with the kpp syntax.
!
!  input arguments:
!     n = number of dependent variables (i.e., time-varying species)
!     [t, tnext] = the integration interval
!     hmin, hmax = lower and upper bounds for the selected step-size.
!          note that for step = hmin the current computed
!          solution is unconditionally accepted by the error
!          control mechanism.
!     hstart = initial guess step-size
!     y = vector of (n) concentrations, contains the
!         initial values on input
!     abstol, reltol = (n) dimensional vectors of
!          componentwise absolute and relative tolerances.
!     yposlimit = vector of (n) upper-limit positive concentrations
!     yneglimit = vector of (n) upper-limit negative concentrations
!     yfixed = vector of (*) concentrations of fixed/non-reacting species
!     rconst = vector of (*) concentrations of fixed/non-reacting species
!
!     lu_nonzero_v = number of non-zero entries in the "augmented jacobian"
!          (i.e., the jacobian with all diagonal elements filled in)
!     lu_crow_v = vector of (n) pointers to the crow (?) elements
!     lu_diag_v = vector of (n) pointers to the diagonal elements
!	   of the sparsely organized jacobian.
!          jac(lu_diag_v(i)) is the i,i diagonal element
!     lu_icol_v = vector of (lu_nonzero_v) pointers to the icol (?) elements
!     info(1) = 1  for  autonomous   system
!             = 0  for nonautonomous system
!     iok = completion code (output)
!	+1 = successful integration
!	+2 = successful integration, but some substeps were h=hmin
!		and error > abstol,reltol
!	-1 = failure -- lu decomposition failed with minimum stepsize h=hmin
!	-1001 --> -1999 = failure -- with minimum stepsize h=hmin,
!		species i = -(iok+1000) was NaN
!	-2001 --> -2999 = failure -- with minimum stepsize h=hmin,
!		species i = -(iok+2000) was > yneglimit
!	-3001 --> -3999 = failure -- with minimum stepsize h=hmin,
!		species i = -(iok+3000) was < yposlimit
!	-5001 --> -5999 = failure -- with minimum stepsize h=hmin,
!		species i = -(iok+5000) had abs(kn(i)) > ylimit_solvesubr
!		before call to solvesubr, where kn is either k1,k2,k3,k4
!
!     dydtsubr = name of routine of derivatives. kpp syntax.
!          see the header below.
!     yjacsubr = name of routine that computes the jacobian, in
!          sparse format. kpp syntax. see the header below.
!     decompsubr = name of routine that does sparse lu decomposition
!     solvesubr = name of routine that does sparse lu backsolve
!
!  output arguments:
!     y = the values of concentrations at tend.
!     t = equals tend on output.
!     info(2) = # of dydtsubr calls.
!     info(3) = # of yjacsubr calls.
!     info(4) = # of accepted steps.
!     info(5) = # of rejected steps.
!     info(6) = # of steps that were accepted but had
!		(hold.eq.hmin) .and. (err.gt.1)
!		which means stepsize=minimum and error>tolerance
!
!     adrian sandu, march 1996
!     the center for global and regional environmental research

      use module_peg_util, only:  peg_message, peg_error_fatal

      implicit none

      integer nvar_maxd, lu_nonzero_v_maxd
      parameter (nvar_maxd=99)
      parameter (lu_nonzero_v_maxd=999)

! common block variables

! subr parameters
      integer    n, info(6), iok, lunerr, lu_nonzero_v
      integer    lu_crow_v(n), lu_diag_v(n), lu_icol_v(lu_nonzero_v)
      real       t, tnext, hmin, hmax, hstart
      real       y(n), abstol(n), reltol(n), yposlimit(n), yneglimit(n)
      real       yfixed(*), rconst(*)
      external   dydtsubr, yjacsubr, decompsubr, solvesubr

! local variables
      logical    isreject, autonom
      integer    nfcn, njac, naccept, nreject, nnocnvg, i, j
      integer    ier

      real       k1(nvar_maxd), k2(nvar_maxd)
      real       k3(nvar_maxd), k4(nvar_maxd)
      real       f1(nvar_maxd), ynew(nvar_maxd)
      real       jac(lu_nonzero_v_maxd)
      real       ghinv, uround
      real       tin, tplus, h, hold, hlowest
      real       err, factor, facmax
      real       dround, c43, tau, x1, x2, ytol
      real       ylimit_solvesubr

      character*80 errmsg

      ylimit_solvesubr = 1.0e18

!     check n and lu_nonzero_v
      if (n .gt. nvar_maxd) then
          call peg_message( lunerr, '*** rodas3 dimensioning problem' )
          write(errmsg,9050) 'n, nvar_maxd = ', n, nvar_maxd
          call peg_message( lunerr, errmsg )
          call peg_error_fatal( lunerr, '*** rodas3 fatal error' )
      else if (lu_nonzero_v .gt. lu_nonzero_v_maxd) then
          call peg_message( lunerr, '*** rodas3 dimensioning problem' )
          write(errmsg,9050) 'lu_nonvero_v, lu_nonzero_v_maxd = ',   &
                lu_nonzero_v, lu_nonzero_v_maxd
          call peg_message( lunerr, errmsg )
          call peg_error_fatal( lunerr, '*** rodas3 fatal error' )
      end if
9050  format( a, 2(1x,i6) )

!     initialization
      uround = 1.e-7
      dround = sqrt(uround)

!     check hmin and hmax
      hlowest = dround
      if (info(1) .eq. 1) hlowest = 1.0e-7
      if (hmin .lt. hlowest) then
          call peg_message( lunerr, '*** rodas3 -- hmin is too small' )
          write(errmsg,9060) 'hmin and minimum allowed value = ',   &
                hmin, hlowest
          call peg_message( lunerr, errmsg )
          call peg_error_fatal( lunerr, '*** rodas3 fatal error' )
      else if (hmin .ge. hmax) then
          call peg_message( lunerr, '*** rodas3 -- hmin >= hmax' )
          write(errmsg,9060) 'hmin, hmax = ', hmin, hmax
          call peg_message( lunerr, errmsg )
          call peg_error_fatal( lunerr, '*** rodas3 fatal error' )
      end if
9060  format( a, 1p, 2e14.4 )

!     initialization of counters, etc.
      autonom = info(1) .eq. 1
!##checkthis##
      c43 = - 8.e0/3.e0
!     h = 60.e0 ! hmin
      h = max( hstart, hmin )
      tplus = t
      tin = t
      isreject = .false.
      naccept  = 0
      nreject  = 0
      nnocnvg  = 0
      nfcn     = 0
      njac     = 0


! === starting the time loop ===
 10    continue
       tplus = t + h
       if ( tplus .gt. tnext ) then
          h = tnext - t
          tplus = tnext
       end if

       call yjacsubr( n, t, y, jac, yfixed, rconst )
       njac = njac+1
       ghinv = -2.0e0/h
       do 20 j=1,n
         jac(lu_diag_v(j)) = jac(lu_diag_v(j)) + ghinv
 20    continue
       call decompsubr( n, jac, ier, lu_crow_v, lu_diag_v, lu_icol_v )

       if (ier.ne.0) then
         if ( h.gt.hmin) then
            h = 5.0e-1*h
            go to 10
         else
!           print *,'ier <> 0, h=',h
!           stop
            iok = -1
            goto 200
         end if
       end if

       call dydtsubr( n, t, y, f1, yfixed, rconst )

! ====== nonautonomous case ===============
       if (.not. autonom) then
!        tau = sign(dround*max( 1.0e-6, abs(t) ), t)
         tau = dround*max( 1.0e-6, abs(t) )
         call dydtsubr( n, t+tau, y, k2, yfixed, rconst )
         nfcn=nfcn+1
         do 30 j = 1,n
           k3(j) = ( k2(j)-f1(j) )/tau
 30      continue

! ----- stage 1 (nonautonomous) -----
         x1 = 0.5*h
         do 40 j = 1,n
           k1(j) =  f1(j) + x1*k3(j)
           if (abs(k1(j)) .gt. ylimit_solvesubr) then
               iok = -(5000+j)
               goto 135
           end if
 40      continue
         call solvesubr( jac, k1 )

! ----- stage 2 (nonautonomous) -----
         x1 = 4.e0/h
         x2 = 1.5e0*h
         do 50 j = 1,n
           k2(j) = f1(j) - x1*k1(j) + x2*k3(j)
           if (abs(k2(j)) .gt. ylimit_solvesubr) then
               iok = -(5000+j)
               goto 135
           end if
 50      continue
         call solvesubr( jac, k2 )

! ====== autonomous case ===============
       else
! ----- stage 1 (autonomous) -----
         do 60 j = 1,n
           k1(j) =  f1(j)
           if (abs(k1(j)) .gt. ylimit_solvesubr) then
               iok = -(5000+j)
               goto 135
           end if
 60      continue
         call solvesubr( jac, k1 )

! ----- stage 2 (autonomous) -----
         x1 = 4.e0/h
         do 70 j = 1,n
           k2(j) = f1(j) - x1*k1(j)
           if (abs(k2(j)) .gt. ylimit_solvesubr) then
               iok = -(5000+j)
               goto 135
           end if
 70      continue
         call solvesubr( jac, k2 )
       end if

! ----- stage 3 -----
       do 80 j = 1,n
         ynew(j) = y(j) - 2.0e0*k1(j)
 80    continue
       call dydtsubr( n, t+h, ynew, f1, yfixed, rconst )
       nfcn=nfcn+1
       do 90 j = 1,n
         k3(j) = f1(j) + ( -k1(j) + k2(j) )/h
         if (abs(k3(j)) .gt. ylimit_solvesubr) then
             iok = -(5000+j)
             goto 135
         end if
 90    continue
       call solvesubr( jac, k3 )

! ----- stage 4 -----
       do 100 j = 1,n
         ynew(j) = y(j) - 2.0e0*k1(j) - k3(j)
 100   continue
       call dydtsubr( n, t+h, ynew, f1, yfixed, rconst )
       nfcn=nfcn+1
       do 110 j = 1,n
         k4(j) = f1(j) + ( -k1(j) + k2(j) - c43*k3(j)  )/h
         if (abs(k4(j)) .gt. ylimit_solvesubr) then
             iok = -(5000+j)
             goto 135
         end if
 110   continue
       call solvesubr( jac, k4 )

! ---- the solution ---

       do 120 j = 1,n
         ynew(j) = y(j) - 2.0e0*k1(j) - k3(j) - k4(j)
 120   continue


! ====== error estimation ========

        err=0.e0
        do 130 i=1,n
           ytol = abstol(i) + reltol(i)*abs(ynew(i))
           err = err + ( k4(i)/ytol )**2
 130    continue
        err = max( uround, sqrt( err/n ) )

! ======= choose the stepsize ===============================

        factor = 0.9/err**(1.e0/3.e0)
        if (isreject) then
            facmax=1.0
        else
            facmax=10.0
        end if
        factor = max( 1.0e-1, min(factor,facmax) )
        hold = h
        h = min( hmax, max(hmin,factor*h) )

! ======= check for nan, too big, too small =================
!   if any of these conditions occur, either
!	exit if hold <= hmin
!	reduce step size and retry if hold > hmin

	iok = 1
	do i = 1, n
	    if (y(i) .ne. y(i)) then
		iok = -(1000+i)
		goto 135
	    else if (y(i) .lt. yneglimit(i)) then
		iok = -(2000+i)
		goto 135
	    else if (y(i) .gt. yposlimit(i)) then
		iok = -(3000+i)
		goto 135
	    end if
	end do
135	if (iok .lt. 0) then
	    if (hold .le. hmin) then
		goto 200
	    else
		isreject = .true.
		nreject  = nreject+1
                h = max(hmin, 0.5*hold)
		if (t.eq.tin) h = max(hmin, 1.0e-1*hold)
		goto 10
	    end if
	end if
		

! ======= rejected/accepted step ============================

        if ( (err.gt.1).and.(hold.gt.hmin) ) then
          isreject = .true.
          nreject  = nreject+1
          if (t.eq.tin) h = max(hmin, 1.0e-1*hold)
        else
          isreject = .false.
          do 140 i=1,n
             y(i)  = ynew(i)
 140      continue
          t = tplus
          if (err.gt.1) then
            nnocnvg = nnocnvg+1
          else
            naccept = naccept+1
          end if
        end if

! ======= end of the time loop ===============================
      if ( t .lt. tnext ) go to 10

      iok = 1
      if (nnocnvg .gt. 0) iok = 2


! ======= output information =================================
200   info(2) = nfcn
      info(3) = njac
      info(4) = naccept
      info(5) = nreject
      info(6) = nnocnvg

      return
      end subroutine rodas3_ff_x2                                


      end module module_cbmz_rodas3_solver
