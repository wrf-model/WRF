      SUBROUTINE INTEGRATE( TIN, TOUT )

      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
      EXTERNAL ITER
      KPP_REAL T
      KPP_REAL V(NVAR), VOLD(NVAR), VNEW(NVAR)
      KPP_REAL startdt, hmin, hmax, h 

      INTEGER    INFO(5)

      INFO(1) = Autonomous
      h = hmin
      
c Number of Jacobi-Seidel iterations      
      numit = 3   

      
      DO i=1,NVAR
        RTOL(i) = 1.e-2
      ENDDO	

      CALL twostepj(NVAR,TIN,TOUT,h,hmin,hmax,
     +                   VOLD,VAR,VNEW, 
     +                   ATOL,RTOL,numit,
     +                   nfcn,naccpt,nrejec,nstart,startdt,ITER)


      RETURN
      END
      
      

      SUBROUTINE ITER(n,T,y,yp,yl)
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
      REAL*8 T, y(NVAR), yp(NVAR), yl(NVAR)
      TOLD = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL FunSPLIT_VAR(y,Rad,yp,yl)
      TIME = TOLD
      RETURN
      END
     

      subroutine twostepj(n,t,te,dt,dtmin,dtmax,
     +                   yold,y,ynew,
     +                   atol,rtol,numit,
     +                   nfcn,naccpt,nrejec,nstart,startdt,ITER)
      implicit real*8 (a-h,o-z)
      external ITER
      integer	 n,numit,nfcn,naccpt,nrejec,nstart,i,j
      real*8	 t,te,dt,dtmin,dtmax,startdt,ytol,
     +           ratio,dtold,a1,a2,c,cp1,dtg,errlte,dy
      real*8       yold(n),y(n),ynew(n),yp(n),yl(n),
     +           work(n),sum(n),atol(n),rtol(n)
      logical 	 accept,failer,restart

c
c     Initialization of counters, etc.

      naccpt=0
      nrejec=0
      nfcn=0
      nstart=0
      failer=.false.
      restart=.false.
      accept=.true.

c     Initial stepsize computation. 
    
   10 if (dtmin.eq.dtmax) then
       nstart=1
       dt=min(dtmin,(te-t)/2)
       goto 28
      endif
      CALL ITER(n,t,y,yp,yl)
      nfcn=nfcn+1
      dt=te-t
      do 20 i=1,n
       ytol=atol(i)+rtol(i)*abs(y(i))
       dy=yp(i)-y(i)*yl(i)
       if (dy.ne.0.0) dt=min(dt,ytol/abs(dy))
   20 continue
   25 nstart=nstart+1
      if (restart) dt=dt/10.0
      restart=.true.
      dt=max(dtmin,min(dt,dtmax))
      CALL FIT(t,te,dt)
      dt=min(dt,(te-t)/2)
      startdt=dt

c     The starting step is carried out, using the implicit Euler method.

   28 do 30 i=1,n
       ynew(i)=y(i)
       yold(i)=y(i)
       sum(i)=y(i)
   30 continue
      do 40 i=1,numit
       CALL ITER(n,t+dt,ynew,yp,yl)
       do i2i=1,n
         ynew(i2i) = (sum(i2i) + dt*yp(i2i))/(1.+dt*yl(i2i))
       end do
       nfcn=nfcn+1
   40 continue
      naccpt=naccpt+1
      t=t+dt
      do 50 j=1,n
       y(j)=ynew(j)
   50 continue

c     Subsequent steps are carried out with the two-step BDF method.

      dtold=dt
      ratio=1.0
   60 continue
      c=1.0/ratio
      cp1=c+1.0
      a1=((c+1.0)**2)/(c*c+2.0*c)
      a2=-1.0/(c*c+2.0*c)
      dtg=dt*(1.0+c)/(2.0+c)
      do 70 j=1,n
       sum(j)=a1*y(j)+a2*yold(j)
       ynew(j)=max(0.0,y(j)+ratio*(y(j)-yold(j)))
   70 continue
      do 80 i=1,numit
       CALL ITER(n,t+dt,ynew,yp,yl)
       do i2i=1,n
        ynew(i2i) = (sum(i2i) + dtg*yp(i2i))/(1.+dtg*yl(i2i))
       end do
       nfcn=nfcn+1
   80 continue

c     If stepsizes should remain equal, stepsize control is omitted.

      if (dtmin.eq.dtmax) then
       t=t+dtold
       naccpt=naccpt+1
       do 85 j=1,n
        yold(j)=y(j)
        y(j)=ynew(j)
   85  continue
       if (dt.ne.dtold) then
	t=t-dtold+dt
	goto 120
       endif
       dt=min(dtold,te-t)
       ratio=dt/dtold
       if (t.ge.te) goto 120
       goto 60
      endif

c     Otherwise stepsize control is carried out.

      errlte=0.0
      do 90 i=1,n
       ytol=atol(i)+rtol(i)*abs(y(i))
       errlte=max(errlte,abs(c*ynew(i)-cp1*y(i)+yold(i))/ytol)
   90 continue
      errlte=2.0*errlte/(c+c*c)
      CALL NEWDT(t,te,dt,dtold,ratio,errlte,accept,
     +           dtmin,dtmax)

c     Here the step has been accepted. 

      if (accept) then
 201   format(2(E24.16,1X))
       failer=.false.
       restart=.false.
       t=t+dtold
       naccpt=naccpt+1
       do 100 j=1,n
	yold(j)=y(j)
	y(j)=ynew(j)
  100  continue
       if (t.ge.te) goto 120
       goto 60
      endif

c     A restart check is carried out.
      
      if (failer) then
       nrejec=nrejec+1
       failer=.false.
       naccpt=naccpt-1
       t=t-dtold
       do 110 j=1,n
        y(j)=yold(j)
  110  continue
       goto 25
      endif

c     Here the step has been rejected.

      nrejec=nrejec+1
      failer=.true.
      goto 60
      
c     End of TWOSTEP.
  120 end
c=====================================================================

      subroutine NEWDT(t,te,dt,dtold,ratio,errlte,
     +                 accept,dtmin,dtmax)
      real*8    t,te,dt,dtold,ratio,errlte,ts,dtmin,dtmax
      logical accept
      if (errlte.gt.1.0.and.dt.gt.dtmin) then
       accept=.false.
       ts=t
      else
       accept=.true.
       dtold=dt
       ts=t+dtold
      endif
      dt=max(0.5,min(2.0,0.8/sqrt(errlte)))*dt
      dt=max(dtmin,min(dt,dtmax))
      CALL FIT(ts,te,dt)
      ratio=dt/dtold
      end

      subroutine FIT(t,te,dt)
      real*8 	t,te,dt,rns
      integer 	ns
      rns=(te-t)/dt
      if (rns.gt.10.0) goto 10
      ns=int(rns)+1
      dt=(te-t)/ns
      dt=(dt+t)-t
   10 return
      end



C End of MAIN function                                             
C **************************************************************** 

