      subroutine inria_n2qn1 (simul, n, x, f, g, dxmin, df1, epsabs, imp, io,&
      mode, iter, nsim, binf, bsup, iz, rz, izs, rzs, dzs)
      implicit double precision (a-h,o-z)
      
      external :: simul
      double precision,intent(inout) :: f,g,x,df1,epsabs
      double precision,intent(in) :: dxmin,binf,bsup
      real,intent(in) :: rzs
      integer,intent(in) :: n,imp,io
      integer,intent(inout) :: nsim,mode,izs,iter
      double precision, intent(inout) :: dzs
      integer, intent(out) :: iz
      double precision, intent(out) :: rz
      
      integer :: nd,nww,nww1,ni,nw,i,nibloc,nindi,nga
      dimension x(n),g(n),dxmin(n),bsup(n),binf(n)
      dimension iz(*),rz(*),izs(*),dzs(*), rzs(*)
      logical plantage, modifx
      double precision gpmopt
      
      if (imp.gt.0) then
      nw=n*(9+n)/2
      ni=2*n+1
      write (io,1000) n,mode,iter,nsim,imp,df1,epsabs
      endif
 1000 format (/" N2QN1 (Version 2.1.a, May, 2004): point d'entree"/&
      5x,"dimension du probleme (n):",i9/&
      5x,"mode d'entree (mode):     ",i9/&
      5x,"max iterations (iter):    ",i9/&
      5x,"max simulation (nsim):    ",i9/&
      5x,"niveau d'impression (imp):",i9/&
      5x,"decroissance attendue de f (df1):",1pd9.2/&
      5x,"precision absolue (epsabs):      ",1pd9.2)
      if (imp.ge.4) then
      write (io,1001) ni,nw
      endif
 1001 format (/" Working zone:"/&
      5x,"integer:          ", i6/&
      5x,"double precision: ", i6)
      if (n.le.0&
      .or. (mode.eq.1 .and. df1.le.0.d0)&
      .or. epsabs.lt.0.d0&
      .or. imp.lt.0 .or. imp.gt.5&
      .or. mode.lt.1 .or. mode.gt.4&
      .or. iter.le.0&
      .or. nsim.le.0) then
      write (io,1002)
      mode=2
      return
      endif
 1002 format (/" >>> m2qn1: appel incoherent (n, df1, epsabs, imp,",&
      " mode, iter ou nsim)"/)
      modifx   = .false.
      plantage = .false.
      do i=1,n
      if (dxmin(i).le.0.d0) then
      if (.not.(modifx.or.plantage)) write (io,'()')
      write (io,'(/a,1pd12.5,a,i4/)')&
      " >>> n2qn1: dxmin(i) = ",dxmin(i)," est negatif, ",&
      "i = ", i
      plantage = .true.
      mode=2
      elseif (bsup(i)-binf(i).lt.-2.d0*dxmin(i)) then
      if (.not.(modifx.or.plantage)) write (io,'()')
      write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')&
      " >>> n2qn1: binf(i) = ",binf(i),"  > bsup(i) = ",&
      bsup(i),", i = ", i
      plantage = .true.
      mode=2
      elseif (x(i).lt.binf(i)-dxmin(i)) then
      plantage = .true.
      mode=2
      if (.not.(modifx.or.plantage)) write (io,'()')
      write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')&
      " >>> n2qn1: x(i) = ",x(i),"  < binf(i) = ",&
      binf(i),", i = ", i
      elseif (x(i).gt.bsup(i)+dxmin(i)) then
      plantage = .true.
      mode=2
      if (.not.(modifx.or.plantage)) write (io,'()')
      write (io,'(/a,1pd12.5,a,d12.5,a,i4/)')&
      " >>> n2qn1: x(i) = ",x(i),"  > bsup(i) = ",&
      bsup(i),", i = ", i
      endif
      enddo
      if (plantage) return
      nd=1+(n*(n+1))/2
      nww=nd+n
      nww1=nww+n
      nga=nww1+n
      nindi=1
      nibloc=nindi+n
      ni=nibloc+n
      s=0.d0
      do i=1,n
      s=s+dxmin(i)*dxmin(i)
      enddo
      epsabs=epsabs*dsqrt(s/dble(float(n)))
      call n2qn1a (simul,n,x,f,g,dxmin,epsabs,gpmopt,df1,mode,&
      iter,nsim,imp,io,rz,rz(nd),rz(nww),rz(nww1),&
      rz(nga),binf,bsup,iz(nindi),iz(nibloc),iz(ni),&
      izs,rzs,dzs)
      if (imp.ge.2) write(io,1003)
 1003 format (1x,79("-"))
      if (imp.ge.1) write(io,1004) mode,iter,nsim,epsabs,gpmopt
 1004 format(/" N2QN1: sortie en mode ",i2/&
      5x,"nombre d'iterations   = ",i4/&
      5x,"nombre de simulations = ",i4/&
      5x,"|gradient projete| moyen       = ",1pd10.3/&
      5x,"|gradient_dxmin projete| moyen = ",1pd10.3)
      if (imp.ge.4) write (io,1005) (i,iz(nibloc+i-1),i=1,n)
 1005 format (5x,"bornes",&
      " (0: inactive, -1: binf active, +1: bsup active)",/&
      (9x,"| ",5(i5,": ",i2," |")))
      return
      end subroutine inria_n2qn1
      subroutine n2qn1a (simul,n,x,f,ga,dxmin,acc,gpmopt,&
      df1,mode,niter,nsim,iprint,lp,h,d,w,w1,g,&
      binf,bsup,indi,ibloc,iz,izs,rzs,dzs)
      implicit double precision (a-h,o-z)
      double precision :: g,w1,binf,dzs,bsup,w,ga,dxmin,f,x,h,d,acc,df1,one
      integer :: ibloc,iz,izs,niter,mode,n,nsim,indi,lp,iprint,i,k,nr1,ir,nca,ncs,nc
      integer :: isign,i1,iecri,nfun,nrp1,indic2,itr,logic,indic1,indic,k1,k2,nh,nr,ii
      dimension x(n),g(n),dxmin(n),h(*),d(n),w(n),w1(n)
      dimension binf(n),bsup(n),ga(n),izs(*),iz(*)
      dimension dzs(*),ibloc(n),indi(n)
      double precision gdxmin,gpmopt,r
      real rzs(*)
      external simul
      logical oltodo
      integer np
      double precision gpm1, gpm, opt, ys, shs, ol
      double precision zero, pi
      parameter (zero = 0.d0, one = 1.d0, pi = 3.1415927d+0)
 1001 format (" n2qn1: termine par voeu de l'utilisateur")
 1002 format (" >>> n2qn1: appel incoherent")
 1024 format (1x)
      alfa=0.99d0
      beta=1.d-4
      prop=one
      nfun=1
      iecri=0
      itr=0
      np=n+1
      indic2=1
      logic=0
      df=df1
      oltodo=.false.
      if (mode.eq.1) oltodo=.true.
      if (mode.eq.4) then
      nr=iz(1)
      go to 400
      endif
      nr=0
      if (mode.eq.1) then
      do i=1,n
      if (x(i).ge.bsup(i)-dxmin(i) .and. ga(i).lt.0.) then
      ibloc(i)=1
      elseif (x(i).le.binf(i)+dxmin(i) .and. ga(i).gt.0.) then
      ibloc(i)=-1
      else
      nr=nr+1
      ibloc(i)=0
      endif
      enddo
      else
      do i=1,n
      if (x(i).ge.bsup(i)-dxmin(i)) then
      ibloc(i)=1
      elseif (x(i).le.binf(i)+dxmin(i)) then
      ibloc(i)=-1
      else
      nr=nr+1
      ibloc(i)=0
      endif
      enddo
      endif
      nr1=nr+1
      r=zero
      c=zero
      dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
      do 100 i=1,n
      if (ibloc(i).ne.0)  go to 100
      gi=ga(i)
      gdxmin=gi*dxmin(i)
      r=r+gi*gi
      c=c+gdxmin*gdxmin
  100 continue
      r=dsqrt(r)
      c=dsqrt(c)
      if (iprint.ge.1) then
      gpm1=c
      gpm=c
      if (nr.ne.0) then
      r=r/dnr
      gpm1=c/dnr
      gpm=one
      endif
      if (gpm1.gt.zero) opt=acc/gpm1
      write (lp,1004) r,gpm1,opt
      endif
 1004 format (/" Conditions initiales:"/&
      5x,"|gradient projete| moyen:       ",1pd11.4/&
      5x,"|gradient_dxmin projete| moyen: ",1pd11.4/&
      5x,"optimalite relative demandee:   ",d11.4/)
      if (c.le.acc1) then
      call fcomp1 (indic2,ibloc,indi,h,ga,d,w,w1,n,nr,ncs,dga,delta,&
      prop,acc,dxmin)
      if (ncs.eq.0) then
      itr=1
      mode=1
      go to 900
      endif
      ibloc(ncs)=0
      nr=nr+1
      endif
      go to (300,310,320),mode
  300 continue
      c=zero
      do i=1,n
      if (ibloc(i).eq.0) then
      gdxmin=ga(i)*dxmin(i)
      c=c+gdxmin*gdxmin
      endif
      enddo
      c=0.5d0*c/df1
      do i=1,n
      sc=dxmin(i)
      w(i)=c/(sc*sc)
      enddo
      nh=n*(n+1)/2
      do i=1,nh
      h(i)=zero
      enddo
      k1=1
      k2=nr+1
      do i=1,n
      if (ibloc(i).eq.0) then
      indi(i)=k1
      k1=k1+1
      else
      indi(i)=k2
      k2=k2+1
      endif
      enddo
      mode=1
      call fmani1 (mode,n,w,d,indi)
      call n2qn1_init_diag (n, nr, h, d)
      go to 400
  310 call fmc11b (h,n,k)
      if (k.lt.n) then
      if (iprint.ne.0) write(lp,1010)
      goto 300
      endif
 1010 format (" n2qn1: remplace le hessien initial (qui n'est",&
      " pas defini positif)"/" par une diagonale positive")
  312 nr=n
      do 313 i=1,n
  313 indi(i)=i
      do 314 i=1,n
      if (ibloc(i).eq.0) go to 314
      nc=i
      call fajc1 (n,nc,nr,h,w,indi)
  314 continue
      go to 400
  320 k=1
      do i=1,n
      if (h(k).le.zero) then
      if (iprint.ne.0) write(lp,1010)
      goto 300
      endif
      k=k+np-i
      enddo
      go to 312
  400 indic2=0
      if (iprint.lt.3) then
      write(lp,'()')
      elseif (iprint.eq.4) then
      write (lp,1003) (i,ibloc(i),i=1,n)
 1003 format (5x,"bornes",&
      " (0: inactive, -1: binf active, +1: bsup active)",/&
      (9x,"| ",5(i5,": ",i2," |")))
      write(lp,'()')
      endif
  410 dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
  500 itr=itr+1
      if (itr.ne.1)df=fa-f
      fa=f
      indic1=0
  501 if (itr.le.niter) go to 502
      mode=4
      go to 900
  502 continue
!     flush (lp)
      if (iprint.eq.3) then
      if (mod(itr-1,40).eq.0) write (lp,'(/a/a)')&
      "  iters  simuls  nactiv       f       |gp|_2/|gp0|_2",&
      "  ^^^^^  ^^^^^^  ^^^^^^  ^^^^^^^^^^^^  ^^^^^^^^^^^^"
      write (lp,1019) itr, nfun, n-nr, f, gpm
 1019 format (1x,i6,2x,i6,2x,i6,2x,1pe12.5,2x,d12.5)
      elseif (iprint.ge.4) then
      write(lp,1020) itr,nfun,f
 1020 format (1x,79("-")/" n2qn1:",i4," iters",i6," simuls","   f=",&
      1pd15.7/)
      endif
      iecri=iecri+1
      if (iecri.eq.-iprint) then
      iecri=0
      indic=1
      call simul(indic,n,x,f,g,izs,rzs,dzs)
      endif
  510 continue
      if (nr.ne.0) go to 511
      indic2=1
      go to 540
  511 mode=1
      call fmani1 (mode,n,ga,w,indi)
      wii=zero
      do i=1,nr
      wi=w(i)
      wiii=wi*dxmin(i)
      wii=wii+wiii*wiii
      w(i)=-wi
      enddo
      wii=dsqrt(wii)
      gpm=wii
      if (wii.gt.acc1) go to 513
      indic2=1
      go to 540
  513 call fmc11e (h,nr,w,w1,nr)
      if (nr.lt.n) then
      nrp1=nr+1
      do i=nrp1,n
      w(i)=zero
      enddo
      endif
      mode=-1
      call fmani1 (mode,n,w,d,indi)
      dga=zero
      do i=1,n
      dga=dga+ga(i)*d(i)
      enddo
      if (dga.lt.zero) go to 522
      indic2=1
      go to 540
  522 if (indic1.eq.1) go to 550
  540 call fcomp1 (indic2,ibloc,indi,h,ga,w,d,g,n,nr,ncs,&
      dga,delta,prop,acc,dxmin)
      if (ncs.ne.0) go to 543
      if (indic2.ne.1) go to 541
      mode=1
      go to 900
  541 mode=-1
      call fmani1 (mode,n,w,d,indi)
      go to 550
  543 if (iprint.ge.2) write(lp,1022) itr,nfun,f,ncs
 1022 format (" n2qn1:",i4," iters",i6," simuls","   f=",d15.7,&
      "   borne",i4,"  desactivee")
      indic1=1
      logic=6
      ibloc(ncs)=0
      call fretc1 (mode,n,ncs,nr,h,w,indi,indic2)
      indic2=0
      dnr=dsqrt(dble(float(nr)))
      acc1=acc*dnr
      if (mode.eq.0) go to 511
      mode=7
      if (iprint.ne.0) write(lp,'(/a/)')&
      " >>> n2qn1: error in the update of L"
      go to 900
  550 romax=1.d50
      nca=0
      do 555 i=1,n
      di=d(i)
      if (di.eq.zero) go to 555
      if (di.lt.zero) then
      bi=binf(i)
      xi=bi-x(i)
      if (-one.ge.di)go to 551
      if (xi.le.(di*1.d20)) go to 555
  551 rocand=xi/di
      i1=-1
      else
      bi=bsup(i)
      xi=bi-x(i)
      if (di.ge.one) go to 553
      if (xi.gt.(di*1.d20)) go to 555
  553 rocand=xi/di
      i1=1
      endif
      if (rocand.lt.romax) then
      nca=i
      romax=rocand
      isign=i1
      endif
  555 continue
      if ((nca.gt.0) .and. (dabs(romax*d(nca)).le.dxmin(nca))) then
      ibloc(nca) = isign
      indic1 = 1
      call fajc1 (n,nca,nr,h,w,indi)
      dnr = dsqrt(dble(float(nr)))
      acc1 = acc*dnr
      if (iprint.ge.2) then
      if (isign.lt.0) then
      write(lp,'(a,i4,a/)') " n2qn1: binf",nca," activated"
      else
      write(lp,'(a,i4,a/)') " n2qn1: bsup",nca," activated"
      endif
      endif
      go to 510
      endif
      if (iprint.ge.3) then
      if (nr.gt.0) gpm=wii/dsqrt(dble(float(nr)))
      if (gpm1.gt.zero) gpm=gpm/gpm1
      if (iprint.ge.4) then
      write (lp,1005) gpm
      gg=zero
      dd=zero
      do i=1,n
      if (ibloc(i).eq.0) then
      gg=gg+ga(i)*ga(i)
      dd=dd+d(i)*d(i)
      endif
      enddo
      dd=dga/dsqrt(gg)/dsqrt(dd)
      dd=dmin1(-dd,1.d+0)
      dd=dacos(dd)*180.d0/pi
      write (lp,1021) sngl(dd)
      endif
      endif
 1005 format (1x,"n2qn1: optimalite relative: ",1pd11.4)
 1021 format (/1x,"n2qn1: angle(-gp,d) = ",f5.1," degrees")
      if ((itr.le.n.and.itr.ne.1).and.mode.eq.1) go to 571
      ro=one
      go to 573
  571 if (logic.eq.1) go to 573
      if (logic.ne.6) go to 572
      ro=one
      go to 573
  572 ro=-2.d0*df/dga
  573 roa=ro
      ro=dmin1(ro,romax)
      romin=zero
      do i=1,n
      z=d(i)
      romin = dmax1(romin,dabs(z/dxmin(i)))
      enddo
      romin=one/romin
      if (iprint.ge.4) write (lp,'(/a)') " n2qn1: linesearch"
      call nlis0 (n,simul,fuclid,x,f,dga,ro,romin,romax,d,g,&
      alfa,beta,iprint,lp,logic,nfun,nsim,&
      w,izs,rzs,dzs)
      if (logic.le.1) go to 575
      if (logic.eq.4)mode=5
      if (logic.eq.5)mode=0
      if (logic.eq.6)mode=6
      if (logic.eq.7)mode=indic
      go to 900
  575 theta=one
      if (logic.ne.0) then
      write (lp,'(/1x,a)') "n2qn1: BFGS update skipped"
      do i = 1,n
      ga(i) = g(i)
      enddo
      goto 500
      endif
      if (oltodo .and. nr.gt.0) then
      ys=zero
      ii=1
      do i=1,n
      ys=ys+(g(i)-ga(i))*d(i)
      ii=ii+np-i
      enddo
      call fmani1 (1,n,d,w,indi)
      shs=zero
      k=1
      do i=1,nr
      shs=shs+h(k)*w(i)*w(i)
      k=k+nr1-i
      enddo
      if (ys.le.zero .or. shs.le.zero) then
      mode=3
      if (iprint.gt.0)&
      write (lp,'(/a,a/(12x,a,1pd12.5))')&
      " >>> n2qn1: unsafe vectors y and s for BFGS ",&
      "update",&
      "y'*s = ", ys*ro,&
      "s'*H*s = ", shs*ro*ro
      goto 900
      endif
      ol=ys/shs/ro
      if (iprint.ge.4) write (lp,'(/1x,a,1pd8.2)')&
      "n2qn1: OL factor for matrix initialization ", ol
      if (oltodo) call n2qn1_mult_diagh (n, nr, h, ol)
      oltodo=.false.
      endif
      mode = 1
      call fmani1 (mode,n,d,w,indi)
      ir = -nr
      call fmani1 (mode,n,ga,d,indi)
      do i = 1,nr
      d(i) = -d(i)
      enddo
      call fmlag1 (n,nr,h,w,d)
      dga = zero
      do i = 1,nr
      dga = dga-w(i)*d(i)
      enddo
      call fmc11z (h,n,nr,d,one/dga,w1,ir,1,zero)
      ir = -ir
      do i = 1,n
      gi = g(i)
      g(i) = theta*gi-ga(i)
      ga(i) = gi
      enddo
      call fmani1 (mode,n,g,d,indi)
      dga = zero
      do i = 1,nr
      dga = dga+w(i)*d(i)
      enddo
      dga = dga*ro
      ro = roa
      call fmc11z (h,n,nr,d,one/dga,w1,ir,0,zero)
      if (ir.lt.nr) then
      mode = 3
      if (iprint.gt.0) write (lp,'(/a/)')&
      " >>> n2qn1: the updated BFGS matrix is rank deficient"
      goto 900
      endif
      goto 500
  900 if (mode.ne.5.and.mode.ne.3.and.mode.ge.0) go to 910
      indic=4
      call simul (indic,n,x,f,ga,izs,rzs,dzs)
  910 iz(1)=nr
      acc=zero
      gpmopt=zero
      do 920 i=1,n
      if (ibloc(i).ne.0) go to 920
      gi=ga(i)
      gdxmin=ga(i)*dxmin(i)
      acc=acc+gi*gi
      gpmopt=gpmopt+gdxmin*gdxmin
  920 continue
      if (dnr.gt.zero) then
      acc=dsqrt(acc)/dnr
      gpmopt=dsqrt(gpmopt)/dnr
      endif
      niter=itr
      nsim=nfun
  999 return
      end subroutine n2qn1a
      subroutine fcomp1 (indic2,ibloc,indi,h,g,d,w,w1,n,nr,ncs,&
      dga,delta,prop,acc,dxmin)
      implicit double precision (a-h,o-z)
      double precision :: w,acc,d,h,g,w1,dga,delta,dxmin,prop,winc,dmu,am,dmin1
      integer :: indi,indic2,ibloc,ncs,n,nr,nh1,nh,k,inc,inc1,inr,nrr,ibi,i,j
      dimension ibloc(n),indi(n),h(*),g(n),d(n),&
      w(n),w1(n),dxmin(n)
      ncs=0
      if (nr.eq.n) return
      zm=0.d0
      if (indic2.eq.1) go to 900
      delta=0.d0
      nh=nr*(nr+1)/2
      nrr=n-nr
      call fmlag1 (n,nr,h,d,w)
      do 500 i=1,n
      ibi=ibloc(i)
      if (ibi.eq.0) go to 500
      gi=g(i)
      inc=indi(i)
      inc1=inc-1
      inr=inc-nr
      winc=w(inc)
      dmu=winc+gi
      am=dmin1(dabs(gi),dabs(dmu))
      if (2.d0*dabs(winc).ge.am) go to 500
      if (ibi.eq.-1.and.dmu.ge.0.d0) go to 500
      if (ibi.eq.1.and.dmu.le.0.d0) go to 500
      dmu=dabs(dmu)
      if (dmu*dxmin(i).le.acc) go to 500
      dmu1=dmu*dmu
      k=inr
      nh1=(inc1)*(n+1)-(inc1)*inc/2+1
      z=h(nh1)
      if (nr.eq.0) go to 350
      do j=1,nr
      w1(j)=h(nh+k)
      k=k+nrr
      enddo
      call fmc11e (h,nr,w1,w1,nr)
      k=inr
      do j=1,nr
      z=z-w1(j)*h(nh+k)
      k=k+nrr
      enddo
  350 dmu1=dmu1/z
      if (dmu1.le.delta) go to 500
      delta=dmu1
      ncs=i
      zm=dmu
  500 continue
      if (ncs.eq.0) return
      if (delta.le.-prop*dga)ncs=0
      return
  900 do 910 i=1,n
      ibi=ibloc(i)
      if (ibi.eq.0) go to 910
      dmu=g(i)
      if (ibi.eq.-1.and.dmu.ge.0.d0) go to 910
      if (ibi.eq.1.and.dmu.le.0.d0) go to 910
      dmu=dabs(dmu)*dxmin(i)
      if (dmu.le.zm) go to 910
      zm=dmu
      ncs=i
  910 continue
      if (zm.le.acc) ncs=0
      return
      end subroutine fcomp1
      subroutine n2qn1_init_diag (n, nr, h, diag)
      implicit none
      integer n, nr
      double precision h(*), diag(n)
      integer i, k, n1, nr1
      double precision u
      n1 = n+1
      nr1 = nr+1
      if (nr.gt.0) then
      k = 1
      do i = 1,nr
      h(k) = diag(i)
      k = k+nr1-i
      enddo
      endif
      if (nr.lt.n) then
      k = nr*nr1/2 + nr*(n-nr) + 1
      do i = nr1,n
      h(k) = diag(i)
      k = k+n1-i
      enddo
      endif
      return
      end subroutine n2qn1_init_diag
      subroutine n2qn1_mult_diagh (n, nr, h, r)
      implicit none
      integer n, nr
      double precision h(*), r
      integer i, k, n1, nr1
      double precision u
      n1 = n+1
      nr1 = nr+1
      if (nr.gt.0) then
      k = 1
      do i = 1,nr
      h(k) = h(k)*r
      k = k+nr1-i
      enddo
      endif
      if (nr.lt.n) then
      k = nr*nr1/2 + nr*(n-nr) + 1
      do i = nr1,n
      h(k) = h(k)*r
      k = k+n1-i
      enddo
      endif
      return
      end subroutine n2qn1_mult_diagh
      subroutine fmc11z (a,n,nr,z,sig,w,ir,mk,eps)
      implicit double precision (a-h,o-z)
      integer :: nr,n,mk,ir,nr1,i,j,nh
      double precision :: eps,z,a,w,sig
      dimension a(*),z(n),w(n)
      if (nr.eq.n) go to 45
      nr1=nr+1
      nh=nr*(nr1)/2+1
      if (nr.ne.0) then
      do i=1,nr
      do j=nr1,n
      a(nh)=a(nh)+sig*z(i)*z(j)
      nh=nh+1
      enddo
      enddo
      endif
      do j=nr1,n
      do i=j,n
      a(nh)=a(nh)+sig*z(i)*z(j)
      nh=nh+1
      enddo
      enddo
      if (nr.eq.0) return
   45 call fmc11a (a,nr,z,sig,w,ir,mk,eps)
      return
      end subroutine fmc11z
      subroutine fajc1(n,nc,nr,h,w,indi)
      implicit double precision (a-h,o-z)
      integer :: indi,nr,nc,n,ik,ij,nh,nkk,nh1,nsaut,j,i,nr2,incm1,nw,nj,inc1
      integer :: nkkmj,nk,ko,kom1,k,ii,nrr,inc,nr1
      double precision :: h,w
      dimension h(*),w(n),indi(n)
      inc=indi(nc)
      nr1=nr+1
      nr2=nr-1
      nrr=n-nr
      nkk=nr-inc
      do 260 i=1,nr
      ik=i
      ij=inc
      ii=1
      ko=min0(ik,inc)
      v=0.d0
      if (ko.eq.1) go to 252
      kom1=ko-1
      do 250 k=1,kom1
      nk=nr1-k
      v=v+h(ij)*h(ik)*h(ii)
      ij=ij+nk-1
      ii=ii+nk
      ik=ik+nk-1
  250 continue
  252 a=1
      b=1
      if (ko.eq.i) go to 253
      a=h(ik)
  253 if (ko.eq.inc) go to 259
      b=h(ij)
  259 w(i)=v+a*b*h(ii)
  260 continue
      if (inc.eq.nr) go to 315
      inc1=inc-1
      nh=inc1*nr1-inc1*inc/2+2
      nh1=nh+nkk
      di=h(nh-1)
      do 310 j=1,nkk
      di1=h(nh1)
      nh1=nh1+1
      a=h(nh)
      ai=a*di
      c=(a**2)*di+di1
      h(nh)=c
      nh=nh+1
      if (j.eq.nkk) go to 315
      nkkmj=nkk-j
      do 300 i=1,nkkmj
      h1=h(nh)
      h2=h(nh1)
      u=ai*h1+h2*di1
      h(nh)=u/c
      h(nh1)=-h1+a*h2
      nh=nh+1
      nh1=nh1+1
  300 continue
      nh=nh+1
      di=di*di1/c
  310 continue
  315 nh=inc+1
      nsaut=1
      nj=nr-2
      if (inc.eq.1) nj=nj+1
      if (nr.eq.1) go to 440
      do 430 i=1,nr2
      do 425 j=1,nj
      h(nh-nsaut)=h(nh)
      nh=nh+1
  425 continue
      nsaut=nsaut+1
      nh=nh+1
      if (i.eq.inc-1) go to 430
      nj=nj-1
      if (nj.eq.0) go to 440
  430 continue
  440 nh=((nr*nr2)/2)+1
      nw=1
      nsaut=nr
      if (inc.eq.1) go to 470
      incm1=inc-1
      do 460 i=1,incm1
      h(nh)=w(nw)
      nw=nw+1
      nsaut=nsaut-1
      if (n.eq.nr) go to 455
      do 450 j=1,nrr
      h(nh+j)=h(nh+nsaut+j)
  450 continue
  455 nh=nh+nrr+1
  460 continue
  470 nw=nw+1
      if (nr.eq.n) go to 485
      do 480 i=1,nrr
      w(nr+i)=h(nh+nsaut+i-1)
  480 continue
      nsaut=nsaut+nrr
  485 if (inc.eq.nr) go to 510
      do 500 i=1,nkk
      nsaut=nsaut-1
      h(nh)=w(nw)
      nw=nw+1
      if (nr.eq.n) go to 495
      do 490 j=1,nrr
      h(nh+j)=h(nh+nsaut+j)
  490 continue
  495 nh=nh+nrr+1
  500 continue
  510 h(nh)=w(inc)
      if (nr.eq.n) go to 540
      do 520 i=1,nrr
  520 h(nh+i)=w(nr+i)
  540 do 550 i=1,n
      ii=indi(i)
      if (ii.le.inc.or.ii.gt.nr) go to 550
      indi(i)=ii-1
  550 continue
      indi(nc)=nr
      nr=nr-1
      return
      end subroutine fajc1
      subroutine fretc1(mode,n,nc,nr,h,w,indi,indic2)
      implicit double precision (a-h,o-z)
      integer :: n,indi,mode,nr,nc,indic2,nh,nw,nr1,ij,nr2,nii,nrr,nsaut,inc,incmr,nl
      integer :: i,nrm1,i1,j,nmr1,ii,nh1,nr1p1
      double precision :: h,w
      dimension h(*),w(n),indi(n)
      inc=indi(nc)
      nr1=nr+1
      nr2=nr-1
      nrr=n-nr
      nii=n-inc
      incmr=inc-nr1
      nsaut=nii+1
      nh=inc*(n+1)-inc*(inc+1)/2
      nw=n
      if (inc.eq.n) go to 20
      do 10 i=1,nii
      w(nw)=h(nh)
      nw=nw-1
   10 nh=nh-1
   20 w(nr1)=h(nh)
      nh=nh-1
      if (inc.eq.nr1) go to 60
      do 40 i=1,incmr
      nl=nii+i-1
      if (nl.eq.0) go to 35
      do 30 j=1,nl
      h(nh+nsaut)=h(nh)
   30 nh=nh-1
   35 w(nw)=h(nh)
      nw=nw-1
      nh=nh-1
   40 nsaut=nsaut+1
      do 50 j=1,incmr
      h(nh+nsaut)=h(nh)
   50 nh=nh-1
   60 nw=nw-1
      nsaut=1
      if (nr.eq.0) go to 125
      if (inc.eq.n) go to 80
      do 70 i=1,nii
      h(nh+nsaut)=h(nh)
   70 nh=nh-1
   80 if (nr.eq.1) go to 110
      do 100 i=1,nr2
      w(nw)=h(nh)
      nw=nw-1
      nh=nh-1
      nsaut=nsaut+1
      if (n.eq.nr1) go to 100
      nrm1=n-nr1
      do 90 j=1,nrm1
      h(nh+nsaut)=h(nh)
   90 nh=nh-1
  100 continue
  110 w(nw)=h(nh)
      nh=nh-1
      nsaut=nsaut+1
      if (inc.eq.nr1) go to 125
      incmr=inc-nr1
      do 120 i=1,incmr
      h(nh+nsaut)=h(nh)
  120 nh=nh-1
  125 if (nr.ne.0) go to 130
      if (w(1).gt.0.d0) go to 220
      mode=-1
      return
  130 if (nr.eq.1) go to 160
      do 150 i=2,nr
      ij=i
      i1=i-1
      v=w(i)
      do 140 j=1,i1
      v=v-h(ij)*w(j)
  140 ij=ij+nr-j
  150 w(i)=v
  160 ij=1
      v=w(nr1)
      do 170 i=1,nr
      wi=w(i)
      hij=h(ij)
      v=v-(wi**2)/hij
      w(i)=wi/hij
  170 ij=ij+nr1-i
      if (v.gt.0.d0) go to 180
      mode=-1
      return
  180 w(nr1)=v
      if (indic2.ne.1) go to 190
      do 185 i=1,nr
  185 w(i)=0.d0
      if (n.eq.nr1) go to 190
      nr1p1=nr1+1
      do 187 i=nr1p1,n
  187 w(i)=0.d0
  190 nh=nr*(nr+1)/2
      nw=nr1
      nsaut=nw
      h(nh+nsaut)=w(nw)
      nw=nw-1
      nsaut=nsaut-1
      if (nr.eq.1) go to 220
      do 210 i=1,nr2
      h(nh+nsaut)=w(nw)
      nw=nw-1
      nsaut=nsaut-1
      do 200 j=1,i
      h(nh+nsaut)=h(nh)
  200 nh=nh-1
  210 continue
  220 h(nr1)=w(1)
      if (n.eq.nr1) go to 233
      nh1=nr*(n+1)-nr*(nr+1)/2+1
      nw=nr1
      nmr1=n-nr1
      do 230 i=1,nmr1
  230 h(nh1+i)=w(nw+i)
  233 do 235 i=1,n
      ii=indi(i)
      if (ii.le.nr.or.ii.ge.inc) go to 235
      indi(i)=ii+1
  235 continue
      nr=nr+1
      indi(nc)=nr
      mode=0
      return
      end subroutine fretc1
      subroutine fmani1 (mode, n, d, w, indi)
      implicit none
      integer mode, n, indi(n)
      double precision d(n), w(n)
      integer i
      if (mode.eq.-1) then
      do i = 1,n
      w(i) = d(indi(i))
      enddo
      else
      do i = 1,n
      w(indi(i)) = d(i)
      enddo
      endif
      return
      end subroutine fmani1
      subroutine fmlag1 (n,nr,a,z,w)
      implicit double precision (a-h,o-z)
      integer :: nr,n
      double precision :: z,a,w
      dimension a(*),z(n),w(n)
      integer i, j, nr1, nrr, nh1, nh, nj
      double precision u
      if (nr.eq.n) return
      if (nr.eq.0) then
      do i = 1,n
      w(i) = 0.d0
      enddo
      return
      endif
      nr1 = nr+1
      nrr = n-nr
      nh1 = nr*nr1/2
      nh = nh1+1
      do j = nr1,n
      u = 0.d0
      nj = nh
      do i = 1,nr
      u = u+a(nj)*z(i)
      nj = nj+nrr
      enddo
      nh = nh+1
      w(j) = u
      enddo
      return
      end subroutine fmlag1
      subroutine nlis0 (n,simul,prosca,xn,fn,fpn,t,tmin,tmax,d,g,&
      amd,amf,imp,io,logic,nap,napmax,x,izs,rzs,dzs)
      external simul,prosca
      integer n,imp,io,logic,nap,napmax,izs(*)
      real rzs(*)
      double precision xn(n),fn,fpn,t,tmin,tmax,d(n),g(n),amd,amf,x(n)
      double precision dzs(*)
      integer i,indic,indica,indicd
      double precision tesf,tesd,tg,fg,fpg,td,ta,fa,fpa,d2,f,fp,ffn,fd,&
      fpd,z,z1,test
 1000 format (5x,"nlis0   ",4x,"fpn=",1pd10.3," d2=",d9.2,&
      "  tmin=",d9.2," tmax=",d9.2)
 1001 format (/5x,"nlis0",3x,"fin sur tmin",8x,&
      "pas",12x,"fonctions",5x,"derivees")
 1002 format (5x,"nlis0",37x,1pd10.3,2d11.3)
 1003 format (5x,"nlis0",1pd14.3,2d11.3)
 1004 format (5x,"nlis0",37x,1pd10.3," indic=",i3)
 1005 format (5x,"nlis0",14x,1pd18.8,d18.8,d11.3)
 1006 format (5x,"nlis0",14x,1pd18.8,"      indic=",i3)
 1008 format (/5x,"nlis0",10x,"appel incoherent")
      if (n.gt.0 .and. fpn.lt.0.d+0 .and. t.gt.0.d+0&
      .and. tmax.gt.0.d+0 .and. amf.gt.0.d+0&
      .and. amd.gt.amf .and. amd.lt.1.d+0) go to 5
      logic=2
      go to 999
    5 tesf=amf*fpn
      tesd=amd*fpn
      td=0.d+0
      tg=0.d+0
      fg=fn
      fpg=fpn
      ta=0.d+0
      fa=fn
      fpa=fpn
      call prosca (n,d,d,d2,izs,rzs,dzs)
      if (t.lt.tmin) then
      t=tmin
      if (t.gt.tmax) then
      if (imp.gt.0) write (io,'(/5x,a,10x,a,1pd12.5,a,d12.5)')&
      "nlis0","tmin = ",tmin," force a tmax = ",tmax
      tmin=tmax
      endif
      endif
   20 if (fn+t*fpn.lt.fn+0.9d+0*t*fpn) go to 30
      t=2.d+0*t
      go to 20
   30 indica=1
      logic=0
      if (t.gt.tmax) then
      t=tmax
      logic=1
      endif
      if (imp.ge.4) write (io,1000) fpn,d2,tmin,tmax
      do 50 i=1,n
      x(i)=xn(i)+t*d(i)
   50 continue
  100 nap=nap+1
      if (nap.gt.napmax) then
      logic=4
      fn=fg
      do 120 i=1,n
      xn(i)=xn(i)+tg*d(i)
  120 continue
      go to 999
      endif
      indic=4
      call simul (indic,n,x,f,g,izs,rzs,dzs)
      if (indic.eq.0) then
      logic=5
      if (imp.ge.3) write (io,'(/a)')&
      " >>> n2qn1: stop required by the simulator"
      fn=f
      do 170 i=1,n
      xn(i)=x(i)
  170 continue
      go to 999
      endif
      if (indic.lt.0) then
      td=t
      indicd=indic
      logic=0
      if (imp.ge.4) write (io,1004) t,indic
      t=tg+0.1d+0*(td-tg)
      go to 905
      endif
      call prosca (n,d,g,fp,izs,rzs,dzs)
      ffn=f-fn
      if (ffn.gt.t*tesf) then
      td=t
      fd=f
      fpd=fp
      indicd=indic
      logic=0
      if (imp.ge.4) write (io,1002) t,ffn,fp
      go to 500
      endif
      if (imp.ge.4) write (io,1003) t,ffn,fp
      if (fp.gt.tesd) then
      logic=0
      go to 320
      endif
      if (logic.eq.0) go to 350
  320 fn=f
      do 330 i=1,n
      xn(i)=x(i)
  330 continue
      go to 999
  350 tg=t
      fg=f
      fpg=fp
      if (td.ne.0.d+0) go to 500
      ta=t
      t=9.d+0*tg
      z=fpn+3.d+0*fp-4.d+0*ffn/tg
      if (z.gt.0.d+0) t=dmin1(t,tg*dmax1(1.d+0,-fp/z))
      t=tg+t
      if (t.lt.tmax) go to 900
      logic=1
      t=tmax
      go to 900
  500 if (indica.le.0) then
      ta=t
      t=0.9d+0*tg+0.1d+0*td
      go to 900
      endif
      z=fp+fpa-3.d+0*(fa-f)/(ta-t)
      z1=z*z-fp*fpa
      if (z1.lt.0.d+0) then
      ta=t
      t=0.5d+0*(td+tg)
      go to 900
      endif
      if (t.lt.ta) z1=z-dsqrt(z1)
      if (t.gt.ta) z1=z+dsqrt(z1)
      z=fp/(fp+z1)
      z=t+z*(ta-t)
      ta=t
      test=0.1d+0*(td-tg)
      t=dmax1(z,tg+test)
      t=dmin1(t,td-test)
  900 fa=f
      fpa=fp
  905 indica=indic
      if (td.eq.0.d+0) go to 950
      if (td-tg.lt.tmin) go to 920
      do 910 i=1,n
      z=xn(i)+t*d(i)
      if (z.ne.xn(i).and.z.ne.x(i)) go to 950
  910 continue
  920 logic=6
      if (indicd.lt.0) logic=indicd
      if (tg.eq.0.d+0) go to 940
      fn=fg
      do 930 i=1,n
  930 xn(i)=xn(i)+tg*d(i)
  940 if (imp.le.0) go to 999
      write (io,1001)
      write (io,1005) tg,fg,fpg
      if (logic.eq.6) write (io,1005) td,fd,fpd
      if (logic.eq.7) write (io,1006) td,indicd
      go to 999
  950 do 960 i=1,n
  960 x(i)=xn(i)+t*d(i)
      go to 100
  999 return
      end subroutine nlis0
      subroutine fcube(t,f,fp,ta,fa,fpa,tlower,tupper)
      implicit double precision (a-h,o-z)
      z1=fp+fpa-3.d0*(fa-f)/(ta-t)
      b=z1+fp
      if (dabs(z1).le.1.d0) then
      discri=z1*z1-fp*fpa
      else
      discri=fp/z1
      discri=discri*fpa
      discri=z1-discri
      if (z1.ge.0.d0 .and. discri.ge.0.d0) then
      discri=dsqrt(z1)*dsqrt(discri)
      go to 200
      endif
      if (z1.le.0.d0 .and. discri.le.0.d0) then
      discri=dsqrt(-z1)*dsqrt(-discri)
      go to 200
      endif
      discri=-1.d0
      endif
      if (discri.lt.0.d0) then
      if (fp.lt.0.d0) t=tupper
      if (fp.ge.0.d0) t=tlower
      go to 990
      endif
      discri=dsqrt(discri)
  200 if (t-ta.lt.0.d0) discri=-discri
      sign=(t-ta)/dabs(t-ta)
      if (b*sign.gt.0.) then
      anum=(ta-t)*fp
      den=b+discri
      else
      den=z1+b+fpa
      anum=(ta-t)*(b-discri)
      endif
      if (dabs(den).ge.1.d0) then
      t=t+anum/den
      else
      if (dabs(anum).lt.(tupper-tlower)*dabs(den)) then
      t=t+anum/den
      else
      if (fp.lt.0.d0) t=tupper
      if (fp.ge.0.d0) t=tlower
      endif
      endif
      t=dmax1(t,tlower)
      t=dmin1(t,tupper)
  990 return
      end subroutine fcube
      subroutine fuclid (n,x,y,ps,izs,rzs,dzs)
      implicit real*8 (a-h,o-z)
      integer :: n,izs(*),i
      double precision x(n),y(n),dzs(*)
      real rzs(*)
      ps=0.d0
      do 10 i=1,n
   10 ps=ps+x(i)*y(i)
      return
      end subroutine fuclid
      subroutine fmc11a(a,n,z,sig,w,ir,mk,eps)
      implicit double precision (a-h,o-z)
      integer :: ir,mk,n,ij,np,i,ip,mm,j
      double precision a(*),z(n),w(n)
      if (n.le.1) then
      a(1)=a(1)+sig *z(1)**2
      ir=1
      if (a(1).gt.0.d0)return
      a(1)=0.d0
      ir=0
      return
      endif
      np=n+1
      if (sig.gt.0.d0)goto 40
      if (sig.eq.0.d0.or.ir.eq.0)return
      ti=1.d0/sig
      ij=1
      if (mk.eq.0)goto 10
      do 7 i=1,n
      if (a(ij).ne.0.d0)ti=ti+w(i)**2/a(ij)
    7 ij=ij+np-i
      goto 20
   10 continue
      do 11 i=1,n
   11 w(i)=z(i)
      do 15 i=1,n
      ip=i+1
      v=w(i)
      if (a(ij).gt.0.d0)goto 12
      w(i)=0.d0
      ij=ij+np-i
      goto 15
   12 continue
      ti=ti+v**2/a(ij)
      if (i.eq.n)goto 14
      do 13 j=ip,n
      ij=ij+1
   13 w(j)=w(j)-v*a(ij)
   14 ij=ij+1
   15 continue
   20 continue
      if (ir.le.0 )goto 21
      if (ti.gt.0.d0)goto 22
      if (mk-1)40,40,23
   21 ti=0.d0
      ir=-ir-1
      goto 23
   22 ti=eps/sig
      if (eps.eq.0.d0)ir=ir-1
   23 continue
      mm=1
      tim=ti
      do 30 i=1,n
      j=np-i
      ij=ij-i
      if (a(ij).ne.0.d0)tim=ti-w(j)**2/a(ij)
      w(j)=ti
   30 ti=tim
      goto 41
   40 continue
      mm=0
      tim=1.d0/sig
   41 continue
      ij=1
      do 66 i=1,n
      ip=i+1
      v=z(i)
      if (a(ij).gt.0.d0)goto 53
      if (ir.gt.0 .or.sig.lt.0.d0.or.v.eq.0.d0)goto 52
      ir=1-ir
      a(ij)=v**2/tim
      if (i.eq.n)return
      do 51 j=ip,n
      ij=ij+1
   51 a(ij)=z(j)/v
      return
   52 continue
      ti=tim
      ij=ij+np-i
      goto 66
   53 continue
      al=v/a(ij)
      if (mm)54,54,55
   54 ti=tim+v*al
      goto 56
   55 ti=w(i)
   56 continue
      r=ti/tim
      a(ij)=a(ij)*r
      if (r.eq.0.d0)goto 70
      if (i.eq.n)goto 70
      b=al/ti
      if (r.gt.4.d0)goto 62
      do 61 j=ip,n
      ij=ij+1
      z(j)=z(j)-v*a(ij)
   61 a(ij)=a(ij)+b*z(j)
      goto 64
   62 gm=tim/ti
      do 63 j=ip,n
      ij=ij+1
      y=a(ij)
      a(ij)=b*z(j)+y*gm
   63 z(j)=z(j)-v*y
   64 continue
      tim=ti
      ij=ij+1
   66 continue
   70 continue
      if (ir.lt.0)ir=-ir
      return
      end subroutine fmc11a
      subroutine fmc11b(a,n,ir)
      implicit double precision (a-h,o-z)
      integer :: n,ir,ii,ni,np,i,ip,jk,ij,ik
      double precision a(*)
      ir=n
      if (n.gt.1)goto 100
      if (a(1).gt.0.d0)return
      a(1)=0.d0
      ir=0
      return
  100 continue
      np=n+1
      ii=1
      do 104 i=2,n
      aa=a(ii)
      ni=ii+np-i
      if (aa.gt.0.d0)goto 101
      a(ii)=0.d0
      ir=ir-1
      ii=ni+1
      goto 104
  101 continue
      ip=ii+1
      ii=ni+1
      jk=ii
      do 103 ij=ip,ni
      v=a(ij)/aa
      do 102 ik=ij,ni
      a(jk)=a(jk)-a(ik)*v
  102 jk=jk+1
  103 a(ij)=v
  104 continue
      if (a(ii).gt.0.d0)return
      a(ii)=0.d0
      ir=ir-1
      return
      end subroutine fmc11b
      subroutine fmc11e(a,n,z,w,ir)
      implicit double precision (a-h,o-z)
      integer :: n,ir,i,j,ij,nip,np,i1,ip,ii
      double precision a(*),z(n),w(n)
      if (ir.lt.n) return
      w(1)=z(1)
      if (n.le.1) then
      z(1)=z(1)/a(1)
      return
      endif
      do i=2,n
      ij=i
      i1=i-1
      v=z(i)
      do j=1,i1
      v=v-a(ij)*z(j)
      ij=ij+n-j
      enddo
      w(i)=v
      z(i)=v
      enddo
      z(n)=z(n)/a(ij)
      np=n+1
      do nip=2,n
      i=np-nip
      ii=ij-nip
      v=z(i)/a(ii)
      ip=i+1
      ij=ii
      do j=ip,n
      ii=ii+1
      v=v-a(ii)*z(j)
      enddo
      z(i)=v
      enddo
      return
      end subroutine fmc11e
      subroutine nqhess(n,imp,lp,iz,rz)
      implicit double precision(a-h,o-z)
      integer :: lp,iz(*),imp,ni,i,n,nw,nw1,nr,nc,nj,i1,j1,j,n1,k
      double precision rz(*)
 1000 format(//)
 1001 format(34h nqhess   hessienne au point final)
 1002 format(9h   nqhess,i4,5d12.4,/,(9h   nqhess,4x,5d12.4))
      ni=2*n
      nw=n*(n+1)/2
      nw1=nw+n
      if (n.eq.1) go to 50
      nr=iz(ni+1)
      if (nr.eq.0) go to 20
      do 10 i=1,n
      if (iz(n+i).ne.0) go to 10
      nc=i
      call fajc1 (n,nc,nr,rz(1),rz(nw+1),iz(1))
      if (nr.eq.0) go to 20
   10 continue
   20 n1=n-1
      do 40 i=1,n1
      j1=iz(i)
      if (j1.eq.i) go to 40
      ni=i
      nj=j1
      call f1qhes (n,ni,nj,nw,rz)
      call f1qhes (n,nj,ni,nw1,rz)
      call f2qhes (n,nj,nw,rz)
      call f2qhes (n,ni,nw1,rz)
      if (i.eq.n1) go to 50
      i1=i+1
      do 30 k=i1,n
      if (iz(k).ne.i) go to 30
      iz(k)=j1
      go to 40
   30 continue
   40 continue
   50 if (imp.le.0) return
      write(lp,1000)
      write(lp,1001)
      do 60 i=1,n
      write(lp,1002) i,(rz(i+(j-1)*(2*n-j)/2),j=1,i)
   60 continue
      return
      end subroutine nqhess
      subroutine f1qhes(n,ni,nj,nw,rz)
      implicit double precision(a-h,o-z)
      integer :: nj,nw,ni,n,nii,nk,nwi,nwj,k
      double precision rz(*)
      nii=ni
      nwi=nw+ni
      nwj=nw+nj
      do 20 k=1,n
      rz(nw+k)=rz(nii)
      if (k.ge.ni) go to 10
      nii=nii+(n-k)
      go to 20
   10 nii=nii+1
   20 continue
      rznw=rz(nwi)
      rz(nwi)=rz(nwj)
      rz(nwj)=rznw
      return
      end subroutine f1qhes
      subroutine f2qhes(n,ni,nw,rz)
      implicit double precision(a-h,o-z)
      integer :: n,ni,nw,k,nii
      double precision rz(*)
      nii=ni
      do 20 k=1,n
      rz(nii)=rz(nw+k)
      if (k.ge.ni) go to 10
      nii=nii+(n-k)
      go to 20
   10 nii=nii+1
   20 continue
      return
      end subroutine f2qhes
