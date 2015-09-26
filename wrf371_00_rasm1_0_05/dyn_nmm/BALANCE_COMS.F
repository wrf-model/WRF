!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  The following common blocks are used in the TC-scale model.
!  The meanings of their elements are listed below:
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  nt ----- the counting number of time steps of model integration
!  nttb---- time steps for the TKE initialization
!  ktops--- Bottom level above which spounge layer is applied
!
      integer nt,nttb,ktops,nstep
      common /contrl/nt,nttb,ktops,nstep
!
!  dtph --- fractional time steps for time integration of TKE and
!           the precipitating terms in the cloud microphysics scheme
!  tsf0---- temperature at sea surface
!  taa ---- the time scale for Newtonian cooling
!
      real dtph,tsf0,taa,epsl,vip(2:kmx,2),tvbh(kmx)
      common /cddt/dtph,tsf0,taa,epsl,vip,tvbh
!
!  sig ---- sigma at full levels (TKE, ...)
!  sig1---- sigma at half levels (u,v,t,qv, ...)
!  dsig --- thickness of the model layers between full levels
!  dsig1 -- thickness between the half levels
!  dsghl -- dsig(k-1)/dsig(k)
!
      real sig(kmx1),sig1(kmx),dsig(kmx)
      real dsig1(kmx1),dsghl(2:kmx)
      common /csig/sig,sig1,dsig,dsig1,dsghl
!
!  rsig --- 1/sig
!  rdsig -- 1/dsig
!  rdsig1 - 1/dsig1
!
      real rsig(kmx1),rdsig(kmx),rdsig1(kmx1)
      common /crsig/rsig,rdsig,rdsig1
!
!  thb ---- basic potential temperature at u,v levels
!  pib ---- Exner function of basic state at u,v levels
!  pibh --- Exner function of basic state at half levels
!
      real thb(kmx),pib(kmx),pibh(kmx1),qb(kmx)
      common /thbpib/thb,pib,pibh,qb
!
!  vp1,vp2,vp3 -- constant in vertical sound-wave equations
!
      real vp1(kmx),vp2(kmx),vp3(kmx),thvb(kmx)
      common /v123/vp1,vp2,vp3,thvb
!
!  jt0 ---- nearest integal zonal gridpoint of tropical cyclone centre
!  it0 ---- nearest integal meridional gridpoint of TC centre
!  psmin--- minimum surface pressure at TC centre
!
      integer jt0,it0
      real psmin
      common /ijt0/jt0,it0,psmin
!
!  vmm ---- maximum wind speed at lowest model level
!  prm ---- time sequence of minimum surface pressure
!  xpt ---- time sequence of TC centre in the coarse mesh domain
!  ypt ---- time sequence of TC centre in the coarse mesh domain
!  rmw ---- time sequence of radius of maximum wind
!
      real vmm(nn),prm(nn),xpt(nn),ypt(nn),rmw(nn)
      common /xytc/vmm,prm,xpt,ypt,rmw
!
!  zz --- Height of half sigma-levels
!  poz -- Height of sigma-levles
!
      real zz(kmx1),poz(kmx),aml(kmx),amh(kmx)
      common /czzp/zz,poz,aml,amh
!
! common block for sensitivity and physical process studies
!
      integer nphy,nevp,micr,ngh,ihadv,ihadq
      common /comexp1/nphy,nevp,micr,ngh,ihadv,ihadq
      integer nenv,isfclyr,idrag,ndisht
      common /cominit/nenv,isfclyr,idrag,ndisht
!
!  Ends common block section
!
