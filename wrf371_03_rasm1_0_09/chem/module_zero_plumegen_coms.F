!
Module module_zero_plumegen_coms
implicit none
integer, parameter :: nkp = 200, ntime = 200
!
real,dimension(nkp) ::  w,t,qv,qc,qh,qi,sc,  &  ! blob
                        vth,vti,rho,txs,  &
                        est,qsat,qpas,qtotal

real,dimension(nkp) ::  wc,wt,tt,qvt,qct,qht,qit,sct
real,dimension(nkp) ::  dzm,dzt,zm,zt,vctr1,vctr2 &
                       ,vt3dc,vt3df,vt3dk,vt3dg,scr1

!
real,dimension(nkp) ::  pke,the,thve,thee,pe,te,qvenv,rhe,dne,sce ! environment at plume grid
real,dimension(nkp) ::  ucon,vcon,wcon,thtcon ,rvcon,picon,tmpcon,dncon,prcon &
                       ,zcon,zzcon,scon ! environment at RAMS  grid

!
real :: DZ,DQSDZ,VISC(nkp),VISCOSITY,TSTPF   
integer :: N,NM1,L
!
real :: ADVW,ADVT,ADVV,ADVC,ADVH,ADVI,CVH(nkp),CVI(nkp),ADIABAT,&
        WBAR,ALAST(10),VHREL,VIREL  ! advection
!
real :: ZSURF,ZBASE,ZTOP
integer :: LBASE
!
real :: AREA,RSURF,ALPHA,RADIUS(nkp)  ! entrain
!
real :: HEATING(ntime),FMOIST,BLOAD   ! heating
!
real :: DT,TIME,TDUR
integer :: MINTIME,MDUR,MAXTIME
!
REAL,    DIMENSION(nkp,2)    :: W_VMD,VMD
REAL	:: upe   (nkp)
REAL	:: vpe   (nkp)
REAL	:: vel_e (nkp)

REAL	:: vel_p (nkp)
REAL	:: rad_p (nkp)
REAL	:: vel_t (nkp)
REAL	:: rad_t (nkp)

real :: ztop_(ntime)

contains
subroutine zero_plumegen_coms

w=0.0;t=0.0;qv=0.0;qc=0.0;qh=0.0;qi=0.0;sc=0.0
vth=0.0;vti=0.0;rho=0.0;txs=0.0
est=0.0;qsat=0.0;qpas=0.0;qtotal=0.0
wc=0.0;wt=0.0;tt=0.0;qvt=0.0;qct=0.0;qht=0.0;qit=0.0;sct=0.0
dzm=0.0;dzt=0.0;zm=0.0;zt=0.0;vctr1=0.0;vctr2=0.0
vt3dc=0.0;vt3df=0.0;vt3dk=0.0;vt3dg=0.0;scr1=0.0
pke=0.0;the=0.0;thve=0.0;thee=0.0;pe=0.0;te=0.0;qvenv=0.0;rhe=0.0;dne=0.0;sce=0.0 
ucon=0.0;vcon=0.0;wcon=0.0;thtcon =0.0;rvcon=0.0;picon=0.0;tmpcon=0.0;dncon=0.0;prcon=0.0 
zcon=0.0;zzcon=0.0;scon=0.0 
dz=0.0;dqsdz=0.0;visc=0.0;viscosity=0.0;tstpf=0.0
advw=0.0;advt=0.0;advv=0.0;advc=0.0;advh=0.0;advi=0.0;cvh=0.0;cvi=0.0;adiabat=0.0
wbar=0.0;alast=0.0;vhrel=0.0;virel=0.0  
zsurf=0.0;zbase=0.0;ztop=0.0;area=0.0;rsurf=0.0;alpha=0.0;radius=0.0;heating=0.0
fmoist=0.0;bload=0.0;dt=0.0;time=0.0;tdur=0.0
ztop_=0.0
upe =0.0  
vpe =0.0  
vel_e =0.0
vel_p =0.0
rad_p =0.0
vel_t =0.0
rad_t =0.0
 W_VMD=0.0
 VMD=0.0
n=0;nm1=0;l=0;lbase=0;mintime=0;mdur=0;maxtime=0
end subroutine zero_plumegen_coms
End Module
