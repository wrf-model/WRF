subroutine seaem(freqghz,zasat,zlsat,ts5,u10,v10,ehorz,evert)
! 
!------------------------------------------------------------------------------
!  PURPOSE: Calculate microwave surface emissivity over sea.
!
!  METHOD:  adopted from GSI code emiss
!       out V and H polarized emissivitys for used in RTTOV
!
!  HISTORY: 03/10/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------
!   input argument list:
!     freqghz  - microwave freqency
!     zasat    - local satellite zenith angle in radians
!     zlsat    - satellite look angle in radians (not used)
!     ts5      - skin temperature
!     u10      - 10m u wind 
!     v10      - 10m v wind 
!
!   output argument list:
!     ehorz    - horizontal polarization emissivity 
!     evert    - vertical polarization emissivity
!
! ...............................................................

  implicit none

! Declare passed variables.
  real(r_kind), intent(in):: freqghz
  real(r_kind), intent(in):: ts5
  real(r_kind), intent(in):: zasat,zlsat,u10,v10
  real(r_kind),intent(out):: ehorz, evert
  real(r_kind),dimension(59):: emc 

! Declare local variables
  integer(i_kind) kcho,n,kch,nn,nnp,i
  integer(i_kind) error_status
  integer(i_kind) quiet
!  integer(i_kind),dimension(nchan)::indx

  real(r_kind) zch4,xcorr2v,evertr,ehorzr,xcorr2h,ffoam,zcv2,zcv3
  real(r_kind) xcorr1,zcv1,zcv4,zch1,zch2,zcv5,zcv6,tau2,degre
  real(r_kind) wind,sec,sec2,freqghz2,dtde
  real(r_kind) u10mps2,usec,tccub,tau1,tc,tcsq,term2
  real(r_kind) term1,u10mps,ps2,pc2,pcc,pss,rvertsi,rverts,rvertsr
  real(r_kind) rverts5,rhorzs5,xcorr15,ffoam5,evertr5,ehorzr5
  real(r_kind) perm_real,perm_imag,rhorzsr,zch5,zch6,zch3,rhorzsi
  real(r_kind) rhorzs,perm_imag2,einf,fen,del2,del1,fen2,perm_real2
  real(r_kind) perm_imag1,perm_real1,den1,den2

  complex(r_kind) perm1,perm2,rvth,rhth,xperm

!  integer     :: ipolar(nchan)
!  real        :: polar(nchan)
  
!
!  Start emiss here
!
!  uuk=zero
!  vvk=zero

! Explanation for emc :
! emc(59): Emissivity model data
! Permittivity model data (Lamkaouchi model)
!   [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1996)
!   [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1996)
!  [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1996)
! [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1996)
! [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1996)
! [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1996)
! Pi is stored for good measure
!    [20]: Stored value of Pi  
! Bragg scattering correction coefficients
!    [21]: Scaling factor for small scale correction - see English (1997)
! Foam model coefficients for Monahan model
!    [22]: First coefficient in Monahan foam model (neutral stability)  - see English (1997)
!    [23]: Second coefficient in Monahan foam model (neutral stability) - see English (1997)
! Alternative permittivity model (Liebe)
!    [30]: a1 in Liebe's dielectric model - see Liebe (1989)
!    [31]: b1 in Liebe's dielectric model - see Liebe (1989)
!    [32]: c1 in Liebe's dielectric model - see Liebe (1989)
!    [33]: c2 in Liebe's dielectric model - see Liebe (1989)
!    [34]: d1 in Liebe's dielectric model - see Liebe (1989)
!    [35]: d2 in Liebe's dielectric model - see Liebe (1989)
!    [36]: d3 in Liebe's dielectric model - see Liebe (1989)
!    [37]: e1 in Liebe's dielectric model - see Liebe (1989)
!    [38]: e2 in Liebe's dielectric model - see Liebe (1989)
! Version 2 of large scale correction which *DOESÅª* take account of
! hemispherical scattering.
! 1.) Vertical polarisation mode
!    [24]: Term a00 in vertical pol of large scale correction model
!    [25]: Term a01 in vertical pol mode of large scale correction model
!    [26]: Term a02 in vertical pol mode of large scale correction model
!    [27]: Term a10 in vertical pol mode of large scale correction model
!    [28]: Term a11 in vertical pol mode of large scale correction model
!    [29]: Term a12 in vertical pol mode of large scale correction model
!    [30]: Term a20 in vertical pol mode of large scale correction model
!    [31]: Term a21 in vertical pol mode of large scale correction model
!    [32]: Term a22 in vertical pol mode of large scale correction model
!    [33]: Term a30 in vertical pol mode of large scale correction model
!    [34]: Term a31 in vertical pol mode of large scale correction model
!    [35]: Term a32 in vertical pol mode of large scale correction model
!    [36]: Term a40 in vertical pol mode of large scale correction model
!    [37]: Term a41 in vertical pol mode of large scale correction model
!    [38]: Term a42 in vertical pol mode of large scale correction model
!    [39]: Term a50 in vertical pol mode of large scale correction model
!    [40]: Term a51 in vertical pol mode of large scale correction model
!    [41]: Term a52 in vertical pol mode of large scale correction model
! 2. ) Horizontal polarisation mode
!    [42]: Term a00 in horizontal pol mode of large scale correction model
!    [43]: Term a01 in horizontal pol mode of large scale correction model
!    [44]: Term a02 in horizontal pol mode of large scale correction model
!    [45]: Term a10 in horizontal pol mode of large scale correction model
!    [46]: Term a11 in horizontal pol mode of large scale correction model
!    [47]: Term a12 in horizontal pol mode of large scale correction model
!    [48]: Term a20 in horizontal pol mode of large scale correction model
!    [49]: Term a21 in horizontal pol mode of large scale correction model
!    [50]: Term a22 in horizontal pol mode of large scale correction model
!    [51]: Term a30 in horizontal pol mode of large scale correction model
!    [52]: Term a31 in horizontal pol mode of large scale correction model
!    [53]: Term a32 in horizontal pol mode of large scale correction model
!    [54]: Term a40 in horizontal pol mode of large scale correction model
!    [55]: Term a41 in horizontal pol mode of large scale correction model
!    [56]: Term a42 in horizontal pol mode of large scale correction model
!    [57]: Term a50 in horizontal pol mode of large scale correction model
!    [58]: Term a51 in horizontal pol mode of large scale correction model
!    [59]: Term a52 in horizontal pol mode of large scale correction model

   emc = (/&
       0.175350E+02_r_kind, -.617670E+00_r_kind,  .894800E-02_r_kind,  .318420E+01_r_kind,&
       0.191890E-01_r_kind, -.108730E-01_r_kind,  .258180E-03_r_kind,  .683960E+02_r_kind,&
       -.406430E+00_r_kind,  .228320E-01_r_kind, -.530610E-03_r_kind,  .476290E+01_r_kind,&
       0.154100E+00_r_kind, -.337170E-01_r_kind,  .844280E-03_r_kind,  .782870E+02_r_kind,&
       -.434630E-02_r_kind,  .531250E+01_r_kind, -.114770E-01_r_kind,  .314159E+01_r_kind,&
       -.100000E+01_r_kind,  .195000E-04_r_kind,  .255000E+01_r_kind, -.637182E+01_r_kind,&
       0.253918E-01_r_kind,  .357569E-04_r_kind,  .942928E+01_r_kind, -.332839E-01_r_kind,&
       -.647724E-04_r_kind, -.329282E+01_r_kind,  .965450E-02_r_kind,  .281588E-04_r_kind,&
       0.252676E+00_r_kind,  .343867E-02_r_kind, -.156362E-04_r_kind, -.156669E-03_r_kind,&
       0.139485E-04_r_kind, -.407633E-07_r_kind, -.141316E+00_r_kind, -.356556E-02_r_kind,&
       0.142869E-04_r_kind, -.240701E+01_r_kind, -.563888E-01_r_kind,  .325227E-03_r_kind,&
       0.296005E+01_r_kind,  .704675E-01_r_kind, -.426440E-03_r_kind, -.751252E+00_r_kind,&
       -.191934E-01_r_kind,  .125937E-03_r_kind, -.288253E+00_r_kind, -.102655E-02_r_kind,&
       0.226701E-05_r_kind, -.119072E-02_r_kind, -.263165E-04_r_kind,  .114597E-06_r_kind,&
       0.406300E+00_r_kind,  .200031E-02_r_kind, -.781635E-05_r_kind/)
     
!      ----- sea (ice-free) MW  -------
           
!          Open ocean points

!          First set constants.  Then perform the calculation.
!           wind  = f10(n)*sqrt(uu5(n)*uu5(n)+vv5(n)*vv5(n))  ! wind speed in m/s
           wind  = sqrt(u10*u10+v10*v10)  ! 10m wind speed in m/s
           u10mps  = wind
           pcc=cos(zasat)
           pss=sin(abs(zasat))
!          pcl2=cos(zlsat)**2
!          psl2=sin(zlsat)**2
           ps2=pss*pss
           pc2=pcc*pcc
           freqghz2=freqghz*freqghz
           u10mps2=u10mps*u10mps
           sec=one/pcc
           sec2=sec*sec
           usec=u10mps*sec
          
!          calculate piom (ellison et al.) xperm
!          to calculate xperm of saline water based on piom model.
!          convert from kelvin to centigrate and define quadratic and
!          cubic functions for later polynomials
           tc=ts5-t_kelvin
           tcsq=tc*tc
           tccub=tcsq*tc
           
!          define two relaxation frequencies, tau1 and tau2
           tau1=emc(1)+emc(2)*tc+emc(3)*tcsq
           tau2=emc(4)+emc(5)*tc+emc(6)*tcsq+emc(7)*tccub
           
!          static xperm estatic=del1+del2+einf
           del1=emc(8)+emc(9)*tc+emc(10)*tcsq+emc(11)*tccub
           del2=emc(12)+emc(13)*tc+emc(14)*tcsq+emc(15)*tccub
           einf=emc(18)+emc(19)*tc
            
!          calculate xperm using double-debye formula
           fen=two*pi*freqghz*0.001_r_kind
           fen2=fen**two
           den1=one+fen2*tau1*tau1
           den2=one+fen2*tau2*tau2
           perm_real1=del1/den1
           perm_real2=del2/den2
           perm_imag1=del1*fen*tau1/den1
           perm_imag2=del2*fen*tau2/den2
           perm_real=perm_real1+perm_real2+einf
           perm_imag=perm_imag1+perm_imag2
           xperm=dcmplx(perm_real,perm_imag)

!          calculate complex fresnel reflection coefficients
!          to calculate vertical and horizontal polarised reflectivities
!          given xperm at local incidencence angle for all channels
!          and profiles
           perm1 = cdsqrt(xperm - dcmplx(ps2,zero))
           perm2  = xperm*pcc
           rhth = (pcc - perm1)/(pcc + perm1)                     
           rvth = (perm2 - perm1)/(perm2 + perm1)
           rvertsr=real(rvth)
           rvertsi=imag(rvth)
           rverts=rvertsr*rvertsr+rvertsi*rvertsi
           rhorzsr=real(rhth)
           rhorzsi=imag(rhth)
           rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi

!          calculate small scale xcorr to reflection coefficients
           xcorr1=exp(emc(21)*u10mps*pc2/freqghz2)

!          calculate large scale geometric correction
!          to calculate a correction to the fresnel reflection coefficients
!          allowing for the presence of large scale roughness      

!          jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)   
           zcv1=emc(24)+emc(25)*freqghz+emc(26)*freqghz2
           zcv2=(emc(27)+emc(28)*freqghz+emc(29)*freqghz2)*sec
           zcv3=(emc(30)+emc(31)*freqghz+emc(32)*freqghz2)*sec2
           zcv4=(emc(33)+emc(34)*freqghz+emc(35)*freqghz2)*u10mps
           zcv5=(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)*u10mps2
           zcv6=(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)*usec
           zch1=emc(42)+emc(43)*freqghz+emc(44)*freqghz2
           zch2=(emc(45)+emc(46)*freqghz+emc(47)*freqghz2)*sec
           zch3=(emc(48)+emc(49)*freqghz+emc(50)*freqghz2)*sec2
           zch4=(emc(51)+emc(52)*freqghz+emc(53)*freqghz2)*u10mps
           zch5=(emc(54)+emc(55)*freqghz+emc(56)*freqghz2)*u10mps2
           zch6=(emc(57)+emc(58)*freqghz+emc(59)*freqghz2)*usec

!          calculate correction for this polarisation
           xcorr2v=.01_r_kind*(zcv1+zcv2+zcv3+zcv4+zcv5+zcv6)
           xcorr2h=.01_r_kind*(zch1+zch2+zch3+zch4+zch5+zch6)
           
           evertr=one-rverts*xcorr1+xcorr2v
           ehorzr=one-rhorzs*xcorr1+xcorr2h

!          calculate foam emissivity correction
           ffoam=emc(22)*(u10mps**emc(23))
           evert=evertr - ffoam*evertr+ ffoam
           ehorz=ehorzr - ffoam*ehorzr + ffoam

!           rverts5 = rverts
!           rhorzs5 = rhorzs
!           xcorr15 = xcorr1
!           ffoam5 = ffoam
!           evertr5 = evertr
!           ehorzr5 = ehorzr


!          Combine horizontal and vertical polarizations.
!           call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )

!          Begin K matrix calculation

!          Combine horizontal and vertical polarizations.
!           dtde=one
!           call adm_ehv2pem(zlsat(n),polar(kch),dtde, ehorz,evert )

!          calculate corrected emissivity from corrected refectivity
!           ehorzr=ehorz - ffoam5*ehorz
!           ffoam =-ehorz*ehorzr5 + ehorz
!           evertr=evert - ffoam5*evert
!           ffoam =ffoam-evert*evertr5 + evert

!          calculate corrected emissivity from corrected refectivity
!           rhorzs = -ehorzr*xcorr15
!           xcorr1 = -rhorzs5*ehorzr
!           xcorr2h = ehorzr
!           rverts = -evertr*xcorr15
!           xcorr1 = xcorr1 - rverts5*evertr
!           xcorr2v = evertr

!          calculate foam emissivity correction
!          calculate correction for this polarisation
!           zch4=.01_r_kind*xcorr2h
!           zch5=.01_r_kind*xcorr2h
!           zch6=.01_r_kind*xcorr2h
!           zcv4=.01_r_kind*xcorr2v
!           zcv5=.01_r_kind*xcorr2v
!           zcv6=.01_r_kind*xcorr2v

!          calculate large scale geometric correction
!          to calculate a correction to the fresnel reflection coefficients
!          allowing for the presence of large scale roughness
!
!          jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)
!           u10mps = emc(23)*ffoam5/wind*ffoam +                       &
!                    zch4*(emc(51)+emc(52)*freqghz+emc(53)*freqghz2) + &
!                    zcv4*(emc(33)+emc(34)*freqghz+emc(35)*freqghz2) + &
!                    xcorr1*emc(21)*pc2/freqghz2*xcorr15

!           usec=zch6*(emc(57)+emc(58)*freqghz+emc(59)*freqghz2) + &
!                zcv6*(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)
!           u10mps2=zch5*(emc(54)+emc(55)*freqghz+emc(56)*freqghz2) + &
!                   zcv5*(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)
!          calculate small scale xcorr to reflection coefficients
!          the following lines are commented out because a warning will
!          be printed from dcalmkaouchi if freqghz<10.

!           u10mps  = u10mps+usec*sec+u10mps2*two*wind
!           u10mps  = f10(n)*f10(n)/wind*u10mps
!           uuk(nn) = uu5(n)*u10mps
!           vvk(nn) = vv5(n)*u10mps


!        Load emissivity into array for radiative transfer model
!       (pems5) and diagnostic output file (emissav).
        
!        pems5(nn)       = max(zero,min(pems5(nn),one))

! End of routine.
  return

end subroutine seaem
