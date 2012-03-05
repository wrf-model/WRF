subroutine gsi_emiss(inst,knchpf,kprof,kchan,knchan,indexn,zasat,zlsat,&
                polar, isflg,pemsv,pemsh,pems5, ts5,soiltype5,soilt5,soilm5, &
                vegtype5,vegf5,snow5,itype,nsdata,nchan,  &
                btm,amsua,amsub,ssmi,u10,v10)
!                .      .    .                                       .
! subprogram:    emiss       compute emissivity for IR and microwave 
!   prgmmr: treadon          org: np23                date: 1998-02-20
!
! abstract: compute surface emissivity for IR and microwave channels.
!
! program history log:
!   1998-02-20  treadon - gather all emissivity calculations from 
!                         setuprad and move into this subroutine
!   2004-07-23  weng,yan,okamoto - incorporate MW land and snow/ice emissivity 
!                         models for AMSU-A/B and SSM/I
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2005-01-20  okamoto- add preprocessing for ocean MW emissivity jacobian 
!   2005-03-05  derber- add adjoint of surface emissivity to this routine
!   2005-09-25  Zhiquan Liu- adopted for wrf-var
!
!
!   input argument list:
!     inst     - wrf-var internal instrument number
!     knchpf   - total number of profiles (obs) times number of 
!                channels for each profile.
!     kprof    - profile number array
!     kchan    - channel number array
!     kochan   - old channel number array
!     knchan   - number of channels for each profile
!     indexn   - index pointing from channel,profile to location in array
!     zasat    - local satellite zenith angle in radians
!     zlsat    - satellite look angle in radians
!     isflg    - surface flag
!                0 sea
!                1 sea ice
!                2 land
!                3 snow
!                4 mixed predominately sea
!                5 mixed predominately sea ice
!                6 mixed predominately land
!                7 mixed predominately snow
!     ts5      - skin temperature
!     soiltype5- soil type
!     soilt5   - soil temperature
!     soilm5   - soil moisture (volumatic ratio 0.0 ~ 1.0)  
!     vegtype5 - vegetation type 
!     vegf5    - vegetation fraction
!     snow5    - snow depth   [mm] 
!     itype    - ir/microwave instrument type    
!     nsdata   - number of profiles              
!     nchan    - maximum number of channels for this satellite/instrument
!     kidsat   - satellite id
!     btm      - observation tb
!     amsua    - logical true if amsua is processed 
!     amsub    - logical true if amsub is processed 
!     ssmi     - logical true if ssmi  is processed 
!     u10      - 10m u wind 
!     v10      - 10m v wind 
!     f10      - factor reducing lowest sigma level to 10m wind
!
!   output argument list:
!     emissav  - same as pems5 but for diagnostic array
!     uuk      - adjoint of lowest sigma level u wind(microwave over ocean only)
!     vvk      - adjoint of lowest sigma level v wind(microwave over ocean only)
!     pems5    - surface emissivity at obs location
!
!     NOTE:  pems5 is passed through include file "prfvark3.h"
!
!   other important variables
!    polar: channel polarization (0=vertical, 1=horizontal, or
!                                  0 to 1=mix of V and H)
!
!  
! ...............................................................
!             land snow ice sea
! landem()     o   o    x   x  : for all MW but f<80GHz,lower accuracy over snow
! snwem_amsu() x   o    x   x  : for AMSU-A/B
! iceem_amsu() x   x    o   x  : for AMSU-A/B
! emiss_ssmi() x   o    o   x  : for SSM/I
!
! if(snow)
!   if(amsua .or. amsub) call snwem_amsu()
!   else if(ssmi)        call emiss_ssmi()
!   else                 call landem()
! if(land)               call landem()
! if(ice)
!   if(amsua .or. amsub) call iceem_amsu()
!   else if(ssmi)        call emiss_ssmi()
!   else                 emissivity=0.92
! if(ocean) calculate
! ..............................................................
!
! attributes:
!   language: f90
!   machine:  IBM sp
!
!$$$
!  use kinds, only: r_kind,r_single,i_kind
!  use error_handler, only: failure  ! change to local parameter
!  use irsse_model, only: forward_irsse
!  use radinfo, only: polar,jppf,newchn_ir
!  use spectral_coefficients, only: sc
!  use constants, only: zero,one,rad2deg,two,pi
  implicit none

  integer, parameter  :: FAILURE = 3, jppf = 1

! Declare passed variables.
  integer(i_kind),intent(in):: inst, knchpf,nsdata,nchan,itype
  integer(i_kind),dimension(nchan*nsdata),intent(in):: kprof,kchan
  integer(i_kind),dimension(nchan,nsdata),intent(in):: indexn
  integer(i_kind),dimension(nsdata),intent(in):: isflg,knchan
  real(r_kind),dimension(nchan,jppf), intent(in) :: btm
  real(r_kind),dimension(nsdata),intent(in):: ts5,snow5,soiltype5,&
       soilt5,soilm5,vegtype5,vegf5
  real(r_kind),dimension(nsdata),intent(in):: zasat,zlsat,u10,v10
  real , dimension(nchan),intent(in)       :: polar
!  real(r_single),dimension(nchan,nsdata),intent(out):: emissav
!  real(r_kind),dimension(nchan,nsdata),intent(out):: emissav
  real(r_kind),dimension(nsdata*nchan),intent(out):: pemsv, pemsh, pems5
!  real(r_kind),dimension(59):: emc 

! Declare local variables
  integer(i_kind) kcho,n,kch,nn,nnp,i
  integer(i_kind) error_status
  integer(i_kind) quiet
  integer(i_kind),dimension(nchan)::indx

  real(r_kind) zch4,xcorr2v,evertr,ehorzr,xcorr2h,ffoam,zcv2,zcv3
  real(r_kind) xcorr1,zcv1,zcv4,zch1,zch2,zcv5,zcv6,tau2,degre
  real(r_kind) wind,ehorz,evert,sec,sec2,freqghz2,dtde
  real(r_kind) u10mps2,usec,tccub,tau1,tc,tcsq,term2,freqghz
  real(r_kind) term1,u10mps,ps2,pc2,pcc,pss,rvertsi,rverts,rvertsr
  real(r_kind) rverts5,rhorzs5,xcorr15,ffoam5,evertr5,ehorzr5
  real(r_kind) perm_real,perm_imag,rhorzsr,zch5,zch6,zch3,rhorzsi
  real(r_kind) rhorzs,perm_imag2,einf,fen,del2,del1,fen2,perm_real2
  real(r_kind) perm_imag1,perm_real1,den1,den2
  real(r_kind),dimension(1):: emissir

  complex(r_kind) perm1,perm2,rvth,rhth,xperm

! -------- ice/snow-MW em  ---------
  integer(i_kind)     :: surf_type 
!  real(r_kind),dimension(nchan,jppf):: btm
  real(r_kind):: tbasnw(4),tbbsnw(2),tbbmi(nchan)
  logical      :: amsuab 
  logical      :: amsua,amsub,ssmi
  real(r_kind):: ice_depth
  
  integer     :: ipolar(nchan)
!  real        :: polar(nchan)
  
!
!  Start emiss here
!
!  uuk=zero
!  vvk=zero

!  ////////////  IR emissivity //////////////////
#ifdef RTTOV
     
! Compute or set the emissivity for IR channels.
  if(itype == 0)then

!$omp parallel do  schedule(dynamic,1) private(n) &
!$omp private(nn,nnp,wind,degre,kch,i,indx,error_status)
     do n = 1,nsdata
        nn   = indexn(1,n)
        nnp  = nn+knchan(n)-1
        if (isflg(n)==0 .or. isflg(n) == 4) then          ! sea

!      use RTTOV ISEM calculated values over sea .
          pemsv(nn:nnp) = zero
          pemsh(nn:nnp) = zero

!       Use fixed IR emissivity values over land, snow, and ice.

        else if(isflg(n) == 1 .or. isflg(n) == 5) then
          pemsv(nn:nnp) = 0.98_r_kind                     ! sea ice
          pemsh(nn:nnp) = 0.98_r_kind
        else if(isflg(n) == 2 .or. isflg(n) == 6) then
          pemsv(nn:nnp) = 0.97_r_kind                     ! land
          pemsh(nn:nnp) = 0.97_r_kind
        else 
          pemsv(nn:nnp) = one                             ! snow
          pemsh(nn:nnp) = one
        end if
!        do i=1,knchan(n)
!           emissav(i,n) = pemsv(nn+i-1)
!        end do
     end do
     

!  ////////////  MW emissivity //////////////////
  else if(itype == 1)then

! Set emissivity for microwave channels 
!
     surf_type=0
     amsuab = amsua .or. amsub

!$omp parallel do  schedule(dynamic,1) private(nn) &
!$omp private(n,kch,kcho,tbasnw,tbbsnw,tbbmi,evert,ehorz,surf_type,ice_depth) &
!$omp private(freqghz,u10mps,pcc,pss,ps2,pc2,freqghz2,u10mps2,sec) &
!$omp private(sec2,usec,tc,tcsq,tccub,tau1,tau2,del1,del2,einf,fen,fen2,den1) &
!$omp private(den2,perm_real1,perm_real2,perm_imag1,perm_imag2,perm_real) &
!$omp private(perm_imag,xperm,perm1,perm2,rhth,rvth,rvertsr,rvertsi) &
!$omp private(rverts,rhorzsr,rhorzsi,rhorzs,xcorr1,zcv1,zcv2) &
!$omp private(zcv3,zcv4,zcv5,zcv6,zch1,zch2,zch3,zch4,zch5,zch6) &
!$omp private(xcorr2v,xcorr2h,ffoam,evertr,ehorzr) & 
!$omp private(rverts5,rhorzs5,xcorr15,ffoam5,evertr5,ehorzr5,dtde)
     do nn = 1,knchpf
        n    = kprof(nn)
        kch  = kchan(nn)
!        ipolar(kch) = coefs(inst)%fastem_polar(kch)
!        if (ipolar(kch) == 3) polar(kch) = 0.0   ! vertical polar
!        if (ipolar(kch) == 4) polar(kch) = 1.0   ! horizontal polar
!        kcho = kochan(nn)
        
        pemsv(nn) = zero  !default value
        pemsh(nn) = zero
        pems5(nn) = zero

!        freqghz = sc%frequency(kch)
        freqghz = coefs(inst)%frequency_ghz(kch)  ! GHz 

        if((isflg(n)==3 .or. isflg(n) == 7).and.snow5(n)>0.1_r_kind)then

!      ----- snow MW -------

!          land/snow points 

           if(amsuab) then 
              if(amsua) then
                 tbasnw(1:3) = btm(1:3,n)
                 tbasnw(4)   = btm(15,n)
                 tbbsnw(1:2) = -999.9_r_kind
              else if(amsub) then
                 tbasnw(1:4) = -999.9_r_kind
                 tbbsnw(1:2) = btm(1:2,n)
              end if
              call snwem_amsu(zasat(n),freqghz, &
                   snow5(n),ts5(n),tbasnw,tbbsnw,ehorz,evert )
              
!               call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
              pemsv(nn) = evert !because esv=esh
              pemsh(nn) = ehorz
              pems5(nn) = evert
           else if(ssmi) then 
              surf_type=3
              tbbmi(1:knchan(n)) = btm(1:knchan(n),n)
              call emiss_ssmi(   &
                   surf_type,zasat(n),freqghz, &
                   soilm5(n),vegf5(n),vegtype5(n),soiltype5(n), &
                   soilt5(n),ts5(n),snow5(n),tbbmi,ehorz,evert )
              pems5(nn) = (one-polar(kch))*evert + polar(kch)*ehorz
              pemsv(nn) = evert !because esv=esh
              pemsh(nn) = ehorz
           else
              if(freqghz<80._r_kind)then  !snow & f<80GHz
!               currently landem only works for frequencies < 80 Ghz
               if (use_landem) then
                 call landem(zasat(n),freqghz,  &
                      soilm5(n),vegf5(n),vegtype5(n),soiltype5(n),soilt5(n), &
                      ts5(n),snow5(n),ehorz,evert)
                 call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
                 pemsv(nn) = evert !because esv=esh
                 pemsh(nn) = ehorz
               else
                 pemsv(nn) = 0.90_r_kind
                 pemsh(nn) = 0.90_r_kind
                 pems5(nn) = 0.90_r_kind
               end if
              else  !snow & f>=80GHz
                 pemsv(nn) = 0.90_r_kind
                 pemsh(nn) = 0.90_r_kind
                 pems5(nn) = 0.90_r_kind
              end if
           end if  !snow & amsuab/ssmi/othermw
           

        else if(isflg(n) == 1 .or. isflg(n) == 5)then
!      ----- sea ice MW  -------

           if(amsuab) then 
              ice_depth=zero
              if(amsua) then
                 tbasnw(1:3) = btm(1:3,n)
                 tbasnw(4)   = btm(15,n)
                 tbbsnw(1:2) = -999.9_r_kind
              else if(amsub) then
                 tbasnw(1:4) = -999.9_r_kind
                 tbbsnw(1:2) = btm(1:2,n)
              end if
              call iceem_amsu(  &
                   zasat(n),freqghz,ice_depth,ts5(n),&
                   tbasnw,tbbsnw,ehorz,evert )
!                call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
              pemsv(nn) = evert  !because esv=esh
              pemsh(nn) = ehorz
              pems5(nn) = evert
           else if(ssmi) then 
              surf_type=2
              tbbmi(1:knchan(n)) = btm(1:knchan(n),n)
              call emiss_ssmi(   &
                   surf_type,zasat(n),freqghz, &
                   soilm5(n),vegf5(n),vegtype5(n),soiltype5(n), &
                   soilt5(n),ts5(n),snow5(n),tbbmi,ehorz,evert )
              pems5(nn) = (one-polar(kch))*evert + polar(kch)*ehorz
              pemsv(nn) = evert  !because esv=esh
              pemsh(nn) = ehorz
           else
              pemsv(nn) = 0.92_r_kind
              pemsh(nn) = 0.92_r_kind
              pems5(nn) = 0.92_r_kind
           end if
        else if(isflg(n) == 0 .or. isflg(n) == 4)then
!      ----- sea (ice-free) MW  -------
           
!          Open ocean points
             call seaem(freqghz,zasat(n),zlsat(n),ts5(n), &
                        u10(n),v10(n),ehorz,evert)

             pemsv(nn) = evert
             pemsh(nn) = ehorz
             call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )

        else  
!      ----- land (snow-free or snow thin ) MW -------
           if(freqghz<80._r_kind)then  !land & f<80GHz
!             currently landem only works for frequencies < 80 Ghz
             if (use_landem) then
              call landem(zasat(n),freqghz,  &
                   soilm5(n),vegf5(n),vegtype5(n),soiltype5(n),soilt5(n), &
                   ts5(n),snow5(n),ehorz,evert  )
              call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
              pemsv(nn) = evert
              pemsh(nn) = ehorz
             else
              pemsv(nn) = 0.95_r_kind
              pemsh(nn) = 0.95_r_kind
              pems5(nn) = 0.95_r_kind
             end if
           else  !land & f>=80GHz
              pemsv(nn) = 0.95_r_kind
              pemsh(nn) = 0.95_r_kind
              pems5(nn) = 0.95_r_kind
           end if  !land & if f><80
           
        end if                         
        

!        Load emissivity into array for radiative transfer model
!       (pems5) and diagnostic output file (emissav).
        
        pemsv(nn)       = max(zero,min(pemsv(nn),one))
        pemsh(nn)       = max(zero,min(pemsh(nn),one))
        pems5(nn)       = max(zero,min(pems5(nn),one))

     end do
  else
    WRITE(UNIT=message(1), FMT='(A,I5)') &
      "ILLEGAL surface emissivity type",itype
    CALL da_error(__FILE__,__LINE__,message(1:1))
  end if   !IR or MW
  

! End of routine.
  return

#endif

  
contains
  
#include "ehv2pem.inc"
#include "adm_ehv2pem.inc"

end subroutine gsi_emiss
