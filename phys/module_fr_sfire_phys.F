!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! This file contains parts copied and/or adapted from earlier codes by 
! Terry Clark, Janice Coen, Don Latham, and Net Patton.

module module_fr_sfire_phys

use module_model_constants, only: cp,xlv
use module_fr_sfire_util


! Following table copied from module_fr_cawfe_fuel by Ned Patton with minor changes. 
! Based on:           Clark, T. L., J. L. Coen and D. Latham: 2004, 
!                      "Description of a coupled atmosphere-fire model",
!                      International Journal of Wildland Fire, 13, 49-63.
!
! edited by Jan Mandel   jmandel@ucar.edu  September 2007
!
! - moved all fuel related constants and the initialization subroutine here
! - copied descriptions for fuel categories from fire_sfc.m4 in the original CAWFE code 
! This file had to be copied under a new name because packages in wrf physics
! layer are not allowed to call each other.

!D in col 2 means quantity derived from the others
!
!  Scalar constants (data same for all fuel categories):
!       HFGL           SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
!       CMBCNST        JOULES PER KG OF DRY FUEL
!       FUELHEAT       FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
!       FUELMC_G       FUEL PARTICLE (SURFACE) MOISTURE CONTENT
!D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
!                        % of total fuel mass that is water (not quite
!                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
!                        so BMST = FUELMC_G / (1 + FUELMC_G)  where
!                        FUELMC_G = ground fuel moisture
!
!  Data arrays indexed by fuel category:
!       FGI            INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
!       FUELDEPTHM     FUEL DEPTH, IN M  (CONVERTED TO FT)              
!       SAVR           FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
!                         GRASS: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
!       FUELMCE        MOISTURE CONTENT OF EXTINCTION; 0.30 FOR MANY DEAD FUELS; 0.15 FOR GRASS
!       FUELDENS       OVENDRY PARTICLE DENSITY, LB/FT^3
!       ST             FUEL PARTICLE TOTAL MINERAL CONTENT
!       SE             FUEL PARTICLE EFFECTIVE MINERAL CONTENT
!       WEIGHT         WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
!                        RANGES FROM ~5 (FAST BURNUP) TO 1000 ( ~40% DECR OVER 10 MIN).
!       FCI_D          INITIAL DRY MASS OF CANOPY FUEL
!       FCT            BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
!       ichap          1 if chaparral, 0 if not
!D      FCI            INITIAL TOTAL MASS OF CANOPY FUEL
!D      FCBR           FUEL CANOPY BURN RATE (KG/M**2/S) 

! =============================================================================
! Standard 13 fire behavior fuel models (for surface fires), along with some
!          estimated canopy properties (for crown fire).
! =============================================================================
!  FUEL MODEL 1: Short grass (1 ft)
!  FUEL MODEL 2: Timber (grass and understory)
!  FUEL MODEL 3: Tall grass (2.5 ft)
!  FUEL MODEL 4: Chaparral (6 ft)
!  FUEL MODEL 5: Brush (2 ft) 
!  FUEL MODEL 6: Dormant brush, hardwood slash
!  FUEL MODEL 7: Southern rough
!  FUEL MODEL 8: Closed timber litter
!  FUEL MODEL 9: Hardwood litter
!  FUEL MODEL 10: Timber (litter + understory)
!  FUEL MODEL 11: Light logging slash
!  FUEL MODEL 12: Medium logging slash
!  FUEL MODEL 13: Heavy logging slash
!  FUEL MODEL 14: no fuel??
!

   REAL, PARAMETER :: cmbcnst  = 17.433e+06             ! J/kg dry fuel
   REAL, PARAMETER :: hfgl     = 17.e4                  ! W/m^2
   REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
   REAL, PARAMETER :: fuelmc_g = 0.08                   ! set = 0 for dry ground fuel
   REAL, PARAMETER :: fuelmc_c = 1.00                   ! set = 0 for dry canopy
   REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
   !real, parameter :: xlv      = 2.5e6                  ! to make it selfcontained
   !real, parameter :: cp      =  7.*287./2              ! to make it selfcontained
   

   INTEGER, PARAMETER :: nfuelcats = 14
   INTEGER, DIMENSION( nfuelcats ), save :: ichap
   REAL   , DIMENSION( nfuelcats ), save :: weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se

   DATA fgi / 0.166, 0.897, 0.675, 2.468, 0.785, 1.345, 1.092, &
              1.121, 0.780, 2.694, 2.582, 7.749, 13.024, 1.e-7 /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305 /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12  / 
   DATA fueldens / nfuelcats * 32. /   ! 32 if solid, 19 if rotten.
   DATA st / nfuelcats* 0.0555 /
   DATA se / nfuelcats* 0.010 /
! ----- Notes on weight: (4) - best fit of Latham data;
!                 (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
!                 (11)-(13) could be 300-1600
   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. / 
! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60.    /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 / 
! =========================================================================

contains
   
subroutine init_fuel_cats
implicit none
!*** purpose: initialize fuel tables by constants
!*** arguments: none
!*** local
integer:: nf      
!*** executable

DO nf = 1,nfuelcats
    fci(nf) = (1.+fuelmc_c)*fci_d(nf)
    fcbr(nf) = fci_d(nf)/fct(nf)
END DO
      
end subroutine init_fuel_cats

!
!*******************
!

subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,             &
                           nfuel_cat,fuel_time  &
#                          include "fr_sfire_params_args.h" 
)

implicit none

!*** purpose: Set all fire model params arrays, constant values.

!*** arguments
integer, intent(in)::ifds,ifde,jfds,jfde                        ! fire domain bounds
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire tile bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays
#define IN out  /* the fire param arrays are set here, elsewhere read only */
#include "fr_sfire_params_decl.h" 
#undef IN

!*** local

real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
integer:: i,j,k
logical:: err
real, dimension(ifts-1:ifte+1,jfts-1:jfte+1)::zsfc
character(len=128)::msg

!*** executable

err=.false.
do j=jfts,jfte
    do i=ifts,ifte
        ! fuel time constant, cell based
        k=int(nfuel_cat(i,j))
        if(k.lt.1.or.k.gt.nfuelcats)then
!            k=nfuelcats
!            err=.true.
             write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
             call message(msg)
             call crash('set_fire_params: fuel category out of bounds')
        endif
        fuel_time(i,j)=weight(k)/0.85 ! cell based
    enddo
enddo

if(err)call crash('set_fire_params: fuel category out of bounds')

do j=jfts,jfte
    do i=ifts,ifte
        
        ! do not understand calculations of stime in binit.m4
        ! set fuel time constant: weight=1000=>40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        ischap(i,j)=ichap(int(nfuel_cat(i,j)))
        fgip(i,j)=fgi(int(nfuel_cat(i,j)))

        ! end jm addition

        !
        !*** rest copied from wf2_janice/fire_startup.m4 with minimal changes
        !

        !     ...Settings of fire spread parameters from Rothermel follows. These
        !        don't need to be recalculated later.
        
        fuelloadm= (1.-bmst) * fgi(int(nfuel_cat(i,j)))  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(int(nfuel_cat(i,j)))/0.3048               ! to ft
        betafl(i,j) = fuelload/(fueldepth * fueldens(int(nfuel_cat(i,j))))! packing ratio
        betaop = 3.348 * savr(int(nfuel_cat(i,j)))**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*fuelmc_g            ! heat of preignition, btu/lb
        epsilon = exp(-138./savr(int(nfuel_cat(i,j))) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * savr(int(nfuel_cat(i,j)))**0.55)    ! const in wind coef
        bbb(i,j) = 0.02526 * savr(int(nfuel_cat(i,j)))**0.54                ! const in wind coef
        e = 0.715 * exp( -3.59e-4 * savr(int(nfuel_cat(i,j))))       ! const in wind coef
        phiwc(i,j) = c * (betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(int(nfuel_cat(i,j)))**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * savr(int(nfuel_cat(i,j)))**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + st(int(nfuel_cat(i,j))))       ! net fuel loading, lb/ft^2
        rtemp1 = fuelmc_g/fuelmce(int(nfuel_cat(i,j)))
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* se(int(nfuel_cat(i,j)))**(-0.19)                ! mineral damping coef
        ir = gamma * wn * fuelheat * etam * etas  !rxn intensity,btu/ft^2 min
        ! jm irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        ! jm: irm set but never used??

        xifr = exp( (0.792 + 0.681*savr(int(nfuel_cat(i,j)))**0.5) &
            * (betafl(i,j)+0.1)) /(192. + 0.2595*savr(int(nfuel_cat(i,j)))) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.

        r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min
        if (int(nfuel_cat(i,j)) .eq. 14) r_0(i,j)=0.     ! no fuel, no spread.
    enddo
enddo

end subroutine set_fire_params

!
!*******************
!

subroutine heat_fluxes(dt,                        &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        iffs,iffe,jffs,jffe,                      &  ! fuel_frac_burnt dims
        fgip,fuel_frac_burnt,                     & !in
        grnhft,grnqft)                              !out
implicit none

!*** purpose        
! compute the heat fluxes on the fire grid cells

!*** arguments
real, intent(in)::dt          ! dt  the fire time step (the fire model advances time by this)
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   ! dimensions                   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft

!*** local
integer::i,j
real:: dmass

!*** executable        
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! ground fuel dry mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2) = fgi(nfuel_cat(i,j)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! J/m^2/sec
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! what the #!@* is that??
         ! xlv is defined in module_model_constants.. who knows that it is.. why .56 ??
    enddo
enddo

end subroutine heat_fluxes

!
!**********************
!            


subroutine set_nfuel_cat(   &
    ifms,ifme,jfms,jfme,               &
    ifts,ifte,jfts,jfte,               &
    ifuelread,nfuel_cat0,zsf,nfuel_cat)

implicit none

! set fuel distributions for testing
integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread,nfuel_cat0
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
real, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat

!*** local

! parameters to control execution
integer:: i,j,iu1
real:: t1
character(len=128)msg

    write(msg,'(a,i3)')'set_nfuel_cat: ifuelread=',ifuelread 
    call message(msg)

if (ifuelread .eq. -1) then
    call message('set_nfuel_cat: assuming nfuel_cat initialized elsewhere') 
    call message(msg)
else if (ifuelread .eq. 0) then
!
    do j=jfts,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=real(nfuel_cat0)
        enddo
    enddo
    write(msg,'(a,i3)')'set_nfuel_cat: fuel initialized with category',nfuel_cat0
    call message(msg)
         
else if (ifuelread .eq. 1) then
!
!         make dependent on altitude (co mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... user defines fuel category spatial variability ! param!
    do j=jfts,jfte
        do  i=ifts,ifte
            ! nfuel_cat(i,j)= 2     ! grass with understory ! jm does nothing
            !jm t1=zsf(i,j)*slngth/100.
            t1 = zsf(i,j)  ! this is in m
            if(t1.le.1524.)then   !  up to 5000 ft
                nfuel_cat(i,j)= 3  ! tall grass
            else if(t1.ge.1524. .and. t1.le.2073.)then  ! 5.0-6.8 kft.
                nfuel_cat(i,j)= 2  ! grass with understory
            else if(t1.ge.2073..and.t1.le.2438.)then  ! 6.8-8.0 kft.
                nfuel_cat(i,j)= 8  ! timber litter - 10 (ponderosa)
            else if(t1.gt.2438. .and. t1.le. 3354.) then ! 8.0-11.0 kft.
!                 ... could also be mixed conifer.
                nfuel_cat(i,j)= 10 ! timber litter - 8 (lodgepole)
            else if(t1.gt.3354. .and. t1.le. 3658.) then ! 11.0-12.0 kft
                nfuel_cat(i,j)= 1  ! alpine meadow - 1
            else if(t1.gt.3658. ) then  ! > 12.0 kft
                nfuel_cat(i,j)= 14 ! no fuel.
            endif
        enddo
    enddo

    call message('set_nfuel_cat: fuel initialized by altitude')
else

    call crash('set_nfuel_cat: bad ifuelread')
endif
!     .............end  load fuel categories (or constant) here.

end subroutine set_nfuel_cat            

!
!**********************
!            

subroutine fire_ros(ros_back,ros_wind,ros_slope, &
speed,tanphi,i,j &
#include "fr_sfire_params_args.h"
)
implicit none
#include "fr_sfire_params_decl.h"

! copied from wf2_janice 
! with the following changes ONLY: 
!   0.5*(speed + abs(speed)) -> max(speed,0.)
!   index l -> j 
!   took out some prints
!   argument fuelloadm never used??
!   not using nfuel_cat here - cell info was put into arrays passed as arguments
!       in include file to avoid transcription errors when used elsewhere
!   betaop is absorbed in phiwc, see module_fr_sfire_model/fire_startup
!   return the backing, wind, and slope contributions to the rate of spread separately
!       because they may be needed to take advantage of known wind and slope vectors.
!       They should add up to get the total rate of spread.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     ... calculates fire spread rate with mcarthur formula or Rothermel
!           using fuel type of fuel cell
!
!      
!         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
!         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
!      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*** arguments
real, intent(out)::ros_back,ros_wind,ros_slope ! rate of spread: backing, due to wind, due to slope
real, intent(in):: speed, tanphi ! windspeed and slope in the directino normal to the fireline
integer, intent(in)::i,j         ! node mesh coordinates

!*** local
real:: umid, phis, phiw, spdms, umidm, excess
integer, parameter::ibeh=1
real, parameter::ros_max=6.
character(len=128)msg

!*** executable

if (ischap(i,j) .eq. 0) then            ! if not chaparral
    if (ibeh .eq. 1) then                ! use Rothermel formula
!       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)          ! 
        umidm = min(spdms,30.)       ! max input wind spd is 30 m/s   !param!
        umid = umidm * 196.850                    ! m/s to ft/min
        !  eqn.: phiw = c * umid**bbb(i,j) * (betafl(i,j)/betaop)**(-e) ! wind coef
        phiw = umid**bbb(i,j) * phiwc(i,j) ! wind coef
        phis=0.
        if (tanphi .gt. 0.) then
            phis = 5.275 *(betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
        endif
        ! rosm = r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
        ros_back = r_0(i,j) * .00508
        ros_wind = ros_back*phiw
        ros_slope= ros_back*phis
        

    else                                   ! MacArthur formula (Australian)
        ! rosm = 0.18*exp(0.8424*max(speed,0.))
        ros_back = 0.18*exp(0.8424)
        ros_wind = 0.18*exp(0.8424*max(speed,0.)) - ros_back
        ros_slope =0.
    endif
!
else   ! chaparral
!        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.)      
    ! rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
    ! note: backing ros is 0 for chaparral without setting nozero value below
    !sp_n=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    !rosm= max(rosm, sp_n)   ! no less than backing r.o.s.

    ros_back=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    ros_wind = 1.2974 * spdms**1.41       ! spread rate, m/s
    ros_wind = max(ros_wind, ros_back)-ros_back
    ros_slope =0.

endif
!
!     ----------note!  put an 6 m/s cap on max spread rate -----------
! rosm= min(rosm, 6.)         ! no faster than this cap   ! param !

excess = ros_back + ros_wind + ros_slope - ros_max

if (excess > 0.)then
    ! take it out of wind and slope in proportion
    ros_wind = ros_wind - excess*ros_wind/(ros_wind+ros_slope)
    ros_slope = ros_slope - excess*ros_slope/(ros_wind+ros_slope)
endif

!     ... to rescale to veloc. carried by model, mult x (svel*snorm(1,3))= .1
!jm: huh ???
!     fire_ros = 0.1*rosm
!
      return
end subroutine fire_ros 

end module module_fr_sfire_phys
