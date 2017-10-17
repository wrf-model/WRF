PROGRAM make_p3_lookuptable1

!______________________________________________________________________________________
!
! This program creates the lookup tables for ice microphysical processes used by
! the P3 microphysics scheme.
!
!  Note:  compile with double-precision (pgf90 -r8 create_p3_lookupTable_1.f90)
!
! P3 package version: v2.4.7
! Last modified     : 2017-06-28
!______________________________________________________________________________________

 implicit none

!--
 integer            :: i_rhor          ! index for rho_rime loop                [1 .. n_rhor]
 integer            :: i_Fr            ! index for rime-mass-fraction loop      [1 .. n_Fr]
 integer            :: i_Qnorm         ! index for normalized qitot loop        [1 .. n_Qnorm]
 integer            :: i_Drscale       ! index for scaled mean rain size loop   [1 .. n_Drscale]

 integer, parameter :: n_rhor    =  5  ! number of indices for i_rhor  loop           (1st; outer)
 integer, parameter :: n_Fr      =  4  ! number of indices for i_Fr    loop           (2nd)
 integer, parameter :: n_Qnorm   = 50  ! number of indices for i_Qnorm loop           (3rd)
 integer, parameter :: n_Drscale = 30  ! number of indices for scaled mean rain size  (4th; inner)
!==

 integer :: i,ii,iii,jj,kk,kkk,dumii

 real :: N,q,qdum,dum1,dum2,cs1,ds1,lam,n0,lamf,qerror,del0,c0,c1,c2,dd,sum1,sum2,       &
         sum3,sum4,xx,a0,b0,a1,b1,dum,bas1,aas1,aas2,bas2,gammq,gamma,d1,d2,delu,lamold, &
         cap,lamr,dia,amg,dv,n0dum,sum5,sum6,sum7,sum8,dg,cg,bag,aag,dcritg,dcrits,      &
         dcritr,Fr,csr,dsr,duml,dum3,dum4,rhodep,cgpold,m1,m2,m3,dt,mu_r,initlamr,lamv,  &
         rdumii,lammin,lammax,pi,g,p,t,rho,mu,mu_i,ds,cs,bas,aas,dcrit

!-- outputs from lookup table (i.e. "f1prxx" read by access_lookup_table in s/r p3_main)
 real, dimension(n_Qnorm,n_Fr) :: uns,ums,refl,dmm,rhomm,nagg,nrwat,qsave,nsave,vdep,    &
        eff,lsave,a_100,n_100,vdep1,nsmall,nlarge,nrwats

 real, dimension(n_Qnorm,n_Drscale,n_Fr) :: qrrain,nrrain,nsrain,qsrain,ngrain
!==

 real, dimension(n_Drscale) :: lamrs
 real, dimension(10000)     :: fall1
 real, dimension(1000)      :: fall2,fallr,num,numi
 real, dimension(n_rhor)    :: cgp,crp
 real, dimension(150)       :: mu_r_table

 real, parameter            :: Dm_max = 2000.e-6   ! max. mean ice size for lambda limiter
 real, parameter            :: Dm_min =    2.e-6   ! min. mean ice size for lambda limiter

 real, parameter            :: thrd = 1./3.
 real, parameter            :: sxth = 1./6.

! end of variable declaration
!-------------------------------------------------------------------------------

! set constants and parameters

! assume 600 hPa, 253 K for p and T for fallspeed calcs (for reference air density)
 pi  = 3.14159        !=acos(-1.)
!pi  = 3.14159265359  !=acos(-1.)        <-- to be replaced with this one (for double precision)
 g   = 9.861                           ! gravity
 p   = 60000.                          ! air pressure (pa)
 t   = 253.15                          ! temp (K)
 rho = p/(287.15*t)                    ! air density (kg m-3)
 mu  = 1.496E-6*t**1.5/(t+120.)/rho    ! viscosity of air
 dv  = 8.794E-5*t**1.81/p              ! diffusivity of water vapor in air
 dt  = 10.                             ! time step for collection (s)

! parameters for surface roughness of ice particle used for fallspeed
! see mitchell and heymsfield 2005
 del0 = 5.83
 c0   = 0.6
 c1   = 4./(del0**2*c0**0.5)
 c2   = del0**2/4.

!--- specified mass-dimension relationship (cgs units) for unrimed crystals:

! ms = cs*D^ds
!
! for graupel:
! mg = cg*D^dg     no longer used, since bulk volume is predicted
!===

!---- Choice of m-D parameters for large unrimed ice:

! Heymsfield et al. 2006
!      ds=1.75
!      cs=0.0040157+6.06e-5*(-20.)

! sector-like branches (P1b)
!      ds=2.02
!      cs=0.00142

! bullet-rosette
!     ds=2.45
!      cs=0.00739

! side planes
!      ds=2.3
!      cs=0.00419

! radiating assemblages of plates (mitchell et al. 1990)
!      ds=2.1
!      cs=0.00239

! aggreagtes of side planes, bullets, etc. (Mitchell 1996)
!      ds=2.1
!      cs=0.0028

!-- ** note: if using one of the above (.i.e. not brown and francis, which is already in mks units),
!           then uncomment line below to convert from cgs units to mks
!      cs=cs*100.**ds/1000.
!==

! Brown and Francis (1995)
 ds = 1.9
! cs = 0.01855 ! original (pre v2.3), based on assumption of Dmax
 cs = 0.0121 ! scaled value based on assumtion of Dmean from Hogan et al. 2012, JAMC

!====

! specify m-D parameter for fully rimed ice
!  note:  cg is not constant, due to variable density
 dg = 3.


!--- projected area-diam relationship (mks units) for unrimed crystals:
!     note: projected area = aas*D^bas

! sector-like branches (P1b)
!      bas = 1.97
!      aas = 0.55*100.**bas/(100.**2)

! bullet-rosettes
!      bas = 1.57
!      aas = 0.0869*100.**bas/(100.**2)

! aggreagtes of side planes, bullets, etc.
 bas = 1.88
 aas = 0.2285*100.**bas/(100.**2)

!===

!--- projected area-diam relationship (mks units) for fully rimed ice:
!    note: projected area = aag*D^bag

! assumed non-spherical
! bag = 2.0
! aag = 0.625*100.**bag/(100.**2)

! assumed spherical:
 bag = 2.
 aag = pi*0.25
!===

 dcrit = (pi/(6.*cs)*900.)**(1./(ds-3.))

! check to make sure projected area at dcrit not greater than than of solid sphere
! stop and give warning message if that is the case

 if (pi/4.*dcrit**2.lt.aas*dcrit**bas) then
    print*,'STOP, area > area of solid ice sphere, unrimed'
    stop
 endif

 open(unit=1,file='./p3_lookup_table_1-v2.4.7.dat',status='unknown')

!.........................................................

! generate lookup table for mu (for rain)
!
! space of a scaled q/N -- initlamr

 do i = 1,150              ! loop over lookup table values

    initlamr = (real(i)*2.)*1.e-6 + 250.e-6
    initlamr = 1./initlamr

! iterate to get mu_r
! mu_r-lambda relationship is from Cao et al. (2008), eq. (7)

! start with first guess, mu_r = 0

    mu_r = 0.

    do ii = 1,50
       lamr = initlamr*(gamma(mu_r+4.)/(6.*gamma(mu_r+1.)))**thrd

! new estimate for mu_r based on lambda
! set max lambda in formula for mu to 20 mm-1, so Cao et al.
! formula is not extrapolated beyond Cao et al. data range
       dum = min(20.,lamr*1.e-3)
       mu_r = max(0.,-0.0201*dum**2+0.902*dum-1.718)

! if lambda is converged within 0.1%, then exit loop
       if (ii.ge.2) then
          if (abs((lamold-lamr)/lamr).lt.0.001) goto 111
       endif

       lamold = lamr

    enddo !ii-loop

111 continue

! assign lookup table values
    mu_r_table(i) = mu_r

 enddo !i-loop

!.........................................................
! main loop over rime density

! alpha parameter of m-D for rimed ice
 crp(1) =  50.*pi*sxth
 crp(2) = 250.*pi*sxth
 crp(3) = 450.*pi*sxth
 crp(4) = 650.*pi*sxth
 crp(5) = 900.*pi*sxth

 do i_rhor = 1,n_rhor

    print*,(pi/(4.*aas))**(1./(bas-2.))
    print*,(aas/aag)**(1./(bag-bas))


!------------------------------------------------------------------------

! find threshold with rimed mass added

! loop over rimed mass fraction (4 points)
! Fr below are values of rime mass fraction for the lookup table
! specific values in model are interpolated between these four points


!- note:  add this code eventually (outside of i_Fr loop)
!     Fr(1) = 0.
!     Fr(2) = 0.333
!     Fr(3) = 0.667
!     Fr(4) = 1.
!=

    do i_Fr = 1,n_Fr   ! loop for rime mass fraction, Fr

!-- these lines to be replaced by Fr(i_Fr) initialization outside of loops
       if (i_Fr.eq.1) Fr = 0.
       if (i_Fr.eq.2) Fr = 0.333
       if (i_Fr.eq.3) Fr = 0.667
       if (i_Fr.eq.4) Fr = 1.
!==

! calculate mass-dimension relationship for partially-rimed crystals
! msr = csr*D^dsr
! formula from P3 Part 1 (JAS)

! dcritr is critical size separating fully-rimed from partially-rime ice

! first guess, set cgp=crp
       cgp(i_rhor) = crp(i_rhor)

! case of no riming (Fr = 0), then we need to set dcrits and dcritr to arbitrary large values

       if (i_Fr.eq.1) then
          dcrits = 1.e+6
          dcritr = dcrits
          csr    = cs
          dsr    = ds
! case of partial riming (Fr between 0 and 1)
       elseif (i_Fr.eq.2.or.i_Fr.eq.3) then
          do
             dcrits = (cs/cgp(i_rhor))**(1./(dg-ds))
             dcritr = ((1.+Fr/(1.-Fr))*cs/cgp(i_rhor))**(1./(dg-ds))
             csr    = cs*(1.+Fr/(1.-Fr))
             dsr    = ds

! get mean density of vapor deposition/aggregation grown ice
             rhodep = 1./(dcritr-dcrits)*6.*cs/(pi*(ds-2.))*(dcritr**(ds-2.)-dcrits**(ds-2.))

! get density of fully-rimed ice as rime mass fraction weighted rime density plus
! density of vapor deposition/aggregation grown ice
             cgpold      = cgp(i_rhor)
             cgp(i_rhor) = crp(i_rhor)*Fr+rhodep*(1.-Fr)*pi*sxth

             if (abs((cgp(i_rhor)-cgpold)/cgp(i_rhor)).lt.0.01) goto 115
          enddo

 115  continue

! case of complete riming (Fr=1.0)
       else

! set threshold size between partially-rimed and fully-rimed ice as arbitrary large
          dcrits = (cs/cgp(i_rhor))**(1./(dg-ds))
          dcritr = 1.e+6       ! here is the "arbitrary large"
          csr    = cgp(i_rhor)
          dsr    = dg

       endif

    print*,'dcrit,dcrits',i_rhor,dcrit,dcrits

!---------------------------------------------------------------------------------------
! set up particle fallspeed arrays
! fallspeed is a function of mass dimension and projected area dimension relationships
! following mitchell and heymsfield (2005), jas

! set up array of particle fallspeed to make computationally efficient

! for high-resolution (in diameter space), ice fallspeed is stored in 'fall1' array (m/s)
! for lower-resolution (in diameter space), ice fallspeed is stored in 'fall2' array (m/s)
! rain fallspeed is stored in 'fallr' (m/s)

       dd = 2.e-6  ! m

! loop over particle size

       do jj = 1,10000

! particle size (m)
!         d1 = real(jj)*dd - 0.5*dd
          d1 = real(jj)*2.*1.e-6 - 1.e-6   ! kept like this to preserve bit-matching

!----- get mass-size and projected area-size relationships for given size (d1)
!          call get_mass_size

          if (d1.le.dcrit) then
             cs1  = pi*sxth*900.
             ds1  = 3.
             bas1 = 2.
             aas1 = pi/4.
          else if (d1.gt.dcrit.and.d1.le.dcrits) then
             cs1  = cs
             ds1  = ds
             bas1 = bas
             aas1 = aas
          else if (d1.gt.dcrits.and.d1.le.dcritr) then
             cs1  = cgp(i_rhor)
             ds1  = dg
             bas1 = bag
             aas1 = aag
          else if (d1.gt.dcritr) then
             cs1  = csr
             ds1  = dsr
             if (i_Fr.eq.1) then
                aas1 = aas
                bas1 = bas
             else

! for projected area, keep bas1 constant, but modify aas1 according to rimed fraction
                bas1 = bas
                dum1 = aas*d1**bas
                dum2 = aag*d1**bag
                m1   = cs1*d1**ds1
                m2   = cs*d1**ds
                m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
!               dum3 = (1.-Fr)*dum1+Fr*dum2
                aas1 = dum3/(d1**bas)

             endif
          endif
!=====

!-----------------------------------------------------------
! neglect turbulent correction for aggregates for now
! correction for turbulence
!            if (d1.lt.500.e-6) then
          a0 = 0.
          b0 = 0.
!            else
!               a0=1.7e-3
!               b0=0.8
!            end if

! fall speed for particle
! best number
          xx = 2.*cs1*g*rho*d1**(ds1+2.-bas1)/(aas1*(mu*rho)**2)

! drag terms
          b1 = c1*xx**0.5/(2.*((1.+c1*xx**0.5)**0.5-1.)*(1.+c1*xx**0.5)**0.5)-a0*b0*xx** &
             b0/(c2*((1.+c1*xx**0.5)**0.5-1.)**2)

          a1 = (c2*((1.+c1*xx**0.5)**0.5-1.)**2-a0*xx**b0)/xx**b1

! velocity in terms of drag terms
          fall1(jj) = a1*mu**(1.-2.*b1)*(2.*cs1*g/(rho*aas1))**b1*d1**(b1*(ds1-bas1+2.)-1.)

       enddo !jj-loop

!.........................................................
! fallspeed array for ice-ice and ice-rain collision calculations

       do jj = 1,1000

! particle size
          d1 = real(jj)*20.*1.e-6 - 10.e-6

          if (d1.le.dcrit) then
             cs1  = pi*sxth*900.
             ds1  = 3.
             bas1 = 2.
             aas1 = pi/4.
          else if (d1.gt.dcrit.and.d1.le.dcrits) then
             cs1  = cs
             ds1  = ds
             bas1 = bas
             aas1 = aas
          else if (d1.gt.dcrits.and.d1.le.dcritr) then
             cs1  = cgp(i_rhor)
             ds1  = dg
             bas1 = bag
             aas1 = aag
          else if (d1.gt.dcritr) then
             cs1  = csr
             ds1  = dsr
             if (i_Fr.eq.1) then
                aas1 = aas
                bas1 = bas
             else
! for area,
! keep bas1 constant, but modify aas1 according
! to rimed fraction
                bas1 = bas
                dum1 = aas*d1**bas
                dum2 = aag*d1**bag
!               dum3 = (1.-Fr)*dum1+Fr*dum2
                m1   = cs1*d1**ds1
                m2   = cs*d1**ds
                m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                aas1 = dum3/(d1**bas)
             endif
          endif

! correction for turbulence
!            if (d1.lt.500.e-6) then
          a0 = 0.
          b0 = 0.
!            else
!               a0=1.7e-3
!               b0=0.8
!            end if

! fall speed for ice
! Best number
          xx = 2.*cs1*g*rho*d1**(ds1+2.-bas1)/(aas1*(mu*rho)**2)

! drag terms
          b1 = c1*xx**0.5/(2.*((1.+c1*xx**0.5)**0.5-1.)*(1.+c1*xx**0.5)**0.5)-a0*b0*xx** &
               b0/(c2*((1.+c1*xx**0.5)**0.5-1.)**2)

          a1 = (c2*((1.+c1*xx**0.5)**0.5-1.)**2-a0*xx**b0)/xx**b1

! velocity in terms of drag terms
          fall2(jj) = a1*mu**(1.-2.*b1)*(2.*cs1*g/(rho*aas1))**b1*d1**(b1*(ds1-bas1+2.)-1.)

!---------------------------------------------------------------
! fall speed for rain particle

!          dia = d1*1.e+6
! HM fix, bug noted by Melissa
          dia = d1  ! diameter m
          amg = pi*sxth*997.*dia**3 ! mass kg
! HM fix, bug noted by Melissa
          amg = amg*1000. ! convert kg to g

          if(dia.le.134.43e-6) then
             dum2 = 4.5795e5*amg**(2.*thrd)
             goto 101
          endif

          if(dia.lt.1511.64e-6) then
             dum2 = 4.962e3*amg**thrd
            goto 101
          endif

          if(dia.lt.3477.84e-6) then
             dum2 = 1.732e3*amg**sxth
             goto 101
          endif

          dum2 = 917.

101 continue

          fallr(jj) = dum2*1.e-2   ! convert (cm s-1) to (m s-1)

       enddo !jj-loop

!---------------------------------------------------------------------------------
! loop around normalized Q (Qnorm)

       do i_Qnorm = 1,n_Qnorm

! lookup table values of normalized Qnorm
! (range of mean mass diameter from ~ 1 micron to 1 cm)

          q = 261.7**((i_Qnorm+10)*0.1)*1.e-18

! uncomment below to test and print proposed values of qovn
!	print*,i_Qnorm,(6./(pi*500.)*q)**0.3333
!	end do
!	stop

! test values
!            N=5.e3
!            q=0.01e-3

             print*,'&&&&&&&&&&&i_rhor',i_rhor
             print*,'***************',i_Qnorm
             print*,'Fr',Fr
             print*,'q,N',q,N

! initialize qerror to arbitrarily large value
             qerror = 1.e+20

!.....................................................................................
! find parameters for gamma distribution

! size distribution for ice is assumed to be
! N(D) = n0 * D^mu_i * exp(-lam*D)

! for the given q and N, we need to find n0, mu_i, and lam

! approach for finding lambda:
! cycle through a range of lambda, find closest lambda to produce correct q

! start with lam, range of lam from 100 to 1 x 10^7 is large enough to
! cover full range over mean size from approximately 1 micron to 1 cm

             do ii = 1,9000

                lam = 1.0013**ii*100.

! get 'mu' parameter (Heymsfield 2003)
! division by 100 is to convert m-1 to cm-1
                mu_i = 0.076*(lam/100.)**0.8-2.
! make sure mu_i >= 0, otherwise size dist is infinity at D = 0
                mu_i = max(mu_i,0.)
! set upper limit at 6
                mu_i = min(mu_i,6.)

!-- for lambda limiter:

! set min lam corresponding to 2000 micron for mean size
!               dum = Dm_max+Fr*(3000.e-6)
                dum = Dm_max
                lam = max(lam,(mu_i+1.)/dum)
! set max lam corresponding to 2 micron mean size
                lam = min(lam,(mu_i+1.)/Dm_min)
! this range corresponds to range of lam of 500 to 5000000
!==

! get normalized n0
                n0 = lam**(mu_i+1.)/(gamma(mu_i+1.))

! calculate integral for each of the 4 parts of the size distribution
! check difference with respect to Qnorm

! dum1 is integral from 0 to dcrit       (solid ice)
! dum2 is integral from dcrit to dcrits  (unrimed large ice)
! dum3 is integral from dcrits to dcritr (fully rimed ice)
! dum4 is integral from dcritr to inf    (partially rimed)

! set up m-D relationship for solid ice with D < Dcrit
                cs1  = pi*sxth*900.
                ds1  = 3.
                dum1 = lam**(-ds1-mu_i-1.)*gamma(mu_i+ds1+1.)*(1.-gammq(mu_i+ds1+1.,dcrit*lam))

                dum2 = lam**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrit*lam))
                dum  = lam**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrits*lam))
                dum2 = dum2-dum
                dum2 = max(dum2,0.)

                dum3 = lam**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcrits*lam))
                dum  = lam**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcritr*lam))
                dum3 = dum3-dum
                dum3 = max(dum3,0.)

                dum4 = lam**(-dsr-mu_i-1.)*gamma(mu_i+dsr+1.)*(gammq(mu_i+dsr+1.,dcritr*lam))

! sum of the integrals from the 4 regions of the size distribution
                qdum = n0*(cs1*dum1+cs*dum2+cgp(i_rhor)*dum3+csr*dum4)

! numerical integration for test to make sure incomplete gamma function is working
!               sum1 = 0.
!               dd = 1.e-6
!               do iii=1,50000
!                  dum=real(iii)*dd
!                  if (dum.lt.dcrit) then
!                  sum1 = sum1+n0*dum**mu_i*cs1*dum**ds1*
!     1                      exp(-lam*dum)*dd
!                  else
!                  sum1 = sum1+n0*dum**mu_i*cs*dum**ds*
!     1                      exp(-lam*dum)*dd
!                  end if
!               end do
!               print*,'sum1=',sum1
!               stop

                if (ii.eq.1) then
                   qerror = abs(q-qdum)
                   lamf   = lam
                endif

! find lam with smallest difference between Qnorm and estimate of Qnorm, assign to lamf
                if (abs(q-qdum).lt.qerror) then
                   lamf   = lam
                   qerror = abs(q-qdum)
                endif


             enddo !ii-loop

! check and print relative error in q to make sure it is not too large
! note: large error is possible if size bounds are exceeded!!!!!!!!!!

             print*,'qerror (%)',qerror/q*100.

! find n0 based on final lam value
! set final lamf to 'lam' variable
! this is the value of lam with the smallest qerror
             lam = lamf
! recalculate mu_i based on final lam
! get 'mu' parameter (Heymsfield 2003)
! division by 100 is to convert m-1 to cm-1
             mu_i = 0.076*(lam/100.)**0.8-2.
! make sure mu_i >= 0, otherwise size dist is infinity at D = 0
             mu_i = max(mu_i,0.)
! set upper limit at 6
             mu_i = min(mu_i,6.)

!            n0 = N*lam**(mu_i+1.)/(gamma(mu_i+1.))

! find n0 from lam and Qnorm
! this is done instead of finding n0 from lam and N, since N
! may need to be adjusted to constrain mean size within reasonable bounds

             dum1 = lam**(-ds1-mu_i-1.)*gamma(mu_i+ds1+1.)*(1.-gammq(mu_i+ds1+1.,dcrit*lam))

             dum2 = lam**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrit*lam))
             dum  = lam**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrits*lam))
             dum2 = dum2-dum

             dum3 = lam**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcrits*lam))
             dum  = lam**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcritr*lam))
             dum3 = dum3-dum

             dum4 = lam**(-dsr-mu_i-1.)*gamma(mu_i+dsr+1.)*(gammq(mu_i+dsr+1.,dcritr*lam))

             n0   = q/(cs1*dum1+cs*dum2+cgp(i_rhor)*dum3+csr*dum4)
             print*,'lam,N0:',lam,n0
             print*,'mu_i:',mu_i

! test final lam, N0 values
!            sum1 = 0.
!            dd = 1.e-6
!               do iii=1,50000
!                  dum=real(iii)*dd
!                  if (dum.lt.dcrit) then
!                     sum1 = sum1+n0*dum**mu_i*cs1*dum**ds1*exp(-lam*dum)*dd
!                  elseif (dum.ge.dcrit.and.dum.lt.dcrits) then
!                     sum1 = sum1+n0*dum**mu_i*cs*dum**ds*exp(-lam*dum)*dd
!                  elseif (dum.ge.dcrits.and.dum.lt.dcritr) then
!                     sum1 = sum1+n0*dum**mu_i*cg*dum**dg*exp(-lam*dum)*dd
!                  elseif (dum.ge.dcritr) then
!                     sum1 = sum1+n0*dum**mu_i*csr*dum**dsr*exp(-lam*dum)*dd
!                  endif
!               enddo
!               print*,'sum1=',sum1
!               stop


! At this point, we have solved for all of the size distribution parameters

!--------------------------------------------------------------------
! find max/min N to constrain mean size (i.e. lambda limiter), this is stored
! and passed to lookup table, so that N can be adjusted during
! the simulation to constrain mean size

! limit min size to 2 micron (diameter)

             duml = (mu_i+1.)/Dm_min
             dum1 = duml**(-ds1-mu_i-1.)*gamma(mu_i+ds1+1.)*(1.-gammq(mu_i+ds1+1.,dcrit*duml))
             dum2 = duml**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrit*duml))
             dum  = duml**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrits*duml))
             dum2 = dum2-dum
             dum3 = duml**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcrits*duml))
             dum  = duml**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcritr*duml))
             dum3 = dum3-dum
             dum4 = duml**(-dsr-mu_i-1.)*gamma(mu_i+dsr+1.)*(gammq(mu_i+dsr+1.,dcritr*duml))

             n0dum = q/(cs1*dum1+cs*dum2+cgp(i_rhor)*dum3+csr*dum4)

! nlarge is used by P3_MAIN to apply the lambda limiter (upper limit)
             nlarge(i_Qnorm,i_Fr) = n0dum/(duml**(mu_i+1.)/(gamma(mu_i+1.)))

! limit max size (number-weighted) to 2 mm
!            dum  = Dm_max+Fr*(3000.e-6)
             dum  = Dm_max
             duml = (mu_i+1.)/(dum)
             dum1 = duml**(-ds1-mu_i-1.)*gamma(mu_i+ds1+1.)*(1.-gammq(mu_i+ds1+1.,dcrit*duml))
             dum2 = duml**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrit*duml))
             dum  = duml**(-ds-mu_i-1.)*gamma(mu_i+ds+1.)*(gammq(mu_i+ds+1.,dcrits*duml))
             dum2 = dum2-dum
             dum3 = duml**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcrits*duml))
             dum  = duml**(-dg-mu_i-1.)*gamma(mu_i+dg+1.)*(gammq(mu_i+dg+1.,dcritr*duml))
             dum3 = dum3-dum
             dum4 = duml**(-dsr-mu_i-1.)*gamma(mu_i+dsr+1.)*(gammq(mu_i+dsr+1.,dcritr*duml))

             n0dum = q/(cs1*dum1+cs*dum2+cgp(i_rhor)*dum3+csr*dum4)

! nsmall is used by P3_MAIN to apply the lambda limiter (lower limit)
             nsmall(i_Qnorm,i_Fr) = n0dum/(duml**(mu_i+1.)/(gamma(mu_i+1.)))

             print*,'nmax,nmin',nsmall(i_Qnorm,i_Fr),nlarge(i_Qnorm,i_Fr)

!.....................................................................................
! begin moment and microphysical process calculations for the lookup table

!.....................................................................................
! mass- and number-weighted mean fallspeed (m/s)
! add reflectivity
!.....................................................................................

! assume conditions for t and p as assumed above (giving rhos), then in microphysics scheme
! multiply by density correction factor (rhos/rho)^0.54, from Heymsfield et al. 2006

! fallspeed formulation from Mitchell and Heymsfield 2005

             dd = 2.e-6 ! dD for numerical integration, units of m

! initialize for numerical integration
             sum1 = 0.
             sum2 = 0.
             sum3 = 0.
             sum4 = 0.
! reflectivity
             sum5 = 0.
! mass mean size
             sum6 = 0.
! mass-weighted mean density
             sum7 = 0.

! numerically integrate over size distribution
             do ii = 1,10000

! dum = particle size
!               dum = real(ii)*dd - 0.5*dd
                dum = real(ii)*dd - 1.e-6  ! units of m

          !assign mass-size parameters (depending on size at ii)
                if (dum.le.dcrit) then
                   ds1 = 3.
                   cs1 = pi*sxth*900.
                else if (dum.gt.dcrit.and.dum.le.dcrits) then
                   ds1 = ds
                   cs1 = cs
                elseif (dum.gt.dcrits.and.dum.le.dcritr) then
                   ds1 = dg
                   cs1 = cgp(i_rhor)
                elseif (dum.gt.dcritr) then
                   ds1 = dsr
                   cs1 = csr
                endif

! numerator of number-weighted velocity - sum1
                sum1 = sum1+fall1(ii)*dum**mu_i*exp(-lam*dum)*dd

! numerator of mass-weighted velocity - sum2
                sum2 = sum2+fall1(ii)*cs1*dum**(ds1+mu_i)*exp(-lam*dum)*dd

! total number and mass for weighting above fallspeeds
! don't need to include n0 and cs since these parameters are
! in both numerator and denominator
      !denominator of number-weighted V
                sum3 = sum3+dum**mu_i*exp(-lam*dum)*dd
      !denominator of mass-weighted V
                sum4 = sum4+cs1*dum**(ds1+mu_i)*exp(-lam*dum)*dd

! reflectivity (integral of mass moment squared)
                sum5 = sum5+n0*(cs1/917.)**2*(6./pi)**2*dum**(2.*ds1+mu_i)*exp(-lam*dum)*dd

! numerator of mass-weighted mean size
                sum6 = sum6+cs1*dum**(ds1+mu_i+1.)*exp(-lam*dum)*dd

! numerator of mass-weighted density
! particle density is defined as mass divided by volume of sphere with same D
                sum7 = sum7+(cs1*dum**ds1)**2/(pi*sxth*dum**3)*dum**mu_i*exp(-lam*dum)*dd

             enddo !ii-loop

! save mean fallspeeds for lookup table
             uns(i_Qnorm,i_Fr)   = sum1/sum3
             ums(i_Qnorm,i_Fr)   = sum2/sum4
             refl(i_Qnorm,i_Fr)  = sum5
             dmm(i_Qnorm,i_Fr)   = sum6/sum4
             rhomm(i_Qnorm,i_Fr) = sum7/sum4

!.....................................................................................
! self-aggregation
!.....................................................................................

             sum1 = 0.
             dd   = 20.e-6

             do jj=1000,1,-1
! set up binned distribution of ice
!               d1     = real(jj)*dd - 0.5*dd
                d1     = real(jj)*dd - 10.e-6  !kept like this to preserve bit-matching
                num(jj) = n0*d1**mu_i*exp(-lam*d1)*dd
             enddo !jj-loop

! loop over exponential size distribution
! note: collection of ice within the same bin is neglected

             do jj = 1000,1,-1
                do kk = 1,jj-1

! particle size
                   d1 = real(jj)*20.*1.e-6 - 10.e-6
                   d2 = real(kk)*20.*1.e-6 - 10.e-6

                   if (d1.le.dcrit) then
                      bas1 = 2.
                      aas1 = pi*0.25
                   elseif (d1.gt.dcrit.and.d1.le.dcrits) then
                      bas1 = bas
                      aas1 = aas
                   else if (d1.gt.dcrits.and.d1.le.dcritr) then
                      bas1 = bag
                      aas1 = aag
                   else if (d1.gt.dcritr) then
                      cs1 = csr
                      ds1 = dsr
                      if (i_Fr.eq.1) then
                         aas1 = aas
                         bas1 = bas
                      else
! for area, keep bas1 constant, but modify aas1 according to rimed fraction
                         bas1 = bas
                         dum1 = aas*d1**bas
                         dum2 = aag*d1**bag
                         m1   = cs1*d1**ds1
                         m2   = cs*d1**ds
                         m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                         dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                         aas1 = dum3/(d1**bas)
                      endif
                   endif

! parameters for particle 2
                   if (d2.le.dcrit) then
                      bas2 = 2.
                      aas2 = pi/4.
                   elseif (d2.gt.dcrit.and.d2.le.dcrits) then
                      bas2 = bas
                      aas2 = aas
                   elseif (d2.gt.dcrits.and.d2.le.dcritr) then
                      bas2 = bag
                      aas2 = aag
                   elseif (d2.gt.dcritr) then
                      cs1 = csr
                      ds1 = dsr
                      if (i_Fr.eq.1) then
                         aas1 = aas
                         bas1 = bas
                      else

! for area,
! keep bas1 constant, but modify aas1 according
! to rimed fraction
                         bas2 = bas
                         dum1 = aas*d2**bas
                         dum2 = aag*d2**bag
                         m1   = cs1*d1**ds1
                         m2   = cs*d1**ds
                         m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                         dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                         aas2 = dum3/(d2**bas)
                      endif
                   endif

! absolute value, differential fallspeed
                   delu = abs(fall2(jj)-fall2(kk))

! note: in micro code we have to multiply by air density
! correction factor for fallspeed, and collection efficiency

! sum for integral

! sum1 = # of collision pairs
! the assumption is that each collision pair reduces crystal
! number mixing ratio by 1 kg^-1 s^-1 per kg/m^3 of air (this is
! why we need to multiply by air density, to get units of
! 1/kg^-1 s^-1)

                   sum1 = sum1+(aas1*d1**bas1+aas2*d2**bas2)*delu*num(jj)*num(kk)
   !               sum1 = sum1+(aas1*d1**bas1+aas2*d2**bas2)*delu*num(jj)*num(kk)

! remove collected particles from distribution over time period dt, update num
!  note -- dt is time scale for removal, not model time step
!                   num(kk) = num(kk)-(aas1*d1**bas1+aas2*d2**bas2)*delu*num(jj)*num(kk)*dt
!                   num(kk) = max(num(kk),0.)

!            write(6,'(2i5,8e15.5)')jj,kk,sum1,num(jj),num(kk),delu,aas1,d1,aas2,d2
!            num(kk)=num(kk)-(aas1*d1**bas1+aas2*d2**bas2)*delu*num(jj)*num(kk)*0.1*0.5
!            num(kk)=max(num(kk),0.)
!            sum1 = sum1+0.5*(aas1*d1**bas1+aas2*d2**bas2)*delu*n0*n0*(d1+d2)**mu_i*exp(-lam*(d1+d2))*dd**2

                enddo !kk-loop
             enddo !jj-loop

! save for output
             nagg(i_Qnorm,i_Fr) = sum1
             print*,'nagg',nagg(i_Qnorm,i_Fr)

!.....................................................................................
! collection of cloud droplets
!.....................................................................................
! note: in microhpysics code needs to be multiplied by collection efficiency Eci
! note: also needs to be multiplied by air density correction factor for fallspeed,
!       air density, and cloud water mixing ratio or number concentration

! initialize sum for integral
             sum1 = 0.
             sum2 = 0.

! dd (dD for numerical integration) (m)
             dd = 2.e-6

! loop over exponential size distribution (from 1 micron to 2 cm)
             do jj = 1,10000

! particle size or dimension (m) for numerical integration
                d1 = real(jj)*2.*1.e-6 - 1.e-6

! get mass-dimension and projected area-dimension relationships
! for different ice types across the size distribution based on critical dimensions
! separating these ice types (see Fig. 2, morrison and grabowski 2008)

! mass = cs1*D^ds1
! projected area = bas1*D^bas1
                if (d1.le.dcrit) then
                   cs1  = pi*sxth*900.
                   ds1  = 3.
                   bas1 = 2.
                   aas1 = pi*0.25
                elseif (d1.gt.dcrit.and.d1.le.dcrits) then
                   cs1  = cs
                   ds1  = ds
                   bas1 = bas
                   aas1 = aas
                elseif (d1.gt.dcrits.and.d1.le.dcritr) then
                   cs1  = cgp(i_rhor)
                   ds1  = dg
                   bas1 = bag
                   aas1 = aag
                elseif (d1.gt.dcritr) then
                   cs1 = csr
                   ds1 = dsr
                   if (i_Fr.eq.1) then
                      aas1 = aas
                      bas1 = bas
                   else
! for area, ! keep bas1 constant, but modify aas1 according to rimed fraction
                      bas1 = bas
                      dum1 = aas*d1**bas
                      dum2 = aag*d1**bag
                      m1   = cs1*d1**ds1
                      m2   = cs*d1**ds
                      m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                      dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                      aas1 = dum3/(d1**bas)
                   endif
                endif

! sum for integral
! include assumed ice particle size threshold for riming of 100 micron

! note: sum1 (nrwat) is the scaled collection rate, units of m^3 kg^-1 s^-1
!       sum2 (nrwats) is mass of snow times scaled collection rate,
!       units of m^3 s^-1

                if (d1.ge.100.e-6) then
                   sum1 = sum1+aas1*d1**bas1*fall1(jj)*n0*d1**mu_i*exp(-lam*d1)*dd
!                  sum2 = sum2+aas1*d1**bas1*fall1(jj)*n0*d1**mu_i*exp(-lam*d1)*dd*cs1*d1**ds1
                endif

             enddo !jj-loop

! save for output
! note: read in as 'f1pr4' in P3_MAIN
             nrwat(i_Qnorm,i_Fr) = sum1
!            nrwats(i_Qnorm,i_Fr) = sum2
             print*,'nrwat',nrwat(i_Qnorm,i_Fr)

!.....................................................................................
! collection of rain
!.....................................................................................
! note: in microphysics code, we need to multiply rate by n0r, collection efficiency,
!       air density, and air density correction factor

! this approach implicitly assumes that the PSDs are constant during the microphysics
! time step, this could produce errors if the time step is large. In particular,
! more mass or number could be removed than is available. This will be taken care
! of by conservation checks in the microphysics code.

! loop around lambda for rain

             do i_Drscale = 1,n_Drscale
                dum = 1.24**i_Drscale*10.e-6

! assumed lamv for tests
!               dum = 7.16e-5
! Note: the lookup table for rain is based on lamv, i.e.,
! inverse volume mean diameter
                lamv      = 1./dum
                lamrs(i_Drscale) = lamv

!------------------------------------------------------------
! get mu_r from lamr
                dum = 1./lamv

                if (dum.lt.282.e-6) then
                   mu_r = 8.282
                elseif (dum.ge.282.e-6 .and. dum.lt.502.e-6) then
! interpolate
                   rdumii = (dum-250.e-6)*1.e6*0.5
                   rdumii = max(rdumii,1.)
                   rdumii = min(rdumii,150.)
                   dumii  = int(rdumii)
                   dumii  = min(149,dumii)
                   mu_r    = mu_r_table(dumii)+(mu_r_table(dumii+1)-mu_r_table(dumii))*(rdumii-real(dumii))
                elseif (dum.ge.502.e-6) then
                   mu_r    = 0.
                endif
! recalculate slope based on mu_r
!               LAMR = (pi*sxth*rhow*nr(i_Qnorm,k)*gamma(mu_r+4.)/(qr(i_Qnorm,k)*gamma(mu_r+1.)))**thrd

! this is done by re-scaling lamv to account for DSD shape (mu_r)
                lamr   = lamv*(gamma(mu_r+4.)/(6.*gamma(mu_r+1.)))**thrd

! set maximum value for rain lambda
!               lammax = (mu_r+1.)/10.e-6
                lammax = (mu_r+1.)*1.e+5

! set to small value since breakup is explicitly included (mean size 5 mm)
!               lammin = (mu_r+1.)/5000.e-6
                lammin = (mu_r+1.)*200.
                lamr   = min(lamr,lammax)
                lamr   = max(lamr,lammin)

! initialize sum
                sum1 = 0.
                sum2 = 0.
                sum6 = 0.

! total rain
!               sum8=0.

                dd = 20.e-6

                do jj=1,1000
! particle size
                   d1 = real(jj)*20.*1.e-6 - 10.e-6

! num is the scaled binned rain size distribution
! we need to multiply by n0r to get unscaled distribution
                   num(jj) = d1**mu_r*exp(-lamr*d1)*dd

! get (unscaled) binned ice size distribution
! bug fix hm, 2/10/12
                   numi(jj) = n0*d1**mu_i*exp(-lam*d1)*dd
                enddo !jj-loop

! loop over rain and ice size distributions
                do jj = 1,1000
                   do kk = 1,1000

! particle size
                      d1 = real(jj)*20.*1.e-6 - 10.e-6  ! ice
                      d2 = real(kk)*20.*1.e-6 - 10.e-6  ! rain

                      if (d1.le.dcrit) then
                         cs1  = pi*sxth*900.
                         ds1  = 3.
                         bas1 = 2.
                         aas1 = pi*0.25
                      elseif (d1.gt.dcrit.and.d1.le.dcrits) then
                         cs1  = cs
                         ds1  = ds
                         bas1 = bas
                         aas1 = aas
                      elseif (d1.gt.dcrits.and.d1.le.dcritr) then
                         cs1  = cgp(i_rhor)
                         ds1  = dg
                         bas1 = bag
                         aas1 = aag
                      else if (d1.gt.dcritr) then
                         cs1  = csr
                         ds1  = dsr
                         if (i_Fr.eq.1) then
                            aas1 = aas
                            bas1 = bas
                         else
! for area, keep bas1 constant, but modify aas1 accordingto rimed fraction
                            bas1 = bas
                            dum1 = aas*d1**bas
                            dum2 = aag*d1**bag
                            m1   = cs1*d1**ds1
                            m2   = cs*d1**ds
                            m3   = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                            dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                            aas1 = dum3/(d1**bas)
                         endif
                      endif

! absolute value, differential fallspeed
                      delu = abs(fall2(jj)-fallr(kk))

! collection of rain mass and number
! allow collection of rain both when rain fallspeed > ice fallspeed
! and ice fallspeed > rain fallspeed
! this is applied below freezing to calculate total amount of rain
! mass and number that collides with ice and freezes

!        if (fall2(jj).ge.fallr(kk)) then

! sum for integral

! change in rain N (units of m^4 s^-1 kg^-1), thus need to multiply
! by air density (units kg m^-3) and n0r (units kg^-1 m^-1) in the
! microphysics code

!                     sum1 = sum1+(aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*        &
!                            exp(-lam*d1)* &dd*num(kk)
                      sum1 = sum1+(aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*    &
                             exp(-lam*d1)*dd*num(kk)
!                     sum1 = sum1+min((aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*    &
!                            exp(-lam*d1)*dd*num(kk),num(kk))

! change in rain q (units of m^4 s^-1), again need to multiply
! by air density and n0r in the microphysics code

!                     sum2 = sum2+(aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*        &
!                            exp(-lam*d1)*dd*num(kk)*pi*sxth*997.*d2**3
                      sum2 = sum2+(aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*    &
                             exp(-lam*d1)*dd*num(kk)*pi*sxth*997.*d2**3

! remove collected rain drops from distribution
!                      num(kk) = num(kk)-(aas1*d1**bas1+pi*0.25*d2**2)*delu*n0*d1**mu_i*  &
!                                exp(-lam*d1)*dd*num(kk)*dt
!                      num(kk) = max(num(kk),0.)

!......................................................
! now calculate collection of ice mass by rain

! ice collecting rain
! again, allow collection both when ice fallspeed > rain fallspeed
! and when rain fallspeed > ice fallspeed
! this is applied to conditions above freezing to calculate
! acceleration of melting due to collisions with liquid (rain)

!        if (fall2(jj).ge.fallr(kk)) then

! collection of ice number

!        sum5=sum5+(aas1*d1**bas1+pi/4.*d2**2)*delu*
!     1  exp(-lamr*d2)*dd*numi(jj)

! collection of ice mass (units of m^4 s^-1), need to multiply by air density and n0r in microphysics code
                      sum6 = sum6+(aas1*d1**bas1+pi*0.25*d2**2)*delu*d2**mu_r*          &
                             exp(-lamr*d2)*dd*numi(jj)*cs1*d1**ds1

! remove collected snow from distribution
!                      numi(jj) = numi(jj)-(aas1*d1**bas1+pi*0.25*d2**2)*delu*d2**mu_r*       &
!                                 exp(-lamr*d2)*dd*numi(jj)*dt
!                      numi(jj) = max(numi(jj),0.)

                   enddo !kk-loop
                enddo !jj-loop

! save for output
                nrrain(i_Qnorm,i_Drscale,i_Fr) = sum1
                qrrain(i_Qnorm,i_Drscale,i_Fr) = sum2
                qsrain(i_Qnorm,i_Drscale,i_Fr) = sum6

             enddo !i_Drscale-loop  (loop around lambda for rain)

!
!.....................................................................................
! vapor deposition/melting/wet growth
!.....................................................................................
! vapor deposition including ventilation effects
! note: in microphysics code we need to multiply by air density
! and (mu/dv)^0.3333*(rhofac/mu)^0.5, where rhofac is air density
! correction factor

             sum1 = 0.
             sum2 = 0.
             dd   = 2.e-6

! loop over exponential size distribution
             do jj = 1,10000

! particle size
                d1 = real(jj)*2.*1.e-6 - 1.e-6

! get capacitance for different ice regimes
                if (d1.le.dcrit) then
                   cap = 1. ! for small spherical crystal use sphere
                elseif (d1.gt.dcrit.and.d1.le.dcrits) then
                   cap = 0.48  ! field et al. 2006
                elseif (d1.gt.dcrits.and.d1.le.dcritr) then
                   cap = 1. ! for graupel assume sphere
                elseif (d1.gt.dcritr) then
                   cs1 = csr
                   ds1 = dsr
                   if (i_Fr.eq.1) then
                      cap  = 0.48
                   else
                      dum1 = 0.48
                      dum2 = 1.
                      m1   = cs1*d1**ds1
                      m2   = cs*d1**ds
                      m3   = cgp(i_rhor)*d1**dg
! linearly interpolate to get capacitance based on particle mass
                      dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                      cap  = dum3
                   endif
                endif

! hm 1/19/13 for ventilation, only include fallspeed
! and size effects, the rest of the term
! Sc^1/3 x Re^1/2 is multiplied in-line in the model code
! to allow effects of atmospheric conditions on ventilation
!               dum = (mu/dv)**0.333333*(fall1(jj)*d1/mu)**0.5
                dum = (fall1(jj)*d1)**0.5

! ventilation from hall and pruppacher 1976
! hm 1/19/13 only include ventilation for super-100 micron particles
!        if (dum.lt.1.) then

! units are m^3 kg^-1 s^-1, thus multiplication by air density in
! microphysics code gives s^-1

                if (d1.lt.100.e-6) then
                   sum1 = sum1+cap*n0*d1**(mu_i+1.)*exp(-lam*d1)*dd
                else
!                  sum1 = sum1+cap*n0*(0.65+0.44*dum)*d1**(mu_i+1.)*exp(-lam*d1)*dd
                   sum1 = sum1+cap*n0*0.65*d1**(mu_i+1.)*exp(-lam*d1)*dd
                   sum2 = sum2+cap*n0*0.44*dum*d1**(mu_i+1.)*exp(-lam*d1)*dd
                endif

             enddo !jj-loop

             vdep(i_Qnorm,i_Fr) = sum1
             vdep1(i_Qnorm,i_Fr) = sum2

             print*,'vdep',vdep(i_Qnorm,i_Fr)

!.....................................................................................
! ice effective radius
! use definition of Francis et al. (1994), e.g., Eq. 3.11 in Fu (1996) J. Climate
!.....................................................................................
             sum1 = 0.
             sum2 = 0.
             dd   = 2.e-6

! loop over exponential size distribution
             do jj = 1,10000

! particle size
                d1 = real(jj)*2.*1.e-6 - 1.e-6

                if (d1.le.dcrit) then
                   cs1  = pi*sxth*900.
                   ds1  = 3.
                   bas1 = 2.
                   aas1 = pi*0.25
                elseif (d1.gt.dcrit.and.d1.le.dcrits) then
                   cs1  = cs
                   ds1  = ds
                   bas1 = bas
                   aas1 = aas
                elseif (d1.gt.dcrits.and.d1.le.dcritr) then
                   cs1  = cgp(i_rhor)
                   ds1  = dg
                   bas1 = bag
                   aas1 = aag
                elseif (d1.gt.dcritr) then
                   cs1  = csr
                   ds1  = dsr
                   if (i_Fr.eq.1) then
                      bas1 = bas
                      aas1 = aas
                   else
! for area, keep bas1 constant, but modify aas1 according to rimed fraction
                      bas1  = bas
                      dum1  = aas*d1**bas
                      dum2  = aag*d1**bag
                      m1    = cs1*d1**ds1
                      m2    = cs*d1**ds
                      m3    = cgp(i_rhor)*d1**dg
! linearly interpolate based on particle mass
                      dum3  = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
                      aas1  = dum3/(d1**bas)
                   endif
                endif

! n0 not included below becuase it is in both numerator and denominator
!               cs1=pi*sxth*917.
!               ds1=3.
!               aas1=pi/4.*2.
!               bas1=2.

                sum1 = sum1+cs1*d1**ds1*d1**mu_i*exp(-lam*d1)*dd
                sum2 = sum2+aas1*d1**bas1*d1**mu_i*exp(-lam*d1)*dd

!               if (d1.ge.100.e-6) then
!                  sum3 = sum3+n0*d1**mu_i*exp(-lam*d1)*dd
!                  sum4 = sum4+n0*aas1*d1**bas1*d1**mu_i*exp(-lam*d1)*dd
!               endif

             enddo !jj-loop

! calculate eff radius

!            eff(i_Qnorm,i_Fr) = sum1/(1.7321*916.7*sum2)
! hm 4/9/09, calculate effective size following Fu (1996)
!            eff(i_Qnorm,i_Fr) = sum1/(1.1547*916.7*sum2)
! hm, calculate for eff rad for twp ice
             eff(i_Qnorm,i_Fr) = 3.*sum1/(4.*sum2*916.7)

!            a_100(i_Qnorm,i_Fr)=sum4
!            n_100(i_Qnorm,i_Fr)=sum3

             print*,'eff rad',eff(i_Qnorm,i_Fr)
!.....................................................................................

522  continue

         !-- this column is not used (for v2.2 and after)
         !   (kept temporarly in order to preserve order of columns in lookup_table_1)
             nsave(i_Qnorm,i_Fr) = 1.  ! HM, set to 1 for consistency w/ old lookup table file (to verify bit matching)
         !==
             qsave(i_Qnorm,i_Fr) = q
             lsave(i_Qnorm,i_Fr) = lam

       enddo !i_Qnorm-loop

    enddo !i_Fr-loop

! output variables to ascii lookup table

222 format(2i5,15e15.5)
223 format(2i5,6e15.5)

!-- ice table
    do i_Fr=1,n_Fr

       do i_Qnorm = 1,n_Qnorm

! set values less than 1.e-99 set to 0, otherwise the 'E' is left off in write statements for floting point
! numbers using some compilers
          if (qsave(i_Qnorm,i_Fr).lt.1.e-99) qsave(i_Qnorm,i_Fr)=0.
          if (nsave(i_Qnorm,i_Fr).lt.1.e-99) nsave(i_Qnorm,i_Fr)=0.
          if (uns(i_Qnorm,i_Fr).lt.1.e-99) uns(i_Qnorm,i_Fr)=0.
          if (ums(i_Qnorm,i_Fr).lt.1.e-99) ums(i_Qnorm,i_Fr)=0.
          if (nagg(i_Qnorm,i_Fr).lt.1.e-99) nagg(i_Qnorm,i_Fr)=0.
          if (nrwat(i_Qnorm,i_Fr).lt.1.e-99) nrwat(i_Qnorm,i_Fr)=0.
          if (vdep(i_Qnorm,i_Fr).lt.1.e-99) vdep(i_Qnorm,i_Fr)=0.
          if (eff(i_Qnorm,i_Fr).lt.1.e-99) eff(i_Qnorm,i_Fr)=0.
          if (nlarge(i_Qnorm,i_Fr).lt.1.e-99) nlarge(i_Qnorm,i_Fr)=0.
          if (nsmall(i_Qnorm,i_Fr).lt.1.e-99) nsmall(i_Qnorm,i_Fr)=0.
          if (lsave(i_Qnorm,i_Fr).lt.1.e-99) lsave(i_Qnorm,i_Fr)=0.
          if (refl(i_Qnorm,i_Fr).lt.1.e-99) refl(i_Qnorm,i_Fr)=0.
          if (vdep1(i_Qnorm,i_Fr).lt.1.e-99) vdep1(i_Qnorm,i_Fr)=0.
          if (dmm(i_Qnorm,i_Fr).lt.1.e-99) dmm(i_Qnorm,i_Fr)=0.
          if (rhomm(i_Qnorm,i_Fr).lt.1.e-99) rhomm(i_Qnorm,i_Fr)=0.


             write(1,222)i_rhor,i_Fr,qsave(i_Qnorm,i_Fr),nsave(i_Qnorm,i_Fr),uns(i_Qnorm,i_Fr),            &
                         ums(i_Qnorm,i_Fr),nagg(i_Qnorm,i_Fr),nrwat(i_Qnorm,i_Fr),vdep(i_Qnorm,i_Fr),      &
                         eff(i_Qnorm,i_Fr),nlarge(i_Qnorm,i_Fr),nsmall(i_Qnorm,i_Fr),lsave(i_Qnorm,i_Fr),  &
                         refl(i_Qnorm,i_Fr),vdep1(i_Qnorm,i_Fr),dmm(i_Qnorm,i_Fr),rhomm(i_Qnorm,i_Fr)
       enddo !i-loop

   !-- ice-rain collection table:
       do i_Qnorm = 1,n_Qnorm
          do i_Drscale=1,n_Drscale

! set values less than 1.e-99 set to 0, otherwise the 'E' is left off in write statements for floting point
! numbers using some compilers
          if (qsave(i_Qnorm,i_Fr).lt.1.e-99) qsave(i_Qnorm,i_Fr)=0.
          if (nsave(i_Qnorm,i_Fr).lt.1.e-99) nsave(i_Qnorm,i_Fr)=0.             
          if (lamrs(i_Drscale).lt.1.e-99) lamrs(i_Drscale)=0.
          if (nrrain(i_Qnorm,i_Drscale,i_Fr).lt.1.e-99) nrrain(i_Qnorm,i_Drscale,i_Fr)=0.
          if (qrrain(i_Qnorm,i_Drscale,i_Fr).lt.1.e-99) qrrain(i_Qnorm,i_Drscale,i_Fr)=0.
          if (qsrain(i_Qnorm,i_Drscale,i_Fr).lt.1.e-99) qsrain(i_Qnorm,i_Drscale,i_Fr)=0.

             write(1,223) i_rhor,i_Fr,qsave(i_Qnorm,i_Fr),nsave(i_Qnorm,i_Fr),lamrs(i_Drscale),            &
                          nrrain(i_Qnorm,i_Drscale,i_Fr),qrrain(i_Qnorm,i_Drscale,i_Fr),                   &
                          qsrain(i_Qnorm,i_Drscale,i_Fr)
          enddo !i_Drscale-loop
       enddo !i-loop

    enddo !i_Fr-loop  (riming fraction)

 enddo !i_rhor-loop (main loop over variable rime density)


END PROGRAM make_p3_lookuptable1
!______________________________________________________________________________________

! Incomplete gamma function
! from Numerical Recipes in Fortran 77: The Art of
! Scientific Computing

      function gammq(a,x)

      real a,gammq,x

! USES gcf,gser
! Returns the incomplete gamma function Q(a,x) = 1-P(a,x)

      real gammcf,gammser,gln
      if (x.lt.0..or.a.le.0) pause 'bad argument in gammq'
      if (x.lt.a+1.) then
         call gser(gamser,a,x,gln)
         gammq=1.-gamser
      else
         call gcf(gammcf,a,x,gln)
         gammq=gammcf
      end if
      return
      end

!-------------------------------------

      subroutine gser(gamser,a,x,gln)
      integer itmax
      real a,gamser,gln,x,eps
      parameter(itmax=100,eps=3.e-7)
      integer n
      real ap,del,sum,gamma
      gln = log(gamma(a))
      if (x.le.0.) then
         if (x.lt.0.) pause 'x < 0 in gser'
         gamser = 0.
         return
      end if
      ap=a
      sum=1./a
      del=sum
      do n=1,itmax
         ap=ap+1.
         del=del*x/ap
         sum=sum+del
         if (abs(del).lt.abs(sum)*eps) goto 1
      end do
      pause 'a too large, itmax too small in gser'
 1    gamser=sum*exp(-x+a*log(x)-gln)
      return
      end

!-------------------------------------

      subroutine gcf(gammcf,a,x,gln)
      integer itmax
      real a,gammcf,gln,x,eps,fpmin
      parameter(itmax=100,eps=3.e-7,fpmin=1.e-30)
      integer i
      real an,b,c,d,del,h,gamma
      gln=log(gamma(a))
      b=x+1.-a
      c=1./fpmin
      d=1./b
      h=d
      do i=1,itmax
         an=-i*(i-a)
         b=b+2.
         d=an*d+b
         if(abs(d).lt.fpmin) d=fpmin
         c=b+an/c
         if(abs(c).lt.fpmin) c=fpmin
         d=1./d
         del=d*c
         h = h*del
         if(abs(del-1.).lt.eps)goto 1
      end do
      pause 'a too large, itmax too small in gcf'
 1    gammcf=exp(-x+a*log(x)-gln)*h
      return
      end

!-------------------------------------

      REAL FUNCTION gamma(X)


!D    DOUBLE PRECISION FUNCTION Dgamma(X)
!----------------------------------------------------------------------
!
! THIS ROUTINE CALCULATES THE gamma FUNCTION FOR A REAL ARGUMENT X.
!   COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN REFERENCE 1.
!   THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE gamma
!   FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS
!   FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.
!   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.
!   THE ACCURACY ACHIEVED DEPENDS ON THE ARITHMETIC SYSTEM, THE
!   COMPILER, THE INTRINSIC FUNCTIONS, AND PROPER SELECTION OF THE
!   MACHINE-DEPENDENT CONSTANTS.
!
!
!----------------------------------------------------------------------
!
! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
!
! BETA   - RADIX FOR THE FLOATING-POINT REPRESENTATION
! MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS
! XBIG   - THE LARGEST ARGUMENT FOR WHICH gamma(X) IS REPRESENTABLE
!          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION
!                  gamma(XBIG) = BETA**MAXEXP
! XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER;
!          APPROXIMATELY BETA**MAXEXP
! EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
!          1.0+EPS .GT. 1.0
! XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
!          1/XMININ IS MACHINE REPRESENTABLE
!
!     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:
!
!                            BETA       MAXEXP        XBIG
!
! CRAY-1         (S.P.)        2         8191        966.961
! CYBER 180/855
!   UNDER NOS    (S.P.)        2         1070        177.803
! IEEE (IBM/XT,
!   SUN, ETC.)   (S.P.)        2          128        35.040
! IEEE (IBM/XT,
!   SUN, ETC.)   (D.P.)        2         1024        171.624
! IBM 3033       (D.P.)       16           63        57.574
! VAX D-FORMAT   (D.P.)        2          127        34.844
! VAX G-FORMAT   (D.P.)        2         1023        171.489
!
!                            XINF         EPS        XMININ
!
! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
! CYBER 180/855
!   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
! IEEE (IBM/XT,
!   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38
! IEEE (IBM/XT,
!   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
! VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39
! VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308
!
!----------------------------------------------------------------------
!
! ERROR RETURNS
!
!  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
!     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
!     TO BE FREE OF UNDERFLOW AND OVERFLOW.
!
!
!  INTRINSIC FUNCTIONS REQUIRED ARE:
!
!     INT, DBLE, EXP, LOG, REAL, SIN
!
!
! REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
!              FUNCTIONS   W. J. CODY, LECTURE NOTES IN MATHEMATICS,
!              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON
!              (ED.), SPRINGER VERLAG, BERLIN, 1976.
!
!              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND
!              SONS, NEW YORK, 1968.
!
!  LATEST MODIFICATION: OCTOBER 12, 1989
!
!  AUTHORS: W. J. CODY AND L. STOLTZ
!           APPLIED MATHEMATICS DIVISION
!           ARGONNE NATIONAL LABORATORY
!           ARGONNE, IL 60439
!
!----------------------------------------------------------------------
      INTEGER I,N
      LOGICAL PARITY
      REAL                  &
!D    DOUBLE PRECISION
          C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE, &
          TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/,  &
           SQRTPI/0.9189385332046727417803297E0/,                     &
           PI/3.1415926535897932384626434E0/
!D    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
!D   1     SQRTPI/0.9189385332046727417803297D0/,
!D   2     PI/3.1415926535897932384626434D0/
!----------------------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS
!----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,         &
           XINF/3.4E38/
!D    DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,
!D   1     XINF/1.79D308/
!----------------------------------------------------------------------
!  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
!     APPROXIMATION OVER (1,2).
!----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
            -1.01515636749021914166146E+3,-3.10777167157231109440444E+3,  &
              2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
!D    DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
!D   1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
!D   2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
!D   3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
!D    DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
!D   1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
!D   2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
!D   3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
!----------------------------------------------------------------------
!  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
!----------------------------------------------------------------------
      DATA C/-1.910444077728E-03,8.4171387781295E-04,                     &
           -5.952379913043012E-04,7.93650793500350248E-04,                &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02,   &
            5.7083835261E-03/
!D    DATA C/-1.910444077728D-03,8.4171387781295D-04,
!D   1     -5.952379913043012D-04,7.93650793500350248D-04,
!D   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
!D   3      5.7083835261D-03/
!----------------------------------------------------------------------
!  STATEMENT FUNCTIONS FOR CONVERSION BETWEEN INTEGER AND FLOAT
!----------------------------------------------------------------------
      CONV(I) = REAL(I)
!D    CONV(I) = DBLE(I)
      PARITY=.FALSE.
      FACT=ONE
      N=0
      Y=X
      IF(Y.LE.ZERO)THEN
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
        Y=-X
        Y1=AINT(Y)
        RES=Y-Y1
        IF(RES.NE.ZERO)THEN
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.
          FACT=-PI/SIN(PI*RES)
          Y=Y+ONE
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
      IF(Y.LT.EPS)THEN
!----------------------------------------------------------------------
!  ARGUMENT .LT. EPS
!----------------------------------------------------------------------
        IF(Y.GE.XMININ)THEN
          RES=ONE/Y
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ELSEIF(Y.LT.TWELVE)THEN
        Y1=Y
        IF(Y.LT.ONE)THEN
!----------------------------------------------------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          Z=Y
          Y=Y+ONE
        ELSE
!----------------------------------------------------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
!----------------------------------------------------------------------
          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        ENDIF
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
!----------------------------------------------------------------------
        XNUM=ZERO
        XDEN=ONE
        DO 260 I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
  260   CONTINUE
        RES=XNUM/XDEN+ONE
        IF(Y1.LT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          RES=RES/Y1
        ELSEIF(Y1.GT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
!----------------------------------------------------------------------
          DO 290 I=1,N
            RES=RES*Y
            Y=Y+ONE
  290     CONTINUE
        ENDIF
      ELSE
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,
!----------------------------------------------------------------------
        IF(Y.LE.XBIG)THEN
          YSQ=Y*Y
          SUM=C(7)
          DO 350 I=1,6
            SUM=SUM/YSQ+C(I)
  350     CONTINUE
          SUM=SUM/Y-Y+SQRTPI
          SUM=SUM+(Y-HALF)*LOG(Y)
          RES=EXP(SUM)
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF
!----------------------------------------------------------------------
!  FINAL ADJUSTMENTS AND RETURN
!----------------------------------------------------------------------
      IF(PARITY)RES=-RES
      IF(FACT.NE.ONE)RES=FACT/RES
  900 gamma=RES
!D900 Dgamma = RES
      RETURN
! ---------- LAST LINE OF gamma ----------
      END


!--------------------------------------------------------------------------
! subroutine get_mass_size
!
! !----- get mass-size and projected area-size relationships for given size (d1)
!           if (d1.le.dcrit) then
!              cs1 = pi*sxth*900.
!              ds1 = 3.
!              bas1 = 2.
!              aas1 = pi/4.
!           else if (d1.gt.dcrit.and.d1.le.dcrits) then
!              cs1  = cs
!              ds1  = ds
!              bas1 = bas
!              aas1 = aas
!           else if (d1.gt.dcrits.and.d1.le.dcritr) then
!               cs1  = cgp(i_rhor)
!               ds1  = dg
!               bas1 = bag
!               aas1 = aag
!           else if (d1.gt.dcritr) then
!              cs1 = csr
!              ds1 = dsr
!              if (i_Fr.eq.1) then
!                 aas1 = aas
!                 bas1 = bas
!              else
!
! ! for projected area, keep bas1 constant, but modify aas1 according to rimed fraction
!                 bas1 = bas
!                 dum1 = aas*d1**bas
!                 dum2 = aag*d1**bag
!                 m1   = cs1*d1**ds1
!                 m2   = cs*d1**ds
!                 m3   = cgp(i_rhor)*d1**dg
! ! linearly interpolate based on particle mass
!                 dum3 = dum1+(m1-m2)*(dum2-dum1)/(m3-m2)
! !               dum3 = (1.-Fr)*dum1+Fr*dum2
!                 aas1 = dum3/(d1**bas)
!              endif
!           endif
! !=====
!
! end subroutine get_mass_size
