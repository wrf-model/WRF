! lke: corrections to aerosol rxns and clean-up

      module mo_usrrxt

      private
      public :: usrrxt_inti, usrrxt

      save

      integer :: usr1_ndx, usr2_ndx, usr3_ndx, usr5_ndx, usr6_ndx, usr7_ndx
      integer :: usr8_ndx, usr9_ndx, usr11_ndx, usr12_ndx, usr14_ndx, usr15_ndx
      integer :: usr16_ndx, usr17_ndx, usr21_ndx, usr22_ndx
      integer :: usr23_ndx, usr24_ndx, usr25_ndx, usr26_ndx, usr27_ndx, usr28_ndx
      integer :: usr17a_ndx
      integer :: so4_ndx, cb2_ndx, oc2_ndx, soa_ndx, nit_ndx
      logical :: has_aerosols

      contains

      subroutine usrrxt_inti
!-----------------------------------------------------------------
!        ... intialize the user reaction constants module
!-----------------------------------------------------------------

      use mo_chem_utls, only : get_rxt_ndx, get_spc_ndx

      implicit none

      usr1_ndx   = get_rxt_ndx( 'usr1' )
      usr2_ndx   = get_rxt_ndx( 'usr2' )
      usr3_ndx   = get_rxt_ndx( 'usr3' )
      usr5_ndx   = get_rxt_ndx( 'usr5' )
      usr6_ndx   = get_rxt_ndx( 'usr6' )
      usr7_ndx   = get_rxt_ndx( 'usr7' )
      usr8_ndx   = get_rxt_ndx( 'usr8' )
      usr9_ndx   = get_rxt_ndx( 'usr9' )
      usr11_ndx  = get_rxt_ndx( 'usr11' )
      usr12_ndx  = get_rxt_ndx( 'usr12' )
      usr14_ndx  = get_rxt_ndx( 'usr14' )
      usr15_ndx  = get_rxt_ndx( 'usr15' )
      usr16_ndx  = get_rxt_ndx( 'usr16' )
      usr17_ndx  = get_rxt_ndx( 'usr17' )
      usr21_ndx  = get_rxt_ndx( 'usr21' )
      usr22_ndx  = get_rxt_ndx( 'usr22' )
      usr17a_ndx = get_rxt_ndx( 'usr17a' )
      usr23_ndx  = get_rxt_ndx( 'usr23' )
      usr24_ndx  = get_rxt_ndx( 'usr24' )
      usr25_ndx  = get_rxt_ndx( 'usr25' )
      usr26_ndx  = get_rxt_ndx( 'usr26' )
      usr27_ndx  = get_rxt_ndx( 'usr27' )
      usr28_ndx  = get_rxt_ndx( 'usr28' )
      so4_ndx    = get_spc_ndx( 'SO4' )
      cb2_ndx    = get_spc_ndx( 'CB2' )
      oc2_ndx    = get_spc_ndx( 'OC2' )
      soa_ndx    = get_spc_ndx( 'SOA' )
      nit_ndx    = get_spc_ndx( 'NH4NO3' )

      write(*,*) ' '
      write(*,*) 'usrrxt_inti: diagnostics '
      write(*,'(10i5)') usr1_ndx, usr2_ndx, usr3_ndx, usr5_ndx, usr6_ndx, usr7_ndx, &
                        usr8_ndx, usr9_ndx, usr11_ndx, usr12_ndx, usr14_ndx, usr15_ndx, &
                        usr16_ndx, usr17_ndx, usr21_ndx, usr22_ndx

      end subroutine usrrxt_inti

      subroutine usrrxt( rxt, temp, invariants, h2ovmr,  ps, &
                         pmid, m, sulfate, vmr, mmr, &
                         relhum, strato_sad, ltrop, lat, ip, plonl, sad_total )

!-----------------------------------------------------------------
!        ... set the user specified reaction rates
!-----------------------------------------------------------------

      use chem_mods,    only : nfs, rxntot
      use mo_setinv,    only : has_h2o, h2o_ndx
      use mo_grid,      only : plev, pcnstm1, pcnst
      use mo_constants, only : pi, avo => avogadro, boltz
 
      implicit none

!-----------------------------------------------------------------
!        ... dummy arguments
!-----------------------------------------------------------------
      integer, intent(in) :: lat, ip
      integer, intent(in) :: plonl
      integer, intent(in) :: ltrop(plonl)                ! tropopause vertical index
      real, intent(in)    :: temp(plonl,plev)            ! temperature (K)
      real, intent(in)    :: m(plonl,plev)               ! total atm density (/cm^3)
      real, intent(in)    :: sulfate(plonl,plev)         ! sulfate aerosol (mol/mol)
      real, intent(in)    :: strato_sad(plonl,plev)      ! stratospheric aerosol sad (1/cm)
      real, intent(in)    :: h2ovmr(plonl,plev)          ! water vapor (mol/mol)
      real, intent(in)    :: relhum(plonl,plev)          ! relative humidity
      real, intent(in)    :: pmid(plonl,plev)            ! midpoint pressure (Pa)
      real, intent(in)    :: ps(plonl)                   ! surface pressure (Pa)
      real, intent(in)    :: invariants(plonl,plev,nfs)  ! invariants density (/cm^3)
      real, intent(in)    :: vmr(plonl,plev,pcnstm1)     ! species concentrations (mol/mol)
      real, intent(in)    :: mmr(plonl,plev,pcnst  )     ! species concentrations (kg/kg)
      real, intent(inout) :: rxt(plonl,plev,rxntot)      ! gas phase rates
      real, intent(inout) :: sad_total(plonl,plev)       ! total surface area density (cm2/cm3)
      
!-----------------------------------------------------------------
!        ... local variables
!-----------------------------------------------------------------

      real, parameter :: dg = 0.1                        ! mole diffusion =0.1 cm2/s (Dentener, 1993)
      real, parameter :: mw_so4 = 98.e-3                 ! so4 molecular wt (kg/mole)

!-----------------------------------------------------------------
! 	... parameters for log-normal distribution by number
! references:
!   Chin et al., JAS, 59, 461, 2003
!   Liao et al., JGR, 108(D1), 4001, 2003
!   Martin et al., JGR, 108(D3), 4097, 2003
!-----------------------------------------------------------------
      real, parameter :: rm_sulf  = 6.95e-6        ! mean radius of sulfate particles (cm) (Chin)
      real, parameter :: sd_sulf  = 2.03           ! standard deviation of radius for sulfate (Chin)
      real, parameter :: rho_sulf = 1.7e3          ! density of sulfate aerosols (kg/m3) (Chin) 

      real, parameter :: rm_orgc  = 2.12e-6        ! mean radius of organic carbon particles (cm) (Chin)
      real, parameter :: sd_orgc  = 2.20           ! standard deviation of radius for OC (Chin)
      real, parameter :: rho_orgc = 1.8e3          ! density of OC aerosols (kg/m3) (Chin)

      real, parameter :: rm_bc    = 1.18e-6        ! mean radius of soot/BC particles (cm) (Chin)
      real, parameter :: sd_bc    = 2.00           ! standard deviation of radius for BC (Chin)
      real, parameter :: rho_bc   = 1.0e3          ! density of BC aerosols (kg/m3) (Chin)

!-----------------------------------------------------------------
! 	... reaction probabilities for heterogeneous reactions
!-----------------------------------------------------------------
      real, parameter :: gamma_n2o5 = 0.10         ! from Jacob, Atm Env, 34, 2131, 2000
      real, parameter :: gamma_ho2  = 0.20         ! 
      real, parameter :: gamma_no2  = 0.0001       ! 
      real, parameter :: gamma_no3  = 0.001        ! 

!-----------------------------------------------------------------
! 	... table for hygroscopic growth effect on radius (Chin et al)
!           (no growth effect for mineral dust)
!-----------------------------------------------------------------
      real, dimension(7) :: table_rh, table_rfac_sulf, table_rfac_bc, table_rfac_oc, table_rfac_ss

      data table_rh(1:7)        / 0.0, 0.5, 0.7, 0.8, 0.9, 0.95, 0.99/
      data table_rfac_sulf(1:7) / 1.0, 1.4, 1.5, 1.6, 1.8, 1.9,  2.2/
      data table_rfac_oc(1:7)   / 1.0, 1.2, 1.4, 1.5, 1.6, 1.8,  2.2/
      data table_rfac_bc(1:7)   / 1.0, 1.0, 1.0, 1.2, 1.4, 1.5,  1.9/
      data table_rfac_ss(1:7)   / 1.0, 1.6, 1.8, 2.0, 2.4, 2.9,  4.8/

      integer  ::  i, k
      real     ::  tp(plonl)                       ! 300/t
      real     ::  tinv(plonl)                     ! 1/t
      real     ::  ko(plonl)   
      real     ::  kinf(plonl)   
      real     ::  fc(plonl)   
      real     ::  sqrt_t(plonl)                   ! sqrt( temp )
      real     ::  exp_fac(plonl)                  ! vector exponential

      real     ::  v, rho_air, n, n_exp, r_rd, r_sd
      real     ::  dm_sulf, dm_sulf_wet, log_sd_sulf, sfc_sulf, sfc_nit
      real     ::  dm_orgc, dm_orgc_wet, log_sd_orgc, sfc_oc, sfc_soa
      real     ::  dm_bc, dm_bc_wet, log_sd_bc, sfc_bc
      real     ::  rxt_sulf, rxt_nit, rxt_oc, rxt_soa
      real     ::  c_n2o5, c_ho2, c_no2, c_no3
      real     ::  s_exp

      integer  ::  irh, rh_l, rh_u
      real     ::  factor, rfac_sulf, rfac_oc, rfac_bc, rfac_ss

!-----------------------------------------------------------------
!	... o + o2 + m --> o3 + m
!-----------------------------------------------------------------
level_loop : &
      do k = 1,plev
         tinv(:)           = 1. / temp(:,k)
         tp(:)             = 300. * tinv(:)
         sqrt_t(:)         = sqrt( temp(:,k) )
         if( usr1_ndx > 0 ) then
            rxt(:,k,usr1_ndx) = 6.e-34 * tp(:)**2.4
         end if
#ifdef IBM
!-----------------------------------------------------------------
!	... n2o5 + m --> no2 + no3 + m
!-----------------------------------------------------------------
         if( usr3_ndx > 0 ) then
            if( usr2_ndx > 0 ) then
               call vexp( exp_fac, -10990.*tinv, plonl )
               rxt(:,k,usr3_ndx) = rxt(:,k,usr2_ndx) * 3.333e26 * exp_fac(:)
            else
               rxt(:,k,usr3_ndx) = 0.
            end if
         end if

!-----------------------------------------------------------------
!	set rates for:
! 	... hno3 + oh --> no3 + h2o
!           ho2no2 + m --> ho2 + no2 + m
!           co + oh --> co2 + ho2
!-----------------------------------------------------------------
	 if( usr5_ndx > 0 ) then
	    call vexp( exp_fac, 1335.*tinv, plonl )
            ko(:) = m(:,k) * 6.5e-34 * exp_fac(:)
	    call vexp( exp_fac, 2199.*tinv, plonl )
            ko(:) = ko(:) / (1. + ko(:)/(2.7e-17*exp_fac(:)))
	    call vexp( exp_fac, 460.*tinv, plonl )
            rxt(:,k,usr5_ndx) = ko(:) + 2.4e-14*exp_fac(:)
	 end if
	 if( usr7_ndx > 0 ) then
	    if( usr6_ndx > 0 ) then
	       call vexp( exp_fac, -10900.*tinv, plonl )
               rxt(:,k,usr7_ndx) = rxt(:,k,usr6_ndx) * exp_fac(:) / 2.1e-27
	    else
               rxt(:,k,usr7_ndx) = 0.
	    end if
	 end if
	 if( usr8_ndx > 0 ) then
            rxt(:,k,usr8_ndx) = 1.5e-13 * (1. + 6.e-7*boltz*m(:,k)*temp(:,k))
	 end if

!-----------------------------------------------------------------
!	... ho2 + ho2 --> h2o2
!	note: this rate involves the water vapor number density
!-----------------------------------------------------------------
         if( usr9_ndx > 0 ) then
	    if( has_h2o ) then
	       call vexp( exp_fac, 600.*tinv, plonl )
               ko(:)   = 2.3e-13 * exp_fac(:)
	       call vexp( exp_fac, 1000.*tinv, plonl )
               kinf(:) = 1.7e-33 * m(:,k) * exp_fac(:)
	       call vexp( exp_fac, 2200.*tinv, plonl )
               fc(:)   = 1. + 1.4e-21 * invariants(:,k,h2o_ndx) * exp_fac(:)
               rxt(:,k,usr9_ndx) = (ko(:) + kinf(:)) * fc(:)
	    else
               rxt(:,k,usr9_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!    	... mco3 + no2 -> mpan
!-----------------------------------------------------------------
	 if( usr14_ndx > 0 ) then
            rxt(:,k,usr14_ndx) = 1.1e-11 * tp(:) / m(:,k)
	 end if

!-----------------------------------------------------------------
!	... pan + m --> ch3co3 + no2 + m
!-----------------------------------------------------------------
	 call vexp( exp_fac, -14000.*tinv, plonl )
	 if( usr12_ndx > 0 ) then
	    if( usr11_ndx > 0 ) then
               rxt(:,k,usr12_ndx) = rxt(:,k,usr11_ndx) * 1.111e28 * exp_fac(:)
	    else
               rxt(:,k,usr12_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!	... mpan + m --> mco3 + no2 + m
!-----------------------------------------------------------------
	 if( usr15_ndx > 0 ) then
	    if( usr14_ndx > 0 ) then
               rxt(:,k,usr15_ndx) = rxt(:,k,usr14_ndx) * 1.111e28 * exp_fac(:)
	    else
               rxt(:,k,usr15_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!       ... xooh + oh -> h2o + oh
!-----------------------------------------------------------------
	 if( usr21_ndx > 0 ) then
	    call vexp( exp_fac, 253.*tinv, plonl )
            rxt(:,k,usr21_ndx) = temp(:,k)**2 * 7.69e-17 * exp_fac(:)
	 end if

!-----------------------------------------------------------------
!       ... ch3coch3 + oh -> ro2 + h2o
!-----------------------------------------------------------------
	 if( usr22_ndx > 0 ) then
	    call vexp( exp_fac, -2000.*tinv, plonl )
            rxt(:,k,usr22_ndx) = 3.82e-11 * exp_fac(:) + 1.33e-13
	 end if

!-----------------------------------------------------------------
!       ... DMS + OH  --> .5 * SO2
!-----------------------------------------------------------------
	 if( usr24_ndx > 0 ) then
	    call vexp( exp_fac, 7460.*tinv, plonl )
            ko(:) = 1. + 5.5e-31 * exp_fac * m(:,k) * 0.21
	    call vexp( exp_fac, 7810.*tinv, plonl )
            rxt(:,k,usr24_ndx) = 1.7e-42 * exp_fac * m(:,k) * 0.21 / ko(:)
         end if

#else
!-----------------------------------------------------------------
!	... n2o5 + m --> no2 + no3 + m
!-----------------------------------------------------------------
	 if( usr3_ndx > 0 ) then
	    if( usr2_ndx > 0 ) then
               rxt(:,k,usr3_ndx) = rxt(:,k,usr2_ndx) * 3.333e26 * exp( -10990.*tinv(:) )
	    else
               rxt(:,k,usr3_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!	set rates for:
! 	... hno3 + oh --> no3 + h2o
!           ho2no2 + m --> ho2 + no2 + m
!           co + oh --> co2 + ho2
!-----------------------------------------------------------------
	 if( usr5_ndx > 0 ) then
            ko(:) = m(:,k) * 6.5e-34 * exp( 1335.*tinv(:) )
            ko(:) = ko(:) / (1. + ko(:)/(2.7e-17*exp( 2199.*tinv(:) )))
            rxt(:,k,usr5_ndx) = ko(:) + 2.4e-14*exp( 460.*tinv(:) )
	 end if
	 if( usr7_ndx > 0 ) then
	    if( usr6_ndx > 0 ) then
               rxt(:,k,usr7_ndx) = rxt(:,k,usr6_ndx) * exp( -10900.*tinv(:) )/ 2.1e-27
	    else
               rxt(:,k,usr7_ndx) = 0.
	    end if
	 end if
	 if( usr8_ndx > 0 ) then
            rxt(:,k,usr8_ndx) = 1.5e-13 * (1. + 6.e-7*boltz*m(:,k)*temp(:,k))
	 end if

!-----------------------------------------------------------------
!	... ho2 + ho2 --> h2o2
!	note: this rate involves the water vapor number density
!-----------------------------------------------------------------
         if( usr9_ndx > 0 ) then
	    if( has_h2o ) then
               ko(:)   = 2.3e-13 * exp( 600.*tinv(:) )
               kinf(:) = 1.7e-33 * m(:,k) * exp( 1000.*tinv(:) )
               fc(:)   = 1. + 1.4e-21 * invariants(:,k,h2o_ndx) * exp( 2200.*tinv(:) )
               rxt(:,k,usr9_ndx) = (ko(:) + kinf(:)) * fc(:)
	    else
               rxt(:,k,usr9_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!    	... mco3 + no2 -> mpan
!-----------------------------------------------------------------
	 if( usr14_ndx > 0 ) then
            rxt(:,k,usr14_ndx) = 1.1e-11 * tp(:) / m(:,k)
	 end if

!-----------------------------------------------------------------
!	... pan + m --> ch3co3 + no2 + m
!-----------------------------------------------------------------
	 exp_fac(:) = exp( -14000.*tinv(:) )
	 if( usr12_ndx > 0 ) then
	    if( usr11_ndx > 0 ) then
               rxt(:,k,usr12_ndx) = rxt(:,k,usr11_ndx) * 1.111e28 * exp_fac(:)
	    else
               rxt(:,k,usr12_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!	... mpan + m --> mco3 + no2 + m
!-----------------------------------------------------------------
	 if( usr15_ndx > 0 ) then
	    if( usr14_ndx > 0 ) then
               rxt(:,k,usr15_ndx) = rxt(:,k,usr14_ndx) * 1.111e28 * exp_fac(:)
	    else
               rxt(:,k,usr15_ndx) = 0.
	    end if
	 end if

!-----------------------------------------------------------------
!       ... xooh + oh -> h2o + oh
!-----------------------------------------------------------------
	 if( usr21_ndx > 0 ) then
            rxt(:,k,usr21_ndx) = temp(:,k)**2 * 7.69e-17 * exp( 253.*tinv(:) )
	 end if

!-----------------------------------------------------------------
!       ... ch3coch3 + oh -> ro2 + h2o
!-----------------------------------------------------------------
	 if( usr22_ndx > 0 ) then
            rxt(:,k,usr22_ndx) = 3.82e-11 * exp( -2000.*tinv(:) ) + 1.33e-13
	 end if

!-----------------------------------------------------------------
!       ... DMS + OH  --> .5 * SO2
!-----------------------------------------------------------------
         if( usr24_ndx > 0 ) then
            ko(:) = 1. + 5.5e-31 * exp( 7460.*tinv(:) ) * m(:,k) * 0.21
            rxt(:,k,usr24_ndx) = 1.7e-42 * exp( 7810.*tinv(:) ) * m(:,k) * 0.21 / ko(:)
         end if
#endif

!-----------------------------------------------------------------
!       ... SO2 + OH  --> SO4  (REFERENCE?? - not Liao)
!-----------------------------------------------------------------
         if( usr23_ndx > 0 ) then
            fc(:) = 3.0e-31 *(300.*tinv(:))**3.3
            ko(:) = fc(:)*m(:,k)/(1. + fc(:)*m(:,k)/1.5e-12) 
            rxt(:,k,usr23_ndx) = ko(:)*.6**(1. + (log10(fc(:)*m(:,k)/1.5e-12))**2.)**(-1.)
         end if

!-----------------------------------------------------------------
!       ... NH3 --> NH4
!-----------------------------------------------------------------
         if( usr25_ndx > 0 )  then
            rxt(:,k,usr25_ndx) = 0.
         end if


!-----------------------------------------------------------------
! 	... exponent for calculating number density
!-----------------------------------------------------------------
         n_exp = exp( -4.5*log(sd_sulf)*log(sd_sulf) )

         dm_sulf = 2. * rm_sulf
         dm_orgc = 2. * rm_orgc
         dm_bc   = 2. * rm_bc

         log_sd_sulf = log(sd_sulf)
         log_sd_orgc = log(sd_orgc)
         log_sd_bc = log(sd_bc)

long_loop : &
         do i = 1,plonl
!-------------------------------------------------------------------------
! 	... air density (kg/m3)
!-------------------------------------------------------------------------
              rho_air = pmid(i,k)/(temp(i,k)*287.04)

!-------------------------------------------------------------------------
!       ... aerosol growth interpolated from M.Chin's table
!-------------------------------------------------------------------------
               if (relhum(i,k) >= table_rh(7)) then
                 rfac_sulf = table_rfac_sulf(7)
                 rfac_oc = table_rfac_oc(7)
                 rfac_bc = table_rfac_bc(7)
               else
                 do irh = 2,7
                   if (relhum(i,k) <= table_rh(irh)) then
                     exit
                   end if
                 end do
                 rh_l = irh-1
                 rh_u = irh

                 factor = (relhum(i,k) - table_rh(rh_l))/(table_rh(rh_u) - table_rh(rh_l))

                 rfac_sulf = table_rfac_sulf(rh_l) + factor*(table_rfac_sulf(rh_u) - table_rfac_sulf(rh_l))
                 rfac_oc = table_rfac_oc(rh_u) + factor*(table_rfac_oc(rh_u) - table_rfac_oc(rh_l))
                 rfac_bc = table_rfac_bc(rh_u) + factor*(table_rfac_bc(rh_u) - table_rfac_bc(rh_l))
               end if

               dm_sulf_wet = dm_sulf * rfac_sulf
               dm_orgc_wet = dm_orgc * rfac_oc
               dm_bc_wet = dm_bc * rfac_bc

!-------------------------------------------------------------------------
! 	... sulfate aerosols
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!       ... use ubvals climatology for stratospheric sulfate surface area density
!-------------------------------------------------------------------------
             if( k < ltrop(i) ) then
                 sfc_sulf = strato_sad(i,k)
             else

              if( so4_ndx > 0 ) then
!-------------------------------------------------------------------------
! convert mass mixing ratio of aerosol to cm3/cm3 (cm^3_aerosol/cm^3_air)
! v=volume density (m^3/m^3)
! rho_aer=density of aerosol (kg/m^3)
! v=m*rho_air/rho_aer   [kg/kg * (kg/m3)_air/(kg/m3)_aer]
!-------------------------------------------------------------------------
                v = mmr(i,k,so4_ndx) * rho_air/rho_sulf
!-------------------------------------------------------------------------
! calculate the number density of aerosol (aerosols/cm3)
! assuming a lognormal distribution
! n  = (aerosols/cm3)
! dm = geometric mean diameter
!
! because only the dry mass of the aerosols is known, we
! use the mean dry radius
!-------------------------------------------------------------------------
                n  = v * (6./pi)*(1./(dm_sulf**3))*n_exp
!-------------------------------------------------------------------------
! find surface area of aerosols using dm_wet, log_sd 
!  (increase of sd due to RH is negligible)
! and number density calculated above as distribution
! parameters
! sfc = surface area of wet aerosols (cm^2/cm^3)
!-------------------------------------------------------------------------
                s_exp    = exp(2.*log_sd_sulf*log_sd_sulf)
                sfc_sulf = n * pi * (dm_sulf_wet**2) * s_exp

              else
!-------------------------------------------------------------------------
!  if so4 not simulated, use off-line sulfate and calculate as above
!  convert sulfate vmr to volume density of aerosol (cm^3_aerosol/cm^3_air)           
!-------------------------------------------------------------------------
                v = sulfate(i,k) * m(i,k) * mw_so4 / (avo * rho_sulf) *1.e6
                n  = v * (6./pi)*(1./(dm_sulf**3))*n_exp
                s_exp    = exp(2.*log_sd_sulf*log_sd_sulf)
                sfc_sulf = n * pi * (dm_sulf_wet**2) * s_exp

              end if
             end if

!-------------------------------------------------------------------------
! ammonium nitrate (follow same procedure as sulfate, using size and density of sulfate)
!-------------------------------------------------------------------------
              if( nit_ndx > 0 ) then
                v = mmr(i,k,nit_ndx) * rho_air/rho_sulf
                n  = v * (6./pi)*(1./(dm_sulf**3))*n_exp
                s_exp   = exp(2.*log_sd_sulf*log_sd_sulf)
                sfc_nit = n * pi * (dm_sulf_wet**2) * s_exp
              else
                sfc_nit = 0.
              end if

!-------------------------------------------------------------------------
! hydrophylic organic carbon (follow same procedure as sulfate)
!-------------------------------------------------------------------------
              if( oc2_ndx > 0 ) then
                v = mmr(i,k,oc2_ndx) * rho_air/rho_orgc
                n  = v * (6./pi)*(1./(dm_orgc**3))*n_exp
                s_exp    = exp(2.*log_sd_orgc*log_sd_orgc)
                sfc_oc   = n * pi * (dm_orgc_wet**2) * s_exp
              else
                sfc_oc = 0.
              end if

!-------------------------------------------------------------------------
! secondary organic carbon (follow same procedure as sulfate)
!-------------------------------------------------------------------------
              if( soa_ndx > 0 ) then
                v = mmr(i,k,soa_ndx) * rho_air/rho_orgc
                n  = v * (6./pi)*(1./(dm_orgc**3))*n_exp
                s_exp     = exp(2.*log_sd_orgc*log_sd_orgc)
                sfc_soa   = n * pi * (dm_orgc_wet**2) * s_exp
              else
                sfc_soa = 0.
              end if

!-------------------------------------------------------------------------
! black carbon (follow same procedure as sulfate)
!-------------------------------------------------------------------------
              if( cb2_ndx > 0 ) then
                v = mmr(i,k,cb2_ndx) * rho_air/rho_bc
                n  = v * (6./pi)*(1./(dm_bc**3))*n_exp
                s_exp     = exp(2.*log_sd_bc*log_sd_bc)
                sfc_bc   = n * pi * (dm_bc_wet**2) * s_exp
              else
                sfc_bc = 0.
              end if

!-------------------------------------------------------------------------
!  	... add up total surface area density for output
!-------------------------------------------------------------------------
              sad_total(i,k) = sfc_sulf + sfc_nit + sfc_oc + sfc_soa + sfc_bc


!-------------------------------------------------------------------------
!  Heterogeneous reaction rates for uptake of a gas on an aerosol:
!    rxt = sfc / ( (rad_aer/Dg_gas) + (4/(c_gas*gamma_gas)))
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! 	... n2o5 -> 2 hno3  (on sulfate, nh4no3, oc2, soa)
!-------------------------------------------------------------------------
              if( usr16_ndx > 0 ) then
                c_n2o5 = 1.40e3 * sqrt_t(i)         ! mean molecular speed of n2o5

                rxt_sulf = sfc_sulf / (0.5*dm_sulf_wet/dg + (4./(c_n2o5*gamma_n2o5))) 
                rxt_nit = sfc_nit / (0.5*dm_sulf_wet/dg + (4./(c_n2o5*gamma_n2o5))) 
                rxt_oc = sfc_oc / (0.5*dm_orgc_wet/dg + (4./(c_n2o5*gamma_n2o5)))
                rxt_soa = sfc_soa / (0.5*dm_orgc_wet/dg + (4./(c_n2o5*gamma_n2o5)))

                rxt(i,k,usr16_ndx) = rxt_sulf + rxt_nit + rxt_oc + rxt_soa

              end if
!-------------------------------------------------------------------------
! 	... no3 -> hno3  (on sulfate, nh4no3, oc, soa)
!-------------------------------------------------------------------------
              if( usr17_ndx > 0 ) then
                c_no3 = 1.85e3 * sqrt_t(i)         ! mean molecular speed of no3

                rxt_sulf = sfc_sulf / (0.5*dm_sulf_wet/dg + (4./(c_no3*gamma_no3))) 
                rxt_nit = sfc_nit / (0.5*dm_sulf_wet/dg + (4./(c_no3*gamma_no3))) 
                rxt_oc = sfc_oc / (0.5*dm_orgc_wet/dg + (4./(c_no3*gamma_no3)))
                rxt_soa = sfc_soa / (0.5*dm_orgc_wet/dg + (4./(c_no3*gamma_no3)))

                rxt(i,k,usr17_ndx) = rxt_sulf + rxt_nit + rxt_oc + rxt_soa
              end if
!-------------------------------------------------------------------------
! 	... no2 -> 0.5 * (ho+no+hno3)  (on sulfate, nh4no3, oc2, soa)
!-------------------------------------------------------------------------
              if( usr17a_ndx > 0 ) then
                c_no2 = 2.15e3 * sqrt_t(i)         ! mean molecular speed of no2

                rxt_sulf = sfc_sulf / (0.5*dm_sulf_wet/dg + (4./(c_no2*gamma_no2))) 
                rxt_nit = sfc_nit / (0.5*dm_sulf_wet/dg + (4./(c_no2*gamma_no2))) 
                rxt_oc = sfc_oc / (0.5*dm_orgc_wet/dg + (4./(c_no2*gamma_no2)))
                rxt_soa = sfc_soa / (0.5*dm_orgc_wet/dg + (4./(c_no2*gamma_no2)))

                rxt(i,k,usr17a_ndx) = rxt_sulf + rxt_nit + rxt_oc + rxt_soa
              end if
!-------------------------------------------------------------------------
! 	... ho2 -> 0.5 * h2o2  (on sulfate, nh4no3, oc2, soa)
!-------------------------------------------------------------------------
              if( usr26_ndx > 0 ) then
                c_ho2 = 2.53e3 * sqrt_t(i)         ! mean molecular speed of ho2

                rxt_sulf = sfc_sulf / (0.5*dm_sulf_wet/dg + (4./(c_ho2*gamma_ho2))) 
                rxt_nit = sfc_nit / (0.5*dm_sulf_wet/dg + (4./(c_ho2*gamma_ho2))) 
                rxt_oc = sfc_oc / (0.5*dm_orgc_wet/dg + (4./(c_ho2*gamma_ho2)))
                rxt_soa = sfc_soa / (0.5*dm_orgc_wet/dg + (4./(c_ho2*gamma_ho2)))

                rxt(i,k,usr26_ndx) = rxt_sulf + rxt_nit + rxt_oc + rxt_soa
              end if
            end do long_loop
      end do level_loop

      end subroutine usrrxt

      end module mo_usrrxt
