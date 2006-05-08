!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for information and terms of use
!**********************************************************************************  
      module module_mosaic_therm



      use module_data_mosaic_therm
      use module_peg_util



      implicit none



      contains



!   zz01aerchemistry.f (mosaic.14.3)
!   28-apr-05 raz - reversed calls to form_cacl2 and form_nacl
!                   fixed caco3 error in subr. electrolytes_to_ions
!                   renamed dens_aer to dens_aer_mac; mw_aer to mw_aer_mac
!   27-apr-05 raz - updated dry_mass calculation approach in mesa_convergence
!   22-apr-05 raz - fixed caso4 mass balance problem and updated algorithm to
!                   calculate phi_volatile for nh3, hno3, and hcl.
!   20-apr-05 raz - updated asceem
!   19-apr-05 raz - updated the algorithm to constrain the nh4 concentration
!                   during simultaneous nh3, hno3, and hcl integration such
!                   that it does not exceed the max possible value for a given bin
!   14-apr-05 raz - fixed asteem_flux_wet_case3 and asteem_flux_dry_case3c
!   11-jan-05 raz - major updates to many subroutines
!   18-nov-04 rce - make sure that acos argument is between +/-1.0
!   28-jan-04 rce - added subr aerchem_boxtest_output;
!	eliminated some unnecessary "include v33com-"
!   01-dec-03 rce - added "implicit none" to many routines;
!	eliminated some unnecessary "include v33com-"
!   05-oct-03 raz - added hysteresis treatment
!   02-sep-03 raz - implemented asteem
!   10-jul-03 raz - changed ix to ixd in interp. subrs fast*_up and fast*_lo
!   08-jul-03 raz - implemented asteem (adaptive step time-split
!                   explicit euler method)
!   26-jun-03 raz - updated almost all the subrs. this version contains
!       options for rigorous and fast solvers (including lsode solver)
!
!   07-oct-02 raz - made zx and zm integers in activity coeff subs.
!   16-sep-02 raz - updated many subrs to treat calcium salts
!   19-aug-02 raz - inlcude v33com9a in subr aerosolmtc
!   14-aug-02 rce - "(msectional.eq.0)" changed to "(msectional.le.0)"
!   07-aug-02 rce - this is rahul's latest version from freshair
!	after adding "real mean_molecular_speed" wherever it is used
!   01-apr-02 raz - made final tests and gave the code to jerome
!
!   04--14-dec-01 rce - several minor changes during initial testing/debug
!	in 3d los angeles simulation
!	(see earlier versions for details about these changes)
!-----------------------------------------------------------------------
!23456789012345678901234567890123456789012345678901234567890123456789012

!***********************************************************************
! interface to mosaic
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine aerchemistry( iclm, jclm, kclm_calcbgn, kclm_calcend,   &
                               dtchem, idiagaa )

      use module_data_mosaic_asect
      use module_data_mosaic_other
      use module_mosaic_movesect, only:  move_sections

!     implicit none
!     include 'v33com'
!     include 'v33com2'
!     include 'v33com3'
!     include 'mosaic.h'
!   subr arguments
      integer iclm, jclm, kclm_calcbgn, kclm_calcend, idiagaa
      real dtchem
!   local variables
      integer istat_mosaic, k, m





      lunerr_aer = lunerr
      ncorecnt_aer = ncorecnt

!   special output for solver testing
      call aerchem_boxtest_output( 1, iclm, jclm, 0, 0, dtchem )

      iclm_aer = iclm
      jclm_aer = jclm
      kclm_aer_calcbgn = kclm_calcbgn
      kclm_aer_calcend = kclm_calcend


      do 200 m = 1, nsubareas
        mclm_aer = m

        do 100 k = kclm_aer_calcbgn, kclm_aer_calcend

          kclm_aer = k
          if (afracsubarea(k,m) .lt. 1.e-4) goto 100

          call print_mosaic_stats_bb( 0 )

          call mosaic( k, m, dtchem, istat_mosaic )

          call print_mosaic_stats_bb( 1 )
          if (istat_mosaic .lt. 0) goto 100

          call specialoutaa( iclm, jclm, k, m, 'befor_movesect' )
          call move_sections( 1, iclm, jclm, k, m)
          call specialoutaa( iclm, jclm, k, m, 'after_movesect' )

100     continue	! k levels

200   continue		! subareas


!   special output for solver testing
      call aerchem_boxtest_output( 3, iclm, jclm, 0, 0, dtchem )

      return
      end subroutine aerchemistry










!***********************************************************************
! mosaic (model for simulating aerosol interactions and chemistry)
!
! author: rahul a. zaveri
! update: dec 2004
!-----------------------------------------------------------------------
      subroutine mosaic( k, m, dtchem, istat_mosaic )

      use module_data_mosaic_asect
      use module_data_mosaic_other

!     implicit none
!     include 'v33com'
!     include 'v33com3'
!     include 'mosaic.h'
!   subr arguments
      integer istat_mosaic, k, m
      real dtchem
!   local variables
      real yh2o


         istat_mosaic = 0

! overwrite inputs
         if(1.eq.0)then
           call hijack_input(k,m)
         endif


          t_k = rsub(ktemp,k,m)			! update temperature  = k
          p_atm = ptotclm(k) /1.032e6		! update pressure = atm
          yh2o = rsub(kh2o,k,m)			! mol(h2o)/mol(air)
          rh_pc = 100.*relhumclm(k)		! rh (%)
          ah2o = relhumclm(k)			! fractional rh


          call load_mosaic_parameters		! sets up indices and other stuff once per simulation

          call update_thermodynamic_constants	! update temperature dependent constants

          call initialize_mosaic_variables

          call map_mosaic_species(k, m, 0)

          call mosaic_dynamic_solver( dtchem, istat_mosaic )
          if (istat_mosaic .lt. 0) return

          call map_mosaic_species(k, m, 1)

!      write(6,*)' done ijk', iclm_aer, jclm_aer, kclm_aer

      return
      end subroutine mosaic




      subroutine hijack_input(k, m)

      use module_data_mosaic_asect
      use module_data_mosaic_other

!     implicit none
!     include 'v33com'
!     include 'v33com3'
!     include 'v33com9a'
!     include 'v33com9b'
!     include 'mosaic.h'
! subr arguments
      integer k, m
! local variables
      integer ibin, igas, iphase, isize, itype
      real t_kdum, p_atmdum, rhdum, cairclmdum
      real gasdum(4), aerdum(14,8)




! read inputs----------------
      open(92, file = 'box.txt')

      read(92,*)t_kdum, p_atmdum, rhdum, cairclmdum
      do igas = 1, 4
        read(92,*)gasdum(igas)
      enddo

      do ibin = 1, nbin_a
        read(92,*)aerdum(1,ibin),aerdum(2,ibin),aerdum(3,ibin),   &
                  aerdum(4,ibin),aerdum(5,ibin),aerdum(6,ibin),   &
                  aerdum(7,ibin),aerdum(8,ibin),aerdum(9,ibin),   &
                  aerdum(10,ibin),aerdum(11,ibin),aerdum(12,ibin),   &
                  aerdum(13,ibin),aerdum(14,ibin)
      enddo

      close(92)
!----------------------------



      rsub(ktemp,k,m) = t_kdum			! update temperature  = k
      ptotclm(k)      = p_atmdum*1.032e6	! update pressure = atm
      relhumclm(k)    = rhdum/100.0		! fractional rh
      cairclm(k)      = cairclmdum		! mol/cc


! 3-d
! calculate air conc in mol/m^3
      cair_mol_m3 = cairclm(k)*1.e6	! cairclm(k) is in mol/cc
      cair_mol_cc = cairclm(k)

! 3-d
! define conversion factors
      conv1a = cair_mol_m3*1.e9		! converts q/mol(air) to nq/m^3 (q = mol or g)
      conv1b = 1./conv1a		! converts nq/m^3 to q/mol(air)
      conv2a = cair_mol_m3*18.*1.e-3	! converts mol(h2o)/mol(air) to kg(h2o)/m^3(air)
      conv2b = 1./conv2a		! converts kg(h2o)/m^3(air) to mol(h2o)/mol(air)


! read rsub (mol/mol(air))
! gas
        rsub(kh2so4,k,m) = gasdum(1)
        rsub(khno3,k,m)  = gasdum(2)
        rsub(khcl,k,m)   = gasdum(3)
        rsub(knh3,k,m)   = gasdum(4)


! aerosol: rsub [mol/mol (air) or g/mol(air)]
        iphase = ai_phase
        ibin = 0
        do 10 itype = 1, ntype_aer
        do 10 isize = 1, nsize_aer(itype)
        ibin = ibin + 1

        rsub(lptr_so4_aer(isize,itype,iphase),k,m) = aerdum(1,ibin)
        rsub(lptr_no3_aer(isize,itype,iphase),k,m) = aerdum(2,ibin)
        rsub(lptr_cl_aer(isize,itype,iphase),k,m)  = aerdum(3,ibin)
        rsub(lptr_nh4_aer(isize,itype,iphase),k,m) = aerdum(4,ibin)
        rsub(lptr_oc_aer(isize,itype,iphase),k,m)  = aerdum(5,ibin)
        rsub(lptr_co3_aer(isize,itype,iphase),k,m) = aerdum(6,ibin)
        rsub(lptr_msa_aer(isize,itype,iphase),k,m) = aerdum(7,ibin)
        rsub(lptr_bc_aer(isize,itype,iphase),k,m)  = aerdum(8,ibin)
        rsub(lptr_na_aer(isize,itype,iphase),k,m)  = aerdum(9,ibin)
        rsub(lptr_ca_aer(isize,itype,iphase),k,m)  = aerdum(10,ibin)
        rsub(lptr_oin_aer(isize,itype,iphase),k,m) = aerdum(11,ibin)

        rsub(hyswptr_aer(isize,itype),k,m) = aerdum(12,ibin) ! kg/m^3(air)
        rsub(waterptr_aer(isize,itype),k,m)       = aerdum(13,ibin)	! kg/m^3(air)
        rsub(numptr_aer(isize,itype,iphase),k,m)          = aerdum(14,ibin)	! num_a is in #/cc
10    continue

      return
      end subroutine hijack_input





!***********************************************************************
! intializes all the mosaic variables to zero or their default values.
!
! author: rahul a. zaveri
! update: jun 2003
!-----------------------------------------------------------------------
      subroutine initialize_mosaic_variables
!     implicit none
!     include 'mosaic.h'
! local variables
      integer iaer, ibin, iv, ja, jc, je



      do ibin = 1, nbin_a
        ah2o_a(ibin) = ah2o				! initialize
      enddo

      if(mactivity_coeff .eq. mmtem)then
        call mtem_compute_log_gamz			! this is done only once every transport time step (function of ah2o)
      endif


      do iv = 1, naer_vol
          gas(iv)           = 0.0
          ctot_a(iv)        = 0.0
          volatile_a(iv)    = 0.0
      enddo

! initialize to zero
      do ibin = 1, nbin_a

        num_a(ibin)          = 0.0
        mass_dry_a(ibin)     = 0.0
        mass_soluble_a(ibin) = 0.0

        do iaer = 1, naer
          aer(iaer,jtotal,ibin)  = 0.0
          aer(iaer,jsolid,ibin)  = 0.0
          aer(iaer,jliquid,ibin) = 0.0
        enddo

        do je = 1, nelectrolyte
          electrolyte(je,jtotal,ibin)  = 0.0
          electrolyte(je,jsolid,ibin)  = 0.0
          electrolyte(je,jliquid,ibin) = 0.0
          activity(je,ibin)            = 0.0
          gam(je,ibin)                 = 0.0
        enddo

          gam_ratio(ibin)   = 0.0

        do iv = 1, naer_vol
          flux(iv,ibin)     = 0.0
          kg(iv,ibin)       = 0.0
          phi_volatile(iv,ibin)  = 0.0
          df_gas(iv,ibin)   = 0.0
        enddo


        jaerosolstate(ibin) = -1	! initialize to default value
        jphase(ibin) = 0

        do jc = 1, ncation
          mc(jc,ibin) = 0.0
          gam_cation(jc,ibin) = 0.0
        enddo

        do ja = 1, nanion
          ma(ja,ibin) = 0.0
          gam_anion(ja,ibin)  = 0.0
        enddo

      enddo	! ibin


      return
      end subroutine initialize_mosaic_variables






!***********************************************************************
! maps rsub(k,l,m) to and from mosaic arrays: gas and aer
!
! author: rahul a. zaveri
! update: nov 2001
!-------------------------------------------------------------------------
      subroutine map_mosaic_species(k, m, imap)

      use module_data_mosaic_asect
      use module_data_mosaic_other
      use module_state_description, only:  param_first_scalar

!     implicit none

!     include 'v33com'
!     include 'v33com3'
!     include 'v33com9a'
!     include 'v33com9b'

! subr arguments
      integer k, m, imap
! local variables
      integer ibin, iphase, isize, itype, l, p1st


! if a species index is less than this value, then the species is not defined
      p1st = param_first_scalar

! 3-d
! calculate air conc in mol/m^3
      cair_mol_m3 = cairclm(k)*1.e6	! cairclm(k) is in mol/cc
      cair_mol_cc = cairclm(k)

! 3-d
! define conversion factors
      conv1a = cair_mol_m3*1.e9		! converts q/mol(air) to nq/m^3 (q = mol or g)
      conv1b = 1./conv1a		! converts nq/m^3 to q/mol(air)
      conv2a = cair_mol_m3*18.*1.e-3	! converts mol(h2o)/mol(air) to kg(h2o)/m^3(air)
      conv2b = 1./conv2a		! converts kg(h2o)/m^3(air) to mol(h2o)/mol(air)


! box
!      conv1 = 1.e15/avogad     ! converts (molec/cc) to (nmol/m^3)
!      conv2 = 1./conv1         ! converts (nmol/m^3) to (molec/cc)
!      kaerstart = ngas_max


      if(imap.eq.0)then    ! map rsub (mol/mol(air)) into aer (nmol/m^3)
! gas
	if (kh2so4 .ge. p1st) then
	    gas(ih2so4_g) = rsub(kh2so4,k,m)*conv1a	! nmol/m^3
	else
	    gas(ih2so4_g) = 0.0
	end if
	if (khno3 .ge. p1st) then
	    gas(ihno3_g)  = rsub(khno3,k,m)*conv1a
	else
	    gas(ihno3_g) = 0.0
	end if
	if (khcl .ge. p1st) then
	    gas(ihcl_g)   = rsub(khcl,k,m)*conv1a
	else
	    gas(ihcl_g) = 0.0
	end if
	if (knh3 .ge. p1st) then
	    gas(inh3_g)   = rsub(knh3,k,m)*conv1a
	else
	    gas(inh3_g) = 0.0
	end if

! aerosol
        iphase = ai_phase
        ibin = 0
        do 10 itype = 1, ntype_aer
        do 10 isize = 1, nsize_aer(itype)
        ibin = ibin + 1

! aer array units are nmol/(m^3 air)

! rce 18-nov-2004 - always map so4 and number, 
! but only map other species when (lptr_xxx .ge. p1st)
        l = lptr_so4_aer(isize,itype,iphase)
        aer(iso4_a,jtotal,ibin)=rsub(l,k,m)*conv1a

        l = lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ino3_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ino3_a,jtotal,ibin)=0.0
        end if

        l = lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(icl_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(icl_a,jtotal,ibin)=0.0
        end if

        l = lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(inh4_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(inh4_a,jtotal,ibin)=0.0
        end if

        l = lptr_oc_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ioc_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ioc_a,jtotal,ibin)=0.0
        end if

        l = lptr_bc_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ibc_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ibc_a,jtotal,ibin)=0.0
        end if

        l = lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ina_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ina_a,jtotal,ibin)=0.0
        end if

        l = lptr_oin_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ioin_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ioin_a,jtotal,ibin)=0.0
        end if

        l = lptr_msa_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(imsa_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(imsa_a,jtotal,ibin)=0.0
        end if

        l = lptr_co3_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ico3_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ico3_a,jtotal,ibin)=0.0
        end if

        l = lptr_ca_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ica_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ica_a,jtotal,ibin)=0.0
        end if

! water_a and water_a_hyst units are kg/(m^3 air)
        l = hyswptr_aer(isize,itype)
        if (l .ge. p1st) then
            water_a_hyst(ibin)=rsub(l,k,m)*conv2a
        else
            water_a_hyst(ibin)=0.0
        end if

! water_a units are kg/(m^3 air)
        l = waterptr_aer(isize,itype)
        if (l .ge. p1st) then
            water_a(ibin)=rsub(l,k,m)*conv2a
        else
            water_a(ibin)=0.0
        end if

! num_a units are #/(cm^3 air)
        l = numptr_aer(isize,itype,iphase)
        num_a(ibin) = rsub(l,k,m)*cair_mol_cc

! other bin parameters (fixed for now)
        sigmag_a(ibin)	= 1.02

10      continue



! save input overall (all bins combined) mass
      call overall_massbal_in

      iprint_input = mYES     ! reset to default


!---------------------------------------------------------------------


      else                 ! map aer & gas (nmol/m^3) back into rsub (mol/mol(air))


! check exit overall (all bins combined) mass balance
      call overall_massbal_out(k,m)
                                                              
! gas
	if (kh2so4 .ge. p1st)   &
	    rsub(kh2so4,k,m) = gas(ih2so4_g)*conv1b
	if (khno3 .ge. p1st)   &
	    rsub(khno3,k,m)  = gas(ihno3_g)*conv1b
	if (khcl .ge. p1st)   &
	    rsub(khcl,k,m)   = gas(ihcl_g)*conv1b
	if (knh3 .ge. p1st)   &
	    rsub(knh3,k,m)   = gas(inh3_g)*conv1b

! aerosol
        iphase = ai_phase
        ibin = 0
        do 20 itype = 1, ntype_aer
        do 20 isize = 1, nsize_aer(itype)
        ibin = ibin + 1


! rce 18-nov-2004 - always map so4 and number, 
! but only map other species when (lptr_xxx .ge. p1st)
        l = lptr_so4_aer(isize,itype,iphase)
        rsub(l,k,m) = aer(iso4_a,jtotal,ibin)*conv1b

        l = lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ino3_a,jtotal,ibin)*conv1b

        l = lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(icl_a,jtotal,ibin)*conv1b

        l = lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(inh4_a,jtotal,ibin)*conv1b

        l = lptr_oc_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ioc_a,jtotal,ibin)*conv1b

        l = lptr_bc_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ibc_a,jtotal,ibin)*conv1b

        l = lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ina_a,jtotal,ibin)*conv1b

        l = lptr_oin_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ioin_a,jtotal,ibin)*conv1b

        l = lptr_msa_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(imsa_a,jtotal,ibin)*conv1b

        l = lptr_co3_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ico3_a,jtotal,ibin)*conv1b

        l = lptr_ca_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) = aer(ica_a,jtotal,ibin)*conv1b

        l = hyswptr_aer(isize,itype)
        if (l .ge. p1st) rsub(l,k,m) = water_a_hyst(ibin)*conv2b

        l = waterptr_aer(isize,itype)
        if (l .ge. p1st) rsub(l,k,m) = water_a(ibin)*conv2b

        l = numptr_aer(isize,itype,iphase)
        if (l .ge. p1st) rsub(l,k,m) =  num_a(ibin)/cair_mol_cc


        drymass_aftgrow(isize,itype) = mass_dry_a(ibin)/cair_mol_cc ! g/mol-air
        if(jaerosolstate(ibin) .eq. no_aerosol) then
	    drydens_aftgrow(isize,itype) = -1.
	else
            drydens_aftgrow(isize,itype) = dens_dry_a(ibin)         ! g/cc
	end if

20      continue

      endif

      return
      end subroutine map_mosaic_species





      subroutine isize_itype_from_ibin( ibin, isize, itype )
!
! inside of mosaic, the "2d" (isize,itype) indexing is replaced
!     by "1d" (ibin) indexing
! this routine gives (isize,itype) corresponding to (ibin)
!
      use module_data_mosaic_asect
      use module_data_mosaic_other, only:  lunerr
!     implicit none

! subr arguments
      integer ibin, isize, itype
! local variables
      integer jdum_bin, jdum_size, jdum_type
      character*80 msg

      isize = -999888777
      itype = -999888777

      jdum_bin = 0
      do jdum_type = 1, ntype_aer
      do jdum_size = 1, nsize_aer(jdum_type)
          jdum_bin = jdum_bin + 1
          if (ibin .eq. jdum_bin) then
              isize = jdum_size
              itype = jdum_type
          end if
      end do
      end do

      if (isize .le. 0) then
          write(msg,'(a,1x,i5)')   &
              '*** subr isize_itype_from_ibin - bad ibin =', ibin
          call peg_error_fatal( lunerr, msg )
      end if

      return
      end subroutine isize_itype_from_ibin       




      subroutine overall_massbal_in

      use module_data_mosaic_asect
      use module_data_mosaic_other

!     implicit none
!     include 'mosaic.h'
      integer ibin

      tot_so4_in = gas(ih2so4_g)
      tot_no3_in = gas(ihno3_g)
      tot_cl_in  = gas(ihcl_g)
      tot_nh4_in = gas(inh3_g)
      tot_na_in  = 0.0
      tot_ca_in  = 0.0


      do ibin = 1, nbin_a
        tot_so4_in = tot_so4_in + aer(iso4_a,jtotal,ibin)
	tot_no3_in = tot_no3_in + aer(ino3_a,jtotal,ibin)
        tot_cl_in  = tot_cl_in  + aer(icl_a, jtotal,ibin)
        tot_nh4_in = tot_nh4_in + aer(inh4_a,jtotal,ibin)
        tot_na_in  = tot_na_in  + aer(ina_a,jtotal,ibin)
        tot_ca_in  = tot_ca_in  + aer(ica_a,jtotal,ibin)
      enddo


      return
      end subroutine overall_massbal_in



      subroutine overall_massbal_out(k,m)
!     implicit none
!     include 'v33com'
!     include 'v33com3'
!     include 'v33com9a'
!     include 'v33com9b'
!     include 'mosaic.h'
! subr arguments
      integer k, m
! local variables
      integer ibin

        k = kclm_aer
        m = mclm_aer


        tot_so4_out = gas(ih2so4_g)
	tot_no3_out = gas(ihno3_g)
        tot_cl_out  = gas(ihcl_g)
        tot_nh4_out = gas(inh3_g)
        tot_na_out  = 0.0
        tot_ca_out  = 0.0

	do ibin = 1, nbin_a
          tot_so4_out = tot_so4_out + aer(iso4_a,jtotal,ibin)
	  tot_no3_out = tot_no3_out + aer(ino3_a,jtotal,ibin)
          tot_cl_out  = tot_cl_out  + aer(icl_a,jtotal,ibin)
          tot_nh4_out = tot_nh4_out + aer(inh4_a,jtotal,ibin)
          tot_na_out  = tot_na_out  + aer(ina_a,jtotal,ibin)
          tot_ca_out  = tot_ca_out  + aer(ica_a,jtotal,ibin)
	enddo

        diff_so4 = tot_so4_out - tot_so4_in
	diff_no3 = tot_no3_out - tot_no3_in
        diff_cl  = tot_cl_out  - tot_cl_in
        diff_nh4 = tot_nh4_out - tot_nh4_in
        diff_na  = tot_na_out  - tot_na_in
        diff_ca  = tot_ca_out  - tot_ca_in


        reldiff_so4 = 0.0
	if(tot_so4_in .gt. 1.e-25 .or. tot_so4_out .gt. 1.e-25)then
	  reldiff_so4 = diff_so4/max(tot_so4_in, tot_so4_out)
	endif

        reldiff_no3 = 0.0
	if(tot_no3_in .gt. 1.e-25 .or. tot_no3_out .gt. 1.e-25)then
	  reldiff_no3 = diff_no3/max(tot_no3_in, tot_no3_out)
	endif

        reldiff_cl = 0.0
	if(tot_cl_in .gt. 1.e-25 .or. tot_cl_out .gt. 1.e-25)then
	  reldiff_cl = diff_cl/max(tot_cl_in, tot_cl_out)
	endif

        reldiff_nh4 = 0.0
	if(tot_nh4_in .gt. 1.e-25 .or. tot_nh4_out .gt. 1.e-25)then
	  reldiff_nh4 = diff_nh4/max(tot_nh4_in, tot_nh4_out)
	endif

        reldiff_na = 0.0
	if(tot_na_in .gt. 1.e-25 .or. tot_na_out .gt. 1.e-25)then
	  reldiff_na = diff_na/max(tot_na_in, tot_na_out)
	endif

        reldiff_ca = 0.0
	if(tot_ca_in .gt. 1.e-25 .or. tot_ca_out .gt. 1.e-25)then
	  reldiff_ca = diff_ca/max(tot_ca_in, tot_ca_out)
	endif



      if(abs(reldiff_so4) .gt. 1.e-4 .or.   &
           abs(reldiff_no3) .gt. 1.e-4 .or.   &
           abs(reldiff_nh4) .gt. 1.e-4 .or.   &
           abs(reldiff_na)  .gt. 1.e-4 .or.   &
           abs(reldiff_ca)  .gt. 1.e-4)then


        if(iprint_input .eq. myes)then
          write(6,*) 'mosaic aerchem overall_massbal_out error'
          call print_input(k, m)
          iprint_input = mno
        endif

      endif


      return
      end subroutine overall_massbal_out




      subroutine print_input(k,m)

      use module_data_mosaic_asect
      use module_data_mosaic_other

!     implicit none
!     include 'v33com'
!     include 'v33com3'
!     include 'v33com9a'
!     include 'v33com9b'
!     include 'mosaic.h'
! subr arguments
      integer k, m
! local variables
      integer ibin, iphase, isize, itype

        k = kclm_aer
        m = mclm_aer



        tot_so4_out = gas(ih2so4_g)
        tot_no3_out = gas(ihno3_g)
        tot_cl_out  = gas(ihcl_g)
        tot_nh4_out = gas(inh3_g)
        tot_na_out  = 0.0
        tot_ca_out  = 0.0

	do ibin = 1, nbin_a
          tot_so4_out = tot_so4_out + aer(iso4_a,jtotal,ibin)
          tot_no3_out = tot_no3_out + aer(ino3_a,jtotal,ibin)
          tot_cl_out  = tot_cl_out  + aer(icl_a,jtotal,ibin)
          tot_nh4_out = tot_nh4_out + aer(inh4_a,jtotal,ibin)
          tot_na_out  = tot_na_out  + aer(ina_a,jtotal,ibin)
          tot_ca_out  = tot_ca_out  + aer(ica_a,jtotal,ibin)
	enddo

        diff_so4 = tot_so4_out - tot_so4_in
	diff_no3 = tot_no3_out - tot_no3_in
        diff_cl  = tot_cl_out  - tot_cl_in
        diff_nh4 = tot_nh4_out - tot_nh4_in
        diff_na  = tot_na_out  - tot_na_in
        diff_ca  = tot_ca_out  - tot_ca_in


        reldiff_so4 = 0.0
	if(tot_so4_in .gt. 1.e-25 .or. tot_so4_out .gt. 1.e-25)then
	  reldiff_so4 = diff_so4/max(tot_so4_in, tot_so4_out)
	endif

        reldiff_no3 = 0.0
	if(tot_no3_in .gt. 1.e-25 .or. tot_no3_out .gt. 1.e-25)then
	  reldiff_no3 = diff_no3/max(tot_no3_in, tot_no3_out)
	endif

        reldiff_cl = 0.0
	if(tot_cl_in .gt. 1.e-25 .or. tot_cl_out .gt. 1.e-25)then
	  reldiff_cl = diff_cl/max(tot_cl_in, tot_cl_out)
	endif

        reldiff_nh4 = 0.0
	if(tot_nh4_in .gt. 1.e-25 .or. tot_nh4_out .gt. 1.e-25)then
	  reldiff_nh4 = diff_nh4/max(tot_nh4_in, tot_nh4_out)
	endif

        reldiff_na = 0.0
	if(tot_na_in .gt. 1.e-25 .or. tot_na_out .gt. 1.e-25)then
	  reldiff_na = diff_na/max(tot_na_in, tot_na_out)
	endif

        reldiff_ca = 0.0
	if(tot_ca_in .gt. 1.e-25 .or. tot_ca_out .gt. 1.e-25)then
	  reldiff_ca = diff_ca/max(tot_ca_in, tot_ca_out)
	endif


! write to monitor screen
          write(6,*) 'mosaic aerchem print_input'
          write(6,*)'+++++++++++++++++++++++++++++++++++++++++'
          write(6,*)'i j k = ', iclm_aer, jclm_aer, kclm_aer
          write(6,*)'ncorecnt = ', ncorecnt_aer
          write(6,*)'relative so4 mass bal = ', reldiff_so4
	  write(6,*)'relative no3 mass bal = ', reldiff_no3
          write(6,*)'relative cl  mass bal = ', reldiff_cl
          write(6,*)'relative nh4 mass bal = ', reldiff_nh4
          write(6,*)'relative na  mass bal = ', reldiff_na
          write(6,*)'relative ca  mass bal = ', reldiff_ca
          write(6,*)'inputs:'
          write(6,*)'t (k) = ',t_k,' p (atm) = ',p_atm,' rh = ',rh_pc
          write(6,*)'cairclm (mol/cc) = ', cairclm(k)
	  write(6,*)'gas h2so4(ppb) = ', rsub(kh2so4,k,m)
          write(6,*)'gas hno3 (ppb) = ', rsub(khno3,k,m)
	  write(6,*)'gas hcl (ppb)  = ', rsub(khcl,k,m)
	  write(6,*)'gas nh3 (ppb)  = ', rsub(knh3,k,m)

          iphase = ai_phase
          ibin = 0
          do itype = 1, ntype_aer
          do isize = 1, nsize_aer(itype)
            ibin = ibin + 1
	    write(6,44)   &
              rsub(lptr_so4_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_no3_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_cl_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_nh4_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_oc_aer(isize,itype,iphase),k,m),	   &  ! ng/m^3(air)
              rsub(lptr_co3_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_msa_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_bc_aer(isize,itype,iphase),k,m),	   &  ! ng/m^3(air)
              rsub(lptr_na_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_ca_aer(isize,itype,iphase),k,m),   &
              rsub(lptr_oin_aer(isize,itype,iphase),k,m),	   &
              rsub(hyswptr_aer(isize,itype),k,m),   &
              rsub(waterptr_aer(isize,itype),k,m),   &
              rsub(numptr_aer(isize,itype,iphase),k,m)
          enddo
          enddo

          write(6,*)'+++++++++++++++++++++++++++++++++++++++++'




! do this in pegasus but not in wrf-chem
! write to fort.67
!          write(67,*)'+++++++++++++++++++++++++++++++++++++++++'
!          write(67,*)'i j k = ', iclm_aer, jclm_aer, kclm_aer
!          write(67,*)'ncorecnt = ', ncorecnt_aer
!          write(67,*)'relative so4 mass bal = ', reldiff_so4
!	  write(67,*)'relative no3 mass bal = ', reldiff_no3
!          write(67,*)'relative cl  mass bal = ', reldiff_cl
!          write(67,*)'relative nh4 mass bal = ', reldiff_nh4
!          write(67,*)'relative na  mass bal = ', reldiff_na
!          write(67,*)'relative ca  mass bal = ', reldiff_ca
!          write(67,*)'inputs:'
!          write(67,*)'t (k) =',t_k,' p (atm) =',p_atm,' rh =',rh_pc,   &
!                     'cairclm (mol/cc) = ', cairclm(k)
!	  write(67,*)'gas h2so4 = ', rsub(kh2so4,k,m)
!          write(67,*)'gas hno3  = ', rsub(khno3,k,m)
!	  write(67,*)'gas hcl   = ', rsub(khcl,k,m)
!	  write(67,*)'gas nh3   = ', rsub(knh3,k,m)
!
!          iphase = ai_phase
!          ibin = 0
!          do itype = 1, ntype_aer
!          do isize = 1, nsize_aer(itype)
!            ibin = ibin + 1
!	    write(67,44)   &
!              rsub(lptr_so4_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_no3_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_cl_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_nh4_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_oc_aer(isize,itype,iphase),k,m),	   &  ! ng/m^3(air)
!              rsub(lptr_co3_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_msa_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_bc_aer(isize,itype,iphase),k,m),	   &  ! ng/m^3(air)
!              rsub(lptr_na_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_ca_aer(isize,itype,iphase),k,m),   &
!              rsub(lptr_oin_aer(isize,itype,iphase),k,m),	   &
!              rsub(hyswptr_aer(isize,itype),k,m),   &
!              rsub(waterptr_aer(isize,itype),k,m),   &
!              rsub(numptr_aer(isize,itype,iphase),k,m)
!          enddo
!          enddo
!
!          write(67,*)'+++++++++++++++++++++++++++++++++++++++++'



44      format(14(e18.10,2x))


      return
      end subroutine print_input






!***********************************************************************
! interface to asceem and asteem dynamic gas-particle exchange solvers
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine mosaic_dynamic_solver( dtchem, istat_mosaic )
!     implicit none
!     include 'v33com'
!     include 'mosaic.h'
! subr arguments
      integer istat_mosaic
      real dtchem
! local variables
      integer ibin, istat_asteem, iv, k, m
      real xt, dumdum
!     real aerosol_water_up				! mosaic func


      istat_mosaic = 0

      if(iclm_aer .eq. 35 .and.   &
         jclm_aer .eq. 14 .and.   &
         kclm_aer .eq. 1  .and.   &
         ncorecnt_aer .eq. 1)then
        dumdum = 0.0
      endif


      do 500 ibin = 1, nbin_a

        call check_aerosol_mass(ibin)
        if(jaerosolstate(ibin) .eq. no_aerosol)goto 500

        call conform_electrolytes(jtotal,ibin,xt) 	! conforms aer(jtotal) to a valid aerosol

        call check_aerosol_mass(ibin) 			! check mass again after conform_electrolytes
        if(jaerosolstate(ibin) .eq. no_aerosol)goto 500	! ignore this bin

        call conform_aerosol_number(ibin)   		! adjusts number conc so that it conforms with bin mass and diameter


        if(jaerosolstate(ibin) .eq. no_aerosol)goto 500

        do iv = 1, naer_vol
          ctot_a(iv) = ctot_a(iv) + aer(iv,jtotal,ibin)
        enddo

500   continue

        do iv = 1, naer_vol
          ctot_a(iv) = ctot_a(iv) + gas(iv)
        enddo


! box
!        call initial_aer_print_box	! box

        call save_pregrow_props

	call specialoutaa( iclm_aer, jclm_aer, kclm_aer, 77,   &
      		'after_conform' )
!
!-------------------------------------
! do dynamic gas-aerosol mass transfer

!      call overall_massbal_out(k ,m)

        if(mdynamic_solver.eq.masceem    .and.mgas_aer_xfer.eq.mon)then
          call asceem(dtchem)
        elseif(mdynamic_solver.eq.masteem.and.mgas_aer_xfer.eq.mon)then
          call asteem( dtchem, istat_asteem )
          istat_mosaic = istat_asteem
        endif
        if (istat_mosaic .lt. 0) return

!      call overall_massbal_out(k ,m)

!-------------------------------------
! box
! grows or shrinks size depending on mass increase or decrease
!
!      do ibin = 1, nbin_a
!        if(jaerosolstate(ibin) .ne. no_aerosol)then
!          call conform_particle_size(ibin)	! box
!        endif
!      enddo



      do 600 ibin = 1, nbin_a
        if(jaerosolstate(ibin).eq.no_aerosol) goto 600

        if(jhyst_leg(ibin) .eq. jhyst_lo)then
          water_a_hyst(ibin) = 0.0
        elseif(jhyst_leg(ibin) .eq. jhyst_up)then
          water_a_up(ibin)   = aerosol_water_up(ibin)	! at 60% rh
          water_a_hyst(ibin) = water_a_up(ibin)
        endif

        call calc_dry_n_wet_aerosol_props(ibin)		! compute final mass and density
600   continue

      return
      end subroutine mosaic_dynamic_solver















!***********************************************************************
! checks if aerosol mass is too low to be of any significance
! and determine jaerosolstate
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine check_aerosol_mass(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin




      mass_dry_a(ibin) = aer(iso4_a,jtotal,ibin)*mw_aer_mac(iso4_a) +	   &  ! ng/m^3(air)
                         aer(ino3_a,jtotal,ibin)*mw_aer_mac(ino3_a) +   &
                         aer(icl_a, jtotal,ibin)*mw_aer_mac(icl_a)  +   &
                         aer(inh4_a,jtotal,ibin)*mw_aer_mac(inh4_a) +   &
                         aer(imsa_a,jtotal,ibin)*mw_aer_mac(imsa_a) +   &
                         aer(ico3_a,jtotal,ibin)*mw_aer_mac(ico3_a) +   &
                         aer(ina_a, jtotal,ibin)*mw_aer_mac(ina_a)  +   &
                         aer(ica_a, jtotal,ibin)*mw_aer_mac(ica_a)  +   &
                         aer(ioin_a,jtotal,ibin)                +   &
                         aer(ioc_a, jtotal,ibin)                +   &
                         aer(ibc_a, jtotal,ibin)


      if(mass_dry_a(ibin) .lt. mass_cutoff)then
        jaerosolstate(ibin) = no_aerosol
        jphase(ibin) = 0
        if(mass_dry_a(ibin) .eq. 0.)num_a(ibin) = 0.0
      endif

      return
      end subroutine check_aerosol_mass





!***********************************************************************
! checks and conforms number according to the mass and bin size range
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine conform_aerosol_number(ibin)

      use module_data_mosaic_asect

!     implicit none
!     include 'v33com9a'
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer isize, itype, je
      real num_at_dlo, num_at_dhi





      vol_dry_a(ibin)  = 0.0		! initialize to 0.0

      if(jaerosolstate(ibin) .eq. no_aerosol) return


! 1st add all electrolytes
      do je = 1, nelectrolyte
        vol_dry_a(ibin) = vol_dry_a(ibin) + 			   &  ! cc(aer)/cc(air)
        electrolyte(je,jtotal,ibin)*mw_electrolyte(je)*1.e-15/	   &
                     dens_electrolyte(je)
      enddo

! next add all other aerosol species
      vol_dry_a(ibin)  = vol_dry_a(ibin)                  +	   &  ! cc/cc(air)
          aer(ioc_a,jtotal,ibin)*1.e-15/dens_aer_mac(ioc_a)   +   &
          aer(ibc_a,jtotal,ibin)*1.e-15/dens_aer_mac(ibc_a)   +   &
          aer(ioin_a,jtotal,ibin)*1.e-15/dens_aer_mac(ioin_a)


! conform number
      call isize_itype_from_ibin( ibin, isize, itype )
      num_at_dlo = vol_dry_a(ibin)/volumlo_sect(isize,itype)
      num_at_dhi = vol_dry_a(ibin)/volumhi_sect(isize,itype)

      num_a(ibin) = min(num_a(ibin), num_at_dlo)
      num_a(ibin) = max(num_a(ibin), num_at_dhi)



      return
      end subroutine conform_aerosol_number







!***********************************************************************
! determines phase state of an aerosol bin. includes kelvin effect.
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine aerosol_phase_state(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer js
      real ah2o_a_new, rel_err
!     real aerosol_water_up				! mosaic func




      ah2o_a(ibin) = ah2o
      kelvin(ibin) = 1.0

      total_dry_mass(ibin) = 0.0
      dry_vol(ibin)  = 0.0
      do js = 1, nelectrolyte
       total_dry_mass(ibin) = total_dry_mass(ibin) +   &
        electrolyte(js,jtotal,ibin)*mw_electrolyte(js)*1.e-9 		! [g/m^3(air)]

       dry_vol(ibin) = dry_vol(ibin) +   &
        electrolyte(js,jtotal,ibin)*mw_electrolyte(js)*1.e-15/		   &  ! [m^3/m^3(air)]
                         dens_comp_a(js)
      enddo

      total_dry_mass(ibin) = total_dry_mass(ibin) + 				   &  ! [g/m^3(air)]
        aer(ioc_a,jtotal,ibin)*1.e-9  +   &
        aer(ibc_a,jtotal,ibin)*1.e-9  +   &
        aer(ioin_a,jtotal,ibin)*1.e-9

      dry_vol(ibin) = dry_vol(ibin) + 					   &  ! [m^3/m^3(air)]
        aer(ioc_a,jtotal,ibin)*1.e-15/dens_comp_a(joc) +   &
        aer(ibc_a,jtotal,ibin)*1.e-15/dens_comp_a(jbc) +   &
        aer(ioin_a,jtotal,ibin)*1.e-15/dens_comp_a(join)


      water_a_up(ibin) = aerosol_water_up(ibin)	! for hysteresis curve determination


10    call phase_equilibrium(ibin)

      call calculate_kelvin(ibin)

      ah2o_a_new = ah2o/kelvin(ibin)

      rel_err = abs( (ah2o_a_new - ah2o_a(ibin))/ah2o_a(ibin))

      if(rel_err .gt. 1.e-2)then
        ah2o_a(ibin) = ah2o_a_new
        goto 10
      endif


! phase determination is done
! now adjust the aerosol and update the appropriate aer(jphase) and aer(jtotal)

! remove if not needed
      if(jaerosolstate(ibin) .eq. all_solid)then
        call adjust_solid_aerosol(ibin)
      elseif(jaerosolstate(ibin) .eq. all_liquid)then
        call adjust_liquid_aerosol(ibin)
      elseif(jaerosolstate(ibin) .eq. mixed)then
        jphase(ibin)    = jliquid
        jhyst_leg(ibin) = jhyst_lo
      else
        write(6,*)'   error in deciding aerosol state'
        write(6,*)'   stopping in subr. aerosol_phase_state'
!       stop
        call peg_error_fatal( lunerr_aer,   &
            '   stopping in subr. aerosol_phase_state' )
      endif


      return
      end subroutine aerosol_phase_state






!***********************************************************************
! computes kelvin effect term (kelvin => 1.0)
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine calculate_kelvin(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real term



      sigma_water = 71.9759e-3	! n/m
      volume_a(ibin) = water_a(ibin)/1000. + dry_vol(ibin) 		! [m^3/m^3(air)]
      dpmv(ibin)=(6.*volume_a(ibin)/(num_a(ibin)*3.1415926))**(1./3.)	! [m]
      sigma_soln(ibin) = sigma_water + 0.049*(1. - ah2o_a(ibin)) 	! [n/m]
      term = 72.*sigma_soln(ibin)/(8314.0*t_k*1000.*dpmv(ibin))		! [-]
      kelvin(ibin) = exp(term)


      return
      end subroutine calculate_kelvin







!***********************************************************************
! called when aerosol bin is completely solid.
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine adjust_solid_aerosol(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iaer, je




      jphase(ibin) = jsolid

      jhyst_leg(ibin) = jhyst_lo	! lower curve
      water_a(ibin)   = 0.0

! transfer aer(jtotal) to aer(jsolid)
      do iaer = 1, naer
        aer(iaer, jsolid, ibin) = aer(iaer,jtotal,ibin)
        aer(iaer, jliquid,ibin) = 0.0
      enddo

! transfer electrolyte(jtotal) to electrolyte(jsolid)
      do je = 1, nelectrolyte
        electrolyte(je,jliquid,ibin) = 0.0
        epercent(je,jliquid,ibin)    = 0.0
        electrolyte(je,jsolid,ibin)  = electrolyte(je,jtotal,ibin)
        epercent(je,jsolid,ibin)     = epercent(je,jtotal,ibin)
      enddo

! degas volatile nh4no3 and nh4cl if they are less than smallp %
      if(epercent(jnh4no3,jsolid,ibin) .gt. 0.0        .and.   &
         epercent(jnh4no3,jsolid,ibin) .le. smallp     .and.   &
         gas(inh3_g)*gas(ihno3_g)      .lt. keq_sg(1) )then

        gas(inh3_g) = gas(inh3_g) + electrolyte(jnh4no3,jsolid,ibin)
        gas(ihno3_g)= gas(ihno3_g)+ electrolyte(jnh4no3,jsolid,ibin)
        aer(inh4_a,jsolid,ibin) = real(   &
                           dble(aer(inh4_a,jsolid,ibin)) -   &
                           dble(electrolyte(jnh4no3,jsolid,ibin)) )
        aer(ino3_a,jsolid,ibin)  = real(   &
                           dble(aer(ino3_a,jsolid,ibin)) -   &
                           dble(electrolyte(jnh4no3,jsolid,ibin)) )
        electrolyte(jnh4no3,jsolid,ibin) = 0.0
        epercent(jnh4no3,jsolid,ibin) = 0.0

      endif

      if(epercent(jnh4cl,jsolid,ibin)  .gt. 0.0        .and.   &
         epercent(jnh4cl,jsolid,ibin)  .le. smallp     .and.   &
         gas(inh3_g)*gas(ihcl_g)       .lt. keq_sg(2) )then

        gas(inh3_g) = gas(inh3_g) + electrolyte(jnh4cl,jsolid,ibin)
        gas(ihcl_g) = gas(ihcl_g) + electrolyte(jnh4cl,jsolid,ibin)
        aer(inh4_a,jsolid,ibin) = real(   &
                           dble(aer(inh4_a,jsolid,ibin)) -   &
                           dble(electrolyte(jnh4cl,jsolid,ibin)) )
        aer(icl_a,jsolid,ibin)  = real(   &
                           dble(aer(icl_a,jsolid,ibin)) -   &
                           dble(electrolyte(jnh4cl,jsolid,ibin)) )
        electrolyte(jnh4cl,jsolid,ibin) = 0.0
        epercent(jnh4cl,jsolid,ibin) = 0.0

      endif

! update aer(jtotal) that may have been affected above
      aer(inh4_a,jtotal,ibin) = aer(inh4_a,jsolid,ibin)
      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jsolid,ibin)
      aer(icl_a,jtotal,ibin)  = aer(icl_a,jsolid,ibin)

! update electrolyte(jtotal)
      do je = 1, nelectrolyte
        electrolyte(je,jtotal,ibin) = electrolyte(je,jsolid,ibin)
        epercent(je,jtotal,ibin)    = epercent(je,jsolid,ibin)
      enddo

      return
      end subroutine adjust_solid_aerosol









!***********************************************************************
! called when aerosol bin is completely liquid.
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine adjust_liquid_aerosol(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer je




      jphase(ibin)    = jliquid
      jhyst_leg(ibin) = jhyst_up	! upper curve

! partition all electrolytes into liquid phase
      do je = 1, nelectrolyte
        electrolyte(je,jsolid,ibin)  = 0.0
        epercent(je,jsolid,ibin)     = 0.0
        electrolyte(je,jliquid,ibin) = electrolyte(je,jtotal,ibin)
        epercent(je,jliquid,ibin)    = epercent(je,jtotal,ibin)
      enddo
! except these electrolytes, which always remain in the solid phase
      electrolyte(jcaco3,jsolid,ibin) = electrolyte(jcaco3,jtotal,ibin)
      electrolyte(jcaso4,jsolid,ibin) = electrolyte(jcaso4,jtotal,ibin)
      epercent(jcaco3,jsolid,ibin)    = epercent(jcaco3,jtotal,ibin)
      epercent(jcaso4,jsolid,ibin)    = epercent(jcaso4,jtotal,ibin)
      electrolyte(jcaco3,jliquid,ibin)= 0.0
      electrolyte(jcaso4,jliquid,ibin)= 0.0
      epercent(jcaco3,jliquid,ibin)   = 0.0
      epercent(jcaso4,jliquid,ibin)   = 0.0


! partition all the aer species into
! solid phase
      aer(iso4_a,jsolid,ibin) = electrolyte(jcaso4,jsolid,ibin)
      aer(ino3_a,jsolid,ibin) = 0.0
      aer(icl_a, jsolid,ibin) = 0.0
      aer(inh4_a,jsolid,ibin) = 0.0
      aer(ioc_a, jsolid,ibin) = aer(ioc_a,jtotal,ibin)
      aer(imsa_a,jsolid,ibin) = 0.0
      aer(ico3_a,jsolid,ibin) = aer(ico3_a,jtotal,ibin)
      aer(ina_a, jsolid,ibin) = 0.0
      aer(ica_a, jsolid,ibin) = electrolyte(jcaco3,jsolid,ibin) +   &
                                electrolyte(jcaso4,jsolid,ibin)
      aer(ibc_a, jsolid,ibin) = aer(ibc_a,jtotal,ibin)
      aer(ioin_a, jsolid,ibin)= aer(ioin_a,jtotal,ibin)

! liquid-phase
      aer(iso4_a,jliquid,ibin) = aer(iso4_a,jtotal,ibin) -   &
                                 electrolyte(jcaso4,jsolid,ibin)
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jtotal,ibin)
      aer(icl_a, jliquid,ibin) = aer(icl_a,jtotal,ibin)
      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jtotal,ibin)
      aer(ioc_a, jliquid,ibin) = 0.0
      aer(imsa_a,jliquid,ibin) = aer(imsa_a,jtotal,ibin)
      aer(ico3_a,jliquid,ibin) = 0.0
      aer(ina_a, jliquid,ibin) = aer(ina_a,jtotal,ibin)
      aer(ica_a, jliquid,ibin) = electrolyte(jcano3,jtotal,ibin) +   &
                                 electrolyte(jcacl2,jtotal,ibin)
      aer(ibc_a, jliquid,ibin) = 0.0
      aer(ioin_a, jliquid,ibin)= 0.0

      return
      end subroutine adjust_liquid_aerosol


!--------------------------------------------------------------------













!***********************************************************************
! computes phase equilibrium and interfaces mesa
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine phase_equilibrium(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin

! local variables
      integer idissolved, j_index, jdum, js
      real crh, solids, sum_soluble, sum_insoluble, xt
!     real aerosol_water				! mosaic func
!     real drh_mutual					! mosaic func



      call calculate_xt(ibin,jtotal,xt)

      crh = 0.1

! step 1: check if ah2o is below crh (crystallization or efflorescence point)
      if(ah2o.lt.crh .and. (xt.gt.1.0 .or. xt.lt.0.))then
        jaerosolstate(ibin) = all_solid
        jphase(ibin)    = jsolid
        jhyst_leg(ibin) = jhyst_lo
        call adjust_solid_aerosol(ibin)
        return
      endif


! step 2: check for supersaturation/metastable state
      if(water_a_hyst(ibin) .gt. 0.5*water_a_up(ibin))then

        call do_full_deliquescence(ibin)
        sum_soluble = 0.0
        do js = 1, nsoluble
          sum_soluble = sum_soluble + electrolyte(js,jtotal,ibin)
        enddo

        solids = electrolyte(jcaso4,jtotal,ibin) +   &
                 electrolyte(jcaco3,jtotal,ibin) +   &
                 aer(ioin_a,jtotal,ibin) +   &
                 aer(ibc_a,jtotal,ibin) +   &
                 aer(ioc_a,jtotal,ibin)


        if(sum_soluble .lt. 1.e-15 .and. solids .gt. 0.0)then

          jaerosolstate(ibin) = all_solid ! no soluble material present
          jphase(ibin) = jsolid
          call adjust_solid_aerosol(ibin)

        elseif(sum_soluble .gt. 0.0)then

          jaerosolstate(ibin) = all_liquid
          jphase(ibin) = jliquid
          water_a(ibin) = aerosol_water(jtotal,ibin)

          if(water_a(ibin) .lt. 0.0)then
            jaerosolstate(ibin) = all_solid ! no soluble material present
            jphase(ibin)    = jsolid
            jhyst_leg(ibin) = jhyst_lo
            call adjust_solid_aerosol(ibin)
          else
            call adjust_liquid_aerosol(ibin)
            call compute_activities(ibin)
          endif

        endif

      return
      endif




! step 3: diagnose mdrh
      if(xt .lt. 1. .and. xt .gt. 0. )goto 10	! excess sulfate domain - no mdrh exists

      jdum = 0
      do js = 1, nsalt
        jsalt_present(js) = 0			! default value - salt absent

        if(epercent(js,jtotal,ibin) .gt. 1.0)then
          jsalt_present(js) = 1			! salt present
          jdum = jdum + jsalt_index(js)
        endif
      enddo

      if(jdum .eq. 0)then
        jaerosolstate(ibin) = all_solid ! no significant soluble material present
        jphase(ibin) = jsolid
        call adjust_solid_aerosol(ibin)
        return
      endif

      if(xt .ge. 2.0 .or. xt .lt. 0.0)then
        j_index = jsulf_poor(jdum)
      else
        j_index = jsulf_rich(jdum)
      endif

      mdrh(ibin) = drh_mutual(j_index) + 1.0

      if(ah2o*100. .lt. mdrh(ibin)) then
        jaerosolstate(ibin) = all_solid
        jphase(ibin) = jsolid
        call adjust_solid_aerosol(ibin)
        return
      endif


! step 4: none of the above means it must be sub-saturated or mixed-phase
10    call do_full_deliquescence(ibin)

      call mesa(ibin)	! determines jaerosolstate(ibin)



      return
      end subroutine phase_equilibrium








!***********************************************************************
! this subroutine completely deliquesces an aerosol and partitions
! all the soluble electrolytes into the liquid phase and insoluble
! ones into the solid phase. it also calculates the corresponding
! aer(js,jliquid,ibin) and aer(js,jsolid,ibin) generic species
! concentrations
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine do_full_deliquescence(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer js




! partition all electrolytes into liquid phase
      do js = 1, nelectrolyte
       electrolyte(js,jsolid,ibin)  = 0.0
       electrolyte(js,jliquid,ibin) = electrolyte(js,jtotal,ibin)
      enddo
!
! except these electrolytes, which always remain in the solid phase
      electrolyte(jcaco3,jsolid,ibin) = electrolyte(jcaco3,jtotal,ibin)
      electrolyte(jcaso4,jsolid,ibin) = electrolyte(jcaso4,jtotal,ibin)
      electrolyte(jcaco3,jliquid,ibin)= 0.0
      electrolyte(jcaso4,jliquid,ibin)= 0.0


! partition all the generic aer species into solid and liquid phases
! solid phase
      aer(iso4_a,jsolid,ibin) = electrolyte(jcaso4,jsolid,ibin)
      aer(ino3_a,jsolid,ibin) = 0.0
      aer(icl_a, jsolid,ibin) = 0.0
      aer(inh4_a,jsolid,ibin) = 0.0
      aer(ioc_a, jsolid,ibin) = aer(ioc_a,jtotal,ibin)
      aer(imsa_a,jsolid,ibin) = 0.0
      aer(ico3_a,jsolid,ibin) = aer(ico3_a,jtotal,ibin)
      aer(ina_a, jsolid,ibin) = 0.0
      aer(ica_a, jsolid,ibin) = electrolyte(jcaco3,jsolid,ibin) +   &
                                electrolyte(jcaso4,jsolid,ibin)
      aer(ibc_a, jsolid,ibin) = aer(ibc_a,jtotal,ibin)
      aer(ioin_a,jsolid,ibin) = aer(ioin_a,jtotal,ibin)

! liquid-phase
      aer(iso4_a,jliquid,ibin) = aer(iso4_a,jtotal,ibin) -   &
                                 electrolyte(jcaso4,jsolid,ibin)
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jtotal,ibin)
      aer(icl_a, jliquid,ibin) = aer(icl_a,jtotal,ibin)
      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jtotal,ibin)
      aer(ioc_a, jliquid,ibin) = 0.0
      aer(imsa_a,jliquid,ibin) = aer(imsa_a,jtotal,ibin)
      aer(ico3_a,jliquid,ibin) = 0.0
      aer(ina_a, jliquid,ibin) = aer(ina_a,jtotal,ibin)
      aer(ica_a, jliquid,ibin) = electrolyte(jcano3,jtotal,ibin) +   &
                                 electrolyte(jcacl2,jtotal,ibin)
      aer(ibc_a, jliquid,ibin) = 0.0
      aer(ioin_a,jliquid,ibin) = 0.0

      return
      end subroutine do_full_deliquescence






















!***********************************************************************
! mesa: multicomponent equilibrium solver for aerosol-phase
! computes equilibrum solid and liquid phases by integrating
! pseudo-transient dissolution and precipitation reactions
!
! author: rahul a. zaveri
! update: jan 2005
! reference: zaveri r.a., r.c. easter, and l.k. peters, jgr, 2005b
!-----------------------------------------------------------------------
      subroutine mesa(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer nmax_mesa
      parameter(nmax_mesa = 100)
      integer iaer, iconverge, iconverge_flux, iconverge_mass,   &
           itdum, js, je, iflux(nsalt,nmax_mesa),   &
           iprod1, iprod2, iprod3, ioscillation, mdissolved
      real tau_p(nsalt), tau_d(nsalt)
      real frac_solid, sumflux, hsalt_min, alpha, xt, dumdum,   &
           p_mesa_fails
! function
!     real aerosol_water



!      if(mod(jmesa_call,10000).eq.0)then
!        p_mesa_fails = 100.*float(jmesa_fail)/
!     &                        max(float(jmesa_call),1.0)
!        write(6,*)'total number of calls to mesa  =', jmesa_call
!        write(6,*)'cumulative avg mesa iteration  =', iter_mesa_avg
!        write(6,*)'percent mesa convergence fails =', p_mesa_fails
!      endif


! initialize
      itdum = 0		! initialize time
      hsalt_max = 1.e25


      do js = 1, nsalt
        hsalt(js)     = 0.0
        sat_ratio(js) = 0.0
        phi_salt(js)  = 0.0
        flux_sl(js)   = 0.0
      enddo



      total_dry_mass(ibin) = 0.0
      do iaer = 1, naer
       total_dry_mass(ibin) = total_dry_mass(ibin) +   &
        aer(iaer,jtotal,ibin)*mw_aer_mac(iaer)*1.e-9 				! [g/m^3(air)]
      enddo

      total_dry_mass(ibin) = total_dry_mass(ibin) + 			   &  ! [g/m^3(air)]
        aer(ioc_a,jtotal,ibin)*1.e-9  +   &
        aer(ibc_a,jtotal,ibin)*1.e-9  +   &
        aer(ioin_a,jtotal,ibin)*1.e-9



      call mesa_check_complete_dissolution(ibin, mdissolved)
      if(mdissolved .eq. myes)return

      jmesa_call = jmesa_call + 1
      jmesa_call_tot = jmesa_call_tot + 1


!----begin pseudo time continuation loop-------------------------------

      do 500 itdum = 1, nmax_mesa


      if(itdum .gt. 50)then
	dumdum = 0.0
      endif

! compute new salt fluxes
      call mesa_flux_salt(ibin)


! check convergence
      call mesa_convergence_criterion(itdum, ibin,   &
                                      iconverge_mass,   &
                                      iconverge_flux)

      if(iconverge_mass .eq. 1)then
        jaerosolstate(ibin) = all_solid
        call adjust_solid_aerosol(ibin)
        iter_mesa = iter_mesa + float(itdum)
        return
      elseif(iconverge_flux .eq. 1)then
        iter_mesa = iter_mesa + itdum
        jaerosolstate(ibin) = mixed
        jhyst_leg(ibin) = jhyst_lo
        water_a(ibin) = aerosol_water(jliquid,ibin)	! kg/m^3(air)
        return
      endif


! check for oscillating fluxes
      do js = 1, nsalt
        if(flux_sl(js) .lt. 0)then
           iflux(js,itdum) = -1
        elseif(flux_sl(js) .gt. 0)then
           iflux(js,itdum) =  1
        else
           iflux(js,itdum) =  0
        endif
      enddo


      ioscillation = mno
      if(itdum.gt.5) then
        do js = 1, nsalt
          iprod1 = iflux(js,itdum-3)*iflux(js,itdum-2)
          iprod2 = iflux(js,itdum-2)*iflux(js,itdum-1)
          iprod3 = iflux(js,itdum-1)*iflux(js,itdum)

          if(iprod1.lt.0 .and. iprod2.lt.0 .and. iprod3.lt.0)then
            ioscillation = myes
          endif

        enddo
      endif


! calculate hsalt(js)	! time step
      hsalt_min = 1.e25
      do js = 1, nsalt

        alpha = min(abs(phi_salt(js)), 0.5)

        if(ioscillation .eq. myes)then
          alpha = alpha/3.0
        endif


        if(flux_sl(js) .gt. 0.)then

          tau_p(js) = eleliquid(js)/flux_sl(js)	! precipitation time scale
          if(tau_p(js) .eq. 0.0)then
            hsalt(js) = 1.e25
            flux_sl(js) = 0.0
            phi_salt(js)= 0.0
          else
            hsalt(js) = alpha*tau_p(js)
          endif

        elseif(flux_sl(js) .lt. 0.)then

          tau_p(js) = -eleliquid(js)/flux_sl(js)	! precipitation time scale
          tau_d(js) = -electrolyte(js,jsolid,ibin)/flux_sl(js) ! dissolution time scale
          if(tau_p(js) .eq. 0.0)then
            hsalt(js) = alpha*tau_d(js)
          else
            hsalt(js) = alpha*min(tau_p(js),tau_d(js))
          endif

        else

          hsalt(js) = 1.e25

        endif

          hsalt_min = min(hsalt(js), hsalt_min)

      enddo

!---------------------------------

! integrate electrolyte(solid)
      do js = 1, nsalt
        electrolyte(js,jsolid,ibin) = real(   &
                         dble(electrolyte(js,jsolid,ibin))  +   &
                         dble(hsalt(js)) * dble(flux_sl(js)) )
      enddo

      call electrolytes_to_ions(jsolid,ibin) ! computes aer(solid) from electrolyte(solid)


! compute new electrolyte(liquid) from mass balance
      do iaer = 1, naer
        aer(iaer,jliquid,ibin) = real( dble(aer(iaer,jtotal,ibin)) -   &
                                       dble(aer(iaer,jsolid,ibin)) )
      enddo

!---------------------------------



500   continue	! end of time continuation loop
!--------------------------------------------------------------------

      jmesa_fail = jmesa_fail + 1
      iter_mesa = iter_mesa + float(itdum)


!      write(6,66)ibin, iclm_aer, jclm_aer, kclm_aer, jmesa_call,
!     &           jmesa_fail
!66    format('mixed-phase did not converge at ibin ijk jmesa_call =',
!     &        i3, 2x, 3(i5,2x), i10, x, i10)



! determine jaerosolstate from the last estimate of frac_solid
      dry_mass(ibin) = 0.0
      do iaer = 1, naer
        dry_mass(ibin) = dry_mass(ibin) +   &
                        aer(iaer,jsolid,ibin)*mw_aer_mac(iaer)*1.e-9
      enddo
      dry_mass(ibin) = dry_mass(ibin)   +    &  ! [g/m^3(air)]
        aer(ioc_a,jtotal,ibin)*1.e-9  +   &
        aer(ibc_a,jtotal,ibin)*1.e-9  +   &
        aer(ioin_a,jtotal,ibin)*1.e-9

      frac_solid = dry_mass(ibin)/total_dry_mass(ibin)

      if(frac_solid.ge.0.97)then
        jaerosolstate(ibin) = all_solid
        call adjust_solid_aerosol(ibin)
      elseif(frac_solid.lt.0.03)then
        jaerosolstate(ibin) = all_liquid
        call adjust_liquid_aerosol(ibin)
        water_a(ibin) = aerosol_water(jliquid,ibin)	! kg/m^3(air)
      elseif(frac_solid.gt.0.0)then
        water_a(ibin) = aerosol_water(jliquid,ibin)	! kg/m^3(air)
        jaerosolstate(ibin) = mixed
        jhyst_leg(ibin) = jhyst_lo
      endif



      return
      end subroutine mesa










!***********************************************************************
! part of mesa: checks if particle is completely deliquesced at the
! current rh
!
! author: rahul a. zaveri
! update: feb 2005
!-----------------------------------------------------------------------
      subroutine mesa_check_complete_dissolution(ibin, mdissolved)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, mdissolved, je, js, iaer
! local variables
      real sumflux, aer_sav(naer,3,nbin_a),   &
           electrolyte_sav(nelectrolyte,3,nbin_a)


! save current solid-liquid arrays
      do je = 1, nelectrolyte
        electrolyte_sav(je,jsolid,ibin) =electrolyte(je,jsolid,ibin)
        electrolyte_sav(je,jliquid,ibin)=electrolyte(je,jliquid,ibin)
      enddo

      do iaer = 1, naer
        aer_sav(iaer,jsolid,ibin) =aer(iaer,jsolid,ibin)
        aer_sav(iaer,jliquid,ibin)=aer(iaer,jliquid,ibin)
      enddo

      call do_full_deliquescence(ibin)

      do js = 1, nsalt
        sat_ratio(js) = 0.0
        phi_salt(js)  = 0.0
        flux_sl(js)   = 0.0
      enddo


! compute new salt fluxes
      call mesa_flux_salt(ibin)


! check if all the fluxes are zero
      sumflux = 0.0
      do js = 1, nsalt
        sumflux = sumflux + abs(flux_sl(js))
      enddo

      if(sumflux .eq. 0.0)then ! it is completely dissolved

        jaerosolstate(ibin) = all_liquid
        jhyst_leg(ibin)     = jhyst_up		! upper curve
        jphase(ibin)        = jliquid
        call adjust_liquid_aerosol(ibin)
        mdissolved = myes

      else ! restore saved solid-liquid arrays

        do je = 1, nelectrolyte
          electrolyte(je,jsolid,ibin) =electrolyte_sav(je,jsolid,ibin)
          electrolyte(je,jliquid,ibin)=electrolyte_sav(je,jliquid,ibin)
        enddo
        do iaer = 1, naer
          aer(iaer,jsolid,ibin) =aer_sav(iaer,jsolid,ibin)
          aer(iaer,jliquid,ibin)=aer_sav(iaer,jliquid,ibin)
        enddo
        mdissolved = mno

      endif


      return
      end subroutine mesa_check_complete_dissolution















!***********************************************************************
! part of mesa: calculates solid-liquid fluxes of soluble salts
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine mesa_flux_salt(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer js
      real xt, calcium, sum_salt


! compute activities and water content
      call ions_to_electrolytes(jliquid,ibin,xt)
      call compute_activities(ibin)
      activity(jna3hso4,ibin)   = 0.0

      if(water_a(ibin) .le. 0.0)then
        do js = 1, nsalt
         flux_sl(js) = 0.0
        enddo
        return
      endif


      call mesa_estimate_eleliquid(ibin,xt)

      calcium = aer(ica_a,jliquid,ibin)

      do js = 1, nsalt
        jsalt_present(js) = 0			! default value - salt absent
        if(epercent(js,jtotal,ibin) .gt. 1.0)then
          jsalt_present(js) = 1			! salt present
        endif
      enddo


! calculate % electrolyte composition in the solid and liquid phases
      sum_salt = 0.0
      do js = 1, nsalt
        sum_salt = sum_salt + electrolyte(js,jsolid,ibin)
      enddo
      electrolyte_sum(jsolid,ibin) = sum_salt
      if(sum_salt .eq. 0.0)sum_salt = 1.0
      do js = 1, nsalt
        frac_salt_solid(js) = electrolyte(js,jsolid,ibin)/sum_salt
        frac_salt_liq(js)   = epercent(js,jliquid,ibin)/100.
      enddo



! compute salt fluxes
      do js = 1, nsalt		! soluble solid salts

! compute new saturation ratio
        sat_ratio(js) = activity(js,ibin)/keq_sl(js)
! compute relative driving force
        phi_salt(js)  = (sat_ratio(js) - 1.0)/max(sat_ratio(js),1.0)

! check if too little solid-phase salt is trying to dissolve
        if(sat_ratio(js)       .lt. 1.00 .and.   &
           frac_salt_solid(js) .lt. 0.01 .and.   &
           frac_salt_solid(js) .gt. 0.0)then
          call mesa_dissolve_small_salt(ibin,js)
          call mesa_estimate_eleliquid(ibin,xt)
          sat_ratio(js) = activity(js,ibin)/keq_sl(js)
        endif

! compute flux
        flux_sl(js) = sat_ratio(js) - 1.0

! apply heaviside function
        if( (sat_ratio(js)               .lt. 1.0 .and.   &
             electrolyte(js,jsolid,ibin) .eq. 0.0) .or.   &
            (calcium .gt. 0.0 .and. jsalt_present(js).eq.0) )then
          flux_sl(js) = 0.0
          phi_salt(js)= 0.0
        endif

      enddo

! force cacl2 and cano3 fluxes to zero
      sat_ratio(jcano3) = 1.0
      phi_salt(jcano3)  = 0.0
      flux_sl(jcano3)   = 0.0

      sat_ratio(jcacl2) = 1.0
      phi_salt(jcacl2)  = 0.0
      flux_sl(jcacl2)   = 0.0


      return
      end subroutine mesa_flux_salt












!***********************************************************************
! part of mesa: calculates liquid electrolytes from ions
!
! notes:
!  - this subroutine is to be used for liquid-phase or total-phase only
!  - this sub transfers caso4 and caco3 from liquid to solid phase
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine mesa_estimate_eleliquid(ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt
! local variables
      integer iaer, je, jc, ja, icase
      real store(naer), thesum, sum_naza, sum_nczc, sum_na_nh4,   &
           f_nh4, f_na, xh, xb, xl, xs, xt_d, xna_d, xnh4_d,   &
           xdum, dum, cat_net
      real nc(ncation), na(nanion)
      real dum_ca, dum_no3, dum_cl, cano3, cacl2







! remove negative concentrations, if any
      do iaer =  1, naer
      aer(iaer,jliquid,ibin) = max(0.0, aer(iaer,jliquid,ibin))
      enddo


! calculate sulfate ratio
      call calculate_xt(ibin,jliquid,xt)

      if(xt .ge. 2.0 .or. xt.lt.0.)then
       icase = 1	! near neutral (acidity is caused by hcl and/or hno3)
      else
       icase = 2	! acidic (acidity is caused by excess so4)
      endif


! initialize to zero
      do je = 1, nelectrolyte
        eleliquid(je) = 0.0
      enddo
!
!---------------------------------------------------------
! initialize moles of ions depending on the sulfate domain

      jp = jliquid

      if(icase.eq.1)then ! xt >= 2 : sulfate poor domain

        dum_ca  = aer(ica_a,jp,ibin)
        dum_no3 = aer(ino3_a,jp,ibin)
        dum_cl  = aer(icl_a,jp,ibin)

        cano3   = min(dum_ca, 0.5*dum_no3)
        dum_ca  = max(0., dum_ca - cano3)
        dum_no3 = max(0., dum_no3 - 2.*cano3)

        cacl2   = min(dum_ca, 0.5*dum_cl)
        dum_ca  = max(0., dum_ca - cacl2)
        dum_cl  = max(0., dum_cl - 2.*cacl2)

        na(ja_hso4)= 0.0
        na(ja_so4) = aer(iso4_a,jp,ibin)
        na(ja_no3) = aer(ino3_a,jp,ibin)
        na(ja_cl)  = aer(icl_a, jp,ibin)

        nc(jc_ca)  = aer(ica_a, jp,ibin)
        nc(jc_na)  = aer(ina_a, jp,ibin)
        nc(jc_nh4) = aer(inh4_a,jp,ibin)

        cat_net = real( dble(2.*na(ja_so4)+na(ja_no3)+na(ja_cl)) -   &
                 dble(nc(jc_h)+2.*nc(jc_ca) +nc(jc_nh4)+nc(jc_na)) )

        if(cat_net .lt. 0.0)then

!          if(aer(inh4_a,jp,ibin) .gt. abs(cat_net))then ! degas excess nh3
!            aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) + cat_net
!            gas(inh3_g) = gas(inh3_g) - cat_net
!          endif

          nc(jc_h) = 0.0

        else  ! cat_net must be 0.0 or positive

          nc(jc_h) = cat_net

        endif


! now compute equivalent fractions
      sum_naza = 0.0
      do ja = 1, nanion
        sum_naza = sum_naza + na(ja)*za(ja)
      enddo

      sum_nczc = 0.0
      do jc = 1, ncation
        sum_nczc = sum_nczc + nc(jc)*zc(jc)
      enddo

      if(sum_naza .eq. 0. .or. sum_nczc .eq. 0.)then
        write(6,*)'ionic concentrations are zero'
        write(6,*)'sum_naza = ', sum_naza
        write(6,*)'sum_nczc = ', sum_nczc
        return
      endif

      do ja = 1, nanion
        xeq_a(ja) = na(ja)*za(ja)/sum_naza
      enddo

      do jc = 1, ncation
        xeq_c(jc) = nc(jc)*zc(jc)/sum_nczc
      enddo

      na_ma(ja_so4) = na(ja_so4) *mw_a(ja_so4)
      na_ma(ja_no3) = na(ja_no3) *mw_a(ja_no3)
      na_ma(ja_cl)  = na(ja_cl)  *mw_a(ja_cl)
      na_ma(ja_hso4)= na(ja_hso4)*mw_a(ja_hso4)

      nc_mc(jc_ca)  = nc(jc_ca) *mw_c(jc_ca)
      nc_mc(jc_na)  = nc(jc_na) *mw_c(jc_na)
      nc_mc(jc_nh4) = nc(jc_nh4)*mw_c(jc_nh4)
      nc_mc(jc_h)   = nc(jc_h)  *mw_c(jc_h)


! now compute electrolyte moles
      eleliquid(jna2so4) = (xeq_c(jc_na) *na_ma(ja_so4) +   &
                            xeq_a(ja_so4)*nc_mc(jc_na))/   &
                             mw_electrolyte(jna2so4)

      eleliquid(jnahso4) = (xeq_c(jc_na) *na_ma(ja_hso4) +   &
                            xeq_a(ja_hso4)*nc_mc(jc_na))/   &
                             mw_electrolyte(jnahso4)

      eleliquid(jnano3)  = (xeq_c(jc_na) *na_ma(ja_no3) +   &
                            xeq_a(ja_no3)*nc_mc(jc_na))/   &
                             mw_electrolyte(jnano3)

      eleliquid(jnacl)   = (xeq_c(jc_na) *na_ma(ja_cl) +   &
                            xeq_a(ja_cl) *nc_mc(jc_na))/   &
                             mw_electrolyte(jnacl)

      eleliquid(jnh4so4) = (xeq_c(jc_nh4)*na_ma(ja_so4) +   &
                            xeq_a(ja_so4)*nc_mc(jc_nh4))/   &
                             mw_electrolyte(jnh4so4)

      eleliquid(jnh4hso4)= (xeq_c(jc_nh4)*na_ma(ja_hso4) +   &
                            xeq_a(ja_hso4)*nc_mc(jc_nh4))/   &
                             mw_electrolyte(jnh4hso4)

      eleliquid(jnh4no3) = (xeq_c(jc_nh4)*na_ma(ja_no3) +   &
                            xeq_a(ja_no3)*nc_mc(jc_nh4))/   &
                             mw_electrolyte(jnh4no3)

      eleliquid(jnh4cl)  = (xeq_c(jc_nh4)*na_ma(ja_cl) +   &
                            xeq_a(ja_cl) *nc_mc(jc_nh4))/   &
                             mw_electrolyte(jnh4cl)

      eleliquid(jcano3)  = (xeq_c(jc_ca) *na_ma(ja_no3) +   &
                            xeq_a(ja_no3)*nc_mc(jc_ca))/   &
                             mw_electrolyte(jcano3)

      eleliquid(jcacl2)  = (xeq_c(jc_ca) *na_ma(ja_cl) +   &
                            xeq_a(ja_cl) *nc_mc(jc_ca))/   &
                             mw_electrolyte(jcacl2)

      eleliquid(jh2so4)  = (xeq_c(jc_h)  *na_ma(ja_hso4) +   &
                            xeq_a(ja_hso4)*nc_mc(jc_h))/   &
                             mw_electrolyte(jh2so4)

      eleliquid(jhno3)   = (xeq_c(jc_h)  *na_ma(ja_no3) +   &
                            xeq_a(ja_no3)*nc_mc(jc_h))/   &
                             mw_electrolyte(jhno3)

      eleliquid(jhcl)    = (xeq_c(jc_h) *na_ma(ja_cl) +   &
                            xeq_a(ja_cl)*nc_mc(jc_h))/   &
                             mw_electrolyte(jhcl)

!--------------------------------------------------------------------

      elseif(icase.eq.2)then ! xt < 2 : sulfate rich domain

        jp = jliquid
        xt_d  = xt
        xna_d = 1. + 0.5*aer(ina_a,jp,ibin)/aer(iso4_a,jp,ibin)
        xdum = aer(iso4_a,jp,ibin) - aer(inh4_a,jp,ibin)

        dum = real( dble(2.*aer(iso4_a,jp,ibin)) -   &
                    dble(aer(ina_a,jp,ibin)) )
        if(aer(inh4_a,jp,ibin) .gt. 0.0 .and. dum .gt. 0.0)then
          xnh4_d = 2.*aer(inh4_a,jp,ibin)/   &
                  (2.*aer(iso4_a,jp,ibin) - aer(ina_a,jp,ibin))
        else
          xnh4_d = 0.0
        endif


        if(aer(inh4_a,jp,ibin) .gt. 0.0)then


        if(xt_d .ge. xna_d)then
          eleliquid(jna2so4) = 0.5*aer(ina_a,jp,ibin)

          if(xnh4_d .ge. 5./3.)then
            eleliquid(jnh4so4) = 1.5*aer(ina_a,jp,ibin)   &
                               - 3.*xdum - aer(inh4_a,jp,ibin)
            eleliquid(jlvcite) = 2.*xdum + aer(inh4_a,jp,ibin)   &
                               - aer(ina_a,jp,ibin)
          elseif(xnh4_d .ge. 1.5)then
            eleliquid(jnh4so4) = aer(inh4_a,jp,ibin)/5.
            eleliquid(jlvcite) = aer(inh4_a,jp,ibin)/5.
          elseif(xnh4_d .ge. 1.0)then
            eleliquid(jnh4so4) = aer(inh4_a,jp,ibin)/6.
            eleliquid(jlvcite) = aer(inh4_a,jp,ibin)/6.
            eleliquid(jnh4hso4)= aer(inh4_a,jp,ibin)/6.
          endif

        elseif(xt_d .gt. 1.0)then
          eleliquid(jnh4so4)  = aer(inh4_a,jp,ibin)/6.
          eleliquid(jlvcite)  = aer(inh4_a,jp,ibin)/6.
          eleliquid(jnh4hso4) = aer(inh4_a,jp,ibin)/6.
          eleliquid(jna2so4)  = aer(ina_a,jp,ibin)/3.
          eleliquid(jnahso4)  = aer(ina_a,jp,ibin)/3.
        elseif(xt_d .le. 1.0)then
          eleliquid(jna2so4)  = aer(ina_a,jp,ibin)/4.
          eleliquid(jnahso4)  = aer(ina_a,jp,ibin)/2.
          eleliquid(jlvcite)  = aer(inh4_a,jp,ibin)/6.
          eleliquid(jnh4hso4) = aer(inh4_a,jp,ibin)/2.
        endif

        else

        if(xt_d .gt. 1.0)then
          eleliquid(jna2so4) = aer(ina_a,jp,ibin) - aer(iso4_a,jp,ibin)
          eleliquid(jnahso4) = 2.*aer(iso4_a,jp,ibin) -   &
                                  aer(ina_a,jp,ibin)
        else
          eleliquid(jna2so4) = aer(ina_a,jp,ibin)/4.
          eleliquid(jnahso4) = aer(ina_a,jp,ibin)/2.
        endif


        endif



      endif
!---------------------------------------------------------
!
! calculate % composition
      thesum = 0.0
      do je = 1, nelectrolyte
        thesum = thesum + eleliquid(je)
      enddo

      electrolyte_sum(jp,ibin) = thesum

      if(thesum .eq. 0.)thesum = 1.0
      do je = 1, nelectrolyte
        epercent(je,jp,ibin) = 100.*eleliquid(je)/thesum
      enddo


      return
      end subroutine mesa_estimate_eleliquid










!***********************************************************************
! part of mesa: completely dissolves small amounts of soluble salts
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine mesa_dissolve_small_salt(ibin,js)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, js



      if(js .eq. jnh4so4)then
        aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
                           2.*electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jlvcite)then
        aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
                           3.*electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                           2.*electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnh4hso4)then
        aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                             electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jna2so4)then
        aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
                           2.*electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jna3hso4)then
        aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
                           3.*electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                           2.*electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnahso4)then
        aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(iso4_a,jliquid,ibin) = aer(iso4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnh4no3)then
        aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnh4cl)then
        aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnano3)then
        aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jnacl)then
        aer(ina_a,jliquid,ibin)  = aer(ina_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jcano3)then
        aer(ica_a,jliquid,ibin)  = aer(ica_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) +   &
                            2.*electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif


      if(js .eq. jcacl2)then
        aer(ica_a,jliquid,ibin) = aer(ica_a,jliquid,ibin) +   &
                              electrolyte(js,jsolid,ibin)
        aer(icl_a,jliquid,ibin) = aer(icl_a,jliquid,ibin) +   &
                            2.*electrolyte(js,jsolid,ibin)
        electrolyte(js,jsolid,ibin) = 0.0
        return
      endif



      return
      end subroutine mesa_dissolve_small_salt






!***********************************************************************
! part of mesa: checks mesa convergence
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine mesa_convergence_criterion(itdum, ibin,   &
                                       iconverge_mass,   &
                                       iconverge_flux)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer itdum, ibin, iconverge_mass, iconverge_flux
! local variables
      integer je, js, iaer
      real frac_solid, xt



! check mass convergence
      iconverge_mass = 0	! default value = no convergence

      call electrolytes_to_ions(jsolid,ibin)

      dry_mass(ibin) = 0.0
      do iaer = 1, naer
        dry_mass(ibin) = dry_mass(ibin) +   &
                        aer(iaer,jsolid,ibin)*mw_aer_mac(iaer)*1.e-9
      enddo
      dry_mass(ibin) = dry_mass(ibin)   +    &  ! [g/m^3(air)]
        aer(ioc_a,jtotal,ibin)*1.e-9  +   &
        aer(ibc_a,jtotal,ibin)*1.e-9  +   &
        aer(ioin_a,jtotal,ibin)*1.e-9

      frac_solid = dry_mass(ibin)/total_dry_mass(ibin)

      if(frac_solid .ge. 0.98)then

        iconverge_mass = 1

      endif



! check relative driving force convergence
      iconverge_flux = 1
      do js = 1, nsalt
!        if(flux_sl(js).ne.0.0 .and. abs(phi_salt(js)).gt.0.01)then
        if(abs(phi_salt(js)).gt.0.02)then
          iconverge_flux = 0
        endif
      enddo


      return
      end subroutine mesa_convergence_criterion



! end of mesa package
!=======================================================================





























!***********************************************************************
! asceem: adaptive step coupled explicit euler method
!
! author: rahul a. zaveri
! update: apr 2005
!-----------------------------------------------------------------------
      subroutine asceem(dtchem)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real dtchem
! local variables
      integer ibin, iv, jp, isteps, nsteps_asceem, jcall,   &
              ieqblm
      real dtmax, t_in, t_new, t_old, t_out, delta_aer, xt,   &
           avg_asceem_steps


      t_in  = 0.0
      t_out = dtchem
      t_old = t_in

      jcall = jcall + 1

      isteps = 0

! set default alpha_gas and alpha_aer, and phi_volatile
      do iv = 1, naer_vol

        do ibin = 1, nbin_a
          phi_volatile(iv,ibin)     = 0.0
          phi_nh4no3(ibin) = 0.0
          phi_nh4cl(ibin)  = 0.0
        enddo

      enddo






! compute aerosol phase state before starting integration
      do ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call aerosol_phase_state(ibin)
        endif
      enddo



! compute new gas-aerosol mass transfer coefficients
      call aerosolmtc




! begin integration over transport time-step
!======================================================
! calculate fluxes
10    do 501 ibin = 1, nbin_a

        mxfer_massbal(ibin) = mno

        if(jphase(ibin) .eq. jsolid)then
          call asceem_flux_dry(ibin)
        elseif(jphase(ibin) .eq. jliquid)then
          call asceem_flux_wet(ibin)
        endif

501   continue



!-------------------------
! check if all the bins have reached equilibrium
      ieqblm = myes		! initalize to default (eqblm)

      do ibin = 1, nbin_a

        do iv = 1, naer_vol
          if(flux(iv, ibin) .ne. 0.0)then
            ieqblm = mno	! non-eqblm
          endif
        enddo

      enddo


      if(ieqblm .eq. myes)then
      do 502 ibin = 1, nbin_a
        if(jaerosolstate(ibin) .eq. no_aerosol)goto 502

        do iv = 1, naer_vol
          aer(iv,jtotal,ibin)=aer(iv,jsolid,ibin)+aer(iv,jliquid,ibin)
        enddo

        mxfer_massbal(ibin) = myes		! always yes in asceem
        if(mxfer_massbal(ibin) .eq. myes)then
          call conform_electrolytes(jtotal,ibin,xt) ! xfer_massbal + mdrh diagnosis
        else
          call form_electrolytes(jtotal,ibin,xt)	  ! for mdrh diagnosis
        endif


        if(jhyst_leg(ibin) .eq. jhyst_lo)then
          call asteem_update_phase_eqblm(ibin)
        else
          call do_full_deliquescence(ibin)	! simply do liquid <-- total
        endif

        call monitor_massbalance_out(ibin, isteps)

502   continue
      endif
!-------------------------




! calculate maximum possible internal time-step
      call asceem_calculate_dtmax(dtchem, dtmax)
      t_new = t_old + dtmax	! update time
      if(t_new .gt. t_out)then	! check if the new time step is too large
        dtmax = t_out - t_old
        t_new = t_out
      endif




      isteps = isteps + 1




! do internal time-step integration--------------

      do 40 ibin = 1, nbin_a
      if(jaerosolstate(ibin) .eq. no_aerosol)goto 40

        jp = jphase(ibin)

        do 20 iv = 1, naer_vol

          delta_aer = dtmax*flux(iv,ibin)

          aer(iv,jp,ibin)=real(dble(aer(iv,jp,ibin)) + dble(delta_aer))
          gas(iv)        =real( dble(gas(iv)) - dble(delta_aer) )

          aer(iv,jp,ibin)=max(aer(iv,jp,ibin), 0.0)
          gas(iv)        =max(gas(iv), 0.0)

20      continue

! degas excess nh3 (if present)
        call form_electrolytes(jp,ibin,xt)

! update jtotal
        do iv = 1, naer_vol
          aer(iv,jtotal,ibin)=aer(iv,jsolid,ibin)+aer(iv,jliquid,ibin)
        enddo

        if(mxfer_massbal(ibin) .eq. myes)then
          call conform_electrolytes(jtotal,ibin,xt) ! xfer_massbal + mdrh diagnosis
        else
          call form_electrolytes(jtotal,ibin,xt)	  ! for mdrh diagnosis
        endif

40    continue
! end internal time-step integration-------------


      do 50 ibin = 1, nbin_a

      if(jaerosolstate(ibin) .eq. no_aerosol)goto 50

        if(jhyst_leg(ibin) .eq. jhyst_lo)then
          call asteem_update_phase_eqblm(ibin)
        else
          call do_full_deliquescence(ibin)	! simply do liquid <-- total
        endif

50    continue



! update time
      t_old = t_new

      if(t_new .lt. 0.9999*t_out) goto 10
!================================================
! end of integration over the transport time-step


      nsteps_asceem = nsteps_asceem + isteps

      avg_asceem_steps = float(nsteps_asceem)/float(jcall)

      if(mod(jcall,1).eq.0)then
        write(6,*)'avg asceem steps =',avg_asceem_steps
      endif




      return
      end subroutine asceem














!***********************************************************************
! part of asceem: computes max time step for gas-aerosol integration
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asceem_calculate_dtmax(dtchem, dtmax)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real dtchem, dtmax
! local variables
      integer ibin, iv
      real alpha, h_aer, h_gas, h_max,   &
           h_gas_i(naer_vol),   &
           h_aer_i_m(naer_vol, nbin_a),   &
           h_aer_m(nbin_a)





! set alpha_gas and alpha_aer
      do ibin = 1, nbin_a
        do iv = 1, naer_vol

          alpha_gas(iv) = alpha_asteem

          if(madapt_alpha .eq. mon)then
            alpha_aer(iv,ibin) = max(abs(phi_volatile(iv,ibin)),   &
                                     alpha_asteem)
            alpha_aer(iv,ibin) = min(alpha_aer(iv,ibin), 10.0)

          else
            alpha_aer(iv,ibin) = alpha_asteem	! fixed alpha_aer

          endif

        enddo
      enddo





! gas-side
! calculate h_gas_i and h_gas

      h_gas = 2.e16
      do 5 ibin = 1, nbin_a
        do iv = 1, naer_vol

          h_gas_i(iv) = 1.e16
          if(flux(iv,ibin) .gt. 0.0)then
            h_gas_i(iv) = alpha_gas(iv)/kg(iv,ibin)
            h_gas = min(h_gas, h_gas_i(iv))
          endif

        enddo
5     continue


! aerosol-side
! calculate h_aer_i_m, h_aer_m, h_aer
      h_aer = 1.e16

      do 20 ibin = 1, nbin_a
        h_aer_m(ibin) = 4.e15		! initialize

        if(jaerosolstate(ibin) .eq. no_aerosol) goto 20

        call make_volatile_a(ibin)


! solid
      if(jphase(ibin) .eq. jsolid)then		! solid aerosol

        do 10 iv = 1, naer_vol
          h_aer_i_m(iv,ibin) = 2.e15		! initialize

          if(flux(iv,ibin).lt.0.)then		! aer -> gas
            alpha = min(alpha_aer(iv,ibin),0.5)
            h_aer_i_m(iv,ibin) = -alpha*volatile_a(iv)/flux(iv,ibin) ! degas completely
          endif

          h_aer_m(ibin) = min(h_aer_m(ibin),h_aer_i_m(iv,ibin))
          h_aer         = min(h_aer,        h_aer_i_m(iv,ibin))
10      continue





! liquid or mixed-phase
      elseif(jphase(ibin) .eq. jliquid)then

        do 11 iv = 1, naer_vol
          h_aer_i_m(iv,ibin) = 2.e15		! initialize

          if(flux(iv,ibin).gt.0. .and. 	   &  ! gas -> aer
             electrolyte(jcaco3,jsolid,ibin) .eq. 0.)then

            if(aer(iv,jliquid,ibin) .gt. 0.0)then
            h_aer_i_m(iv,ibin)=alpha_aer(iv,ibin)*aer(iv,jliquid,ibin)/	   &  ! aer(i,jliquid) =< aer(i,jtotal)
                                          flux(iv,ibin)
            endif

          elseif(flux(iv,ibin).lt.0. .and. volatile_a(iv).gt.0.0)then	! aer -> gas
            alpha = min(alpha_aer(iv,ibin), 0.5)
            h_aer_i_m(iv,ibin)=-alpha*volatile_a(iv)/flux(iv,ibin)
          endif

          h_aer_m(ibin) = min(h_aer_m(ibin),h_aer_i_m(iv,ibin))
          h_aer         = min(h_aer,        h_aer_i_m(iv,ibin))
11      continue

      endif





20    continue


      h_max = min(h_aer, h_gas)

      dtmax = min(dtchem, h_max)
      h_max = dtmax


      if(dtmax .le. 1.0e-10)then
        write(6,*)' dtmax = ', dtmax
      endif

      return
      end subroutine asceem_calculate_dtmax


















!***********************************************************************
! part of asceem: computes gas-aerosol fluxes over dry aerosols.
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asceem_flux_dry(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv
      real xt, xnh4, g_nh3_hno3, g_nh3_hcl,   &
           a_nh4_no3, a_nh4_cl,   &
           prod_nh4no3, prod_nh4cl,   &
           volatile_cl, volatile_no3





      call calculate_xt(ibin,jsolid,xt)

! h2so4
      flux(ih2so4_g,ibin)         = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      phi_volatile(ih2so4_g,ibin) = 1.0

!-----------------------------------------------------------------
! case 1: sulfate-rich domain

      if(xt.lt.2.0 .and. xt.ge.0.)then	! excess sulfate (acidic)

	call asceem_flux_dry_case1(ibin)

        return
      endif

!-----------------------------------------------------------------
! case 2:  caco3 > 0 absorb all acids (and indirectly degas co2)

      if(electrolyte(jcaco3,jtotal,ibin) .gt. 0.0)then

        call asceem_flux_dry_case2(ibin)

        return
      endif

!-----------------------------------------------------------------
! case 3: hno3 and hcl exchange may happen here

      volatile_cl  = electrolyte(jnacl,jsolid,ibin) +   &
                     electrolyte(jcacl2,jsolid,ibin)


      if(volatile_cl .gt. 0.0 .and.   &
         gas(ihno3_g).gt. 0.0 )then

        call asceem_flux_dry_case3(ibin)

        return
      endif

!-----------------------------------------------------------------
! case 4: nh4no3 or nh4cl or both may be active

      prod_nh4no3 = max( (gas(inh3_g)*gas(ihno3_g)-keq_sg(1)), 0.0) +   &
                    epercent(jnh4no3,jsolid,ibin)
      prod_nh4cl  = max( (gas(inh3_g)*gas(ihcl_g) -keq_sg(2)), 0.0) +   &
                    epercent(jnh4cl,jsolid,ibin)

      if(prod_nh4no3 .gt. 0.0 .or. prod_nh4cl .gt. 0.0)then
        call asceem_flux_dry_case4(ibin)
        return
      endif

!-----------------------------------------------------------------
! case 5: condense h2so4 and degas hno3
      volatile_no3 = epercent(jnano3,jsolid,ibin) +   &
                     epercent(jcano3,jsolid,ibin)

      if(volatile_no3 .gt. 0.0 .and.   &
         gas(ih2so4_g).gt. 0.0 )then

        call asceem_flux_dry_case5(ibin)

        return
      endif

!-------------------------------------------------------------------
! case 6: probably pure (nh4)2so4 particle.
        flux(ih2so4_g,ibin) = kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin)  = 0.0
        flux(ihcl_g,ibin)   = 0.0
        flux(inh3_g,ibin)   = min( kg(inh3_g,ibin)*gas(inh3_g),   &
                                 2.*flux(ih2so4_g,ibin) )
        return

      end subroutine asceem_flux_dry

!----------------------------------------------------------------------












!***********************************************************************
! part of asceem: subroutines for various flux_dry cases
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
!
!
! case 1: sulfate-rich domain
!
      subroutine asceem_flux_dry_case1(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin



        sfc_a(ih2so4_g)= 0.0
        sfc_a(ihno3_g) = gas(ihno3_g)
        sfc_a(ihcl_g)  = gas(ihcl_g)
        sfc_a(inh3_g)  = 0

        df_gas(ih2so4_g,ibin) = gas(ih2so4_g)
        df_gas(ihno3_g,ibin)  = 0.0
        df_gas(ihcl_g,ibin)   = 0.0
        df_gas(inh3_g,ibin)   = gas(inh3_g)

        phi_volatile(ihno3_g,ibin) = 0.0
        phi_volatile(ihcl_g,ibin)  = 0.0
        phi_volatile(inh3_g,ibin)  = 1.0

        flux(ih2so4_g,ibin)   = kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin)    = 0.0
        flux(ihcl_g,ibin)     = 0.0
        flux(inh3_g,ibin)     = kg(inh3_g,ibin)*gas(inh3_g)


      return
      end subroutine asceem_flux_dry_case1






! case 2:  caco3 > 0 absorb all acids (and indirectly degas co2)
!
      subroutine asceem_flux_dry_case2(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


      mxfer_massbal(ibin) = myes		! degas co2 via mass bal

      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = 0.0
      sfc_a(ihcl_g)  = 0.0
      sfc_a(inh3_g)  = gas(inh3_g)

      df_gas(ih2so4_g,ibin) = gas(ih2so4_g)
      df_gas(ihno3_g,ibin)  = gas(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)
      df_gas(inh3_g,ibin)   = 0.0

      phi_volatile(ih2so4_g,ibin)= 1.0
      phi_volatile(ihno3_g,ibin) = 1.0
      phi_volatile(ihcl_g,ibin)  = 1.0
      phi_volatile(inh3_g,ibin)  = 0.0

      flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihno3_g,ibin) = kg(ihno3_g,ibin)*gas(ihno3_g)
      flux(ihcl_g,ibin)  = kg(ihcl_g,ibin)*gas(ihcl_g)
      flux(inh3_g,ibin)  = 0.0


      return
      end subroutine asceem_flux_dry_case2










! case 3: hno3 and hcl exchange may happen here
!
      subroutine asceem_flux_dry_case3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


! just degas hcl from nacl or cacl2 by flux balance with 2 h2so4 and hno3
      mxfer_massbal(ibin) = myes

      flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihno3_g,ibin) = kg(ihno3_g,ibin)*gas(ihno3_g)
      flux(ihcl_g,ibin)  = 0.0 ! degas via mass bal
      flux(inh3_g,ibin)  = 0.0


      return
      end subroutine asceem_flux_dry_case3






! case 4: nh4no3 and/or nh4cl may be active
      subroutine asceem_flux_dry_case4(ibin)

!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iactive_nh4no3, iactive_nh4cl, iactive
      real gnh3_hno3, gnh3_hcl, pcnt_nh4no3, pcnt_nh4cl,   &
           a, b, c, ratio_flux,   &
           flux_nh3_max, flux_nh3_est,   &
           flux_nh3_max_d, flux_nh3_est_d
! function
!     real quadratic


!-------------------
! set default values for flags
      iactive_nh4no3 = 1
      iactive_nh4cl  = 2

!-------------------
! compute diagnostic products and ratios
      gnh3_hno3   = gas(inh3_g)*gas(ihno3_g)
      gnh3_hcl    = gas(inh3_g)*gas(ihcl_g)

      phi_nh4no3(ibin) = abs(keq_sg(1) - gnh3_hno3)/   &
                         max(keq_sg(1), gnh3_hno3)
      phi_nh4cl(ibin)  = abs(keq_sg(2) - gnh3_hcl)/   &
                         max(keq_sg(2), gnh3_hcl)

      pcnt_nh4no3 = epercent(jnh4no3,jsolid,ibin)
      pcnt_nh4cl  = epercent(jnh4cl, jsolid,ibin)


!-------------------
! now determine if nh4no3 and/or nh4cl are active or significant

! nh4no3
      if( phi_nh4no3(ibin) .lt. 0.02 )then
        iactive_nh4no3 = 0
      elseif(gnh3_hno3.lt.keq_sg(1) .and. pcnt_nh4no3.lt.1.0)then
        iactive_nh4no3 = 0
        call degas_solid_nh4no3(ibin)
      endif

! nh4cl
      if( phi_nh4cl(ibin) .lt. 0.02 )then
        iactive_nh4cl = 0
      elseif(gnh3_hcl.lt.keq_sg(2) .and. pcnt_nh4cl.lt.1.0)then
        iactive_nh4cl = 0
        call degas_solid_nh4cl(ibin)
      endif


      iactive = iactive_nh4no3 + iactive_nh4cl

! check the outcome
      if(iactive .eq. 0)then
        flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      endif

      goto (1,2,3),iactive

!---------------------------------
! only nh4no3 is active
1     flux(ih2so4_g,ibin) = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihcl_g,ibin)   = 0.0

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)   &
          + 2.0*flux(ih2so4_g,ibin)
      c = -(kg(ihno3_g,ibin)*keq_sg(1))

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_sg(1)/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)

      df_gas(ihno3_g,ibin) = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g)  - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)=     df_gas(ihno3_g,ibin)/   &
                                max(sfc_a(ihno3_g), 1.e-10)
      phi_volatile(ihcl_g,ibin) = 0.0
      phi_volatile(inh3_g,ibin) =     df_gas(inh3_g,ibin)/   &
                                max(sfc_a(inh3_g), 1.e-10)


      if(gnh3_hno3      .gt. keq_sg(1) .and.   &
         sfc_a(ihno3_g) .gt. gas(ihno3_g) )then  ! degas hno3 via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihno3_g,ibin)= 0.0
        df_gas(ihno3_g,ibin) = 0.0
        flux(ihno3_g,ibin)   = 0.0
      else
        flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      endif


      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else ! reduce hno3 flux as necessary

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = max(flux_nh3_max-flux(ih2so4_g,ibin),0.0)

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihno3_g,ibin) = df_gas(ihno3_g,ibin)/	   &  ! recompute phi_volatile(ihno3_g,ibin)
                                     max(sfc_a(ihno3_g), 1.e-10)
      endif

      return

!-----------------
! only nh4cl is active
2     flux(ih2so4_g,ibin) = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihno3_g,ibin)  = 0.0

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)   &
          + 2.0*flux(ih2so4_g,ibin)
      c = -(kg(ihcl_g,ibin)*keq_sg(2))

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihcl_g ) = keq_sg(2)/sfc_a(inh3_g)
      sfc_a(ihno3_g) = gas(ihno3_g)


      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihcl_g,ibin) =     df_gas(ihcl_g,ibin)/   &
                                max(sfc_a(ihcl_g), 1.e-10)
      phi_volatile(ihno3_g,ibin)= 0.0
      phi_volatile(inh3_g,ibin) =     df_gas(inh3_g,ibin)/   &
                                max(sfc_a(inh3_g), 1.e-10)


      if(gnh3_hcl      .gt. keq_sg(2) .and.   &
         sfc_a(ihcl_g) .gt. gas(ihcl_g) )then  ! degas hcl via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihcl_g,ibin) = 0.0
        df_gas(ihcl_g,ibin)  = 0.0
        flux(ihcl_g,ibin)    = 0.0
      else
        flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
      endif

      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else ! reduce hcl flux as necessary

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihcl_g,ibin)  = max(flux_nh3_max-flux(ih2so4_g,ibin),0.0)

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihcl_g,ibin)  = df_gas(ihcl_g,ibin)/	   &  ! recompute phi_volatile(ihcl_g,ibin)
                                     max(sfc_a(ihcl_g), 1.e-10)

      endif

      return

!-----------------
! both nh4no3 and nh4cl are active
3     continue

      flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)   &
          + 2.0*flux(ih2so4_g,ibin)
      c = -( kg(ihno3_g,ibin)*keq_sg(1) + kg(ihcl_g,ibin)*keq_sg(2) )

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_sg(1)/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = keq_sg(2)/sfc_a(inh3_g)
      df_gas(ihno3_g,ibin)  = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)  - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)   = gas(inh3_g)  - sfc_a(inh3_g)

      if(gnh3_hno3    .gt. keq_sg(1) .and.   &
         sfc_a(ihno3_g) .gt. gas(ihno3_g) )then  ! degas hno3 via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihno3_g,ibin)= 0.0
        df_gas(ihno3_g,ibin) = 0.0
        flux(ihno3_g,ibin)   = 0.0
      else
        flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      endif


      if(gnh3_hcl    .gt. keq_sg(2) .and.   &
         sfc_a(ihcl_g) .gt. gas(ihcl_g) )then  ! degas hcl via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihcl_g,ibin) = 0.0
        df_gas(ihcl_g,ibin)  = 0.0
        flux(ihcl_g,ibin)    = 0.0
      else
        flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
      endif


      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else			! reduce hno3 and hcl fluxes as necessary so that nh3 flux = flux_nh3_max

        flux_nh3_est_d = max(flux_nh3_est-flux(ih2so4_g,ibin), 0.0)
        flux_nh3_max_d = max(flux_nh3_max-flux(ih2so4_g,ibin), 0.0)

        if(flux_nh3_max_d .eq. 0.0)then
          ratio_flux = 0.0
        else
          ratio_flux = flux_nh3_max_d/flux_nh3_est_d
        endif

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = flux(ihno3_g,ibin)*ratio_flux
        flux(ihcl_g, ibin) = flux(ihcl_g,ibin) *ratio_flux

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihno3_g,ibin) = df_gas(ihno3_g,ibin)/	   &  ! recompute phi_volatile(ihno3_g,ibin)
                                     max(sfc_a(ihno3_g), 1.e-10)
        phi_volatile(ihcl_g,ibin)  = df_gas(ihcl_g,ibin)/	   &  ! recompute phi_volatile(ihcl_g,ibin)
                                     max(sfc_a(ihcl_g), 1.e-10)

      endif



      return
      end subroutine asceem_flux_dry_case4



















      subroutine asceem_flux_dry_case5(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


! just degas hno3 from nano3 or cano3 by flux balance with h2so4
      mxfer_massbal(ibin) = myes

      flux(ih2so4_g,ibin) = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihno3_g,ibin)  = 0.0
      flux(ihcl_g,ibin)   = 0.0
      flux(inh3_g,ibin)   = 0.0


      return
      end subroutine asceem_flux_dry_case5

!----------------------------------------------------------------------





































!***********************************************************************
! part of asceem: computes gas-aerosol fluxes over wet aerosols
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asceem_flux_wet(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv, iadjust, iadjust_intermed, icontinue_case4
      real xt, xnh4, g_nh3_hno3, g_nh3_hcl,   &
           a_nh4_no3, a_nh4_cl, a_no3, a_cl,   &
           prod_nh4no3, prod_nh4cl



!c      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
!c      call degas_acids(jliquid,ibin,xt)
      call ions_to_electrolytes(jliquid,ibin,xt)  	! for water content calculation
      call compute_activities(ibin)

      if(water_a(ibin) .eq. 0.0)then
	write(6,*)'water is zero in liquid phase'
	write(6,*)'stopping in asceem_flux_wet'
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping in asceem_flux_wet' )
      endif

      call calculate_xt(ibin,jliquid,xt)

! calculate xnh4
      if(aer(iso4_a,jliquid,ibin).gt.0.0)then
        xnh4 = aer(inh4_a,jliquid,ibin)/aer(iso4_a,jliquid,ibin)
      else
        xnh4 = -1.0
      endif


! h2so4
      flux(ih2so4_g,ibin)    = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      df_gas(ih2so4_g,ibin)  = gas(ih2so4_g)
      phi_volatile(ih2so4_g,ibin) = 1.0


!-------------------------------------------------------------------
! case 1: sulfate-rich domain

      if(xt.lt.2.0 .and. xt.ge.0.)then
        call asceem_flux_wet_case1(ibin)
        return
      endif

!-------------------------------------------------------------------
! case 2: caco3 > 0 absorb acids (and indirectly degas co2)

      if(electrolyte(jcaco3,jsolid,ibin) .gt. 0.0)then
        call asceem_flux_wet_case2(ibin)
        return
      endif

!-------------------------------------------------------------------
! do some small adjustments before deciding case 3
!
      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
      iadjust = mno		! default
      iadjust_intermed = mno	! default

! nh4no3
      g_nh3_hno3= gas(inh3_g)*gas(ihno3_g)
      a_nh4_no3 = epercent(jnh4no3,jliquid,ibin)

      if(g_nh3_hno3 .gt. 0.0 .and. a_nh4_no3 .lt. 0.1)then
        call absorb_tiny_nh4no3(ibin)
        iadjust = myes
        iadjust_intermed = myes
      elseif(g_nh3_hno3 .eq. 0.0 .and. a_nh4_no3 .gt. 0.0)then
        call degas_tiny_nh4no3(ibin)
        iadjust = myes
        iadjust_intermed = myes
      endif

      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
        iadjust_intermed = mno	! reset
      endif

! nh4cl
      g_nh3_hcl= gas(inh3_g)*gas(ihcl_g)
      a_nh4_cl = epercent(jnh4cl,jliquid,ibin)

      if(g_nh3_hcl .gt. 0.0 .and. a_nh4_cl .lt. 0.1)then
        call absorb_tiny_nh4cl(ibin)
        iadjust = myes
        iadjust_intermed = myes
      elseif(g_nh3_hcl .eq. 0.0 .and. a_nh4_cl .gt. 0.0)then
        call degas_tiny_nh4cl(ibin)
        iadjust = myes
        iadjust_intermed = myes
      endif

      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
        iadjust_intermed = mno	! reset
      endif

! hno3
      a_no3 = aer_percent(ino3_a,jliquid,ibin)
      if(gas(ihno3_g).gt.0. .and. a_no3 .lt. 0.1 .and.   &
         aer(icl_a,jliquid,ibin) .gt. 0.0)then
        call absorb_tiny_hno3(ibin)	! and degas tiny hcl
        iadjust = myes
        iadjust_intermed = myes
      endif


! hcl
      a_cl = aer_percent(icl_a,jliquid,ibin)
      if(gas(ihcl_g).gt.0. .and. a_cl .lt. 0.1 .and.   &
         aer(ino3_a,jliquid,ibin) .gt. 0.0)then
        call absorb_tiny_hcl(ibin)	! and degas tiny hno3
        iadjust = myes
        iadjust_intermed = myes
      endif


      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
      endif

      if(iadjust .eq. myes)then
        call compute_activities(ibin)			! update after adjustments
      endif


! all adjustments done...

!--------
      prod_nh4no3 = gas(inh3_g)*gas(ihno3_g) + activity(jnh4no3,ibin)
      prod_nh4cl  = gas(inh3_g)*gas(ihcl_g)  + activity(jnh4cl,ibin)
!
! case 3: nh4no3 and/or nh4cl maybe active
      if(prod_nh4no3 .gt. 0.0 .or. prod_nh4cl .gt. 0.0)then
        call asceem_flux_wet_case3(ibin, icontinue_case4)
        if(icontinue_case4 .eq. mno)return
      endif

!-------------------------------------------------------------------
! case 4: nh3 & nh4 ~ 0 (in gas and aerosol). hno3 and hcl exchange may happen here

      if(ma(ja_no3,ibin)*ma(ja_cl,ibin) .gt. 0.0)then  ! nh3 & nh4 ~ 0
        call asceem_flux_wet_case4(ibin)
        return
      endif

!-------------------------------------------------------------------
! case 5: default (may degas nh3)

        call asceem_flux_wet_case5(ibin)
        return

      end subroutine asceem_flux_wet


















!***********************************************************************
! part of asceem: subroutines for various flux_wet cases
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
!
!
! case 1: sulfate-rich domain
!
      subroutine asceem_flux_wet_case1(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin



      if(aer(inh4_a,jliquid,ibin) .eq. 0. .and. gas(inh3_g) .gt. 0.)then
        call absorb_tiny_nh3(ibin)
      endif

      call equilibrate_acids(ibin) 	  	! updates aer(icl_a, ino3_a, jtotal), activity(jhcl,jhno3)

      sfc_a(ihno3_g) = gas(ihno3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)
      sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                      (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

      df_gas(ihno3_g,ibin)  = 0.0
      df_gas(ihcl_g,ibin)   = 0.0
      df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

      phi_volatile(ihno3_g,ibin) = 0.0
      phi_volatile(ihcl_g,ibin)  = 0.0
      phi_volatile(inh3_g,ibin)  =   df_gas(inh3_g,ibin)/   &
                                 max(sfc_a(inh3_g), 1.e-10)

      flux(ihno3_g,ibin)    = 0.0
      flux(ihcl_g,ibin)     = 0.0

! check for equilibrium
      if(phi_volatile(inh3_g,ibin) .lt. 0.01 .and.   &
         flux(ih2so4_g,ibin)  .eq. 0.0)then
        flux(inh3_g,ibin)   = 0.0
      else
        flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      endif


      return
      end subroutine asceem_flux_wet_case1











! case 2: caco3 > 0
!
      subroutine asceem_flux_wet_case2(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, iv


      mxfer_massbal(ibin) = myes

      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = 0.0
      sfc_a(ihcl_g)  = 0.0
      sfc_a(inh3_g)  = gas(inh3_g)

      df_gas(ih2so4_g,ibin) = gas(ih2so4_g)
      df_gas(ihno3_g,ibin)  = gas(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)
      df_gas(inh3_g,ibin)   = 0.0

      phi_volatile(ih2so4_g,ibin)= 1.0
      phi_volatile(ihno3_g,ibin) = 1.0
      phi_volatile(ihcl_g,ibin)  = 1.0
      phi_volatile(inh3_g,ibin)  = 0.0

      do iv = 1, naer_vol
        flux(iv,ibin)     = kg(iv,ibin)*df_gas(iv,ibin)
      enddo

      mc(jc_h,ibin) = sqrt(keq_ll(3))
      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine asceem_flux_wet_case2













! case 3: both mh4no3 and/or nh4cl may be active
      subroutine asceem_flux_wet_case3(ibin, icontinue_case4)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, icontinue_case4
! local variables
      integer iv, iactive_nh4no3, iactive_nh4cl, iactive
      real gnh3_hno3, gnh3_hcl, beta_nh4no3, beta_nh4cl,   &
           keq_nh4no3, keq_nh4cl, pcnt_nh4no3, pcnt_nh4cl,   &
           ratio_flux,a, b, c, hplus1, hplus2,   &
           flux_nh3_max, flux_nh3_est,   &
           flux_nh3_max_d, flux_nh3_est_d,   &
           xt
! function
!     real quadratic



!-------------------
! set default values for flags
      iactive_nh4no3 = 1
      iactive_nh4cl  = 2
      icontinue_case4 = mno	! default

!-------------------
! compute diagnostic products and ratios
      gnh3_hno3   = gas(inh3_g)*gas(ihno3_g)
      gnh3_hcl    = gas(inh3_g)*gas(ihcl_g)

      keq_nh4no3  = activity(jnh4no3,ibin)*kp_nh4no3	! = [nh3]s * [hno3]s
      keq_nh4cl   = activity(jnh4cl,ibin)*kp_nh4cl	! = [nh3]s * [hcl]s

      beta_nh4no3 = gas(inh3_g)*gas(ihno3_g)/keq_nh4no3_0
      beta_nh4cl  = gas(inh3_g)*gas(ihcl_g)/keq_nh4cl_0

      if(gnh3_hno3 .gt. 0. .or. keq_nh4no3 .gt. 0.)then
        phi_nh4no3(ibin) =    (gnh3_hno3 - keq_nh4no3)/   &
                           max(gnh3_hno3, keq_nh4no3)
      else
        phi_nh4no3(ibin) = 0.0
      endif

      if(gnh3_hcl .gt. 0. .or. keq_nh4cl .gt. 0.)then
        phi_nh4cl(ibin)  =    (gnh3_hcl - keq_nh4cl)/   &
                           max(gnh3_hcl, keq_nh4cl)
      else
        phi_nh4cl(ibin)  = 0.0
      endif


!
! the following checks are order sensitive
!-------------------
! first check if the bin has reached equilibrium
      if(abs(phi_nh4no3(ibin)).lt.0.02 .and.   &
         abs(phi_nh4cl(ibin)) .lt.0.02)then
        iactive_nh4no3 = 0
        iactive_nh4cl  = 0
        iactive = 0
        flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return	! yes, the bin has reached equilibrium. quit
      endif


!------------------
! now check if nh4no3 and/or nh4cl want to evaporate completely
      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
      pcnt_nh4no3 = epercent(jnh4no3,jliquid,ibin)
      pcnt_nh4cl  = epercent(jnh4cl, jliquid,ibin)

      if( (gnh3_hno3.le.keq_nh4no3 .and. pcnt_nh4no3.lt.1.0) .and.   &
          (gnh3_hcl .le.keq_nh4cl  .and. pcnt_nh4cl .lt.1.0) )then
        if(electrolyte(jnh4so4,jliquid,ibin) .gt. 0.0)then
          call evaporate_nh4no3_nh4cl(ibin)
          return
        else
          icontinue_case4 = myes ! its a nacl/nano3/cacl2/cano3 particle with negligible nh3, nh4
          return
        endif
      endif

!--------------------
! now determine if nh4no3 and/or nh4cl are active or significant

! nh4no3
      if( (gnh3_hno3.gt.keq_nh4no3 .and. beta_nh4no3.lt.0.03) .or.   &
          (abs(phi_nh4no3(ibin)) .lt. 0.02) )then
        iactive_nh4no3 = 0
      elseif(gnh3_hno3.lt.keq_nh4no3 .and. pcnt_nh4no3.lt.1.0)then
        iactive_nh4no3 = 0
        if(pcnt_nh4no3 .gt. 0.5)call evaporate_half_nh4no3(ibin)
      endif

! nh4cl
      if( (gnh3_hcl.gt.keq_nh4cl .and. beta_nh4cl.lt.0.03) .or.   &
          (abs(phi_nh4cl(ibin)) .lt. 0.02) )then
        iactive_nh4cl = 0
      elseif(gnh3_hcl.lt.keq_nh4cl .and. pcnt_nh4cl.lt.1.0)then
        iactive_nh4cl = 0
        if(pcnt_nh4cl .gt. 0.5)call evaporate_half_nh4cl(ibin)
      endif

      iactive = iactive_nh4no3 + iactive_nh4cl

! check the outcome
      if(iactive                          .eq. 0    .and.   &
         phi_nh4no3(ibin)                 .gt. 0.0  .and.   &
         phi_nh4cl(ibin)                  .gt. 0.0 )then
        flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      elseif(iactive                      .eq. 0    .and.   &
         abs(phi_nh4no3(ibin))            .gt. 0.02 .and.   &
         abs(phi_nh4cl(ibin))             .gt. 0.02 .and.   &
         aer_percent(inh4_a,jliquid,ibin) .lt. 1.0  .and.   &
         (aer_percent(icl_a,jliquid,ibin) .gt. 1.0  .or.   &
          aer_percent(ino3_a,jliquid,ibin).gt. 1.0) )then
        icontinue_case4 = myes		! nh3 and nh4 seems to be insignificant
        return				! therefore continue with case 4
      elseif(iactive .eq. 0)then
        flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      endif

      goto (1,2,3),iactive

!---------------------------------
! only nh4no3 is active
1     flux(ihcl_g,ibin)    = 0.0

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)   &
          + 2.0*flux(ih2so4_g,ibin)
      c = -(kg(ihno3_g,ibin)*keq_nh4no3)

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_nh4no3/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)


      df_gas(ihno3_g,ibin) = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g)  - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)=   df_gas(ihno3_g,ibin)/   &
                                max(sfc_a(ihno3_g), 1.e-10)
      phi_volatile(ihcl_g,ibin) = 0.0
      phi_volatile(inh3_g,ibin) =   df_gas(inh3_g,ibin)/   &
                                max(sfc_a(inh3_g), 1.e-10)


      if(gnh3_hno3      .gt. keq_nh4no3 .and.   &
         sfc_a(ihno3_g) .gt. gas(ihno3_g) )then  ! degas hno3 via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihno3_g,ibin)= 0.0
        df_gas(ihno3_g,ibin) = 0.0
        flux(ihno3_g,ibin)   = 0.0
      else
        flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      endif


      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else ! reduce hno3 flux as necessary

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = max(flux_nh3_max-flux(ih2so4_g,ibin),0.0)

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihno3_g,ibin) = df_gas(ihno3_g,ibin)/	   &  ! recompute phi_volatile(ihno3_g,ibin)
                                     max(sfc_a(ihno3_g), 1.e-10)
      endif


      mc(jc_h,ibin) = keq_gl(3)*sfc_a(ihno3_g)/   &
                     (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return

!-----------------
! only nh4cl is active
2     flux(ihno3_g,ibin)  = 0.0

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)
      c = -(kg(ihcl_g,ibin)*keq_nh4cl)

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihcl_g)  = keq_nh4cl /sfc_a(inh3_g)
      sfc_a(ihno3_g) = gas(ihno3_g)


      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)= 0.0
      phi_volatile(ihcl_g,ibin) =   df_gas(ihcl_g,ibin)/   &
                                max(sfc_a(ihcl_g), 1.e-10)
      phi_volatile(inh3_g,ibin) =   df_gas(inh3_g,ibin)/   &
                                max(sfc_a(inh3_g), 1.e-10)


      if(gnh3_hcl    .gt. keq_nh4cl .and.   &
         sfc_a(ihcl_g) .gt. gas(ihcl_g) )then  ! degas hcl via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihcl_g,ibin) = 0.0
        df_gas(ihcl_g,ibin)  = 0.0
        flux(ihcl_g,ibin)    = 0.0
      else
        flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
      endif

      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else			! reduce hcl flux as necessary

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihcl_g,ibin)  = max(flux_nh3_max-flux(ih2so4_g,ibin),0.0)

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihcl_g,ibin)  = df_gas(ihcl_g,ibin)/	   &  ! recompute phi_volatile(ihcl_g,ibin)
                                     max(sfc_a(ihcl_g), 1.e-10)

      endif


      mc(jc_h,ibin) = keq_gl(4)*sfc_a(ihcl_g)/   &
                     (gam(jhcl,ibin)**2 * ma(ja_cl,ibin))

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return

!-----------------
! both nh4no3 and nh4cl are active
3     continue

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)   &
          + 2.0*flux(ih2so4_g,ibin)
      c = -( kg(ihno3_g,ibin)*keq_nh4no3 + kg(ihcl_g,ibin)*keq_nh4cl )

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_nh4no3/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = keq_nh4cl/sfc_a(inh3_g)
      df_gas(ihno3_g,ibin)  = gas(ihno3_g)- sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)  - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)   = gas(inh3_g)  - sfc_a(inh3_g)


      if(gnh3_hno3      .gt. keq_nh4no3 .and.   &
         sfc_a(ihno3_g) .gt. gas(ihno3_g) )then  ! degas hno3 via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihno3_g,ibin)= 0.0
        df_gas(ihno3_g,ibin) = 0.0
        flux(ihno3_g,ibin)   = 0.0
        hplus1 = sqrt(keq_ll(3))
      else
        flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
        hplus1 = keq_gl(3)*sfc_a(ihno3_g)/   &
                 (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))
      endif


      if(gnh3_hcl      .gt. keq_nh4cl .and.   &
         sfc_a(ihcl_g) .gt. gas(ihcl_g) )then  ! degas hcl via mass bal
        mxfer_massbal(ibin) = myes
        phi_volatile(ihcl_g,ibin) = 0.0
        df_gas(ihcl_g,ibin)  = 0.0
        flux(ihcl_g,ibin)    = 0.0
        hplus2 = sqrt(keq_ll(3))
      else
        flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
        hplus2 = keq_gl(4)*sfc_a(ihcl_g)/   &
                 (gam(jhcl,ibin)**2 * ma(ja_cl,ibin))
      endif



      flux_nh3_est = 2.*flux(ih2so4_g,ibin) +   &
                        flux(ihno3_g,ibin)  +   &
                        flux(ihcl_g,ibin)

      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        phi_volatile(inh3_g,ibin) = df_gas(inh3_g,ibin)/	   &  ! recompute phi_volatile(inh3_g,ibin)
                                    max(sfc_a(inh3_g), 1.e-10)

      else			! reduce hno3 and hcl fluxes as necessary so that nh3 flux = flux_nh3_max

        flux_nh3_est_d = max(flux_nh3_est-flux(ih2so4_g,ibin), 0.0)
        flux_nh3_max_d = max(flux_nh3_max-flux(ih2so4_g,ibin), 0.0)

        if(flux_nh3_max_d .eq. 0.0)then
          ratio_flux = 0.0
        else
          ratio_flux = flux_nh3_max_d/flux_nh3_est_d
        endif

        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = flux(ihno3_g,ibin)*ratio_flux
        flux(ihcl_g, ibin) = flux(ihcl_g,ibin) *ratio_flux

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

        phi_volatile(inh3_g,ibin)  = 10.0
        phi_volatile(ihno3_g,ibin) = df_gas(ihno3_g,ibin)/	   &  ! recompute phi_volatile(ihno3_g,ibin)
                                     max(sfc_a(ihno3_g), 1.e-10)
        phi_volatile(ihcl_g,ibin)  = df_gas(ihcl_g,ibin)/	   &  ! recompute phi_volatile(ihcl_g,ibin)
                                     max(sfc_a(ihcl_g), 1.e-10)

      endif


      mc(jc_h,ibin) = max(hplus1, hplus2)

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))


      return
      end subroutine asceem_flux_wet_case3
















! case 4: nh3 = 0 (in gas and aerosol)	! may have trouble
!
      subroutine asceem_flux_wet_case4(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real gas_eqb_ratio, gas_act_ratio, phi_ratio



! now diagnose the situation

      gas_eqb_ratio = (keq_gl(4)*ma(ja_no3,ibin)*gam(jhno3,ibin)**2)/     &  ! ce,hno3/ce,hcl
                      (keq_gl(3)*ma(ja_cl ,ibin)*gam(jhcl,ibin)**2)

      gas_act_ratio = gas(ihno3_g)/gas(ihcl_g)

      phi_ratio = abs(gas_eqb_ratio - gas_act_ratio)/   &
                  max(gas_eqb_ratio , gas_act_ratio)


! check if equilibrium reached...
      if(phi_ratio .lt. 0.01)then
          flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)
          flux(ihno3_g,ibin) = 0.0
          flux(ihcl_g,ibin)  = 0.0
          flux(inh3_g,ibin)  = 0.0
          return
      endif


! compute equilibrium surface concentrations
      flux(ih2so4_g,ibin)= kg(ih2so4_g,ibin)*gas(ih2so4_g)

      sfc_a(ihcl_g) = (2.*flux(ih2so4_g,ibin)      +   &
                      kg(ihno3_g,ibin)*gas(ihno3_g) +   &
                      kg(ihcl_g,ibin) *gas(ihcl_g))/   &
                     (kg(ihcl_g,ibin)+gas_eqb_ratio*kg(ihno3_g,ibin))

      sfc_a(ihno3_g) = gas_eqb_ratio*sfc_a(ihcl_g)

      df_gas(ihno3_g,ibin) = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

      phi_volatile(ihno3_g,ibin) =   df_gas(ihno3_g,ibin)/   &
                                 max(sfc_a(ihno3_g), 1.e-10)
      phi_volatile(ihcl_g,ibin)  =   df_gas(ihcl_g,ibin)/   &
                                 max(sfc_a(ihcl_g), 1.e-10)


      flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      flux(ihcl_g,ibin)    = kg(ihcl_g,ibin) *df_gas(ihcl_g, ibin)
      flux(inh3_g,ibin)    = 0.0



! check if degassing species is less than 1% of the total particle moles
      if(flux(ihcl_g,ibin) .lt. 0.0 .and.   &
         aer_percent(icl_a,jliquid,ibin) .lt. 1.0)then
          flux(ihcl_g,ibin)  = 0.0
          flux(ihno3_g,ibin) = -2.0*flux(ih2so4_g,ibin)
      endif

      if(flux(ihno3_g,ibin) .lt. 0.0 .and.   &
         aer_percent(ino3_a,jliquid,ibin) .lt. 1.0)then
          flux(ihno3_g,ibin) = 0.0
          flux(ihcl_g,ibin)  = -2.0*flux(ih2so4_g,ibin)
      endif

! check hcl again
      if(flux(ihcl_g,ibin) .lt. 0.0 .and.   &
         aer_percent(icl_a,jliquid,ibin) .lt. 1.0)then
          flux(ihcl_g,ibin)  = 0.0
        mxfer_massbal(ibin) = myes
      endif



      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine asceem_flux_wet_case4










! case 5
      subroutine asceem_flux_wet_case5(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


      call equilibrate_acids(ibin)	! hno3/hcl may be > 0 in the gas phase

      mc(jc_h,ibin) = max(sqrt(keq_ll(3)), mc(jc_h,ibin))

      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = gas(ihno3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)
      sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                      (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

      df_gas(ih2so4_g,ibin)= gas(ih2so4_g)
      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin) = 0.0
      phi_volatile(ihcl_g,ibin)  = 0.0
      phi_volatile(inh3_g,ibin)  =   df_gas(inh3_g,ibin)/   &
                                 max(sfc_a(inh3_g), 1.e-10)


      if(abs(phi_volatile(inh3_g,ibin)) .lt. 0.01)then
        df_gas(inh3_g,ibin) = 0.0
      endif

      flux(ih2so4_g,ibin) = kg(ih2so4_g,ibin)*gas(ih2so4_g)
      flux(ihno3_g,ibin)  = 0.0
      flux(ihcl_g,ibin)   = 0.0
      flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine asceem_flux_wet_case5

!**********************************************************************
! end of asceem



























!***********************************************************************
! asteem: adaptive step time-split explicit euler method
!
! author: rahul a. zaveri
! update: jan 2005
! reference: zaveri r.a., r.c. easter, and l.k. peters, jgr (2005c)
!-----------------------------------------------------------------------
      subroutine asteem( dtchem, istat_asteem )
!     implicit none
!     include 'v33com'
!     include 'mosaic.h'
! subr arguments
      integer istat_asteem
      real dtchem
! local variables
      integer ibin, iv, itsi, ntsi, jcall, k, m, isteps
      integer iclm_debug, jclm_debug, kclm_debug, ncnt_debug
      real tsi, dumdum, p_mesa_fails

      data iclm_debug /28/
      data jclm_debug /13/
      data kclm_debug /11/
      data ncnt_debug /0/





      istat_asteem = 0

      if(iclm_aer .eq. iclm_debug .and.   &
         jclm_aer .eq. jclm_debug .and.   &
         kclm_aer .eq. kclm_debug  .and.   &
         ncorecnt_aer .eq. ncnt_debug)then
        dumdum = 0.0
      endif





! calculate water content and wet size for computing mass transfer coefficients
      do ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call aerosol_phase_state(ibin)
        endif
      enddo

      call aerosolmtc
      call asteem_calculate_tsi(dtchem, tsi, ntsi) ! do this here. kg(nh3_g) may go to zero after consdense_non_volatiles


! now condense non-volatiles only for dtchem [s]
      call asteem_condense_non_volatiles(dtchem)


! recompute phase equilibrium
      do ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call aerosol_phase_state(ibin)
        endif
      enddo



      if(ntsi .lt. 0)return		! mass transfer rates = 0



! integrate each bin separately over tsi (time-split interval)
! using alternate direction implicit (adi) technique
      do 10 itsi = 1, ntsi/2

! first in forward order
      do ibin = 1, nbin_a

        if(jaerosolstate(ibin) .ne. no_aerosol)then

          call asteem_condense_semi_volatiles( ibin, tsi, istat_asteem )
          if (istat_asteem .lt. 0) goto 80

        endif

      enddo


! then in reverse order
      do ibin = nbin_a, 1, -1

        if(jaerosolstate(ibin) .ne. no_aerosol)then

          call asteem_condense_semi_volatiles( ibin, tsi, istat_asteem )
          if (istat_asteem .lt. 0) goto 80

        endif

      enddo


10    continue


! update asteem call counter
80    continue
       jasteem_call = jasteem_call + 1





      return
      end subroutine asteem





      subroutine print_mosaic_stats( iflag1 )
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer iflag1
! local variables
      integer ibin
      real p_mesa_fails


       if (iflag1 .le. 0) goto 2000

         p_mesa_fails  = 100.*float(jmesa_fail)/   &
                              max(float(jmesa_call_tot),1.0)
         iter_mesa_avg = iter_mesa/max(float(jmesa_call),1.0)

         do ibin = 1, nbin_a
           steps_asteem_avg(ibin) = steps_asteem(ibin)/   &
                                    float(jasteem_call)
         enddo

         write(6,*)'------------------------------------------------'
         write(6,*)'     asteem performance statistics'
         write(6,*)'number asteem calls  =', jasteem_call
         write(6,*)'total asteem fails   =', jasteem_fail
         write(6,40)(steps_asteem_avg(ibin), ibin=1,nbin_a)
         write(6,41)(steps_asteem_max(ibin), ibin=1,nbin_a)
         write(6,*)'     mesa performance statistics'
         write(6,*)'number of mesa calls =', jmesa_call
         write(6,*)'avg mesa iterations  =', iter_mesa_avg
         write(6,*)'total mesa fails     =', jmesa_fail
         write(6,*)'percent mesa fails   =', p_mesa_fails
         write(6,*)'  '

2000     continue
         jasteem_call = 0				! reset
         jmesa_call   = 0				! reset
         iter_mesa    = 0.0				! reset
         do ibin = 1, nbin_a
           steps_asteem(ibin)     = 0.0			! reset
           steps_asteem_max(ibin) = 0.0			! reset
         enddo


40    format(' avg asteem steps/bin =', 8(f8.1,x))
41    format(' max asteem steps/bin =', 8(f8.1,x))

      return
      end subroutine print_mosaic_stats





!-----------------------------------------------------------------------
	subroutine print_mosaic_stats_bb( iflag1 )

!	include 'mosaic.h'

!   subr arguments
	integer iflag1

!   local variables
	integer ibin, ndum


	if (iflag1 .gt. 0) goto 2000

!   iflag1 .le. 0 -- start of current gridpt calcs initialization
1000	continue
	do ibin = 1, nbin_a
	    masbalout_err_count(ibin) = 0
	    masbalout_maxreldiff_so4(ibin) = 0
	    masbalout_maxreldiff_nh4(ibin) = 0
	    masbalout_maxreldiff_no3(ibin) = 0
	    masbalout_maxreldiff_cl(ibin) = 0
	    masbalout_maxreldiff_na(ibin) = 0
	    masbalout_maxreldiff_ca(ibin) = 0
	end do

	return


!   iflag1 .gt. 0 -- end of current gridpt calcs
2000	continue

!   output of masbalout_err_count
	ndum = 0
	do ibin = 1, nbin_a
	    if (masbalout_err_count(ibin) .gt. 1) ndum = ndum + 1
	end do
	if (ndum .gt. 0) then
	    write(6,9310)   &
      		'mosaic aerchem monitor_masbal_out - ijk',   &
      		iclm_aer, jclm_aer, kclm_aer
	    write(6,9300)   &
      		'err_counts / maxreldiff_so4 / nh4 / no3 / cl / na / ca'
	    write(6,9320) (masbalout_err_count(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_so4(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_nh4(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_no3(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_cl(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_na(ibin), ibin=1,nbin_a)
	    write(6,9330) (masbalout_maxreldiff_ca(ibin), ibin=1,nbin_a)
	end if
9300	format( 10a )
9310	format( a, 3i5 )
9320	format( 8( 1x, i8 ) )
9330	format( 8( 1pe9.1 ) )

	return

	end subroutine print_mosaic_stats_bb







!***********************************************************************
! part of asteem: computes time splitting interval (tsi)
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_calculate_tsi(dtchem, tsi, ntsi)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real tsi, dtchem
! local variables
      integer ibin, ntsi
      real gnh3_ghcl, gnh3_ghno3, sumkg, tau_gas_tot, tau_g_min
      real tau_gas_bin(nbin_a)




      tau_g_min = 1.e20

      sumkg = 0.0
      do 50 ibin = 1, nbin_a

        if(jaerosolstate(ibin) .ne. no_aerosol)then
          sumkg = sumkg + kg(inh3_g,ibin)
          tau_gas_bin(ibin) = 1./kg(inh3_g,ibin)
          tau_g_min = min(tau_g_min, tau_gas_bin(ibin))
        endif

50    continue


      if(sumkg .gt. 0.)then
        tau_gas_tot = 1./sumkg
      else
        tsi = -99.0
        ntsi = -99
        return
      endif



      tsi = 0.1*tau_gas_tot



      if(tsi .lt. dtchem)then

        ntsi  = int(dtchem/tsi) + 1
        ntsi  = (ntsi/2)*2		! even number
        tsi = dtchem/float(ntsi)

      else

        tsi = dtchem/2.0
        ntsi  = 2

      endif


      return
      end subroutine asteem_calculate_tsi













!***********************************************************************
! part of asteem: condenses non-volatiles
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_condense_non_volatiles(dtchem)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real dtchem
! local variables
      integer iaer, ibin, icallpt, je
      real xt





      if(gas(inh3_g) .eq. 0.0)then
        call asteem_condense_only_h2so4(dtchem)
      else
        call asteem_condense_nh3_h2so4(dtchem)
      endif


      do ibin = 1, nbin_a

        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call conform_electrolytes(jtotal,ibin,xt)
        endif

      enddo


      return
      end subroutine asteem_condense_non_volatiles







!***********************************************************************
! part of asteem: condenses h2so4 only analytically
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_condense_only_h2so4(dtchem)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real dtchem
! local variables
      integer ibin
      real sumkg, decay, delta_h2so4


      sumkg = 0.0
      do 50 ibin = 1, nbin_a
        sumkg = sumkg + kg(ih2so4_g,ibin)
50    continue


      if(sumkg*dtchem .lt. 1.e-9)return


! integrate gas-phase analytically
      decay = exp(-sumkg*dtchem)
      delta_h2so4 = gas(ih2so4_g)*(1.0 - decay)
      gas(ih2so4_g) = gas(ih2so4_g)*decay

! distribute the difference by mtc weights
      do 60 ibin = 1, nbin_a
        aer(iso4_a,jtotal,ibin) = aer(iso4_a,jtotal,ibin) +   &
                         delta_h2so4*kg(ih2so4_g,ibin)/sumkg

60    continue

      return
      end subroutine asteem_condense_only_h2so4






!***********************************************************************
! part of asteem: condenses nh3 and h2so4 using explicit euler method
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_condense_nh3_h2so4(dtchem)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      real dtchem
! local variables
      integer ibin
      real delta_nh4, delta_so4, dtmax,   &
        t_new, t_old, t_out, xt
      real sumkg(naer_vol)





      t_old = 0.0
      t_out = dtchem




! calculate maximum possible internal time-step
      sumkg(inh3_g) = 0.0
      do ibin = 1, nbin_a
        sumkg(inh3_g) = sumkg(inh3_g) + kg(inh3_g,ibin)
      enddo


! compute max possible nh4 condensation for each bin
      if(sumkg(inh3_g) .gt. 0.0)then
         do ibin = 1, nbin_a
           aer_nh4_max(ibin) = aer(inh4_a,jtotal,ibin) +   &
             gas(inh3_g)*(1.0 - exp(-sumkg(inh3_g)*dtchem))*   &
             kg(inh3_g,ibin)/sumkg(inh3_g)
         enddo
      endif


      if(sumkg(inh3_g) .gt. 0.0)then
        dtmax = alpha_asteem/sumkg(inh3_g)
      else
        dtmax = dtchem		! mass transfer rates = 0
        return
      endif


! begin integration over transport time-step
!======================================================

! calculate fluxes
10    do 501 ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call asteem_flux_nh3_h2so4(ibin)
        endif
501   continue




      t_new = t_old + dtmax	! update time
      if(t_new .gt. t_out)then	! check if the new time step is too large
        dtmax = t_out - t_old
        t_new = t_out
      endif



! integrate

      do ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then

          delta_so4 = dtmax*flux(ih2so4_g,ibin)
          aer(iso4_a,jtotal,ibin) = aer(iso4_a,jtotal,ibin) + delta_so4
          gas(ih2so4_g) = gas(ih2so4_g) - delta_so4

          delta_nh4 = dtmax*flux(inh3_g,ibin)
          aer(inh4_a,jtotal,ibin) = aer(inh4_a,jtotal,ibin) + delta_nh4
          gas(inh3_g)   = gas(inh3_g) - delta_nh4

        endif
      enddo


      do ibin = 1, nbin_a
        if(jaerosolstate(ibin) .ne. no_aerosol)then
          call conform_electrolytes(jtotal,ibin,xt)
        endif
      enddo


! update time
      t_old = t_new

      if(t_new .lt. 0.9999*t_out) goto 10
!================================================
! end of integration over the transport time-step



      return
      end subroutine asteem_condense_nh3_h2so4












!***********************************************************************
! part of asteem: computes fluxes of h2so4 and nh3 over dry or wet aerosols
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_flux_nh3_h2so4(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin



      if(epercent(jnacl,jtotal,ibin)  .gt. smallp .or.   &
         epercent(jcacl2,jtotal,ibin) .gt. smallp .or.   &
         epercent(jnano3,jtotal,ibin) .gt. smallp .or.   &
         epercent(jcano3,jtotal,ibin) .gt. smallp .or.   &
         epercent(jcaco3,jtotal,ibin) .gt. 0.0)then

        df_gas(ih2so4_g,ibin) = gas(ih2so4_g)
        df_gas(inh3_g,ibin)   = 0.0

        flux(ih2so4_g,ibin)   = kg(ih2so4_g,ibin)*df_gas(ih2so4_g,ibin)
        flux(inh3_g,ibin)     = 0.0


      else	! sulfate rich or sulfate poor ammonium sulfate aerosol


        df_gas(ih2so4_g,ibin) = gas(ih2so4_g)
        df_gas(inh3_g,ibin)   = gas(inh3_g)

	flux(ih2so4_g,ibin)   = kg(ih2so4_g,ibin)*df_gas(ih2so4_g,ibin)
        flux(inh3_g,ibin)     = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)

        flux(inh3_g,ibin) = min(2.*flux(ih2so4_g,ibin),   &
                                   flux(inh3_g,ibin))

      endif


      return
      end subroutine asteem_flux_nh3_h2so4













!***********************************************************************
! part of asteem: condenses semi-volatiles over tsi time interval
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_condense_semi_volatiles(   &
          ibin, tsi, istat_asteem )
!     implicit none
!     include 'v33com'
!     include 'mosaic.h'
! subr arguments
      integer ibin, istat_asteem
      real tsi
! local variables
      integer isteps, iv, jp, nsteps_max, kdum, mdum
      parameter(nsteps_max = 400)
      real delta_aer, dtmax, dum, t_new, t_old, t_out, xt
      real dumdum

      integer iclm_debug, jclm_debug, kclm_debug, ncnt_debug, ibin_debug
      data iclm_debug /4/
      data jclm_debug /1/
      data kclm_debug /15/
      data ncnt_debug /270/
      data ibin_debug /2/


      istat_asteem = 0
      kdum = kclm_aer
      mdum = mclm_aer


      t_old = 0.0
      t_out = tsi

      if(iclm_aer .eq. iclm_debug .and.   &
         jclm_aer .eq. jclm_debug .and.   &
         kclm_aer .eq. kclm_debug  .and.   &
         ncorecnt_aer .eq. ncnt_debug  .and.   &
         ibin     .eq. ibin_debug)then
        dumdum = 0.0
      endif


      call monitor_massbalance_in(ibin)


      isteps = 0

! integrate ibin over tsi time interval
!
! calculate fluxes
10    isteps = isteps + 1
      mxfer_massbal(ibin) = mno


      if(jaerosolstate(ibin) .eq. all_solid)then
        jphase(ibin) = jsolid
        call asteem_flux_dry(ibin)
      elseif(jaerosolstate(ibin) .eq. all_liquid .or.   &
             jaerosolstate(ibin) .eq. mixed)then
        jphase(ibin) = jliquid
        call asteem_flux_wet(ibin)
        call degas_acids(jliquid,ibin,xt)	! degases equilibrated acids (if present) and also updates jtotal
      endif





! check if the bin has reached equilibrium
      ieqblm_bin(ibin) = myes		! initalize to default (eqblm)
      do iv = 1, naer_vol
        if(flux(iv, ibin) .ne. 0.0)then
          ieqblm_bin(ibin) = mno		! non-eqblm
        endif
      enddo

      if(ieqblm_bin(ibin) .eq. myes)then

! update jtotal
        do iv = 1, naer_vol
          aer(iv,jtotal,ibin)=aer(iv,jsolid,ibin)+aer(iv,jliquid,ibin)
        enddo

        if(mxfer_massbal(ibin) .eq. myes)then
          call conform_electrolytes(jtotal,ibin,xt) ! xfer_massbal + mdrh diagnosis
        else
          call form_electrolytes(jtotal,ibin,xt)    ! for mdrh diagnosis
        endif


        if(jhyst_leg(ibin) .eq. jhyst_lo)then
          call asteem_update_phase_eqblm(ibin)
        else
          call do_full_deliquescence(ibin)	! simply do liquid <-- total
        endif

        steps_asteem(ibin) = steps_asteem(ibin) + float(isteps)
        steps_asteem_max(ibin) = max(steps_asteem_max(ibin),   &
                                   float(isteps))

      return	! the bin has reached equilibrium
      endif





! calculate maximum possible internal time-step
      call asteem_calculate_dtmax(ibin, tsi, dtmax)
      t_new = t_old + dtmax	! update time
      if(t_new .gt. t_out)then	! check if the new time step is too large
        dtmax = t_out - t_old
        t_new = t_out
      endif









! integrate
      jp = jphase(ibin)

      do 20 iv = 1, naer_vol

        delta_aer = dtmax*flux(iv,ibin)

        aer(iv,jp,ibin)=real(dble(aer(iv,jp,ibin)) + dble(delta_aer))
        gas(iv)        =real(dble(gas(iv)) - dble(delta_aer))

        aer(iv,jp,ibin)=max(aer(iv,jp,ibin), 0.0)
        gas(iv)        =max(gas(iv), 0.0)

20    continue


! degas excess nh3 (if present)
      call form_electrolytes(jp,ibin,xt)

! update jtotal
      do iv = 1, naer_vol
        aer(iv,jtotal,ibin)=aer(iv,jsolid,ibin)+aer(iv,jliquid,ibin)
      enddo


      if(mxfer_massbal(ibin) .eq. myes)then
        call conform_electrolytes(jtotal,ibin,xt) ! xfer_massbal + mdrh diagnosis
      else
        call form_electrolytes(jtotal,ibin,xt)	  ! for mdrh diagnosis
      endif


      if(jhyst_leg(ibin) .eq. jhyst_lo)then
        call asteem_update_phase_eqblm(ibin)
      else
        call do_full_deliquescence(ibin)	! simply do liquid <-- total
      endif


      call monitor_massbalance_out(ibin, isteps)


! update time
      t_old = t_new

      if(isteps .ge. nsteps_max)then
        istat_asteem = -10
        jasteem_fail = jasteem_fail + 1
        write(6,*) 'mosaic aerchem asteem nsteps_max exceeded',   &
            nsteps_max
        if(iprint_input .eq. myes)then
          call print_input(kdum,mdum)
          iprint_input = mno
        endif
        goto 30
      elseif(t_new .lt. 0.9999*t_out)then
        goto 10
      endif
!================================================
! end of integration over the time split interval


30    steps_asteem(ibin) = steps_asteem(ibin) + float(isteps)
      steps_asteem_max(ibin) = max(steps_asteem_max(ibin),   &
                                   float(isteps))


      return
      end subroutine asteem_condense_semi_volatiles








      subroutine monitor_massbalance_in(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv, iaer

      total_so4_in = gas(ih2so4_g)+ aer(iso4_a,jtotal,ibin)
      total_no3_in = gas(ihno3_g) + aer(ino3_a,jtotal,ibin)
      total_cl_in  = gas(ihcl_g)  + aer(icl_a,jtotal,ibin)
      total_nh4_in = gas(inh3_g)  + aer(inh4_a,jtotal,ibin)
      total_na_in  = aer(ina_a,jtotal,ibin)
      total_ca_in  = aer(ica_a,jtotal,ibin)


      do iv = 1, naer_vol
        gassav(iv) = gas(iv)
      enddo

      do iaer = 1, naer
        aersav(iaer) = aer(iaer,jtotal,ibin)
      enddo


      return
      end subroutine monitor_massbalance_in




      subroutine monitor_massbalance_out(ibin, isteps)
!     implicit none
!     include 'v33com'
!     include 'v33com3'
!     include 'v33com9a'
!     include 'v33com9b'
!     include 'mosaic.h'
! subr arguments
      integer ibin, isteps
! local variables
      integer kdum, mdum

      integer iclm_debug, jclm_debug, kclm_debug, ncnt_debug, ibin_debug
      data iclm_debug /4/
      data jclm_debug /1/
      data kclm_debug /15/
      data ncnt_debug /270/
      data ibin_debug /2/

      kdum = kclm_aer
      mdum = mclm_aer



      total_so4 = gas(ih2so4_g)+ aer(iso4_a,jtotal,ibin)
      total_no3 = gas(ihno3_g) + aer(ino3_a,jtotal,ibin)
      total_cl  = gas(ihcl_g)  + aer(icl_a,jtotal,ibin)
      total_nh4 = gas(inh3_g)  + aer(inh4_a,jtotal,ibin)
      total_na  = aer(ina_a,jtotal,ibin)
      total_ca  = aer(ica_a,jtotal,ibin)


      if(total_so4_in .gt. 1.e-25 .or. total_so4 .gt. 1.e-25)then
        rel_diff_so4 = (total_so4 - total_so4_in)/   &
                    max(total_so4,  total_so4_in)
      else
        rel_diff_so4 = 0.0
      endif

      if(total_no3_in .gt. 1.e-25 .or. total_no3 .gt. 1.e-25)then
        rel_diff_no3 = (total_no3 - total_no3_in)/   &
                    max(total_no3,  total_no3_in)
      else
        rel_diff_no3 = 0.0
      endif

      if(total_cl_in .gt. 1.e-25 .or. total_cl .gt. 1.e-25)then
        rel_diff_cl = (total_cl  - total_cl_in)/   &
                   max(total_cl,   total_cl_in)
      else
        rel_diff_cl = 0.0
      endif

      if(total_nh4_in .gt. 1.e-25 .or. total_nh4 .gt. 1.e-25)then
        rel_diff_nh4 = (total_nh4 - total_nh4_in)/   &
                    max(total_nh4,  total_nh4_in)
      else
        rel_diff_nh4 = 0.0
      endif

      if(total_na_in .gt. 1.e-25 .or. total_na .gt. 1.e-25)then
        rel_diff_na = (total_na  - total_na_in)/   &
                   max(total_na,   total_na_in)
      else
        rel_diff_na = 0.0
      endif


      if(total_ca_in .gt. 1.e-25 .or. total_ca .gt. 1.e-25)then
        rel_diff_ca = (total_ca  - total_ca_in)/   &
                   max(total_ca,   total_ca_in)
      else
        rel_diff_ca = 0.0
      endif


      if(abs(rel_diff_so4).gt.1.e-4 .or.   &
         abs(rel_diff_no3).gt.1.e-4 .or.   &
         abs(rel_diff_nh4).gt.1.e-4 .or.   &
         abs(rel_diff_na) .gt.1.e-4 .or.   &
         abs(rel_diff_ca) .gt.1.e-4)then

        masbalout_err_count(ibin) = masbalout_err_count(ibin) + 1
        masbalout_maxreldiff_so4(ibin) = max(   &
            masbalout_maxreldiff_so4(ibin), abs(rel_diff_so4) )
        masbalout_maxreldiff_nh4(ibin) = max(   &
            masbalout_maxreldiff_nh4(ibin), abs(rel_diff_nh4) )
        masbalout_maxreldiff_no3(ibin) = max(   &
            masbalout_maxreldiff_no3(ibin), abs(rel_diff_no3) )
        masbalout_maxreldiff_cl(ibin) = max(   &
            masbalout_maxreldiff_cl(ibin),  abs(rel_diff_cl) )
        masbalout_maxreldiff_na(ibin) = max(   &
            masbalout_maxreldiff_na(ibin),  abs(rel_diff_na) )
        masbalout_maxreldiff_ca(ibin) = max(   &
            masbalout_maxreldiff_ca(ibin),  abs(rel_diff_ca) )

        if(iprint_input .eq. myes)then
          write(6,*) 'mosaic aerchem monitor_massbalance_out failure'
          call print_input(kdum, mdum)
          iprint_input = mno
        endif

      endif


      return
      end subroutine monitor_massbalance_out











!***********************************************************************
! part of asteem: updates solid-liquid partitioning after each gas-aerosol
! mass transfer step
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_update_phase_eqblm(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jdum, js, j_index, mdissolved
      real xt
! function
!     real drh_mutual, dum



! calculate overall sulfate ratio
      call calculate_xt(ibin,jtotal,xt)		! calc updated xt

! now diagnose mdrh
      if(xt .lt. 1. .and. xt .gt. 0. )goto 10	! excess sulfate domain - no mdrh exists

      jdum = 0
      do js = 1, nsalt
        jsalt_present(js) = 0			! default value - salt absent

        if(epercent(js,jtotal,ibin) .gt. 1.0)then
          jsalt_present(js) = 1			! salt present
          jdum = jdum + jsalt_index(js)
        endif
      enddo

      if(jdum .eq. 0)then
        jaerosolstate(ibin) = all_solid ! no significant soluble material present
        jphase(ibin) = jsolid
        call adjust_solid_aerosol(ibin)
        return
      endif

      if(xt .ge. 2.0 .or. xt .lt. 0.0)then
        j_index = jsulf_poor(jdum)
      else
        j_index = jsulf_rich(jdum)
      endif

      mdrh(ibin) = drh_mutual(j_index) + 1.0

      if(ah2o*100. .lt. mdrh(ibin)) then
        jaerosolstate(ibin) = all_solid
        jphase(ibin) = jsolid
        call adjust_solid_aerosol(ibin)
        return
      endif


! none of the above means it must be sub-saturated or mixed-phase
10    if(mxfer_massbal(ibin).eq.myes .or. jphase(ibin).eq.jsolid)then
        call do_full_deliquescence(ibin)
        call mesa(ibin)
      else
        call mesa(ibin)
      endif



      return
      end subroutine asteem_update_phase_eqblm
























!***********************************************************************
! part of asteem: computes max time step for gas-aerosol integration
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_calculate_dtmax(ibin, tsi, dtmax)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
      real tsi, dtmax
! local variables
      integer iv
      real alpha, h_aer, h_gas, h_max,   &
           h_gas_i(naer_vol), h_nh3_max,   &
           h_aer_i_m(naer_vol, nbin_a),   &
           h_aer_m(nbin_a)




! set alpha_gas and alpha_aer
      do iv = 1, naer_vol

          alpha_gas(iv) = alpha_asteem

          if(madapt_alpha .eq. mon)then
            alpha_aer(iv,ibin) = max(abs(phi_volatile(iv,ibin)),   &
                                     alpha_asteem)
            alpha_aer(iv,ibin) = min(alpha_aer(iv,ibin), 10.0)

          else
            alpha_aer(iv,ibin) = alpha_asteem	! fixed alpha_aer

          endif

      enddo





! gas-side
! calculate h_gas_i and h_gas

      h_gas = 2.e16

      do iv = 2, naer_vol

        h_gas_i(iv) = 1.e16
        if(flux(iv,ibin) .gt. 0.0)then

          h_gas_i(iv) = alpha_gas(iv)/kg(iv,ibin)
          h_gas       = min(h_gas, h_gas_i(iv))

        endif

      enddo

! restrict gas-side time step so that max allowable nh3 concentration is not exceeded
      if(flux(inh3_g,ibin) .gt. 0.0)then
        h_nh3_max = (aer_nh4_max(ibin)-aer(inh4_a,jtotal,ibin))/   &
                                flux(inh3_g,ibin)
        h_gas     = min(h_gas, h_nh3_max)
      endif


!
! aerosol-side
! calculate h_aer_i_m, h_aer_m, h_aer
      h_aer = 1.e16
      h_aer_m(ibin) = 4.e15		! initialize

      call make_volatile_a(ibin)


! solid aerosol
      if(jphase(ibin) .eq. jsolid)then		! solid aerosol

        do 10 iv = 2, naer_vol
          h_aer_i_m(iv,ibin) = 2.e15		! initialize

          if(flux(iv,ibin).lt.0.)then		! aer -> gas
            alpha = min(alpha_aer(iv,ibin),0.5)
            h_aer_i_m(iv,ibin) = -alpha*volatile_a(iv)/flux(iv,ibin)
          endif

          h_aer_m(ibin) = min(h_aer_m(ibin),h_aer_i_m(iv,ibin))
          h_aer         = min(h_aer,        h_aer_i_m(iv,ibin))
10      continue

        goto 100

      endif






! liquid or mixed-phase aerosol
      if(jphase(ibin) .eq. jliquid)then

        do 11 iv = 2, naer_vol
          h_aer_i_m(iv,ibin) = 2.e15		! initialize

          if(flux(iv,ibin).gt.0. .and. 					   &  ! gas -> aer
             electrolyte(jcaco3,jtotal,ibin) .eq. 0.)then

            if(aer(iv,jliquid,ibin) .gt. 0.0)then
            h_aer_i_m(iv,ibin)=alpha_aer(iv,ibin)*aer(iv,jliquid,ibin)/	   &  ! aer(i,jliquid) =< aer(i,jtotal)
                                          flux(iv,ibin)
            endif

          elseif(flux(iv,ibin).lt.0. .and. volatile_a(iv).gt.0.0)then	! aer -> gas
            alpha = min(alpha_aer(iv,ibin), 0.5)
            h_aer_i_m(iv,ibin)=-alpha*volatile_a(iv)/flux(iv,ibin)

          endif


          h_aer_m(ibin) = min(h_aer_m(ibin),h_aer_i_m(iv,ibin))
          h_aer         = min(h_aer,        h_aer_i_m(iv,ibin))
11      continue

      endif




100   h_max = min(h_aer, h_gas)

      dtmax = min(tsi, h_max)
      h_max = dtmax


      if(dtmax .le. 1.0e-5)then
        write(6,*)' dtmax = ', dtmax
      endif



      if(dtmax .eq. 0.0)then

        write(6,*)'iclm jclm kclm = ',iclm_aer, jclm_aer, kclm_aer
        write(6,*)'ibin= ',ibin,'  jaerosolstate= ',jaerosolstate(ibin)
        write(6,*)'aerso4 = ', aer(iso4_a,jphase(ibin),ibin)
        write(6,*)'aerno3 = ', aer(ino3_a,jphase(ibin),ibin)
        write(6,*)'aercl  = ', aer(icl_a,jphase(ibin),ibin)
        write(6,*)'aernh3 = ', aer(inh4_a,jphase(ibin),ibin)
        write(6,*)'  '
        write(6,*)'h_gas = ', h_gas, '   h_aer =', h_aer
        write(6,*)'iv        gas        volatile          flux'
        write(6,*)'hno3 ',gas(ihno3_g),volatile_a(ihno3_g),   &
                   flux(ihno3_g,ibin)
        write(6,*)'hcl  ',gas(ihcl_g), volatile_a(ihcl_g),   &
                   flux(ihcl_g,ibin)
        write(6,*)'nh3  ',gas(inh3_g), volatile_a(inh3_g),   &
                   flux(inh3_g,ibin)


        dtmax = tsi
        do iv = 1, naer_vol
          flux(iv,ibin) = 0.0
        enddo
      endif


      return
      end subroutine asteem_calculate_dtmax
























!***********************************************************************
! part of asteem and asceem: computes volatile species concentration
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine make_volatile_a(ibin)
!     implicit none
!     include 'v33com'
!     include 'mosaic.h'
! subr arguments
      integer ibin



      if(jaerosolstate(ibin) .eq. all_solid)then

        volatile_a(iso4_a) = 0.0
        volatile_a(ino3_a) = aer(ino3_a,jsolid,ibin)
        volatile_a(icl_a)  = aer(icl_a,jsolid,ibin)
        volatile_a(inh4_a) = electrolyte(jnh4cl,jsolid,ibin) +   &
                             electrolyte(jnh4no3,jsolid,ibin)

      elseif(jaerosolstate(ibin) .eq. all_liquid)then

        volatile_a(iso4_a) = 0.0
        volatile_a(ino3_a) = aer(ino3_a,jliquid,ibin)-   &
                             electrolyte(jhno3,jliquid,ibin)
        volatile_a(icl_a)  = aer(icl_a,jliquid,ibin) -   &
                             electrolyte(jhcl,jliquid,ibin)
        volatile_a(inh4_a) = aer(inh4_a,jliquid,ibin)

      elseif(jaerosolstate(ibin) .eq. mixed)then

        volatile_a(iso4_a) = 0.0
        volatile_a(ino3_a) = aer(ino3_a,jliquid,ibin)-   &
                             electrolyte(jhno3,jliquid,ibin)
        volatile_a(icl_a)  = aer(icl_a,jliquid,ibin) -   &
                             electrolyte(jhcl,jliquid,ibin)
        volatile_a(inh4_a) = aer(inh4_a,jliquid,ibin)	! different

      else

        write(6,*)'bad jaerosolstate in subr. make_volatile_a'
        write(6,*)'ibin =',ibin,'jaerosolstate =',jaerosolstate(ibin)
        write(6,*)'i j k = ', iclm_aer, jclm_aer, kclm_aer
        write(6,*)'ncorecnt = ', ncorecnt_aer
        write(6,*)'stopping in subroutine make_volatile_a'
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping in subroutine make_volatile_a' )

      endif


      return
      end subroutine make_volatile_a
















!***********************************************************************
! part of asteem: computes gas-aerosol fluxes over dry aerosols
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_flux_dry(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real xt, prod_nh4no3, prod_nh4cl, volatile_cl




      call calculate_xt(ibin,jsolid,xt)

      flux(ih2so4_g,ibin)  = 0.0

!-----------------------------------------------------------------
! case 1: sulfate-rich domain

      if(xt.lt.2.0 .and. xt.ge.0.)then	! excess sulfate (acidic)

!	call asteem_flux_dry_case1(ibin)

        flux(ihno3_g,ibin)    = 0.0
        flux(ihcl_g,ibin)     = 0.0
        flux(inh3_g,ibin)     = 0.0

        return
      endif

!-----------------------------------------------------------------
! case 2:  caco3 > 0 absorb all acids (and indirectly degas co2)

      if(electrolyte(jcaco3,jsolid,ibin) .gt. 0.0)then

        call asteem_flux_dry_case2(ibin)

        return
      endif

!-------------------------------------------------------------------
! case 3: hno3 and hcl exchange may happen here

      volatile_cl  = electrolyte(jnacl,jsolid,ibin) +   &
                     electrolyte(jcacl2,jsolid,ibin)


      if(volatile_cl .gt. 0.0 .and.   &
         gas(ihno3_g).gt. 0.0 )then

        call asteem_flux_dry_case3(ibin)

        return
      endif

!-----------------------------------------------------------------
! case 4: nh4no3 or nh4cl or both may be active

      prod_nh4no3 = max( (gas(inh3_g)*gas(ihno3_g)-keq_sg(1)), 0.0) +   &
                    epercent(jnh4no3,jsolid,ibin)
      prod_nh4cl  = max( (gas(inh3_g)*gas(ihcl_g) -keq_sg(2)), 0.0) +   &
                    epercent(jnh4cl, jsolid,ibin)

      if(prod_nh4no3 .gt. 0.0 .or. prod_nh4cl .gt. 0.0)then
        call asteem_flux_dry_case4(ibin)
        return
      endif

!-----------------------------------------------------------------
! case 5: default

        call asteem_flux_dry_case5(ibin)
        return


      end subroutine asteem_flux_dry

!----------------------------------------------------------------------








!***********************************************************************
! part of asteem: subroutines for flux_dry cases
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
!
!
!
! case 1: sulfate-rich domain
!
      subroutine asteem_flux_dry_case1(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin



      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = gas(ihno3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)
      sfc_a(inh3_g)  = 0

      df_gas(ih2so4_g,ibin) = 0.0
      df_gas(ihno3_g,ibin)  = 0.0
      df_gas(ihcl_g,ibin)   = 0.0
      df_gas(inh3_g,ibin)   = gas(inh3_g)

      phi_volatile(ihno3_g,ibin) = 0.0
      phi_volatile(ihcl_g,ibin)  = 0.0
      phi_volatile(inh3_g,ibin)  = 1.0

      flux(ihno3_g,ibin)    = 0.0
      flux(ihcl_g,ibin)     = 0.0
      flux(inh3_g,ibin)     = kg(inh3_g,ibin)*gas(inh3_g)


      return
      end subroutine asteem_flux_dry_case1




! case 2:  caco3 > 0 absorb all acids (and indirectly degas co2)
!
      subroutine asteem_flux_dry_case2(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


      mxfer_massbal(ibin) = myes

      sfc_a(ih2so4_g) = 0.0
      sfc_a(ihno3_g)  = 0.0
      sfc_a(ihcl_g)   = 0.0
      sfc_a(inh3_g)   = gas(inh3_g)

      df_gas(ih2so4_g,ibin) = 0.0
      df_gas(ihno3_g,ibin)  = gas(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)
      df_gas(inh3_g,ibin)   = 0.0

      phi_volatile(ihno3_g,ibin) = 1.0
      phi_volatile(ihcl_g,ibin)  = 1.0
      phi_volatile(inh3_g,ibin)  = 0.0

      flux(ih2so4_g,ibin)   = 0.0
      flux(ihno3_g,ibin)    = kg(ihno3_g,ibin)*gas(ihno3_g)
      flux(ihcl_g,ibin)     = kg(ihcl_g,ibin)*gas(ihcl_g)
      flux(inh3_g,ibin)     = 0.0


      return
      end subroutine asteem_flux_dry_case2











! case 3: hno3 and hcl exchange may happen here
!
      subroutine asteem_flux_dry_case3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin


! just degas hcl from nacl or cacl2 by flux balance with hno3
      mxfer_massbal(ibin) = myes

      flux(ih2so4_g,ibin)= 0.0
      flux(ihno3_g,ibin) = kg(ihno3_g,ibin)*gas(ihno3_g)
      flux(ihcl_g,ibin)  = 0.0 ! degas in conformaerosol or formelectrolytes
      flux(inh3_g,ibin)  = 0.0



      return
      end subroutine asteem_flux_dry_case3










! case 4: nh4no3 and/or nh4cl may be active
      subroutine asteem_flux_dry_case4(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv, iactive_nh4no3, iactive_nh4cl, iactive
      real gnh3_hno3, gnh3_hcl, pcnt_nh4no3, pcnt_nh4cl,   &
           beta_nh4,   &
           a, b, c, sfc_nh3_1, sfc_nh3_2, phi_nh3_1, phi_nh3_2,   &
           flux_nh3_est, flux_nh3_max, ratio_flux
! function
!     real quadratic


!-------------------
! set default values for flags
      iactive_nh4no3 = 1
      iactive_nh4cl  = 2

!-------------------
! compute diagnostic products and ratios
      gnh3_hno3   = gas(inh3_g)*gas(ihno3_g)
      gnh3_hcl    = gas(inh3_g)*gas(ihcl_g)

      beta_nh4    = aer(inh4_a,jtotal,ibin)/aer_nh4_max(ibin)

      phi_nh4no3(ibin) = (gnh3_hno3 - keq_sg(1))/   &
                               keq_sg(1)
      phi_nh4cl(ibin)  = (gnh3_hcl - keq_sg(2))/   &
                               keq_sg(2)

      pcnt_nh4no3 = epercent(jnh4no3,jsolid,ibin)
      pcnt_nh4cl  = epercent(jnh4cl, jsolid,ibin)


!-------------------
! now determine if nh4no3 and/or nh4cl are active or significant

! nh4no3
      if( (gnh3_hno3.gt.keq_sg(1) .and. beta_nh4 .gt.0.99) .or.   &
          (abs(phi_nh4no3(ibin)) .lt. 0.02) )then
        iactive_nh4no3 = 0
      elseif(gnh3_hno3.lt.keq_sg(1) .and. pcnt_nh4no3.lt.1.0)then
        iactive_nh4no3 = 0
        call degas_solid_nh4no3(ibin)
      endif

! nh4cl
      if( (gnh3_hcl.gt.keq_sg(2) .and. beta_nh4 .gt.0.99) .or.   &
          abs(phi_nh4cl(ibin)) .lt. 0.02 )then
        iactive_nh4cl = 0
      elseif(gnh3_hcl.lt.keq_sg(2) .and. pcnt_nh4cl.lt.1.0)then
        iactive_nh4cl = 0
        call degas_solid_nh4cl(ibin)
      endif


      iactive = iactive_nh4no3 + iactive_nh4cl

! check the outcome
      if(iactive .eq. 0)then
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      endif

      goto (1,2,3),iactive

!---------------------------------
! only nh4no3 is active
1     continue

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)
      c = -(kg(ihno3_g,ibin)*keq_sg(1))

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_sg(1)/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)


      df_gas(ihno3_g,ibin) = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g)  - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)= phi_nh4no3(ibin)
      phi_volatile(ihcl_g,ibin) = 0.0
      phi_volatile(inh3_g,ibin) = phi_nh4no3(ibin)


      flux(inh3_g,ibin)    = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      flux(ihno3_g,ibin)   = flux(inh3_g,ibin)
      flux(ihcl_g,ibin)    = 0.0

      return



!-----------------
! only nh4cl is active
2     continue

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)
      c = -(kg(ihcl_g,ibin)*keq_sg(2))

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihcl_g)  = keq_sg(2) /sfc_a(inh3_g)
      sfc_a(ihno3_g) = gas(ihno3_g)


      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)= 0.0
      phi_volatile(ihcl_g,ibin) = phi_nh4cl(ibin)
      phi_volatile(inh3_g,ibin) = phi_nh4cl(ibin)


      flux(inh3_g,ibin)    = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      flux(ihcl_g,ibin)    = flux(inh3_g,ibin)
      flux(ihno3_g,ibin)   = 0.0

      return


!-----------------
! both nh4no3 and nh4cl are active
3     continue

! nh4no3
      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)
      c = -(kg(ihno3_g,ibin)*keq_sg(1))

      sfc_nh3_1    = quadratic(a,b,c)
      sfc_a(inh3_g)  = sfc_nh3_1
      sfc_a(ihno3_g) = keq_sg(1)/sfc_a(inh3_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
      df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
      flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      phi_volatile(ihno3_g,ibin) = phi_nh4no3(ibin)


! nh4cl
      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)
      c = -(kg(ihcl_g,ibin)*keq_sg(2))

      sfc_nh3_2    = quadratic(a,b,c)
      sfc_a(inh3_g)  = sfc_nh3_2
      sfc_a(ihcl_g)  = keq_sg(2)/sfc_a(inh3_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
      phi_volatile(ihcl_g,ibin) = phi_nh4cl(ibin)

! nh3
      phi_volatile(inh3_g,ibin) = max( abs(phi_nh4no3(ibin)),   &
                                       abs(phi_nh4cl(ibin)) )


! estimate nh3 flux and adjust hno3 and/or hcl if necessary

      flux_nh3_est = flux(ihno3_g,ibin) + flux(ihcl_g,ibin)
      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)


      if(flux_nh3_est .le. flux_nh3_max)then

        flux(inh3_g,ibin) = flux_nh3_est		! all ok - no adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)

      else			! reduce hno3 and hcl fluxes as necessary so that nh3 flux = flux_nh3_max

        ratio_flux   = flux_nh3_max/flux_nh3_est
        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = flux(ihno3_g,ibin)*ratio_flux
        flux(ihcl_g, ibin) = flux(ihcl_g,ibin) *ratio_flux

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

      endif




      return
      end subroutine asteem_flux_dry_case4









      subroutine asteem_flux_dry_case5(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin

      flux(ih2so4_g,ibin) = 0.0
      flux(ihno3_g,ibin)  = 0.0
      flux(ihcl_g,ibin)   = 0.0
      flux(inh3_g,ibin)   = 0.0


      return
      end subroutine asteem_flux_dry_case5
























!***********************************************************************
! part of asteem: computes fluxes over wet aerosols
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine asteem_flux_wet(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv, iadjust, iadjust_intermed, icontinue_case4
      real xt, xnh4, g_nh3_hno3, g_nh3_hcl,   &
           a_nh4_no3, a_nh4_cl, a_no3, a_cl,   &
           prod_nh4no3, prod_nh4cl



! check
!c      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
!c      call degas_acids(jliquid,ibin,xt)
      call ions_to_electrolytes(jliquid,ibin,xt)  	! for water content calculation
      call compute_activities(ibin)

      if(water_a(ibin) .eq. 0.0)then
	write(6,*)'water is zero in liquid phase'
	write(6,*)'stopping in asteem_flux_wet'
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping in asteem_flux_wet' )
      endif


! calculate xnh4
      if(aer(iso4_a,jliquid,ibin).gt.0.0)then
        xnh4 = aer(inh4_a,jliquid,ibin)/aer(iso4_a,jliquid,ibin)
      else
        xnh4 = -1.0
      endif


! h2so4
      sfc_a(ih2so4_g)        = 0.0
      df_gas(ih2so4_g,ibin)  = 0.0
      flux(ih2so4_g,ibin)    = 0.0
      phi_volatile(ih2so4_g,ibin) = 0.0


!-------------------------------------------------------------------
! case 1: sulfate-rich domain

      if(xt.lt.2.0 .and. xt.ge.0.)then
        call asteem_flux_wet_case1(ibin)
        return
      endif

!-------------------------------------------------------------------
! case 2: caco3 > 0 absorb acids (and indirectly degas co2)

      if(electrolyte(jcaco3,jsolid,ibin) .gt. 0.0)then
        call asteem_flux_wet_case2(ibin)
        return
      endif

!-------------------------------------------------------------------
! do some small adjustments before deciding case 3
!
      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
      iadjust = mno		! default
      iadjust_intermed = mno	! default

! nh4no3
      g_nh3_hno3= gas(inh3_g)*gas(ihno3_g)
      a_nh4_no3 = epercent(jnh4no3,jliquid,ibin)

      if(g_nh3_hno3 .gt. 0.0 .and. a_nh4_no3 .lt. 0.1)then
        call absorb_tiny_nh4no3(ibin)
        iadjust = myes
        iadjust_intermed = myes
      elseif(g_nh3_hno3 .eq. 0.0 .and. a_nh4_no3 .gt. 0.0)then
        call degas_tiny_nh4no3(ibin)
        iadjust = myes
        iadjust_intermed = myes
      endif

      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
        iadjust_intermed = mno	! reset
      endif

! nh4cl
      g_nh3_hcl= gas(inh3_g)*gas(ihcl_g)
      a_nh4_cl = epercent(jnh4cl,jliquid,ibin)

      if(g_nh3_hcl .gt. 0.0 .and. a_nh4_cl .lt. 0.1)then
        call absorb_tiny_nh4cl(ibin)
        iadjust = myes
        iadjust_intermed = myes
      elseif(g_nh3_hcl .eq. 0.0 .and. a_nh4_cl .gt. 0.0)then
        call degas_tiny_nh4cl(ibin)
        iadjust = myes
        iadjust_intermed = myes
      endif

      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
        iadjust_intermed = mno	! reset
      endif

! hno3
      a_no3 = aer_percent(ino3_a,jliquid,ibin)
      if(gas(ihno3_g).gt.0. .and. a_no3 .lt. 0.1 .and.   &
         aer(icl_a,jliquid,ibin) .gt. 0.0)then
        call absorb_tiny_hno3(ibin)	! and degas tiny hcl
        iadjust = myes
        iadjust_intermed = myes
      endif


! hcl
      a_cl = aer_percent(icl_a,jliquid,ibin)
      if(gas(ihcl_g).gt.0. .and. a_cl .lt. 0.1 .and.   &
         aer(ino3_a,jliquid,ibin) .gt. 0.0)then
        call absorb_tiny_hcl(ibin)	! and degas tiny hno3
        iadjust = myes
        iadjust_intermed = myes
      endif


      if(iadjust_intermed .eq. myes)then
        call ions_to_electrolytes(jliquid,ibin,xt)  	! update after adjustments
      endif

      if(iadjust .eq. myes)then
        call compute_activities(ibin)			! update after adjustments
      endif


! all adjustments done...

!--------
      prod_nh4no3 = gas(inh3_g)*gas(ihno3_g) + activity(jnh4no3,ibin)
      prod_nh4cl  = gas(inh3_g)*gas(ihcl_g)  + activity(jnh4cl,ibin)
!
! case 3: nh4no3 and/or nh4cl maybe active
      if(prod_nh4no3 .gt. 0.0 .or. prod_nh4cl .gt. 0.0)then
        call asteem_flux_wet_case3(ibin, icontinue_case4)
        if(icontinue_case4 .eq. mno)return
      endif

!-------------------------------------------------------------------
! case 4: nh3 = 0 (in gas and aerosol). hno3 and hcl exchange may happen here

      if(ma(ja_no3,ibin)*ma(ja_cl,ibin) .gt. 0.0)then
        call asteem_flux_wet_case4(ibin)
        return
      endif

!-------------------------------------------------------------------
! case 5: default (may degas nh3)

        call asteem_flux_wet_case5(ibin)
        return

      end subroutine asteem_flux_wet



















!***********************************************************************
! part of asteem: subroutines for flux_wet cases
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
!
!
!
! case 1: sulfate-rich domain
!
      subroutine asteem_flux_wet_case1(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
      real xnh4



      if(aer(inh4_a,jliquid,ibin) .eq. 0. .and. gas(inh3_g) .gt. 0.)then
        call absorb_tiny_nh3(ibin)
      endif

! calculate xnh4
      xnh4 = aer(inh4_a,jliquid,ibin)/aer(iso4_a,jliquid,ibin)

      if(xnh4 .eq. 0.0)return	! no nh3 or nh4+ present - so skip

      call equilibrate_acids(ibin) 	  	! updates aer(icl_a, ino3_a, jtotal), activity(jhcl,jhno3)

      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = gas(ihno3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)
      sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                     (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

      df_gas(ih2so4_g,ibin) = 0.0
      df_gas(ihno3_g,ibin)  = 0.0
      df_gas(ihcl_g,ibin)   = 0.0
      df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

      phi_volatile(ihno3_g,ibin) = 0.0
      phi_volatile(ihcl_g,ibin)  = 0.0
      phi_volatile(inh3_g,ibin)  =   df_gas(inh3_g,ibin)/   &
                                 max(sfc_a(inh3_g), 1.e-10)

      flux(ih2so4_g,ibin)   = 0.0
      flux(ihno3_g,ibin)    = 0.0
      flux(ihcl_g,ibin)     = 0.0

      if(df_gas(inh3_g,ibin) .gt. 0.0)then
        df_gas(inh3_g,ibin) = 0.0	! only degassing is allowed here
        flux(inh3_g,ibin)   = 0.0
        phi_volatile(inh3_g,ibin) = 0.0
        return
      endif

! check for equilibrium
      if(abs(phi_volatile(inh3_g,ibin)) .lt. 0.01)then
        flux(inh3_g,ibin)   = 0.0
      else
        flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      endif


      return
      end subroutine asteem_flux_wet_case1






! case 2: caco3 > 0 absorb all acids (and indirectly degas co2)
!
      subroutine asteem_flux_wet_case2(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer iv


      mxfer_massbal(ibin) = myes

      sfc_a(ihno3_g) = 0.0
      sfc_a(ihcl_g)  = 0.0
      sfc_a(inh3_g)  = gas(inh3_g)

      df_gas(ih2so4_g,ibin) = 0.0
      df_gas(ihno3_g,ibin)  = gas(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g)
      df_gas(inh3_g,ibin)   = 0.0

      phi_volatile(ihno3_g,ibin) = 1.0
      phi_volatile(ihcl_g,ibin)  = 1.0
      phi_volatile(inh3_g,ibin)  = 0.0

      do iv = 1, naer_vol
        flux(iv,ibin)     = kg(iv,ibin)*df_gas(iv,ibin)
      enddo


      mc(jc_h,ibin) = sqrt(keq_ll(3))
      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))


      return
      end subroutine asteem_flux_wet_case2









! case 3:
! case 3: nh4no3 and/or nh4cl may be active
      subroutine asteem_flux_wet_case3(ibin, icontinue_case4)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, icontinue_case4
! local variables
      integer iv, iactive_nh4no3, iactive_nh4cl, iactive
      real xt, gnh3_hno3, gnh3_hcl, beta_nh4no3, beta_nh4cl,   &
           beta_nh4,   &
           keq_nh4no3, keq_nh4cl, pcnt_nh4no3, pcnt_nh4cl,   &
           ratio_flux, a, b, c, sfc_nh3_1, sfc_nh3_2,   &
           phi_nh3_1, phi_nh3_2, flux_nh3_est, flux_nh3_max
! function
!     real quadratic


!-------------------
! set default values for flags
      iactive_nh4no3 = 1
      iactive_nh4cl  = 2
      icontinue_case4 = mno	! default

!-------------------
! compute diagnostic products and ratios
      gnh3_hno3   = gas(inh3_g)*gas(ihno3_g)
      gnh3_hcl    = gas(inh3_g)*gas(ihcl_g)

      keq_nh4no3  = activity(jnh4no3,ibin)*kp_nh4no3	! = [nh3]s * [hno3]s
      keq_nh4cl   = activity(jnh4cl,ibin)*kp_nh4cl	! = [nh3]s * [hcl]s

      beta_nh4no3 = gnh3_hno3/keq_nh4no3_0
      beta_nh4cl  = gnh3_hcl/keq_nh4cl_0

      beta_nh4    = aer(inh4_a,jtotal,ibin)/aer_nh4_max(ibin)

      if(keq_nh4no3 .gt. 0.)then
        phi_nh4no3(ibin) =    (gnh3_hno3 - keq_nh4no3)/   &
                                     keq_nh4no3
      else
        phi_nh4no3(ibin) = 0.0
      endif

      if(keq_nh4cl .gt. 0.)then
        phi_nh4cl(ibin)  =    (gnh3_hcl - keq_nh4cl)/   &
                                     keq_nh4cl
      else
        phi_nh4cl(ibin)  = 0.0
      endif


!
! the following checks are order sensitive
!-------------------
! first check if the bin has reached equilibrium
      if(abs(phi_nh4no3(ibin)).lt.0.02 .and.   &
         abs(phi_nh4cl(ibin)) .lt.0.02)then
        iactive_nh4no3 = 0
        iactive_nh4cl  = 0
        iactive = 0
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return	! yes, the bin has reached equilibrium. quit
      endif


!------------------
! now check if nh4no3 and/or nh4cl want to evaporate completely
      call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)
      pcnt_nh4no3 = epercent(jnh4no3,jliquid,ibin)
      pcnt_nh4cl  = epercent(jnh4cl, jliquid,ibin)

      if( (gnh3_hno3.le.keq_nh4no3 .and. pcnt_nh4no3.lt.1.0) .and.   &
          (gnh3_hcl .le.keq_nh4cl  .and. pcnt_nh4cl .lt.1.0) )then
        if(electrolyte(jnh4so4,jliquid,ibin) .gt. 0.0)then
          call evaporate_nh4no3_nh4cl(ibin)
          return
        else
          icontinue_case4 = myes ! its a nacl/nano3/cacl2/cano3 particle with negligible nh3, nh4
          return
        endif
      endif

!--------------------
! now determine if nh4no3 and/or nh4cl are active or significant

! nh4no3
      if( (gnh3_hno3.gt.keq_nh4no3 .and. beta_nh4no3.lt.0.03) .or.   &
          (gnh3_hno3.gt.keq_nh4no3 .and. beta_nh4   .gt.0.99) .or.   &
          (abs(phi_nh4no3(ibin)) .lt. 0.02) )then
        iactive_nh4no3 = 0
      elseif(gnh3_hno3.lt.keq_nh4no3 .and. pcnt_nh4no3.lt.1.0)then
        iactive_nh4no3 = 0
        if(pcnt_nh4no3 .gt. 0.5)call evaporate_half_nh4no3(ibin)
      endif

! nh4cl
      if( (gnh3_hcl.gt.keq_nh4cl .and. beta_nh4cl.lt.0.03) .or.   &
          (gnh3_hcl.gt.keq_nh4cl .and. beta_nh4  .gt.0.99) .or.   &
          (abs(phi_nh4cl(ibin)) .lt. 0.02) )then
        iactive_nh4cl = 0
      elseif(gnh3_hcl.lt.keq_nh4cl .and. pcnt_nh4cl.lt.1.0)then
        iactive_nh4cl = 0
        if(pcnt_nh4cl .gt. 0.5)call evaporate_half_nh4cl(ibin)
      endif

      iactive = iactive_nh4no3 + iactive_nh4cl

! check the outcome
      if(iactive                          .eq. 0    .and.   &
         phi_nh4no3(ibin)                 .gt. 0.0  .and.   &
         phi_nh4cl(ibin)                  .gt. 0.0 )then
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      elseif(iactive                      .eq. 0    .and.   &
         abs(phi_nh4no3(ibin))            .gt. 0.02 .and.   &
         abs(phi_nh4cl(ibin))             .gt. 0.02 .and.   &
         aer_percent(inh4_a,jliquid,ibin) .lt. 1.0  .and.   &
         (aer_percent(icl_a,jliquid,ibin) .gt. 1.0  .or.   &
          aer_percent(ino3_a,jliquid,ibin).gt. 1.0) )then
        icontinue_case4 = myes		! nh3 and nh4 seems to be insignificant
        return				! therefore continue with case 4
      elseif(iactive .eq. 0)then
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      endif

      goto (1,2,3),iactive

!---------------------------------
! only nh4no3 is active
1     continue

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)
      c = -(kg(ihno3_g,ibin)*keq_nh4no3)

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihno3_g) = keq_nh4no3/sfc_a(inh3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)


      df_gas(ihno3_g,ibin) = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g)  - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)= phi_nh4no3(ibin)
      phi_volatile(ihcl_g,ibin) = 0.0
      phi_volatile(inh3_g,ibin) = phi_nh4no3(ibin)


      flux(inh3_g,ibin) = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      flux(ihno3_g,ibin)  = flux(inh3_g,ibin)
      flux(ihcl_g,ibin)   = 0.0

      mc(jc_h,ibin) = keq_gl(3)*sfc_a(ihno3_g)/   &
                      (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return



!-----------------
! only nh4cl is active
2     continue

      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)
      c = -(kg(ihcl_g,ibin)*keq_nh4cl)

      sfc_a(inh3_g)  = quadratic(a,b,c)
      sfc_a(ihcl_g)  = keq_nh4cl /sfc_a(inh3_g)
      sfc_a(ihno3_g) = gas(ihno3_g)


      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin)= 0.0
      phi_volatile(ihcl_g,ibin) = phi_nh4cl(ibin)
      phi_volatile(inh3_g,ibin) = phi_nh4cl(ibin)


      flux(inh3_g,ibin) = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
      flux(ihcl_g,ibin)   = flux(inh3_g,ibin)
      flux(ihno3_g,ibin)  = 0.0

        mc(jc_h,ibin) = keq_gl(4)*sfc_a(ihcl_g)/   &
                       (gam(jhcl,ibin)**2 * ma(ja_cl,ibin))

        ph(ibin) = -alog10(mc(jc_h,ibin))
        ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return


!-----------------
! both hno3 and hcl are active
3     continue

! nh4no3
      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihno3_g,ibin)*gas(ihno3_g)
      c = -(kg(ihno3_g,ibin)*keq_nh4no3)

      sfc_nh3_1    = quadratic(a,b,c)
      sfc_a(inh3_g)  = sfc_nh3_1
      sfc_a(ihno3_g) = keq_nh4no3/sfc_a(inh3_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
      df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
      flux(ihno3_g,ibin)   = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      phi_volatile(ihno3_g,ibin) = phi_nh4no3(ibin)


! nh4cl
      a =   kg(inh3_g,ibin)
      b = - kg(inh3_g,ibin)*gas(inh3_g)   &
          + kg(ihcl_g,ibin)*gas(ihcl_g)
      c = -(kg(ihcl_g,ibin)*keq_nh4cl)

      sfc_nh3_2    = quadratic(a,b,c)
      sfc_a(inh3_g)  = sfc_nh3_2
      sfc_a(ihcl_g)  = keq_nh4cl /sfc_a(inh3_g)
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
      df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)
      flux(ihcl_g,ibin)    = kg(ihcl_g,ibin)*df_gas(ihcl_g,ibin)
      phi_volatile(ihcl_g,ibin) = phi_nh4cl(ibin)

! nh3
      phi_volatile(inh3_g,ibin) = max( abs(phi_nh4cl(ibin)),   &
                                       abs(phi_nh4no3(ibin)) )


! now compute nh3 flux and adjust hno3 and/or hcl if necessary

      flux_nh3_est = flux(ihno3_g,ibin) + flux(ihcl_g,ibin)
      flux_nh3_max = kg(inh3_g,ibin)*gas(inh3_g)

      if(flux_nh3_est .le. flux_nh3_max)then
        flux(inh3_g,ibin) = flux_nh3_est			! all ok - no flux adjustments needed
        sfc_a(inh3_g)     = gas(inh3_g) - 			   &  ! recompute sfc_a(inh3_g)
                            flux(inh3_g,ibin)/kg(inh3_g,ibin)
        df_gas(inh3_g,ibin) = gas(inh3_g) - sfc_a(inh3_g)
        mc(jc_h,ibin)   = keq_gl(3)*sfc_a(ihno3_g)/   &
                         (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))

      else			! reduce hno3 and hcl fluxes as necessary so that nh3 flux = flux_nh3_max

        ratio_flux   = flux_nh3_max/flux_nh3_est
        flux(inh3_g,ibin)  = flux_nh3_max
        flux(ihno3_g,ibin) = flux(ihno3_g,ibin)*ratio_flux
        flux(ihcl_g, ibin) = flux(ihcl_g,ibin) *ratio_flux

        sfc_a(inh3_g)      = 0.0
        sfc_a(ihno3_g)     = gas(ihno3_g) -  			   &  ! recompute sfc_a(ihno3_g)
                             flux(ihno3_g,ibin)/kg(ihno3_g,ibin)
        sfc_a(ihcl_g)      = gas(ihcl_g)  -  			   &  ! recompute sfc_a(ihcl_g)
                             flux(ihcl_g,ibin)/kg(ihcl_g,ibin)

        df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)
        df_gas(ihno3_g,ibin) = gas(ihno3_g)- sfc_a(ihno3_g)
        df_gas(ihcl_g,ibin)  = gas(ihcl_g) - sfc_a(ihcl_g)

        mc(jc_h,ibin)    = keq_gl(3)*sfc_a(ihno3_g)/   &
                          (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))
      endif


      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))


      return
      end subroutine asteem_flux_wet_case3
















! case 4: nh3 = 0 (in gas and aerosol). hno3 and hcl exchange may happen here
      subroutine asteem_flux_wet_case4(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real gas_eqb_ratio, gas_act_ratio, phi_ratio




! now diagnose the situation

      gas_eqb_ratio = (keq_gl(4)*ma(ja_no3,ibin)*gam(jhno3,ibin)**2)/     &  ! ce,hno3/ce,hcl
                      (keq_gl(3)*ma(ja_cl ,ibin)*gam(jhcl,ibin)**2)

      gas_act_ratio = gas(ihno3_g)/gas(ihcl_g)

      phi_ratio = abs(gas_eqb_ratio - gas_act_ratio)/   &
                  max(gas_eqb_ratio , gas_act_ratio)


! check if equilibrium reached...
      if(phi_ratio .lt. 0.01)then
        flux(ihno3_g,ibin) = 0.0
        flux(ihcl_g,ibin)  = 0.0
        flux(inh3_g,ibin)  = 0.0
        return
      endif


! compute equilibrium surface concentrations
      sfc_a(ihcl_g) =   &
       (kg(ihno3_g,ibin)*gas(ihno3_g)+kg(ihcl_g,ibin)*gas(ihcl_g))/   &
            (kg(ihcl_g,ibin) + gas_eqb_ratio*kg(ihno3_g,ibin))
      sfc_a(ihno3_g)= gas_eqb_ratio*sfc_a(ihcl_g)

      df_gas(ihno3_g,ibin)  = gas(ihno3_g) - sfc_a(ihno3_g)
      df_gas(ihcl_g,ibin)   = gas(ihcl_g) - sfc_a(ihcl_g)

      phi_volatile(ihno3_g,ibin) =   df_gas(ihno3_g,ibin)/   &
                                 max(sfc_a(ihno3_g), 1.e-10)
      phi_volatile(ihcl_g,ibin)  =   df_gas(ihcl_g,ibin)/   &
                                 max(sfc_a(ihcl_g), 1.e-10)


      flux(ihno3_g,ibin)    = kg(ihno3_g,ibin)*df_gas(ihno3_g,ibin)
      flux(ihcl_g,ibin)     = -flux(ihno3_g,ibin)   ! kg(ihcl_g,ibin) *df_gas(ihcl_g, ibin)
      flux(inh3_g,ibin)     = 0.0

      mc(jc_h,ibin)       = keq_gl(3)*sfc_a(ihno3_g)/   &
                           (gam(jhno3,ibin)**2 * ma(ja_no3,ibin))

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine asteem_flux_wet_case4









! case 5
      subroutine asteem_flux_wet_case5(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin




      call equilibrate_acids(ibin)	! hno3 and/or hcl may be > 0 in the gas phase

      mc(jc_h,ibin) = max(sqrt(keq_ll(3)), mc(jc_h,ibin))

      sfc_a(ih2so4_g)= 0.0
      sfc_a(ihno3_g) = gas(ihno3_g)
      sfc_a(ihcl_g)  = gas(ihcl_g)
      sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                      (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

      df_gas(ih2so4_g,ibin)= 0.0
      df_gas(ihno3_g,ibin) = 0.0
      df_gas(ihcl_g,ibin)  = 0.0
      df_gas(inh3_g,ibin)  = gas(inh3_g) - sfc_a(inh3_g)


      phi_volatile(ihno3_g,ibin) = 0.0
      phi_volatile(ihcl_g,ibin)  = 0.0
      phi_volatile(inh3_g,ibin)  =   df_gas(inh3_g,ibin)/   &
                                 max(sfc_a(inh3_g), 1.e-10)


      if(abs(phi_volatile(inh3_g,ibin)) .lt. 0.01)then
        df_gas(inh3_g,ibin) = 0.0
      endif

      flux(ih2so4_g,ibin) = 0.0
      flux(ihno3_g,ibin)  = 0.0
      flux(ihcl_g,ibin)   = 0.0
      flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine asteem_flux_wet_case5
































!***********************************************************************
! computes mass transfer coefficients for each condensing species for
! all the aerosol bins
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine aerosolmtc

      use module_data_mosaic_asect

!     implicit none
!     include 'v33com9a'
!     include 'mosaic.h'
! local variables
      integer nghq
      parameter (nghq = 2)		! gauss-hermite quadrature order
      integer ibin, iq, iv
      real tworootpi, root2, beta
      parameter (tworootpi = 3.5449077, root2 = 1.4142135, beta = 2.0)
      real cdum, dp, dp_avg, fkn, kn, lnsg, lndpgn, lndp, speed, sumghq
      real xghq(nghq), wghq(nghq)	! quadrature abscissae and weights
      real mw_vol(naer_vol), v_molar(naer_vol) ! mw and molar vols of volatile species
      real freepath(naer_vol), accom(naer_vol), dg(naer_vol) ! keep local
!     real fuchs_sutugin				! mosaic func
!     real gas_diffusivity				! mosaic func
!     real mean_molecular_speed				! mosaic func






      mw_vol(ih2so4_g) = 98.0
      mw_vol(ihno3_g)  = 63.0
      mw_vol(ihcl_g)   = 36.5
      mw_vol(inh3_g)   = 17.0

      v_molar(ih2so4_g)= 42.88
      v_molar(ihno3_g) = 24.11
      v_molar(ihcl_g)  = 21.48
      v_molar(inh3_g)  = 14.90

      accom(ih2so4_g)  = 0.1
      accom(ihno3_g)   = 0.1
      accom(ihcl_g)    = 0.1
      accom(inh3_g)    = 0.1

      xghq(1) =  0.70710678
      xghq(2) = -0.70710678
      wghq(1) =  0.88622693
      wghq(2) =  0.88622693





! calculate gas diffusivity and mean free path for condensing gases
      do iv = 1, naer_vol
        speed  = mean_molecular_speed(t_k,mw_vol(iv))	! cm/s
        dg(iv) = gas_diffusivity(t_k,p_atm,mw_vol(iv),v_molar(iv)) ! cm^2/s
        freepath(iv) = 3.*dg(iv)/speed			! cm
      enddo


! calc mass transfer coefficients for gases over various aerosol bins

      if (msize_framework .eq. mmodal) then

! for modal approach
      do 10 ibin = 1, nbin_a

        if(jaerosolstate(ibin) .eq. no_aerosol)goto 10
        call calc_dry_n_wet_aerosol_props(ibin)

        dpgn_a(ibin) = dp_wet_a(ibin)	! cm

        lnsg   = alog(sigmag_a(ibin))
        lndpgn = alog(dpgn_a(ibin))
        cdum   = tworootpi*num_a(ibin)*   &
                 exp(beta*lndpgn + 0.5*(beta*lnsg)**2)

        do 20 iv = 1, naer_vol

          sumghq = 0.0
          do 30 iq = 1, nghq	! sum over gauss-hermite quadrature points
            lndp = lndpgn + beta*lnsg**2 + root2*lnsg*xghq(iq)
            dp = exp(lndp)
            kn = 2.*freepath(iv)/dp
            fkn = fuchs_sutugin(kn,accom(iv))
            sumghq = sumghq + wghq(iq)*dp*fkn/(dp**beta)
30        continue

        kg(iv,ibin) = cdum*dg(iv)*sumghq		! 1/s

20      continue
10    continue

      elseif(msize_framework .eq. msection)then

! for sectional approach
      do 11 ibin = 1, nbin_a

        if(jaerosolstate(ibin) .eq. no_aerosol)goto 11

        call calc_dry_n_wet_aerosol_props(ibin)

        dp_avg = dp_wet_a(ibin)
        cdum  = 6.283185*dp_avg*num_a(ibin)

        do 21 iv = 1, naer_vol
          kn = 2.*freepath(iv)/dp_avg
          fkn = fuchs_sutugin(kn,accom(iv))
          kg(iv,ibin) = cdum*dg(iv)*fkn		! 1/s
21      continue

11    continue

      else

        write(6,*)'error in the choice of msize_framework'
        write(6,*)'stopping in subr. aerosolmtc'
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping in subr. aerosolmtc' )

      endif


      return
      end subroutine aerosolmtc












!***********************************************************************
! calculates dry and wet aerosol properties: density, refractive indices
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine calc_dry_n_wet_aerosol_props(ibin)

      use module_data_mosaic_asect

!     implicit none
!     include 'v33com9a'
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer isize, itype, jc, je
      complex ri_dum




      if(jaerosolstate(ibin) .ne. no_aerosol)then

! calculate dry mass and dry volume of a bin
        mass_dry_a(ibin) = 0.0		! initialize to 0.0
        vol_dry_a(ibin)  = 0.0		! initialize to 0.0

! first add all electrolytes
        do je = 1, nelectrolyte
          comp_a(je)=electrolyte(je,jtotal,ibin)*mw_comp_a(je)*1.e-15	! g/cc(air)
          mass_dry_a(ibin) = mass_dry_a(ibin) +	comp_a(je)		! g/cc(air)

          vol_dry_a(ibin) = vol_dry_a(ibin) + comp_a(je)/ 		   &  ! cc(aer)/cc(air)
                                    dens_comp_a(je)
        enddo

! next add all other aerosol species except water
        comp_a(joc) = aer(ioc_a,jtotal,ibin)*1.e-15			! g/cc(air)
        comp_a(jbc) = aer(ibc_a,jtotal,ibin)*1.e-15			! g/cc(air)
        comp_a(join)= aer(ioin_a,jtotal,ibin)*1.e-15			! g/cc(air)
        comp_a(jh2o)= water_a(ibin)*1.e-3				! g/cc(air)

        mass_dry_a(ibin) = mass_dry_a(ibin) +				   &  ! g/cc(air)
                           comp_a(joc)      +   &
                           comp_a(jbc)      +   &
                           comp_a(join)

        vol_dry_a(ibin)  = vol_dry_a(ibin)                +		   &  ! cc(aer)/cc(air)
          		   comp_a(joc)/dens_comp_a(joc)   +   &
          		   comp_a(jbc)/dens_comp_a(jbc)   +   &
          		   comp_a(join)/dens_comp_a(join)

! wet mass and wet volume
        mass_wet_a(ibin) = mass_dry_a(ibin) + comp_a(jh2o)		! g/cc(air)

        vol_wet_a(ibin) = vol_dry_a(ibin) + 				   &  ! cc(aer)/cc(air)
      			  comp_a(jh2o)/dens_comp_a(jh2o)


! calculate mean dry and wet particle densities
        dens_dry_a(ibin) = mass_dry_a(ibin)/vol_dry_a(ibin) ! g/cc(aerosol)
        dens_wet_a(ibin) = mass_wet_a(ibin)/vol_wet_a(ibin) ! g/cc(aerosol)


! calculate mean dry and wet particle diameters
        dp_dry_a(ibin)=(1.90985*vol_dry_a(ibin)/num_a(ibin))**0.3333333	! cm
        dp_wet_a(ibin)=(1.90985*vol_wet_a(ibin)/num_a(ibin))**0.3333333 ! cm


! calculate volume average refractive index
        ri_dum = (0.0,0.0)
        do jc = 1, naercomp
          ri_dum = ri_dum + ref_index_a(jc)*   &
                            comp_a(jc)/dens_comp_a(jc)
        enddo

        ri_avg_a(ibin) = ri_dum/vol_wet_a(ibin)

      else	! use defaults

        dens_dry_a(ibin) = 1.0	 ! g/cc(aerosol)
        dens_wet_a(ibin) = 1.0	 ! g/cc(aerosol)

        call isize_itype_from_ibin( ibin, isize, itype )
        dp_dry_a(ibin) = dcen_sect(isize,itype)	! cm
        dp_wet_a(ibin) = dcen_sect(isize,itype)	! cm


        ri_avg_a(ibin) = (1.5,0.0)
      endif


      return
      end subroutine calc_dry_n_wet_aerosol_props




















!***********************************************************************
! computes activities
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine compute_activities(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real xt
! function
!     real aerosol_water


      water_a(ibin) = aerosol_water(jliquid,ibin)	! kg/m^3(air)
      if(water_a(ibin) .eq. 0.0)return

      call calculate_xt(ibin,jliquid,xt)

      if(xt.gt.2.0 .or. xt.lt.0.)then		! check .ge. if that messes up phase calc
        call sulfate_poor_activities(ibin)	! fully dissociated electrolytes
      else
        call sulfate_rich_activities(ibin)	! solve for so4= and hso4- ions
      endif


      return
      end subroutine compute_activities










!***********************************************************************
! computes activities for sulfate-poor systems
! all electrolytes in the liquid phase are completely dissociated
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine sulfate_poor_activities(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real equiv_anions, equiv_cations, a_c


! water molality
      mh2o        = 55.509	! 1000 g water / mw(h2o),   (mw(h2o) = 18.016)

! anion molalities (mol/kg water)
      ma(ja_so4,ibin)  = 1.e-9*aer(iso4_a,jliquid,ibin)/water_a(ibin)
      ma(ja_hso4,ibin) = 0.0
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)
      equiv_anions  = 2.*ma(ja_so4,ibin) +   &
                         ma(ja_no3,ibin) +   &
                         ma(ja_cl,ibin)

! cation molalities (mol/kg water)
      mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)
      equiv_cations =    mc(jc_nh4,ibin) +   &
                         mc(jc_na,ibin)  +   &
                      2.*mc(jc_ca,ibin)

      a_c = real(dble(equiv_anions) - dble(equiv_cations))
      mc(jc_h,ibin)    = 0.5*real( dble(a_c) +   &
                              dble(sqrt(a_c**2 + 4.*keq_ll(3))) )

!      mc(jc_h,ibin)    = max( mc(jc_h,ibin), sqrt(keq_ll(3)) )

      if(mc(jc_h,ibin) .eq. 0.0)then
        mc(jc_h,ibin) = 1.e-10
      endif

      ph(ibin) = -alog10(mc(jc_h,ibin))

! compute activity coefficients
      if(mactivity_coeff .eq. mmtem)then
          call mtem_sulfate_poor(ibin)		! mtem (2004)
      elseif(mactivity_coeff .eq. mpsc)then
          call psc_sulfate_poor(ibin)		! psc (1992, 1998)
      elseif(mactivity_coeff .eq. mkm)then
          call km_sulfate_poor(ibin)		! km (1978)
      elseif(mactivity_coeff .eq. mbrom)then
          call brom_sulfate_poor(ibin)	! bromley (1973)
      endif


! compute activities
      activity(jnh4so4,ibin) = mc(jc_nh4,ibin)**2 * ma(ja_so4,ibin) *   &
                               gam(jnh4so4,ibin)**3

      activity(jnh4no3,ibin) = mc(jc_nh4,ibin) * ma(ja_no3,ibin) *   &
                               gam(jnh4no3,ibin)**2

      activity(jnh4cl,ibin)  = mc(jc_nh4,ibin) * ma(ja_cl,ibin) *   &
                               gam(jnh4cl,ibin)**2

      activity(jna2so4,ibin) = mc(jc_na,ibin)**2 * ma(ja_so4,ibin) *   &
                               gam(jna2so4,ibin)**3

      activity(jnano3,ibin)  = mc(jc_na,ibin) * ma(ja_no3,ibin) *   &
                               gam(jnano3,ibin)**2

      activity(jnacl,ibin)   = mc(jc_na,ibin) * ma(ja_cl,ibin) *   &
                               gam(jnacl,ibin)**2

      activity(jcano3,ibin)  = mc(jc_ca,ibin) * ma(ja_no3,ibin)**2 *   &
                               gam(jcano3,ibin)**3

      activity(jcacl2,ibin)  = mc(jc_ca,ibin) * ma(ja_cl,ibin)**2 *   &
                               gam(jcacl2,ibin)**3

      activity(jhno3,ibin)   = mc(jc_h,ibin) * ma(ja_no3,ibin) *   &
                               gam(jhno3,ibin)**2

      activity(jhcl,ibin)    = mc(jc_h,ibin) * ma(ja_cl,ibin) *   &
                               gam(jhcl,ibin)**2

!
      activity(jlvcite,ibin) = 0.0

      activity(jnh4hso4,ibin)= 0.0

      activity(jnahso4,ibin) = 0.0

      activity(jna3hso4,ibin)= 0.0

      return
      end subroutine sulfate_poor_activities















!***********************************************************************
! pitzer-simonson-clegg (psc) model for multicomponent activity coefficients
!
! author: rahul a. zaveri
! update: jan 2005
! reference: j. phys. chem. a 1998, 102, 2155-2171
!-----------------------------------------------------------------------
      subroutine psc_sulfate_poor(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jx, jm, ja, jc, izi, izj
      real mtot, mion, mcation, manion, ix_c, ix_a,   &
           sum_xczc, sum_xaza, dum
! function
!     real fn_thetahoe, fn_dthetahoe, fm, fx, fn



! calculate mol fractions

      mh2o = 55.509	! molality of water

      mcation = 0.0
      do jc = 1, ncation
        mcation = mcation + mc(jc,ibin)
      enddo

      manion = 0.0
      do ja = 1, nanion
        manion = manion + ma(ja,ibin)
      enddo

      mion = mcation + manion

      mtot   = mh2o + mion

      do jc = 1, ncation
        xc(jc) = mc(jc,ibin)/mtot
      enddo

      do ja = 1, nanion
        xa(ja) = ma(ja,ibin)/mtot
      enddo

      xh2o  = mh2o/mtot
!
! calculate variables for mol-fraction
! scale activity coefficient model

      sum_xczc = 0.0
      ix_c = 0.0
      do jc = 1, ncation
        sum_xczc = sum_xczc + xc(jc)*zc(jc)
        ix_c = ix_c + 0.5*xc(jc)*zc(jc)**2
      enddo

      sum_xaza = 0.0
      ix_a = 0.0
      do ja = 1, nanion
        sum_xaza = sum_xaza + xa(ja)*za(ja)
        ix_a = ix_a + 0.5*xa(ja)*za(ja)**2
      enddo

      ix = ix_c + ix_a		! mole fraction ionic strength
      ff = 2./(sum_xczc + sum_xaza)
!
! equivalent cation fractions
      do jc = 1, ncation
      ec(jc)=  xc(jc)*zc(jc)/sum_xczc
      enddo

! differentials
      do jm = 1, ncation
      	do jc = 1, ncation
          if(jm.eq.jc)then
      	    emc(jm,jc) = (zc(jm)/sum_xczc) * (1 - ec(jm))
      	  else
            emc(jm,jc) = -zc(jm)*ec(jc)/sum_xczc
          endif
     	enddo
      enddo

! equivalent anion fractions
      do ja = 1, nanion
      ea(ja)=  xa(ja)*za(ja)/sum_xaza
      enddo

! differentials
      do jx = 1, nanion
      	do ja = 1, nanion
          if(jx.eq.ja)then
      	    exa(jx,ja) = (za(jx)/sum_xaza) * (1 - ea(jx))
      	  else
            exa(jx,ja) = -za(jx)*ea(ja)/sum_xaza
          endif
     	enddo
      enddo


      do izi = 1, 2
      do izj = 1, 2

        thetahoe(izi,izj) = fn_thetahoe(izi,izj)
        dthetahoe(izi,izj) = fn_dthetahoe(izi,izj)

      enddo
      enddo

!
!
! mole fraction-scale activity coefficients
        fh2o   = fn(1) ! neutral species (h2o)

      do jm = 1, ncation_clegg
        fc(jm,ibin) = fm(jm)	! cations
      enddo


      do jx = 1, nanion_clegg
        fa(jx,ibin) = fx(jx)	! anions
      enddo


!
! molality-scale activity coefficients
      dum   = (1. + mion/mh2o)

      do jc = 1, ncation_clegg
       gam_cation(jc,ibin) = fc(jc,ibin)/dum
      enddo

      do ja = 1, nanion_clegg
       gam_anion(ja,ibin)  = fa(ja,ibin)/dum
      enddo


      gam(jnh4no3,ibin) = (gam_cation(jc_nh4,ibin)*   &
                           gam_anion(ja_no3,ibin))**0.5
      gam(jnh4cl,ibin)  = (gam_cation(jc_nh4,ibin)*   &
                           gam_anion(ja_cl,ibin))**0.5
      gam(jnh4so4,ibin) = (gam_cation(jc_nh4,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)
      gam(jnacl,ibin)   = (gam_cation(jc_na,ibin)*   &
                           gam_anion(ja_cl,ibin))**0.5
      gam(jnano3,ibin)  = (gam_cation(jc_na,ibin)*   &
                           gam_anion(ja_no3,ibin))**0.5
      gam(jna2so4,ibin) = (gam_cation(jc_na,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)
      gam(jhno3,ibin)   = (gam_cation(jc_h,ibin)*   &
                           gam_anion(ja_no3,ibin))**0.5
      gam(jhcl,ibin)    = (gam_cation(jc_h,ibin)*   &
                           gam_anion(ja_cl,ibin))**0.5
      gam(jcacl2,ibin)  = (gam_cation(jc_ca,ibin)*   &
                           gam_anion(ja_cl,ibin)**2)**(1./3.)
      gam(jcano3,ibin)  = (gam_cation(jc_ca,ibin)*   &
                           gam_anion(ja_no3,ibin)**2)**(1./3.)

      gam_ratio(ibin)   = gam_cation(jc_nh4,ibin)/gam_cation(jc_h,ibin)

!----
      gam(jlvcite,ibin) = (gam_cation(jc_nh4,ibin)**3 *   &
                           gam_anion(ja_hso4,ibin) *   &
                           gam_anion(ja_so4,ibin))**(1./5.)

      gam(jnh4hso4,ibin)= (gam_cation(jc_nh4,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)

      gam(jnahso4,ibin) = (gam_cation(jc_na,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)

      gam(jna3hso4,ibin) = (gam_cation(jc_na,ibin)**3 *   &
                           gam_anion(ja_hso4,ibin) *   &
                           gam_anion(ja_so4,ibin))**(1./5.)

      gam(jh2so4,ibin)  = (gam_cation(jc_h,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)

      gam(jhhso4,ibin)  = (gam_cation(jc_h,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)


      return
      end subroutine psc_sulfate_poor





!***********************************************************************
! kusik and meissner's method for multicomponent activity coefficients
!
! author: rahul a. zaveri
! update: jan 2005
! reference: aiche j., 1978
!-----------------------------------------------------------------------
      subroutine km_sulfate_poor(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jc, ja
      real im_c, im_a, x
! functions
!     real fnlog_gam0


! compute molality-scale ionic strength
      im_c = 0.0
      do jc = 1, ncation
        im_c = im_c + 0.5*mc(jc,ibin)*zc(jc)**2
      enddo

      im_a = 0.0
      do ja = 1, nanion
        im_a = im_a + 0.5*ma(ja,ibin)*za(ja)**2
      enddo

      im(ibin) = im_c + im_a		! molality ionic strength
      x = im(ibin)

      log_gam0(jnh4so4) = fnlog_gam0(jnh4so4,im(ibin))
      log_gam0(jnh4no3) = fnlog_gam0(jnh4no3,im(ibin))
      log_gam0(jnh4cl)  = fnlog_gam0(jnh4cl,im(ibin))
      log_gam0(jnacl)   = fnlog_gam0(jnacl,im(ibin))
      log_gam0(jnano3)  = fnlog_gam0(jnano3,im(ibin))
      log_gam0(jna2so4) = fnlog_gam0(jna2so4,im(ibin))
      log_gam0(jcacl2)  = fnlog_gam0(jcacl2,im(ibin))
      log_gam0(jcano3)  = fnlog_gam0(jcano3,im(ibin))
      log_gam0(jhno3)   = fnlog_gam0(jhno3,im(ibin))
      log_gam0(jhcl)    = fnlog_gam0(jhcl,im(ibin))
      log_gam0(jh2so4)  = fnlog_gam0(jh2so4,im(ibin))
      log_gam0(jhhso4)  = fnlog_gam0(jhhso4,im(ibin))


! nh4no3
        log_gam(jnh4no3) = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnh4no3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnh4cl)    +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jnh4so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4no3)   +   &
                           mc(jc_na,ibin) *log_gam0(jnano3)    +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcano3)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhno3)  )
        gam(jnh4no3,ibin) = 10.**log_gam(jnh4no3)



! nh4cl
        log_gam(jnh4cl)  = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnh4no3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnh4cl)    +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jnh4so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4cl)    +   &
                           mc(jc_na,ibin) *log_gam0(jnacl)     +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcacl2)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhcl)   )
        gam(jnh4cl,ibin) = 10.**log_gam(jnh4cl)



! (nh4)2so4
        log_gam(jnh4so4) = (0.666666667/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnh4no3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnh4cl)    +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jnh4so4) ) +   &
                         (0.333333333/im(ibin))*   &
                    ( 2.25*mc(jc_nh4,ibin)*log_gam0(jnh4so4)   +   &
                      2.25*mc(jc_na,ibin) *log_gam0(jna2so4) )
        gam(jnh4so4,ibin) = 10.**log_gam(jnh4so4)



! nacl
        log_gam(jnacl)   = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnano3)    +   &
                           ma(ja_cl,ibin) *log_gam0(jnacl)     +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jna2so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4cl)    +   &
                           mc(jc_na,ibin) *log_gam0(jnacl)     +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcacl2)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhcl)   )
        gam(jnacl,ibin)  = 10.**log_gam(jnacl)



! nano3
        log_gam(jnano3)  = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnano3)    +   &
                           ma(ja_cl,ibin) *log_gam0(jnacl)     +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jna2so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4no3)   +   &
                           mc(jc_na,ibin) *log_gam0(jnano3)    +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcano3)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhno3)  )
        gam(jnano3,ibin) = 10.**log_gam(jnano3)



! na2so4
        log_gam(jna2so4) = (0.666666667/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnano3)    +   &
                           ma(ja_cl,ibin) *log_gam0(jnacl)     +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jna2so4) ) +   &
                         (0.333333333/im(ibin))*   &
                    ( 2.25*mc(jc_nh4,ibin)*log_gam0(jnh4so4)   +   &
                      2.25*mc(jc_na,ibin) *log_gam0(jna2so4) )
        gam(jna2so4,ibin) = 10.**log_gam(jna2so4)


! ca(no3)2
        log_gam(jcano3)  = (0.333333333/im(ibin))*   &
                    ( 2.25*ma(ja_no3,ibin)*log_gam0(jcano3)    +   &
                      2.25*ma(ja_cl,ibin) *log_gam0(jcacl2)  ) +   &
                         (0.666666666/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4no3)   +   &
                           mc(jc_na,ibin) *log_gam0(jnano3)    +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcano3)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhno3)  )
        gam(jcano3,ibin) = 10.**log_gam(jcano3)


! cacl2
        log_gam(jcacl2)  = (0.333333333/im(ibin))*   &
                    ( 2.25*ma(ja_no3,ibin)*log_gam0(jcano3)    +   &
                      2.25*ma(ja_cl,ibin) *log_gam0(jcacl2) )  +   &
                         (0.666666666/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4cl)    +   &
                           mc(jc_na,ibin) *log_gam0(jnacl)     +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcacl2)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhcl)   )
        gam(jcacl2,ibin) = 10.**log_gam(jcacl2)


! hno3
      log_gam(jhno3)   = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4no3)   +   &
                           mc(jc_na,ibin) *log_gam0(jnano3)    +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcano3)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhno3)  )
      gam(jhno3,ibin)  = 10.**log_gam(jhno3)


! hcl
      log_gam(jhcl)    = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4cl)    +   &
                           mc(jc_na,ibin) *log_gam0(jnacl)     +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcacl2)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhcl)   )
      gam(jhcl,ibin)   = 10.**log_gam(jhcl)

!----
      gam(jlvcite,ibin) = 1.0

      gam(jnh4hso4,ibin)= 1.0

      gam(jnahso4,ibin) = 1.0

      gam(jna3hso4,ibin) = 1.0

      gam_ratio(ibin) = gam(jnh4no3,ibin)**2/gam(jhno3,ibin)**2

      return
      end subroutine km_sulfate_poor










!***********************************************************************
! bromley method for multicomponent activity coefficients
!
! author: rahul a. zaveri
! update: jan 2005
! reference: aiche j., 1973
!-----------------------------------------------------------------------
      subroutine brom_sulfate_poor(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jc, ja
      real im_c, im_a, x, aterm, agam, sqim,   &
           z_nh4, z_na, z_ca, z_h,   &
           z_so4, z_no3, z_cl,   &
           y_so4_nh4, y_so4_na, y_so4_h,   &
           y_no3_nh4, y_no3_na, y_no3_ca, y_no3_h,   &
           y_cl_nh4, y_cl_na, y_cl_ca, y_cl_h,   &
           x_nh4_so4, x_na_so4, x_h_so4, x_nh4_no3,   &
           x_na_no3, x_ca_no3, x_h_no3,   &
           x_nh4_cl, x_na_cl, x_ca_cl, x_h_cl,   &
           f_nh4, f_na, f_ca, f_h,   &
           f_no3, f_cl, f_so4
! function
!     real fnlog_gam0


! compute molality-scale ionic strength
      im_c = 0.0
      do jc = 1, ncation
        im_c = im_c + 0.5*mc(jc,ibin)*zc(jc)**2
      enddo

      im_a = 0.0
      do ja = 1, nanion
        im_a = im_a + 0.5*ma(ja,ibin)*za(ja)**2
      enddo

      im(ibin) = im_c + im_a		! molality ionic strength
      x = im(ibin)

      log_gam0(jnh4so4) = fnlog_gam0(jnh4so4,im(ibin))
      log_gam0(jnh4no3) = fnlog_gam0(jnh4no3,im(ibin))
      log_gam0(jnh4cl)  = fnlog_gam0(jnh4cl,im(ibin))
      log_gam0(jnacl)   = fnlog_gam0(jnacl,im(ibin))
      log_gam0(jnano3)  = fnlog_gam0(jnano3,im(ibin))
      log_gam0(jna2so4) = fnlog_gam0(jna2so4,im(ibin))
      log_gam0(jcacl2)  = fnlog_gam0(jcacl2,im(ibin))
      log_gam0(jcano3)  = fnlog_gam0(jcano3,im(ibin))
      log_gam0(jhno3)   = fnlog_gam0(jhno3,im(ibin))
      log_gam0(jhcl)    = fnlog_gam0(jhcl,im(ibin))
      log_gam0(jh2so4)  = fnlog_gam0(jh2so4,im(ibin))
      log_gam0(jhhso4)  = fnlog_gam0(jhhso4,im(ibin))


      agam = 0.511 ! (kg/mol)^0.5
      sqim = sqrt(im(ibin))

      aterm = agam*sqim/(1. + sqim)

      z_nh4 = 1.0
      z_na  = 1.0
      z_ca  = 2.0
      z_h   = 1.0

      z_so4 = 2.0
      z_no3 = 1.0
      z_cl  = 1.0

! y
      y_so4_nh4 = ((z_nh4+z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)
      y_so4_na  = ((z_na +z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)
      y_so4_h   = ((z_h  +z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)

      y_no3_nh4 = ((z_nh4+z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_na  = ((z_na +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_ca  = ((z_ca +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_h   = ((z_h  +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)

      y_cl_nh4  = ((z_nh4+z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_na   = ((z_na +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_ca   = ((z_ca +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_h    = ((z_h  +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)

! x
      x_nh4_so4 = ((z_nh4+z_so4)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_so4  = ((z_na +z_so4)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_h_so4   = ((z_h  +z_so4)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)

      x_nh4_no3 = ((z_nh4+z_no3)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_no3  = ((z_na +z_no3)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_ca_no3  = ((z_ca +z_no3)/2.0)**2.0*mc(jc_ca,ibin)/im(ibin)
      x_h_no3   = ((z_h  +z_no3)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)

      x_nh4_cl  = ((z_nh4+z_cl)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_cl   = ((z_na +z_cl)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_ca_cl   = ((z_ca +z_cl)/2.0)**2.0*mc(jc_ca,ibin)/im(ibin)
      x_h_cl    = ((z_h  +z_cl)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)



      f_nh4 = y_no3_nh4*log_gam0(jnh4no3) +   &
              y_so4_nh4*log_gam0(jnh4so4) +   &
              y_cl_nh4 *log_gam0(jnh4cl)  +   &
       aterm*(z_nh4*z_no3*y_no3_nh4 +   &
              z_nh4*z_so4*y_so4_nh4 +   &
              z_nh4*z_cl *y_cl_nh4)

      f_na  = y_no3_na*log_gam0(jnano3)  +   &
              y_so4_na*log_gam0(jna2so4) +   &
              y_cl_na *log_gam0(jnacl)   +   &
       aterm*(z_na*z_no3*y_no3_na +   &
              z_na*z_so4*y_so4_na +   &
              z_na*z_cl *y_cl_na)

      f_ca  = y_no3_ca*log_gam0(jcano3) +   &
              y_cl_ca *log_gam0(jcacl2) +   &
       aterm*(z_ca*z_no3*y_no3_ca +   &
              z_ca*z_cl *y_cl_ca)

      f_h   = y_so4_h*log_gam0(jh2so4) +   &
              y_no3_h*log_gam0(jhno3)  +   &
              y_cl_h *log_gam0(jhcl)   +   &
       aterm*(z_h*z_so4*y_so4_h +   &
              z_h*z_no3*y_no3_h +   &
              z_h*z_cl *y_cl_h)


      f_no3 = x_nh4_no3*log_gam0(jnh4no3) +   &
              x_na_no3 *log_gam0(jnano3)  +   &
              x_ca_no3 *log_gam0(jcano3)  +   &
              x_h_no3  *log_gam0(jhno3)   +   &
       aterm*(z_nh4*z_no3*x_nh4_no3 +   &
              z_na *z_no3*x_na_no3  +   &
              z_ca *z_no3*x_ca_no3  +   &
              z_h  *z_no3*x_h_no3)

      f_cl  = x_nh4_cl*log_gam0(jnh4cl) +   &
              x_na_cl *log_gam0(jnacl)  +   &
              x_ca_cl *log_gam0(jcacl2) +   &
              x_h_cl  *log_gam0(jhcl)   +   &
       aterm*(z_nh4*z_cl*x_nh4_cl +   &
              z_na *z_cl*x_na_cl  +   &
              z_ca *z_cl*x_ca_cl  +   &
              z_h  *z_cl*x_h_cl)

      f_so4 = x_nh4_so4*log_gam0(jnh4so4) +   &
              x_na_so4 *log_gam0(jna2so4) +   &
              x_h_so4  *log_gam0(jh2so4)  +   &
       aterm*(z_nh4*z_so4*x_nh4_so4 +   &
              z_na *z_so4*x_na_so4  +   &
              z_h  *z_so4*x_h_so4)


! nh4no3
      log_gam(jnh4no3) = -z_nh4*z_no3*aterm +   &
            z_nh4*z_no3/(z_nh4+z_no3)*(f_nh4/z_nh4 + f_no3/z_no3)
      gam(jnh4no3,ibin) = 10.**log_gam(jnh4no3)


! nh4cl
      log_gam(jnh4cl)  = -z_nh4*z_cl*aterm +   &
            z_nh4*z_cl/(z_nh4+z_cl)*(f_nh4/z_nh4 + f_cl/z_cl)
      gam(jnh4cl,ibin) = 10.**log_gam(jnh4cl)


! (nh4)2so4
      log_gam(jnh4so4) = -z_nh4*z_so4*aterm +   &
            z_nh4*z_so4/(z_nh4+z_so4)*(f_nh4/z_nh4 + f_so4/z_so4)
      gam(jnh4so4,ibin) = 10.**log_gam(jnh4so4)

! nacl
      log_gam(jnacl)   = -z_na*z_cl*aterm +   &
            z_na*z_cl/(z_na+z_cl)*(f_na/z_na + f_cl/z_cl)
      gam(jnacl,ibin)  = 10.**log_gam(jnacl)

! nano3
      log_gam(jnano3)  = -z_na*z_no3*aterm +   &
            z_na*z_no3/(z_na+z_no3)*(f_na/z_na + f_no3/z_no3)
      gam(jnano3,ibin) = 10.**log_gam(jnano3)

! na2so4
      log_gam(jna2so4) = -z_na*z_so4*aterm +   &
            z_na*z_so4/(z_na+z_so4)*(f_na/z_na + f_so4/z_so4)
      gam(jna2so4,ibin) = 10.**log_gam(jna2so4)


! ca(no3)2
      log_gam(jcano3)  = -z_ca*z_no3*aterm +   &
            z_ca*z_no3/(z_ca+z_no3)*(f_ca/z_ca + f_no3/z_no3)
      gam(jcano3,ibin) = 10.**log_gam(jcano3)


! cacl2
      log_gam(jcacl2)  = -z_ca*z_cl*aterm +   &
            z_ca*z_cl/(z_ca+z_cl)*(f_ca/z_ca + f_cl/z_cl)
      gam(jcacl2,ibin) = 10.**log_gam(jcacl2)


! hno3
      log_gam(jhno3)   = -z_h*z_no3*aterm +   &
            z_h*z_no3/(z_h+z_no3)*(f_h/z_h + f_no3/z_no3)
      gam(jhno3,ibin)  = 10.**log_gam(jhno3)


! hcl
      log_gam(jhcl)    = -z_h*z_cl*aterm +   &
            z_h*z_cl/(z_h+z_cl)*(f_h/z_h + f_cl/z_cl)
      gam(jhcl,ibin)   = 10.**log_gam(jhcl)

!----
      gam(jlvcite,ibin) = 1.0

      gam(jnh4hso4,ibin)= 1.0

      gam(jnahso4,ibin) = 1.0

      gam(jna3hso4,ibin) = 1.0

      gam_ratio(ibin) = gam(jnh4no3,ibin)**2/gam(jhno3,ibin)**2



      return
      end subroutine brom_sulfate_poor









!***********************************************************************
! multicomponent taylor expansion method (mtem)
!
! author: rahul a. zaveri
! update: jan 2005
! reference: zaveri et al., jgr 2005
!-----------------------------------------------------------------------
      subroutine mtem_sulfate_poor(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jp, ja
      real xmol(nelectrolyte), sum_elec




      jp = jliquid


      sum_elec = 2.*electrolyte(jnh4no3,jp,ibin) +   &
                 2.*electrolyte(jnh4cl,jp,ibin)  +   &
                 3.*electrolyte(jnh4so4,jp,ibin) +   &
                 3.*electrolyte(jna2so4,jp,ibin) +   &
                 2.*electrolyte(jnano3,jp,ibin)  +   &
                 2.*electrolyte(jnacl,jp,ibin)   +   &
                 3.*electrolyte(jcano3,jp,ibin)  +   &
                 3.*electrolyte(jcacl2,jp,ibin)  +   &
                 2.*electrolyte(jhno3,jp,ibin)   +   &
                 2.*electrolyte(jhcl,jp,ibin)


! ionic mole fractions
      xmol(jnh4no3) = 2.*electrolyte(jnh4no3,jp,ibin)/sum_elec
      xmol(jnh4cl)  = 2.*electrolyte(jnh4cl,jp,ibin) /sum_elec
      xmol(jnh4so4) = 3.*electrolyte(jnh4so4,jp,ibin)/sum_elec
      xmol(jna2so4) = 3.*electrolyte(jna2so4,jp,ibin)/sum_elec
      xmol(jnano3)  = 2.*electrolyte(jnano3,jp,ibin) /sum_elec
      xmol(jnacl)   = 2.*electrolyte(jnacl,jp,ibin)  /sum_elec
      xmol(jcano3)  = 3.*electrolyte(jcano3,jp,ibin) /sum_elec
      xmol(jcacl2)  = 3.*electrolyte(jcacl2,jp,ibin) /sum_elec
      xmol(jhno3)   = 2.*electrolyte(jhno3,jp,ibin)  /sum_elec
      xmol(jhcl)    = 2.*electrolyte(jhcl,jp,ibin)   /sum_elec


      ja = jnh4so4
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)



      ja = jnh4no3
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jnh4cl
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jna2so4
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jnano3
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jnacl
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jcano3
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)



      ja = jcacl2
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jhno3
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      ja = jhcl
      log_gam(ja) = xmol(jnh4no3)*log_gamz(ja,jnh4no3) +   &
                    xmol(jnh4cl) *log_gamz(ja,jnh4cl)  +   &
                    xmol(jnh4so4)*log_gamz(ja,jnh4so4) +   &
                    xmol(jna2so4)*log_gamz(ja,jna2so4) +   &
                    xmol(jnano3) *log_gamz(ja,jnano3)  +   &
                    xmol(jnacl)  *log_gamz(ja,jnacl)   +   &
                    xmol(jcano3) *log_gamz(ja,jcano3)  +   &
                    xmol(jcacl2) *log_gamz(ja,jcacl2)  +   &
                    xmol(jhno3)  *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)   *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)

!----
      gam(jlvcite,ibin) = 1.0

      gam(jnh4hso4,ibin)= 1.0

      gam(jnahso4,ibin) = 1.0

      gam(jna3hso4,ibin) = 1.0

      gam_ratio(ibin) = gam(jnh4no3,ibin)**2/gam(jhno3,ibin)**2

!      gam_ratio(ibin) = max( gam(jnh4no3,ibin)**2/gam(jhno3,ibin)**2,
!     &                       gam(jnh4cl,ibin)**2/gam(jhcl,ibin)**2 )

      return
      end subroutine mtem_sulfate_poor








!***********************************************************************
! computes activity coefficients for sulfate-rich systems
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine sulfate_rich_activities(ibin)	! rigorous method: psc model
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin





! compute activity coefficients
      if(mactivity_coeff .eq. mmtem)then
        call mtem_sulfate_rich(ibin)		! mtem (2004)
      elseif(mactivity_coeff .eq. mpsc .or. 		   &  ! psc (1992, 1998)
             mactivity_coeff .eq. mkm  .or. 		   &  ! km (1978)
             mactivity_coeff .eq. mbrom)then		! bromley (1973)
        call sulfate_rich_iterations(ibin)
      endif


! compute activities
      activity(jnh4so4,ibin) = mc(jc_nh4,ibin)**2 * ma(ja_so4,ibin) *   &
                               gam(jnh4so4,ibin)**3

      activity(jlvcite,ibin) = mc(jc_nh4,ibin)**3 * ma(ja_hso4,ibin) *   &
                               ma(ja_so4,ibin) * gam(jlvcite,ibin)**5

      activity(jnh4hso4,ibin)= mc(jc_nh4,ibin) * ma(ja_hso4,ibin) *   &
                               gam(jnh4hso4,ibin)**2

      activity(jna2so4,ibin) = mc(jc_na,ibin)**2 * ma(ja_so4,ibin) *   &
                               gam(jna2so4,ibin)**3

      activity(jnahso4,ibin) = mc(jc_na,ibin) * ma(ja_hso4,ibin) *   &
                               gam(jnahso4,ibin)**2

      activity(jna3hso4,ibin)= mc(jc_na,ibin)**3 * ma(ja_hso4,ibin) *   &
                               ma(ja_so4,ibin) * gam(jna3hso4,ibin)**5

      activity(jhno3,ibin)   = mc(jc_h,ibin) * ma(ja_no3,ibin) *   &
                               gam(jhno3,ibin)**2

      activity(jhcl,ibin)    = mc(jc_h,ibin) * ma(ja_cl,ibin) *   &
                               gam(jhcl,ibin)**2


! sulfate-poor species
      activity(jnh4no3,ibin) = mc(jc_nh4,ibin) * ma(ja_no3,ibin) *   &
                               gam(jnh4no3,ibin)**2

      activity(jnh4cl,ibin)  = mc(jc_nh4,ibin) * ma(ja_cl,ibin) *   &
                               gam(jnh4cl,ibin)**2

      activity(jnano3,ibin)  = mc(jc_na,ibin) * ma(ja_no3,ibin) *   &
                               gam(jnano3,ibin)**2

      activity(jnacl,ibin)   = mc(jc_na,ibin) * ma(ja_cl,ibin) *   &
                               gam(jnacl,ibin)**2

      activity(jcano3,ibin)  = mc(jc_ca,ibin) * ma(ja_no3,ibin)**2 *   &
                               gam(jcano3,ibin)**3

      activity(jcacl2,ibin)  = mc(jc_ca,ibin) * ma(ja_cl,ibin)**2 *   &
                               gam(jcacl2,ibin)**3

      return
      end subroutine sulfate_rich_activities










!***********************************************************************
! performs iterations for solving equilibrium h+ ion in sulfate-rich systems
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine sulfate_rich_iterations(ibin)	! rigorous method: psc model
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer ja, jc, idum, k1, k2, k3
      real mh, mh0, mh1, mh_neutral,		   &  ! mol/kg water
           c_plus, c_minus, c_bal, q, q0, q1,   &
           charge_c, charge_a, charge_net
! function
!     real fzero



! all strong electrolytes are completely dissociated
!
!
! water molality
      mh2o        = 55.509	! 1000 g water / mw(h2o),   (mw(h2o) = 18.016)

! cation molalities (mol/kg water)
      mc(jc_ca,ibin)   = 0.0	! aqueous ca never exists in sulfate rich cases
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)

!
! anion molalities (mol/kg water)
      msulf            = 1.e-9*aer(iso4_a,jliquid,ibin)/water_a(ibin)
      ma(ja_hso4,ibin) = 0.0
      ma(ja_so4,ibin)  = 0.0
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)


      c_plus = mc(jc_nh4,ibin) + mc(jc_na,ibin) + 2.*mc(jc_ca,ibin)
      c_minus= ma(ja_no3,ibin) + ma(ja_cl,ibin)
      c_bal  = real( dble(msulf + c_minus) - dble(c_plus) )


!----------------------------------------------------------
! inital guesses

      mh_neutral = sqrt(keq_ll(3))

! lower limit
      mh0  = c_bal

      if(mh0 .le. 0.)then
        mh0 = mh_neutral
      endif

! upper limit
      mh1  = real( dble(msulf) + dble(c_bal) )

      if(mh1 .lt. mh_neutral)then
        mh = mh_neutral
        mc(jc_h,ibin) = mh
        call gamma_a(ibin)

        ph(ibin) = -alog10(mc(jc_h,ibin))

! solve for moles----------------------------------------
        charge_c = 0.0
        do jc = 1, ncation
          charge_c = charge_c + zc(jc)*mc(jc,ibin)
        enddo

        charge_a = 0.0
        do ja = 1, nanion
          if(ja .ne. ja_hso4  .and. ja .ne. ja_so4)then
            charge_a = charge_a + za(ja)*ma(ja,ibin)
          endif
        enddo

        charge_net = real( dble(charge_c) - dble(charge_a) )

        ma(ja_hso4,ibin)= max(real(dble(2.*msulf)-dble(charge_net)),0.0)
        ma(ja_so4,ibin) = max(real(dble(charge_net) - dble(msulf)), 0.0)

        return
      endif


!--------------------------------------------------------------
! narrowing the search to within a decade


      do idum = 1, 10

        mh = mh0*10.

        q = fzero(mh, ibin)

        if(q.gt.0.)then
          mh0 = mh
        else
          goto 41
        endif

      enddo

41    continue
!-----------------------------------------------------
! bisection method'
!      q0 = fzero(mh0, ibin)
!      q1 = fzero(mh1, ibin)

      k1 = 0
      k2 = 0
      k3 = 0

! iteration loop
      do k1 = 1, 20

        mh = 0.5*(mh0 + mh1)         ! bisect mh

        if(abs(mh0-mh)/mh .lt. 1.e-3)goto 20

        q  = fzero(mh, ibin)

          if(q.gt.0)then
            mh0 = mh
             q0 = q
          else
            mh1 = mh
             q1 = q
          endif
      enddo

!--------------------------------------------------------------
42    continue

! secant method
!      q0 = azero(mh0, ibin)
!      q1 = azero(mh1, ibin)

! iteration loop
      do k2 =1,5

        if(abs(mh1-mh0)/mh0.lt.1.e-3 .or.   &
           abs(q1-q0).eq.0.0) goto 20

        mh = mh1 - q1*(mh1 - mh0)/(q1 - q0)

        if(abs(mh-mh0)/mh .lt. 1.e-2)goto 20

        mh0 = mh
         q  = fzero(mh, ibin)
        q0  = q

      enddo

!--------------------------------------------------------------
43    continue

! bisection method
!
! iteration loop
      do k3 = 1,100

        mh = 0.5*(mh0 + mh1)		! bisect mh

        if(abs(mh0-mh)/mh.lt.1.e-3)goto 20

        q  = fzero(mh, ibin)

!        if(mh .lt. 1.e-7)goto 20

          if(q.gt.0)then
            mh0 = mh
          else
            mh1 = mh
          endif
      enddo


20    continue				! mh converged
!      write(6,*)'k1 k2 k3 ktot = ', k1, k2, k3, (k1+k2+k3)
      mc(jc_h,ibin) = mh

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))

      if(mactivity_coeff .eq. mpsc)then		! psc
        gam_ratio(ibin) = gam_cation(jc_nh4,ibin)/gam_cation(jc_h,ibin)
      else				! km or bromley
        gam_ratio(ibin)  = gam(jnh4hso4,ibin)**2/gam(jhhso4,ibin)**2
      endif


      return
      end subroutine sulfate_rich_iterations









      real function fzero(mh, ibin)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer ibin
      real mh




      mc(jc_h,ibin) = mh

      if(mactivity_coeff .eq. mpsc)then

        call psc_sulfate_rich(ibin)
        fzero  = keq_ll(1)*gam_anion(ja_hso4,ibin)*ma(ja_hso4,ibin) -   &
                 gam_cation(jc_h,ibin)*mc(jc_h,ibin)*   &
                 gam_anion(ja_so4,ibin)*ma(ja_so4,ibin)

      elseif(mactivity_coeff .eq. mkm)then

        call km_sulfate_rich(ibin)
        fzero  = keq_ll(1)*ma(ja_hso4,ibin) -   &
                 gam(jh2so4,ibin)**3/gam(jhhso4,ibin)**2 *   &
                 mc(jc_h,ibin)*ma(ja_so4,ibin)

      elseif(mactivity_coeff .eq. mbrom)then

        call brom_sulfate_rich(ibin)
        fzero  = keq_ll(1)*ma(ja_hso4,ibin) -   &
                 gam(jh2so4,ibin)**3/gam(jhhso4,ibin)**2 *   &
                 mc(jc_h,ibin)*ma(ja_so4,ibin)

      endif

      return
      end function fzero







      subroutine gamma_a(ibin)

!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin




      if(mactivity_coeff .eq. mpsc)then
        call psc_sulfate_rich(ibin)
      elseif(mactivity_coeff .eq. mkm)then
        call km_sulfate_rich(ibin)
      elseif(mactivity_coeff .eq. mbrom)then
        call brom_sulfate_rich(ibin)
      endif

      return
      end subroutine gamma_a





!***********************************************************************
! pitzer-simonson-clegg (psc) model
!
! author: rahul a. zaveri
! update: jan 2005
! references: clegg et al., 1992, 1998
!-----------------------------------------------------------------------
      subroutine psc_sulfate_rich(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jx, jm, ja, jc, izi, izj
      real mtot, mion, mcation, manion, ix_c, ix_a,   &
           im_c, im_a, charge_net, charge_c, charge_a,   &
           sum_xczc, sum_xaza, dum
! functions
!     real fn_thetahoe, fn_dthetahoe, fn, fm, fx



! solve for moles----------------------------------------
      charge_c = 0.0
      do jc = 1, ncation
        charge_c = charge_c + zc(jc)*mc(jc,ibin)
      enddo

      charge_a = 0.0
      do ja = 1, nanion
        if(ja .ne. ja_hso4  .and. ja .ne. ja_so4)then
          charge_a = charge_a + za(ja)*ma(ja,ibin)
        endif
      enddo

      charge_net = real( dble(charge_c) - dble(charge_a) )

      ma(ja_hso4,ibin)= max(real(dble(2.*msulf)-dble(charge_net)),0.0)
      ma(ja_so4,ibin) = max(real(dble(charge_net) - dble(msulf)), 0.0)
!
!
! calculate mol fractions -------------------------------

      mcation = 0.0
      do jc = 1, ncation
        mcation = mcation + mc(jc,ibin)
      enddo

      manion = 0.0
      do ja = 1, nanion
        manion = manion + ma(ja,ibin)
      enddo

      mion = mcation + manion

      mtot = mh2o + mion


! compute molality-scale ionic strength
      im_c = 0.0
      do jc = 1, ncation
        im_c = im_c + 0.5*mc(jc,ibin)*zc(jc)**2
      enddo

      im_a = 0.0
      do ja = 1, nanion
        im_a = im_a + 0.5*ma(ja,ibin)*za(ja)**2
      enddo

      im(ibin) = im_c + im_a		! molality ionic strength



      do jc = 1, ncation
        xc(jc) = mc(jc,ibin)/mtot
      enddo

      do ja = 1, nanion
        xa(ja) = ma(ja,ibin)/mtot
      enddo

      xh2o = mh2o/mtot

!
! calculate variables for mol-fraction------------------
! scale activity coefficient model

      sum_xczc = 0.0
      ix_c = 0.0
      do jc = 1, ncation
        sum_xczc = sum_xczc + xc(jc)*zc(jc)
        ix_c = ix_c + xc(jc)*zc(jc)**2
      enddo

      sum_xaza = 0.0
      ix_a = 0.0
      do ja = 1, nanion
        sum_xaza = sum_xaza + xa(ja)*za(ja)
        ix_a = ix_a + xa(ja)*za(ja)**2
      enddo

      ix = 0.5*(ix_c + ix_a)		! mole fraction ionic strength
      ff = 2./(sum_xczc + sum_xaza)
!
! equivalent cation fractions
      do jc = 1, ncation
      ec(jc)=  xc(jc)*zc(jc)/sum_xczc
      enddo

! differentials
      do jm = 1, ncation
      	do jc = 1, ncation
          if(jm.eq.jc)then
      	    emc(jm,jc) = (zc(jm)/sum_xczc) * (1 - ec(jm))
      	  else
            emc(jm,jc) = -zc(jm)*ec(jc)/sum_xczc
          endif
     	enddo
      enddo

! equivalent anion fractions
      do ja = 1, nanion
      ea(ja)=  xa(ja)*za(ja)/sum_xaza
      enddo

! differentials
      do jx = 1, nanion
      	do ja = 1, nanion
          if(jx.eq.ja)then
      	    exa(jx,ja) = (za(jx)/sum_xaza) * (1 - ea(jx))
      	  else
            exa(jx,ja) = -za(jx)*ea(ja)/sum_xaza
          endif
     	enddo
      enddo

      do izi = 1, 2
      do izj = 1, 2

        thetahoe(izi,izj) = fn_thetahoe(izi,izj)
        dthetahoe(izi,izj) = fn_dthetahoe(izi,izj)

      enddo
      enddo


! mole fraction-scale activity coefficients
        fh2o   = fn(1) ! neutral species (h2o)

      do jm = 1, ncation_clegg
        fc(jm,ibin) = fm(jm)	! cations
      enddo

      do jx = 1, nanion_clegg
        fa(jx,ibin) = fx(jx)	! anions
      enddo
!
!
! molality-scale activity coefficients
10    dum   = (1. + mion/mh2o)

      do jc = 1, ncation_clegg
       gam_cation(jc,ibin) = fc(jc,ibin)/dum
      enddo

      do ja = 1, nanion_clegg
       gam_anion(ja,ibin)  = fa(ja,ibin)/dum
      enddo


! compute mean electrolyte activity coefficients
      gam(jnh4so4,ibin) = (gam_cation(jc_nh4,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)

      gam(jlvcite,ibin) = (gam_cation(jc_nh4,ibin)**3 *   &
                           gam_anion(ja_hso4,ibin) *   &
                           gam_anion(ja_so4,ibin))**(1./5.)

      gam(jnh4hso4,ibin)= (gam_cation(jc_nh4,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)

      gam(jna2so4,ibin) = (gam_cation(jc_na,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)

      gam(jnahso4,ibin) = (gam_cation(jc_na,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)

      gam(jna3hso4,ibin) = (gam_cation(jc_na,ibin)**3 *   &
                           gam_anion(ja_hso4,ibin) *   &
                           gam_anion(ja_so4,ibin))**(1./5.)

      gam(jh2so4,ibin)  = (gam_cation(jc_h,ibin)**2 *   &
                           gam_anion(ja_so4,ibin))**(1./3.)

      gam(jhhso4,ibin)  = (gam_cation(jc_h,ibin) *   &
                           gam_anion(ja_hso4,ibin))**(1./2.)

      gam(jhno3,ibin)   = (gam_cation(jc_h,ibin) *   &
                           gam_anion(ja_no3,ibin))**0.5

      gam(jhcl,ibin)    = (gam_cation(jc_h,ibin) *   &
                           gam_anion(ja_cl,ibin))**0.5



      gam_ratio(ibin)   = gam_cation(jc_nh4,ibin)/gam_cation(jc_h,ibin)



      gam(jnh4no3,ibin) = (gam_cation(jc_nh4,ibin)*   &
                           gam_anion(ja_no3,ibin))**0.5
      gam(jnh4cl,ibin)  = (gam_cation(jc_nh4,ibin)*   &
                           gam_anion(ja_cl,ibin))**0.5
      gam(jnacl,ibin)   = (gam_cation(jc_na,ibin)*   &
                           gam_anion(ja_cl,ibin))**0.5
      gam(jnano3,ibin)  = (gam_cation(jc_na,ibin)*   &
                           gam_anion(ja_no3,ibin))**0.5
      gam(jcacl2,ibin)  = (gam_cation(jc_ca,ibin)*   &
                           gam_anion(ja_cl,ibin)**2)**(1./3.)
      gam(jcano3,ibin)  = (gam_cation(jc_ca,ibin)*   &
                           gam_anion(ja_no3,ibin)**2)**(1./3.)


      return
      end subroutine psc_sulfate_rich






!***********************************************************************
! kusik and meissner mixing rule for sulfate-rich systems
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine km_sulfate_rich(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jc, ja
      real im_c, im_a, charge_a, charge_c, charge_net
! function
!     real fnlog_gam0


! solve for moles----------------------------------------
      charge_c = 0.0
      do jc = 1, ncation
        charge_c = charge_c + zc(jc)*mc(jc,ibin)
      enddo

      charge_a = 0.0
      do ja = 1, nanion
        if(ja .ne. ja_hso4  .and. ja .ne. ja_so4)then
          charge_a = charge_a + za(ja)*ma(ja,ibin)
        endif
      enddo

      charge_net = real( dble(charge_c) - dble(charge_a) )

      ma(ja_hso4,ibin)= max(real(dble(2.*msulf)-dble(charge_net)),0.0)
      ma(ja_so4,ibin) = max(real(dble(charge_net) - dble(msulf)), 0.0)



! compute molality-scale ionic strength
      im_c = 0.0
      do jc = 1, ncation
        im_c = im_c + 0.5*mc(jc,ibin)*zc(jc)**2
      enddo

      im_a = 0.0
      do ja = 1, nanion
        im_a = im_a + 0.5*ma(ja,ibin)*za(ja)**2
      enddo

      im(ibin) = im_c + im_a		! molality ionic strength

      log_gam0(jnh4so4) = fnlog_gam0(jnh4so4,im(ibin))
      log_gam0(jnh4no3) = fnlog_gam0(jnh4no3,im(ibin))
      log_gam0(jnh4cl)  = fnlog_gam0(jnh4cl,im(ibin))
      log_gam0(jnacl)   = fnlog_gam0(jnacl,im(ibin))
      log_gam0(jnano3)  = fnlog_gam0(jnano3,im(ibin))
      log_gam0(jna2so4) = fnlog_gam0(jna2so4,im(ibin))
      log_gam0(jcacl2)  = fnlog_gam0(jcacl2,im(ibin))
      log_gam0(jcano3)  = fnlog_gam0(jcano3,im(ibin))
      log_gam0(jhno3)   = fnlog_gam0(jhno3,im(ibin))
      log_gam0(jhcl)    = fnlog_gam0(jhcl,im(ibin))
      log_gam0(jh2so4)  = fnlog_gam0(jh2so4,im(ibin))
      log_gam0(jhhso4)  = fnlog_gam0(jhhso4,im(ibin))
      log_gam0(jnh4hso4)= fnlog_gam0(jnh4hso4,im(ibin))
!      log_gam0(jlvcite) = fnlog_gam0(jlvcite,im(ibin))
      log_gam0(jnahso4) = fnlog_gam0(jnahso4,im(ibin))
!      log_gam0(jna3hso4)= fnlog_gam0(jna3hso4,im(ibin))


! km
! (nh4)2so4
      log_gam(jnh4so4) = (0.666666667/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnh4no3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnh4cl)    +   &
                          ma(ja_hso4,ibin)*log_gam0(jnh4hso4)  +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jnh4so4) ) +   &
                         (0.333333333/im(ibin))*   &
                    ( 2.25*mc(jc_nh4,ibin)*log_gam0(jnh4so4)   +   &
                      2.25*mc(jc_h,ibin)  *log_gam0(jh2so4)    +   &
                      2.25*mc(jc_na,ibin) *log_gam0(jna2so4) )
      gam(jnh4so4,ibin) = 10.**log_gam(jnh4so4)


! na2so4
      log_gam(jna2so4) = (0.666666667/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnano3)    +   &
                           ma(ja_cl,ibin) *log_gam0(jnacl)     +   &
                          ma(ja_hso4,ibin)*log_gam0(jnahso4)  +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jna2so4) ) +   &
                         (0.333333333/im(ibin))*   &
                    ( 2.25*mc(jc_nh4,ibin)*log_gam0(jnh4so4)   +   &
                      2.25*mc(jc_h,ibin)  *log_gam0(jh2so4)    +   &
                      2.25*mc(jc_na,ibin) *log_gam0(jna2so4) )
      gam(jna2so4,ibin) = 10.**log_gam(jna2so4)


! hno3
      log_gam(jhno3)   = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4no3)   +   &
                           mc(jc_na,ibin) *log_gam0(jnano3)    +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcano3)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhno3)  )
      gam(jhno3,ibin)  = 10.**log_gam(jhno3)


! hcl
      log_gam(jhcl)    = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4cl)    +   &
                           mc(jc_na,ibin) *log_gam0(jnacl)     +   &
                      2.25*mc(jc_ca,ibin) *log_gam0(jcacl2)    +   &
                           mc(jc_h,ibin)  *log_gam0(jhcl)   )
      gam(jhcl,ibin)   = 10.**log_gam(jhcl)


! h2so4
      log_gam(jh2so4)  = (0.666666667/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.333333333/im(ibin))*   &
                    ( 2.25*mc(jc_nh4,ibin)*log_gam0(jnh4so4)   +   &
                      2.25*mc(jc_h,ibin)  *log_gam0(jh2so4)    +   &
                      2.25*mc(jc_na,ibin) *log_gam0(jna2so4) )
      gam(jh2so4,ibin) = 10.**log_gam(jh2so4)


! hhso4
      log_gam(jhhso4)  = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin) *log_gam0(jhno3)    +   &
                      2.25*ma(ja_so4,ibin) *log_gam0(jh2so4)   +   &
                           ma(ja_hso4,ibin)*log_gam0(jhhso4)   +   &
                           ma(ja_cl,ibin)  *log_gam0(jhcl)   ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4hso4)  +   &
                           mc(jc_h,ibin)  *log_gam0(jhhso4)    +   &
                           mc(jc_na,ibin) *log_gam0(jnahso4) )
      gam(jhhso4,ibin) = 10.**log_gam(jhhso4)


! nh4hso4
      gam(jnh4hso4,ibin)= (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnh4no3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnh4cl)    +   &
                          ma(ja_hso4,ibin)*log_gam0(jnh4hso4)  +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jnh4so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4hso4)  +   &
                           mc(jc_h,ibin)  *log_gam0(jhhso4)    +   &
                           mc(jc_na,ibin) *log_gam0(jnahso4) )
      gam(jnh4hso4,ibin) = 10.**log_gam(jnh4hso4)


! nahso4
      gam(jnahso4,ibin) = (0.5/im(ibin))*   &
                         ( ma(ja_no3,ibin)*log_gam0(jnano3)   +   &
                           ma(ja_cl,ibin) *log_gam0(jnacl)    +   &
                          ma(ja_hso4,ibin)*log_gam0(jnahso4)  +   &
                      2.25*ma(ja_so4,ibin)*log_gam0(jna2so4) ) +   &
                         (0.5/im(ibin))*   &
                         ( mc(jc_nh4,ibin)*log_gam0(jnh4hso4)  +   &
                           mc(jc_h,ibin)  *log_gam0(jhhso4)    +   &
                           mc(jc_na,ibin) *log_gam0(jnahso4) )
      gam(jnahso4,ibin) = 10.**log_gam(jnahso4)


! derived quantities common to both km and bromley...


      gam(jlvcite,ibin) = (gam(jnh4so4,ibin)**3 *   &
                           gam(jnh4hso4,ibin)**2 )**0.2

      gam(jna3hso4,ibin)= (gam(jna2so4,ibin)**3 *   &
                           gam(jnahso4,ibin)**2 )**0.2

      gam_ratio(ibin) = gam(jnh4hso4,ibin)**2/gam(jhhso4,ibin)**2

      return
      end subroutine km_sulfate_rich







!***********************************************************************
! bromley mixing rule for sulfate-rich systems
!
! author: rahul a. zaveri
! update: jan 2005
! references: clegg et al., 1992, 1998
!-----------------------------------------------------------------------
      subroutine brom_sulfate_rich(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jc, ja
      real im_c, im_a, x, aterm, agam, sqim,   &
           charge_a, charge_c, charge_net,   &
           z_nh4, z_na, z_ca, z_h,   &
           z_so4, z_hso4, z_no3, z_cl,   &
           y_so4_nh4, y_so4_na, y_so4_h,   &
           y_hso4_nh4, y_hso4_na, y_hso4_h,   &
           y_no3_nh4, y_no3_na, y_no3_ca, y_no3_h,   &
           y_cl_nh4, y_cl_na, y_cl_ca, y_cl_h,   &
           x_nh4_so4, x_na_so4, x_h_so4,   &
           x_nh4_hso4, x_na_hso4, x_h_hso4,   &
           x_nh4_no3, x_na_no3, x_ca_no3, x_h_no3,   &
           x_nh4_cl, x_na_cl, x_ca_cl, x_h_cl,   &
           f_nh4, f_na, f_ca, f_h,   &
           f_no3, f_cl, f_so4, f_hso4
! function
!     real fnlog_gam0



! solve for moles----------------------------------------
      charge_c = 0.0
      do jc = 1, ncation
        charge_c = charge_c + zc(jc)*mc(jc,ibin)
      enddo

      charge_a = 0.0
      do ja = 1, nanion
        if(ja .ne. ja_hso4  .and. ja .ne. ja_so4)then
          charge_a = charge_a + za(ja)*ma(ja,ibin)
        endif
      enddo

      charge_net = real( dble(charge_c) - dble(charge_a) )

      ma(ja_hso4,ibin)= max(real(dble(2.*msulf)-dble(charge_net)),0.0)
      ma(ja_so4,ibin) = max(real(dble(charge_net) - dble(msulf)), 0.0)



! compute molality-scale ionic strength
      im_c = 0.0
      do jc = 1, ncation
        im_c = im_c + 0.5*mc(jc,ibin)*zc(jc)**2
      enddo

      im_a = 0.0
      do ja = 1, nanion
        im_a = im_a + 0.5*ma(ja,ibin)*za(ja)**2
      enddo

      im(ibin) = im_c + im_a		! molality ionic strength

      log_gam0(jnh4so4) = fnlog_gam0(jnh4so4,im(ibin))
      log_gam0(jnh4no3) = fnlog_gam0(jnh4no3,im(ibin))
      log_gam0(jnh4cl)  = fnlog_gam0(jnh4cl,im(ibin))
      log_gam0(jnacl)   = fnlog_gam0(jnacl,im(ibin))
      log_gam0(jnano3)  = fnlog_gam0(jnano3,im(ibin))
      log_gam0(jna2so4) = fnlog_gam0(jna2so4,im(ibin))
      log_gam0(jcacl2)  = fnlog_gam0(jcacl2,im(ibin))
      log_gam0(jcano3)  = fnlog_gam0(jcano3,im(ibin))
      log_gam0(jhno3)   = fnlog_gam0(jhno3,im(ibin))
      log_gam0(jhcl)    = fnlog_gam0(jhcl,im(ibin))
      log_gam0(jh2so4)  = fnlog_gam0(jh2so4,im(ibin))
      log_gam0(jhhso4)  = fnlog_gam0(jhhso4,im(ibin))
      log_gam0(jnh4hso4)= fnlog_gam0(jnh4hso4,im(ibin))
!      log_gam0(jlvcite) = fnlog_gam0(jlvcite,im(ibin))
      log_gam0(jnahso4) = fnlog_gam0(jnahso4,im(ibin))
!      log_gam0(jna3hso4)= fnlog_gam0(jna3hso4,im(ibin))
!-------------------------------------------------------------------

      agam = 0.511 ! (kg/mol)^0.5
      sqim = sqrt(im(ibin))

      aterm = agam*sqim/(1. + sqim)

      z_nh4 = 1.0
      z_na  = 1.0
      z_ca  = 2.0
      z_h   = 1.0

      z_so4 = 2.0
      z_hso4= 1.0
      z_no3 = 1.0
      z_cl  = 1.0

! y
      y_so4_nh4 = ((z_nh4+z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)
      y_so4_na  = ((z_na +z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)
      y_so4_h   = ((z_h  +z_so4)/2.0)**2.0*ma(ja_so4,ibin)/im(ibin)

      y_hso4_nh4= ((z_nh4+z_hso4)/2.0)**2.0*ma(ja_hso4,ibin)/im(ibin)
      y_hso4_na = ((z_na +z_hso4)/2.0)**2.0*ma(ja_hso4,ibin)/im(ibin)
      y_hso4_h  = ((z_h  +z_hso4)/2.0)**2.0*ma(ja_hso4,ibin)/im(ibin)

      y_no3_nh4 = ((z_nh4+z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_na  = ((z_na +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_ca  = ((z_ca +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)
      y_no3_h   = ((z_h  +z_no3)/2.0)**2.0*ma(ja_no3,ibin)/im(ibin)

      y_cl_nh4  = ((z_nh4+z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_na   = ((z_na +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_ca   = ((z_ca +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)
      y_cl_h    = ((z_h  +z_cl)/2.0)**2.0*ma(ja_cl,ibin)/im(ibin)

! x
      x_nh4_so4 = ((z_nh4+z_so4)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_so4  = ((z_na +z_so4)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_h_so4   = ((z_h  +z_so4)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)

      x_nh4_hso4= ((z_nh4+z_hso4)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_hso4 = ((z_na +z_hso4)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_h_hso4  = ((z_h  +z_hso4)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)

      x_nh4_no3 = ((z_nh4+z_no3)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_no3  = ((z_na +z_no3)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_ca_no3  = ((z_ca +z_no3)/2.0)**2.0*mc(jc_ca,ibin)/im(ibin)
      x_h_no3   = ((z_h  +z_no3)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)

      x_nh4_cl  = ((z_nh4+z_cl)/2.0)**2.0*mc(jc_nh4,ibin)/im(ibin)
      x_na_cl   = ((z_na +z_cl)/2.0)**2.0*mc(jc_na,ibin)/im(ibin)
      x_ca_cl   = ((z_ca +z_cl)/2.0)**2.0*mc(jc_ca,ibin)/im(ibin)
      x_h_cl    = ((z_h  +z_cl)/2.0)**2.0*mc(jc_h,ibin)/im(ibin)



      f_nh4 = y_so4_nh4 *log_gam0(jnh4so4) +   &
              y_hso4_nh4*log_gam0(jnh4hso4)+   &
              y_no3_nh4*log_gam0(jnh4no3)  +   &
              y_cl_nh4 *log_gam0(jnh4cl)   +   &
       aterm*(z_nh4*z_so4 *y_so4_nh4  +   &
              z_nh4*z_hso4*y_hso4_nh4 +   &
              z_nh4*z_no3*y_no3_nh4   +   &
              z_nh4*z_cl *y_cl_nh4)

      f_na  = y_so4_na *log_gam0(jna2so4) +   &
              y_hso4_na*log_gam0(jnahso4) +   &
              y_no3_na*log_gam0(jnano3)   +   &
              y_cl_na *log_gam0(jnacl)    +   &
       aterm*(z_na*z_so4 *y_so4_na  +   &
              z_na*z_hso4*y_hso4_na +   &
              z_na*z_no3*y_no3_na   +   &
              z_na*z_cl *y_cl_na)

      f_h   = y_so4_h *log_gam0(jh2so4) +   &
              y_hso4_h*log_gam0(jhhso4) +   &
              y_no3_h *log_gam0(jhno3)  +   &
              y_cl_h  *log_gam0(jhcl)   +   &
       aterm*(z_h*z_so4* y_so4_h  +   &
              z_h*z_hso4*y_hso4_h +   &
              z_h*z_no3* y_no3_h  +   &
              z_h*z_cl * y_cl_h)


      f_no3 = x_nh4_no3*log_gam0(jnh4no3) +   &
              x_na_no3 *log_gam0(jnano3)  +   &
              x_ca_no3 *log_gam0(jcano3)  +   &
              x_h_no3  *log_gam0(jhno3)   +   &
       aterm*(z_nh4*z_no3*x_nh4_no3 +   &
              z_na *z_no3*x_na_no3  +   &
              z_ca *z_no3*x_ca_no3  +   &
              z_h  *z_no3*x_h_no3)

      f_cl  = x_nh4_cl*log_gam0(jnh4cl) +   &
              x_na_cl *log_gam0(jnacl)  +   &
              x_ca_cl *log_gam0(jcacl2) +   &
              x_h_cl  *log_gam0(jhcl)   +   &
       aterm*(z_nh4*z_cl*x_nh4_cl +   &
              z_na *z_cl*x_na_cl  +   &
              z_ca *z_cl*x_ca_cl  +   &
              z_h  *z_cl*x_h_cl)

      f_so4 = x_nh4_so4*log_gam0(jnh4so4) +   &
              x_na_so4 *log_gam0(jna2so4) +   &
              x_h_so4  *log_gam0(jh2so4)  +   &
       aterm*(z_nh4*z_so4*x_nh4_so4 +   &
              z_na *z_so4*x_na_so4  +   &
              z_h  *z_so4*x_h_so4)

      f_hso4= x_nh4_hso4*log_gam0(jnh4hso4) +   &
              x_na_hso4 *log_gam0(jnahso4) +   &
              x_h_hso4  *log_gam0(jhhso4)  +   &
       aterm*(z_nh4*z_hso4*x_nh4_hso4 +   &
              z_na *z_hso4*x_na_hso4  +   &
              z_h  *z_hso4*x_h_hso4)



! (nh4)2so4
      log_gam(jnh4so4) = -z_nh4*z_so4*aterm +   &
            z_nh4*z_so4/(z_nh4+z_so4)*(f_nh4/z_nh4 + f_so4/z_so4)
      gam(jnh4so4,ibin) = 10.**log_gam(jnh4so4)

! nh4hso4
      log_gam(jnh4hso4)= -z_nh4*z_hso4*aterm +   &
            z_nh4*z_hso4/(z_nh4+z_hso4)*(f_nh4/z_nh4 + f_hso4/z_hso4)
      gam(jnh4hso4,ibin)  = 10.**log_gam(jnh4hso4)

! na2so4
      log_gam(jna2so4) = -z_na*z_so4*aterm +   &
            z_na*z_so4/(z_na+z_so4)*(f_na/z_na + f_so4/z_so4)
      gam(jna2so4,ibin) = 10.**log_gam(jna2so4)

! nahso4
      log_gam(jnahso4) = -z_na*z_hso4*aterm +   &
            z_na*z_hso4/(z_na+z_hso4)*(f_na/z_na + f_hso4/z_hso4)
      gam(jnahso4,ibin) = 10.**log_gam(jnahso4)

! h2so4
      log_gam(jh2so4)  = -z_h*z_so4*aterm +   &
            z_h*z_so4/(z_h+z_so4)*(f_h/z_h + f_so4/z_so4)
      gam(jh2so4,ibin) = 10.**log_gam(jh2so4)


! hhso4
      log_gam(jhhso4)  = -z_h*z_hso4*aterm +   &
            z_h*z_hso4/(z_h+z_hso4)*(f_h/z_h + f_hso4/z_hso4)
      gam(jhhso4,ibin) = 10.**log_gam(jhhso4)


! hno3
      log_gam(jhno3)   = -z_h*z_no3*aterm +   &
            z_h*z_no3/(z_h+z_no3)*(f_h/z_h + f_no3/z_no3)
      gam(jhno3,ibin)  = 10.**log_gam(jhno3)


! hcl
      log_gam(jhcl)    = -z_h*z_cl*aterm +   &
            z_h*z_cl/(z_h+z_cl)*(f_h/z_h + f_cl/z_cl)
      gam(jhcl,ibin)   = 10.**log_gam(jhcl)

!-------------------------------------------------------------------


! derived quantities common to both km and bromley...


      gam(jlvcite,ibin) = (gam(jnh4so4,ibin)**3 *   &
                           gam(jnh4hso4,ibin)**2 )**0.2

      gam(jna3hso4,ibin)= (gam(jna2so4,ibin)**3 *   &
                           gam(jnahso4,ibin)**2 )**0.2

      gam_ratio(ibin) = gam(jnh4hso4,ibin)**2/gam(jhhso4,ibin)**2

      return
      end subroutine brom_sulfate_rich










!***********************************************************************
! multicomponent taylor expansion method (mtem)
!
! author: rahul a. zaveri
! update: jan 2005
! reference: zaveri, r.a., r.c. easter, and a.s. wexler,
! a new method for multicomponent activity coefficients of electrolytes
! in aqueous atmospheric aerosols, j. geophys. res., 2005.
!-----------------------------------------------------------------------
      subroutine mtem_sulfate_rich(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jp, ja
      real xmol(nelectrolyte), sum_elec, dumk,   &
           c_bal, a, b, c
! function
!     real quadratic


      jp = jliquid

      sum_elec = 3.*electrolyte(jh2so4,jp,ibin)    +   &
                 2.*electrolyte(jnh4hso4,jp,ibin)  +   &
                 5.*electrolyte(jlvcite,jp,ibin)   +   &
                 3.*electrolyte(jnh4so4,jp,ibin)   +   &
                 2.*electrolyte(jnahso4,jp,ibin)   +   &
                 5.*electrolyte(jna3hso4,jp,ibin)  +   &
                 3.*electrolyte(jna2so4,jp,ibin)   +   &
                 2.*electrolyte(jhno3,jp,ibin)     +   &
                 2.*electrolyte(jhcl,jp,ibin)


      xmol(jh2so4)  = 3.*electrolyte(jh2so4,jp,ibin)/sum_elec
      xmol(jnh4hso4)= 2.*electrolyte(jnh4hso4,jp,ibin)/sum_elec
      xmol(jlvcite) = 5.*electrolyte(jlvcite,jp,ibin)/sum_elec
      xmol(jnh4so4) = 3.*electrolyte(jnh4so4,jp,ibin)/sum_elec
      xmol(jnahso4) = 2.*electrolyte(jnahso4,jp,ibin)/sum_elec
      xmol(jna3hso4)= 5.*electrolyte(jna3hso4,jp,ibin)/sum_elec
      xmol(jna2so4) = 3.*electrolyte(jna2so4,jp,ibin)/sum_elec
      xmol(jhno3)   = 2.*electrolyte(jhno3,jp,ibin)/sum_elec
      xmol(jhcl)    = 2.*electrolyte(jhcl,jp,ibin)/sum_elec


! 2h.so4
      ja = jh2so4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! h.hso4
      ja = jhhso4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! nh4hso4
      ja = jnh4hso4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! letovicite
      ja = jlvcite
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! (nh4)2so4
      ja = jnh4so4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! nahso4
      ja = jnahso4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! na3h(so4)2
      ja = jna3hso4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! na2so4
      ja = jna2so4
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! hno3
      ja = jhno3
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


! hcl
      ja = jhcl
      log_gam(ja) = xmol(jh2so4)  *log_gamz(ja,jh2so4)  +   &
                    xmol(jnh4hso4)*log_gamz(ja,jnh4hso4)+   &
                    xmol(jlvcite) *log_gamz(ja,jlvcite) +   &
                    xmol(jnh4so4) *log_gamz(ja,jnh4so4) +   &
                    xmol(jnahso4) *log_gamz(ja,jnahso4) +   &
                    xmol(jna3hso4)*log_gamz(ja,jna3hso4)+   &
                    xmol(jna2so4) *log_gamz(ja,jna2so4) +   &
                    xmol(jhno3)   *log_gamz(ja,jhno3)   +   &
                    xmol(jhcl)    *log_gamz(ja,jhcl)
      gam(ja,ibin) = 10.**log_gam(ja)


      gam(jnh4no3,ibin) = 1.0
      gam(jnh4cl,ibin)  = 1.0
      gam(jnano3,ibin)  = 1.0
      gam(jnacl,ibin)   = 1.0
      gam(jcano3,ibin)  = 1.0
      gam(jcacl2,ibin)  = 1.0


! compute equilibrium ph
! cation molalities (mol/kg water)
      mc(jc_ca,ibin)   = 0.0	! aqueous ca never exists in sulfate rich cases
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)

! anion molalities (mol/kg water)
      msulf            = 1.e-9*aer(iso4_a,jliquid,ibin)/water_a(ibin)
      ma(ja_hso4,ibin) = 0.0
      ma(ja_so4,ibin)  = 0.0
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

      gam_ratio(ibin)  = gam(jnh4hso4,ibin)**2/gam(jhhso4,ibin)**2
      dumk = keq_ll(1)*gam(jhhso4,ibin)**2/gam(jh2so4,ibin)**3

      c_bal =  mc(jc_nh4,ibin) + mc(jc_na,ibin)   &
             - ma(ja_no3,ibin) - ma(ja_cl,ibin) - msulf

      a = 1.0
      b = dumk + c_bal
      c = dumk*(c_bal - msulf)
      mc(jc_h,ibin) = quadratic(a,b,c)

      mc(jc_h,ibin) = max(sqrt(keq_ll(3)), mc(jc_h,ibin))

      ph(ibin) = -alog10(mc(jc_h,ibin))
      ph_est(ibin) = -alog10(mc(jc_h,ibin))


      ma(ja_so4,ibin) = msulf*dumk/(mc(jc_h,ibin) + dumk)
      ma(ja_hso4,ibin)= real( dble(msulf) - dble(ma(ja_so4,ibin)) )



      return
      end subroutine mtem_sulfate_rich










!***********************************************************************
! computes mtem ternary parameters only once per transport time-step
! for a given ah2o (= rh)
!
! author: rahul a. zaveri
! update: jan 2005
! reference: zaveri, r.a., r.c. easter, and a.s. wexler,
! a new method for multicomponent activity coefficients of electrolytes
! in aqueous atmospheric aerosols, j. geophys. res., 2005.
!-----------------------------------------------------------------------
      subroutine mtem_compute_log_gamz
!     implicit none
!     include 'mosaic.h'
! local variables
      integer ja
! functions
!     real fnlog_gamz, bin_molality


! sulfate-poor species
      ja = jhno3
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)


      ja = jhcl
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)


      ja = jnh4so4
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)


      ja = jnh4no3
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)

      gam_nh4no3_0 = 10.**log_gamz(ja, jnh4no3)
      keq_nh4no3_0 = (bin_molality(ja,1)*gam_nh4no3_0)**2 *keq_ll(3)/	   &  ! = [nh3]0s * [hno3]0s
                     (keq_ll(2)*keq_gl(2)*keq_gl(3))


      ja = jnh4cl
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)

      gam_nh4cl_0 = 10.**log_gamz(ja, jnh4cl)
      keq_nh4cl_0 = (bin_molality(ja,1)*gam_nh4cl_0)**2 *keq_ll(3)/	   &  ! = [nh3]0s * [hcl]0s
                    (keq_ll(2)*keq_gl(2)*keq_gl(4))


      ja = jna2so4
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)


      ja = jnano3
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jnacl
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jcano3
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jcacl2
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnh4no3) = fnlog_gamz(ja,jnh4no3)
      log_gamz(ja,jnh4cl)  = fnlog_gamz(ja,jnh4cl)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jnano3)  = fnlog_gamz(ja,jnano3)
      log_gamz(ja,jnacl)   = fnlog_gamz(ja,jnacl)
      log_gamz(ja,jcano3)  = fnlog_gamz(ja,jcano3)
      log_gamz(ja,jcacl2)  = fnlog_gamz(ja,jcacl2)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


! sulfate-rich species
      ja = jh2so4
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jhhso4
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jnh4hso4
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jlvcite
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jnahso4
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)


      ja = jna3hso4
      log_gamz(ja,jh2so4)  = fnlog_gamz(ja,jh2so4)
      log_gamz(ja,jnh4hso4)= fnlog_gamz(ja,jnh4hso4)
      log_gamz(ja,jlvcite) = fnlog_gamz(ja,jlvcite)
      log_gamz(ja,jnh4so4) = fnlog_gamz(ja,jnh4so4)
      log_gamz(ja,jnahso4) = fnlog_gamz(ja,jnahso4)
      log_gamz(ja,jna3hso4)= fnlog_gamz(ja,jna3hso4)
      log_gamz(ja,jna2so4) = fnlog_gamz(ja,jna2so4)
      log_gamz(ja,jhno3)   = fnlog_gamz(ja,jhno3)
      log_gamz(ja,jhcl)    = fnlog_gamz(ja,jhcl)

      return
      end subroutine mtem_compute_log_gamz




























!***********************************************************************
! computes sulfate ratio
!
! author: rahul a. zaveri
! update: dec 1999
!-----------------------------------------------------------------------
      subroutine calculate_xt(ibin,jp,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt


      if(aer(iso4_a,jp,ibin).gt.0.0)then
        xt   = ( aer(inh4_a,jp,ibin) +   &
                 aer(ina_a,jp,ibin)  +   &
              2.*aer(ica_a,jp,ibin) )/   &
                 aer(iso4_a,jp,ibin)
      else
        xt   = -1.0
      endif


      return
      end subroutine calculate_xt





!***********************************************************************
! computes ions from electrolytes
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine electrolytes_to_ions(jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
! local variables
      real thesum


      aer(iso4_a,jp,ibin) = electrolyte(jcaso4,jp,ibin)  +   &
                            electrolyte(jna2so4,jp,ibin) +   &
                         2.*electrolyte(jna3hso4,jp,ibin)+   &
                            electrolyte(jnahso4,jp,ibin) +   &
                            electrolyte(jnh4so4,jp,ibin) +   &
                         2.*electrolyte(jlvcite,jp,ibin) +   &
                            electrolyte(jnh4hso4,jp,ibin)+   &
                            electrolyte(jh2so4,jp,ibin)

      aer(ino3_a,jp,ibin) = electrolyte(jnano3,jp,ibin)  +   &
                         2.*electrolyte(jcano3,jp,ibin)  +   &
                            electrolyte(jnh4no3,jp,ibin) +   &
                            electrolyte(jhno3,jp,ibin)

      aer(icl_a,jp,ibin)  = electrolyte(jnacl,jp,ibin)   +   &
                         2.*electrolyte(jcacl2,jp,ibin)  +   &
                            electrolyte(jnh4cl,jp,ibin)  +   &
                            electrolyte(jhcl,jp,ibin)

      aer(ico3_a,jp,ibin) = electrolyte(jcaco3,jp,ibin)

      aer(ica_a,jp,ibin)  = electrolyte(jcaso4,jp,ibin)  +   &
                            electrolyte(jcano3,jp,ibin)  +   &
                            electrolyte(jcacl2,jp,ibin)  +   &
                            electrolyte(jcaco3,jp,ibin)

      aer(ina_a,jp,ibin)  = electrolyte(jnano3,jp,ibin)  +   &
                            electrolyte(jnacl,jp,ibin)   +   &
                         2.*electrolyte(jna2so4,jp,ibin) +   &
                         3.*electrolyte(jna3hso4,jp,ibin)+   &
                            electrolyte(jnahso4,jp,ibin)

      aer(inh4_a,jp,ibin) = electrolyte(jnh4no3,jp,ibin) +   &
                            electrolyte(jnh4cl,jp,ibin)  +   &
                         2.*electrolyte(jnh4so4,jp,ibin) +   &
                         3.*electrolyte(jlvcite,jp,ibin) +   &
                            electrolyte(jnh4hso4,jp,ibin)


      thesum = aer(ica_a,jp,ibin) +   &
            aer(ina_a,jp,ibin) +   &
            aer(inh4_a,jp,ibin)+   &
            aer(iso4_a,jp,ibin)+   &
            aer(ino3_a,jp,ibin)+   &
            aer(icl_a,jp,ibin) +   &
            aer(ico3_a,jp,ibin)

      if(thesum .eq. 0.)thesum = 1.0

      aer_percent(ica_a,jp,ibin) = 100.*aer(ica_a,jp,ibin)/thesum
      aer_percent(ina_a,jp,ibin) = 100.*aer(ina_a,jp,ibin)/thesum
      aer_percent(inh4_a,jp,ibin)= 100.*aer(inh4_a,jp,ibin)/thesum
      aer_percent(iso4_a,jp,ibin)= 100.*aer(iso4_a,jp,ibin)/thesum
      aer_percent(ino3_a,jp,ibin)= 100.*aer(ino3_a,jp,ibin)/thesum
      aer_percent(icl_a,jp,ibin) = 100.*aer(icl_a,jp,ibin)/thesum
      aer_percent(ico3_a,jp,ibin)= 100.*aer(ico3_a,jp,ibin)/thesum


      return
      end subroutine electrolytes_to_ions










!***********************************************************************
! combinatorial method for computing electrolytes from ions
!
! notes:
!  - to be used for liquid-phase or total-phase only
!  - transfers caso4 and caco3 from liquid to solid phase
!
! author: rahul a. zaveri (based on code provided by a.s. wexler
! update: apr 2005
!-----------------------------------------------------------------------
      subroutine ions_to_electrolytes(jp,ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt
! local variables
      integer iaer, je, jc, ja, icase
      real store(naer), thesum, sum_naza, sum_nczc, sum_na_nh4,   &
           f_nh4, f_na, xh, xb, xl, xs, cat_net
      real nc(ncation), na(nanion)




      if(jp .ne. jliquid)then
        write(6,*)' jp must be jliquid'
        write(6,*)' in ions_to_electrolytes sub'
        write(6,*)' wrong jp = ', jp
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping in ions_to_electrolytes' )
      endif

! remove negative concentrations, if any
      do iaer = 1, naer
      aer(iaer,jp,ibin) = max(0.0, aer(iaer,jp,ibin))
      enddo


! first transfer caso4 from liquid to solid phase (caco3 should not be present here)
      store(ica_a)  = aer(ica_a, jp,ibin)
      store(iso4_a) = aer(iso4_a,jp,ibin)

      call form_caso4(store,jp,ibin)

      if(jp .eq. jliquid)then ! transfer caso4 from liquid to solid phase
        aer(ica_a,jliquid,ibin) = aer(ica_a,jliquid,ibin) -   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(iso4_a,jliquid,ibin)= aer(iso4_a,jliquid,ibin)-   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(ica_a,jsolid,ibin)  = aer(ica_a,jsolid,ibin) +   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(iso4_a,jsolid,ibin) = aer(iso4_a,jsolid,ibin) +   &
                                  electrolyte(jcaso4,jliquid,ibin)

        electrolyte(jcaso4,jsolid,ibin)=electrolyte(jcaso4,jsolid,ibin)   &
                                       +electrolyte(jcaso4,jliquid,ibin)
        electrolyte(jcaso4,jliquid,ibin)= 0.0
      endif


! calculate sulfate ratio
      call calculate_xt(ibin,jp,xt)

      if(xt .ge. 2.0 .or. xt.lt.0.)then
       icase = 1	! near neutral (acidity is caused by hcl and/or hno3)
      else
       icase = 2	! acidic (acidity is caused by excess so4)
      endif


! initialize to zero
      do je = 1, nelectrolyte
        electrolyte(je,jp,ibin) = 0.0
      enddo
!
!---------------------------------------------------------
! initialize moles of ions depending on the sulfate domain

      if(icase.eq.1)then ! xt >= 2 : sulfate poor domain

        na(ja_hso4)= 0.0
        na(ja_so4) = aer(iso4_a,jp,ibin)
        na(ja_no3) = aer(ino3_a,jp,ibin)
        na(ja_cl)  = aer(icl_a, jp,ibin)

        nc(jc_ca)  = aer(ica_a, jp,ibin)
        nc(jc_na)  = aer(ina_a, jp,ibin)
        nc(jc_nh4) = aer(inh4_a,jp,ibin)

        cat_net = real( dble(2.*na(ja_so4)+na(ja_no3)+na(ja_cl)) -   &
                 dble(2.*nc(jc_ca) +nc(jc_nh4)+nc(jc_na)) )

        if(cat_net .lt. 0.0)then

!          if(aer(inh4_a,jp,ibin) .gt. abs(cat_net))then ! degas excess nh3
!            aer(inh4_a,jp,ibin)     = aer(inh4_a,jp,ibin) + cat_net
!            aer(inh4_a,jtotal,ibin) = aer(inh4_a,jtotal,ibin)+cat_net
!            gas(inh3_g)             = gas(inh3_g) - cat_net
!          endif

          nc(jc_h) = 0.0

        else  ! cat_net must be 0.0 or positive

          nc(jc_h) = cat_net

        endif


! now compute equivalent fractions
      sum_naza = 0.0
      do ja = 1, nanion
        sum_naza = sum_naza + na(ja)*za(ja)
      enddo

      sum_nczc = 0.0
      do jc = 1, ncation
        sum_nczc = sum_nczc + nc(jc)*zc(jc)
      enddo

      if(sum_naza .eq. 0. .or. sum_nczc .eq. 0.)then
        write(6,*)'ionic concentrations are zero'
        write(6,*)'sum_naza = ', sum_naza
        write(6,*)'sum_nczc = ', sum_nczc
        return
      endif

      do ja = 1, nanion
        xeq_a(ja) = na(ja)*za(ja)/sum_naza
      enddo

      do jc = 1, ncation
        xeq_c(jc) = nc(jc)*zc(jc)/sum_nczc
      enddo

      na_ma(ja_so4) = na(ja_so4) *mw_a(ja_so4)
      na_ma(ja_no3) = na(ja_no3) *mw_a(ja_no3)
      na_ma(ja_cl)  = na(ja_cl)  *mw_a(ja_cl)
      na_ma(ja_hso4)= na(ja_hso4)*mw_a(ja_hso4)

      nc_mc(jc_ca)  = nc(jc_ca) *mw_c(jc_ca)
      nc_mc(jc_na)  = nc(jc_na) *mw_c(jc_na)
      nc_mc(jc_nh4) = nc(jc_nh4)*mw_c(jc_nh4)
      nc_mc(jc_h)   = nc(jc_h)  *mw_c(jc_h)


! now compute electrolyte moles
      electrolyte(jna2so4,jp,ibin) = (xeq_c(jc_na) *na_ma(ja_so4) +   &
                                      xeq_a(ja_so4)*nc_mc(jc_na))/   &
                                       mw_electrolyte(jna2so4)

      electrolyte(jnahso4,jp,ibin) = (xeq_c(jc_na) *na_ma(ja_hso4) +   &
                                      xeq_a(ja_hso4)*nc_mc(jc_na))/   &
                                       mw_electrolyte(jnahso4)

      electrolyte(jnano3, jp,ibin) = (xeq_c(jc_na) *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_na))/   &
                                       mw_electrolyte(jnano3)

      electrolyte(jnacl,  jp,ibin) = (xeq_c(jc_na) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_na))/   &
                                       mw_electrolyte(jnacl)

      electrolyte(jnh4so4,jp,ibin) = (xeq_c(jc_nh4)*na_ma(ja_so4) +   &
                                      xeq_a(ja_so4)*nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4so4)

      electrolyte(jnh4hso4,jp,ibin)= (xeq_c(jc_nh4)*na_ma(ja_hso4) +   &
                                      xeq_a(ja_hso4)*nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4hso4)

      electrolyte(jnh4no3,jp,ibin) = (xeq_c(jc_nh4)*na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4no3)

      electrolyte(jnh4cl, jp,ibin) = (xeq_c(jc_nh4)*na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4cl)

      electrolyte(jcano3, jp,ibin) = (xeq_c(jc_ca) *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_ca))/   &
                                       mw_electrolyte(jcano3)

      electrolyte(jcacl2, jp,ibin) = (xeq_c(jc_ca) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_ca))/   &
                                       mw_electrolyte(jcacl2)

      electrolyte(jh2so4, jp,ibin) = (xeq_c(jc_h)  *na_ma(ja_hso4) +   &
                                      xeq_a(ja_hso4)*nc_mc(jc_h))/   &
                                       mw_electrolyte(jh2so4)

      electrolyte(jhno3,  jp,ibin) = (xeq_c(jc_h)  *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_h))/   &
                                       mw_electrolyte(jhno3)

      electrolyte(jhcl,   jp,ibin) = (xeq_c(jc_h) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl)*nc_mc(jc_h))/   &
                                       mw_electrolyte(jhcl)

!--------------------------------------------------------------------

      elseif(icase.eq.2)then ! xt < 2 : sulfate rich domain

        sum_na_nh4 = aer(ina_a,jp,ibin) + aer(inh4_a,jp,ibin)
        if(sum_na_nh4 .gt. 0.0)then
          f_nh4 = aer(inh4_a,jp,ibin)/sum_na_nh4
          f_na  = aer(ina_a,jp,ibin)/sum_na_nh4
        else
          f_nh4 = 0.0
          f_na  = 0.0
        endif

        if(xt .le. 1.0)then	! h2so4 + bisulfate
          xh = (1.0 - xt)
          xb = xt
          electrolyte(jh2so4,jp,ibin)   = xh*aer(iso4_a,jp,ibin)
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
        elseif(xt .le. 1.5)then	! bisulfate + letovicite
          xb = 3.0 - 2.0*xt
          xl = xt - 1.0
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
        else			! letovicite + sulfate
          xl = 2.0 - xt
          xs = 2.0*xt - 3.0
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jnh4so4,jp,ibin)  = xs*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna2so4,jp,ibin)  = xs*f_na *aer(iso4_a,jp,ibin)
        endif

        electrolyte(jhno3,jp,ibin) = aer(ino3_a,jp,ibin)
        electrolyte(jhcl,jp,ibin)  = aer(icl_a,jp,ibin)

      endif
!---------------------------------------------------------
!
! calculate % composition
      thesum = 0.0
      do je = 1, nelectrolyte
        thesum = thesum + electrolyte(je,jp,ibin)
      enddo

      electrolyte_sum(jp,ibin) = thesum

      if(thesum .eq. 0.)thesum = 1.0
      do je = 1, nelectrolyte
        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/thesum
      enddo



      thesum = aer(ica_a,jp,ibin) +   &
            aer(ina_a,jp,ibin) +   &
            aer(inh4_a,jp,ibin)+   &
            aer(iso4_a,jp,ibin)+   &
            aer(ino3_a,jp,ibin)+   &
            aer(icl_a,jp,ibin) +   &
            aer(ico3_a,jp,ibin)

      if(thesum .eq. 0.)thesum = 1.0

      aer_percent(ica_a,jp,ibin) = 100.*aer(ica_a,jp,ibin)/thesum
      aer_percent(ina_a,jp,ibin) = 100.*aer(ina_a,jp,ibin)/thesum
      aer_percent(inh4_a,jp,ibin)= 100.*aer(inh4_a,jp,ibin)/thesum
      aer_percent(iso4_a,jp,ibin)= 100.*aer(iso4_a,jp,ibin)/thesum
      aer_percent(ino3_a,jp,ibin)= 100.*aer(ino3_a,jp,ibin)/thesum
      aer_percent(icl_a,jp,ibin) = 100.*aer(icl_a,jp,ibin)/thesum
      aer_percent(ico3_a,jp,ibin)= 100.*aer(ico3_a,jp,ibin)/thesum



      return
      end subroutine ions_to_electrolytes



























!***********************************************************************
! conforms aerosol generic species to a valid electrolyte composition
!
! author: rahul a. zaveri
! update: june 2000
!-----------------------------------------------------------------------
      subroutine conform_electrolytes(jp,ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt
! local variables
      integer i, ixt_case, je
      real thesum, xna_prime, xnh4_prime, xt_prime
      real store(naer)

! remove negative concentrations, if any
      do i=1,naer
      aer(i,jp,ibin) = max(0.0, aer(i,jp,ibin))
      enddo


      call calculate_xt(ibin,jp,xt)

      if(xt .ge. 2.0 .or. xt.lt.0.)then
       ixt_case = 1	! near neutral (acidity is caused by hcl and/or hno3)
      else
       ixt_case = 2	! acidic (acidity is caused by excess so4)
      endif

! initialize
!
! put total aer(*) into store(*)
      store(iso4_a) = aer(iso4_a,jp,ibin)
      store(ino3_a) = aer(ino3_a,jp,ibin)
      store(icl_a)  = aer(icl_a, jp,ibin)
      store(ico3_a) = aer(ico3_a,jp,ibin)
      store(inh4_a) = aer(inh4_a,jp,ibin)
      store(ina_a)  = aer(ina_a, jp,ibin)
      store(ica_a)  = aer(ica_a, jp,ibin)
!
      do je=1,nelectrolyte
      electrolyte(je,jp,ibin) = 0.0
      enddo
!
!---------------------------------------------------------
!
      if(ixt_case.eq.1)then

! xt >= 2   : sulfate deficient

        call form_caso4(store,jp,ibin)
        call form_na2so4(store,jp,ibin)
        call form_cano3(store,jp,ibin)
        call form_nano3(store,jp,ibin)
        call conform_nacl(store,jp,ibin)
        call form_cacl2(store,jp,ibin)
        call form_caco3(store,jp,ibin)
        call form_nh4so4(store,jp,ibin)
        call form_nh4no3(store,jp,ibin)
        call form_nh4cl(store,jp,ibin)
        call degas_hno3(store,jp,ibin)
        call degas_hcl(store,jp,ibin)
        call degas_nh3(store,jp,ibin)

      elseif(ixt_case.eq.2)then

! xt < 2   : sulfate enough or sulfate excess

        call form_caso4(store,jp,ibin)

        xt_prime =(store(ina_a)+store(inh4_a))/   &
                        store(iso4_a)
        xna_prime=0.5*store(ina_a)/store(iso4_a) + 1.

        if(xt_prime.ge.xna_prime)then
          call form_na2so4(store,jp,ibin)
          xnh4_prime = 0.0
          if(store(iso4_a).gt.1.e-15)then
            xnh4_prime = store(inh4_a)/store(iso4_a)
          endif

          if(xnh4_prime .ge. 1.5)then
            call form_nh4so4_lvcite(store,jp,ibin)
          else
            call form_lvcite_nh4hso4(store,jp,ibin)
          endif

        elseif(xt_prime.ge.1.)then
          call form_nh4hso4(store,jp,ibin)
          call form_na2so4_nahso4(store,jp,ibin)
        elseif(xt_prime.lt.1.)then
          call form_nahso4(store,jp,ibin)
          call form_nh4hso4(store,jp,ibin)
          call form_h2so4(store,jp,ibin)
        endif

      call degas_hno3(store,jp,ibin)
      call degas_hcl(store,jp,ibin)
      call degas_nh3(store,jp,ibin)

      endif ! case 1, 2
!---------------------------------------------------------
!
! calculate % composition
      thesum = 0.0
      do je = 1, nelectrolyte
        electrolyte(je,jp,ibin) = max(0.,electrolyte(je,jp,ibin)) ! remove -ve
        thesum = thesum + electrolyte(je,jp,ibin)
      enddo

      electrolyte_sum(jp,ibin) = thesum

      if(thesum .eq. 0.)thesum = 1.0
      do je = 1, nelectrolyte
        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/thesum
      enddo


      thesum = aer(ica_a,jp,ibin) +   &
            aer(ina_a,jp,ibin) +   &
            aer(inh4_a,jp,ibin)+   &
            aer(iso4_a,jp,ibin)+   &
            aer(ino3_a,jp,ibin)+   &
            aer(icl_a,jp,ibin) +   &
            aer(ico3_a,jp,ibin)

      if(thesum .eq. 0.)thesum = 1.0

      aer_percent(ica_a,jp,ibin) = 100.*aer(ica_a,jp,ibin)/thesum
      aer_percent(ina_a,jp,ibin) = 100.*aer(ina_a,jp,ibin)/thesum
      aer_percent(inh4_a,jp,ibin)= 100.*aer(inh4_a,jp,ibin)/thesum
      aer_percent(iso4_a,jp,ibin)= 100.*aer(iso4_a,jp,ibin)/thesum
      aer_percent(ino3_a,jp,ibin)= 100.*aer(ino3_a,jp,ibin)/thesum
      aer_percent(icl_a,jp,ibin) = 100.*aer(icl_a,jp,ibin)/thesum
      aer_percent(ico3_a,jp,ibin)= 100.*aer(ico3_a,jp,ibin)/thesum

      return
      end subroutine conform_electrolytes











!***********************************************************************
! forms electrolytes from ions
!
! author: rahul a. zaveri
! update: june 2000
!-----------------------------------------------------------------------
      subroutine form_electrolytes(jp,ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt
! local variables
      integer i, ixt_case, j, je
      real thesum, xna_prime, xnh4_prime, xt_prime
      real store(naer)

! remove negative concentrations, if any
      do i=1,naer
      aer(i,jp,ibin) = max(0.0, aer(i,jp,ibin))
      enddo


      call calculate_xt(ibin,jp,xt)

      if(xt .ge. 2.0 .or. xt.lt.0.)then
       ixt_case = 1	! near neutral (acidity is caused by hcl and/or hno3)
      else
       ixt_case = 2	! acidic (acidity is caused by excess so4)
      endif

! initialize
!
! put total aer(*) into store(*)
      store(iso4_a) = aer(iso4_a,jp,ibin)
      store(ino3_a) = aer(ino3_a,jp,ibin)
      store(icl_a)  = aer(icl_a, jp,ibin)
      store(ico3_a) = aer(ico3_a,jp,ibin)
      store(inh4_a) = aer(inh4_a,jp,ibin)
      store(ina_a)  = aer(ina_a, jp,ibin)
      store(ica_a)  = aer(ica_a, jp,ibin)
!
      do j=1,nelectrolyte
      electrolyte(j,jp,ibin) = 0.0
      enddo
!
!---------------------------------------------------------
!
      if(ixt_case.eq.1)then

! xt >= 2   : sulfate deficient
        call form_caso4(store,jp,ibin)
        call form_na2so4(store,jp,ibin)
        call form_cano3(store,jp,ibin)
        call form_nano3(store,jp,ibin)
        call form_nacl(store,jp,ibin)
        call form_cacl2(store,jp,ibin)
        call form_caco3(store,jp,ibin)
        call form_nh4so4(store,jp,ibin)
        call form_nh4no3(store,jp,ibin)
        call form_nh4cl(store,jp,ibin)

        if(jp .eq. jsolid)then
          call degas_hno3(store,jp,ibin)
          call degas_hcl(store,jp,ibin)
          call degas_nh3(store,jp,ibin)
        else
          call form_hno3(store,jp,ibin)
          call form_hcl(store,jp,ibin)
          call degas_nh3(store,jp,ibin)
        endif



      elseif(ixt_case.eq.2)then

! xt < 2   : sulfate enough or sulfate excess

        call form_caso4(store,jp,ibin)

        xt_prime =(store(ina_a)+store(inh4_a))/   &
                        store(iso4_a)
        xna_prime=0.5*store(ina_a)/store(iso4_a) + 1.

        if(xt_prime.ge.xna_prime)then
          call form_na2so4(store,jp,ibin)
          xnh4_prime = 0.0
          if(store(iso4_a).gt.1.e-15)then
            xnh4_prime = store(inh4_a)/store(iso4_a)
          endif

          if(xnh4_prime .ge. 1.5)then
            call form_nh4so4_lvcite(store,jp,ibin)
          else
            call form_lvcite_nh4hso4(store,jp,ibin)
          endif

        elseif(xt_prime.ge.1.)then
          call form_nh4hso4(store,jp,ibin)
          call form_na2so4_nahso4(store,jp,ibin)
        elseif(xt_prime.lt.1.)then
          call form_nahso4(store,jp,ibin)
          call form_nh4hso4(store,jp,ibin)
          call form_h2so4(store,jp,ibin)
        endif

        if(jp .eq. jsolid)then
          call degas_hno3(store,jp,ibin)
          call degas_hcl(store,jp,ibin)
          call degas_nh3(store,jp,ibin)
        else
          call form_hno3(store,jp,ibin)
          call form_hcl(store,jp,ibin)
          call degas_nh3(store,jp,ibin)
        endif

      endif ! case 1, 2
!---------------------------------------------------------
!
! calculate % composition
      thesum = 0.0
      do je = 1, nelectrolyte
        electrolyte(je,jp,ibin) = max(0.,electrolyte(je,jp,ibin)) ! remove -ve
        thesum = thesum + electrolyte(je,jp,ibin)
      enddo

      electrolyte_sum(jp,ibin) = thesum

      if(thesum .eq. 0.)thesum = 1.0
      do je = 1, nelectrolyte
        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/thesum
      enddo


      thesum = aer(ica_a,jp,ibin) +   &
            aer(ina_a,jp,ibin) +   &
            aer(inh4_a,jp,ibin)+   &
            aer(iso4_a,jp,ibin)+   &
            aer(ino3_a,jp,ibin)+   &
            aer(icl_a,jp,ibin) +   &
            aer(ico3_a,jp,ibin)

      if(thesum .eq. 0.)thesum = 1.0

      aer_percent(ica_a,jp,ibin) = 100.*aer(ica_a,jp,ibin)/thesum
      aer_percent(ina_a,jp,ibin) = 100.*aer(ina_a,jp,ibin)/thesum
      aer_percent(inh4_a,jp,ibin)= 100.*aer(inh4_a,jp,ibin)/thesum
      aer_percent(iso4_a,jp,ibin)= 100.*aer(iso4_a,jp,ibin)/thesum
      aer_percent(ino3_a,jp,ibin)= 100.*aer(ino3_a,jp,ibin)/thesum
      aer_percent(icl_a,jp,ibin) = 100.*aer(icl_a,jp,ibin)/thesum
      aer_percent(ico3_a,jp,ibin)= 100.*aer(ico3_a,jp,ibin)/thesum


      return
      end subroutine form_electrolytes













!***********************************************************************
! part of asteem: does arbitrary electrolyte formation for non-volatiles and
!    uses wexler's algorithm for nh4no3, nh4cl, hno3, and hcl
!
! notes:
!  - to be used for liquid-phase or total-phase only
!  - transfers caso4 and caco3 from liquid to solid phase
!  - may transfer excess nh3 in the liquid phase to gas, but does not update aer(jtotal)
!
! author: rahul a. zaveri
! update: oct 2004
!-----------------------------------------------------------------------
      subroutine asteem_formelectrolytes_hybrid(jp,ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin, jp
      real xt
! local variables
      integer iaer, icase, je, ja, jc
      real thesum, xna_prime, xnh4_prime, xt_prime
      real store(naer), nc(ncation), na(nanion)
      real sum_naza, sum_nczc, sum_na_nh4, f_nh4, f_na,   &
           xh, xb, xs, xl, cat_net





      if(jp .ne. jliquid)then
        write(6,*)' jp must be jliquid'
        write(6,*)' in ions_to_electrolytes sub'
        write(6,*)' wrong jp = ', jp
!       stop
        call peg_error_fatal( lunerr_aer,   &
            'stopping asteem_formelectrolytes_hybrid' )
      endif

! remove negative concentrations, if any
      do iaer = 1, naer
      aer(iaer,jp,ibin) = max(0.0, aer(iaer,jp,ibin))
      enddo


! first transfer caso4 from liquid to solid phase (caco3 should not be present here)
      store(ica_a)  = aer(ica_a, jp,ibin)
      store(iso4_a) = aer(iso4_a,jp,ibin)

      call form_caso4(store,jp,ibin)

      if(jp .eq. jliquid)then ! transfer caso4 from liquid to solid phase
        aer(ica_a,jliquid,ibin) = aer(ica_a,jliquid,ibin) -   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(iso4_a,jliquid,ibin)= aer(iso4_a,jliquid,ibin)-   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(ica_a,jsolid,ibin)  = aer(ica_a,jsolid,ibin) +   &
                                  electrolyte(jcaso4,jliquid,ibin)

        aer(iso4_a,jsolid,ibin) = aer(iso4_a,jsolid,ibin) +   &
                                  electrolyte(jcaso4,jliquid,ibin)

        electrolyte(jcaso4,jsolid,ibin)=electrolyte(jcaso4,jsolid,ibin)   &
                                       +electrolyte(jcaso4,jliquid,ibin)
        electrolyte(jcaso4,jliquid,ibin)= 0.0
      endif

! calculate sulfate ratio
      call calculate_xt(ibin,jp,xt)

      if(xt .ge. 2.0 .or. xt.lt.0.)then
       icase = 1	! near neutral (acidity is caused by hcl and/or hno3)
      else
       icase = 2	! acidic (acidity is caused by excess so4)
      endif


! initialize to zero
      do je = 1, nelectrolyte
        electrolyte(je,jp,ibin) = 0.0
      enddo


! initialize store
!
! put total aer(*) into store(*)
      store(iso4_a) = aer(iso4_a,jp,ibin)
      store(ino3_a) = aer(ino3_a,jp,ibin)
      store(icl_a)  = aer(icl_a, jp,ibin)
      store(ico3_a) = aer(ico3_a,jp,ibin)
      store(inh4_a) = aer(inh4_a,jp,ibin)
      store(ina_a)  = aer(ina_a, jp,ibin)
      store(ica_a)  = aer(ica_a, jp,ibin)

!
!---------------------------------------------------------
! sulfate-poor domain
      if(icase.eq.1)then

        call form_na2so4(store,jp,ibin)
        call form_nh4so4(store,jp,ibin)
!        call form_cano3(store,jp,ibin)
!        call form_nano3(store,jp,ibin)
!        call form_cacl2(store,jp,ibin)
!        call form_nacl(store,jp,ibin)

! now use wexler's algorithm
        na(ja_hso4)= 0.0	! = 0 in sulfate-poor domain
        na(ja_so4) = 0.0	! by now store(iso4_a) must be 0
        na(ja_no3) = store(ino3_a)
        na(ja_cl)  = store(icl_a)

        nc(jc_ca)  = store(ica_a)	! by now store(ica_a) must be 0
        nc(jc_na)  = store(ina_a)	! by now store(ica_a) must be 0
        nc(jc_nh4) = store(inh4_a)

        cat_net = real( dble(na(ja_no3)+na(ja_cl)) -   &
                        dble(nc(jc_nh4)+nc(jc_na)+2.*nc(jc_ca)) )

        if(cat_net .lt. 0.0)then

!          if(aer(inh4_a,jp,ibin) .gt. abs(cat_net))then ! degas excess nh3
!            aer(inh4_a,jp,ibin)     = aer(inh4_a,jp,ibin) + cat_net
!            aer(inh4_a,jtotal,ibin) = aer(inh4_a,jtotal,ibin)+cat_net
!            gas(inh3_g)             = gas(inh3_g) - cat_net
!          endif

          nc(jc_h) = 0.0

        else  ! cat_net must be 0.0 or positive

          nc(jc_h) = cat_net

        endif


! now compute equivalent fractions
      sum_naza = 0.0
      do ja = 1, nanion
        sum_naza = sum_naza + na(ja)*za(ja)
      enddo

      sum_nczc = 0.0
      do jc = 1, ncation
        sum_nczc = sum_nczc + nc(jc)*zc(jc)
      enddo

      if(sum_naza .eq. 0. .or. sum_nczc .eq. 0.)then
!        write(6,*)'ionic concentrations are zero'
!        write(6,*)'sum_naza = ', sum_naza
!        write(6,*)'sum_nczc = ', sum_nczc
        return
      endif

      do ja = 1, nanion
        xeq_a(ja) = na(ja)*za(ja)/sum_naza
      enddo

      do jc = 1, ncation
        xeq_c(jc) = nc(jc)*zc(jc)/sum_nczc
      enddo

      na_ma(ja_so4) = na(ja_so4) *mw_a(ja_so4)
      na_ma(ja_no3) = na(ja_no3) *mw_a(ja_no3)
      na_ma(ja_cl)  = na(ja_cl)  *mw_a(ja_cl)
      na_ma(ja_hso4)= na(ja_hso4)*mw_a(ja_hso4)

      nc_mc(jc_ca)  = nc(jc_ca) *mw_c(jc_ca)
      nc_mc(jc_na)  = nc(jc_na) *mw_c(jc_na)
      nc_mc(jc_nh4) = nc(jc_nh4)*mw_c(jc_nh4)
      nc_mc(jc_h)   = nc(jc_h)  *mw_c(jc_h)


! now compute electrolyte moles

      electrolyte(jnano3, jp,ibin) = (xeq_c(jc_na) *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_na))/   &
                                        mw_electrolyte(jnano3)

      electrolyte(jnacl,  jp,ibin) = (xeq_c(jc_na) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_na))/   &
                                       mw_electrolyte(jnacl)

      electrolyte(jnh4no3,jp,ibin) = (xeq_c(jc_nh4)*na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4no3)

      electrolyte(jnh4cl, jp,ibin) = (xeq_c(jc_nh4)*na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_nh4))/   &
                                       mw_electrolyte(jnh4cl)

      electrolyte(jcano3, jp,ibin) = (xeq_c(jc_ca) *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_ca))/   &
                                       mw_electrolyte(jcano3)

      electrolyte(jcacl2, jp,ibin) = (xeq_c(jc_ca) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl) *nc_mc(jc_ca))/   &
                                       mw_electrolyte(jcacl2)

      electrolyte(jhno3, jp,ibin)  = (xeq_c(jc_h)  *na_ma(ja_no3) +   &
                                      xeq_a(ja_no3)*nc_mc(jc_h))/   &
                                       mw_electrolyte(jhno3)

      electrolyte(jhcl,   jp,ibin) = (xeq_c(jc_h) *na_ma(ja_cl) +   &
                                      xeq_a(ja_cl)*nc_mc(jc_h))/   &
                                       mw_electrolyte(jhcl)

!----------------------------------------------------------------
! sulfate-rich domain
      elseif(icase.eq.2)then

        sum_na_nh4 = aer(ina_a,jp,ibin) + aer(inh4_a,jp,ibin)
        if(sum_na_nh4 .gt. 0.0)then
          f_nh4 = aer(inh4_a,jp,ibin)/sum_na_nh4
          f_na  = aer(ina_a,jp,ibin)/sum_na_nh4
        else
          f_nh4 = 0.0
          f_na  = 0.0
        endif

        if(xt .le. 1.0)then	! h2so4 + bisulfate
          xh = (1.0 - xt)
          xb = xt
          electrolyte(jh2so4,jp,ibin)   = xh*aer(iso4_a,jp,ibin)
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
        elseif(xt .le. 1.5)then	! bisulfate + letovicite
          xb = 3.0 - 2.0*xt
          xl = xt - 1.0
          electrolyte(jnh4hso4,jp,ibin) = xb*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jnahso4,jp,ibin)  = xb*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
        else			! letovicite + sulfate
          xl = 2.0 - xt
          xs = 2.0*xt - 3.0
          electrolyte(jlvcite,jp,ibin)  = xl*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna3hso4,jp,ibin) = xl*f_na *aer(iso4_a,jp,ibin)
          electrolyte(jnh4so4,jp,ibin)  = xs*f_nh4*aer(iso4_a,jp,ibin)
          electrolyte(jna2so4,jp,ibin)  = xs*f_na *aer(iso4_a,jp,ibin)
        endif

        electrolyte(jhno3,jp,ibin) = aer(ino3_a,jp,ibin)
        electrolyte(jhcl,jp,ibin)  = aer(icl_a,jp,ibin)

      endif ! case 1, 2
!---------------------------------------------------------
!
! calculate % composition
      thesum = 0.0
      do je = 1, nelectrolyte
        electrolyte(je,jp,ibin) = max(0.,electrolyte(je,jp,ibin)) ! remove -ve
        thesum = thesum + electrolyte(je,jp,ibin)
      enddo

      electrolyte_sum(jp,ibin) = thesum

      if(thesum .eq. 0.)thesum = 1.0
      do je = 1, nelectrolyte
        epercent(je,jp,ibin) = 100.*electrolyte(je,jp,ibin)/thesum
      enddo



      thesum = aer(ica_a,jp,ibin) +   &
            aer(ina_a,jp,ibin) +   &
            aer(inh4_a,jp,ibin)+   &
            aer(iso4_a,jp,ibin)+   &
            aer(ino3_a,jp,ibin)+   &
            aer(icl_a,jp,ibin) +   &
            aer(ico3_a,jp,ibin)

      if(thesum .eq. 0.)thesum = 1.0

      aer_percent(ica_a,jp,ibin) = 100.*aer(ica_a,jp,ibin)/thesum
      aer_percent(ina_a,jp,ibin) = 100.*aer(ina_a,jp,ibin)/thesum
      aer_percent(inh4_a,jp,ibin)= 100.*aer(inh4_a,jp,ibin)/thesum
      aer_percent(iso4_a,jp,ibin)= 100.*aer(iso4_a,jp,ibin)/thesum
      aer_percent(ino3_a,jp,ibin)= 100.*aer(ino3_a,jp,ibin)/thesum
      aer_percent(icl_a,jp,ibin) = 100.*aer(icl_a,jp,ibin)/thesum
      aer_percent(ico3_a,jp,ibin)= 100.*aer(ico3_a,jp,ibin)/thesum


      return
      end subroutine asteem_formelectrolytes_hybrid































!***********************************************************************
! electrolyte formation subroutines
!
! author: rahul a. zaveri
! update: june 2000
!-----------------------------------------------------------------------
      subroutine form_caso4(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jcaso4,jp,ibin) = min(store(ica_a),store(iso4_a))
      store(ica_a)  = real( dble(store(ica_a)) -   &
                            dble(electrolyte(jcaso4,jp,ibin)) )
      store(iso4_a) = real( dble(store(iso4_a)) -   &
                            dble(electrolyte(jcaso4,jp,ibin)) )
      store(ica_a)  = max(0., store(ica_a))
      store(iso4_a) = max(0., store(iso4_a))

      return
      end subroutine form_caso4


      subroutine form_cano3(store,jp,ibin)	! ca(no3)2
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jcano3,jp,ibin) = min(store(ica_a),0.5*store(ino3_a))

      store(ica_a)  = real( dble(store(ica_a)) -   &
                            dble(electrolyte(jcano3,jp,ibin)) )
      store(ino3_a) = real( dble(store(ino3_a)) -   &
                            dble(2.*electrolyte(jcano3,jp,ibin)) )
      store(ica_a)  = max(0., store(ica_a))
      store(ino3_a) = max(0., store(ino3_a))

      return
      end subroutine form_cano3


      subroutine form_cacl2(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jcacl2,jp,ibin) = min(store(ica_a),0.5*store(icl_a))

      store(ica_a)  = real( dble(store(ica_a)) -   &
                            dble(electrolyte(jcacl2,jp,ibin)) )
      store(icl_a)  = real( dble(store(icl_a)) -   &
                            dble(2.*electrolyte(jcacl2,jp,ibin)) )
      store(ica_a)  = max(0., store(ica_a))
      store(icl_a)  = max(0., store(icl_a))

      return
      end subroutine form_cacl2


      subroutine form_caco3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      if(jp.eq.jtotal .or. jp.eq.jsolid)then
      electrolyte(jcaco3,jp,ibin) = store(ica_a)

      aer(ico3_a,jp,ibin)= electrolyte(jcaco3,jp,ibin)	! force co3 = caco3

      store(ica_a) = 0.0
      store(ico3_a)= 0.0
      endif

      return
      end subroutine form_caco3


      subroutine form_na2so4(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jna2so4,jp,ibin) = min(.5*store(ina_a),   &
                                            store(iso4_a))
      store(ina_a) =real( dble(store(ina_a)) -   &
                          dble(2.*electrolyte(jna2so4,jp,ibin)) )
      store(iso4_a)=real( dble(store(iso4_a)) -   &
                          dble(electrolyte(jna2so4,jp,ibin)) )
      store(ina_a) =max(0., store(ina_a))
      store(iso4_a)=max(0., store(iso4_a))

      return
      end subroutine form_na2so4



      subroutine form_nahso4(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnahso4,jp,ibin) = min(store(ina_a),   &
                                         store(iso4_a))
      store(ina_a)  = real( dble(store(ina_a)) -   &
                            dble(electrolyte(jnahso4,jp,ibin)) )
      store(iso4_a) = real( dble(store(iso4_a)) -   &
                            dble(electrolyte(jnahso4,jp,ibin)) )
      store(ina_a)  = max(0., store(ina_a))
      store(iso4_a) = max(0., store(iso4_a))

      return
      end subroutine form_nahso4



      subroutine form_nano3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnano3,jp,ibin)=min(store(ina_a),store(ino3_a))
      store(ina_a)  = real( dble(store(ina_a)) -   &
                            dble(electrolyte(jnano3,jp,ibin)) )
      store(ino3_a) = real( dble(store(ino3_a)) -   &
                            dble(electrolyte(jnano3,jp,ibin)) )
      store(ina_a)  = max(0., store(ina_a))
      store(ino3_a) = max(0., store(ino3_a))

      return
      end subroutine form_nano3



      subroutine form_nacl(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnacl,jp,ibin) = min(store(ina_a),store(icl_a))

      store(ina_a) = 0.0
      store(icl_a) = real( dble(store(icl_a)) -   &
                           dble(electrolyte(jnacl,jp,ibin)) )
      store(icl_a) = max(0., store(icl_a))

      return
      end subroutine form_nacl



      subroutine conform_nacl(store,jp,ibin)	! may artificially produce cl
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnacl,jp,ibin) = store(ina_a)

      store(ina_a) = 0.0
      store(icl_a) = real( dble(store(icl_a)) -   &
                           dble(electrolyte(jnacl,jp,ibin)) )

      if(store(icl_a) .lt. 0.)then 				! cl deficit in aerosol. take some from gas
        aer(icl_a,jp,ibin)= aer(icl_a,jp,ibin)- store(icl_a)	! update aer(icl_a)

! also update for jtotal
        if(jp .ne. jtotal)then
          aer(icl_a,jtotal,ibin)= aer(icl_a,jtotal,ibin)- store(icl_a)
        endif
        gas(ihcl_g) = gas(ihcl_g) + store(icl_a)			! update gas(ihcl_g)
        gas(ihcl_g) = max(0., gas(ihcl_g))				! restrict gas(ihcl_g) to >= 0.
        store(icl_a) = 0.        				! force store(icl_a) to 0.
      endif

      store(icl_a) = max(0., store(icl_a))

      return
      end subroutine conform_nacl



      subroutine form_nh4so4(store,jp,ibin)	! (nh4)2so4
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnh4so4,jp,ibin)= min(.5*store(inh4_a),   &
                                           store(iso4_a))
      store(inh4_a)= real( dble(store(inh4_a)) -   &
                           dble(2.*electrolyte(jnh4so4,jp,ibin)) )
      store(iso4_a)= real( dble(store(iso4_a)) -   &
                           dble(electrolyte(jnh4so4,jp,ibin)) )
      store(inh4_a) = max(0., store(inh4_a))
      store(iso4_a) = max(0., store(iso4_a))

      return
      end subroutine form_nh4so4



      subroutine form_nh4hso4(store,jp,ibin)	! nh4hso4
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnh4hso4,jp,ibin) = min(store(inh4_a),   &
                                          store(iso4_a))
      store(inh4_a)= real( dble(store(inh4_a)) -   &
                           dble(electrolyte(jnh4hso4,jp,ibin)) )
      store(iso4_a)= real( dble(store(iso4_a)) -   &
                           dble(electrolyte(jnh4hso4,jp,ibin)) )
      store(inh4_a) = max(0., store(inh4_a))
      store(iso4_a) = max(0., store(iso4_a))

      return
      end subroutine form_nh4hso4



      subroutine form_nh4cl(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnh4cl,jp,ibin) = min(store(inh4_a),   &
                                        store(icl_a))
      store(inh4_a) = real( dble(store(inh4_a)) -   &
                            dble(electrolyte(jnh4cl,jp,ibin)) )
      store(icl_a)  = real( dble(store(icl_a)) -   &
                            dble(electrolyte(jnh4cl,jp,ibin)) )
      store(inh4_a) = max(0., store(inh4_a))
      store(icl_a)  = max(0., store(icl_a))

      return
      end subroutine form_nh4cl



      subroutine form_nh4no3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnh4no3,jp,ibin) = min(store(inh4_a),   &
                                         store(ino3_a))
      store(inh4_a) = real( dble(store(inh4_a)) -   &
                            dble(electrolyte(jnh4no3,jp,ibin)) )
      store(ino3_a) = real( dble(store(ino3_a)) -   &
                            dble(electrolyte(jnh4no3,jp,ibin)) )
      store(inh4_a) = max(0., store(inh4_a))
      store(ino3_a) = max(0., store(ino3_a))

      return
      end subroutine form_nh4no3



      subroutine form_nh4so4_lvcite(store,jp,ibin) ! (nh4)2so4 + (nh4)3h(so4)2
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jnh4so4,jp,ibin)= real( dble(2.*store(inh4_a)) -   &
                                          dble(3.*store(iso4_a)) )
      electrolyte(jlvcite,jp,ibin)= real( dble(2.*store(iso4_a)) -   &
                                          dble(store(inh4_a)) )
      electrolyte(jnh4so4,jp,ibin)= max(0.,   &
                                    electrolyte(jnh4so4,jp,ibin))
      electrolyte(jlvcite,jp,ibin)= max(0.,   &
                                    electrolyte(jlvcite,jp,ibin))
      store(inh4_a) = 0.
      store(iso4_a) = 0.

      return
      end subroutine form_nh4so4_lvcite



      subroutine form_lvcite_nh4hso4(store,jp,ibin) ! (nh4)3h(so4)2 + nh4hso4
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jlvcite,jp,ibin) = real( dble(store(inh4_a)) -   &
                                           dble(store(iso4_a)) )
      electrolyte(jnh4hso4,jp,ibin)= real( dble(3.*store(iso4_a)) -   &
                                           dble(2.*store(inh4_a)) )
      electrolyte(jlvcite,jp,ibin) = max(0.,   &
                                      electrolyte(jlvcite,jp,ibin))
      electrolyte(jnh4hso4,jp,ibin)= max(0.,   &
                                      electrolyte(jnh4hso4,jp,ibin))
      store(inh4_a) = 0.
      store(iso4_a) = 0.

      return
      end subroutine form_lvcite_nh4hso4



      subroutine form_na2so4_nahso4(store,jp,ibin) ! na2so4 + nahso4
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jna2so4,jp,ibin)= real( dble(store(ina_a)) -   &
                                          dble(store(iso4_a)) )
      electrolyte(jnahso4,jp,ibin)= real( dble(2.*store(iso4_a))-   &
                                          dble(store(ina_a)) )
      electrolyte(jna2so4,jp,ibin)= max(0.,   &
                                    electrolyte(jna2so4,jp,ibin))
      electrolyte(jnahso4,jp,ibin)= max(0.,   &
                                    electrolyte(jnahso4,jp,ibin))
      store(ina_a)  = 0.
      store(iso4_a) = 0.

!	write(6,*)'na2so4 + nahso4'

      return
      end subroutine form_na2so4_nahso4




      subroutine form_h2so4(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jh2so4,jp,ibin) = max(0.0, store(iso4_a))
      store(iso4_a) = 0.0

      return
      end subroutine form_h2so4




      subroutine form_hno3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jhno3,jp,ibin) = max(0.0, store(ino3_a))
      store(ino3_a) = 0.0

      return
      end subroutine form_hno3




      subroutine form_hcl(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      electrolyte(jhcl,jp,ibin) = max(0.0, store(icl_a))
      store(icl_a) = 0.0

      return
      end subroutine form_hcl




      subroutine degas_hno3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      store(ino3_a) = max(0.0, store(ino3_a))
      gas(ihno3_g) = gas(ihno3_g) + store(ino3_a)
      aer(ino3_a,jp,ibin) = real( dble(aer(ino3_a,jp,ibin)) -   &
                                   dble(store(ino3_a)) )
      aer(ino3_a,jp,ibin) = max(0.0,aer(ino3_a,jp,ibin))

! also do it for jtotal
      if(jp .ne. jtotal)then
        aer(ino3_a,jtotal,ibin) = aer(ino3_a,jsolid, ibin) +   &
                                  aer(ino3_a,jliquid,ibin)
      endif

      electrolyte(jhno3,jp,ibin) = 0.0
      store(ino3_a) = 0.0

      return
      end subroutine degas_hno3



      subroutine degas_hcl(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      store(icl_a) = max(0.0, store(icl_a))
      gas(ihcl_g) = gas(ihcl_g) + store(icl_a)
      aer(icl_a,jp,ibin) = real( dble(aer(icl_a,jp,ibin)) -   &
                                  dble(store(icl_a)) )
      aer(icl_a,jp,ibin) = max(0.0,aer(icl_a,jp,ibin))

! also do it for jtotal
      if(jp .ne. jtotal)then
        aer(icl_a,jtotal,ibin) = aer(icl_a,jsolid, ibin) +   &
                                 aer(icl_a,jliquid,ibin)
      endif

      electrolyte(jhcl,jp,ibin) = 0.0
      store(icl_a) = 0.0

      return
      end subroutine degas_hcl



      subroutine degas_nh3(store,jp,ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real store(naer)

      store(inh4_a) = max(0.0, store(inh4_a))
      gas(inh3_g) = gas(inh3_g) + store(inh4_a)
      aer(inh4_a,jp,ibin) = real( dble(aer(inh4_a,jp,ibin)) -   &
                                   dble(store(inh4_a)) )
      aer(inh4_a,jp,ibin) = max(0.0,aer(inh4_a,jp,ibin))

! also do it for jtotal
      if(jp .ne. jtotal)then
        aer(inh4_a,jtotal,ibin)= aer(inh4_a,jsolid, ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      endif

      store(inh4_a) = 0.0

      return
      end subroutine degas_nh3









      subroutine degas_acids(jp,ibin,xt)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer jp, ibin
      real xt
! local variables
      real ehno3, ehcl



      if(jp .ne. jliquid)then
        write(6,*)'error in degas_acids'
        write(6,*)'wrong jp'
      endif

      ehno3 = electrolyte(jhno3,jp,ibin)
      ehcl  = electrolyte(jhcl,jp,ibin)

! add to gas
      gas(ihno3_g) = gas(ihno3_g) + ehno3
      gas(ihcl_g)  = gas(ihcl_g)  + ehcl

! remove from aer
      aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) - ehno3
      aer(icl_a, jp,ibin) = aer(icl_a, jp,ibin) - ehcl

! update jtotal
      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
                                aer(ino3_a,jsolid, ibin)

      aer(icl_a,jtotal,ibin)  = aer(icl_a,jliquid,ibin) +   &
                                aer(icl_a,jsolid, ibin)

      electrolyte(jhno3,jp,ibin) = 0.0
      electrolyte(jhcl,jp,ibin)  = 0.0


      return
      end subroutine degas_acids











!***********************************************************************
! subroutines to evaporate volatile species by half
!
! author: rahul a. zaveri
! update: jan 2004
!-----------------------------------------------------------------------
      subroutine evaporate_half_nh4no3(ibin)	! so that epercent(jnh4no3) remains below 1.0
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real x


      x = 0.5*min(aer(ino3_a,jliquid,ibin),   &
                  aer(inh4_a,jliquid,ibin))

! update gas and aer(jliquid)
      gas(ihno3_g) = gas(ihno3_g) + x
      gas(inh3_g)  = gas(inh3_g)  + x
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) - x
      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) - x

! also update jtotal
      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
                                aer(ino3_a,jsolid, ibin)
      aer(inh4_a,jtotal,ibin) = aer(inh4_a,jliquid,ibin) +   &
                                aer(inh4_a,jsolid, ibin)

      return
      end subroutine evaporate_half_nh4no3


      subroutine evaporate_half_nh4cl(ibin)	! so that epercent(jnh4cl) remains below 1.0
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real x


      x = 0.5*min(aer(icl_a,jliquid,ibin),   &
                  aer(inh4_a,jliquid,ibin))

! update gas and aer(jliquid)
      gas(ihcl_g) = gas(ihcl_g) + x
      gas(inh3_g) = gas(inh3_g) + x
      aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin)  - x
      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) - x

! also update jtotal
      aer(icl_a,jtotal,ibin)  = aer(icl_a,jliquid,ibin)  +   &
                                aer(icl_a,jsolid, ibin)
      aer(inh4_a,jtotal,ibin) = aer(inh4_a,jliquid,ibin) +   &
                                aer(inh4_a,jsolid, ibin)

      return
      end subroutine evaporate_half_nh4cl





















!***********************************************************************
! subroutines to evaporate volatile species close to sulfate domain change
!
! author: rahul a. zaveri
! update: jan 2004
!-----------------------------------------------------------------------
      subroutine evaporate_nh4no3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real acl_l, anh4_l, ano3_l, ghcl, ghno3, gnh3,   &
        enh4no3, gnh3_ghno3, xt, keq_nh4no3


      if(electrolyte(jnh4so4,jliquid,ibin) .gt. 0.0)then

          call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)	! may also changes aer(inh4_a,jtotal,ibin) if excess nh3 is degassed
          ghno3   = gas(ihno3_g)
          ghcl    = gas(ihcl_g)
          gnh3    = gas(inh3_g)

          ano3_l  = aer(ino3_a,jliquid,ibin)
          acl_l   = aer(icl_a,jliquid,ibin)
          anh4_l  = aer(inh4_a,jliquid,ibin)

          enh4no3 = electrolyte(jnh4no3,jliquid,ibin)

! completely evaporate nh4no3
          gas(ihno3_g) = ghno3 + enh4no3
          gas(inh3_g)  = gnh3  + enh4no3
          aer(ino3_a,jliquid,ibin) = max(ano3_l - enh4no3, 0.0)
          aer(inh4_a,jliquid,ibin) = max(anh4_l - enh4no3, 0.0)

          call ions_to_electrolytes(jliquid,ibin,xt)
          call compute_activities(ibin)

          call equilibrate_acids(ibin)
          gnh3_ghno3 = gas(inh3_g)*gas(ihno3_g)
          keq_nh4no3 = activity(jnh4no3,ibin)*kp_nh4no3

          if(gnh3_ghno3 .lt. keq_nh4no3)then	! evaporate only nh3

            sfc_a(ih2so4_g)= 0.0
            sfc_a(ihno3_g) = gas(ihno3_g)
            sfc_a(ihcl_g)  = gas(ihcl_g)
            sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                            (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

            df_gas(ihno3_g,ibin)  = 0.0
            df_gas(ihcl_g,ibin)   = 0.0
            df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

            phi_volatile(ihno3_g,ibin) = 0.0
            phi_volatile(ihcl_g,ibin)  = 0.0
            phi_volatile(inh3_g,ibin)  = abs(df_gas(inh3_g,ibin))/   &
                                  max(gas(inh3_g), sfc_a(inh3_g))

            flux(ihno3_g,ibin)    = 0.0
            flux(ihcl_g,ibin)     = 0.0

! check for equilibrium
            if(phi_volatile(inh3_g,ibin) .lt. 0.01)then
              flux(inh3_g,ibin)   = 0.0
            else
              flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
            endif

          else	! restore original values and ignore evaporation of nh4no3

            gas(ihno3_g) = ghno3
            gas(ihcl_g)  = ghcl
            gas(inh3_g)  = gnh3

            aer(ino3_a,jliquid,ibin) = ano3_l
            aer(icl_a,jliquid,ibin)  = acl_l
            aer(inh4_a,jliquid,ibin) = anh4_l


            flux(ihno3_g,ibin)  = 0.0
            flux(inh3_g,ibin)   = 0.0
            call ions_to_electrolytes(jliquid,ibin,xt)
          endif

      else	! na and/or ca salts are present

!          call equilibrate_tiny_nh4no3(ibin)
          flux(ihno3_g,ibin) = 0.0
          flux(ihcl_g,ibin)  = 0.0
          flux(inh3_g,ibin)  = 0.0

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)


      return
      end subroutine evaporate_nh4no3












      subroutine evaporate_nh4cl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real acl_l, anh4_l, ano3_l, ghcl, ghno3, gnh3,   &
        enh4cl, enh4no3,  gnh3_ghcl, xt, keq_nh4cl


      if(electrolyte(jnh4so4,jliquid,ibin) .gt. 0.0)then

          call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)	! may also changes aer(inh4_a,jtotal,ibin) if excess nh3 is degassed
          ghno3   = gas(ihno3_g)
          ghcl    = gas(ihcl_g)
          gnh3    = gas(inh3_g)

          ano3_l  = aer(ino3_a,jliquid,ibin)
          acl_l   = aer(icl_a,jliquid,ibin)
          anh4_l  = aer(inh4_a,jliquid,ibin)

          enh4cl  = electrolyte(jnh4cl,jliquid,ibin)

! completely evaporate nh4cl
          gas(ihcl_g)  = ghcl  + enh4cl
          gas(inh3_g)  = gnh3  + enh4cl
          aer(icl_a,jliquid,ibin)  = max(acl_l  - enh4cl, 0.0)
          aer(inh4_a,jliquid,ibin) = max(anh4_l - enh4cl, 0.0)

          call ions_to_electrolytes(jliquid,ibin,xt)
          call compute_activities(ibin)

          call equilibrate_acids(ibin)
          gnh3_ghcl = gas(inh3_g)*gas(ihcl_g)
          keq_nh4cl = activity(jnh4cl,ibin)*kp_nh4cl

          if(gnh3_ghcl .lt. keq_nh4cl)then	! evaporate only nh3

            sfc_a(ih2so4_g)= 0.0
            sfc_a(ihno3_g) = gas(ihno3_g)
            sfc_a(ihcl_g)  = gas(ihcl_g)
            sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                           (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

            df_gas(ihno3_g,ibin)  = 0.0
            df_gas(ihcl_g,ibin)   = 0.0
            df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

            phi_volatile(ihno3_g,ibin) = 0.0
            phi_volatile(ihcl_g,ibin)  = 0.0
            phi_volatile(inh3_g,ibin)  = abs(df_gas(inh3_g,ibin))/   &
                                  max(gas(inh3_g), sfc_a(inh3_g))

            flux(ihno3_g,ibin)    = 0.0
            flux(ihcl_g,ibin)     = 0.0

! check for equilibrium
            if(phi_volatile(inh3_g,ibin) .lt. 0.01)then
              flux(inh3_g,ibin)   = 0.0
            else
              flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
            endif

          else	! restore original values and ignore evaporation of nh4cl

            gas(ihno3_g) = ghno3
            gas(ihcl_g)  = ghcl
            gas(inh3_g)  = gnh3

            aer(ino3_a,jliquid,ibin) = ano3_l
            aer(icl_a,jliquid,ibin)  = acl_l
            aer(inh4_a,jliquid,ibin) = anh4_l

            flux(ihcl_g,ibin)   = 0.0
            flux(inh3_g,ibin)   = 0.0
            call ions_to_electrolytes(jliquid,ibin,xt)

          endif

      else	! na and/or ca salts are present

!          call equilibrate_tiny_nh4cl(ibin)
          flux(ihno3_g,ibin) = 0.0
          flux(ihcl_g,ibin)  = 0.0
          flux(inh3_g,ibin)  = 0.0

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)


      return
      end subroutine evaporate_nh4cl










      subroutine evaporate_nh4no3_nh4cl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real acl_l, anh4_l, ano3_l, ghcl, ghno3, gnh3,   &
        enh4cl, enh4no3,  gnh3_ghcl, gnh3_ghno3, xt,   &
        keq_nh4no3, keq_nh4cl


      ghno3   = gas(ihno3_g)
      ghcl    = gas(ihcl_g)
      gnh3    = gas(inh3_g)

      ano3_l  = aer(ino3_a,jliquid,ibin)
      acl_l   = aer(icl_a,jliquid,ibin)
      anh4_l  = aer(inh4_a,jliquid,ibin)

      enh4no3 = electrolyte(jnh4no3,jliquid,ibin)
      enh4cl  = electrolyte(jnh4cl,jliquid,ibin)

! completely evaporate nh4no3  and nh4cl
      gas(ihno3_g) = ghno3 + ano3_l
      gas(ihcl_g)  = ghcl  + acl_l
      gas(inh3_g)  = gnh3  + ano3_l + acl_l
      aer(ino3_a,jliquid,ibin) = 0.0
      aer(icl_a,jliquid,ibin)  = 0.0
      aer(inh4_a,jliquid,ibin) = max(anh4_l-(ano3_l+acl_l), 0.0)

      call ions_to_electrolytes(jliquid,ibin,xt)
      call compute_activities(ibin)

      call equilibrate_acids(ibin)
      gnh3_ghcl  = gas(inh3_g)*gas(ihcl_g)
      gnh3_ghno3 = gas(inh3_g)*gas(ihno3_g)
      keq_nh4no3 = activity(jnh4no3,ibin)*kp_nh4no3
      keq_nh4cl  = activity(jnh4cl,ibin) *kp_nh4cl


! now check what to do further
      if(gnh3_ghno3 .le. keq_nh4no3 .and.   &
         gnh3_ghcl  .le. keq_nh4cl)then	! evaporate only nh3

        sfc_a(ih2so4_g)= 0.0
        sfc_a(ihno3_g) = gas(ihno3_g)
        sfc_a(ihcl_g)  = gas(ihcl_g)
        sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                           (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

        df_gas(ihno3_g,ibin)  = 0.0
        df_gas(ihcl_g,ibin)   = 0.0
        df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

        phi_volatile(ihno3_g,ibin) = 0.0
        phi_volatile(ihcl_g,ibin)  = 0.0
        phi_volatile(inh3_g,ibin)  =  df_gas(inh3_g,ibin)/   &
                                  max(gas(inh3_g), sfc_a(inh3_g))

        flux(ihno3_g,ibin)    = 0.0
        flux(ihcl_g,ibin)     = 0.0

! check for equilibrium
        if(abs(phi_volatile(inh3_g,ibin)) .lt. 0.01)then
          flux(inh3_g,ibin)   = 0.0
        else
          flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
        endif

      else	! restore original values and ignore evaporation of nh4no3

        gas(ihno3_g) = ghno3
        gas(ihcl_g)  = ghcl
        gas(inh3_g)  = gnh3

        aer(ino3_a,jliquid,ibin) = ano3_l
        aer(icl_a,jliquid,ibin)  = acl_l
        aer(inh4_a,jliquid,ibin) = anh4_l

        flux(ihno3_g,ibin)  = 0.0
        flux(ihcl_g,ibin)   = 0.0
        flux(inh3_g,ibin)   = 0.0
        call ions_to_electrolytes(jliquid,ibin,xt)
      endif


! update jtotal
      aer(inh4_a,jtotal,ibin) = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)  = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)


      return
      end subroutine evaporate_nh4no3_nh4cl










      subroutine evaporate_nh4no3_nh4cl_old(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real acl_l, anh4_l, ano3_l, ghcl, ghno3, gnh3,   &
        enh4cl, enh4no3,  gnh3_ghcl, gnh3_ghno3, xt,   &
        keq_nh4no3, keq_nh4cl


      if(electrolyte(jnh4so4,jliquid,ibin) .gt. 0.0)then

          call asteem_formelectrolytes_hybrid(jliquid,ibin,xt)	! may also changes aer(inh4_a,jtotal,ibin) if excess nh3 is degassed
          ghno3   = gas(ihno3_g)
          ghcl    = gas(ihcl_g)
          gnh3    = gas(inh3_g)

          ano3_l  = aer(ino3_a,jliquid,ibin)
          acl_l   = aer(icl_a,jliquid,ibin)
          anh4_l  = aer(inh4_a,jliquid,ibin)

          enh4no3 = electrolyte(jnh4no3,jliquid,ibin)
          enh4cl  = electrolyte(jnh4cl,jliquid,ibin)

! completely evaporate nh4no3  and nh4cl
          gas(ihno3_g) = ghno3 + enh4no3
          gas(ihcl_g)  = ghcl  + enh4cl
          gas(inh3_g)  = gnh3  + enh4no3 + enh4cl
          aer(ino3_a,jliquid,ibin) = max(ano3_l - enh4no3, 0.0)
          aer(icl_a,jliquid,ibin)  = max(acl_l  - enh4cl, 0.0)
          aer(inh4_a,jliquid,ibin) = max(anh4_l - enh4no3-enh4cl, 0.0)

          call ions_to_electrolytes(jliquid,ibin,xt)
          call compute_activities(ibin)

          call equilibrate_acids(ibin)
          gnh3_ghcl  = gas(inh3_g)*gas(ihcl_g)
          gnh3_ghno3 = gas(inh3_g)*gas(ihno3_g)
          keq_nh4no3 = activity(jnh4no3,ibin)*kp_nh4no3
          keq_nh4cl  = activity(jnh4cl,ibin) *kp_nh4cl

          if(gnh3_ghno3 .lt. keq_nh4no3 .and.   &
             gnh3_ghcl  .lt. keq_nh4cl)then	! evaporate only nh3

            sfc_a(ih2so4_g)= 0.0
            sfc_a(ihno3_g) = gas(ihno3_g)
            sfc_a(ihcl_g)  = gas(ihcl_g)
            sfc_a(inh3_g)  = gam_ratio(ibin)*mc(jc_nh4,ibin)*keq_ll(3)/   &
                           (mc(jc_h,ibin)*keq_ll(2)*keq_gl(2))

            df_gas(ihno3_g,ibin)  = 0.0
            df_gas(ihcl_g,ibin)   = 0.0
            df_gas(inh3_g,ibin)   = gas(inh3_g) - sfc_a(inh3_g)

            phi_volatile(ihno3_g,ibin) = 0.0
            phi_volatile(ihcl_g,ibin)  = 0.0
            phi_volatile(inh3_g,ibin)  = abs(df_gas(inh3_g,ibin))/   &
                                  max(gas(inh3_g), sfc_a(inh3_g))

            flux(ihno3_g,ibin)    = 0.0
            flux(ihcl_g,ibin)     = 0.0

! check for equilibrium
            if(phi_volatile(inh3_g,ibin) .lt. 0.01)then
              flux(inh3_g,ibin)   = 0.0
            else
              flux(inh3_g,ibin)   = kg(inh3_g,ibin)*df_gas(inh3_g,ibin)
            endif

          else	! restore original values and ignore evaporation of nh4no3

            gas(ihno3_g) = ghno3
            gas(ihcl_g)  = ghcl
            gas(inh3_g)  = gnh3

            aer(ino3_a,jliquid,ibin) = ano3_l
            aer(icl_a,jliquid,ibin)  = acl_l
            aer(inh4_a,jliquid,ibin) = anh4_l

            flux(ihno3_g,ibin)  = 0.0
            flux(ihcl_g,ibin)   = 0.0
            flux(inh3_g,ibin)   = 0.0
            call ions_to_electrolytes(jliquid,ibin,xt)
          endif

      else	! na and/or ca salts are probably present

          flux(ihcl_g,ibin)  = 0.0
          flux(ihno3_g,ibin) = 0.0
          flux(inh3_g,ibin)  = 0.0

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)



      return
      end subroutine evaporate_nh4no3_nh4cl_old























!***********************************************************************
! subroutines to evaporate solid volatile species
!
! author: rahul a. zaveri
! update: sep 2004
!-----------------------------------------------------------------------
!
! only nh4no3
      subroutine degas_solid_nh4no3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jp
      real a, b, c, xgas
!     real quadratic					! mosaic func


      jp = jsolid

      a = 1.0
      b = gas(inh3_g) + gas(ihno3_g)
      c = gas(inh3_g)*gas(ihno3_g) - keq_sg(1)
      xgas = quadratic(a,b,c)

      if(xgas .ge. electrolyte(jnh4no3,jp,ibin))then ! degas all nh4no3

          gas(inh3_g) = gas(inh3_g)  + electrolyte(jnh4no3,jp,ibin)
          gas(ihno3_g)= gas(ihno3_g) + electrolyte(jnh4no3,jp,ibin)
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
                                electrolyte(jnh4no3,jp,ibin)
          aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) -   &
                                electrolyte(jnh4no3,jp,ibin)
          electrolyte(jnh4no3,jp,ibin) = 0.0
          epercent(jnh4no3,jp,ibin) = 0.0

      else	! degas only xgas amount of nh4no3

          gas(inh3_g) = gas(inh3_g)  + xgas
          gas(ihno3_g)= gas(ihno3_g) + xgas
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
          aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) - xgas

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)


      return
      end subroutine degas_solid_nh4no3






! only nh4cl
      subroutine degas_solid_nh4cl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jp
      real a, b, c, xgas
!     real quadratic					! mosaic func


      jp = jsolid

      a = 1.0
      b = gas(inh3_g) + gas(ihcl_g)
      c = gas(inh3_g)*gas(ihcl_g) - keq_sg(2)
      xgas = quadratic(a,b,c)

      if(xgas .ge. electrolyte(jnh4cl,jp,ibin))then ! degas all nh4cl

          gas(inh3_g) = gas(inh3_g) + electrolyte(jnh4cl,jp,ibin)
          gas(ihcl_g) = gas(ihcl_g) + electrolyte(jnh4cl,jp,ibin)
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
                                electrolyte(jnh4cl,jp,ibin)
          aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin) -   &
                                electrolyte(jnh4cl,jp,ibin)
          electrolyte(jnh4cl,jsolid,ibin) = 0.0
          epercent(jnh4cl,jsolid,ibin)    = 0.0

      else	! degas only xgas amount of nh4cl

          gas(inh3_g) = gas(inh3_g) + xgas
          gas(ihcl_g) = gas(ihcl_g) + xgas
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
          aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin)  - xgas

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)


      return
      end subroutine degas_solid_nh4cl








! both nh4no3 and nh4cl
      subroutine degas_solid_volatiles(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      integer jp
      real a, b, c, xgas
!     real quadratic					! mosaic func


      jp = jsolid

      if(epercent(jnh4no3,jp,ibin) .gt. 0. .and.   &
         gas(inh3_g)*gas(ihno3_g)  .lt. keq_sg(1) )then

      a = 1.0
      b = gas(inh3_g) + gas(ihno3_g)
      c = gas(inh3_g)*gas(ihno3_g) - keq_sg(1)
      xgas = quadratic(a,b,c)

        if(xgas .ge. electrolyte(jnh4no3,jp,ibin))then ! degas all nh4no3

          gas(inh3_g) = gas(inh3_g)  + electrolyte(jnh4no3,jp,ibin)
          gas(ihno3_g)= gas(ihno3_g) + electrolyte(jnh4no3,jp,ibin)
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
                                    electrolyte(jnh4no3,jp,ibin)
          aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) -   &
                                    electrolyte(jnh4no3,jp,ibin)
          electrolyte(jnh4no3,jp,ibin) = 0.0
          epercent(jnh4no3,jp,ibin) = 0.0

        else	! degas only xgas amount of nh4no3

          gas(inh3_g) = gas(inh3_g)  + xgas
          gas(ihno3_g)= gas(ihno3_g) + xgas
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
          aer(ino3_a,jp,ibin) = aer(ino3_a,jp,ibin) - xgas

        endif

      endif



      if(epercent(jnh4cl,jp,ibin) .gt. 0. .and.   &
         gas(inh3_g)*gas(ihcl_g)  .lt. keq_sg(2) )then

      a = 1.0
      b = gas(inh3_g) + gas(ihcl_g)
      c = gas(inh3_g)*gas(ihcl_g) - keq_sg(2)
      xgas = quadratic(a,b,c)

        if(xgas .ge. electrolyte(jnh4cl,jp,ibin))then ! degas all nh4cl

          gas(inh3_g) = gas(inh3_g) + electrolyte(jnh4cl,jp,ibin)
          gas(ihcl_g) = gas(ihcl_g) + electrolyte(jnh4cl,jp,ibin)
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) -   &
                                    electrolyte(jnh4cl,jp,ibin)
          aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin) -   &
                                    electrolyte(jnh4cl,jp,ibin)
          electrolyte(jnh4cl,jp,ibin) = 0.0
          epercent(jnh4cl,jp,ibin) = 0.0

        else	! degas only xgas amount of nh4cl

          gas(inh3_g) = gas(inh3_g) + xgas
          gas(ihcl_g) = gas(ihcl_g) + xgas
          aer(inh4_a,jp,ibin) = aer(inh4_a,jp,ibin) - xgas
          aer(icl_a,jp,ibin)  = aer(icl_a,jp,ibin)  - xgas

        endif

      endif


! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)

      return
      end subroutine degas_solid_volatiles







!***********************************************************************
! subroutines to equilibrate volatile acids
!
! author: rahul a. zaveri
! update: may 2002
!-----------------------------------------------------------------------
      subroutine equilibrate_acids(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real salts_cl, salts_no3, sum_salts



      if(electrolyte(jcaco3,jliquid,ibin) .gt. 0.0 .or.   &
         water_a(ibin) .eq. 0.0)return

      salts_cl  = electrolyte(jnacl,jliquid,ibin)  +   &
                  electrolyte(jcacl2,jliquid,ibin) +   &
                  electrolyte(jnh4cl,jliquid,ibin)

      salts_no3 = electrolyte(jnano3,jliquid,ibin) +   &
                  electrolyte(jcano3,jliquid,ibin) +   &
                  electrolyte(jnh4no3,jliquid,ibin)

      sum_salts = salts_cl + salts_no3


      if(gas(ihcl_g)*sum_salts.gt.0.)then
          call equilibrate_hcl(ibin)
      elseif(gas(ihno3_g)*salts_no3.gt.0. .and. salts_cl.eq.0.)then
          call equilibrate_hno3(ibin)
      elseif(gas(ihcl_g)*gas(ihno3_g).gt.0. .and. sum_salts.eq.0.)then
          call equilibrate_hcl_and_hno3(ibin)
      elseif(gas(ihcl_g).gt.0. .and. sum_salts.eq.0.)then
          call equilibrate_hcl(ibin)
      elseif(gas(ihno3_g).gt.0. .and. sum_salts.eq.0.)then
          call equilibrate_hno3(ibin)
      endif

      return
      end subroutine equilibrate_acids








! only hcl
      subroutine equilibrate_hcl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real a, aerh, aerhso4, aerso4, b, c, dum, kdash_hcl, mh, tcl,   &
        w, xt, z
!     real quadratic					! mosaic func

      aerso4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
      aerhso4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

      tcl = aer(icl_a,jliquid,ibin) + gas(ihcl_g)		! nmol/m^3(air)
      kdash_hcl = keq_gl(4)*1.e+18/gam(jhcl,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      z = (   aer(ina_a, jliquid,ibin) + 		   &  ! nmol/m^3(air)
              aer(inh4_a,jliquid,ibin) +   &
           2.*aer(ica_a, jliquid,ibin) ) -   &
          (2.*aerso4  +   &
              aerhso4 +   &
              aer(ino3_a,jliquid,ibin) )


      w     = water_a(ibin)				! kg/m^3(air)

      kdash_hcl = keq_gl(4)*1.e+18/gam(jhcl,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      a = 1.0
      b = real(dble(kdash_hcl*w) + dble(z/w))*1.e-9
      c = kdash_hcl*(z - tcl)*1.e-18


      dum = real(dble(b*b)-dble(4.*a*c))
      if (dum .lt. 0.) return		! no real root


      if(c .lt. 0.)then
        mh = quadratic(a,b,c)	! mol/kg(water)
        aerh = mh*w*1.e+9
        aer(icl_a,jliquid,ibin) = real(dble(aerh) + dble(z))
      else
        mh = sqrt(keq_ll(3))
      endif

      call form_electrolytes(jliquid,ibin,xt)

! update gas phase concentration
      gas(ihcl_g) = real( dble(tcl)  - dble(aer(icl_a,jliquid,ibin))  )


! update the following molalities
      ma(ja_so4,ibin)  = 1.e-9*aerso4/water_a(ibin)
      ma(ja_hso4,ibin) = 1.e-9*aerhso4/water_a(ibin)
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

      mc(jc_h,ibin)    = mh
      mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


! update the following activities
      activity(jhcl,ibin)    = mc(jc_h,ibin)  *ma(ja_cl,ibin)  *   &
                               gam(jhcl,ibin)**2

      activity(jhno3,ibin)   = mc(jc_h,ibin)  *ma(ja_no3,ibin) *   &
                               gam(jhno3,ibin)**2

      activity(jnh4cl,ibin)  = mc(jc_nh4,ibin)*ma(ja_cl,ibin) *   &
                               gam(jnh4cl,ibin)**2


! also update xyz(jtotal)
      aer(icl_a,jtotal,ibin) = aer(icl_a,jliquid,ibin) +   &
                               aer(icl_a,jsolid,ibin)

      electrolyte(jhcl,jtotal,ibin) = electrolyte(jhcl,jliquid,ibin)

      ph(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine equilibrate_hcl




! only hno3
      subroutine equilibrate_hno3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real a, aerh, aerhso4, aerso4, b, c, dum, kdash_hno3, mh,   &
        tno3, w, xt, z
!     real quadratic					! mosaic func

      aerso4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
      aerhso4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

      tno3 = aer(ino3_a,jliquid,ibin) + gas(ihno3_g)	! nmol/m^3(air)
      kdash_hno3 = keq_gl(3)*1.e+18/gam(jhno3,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      z = (   aer(ina_a, jliquid,ibin) + 		   &  ! nmol/m^3(air)
              aer(inh4_a,jliquid,ibin) +   &
           2.*aer(ica_a, jliquid,ibin) ) -   &
          (2.*aerso4  +   &
              aerhso4 +   &
              aer(icl_a,jliquid,ibin) )


      w     = water_a(ibin)				! kg/m^3(air)

      kdash_hno3 = keq_gl(3)*1.e+18/gam(jhno3,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      a = 1.0
      b = real(dble(kdash_hno3*w) + dble(z/w))*1.e-9
      c = kdash_hno3*(z - tno3)*1.e-18

      dum = real(dble(b*b)-dble(4.*a*c))
      if (dum .lt. 0.) return		! no real root



      if(c .lt. 0.)then
        mh = quadratic(a,b,c)	! mol/kg(water)
        aerh = mh*w*1.e+9
        aer(ino3_a,jliquid,ibin) = real(dble(aerh) + dble(z))
      else
        mh = sqrt(keq_ll(3))
      endif

      call form_electrolytes(jliquid,ibin,xt)

! update gas phase concentration
      gas(ihno3_g)= real( dble(tno3) - dble(aer(ino3_a,jliquid,ibin)) )


! update the following molalities
      ma(ja_so4,ibin)  = 1.e-9*aerso4/water_a(ibin)
      ma(ja_hso4,ibin) = 1.e-9*aerhso4/water_a(ibin)
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

      mc(jc_h,ibin)    = mh
      mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


! update the following activities
      activity(jhcl,ibin)    = mc(jc_h,ibin)  *ma(ja_cl,ibin)  *   &
                               gam(jhcl,ibin)**2

      activity(jhno3,ibin)   = mc(jc_h,ibin)  *ma(ja_no3,ibin) *   &
                               gam(jhno3,ibin)**2

      activity(jnh4no3,ibin) = mc(jc_nh4,ibin)*ma(ja_no3,ibin) *   &
                               gam(jnh4no3,ibin)**2


! also update xyz(jtotal)
      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
                                aer(ino3_a,jsolid,ibin)

      electrolyte(jhno3,jtotal,ibin) = electrolyte(jhno3,jliquid,ibin)

      ph(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine equilibrate_hno3










! both hcl and hno3
      subroutine equilibrate_hcl_and_hno3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real aerh, aerhso4, aerso4, kdash_hcl, kdash_hno3,   &
        mh, p, q, r, tcl, tno3, w, xt, z
!     real cubic					! mosaic func


      aerso4 = ma(ja_so4,ibin)*water_a(ibin)*1.e+9
      aerhso4= ma(ja_hso4,ibin)*water_a(ibin)*1.e+9

      tcl  = aer(icl_a,jliquid,ibin)  + gas(ihcl_g)	! nmol/m^3(air)
      tno3 = aer(ino3_a,jliquid,ibin) + gas(ihno3_g)	! nmol/m^3(air)

      kdash_hcl  = keq_gl(4)*1.e+18/gam(jhcl,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      kdash_hno3 = keq_gl(3)*1.e+18/gam(jhno3,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))

      z = (   aer(ina_a, jliquid,ibin) + 		   &  ! nmol/m^3(air)
              aer(inh4_a,jliquid,ibin) +   &
           2.*aer(ica_a, jliquid,ibin) ) -   &
          (2.*aerso4 + aerhso4 )


      w = water_a(ibin)

      kdash_hcl  = keq_gl(4)*1.e+18/gam(jhcl,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))
      kdash_hno3 = keq_gl(3)*1.e+18/gam(jhno3,ibin)**2	! (nmol^2/kg^2)/(nmol/m^3(air))

      p = (z/w + w*(kdash_hcl + kdash_hno3))*1.e-9

      q = 1.e-18*kdash_hcl*kdash_hno3*w**2  +   &
          1.e-18*z*(kdash_hcl + kdash_hno3) -   &
          1.e-18*kdash_hcl*tcl -   &
          1.e-18*kdash_hno3*tno3

      r = 1.e-18*kdash_hcl*kdash_hno3*w*(z - tcl - tno3)*1.e-9

      mh = cubic(p,q,r)

      if(mh .gt. 0.0)then
        aerh = mh*w*1.e+9
        aer(ino3_a,jliquid,ibin) = kdash_hno3*w*w*tno3/   &
                                  (aerh + kdash_hno3*w*w)
        aer(icl_a, jliquid,ibin) = kdash_hcl*w*w*tcl/   &
                                  (aerh + kdash_hcl*w*w)
      else
        mh = sqrt(keq_ll(3))
      endif

      call form_electrolytes(jliquid,ibin,xt)

! update gas phase concentration
      gas(ihno3_g)= real( dble(tno3) - dble(aer(ino3_a,jliquid,ibin)) )
      gas(ihcl_g) = real( dble(tcl)  - dble(aer(icl_a,jliquid,ibin))  )


! update the following molalities
      ma(ja_so4,ibin)  = 1.e-9*aerso4/water_a(ibin)
      ma(ja_hso4,ibin) = 1.e-9*aerhso4/water_a(ibin)
      ma(ja_no3,ibin)  = 1.e-9*aer(ino3_a,jliquid,ibin)/water_a(ibin)
      ma(ja_cl,ibin)   = 1.e-9*aer(icl_a, jliquid,ibin)/water_a(ibin)

      mc(jc_h,ibin)    = mh
      mc(jc_ca,ibin)   = 1.e-9*aer(ica_a, jliquid,ibin)/water_a(ibin)
      mc(jc_nh4,ibin)  = 1.e-9*aer(inh4_a,jliquid,ibin)/water_a(ibin)
      mc(jc_na,ibin)   = 1.e-9*aer(ina_a, jliquid,ibin)/water_a(ibin)


! update the following activities
      activity(jhcl,ibin)    = mc(jc_h,ibin)*ma(ja_cl,ibin)   *   &
                               gam(jhcl,ibin)**2

      activity(jhno3,ibin)   = mc(jc_h,ibin)*ma(ja_no3,ibin)  *   &
                               gam(jhno3,ibin)**2

      activity(jnh4no3,ibin) = mc(jc_nh4,ibin)*ma(ja_no3,ibin)*   &
                               gam(jnh4no3,ibin)**2

      activity(jnh4cl,ibin)  = mc(jc_nh4,ibin)*ma(ja_cl,ibin) *   &
                               gam(jnh4cl,ibin)**2


! also update xyz(jtotal)
      aer(icl_a,jtotal,ibin)  = aer(icl_a,jliquid,ibin) +   &
                                aer(icl_a,jsolid,ibin)

      aer(ino3_a,jtotal,ibin) = aer(ino3_a,jliquid,ibin) +   &
                                aer(ino3_a,jsolid,ibin)

      electrolyte(jhno3,jtotal,ibin) = electrolyte(jhno3,jliquid,ibin)
      electrolyte(jhcl, jtotal,ibin) = electrolyte(jhcl, jliquid,ibin)

      ph(ibin) = -alog10(mc(jc_h,ibin))

      return
      end subroutine equilibrate_hcl_and_hno3














!***********************************************************************
! subroutines to absorb and degas small amounts of volatile species
!
! author: rahul a. zaveri
! update: jun 2002
!-----------------------------------------------------------------------
!
! nh3
      subroutine absorb_tiny_nh3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_aer, small_amt, small_gas

      small_gas = 0.02 * gas(inh3_g)
      small_aer = 0.05 * aer(iso4_a,jliquid,ibin)

      small_amt = min(small_gas, small_aer)

      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) + small_amt

! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)

! update gas
      gas(inh3_g)              = gas(inh3_g) - small_amt

      return
      end subroutine absorb_tiny_nh3



! hcl
      subroutine absorb_tiny_hcl(ibin)	! and degas tiny hno3
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_aer, small_amt, small_gas

      small_gas = 0.01 * gas(ihcl_g)
      small_aer = 0.01 * aer(ino3_a,jliquid,ibin)

      small_amt = min(small_gas, small_aer)

! absorb tiny hcl
      aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) + small_amt
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin) +   &
                                 aer(icl_a,jliquid,ibin)
      gas(ihcl_g)              = gas(ihcl_g) - small_amt

! degas tiny hno3
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) - small_amt
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)

! update gas
      gas(ihno3_g)             = gas(ihno3_g) + small_amt

      return
      end subroutine absorb_tiny_hcl




! hno3
      subroutine absorb_tiny_hno3(ibin)	! and degas tiny hcl
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_aer, small_amt, small_gas

      small_gas = 0.01 * gas(ihno3_g)
      small_aer = 0.01 * aer(icl_a,jliquid,ibin)

      small_amt = min(small_gas, small_aer)

! absorb tiny hno3
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) + small_amt
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)
      gas(ihno3_g)             = gas(ihno3_g) - small_amt

! degas tiny hcl
      aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin) - small_amt
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin) +   &
                                 aer(icl_a,jliquid,ibin)

! update gas
      gas(ihcl_g)              = gas(ihcl_g) + small_amt

      return
      end subroutine absorb_tiny_hno3




! nh4cl
      subroutine absorb_tiny_nh4cl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_aer, small_amt, small_gas

      small_gas = 0.01 * min(gas(inh3_g), gas(ihcl_g))
      small_aer = 0.01 * electrolyte_sum(jtotal,ibin)
      if(small_aer .eq. 0.0)small_aer = small_gas

      small_amt = min(small_gas, small_aer)

      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) + small_amt
      aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin)  + small_amt

! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)

! update gas
      gas(inh3_g)              = gas(inh3_g) - small_amt
      gas(ihcl_g)              = gas(ihcl_g) - small_amt

      return
      end subroutine absorb_tiny_nh4cl




! both nh4no3
      subroutine absorb_tiny_nh4no3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_aer, small_amt, small_gas

      small_gas = 0.01 * min(gas(inh3_g), gas(ihno3_g))
      small_aer = 0.01 * electrolyte_sum(jtotal,ibin)

      small_amt = min(small_gas, small_aer)

      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) + small_amt
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) + small_amt

! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)

! update gas
      gas(inh3_g)                = gas(inh3_g)  - small_amt
      gas(ihno3_g)               = gas(ihno3_g) - small_amt

      return
      end subroutine absorb_tiny_nh4no3







! nh4cl
      subroutine degas_tiny_nh4cl(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_amt


      small_amt = 0.01 * electrolyte(jnh4cl,jliquid,ibin)

      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) - small_amt
      aer(icl_a,jliquid,ibin)  = aer(icl_a,jliquid,ibin)  - small_amt

! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(icl_a,jtotal,ibin)   = aer(icl_a,jsolid,ibin)  +   &
                                 aer(icl_a,jliquid,ibin)

! update gas
      gas(inh3_g)                = gas(inh3_g) + small_amt
      gas(ihcl_g)                = gas(ihcl_g) + small_amt

      return
      end subroutine degas_tiny_nh4cl








! nh4no3
      subroutine degas_tiny_nh4no3(ibin)
!     implicit none
!     include 'mosaic.h'
! subr arguments
      integer ibin
! local variables
      real small_amt

      small_amt = 0.01 * electrolyte(jnh4no3,jliquid,ibin)

      aer(inh4_a,jliquid,ibin) = aer(inh4_a,jliquid,ibin) - small_amt
      aer(ino3_a,jliquid,ibin) = aer(ino3_a,jliquid,ibin) - small_amt

! update jtotal
      aer(inh4_a,jtotal,ibin)  = aer(inh4_a,jsolid,ibin) +   &
                                 aer(inh4_a,jliquid,ibin)
      aer(ino3_a,jtotal,ibin)  = aer(ino3_a,jsolid,ibin) +   &
                                 aer(ino3_a,jliquid,ibin)

! update gas
      gas(inh3_g)                = gas(inh3_g)  + small_amt
      gas(ihno3_g)               = gas(ihno3_g) + small_amt

      return
      end subroutine degas_tiny_nh4no3












!***********************************************************************
! functions used in psc model
!
! author: rahul a. zaveri
! update: aug 1999
!-----------------------------------------------------------------------

!===========================================================================
!
! neutral species activity coefficient
!
!===========================================================================
      real function fn(jn)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer jn
! local variables
      integer jc, ja, jcp, jap
      real term1, term2, term3, term4, term5, term6, term7, term8,   &
           term9, sumover_c_a, sumover_c_cp, sumover_a_ap, sumover_a,   &
           sumover_c, lnf, lnf_s, lnf_dh, lnf_hoe, six





      six = sqrt(ix)
!
! short-range contributions
!
!--term1------------------------
      sumover_c_a = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        sumover_c_a = sumover_c_a + xc(jc)*xa(ja) *   &
                      ((zc(jc)+za(ja))**2)/(zc(jc)*za(ja)) *   &
                      (u1_c_a(jc,ja) - 2.*xh2o*u1_c_a(jc,ja))
      enddo
      enddo

      term1 = sumover_c_a

!--term2------------------------
      sumover_c_a = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        sumover_c_a = sumover_c_a + xc(jc)*xa(ja) *   &
                  (2.*xh2o*v1_c_a(jc,ja) - 3.*(xh2o**2)*v1_c_a(jc,ja))
      enddo
      enddo

      term2 = 4.*sumover_c_a

!--term3------------------------
      sumover_c_a = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        sumover_c_a = sumover_c_a + ec(jc)*ea(ja) *   &
                      (zc(jc)+za(ja))/(zc(jc)*za(ja)) *   &
                      (w1_c_a(jc,ja) - xh2o*w1_c_a(jc,ja))
      enddo
      enddo

      term3 = (1./ff)*sumover_c_a

!--term4------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0
        do jc = 1, ncation
        do jcp = jc+1, ncation
        sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                       (xc(jc)/xnuc(jc,ja) - xc(jcp)/xnuc(jcp,ja))*   &
                       uc_cp_a(jc,jcp,ja)
        enddo
        enddo

      sumover_a = sumover_a + ea(ja)*sumover_c_cp
      enddo

      term4 = -4.*sumover_a

!--term5------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0
        do ja = 1, nanion
        do jap = ja+1, nanion
        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                       (xa(ja)/xnua(ja,jc) - xa(jap)/xnua(jap,jc))*   &
                       ua_ap_c(ja,jap,jc)
        enddo
        enddo

      sumover_c = sumover_c + ec(jc)*sumover_a_ap
      enddo

      term5 = -4.*sumover_c

!--term6------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0
        do jc = 1, ncation
        do jcp = jc+1, ncation
        sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                                      wc_cp_a(jc,jcp,ja)
        enddo
        enddo

      sumover_a = sumover_a + ea(ja)*sumover_c_cp
      enddo

      term6 = -2.*sumover_a

!--term7------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0
        do ja = 1, nanion
        do jap = ja+1, nanion
        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                                      wa_ap_c(ja,jap,jc)
        enddo
        enddo

      sumover_c = sumover_c + ec(jc)*sumover_a_ap
      enddo

      term7 = -2.*sumover_c

!--term8------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0
        do jc = 1, ncation
        do jcp = jc+1, ncation
        sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                       q1_c_cp_a(jc,jcp,ja)*(1 - 2.*xh2o)
        enddo
        enddo

      sumover_a = sumover_a + ea(ja)*sumover_c_cp
      enddo

      term8 = 4.*sumover_a

!--term9------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0
        do ja = 1, nanion
        do jap = ja+1, nanion
        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                       q1_a_ap_c(ja,jap,jc)*(1 - 2.*xh2o)
        enddo
        enddo

      sumover_c = sumover_c + ec(jc)*sumover_a_ap
      enddo

      term9 = 4.*sumover_c

!-------------------------------
      lnf_s = term1 + term2 + term3 + term4 + term5 +   &
              term6 + term7 + term8 + term9


! long-range contributions (debye-huckel)

!--term1------------------------
      term1 = 2.*ax*(ix**1.5)/(1.+rho*six)

!--term2 & term3----------------
      term2 = 0.0
      term3 = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        term2 = term2 + xc(jc)*xa(ja)*bc_a(jc,ja)*   &
                        exp(-alpha_c_a(jc,ja)*six)
        term3 = term3 + xc(jc)*xa(ja)*b1_c_a(jc,ja)*   &
                        exp(-alpha1_c_a(jc,ja)*six)

      enddo
      enddo

      term2 = -term2
      term3 = -term3

!-------------------------------
      lnf_dh = term1 + term2 + term3



! long-range contributions (higher order electrostatic)

!--term1------------------------
      sumover_c_cp = 0.0
      do jc = 1, ncation
      do jcp = jc+1, ncation
        sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                       ( thetahoe(izc(jc),izc(jcp)) +   &
                         ix*dthetahoe(izc(jc),izc(jcp)) )
      enddo
      enddo

      term1 = -2.*sumover_c_cp

!--term2------------------------
      sumover_a_ap = 0.0
      do ja = 1, nanion
      do jap = ja+1, nanion
        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                       ( thetahoe(iza(ja),iza(jap)) +   &
                         ix*dthetahoe(iza(ja),iza(jap)) )
      enddo
      enddo

      term2 = -2.*sumover_a_ap

!-------------------------------
      lnf_hoe = term1 + term2

!================================
      lnf = lnf_s + lnf_dh + lnf_hoe
      fn = exp(lnf)

      return
      end function fn
!
!
!
!===========================================================================
!
! cation activity coefficient
!
!===========================================================================

      real function fm(jm)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer jm
! local variables
      integer jc, ja, jcp, jap, izm
      real term1, term2, term3, term4, term5, term6, term7, term8,   &
           term9, term10, sumover_c_a, sumover_c_cp, sumover_a_ap,   &
           sumover_a, sumover_c, sumoverp_a, sumoverp_c,   &
           sumoverp_c_cp, lnf, lnf_s, lnf_dh, lnf_hoe, six, zm, zm2
! functions
!     real gg


! short-range contributions
      zm = zc(jm)
      izm = izc(jm)

!--term1------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c = 0.0
        do jc = 1, ncation
        sumover_c = sumover_c + xc(jc)*   &
               (zc(jc)+za(ja))**2/(zc(jc)*za(ja))*u1_c_a(jc,ja)
        enddo

        sumover_a = sumover_a + xa(ja)*   &
                ( (zm + za(ja))**2/(zm*za(ja))*u1_c_a(jm,ja)   &
                 -2.*sumover_c )
      enddo

      term1 = xh2o*sumover_a

!--term2------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c = 0.0
        do jc = 1, ncation
        sumover_c = sumover_c + xc(jc)*v1_c_a(jc,ja)
        enddo

        sumover_a = sumover_a + xa(ja)*   &
                    ( v1_c_a(jm,ja) - 3.*sumover_c )
      enddo

      term2 = 4.*xh2o**2 * sumover_a

!--term3------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c = 0.0
        do jc = 1, ncation
        sumover_c = sumover_c +   &
               ec(jc)*(zc(jc)+za(ja))/(zc(jc)*za(ja))*w1_c_a(jc,ja)
        enddo

        sumover_a = sumover_a + ea(ja)*   &
                    ( (zm+za(ja))/za(ja)*w1_c_a(jm,ja)   &
                     -(zm/2. + 1/ff)*sumover_c )
      enddo

      term3 = xh2o*sumover_a

!--term4-------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumoverp_c = 0.0
        do jc = 1, ncation
          if(jc.ne.jm)then
          sumoverp_c = sumoverp_c + ec(jc)*   &
                      (zc(jc)+za(ja))/(zc(jc)*za(ja))*w1_c_a(jc,ja)

          endif
        enddo

      sumover_a = sumover_a + ea(ja)*   &
                  ( (1-ec(jm)/2.)*(zm+za(ja))/za(ja)*w1_c_a(jm,ja)   &
                   -zm/2.*sumoverp_c )

      enddo

      term4 = -sumover_a

!--term5-------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumoverp_c = 0.0
        sumover_c_cp = 0.0

        do jc = 1, ncation
          if(jc.ne.jm)then
          sumoverp_c = sumoverp_c + xc(jc)*   &
                       (2.*xc(jm)/xnuc(jm,ja) - xc(jc)/xnuc(jc,ja))*   &
                       uc_cp_a(jm,jc,ja)
          endif

          do jcp = jc+1, ncation
          sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                         (xc(jc)/xnuc(jc,ja) - xc(jcp)/xnuc(jcp,ja))*   &
                         uc_cp_a(jc,jcp,ja)
          enddo

        enddo

      sumover_a = sumover_a + sumoverp_c - 2.*sumover_c_cp

      enddo

      term5 = 2*sumover_a

!--term6-------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0
        do ja = 1, nanion
        do jap = ja+1, nanion

        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                       ( xa(ja)/xnua(ja,jc) - xa(jap)/xnua(jap,jc) )*   &
                       ua_ap_c(ja,jap,jc)

        enddo
        enddo

      sumover_c = sumover_c + (2.*ec(jc) - emc(jm,jc))*sumover_a_ap

      enddo

      term6 = -2.*sumover_c

!--term7-------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumoverp_c = 0.0
        sumover_c_cp = 0.0

        do jc = 1, ncation
          if(jm.ne.jc)then
          sumoverp_c = sumoverp_c + xc(jc)*wc_cp_a(jm,jc,ja)
          endif

          do jcp = jc+1, ncation
          sumover_c_cp = sumover_c_cp +   &
                         xc(jc)*xc(jcp)*wc_cp_a(jc,jcp,ja)
          enddo
        enddo

      sumover_a = sumover_a + ea(ja)*(sumoverp_c - sumover_c_cp)

      enddo

      term7 = 2.*sumover_a

!--term8-------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0
        do ja = 1, nanion
        do jap = ja+1, nanion

	sumover_a_ap = sumover_a_ap +   &
                       xa(ja)*xa(jap)*wa_ap_c(ja,jap,jc)

        enddo
        enddo

      sumover_c = sumover_c + (ec(jc) - emc(jm,jc))*sumover_a_ap

      enddo

      term8 = -2.*sumover_c

!--term9-------------------------
      sumover_a = 0.0
      do ja = 1, nanion

        sumoverp_c = 0.0
        sumover_c_cp = 0.0

        do jc = 1, ncation
          if(jm.ne.jc)then
          sumoverp_c = sumoverp_c + xc(jc)*q1_c_cp_a(jm,jc,ja)
          endif

          do jcp = jc+1, ncation
          sumover_c_cp = sumover_c_cp +   &
                         xc(jc)*xc(jcp)*q1_c_cp_a(jc,jcp,ja)
          enddo
        enddo

      sumover_a = sumover_a + ea(ja)*(sumoverp_c - 2.*sumover_c_cp)

      enddo

      term9 = 4.*xh2o*sumover_a

!--term10------------------------
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a_ap = 0.0

        do ja = 1, nanion
        do jap = ja+1, nanion
          sumover_a_ap = sumover_a_ap +   &
                         xa(ja)*xa(jap)*q1_a_ap_c(ja,jap,jc)
        enddo
        enddo

      sumover_c = sumover_c +   &
                  (2.*ec(jc) - emc(jm,jc))*sumover_a_ap

      enddo

      term10 = -4.*xh2o*sumover_c

!--------------------------------
      lnf_s =  term1 + term2 + term3 + term4 + term5 +   &
               term6 + term7 + term8 + term9 + term10



! long-range contributions (debye-huckel)
      zm2 = zc(jm)**2
      six = sqrt(ix)

!--term1-------------------------
      term1 = - zm2*ax*(2./rho*alog(1.+rho*six) +   &
                        six*(1.-2.*ix/zm2)/(1.+rho*six))

!--term2 & term3-----------------
      term2 = 0.0
      term3 = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        term2 = term2 + xc(jc)*xa(ja)*bc_a(jc,ja)*   &
                ( zm2*gg(alpha_c_a(jc,ja)*six)/(2.*ix) +   &
                  (1-zm2/(2.*ix))*exp(-alpha_c_a(jc,ja)*six) )

        term3 = term3 + xc(jc)*xa(ja)*b1_c_a(jc,ja)*   &
                ( zm2*gg(alpha1_c_a(jc,ja)*six)/(2.*ix) +   &
                  (1-zm2/(2.*ix))*exp(-alpha1_c_a(jc,ja)*six) )
      enddo
      enddo

      term2 = -term2
      term3 = -term3

!--term4 & term5-----------------
      term4 = 0.0
      term5 = 0.0
      do ja = 1, nanion
        term4=term4+xa(ja)*bc_a(jm,ja)*gg(alpha_c_a(jm,ja)*six)
        term5=term5+xa(ja)*b1_c_a(jm,ja)*gg(alpha1_c_a(jm,ja)*six)
      enddo

      lnf_dh = term1 + term2 + term3 + term4 + term5



! long-range contributions (higher order electrostatic)

!--term1-------------------------
      sumoverp_c = 0.0
      do jc = 1, ncation
        if(jc.ne.jm)then
        sumoverp_c = sumoverp_c + xc(jc)*( thetahoe(izm,izc(jc))   &
                     - xc(jm)*(thetahoe(izm,izc(jc)) +   &
                       dthetahoe(izm,izc(jc))*(ix - zm2/2.)) )
        endif
      enddo
      term1 = 2.*sumoverp_c

!--term2-------------------------
      sumoverp_c_cp = 0.0
      do jc = 1, ncation
       if(jc.ne.jm)then
         do jcp = jc+1, ncation
          if(jcp.ne.jm)then
          sumoverp_c_cp = sumoverp_c_cp + xc(jc)*xc(jcp)*   &
                         ( thetahoe(izc(jc),izc(jcp)) +   &
                           dthetahoe(izc(jc),izc(jcp))*(ix-zm2/2.) )
          endif
         enddo
       endif
      enddo
      term2 = -2.*sumoverp_c_cp

!--term3-------------------------
      sumover_a_ap = 0.0
      do ja = 1, nanion
      do jap = ja+1, nanion
        sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                       ( thetahoe(iza(ja),iza(jap)) +   &
                        dthetahoe(iza(ja),iza(jap))*(ix-zm2/2.) )
      enddo
      enddo
      term3 = -2.*sumover_a_ap

!--------------------------------
      lnf_hoe = term1 + term2 + term3



!================================
      lnf = lnf_s + lnf_dh + lnf_hoe
      fm = exp(lnf)


      return
      end function fm
!
!
!
!===========================================================================
!
! anion activity coefficient
!
!===========================================================================
      real function fx(jx)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer jx
! local variables
      integer jc, ja, jcp, jap, izx
      real term1, term2, term3, term4, term5, term6, term7, term8,   &
           term9, term10, sumover_c_a, sumover_c_cp, sumover_a_ap,   &
           sumover_a, sumover_c, sumoverp_a, sumoverp_c,   &
           sumoverp_c_cp, lnf, lnf_s, lnf_dh, lnf_hoe, six, zx, zx2
! functions
!     real gg


! short-range contributions
      zx = za(jx)
      izx = iza(jx)

!--term1------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a = 0.0
        do ja = 1, nanion
        sumover_a = sumover_a + xa(ja)*   &
                    (za(ja)+zc(jc))**2/(za(ja)*zc(jc))*u1_c_a(jc,ja)
        enddo

        sumover_c = sumover_c + xc(jc)*   &
                ( (zx + zc(jc))**2/(zx*zc(jc))*u1_c_a(jc,jx)   &
                 -2.*sumover_a )
      enddo

      term1 = xh2o*sumover_c

!--term2------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a = 0.0
        do ja = 1, nanion
        sumover_a = sumover_a + xa(ja)*v1_c_a(jc,ja)
        enddo

        sumover_c = sumover_c + xc(jc)*   &
                    ( v1_c_a(jc,jx) - 3.*sumover_a )
      enddo

      term2 = 4.*xh2o**2 * sumover_c

!--term3------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumover_a = 0.0
        do ja = 1, nanion
        sumover_a = sumover_a +   &
                ea(ja)*(za(ja)+zc(jc))/(za(ja)*zc(jc))*w1_c_a(jc,ja)
        enddo

        sumover_c = sumover_c + ec(jc)*   &
                    ( (zx+zc(jc))/zc(jc)*w1_c_a(jc,jx)   &
                     -(zx/2. + 1/ff)*sumover_a )
      enddo

      term3 = xh2o*sumover_c

!--term4-------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumoverp_a = 0.0
        do ja = 1, nanion
          if(ja.ne.jx)then
          sumoverp_a = sumoverp_a + ea(ja)*   &
                      (za(ja)+zc(jc))/(za(ja)*zc(jc))*w1_c_a(jc,ja)

          endif
        enddo

      sumover_c = sumover_c + ec(jc)*   &
                  ( (1-ea(jx)/2.)*(zx+zc(jc))/zc(jc)*w1_c_a(jc,jx)   &
                   -zx/2.*sumoverp_a )

      enddo

      term4 = -sumover_c

!--term5-------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumoverp_a = 0.0
        sumover_a_ap = 0.0

        do ja = 1, nanion
          if(ja.ne.jx)then
          sumoverp_a = sumoverp_a + xa(ja)*   &
                       (2.*xa(jx)/xnua(jx,jc) - xa(ja)/xnua(ja,jc))*   &
                       ua_ap_c(jx,ja,jc)
          endif

          do jap = ja+1, nanion
          sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                         (xa(ja)/xnua(ja,jc) - xa(jap)/xnua(jap,jc))*   &
                         ua_ap_c(ja,jap,jc)
          enddo

        enddo

      sumover_c = sumover_c + sumoverp_a - 2.*sumover_a_ap

      enddo

      term5 = 2.*sumover_c

!--term6-------------------------	done
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0
        do jc = 1, ncation
        do jcp = jc+1, ncation

        sumover_c_cp = sumover_c_cp + xc(jc)*xc(jcp)*   &
                       ( xc(jc)/xnuc(jc,ja) - xc(jcp)/xnuc(jcp,ja) )*   &
                       uc_cp_a(jc,jcp,ja)

        enddo
        enddo

      sumover_a = sumover_a + (2.*ea(ja) - exa(jx,ja))*sumover_c_cp

      enddo

      term6 = -2.*sumover_a

!--term7-------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumoverp_a = 0.0
        sumover_a_ap = 0.0

        do ja = 1, nanion
          if(ja.ne.jx)then
          sumoverp_a = sumoverp_a + xa(ja)*wa_ap_c(jx,ja,jc)
          endif

          do jap = ja+1, nanion
          sumover_a_ap = sumover_a_ap +   &
                         xa(ja)*xa(jap)*wa_ap_c(ja,jap,jc)
          enddo
        enddo

      sumover_c = sumover_c + ec(jc)*(sumoverp_a - sumover_a_ap)

      enddo

      term7 = 2.*sumover_c

!--term8-------------------------	done
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0
        do jc = 1, ncation
        do jcp = jc+1, ncation

	sumover_c_cp = sumover_c_cp +   &
                       xc(jc)*xc(jcp)*wc_cp_a(jc,jcp,ja)

        enddo
        enddo

      sumover_a = sumover_a + (ea(ja) - exa(jx,ja))*sumover_c_cp

      enddo

      term8 = -2.*sumover_a

!--term9-------------------------	done
      sumover_c = 0.0
      do jc = 1, ncation

        sumoverp_a = 0.0
        sumover_a_ap = 0.0

        do ja = 1, nanion
          if(ja.ne.jx)then
          sumoverp_a = sumoverp_a + xa(ja)*q1_a_ap_c(jx,ja,jc)
          endif

          do jap = ja+1, nanion
          sumover_a_ap = sumover_a_ap +   &
                         xa(ja)*xa(jap)*q1_a_ap_c(ja,jap,jc)
          enddo
        enddo

      sumover_c = sumover_c + ec(jc)*(sumoverp_a - 2.*sumover_a_ap)

      enddo

      term9 = 4.*xh2o*sumover_c

!--term10------------------------	done
      sumover_a = 0.0
      do ja = 1, nanion

        sumover_c_cp = 0.0

        do jc = 1, ncation
        do jcp = jc+1, ncation
          sumover_c_cp = sumover_c_cp +   &
                         xc(jc)*xc(jcp)*q1_c_cp_a(jc,jcp,ja)
        enddo
        enddo

      sumover_a = sumover_a +   &
                  (2.*ea(ja) - exa(jx,ja))*sumover_c_cp

      enddo

      term10 = -4.*xh2o*sumover_a

!--------------------------------
      lnf_s =  term1 + term2 + term3 + term4 + term5 +   &
               term6 + term7 + term8 + term9 + term10


! long-range contributions (debye-huckel)
      zx2 = za(jx)**2
      six = sqrt(ix)

!--term1-------------------------
      term1 = - zx2*ax*(2./rho*alog(1.+rho*six) +   &
                        six*(1.-2.*ix/zx2)/(1.+rho*six))

!--term2 & term3-----------------	done
      term2 = 0.0
      term3 = 0.0
      do jc = 1, ncation
      do ja = 1, nanion
        term2 = term2 + xc(jc)*xa(ja)*bc_a(jc,ja)*   &
                ( zx2*gg(alpha_c_a(jc,ja)*six)/(2.*ix) +   &
                  (1-zx2/(2.*ix))*exp(-alpha_c_a(jc,ja)*six) )

        term3 = term3 + xc(jc)*xa(ja)*b1_c_a(jc,ja)*   &
                ( zx2*gg(alpha1_c_a(jc,ja)*six)/(2.*ix) +   &
                  (1-zx2/(2.*ix))*exp(-alpha1_c_a(jc,ja)*six) )
      enddo
      enddo

      term2 = -term2
      term3 = -term3

!--term4 & term5-----------------	done
      term4 = 0.0
      term5 = 0.0
      do jc = 1, ncation
        term4=term4+xc(jc)*bc_a(jc,jx)*gg(alpha_c_a(jc,jx)*six)
        term5=term5+xc(jc)*b1_c_a(jc,jx)*gg(alpha1_c_a(jc,jx)*six)
      enddo

      lnf_dh = term1 + term2 + term3 + term4 + term5


! long-range contributions (higher order electrostatic)

!--term1-------------------------	done
      sumoverp_a = 0.0
      do ja = 1, nanion
        if(ja.ne.jx)then
        sumoverp_a = sumoverp_a + xa(ja)*( thetahoe(izx,iza(ja))   &
                     - xa(jx)*(thetahoe(izx,iza(ja)) +   &
                       dthetahoe(izx,iza(ja))*(ix - zx2/2.)) )
        endif
      enddo
      term1 = 2.*sumoverp_a

!--term2-------------------------
      sumoverp_c_cp = 0.0
      do jc = 1, ncation
      do jcp = jc+1, ncation
        sumoverp_c_cp = sumoverp_c_cp + xc(jc)*xc(jcp)*   &
                        ( thetahoe(izc(jc),izc(jcp)) +   &
                         dthetahoe(izc(jc),izc(jcp))*(ix-zx2/2.) )
      enddo
      enddo
      term2 = -2.*sumoverp_c_cp

!--term3-------------------------
      sumover_a_ap = 0.0
      do ja = 1, nanion
        if(ja.ne.jx)then
          do jap = ja+1, nanion
            if(jap.ne.jx)then
              sumover_a_ap = sumover_a_ap + xa(ja)*xa(jap)*   &
                           ( thetahoe(iza(ja),iza(jap)) +   &
                             dthetahoe(iza(ja),iza(jap))*(ix-zx2/2.) )
            endif
          enddo
        endif
      enddo
      term3 = -2.*sumover_a_ap

!--------------------------------
      lnf_hoe = term1 + term2 + term3


!================================
      lnf = lnf_s + lnf_dh + lnf_hoe
      fx = exp(lnf)

      return
      end function fx
!
!
!
!===========================================================================
!
! miscellaneous functions used in psc
!
!===========================================================================

      real function gg(x)
!     implicit none
      real x

      if(x.ne.0.)then
        gg = 2.*(1. - (1.+x)*exp(-x))/x**2
      endif
      return
      end function gg
!
!
!
!-----------------------------
      real function fn_thetahoe(izi,izj)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer izi, izj
! local variables
      real zi, zj, dum, xij, xii, xjj
! functions
!     real xj


      if(izi.eq.izj)then
        fn_thetahoe = 0.0
      else
        zi = float(izi)
        zj = float(izj)
        dum = 6.*ax*sqrt(ix)
        xij = zi*zj*dum
        xii = zi*zi*dum
        xjj = zj*zj*dum
        fn_thetahoe=(zi*zj/(4.*ix))*(xj(xij) - .5*xj(xii) - .5*xj(xjj))
      endif

      return
      end function fn_thetahoe
!
!
!
!-----------------------------
      real function fn_dthetahoe(izi,izj)
!     implicit none
!     include 'mosaic.h'
! func arguments
      integer izi, izj
! local variables
      real zi, zj, dum, xij, xii, xjj
! functions
!     real xj1


      if(izi.eq.izj)then
        fn_dthetahoe = 0.0
      else
        zi = float(izi)
        zj = float(izj)
        dum = 6.*ax*sqrt(ix)
        xij = zi*zj*dum
        xii = zi*zi*dum
        xjj = zj*zj*dum
        fn_dthetahoe = -thetahoe(izi,izj)/ix + (zi*zj/(8.*ix**2))*   &
                   (xij*xj1(xij) -.5*xii*xj1(xii) -.5*xjj*xj1(xjj))
      endif

      return
      end function fn_dthetahoe
!
!
!
!-----------------------------
      real function xj(x)
!     implicit none
! func arguments
      real x
! local variables
      real c1, c2, c3, c4

        c1 =  4.581
        c2 = -0.7237
        c3 = -0.0120
        c4 =  0.528
        xj = x/(4. + (c1*x**c2) * exp(c3*x**c4))

      return
      end function xj
!
!
!
!-----------------------------
      real function xj1(x)
!     implicit none
! func arguments
      real x
! local variables
      real c1, c2, c3, c4

        c1 =  4.581
        c2 = -0.7237
        c3 = -0.0120
        c4 =  0.528
        xj1 = 1./(4. + c1*exp(c3*x**c4)*x**c2) -   &
             c1*exp(c3*x**c4)*x**c2*(c2 + c3*c4*x**c4)/   &
             (4. + c1*exp(c3*x**c4)*x**c2)**2

      return
      end function xj1
!======================================================================







!***********************************************************************
! called only once per entire simulation to load gas and aerosol
! indices, parameters, physico-chemical constants, polynomial coeffs, etc.
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine load_mosaic_parameters
!     implicit none
!     include 'v33com2'
!     include 'mosaic.h'
! local variables
      integer je, ja, jc, j_index, ibin
      real tt, tdum
      logical first
      save first
      data first/.true./






      if(first)then
        first=.false.

!----------------------------------------------------------------
! control settings
      alpha_asteem    = 0.03		! choose a value between 0.01 and 0.05
      msize_framework = msection	! mmodal or msection
      mactivity_coeff = mmtem		! mmtem, mpsc, mkm, mbrom
      mdynamic_solver = masteem		! masteem, masceem
      mgas_aer_xfer   = mon		! mon, moff
      madapt_alpha    = mon		! mon, moff
!----------------------------------------------------------------
!
!
         jasteem_call = 0				! reset
         jmesa_call   = 0				! reset
         iter_mesa    = 0.0				! reset
         do ibin = 1, nbin_a
           steps_asteem(ibin)     = 0.0			! reset
           steps_asteem_max(ibin) = 0.0			! reset
         enddo

! set gas and aerosol indices
!
! gas
      ih2so4_g	= 1
      ihno3_g	= 2
      ihcl_g	= 3
      inh3_g	= 4
!      isoa_g	= 5	! currently not used
!      imsa_g	= 6	! currently not used
!      ico2_g	= 7	! currently not used
!
! aerosol (local): used for total species
      iso4_a	=  1	! <-> ih2so4_g
      ino3_a	=  2	! <-> ihno3_g
      icl_a	=  3	! <-> ihcl_g
      inh4_a	=  4	! <-> inh3_g
      ioc_a	=  5	! <-> isoa
      imsa_a	=  6	! <-> imsa
      ico3_a	=  7	! <-> ico2
      ina_a	=  8
      ica_a	=  9
      ibc_a	= 10
      ioin_a	= 11
!
! electrolyte indices (used for water content calculations)
! these indices are order sensitive
      jnh4so4	=  1	! soluble
      jlvcite	=  2	! soluble
      jnh4hso4	=  3	! soluble
      jnh4no3	=  4	! soluble
      jnh4cl	=  5	! soluble
      jna2so4	=  6	! soluble
      jna3hso4	=  7	! soluble
      jnahso4	=  8	! soluble
      jnano3	=  9	! soluble
      jnacl	= 10	! soluble
      jcano3	= 11	! soluble
      jcacl2	= 12	! soluble     nsalt
      jh2so4	= 13	! soluble
      jhno3	= 14	! soluble
      jhcl	= 15	! soluble
      jhhso4	= 16	! soluble
      jcaso4	= 17	! insoluble
      jcaco3	= 18	! insoluble
      joc	= 19	! insoluble - part of naercomp
      jbc	= 20	! insoluble - part of naercomp
      join	= 21	! insoluble - part of naercomp
      jh2o	= 22	! water - part of naercomp


! local aerosol ions
! cations
      jc_h	=  1
      jc_nh4	=  2
      jc_na	=  3
      jc_ca	=  4
!
! anions
      ja_hso4	=  1
      ja_so4  	=  2
      ja_no3  	=  3
      ja_cl   	=  4
!     ja_co3	=  5

!--------------------------------------------------------------------
! names of aer species
      aer_name(iso4_a) = "so4"
      aer_name(ino3_a) = "no3"
      aer_name(icl_a)  = "cl "
      aer_name(inh4_a) = "nh4"
      aer_name(ioc_a)  = "oc "
      aer_name(imsa_a) = "msa"
      aer_name(ico3_a) = "co3"
      aer_name(ina_a)  = "na "
      aer_name(ica_a)  = "ca "
      aer_name(ibc_a)  = "bc "
      aer_name(ioin_a) = "oin"

! names of gas species
      gas_name(1)    = "h2so4"
      gas_name(2)    = "hno3 "
      gas_name(3)    = "hcl  "
      gas_name(4)    = "nh3  "

! names of electrolytes
      ename(jnh4so4) = "amso4"
      ename(jlvcite) = "(nh4)3h(so4)2"
      ename(jnh4hso4)= "nh4hso4"
      ename(jnh4no3) = "nh4no3"
      ename(jnh4cl)  = "nh4cl"
      ename(jnacl)   = "nacl"
      ename(jnano3)  = "nano3"
      ename(jna2so4) = "na2so4"
      ename(jna3hso4)= "na3h(so4)2"
      ename(jnahso4) = "nahso4"
      ename(jcaso4)  = "caso4"
      ename(jcano3)  = "ca(no3)2"
      ename(jcacl2)  = "cacl2"
      ename(jcaco3)  = "caco3"
      ename(jh2so4)  = "h2so4"
      ename(jhhso4)  = "hhso4"
      ename(jhno3)   = "hno3"
      ename(jhcl)    = "hcl"

! molecular weights of electrolytes
      mw_electrolyte(jnh4so4) = 132.0
      mw_electrolyte(jlvcite) = 247.0
      mw_electrolyte(jnh4hso4)= 115.0
      mw_electrolyte(jnh4no3) = 80.0
      mw_electrolyte(jnh4cl)  = 53.5
      mw_electrolyte(jnacl)   = 58.5
      mw_electrolyte(jnano3)  = 85.0
      mw_electrolyte(jna2so4) = 142.0
      mw_electrolyte(jna3hso4)= 262.0
      mw_electrolyte(jnahso4) = 120.0
      mw_electrolyte(jcaso4)  = 136.0
      mw_electrolyte(jcano3)  = 164.0
      mw_electrolyte(jcacl2)  = 111.0
      mw_electrolyte(jcaco3)  = 100.0
      mw_electrolyte(jh2so4)  = 98.0
      mw_electrolyte(jhno3)   = 63.0
      mw_electrolyte(jhcl)    = 36.5


! molecular weights of ions [g/mol]
      mw_c(jc_h)  =  1.0
      mw_c(jc_nh4)= 18.0
      mw_c(jc_na) = 23.0
      mw_c(jc_ca) = 40.0

      mw_a(ja_so4) = 96.0
      mw_a(ja_hso4)= 97.0
      mw_a(ja_no3) = 62.0
      mw_a(ja_cl)  = 35.5


! densities of pure electrolytes in g/cc
      dens_electrolyte(jnh4so4)  = 1.77
      dens_electrolyte(jlvcite)  = 1.77
      dens_electrolyte(jnh4hso4) = 1.78
      dens_electrolyte(jnh4no3)  = 1.72
      dens_electrolyte(jnh4cl)   = 1.53
      dens_electrolyte(jnacl)    = 2.17
      dens_electrolyte(jnano3)   = 2.26
      dens_electrolyte(jna2so4)  = 2.68
      dens_electrolyte(jna3hso4) = 2.50
      dens_electrolyte(jnahso4)  = 2.43
      dens_electrolyte(jcaso4)   = 2.61
      dens_electrolyte(jcano3)   = 2.50
      dens_electrolyte(jcacl2)   = 2.15
      dens_electrolyte(jcaco3)   = 2.80
      dens_electrolyte(jh2so4)   = 1.84
      dens_electrolyte(jhhso4)   = 1.84
      dens_electrolyte(jhno3)    = 1.50
      dens_electrolyte(jhcl)     = 1.19

      do je = 1, nelectrolyte
        dens_electrolyte(je) = 1.8
      enddo

! densities of compounds in g/cc
      dens_comp_a(jnh4so4)  = 1.77
      dens_comp_a(jlvcite)  = 1.77
      dens_comp_a(jnh4hso4) = 1.78
      dens_comp_a(jnh4no3)  = 1.72
      dens_comp_a(jnh4cl)   = 1.53
      dens_comp_a(jnacl)    = 2.17
      dens_comp_a(jnano3)   = 2.26
      dens_comp_a(jna2so4)  = 2.68
      dens_comp_a(jna3hso4) = 2.50
      dens_comp_a(jnahso4)  = 2.43
      dens_comp_a(jcaso4)   = 2.61
      dens_comp_a(jcano3)   = 2.50
      dens_comp_a(jcacl2)   = 2.15
      dens_comp_a(jcaco3)   = 2.80
      dens_comp_a(jh2so4)   = 1.84
      dens_comp_a(jhhso4)   = 1.84
      dens_comp_a(jhno3)    = 1.50
      dens_comp_a(jhcl)     = 1.19
      dens_comp_a(joc)      = 1.00
      dens_comp_a(jbc)      = 1.70
      dens_comp_a(join)     = 2.60
      dens_comp_a(jh2o)     = 1.00

      do je = 1, naercomp
        dens_comp_a(je) = 1.8
      enddo

! molecular weights of generic aerosol species
      mw_aer_mac(iso4_a) = 96.0
      mw_aer_mac(ino3_a) = 62.0
      mw_aer_mac(icl_a)  = 35.5
      mw_aer_mac(imsa_a) = 96.0
      mw_aer_mac(ico3_a) = 60.0
      mw_aer_mac(inh4_a) = 18.0
      mw_aer_mac(ina_a)  = 23.0
      mw_aer_mac(ica_a)  = 40.0
      mw_aer_mac(ioin_a) = 1.0	! not used
      mw_aer_mac(ioc_a)  = 1.0	! not used
      mw_aer_mac(ibc_a)  = 1.0	! not used

! molecular weights of compounds
      mw_comp_a(jnh4so4) = 132.0
      mw_comp_a(jlvcite) = 247.0
      mw_comp_a(jnh4hso4)= 115.0
      mw_comp_a(jnh4no3) = 80.0
      mw_comp_a(jnh4cl)  = 53.5
      mw_comp_a(jnacl)   = 58.5
      mw_comp_a(jnano3)  = 85.0
      mw_comp_a(jna2so4) = 142.0
      mw_comp_a(jna3hso4)= 262.0
      mw_comp_a(jnahso4) = 120.0
      mw_comp_a(jcaso4)  = 136.0
      mw_comp_a(jcano3)  = 164.0
      mw_comp_a(jcacl2)  = 111.0
      mw_comp_a(jcaco3)  = 100.0
      mw_comp_a(jh2so4)  = 98.0
      mw_comp_a(jhhso4)  = 98.0
      mw_comp_a(jhno3)   = 63.0
      mw_comp_a(jhcl)    = 36.5
      mw_comp_a(joc)	 = 1.0
      mw_comp_a(jbc)	 = 1.0
      mw_comp_a(join)    = 1.0
      mw_comp_a(jh2o)    = 18.0

! densities of generic aerosol species
      dens_aer_mac(iso4_a) = 1.0	! not used
      dens_aer_mac(ino3_a) = 1.0	! not used
      dens_aer_mac(icl_a)  = 1.0	! not used
      dens_aer_mac(imsa_a) = 1.0	! not used
      dens_aer_mac(ico3_a) = 1.0	! not used
      dens_aer_mac(inh4_a) = 1.0	! not used
      dens_aer_mac(ina_a)  = 1.0	! not used
      dens_aer_mac(ica_a)  = 1.0	! not used
      dens_aer_mac(ioin_a) = 2.6	! used
      dens_aer_mac(ioc_a)  = 1.0	! used
      dens_aer_mac(ibc_a)  = 1.7	! used

! refractive index
      ref_index_a(jnh4so4) = cmplx(1.52,0.)
      ref_index_a(jlvcite) = cmplx(1.50,0.)
      ref_index_a(jnh4hso4)= cmplx(1.47,0.)
      ref_index_a(jnh4no3) = cmplx(1.50,0.)
      ref_index_a(jnh4cl)  = cmplx(1.50,0.)
      ref_index_a(jnacl)   = cmplx(1.45,0.)
      ref_index_a(jnano3)  = cmplx(1.50,0.)
      ref_index_a(jna2so4) = cmplx(1.50,0.)
      ref_index_a(jna3hso4)= cmplx(1.50,0.)
      ref_index_a(jnahso4) = cmplx(1.50,0.)
      ref_index_a(jcaso4)  = cmplx(1.56,0.006)
      ref_index_a(jcano3)  = cmplx(1.56,0.006)
      ref_index_a(jcacl2)  = cmplx(1.52,0.006)
      ref_index_a(jcaco3)  = cmplx(1.68,0.006)
      ref_index_a(jh2so4)  = cmplx(1.43,0.)
      ref_index_a(jhhso4)  = cmplx(1.43,0.)
      ref_index_a(jhno3)   = cmplx(1.50,0.)
      ref_index_a(jhcl)    = cmplx(1.50,0.)
      ref_index_a(joc)	   = cmplx(1.45,0.)
      ref_index_a(jbc)	   = cmplx(1.82,0.74)
      ref_index_a(join)    = cmplx(1.55,0.006)
      ref_index_a(jh2o)    = cmplx(1.33,0.)

! jsalt_index
      jsalt_index(jnh4so4) = 5		! as
      jsalt_index(jlvcite) = 2		! lv
      jsalt_index(jnh4hso4)= 1		! ab
      jsalt_index(jnh4no3) = 2		! an
      jsalt_index(jnh4cl)  = 1		! ac
      jsalt_index(jna2so4) = 60		! ss
      jsalt_index(jnahso4) = 10		! sb
      jsalt_index(jnano3)  = 40		! sn
      jsalt_index(jnacl)   = 10		! sc
      jsalt_index(jcano3)  = 120	! cn
      jsalt_index(jcacl2)  = 80		! cc

! aerosol indices
!  ac = 1, an = 2, as = 5, sc = 10, sn = 40, ss = 60, cc = 80, cn = 120,
!  ab = 1, lv = 2, sb = 10
!
! sulfate-poor domain
      jsulf_poor(1)   = 	1	! 	ac
      jsulf_poor(2)   = 	2	! 	an
      jsulf_poor(5)   = 	3	! 	as
      jsulf_poor(10)  = 	4	! 	sc
      jsulf_poor(40)  = 	5	! 	sn
      jsulf_poor(60)  = 	6	! 	ss
      jsulf_poor(80)  = 	7	! 	cc
      jsulf_poor(120) = 	8	! 	cn
      jsulf_poor(3)   = 	9	! 	an + ac
      jsulf_poor(6)   = 	10	! 	as + ac
      jsulf_poor(7)   = 	11	! 	as + an
      jsulf_poor(8)   =  	12	! 	as + an + ac
      jsulf_poor(11)  = 	13	! 	sc + ac
      jsulf_poor(41)  = 	14	! 	sn + ac
      jsulf_poor(42)  = 	15	! 	sn + an
      jsulf_poor(43)  = 	16	! 	sn + an + ac
      jsulf_poor(50)  = 	17	! 	sn + sc
      jsulf_poor(51)  = 	18	! 	sn + sc + ac
      jsulf_poor(61)  = 	19	! 	ss + ac
      jsulf_poor(62)  = 	20	! 	ss + an
      jsulf_poor(63)  = 	21	! 	ss + an + ac
      jsulf_poor(65)  = 	22	! 	ss + as
      jsulf_poor(66)  = 	23	! 	ss + as + ac
      jsulf_poor(67)  = 	24	! 	ss + as + an
      jsulf_poor(68)  = 	25	! 	ss + as + an + ac
      jsulf_poor(70)  = 	26	! 	ss + sc
      jsulf_poor(71)  = 	27	! 	ss + sc + ac
      jsulf_poor(100) = 	28	! 	ss + sn
      jsulf_poor(101) = 	29	! 	ss + sn + ac
      jsulf_poor(102) = 	30	! 	ss + sn + an
      jsulf_poor(103) = 	31	! 	ss + sn + an + ac
      jsulf_poor(110) = 	32	! 	ss + sn + sc
      jsulf_poor(111) = 	33	! 	ss + sn + sc + ac
      jsulf_poor(81)  = 	34	! 	cc + ac
      jsulf_poor(90)  = 	35	! 	cc + sc
      jsulf_poor(91)  = 	36	! 	cc + sc + ac
      jsulf_poor(121) = 	37	! 	cn + ac
      jsulf_poor(122) = 	38	! 	cn + an
      jsulf_poor(123) = 	39	! 	cn + an + ac
      jsulf_poor(130) = 	40	! 	cn + sc
      jsulf_poor(131) = 	41	! 	cn + sc + ac
      jsulf_poor(160) = 	42	! 	cn + sn
      jsulf_poor(161) = 	43	! 	cn + sn + ac
      jsulf_poor(162) = 	44	! 	cn + sn + an
      jsulf_poor(163) = 	45	! 	cn + sn + an + ac
      jsulf_poor(170) = 	46	! 	cn + sn + sc
      jsulf_poor(171) = 	47	! 	cn + sn + sc + ac
      jsulf_poor(200) = 	48	! 	cn + cc
      jsulf_poor(201) = 	49	! 	cn + cc + ac
      jsulf_poor(210) = 	50	! 	cn + cc + sc
      jsulf_poor(211) = 	51	! 	cn + cc + sc + ac
!
! sulfate-rich domain
      jsulf_rich(1)   = 	52	! 	ab
      jsulf_rich(2)   = 	53	! 	lv
      jsulf_rich(10)  = 	54	! 	sb
      jsulf_rich(3)   = 	55	! 	ab + lv
      jsulf_rich(7)   = 	56	! 	as + lv
      jsulf_rich(70)  = 	57	! 	ss + sb
      jsulf_rich(62)  = 	58	! 	ss + lv
      jsulf_rich(67)  = 	59	! 	ss + as + lv
      jsulf_rich(61)  = 	60	! 	ss + ab
      jsulf_rich(63)  = 	61	! 	ss + lv + ab
      jsulf_rich(11)  = 	62	! 	sb + ab
      jsulf_rich(71)  = 	63	! 	ss + sb + ab
      jsulf_rich(5)   = 	3	!	as
      jsulf_rich(60)  = 	6	! 	ss
      jsulf_rich(65)  = 	22	! 	ss + as



!
! polynomial coefficients for binary molality (used in zsr equation)
!
!
! a_zsr for aw < 0.97
!
! (nh4)2so4
      je = jnh4so4
      a_zsr(1,je)  =  1.30894
      a_zsr(2,je)  = -7.09922
      a_zsr(3,je)  =  20.62831
      a_zsr(4,je)  = -32.19965
      a_zsr(5,je)  =  25.17026
      a_zsr(6,je)  = -7.81632
      aw_min(je)   = 0.1
!
! (nh4)3h(so4)2
      je = jlvcite
      a_zsr(1,je)  =  1.10725
      a_zsr(2,je)  = -5.17978
      a_zsr(3,je)  =  12.29534
      a_zsr(4,je)  = -16.32545
      a_zsr(5,je)  =  11.29274
      a_zsr(6,je)  = -3.19164
      aw_min(je)   = 0.1
!
! nh4hso4
      je = jnh4hso4
      a_zsr(1,je)  =  1.15510
      a_zsr(2,je)  = -3.20815
      a_zsr(3,je)  =  2.71141
      a_zsr(4,je)  =  2.01155
      a_zsr(5,je)  = -4.71014
      a_zsr(6,je)  =  2.04616
      aw_min(je)   = 0.1
!
! nh4no3
      je = jnh4no3
      a_zsr(1,je)  =  0.43507
      a_zsr(2,je)  =  6.38220
      a_zsr(3,je)  = -30.19797
      a_zsr(4,je)  =  53.36470
      a_zsr(5,je)  = -43.44203
      a_zsr(6,je)  =  13.46158
      aw_min(je)   = 0.1
!
! nh4cl: revised on nov 13, 2003. based on chan and ha (1999) jgr.
      je = jnh4cl
      a_zsr(1,je)  =  0.45309
      a_zsr(2,je)  =  2.65606
      a_zsr(3,je)  = -14.7730
      a_zsr(4,je)  =  26.2936
      a_zsr(5,je)  = -20.5735
      a_zsr(6,je)  =  5.94255
      aw_min(je)   = 0.1
!
! nacl
      je = jnacl
      a_zsr(1,je)  =  0.42922
      a_zsr(2,je)  = -1.17718
      a_zsr(3,je)  =  2.80208
      a_zsr(4,je)  = -4.51097
      a_zsr(5,je)  =  3.76963
      a_zsr(6,je)  = -1.31359
      aw_min(je)   = 0.1
!
! nano3
      je = jnano3
      a_zsr(1,je)  =  1.34966
      a_zsr(2,je)  = -5.20116
      a_zsr(3,je)  =  11.49011
      a_zsr(4,je)  = -14.41380
      a_zsr(5,je)  =  9.07037
      a_zsr(6,je)  = -2.29769
      aw_min(je)   = 0.1
!
! na2so4
      je = jna2so4
      a_zsr(1,je)  =  0.39888
      a_zsr(2,je)  = -1.27150
      a_zsr(3,je)  =  3.42792
      a_zsr(4,je)  = -5.92632
      a_zsr(5,je)  =  5.33351
      a_zsr(6,je)  = -1.96541
      aw_min(je)   = 0.1
!
! na3h(so4)2  added on 1/14/2004
      je = jna3hso4
      a_zsr(1,je)  =  0.31480
      a_zsr(2,je)  = -1.01087
      a_zsr(3,je)  =  2.44029
      a_zsr(4,je)  = -3.66095
      a_zsr(5,je)  =  2.77632
      a_zsr(6,je)  = -0.86058
      aw_min(je)   = 0.1
!
! nahso4
      je = jnahso4
      a_zsr(1,je)  =  0.62764
      a_zsr(2,je)  = -1.63520
      a_zsr(3,je)  =  4.62531
      a_zsr(4,je)  = -10.06925
      a_zsr(5,je)  =  10.33547
      a_zsr(6,je)  = -3.88729
      aw_min(je)   = 0.1
!
! ca(no3)2
      je = jcano3
      a_zsr(1,je)  =  0.38895
      a_zsr(2,je)  = -1.16013
      a_zsr(3,je)  =  2.16819
      a_zsr(4,je)  = -2.23079
      a_zsr(5,je)  =  1.00268
      a_zsr(6,je)  = -0.16923
      aw_min(je)   = 0.1
!
! cacl2: kim and seinfeld
      je = jcacl2
      a_zsr(1,je)  =  0.29891
      a_zsr(2,je)  = -1.31104
      a_zsr(3,je)  =  3.68759
      a_zsr(4,je)  = -5.81708
      a_zsr(5,je)  =  4.67520
      a_zsr(6,je)  = -1.53223
      aw_min(je)   = 0.1
!
! h2so4
      je = jh2so4
      a_zsr(1,je) =  0.32751
      a_zsr(2,je) = -1.00692
      a_zsr(3,je) =  2.59750
      a_zsr(4,je) = -4.40014
      a_zsr(5,je) =  3.88212
      a_zsr(6,je) = -1.39916
      aw_min(je)  = 0.1
!
! hhso4
      je = jhhso4
      a_zsr(1,je) =  0.32751
      a_zsr(2,je) = -1.00692
      a_zsr(3,je) =  2.59750
      a_zsr(4,je) = -4.40014
      a_zsr(5,je) =  3.88212
      a_zsr(6,je) = -1.39916
      aw_min(je)  = 1.0
!
! hno3
      je = jhno3
      a_zsr(1,je) =  0.75876
      a_zsr(2,je) = -3.31529
      a_zsr(3,je) =  9.26392
      a_zsr(4,je) = -14.89799
      a_zsr(5,je) =  12.08781
      a_zsr(6,je) = -3.89958
      aw_min(je)  = 0.1
!
! hcl
      je = jhcl
      a_zsr(1,je) =  0.31133
      a_zsr(2,je) = -0.79688
      a_zsr(3,je) =  1.93995
      a_zsr(4,je) = -3.31582
      a_zsr(5,je) =  2.93513
      a_zsr(6,je) = -1.07268
      aw_min(je)  = 0.1
!
! caso4
      je = jcaso4
      a_zsr(1,je)  =  0.0
      a_zsr(2,je)  =  0.0
      a_zsr(3,je)  =  0.0
      a_zsr(4,je)  =  0.0
      a_zsr(5,je)  =  0.0
      a_zsr(6,je)  =  0.0
      aw_min(je)   = 1.0
!
! caco3
      je = jcaco3
      a_zsr(1,je)  =  0.0
      a_zsr(2,je)  =  0.0
      a_zsr(3,je)  =  0.0
      a_zsr(4,je)  =  0.0
      a_zsr(5,je)  =  0.0
      a_zsr(6,je)  =  0.0
      aw_min(je)   = 1.0



!-------------------------------------------
! b_zsr for aw => 0.97 to 0.99999
!
! (nh4)2so4
      b_zsr(jnh4so4)  = 28.0811
!
! (nh4)3h(so4)2
      b_zsr(jlvcite)  = 14.7178
!
! nh4hso4
      b_zsr(jnh4hso4) = 29.4779
!
! nh4no3
      b_zsr(jnh4no3)  = 33.4049
!
! nh4cl
      b_zsr(jnh4cl)   = 30.8888
!
! nacl
      b_zsr(jnacl)    = 29.8375
!
! nano3
      b_zsr(jnano3)   = 32.2756
!
! na2so4
      b_zsr(jna2so4)  = 27.6889
!
! na3h(so4)2
      b_zsr(jna3hso4) = 14.2184
!
! nahso4
      b_zsr(jnahso4)  = 28.3367
!
! ca(no3)2
      b_zsr(jcano3)   = 18.3661
!
! cacl2
      b_zsr(jcacl2)   = 20.8792
!
! h2so4
      b_zsr(jh2so4)   = 26.7347
!
! hhso4
      b_zsr(jhhso4)   = 26.7347
!
! hno3
      b_zsr(jhno3)    = 28.8257
!
! hcl
      b_zsr(jhcl)     = 27.7108
!
! caso4
      b_zsr(jcaso4)   = 0.0
!
! caco3
      b_zsr(jcaco3)   = 0.0






!-----------------------------------------------------------
! coefficients for activity coefficient polynomials (kusik and meissner)
!
!     log(gamma0) = b1*im^0.5 + b2*im + b3*im^2 + b4*im^3 + b5*im^4
!
! revised coefficients on nov 14, 2003.
!
!-----------------------------------------------------------
! nh4no3
      je = jnh4no3
      b_km(1,je)  = -0.267009
      b_km(2,je)  =  0.0202668
      b_km(3,je)  = -0.0000942908
      b_km(4,je)  =  3.07824e-07
      b_km(5,je)  = -3.73474e-10
      im_max(je)=  356.62
!
! nh4cl
      je = jnh4cl
      b_km(1,je)  = -0.266786
      b_km(2,je)  =  0.0777832
      b_km(3,je)  = -0.00217383
      b_km(4,je)  =  3.16802e-05
      b_km(5,je)  = -1.69576e-07
      im_max(je)=  74.4
!
! (nh4)2so4
      je = jnh4so4
      b_km(1,je)  = -0.519781
      b_km(2,je)  =  0.0804842
      b_km(3,je)  = -0.00072761
      b_km(4,je)  =  4.27569e-06
      b_km(5,je)  = -9.84832e-09
      im_max(je)=  162.84
!
! nacl
      je = jnacl
      b_km(1,je)  = -0.319457
      b_km(2,je)  =  0.140217
      b_km(3,je)  = -0.00171166
      b_km(4,je)  = -5.85982e-06
      b_km(5,je)  = 2.88269e-07
      im_max(je)=  28.01
!
! nano3
      je = jnano3
      b_km(1,je)  = -0.290263
      b_km(2,je)  =  0.0493945
      b_km(3,je)  = -0.000741747
      b_km(4,je)  =  6.81616e-06
      b_km(5,je)  = -2.38311e-08
      im_max(je)=  111.43
!
! na2so4
      je = jna2so4
      b_km(1,je)  = -0.58568
      b_km(2,je)  =  0.107867
      b_km(3,je)  = -0.000900192
      b_km(4,je)  =  7.4922e-06
      b_km(5,je)  = -4.72199e-08
      im_max(je)=  53.22
!
! cacl2 (psc92: revised on 11/27/2003)
      je = jcacl2
      b_km(1,je)  = -0.531123
      b_km(2,je)  =  0.206408
      b_km(3,je)  = -0.00109233
      b_km(4,je)  =  -2.62e-06
      b_km(5,je)  = -2.95e-07
      im_max(je)=  28.16
!
! ca(no3)2
      je = jcano3
      b_km(1,je)  = -0.522974
      b_km(2,je)  =  0.15075
      b_km(3,je)  = -0.00298182
      b_km(4,je)  =  5.01151e-05
      b_km(5,je)  = -3.82811e-07
      im_max(je)=  49.01
!
! hno3
      je = jhno3
      b_km(1,je)  = -0.318267
      b_km(2,je)  =  0.187029
      b_km(3,je)  = -0.00815311
      b_km(4,je)  =  0.000218321
      b_km(5,je)  = -2.41756e-06
      im_max(je)=  31.93
!
! hcl
      je = jhcl
      b_km(1,je)  = -0.323059
      b_km(2,je)  =  0.235552
      b_km(3,je)  = -0.00350478
      b_km(4,je)  =  6.42091e-05
      b_km(5,je)  = -1.48899e-06
      im_max(je)=  18.3
!
! h2so4
      je = jh2so4
      b_km(1,je)  = -0.7185
      b_km(2,je)  = 0.1962
      b_km(3,je)  = 2.5783e-3
      b_km(4,je)  = -2.1450e-4
      b_km(5,je)  = 2.7901e-6
      im_max(je)= 22.91
!
! hhso4
      je = jhhso4
      b_km(1,je)  = -0.2312
      b_km(2,je)  = 0.1321
      b_km(3,je)  = 4.9554e-3
      b_km(4,je)  = -2.7074e-4
      b_km(5,je)  = 3.1003e-6
      im_max(je)= 22.91
!
! nh4hso4
      je = jnh4hso4
      b_km(1,je)  = -0.1976
      b_km(2,je)  = 0.0445
      b_km(3,je)  = -7.8636e-4
      b_km(4,je)  = 6.6343e-6
      b_km(5,je)  = -1.9562e-8
      im_max(je)= 146.5
!
! (nh4)3h(so4)2
      je = jlvcite
      b_km(1,je)  = -0.3935
      b_km(2,je)  = 0.0591
      b_km(3,je)  = -5.5720e-4
      b_km(4,je)  = 3.1519e-6
      b_km(5,je)  = -6.6097e-9
      im_max(je)= 197.0
!
! nahso4
      je = jnahso4
      b_km(1,je)  = -0.2961
      b_km(2,je)  = 0.1238
      b_km(3,je)  = -5.1698e-3
      b_km(4,je)  = 1.2267e-4
      b_km(5,je)  = -1.0722e-6
      im_max(je)= 43.59
!
! na3h(so4)2
      je = jna3hso4
      b_km(1,je)  = -0.4688
      b_km(2,je)  = 0.1020
      b_km(3,je)  = -1.7013e-3
      b_km(4,je)  = 2.9740e-5
      b_km(5,je)  = -2.3703e-7
      im_max(je)= 50.44


!----------------------------------------------------------------
! parameters for mtem mixing rule (zaveri, easter, and wexler, 2005)
! log_gamz(ja,je)   a in e
!----------------------------------------------------------------
!
! (nh4)2so4 in e
      ja = jnh4so4

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.94685
      b_mtem(2,ja,je) = 17.3328
      b_mtem(3,ja,je) = -64.8441
      b_mtem(4,ja,je) = 122.7070
      b_mtem(5,ja,je) = -114.4373
      b_mtem(6,ja,je) = 41.6811

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -2.7503
      b_mtem(2,ja,je) = 4.3806
      b_mtem(3,ja,je) = -1.1110
      b_mtem(4,ja,je) = -1.7005
      b_mtem(5,ja,je) = -4.4207
      b_mtem(6,ja,je) = 5.1990

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -2.06952
      b_mtem(2,ja,je) = 7.1240
      b_mtem(3,ja,je) = -24.4274
      b_mtem(4,ja,je) = 51.1458
      b_mtem(5,ja,je) = -54.2056
      b_mtem(6,ja,je) = 22.0606

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -2.17361
      b_mtem(2,ja,je) = 15.9919
      b_mtem(3,ja,je) = -69.0952
      b_mtem(4,ja,je) = 139.8860
      b_mtem(5,ja,je) = -134.9890
      b_mtem(6,ja,je) = 49.8877

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -4.4370
      b_mtem(2,ja,je) = 24.0243
      b_mtem(3,ja,je) = -76.2437
      b_mtem(4,ja,je) = 128.6660
      b_mtem(5,ja,je) = -110.0900
      b_mtem(6,ja,je) = 37.7414

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = -1.5394
      b_mtem(2,ja,je) = 5.8671
      b_mtem(3,ja,je) = -22.7726
      b_mtem(4,ja,je) = 47.0547
      b_mtem(5,ja,je) = -47.8266
      b_mtem(6,ja,je) = 18.8489

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = -0.35750
      b_mtem(2,ja,je) = -3.82466
      b_mtem(3,ja,je) = 4.55462
      b_mtem(4,ja,je) = 5.05402
      b_mtem(5,ja,je) = -14.7476
      b_mtem(6,ja,je) = 8.8009

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = -2.15146
      b_mtem(2,ja,je) = 5.50205
      b_mtem(3,ja,je) = -19.1476
      b_mtem(4,ja,je) = 39.1880
      b_mtem(5,ja,je) = -39.9460
      b_mtem(6,ja,je) = 16.0700

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = -2.52604
      b_mtem(2,ja,je) = 9.76022
      b_mtem(3,ja,je) = -35.2540
      b_mtem(4,ja,je) = 71.2981
      b_mtem(5,ja,je) = -71.8207
      b_mtem(6,ja,je) = 28.0758

!
! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -4.13219
      b_mtem(2,ja,je) = 13.8863
      b_mtem(3,ja,je) = -34.5387
      b_mtem(4,ja,je) = 56.5012
      b_mtem(5,ja,je) = -51.8702
      b_mtem(6,ja,je) = 19.6232

!
! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -2.53482
      b_mtem(2,ja,je) = 12.3333
      b_mtem(3,ja,je) = -46.1020
      b_mtem(4,ja,je) = 90.4775
      b_mtem(5,ja,je) = -88.1254
      b_mtem(6,ja,je) = 33.4715

!
! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -3.23425
      b_mtem(2,ja,je) = 18.7842
      b_mtem(3,ja,je) = -78.7807
      b_mtem(4,ja,je) = 161.517
      b_mtem(5,ja,je) = -154.940
      b_mtem(6,ja,je) = 56.2252

!
! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -1.25316
      b_mtem(2,ja,je) = 7.40960
      b_mtem(3,ja,je) = -34.8929
      b_mtem(4,ja,je) = 72.8853
      b_mtem(5,ja,je) = -72.4503
      b_mtem(6,ja,je) = 27.7706


!-----------------
! nh4no3 in e
      ja = jnh4no3

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -3.5201
      b_mtem(2,ja,je) = 21.6584
      b_mtem(3,ja,je) = -72.1499
      b_mtem(4,ja,je) = 126.7000
      b_mtem(5,ja,je) = -111.4550
      b_mtem(6,ja,je) = 38.5677

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -2.2630
      b_mtem(2,ja,je) = -0.1518
      b_mtem(3,ja,je) = 17.0898
      b_mtem(4,ja,je) = -36.7832
      b_mtem(5,ja,je) = 29.8407
      b_mtem(6,ja,je) = -7.9314

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -1.3851
      b_mtem(2,ja,je) = -0.4462
      b_mtem(3,ja,je) = 8.4567
      b_mtem(4,ja,je) = -11.5988
      b_mtem(5,ja,je) = 2.9802
      b_mtem(6,ja,je) = 1.8132

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -1.7602
      b_mtem(2,ja,je) = 10.4044
      b_mtem(3,ja,je) = -35.5894
      b_mtem(4,ja,je) = 64.3584
      b_mtem(5,ja,je) = -57.8931
      b_mtem(6,ja,je) = 20.2141

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -3.24346
      b_mtem(2,ja,je) = 16.2794
      b_mtem(3,ja,je) = -48.7601
      b_mtem(4,ja,je) = 79.2246
      b_mtem(5,ja,je) = -65.8169
      b_mtem(6,ja,je) = 22.1500

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = -1.75658
      b_mtem(2,ja,je) = 7.71384
      b_mtem(3,ja,je) = -22.7984
      b_mtem(4,ja,je) = 39.1532
      b_mtem(5,ja,je) = -34.6165
      b_mtem(6,ja,je) = 12.1283

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = -0.97178
      b_mtem(2,ja,je) = 6.61964
      b_mtem(3,ja,je) = -26.2353
      b_mtem(4,ja,je) = 50.5259
      b_mtem(5,ja,je) = -47.6586
      b_mtem(6,ja,je) = 17.5074

! in cacl2 added on 12/22/2003
      je = jcacl2
      b_mtem(1,ja,je) = -0.41515
      b_mtem(2,ja,je) = 6.44101
      b_mtem(3,ja,je) = -26.4473
      b_mtem(4,ja,je) = 49.0718
      b_mtem(5,ja,je) = -44.2631
      b_mtem(6,ja,je) = 15.3771

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = -1.20644
      b_mtem(2,ja,je) = 5.70117
      b_mtem(3,ja,je) = -18.2783
      b_mtem(4,ja,je) = 31.7199
      b_mtem(5,ja,je) = -27.8703
      b_mtem(6,ja,je) = 9.7299

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = -0.680862
      b_mtem(2,ja,je) = 3.59456
      b_mtem(3,ja,je) = -10.7969
      b_mtem(4,ja,je) = 17.8434
      b_mtem(5,ja,je) = -15.3165
      b_mtem(6,ja,je) = 5.17123


!----------
! nh4cl in e
      ja = jnh4cl

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.8850
      b_mtem(2,ja,je) = 20.6970
      b_mtem(3,ja,je) = -70.6810
      b_mtem(4,ja,je) = 124.3690
      b_mtem(5,ja,je) = -109.2880
      b_mtem(6,ja,je) = 37.5831

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -1.9386
      b_mtem(2,ja,je) = 1.3238
      b_mtem(3,ja,je) = 11.8500
      b_mtem(4,ja,je) = -28.1168
      b_mtem(5,ja,je) = 21.8543
      b_mtem(6,ja,je) = -5.1671

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -0.9559
      b_mtem(2,ja,je) = 0.8121
      b_mtem(3,ja,je) = 4.3644
      b_mtem(4,ja,je) = -8.9258
      b_mtem(5,ja,je) = 4.2362
      b_mtem(6,ja,je) = 0.2891

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 0.0377
      b_mtem(2,ja,je) = 6.0752
      b_mtem(3,ja,je) = -30.8641
      b_mtem(4,ja,je) = 63.3095
      b_mtem(5,ja,je) = -61.0070
      b_mtem(6,ja,je) = 22.1734

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -1.8336
      b_mtem(2,ja,je) = 12.8160
      b_mtem(3,ja,je) = -42.3388
      b_mtem(4,ja,je) = 71.1816
      b_mtem(5,ja,je) = -60.5708
      b_mtem(6,ja,je) = 20.5853

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = -0.1429
      b_mtem(2,ja,je) = 2.3561
      b_mtem(3,ja,je) = -10.4425
      b_mtem(4,ja,je) = 20.8951
      b_mtem(5,ja,je) = -20.7739
      b_mtem(6,ja,je) = 7.9355

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = 0.76235
      b_mtem(2,ja,je) = 3.08323
      b_mtem(3,ja,je) = -23.6772
      b_mtem(4,ja,je) = 53.7415
      b_mtem(5,ja,je) = -55.4043
      b_mtem(6,ja,je) = 21.2944

! in cacl2 (revised on 11/27/2003)
      je = jcacl2
      b_mtem(1,ja,je) = 1.13864
      b_mtem(2,ja,je) = -0.340539
      b_mtem(3,ja,je) = -8.67025
      b_mtem(4,ja,je) = 22.8008
      b_mtem(5,ja,je) = -24.5181
      b_mtem(6,ja,je) = 9.3663

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 2.42532
      b_mtem(2,ja,je) = -14.1755
      b_mtem(3,ja,je) = 38.804
      b_mtem(4,ja,je) = -58.2437
      b_mtem(5,ja,je) = 43.5431
      b_mtem(6,ja,je) = -12.5824

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 0.330337
      b_mtem(2,ja,je) = 0.0778934
      b_mtem(3,ja,je) = -2.30492
      b_mtem(4,ja,je) = 4.73003
      b_mtem(5,ja,je) = -4.80849
      b_mtem(6,ja,je) = 1.78866


!----------
! na2so4 in e
      ja = jna2so4

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.6982
      b_mtem(2,ja,je) = 22.9875
      b_mtem(3,ja,je) = -98.9840
      b_mtem(4,ja,je) = 198.0180
      b_mtem(5,ja,je) = -188.7270
      b_mtem(6,ja,je) = 69.0548

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -2.4844
      b_mtem(2,ja,je) = 6.5420
      b_mtem(3,ja,je) = -9.8998
      b_mtem(4,ja,je) = 11.3884
      b_mtem(5,ja,je) = -13.6842
      b_mtem(6,ja,je) = 7.7411

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -1.3325
      b_mtem(2,ja,je) = 13.0406
      b_mtem(3,ja,je) = -56.1935
      b_mtem(4,ja,je) = 107.1170
      b_mtem(5,ja,je) = -97.3721
      b_mtem(6,ja,je) = 34.3763

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -1.2832
      b_mtem(2,ja,je) = 12.8526
      b_mtem(3,ja,je) = -62.2087
      b_mtem(4,ja,je) = 130.3876
      b_mtem(5,ja,je) = -128.2627
      b_mtem(6,ja,je) = 48.0340

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -3.5384
      b_mtem(2,ja,je) = 21.3758
      b_mtem(3,ja,je) = -70.7638
      b_mtem(4,ja,je) = 121.1580
      b_mtem(5,ja,je) = -104.6230
      b_mtem(6,ja,je) = 36.0557

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = 0.2175
      b_mtem(2,ja,je) = -0.5648
      b_mtem(3,ja,je) = -8.0288
      b_mtem(4,ja,je) = 25.9734
      b_mtem(5,ja,je) = -32.3577
      b_mtem(6,ja,je) = 14.3924

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = -0.309617
      b_mtem(2,ja,je) = -1.82899
      b_mtem(3,ja,je) = -1.5505
      b_mtem(4,ja,je) = 13.3847
      b_mtem(5,ja,je) = -20.1284
      b_mtem(6,ja,je) = 9.93163

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = -0.259455
      b_mtem(2,ja,je) = -0.819366
      b_mtem(3,ja,je) = -4.28964
      b_mtem(4,ja,je) = 16.4305
      b_mtem(5,ja,je) = -21.8546
      b_mtem(6,ja,je) = 10.3044

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = -1.84257
      b_mtem(2,ja,je) = 7.85788
      b_mtem(3,ja,je) = -29.9275
      b_mtem(4,ja,je) = 61.7515
      b_mtem(5,ja,je) = -63.2308
      b_mtem(6,ja,je) = 24.9542

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -1.05891
      b_mtem(2,ja,je) = 2.84831
      b_mtem(3,ja,je) = -21.1827
      b_mtem(4,ja,je) = 57.5175
      b_mtem(5,ja,je) = -64.8120
      b_mtem(6,ja,je) = 26.1986

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -1.16584
      b_mtem(2,ja,je) = 8.50075
      b_mtem(3,ja,je) = -44.3420
      b_mtem(4,ja,je) = 97.3974
      b_mtem(5,ja,je) = -98.4549
      b_mtem(6,ja,je) = 37.6104

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -1.95805
      b_mtem(2,ja,je) = 6.62417
      b_mtem(3,ja,je) = -31.8072
      b_mtem(4,ja,je) = 77.8603
      b_mtem(5,ja,je) = -84.6458
      b_mtem(6,ja,je) = 33.4963

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -0.36045
      b_mtem(2,ja,je) = 3.55223
      b_mtem(3,ja,je) = -24.0327
      b_mtem(4,ja,je) = 54.4879
      b_mtem(5,ja,je) = -56.6531
      b_mtem(6,ja,je) = 22.4956


!----------
! nano3 in e
      ja = jnano3

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.5888
      b_mtem(2,ja,je) = 17.6192
      b_mtem(3,ja,je) = -63.2183
      b_mtem(4,ja,je) = 115.3520
      b_mtem(5,ja,je) = -104.0860
      b_mtem(6,ja,je) = 36.7390

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -2.0669
      b_mtem(2,ja,je) = 1.4792
      b_mtem(3,ja,je) = 10.5261
      b_mtem(4,ja,je) = -27.0987
      b_mtem(5,ja,je) = 23.0591
      b_mtem(6,ja,je) = -6.0938

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -0.8325
      b_mtem(2,ja,je) = 3.9933
      b_mtem(3,ja,je) = -15.3789
      b_mtem(4,ja,je) = 30.4050
      b_mtem(5,ja,je) = -29.4204
      b_mtem(6,ja,je) = 11.0597

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -1.1233
      b_mtem(2,ja,je) = 8.3998
      b_mtem(3,ja,je) = -31.9002
      b_mtem(4,ja,je) = 60.1450
      b_mtem(5,ja,je) = -55.5503
      b_mtem(6,ja,je) = 19.7757

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -2.5386
      b_mtem(2,ja,je) = 13.9039
      b_mtem(3,ja,je) = -42.8467
      b_mtem(4,ja,je) = 69.7442
      b_mtem(5,ja,je) = -57.8988
      b_mtem(6,ja,je) = 19.4635

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = -0.4351
      b_mtem(2,ja,je) = 2.8311
      b_mtem(3,ja,je) = -11.4485
      b_mtem(4,ja,je) = 22.7201
      b_mtem(5,ja,je) = -22.4228
      b_mtem(6,ja,je) = 8.5792

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = -0.72060
      b_mtem(2,ja,je) = 5.64915
      b_mtem(3,ja,je) = -23.5020
      b_mtem(4,ja,je) = 46.0078
      b_mtem(5,ja,je) = -43.8075
      b_mtem(6,ja,je) = 16.1652

! in cacl2
      je = jcacl2
      b_mtem(1,ja,je) = 0.003928
      b_mtem(2,ja,je) = 3.54724
      b_mtem(3,ja,je) = -18.6057
      b_mtem(4,ja,je) = 38.1445
      b_mtem(5,ja,je) = -36.7745
      b_mtem(6,ja,je) = 13.4529

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = -1.1712
      b_mtem(2,ja,je) = 7.20907
      b_mtem(3,ja,je) = -22.9215
      b_mtem(4,ja,je) = 38.1257
      b_mtem(5,ja,je) = -32.0759
      b_mtem(6,ja,je) = 10.6443

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 0.738022
      b_mtem(2,ja,je) = -1.14313
      b_mtem(3,ja,je) = 0.32251
      b_mtem(4,ja,je) = 0.838679
      b_mtem(5,ja,je) = -1.81747
      b_mtem(6,ja,je) = 0.873986


!----------
! nacl in e
      ja = jnacl

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -1.9525
      b_mtem(2,ja,je) = 16.6433
      b_mtem(3,ja,je) = -61.7090
      b_mtem(4,ja,je) = 112.9910
      b_mtem(5,ja,je) = -101.9370
      b_mtem(6,ja,je) = 35.7760

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -1.7525
      b_mtem(2,ja,je) = 3.0713
      b_mtem(3,ja,je) = 4.8063
      b_mtem(4,ja,je) = -17.5334
      b_mtem(5,ja,je) = 14.2872
      b_mtem(6,ja,je) = -3.0690

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -0.4021
      b_mtem(2,ja,je) = 5.2399
      b_mtem(3,ja,je) = -19.4278
      b_mtem(4,ja,je) = 33.0027
      b_mtem(5,ja,je) = -28.1020
      b_mtem(6,ja,je) = 9.5159

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 0.6692
      b_mtem(2,ja,je) = 4.1207
      b_mtem(3,ja,je) = -27.3314
      b_mtem(4,ja,je) = 59.3112
      b_mtem(5,ja,je) = -58.7998
      b_mtem(6,ja,je) = 21.7674

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -1.17444
      b_mtem(2,ja,je) = 10.9927
      b_mtem(3,ja,je) = -38.9013
      b_mtem(4,ja,je) = 66.8521
      b_mtem(5,ja,je) = -57.6564
      b_mtem(6,ja,je) = 19.7296

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = 1.17679
      b_mtem(2,ja,je) = -2.5061
      b_mtem(3,ja,je) = 0.8508
      b_mtem(4,ja,je) = 4.4802
      b_mtem(5,ja,je) = -8.4945
      b_mtem(6,ja,je) = 4.3182

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = 1.01450
      b_mtem(2,ja,je) = 2.10260
      b_mtem(3,ja,je) = -20.9036
      b_mtem(4,ja,je) = 49.1481
      b_mtem(5,ja,je) = -51.4867
      b_mtem(6,ja,je) = 19.9301

! in cacl2 (psc92: revised on 11/27/2003)
      je = jcacl2
      b_mtem(1,ja,je) = 1.55463
      b_mtem(2,ja,je) = -3.20122
      b_mtem(3,ja,je) = -0.957075
      b_mtem(4,ja,je) = 12.103
      b_mtem(5,ja,je) = -17.221
      b_mtem(6,ja,je) = 7.50264

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 2.46187
      b_mtem(2,ja,je) = -12.6845
      b_mtem(3,ja,je) = 34.2383
      b_mtem(4,ja,je) = -51.9992
      b_mtem(5,ja,je) = 39.4934
      b_mtem(6,ja,je) = -11.7247

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 1.74915
      b_mtem(2,ja,je) = -4.65768
      b_mtem(3,ja,je) = 8.80287
      b_mtem(4,ja,je) = -12.2503
      b_mtem(5,ja,je) = 8.668751
      b_mtem(6,ja,je) = -2.50158


!----------
! ca(no3)2 in e
      ja = jcano3

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -1.86260
      b_mtem(2,ja,je) = 11.6178
      b_mtem(3,ja,je) = -30.9069
      b_mtem(4,ja,je) = 41.7578
      b_mtem(5,ja,je) = -33.7338
      b_mtem(6,ja,je) = 12.7541

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -1.1798
      b_mtem(2,ja,je) = 25.9608
      b_mtem(3,ja,je) = -98.9373
      b_mtem(4,ja,je) = 160.2300
      b_mtem(5,ja,je) = -125.9540
      b_mtem(6,ja,je) = 39.5130

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -1.44384
      b_mtem(2,ja,je) = 13.6044
      b_mtem(3,ja,je) = -54.4300
      b_mtem(4,ja,je) = 100.582
      b_mtem(5,ja,je) = -91.2364
      b_mtem(6,ja,je) = 32.5970

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = -0.099114
      b_mtem(2,ja,je) = 2.84091
      b_mtem(3,ja,je) = -16.9229
      b_mtem(4,ja,je) = 37.4839
      b_mtem(5,ja,je) = -39.5132
      b_mtem(6,ja,je) = 15.8564

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = 0.055116
      b_mtem(2,ja,je) = 4.58610
      b_mtem(3,ja,je) = -27.6629
      b_mtem(4,ja,je) = 60.8288
      b_mtem(5,ja,je) = -61.4988
      b_mtem(6,ja,je) = 23.3136

! in cacl2 (psc92: revised on 11/27/2003)
      je = jcacl2
      b_mtem(1,ja,je) = 1.57155
      b_mtem(2,ja,je) = -3.18486
      b_mtem(3,ja,je) = -3.35758
      b_mtem(4,ja,je) = 18.7501
      b_mtem(5,ja,je) = -24.5604
      b_mtem(6,ja,je) = 10.3798

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 1.04446
      b_mtem(2,ja,je) = -3.19066
      b_mtem(3,ja,je) = 2.44714
      b_mtem(4,ja,je) = 2.07218
      b_mtem(5,ja,je) = -6.43949
      b_mtem(6,ja,je) = 3.66471

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 1.05723
      b_mtem(2,ja,je) = -1.46826
      b_mtem(3,ja,je) = -1.0713
      b_mtem(4,ja,je) = 4.64439
      b_mtem(5,ja,je) = -6.32402
      b_mtem(6,ja,je) = 2.78202


!----------
! cacl2 in e
      ja = jcacl2

! in nh4no3 (psc92: revised on 12/22/2003)
      je = jnh4no3
      b_mtem(1,ja,je) = -1.43626
      b_mtem(2,ja,je) = 13.6598
      b_mtem(3,ja,je) = -38.2068
      b_mtem(4,ja,je) = 53.9057
      b_mtem(5,ja,je) = -44.9018
      b_mtem(6,ja,je) = 16.6120

! in nh4cl (psc92: revised on 11/27/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -0.603965
      b_mtem(2,ja,je) = 27.6027
      b_mtem(3,ja,je) = -104.258
      b_mtem(4,ja,je) = 163.553
      b_mtem(5,ja,je) = -124.076
      b_mtem(6,ja,je) = 37.4153

! in nano3 (psc92: revised on 12/22/2003)
      je = jnano3
      b_mtem(1,ja,je) = 0.44648
      b_mtem(2,ja,je) = 8.8850
      b_mtem(3,ja,je) = -45.5232
      b_mtem(4,ja,je) = 89.3263
      b_mtem(5,ja,je) = -83.8604
      b_mtem(6,ja,je) = 30.4069

! in nacl (psc92: revised on 11/27/2003)
      je = jnacl
      b_mtem(1,ja,je) = 1.61927
      b_mtem(2,ja,je) = 0.247547
      b_mtem(3,ja,je) = -18.1252
      b_mtem(4,ja,je) = 45.2479
      b_mtem(5,ja,je) = -48.6072
      b_mtem(6,ja,je) = 19.2784

! in ca(no3)2 (psc92: revised on 11/27/2003)
      je = jcano3
      b_mtem(1,ja,je) = 2.36667
      b_mtem(2,ja,je) = -0.123309
      b_mtem(3,ja,je) = -24.2723
      b_mtem(4,ja,je) = 65.1486
      b_mtem(5,ja,je) = -71.8504
      b_mtem(6,ja,je) = 28.3696

! in cacl2 (psc92: revised on 11/27/2003)
      je = jcacl2
      b_mtem(1,ja,je) = 3.64023
      b_mtem(2,ja,je) = -12.1926
      b_mtem(3,ja,je) = 20.2028
      b_mtem(4,ja,je) = -16.0056
      b_mtem(5,ja,je) = 1.52355
      b_mtem(6,ja,je) = 2.44709

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 5.88794
      b_mtem(2,ja,je) = -29.7083
      b_mtem(3,ja,je) = 78.6309
      b_mtem(4,ja,je) = -118.037
      b_mtem(5,ja,je) = 88.932
      b_mtem(6,ja,je) = -26.1407

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 2.40628
      b_mtem(2,ja,je) = -6.16566
      b_mtem(3,ja,je) = 10.2851
      b_mtem(4,ja,je) = -12.9035
      b_mtem(5,ja,je) = 7.7441
      b_mtem(6,ja,je) = -1.74821


!----------
! hno3 in e
      ja = jhno3

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -3.57598
      b_mtem(2,ja,je) = 21.5469
      b_mtem(3,ja,je) = -77.4111
      b_mtem(4,ja,je) = 144.136
      b_mtem(5,ja,je) = -132.849
      b_mtem(6,ja,je) = 47.9412

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -2.00209
      b_mtem(2,ja,je) = -3.48399
      b_mtem(3,ja,je) = 34.9906
      b_mtem(4,ja,je) = -68.6653
      b_mtem(5,ja,je) = 54.0992
      b_mtem(6,ja,je) = -15.1343

! in nh4cl revised on 12/22/2003
      je = jnh4cl
      b_mtem(1,ja,je) = -0.63790
      b_mtem(2,ja,je) = -1.67730
      b_mtem(3,ja,je) = 10.1727
      b_mtem(4,ja,je) = -14.9097
      b_mtem(5,ja,je) = 7.67410
      b_mtem(6,ja,je) = -0.79586

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = 1.3446
      b_mtem(2,ja,je) = -2.5578
      b_mtem(3,ja,je) = 1.3464
      b_mtem(4,ja,je) = 2.90537
      b_mtem(5,ja,je) = -6.53014
      b_mtem(6,ja,je) = 3.31339

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = -0.546636
      b_mtem(2,ja,je) = 10.3127
      b_mtem(3,ja,je) = -39.9603
      b_mtem(4,ja,je) = 71.4609
      b_mtem(5,ja,je) = -63.4958
      b_mtem(6,ja,je) = 22.0679

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 1.35059
      b_mtem(2,ja,je) = 4.34557
      b_mtem(3,ja,je) = -35.8425
      b_mtem(4,ja,je) = 80.9868
      b_mtem(5,ja,je) = -81.6544
      b_mtem(6,ja,je) = 30.4841

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = 0.869414
      b_mtem(2,ja,je) = 2.98486
      b_mtem(3,ja,je) = -22.255
      b_mtem(4,ja,je) = 50.1863
      b_mtem(5,ja,je) = -51.214
      b_mtem(6,ja,je) = 19.2235

! in cacl2 (km) revised on 12/22/2003
      je = jcacl2
      b_mtem(1,ja,je) = 1.42800
      b_mtem(2,ja,je) = -1.78959
      b_mtem(3,ja,je) = -2.49075
      b_mtem(4,ja,je) = 10.1877
      b_mtem(5,ja,je) = -12.1948
      b_mtem(6,ja,je) = 4.64475

! in hno3 (added on 12/06/2004)
      je = jhno3
      b_mtem(1,ja,je) = 0.22035
      b_mtem(2,ja,je) = 2.94973
      b_mtem(3,ja,je) = -12.1469
      b_mtem(4,ja,je) = 20.4905
      b_mtem(5,ja,je) = -17.3966
      b_mtem(6,ja,je) = 5.70779

! in hcl (added on 12/06/2004)
      je = jhcl
      b_mtem(1,ja,je) = 1.55503
      b_mtem(2,ja,je) = -3.61226
      b_mtem(3,ja,je) = 6.28265
      b_mtem(4,ja,je) = -8.69575
      b_mtem(5,ja,je) = 6.09372
      b_mtem(6,ja,je) = -1.80898

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 1.10783
      b_mtem(2,ja,je) = -1.3363
      b_mtem(3,ja,je) = -1.83525
      b_mtem(4,ja,je) = 7.47373
      b_mtem(5,ja,je) = -9.72954
      b_mtem(6,ja,je) = 4.12248

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -0.851026
      b_mtem(2,ja,je) = 12.2515
      b_mtem(3,ja,je) = -49.788
      b_mtem(4,ja,je) = 91.6215
      b_mtem(5,ja,je) = -81.4877
      b_mtem(6,ja,je) = 28.0002

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -3.09464
      b_mtem(2,ja,je) = 14.9303
      b_mtem(3,ja,je) = -43.0454
      b_mtem(4,ja,je) = 72.6695
      b_mtem(5,ja,je) = -65.2140
      b_mtem(6,ja,je) = 23.4814

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = 1.22973
      b_mtem(2,ja,je) = 2.82702
      b_mtem(3,ja,je) = -17.5869
      b_mtem(4,ja,je) = 28.9564
      b_mtem(5,ja,je) = -23.5814
      b_mtem(6,ja,je) = 7.91153

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = 1.64773
      b_mtem(2,ja,je) = 0.94188
      b_mtem(3,ja,je) = -19.1242
      b_mtem(4,ja,je) = 46.9887
      b_mtem(5,ja,je) = -50.9494
      b_mtem(6,ja,je) = 20.2169


!----------
! hcl in e
      ja = jhcl

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.93783
      b_mtem(2,ja,je) = 20.5546
      b_mtem(3,ja,je) = -75.8548
      b_mtem(4,ja,je) = 141.729
      b_mtem(5,ja,je) = -130.697
      b_mtem(6,ja,je) = 46.9905

! in nh4no3
      je = jnh4no3
      b_mtem(1,ja,je) = -1.69063
      b_mtem(2,ja,je) = -1.85303
      b_mtem(3,ja,je) = 29.0927
      b_mtem(4,ja,je) = -58.7401
      b_mtem(5,ja,je) = 44.999
      b_mtem(6,ja,je) = -11.9988

! in nh4cl (revised on 11/15/2003)
      je = jnh4cl
      b_mtem(1,ja,je) = -0.2073
      b_mtem(2,ja,je) = -0.4322
      b_mtem(3,ja,je) = 6.1271
      b_mtem(4,ja,je) = -12.3146
      b_mtem(5,ja,je) = 8.9919
      b_mtem(6,ja,je) = -2.3388

! in nacl
      je = jnacl
      b_mtem(1,ja,je) = 2.95913
      b_mtem(2,ja,je) = -7.92254
      b_mtem(3,ja,je) = 13.736
      b_mtem(4,ja,je) = -15.433
      b_mtem(5,ja,je) = 7.40386
      b_mtem(6,ja,je) = -0.918641

! in nano3
      je = jnano3
      b_mtem(1,ja,je) = 0.893272
      b_mtem(2,ja,je) = 6.53768
      b_mtem(3,ja,je) = -32.3458
      b_mtem(4,ja,je) = 61.2834
      b_mtem(5,ja,je) = -56.4446
      b_mtem(6,ja,je) = 19.9202

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 3.14484
      b_mtem(2,ja,je) = 0.077019
      b_mtem(3,ja,je) = -31.4199
      b_mtem(4,ja,je) = 80.5865
      b_mtem(5,ja,je) = -85.392
      b_mtem(6,ja,je) = 32.6644

! in ca(no3)2
      je = jcano3
      b_mtem(1,ja,je) = 2.60432
      b_mtem(2,ja,je) = -0.55909
      b_mtem(3,ja,je) = -19.6671
      b_mtem(4,ja,je) = 53.3446
      b_mtem(5,ja,je) = -58.9076
      b_mtem(6,ja,je) = 22.9927

! in cacl2 (km) revised on 3/13/2003 and again on 11/27/2003
      je = jcacl2
      b_mtem(1,ja,je) = 2.98036
      b_mtem(2,ja,je) = -8.55365
      b_mtem(3,ja,je) = 15.2108
      b_mtem(4,ja,je) = -15.9359
      b_mtem(5,ja,je) = 7.41772
      b_mtem(6,ja,je) = -1.32143

! in hno3 (added on 12/06/2004)
      je = jhno3
      b_mtem(1,ja,je) = 3.8533
      b_mtem(2,ja,je) = -16.9427
      b_mtem(3,ja,je) = 45.0056
      b_mtem(4,ja,je) = -69.6145
      b_mtem(5,ja,je) = 54.1491
      b_mtem(6,ja,je) = -16.6513

! in hcl (added on 12/06/2004)
      je = jhcl
      b_mtem(1,ja,je) = 2.56665
      b_mtem(2,ja,je) = -7.13585
      b_mtem(3,ja,je) = 14.8103
      b_mtem(4,ja,je) = -21.8881
      b_mtem(5,ja,je) = 16.6808
      b_mtem(6,ja,je) = -5.22091

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 2.50179
      b_mtem(2,ja,je) = -6.69364
      b_mtem(3,ja,je) = 11.6551
      b_mtem(4,ja,je) = -13.6897
      b_mtem(5,ja,je) = 7.36796
      b_mtem(6,ja,je) = -1.33245

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = 0.149955
      b_mtem(2,ja,je) = 11.8213
      b_mtem(3,ja,je) = -53.9164
      b_mtem(4,ja,je) = 101.574
      b_mtem(5,ja,je) = -91.4123
      b_mtem(6,ja,je) = 31.5487

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -2.36927
      b_mtem(2,ja,je) = 14.8359
      b_mtem(3,ja,je) = -44.3443
      b_mtem(4,ja,je) = 73.6229
      b_mtem(5,ja,je) = -65.3366
      b_mtem(6,ja,je) = 23.3250

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = 2.72993
      b_mtem(2,ja,je) = -0.23406
      b_mtem(3,ja,je) = -10.4103
      b_mtem(4,ja,je) = 13.1586
      b_mtem(5,ja,je) = -7.79925
      b_mtem(6,ja,je) = 2.30843

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = 3.51258
      b_mtem(2,ja,je) = -3.95107
      b_mtem(3,ja,je) = -11.0175
      b_mtem(4,ja,je) = 38.8617
      b_mtem(5,ja,je) = -48.1575
      b_mtem(6,ja,je) = 20.4717


!----------
! 2h.so4 in e
      ja = jh2so4

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 0.76734
      b_mtem(2,ja,je) = -1.12263
      b_mtem(3,ja,je) = -9.08728
      b_mtem(4,ja,je) = 30.3836
      b_mtem(5,ja,je) = -38.4133
      b_mtem(6,ja,je) = 17.0106

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -2.03879
      b_mtem(2,ja,je) = 15.7033
      b_mtem(3,ja,je) = -58.7363
      b_mtem(4,ja,je) = 109.242
      b_mtem(5,ja,je) = -102.237
      b_mtem(6,ja,je) = 37.5350

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -3.10228
      b_mtem(2,ja,je) = 16.6920
      b_mtem(3,ja,je) = -59.1522
      b_mtem(4,ja,je) = 113.487
      b_mtem(5,ja,je) = -110.890
      b_mtem(6,ja,je) = 42.4578

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -3.43885
      b_mtem(2,ja,je) = 21.0372
      b_mtem(3,ja,je) = -84.7026
      b_mtem(4,ja,je) = 165.324
      b_mtem(5,ja,je) = -156.101
      b_mtem(6,ja,je) = 57.3101

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = 0.33164
      b_mtem(2,ja,je) = 6.55864
      b_mtem(3,ja,je) = -33.5876
      b_mtem(4,ja,je) = 65.1798
      b_mtem(5,ja,je) = -63.2046
      b_mtem(6,ja,je) = 24.1783

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = 3.06830
      b_mtem(2,ja,je) = -3.18408
      b_mtem(3,ja,je) = -19.6332
      b_mtem(4,ja,je) = 61.3657
      b_mtem(5,ja,je) = -73.4438
      b_mtem(6,ja,je) = 31.2334

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 2.58649
      b_mtem(2,ja,je) = 0.87921
      b_mtem(3,ja,je) = -39.3023
      b_mtem(4,ja,je) = 101.603
      b_mtem(5,ja,je) = -109.469
      b_mtem(6,ja,je) = 43.0188

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 1.54587
      b_mtem(2,ja,je) = -7.50976
      b_mtem(3,ja,je) = 12.8237
      b_mtem(4,ja,je) = -10.1452
      b_mtem(5,ja,je) = -0.541956
      b_mtem(6,ja,je) = 3.34536

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 0.829757
      b_mtem(2,ja,je) = -4.11316
      b_mtem(3,ja,je) = 3.67111
      b_mtem(4,ja,je) = 3.6833
      b_mtem(5,ja,je) = -11.2711
      b_mtem(6,ja,je) = 6.71421


!----------
! h.hso4 in e
      ja = jhhso4

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 2.63953
      b_mtem(2,ja,je) = -6.01532
      b_mtem(3,ja,je) = 10.0204
      b_mtem(4,ja,je) = -12.4840
      b_mtem(5,ja,je) = 7.78853
      b_mtem(6,ja,je) = -2.12638

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -0.77412
      b_mtem(2,ja,je) = 14.1656
      b_mtem(3,ja,je) = -53.4087
      b_mtem(4,ja,je) = 93.2013
      b_mtem(5,ja,je) = -80.5723
      b_mtem(6,ja,je) = 27.1577

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -2.98882
      b_mtem(2,ja,je) = 14.4436
      b_mtem(3,ja,je) = -40.1774
      b_mtem(4,ja,je) = 67.5937
      b_mtem(5,ja,je) = -61.5040
      b_mtem(6,ja,je) = 22.3695

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -1.15502
      b_mtem(2,ja,je) = 8.12309
      b_mtem(3,ja,je) = -38.4726
      b_mtem(4,ja,je) = 80.8861
      b_mtem(5,ja,je) = -80.1644
      b_mtem(6,ja,je) = 30.4717

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = 1.99641
      b_mtem(2,ja,je) = -2.96061
      b_mtem(3,ja,je) = 5.54778
      b_mtem(4,ja,je) = -14.5488
      b_mtem(5,ja,je) = 14.8492
      b_mtem(6,ja,je) = -5.1389

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = 2.23816
      b_mtem(2,ja,je) = -3.20847
      b_mtem(3,ja,je) = -4.82853
      b_mtem(4,ja,je) = 20.9192
      b_mtem(5,ja,je) = -27.2819
      b_mtem(6,ja,je) = 11.8655

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 2.56907
      b_mtem(2,ja,je) = 1.13444
      b_mtem(3,ja,je) = -34.6853
      b_mtem(4,ja,je) = 87.9775
      b_mtem(5,ja,je) = -93.2330
      b_mtem(6,ja,je) = 35.9260

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 2.00024
      b_mtem(2,ja,je) = -4.80868
      b_mtem(3,ja,je) = 8.29222
      b_mtem(4,ja,je) = -11.0849
      b_mtem(5,ja,je) = 7.51262
      b_mtem(6,ja,je) = -2.07654

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 2.8009
      b_mtem(2,ja,je) = -6.98416
      b_mtem(3,ja,je) = 14.3146
      b_mtem(4,ja,je) = -22.0068
      b_mtem(5,ja,je) = 17.5557
      b_mtem(6,ja,je) = -5.84917


!----------
! nh4hso4 in e
      ja = jnh4hso4

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 0.169160
      b_mtem(2,ja,je) = 2.15094
      b_mtem(3,ja,je) = -9.62904
      b_mtem(4,ja,je) = 18.2631
      b_mtem(5,ja,je) = -17.3333
      b_mtem(6,ja,je) = 6.19835

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -2.34457
      b_mtem(2,ja,je) = 12.8035
      b_mtem(3,ja,je) = -35.2513
      b_mtem(4,ja,je) = 53.6153
      b_mtem(5,ja,je) = -42.7655
      b_mtem(6,ja,je) = 13.7129

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -2.56109
      b_mtem(2,ja,je) = 11.1414
      b_mtem(3,ja,je) = -30.2361
      b_mtem(4,ja,je) = 50.0320
      b_mtem(5,ja,je) = -44.1586
      b_mtem(6,ja,je) = 15.5393

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -0.97315
      b_mtem(2,ja,je) = 7.06295
      b_mtem(3,ja,je) = -29.3032
      b_mtem(4,ja,je) = 57.6101
      b_mtem(5,ja,je) = -54.9020
      b_mtem(6,ja,je) = 20.2222

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -0.44450
      b_mtem(2,ja,je) = 3.33451
      b_mtem(3,ja,je) = -15.2791
      b_mtem(4,ja,je) = 30.1413
      b_mtem(5,ja,je) = -26.7710
      b_mtem(6,ja,je) = 8.78462

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -0.99780
      b_mtem(2,ja,je) = 4.69200
      b_mtem(3,ja,je) = -16.1219
      b_mtem(4,ja,je) = 29.3100
      b_mtem(5,ja,je) = -26.3383
      b_mtem(6,ja,je) = 9.20695

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -0.52694
      b_mtem(2,ja,je) = 7.02684
      b_mtem(3,ja,je) = -33.7508
      b_mtem(4,ja,je) = 70.0565
      b_mtem(5,ja,je) = -68.3226
      b_mtem(6,ja,je) = 25.2692

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 0.572926
      b_mtem(2,ja,je) = -2.04791
      b_mtem(3,ja,je) = 2.1134
      b_mtem(4,ja,je) = 0.246654
      b_mtem(5,ja,je) = -3.06019
      b_mtem(6,ja,je) = 1.98126

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 0.56514
      b_mtem(2,ja,je) = 0.22287
      b_mtem(3,ja,je) = -2.76973
      b_mtem(4,ja,je) = 4.54444
      b_mtem(5,ja,je) = -3.86549
      b_mtem(6,ja,je) = 1.13441


!----------
! (nh4)3h(so4)2 in e
      ja = jlvcite

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = -1.44811
      b_mtem(2,ja,je) = 6.71815
      b_mtem(3,ja,je) = -25.0141
      b_mtem(4,ja,je) = 50.1109
      b_mtem(5,ja,je) = -50.0561
      b_mtem(6,ja,je) = 19.3370

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -3.41707
      b_mtem(2,ja,je) = 13.4496
      b_mtem(3,ja,je) = -34.8018
      b_mtem(4,ja,je) = 55.2987
      b_mtem(5,ja,je) = -48.1839
      b_mtem(6,ja,je) = 17.2444

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -2.54479
      b_mtem(2,ja,je) = 11.8501
      b_mtem(3,ja,je) = -39.7286
      b_mtem(4,ja,je) = 74.2479
      b_mtem(5,ja,je) = -70.4934
      b_mtem(6,ja,je) = 26.2836

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -2.30561
      b_mtem(2,ja,je) = 14.5806
      b_mtem(3,ja,je) = -55.1238
      b_mtem(4,ja,je) = 103.451
      b_mtem(5,ja,je) = -95.2571
      b_mtem(6,ja,je) = 34.2218

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -2.20809
      b_mtem(2,ja,je) = 13.6391
      b_mtem(3,ja,je) = -57.8246
      b_mtem(4,ja,je) = 117.907
      b_mtem(5,ja,je) = -112.154
      b_mtem(6,ja,je) = 40.3058

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -1.15099
      b_mtem(2,ja,je) = 6.32269
      b_mtem(3,ja,je) = -27.3860
      b_mtem(4,ja,je) = 55.4592
      b_mtem(5,ja,je) = -54.0100
      b_mtem(6,ja,je) = 20.3469

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -1.15678
      b_mtem(2,ja,je) = 8.28718
      b_mtem(3,ja,je) = -37.3231
      b_mtem(4,ja,je) = 76.6124
      b_mtem(5,ja,je) = -74.9307
      b_mtem(6,ja,je) = 28.0559

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 0.01502
      b_mtem(2,ja,je) = -3.1197
      b_mtem(3,ja,je) = 3.61104
      b_mtem(4,ja,je) = 3.05196
      b_mtem(5,ja,je) = -9.98957
      b_mtem(6,ja,je) = 6.04155

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = -1.06477
      b_mtem(2,ja,je) = 3.38801
      b_mtem(3,ja,je) = -12.5784
      b_mtem(4,ja,je) = 25.2823
      b_mtem(5,ja,je) = -25.4611
      b_mtem(6,ja,je) = 10.0754


!----------
! nahso4 in e
      ja = jnahso4

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = 0.68259
      b_mtem(2,ja,je) = 0.71468
      b_mtem(3,ja,je) = -5.59003
      b_mtem(4,ja,je) = 11.0089
      b_mtem(5,ja,je) = -10.7983
      b_mtem(6,ja,je) = 3.82335

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -0.03956
      b_mtem(2,ja,je) = 4.52828
      b_mtem(3,ja,je) = -25.2557
      b_mtem(4,ja,je) = 54.4225
      b_mtem(5,ja,je) = -52.5105
      b_mtem(6,ja,je) = 18.6562

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -1.53503
      b_mtem(2,ja,je) = 8.27608
      b_mtem(3,ja,je) = -28.9539
      b_mtem(4,ja,je) = 55.2876
      b_mtem(5,ja,je) = -51.9563
      b_mtem(6,ja,je) = 18.6576

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -0.38793
      b_mtem(2,ja,je) = 7.14680
      b_mtem(3,ja,je) = -38.7201
      b_mtem(4,ja,je) = 84.3965
      b_mtem(5,ja,je) = -84.7453
      b_mtem(6,ja,je) = 32.1283

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -0.41982
      b_mtem(2,ja,je) = 4.26491
      b_mtem(3,ja,je) = -20.2351
      b_mtem(4,ja,je) = 42.6764
      b_mtem(5,ja,je) = -40.7503
      b_mtem(6,ja,je) = 14.2868

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -0.32912
      b_mtem(2,ja,je) = 1.80808
      b_mtem(3,ja,je) = -8.01286
      b_mtem(4,ja,je) = 15.5791
      b_mtem(5,ja,je) = -14.5494
      b_mtem(6,ja,je) = 5.27052

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = 0.10271
      b_mtem(2,ja,je) = 5.09559
      b_mtem(3,ja,je) = -30.3295
      b_mtem(4,ja,je) = 66.2975
      b_mtem(5,ja,je) = -66.3458
      b_mtem(6,ja,je) = 24.9443

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 0.608309
      b_mtem(2,ja,je) = -0.541905
      b_mtem(3,ja,je) = -2.52084
      b_mtem(4,ja,je) = 6.63297
      b_mtem(5,ja,je) = -7.24599
      b_mtem(6,ja,je) = 2.88811

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 1.98399
      b_mtem(2,ja,je) = -4.51562
      b_mtem(3,ja,je) = 8.36059
      b_mtem(4,ja,je) = -12.4948
      b_mtem(5,ja,je) = 9.67514
      b_mtem(6,ja,je) = -3.18004


!----------
! na3h(so4)2 in e
      ja = jna3hso4

! in h2so4
      je = jh2so4
      b_mtem(1,ja,je) = -0.83214
      b_mtem(2,ja,je) = 4.99572
      b_mtem(3,ja,je) = -20.1697
      b_mtem(4,ja,je) = 41.4066
      b_mtem(5,ja,je) = -42.2119
      b_mtem(6,ja,je) = 16.4855

! in nh4hso4
      je = jnh4hso4
      b_mtem(1,ja,je) = -0.65139
      b_mtem(2,ja,je) = 3.52300
      b_mtem(3,ja,je) = -22.8220
      b_mtem(4,ja,je) = 56.2956
      b_mtem(5,ja,je) = -59.9028
      b_mtem(6,ja,je) = 23.1844

! in (nh4)3h(so4)2
      je = jlvcite
      b_mtem(1,ja,je) = -1.31331
      b_mtem(2,ja,je) = 8.40835
      b_mtem(3,ja,je) = -38.1757
      b_mtem(4,ja,je) = 80.5312
      b_mtem(5,ja,je) = -79.8346
      b_mtem(6,ja,je) = 30.0219

! in (nh4)2so4
      je = jnh4so4
      b_mtem(1,ja,je) = -1.03054
      b_mtem(2,ja,je) = 8.08155
      b_mtem(3,ja,je) = -38.1046
      b_mtem(4,ja,je) = 78.7168
      b_mtem(5,ja,je) = -77.2263
      b_mtem(6,ja,je) = 29.1521

! in nahso4
      je = jnahso4
      b_mtem(1,ja,je) = -1.90695
      b_mtem(2,ja,je) = 11.6241
      b_mtem(3,ja,je) = -50.3175
      b_mtem(4,ja,je) = 105.884
      b_mtem(5,ja,je) = -103.258
      b_mtem(6,ja,je) = 37.6588

! in na3h(so4)2
      je = jna3hso4
      b_mtem(1,ja,je) = -0.34780
      b_mtem(2,ja,je) = 2.85363
      b_mtem(3,ja,je) = -17.6224
      b_mtem(4,ja,je) = 38.9220
      b_mtem(5,ja,je) = -39.8106
      b_mtem(6,ja,je) = 15.6055

! in na2so4
      je = jna2so4
      b_mtem(1,ja,je) = -0.75230
      b_mtem(2,ja,je) = 10.0140
      b_mtem(3,ja,je) = -50.5677
      b_mtem(4,ja,je) = 106.941
      b_mtem(5,ja,je) = -105.534
      b_mtem(6,ja,je) = 39.5196

! in hno3
      je = jhno3
      b_mtem(1,ja,je) = 0.057456
      b_mtem(2,ja,je) = -1.31264
      b_mtem(3,ja,je) = -1.94662
      b_mtem(4,ja,je) = 10.7024
      b_mtem(5,ja,je) = -14.9946
      b_mtem(6,ja,je) = 7.12161

! in hcl
      je = jhcl
      b_mtem(1,ja,je) = 0.637894
      b_mtem(2,ja,je) = -2.29719
      b_mtem(3,ja,je) = 0.765361
      b_mtem(4,ja,je) = 4.8748
      b_mtem(5,ja,je) = -9.25978
      b_mtem(6,ja,je) = 4.91773
!
!
!
!----------------------------------------------------------
! coefficients for %mdrh(t) = d1 + d2*t + d3*t^2 + d4*t^3    (t in kelvin)
! valid temperature range: 240 - 320 k
!----------------------------------------------------------
!
! sulfate-poor systems
! ac
      j_index = 1
      d_mdrh(j_index,1) = -58.00268351
      d_mdrh(j_index,2) = 2.031077573
      d_mdrh(j_index,3) = -0.008281218
      d_mdrh(j_index,4) = 1.00447e-05

! an
      j_index = 2
      d_mdrh(j_index,1) = 1039.137773
      d_mdrh(j_index,2) = -11.47847095
      d_mdrh(j_index,3) = 0.047702786
      d_mdrh(j_index,4) = -6.77675e-05

! as
      j_index = 3
      d_mdrh(j_index,1) = 115.8366357
      d_mdrh(j_index,2) = 0.491881663
      d_mdrh(j_index,3) = -0.00422807
      d_mdrh(j_index,4) = 7.29274e-06

! sc
      j_index = 4
      d_mdrh(j_index,1) = 253.2424151
      d_mdrh(j_index,2) = -1.429957864
      d_mdrh(j_index,3) = 0.003727554
      d_mdrh(j_index,4) = -3.13037e-06

! sn
      j_index = 5
      d_mdrh(j_index,1) = -372.4306506
      d_mdrh(j_index,2) = 5.3955633
      d_mdrh(j_index,3) = -0.019804438
      d_mdrh(j_index,4) = 2.25662e-05

! ss
      j_index = 6
      d_mdrh(j_index,1) = 286.1271416
      d_mdrh(j_index,2) = -1.670787758
      d_mdrh(j_index,3) = 0.004431373
      d_mdrh(j_index,4) = -3.57757e-06

! cc
      j_index = 7
      d_mdrh(j_index,1) = -1124.07059
      d_mdrh(j_index,2) = 14.26364209
      d_mdrh(j_index,3) = -0.054816822
      d_mdrh(j_index,4) = 6.70107e-05

! cn
      j_index = 8
      d_mdrh(j_index,1) = 1855.413934
      d_mdrh(j_index,2) = -20.29219473
      d_mdrh(j_index,3) = 0.07807482
      d_mdrh(j_index,4) = -1.017887858e-4

! an + ac
      j_index = 9
      d_mdrh(j_index,1) = 1761.176886
      d_mdrh(j_index,2) = -19.29811062
      d_mdrh(j_index,3) = 0.075676987
      d_mdrh(j_index,4) = -1.0116959e-4

! as + ac
      j_index = 10
      d_mdrh(j_index,1) = 122.1074303
      d_mdrh(j_index,2) = 0.429692122
      d_mdrh(j_index,3) = -0.003928277
      d_mdrh(j_index,4) = 6.43275e-06

! as + an
      j_index = 11
      d_mdrh(j_index,1) = 2424.634678
      d_mdrh(j_index,2) = -26.54031307
      d_mdrh(j_index,3) = 0.101625387
      d_mdrh(j_index,4) = -1.31544547798e-4

! as + an + ac
      j_index = 12
      d_mdrh(j_index,1) = 2912.082599
      d_mdrh(j_index,2) = -31.8894185
      d_mdrh(j_index,3) = 0.121185849
      d_mdrh(j_index,4) = -1.556534623e-4

! sc + ac
      j_index = 13
      d_mdrh(j_index,1) = 172.2596493
      d_mdrh(j_index,2) = -0.511006195
      d_mdrh(j_index,3) = 4.27244597e-4
      d_mdrh(j_index,4) = 4.12797e-07

! sn + ac
      j_index = 14
      d_mdrh(j_index,1) = 1596.184935
      d_mdrh(j_index,2) = -16.37945565
      d_mdrh(j_index,3) = 0.060281218
      d_mdrh(j_index,4) = -7.6161e-05

! sn + an
      j_index = 15
      d_mdrh(j_index,1) = 1916.072988
      d_mdrh(j_index,2) = -20.85594868
      d_mdrh(j_index,3) = 0.081140141
      d_mdrh(j_index,4) = -1.07954274796e-4

! sn + an + ac
      j_index = 16
      d_mdrh(j_index,1) = 1467.165935
      d_mdrh(j_index,2) = -16.01166196
      d_mdrh(j_index,3) = 0.063505582
      d_mdrh(j_index,4) = -8.66722e-05

! sn + sc
      j_index = 17
      d_mdrh(j_index,1) = 158.447059
      d_mdrh(j_index,2) = -0.628167358
      d_mdrh(j_index,3) = 0.002014448
      d_mdrh(j_index,4) = -3.13037e-06

! sn + sc + ac
      j_index = 18
      d_mdrh(j_index,1) = 1115.892468
      d_mdrh(j_index,2) = -11.76936534
      d_mdrh(j_index,3) = 0.045577399
      d_mdrh(j_index,4) = -6.05779e-05

! ss + ac
      j_index = 19
      d_mdrh(j_index,1) = 269.5432407
      d_mdrh(j_index,2) = -1.319963885
      d_mdrh(j_index,3) = 0.002592363
      d_mdrh(j_index,4) = -1.44479e-06

! ss + an
      j_index = 20
      d_mdrh(j_index,1) = 2841.334784
      d_mdrh(j_index,2) = -31.1889487
      d_mdrh(j_index,3) = 0.118809274
      d_mdrh(j_index,4) = -1.53007e-4

! ss + an + ac
      j_index = 21
      d_mdrh(j_index,1) = 2199.36914
      d_mdrh(j_index,2) = -24.11926569
      d_mdrh(j_index,3) = 0.092932361
      d_mdrh(j_index,4) = -1.21774e-4

! ss + as
      j_index = 22
      d_mdrh(j_index,1) = 395.0051604
      d_mdrh(j_index,2) = -2.521101657
      d_mdrh(j_index,3) = 0.006139319
      d_mdrh(j_index,4) = -4.43756e-06

! ss + as + ac
      j_index = 23
      d_mdrh(j_index,1) = 386.5150675
      d_mdrh(j_index,2) = -2.4632138
      d_mdrh(j_index,3) = 0.006139319
      d_mdrh(j_index,4) = -4.98796e-06

! ss + as + an
      j_index = 24
      d_mdrh(j_index,1) = 3101.538491
      d_mdrh(j_index,2) = -34.19978105
      d_mdrh(j_index,3) = 0.130118605
      d_mdrh(j_index,4) = -1.66873e-4

! ss + as + an + ac
      j_index = 25
      d_mdrh(j_index,1) = 2307.579403
      d_mdrh(j_index,2) = -25.43136774
      d_mdrh(j_index,3) = 0.098064728
      d_mdrh(j_index,4) = -1.28301e-4

! ss + sc
      j_index = 26
      d_mdrh(j_index,1) = 291.8309602
      d_mdrh(j_index,2) = -1.828912974
      d_mdrh(j_index,3) = 0.005053148
      d_mdrh(j_index,4) = -4.57516e-06

! ss + sc + ac
      j_index = 27
      d_mdrh(j_index,1) = 188.3914345
      d_mdrh(j_index,2) = -0.631345031
      d_mdrh(j_index,3) = 0.000622807
      d_mdrh(j_index,4) = 4.47196e-07

! ss + sn
      j_index = 28
      d_mdrh(j_index,1) = -167.1252839
      d_mdrh(j_index,2) = 2.969828002
      d_mdrh(j_index,3) = -0.010637255
      d_mdrh(j_index,4) = 1.13175e-05

! ss + sn + ac
      j_index = 29
      d_mdrh(j_index,1) = 1516.782768
      d_mdrh(j_index,2) = -15.7922661
      d_mdrh(j_index,3) = 0.058942209
      d_mdrh(j_index,4) = -7.5301e-05

! ss + sn + an
      j_index = 30
      d_mdrh(j_index,1) = 1739.963163
      d_mdrh(j_index,2) = -19.06576022
      d_mdrh(j_index,3) = 0.07454963
      d_mdrh(j_index,4) = -9.94302e-05

! ss + sn + an + ac
      j_index = 31
      d_mdrh(j_index,1) = 2152.104877
      d_mdrh(j_index,2) = -23.74998008
      d_mdrh(j_index,3) = 0.092256654
      d_mdrh(j_index,4) = -1.21953e-4

! ss + sn + sc
      j_index = 32
      d_mdrh(j_index,1) = 221.9976265
      d_mdrh(j_index,2) = -1.311331272
      d_mdrh(j_index,3) = 0.004406089
      d_mdrh(j_index,4) = -5.88235e-06

! ss + sn + sc + ac
      j_index = 33
      d_mdrh(j_index,1) = 1205.645615
      d_mdrh(j_index,2) = -12.71353459
      d_mdrh(j_index,3) = 0.048803922
      d_mdrh(j_index,4) = -6.41899e-05

! cc + ac
      j_index = 34
      d_mdrh(j_index,1) = 506.6737879
      d_mdrh(j_index,2) = -3.723520818
      d_mdrh(j_index,3) = 0.010814242
      d_mdrh(j_index,4) = -1.21087e-05

! cc + sc
      j_index = 35
      d_mdrh(j_index,1) = -1123.523841
      d_mdrh(j_index,2) = 14.08345977
      d_mdrh(j_index,3) = -0.053687823
      d_mdrh(j_index,4) = 6.52219e-05

! cc + sc + ac
      j_index = 36
      d_mdrh(j_index,1) = -1159.98607
      d_mdrh(j_index,2) = 14.44309169
      d_mdrh(j_index,3) = -0.054841073
      d_mdrh(j_index,4) = 6.64259e-05

! cn + ac
      j_index = 37
      d_mdrh(j_index,1) = 756.0747916
      d_mdrh(j_index,2) = -8.546826257
      d_mdrh(j_index,3) = 0.035798677
      d_mdrh(j_index,4) = -5.06629e-05

! cn + an
      j_index = 38
      d_mdrh(j_index,1) = 338.668191
      d_mdrh(j_index,2) = -2.971223403
      d_mdrh(j_index,3) = 0.012294866
      d_mdrh(j_index,4) = -1.87558e-05

! cn + an + ac
      j_index = 39
      d_mdrh(j_index,1) = -53.18033508
      d_mdrh(j_index,2) = 0.663911748
      d_mdrh(j_index,3) = 9.16326e-4
      d_mdrh(j_index,4) = -6.70354e-06

! cn + sc
      j_index = 40
      d_mdrh(j_index,1) = 3623.831129
      d_mdrh(j_index,2) = -39.27226457
      d_mdrh(j_index,3) = 0.144559515
      d_mdrh(j_index,4) = -1.78159e-4

! cn + sc + ac
      j_index = 41
      d_mdrh(j_index,1) = 3436.656743
      d_mdrh(j_index,2) = -37.16192684
      d_mdrh(j_index,3) = 0.136641377
      d_mdrh(j_index,4) = -1.68262e-4

! cn + sn
      j_index = 42
      d_mdrh(j_index,1) = 768.608476
      d_mdrh(j_index,2) = -8.051517149
      d_mdrh(j_index,3) = 0.032342332
      d_mdrh(j_index,4) = -4.52224e-05

! cn + sn + ac
      j_index = 43
      d_mdrh(j_index,1) = 33.58027951
      d_mdrh(j_index,2) = -0.308772182
      d_mdrh(j_index,3) = 0.004713639
      d_mdrh(j_index,4) = -1.19658e-05

! cn + sn + an
      j_index = 44
      d_mdrh(j_index,1) = 57.80183041
      d_mdrh(j_index,2) = 0.215264604
      d_mdrh(j_index,3) = 4.11406e-4
      d_mdrh(j_index,4) = -4.30702e-06

! cn + sn + an + ac
      j_index = 45
      d_mdrh(j_index,1) = -234.368984
      d_mdrh(j_index,2) = 2.721045204
      d_mdrh(j_index,3) = -0.006688341
      d_mdrh(j_index,4) = 2.31729e-06

! cn + sn + sc
      j_index = 46
      d_mdrh(j_index,1) = 3879.080557
      d_mdrh(j_index,2) = -42.13562874
      d_mdrh(j_index,3) = 0.155235005
      d_mdrh(j_index,4) = -1.91387e-4

! cn + sn + sc + ac
      j_index = 47
      d_mdrh(j_index,1) = 3600.576985
      d_mdrh(j_index,2) = -39.0283489
      d_mdrh(j_index,3) = 0.143710316
      d_mdrh(j_index,4) = -1.77167e-4

! cn + cc
      j_index = 48
      d_mdrh(j_index,1) = -1009.729826
      d_mdrh(j_index,2) = 12.9145339
      d_mdrh(j_index,3) = -0.049811146
      d_mdrh(j_index,4) = 6.09563e-05

! cn + cc + ac
      j_index = 49
      d_mdrh(j_index,1) = -577.0919514
      d_mdrh(j_index,2) = 8.020324227
      d_mdrh(j_index,3) = -0.031469556
      d_mdrh(j_index,4) = 3.82181e-05

! cn + cc + sc
      j_index = 50
      d_mdrh(j_index,1) = -728.9983499
      d_mdrh(j_index,2) = 9.849458215
      d_mdrh(j_index,3) = -0.03879257
      d_mdrh(j_index,4) = 4.78844e-05

! cn + cc + sc + ac
      j_index = 51
      d_mdrh(j_index,1) = -803.7026845
      d_mdrh(j_index,2) = 10.61881494
      d_mdrh(j_index,3) = -0.041402993
      d_mdrh(j_index,4) = 5.08084e-05

!
! sulfate-rich systems
! ab
      j_index = 52
      d_mdrh(j_index,1) = -493.6190458
      d_mdrh(j_index,2) = 6.747053851
      d_mdrh(j_index,3) = -0.026955267
      d_mdrh(j_index,4) = 3.45118e-05

! lv
      j_index = 53
      d_mdrh(j_index,1) = 53.37874093
      d_mdrh(j_index,2) = 1.01368249
      d_mdrh(j_index,3) = -0.005887513
      d_mdrh(j_index,4) = 8.94393e-06

! sb
      j_index = 54
      d_mdrh(j_index,1) = 206.619047
      d_mdrh(j_index,2) = -1.342735684
      d_mdrh(j_index,3) = 0.003197691
      d_mdrh(j_index,4) = -1.93603e-06

! ab + lv
      j_index = 55
      d_mdrh(j_index,1) = -493.6190458
      d_mdrh(j_index,2) = 6.747053851
      d_mdrh(j_index,3) = -0.026955267
      d_mdrh(j_index,4) = 3.45118e-05

! as + lv
      j_index = 56
      d_mdrh(j_index,1) = 53.37874093
      d_mdrh(j_index,2) = 1.01368249
      d_mdrh(j_index,3) = -0.005887513
      d_mdrh(j_index,4) = 8.94393e-06

! ss + sb
      j_index = 57
      d_mdrh(j_index,1) = 206.619047
      d_mdrh(j_index,2) = -1.342735684
      d_mdrh(j_index,3) = 0.003197691
      d_mdrh(j_index,4) = -1.93603e-06

! ss + lv
      j_index = 58
      d_mdrh(j_index,1) = 41.7619047
      d_mdrh(j_index,2) = 1.303872053
      d_mdrh(j_index,3) = -0.007647908
      d_mdrh(j_index,4) = 1.17845e-05

! ss + as + lv
      j_index = 59
      d_mdrh(j_index,1) = 41.7619047
      d_mdrh(j_index,2) = 1.303872053
      d_mdrh(j_index,3) = -0.007647908
      d_mdrh(j_index,4) = 1.17845e-05

! ss + ab
      j_index = 60
      d_mdrh(j_index,1) = -369.7142842
      d_mdrh(j_index,2) = 5.512878771
      d_mdrh(j_index,3) = -0.02301948
      d_mdrh(j_index,4) = 3.0303e-05

! ss + lv + ab
      j_index = 61
      d_mdrh(j_index,1) = -369.7142842
      d_mdrh(j_index,2) = 5.512878771
      d_mdrh(j_index,3) = -0.02301948
      d_mdrh(j_index,4) = 3.0303e-05

! sb + ab
      j_index = 62
      d_mdrh(j_index,1) = -162.8095232
      d_mdrh(j_index,2) = 2.399326592
      d_mdrh(j_index,3) = -0.009336219
      d_mdrh(j_index,4) = 1.17845e-05

! ss + sb + ab
      j_index = 63
      d_mdrh(j_index,1) = -735.4285689
      d_mdrh(j_index,2) = 8.885521857
      d_mdrh(j_index,3) = -0.033488456
      d_mdrh(j_index,4) = 4.12458e-05



!-----------------------------------------------------------------------------
!
!			psc activity coefficient model data
!
!-----------------------------------------------------------------------------
!
! magnitude of the charges on ions
      izc(jc_h)		= 1
      izc(jc_nh4)	= 1
      izc(jc_na)	= 1
      izc(jc_ca)	= 2
!
      iza(ja_hso4)	= 1
      iza(ja_so4)	= 2
      iza(ja_no3)	= 1
      iza(ja_cl)	= 1

      do jc = 1, ncation
        zc(jc) = float(izc(jc))
      enddo

      do ja = 1, nanion
        za(ja) = float(iza(ja))
      enddo


! model parameters
!
!		fitted binary parameters
!
! long-range debye-huckel contributions

      ax	=  2.917 ! debye-huckel parameter (mol fraction basis) @ 298 k
      rho	= 13.0	 ! a constant at all temperatures and pressures.
      tdum      = 298.15
!      tt	= te - 328.15
      tt	= 298.15 - 328.15

!
!---------------------------
! (h,a)
      bc_a(jc_h,ja_hso4)	=  0.178334467e2 +   &
                                   tt*(-0.625268629e1  *1.e-1 +   &
                                   tt*(0.295714662     *1.e-2 +   &
                                   tt*(0.223751841     *1.e-3/6.0 +   &
                                   tt*(0.0             *1.e-3/12.0 +   &
                                   tt*0.0              *1.e-3/20.0))))
      bc_a(jc_h,ja_so4)		= -0.982408701e2 +   &
                                   tt*(-0.205401806e2  *1.e-1 +   &
                                   tt*(-0.103568646e1  *1.e-2 +   &
                                   tt*(-0.376521937e-1 *1.e-3/6.0 +   &
                                   tt*(-0.139689758e-1 *1.e-3/12.0 +   &
                                   tt*0.0              *1.e-3/20.0))))
      bc_a(jc_h,ja_no3)		= 13.53417796	! @@@@
      bc_a(jc_h,ja_cl)		= 17.5347093	! @@@@

      b1_c_a(jc_h,ja_hso4)	= 0.0 		! @@@@
      b1_c_a(jc_h,ja_so4)	= 0.0 		! @@@@
      b1_c_a(jc_h,ja_no3)	= 0.0 		! @@@@
      b1_c_a(jc_h,ja_cl)	= 0.0		! @@@@

      alpha_c_a(jc_h,ja_hso4)	= 17.0		! @@@@
      alpha_c_a(jc_h,ja_so4)	=  9.5		! @@@@
      alpha_c_a(jc_h,ja_no3)	= 17.0		! @@@@
      alpha_c_a(jc_h,ja_cl)	= 13.0		! @@@@

      alpha1_c_a(jc_h,ja_hso4)	= 0.0		! @@@@
      alpha1_c_a(jc_h,ja_so4)	= 0.0		! @@@@
      alpha1_c_a(jc_h,ja_no3)	= 0.0		! @@@@
      alpha1_c_a(jc_h,ja_cl)	= 0.0		! @@@@
!
! (nh4,a)
      bc_a(jc_nh4,ja_hso4)	= 14.2261681 	! @@@@
      bc_a(jc_nh4,ja_so4)	= -2.858988 	! @@@@
      bc_a(jc_nh4,ja_no3)	= 24.7529  	! @@@@
      bc_a(jc_nh4,ja_cl)	=  4.659688	! @@@@

      b1_c_a(jc_nh4,ja_hso4)	=  0.0 		! @@@@
      b1_c_a(jc_nh4,ja_so4)	=  0.0		! @@@@
      b1_c_a(jc_nh4,ja_no3)	= -29.9961 	! @@@@
      b1_c_a(jc_nh4,ja_cl)	=  0.0 		! @@@@

      alpha_c_a(jc_nh4,ja_hso4)	= 19.0		! @@@@
      alpha_c_a(jc_nh4,ja_so4)	= 13.0		! @@@@
      alpha_c_a(jc_nh4,ja_no3)	=  7.0 		! @@@@
      alpha_c_a(jc_nh4,ja_cl)	= 15.0		! @@@@

      alpha1_c_a(jc_nh4,ja_hso4)=  0.0 		! @@@@
      alpha1_c_a(jc_nh4,ja_so4)	=  1.5		! @@@@
      alpha1_c_a(jc_nh4,ja_no3)	= 13.0		! @@@@
      alpha1_c_a(jc_nh4,ja_cl)	=  0.0 		! @@@@
!
! (na,a)
      bc_a(jc_na,ja_hso4)	= 62.27961 	! @@@@
      bc_a(jc_na,ja_so4)	= 34.46602	! @@@@
      bc_a(jc_na,ja_no3)	= 26.99939	! @@@@
      bc_a(jc_na,ja_cl)		= 19.93376	! @@@@

      b1_c_a(jc_na,ja_hso4)	=  0.0 		! @@@@
      b1_c_a(jc_na,ja_so4)	=  0.0 		! @@@@
      b1_c_a(jc_na,ja_no3)	= -21.6050	! @@@@
      b1_c_a(jc_na,ja_cl)	=  0.0 		! @@@@

      alpha_c_a(jc_na,ja_hso4)	= 19.0 		! @@@@
      alpha_c_a(jc_na,ja_so4)	=  8.0		! @@@@
      alpha_c_a(jc_na,ja_no3)	=  5.0 		! @@@@
      alpha_c_a(jc_na,ja_cl)	=  5.0 		! @@@@

      alpha1_c_a(jc_na,ja_hso4)	=  0.0 		! @@@@
      alpha1_c_a(jc_na,ja_so4)	=  0.0 		! @@@@
      alpha1_c_a(jc_na,ja_no3)	= 13.0 		! @@@@
      alpha1_c_a(jc_na,ja_cl)	=  0.0 		! @@@@

!
! (ca,a)					! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      bc_a(jc_ca,ja_hso4)	= 0.0
      bc_a(jc_ca,ja_so4)	= 0.0
      bc_a(jc_ca,ja_no3)	= 33.251	! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      bc_a(jc_ca,ja_cl)		= 55.396	! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      b1_c_a(jc_ca,ja_hso4)	= 0.0
      b1_c_a(jc_ca,ja_so4)	= 0.0
      b1_c_a(jc_ca,ja_no3)	= 66.931	! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      b1_c_a(jc_ca,ja_cl)	= 114.47	! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      alpha_c_a(jc_ca,ja_hso4)	= 0.0
      alpha_c_a(jc_ca,ja_so4)	= 0.0
      alpha_c_a(jc_ca,ja_no3)	= 13.0		! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      alpha_c_a(jc_ca,ja_cl)	= 13.0		! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      alpha1_c_a(jc_ca,ja_hso4)	=  0.0
      alpha1_c_a(jc_ca,ja_so4)	=  0.0
      alpha1_c_a(jc_ca,ja_no3)	=  2.0 		! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      alpha1_c_a(jc_ca,ja_cl)	=  2.0 		! clegg, pitzer & brimblecombe, j.phys.chem. 1992

!
!
! short-range contributions
!
! (h,a)
      u1_c_a(jc_h,ja_hso4)	= -0.143238371e1 +   &
                                  tt*(-0.201636224    *1.e-1 +   &
                                  tt*(-0.221902116e-1 *1.e-2 +   &
                                  tt*(0.641847819e-2  *1.e-3/6.0 +   &
                                  tt*(0.296327801e-3  *1.e-3/12.0 +   &
                                  tt*0.0              *1.e-3/20.0))))

      u1_c_a(jc_h,ja_so4)	= -0.133603464e2 +   &
                                  tt*(-0.459479578e1  *1.e-1 +   &
                                  tt*(-0.731101730    *1.e-2 +   &
                                  tt*(-0.157872023    *1.e-3/6.0 +   &
                                  tt*(-0.162230945e-3 *1.e-3/12.0 +   &
                                  tt* 0.0             *1.e-3/20.0))))
      u1_c_a(jc_h,ja_no3)	= 1.965818001  		! @@@@

      u1_c_a(jc_h,ja_cl)	= -13.7294155		! @@@@

      v1_c_a(jc_h,ja_hso4)	= -0.207474566e1 +   &
                                  tt*(0.594737744     *1.e-1 +   &
                                  tt*(0.337026110e-1  *1.e-2 +   &
                                  tt*(0.0             *1.e-3/6.0 +   &
                                  tt*(-0.394845016e-3 *1.e-3/12.0 +   &
                                  tt*0.d0             *1.e-3/20.0))))

      v1_c_a(jc_h,ja_so4)	= 0.310121997e1    +   &
                                  tt*(0.446189009e1   *1.e-1 +   &
                                  tt*(0.487627359     *1.e-2 +   &
                                  tt*(0.588748231e-2  *1.e-3/6.0 +   &
                                  tt*(-0.901983372e-3 *1.e-3/12.0 +   &
                                  tt*0.0              *1.e-3/20.0))))

      v1_c_a(jc_h,ja_no3)	= -1.411912043		! @@@@
      v1_c_a(jc_h,ja_cl)	= 3.20778857		! @@@@

      w1_c_a(jc_h,ja_hso4)	= -0.998416390e1 +   &
                                  tt*(0.348821776     *1.e-1 +   &
                                  tt*(-0.597630850e-2 *1.e-2 +   &
                                  tt*(0.909425662e-2  *1.e-3/6.0 +   &
                                  tt*(0.149166944e-3  *1.e-3/12.0 +   &
                                  tt*0.0              *1.e-3/20.0))))

      w1_c_a(jc_h,ja_so4)	= -0.107752155e2   +   &
                                  tt*(-0.879298257    *1.e-1 +   &
                                  tt*(-0.220264243    *1.e-2 +   &
                                  tt*(-0.544913927e-1 *1.e-3/6.0 +   &
                                  tt*(-0.173541364e-3 *1.e-3/12.0 +   &
                                  tt*0.0              *1.e-3/20.0))))

      w1_c_a(jc_h,ja_no3)	= -3.071864721 		! @@@@
      w1_c_a(jc_h,ja_cl)	= -14.9654933		! @@@@
!
! (nh4,a)
      u1_c_a(jc_nh4,ja_hso4)	= -0.796273529		! @@@@
      u1_c_a(jc_nh4,ja_so4)	= 0.940860		! @@@@
      u1_c_a(jc_nh4,ja_no3)	= 0.379736		! @@@@
      u1_c_a(jc_nh4,ja_cl)	= 2.072437		! @@@@

      v1_c_a(jc_nh4,ja_hso4)	= 0.663584552		! @@@@
      v1_c_a(jc_nh4,ja_so4)	= -2.587430		! @@@@
      v1_c_a(jc_nh4,ja_no3)	= -1.42646  		! @@@@
      v1_c_a(jc_nh4,ja_cl)	= -1.25000  		! @@@@

      w1_c_a(jc_nh4,ja_hso4)	= -2.56359462		! @@@@
      w1_c_a(jc_nh4,ja_so4)	= -0.740149		! @@@@
      w1_c_a(jc_nh4,ja_no3)	= 0.900729 		! @@@@
      w1_c_a(jc_nh4,ja_cl)	= -0.5682911		! @@@@
!
! (na,a)
      u1_c_a(jc_na,ja_hso4)	= -4.857197 		! @@@@
      u1_c_a(jc_na,ja_so4)	= -1.95916  		! @@@@
      u1_c_a(jc_na,ja_no3)	= 0.2666436		! @@@@
      u1_c_a(jc_na,ja_cl)	= -3.609246		! @@@@

      v1_c_a(jc_na,ja_hso4)	= 4.888311 		! @@@@
      v1_c_a(jc_na,ja_so4)	= -4.86057		! @@@@
      v1_c_a(jc_na,ja_no3)	= -2.302876		! @@@@
      v1_c_a(jc_na,ja_cl)	= -2.459821		! @@@@

      w1_c_a(jc_na,ja_hso4)	= -2.932425 		! @@@@
      w1_c_a(jc_na,ja_so4)	= -3.725962		! @@@@
      w1_c_a(jc_na,ja_no3)	= 0.5269081		! @@@@
      w1_c_a(jc_na,ja_cl)	= -5.646077		! @@@@

!
! (ca,a)					! clegg, pitzer & brimblecombe, j.phys.chem. 1992
      u1_c_a(jc_ca,ja_hso4)	= 0.0
      u1_c_a(jc_ca,ja_so4)	= 0.0
      u1_c_a(jc_ca,ja_no3)	= 0.45953
      u1_c_a(jc_ca,ja_cl)	= 19.148

      v1_c_a(jc_ca,ja_hso4)	= 0.0
      v1_c_a(jc_ca,ja_so4)	= 0.0
      v1_c_a(jc_ca,ja_no3)	= -8.387
      v1_c_a(jc_ca,ja_cl)	= -36.497

      w1_c_a(jc_ca,ja_hso4)	= 0.0
      w1_c_a(jc_ca,ja_so4)	= 0.0
      w1_c_a(jc_ca,ja_no3)	= 0.0
      w1_c_a(jc_ca,ja_cl)	= 0.0

!		fitted ternary parameters (short-range contributions)
!
! (c,c',hso4)
      uc_cp_a(jc_h,jc_nh4,ja_hso4)	=  0.0		! @@@@
      uc_cp_a(jc_h,jc_na,ja_hso4)	= -2.92819	! @@@@
      uc_cp_a(jc_nh4,jc_na,ja_hso4)	=  0.0    	! @@@@

      uc_cp_a(jc_nh4,jc_h,ja_hso4)	= -uc_cp_a(jc_h,jc_nh4,ja_hso4)
      uc_cp_a(jc_na,jc_h,ja_hso4)	= -uc_cp_a(jc_h,jc_na,ja_hso4)
      uc_cp_a(jc_na,jc_nh4,ja_hso4)	= -uc_cp_a(jc_nh4,jc_na,ja_hso4)


      wc_cp_a(jc_h,jc_nh4,ja_hso4)	= -19.494	! @@@@
      wc_cp_a(jc_h,jc_na,ja_hso4)	= -8.96894	! @@@@
      wc_cp_a(jc_nh4,jc_na,ja_hso4)	=  0.0     	! @@@@

      wc_cp_a(jc_nh4,jc_h,ja_hso4)	= wc_cp_a(jc_h,jc_nh4,ja_hso4)
      wc_cp_a(jc_na,jc_h,ja_hso4)	= wc_cp_a(jc_h,jc_na,ja_hso4)
      wc_cp_a(jc_na,jc_nh4,ja_hso4)	= wc_cp_a(jc_nh4,jc_na,ja_hso4)


      q1_c_cp_a(jc_h,jc_nh4,ja_hso4)	= 8.7607  	! @@@@
      q1_c_cp_a(jc_h,jc_na,ja_hso4)	= 4.16202	! @@@@
      q1_c_cp_a(jc_nh4,jc_na,ja_hso4)	= 0.0

      q1_c_cp_a(jc_nh4,jc_h,ja_hso4)	= 8.7607  	! @@@@
      q1_c_cp_a(jc_na,jc_h,ja_hso4)	= 4.16202	! @@@@
      q1_c_cp_a(jc_na,jc_nh4,ja_hso4)	= 0.0    	! @@@@


! (c,c',so4)
      uc_cp_a(jc_h,jc_nh4,ja_so4)	= 6.5216  	! @@@@
      uc_cp_a(jc_h,jc_na,ja_so4)	= 0.0    	! @@@@
      uc_cp_a(jc_nh4,jc_na,ja_so4)	= 0.0    	! @@@@

      uc_cp_a(jc_nh4,jc_h,ja_so4)	= -uc_cp_a(jc_h,jc_nh4,ja_so4)
      uc_cp_a(jc_na,jc_h,ja_so4)	= -uc_cp_a(jc_h,jc_na,ja_so4)
      uc_cp_a(jc_na,jc_nh4,ja_so4)	= -uc_cp_a(jc_nh4,jc_na,ja_so4)


      wc_cp_a(jc_h,jc_nh4,ja_so4)	= -4.3507 	! @@@@
      wc_cp_a(jc_h,jc_na,ja_so4)	= 15.9075 	! @@@@
      wc_cp_a(jc_nh4,jc_na,ja_so4)	= -1.4832 	! @@@@

      wc_cp_a(jc_nh4,jc_h,ja_so4)	= wc_cp_a(jc_h,jc_nh4,ja_so4)
      wc_cp_a(jc_na,jc_h,ja_so4)	= wc_cp_a(jc_h,jc_na,ja_so4)
      wc_cp_a(jc_na,jc_nh4,ja_so4)	= wc_cp_a(jc_nh4,jc_na,ja_so4)


      q1_c_cp_a(jc_h,jc_nh4,ja_so4)	=  0.0 		! @@@@
      q1_c_cp_a(jc_h,jc_na,ja_so4)	= -8.82425	! @@@@
      q1_c_cp_a(jc_nh4,jc_na,ja_so4)	=  0.76211	! @@@@

      q1_c_cp_a(jc_nh4,jc_h,ja_so4)	=  0.0 		! @@@@
      q1_c_cp_a(jc_na,jc_h,ja_so4)	= -8.82425	! @@@@
      q1_c_cp_a(jc_na,jc_nh4,ja_so4)	=  0.76211	! @@@@


! (c,c',no3)
      uc_cp_a(jc_h,jc_nh4,ja_no3)	= -0.46338 	! @@@@
      uc_cp_a(jc_h,jc_na,ja_no3)	= 1.1749  	! @@@@
      uc_cp_a(jc_nh4,jc_na,ja_no3)	= 0.2130  	! @@@@
      uc_cp_a(jc_h,jc_ca,ja_no3)        = 0.0		! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      uc_cp_a(jc_nh4,jc_h,ja_no3)	= -uc_cp_a(jc_h,jc_nh4,ja_no3)
      uc_cp_a(jc_na,jc_h,ja_no3)	= -uc_cp_a(jc_h,jc_na,ja_no3)
      uc_cp_a(jc_na,jc_nh4,ja_no3)	= -uc_cp_a(jc_nh4,jc_na,ja_no3)
      uc_cp_a(jc_ca,jc_h,ja_no3)        = -uc_cp_a(jc_h,jc_ca,ja_no3)


      wc_cp_a(jc_h,jc_nh4,ja_no3)	= -3.0708 	! @@@@
      wc_cp_a(jc_h,jc_na,ja_no3)	=  0.46039	! @@@@
      wc_cp_a(jc_nh4,jc_na,ja_no3)	= -0.35411	! @@@@
      wc_cp_a(jc_h,jc_ca,ja_no3)	=  8.343 ! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      wc_cp_a(jc_nh4,jc_h,ja_no3)	= wc_cp_a(jc_h,jc_nh4,ja_no3)
      wc_cp_a(jc_na,jc_h,ja_no3)	= wc_cp_a(jc_h,jc_na,ja_no3)
      wc_cp_a(jc_na,jc_nh4,ja_no3)	= wc_cp_a(jc_nh4,jc_na,ja_no3)
      wc_cp_a(jc_ca,jc_h,ja_no3)	= wc_cp_a(jc_h,jc_ca,ja_no3) ! clegg, pitzer & brimblecombe, j.phys.chem. 1992


      q1_c_cp_a(jc_h,jc_nh4,ja_no3)	= 0.28491 	! @@@@
      q1_c_cp_a(jc_h,jc_na,ja_no3)	= 0.0     	! @@@@
      q1_c_cp_a(jc_nh4,jc_na,ja_no3)	= 0.046254	! @@@@
      q1_c_cp_a(jc_h,jc_ca,ja_no3)	= -3.678 ! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      q1_c_cp_a(jc_nh4,jc_h,ja_no3)	= 0.28491 	! @@@@
      q1_c_cp_a(jc_na,jc_h,ja_no3)	= 0.0     	! @@@@
      q1_c_cp_a(jc_na,jc_nh4,ja_no3)	= 0.046254	! @@@@
      q1_c_cp_a(jc_ca,jc_h,ja_no3)	= -3.678 ! clegg, pitzer & brimblecombe, j.phys.chem. 1992


! (c,c',cl)
      uc_cp_a(jc_h,jc_nh4,ja_cl)	= 0.0 		! @@@@
      uc_cp_a(jc_h,jc_na,ja_cl)		= 0.0 		! @@@@
      uc_cp_a(jc_nh4,jc_na,ja_cl)	= 0.0		! @@@@
      uc_cp_a(jc_h,jc_ca,ja_cl)         = 0.0	! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      uc_cp_a(jc_nh4,jc_h,ja_cl)	= -uc_cp_a(jc_h,jc_nh4,ja_cl)
      uc_cp_a(jc_na,jc_h,ja_cl)		= -uc_cp_a(jc_h,jc_na,ja_cl)
      uc_cp_a(jc_na,jc_nh4,ja_cl)	= -uc_cp_a(jc_nh4,jc_na,ja_cl)
      uc_cp_a(jc_ca,jc_h,ja_cl)         = -uc_cp_a(jc_h,jc_ca,ja_cl) ! clegg, pitzer & brimblecombe, j.phys.chem. 1992


      wc_cp_a(jc_h,jc_nh4,ja_cl)	= -19.977 	! @@@@
      wc_cp_a(jc_h,jc_na,ja_cl)		= 2.2490 	! @@@@
      wc_cp_a(jc_nh4,jc_na,ja_cl)	= -5.6414 	! @@@@
      wc_cp_a(jc_h,jc_ca,ja_cl)		= -8.112 ! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      wc_cp_a(jc_nh4,jc_h,ja_cl)	= wc_cp_a(jc_h,jc_nh4,ja_cl)
      wc_cp_a(jc_na,jc_h,ja_cl)		= wc_cp_a(jc_h,jc_na,ja_cl)
      wc_cp_a(jc_na,jc_nh4,ja_cl)	= wc_cp_a(jc_nh4,jc_na,ja_cl)
      wc_cp_a(jc_ca,jc_h,ja_cl)		= wc_cp_a(jc_h,jc_ca,ja_cl) ! clegg, pitzer & brimblecombe, j.phys.chem. 1992


      q1_c_cp_a(jc_h,jc_nh4,ja_cl)	= 10.233 	! @@@@
      q1_c_cp_a(jc_h,jc_na,ja_cl)	= -0.25080	! @@@@
      q1_c_cp_a(jc_nh4,jc_na,ja_cl)	= 3.2919 	! @@@@
      q1_c_cp_a(jc_h,jc_ca,ja_cl)	= 12.67	! clegg, pitzer & brimblecombe, j.phys.chem. 1992

      q1_c_cp_a(jc_nh4,jc_h,ja_cl)	= 10.233 	! @@@@
      q1_c_cp_a(jc_na,jc_h,ja_cl)	= -0.25080	! @@@@
      q1_c_cp_a(jc_na,jc_nh4,ja_cl)	= 3.2919 	! @@@@
      q1_c_cp_a(jc_ca,jc_h,ja_cl)	= 12.67	! clegg, pitzer & brimblecombe, j.phys.chem. 1992


! (a,a',h)
      ua_ap_c(ja_hso4,ja_so4,jc_h)	= 0.0 		! @@@@
      ua_ap_c(ja_hso4,ja_no3,jc_h) =0.201362+0.08483*(tdum-273.15) ! @@@@
      ua_ap_c(ja_hso4,ja_cl,jc_h)	= 0.0 		! @@@@
      ua_ap_c(ja_so4,ja_no3,jc_h)	= 0.0  		! @@@@
      ua_ap_c(ja_so4,ja_cl,jc_h)	= 0.0  		! @@@@
      ua_ap_c(ja_no3,ja_cl,jc_h)	= 0.0  		! @@@@

      ua_ap_c(ja_so4,ja_hso4,jc_h)	= -ua_ap_c(ja_hso4,ja_so4,jc_h)
      ua_ap_c(ja_no3,ja_hso4,jc_h)	= -ua_ap_c(ja_hso4,ja_no3,jc_h)
      ua_ap_c(ja_cl,ja_hso4,jc_h)	= -ua_ap_c(ja_hso4,ja_cl,jc_h)
      ua_ap_c(ja_no3,ja_so4,jc_h)	= -ua_ap_c(ja_so4,ja_no3,jc_h)
      ua_ap_c(ja_cl,ja_so4,jc_h)	= -ua_ap_c(ja_so4,ja_cl,jc_h)
      ua_ap_c(ja_cl,ja_no3,jc_h)	= -ua_ap_c(ja_no3,ja_cl,jc_h)


      wa_ap_c(ja_hso4,ja_so4,jc_h)	= 0.0 		! @@@@
      wa_ap_c(ja_hso4,ja_no3,jc_h)	= -4.280 	! @@@@
      wa_ap_c(ja_hso4,ja_cl,jc_h)	= 0.0  		! @@@@
      wa_ap_c(ja_so4,ja_no3,jc_h) = -0.033291*(tdum-273.15) ! @@@@
      wa_ap_c(ja_so4,ja_cl,jc_h)	= 0.0  		! @@@@
      wa_ap_c(ja_no3,ja_cl,jc_h)	= 0.0  		! @@@@

      wa_ap_c(ja_so4,ja_hso4,jc_h)	= wa_ap_c(ja_hso4,ja_so4,jc_h)
      wa_ap_c(ja_no3,ja_hso4,jc_h)	= wa_ap_c(ja_hso4,ja_no3,jc_h)
      wa_ap_c(ja_cl,ja_hso4,jc_h)	= wa_ap_c(ja_hso4,ja_cl,jc_h)
      wa_ap_c(ja_no3,ja_so4,jc_h)	= wa_ap_c(ja_so4,ja_no3,jc_h)
      wa_ap_c(ja_cl,ja_so4,jc_h)	= wa_ap_c(ja_so4,ja_cl,jc_h)
      wa_ap_c(ja_cl,ja_no3,jc_h)	= wa_ap_c(ja_no3,ja_cl,jc_h)


      q1_a_ap_c(ja_hso4,ja_so4,jc_h)	= 0.0 		! @@@@
      q1_a_ap_c(ja_hso4,ja_no3,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_hso4,ja_cl,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_so4,ja_no3,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_so4,ja_cl,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_no3,ja_cl,jc_h)	= 0.0  		! @@@@

      q1_a_ap_c(ja_so4,ja_hso4,jc_h)	= 0.0 		! @@@@
      q1_a_ap_c(ja_no3,ja_hso4,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_cl,ja_hso4,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_no3,ja_so4,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_cl,ja_so4,jc_h)	= 0.0  		! @@@@
      q1_a_ap_c(ja_cl,ja_no3,jc_h)	= 0.0  		! @@@@


! (a,a',nh4)
      ua_ap_c(ja_hso4,ja_so4,jc_nh4)	= -16.317 	! @@@@
      ua_ap_c(ja_hso4,ja_no3,jc_nh4)	=  0.0   	! @@@@
      ua_ap_c(ja_hso4,ja_cl,jc_nh4)	=  0.0		! @@@@
      ua_ap_c(ja_so4,ja_no3,jc_nh4)	= -1.2163	! @@@@
      ua_ap_c(ja_so4,ja_cl,jc_nh4)	= -1.0709	! @@@@
      ua_ap_c(ja_no3,ja_cl,jc_nh4)	=  0.0

      ua_ap_c(ja_so4,ja_hso4,jc_nh4)	= +16.317	! @@@@
      ua_ap_c(ja_no3,ja_hso4,jc_nh4)	=  0.0   	! @@@@
      ua_ap_c(ja_cl,ja_hso4,jc_nh4)	=  0.0   	! @@@@
      ua_ap_c(ja_no3,ja_so4,jc_nh4)	= +1.2163	! @@@@
      ua_ap_c(ja_cl,ja_so4,jc_nh4)	= +1.0709	! @@@@
      ua_ap_c(ja_cl,ja_no3,jc_nh4)	=  0.0


      wa_ap_c(ja_hso4,ja_so4,jc_nh4)	= -14.753 	! @@@@
      wa_ap_c(ja_hso4,ja_no3,jc_nh4)	= -2.9369	! @@@@
      wa_ap_c(ja_hso4,ja_cl,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_so4,ja_no3,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_so4,ja_cl,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_no3,ja_cl,jc_nh4)	= -0.2207	! @@@@

      wa_ap_c(ja_so4,ja_hso4,jc_nh4)	= -14.753 	! @@@@
      wa_ap_c(ja_no3,ja_hso4,jc_nh4)	= -2.9369	! @@@@
      wa_ap_c(ja_cl,ja_hso4,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_no3,ja_so4,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_cl,ja_so4,jc_nh4)	= 0.0    	! @@@@
      wa_ap_c(ja_cl,ja_no3,jc_nh4)	= -0.2207	! @@@@


      q1_a_ap_c(ja_hso4,ja_so4,jc_nh4)	= 4.7204  	! @@@@
      q1_a_ap_c(ja_hso4,ja_no3,jc_nh4)	= 0.0    	! @@@@
      q1_a_ap_c(ja_hso4,ja_cl,jc_nh4)	= 0.0    	! @@@@
      q1_a_ap_c(ja_so4,ja_no3,jc_nh4)	= 2.9795 	! @@@@
      q1_a_ap_c(ja_so4,ja_cl,jc_nh4)	= 1.0869 	! @@@@
      q1_a_ap_c(ja_no3,ja_cl,jc_nh4)	= -0.1173 	! @@@@

      q1_a_ap_c(ja_so4,ja_hso4,jc_nh4)	= 4.7204  	! @@@@
      q1_a_ap_c(ja_no3,ja_hso4,jc_nh4)	= 0.0    	! @@@@
      q1_a_ap_c(ja_cl,ja_hso4,jc_nh4)	= 0.0    	! @@@@
      q1_a_ap_c(ja_no3,ja_so4,jc_nh4)	= 2.9795 	! @@@@
      q1_a_ap_c(ja_cl,ja_so4,jc_nh4)	= 1.0869 	! @@@@
      q1_a_ap_c(ja_cl,ja_no3,jc_nh4)	= -0.1173 	! @@@@


! (a,a',na)
      ua_ap_c(ja_hso4,ja_so4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_hso4,ja_no3,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_hso4,ja_cl,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_so4,ja_no3,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_so4,ja_cl,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_no3,ja_cl,jc_na)	= 0.0     	! @@@@

      ua_ap_c(ja_so4,ja_hso4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_no3,ja_hso4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_cl,ja_hso4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_no3,ja_so4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_cl,ja_so4,jc_na)	= 0.0     	! @@@@
      ua_ap_c(ja_cl,ja_no3,jc_na)	= 0.0     	! @@@@


      wa_ap_c(ja_hso4,ja_so4,jc_na)	= 0.0      	! @@@@
      wa_ap_c(ja_hso4,ja_no3,jc_na)	= 0.0      	! @@@@
      wa_ap_c(ja_hso4,ja_cl,jc_na)	= 0.0      	! @@@@
      wa_ap_c(ja_so4,ja_no3,jc_na)	= -9.498	! @@@@
      wa_ap_c(ja_so4,ja_cl,jc_na)	= 4.827		! @@@@
      wa_ap_c(ja_no3,ja_cl,jc_na)	= -6.923	! @@@@

      wa_ap_c(ja_so4,ja_hso4,jc_na)	= 0.0      	! @@@@
      wa_ap_c(ja_no3,ja_hso4,jc_na)	= 0.0      	! @@@@
      wa_ap_c(ja_cl,ja_hso4,jc_na)	= 0.0     	! @@@@
      wa_ap_c(ja_no3,ja_so4,jc_na)	= -9.498	! @@@@
      wa_ap_c(ja_cl,ja_so4,jc_na)	= 4.827		! @@@@
      wa_ap_c(ja_cl,ja_no3,jc_na)	= -6.923	! @@@@


      q1_a_ap_c(ja_hso4,ja_so4,jc_na)	= -4.68641	! @@@@
      q1_a_ap_c(ja_hso4,ja_no3,jc_na)	= 0.0     	! @@@@
      q1_a_ap_c(ja_hso4,ja_cl,jc_na)	= 0.0    	! @@@@
      q1_a_ap_c(ja_so4,ja_no3,jc_na)	= 8.528  	! @@@@
      q1_a_ap_c(ja_so4,ja_cl,jc_na)	= 0.05163	! @@@@
      q1_a_ap_c(ja_no3,ja_cl,jc_na)	= 4.181  	! @@@@

      q1_a_ap_c(ja_so4,ja_hso4,jc_na)	= -4.68641	! @@@@
      q1_a_ap_c(ja_no3,ja_hso4,jc_na)	= 0.0    	! @@@@
      q1_a_ap_c(ja_cl,ja_hso4,jc_na)	= 0.0    	! @@@@
      q1_a_ap_c(ja_no3,ja_so4,jc_na)	= 8.528  	! @@@@
      q1_a_ap_c(ja_cl,ja_so4,jc_na)	= 0.05163	! @@@@
      q1_a_ap_c(ja_cl,ja_no3,jc_na)	= 4.181  	! @@@@




!--------------

      xnuc(jc_h,ja_hso4)   = 0.5	! za(ja_hso4)/(zc(jc_h)+za(ja_hso4))
      xnuc(jc_h,ja_so4)    = 0.66666667	! za(ja_so4)/(zc(jc_h)+za(ja_so4))
      xnuc(jc_h,ja_no3)    = 0.5	! za(ja_no3)/(zc(jc_h)+za(ja_no3))
      xnuc(jc_h,ja_cl)     = 0.5	! za(ja_cl)/(zc(jc_h)+za(ja_cl))

      xnuc(jc_nh4,ja_hso4) = 0.5	! za(ja_hso4)/(zc(jc_nh4)+za(ja_hso4))
      xnuc(jc_nh4,ja_so4)  = 0.66666667	! za(ja_so4)/(zc(jc_nh4)+za(ja_so4))
      xnuc(jc_nh4,ja_no3)  = 0.5	! za(ja_no3)/(zc(jc_nh4)+za(ja_no3))
      xnuc(jc_nh4,ja_cl)   = 0.5	! za(ja_cl)/(zc(jc_nh4)+za(ja_cl))

      xnuc(jc_na,ja_hso4)  = 0.5	! za(ja_hso4)/(zc(jc_na)+za(ja_hso4))
      xnuc(jc_na,ja_so4)   = 0.66666667	! za(ja_so4)/(zc(jc_na)+za(ja_so4))
      xnuc(jc_na,ja_no3)   = 0.5	! za(ja_no3)/(zc(jc_na)+za(ja_no3))
      xnuc(jc_na,ja_cl)    = 0.5	! za(ja_cl)/(zc(jc_na)+za(ja_cl))

      xnuc(jc_ca,ja_hso4)  = 0.33333333	! za(ja_hso4)/(zc(jc_ca)+za(ja_hso4))
      xnuc(jc_ca,ja_so4)   = 0.5	! za(ja_so4)/(zc(jc_ca)+za(ja_so4))
      xnuc(jc_ca,ja_no3)   = 0.33333333	! za(ja_no3)/(zc(jc_ca)+za(ja_no3))
      xnuc(jc_ca,ja_cl)    = 0.33333333	! za(ja_cl)/(zc(jc_ca)+za(ja_cl))

!-------------

      xnua(ja_hso4,jc_h)   = 0.5	! zc(jc_h)/(zc(jc_h)+za(ja_hso4))
      xnua(ja_hso4,jc_nh4) = 0.5	! zc(jc_nh4)/(zc(jc_nh4)+za(ja_hso4))
      xnua(ja_hso4,jc_na)  = 0.5	! zc(jc_na)/(zc(jc_na)+za(ja_hso4))
      xnua(ja_hso4,jc_ca)  = 0.66666667	! zc(jc_ca)/(zc(jc_ca)+za(ja_hso4))

      xnua(ja_so4,jc_h)    = 0.33333333 ! zc(jc_h)/(zc(jc_h)+za(ja_so4))
      xnua(ja_so4,jc_nh4)  = 0.33333333	! zc(jc_nh4)/(zc(jc_nh4)+za(ja_so4))
      xnua(ja_so4,jc_na)   = 0.33333333	! zc(jc_na)/(zc(jc_na)+za(ja_so4))
      xnua(ja_so4,jc_ca)   = 0.5	! zc(jc_ca)/(zc(jc_ca)+za(ja_so4))

      xnua(ja_no3,jc_h)    = 0.5	! zc(jc_h)/(zc(jc_h)+za(ja_no3))
      xnua(ja_no3,jc_nh4)  = 0.5	! zc(jc_nh4)/(zc(jc_nh4)+za(ja_no3))
      xnua(ja_no3,jc_na)   = 0.5	! zc(jc_na)/(zc(jc_na)+za(ja_no3))
      xnua(ja_no3,jc_ca)   = 0.66666667	! zc(jc_ca)/(zc(jc_ca)+za(ja_no3))

      xnua(ja_cl,jc_h)     = 0.5	! zc(jc_h)/(zc(jc_h)+za(ja_cl))
      xnua(ja_cl,jc_nh4)   = 0.5	! zc(jc_nh4)/(zc(jc_nh4)+za(ja_cl))
      xnua(ja_cl,jc_na)    = 0.5	! zc(jc_na)/(zc(jc_na)+za(ja_cl))
      xnua(ja_cl,jc_ca)    = 0.66666667	! zc(jc_ca)/(zc(jc_ca)+za(ja_cl))



      endif ! first

      return
      end subroutine load_mosaic_parameters











!***********************************************************************
! updates all temperature dependent thermodynamic parameters
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine update_thermodynamic_constants
!     implicit none
!     include 'mosaic.h'
! local variables
      real tr, rt, term
! function
!     real fn_keq

!
      tr = 298.15			! reference temperature
!
      rt = 82.056*t_k/(1.e9*1.e6)	! [m^3 atm/nmol]

! gas-liquid
      keq_gl(1)= 1.0				        ! kelvin effect (default)
      keq_gl(2)= fn_keq(57.64 , 13.79, -5.39,t_k)*rt     ! nh3(g)  <=> nh3(l)
      keq_gl(3)= fn_keq(2.63e6, 29.17, 16.83,t_k)*rt     ! hno3(g) <=> no3- + h+
      keq_gl(4)= fn_keq(2.00e6, 30.20, 19.91,t_k)*rt     ! hcl(g)  <=> cl- + h+

! liquid-liquid
      keq_ll(1)= fn_keq(1.0502e-2, 8.85, 25.14,t_k)      ! hso4- <=> so4= + h+
      keq_ll(2)= fn_keq(1.805e-5, -1.50, 26.92,t_k)      ! nh3(l) + h2o = nh4+ + oh-
      keq_ll(3)= fn_keq(1.01e-14,-22.52, 26.92,t_k)      ! h2o(l) <=> h+ + oh-


      kp_nh3   = keq_ll(3)/(keq_ll(2)*keq_gl(2))
      kp_nh4no3= kp_nh3/keq_gl(3)
      kp_nh4cl = kp_nh3/keq_gl(4)


! solid-gas
      keq_sg(1)= fn_keq(4.72e-17,-74.38,6.12,t_k)/rt**2  ! nh4no3<=>nh3(g)+hno3(g)
      keq_sg(2)= fn_keq(8.43e-17,-71.00,2.40,t_k)/rt**2  ! nh4cl <=>nh3(g)+hcl(g)

! solid-liquid
      keq_sl(jnh4so4) = fn_keq(1.040,-2.65, 38.57, t_k)  ! amso4(s) = 2nh4+ + so4=
      keq_sl(jlvcite) = fn_keq(11.8, -5.19, 54.40, t_k)  ! lvcite(s)= 3nh4+ + hso4- + so4=
      keq_sl(jnh4hso4)= fn_keq(117.0,-2.87, 15.83, t_k)  ! amhso4(s)= nh4+ + hso4-
      keq_sl(jnh4no3) = fn_keq(12.21,-10.4, 17.56, t_k)  ! nh4no3(s)= nh4+ + no3-
      keq_sl(jnh4cl)  = fn_keq(17.37,-6.03, 16.92, t_k)  ! nh4cl(s) = nh4+ + cl-
      keq_sl(jna2so4) = fn_keq(0.491, 0.98, 39.75, t_k)  ! na2so4(s)= 2na+ + so4=
      keq_sl(jnahso4) = fn_keq(313.0, 0.8,  14.79, t_k)  ! nahso4(s)= na+ + hso4-
      keq_sl(jna3hso4)= 1.e10		 	        ! na3h(so4)2(s) = 2na+ + hso4- + so4=
      keq_sl(jnano3)  = fn_keq(11.95,-8.22, 16.01, t_k)  ! nano3(s) = na+ + no3-
      keq_sl(jnacl)   = fn_keq(38.28,-1.52, 16.89, t_k)  ! nacl(s)  = na+ + cl-
      keq_sl(jcacl2)  = fn_keq(8.0e11,32.84,44.79, t_k)*1.e5  ! cacl2(s) = ca++ + 2cl-
      keq_sl(jcano3)  = fn_keq(4.31e5, 7.83,42.01, t_k)*1.e5  ! ca(no3)2(s) = ca++ + 2no3-


      term = (647.15 - t_k)/647.15
      sigma_water = 0.2358*term**1.256 * (1. - 0.625*term) ! surface tension of pure water in n/m


      return
      end subroutine update_thermodynamic_constants




!***********************************************************************
! functions used in mosaic
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------



!----------------------------------------------------------
      real function fn_keq(keq_298, a, b, t)
!     implicit none
! subr. arguments
      real keq_298, a, b, t
! local variables
      real tt


        tt = 298.15/t
        fn_keq = keq_298*exp(a*(tt-1.)+b*(1.+alog(tt)-tt))

      return
      end function fn_keq
!----------------------------------------------------------




!----------------------------------------------------------
      real function drh_mutual(j_index)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer j_index
! local variables
      integer j


      j = j_index

      drh_mutual = d_mdrh(j,1)    +   &
                   d_mdrh(j,2)*t_k +   &
                   d_mdrh(j,3)*t_k**2 +   &
                   d_mdrh(j,4)*t_k**3


      return
      end function drh_mutual
!----------------------------------------------------------






!----------------------------------------------------------
! zsr method at 60% rh
!
      real function aerosol_water_up(ibin) ! kg (water)/m^3 (air)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer ibin
! local variables
      integer jp, je
      real dum
! function
!     real bin_molality_60


      jp = jtotal
      dum = 0.0

      do je = 1, 12	! exclude hno3 and hcl in water calculation
        dum = dum + 1.e-9*electrolyte(je,jp,ibin)/bin_molality_60(je)
      enddo

      aerosol_water_up = dum

      return
      end function aerosol_water_up
!----------------------------------------------------------






!----------------------------------------------------------
! zsr method
      real function aerosol_water(jp,ibin) ! kg (water)/m^3 (air)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer jp, ibin
! local variables
      integer je
      real dum
! function
!     real bin_molality

      dum = 0.0

      do je = 1, 15	! exclude hno3 and hcl in water calculation
        dum = dum + 1.e-9*electrolyte(je,jp,ibin)/bin_molality(je,ibin)
      enddo

      aerosol_water = dum

      if(aerosol_water .le. 0.0)then
        write(6,*)'iclm  jclm  ibin  jp = ',   &
                   iclm_aer, jclm_aer, ibin, jp
        write(6,*)'ah2o, water = ', ah2o, aerosol_water
        write(6,*)'dry mass = ', mass_dry_a(ibin)
        write(6,*)'soluble mass = ', mass_soluble_a(ibin)
        write(6,*)'number = ', num_a(ibin)
        do je = 1, nsoluble
          write(6,44)ename(je), electrolyte(je,jp,ibin)
        enddo
        write(6,*)'error in water calculation'
        write(6,*)'water content cannot be negative or zero'
        write(6,*)'setting jaerosolstate to all_solid'

        jaerosolstate(ibin) = all_solid
        jphase(ibin)    = jsolid
        jhyst_leg(ibin) = jhyst_lo

!        write(6,*)'stopping execution in function aerosol_water'
!        stop
!        call peg_error_fatal( lunerr_aer,
!     &      'stopping execution in function aerosol_water' )
      endif

44    format(a7, 2x, e11.3)


      return
      end function aerosol_water
!----------------------------------------------------------





!----------------------------------------------------------
      real function bin_molality(je,ibin)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer je, ibin
! local variables
      real aw, xm


      aw = max(ah2o_a(ibin), aw_min(je))
      aw = min(aw, 0.99999)


      if(aw .lt. 0.97)then

        xm = a_zsr(1,je)        +   &
             a_zsr(2,je)*aw    +   &
             a_zsr(3,je)*aw**2 +   &
             a_zsr(4,je)*aw**3 +   &
             a_zsr(5,je)*aw**4 +   &
             a_zsr(6,je)*aw**5

        bin_molality = 55.509*xm/(1. - xm)

      else

        bin_molality = -b_zsr(je)*alog(aw)

      endif


      return
      end function bin_molality
!----------------------------------------------------------





!----------------------------------------------------------
      real function bin_molality_60(je)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer je
! local variables
      real aw, xm


      aw = 0.6

      xm = a_zsr(1,je)        +   &
           a_zsr(2,je)*aw    +   &
           a_zsr(3,je)*aw**2 +   &
           a_zsr(4,je)*aw**3 +   &
           a_zsr(5,je)*aw**4 +   &
           a_zsr(6,je)*aw**5

      bin_molality_60 = 55.509*xm/(1. - xm)

      return
      end function bin_molality_60
!----------------------------------------------------------





!----------------------------------------------------------
      real function fnlog_gamz(ja,je)	! ja in je
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer ja, je
! local variables
      real aw


      aw = max(ah2o, aw_min(je))

      fnlog_gamz = b_mtem(1,ja,je) + aw*   &
                  (b_mtem(2,ja,je) + aw*   &
                  (b_mtem(3,ja,je) + aw*   &
                  (b_mtem(4,ja,je) + aw*   &
                  (b_mtem(5,ja,je) + aw*   &
                   b_mtem(6,ja,je) ))))

      return
      end function fnlog_gamz
!----------------------------------------------------------




!----------------------------------------------------------
      real function fnlog_gam0(je,x)
!     implicit none
!     include 'mosaic.h'
! subr. arguments
      integer je
      real x
! local variables
      real xi


      xi = min(x, im_max(je))
      fnlog_gam0 = b_km(1,je)*sqrt(xi) + xi*(b_km(2,je) +   &
                   xi*(b_km(3,je) + xi*(b_km(4,je) + xi* b_km(5,je) )))

      return
      end function fnlog_gam0
!----------------------------------------------------------




!----------------------------------------------------------
      real function mean_molecular_speed(t, mw)	! in cm/s
!     implicit none
! subr. arguments
      real t, mw	! t(k)

        mean_molecular_speed = 1.455e4 * sqrt(t/mw)

      return
      end function mean_molecular_speed
!----------------------------------------------------------




!----------------------------------------------------------
      real function gas_diffusivity(t, p, mw, vm)	! in cm^2/s
!     implicit none
! subr. arguments
      real mw, vm, t, p	! t(k), p(atm)


      gas_diffusivity = (1.0e-3 * t**1.75 * sqrt(1./mw + 0.035))/   &
                             (p * (vm**0.333333 + 2.7189)**2)


      return
      end function gas_diffusivity
!----------------------------------------------------------




!----------------------------------------------------------
      real function fuchs_sutugin(rkn,a)
!     implicit none
! subr. arguments
      real rkn, a
! local variables
      real rnum, denom


      rnum  = 0.75*a*(1. + rkn)
      denom = rkn**2 + rkn + 0.283*rkn*a + 0.75*a
      fuchs_sutugin = rnum/denom

      return
      end function fuchs_sutugin
!----------------------------------------------------------





!----------------------------------------------------------
      real function cubic( psngl, qsngl, rsngl )
!     implicit none
! subr arguments
      real psngl, qsngl, rsngl
! local variables
      double precision p, q, r, a, b, d, m, n, third, y
      real k, phi, thesign, x(3), duma
      integer icase, kk

      third = 1.d0/3.d0

      q = dble(qsngl)
      p = dble(psngl)
      r = dble(rsngl)

      a = (1.d0/3.d0)*((3.d0*q) - (p*p))
      b = (1.d0/27.d0)*((2.d0*p*p*p) - (9.d0*p*q) + (27.d0*r))

      d = ( ((a*a*a)/27.d0) + ((b*b)/4.d0) )

      if(d .gt. 0.)then	!	=> 1 real and 2 complex roots
        icase = 1
      elseif(d .eq. 0.)then !	=> 3 real roots, atleast 2 identical
        icase = 2
      else	! d < 0		=> 3 distinct real roots
        icase = 3
      endif


      goto (1,2,3), icase

! case 1: d > 0
1     thesign = 1.
      if(b .gt. 0.)then
        b = -b
        thesign = -1.
      endif

      m = thesign*((-b/2.d0) + (sqrt(d)))**(third)
      n = thesign*((-b/2.d0) - (sqrt(d)))**(third)

      cubic = real( (m) + (n) - (p/3.d0) )
      return

! case 2: d = 0
2     thesign = 1.
      if(b .gt. 0.)then
        b = -b
        thesign = -1.
      endif

      m = thesign*(-b/2.d0)**third
      n = m

      x(1) = real( (m) + (n) - (p/3.d0) )
      x(2) = real( (-m/2.d0) + (-n/2.d0) - (p/3.d0) )
      x(2) = real( (-m/2.d0) + (-n/2.d0) - (p/3.d0) )

      cubic = 0.
      do kk = 1, 3
        if(x(kk).gt.cubic) cubic = x(kk)
      enddo
      return

! case 3: d < 0
3     if(b.gt.0.)then
        thesign = -1.
      elseif(b.lt.0.)then
        thesign = 1.
      endif

! rce 18-nov-2004 -- make sure that acos argument is between +/-1.0
!     phi = acos(thesign*sqrt( (b*b/4.d0)/(-a*a*a/27.d0) ))	! radians
      duma = thesign*sqrt( (b*b/4.d0)/(-a*a*a/27.d0) )
      duma = min( duma, +1.0 )
      duma = max( duma, -1.0 )
      phi  = acos( duma )	! radians


      cubic = 0.
      do kk = 1, 3
        k = kk-1
        y = 2.*sqrt(-a/3.)*cos(phi + 120.*k*0.017453293)
        x(kk) = real((y) - (p/3.d0))
        if(x(kk).gt.cubic) cubic = x(kk)
      enddo
      return

      end function cubic
!----------------------------------------------------------




!----------------------------------------------------------
      real function quadratic(a,b,c)
!     implicit none
! subr. arguments
      real a, b, c
! local variables
      real x, dum


        if(b .ne. 0.0)then
        x = 4.*(a/b)*(c/b)
        else
        x = 1.e+6
        endif

        if(abs(x) .lt. 1.e-6)then
          dum = real( dble(0.5*x) +   &
                      dble(0.125*x**2) +   &
                      dble(0.0625*x**3) )

          quadratic = (-0.5*b/a)*dum

          if(quadratic .lt. 0.)then
            quadratic = -b/a - quadratic
          endif

        else
          quadratic = real(dble(-b)+dsqrt(dble(b*b)-dble(4.*a*c)))/   &
                                          (2.*a)
        endif

      return
      end function quadratic
!----------------------------------------------------------


!=====================================================================

















!***********************************************************************
! computes aerosol optical properties
!
! author: rahul a. zaveri
! update: jan 2005
!-----------------------------------------------------------------------
      subroutine aerosol_optical_properties(iclm, jclm, nz, refindx, &
        radius_wet, number_bin)
! changed to use rsub instead of rclm 7-8-04 egc
      use module_data_mosaic_asect
      use module_data_mosaic_other
      use module_state_description, only:  param_first_scalar

!     implicit none

! subr arguments
      integer, intent(in   ) :: iclm, jclm, nz
      real, dimension (1:nbin_a_maxd, 1:kmaxd), intent(inout ) :: &
            number_bin, radius_wet
      complex, dimension (1:nbin_a_maxd, 1:kmaxd), intent(inout ) :: &
            refindx

! local variables
      integer iaer, ibin, iphase, isize, itype, je, k, l, m
      integer ilaporte, jlaporte
      integer p1st
      real xt


! if a species index is less than this value, then the species is not defined
      p1st = param_first_scalar

! fix number of subareas at 1
      nsubareas = 1
	
      lunerr_aer = lunerr
      ncorecnt_aer = ncorecnt
	
      call load_mosaic_parameters
      
      do 110 m = 1, nsubareas	
      do 100 k = 1, nz

        cair_mol_m3 = cairclm(k)*1.e6	! cairclm(k) is in mol/cc
        cair_mol_cc = cairclm(k)

        conv1a = cair_mol_m3*1.e9		! converts q/mol(air) to nq/m^3 (q = mol or g)
        conv1b = 1./conv1a			! converts nq/m^3 to q/mol(air)
        conv2a = cair_mol_m3*18.*1.e-3		! converts mol(h2o)/mol(air) to kg(h2o)/m^3(air)
        conv2b = 1./conv2a			! converts kg(h2o)/m^3(air) to mol(h2o)/mol(air)


! initialize to zero
        do ibin = 1, nbin_a
          do iaer = 1, naer
            aer(iaer,jtotal,ibin)  = 0.0
          enddo

          do je = 1, nelectrolyte
            electrolyte(je,jtotal,ibin)  = 0.0
          enddo

          jaerosolstate(ibin) = -1	! initialize to default value

        enddo


! rce 18-nov-2004 - map (transfer) aerosol mass/water/number from rsub 
!   to mosaic arrays (aer, watr_a, num_a)
! always map so4 and number, 
!   but only map other species when (lptr_xxx .ge. p1st)
! (the mapping is identical to that done in mapgasaerspecies)

        iphase = ai_phase
        ibin = 0
        do 90 itype = 1, ntype_aer
        do 90 isize = 1, nsize_aer(itype)
        ibin = ibin + 1

! aer array units are nmol/(m^3 air)
        l = lptr_so4_aer(isize,itype,iphase)
        aer(iso4_a,jtotal,ibin)=rsub(l,k,m)*conv1a

        l = lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ino3_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ino3_a,jtotal,ibin)=0.0
        end if

        l = lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(icl_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(icl_a,jtotal,ibin)=0.0
        end if

        l = lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(inh4_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(inh4_a,jtotal,ibin)=0.0
        end if

        l = lptr_oc_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ioc_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ioc_a,jtotal,ibin)=0.0
        end if

        l = lptr_bc_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ibc_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ibc_a,jtotal,ibin)=0.0
        end if

        l = lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ina_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ina_a,jtotal,ibin)=0.0
        end if

        l = lptr_oin_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ioin_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ioin_a,jtotal,ibin)=0.0
        end if

        l = lptr_msa_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(imsa_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(imsa_a,jtotal,ibin)=0.0
        end if

        l = lptr_co3_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ico3_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ico3_a,jtotal,ibin)=0.0
        end if

        l = lptr_ca_aer(isize,itype,iphase)
        if (l .ge. p1st) then
            aer(ica_a,jtotal,ibin)=rsub(l,k,m)*conv1a
        else
            aer(ica_a,jtotal,ibin)=0.0
        end if

! water_a and water_a_hyst units are kg/(m^3 air)
        l = hyswptr_aer(isize,itype)
        if (l .ge. p1st) then
            water_a_hyst(ibin)=rsub(l,k,m)*conv2a
        else
            water_a_hyst(ibin)=0.0
        end if

! water_a units are kg/(m^3 air)
        l = waterptr_aer(isize,itype)
        if (l .ge. p1st) then
            water_a(ibin)=rsub(l,k,m)*conv2a
        else
            water_a(ibin)=0.0
        end if

! num_a units are #/(cm^3 air)
        l = numptr_aer(isize,itype,iphase)
        num_a(ibin) = rsub(l,k,m)*cair_mol_cc


          call check_aerosol_mass(ibin)
          if(jaerosolstate(ibin) .eq. no_aerosol)goto 90	! ignore this bin
          call conform_electrolytes(jtotal,ibin,xt) 			! conforms aer(jtotal) to a valid aerosol
          call check_aerosol_mass(ibin) 			! check mass again after conform_electrolytes
          if(jaerosolstate(ibin) .eq. no_aerosol)goto 90	! ignore this bin
          call conform_aerosol_number(ibin)   			! adjusts number conc so that it conforms with bin mass and diameter
          call calc_dry_n_wet_aerosol_props(ibin)		! calc dp_wet, ref index



          refindx(ibin,k)    = ri_avg_a(ibin)			! vol avg ref index
          radius_wet(ibin,k) = dp_wet_a(ibin)/2.0		! wet radius (cm)
          number_bin(ibin,k) = num_a(ibin)			! #/cc air

90      continue

100   continue	! k levels
110   continue	! m subareas


      return
      end subroutine aerosol_optical_properties










!***********************************************************************
!  save aerosol drymass and drydens before aerosol mass transfer is
!  calculated this subr is called from within subr mosaic_dynamic_solver,
!  after the initial calls to check_aerosol_mass, conform_electrolytes,
!  conform_aerosol_number, and aerosol_phase_state, but before the mass
!  transfer is calculated
!
! author: richard c. easter
!-----------------------------------------------------------------------
      subroutine save_pregrow_props

      use module_data_mosaic_asect, only:  drydens_pregrow, drymass_pregrow, &
                                           ntype_aer, nsize_aer
      use module_data_mosaic_other, only:  cairclm

!     implicit none
!     include 'v33com'
!     include 'v33com9a'
!     include 'v33com9b'
!     include 'mosaic.h'

!   subr arguments (none)

!   local variables
      integer ibin, isize, itype


! air conc in mol/cm^3
      cair_mol_cc = cairclm(kclm_aer)

! compute then save drymass and drydens for each bin
      ibin = 0
      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
      ibin = ibin + 1

      call calc_dry_n_wet_aerosol_props( ibin )

      drymass_pregrow(isize,itype) = mass_dry_a(ibin)/cair_mol_cc	! g/mol(air)
      if(jaerosolstate(ibin) .eq. no_aerosol) then
          drydens_pregrow(isize,itype) = -1.
      else
          drydens_pregrow(isize,itype) = dens_dry_a(ibin)		! g/cc
      end if

      end do
      end do

      return
      end subroutine save_pregrow_props





!***********************************************************************
! special output
!
! author: richard c. easter
!-----------------------------------------------------------------------
	subroutine specialoutaa( iclm, jclm, kclm, msub, fromwhere )

!	implicit none

	integer iclm, jclm, kclm, msub
	character*(*) fromwhere

	return
	end subroutine specialoutaa                                     




!***********************************************************************
! box model test output
!
! author: richard c. easter
!-----------------------------------------------------------------------
	subroutine aerchem_boxtest_output(   &
      		iflag, iclm, jclm, kclm, msub, dtchem )

	use module_data_mosaic_asect
	use module_data_mosaic_other
!	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com9a'

	integer iflag, iclm, jclm, kclm, msub
	real dtchem

!   local variables
	integer lun
	parameter (lun=83)
	integer ientryno
	save ientryno
	integer icomp, iphase, isize, itype, k, l, m, n

	real dtchem_sv1
	save dtchem_sv1
	real rsub_sv1(l2maxd,kmaxd,nsubareamaxd)

	data ientryno / -13579 /


!   bypass unless maerchem_boxtest_output > 0
	if (maerchem_boxtest_output .le. 0) return



!
! *** currently this only works for ntype_aer = 1
!
	itype = 1
	iphase = ai_phase

!   do initial output
	if (ientryno .ne. -13579) goto 1000

	ientryno = +1
	call peg_message( lunerr, '***' )
	call peg_message( lunerr, '*** doing initial aerchem_boxtest_output' )
	call peg_message( lunerr, '***' )

	write(lun) ltot, ltot2, itot, jtot, ktot
	write(lun) (name(l), l=1,ltot2)

	write(lun) maerocoag, maerchem, maeroptical
	write(lun) msectional, maerosolincw

	write(lun) nsize_aer(itype), ntot_mastercomp_aer

	do icomp = 1, ntot_mastercomp_aer
	    write(lun)   &
      		name_mastercomp_aer(icomp)
	    write(lun)   &
      		dens_mastercomp_aer(icomp),     mw_mastercomp_aer(icomp)
	end do

	do isize = 1, nsize_aer(itype)
	    write(lun)   &
      		ncomp_plustracer_aer(itype),   &
		ncomp_aer(itype),   &
      		waterptr_aer(isize,itype),   &
		numptr_aer(isize,itype,iphase),   &
      		mprognum_aer(isize,itype,iphase)
	    write(lun)   &
      	      ( mastercompptr_aer(l,itype),   &
		massptr_aer(l,isize,itype,iphase),   &
      		l=1,ncomp_plustracer_aer(itype) )
	    write(lun)   &
      		volumcen_sect(isize,itype),   &
		volumlo_sect(isize,itype),   &
      		volumhi_sect(isize,itype),   &
		dcen_sect(isize,itype),   &
      		dlo_sect(isize,itype),   &
		dhi_sect(isize,itype)
	    write(lun)   &
      		lptr_so4_aer(isize,itype,iphase),   &
      		lptr_msa_aer(isize,itype,iphase),   &
      		lptr_no3_aer(isize,itype,iphase),   &
      		lptr_cl_aer(isize,itype,iphase),   &
      		lptr_co3_aer(isize,itype,iphase),   &
      		lptr_nh4_aer(isize,itype,iphase),   &
      		lptr_na_aer(isize,itype,iphase),   &
      		lptr_ca_aer(isize,itype,iphase),   &
      		lptr_oin_aer(isize,itype,iphase),   &
      		lptr_oc_aer(isize,itype,iphase),   &
      		lptr_bc_aer(isize,itype,iphase),   &
      		hyswptr_aer(isize,itype)
	end do

!
!   test iflag
!
1000	continue
	if (iflag .eq. 1) goto 1010
	if (iflag .eq. 2) goto 2000
	if (iflag .eq. 3) goto 3000
	return

!
!   iflag=1 -- save initial values
!
1010	continue
	dtchem_sv1 = dtchem
	do m = 1, nsubareas
	do k = 1, ktot
	do l = 1, ltot2
	    rsub_sv1(l,k,m) = rsub(l,k,m)
	end do
	end do
	end do

	return

!
!   iflag=2 -- save intermediate values before doing move_sections
!   (this is deactivated for now)
!
2000	continue
	return


!
!   iflag=3 -- do output
!
3000	continue
	do m = 1, nsubareas
	do k = 1, ktot

	write(lun) iymdcur, ihmscur, iclm, jclm, k, m, nsubareas
	write(lun) t, dtchem_sv1, cairclm(k), relhumclm(k),   &
      		ptotclm(k), afracsubarea(k,m)

	write(lun) (rsub_sv1(l,k,m), rsub(l,k,m), l=1,ltot2)

	end do
	end do


	return
	end subroutine aerchem_boxtest_output 



!***********************************************************************
! "debugging" output when mosaic encounters "fatal error" situation
!
! author: richard c. easter
!-----------------------------------------------------------------------
	subroutine mosaic_error_fatal( luna, ibin, msga )
!
!   dumps current column information 
!   when a fatal computational error occurs
!
	use module_data_mosaic_asect
	use module_data_mosaic_other
!	implicit none

!   arguments
	integer luna, ibin
	character*(*) msga

!   local variables
	integer icomp, iphase, isize, itype, k, l, lunb, m, n
	real dtchem_sv1


!
! *** currently this only works for ntype_aer = 1
!
	itype = 1


	lunb = luna
	if (lunb .le. 0) lunb = 6

9000	format( a )
9010	format( 7i10 )
9020	format( 3(1pe19.11) )

	write(lunb,9000)
	write(lunb,9000) 'mosaic_error_fatal - msga ='
	write(lunb,9000) msga
	write(lunb,9000) 'i, j, k, msub,ibin ='
	write(lunb,9010) iclm_aer, jclm_aer, kclm_aer, mclm_aer, ibin

	write(lunb,9010) ltot, ltot2, itot, jtot, ktot
	write(lunb,9000) (name(l), l=1,ltot2)

	write(lunb,9010) maerocoag, maerchem, maeroptical
	write(lunb,9010) msectional, maerosolincw

	write(lunb,9010) nsize_aer(itype), ntot_mastercomp_aer

	do icomp = 1, ntot_mastercomp_aer
	    write(lunb,9000)   &
      		name_mastercomp_aer(icomp)
	    write(lunb,9020)   &
      		dens_mastercomp_aer(icomp),     mw_mastercomp_aer(icomp)
	end do

	do isize = 1, nsize_aer(itype)
	    write(lunb,9010)   &
      		ncomp_plustracer_aer(itype),   &
		ncomp_aer(itype),   &
      		waterptr_aer(isize,itype),   &
		numptr_aer(isize,itype,iphase),   &
      		mprognum_aer(isize,itype,iphase)
	    write(lunb,9010)   &
      	      ( mastercompptr_aer(l,itype),   &
		massptr_aer(l,isize,itype,iphase),   &
      		l=1,ncomp_plustracer_aer(itype) )
	    write(lunb,9020)   &
      		volumcen_sect(isize,itype),   &
		volumlo_sect(isize,itype),   &
      		volumhi_sect(isize,itype),   &
		dcen_sect(isize,itype),   &
      		dlo_sect(isize,itype),   &
		dhi_sect(isize,itype)
	    write(lunb,9010)   &
      		lptr_so4_aer(isize,itype,iphase),   &
      		lptr_msa_aer(isize,itype,iphase),   &
      		lptr_no3_aer(isize,itype,iphase),   &
      		lptr_cl_aer(isize,itype,iphase),   &
      		lptr_co3_aer(isize,itype,iphase),   &
      		lptr_nh4_aer(isize,itype,iphase),   &
      		lptr_na_aer(isize,itype,iphase),   &
      		lptr_ca_aer(isize,itype,iphase),   &
      		lptr_oin_aer(isize,itype,iphase),   &
      		lptr_oc_aer(isize,itype,iphase),   &
      		lptr_bc_aer(isize,itype,iphase),   &
      		hyswptr_aer(isize,itype)
	end do


	dtchem_sv1 = -1.0
	do m = 1, nsubareas
	do k = 1, ktot

	write(lunb,9010) iymdcur, ihmscur, iclm_aer, jclm_aer, k, m, nsubareas
	write(lunb,9020) t, dtchem_sv1, cairclm(k), relhumclm(k),   &
      		ptotclm(k), afracsubarea(k,m)

	write(lunb,9020) (rsub(l,k,m), l=1,ltot2)

	end do
	end do


	call peg_error_fatal( luna, msga )

	return
	end subroutine mosaic_error_fatal
!-----------------------------------------------------------------------



      end module module_mosaic_therm
